## -------------------------------------------------------------------------
## Builder worker protocol and restart contracts.
## -------------------------------------------------------------------------

builder_profile_source_runtime(globalenv())
builder_repo_source("io.R", local = globalenv())
builder_repo_source("profile.R", local = globalenv())
builder_repo_source("inspect.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())
builder_repo_source("state.R", local = globalenv())

builder_worker_path <- builder_profile_inst_path("builder", "worker.R")
if (nzchar(builder_worker_path) && file.exists(builder_worker_path)) {
  sys.source(builder_worker_path, envir = globalenv())
}
builder_repo_source("session.R", local = globalenv())

test_that("an isolated worker reopens a QS2-backed snapshot", {
  skip_if_not_installed("callr")
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  source <- file.path(root, "pbmc.qs2")
  qs2::qs_save(SeuratObject::pbmc_small, source)
  inspected <- builder_adapter_inspect(
    builder_seurat_file_adapter(source)
  )
  frozen <- .builder_snapshot_seurat_impl(
    inspected$object,
    file.path(root, "dataset-a"),
    available_bytes = 2^40,
    source = inspected$snapshot_source
  )

  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list(`dataset-a` = frozen$snapshot)
  )
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  expect_s3_class(worker, "builder_worker")
  expect_equal(builder_worker_cell_count(worker, "dataset-a"), 80L)

  worker <- builder_worker_restart(worker)
  expect_s3_class(worker, "builder_worker")
  expect_equal(builder_worker_cell_count(worker, "dataset-a"), 80L)
})

test_that("the real worker defers the package runtime until Build", {
  skip_if_not_installed("callr")
  worker <- builder_worker_start(builder_profile_inst_path("builder"))
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  before <- worker$process$run(function() {
    get(".builder_worker_capabilities", envir = globalenv())$states
  })
  expect_identical(before[["package"]], "unloaded")

  after <- worker$process$run(function() {
    builder_worker_require_capability("build")
    list(
      states = get(
        ".builder_worker_capabilities",
        envir = globalenv()
      )$states,
      exporter = exists("exportFromSeurat", mode = "function")
    )
  })
  expect_identical(after$states[["package"]], "ready")
  expect_identical(after$states[["build"]], "ready")
  expect_true(after$exporter)
})

test_that("an installed worker can check the HLA page without a content check", {
  skip_if_not_installed("callr")
  skip_if_not(
    nzchar(base::system.file(package = "CerebroNexus", lib.loc = .libPaths())),
    "CerebroNexus is not installed for a background R process"
  )
  ## A worker restarted after Stop goes straight to Build. With an installed
  ## package the HLA core is bound only by the immune capability.
  original <- .builder_worker_package_source
  assign(".builder_worker_package_source", function(...) NULL, globalenv())
  withr::defer(assign(".builder_worker_package_source", original, globalenv()))
  worker <- builder_worker_start(builder_profile_inst_path("builder"))
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  found <- worker$process$run(function() {
    builder_worker_require_capability("build")
    exists("hla_detect_chains", mode = "function")
  })
  expect_true(found)
})

test_that("authentication accounts are redacted from every protocol owner", {
  raw_contains <- function(value, text) {
    bytes <- serialize(value, NULL)
    needle <- charToRaw(text)
    any(vapply(
      seq_len(max(0L, length(bytes) - length(needle) + 1L)),
      function(index) identical(bytes[index + seq_along(needle) - 1L], needle),
      logical(1)
    ))
  }
  sentinel <- "protocol-password-8b21"
  request <- builder_command(
    "build",
    "session",
    payload = list(
      kind = "build",
      auth_accounts = list(list(
        id = "auth-account-1",
        username = "protocol-user-8b21",
        password = sentinel
      ))
    )
  )
  protocol <- builder_enqueue(builder_request_protocol("epoch-auth"), request)
  queued <- protocol$queue[[1L]]
  protocol$pending <- queued
  protocol$awaiting_ack[[queued$request_id]] <- queued
  redacted <- builder_protocol_redact_auth(protocol)

  expect_s3_class(redacted, "builder_request_protocol")
  expect_false(raw_contains(redacted, sentinel))

  terminal <- .builder_protocol_terminal_request(queued, "stopped")
  expect_false(raw_contains(terminal, sentinel))
})

test_that("build recovery never retries or returns authentication accounts", {
  raw_contains <- function(value, text) {
    bytes <- serialize(value, NULL)
    needle <- charToRaw(text)
    any(vapply(
      seq_len(max(0L, length(bytes) - length(needle) + 1L)),
      function(index) identical(bytes[index + seq_along(needle) - 1L], needle),
      logical(1)
    ))
  }
  sentinel <- "recovery-password-1d73"
  request <- builder_command(
    "build",
    "session",
    payload = list(
      kind = "build",
      auth_accounts = list(list(password = sentinel))
    )
  )
  protocol <- builder_enqueue(builder_request_protocol("epoch-old"), request)
  protocol <- builder_protocol_dispatch(protocol)$protocol
  recovered <- builder_protocol_recover(protocol, epoch = "epoch-new")

  expect_length(recovered$retried, 0L)
  expect_false(raw_contains(recovered, sentinel))
})

test_that("persistent commands are FIFO and queries replace only their slot", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 1L, slot = "projection")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 1L, slot = "spatial")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 2L, slot = "projection")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command("alignment", "a")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command("settings", "a")
  )

  expect_identical(
    builder_pending_ids(protocol),
    c(
      "preview:a:spatial:1",
      "preview:a:projection:2",
      "alignment:a",
      "settings:a"
    )
  )
})

test_that("preview queries overtake queued loads of other datasets", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(protocol, builder_command("load", "b"))
  dispatched <- builder_protocol_dispatch(protocol)
  protocol <- dispatched$protocol
  for (id in c("c", "d", "e")) {
    protocol <- builder_enqueue(protocol, builder_command("load", id))
  }
  protocol <- builder_enqueue(
    protocol,
    builder_query("projection_previews", "a", 1L, slot = "gallery")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 2L, slot = "projection")
  )

  # The running load stays pending; the queries jump the queued loads but
  # keep their own FIFO order.
  expect_identical(protocol$pending$id, "load:b")
  expect_identical(
    builder_pending_ids(protocol),
    c(
      "projection_previews:a:gallery:1",
      "preview:a:projection:2",
      "load:c",
      "load:d",
      "load:e"
    )
  )
  persistent <- Filter(
    function(request) isTRUE(request$persistent),
    protocol$queue
  )
  expect_identical(
    vapply(persistent, `[[`, integer(1), "persistent_sequence"),
    2:4
  )

  protocol <- builder_protocol_complete(
    protocol,
    builder_worker_response(dispatched$request, list())
  )$protocol
  protocol <- builder_protocol_acknowledge(
    protocol,
    dispatched$request$request_id
  )
  expect_identical(
    builder_protocol_dispatch(protocol)$request$id,
    "projection_previews:a:gallery:1"
  )
})

test_that("queries never overtake their own load or other commands", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(protocol, builder_command("load", "a"))
  protocol <- builder_enqueue(protocol, builder_command("settings", "b"))
  protocol <- builder_enqueue(protocol, builder_command("load", "c"))
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "b", 1L, slot = "projection")
  )
  protocol <- builder_enqueue(protocol, builder_command("load", "d"))
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "d", 1L, slot = "projection")
  )

  expect_identical(
    builder_pending_ids(protocol),
    c(
      "load:a",
      "settings:b",
      "preview:b:projection:1",
      "load:c",
      "load:d",
      "preview:d:projection:1"
    )
  )
})

test_that("query replacement never crosses dataset or logical slot", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 1L, slot = "projection")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "b", 1L, slot = "projection")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 1L, slot = "spatial")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 2L, slot = "projection")
  )

  expect_identical(
    builder_pending_ids(protocol),
    c(
      "preview:b:projection:1",
      "preview:a:spatial:1",
      "preview:a:projection:2"
    )
  )
})

test_that("persistent completion waits for owner acknowledgement", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "alignment",
      "a",
      revision = 4L,
      snapshot_identity = "snapshot-a"
    )
  )
  sent <- builder_protocol_dispatch(protocol)
  protocol <- sent$protocol
  request <- sent$request

  completed <- builder_protocol_complete(
    protocol,
    list(
      epoch = request$epoch,
      token = request$token,
      generation = request$generation,
      dataset_revision = request$dataset_revision,
      snapshot_identity = request$snapshot_identity,
      value = TRUE
    )
  )

  expect_null(completed$protocol$pending)
  expect_identical(length(completed$protocol$awaiting_ack), 1L)
  expect_identical(
    builder_protocol_dispatch(completed$protocol)$blocked,
    "awaiting_ack"
  )
  acknowledged <- builder_protocol_acknowledge(
    completed$protocol,
    request$token
  )
  expect_length(acknowledged$awaiting_ack, 0L)
})

test_that("responses must match every request identity before pending clears", {
  protocol <- builder_enqueue(
    builder_request_protocol(epoch = "worker-1"),
    builder_query(
      "preview",
      "a",
      7L,
      slot = "projection",
      revision = 12L,
      snapshot_identity = "snapshot-a"
    )
  )
  sent <- builder_protocol_dispatch(protocol)
  request <- sent$request

  fields <- c(
    "epoch",
    "token",
    "generation",
    "dataset_revision",
    "snapshot_identity"
  )
  for (field in fields) {
    stale <- list(
      epoch = request$epoch,
      token = request$token,
      generation = request$generation,
      dataset_revision = request$dataset_revision,
      snapshot_identity = request$snapshot_identity,
      value = "stale"
    )
    stale[[field]] <- if (is.integer(stale[[field]])) {
      stale[[field]] + 1L
    } else {
      paste0(stale[[field]], "-old")
    }
    rejected <- builder_protocol_complete(sent$protocol, stale)
    expect_false(rejected$accepted, info = field)
    expect_false(is.null(rejected$protocol$pending), info = field)
  }
})

test_that("Review and Build wait behind the acknowledged command cutoff", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(
    protocol,
    builder_command("settings", "a")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command("review", "session")
  )
  first <- builder_protocol_dispatch(protocol)
  request <- first$request
  completed <- builder_protocol_complete(
    first$protocol,
    list(
      epoch = request$epoch,
      token = request$token,
      generation = request$generation,
      dataset_revision = request$dataset_revision,
      snapshot_identity = request$snapshot_identity,
      value = TRUE
    )
  )

  expect_identical(
    builder_protocol_dispatch(completed$protocol)$blocked,
    "awaiting_ack"
  )
  acknowledged <- builder_protocol_acknowledge(
    completed$protocol,
    request$token
  )
  expect_identical(
    builder_protocol_dispatch(acknowledged)$request$kind,
    "review"
  )
})

test_that("Review and Build barriers discard queued replaceable queries", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 1L, slot = "projection")
  )
  protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
  protocol <- builder_enqueue(
    protocol,
    builder_query("coords", "a", 1L, slot = "section-a")
  )
  protocol <- builder_enqueue(protocol, builder_command("review", "session"))

  expect_identical(
    builder_pending_ids(protocol),
    c("settings:a", "review:session")
  )

  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 2L, slot = "projection")
  )
  protocol <- builder_enqueue(protocol, builder_command("build", "session"))
  expect_identical(
    builder_pending_ids(protocol),
    c("settings:a", "review:session", "build:session")
  )
})

test_that("a barrier invalidates a pending query without consuming its generation", {
  protocol <- builder_enqueue(
    builder_request_protocol(epoch = "worker-1"),
    builder_query("preview", "a", 1L, slot = "projection")
  )
  dispatched <- builder_protocol_dispatch(protocol)
  protocol <- builder_enqueue(
    dispatched$protocol,
    builder_command("build", "session")
  )

  completed <- builder_protocol_complete(
    protocol,
    builder_worker_response(
      dispatched$request,
      error = "obsolete query failed"
    )
  )

  expect_false(completed$accepted)
  expect_null(completed$protocol$pending)
  expect_null(completed$value)
  expect_no_error(
    builder_enqueue(
      completed$protocol,
      builder_query("preview", "a", 2L, slot = "projection")
    )
  )
  expect_identical(
    builder_protocol_dispatch(completed$protocol)$request$kind,
    "build"
  )
})

test_that("snapshot-bound previews survive settings-only revision changes", {
  protocol <- builder_protocol_dataset(
    builder_request_protocol(epoch = "worker-1"),
    "a",
    revision = 1L,
    snapshot_identity = "snapshot-a"
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query(
      "projection_previews",
      "a",
      1L,
      slot = "viewer-projection-gallery",
      payload = list(revision_independent = TRUE),
      revision = 1L,
      snapshot_identity = "snapshot-a"
    )
  )
  dispatched <- builder_protocol_dispatch(protocol)
  revised <- builder_protocol_dataset(
    dispatched$protocol,
    "a",
    revision = 2L,
    snapshot_identity = "snapshot-a"
  )

  completed <- builder_protocol_complete(
    revised,
    builder_worker_response(dispatched$request, list(umap = "preview"))
  )

  expect_true(completed$accepted)
  expect_identical(completed$value, list(umap = "preview"))
})

test_that("persistent dispatch rebinds the latest dataset identity in FIFO order", {
  protocol <- builder_protocol_dataset(
    builder_request_protocol(epoch = "worker-1"),
    "a",
    revision = 0L,
    snapshot_identity = "snapshot-0"
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "settings",
      "a",
      revision = 0L,
      snapshot_identity = "snapshot-0"
    )
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "alignment",
      "a",
      revision = 0L,
      snapshot_identity = "snapshot-0"
    )
  )

  first <- builder_protocol_dispatch(protocol)
  completed <- builder_protocol_complete(
    first$protocol,
    builder_worker_response(first$request, TRUE)
  )
  acknowledged <- builder_protocol_acknowledge(
    completed$protocol,
    first$request$request_id
  )
  acknowledged <- builder_protocol_dataset(
    acknowledged,
    "a",
    revision = 1L,
    snapshot_identity = "snapshot-1"
  )

  second <- builder_protocol_dispatch(acknowledged)
  expect_identical(second$request$kind, "alignment")
  expect_identical(second$request$dataset_revision, 1L)
  expect_identical(second$request$snapshot_identity, "snapshot-1")
})

test_that("Build is single-flight while queued running or cancelling", {
  queued <- builder_enqueue(
    builder_request_protocol(epoch = "worker-1"),
    builder_command("build", "session")
  )
  expect_error(
    builder_enqueue(queued, builder_command("build", "session")),
    "already"
  )

  running <- builder_protocol_dispatch(queued)$protocol
  expect_error(
    builder_enqueue(running, builder_command("build", "session")),
    "already"
  )

  cancelling <- builder_protocol_cancel(running)
  expect_identical(cancelling$build_status, "cancelling")
  expect_error(
    builder_enqueue(cancelling, builder_command("build", "session")),
    "already"
  )
})

test_that("protocol recovery retries FIFO commands but never a Build", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
  protocol <- builder_enqueue(protocol, builder_command("alignment", "a"))
  protocol <- builder_enqueue(
    protocol,
    builder_command("build", "session", payload = list(id = "build-1"))
  )
  dispatched <- builder_protocol_dispatch(protocol)

  recovered <- builder_protocol_recover(
    dispatched$protocol,
    epoch = "worker-2",
    reason = "worker_restarted"
  )

  expect_identical(recovered$protocol$epoch, "worker-2")
  expect_identical(
    builder_pending_ids(recovered$protocol),
    c("settings:a", "alignment:a")
  )
  expect_identical(
    vapply(recovered$retried, `[[`, character(1), "kind"),
    c("settings", "alignment")
  )
  expect_identical(
    vapply(recovered$failed, `[[`, character(1), "kind"),
    "build"
  )
  expect_identical(recovered$failed[[1L]]$build_id, "build-1")
  expect_identical(
    recovered$failed[[1L]]$terminal_reason,
    "worker_restarted"
  )
  expect_identical(recovered$protocol$build_status, "idle")
})

test_that("a deterministic command failure advances without clearing FIFO", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
  protocol <- builder_enqueue(protocol, builder_command("review", "session"))
  dispatched <- builder_protocol_dispatch(protocol)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    builder_worker_response(dispatched$request, error = "invalid settings")
  )

  terminal <- builder_protocol_fail_pending(
    completed$protocol,
    reason = "command_error"
  )

  expect_identical(terminal$failed$kind, "settings")
  expect_identical(terminal$failed$terminal_reason, "command_error")
  expect_identical(terminal$failed$terminal_status, "failed")
  expect_length(terminal$protocol$awaiting_ack, 0L)
  expect_identical(
    builder_protocol_dispatch(terminal$protocol)$request$kind,
    "review"
  )
})

test_that("failing a pending Build opens a new single-flight", {
  protocol <- builder_enqueue(
    builder_request_protocol(epoch = "worker-1"),
    builder_command("build", "session", payload = list(id = "build-1"))
  )
  dispatched <- builder_protocol_dispatch(protocol)

  terminal <- builder_protocol_fail_pending(
    dispatched$protocol,
    reason = "build_error"
  )

  expect_identical(terminal$failed$build_id, "build-1")
  expect_identical(terminal$protocol$build_status, "idle")
  expect_no_error(
    builder_enqueue(
      terminal$protocol,
      builder_command("build", "session", payload = list(id = "build-2"))
    )
  )
})

test_that("forgetting a dataset drops only its work and rebases barriers", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_protocol_dataset(protocol, "a", 2L, "snapshot-a")
  protocol <- builder_protocol_dataset(protocol, "b", 3L, "snapshot-b")
  protocol <- builder_enqueue(protocol, builder_command("settings", "a"))
  protocol <- builder_enqueue(protocol, builder_command("settings", "b"))
  protocol <- builder_enqueue(protocol, builder_command("review", "session"))
  protocol <- builder_enqueue(
    protocol,
    builder_command("build", "session", payload = list(id = "build-1"))
  )

  forgotten <- builder_protocol_forget_dataset(
    protocol,
    "a",
    reason = "drop_succeeded"
  )

  expect_null(forgotten$protocol$datasets[["a"]])
  expect_identical(forgotten$protocol$datasets[["b"]]$revision, 3L)
  expect_identical(
    vapply(forgotten$failed, `[[`, character(1), "kind"),
    "settings"
  )
  expect_identical(
    builder_pending_ids(forgotten$protocol),
    c("settings:b", "review:session", "build:session")
  )
  persistent <- Filter(
    function(request) isTRUE(request$persistent),
    forgotten$protocol$queue
  )
  expect_identical(
    vapply(persistent, `[[`, integer(1), "persistent_sequence"),
    1:3
  )
  expect_identical(vapply(persistent, `[[`, integer(1), "cutoff"), 0:2)

  settings <- builder_protocol_dispatch(forgotten$protocol)
  settings_done <- builder_protocol_complete(
    settings$protocol,
    builder_worker_response(settings$request, TRUE)
  )
  protocol <- builder_protocol_acknowledge(
    settings_done$protocol,
    settings$request$request_id
  )
  review <- builder_protocol_dispatch(protocol)
  expect_identical(review$request$kind, "review")
  review_done <- builder_protocol_complete(
    review$protocol,
    builder_worker_response(review$request, TRUE)
  )
  protocol <- builder_protocol_acknowledge(
    review_done$protocol,
    review$request$request_id
  )
  expect_identical(builder_protocol_dispatch(protocol)$request$kind, "build")
})

test_that("forgetting a dataset discards its queries and awaiting drop", {
  protocol <- builder_protocol_dataset(
    builder_request_protocol(epoch = "worker-1"),
    "a",
    1L,
    "snapshot-a"
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "a", 1L, slot = "projection")
  )
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "b", 1L, slot = "projection")
  )
  forgotten_queries <- builder_protocol_forget_dataset(protocol, "a")
  expect_identical(
    vapply(forgotten_queries$discarded, `[[`, character(1), "dataset"),
    "a"
  )
  expect_identical(
    builder_pending_ids(forgotten_queries$protocol),
    "preview:b:projection:1"
  )

  drop_protocol <- builder_protocol_dataset(
    builder_request_protocol(epoch = "worker-1"),
    "a",
    1L,
    "snapshot-a"
  )
  drop_protocol <- builder_enqueue(drop_protocol, builder_command("drop", "a"))
  drop_protocol <- builder_enqueue(
    drop_protocol,
    builder_command("settings", "a")
  )
  drop_protocol <- builder_enqueue(
    drop_protocol,
    builder_command("review", "session")
  )
  dispatched <- builder_protocol_dispatch(drop_protocol)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    builder_worker_response(dispatched$request, TRUE)
  )

  forgotten_drop <- builder_protocol_forget_dataset(completed$protocol, "a")
  expect_identical(
    vapply(forgotten_drop$failed, `[[`, character(1), "kind"),
    c("drop", "settings")
  )
  expect_length(forgotten_drop$protocol$awaiting_ack, 0L)
  expect_identical(
    builder_protocol_dispatch(forgotten_drop$protocol)$request$kind,
    "review"
  )
})

.builder_fake_process <- function(state, message = NULL) {
  process <- new.env(parent = emptyenv())
  process$poll_process <- function(timeout) state
  process$read <- function() message
  process
}

.builder_fake_worker <- function(process) {
  structure(
    list(process = process),
    class = c("builder_worker", "list")
  )
}

.builder_test_pid_alive <- function(pid) {
  tryCatch(
    ps::ps_is_running(ps::ps_handle(as.integer(pid))),
    error = function(error) FALSE
  )
}

# On Windows bin/R.exe is a launcher that starts Rterm.exe through cmd.exe,
# so killing its pid leaves the sleeping Rterm.exe behind. Start Rterm.exe
# directly so the recorded pid is the process that has to die.
.builder_test_sleeper_binary <- function() {
  file.path(
    R.home("bin"),
    if (.Platform$OS.type == "windows") "Rterm.exe" else "R"
  )
}

# Registered before the child exists: kills whatever pid the file names
# when the test ends, even when an expectation or the pid wait fails.
.builder_defer_kill_pid_file <- function(path, envir = parent.frame()) {
  withr::defer(
    {
      pid <- if (file.exists(path)) {
        suppressWarnings(as.integer(readLines(path, warn = FALSE)[1L]))
      } else {
        NA_integer_
      }
      if (length(pid) == 1L && !is.na(pid) && .builder_test_pid_alive(pid)) {
        try(tools::pskill(pid, signal = 9L), silent = TRUE)
      }
      unlink(path, force = TRUE)
    },
    envir = envir
  )
}

.builder_wait_for_pid_file <- function(path, timeout = 10) {
  deadline <- Sys.time() + timeout
  repeat {
    if (file.exists(path)) {
      value <- suppressWarnings(as.integer(readLines(path, warn = FALSE)[1L]))
      if (length(value) == 1L && !is.na(value)) {
        return(value)
      }
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the Builder child process.")
    }
    Sys.sleep(0.01)
  }
}

builder_worker_fixture <- function() {
  root <- tempfile("builder-main-snapshots-")
  if (!dir.create(root, mode = "0700")) {
    stop("Could not create the worker fixture root.")
  }
  snapshot <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-a"),
    available_bytes = 2^40
  )
  list(
    root = root,
    registry = list("dataset-a" = snapshot),
    snapshot = snapshot
  )
}

test_that("session startup returns before a slow worker bootstrap completes", {
  skip_if_not_installed("callr")
  gate <- tempfile("builder-worker-startup-gate-")
  withr::defer(unlink(gate, force = TRUE))

  started_at <- system.time({
    started <- builder_session_start(
      builder_profile_inst_path("builder"),
      .async = TRUE,
      .bootstrap = local({
        gate_path <- gate
        function(...) {
          while (!file.exists(gate_path)) {
            Sys.sleep(0.01)
          }
          character()
        }
      })
    )
  })[["elapsed"]]
  worker <- started$worker
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  expect_null(started$error)
  ## The gate keeps the bootstrap from finishing, so "starting" below is what
  ## shows that start did not wait for it; this bound only catches a hang.
  expect_lt(started_at, 20)
  expect_identical(worker$state, "starting")
  expect_false(worker$ready)

  file.create(gate)
  deadline <- Sys.time() + 60
  repeat {
    startup <- builder_session_poll_startup(worker)
    worker <- startup$worker
    if (!identical(startup$state, "starting")) {
      break
    }
    if (Sys.time() >= deadline) {
      stop("The gated Builder worker did not finish startup.", call. = FALSE)
    }
    Sys.sleep(0.01)
  }

  expect_identical(startup$state, "ready")
  expect_true(worker$ready)
})

test_that("copied Builder runtimes accept an explicit development source", {
  copied_runtime <- withr::local_tempdir()
  copied_builder <- file.path(copied_runtime, "builder")
  fs::dir_copy(builder_profile_inst_path("builder"), copied_builder)
  source_root <- withr::local_tempdir()
  file.copy(builder_test_description_path(), source_root)
  dir.create(file.path(source_root, "R"))
  writeLines(
    "# Source-tree identity fixture",
    file.path(source_root, "R", "fixture.R")
  )
  source_root <- normalizePath(source_root, winslash = "/", mustWork = TRUE)
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = source_root)

  expect_identical(
    .builder_worker_package_source(copied_builder),
    source_root
  )
})

test_that("main registry trusts the worker MD5 instead of rehashing", {
  fixture <- builder_worker_fixture()
  withr::defer({
    .builder_snapshot_release(fixture$snapshot)
    unlink(fixture$root, recursive = TRUE, force = TRUE)
  })
  worker <- structure(
    list(
      snapshot_root = fixture$root,
      snapshot_registry = list()
    ),
    class = "builder_worker"
  )
  digests <- 0L
  testthat::local_mocked_bindings(
    md5sum = function(files) {
      digests <<- digests + 1L
      stats::setNames(rep(strrep("0", 32L), length(files)), files)
    },
    .package = "tools"
  )

  registered <- builder_worker_register_snapshot(
    worker,
    "dataset-a",
    fixture$snapshot
  )

  expect_identical(digests, 0L)
  expect_identical(
    registered$snapshot_registry[["dataset-a"]],
    fixture$snapshot
  )

  marker <- .builder_snapshot_marker_path(fixture$snapshot$path)
  changed <- readRDS(marker)
  changed$owner_token <- paste0(changed$owner_token, "-other")
  saveRDS(changed, marker)
  expect_error(
    builder_worker_register_snapshot(worker, "dataset-a", fixture$snapshot),
    "owned immutable snapshot"
  )
})

test_that("dead workers reopen main-owned immutable snapshots", {
  skip_if_not_installed("callr")
  fixture <- builder_worker_fixture()
  withr::defer(.builder_snapshot_release(fixture$snapshot))
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = fixture$root,
    snapshot_registry = fixture$registry
  )
  withr::defer(try(worker$process$close(), silent = TRUE))
  expect_true(worker$ready)
  expect_equal(builder_worker_cell_count(worker, "dataset-a"), 80L)
  old_epoch <- worker$epoch
  old_process <- worker$process

  restarted <- builder_worker_restart(worker)
  withr::defer(try(restarted$process$close(), silent = TRUE))

  expect_false(old_process$is_alive())
  expect_true(restarted$ready)
  expect_false(identical(restarted$epoch, old_epoch))
  expect_identical(
    restarted$snapshot_root,
    normalizePath(fixture$root, mustWork = TRUE)
  )
  expect_identical(restarted$snapshot_registry, fixture$registry)
  expect_equal(builder_worker_cell_count(restarted, "dataset-a"), 80L)
  expect_true(.builder_snapshot_owned(fixture$snapshot))
})

test_that("a zero object budget keeps one decoded dataset resident", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list(),
    .object_budget = 0
  )
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  residency <- worker$process$run(
    function(object) {
      ids <- paste0("dataset-", letters[1:3])
      for (index in seq_along(ids)) {
        object@project.name <- ids[[index]]
        .builder_register_adapter(
          builder_example_adapter(ids[[index]], object),
          ids[[index]]
        )
      }
      objects <- get(".builder_objects", envir = globalenv())
      snapshots <- get(".builder_snapshots", envir = globalenv())
      list(
        resident = ls(objects, all.names = TRUE),
        registered = ls(snapshots, all.names = TRUE),
        reopened = as.integer(ncol(.builder_worker_object(ids[[1L]]))),
        resident_after_reopen = ls(objects, all.names = TRUE)
      )
    },
    args = list(object = SeuratObject::pbmc_small)
  )

  expect_identical(residency$resident, "dataset-c")
  expect_identical(residency$registered, paste0("dataset-", letters[1:3]))
  expect_identical(residency$reopened, 80L)
  expect_identical(residency$resident_after_reopen, "dataset-a")
})

test_that("an object budget keeps recently used datasets resident", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list(),
    .object_budget = 2^40
  )
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  residency <- worker$process$run(
    function(object) {
      ids <- paste0("dataset-", letters[1:3])
      for (index in seq_along(ids)) {
        object@project.name <- ids[[index]]
        .builder_register_adapter(
          builder_example_adapter(ids[[index]], object),
          ids[[index]]
        )
      }
      counter <- new.env(parent = emptyenv())
      counter$opened <- 0L
      original <- get(".builder_open_snapshot", envir = globalenv())
      assign(
        ".builder_open_snapshot",
        function(...) {
          counter$opened <- counter$opened + 1L
          original(...)
        },
        envir = globalenv()
      )
      on.exit(assign(".builder_open_snapshot", original, envir = globalenv()))
      objects <- get(".builder_objects", envir = globalenv())
      first <- as.integer(ncol(.builder_worker_object(ids[[1L]])))
      second <- as.integer(ncol(.builder_worker_object(ids[[2L]])))
      resident <- ls(objects, all.names = TRUE)
      rm(list = ids[[1L]], envir = objects)
      reopened <- as.integer(ncol(.builder_worker_object(ids[[1L]])))
      list(
        budget = get(".builder_object_budget", envir = globalenv()),
        resident = resident,
        cells = c(first, second, reopened),
        opened = counter$opened
      )
    },
    args = list(object = SeuratObject::pbmc_small)
  )

  expect_identical(residency$budget, 2^40)
  expect_identical(residency$resident, paste0("dataset-", letters[1:3]))
  expect_identical(residency$cells, c(80L, 80L, 80L))
  # Only the evicted dataset was reopened from its snapshot.
  expect_identical(residency$opened, 1L)
})

test_that("the worker object budget defaults to a quarter of memory", {
  expect_identical(builder_worker_object_budget(0), 0)
  expect_identical(builder_worker_object_budget(5e9), 5e9)
  expect_identical(
    builder_worker_object_budget(NULL, .total_memory = function() 16e9),
    4e9
  )
  expect_identical(
    builder_worker_object_budget(-1, .total_memory = function() 16e9),
    4e9
  )
  expect_identical(
    builder_worker_object_budget(
      NULL,
      .total_memory = function() stop("unavailable")
    ),
    2 * 1024^3
  )
  withr::local_options(CerebroNexus.builder.worker_object_budget = 123)
  expect_identical(builder_worker_object_budget(), 123)
})

test_that("orphan cleanup retains strict state checks for every snapshot format", {
  root <- withr::local_tempdir()
  snapshots <- lapply(c("rds", "qs", "qs2"), function(serialization) {
    snapshot <- builder_snapshot_seurat(
      SeuratObject::pbmc_small,
      file.path(root, paste0("dataset-", serialization)),
      available_bytes = 2^40
    )
    marker_path <- .builder_snapshot_marker_path(snapshot$path)
    marker <- readRDS(marker_path)
    marker$serialization <- serialization
    saveRDS(marker, marker_path)
    snapshot
  })
  tampered <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-tampered-state"),
    available_bytes = 2^40
  )
  marker_path <- .builder_snapshot_marker_path(tampered$path)
  marker <- readRDS(marker_path)
  marker$object_state$bytes <- marker$object_state$bytes + 1
  saveRDS(marker, marker_path)

  report <- .builder_worker_cleanup_orphans(list(
    snapshot_root = root,
    snapshot_registry = list()
  ))

  expect_false(any(vapply(
    snapshots,
    function(snapshot) {
      dir.exists(snapshot$path)
    },
    logical(1)
  )))
  expect_setequal(report$snapshots, paste0("dataset-", c("rds", "qs", "qs2")))
  expect_true(dir.exists(tampered$path))
  expect_identical(report$errors, "dataset-tampered-state")
})

test_that("restart failure is typed and leaves no running old process", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  snapshot <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-a"),
    available_bytes = 2^40
  )
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list("dataset-a" = snapshot)
  )
  old_process <- worker$process
  unlink(snapshot$object_file)

  failed <- builder_worker_restart(worker)

  expect_s3_class(failed, "builder_worker_restart_failed")
  expect_identical(failed$event, "restart_failed")
  expect_match(
    failed$error,
    "snapshot is no longer owned",
    ignore.case = TRUE
  )
  expect_false(failed$worker$ready)
  expect_false(old_process$is_alive())
})

test_that("poll restarts a closed worker without reading its closed pipe", {
  skip_if_not_installed("callr")
  fixture <- builder_worker_fixture()
  withr::defer(.builder_snapshot_release(fixture$snapshot))
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = fixture$root,
    snapshot_registry = fixture$registry
  )
  worker$process$close()

  started <- Sys.time()
  polled <- builder_worker_poll(worker, timeout = 0)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  withr::defer(try(polled$worker$process$kill_tree(), silent = TRUE))

  expect_identical(polled$event, "restarted")
  # The replacement re-sources its runtime in the background.
  expect_identical(polled$worker$state, "starting")
  expect_false(polled$worker$ready)
  expect_lt(elapsed, 10)
  restarted <- polled$worker
  deadline <- Sys.time() + 60
  repeat {
    startup <- builder_worker_poll_startup(restarted, timeout = 100L)
    restarted <- startup$worker
    if (!identical(startup$state, "starting") || Sys.time() > deadline) {
      break
    }
  }
  expect_identical(startup$state, "ready")
  expect_equal(builder_worker_cell_count(restarted, "dataset-a"), 80L)
})

test_that("a callr read failure is one completed typed error", {
  process <- .builder_fake_process("ready")
  process$read <- function() stop("closed response channel")
  worker <- .builder_fake_worker(process)

  first <- builder_worker_poll(worker, timeout = 0)

  expect_identical(first$event, "completed")
  expect_true(first$result$done)
  expect_match(first$result$error, "could not be read")
})

test_that("worker completion preserves bounded callr transport diagnostics", {
  condition <- simpleError(
    "remote transport failure",
    call = quote(worker_failure())
  )
  response <- list(state = "failure", error = "build failed")
  process <- .builder_fake_process(
    "ready",
    list(
      code = 200L,
      result = response,
      stdout = strrep("o", .builder_worker_transport_output_limit + 32L),
      stderr = "worker stderr sentinel\n",
      error = condition
    )
  )

  completed <- builder_worker_poll(.builder_fake_worker(process), timeout = 0)
  transport <- completed$result$transport

  expect_identical(completed$event, "completed")
  expect_identical(completed$result$error, "remote transport failure")
  expect_identical(transport$response, response)
  expect_true(transport$stdout_truncated)
  expect_lte(
    nchar(transport$stdout, type = "bytes"),
    .builder_worker_transport_output_limit
  )
  expect_identical(transport$stderr, "worker stderr sentinel\n")
  expect_s3_class(transport$error_condition, "simpleError")
  expect_identical(
    deparse(conditionCall(transport$error_condition)),
    "worker_failure()"
  )
})

test_that("worker stop timeout conversion never overflows", {
  deadline <- Sys.time() + (.Machine$integer.max / 1000) + 10

  expect_identical(
    .builder_worker_remaining_ms(deadline),
    .Machine$integer.max
  )
})

test_that("worker stop confirms process death before returning", {
  skip_if_not_installed("callr")
  worker <- builder_worker_start(builder_profile_inst_path("builder"))
  root <- worker$snapshot_root
  old_process <- worker$process

  stopped <- builder_worker_stop(worker, grace_ms = 9000L)
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  expect_true(stopped$stopped)
  expect_null(stopped$error)
  expect_false(old_process$is_alive())
})

test_that("worker interrupts use CTRL+BREAK on Windows", {
  calls <- character()
  process <- list(
    get_pid = function() 4242L,
    interrupt = function() {
      calls <<- c(calls, "interrupt")
      TRUE
    }
  )
  .builder_worker_interrupt(
    process,
    os_type = "windows",
    .ctrl_break = function(pid) calls <<- c(calls, paste("break", pid))
  )
  expect_identical(calls, "break 4242")

  calls <- character()
  .builder_worker_interrupt(
    process,
    os_type = "windows",
    .ctrl_break = function(pid) stop("no console")
  )
  expect_identical(calls, "interrupt")

  calls <- character()
  .builder_worker_interrupt(process, os_type = "unix")
  expect_identical(calls, "interrupt")
})

test_that("dead-parent restart kills a lost child before snapshot cleanup", {
  skip_if_not_installed("callr")
  skip_if_not_installed("processx")
  skip_if_not_installed("ps")
  root <- withr::local_tempdir()
  pid_file <- tempfile("builder-orphan-child-pid-")
  .builder_defer_kill_pid_file(pid_file)
  registered <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-registered"),
    available_bytes = 2^40
  )
  orphan <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-orphan"),
    available_bytes = 2^40
  )
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list("dataset-a" = registered)
  )
  worker$process$call(
    function(path, r_binary) {
      child <- processx::process$new(
        r_binary,
        c("--vanilla", "--slave", "-e", "repeat { Sys.sleep(1) }"),
        cleanup = FALSE,
        cleanup_tree = FALSE
      )
      writeLines(as.character(child$get_pid()), path)
      quit(save = "no", status = 17L, runLast = FALSE)
    },
    args = list(path = pid_file, r_binary = .builder_test_sleeper_binary())
  )
  child_pid <- .builder_wait_for_pid_file(pid_file)
  child_handle <- ps::ps_handle(child_pid)
  withr::defer({
    if (
      tryCatch(ps::ps_is_running(child_handle), error = function(error) FALSE)
    ) {
      try(tools::pskill(child_pid, signal = 9L), silent = TRUE)
    }
  })
  deadline <- Sys.time() + 10
  while (worker$process$is_alive() && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_false(worker$process$is_alive())
  expect_true(ps::ps_is_running(child_handle))

  restarted <- builder_worker_restart(worker)
  withr::defer({
    if (restarted$process$is_alive()) {
      try(restarted$process$kill_tree(), silent = TRUE)
    }
  })

  expect_s3_class(restarted, "builder_worker")
  expect_true(restarted$cleanup_safe)
  expect_length(restarted$lost_trees, 0L)
  expect_false(.builder_test_pid_alive(child_pid))
  expect_identical(restarted$cleanup$snapshots, "dataset-orphan")
  expect_true(dir.exists(registered$path))
  expect_false(dir.exists(orphan$path))
  expect_equal(builder_worker_cell_count(restarted, "dataset-a"), 80L)

  stopped <- builder_worker_stop(restarted)

  expect_true(stopped$stopped)
  expect_true(stopped$worker$cleanup_safe)
  expect_true(dir.exists(registered$path))
})

test_that("an untraceable crashed worker keeps the snapshot root", {
  process <- .builder_fake_process("closed")
  process$is_alive <- function() FALSE
  worker <- .builder_fake_worker(process)

  stopped <- builder_worker_stop(worker)

  expect_false(stopped$stopped)
  expect_true(stopped$restart_allowed)
  expect_true(stopped$quarantined)
  expect_false(stopped$worker$cleanup_safe)
  expect_identical(stopped$worker$lost_trees, NA_character_)
  expect_match(stopped$worker$quarantine_reason, "cannot be traced")
  settled <- .builder_worker_settle_lost(stopped$worker, Sys.time() + 1)
  expect_false(settled$cleanup_safe)
})

test_that("a quarantine ends once the lost processes are gone", {
  skip_if_not_installed("processx")
  skip_if_not_installed("ps")
  marker <- paste0("PSbuildertest", Sys.getpid(), "_", sample.int(1e6, 1L))
  sleeper <- processx::process$new(
    .builder_test_sleeper_binary(),
    c("--vanilla", "--slave", "-e", "repeat { Sys.sleep(1) }"),
    env = stats::setNames(c("current", "YES"), c("", marker))
  )
  withr::defer(try(sleeper$kill(), silent = TRUE))
  worker <- .builder_fake_worker(.builder_fake_process("closed"))
  worker$cleanup_safe <- FALSE
  worker$lost_trees <- marker

  settled <- .builder_worker_settle_lost(worker, Sys.time() + 5)

  expect_false(sleeper$is_alive())
  expect_true(settled$cleanup_safe)
  expect_length(settled$lost_trees, 0L)
  expect_null(settled$quarantine_reason)
})

test_that("worker stop kills descendants before snapshot cleanup", {
  skip_if_not_installed("callr")
  skip_if_not_installed("processx")
  skip_if_not_installed("ps")
  root <- withr::local_tempdir()
  pid_file <- tempfile("builder-child-pid-")
  .builder_defer_kill_pid_file(pid_file)
  orphan <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-orphan"),
    available_bytes = 2^40
  )
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list()
  )
  parent_process <- worker$process
  worker$process$call(
    function(path, r_binary) {
      child <- processx::process$new(
        r_binary,
        c("--vanilla", "--slave", "-e", "repeat { Sys.sleep(1) }"),
        stdout = "|",
        stderr = "|",
        cleanup = FALSE,
        cleanup_tree = FALSE
      )
      writeLines(as.character(child$get_pid()), path)
      repeat {
        Sys.sleep(0.05)
      }
    },
    args = list(path = pid_file, r_binary = .builder_test_sleeper_binary())
  )
  child_pid <- .builder_wait_for_pid_file(pid_file)
  withr::defer({
    if (.builder_test_pid_alive(child_pid)) {
      try(tools::pskill(child_pid, signal = 9L), silent = TRUE)
    }
  })
  expect_true(.builder_test_pid_alive(child_pid))

  cleanup_observation <- new.env(parent = emptyenv())
  cleanup_observation$child_alive <- NA
  original_cleanup_hook <- .builder_cleanup_after_check
  assign(
    ".builder_cleanup_after_check",
    function(path) {
      cleanup_observation$child_alive <- .builder_test_pid_alive(child_pid)
      invisible(path)
    },
    envir = globalenv()
  )
  withr::defer(assign(
    ".builder_cleanup_after_check",
    original_cleanup_hook,
    envir = globalenv()
  ))

  stopped <- builder_worker_stop(worker)

  expect_true(stopped$stopped)
  expect_false(parent_process$is_alive())
  expect_false(.builder_test_pid_alive(child_pid))
  expect_identical(cleanup_observation$child_alive, FALSE)
  expect_false(dir.exists(orphan$path))
})

test_that("worker capabilities load once and preserve failures", {
  loads <- 0L
  failed_loads <- 0L
  registry <- builder_worker_capability_registry(list(
    analysis = function() {
      loads <<- loads + 1L
      invisible(TRUE)
    },
    build = function() {
      failed_loads <<- failed_loads + 1L
      stop("broken build loader")
    }
  ))

  expect_identical(builder_worker_ensure_capability(registry, "analysis"), TRUE)
  expect_identical(builder_worker_ensure_capability(registry, "analysis"), TRUE)
  expect_identical(loads, 1L)
  expect_error(
    builder_worker_ensure_capability(registry, "build"),
    "broken build loader"
  )
  expect_identical(registry$states[["build"]], "failed")
  expect_error(
    builder_worker_ensure_capability(registry, "build"),
    "broken build loader"
  )
  expect_identical(failed_loads, 1L)
})

test_that("authentication creation failure leaves protocol ready for another build", {
  first <- builder_command(
    "build",
    "session",
    payload = list(
      kind = "build",
      auth_accounts = list(list(password = "lifecycle-secret-4f61"))
    )
  )
  protocol <- builder_enqueue(builder_request_protocol("auth-lifecycle"), first)
  sent <- builder_protocol_dispatch(protocol)
  safe_protocol <- builder_protocol_redact_auth(sent$protocol)
  safe_request <- builder_request_redact_auth(sent$request)
  completed <- builder_protocol_complete(
    safe_protocol,
    builder_worker_response(
      safe_request,
      value = list(error = "Authentication setup failed.")
    )
  )
  terminal <- builder_protocol_acknowledge(
    completed$protocol,
    safe_request$request_id
  )

  expect_identical(terminal$build_status, "idle")
  expect_null(terminal$pending)
  expect_length(terminal$awaiting_ack, 0L)
  second <- builder_command(
    "build",
    "session",
    payload = list(kind = "build", auth_accounts = list())
  )
  requeued <- builder_enqueue(terminal, second)
  expect_identical(requeued$build_status, "queued")
  expect_false(builder_auth_raw_contains(
    serialize(requeued, NULL),
    "lifecycle-secret-4f61"
  ))
})

test_that("a stale persistent response has an explicit acknowledged terminal", {
  protocol <- builder_request_protocol(epoch = "worker-1")
  protocol <- builder_protocol_dataset(
    protocol,
    "a",
    1L,
    "snapshot-a"
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "alignment",
      "a",
      revision = 1L,
      snapshot_identity = "snapshot-a"
    )
  )
  sent <- builder_protocol_dispatch(protocol)
  protocol <- builder_protocol_dataset(
    sent$protocol,
    "a",
    2L,
    "snapshot-a"
  )
  response <- builder_worker_response(sent$request, TRUE)
  completed <- builder_protocol_complete(protocol, response)

  expect_false(completed$accepted)
  expect_null(completed$protocol$pending)
  expect_length(completed$protocol$awaiting_ack, 1L)
  terminal <- builder_protocol_acknowledge(
    completed$protocol,
    sent$request$request_id
  )
  expect_length(terminal$awaiting_ack, 0L)
  expect_identical(builder_protocol_dispatch(terminal)$blocked, "empty")
})

.builder_wait_for_session <- function(worker, timeout = 30) {
  deadline <- Sys.time() + timeout
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      return(list(worker = worker, result = polled$result))
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the Builder worker.")
    }
  }
}

test_that("load returns a descriptor and drop releases only after main ACK", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  source <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, source)
  started <- builder_session_start(
    builder_profile_inst_path("builder"),
    snapshot_root = file.path(root, "snapshots"),
    snapshot_registry = list()
  )
  worker <- started$worker
  withr::defer(try(worker$process$close(), silent = TRUE))

  load_protocol <- builder_request_protocol(epoch = worker$epoch)
  load_protocol <- builder_enqueue(
    load_protocol,
    builder_command("load", "dataset-a")
  )
  sent <- builder_protocol_dispatch(load_protocol)
  builder_session_load(worker, "dataset-a", source, sent$request)
  loaded <- .builder_wait_for_session(worker)
  worker <- loaded$worker
  completed <- builder_protocol_complete(
    sent$protocol,
    loaded$result$value
  )
  expect_true(completed$accepted)
  snapshot <- completed$value$snapshot
  expect_true(.builder_snapshot_owned(snapshot))
  worker <- builder_worker_register_snapshot(worker, "dataset-a", snapshot)
  completed$protocol <- builder_protocol_acknowledge(
    completed$protocol,
    sent$request$request_id
  )
  unlink(source)

  drop_protocol <- builder_request_protocol(epoch = worker$epoch)
  identity <- .builder_worker_identity(snapshot)
  drop_protocol <- builder_enqueue(
    drop_protocol,
    builder_command(
      "drop",
      "dataset-a",
      snapshot_identity = identity
    )
  )
  dropped_request <- builder_protocol_dispatch(drop_protocol)
  builder_session_drop(worker, "dataset-a", dropped_request$request)
  dropped <- .builder_wait_for_session(worker)
  worker <- dropped$worker
  completed_drop <- builder_protocol_complete(
    dropped_request$protocol,
    dropped$result$value
  )

  expect_true(completed_drop$accepted)
  expect_true(.builder_snapshot_owned(snapshot))
  expect_true(dir.exists(snapshot$path))
  worker <- builder_worker_release_snapshot(
    worker,
    "dataset-a",
    expected_identity = identity
  )
  completed_drop$protocol <- builder_protocol_acknowledge(
    completed_drop$protocol,
    dropped_request$request$request_id
  )

  expect_true(dir.exists(snapshot$path))
  expect_null(worker$snapshot_registry[["dataset-a"]])
  expect_length(completed_drop$protocol$awaiting_ack, 0L)
})

test_that("worker tree inspection finds marked descendants without a ppid scan", {
  skip_if_not_installed("callr")
  skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  process <- callr::r_session$new()
  withr::defer(try(process$kill_tree(), silent = TRUE))
  marker <- .builder_worker_tree_marker(process)
  expect_match(marker, "^PS[0-9A-Za-z]+_[0-9]+$")
  process$call(function() {
    child <- processx::process$new(
      file.path(R.home("bin"), "Rscript"),
      c("-e", "Sys.sleep(60)")
    )
    Sys.sleep(60)
  })
  # Wait for the session and its child to carry the marker. Count processes
  # rather than match names: R processes are named "R" on Linux.
  deadline <- Sys.time() + 20
  while (length(ps::ps_find_tree(marker)) < 2L && Sys.time() <= deadline) {
    Sys.sleep(0.1)
  }
  # ps_children() opens every process on the machine (~40 s on Windows); the
  # marker scan must be the only descendant lookup for a processx worker.
  testthat::local_mocked_bindings(
    ps_children = function(...) stop("unexpected machine-wide ppid scan"),
    .package = "ps"
  )
  started <- Sys.time()
  tree <- .builder_worker_tree_handles(process)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  expect_null(tree$error)
  expect_lt(elapsed, 5)
  pids <- vapply(tree$handles, ps::ps_pid, integer(1))
  expect_identical(pids[[1L]], process$get_pid())
  expect_false(anyDuplicated(pids) > 0L)
  expect_gte(length(pids), 2L)
  expect_false(Sys.getpid() %in% pids)
})

test_that("a verified stop of a busy worker settles in seconds", {
  skip_if_not_installed("callr")
  skip_if_not_installed("ps")
  skip_if_not(ps::ps_is_supported())
  root <- withr::local_tempdir()
  process <- callr::r_session$new()
  withr::defer(try(process$kill_tree(), silent = TRUE))
  process$call(function() {
    processx::process$new(
      file.path(R.home("bin"), "Rscript"),
      c("-e", "Sys.sleep(60)")
    )
    Sys.sleep(60)
  })
  Sys.sleep(1)
  marker <- .builder_worker_tree_marker(process)
  worker <- structure(
    list(
      process = process,
      state = "ready",
      snapshot_root = root,
      snapshot_registry = list()
    ),
    class = c("builder_worker", "list")
  )

  started <- Sys.time()
  stopped <- builder_worker_stop(worker, grace_ms = 500L)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))

  expect_true(stopped$stopped)
  expect_true(stopped$tree_verified)
  expect_lt(elapsed, 15)
  expect_false(process$is_alive())
  alive <- vapply(
    ps::ps_find_tree(marker),
    function(handle) isTRUE(ps::ps_is_running(handle)),
    logical(1)
  )
  expect_false(any(alive))
})

test_that("worker stall timeouts default per call type and can be set", {
  withr::local_options(CerebroNexus.builder.worker_stall_timeout = NULL)
  withr::local_envvar(CEREBRO_BUILDER_WORKER_STALL_TIMEOUT = NA)
  expect_identical(builder_worker_stall_timeout(list(persistent = FALSE)), 120)
  expect_identical(builder_worker_stall_timeout(list(persistent = TRUE)), 600)
  withr::local_envvar(CEREBRO_BUILDER_WORKER_STALL_TIMEOUT = "3")
  expect_identical(builder_worker_stall_timeout(list(persistent = TRUE)), 3)
  withr::local_options(CerebroNexus.builder.worker_stall_timeout = 0.5)
  expect_identical(builder_worker_stall_timeout(list(persistent = FALSE)), 0.5)
})

test_that("the watchdog flags a call only after it stops using CPU", {
  cpu <- 1
  watch <- function(state, now, request_id = "r1", timeout = 10) {
    builder_worker_watchdog(
      state,
      list(process = NULL),
      request_id,
      timeout,
      now = now,
      .cpu = function(process) cpu
    )
  }
  step <- watch(NULL, 0)
  for (now in 1:9) {
    cpu <- cpu + 0.01
    step <- watch(step$state, now)
    expect_false(step$stalled)
  }
  for (now in 10:18) {
    step <- watch(step$state, now)
    expect_false(step$stalled)
  }
  step <- watch(step$state, 19)
  expect_true(step$stalled)

  # A new call, a long pause of this process, or timeout 0 starts over.
  expect_false(watch(step$state, 20, request_id = "r2")$stalled)
  expect_false(watch(step$state, 40)$stalled)
  expect_false(watch(step$state, 20, timeout = 0)$stalled)
  # CPU that cannot be measured never counts as a stall.
  cpu <- NA_real_
  step <- watch(NULL, 0)
  for (now in 1:30) {
    step <- watch(step$state, now)
  }
  expect_false(step$stalled)
})

test_that("a worker that sleeps forever is seen as stalled", {
  skip_if_not_installed("callr")
  skip_if_not_installed("ps")
  worker <- builder_worker_start(builder_profile_inst_path("builder"))
  withr::defer(try(builder_worker_stop(worker, grace_ms = 0L), silent = TRUE))
  worker$process$call(function() Sys.sleep(600))

  step <- builder_worker_watchdog(NULL, worker, "sleep", 1)
  deadline <- Sys.time() + 10
  while (!isTRUE(step$stalled) && Sys.time() < deadline) {
    Sys.sleep(0.1)
    step <- builder_worker_watchdog(step$state, worker, "sleep", 1)
  }
  expect_true(step$stalled)
  expect_false(is.na(step$state$cpu))
})

test_that("a busy worker is not seen as stalled", {
  skip_if_not_installed("callr")
  skip_if_not_installed("ps")
  worker <- builder_worker_start(builder_profile_inst_path("builder"))
  withr::defer(try(builder_worker_stop(worker, grace_ms = 0L), silent = TRUE))
  worker$process$call(function() {
    repeat {
      sum(stats::runif(1e5))
    }
  })

  step <- builder_worker_watchdog(NULL, worker, "busy", 1)
  deadline <- Sys.time() + 3
  while (Sys.time() < deadline) {
    Sys.sleep(0.1)
    step <- builder_worker_watchdog(step$state, worker, "busy", 1)
    expect_false(step$stalled)
  }
})

test_that("a callr startup error is shown by its cause", {
  raw <- paste(
    "! in callr subprocess.",
    "Caused by error in `(function (dir, root, registry) {  ... })()`:",
    "! A registered Builder snapshot is no longer owned.",
    sep = "\n"
  )
  expect_identical(
    builder_plain_error_text(raw),
    "A registered Builder snapshot is no longer owned."
  )
  expect_identical(
    builder_plain_error_text("The background worker startup timed out."),
    "The background worker startup timed out."
  )
  expect_identical(builder_plain_error_text("! in callr subprocess."), "! in callr subprocess.")
  expect_null(builder_plain_error_text(NULL))
})
