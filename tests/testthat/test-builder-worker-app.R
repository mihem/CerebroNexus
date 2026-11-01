## -------------------------------------------------------------------------
## Builder worker integration contracts owned by the Shiny main process.
## -------------------------------------------------------------------------

builder_app_source_runtime_prerequisites(globalenv())
builder_profile_source_runtime(globalenv())
builder_stage_contract_source_runtime(globalenv())
builder_repo_source("io.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())
builder_repo_source("state.R", local = globalenv())
builder_runtime_test_env <- new.env(parent = globalenv())
builder_repo_source("coordinator.R", local = builder_runtime_test_env)

builder_app_worker_path <- builder_profile_inst_path("builder", "worker.R")
if (nzchar(builder_app_worker_path) && file.exists(builder_app_worker_path)) {
  sys.source(builder_app_worker_path, envir = globalenv())
}
builder_repo_source("session.R", local = globalenv())

test_that("Builder launcher defaults to unlimited local uploads", {
  expect_identical(
    eval(formals(CerebroNexus::launchCerebroBuilder)$max_file_size),
    Inf
  )
})

test_that("Builder app preserves the launch request-size limit", {
  configured <- as.double(1024^2)
  withr::local_options(list(shiny.maxRequestSize = configured))
  original_encoding <- getOption("encoding")
  original_source <- Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = NA_character_)
  original_search <- search()
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))

  sys.source("app.R", envir = app_env)

  expect_identical(getOption("shiny.maxRequestSize"), configured)
  expect_identical(getOption("encoding"), original_encoding)
  expect_identical(
    Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = NA_character_),
    original_source
  )
  expect_identical(search(), original_search)
})

test_that("Builder restores process settings after its App lifecycle", {
  withr::local_options(list(shiny.maxRequestSize = NULL))
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = NA_character_)
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  expect_true(app_env$builder_app_start(
    list(available = TRUE, reason = NULL),
    .prewarm = function() FALSE
  ))
  expect_identical(getOption("shiny.maxRequestSize"), Inf)
  if (!is.null(app_env$builder_source_package)) {
    expect_identical(
      Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = NA_character_),
      app_env$builder_source_package
    )
  }

  expect_true(app_env$builder_app_restore_process_state())
  expect_null(getOption("shiny.maxRequestSize"))
  expect_true(is.na(Sys.getenv(
    "CEREBRO_PACKAGE_SOURCE",
    unset = NA_character_
  )))
})

test_that("Builder prewarms and hands the first session an isolated worker", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  process <- list(is_alive = function() TRUE)
  candidate <- structure(
    list(process = process, state = "starting", error = NULL),
    class = c("builder_worker", "list")
  )
  started <- NULL
  scheduled <- NULL
  expect_true(app_env$builder_app_prewarm_worker(
    .start = function(builder_dir, .async) {
      started <<- list(builder_dir = builder_dir, async = .async)
      candidate
    },
    .schedule = function(callback, delay) {
      scheduled <<- list(callback = callback, delay = delay)
    }
  ))
  expect_identical(started$builder_dir, app_env$builder_app_directory)
  expect_true(started$async)
  expect_identical(scheduled$delay, 0)
  expect_identical(app_env$builder_app_take_prewarmed_worker(), candidate)
  expect_null(app_env$.builder_app_process_state$prewarmed_worker)
})

test_that("failed Builder prewarm is discarded without blocking startup", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)

  expect_false(app_env$builder_app_prewarm_worker(
    .start = function(...) list(error = "cold-start failure"),
    .schedule = function(...) stop("must not schedule")
  ))
  expect_null(app_env$.builder_app_process_state$prewarmed_worker)
})

test_that("Builder replays ready worker status when the client connects", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this status replay test.")
  }

  shiny::testServer(app_env$server, {
    messages <- list()
    original_send <- session$sendCustomMessage
    session$sendCustomMessage <- function(type, message) {
      if (identical(type, "builder_worker_status")) {
        messages[[length(messages) + 1L]] <<- message
      }
      original_send(type, message)
    }
    worker(structure(
      list(state = "ready", error = NULL),
      class = c("builder_worker", "list")
    ))
    worker_available(TRUE)

    session$setInputs(
      builder_client_connection = list(
        status = "connected",
        nonce = 1
      )
    )
    session$flushReact()

    expect_true(any(vapply(
      messages,
      function(message) identical(message$state, "ready"),
      logical(1)
    )))
  })
})

test_that("Builder runApp restores default process settings on stop", {
  skip_if_not_installed("httpuv")
  withr::local_options(list(shiny.maxRequestSize = NULL))
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = "original-source-root")
  original_search <- search()
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  app <- base::source("app.R", local = app_env)$value
  app_env$builder_runtime_capability <- function(...) {
    list(available = TRUE, reason = NULL)
  }
  app_env$builder_app_prewarm_worker <- function(...) FALSE
  during <- NULL
  later::later(
    function() {
      during <<- list(
        request_size = getOption("shiny.maxRequestSize"),
        source_package = Sys.getenv(
          "CEREBRO_PACKAGE_SOURCE",
          unset = NA_character_
        )
      )
      shiny::stopApp()
    },
    delay = 0.1
  )

  shiny::runApp(
    app,
    host = "127.0.0.1",
    port = httpuv::randomPort(),
    launch.browser = FALSE,
    quiet = TRUE
  )

  expect_identical(during$request_size, Inf)
  expect_identical(
    during$source_package,
    app_env$builder_source_package %||% "original-source-root"
  )
  expect_null(getOption("shiny.maxRequestSize"))
  expect_identical(
    Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = NA_character_),
    "original-source-root"
  )
  expect_identical(search(), original_search)
})

test_that("a missing align-all fails once and later FIFO work proceeds", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list()
  )
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))

  protocol <- builder_request_protocol(worker$epoch)
  protocol <- builder_enqueue(
    protocol,
    builder_command(
      "align_all",
      "missing",
      payload = list(kind = "align_all")
    )
  )
  protocol <- builder_enqueue(
    protocol,
    builder_command("drop", "missing", payload = list(kind = "drop"))
  )
  dispatched <- builder_protocol_dispatch(protocol)
  expect_identical(dispatched$request$kind, "align_all")

  builder_session_section_bounds(
    worker,
    "missing",
    sections = "section-a",
    mode = "pixels",
    extent_width = 100,
    extent_height = 100,
    request = dispatched$request
  )
  deadline <- Sys.time() + 10
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      break
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for align-all failure.", call. = FALSE)
    }
  }

  expect_null(polled$result$error)
  for (field in c(
    "epoch",
    "request_id",
    "token",
    "generation",
    "dataset_revision",
    "snapshot_identity"
  )) {
    expect_identical(
      polled$result$value[[field]],
      dispatched$request[[field]],
      info = paste("Typed failure identity field:", field)
    )
  }
  expect_match(polled$result$value$error, "not found", ignore.case = TRUE)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    polled$result$value
  )
  expect_true(completed$accepted)
  expect_match(completed$error, "not found", ignore.case = TRUE)
  protocol <- builder_protocol_acknowledge(
    completed$protocol,
    dispatched$request$request_id
  )

  following <- builder_protocol_dispatch(protocol)
  expect_identical(following$request$kind, "drop")
  expect_identical(following$request$dataset, "missing")
  expect_identical(following$request$epoch, worker$epoch)
})

test_that("a replaced spatial preview response is rejected", {
  protocol <- builder_request_protocol("worker-a")
  protocol <- builder_enqueue(
    protocol,
    builder_query(
      "spatial_preview",
      "dataset-a",
      generation = 1L,
      slot = "spatial_alignment"
    )
  )
  first <- builder_protocol_dispatch(protocol)
  protocol <- builder_enqueue(
    first$protocol,
    builder_query(
      "spatial_preview",
      "dataset-a",
      generation = 2L,
      slot = "spatial_alignment"
    )
  )

  stale <- builder_protocol_complete(
    protocol,
    builder_worker_response(first$request, list(available = TRUE))
  )
  expect_false(stale$accepted)

  current <- builder_protocol_dispatch(stale$protocol)
  expect_identical(current$request$generation, 2L)
  expect_identical(current$request$kind, "spatial_preview")
})

test_that("Build status projection keeps one stable typed host", {
  skip_if_not_installed("shiny")
  withr::local_package("shiny")
  idle <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = builder_request_protocol("worker-idle"),
    note = NULL,
    result = NULL
  )
  no_destination <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = builder_request_protocol("worker-no-destination"),
    note = NULL,
    result = NULL
  )
  choosing <- builder_build_stage_status_model(
    flow = list(stage = "choosing_folder"),
    protocol = NULL,
    note = NULL,
    result = NULL
  )
  checking <- builder_build_stage_status_model(
    flow = list(stage = "checking_folder"),
    protocol = NULL,
    note = NULL,
    result = NULL
  )
  preparing <- builder_build_stage_status_model(
    flow = list(stage = "preparing"),
    protocol = builder_request_protocol("worker-preparing"),
    note = NULL,
    result = NULL
  )
  queued <- builder_build_stage_status_model(
    flow = list(stage = "building"),
    protocol = list(build_status = "queued"),
    note = NULL,
    result = NULL
  )
  running <- builder_build_stage_status_model(
    flow = list(stage = "building"),
    protocol = list(build_status = "running"),
    note = "Building 3 datasets…",
    result = NULL
  )
  cancelling <- builder_build_stage_status_model(
    flow = list(stage = "building"),
    protocol = list(build_status = "cancelling"),
    note = NULL,
    result = NULL
  )
  conflict <- builder_build_stage_status_model(
    flow = list(stage = "conflict"),
    protocol = list(build_status = "idle"),
    note = NULL,
    result = NULL
  )
  malformed <- builder_build_stage_status_model(
    flow = NULL,
    protocol = list(build_status = NA_character_),
    note = list("not a status note"),
    result = NULL
  )
  incomplete_protocol <- structure(
    list(build_status = "idle"),
    class = c("builder_request_protocol", "list")
  )
  incomplete <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = incomplete_protocol,
    note = "The background worker is not ready.",
    result = NULL
  )
  missing_protocol <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = NULL,
    note = "The background worker is not ready.",
    result = NULL
  )
  pending_protocol <- builder_request_protocol("worker-pending")
  pending_protocol <- builder_enqueue(
    pending_protocol,
    builder_query("preview", "dataset-a", generation = 1L)
  )
  pending <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = pending_protocol,
    note = "Preparing preview…",
    result = NULL
  )
  quiescent <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = builder_request_protocol("worker-ready"),
    note = NULL,
    result = NULL
  )
  finalizing <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = builder_request_protocol("worker-finalizing"),
    note = "Export complete. Saving reusable CRBs to the Builder Project…",
    result = builder_result_success(character()),
    finalizing = TRUE
  )

  expect_named(
    idle,
    c(
      "state",
      "message",
      "pipeline_state",
      "can_build",
      "last_output_parent",
      "result_model"
    ),
    ignore.order = FALSE
  )
  expect_identical(idle$state, "ready")
  expect_true(idle$can_build)
  expect_true(no_destination$can_build)
  expect_identical(finalizing$state, "result")
  expect_null(finalizing$pipeline_state)
  expect_false(finalizing$can_build)
  expect_s3_class(finalizing$result_model, "builder_build_status")
  expect_identical(finalizing$result_model$type, "success")
  expect_identical(choosing$state, "choosing_folder")
  expect_false(choosing$can_build)
  expect_identical(checking$state, "checking_folder")
  expect_identical(
    builder_build_stage_status_label(checking),
    "Checking output folder…"
  )
  expect_false(checking$can_build)
  expect_identical(preparing$state, "preparing")
  expect_identical(
    builder_build_stage_status_label(preparing),
    "Preparing build…"
  )
  expect_false(preparing$can_build)
  expect_identical(queued$state, "queued")
  expect_identical(queued$pipeline_state, "queued")
  expect_false(queued$can_build)
  expect_identical(running$state, "building")
  expect_identical(running$message, "Building 3 datasets…")
  expect_identical(running$pipeline_state, "building")
  expect_false(running$can_build)
  expect_identical(cancelling$state, "building")
  expect_false(cancelling$can_build)
  expect_identical(conflict$state, "ready")
  expect_false(conflict$can_build)
  expect_identical(malformed$state, "ready")
  expect_null(malformed$message)
  expect_false(malformed$can_build)
  expect_false(incomplete$can_build)
  expect_false(missing_protocol$can_build)
  expect_identical(
    missing_protocol$message,
    "The background worker is not ready."
  )
  expect_false(pending$can_build)
  expect_identical(pending$message, "Preparing preview…")
  expect_true(quiescent$can_build)
  render_status <- function(model) {
    htmltools::renderTags(htmltools::tagList(
      builder_build_stage_status_body_ui(model),
      builder_build_stage_primary_action_ui(model)
    ))$html
  }

  results <- list(
    success = builder_result_success(
      published = TRUE,
      built = "/release/dataset.crb",
      app_dir = "/release/cerebro_app",
      app_verified = TRUE,
      report_path = "/release/build-report.json"
    ),
    needs_decision = builder_result_needs_decision(
      "Choose one.",
      retry_closure = "marker_genes",
      failed_dataset_id = "dataset-a"
    ),
    failure = builder_result_failure(
      "Worker stopped.",
      restartable_worker = TRUE
    ),
    recovery_required = builder_release_error_result(
      "Original build failure.",
      "/release",
      .recovery = function(target) {
        list(
          state = "recovery_required",
          target = "/release",
          phase = "old_moved",
          backup = "/.release.cerebro-control/backup",
          backup_exists = TRUE,
          message = "Restore the preserved backup."
        )
      }
    ),
    clear_recovery = builder_result_recovery_required(
      "Clear the interrupted release.",
      recovery = list(
        state = "recovery_required",
        target = "/release",
        phase = "prepared",
        backup = "/.release.cerebro-control/backup",
        backup_exists = FALSE,
        safe_to_abort = TRUE
      )
    ),
    manual_recovery = builder_result_recovery_required(
      "Inspect the interrupted release.",
      recovery = list(
        state = "recovery_required",
        target = "/release",
        phase = "new_published",
        backup = "/.release.cerebro-control/backup",
        backup_exists = FALSE,
        safe_to_abort = FALSE
      )
    )
  )
  result_html <- lapply(results, function(value) {
    model <- builder_build_stage_status_model(
      flow = list(stage = "idle"),
      protocol = list(build_status = "idle"),
      note = NULL,
      result = value
    )
    expect_identical(model$state, "result")
    expect_s3_class(model$result_model, "builder_build_status")
    render_status(model)
  })

  ready_html <- render_status(idle)
  choosing_html <- render_status(choosing)
  queued_html <- render_status(queued)
  running_html <- render_status(running)
  missing_html <- render_status(missing_protocol)
  finalizing_html <- render_status(finalizing)
  all_html <- c(
    list(ready_html, choosing_html, queued_html, running_html, missing_html),
    result_html
  )
  for (html in all_html) {
    expect_false(grepl('id="build-stage-status"', html, fixed = TRUE))
  }
  expect_match(ready_html, ">Build<", fixed = TRUE)
  expect_match(ready_html, "btn btn-action", fixed = TRUE)
  expect_match(
    choosing_html,
    "Choosing a parent folder for the new release…",
    fixed = TRUE
  )
  expect_match(queued_html, "Build queued…", fixed = TRUE)
  expect_match(running_html, "Building 3 datasets…", fixed = TRUE)
  expect_match(finalizing_html, "Build complete", fixed = TRUE)
  expect_match(
    finalizing_html,
    "Export complete. Saving reusable CRBs to the Builder Project…",
    fixed = TRUE
  )
  expect_match(
    missing_html,
    "The background worker is not ready.",
    fixed = TRUE
  )
  expect_match(missing_html, " disabled", fixed = TRUE)
  expect_false(grepl("Launch App", result_html$success, fixed = TRUE))
  expect_match(result_html$success, "Reveal Folder", fixed = TRUE)
  expect_match(result_html$success, "Copy Path", fixed = TRUE)
  expect_match(result_html$success, "Copy Report", fixed = TRUE)
  expect_match(result_html$needs_decision, "Retry optional work", fixed = TRUE)
  expect_match(result_html$needs_decision, "Remove and review", fixed = TRUE)
  expect_false(grepl(
    "remove and rebuild",
    result_html$needs_decision,
    ignore.case = TRUE
  ))
  expect_match(
    result_html$needs_decision,
    "remove it, review, and build again",
    ignore.case = TRUE
  )
  expect_match(result_html$failure, "Restart worker", fixed = TRUE)
  expect_identical(
    results$recovery_required$error,
    "Original build failure."
  )
  expect_identical(
    results$recovery_required$recovery_message,
    "Restore the preserved backup."
  )
  results$recovery_required$diagnostics_path <-
    "/release/.cbr/diagnostics/settlement-build-1"
  results$recovery_required$settlement_error <-
    "The release owner subprocess exited with status 1."
  result_html$recovery_required <- as.character(builder_build_status_ui(
    builder_build_status_model(results$recovery_required)
  ))
  recovery_followup <- builder_result_recovery_followup(
    results$recovery_required,
    "The recovery attempt failed.",
    results$recovery_required$recovery
  )
  expect_identical(recovery_followup$error, "Original build failure.")
  expect_identical(
    recovery_followup$recovery_message,
    "The recovery attempt failed."
  )
  expect_identical(
    recovery_followup$settlement_error,
    "The release owner subprocess exited with status 1."
  )
  expect_match(
    result_html$recovery_required,
    "Original build failure.",
    fixed = TRUE
  )
  expect_match(
    result_html$recovery_required,
    "Restore the preserved backup.",
    fixed = TRUE
  )
  expect_match(
    result_html$recovery_required,
    "/release/.cbr/diagnostics/settlement-build-1",
    fixed = TRUE
  )
  expect_match(
    result_html$recovery_required,
    "The release owner subprocess exited with status 1.",
    fixed = TRUE
  )
  expect_lt(
    regexpr(
      "Original build failure.",
      result_html$recovery_required,
      fixed = TRUE
    ),
    regexpr(
      "Restore the preserved backup.",
      result_html$recovery_required,
      fixed = TRUE
    )
  )
  expect_match(
    result_html$recovery_required,
    "Recover interrupted release",
    fixed = TRUE
  )
  expect_match(
    result_html$recovery_required,
    "Restore previous release",
    fixed = TRUE
  )
  expect_match(result_html$recovery_required, "old_moved", fixed = TRUE)
  expect_match(
    result_html$clear_recovery,
    "Clear interrupted release",
    fixed = TRUE
  )
  expect_match(result_html$clear_recovery, "prepared", fixed = TRUE)
  expect_match(
    result_html$manual_recovery,
    "cannot recover this release automatically",
    fixed = TRUE
  )
  expect_false(grepl(
    'id="recover_builder_release"',
    result_html$manual_recovery,
    fixed = TRUE
  ))
  expect_error(
    render_status(list(state = "future")),
    "Build-stage status is unsupported"
  )
  expect_error(
    builder_build_stage_status_model(
      flow = NULL,
      protocol = NULL,
      note = NULL,
      result = list(error = "legacy")
    ),
    "typed"
  )
})

test_that("dataset mutation lock covers every active build state", {
  idle_protocol <- builder_request_protocol("worker-lock")
  expect_false(builder_mutations_locked(
    list(stage = "idle", plan = NULL),
    idle_protocol
  ))
  for (stage in c(
    "preparing",
    "building",
    "choosing_folder"
  )) {
    expect_true(
      builder_mutations_locked(
        list(stage = stage, plan = NULL),
        idle_protocol
      ),
      info = stage
    )
  }
  for (status in c("queued", "running", "cancelling")) {
    active_protocol <- idle_protocol
    active_protocol$build_status <- status
    expect_true(
      builder_mutations_locked(
        list(stage = "idle", plan = NULL),
        active_protocol
      ),
      info = status
    )
  }
  expect_true(builder_mutations_locked(NULL, idle_protocol))
  expect_true(builder_mutations_locked(
    list(stage = "idle", plan = NULL),
    list(build_status = NA_character_)
  ))
})

test_that("completed preview protocols enable a selected Build", {
  protocol <- builder_request_protocol("worker-completed-preview")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "dataset-a", generation = 1L)
  )
  dispatched <- builder_protocol_dispatch(protocol)
  completed <- builder_protocol_complete(
    dispatched$protocol,
    builder_worker_response(dispatched$request, value = list())
  )$protocol

  expect_false("pending" %in% names(completed))
  model <- builder_build_stage_status_model(
    flow = list(stage = "idle"),
    protocol = completed,
    note = NULL,
    result = NULL
  )
  expect_true(model$can_build)
})

test_that("native output directory selection normalizes selection and preserves cancellation", {
  root <- withr::local_tempdir()
  nested <- file.path(root, "nested", "..", "output")
  dir.create(file.path(root, "nested"))
  dir.create(file.path(root, "output"))

  selected <- builder_native_picker_result("output_directory", function() {
    nested
  })
  cancelled <- builder_native_picker_result("output_directory", function() NULL)
  failed <- builder_native_picker_result("output_directory", function() {
    stop("picker unavailable")
  })

  expect_identical(selected$status, "selected")
  expect_identical(
    selected$path,
    normalizePath(file.path(root, "output"), winslash = "/")
  )
  expect_identical(cancelled, list(status = "cancelled", path = NULL))
  expect_identical(failed$status, "error")
  expect_match(failed$error, "picker unavailable", fixed = TRUE)
})

test_that("native pickers can run without blocking the Shiny process", {
  root <- withr::local_tempdir()
  output <- file.path(root, "output")
  dir.create(output)
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() FALSE
  process$get_exit_status <- function() 0L
  process$read_all_output_lines <- function() output
  process$read_all_error_lines <- function() character()
  process$kill <- function() invisible(TRUE)

  started <- builder_start_native_picker(
    "output_directory",
    .spec = list(command = "picker", args = character()),
    .process_new = function(...) process
  )

  expect_identical(started$kind, "output_directory")
  expect_identical(started$process, process)
  expect_false(builder_native_picker_is_alive(started))
  expect_identical(
    builder_collect_native_picker(started),
    list(
      status = "selected",
      path = normalizePath(output, winslash = "/")
    )
  )
})

test_that("unchanged settings do not write state or advance revisions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this state-only test.")
  }

  shiny::testServer(app_env$server, {
    protocol(builder_request_protocol("worker-a"))
    entry <- list(
      id = "dataset-a",
      revision = 0L,
      snapshot = list(
        path = "/private/dataset-a",
        owner_token = "owner-a",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "current", extras = list()),
      settings = list(name = "Dataset A")
    )
    use_state_only_fixture(list(entry))

    expect_false(replace_entry(entry))
    expect_identical(sets()[[1L]]$revision, 0L)
    expect_null(protocol()$datasets[["dataset-a"]])

    changed <- entry
    changed$settings$name <- "Dataset A renamed"
    changed$profile$marker <- "stale candidate"
    expect_true(replace_entry(changed))
    expect_identical(sets()[[1L]]$revision, 1L)
    expect_identical(sets()[[1L]]$profile$marker, "current")
    expect_identical(
      protocol()$datasets[["dataset-a"]]$revision,
      1L
    )

    expect_false(replace_entry(sets()[[1L]]))
    expect_identical(sets()[[1L]]$revision, 1L)
  })
})

test_that("BuildPlan snapshots must match the worker registry exactly", {
  expect_true(exists(
    ".builder_session_plan_snapshot_error",
    mode = "function",
    inherits = TRUE
  ))
  if (
    !exists(
      ".builder_session_plan_snapshot_error",
      mode = "function",
      inherits = TRUE
    )
  ) {
    return(invisible(NULL))
  }

  snapshot <- list(
    path = "/private/snapshot-a",
    object_file = "/private/snapshot-a/object.rds",
    owner_token = "owner-a",
    created_at = as.POSIXct("2026-08-04 12:00:00", tz = "UTC"),
    object_md5 = strrep("a", 32L),
    closure_bytes = 1024
  )
  identity <- c(
    list(available = TRUE, snapshot = snapshot, source = list(id = "a")),
    snapshot
  )
  plan <- list(
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      source_snapshot_identity = identity
    ))
  )

  expect_null(.builder_session_plan_snapshot_error(
    plan,
    list(`dataset-a` = snapshot)
  ))

  replaced <- snapshot
  replaced$object_md5 <- strrep("b", 32L)
  expect_match(
    .builder_session_plan_snapshot_error(
      plan,
      list(`dataset-a` = replaced)
    ),
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )
  expect_match(
    .builder_session_plan_snapshot_error(plan, list()),
    "missing",
    fixed = TRUE
  )

  unavailable <- plan
  unavailable$items[[1L]]$source_snapshot_identity$available <- FALSE
  expect_match(
    .builder_session_plan_snapshot_error(
      unavailable,
      list(`dataset-a` = snapshot)
    ),
    "does not contain an owned frozen snapshot",
    fixed = TRUE
  )
})

test_that("runtime identity reports the exact parent verification drift", {
  root <- withr::local_tempdir()
  first <- file.path(root, "first.R")
  second <- file.path(root, "second.R")
  writeLines("first <- TRUE", first, useBytes = TRUE)
  writeLines("second <- TRUE", second, useBytes = TRUE)
  expected <- builder_runtime_test_env$builder_runtime_file_identity(c(
    second,
    first
  ))

  expect_true(builder_runtime_test_env$builder_runtime_assert_identity(
    expected,
    builder_runtime_test_env$builder_runtime_file_identity(c(first, second))
  ))

  writeLines("second <- FALSE", second, useBytes = TRUE)
  expect_error(
    builder_runtime_test_env$builder_runtime_assert_identity(
      expected,
      builder_runtime_test_env$builder_runtime_file_identity(c(first, second)),
      context = "parent verification"
    ),
    regexp = paste0(
      "runtime changed before parent verification.*",
      "Nothing was published.*second[.]R"
    )
  )
})

test_that("serialized session build arguments contain paths but no login secrets", {
  username <- "serialized-user-42b8"
  password <- "serialized-password-42b8"
  auth_runtime <- new.env(parent = globalenv())
  builder_repo_source("app_bundle.R", local = auth_runtime)
  validate_payload <- get(
    "builder_auth_validate_payload",
    envir = auth_runtime
  )
  create_material <- get("builder_auth_create_material", envir = auth_runtime)
  read_env_file <- get("builder_auth_read_env_file", envir = auth_runtime)
  validate_material <- get(
    "builder_auth_validate_material",
    envir = auth_runtime
  )
  stage <- withr::local_tempdir()
  accounts <- validate_payload(
    TRUE,
    list(list(
      id = "auth-account-1",
      username = username,
      password = password
    ))
  )$accounts
  material <- create_material(
    accounts,
    stage,
    .random_bytes = function(n) as.raw(rep(12L, n)),
    .create_db = function(credentials_data, sqlite_path, passphrase) {
      writeBin(as.raw(1:8), sqlite_path)
    },
    .capability = function() list(available = TRUE, reason = NULL)
  )
  passphrase <- read_env_file(material$env_file)
  captured_args <- NULL
  captured_callback <- NULL
  process <- list(call = function(callback, args) {
    captured_callback <<- callback
    captured_args <<- args
    invisible(TRUE)
  })
  plan <- list(
    make_app = FALSE,
    app_contract_version = 1L,
    app_auth = list(
      enabled = TRUE,
      account_count = 1L,
      timeout_minutes = 15L
    ),
    items = list()
  )
  request_with_accounts <- builder_command(
    "build",
    "session",
    payload = list(kind = "build", auth_accounts = accounts)
  )
  expect_true(builder_auth_value_contains(request_with_accounts, username))
  expect_true(builder_auth_value_contains(request_with_accounts, password))
  request <- builder_request_redact_auth(request_with_accounts)
  request$build_id <- "serialized-build"
  coordinator <- list(stage = stage, control = dirname(stage))
  original_handle <- get0(
    ".builder_coordinator_handle",
    envir = environment(builder_session_build),
    inherits = TRUE
  )
  assign(
    ".builder_coordinator_handle",
    function(value) value,
    envir = environment(builder_session_build)
  )
  original_validate <- get0(
    "builder_auth_validate_material",
    envir = environment(builder_session_build),
    inherits = FALSE
  )
  assign(
    "builder_auth_validate_material",
    validate_material,
    envir = environment(builder_session_build)
  )
  on.exit(
    {
      if (is.null(original_handle)) {
        rm(
          ".builder_coordinator_handle",
          envir = environment(builder_session_build)
        )
      } else {
        assign(
          ".builder_coordinator_handle",
          original_handle,
          envir = environment(builder_session_build)
        )
      }
      if (is.null(original_validate)) {
        rm(
          "builder_auth_validate_material",
          envir = environment(builder_session_build)
        )
      } else {
        assign(
          "builder_auth_validate_material",
          original_validate,
          envir = environment(builder_session_build)
        )
      }
    },
    add = TRUE
  )

  builder_session_build(
    process,
    plan,
    request,
    coordinator = coordinator,
    auth_material = material
  )

  serialized <- serialize(captured_args, NULL)
  expect_false(builder_auth_raw_contains(serialized, username))
  expect_false(builder_auth_raw_contains(serialized, password))
  expect_false(builder_auth_raw_contains(serialized, passphrase))
  expect_null(captured_args$request$payload$auth_accounts)
  expect_false("auth_accounts" %in% names(captured_args))
  expect_false("accounts" %in% names(captured_args$plan$app_auth))
  expect_identical(
    names(captured_args$auth_material),
    c("source_dir", "credentials", "env_file", "descriptor")
  )
  expect_false("auth_accounts" %in% names(formals(builder_session_build)))
  expect_false("auth_accounts" %in% names(formals(captured_callback)))
})

test_that("release conversion keeps recovery secondary to the first error", {
  path <- builder_profile_inst_path("builder", "server", "imports.R")
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  first <- grep(
    "builder_release_settlement_result <- function",
    lines,
    fixed = TRUE
  )[[1L]]
  last <- grep(
    "acknowledge_builder_build <- function",
    lines,
    fixed = TRUE
  )[[1L]]
  runtime <- new.env(parent = baseenv())
  runtime$`%||%` <- function(left, right) if (is.null(left)) right else left
  runtime$builder_release_error_result <- builder_release_error_result
  runtime$builder_result_recovery_required <- builder_result_recovery_required
  runtime$builder_result_failure <- builder_result_failure
  runtime$builder_as_result <- builder_as_result
  runtime$builder_coordinator_abort <- function(handle) list(aborted = TRUE)
  eval(parse(text = paste(lines[first:(last - 1L)], collapse = "\n")), runtime)

  converted <- runtime$builder_release_settlement_result(
    list(
      ok = FALSE,
      error = "original build failure",
      cleanup_error = "stage cleanup failure",
      recovery = list(
        state = "recovery_required",
        message = "Restore the preserved release."
      )
    ),
    list(handle = list(target = "/release"))
  )

  expect_s3_class(converted, "builder_result_recovery_required")
  expect_identical(converted$error, "original build failure")
  expect_identical(converted$cleanup_error, "stage cleanup failure")
  expect_identical(
    converted$recovery_message,
    "Restore the preserved release."
  )

  child_exit <- runtime$builder_release_settlement_result(
    simpleError("owner subprocess exited", call = quote(settle_release())),
    list(handle = list(target = "/release")),
    builder_result_failure("original worker build failure")
  )

  expect_identical(child_exit$error, "original worker build failure")
  expect_identical(
    child_exit$settlement_error,
    paste0(
      "The completed Build could not be published: ",
      "owner subprocess exited"
    )
  )

  start_failure <- runtime$builder_release_settlement_result(
    list(
      ok = FALSE,
      error = "The release process could not start: unavailable",
      recovery = NULL
    ),
    list(handle = list(target = "/release")),
    builder_result_failure("original worker build failure")
  )
  expect_identical(start_failure$error, "original worker build failure")
  expect_identical(
    start_failure$settlement_error,
    "The release process could not start: unavailable"
  )
})

test_that("release stop failure retains its process and cancellation lock", {
  path <- builder_profile_inst_path("builder", "server", "imports.R")
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  first <- grep(
    "stop_builder_release_settlement <- function",
    lines,
    fixed = TRUE
  )[[1L]]
  last <- grep("consume_transport_retained <- function", lines, fixed = TRUE)[[
    1L
  ]]
  runtime <- new.env(parent = baseenv())
  runtime$`%||%` <- function(left, right) if (is.null(left)) right else left
  runtime$isolate <- function(value) value
  holder <- function(initial) {
    value <- initial
    function(next_value) {
      if (!missing(next_value)) {
        value <<- next_value
      }
      value
    }
  }
  alive <- TRUE
  process <- list(
    is_alive = function() alive,
    kill_tree = function() invisible(FALSE),
    kill = function() invisible(FALSE),
    wait = function(timeout) invisible(FALSE)
  )
  context <- list(
    release = list(handle = list(), id = "build-1"),
    build_id = "build-1",
    request_id = "request-1",
    status = "settling"
  )
  runtime$release_settlement_process <- holder(process)
  runtime$release_settlement_context <- holder(context)
  note <- NULL
  runtime$busy_note <- function(value) {
    if (!missing(value)) {
      note <<- value
    }
    note
  }
  scheduled <- 0L
  runtime$schedule_builder_release_settlement_poll <- function(delay) {
    scheduled <<- scheduled + 1L
    invisible(TRUE)
  }
  runtime$finish_builder_release_cancellation <- function() TRUE
  runtime$observeEvent <- function(...) NULL
  runtime$.builder_diagnostics_text <- function(value) {
    is.character(value) && length(value) == 1L && nzchar(value)
  }
  eval(parse(text = paste(lines[first:(last - 1L)], collapse = "\n")), runtime)

  expect_false(runtime$stop_builder_release_settlement("user stop"))
  expect_identical(runtime$release_settlement_process(), process)
  expect_identical(
    runtime$release_settlement_context()$status,
    "cancelling"
  )
  expect_identical(runtime$release_settlement_context()$error, "user stop")
  expect_match(note, "Stopping release finalization", fixed = TRUE)
  expect_identical(scheduled, 1L)

  alive <- FALSE
  expect_true(runtime$stop_builder_release_settlement())
})

test_that("release monitor failures become terminal recovery states", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this monitor test.")
  }

  app_env$schedule_builder_release_settlement_poll <- function(...) {
    invisible(TRUE)
  }
  shiny::testServer(app_env$server, {
    request <- builder_command("build", "session", payload = list())
    queued <- builder_enqueue(
      builder_request_protocol("liveness-worker"),
      request
    )
    dispatched <- builder_protocol_dispatch(queued)
    completed <- builder_protocol_complete(
      dispatched$protocol,
      builder_worker_response(dispatched$request, builder_result_success())
    )
    process <- list(
      is_alive = function() NA,
      kill_tree = function() invisible(FALSE),
      kill = function() invisible(FALSE),
      wait = function(timeout) invisible(FALSE)
    )
    release <- list(
      id = dispatched$request$build_id,
      handle = list(
        target = tempfile("release-"),
        stage = tempfile("stage-"),
        control = tempfile("control-"),
        build_id = dispatched$request$build_id
      )
    )
    protocol(completed$protocol)
    active_release(release)
    release_settlement_process(process)
    release_settlement_context(list(
      release = release,
      build_id = dispatched$request$build_id,
      request_id = dispatched$request$request_id,
      status = "settling",
      phase = "verify",
      worker_result = builder_result_success(),
      diagnostics = list(stdout = character(), stderr = character())
    ))
    build_flow(list(stage = "building", plan = NULL))

    expect_true(poll_builder_release_settlement())
    expect_true(poll_builder_release_settlement())
    expect_false(poll_builder_release_settlement())
    expect_identical(result()$state, "recovery_required")
    expect_identical(
      release_settlement_context()$liveness_failures,
      builder_release_settlement_monitor_limit
    )
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
  })
})

test_that("the real dispatcher terminalizes a runtime-freeze failure", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this dispatch test.")
  }

  shiny::testServer(app_env$server, {
    assign(
      "builder_start_release_preparation_process",
      function(...) stop("runtime identity unavailable", call. = FALSE),
      envir = environment(finish_builder_build_dispatch_failure)
    )
    secret <- "dispatch-secret-must-not-survive"
    request <- builder_command(
      "build",
      "session",
      payload = list(
        kind = "build",
        plan = list(
          error = NULL,
          existing_targets = character(),
          overwrite = FALSE
        ),
        auth_accounts = list(list(password = secret))
      )
    )
    worker(list(state = "ready"))
    build_flow(list(stage = "building", plan = request$payload$plan))
    busy_note("Checking the runtime…")
    build_operation_phase("preflight")
    protocol(builder_enqueue(
      builder_request_protocol("runtime-freeze-worker"),
      request
    ))

    session$flushReact()
    later::run_now(0.1)
    session$flushReact()

    expect_identical(result()$state, "failure")
    expect_match(result()$error, "runtime identity unavailable", fixed = TRUE)
    expect_identical(protocol()$build_status, "idle")
    expect_null(protocol()$pending)
    expect_length(protocol()$awaiting_ack, 0L)
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_null(busy_note())
    expect_null(build_operation_phase())
    expect_false(builder_auth_raw_contains(serialize(protocol(), NULL), secret))
  })
})

test_that("release settlement polling ignores an ended Builder session", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this dispatch test.")
  }
  callback <- NULL

  shiny::testServer(app_env$server, {
    expect_true(schedule_builder_release_settlement_poll(
      delay = 0,
      .schedule = function(value, delay) {
        callback <<- value
        invisible(NULL)
      }
    ))
  })

  expect_true(is.function(callback))
  expect_false(callback())
})

test_that("session build returns executable unexpected-condition diagnostics", {
  captured <- NULL
  process <- list(call = function(callback, args) {
    captured <<- list(callback = callback, args = args)
    invisible(TRUE)
  })
  plan <- list(
    make_app = FALSE,
    app_auth = list(enabled = FALSE),
    items = list()
  )

  builder_session_build(process, plan)
  expect_true(is.function(captured$callback))

  callback <- captured$callback
  callback_env <- new.env(parent = environment(callback))
  callback_env$builder_worker_require_capability <- function(...) TRUE
  callback_env$builder_execute_plan <- function(...) {
    stop("unexpected executable failure", call. = FALSE)
  }
  callback_env$.builder_worker_condition_diagnostics <- function(
    condition,
    calls
  ) {
    list(message = conditionMessage(condition), calls = length(calls))
  }
  environment(callback) <- callback_env
  response <- do.call(callback, captured$args)

  expect_identical(response$error, "unexpected executable failure")
  expect_identical(
    response$diagnostics$build$message,
    "unexpected executable failure"
  )
  expect_true(response$diagnostics$build$calls > 0L)
})

test_that("Build overlay trusts explicit backend phases", {
  protocol <- structure(
    list(build_status = "running"),
    class = c("builder_request_protocol", "list")
  )
  phases <- c(
    "preflight",
    "stage",
    "dispatch",
    "datasets",
    "viewer",
    "verify",
    "publish"
  )

  for (phase in phases) {
    model <- builder_build_operation_overlay_model(
      flow = list(stage = "building"),
      protocol = protocol,
      note = "Backend detail",
      result = NULL,
      phase = phase,
      can_stop = TRUE
    )
    expect_true(model$active)
    expect_identical(model$phase, phase)
    expect_identical(model$detail, "Backend detail")
    expect_identical(
      model$cancelable,
      phase %in%
        c("stage", "dispatch", "datasets", "viewer", "verify", "publish")
    )
  }
})

test_that("release settlement timeout is finite and explicit", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this timeout test.")
  }

  shiny::testServer(app_env$server, {
    now <- as.POSIXct("2026-09-26 12:00:00", tz = "UTC")
    expect_identical(builder_release_settlement_timeout(NA_real_), 7200)
    expect_identical(builder_release_settlement_timeout(-1), 7200)
    expect_identical(builder_release_settlement_timeout(90), 90)
    expect_false(builder_release_settlement_timed_out(
      list(
        status = "settling",
        deadline = now + 1
      ),
      now
    ))
    expect_true(builder_release_settlement_timed_out(
      list(
        status = "settling",
        deadline = now
      ),
      now
    ))
    expect_false(builder_release_settlement_timed_out(
      list(
        status = "cancelling",
        deadline = now - 1
      ),
      now
    ))
  })
})

test_that("Build overlay covers output-folder preflight", {
  choosing <- builder_build_operation_overlay_model(
    flow = list(stage = "choosing_folder"),
    protocol = NULL,
    note = NULL,
    result = NULL,
    can_stop = TRUE
  )
  expect_true(choosing$active)
  expect_true(choosing$picker_waiting)
  expect_null(choosing$phase)
  expect_false(choosing$cancelable)
  model <- builder_build_operation_overlay_model(
    flow = list(stage = "checking_folder"),
    protocol = NULL,
    note = NULL,
    result = NULL
  )

  expect_true(model$active)
  expect_identical(model$phase, "preflight")
  expect_identical(model$title, "Building output")
  expect_identical(model$message, "Keep this page open.")
  expect_identical(model$detail, "Checking the selected output folder…")
})

test_that("Build failures expose phase and diagnostic identity", {
  failure <- builder_result_failure("The staged output changed.")
  failure$failure_phase <- "verify"
  failure$failure_stage <- "C:/release/.cbr/stages/build-42"
  failure$failure_build_id <- "build-42"
  failure$diagnostics_path <-
    "C:/release/.cbr/diagnostics/settlement-build-42"

  html <- as.character(builder_build_status_ui(
    builder_build_status_model(failure)
  ))

  expect_match(html, "Failed during", fixed = TRUE)
  expect_match(html, "output verification", fixed = TRUE)
  expect_match(html, "build-42", fixed = TRUE)
  expect_match(html, "Temporary output", fixed = TRUE)
  expect_match(html, "Diagnostic log", fixed = TRUE)
  expect_match(html, "settlement-build-42", fixed = TRUE)
})
