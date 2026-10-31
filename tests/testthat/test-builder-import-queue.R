test_that("the live app exposes imports before their worker result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }
  progress_root <- tempfile("builder-import-queue-")
  dir.create(progress_root, mode = "0700")
  withr::defer(unlink(progress_root, recursive = TRUE, force = TRUE))

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-import-queue", snapshot_root = progress_root))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-import-queue"))

    expect_true(start_load(
      "example",
      "complete_viewer_data",
      "Complete Viewer data"
    ))
    session$flushReact()
    entry <- app_env$builder_import_find(imports(), "ds1")
    expect_s3_class(entry, "builder_import_entry")
    expect_true(entry$load_state %in% c("queued", "reading"))
    expect_identical(active_import_id(), "ds1")

    queue <- imports()
    queue$entries$ds1$size <- 15 * 1024^3
    queue$entries$ds1$started_at_ms <- as.numeric(Sys.time()) * 1000 - 125000
    queue$entries$ds1$load_state <- "reading"
    imports(queue)
    restore_message <- builder_activity_message(
      app_env$builder_activity_state(
        project_phase = "restoring",
        has_project = TRUE,
        has_datasets = FALSE
      ),
      list(total = 7L, remaining = 5L, selected_total = 7L, ready = 2L)
    )
    expect_match(
      restore_message$busy_message,
      "Loading complete_viewer_data.rds"
    )
    expect_match(
      restore_message$busy_detail,
      "2 of 7 selected datasets ready · 15 GB · 2m 0[5-6]s elapsed"
    )
    expect_identical(
      restore_message$busy_detail_items[1:2],
      c("2 of 7 selected datasets ready", "15 GB")
    )
    expect_match(restore_message$busy_detail_items[[3L]], "2m 0[5-6]s elapsed")
    expect_false(restore_message$busy_modal)
    opening_message <- builder_activity_message(
      app_env$builder_activity_state(project_phase = "opening"),
      list(mode = "opening")
    )
    expect_true(opening_message$page_inert)
    expect_true(opening_message$busy_modal)
    attaching_message <- builder_activity_message(
      app_env$builder_activity_state(project_phase = "opening"),
      list(mode = "restoring")
    )
    expect_false(attaching_message$busy_modal)

    request <- Filter(
      Negate(is.null),
      c(list(protocol()$pending), protocol()$queue)
    )[[1L]]
    expect_identical(request$payload$import_generation, 1L)
    expect_true(is.character(request$payload$progress_path))
    expect_true(startsWith(
      normalizePath(dirname(request$payload$progress_path)),
      normalizePath(progress_root)
    ))

    rail_html <- paste(
      vapply(last_import_rail_patch()$rows, `[[`, character(1), "html"),
      collapse = " "
    )
    workbench_html <- paste(unlist(output$workbench), collapse = " ")
    expect_match(rail_html, "Complete Viewer data", fixed = TRUE)
    expect_match(rail_html, "builder-import-status", fixed = TRUE)
    expect_identical(output$ds_count, "1")
    expect_match(workbench_html, "builder-import-loading-note", fixed = TRUE)
    expect_match(workbench_html, "Loading in the background", fixed = TRUE)
    expect_false(grepl(
      "Preparing your workspace",
      workbench_html,
      fixed = TRUE
    ))
    expect_false(grepl("Add files", workbench_html, fixed = TRUE))
    expect_false(grepl('id="build"', workbench_html, fixed = TRUE))
    expect_false(grepl('id="make_app"', workbench_html, fixed = TRUE))
    expect_false(grepl('id="continue_to_review"', workbench_html, fixed = TRUE))
    expect_identical(workflow()$stage, "upload")
  })
})

test_that("import elapsed time starts when worker reading starts", {
  runtime <- new.env(parent = globalenv())
  sys.source(builder_profile_inst_path("builder", "loading.R"), envir = runtime)
  queue <- runtime$builder_import_queue()
  entry <- runtime$builder_import_entry(
    "ds1",
    "Queued dataset",
    list(kind = "file", path = "dataset.qs"),
    size = 1024^3
  )
  entry$started_at_ms <- 1
  queue <- runtime$builder_import_add(queue, entry)

  queue <- runtime$builder_import_transition(queue, "ds1", "reading", 1L)

  expect_gt(queue$entries$ds1$started_at_ms, 1)
  expect_identical(queue$entries$ds1$load_state, "reading")
})

test_that("a failed import logs raw error and shows safe rail error", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-import-error"))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-import-error"))
    expect_true(start_load(
      "example",
      "complete_viewer_data",
      "Complete Viewer data"
    ))
    expect_message(
      expect_true(set_import_state(
        "ds1",
        "error",
        1L,
        "Could not read /private/project/sample.qs: qs-legacy format detected."
      )),
      paste0(
        "Dataset import failed \\[ds1: Complete Viewer data\\]: ",
        "Could not read /private/project/sample\\.qs: ",
        "qs-legacy format detected\\."
      )
    )
    session$flushReact()

    rail_html <- paste(
      vapply(last_import_rail_patch()$rows, `[[`, character(1), "html"),
      collapse = " "
    )
    expect_match(
      rail_html,
      paste(
        "The selected file could not be read.",
        "Check that it is a supported Seurat object and try again."
      ),
      fixed = TRUE
    )
  })
})

test_that("a missing format reader rejects before an import is queued", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  qs_index <- which(vapply(
    app_env$builder_formats,
    function(format) identical(format$id, "qs"),
    logical(1)
  ))
  app_env$builder_formats[[qs_index]]$package <- paste0(
    "CerebroNexusMissingQsReader",
    as.integer(Sys.time())
  )
  source <- withr::local_tempfile(fileext = ".qs")
  writeBin(charToRaw("not-read"), source)

  shiny::testServer(app_env$server, {
    expect_false(start_load(
      "file",
      source,
      "Unavailable QS",
      file_meta = list(
        name = "unavailable.qs",
        type = "application/octet-stream",
        size = file.info(source)$size
      ),
      client_id = "missing-qs-client"
    ))
    expect_length(imports()$entries, 0L)
    expect_match(isolate(add_error()), "QS unavailable", fixed = TRUE)
    record <- isolate(released_client_import_records())[["missing-qs-client"]]
    expect_identical(record$state, "rejected")
    expect_match(
      record$message,
      "Use RDS or QS2",
      fixed = TRUE
    )
  })
})

test_that("a restored source explains when its worker is unavailable", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }

  shiny::testServer(app_env$server, {
    worker(NULL)
    worker_available(FALSE)
    expect_error(
      start_builder_project_record_load(
        list(
          id = "ds1",
          name = "Saved dataset",
          source = list(kind = "example", example = "complete_viewer_data")
        ),
        tempdir()
      ),
      "background worker is still starting"
    )
  })
})

test_that("Project resume forwards its saved MD5 to the worker", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "source.rds")
  saveRDS(SeuratObject::pbmc_small, source_path)
  fingerprint <- app_env$builder_project_file_fingerprint(
    source_path,
    content = TRUE
  )
  state <- app_env$builder_project_file_metadata(source_path)
  source_reference <- "sources/ds1/source.rds"
  state$path <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  signature <- stats::setNames(list(state), source_reference)

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-project-md5", snapshot_root = root))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-project-md5"))
    record <- list(
      id = "ds1",
      name = "Saved dataset",
      runtime_restore_action = "resume",
      runtime_restore_status = list(stat_signature = signature),
      source = list(
        kind = "managed",
        path = source_reference,
        filename = "source.rds",
        fingerprint = fingerprint
      )
    )

    expect_true(start_builder_project_record_load(record, root))
    request <- Filter(
      Negate(is.null),
      c(list(protocol()$pending), protocol()$queue)
    )[[1L]]
    expect_identical(request$payload$expected_md5, fingerprint$md5)
    expect_identical(request$payload$path, state$path)
    expect_identical(isolate(pending_uploads())$ds1$size, fingerprint$bytes)
  })
})

test_that("Project source resume is idempotent and clears orphan reservations", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "source.rds")
  saveRDS(SeuratObject::pbmc_small, source_path)
  fingerprint <- app_env$builder_project_file_fingerprint(
    source_path,
    content = TRUE
  )
  state <- app_env$builder_project_file_metadata(source_path)
  source_reference <- "sources/ds1/source.rds"
  state$path <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  signature <- stats::setNames(list(state), source_reference)

  shiny::testServer(app_env$server, {
    worker(list(
      epoch = "worker-idempotent-resume",
      snapshot_root = root
    ))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-idempotent-resume"))
    record <- list(
      id = "ds1",
      name = "Saved dataset",
      runtime_restore_action = "resume",
      runtime_restore_status = list(stat_signature = signature),
      source = list(
        kind = "managed",
        path = source_reference,
        filename = "source.rds",
        fingerprint = fingerprint
      )
    )
    builder_project_pending_entries(list(ds1 = record))
    orphan <- app_env$builder_source_key("file", source_path)
    pending_sources(orphan)

    expect_true(start_builder_project_record_load(record, root))
    expect_identical(length(protocol()$queue), 1L)
    expect_identical(imports()$entries$ds1$load_state, "queued")

    expect_true(start_builder_project_record_load(record, root))
    expect_identical(length(protocol()$queue), 1L)
    expect_identical(imports()$entries$ds1$load_state, "queued")
  })
})

test_that("client imports bind identities and release exactly once", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this protocol test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-client-protocol"))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-client-protocol"))

    expect_true(start_load(
      "example",
      "complete_viewer_data",
      "Complete Viewer data",
      client_id = "client-import-1"
    ))
    expect_identical(client_import_id_for("ds1"), "client-import-1")
    expect_true(release_client_import(
      "client-import-1",
      server_id = "ds1",
      outcome = "ready"
    ))
    expect_false(release_client_import(
      "client-import-1",
      server_id = "ds1",
      outcome = "error",
      message = "late failure"
    ))
    expect_identical(released_client_imports(), "client-import-1")
  })
})

test_that("ten queued sources stay lightweight and single-flight", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this backpressure test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }
  progress_root <- tempfile("builder-import-backpressure-")
  dir.create(progress_root, mode = "0700")
  withr::defer(unlink(progress_root, recursive = TRUE, force = TRUE))

  shiny::testServer(app_env$server, {
    worker(list(epoch = "worker-backpressure", snapshot_root = progress_root))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-backpressure"))

    for (index in seq_len(10L)) {
      expect_true(start_load(
        "example",
        paste0("fixture_", index),
        paste("Dataset", index)
      ))
    }

    queue <- imports()
    expect_length(queue$entries, 10L)
    expect_lte(length(app_env$builder_import_active_ids(queue)), 1L)
    expect_identical(length(protocol()$awaiting_ack), 0L)
    expect_lte(
      length(Filter(
        Negate(is.null),
        list(protocol()$pending)
      )),
      1L
    )
    expect_true(all(vapply(
      queue$entries,
      function(entry) {
        is.null(entry$profile) && is.null(entry$settings)
      },
      logical(1)
    )))

    session$setInputs(remove_import = list(id = "ds10", nonce = 1))
    expect_null(app_env$builder_import_find(imports(), "ds10"))
    expect_false(any(vapply(
      c(list(protocol()$pending), protocol()$queue),
      function(request) {
        !is.null(request) && identical(request$dataset, "ds10")
      },
      logical(1)
    )))
  })
})

test_that("loading datasets block Configure with a user-facing reason", {
  app <- builder_app_source_text()
  workflow_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    app,
    "Wait for all datasets to finish loading.",
    fixed = TRUE
  )
  expect_match(
    app,
    "Retry or remove datasets that could not load.",
    fixed = TRUE
  )
  expect_match(app, "active_import_id", fixed = TRUE)
  expect_false(grepl(
    "builder_loading_workbench_ui",
    workflow_server,
    fixed = TRUE
  ))
})

builder_import_queue_restart_stub <- function(app_env, server_env, calls) {
  server_env$restart_worker_protocol <- function(
    current_worker,
    current_protocol,
    reason,
    grace_ms = 5000L,
    async = FALSE,
    notify = TRUE
  ) {
    calls$items[[length(calls$items) + 1L]] <- list(
      reason = reason,
      grace_ms = grace_ms,
      async = async,
      notify = notify
    )
    recovered <- app_env$builder_protocol_recover(
      current_protocol,
      epoch = "worker-restarted",
      reason = reason
    )
    server_env$protocol(recovered$protocol)
    server_env$worker(list(epoch = "worker-restarted", state = "starting"))
    server_env$worker_available(FALSE)
    invisible(TRUE)
  }
}

test_that("cancelling a running import restarts the busy worker", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }
  app_env$builder_session_example <- function(...) invisible(TRUE)
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }

  shiny::testServer(app_env$server, {
    calls <- new.env(parent = emptyenv())
    calls$items <- list()
    builder_import_queue_restart_stub(
      app_env,
      environment(remove_pending_import),
      calls
    )
    worker(list(epoch = "worker-cancel", state = "ready"))
    worker_available(TRUE)
    protocol(app_env$builder_request_protocol("worker-cancel"))
    expect_true(start_load(
      "example",
      "complete_viewer_data",
      "Complete Viewer data"
    ))
    session$flushReact()
    expect_identical(isolate(protocol())$pending$dataset, "ds1")

    expect_true(remove_pending_import("ds1"))
    entry <- app_env$builder_import_find(isolate(imports()), "ds1")
    expect_true(entry$cancelling)
    expect_identical(entry$progress_label, "Cancelling…")
    expect_length(calls$items, 0L)
    ## A second click while stopping does not queue another restart.
    expect_true(remove_pending_import("ds1"))

    session$flushReact()
    rail_html <- paste(
      vapply(last_import_rail_patch()$rows, `[[`, character(1), "html"),
      collapse = " "
    )
    expect_match(rail_html, "Cancelling…", fixed = TRUE)
    later::run_now(1)

    expect_length(calls$items, 1L)
    expect_identical(calls$items[[1L]]$reason, "The import was cancelled.")
    expect_identical(calls$items[[1L]]$grace_ms, 0L)
    expect_true(calls$items[[1L]]$async)
    expect_false(calls$items[[1L]]$notify)
    expect_null(app_env$builder_import_find(isolate(imports()), "ds1"))
    recovered <- isolate(protocol())
    expect_null(recovered$pending)
    expect_length(recovered$queue, 0L)
    expect_false("ds1" %in% isolate(cancelled_loads()))
  })
})

test_that("a Build that ignores Stop is ended after the grace period", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  withr::local_options(CerebroNexus.builder.build_stop_grace_ms = 0)
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this import queue test.")
  }
  app_env$builder_session_poll <- function(worker, ...) {
    list(worker = worker, event = NULL, result = NULL)
  }

  shiny::testServer(app_env$server, {
    calls <- new.env(parent = emptyenv())
    calls$items <- list()
    builder_import_queue_restart_stub(
      app_env,
      environment(schedule_builder_build_stop_fallback),
      calls
    )
    worker(list(epoch = "worker-build-stop", state = "ready"))
    current <- app_env$builder_request_protocol("worker-build-stop")
    current <- app_env$builder_enqueue(
      current,
      app_env$builder_command(
        kind = "build",
        dataset = "session",
        payload = list(kind = "build", progress_path = tempfile())
      )
    )
    dispatched <- app_env$builder_protocol_dispatch(current)
    protocol(dispatched$protocol)
    request_id <- dispatched$request$request_id

    schedule_builder_build_stop_fallback("another-request")
    later::run_now(1)
    expect_length(calls$items, 0L)

    schedule_builder_build_stop_fallback(request_id)
    later::run_now(1)
    expect_length(calls$items, 1L)
    expect_match(
      calls$items[[1L]]$reason,
      "^Build stopped by the user"
    )
    expect_identical(calls$items[[1L]]$grace_ms, 0L)
    expect_true(calls$items[[1L]]$async)
    expect_false(calls$items[[1L]]$notify)
  })
})
