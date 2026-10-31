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

builder_app_lines <- function() {
  builder_app_source_lines()
}

builder_session_lines <- function() {
  readLines(
    builder_profile_inst_path("builder", "session.R"),
    warn = FALSE
  )
}

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

test_that("Builder defers initial prewarm until the server loop is ready", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  scheduled <- NULL
  prewarm <- function() TRUE
  app_env$builder_app_schedule_prewarm_worker <- function(delay, .prewarm) {
    scheduled <<- list(delay = delay, prewarm = .prewarm)
    TRUE
  }

  expect_true(app_env$builder_app_start(
    list(available = TRUE, reason = NULL),
    .prewarm = prewarm
  ))
  expect_identical(scheduled$delay, 0)
  expect_identical(scheduled$prewarm, prewarm)
  expect_true(app_env$builder_app_restore_process_state())
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

test_that("a ready session schedules exactly one warm worker for refresh", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$.builder_app_process_state$active <- TRUE

  scheduled <- list()
  prewarmed <- 0L
  expect_true(app_env$builder_app_schedule_prewarm_worker(
    delay = 0.25,
    .schedule = function(callback, delay) {
      scheduled[[length(scheduled) + 1L]] <<- list(
        callback = callback,
        delay = delay
      )
    },
    .prewarm = function() {
      prewarmed <<- prewarmed + 1L
      TRUE
    }
  ))
  expect_false(app_env$builder_app_schedule_prewarm_worker(
    .schedule = function(...) stop("must not schedule twice")
  ))
  expect_length(scheduled, 1L)
  expect_identical(scheduled[[1L]]$delay, 0.25)

  scheduled[[1L]]$callback()
  expect_identical(prewarmed, 1L)
  expect_false(app_env$.builder_app_process_state$prewarm_scheduled)

  process <- list(is_alive = function() TRUE)
  candidate <- structure(
    list(process = process, state = "ready", error = NULL),
    class = c("builder_worker", "list")
  )
  app_env$.builder_app_process_state$prewarmed_worker <- candidate
  replenished <- 0L
  expect_identical(
    app_env$builder_app_take_prewarmed_worker(
      .replenish = function() {
        replenished <<- replenished + 1L
        TRUE
      }
    ),
    candidate
  )
  expect_identical(replenished, 1L)
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

builder_app_block <- function(lines, start, finish) {
  first <- grep(start, lines, fixed = TRUE)[1L]
  last <- grep(finish, lines, fixed = TRUE)
  last <- last[last > first][1L]
  expect_false(is.na(first), info = paste("Missing App marker:", start))
  expect_false(is.na(last), info = paste("Missing App marker:", finish))
  paste(lines[first:(last - 1L)], collapse = "\n")
}

test_that("worker test runtime loads the App path contract first", {
  runtime <- new.env(parent = baseenv())
  paths <- builder_app_source_runtime_prerequisites(runtime)

  expect_true(length(paths) > 0L)
  expect_true(all(file.exists(paths)))
  expect_true(exists(".pathWithin", envir = runtime, inherits = FALSE))
})

test_that("parent and worker load App assembly before build authorities", {
  app <- paste(builder_app_lines(), collapse = "\n")
  worker <- paste(
    readLines(builder_app_worker_path, warn = FALSE),
    collapse = "\n"
  )
  app_bundle_parent <- regexpr(
    'source("app_bundle.R", local = TRUE)',
    app,
    fixed = TRUE
  )[1L]
  coordinator_parent <- regexpr(
    'source("coordinator.R", local = TRUE)',
    app,
    fixed = TRUE
  )[1L]
  build_parent <- regexpr('source("build.R", local = TRUE)', app, fixed = TRUE)[
    1L
  ]
  app_bundle_worker <- regexpr(
    'file.path(dir, "app_bundle.R")',
    worker,
    fixed = TRUE
  )[1L]
  build_worker <- regexpr(
    'file.path(dir, "build.R")',
    worker,
    fixed = TRUE
  )[1L]

  expect_gt(app_bundle_parent, 0L)
  expect_lt(app_bundle_parent, coordinator_parent)
  expect_lt(app_bundle_parent, build_parent)
  expect_gt(app_bundle_worker, 0L)
  expect_lt(app_bundle_worker, build_worker)
  expect_false(grepl(
    'source(file.path(dir, "publish.R"))',
    worker,
    fixed = TRUE
  ))
})

test_that("preview and coordinates apply only to the visible dataset section", {
  lines <- builder_app_lines()
  preview <- builder_app_block(
    lines,
    'identical(p$kind, "preview")',
    'identical(p$kind, "coords")'
  )
  coords <- builder_app_block(
    lines,
    'identical(p$kind, "coords")',
    'identical(p$kind, "align_all")'
  )

  expect_match(preview, "identical(current(), p$id)", fixed = TRUE)
  expect_match(coords, "identical(current(), p$id)", fixed = TRUE)
  expect_match(coords, "identical(active_slice(), p$image)", fixed = TRUE)
  expect_true(grepl("preview_frame(value)", preview, fixed = TRUE))
  expect_true(grepl("spatial_coords(value)", coords, fixed = TRUE))
})

test_that("the App polls replaceable worker results every 100 milliseconds", {
  lines <- builder_app_lines()
  poller <- builder_app_block(
    lines,
    "## -- one poller drains",
    "## Take the per-section extents"
  )

  expect_match(poller, "invalidateLater(100, session)", fixed = TRUE)
  expect_false(grepl("invalidateLater(300, session)", poller, fixed = TRUE))
})

test_that("session calls return deterministic failures as typed responses", {
  session <- paste(builder_session_lines(), collapse = "\n")

  expect_gte(
    lengths(regmatches(
      session,
      gregexpr(
        "builder_worker_response\\(request, error = conditionMessage\\(error\\)\\)",
        session,
        perl = TRUE
      )
    )),
    7L
  )
  expect_match(
    session,
    "builder_worker_response(request, error = error)",
    fixed = TRUE
  )
  expect_false(grepl(
    "builder_worker_response(request, list(error = error))",
    session,
    fixed = TRUE
  ))
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

test_that("an invalidated query failure leaves a queued Build runnable", {
  protocol <- builder_request_protocol("worker-a")
  protocol <- builder_enqueue(
    protocol,
    builder_query("preview", "dataset-a", generation = 1L)
  )
  dispatched <- builder_protocol_dispatch(protocol)
  protocol <- builder_enqueue(
    dispatched$protocol,
    builder_command("build", "session", payload = list(id = "build-a"))
  )

  completed <- builder_protocol_complete(
    protocol,
    builder_worker_response(
      dispatched$request,
      error = "The old preview no longer exists."
    )
  )
  expect_false(completed$accepted)
  expect_null(completed$protocol$pending)
  expect_identical(builder_pending_ids(completed$protocol), "build:session")

  build <- builder_protocol_dispatch(completed$protocol)
  expect_identical(build$request$kind, "build")
  expect_identical(build$protocol$build_status, "running")
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

test_that("business errors are terminalized without worker recovery", {
  lines <- builder_app_lines()
  poller <- builder_app_block(
    lines,
    "## -- one poller drains",
    "## Take the per-section extents"
  )
  typed_error <- builder_app_block(
    strsplit(poller, "\n", fixed = TRUE)[[1L]],
    "if (!is.null(completed$error))",
    'if (identical(p$kind, "load"))'
  )

  expect_match(typed_error, "builder_protocol_acknowledge", fixed = TRUE)
  expect_false(grepl("restart_worker_protocol", typed_error, fixed = TRUE))
})

test_that("a successful drop forgets all stale protocol state", {
  lines <- builder_app_lines()
  drop <- builder_app_block(
    lines,
    '} else if (identical(p$kind, "drop")) {',
    "## Take the per-section extents"
  )

  released <- regexpr("builder_worker_release_snapshot", drop, fixed = TRUE)[1L]
  acknowledged <- regexpr("builder_protocol_acknowledge", drop, fixed = TRUE)[
    1L
  ]
  forgotten <- regexpr("builder_protocol_forget_dataset", drop, fixed = TRUE)[
    1L
  ]

  expect_gt(released, 0L)
  expect_gt(acknowledged, released)
  expect_gt(forgotten, acknowledged)
  expect_match(drop, "failed", fixed = TRUE)
  expect_match(drop, "discarded", fixed = TRUE)
})

test_that("workflow server exclusively owns loading and stage rendering", {
  lines <- builder_app_lines()
  app <- paste(lines, collapse = "\n")
  workbench <- builder_app_block(
    lines,
    "output$workbench <- renderUI({",
    "observeEvent(input$continue_to_review, {"
  )

  expect_identical(
    lengths(regmatches(
      app,
      gregexpr("output$workbench <- renderUI({", app, fixed = TRUE)
    )),
    1L
  )
  expect_match(app, "selected_workflow_stage <- reactiveVal(", fixed = TRUE)
  expect_false(grepl("active_import_id()", workbench, fixed = TRUE))
  expect_false(grepl("import_focus_id()", workbench, fixed = TRUE))
  expect_false(grepl("builder_loading_workbench_ui", workbench, fixed = TRUE))
  expect_false(grepl("imports()", workbench, fixed = TRUE))
  expect_false(grepl("builder_importing_workbench_ui", workbench, fixed = TRUE))
  expect_match(
    workbench,
    "import_counts <- workbench_import_counts()",
    fixed = TRUE
  )
  expect_match(
    workbench,
    paste0(
      "    upload = if (\n",
      "      import_counts[[\"failed\"]] > 0L && ",
      "import_counts[[\"active\"]] == 0L\n",
      "    ) {\n",
      "      builder_failed_import_workbench_ui("
    ),
    fixed = TRUE
  )
  expect_match(
    workbench,
    paste0(
      "    } else {\n      tagAppendAttributes(\n",
      "        builder_empty_workbench_ui("
    ),
    fixed = TRUE
  )
  expect_match(workbench, "stage <- selected_workflow_stage()", fixed = TRUE)
  expect_match(
    workbench,
    "configure = render_configure_workbench()",
    fixed = TRUE
  )
  expect_match(workbench, "review = render_review_workbench()", fixed = TRUE)
  expect_match(workbench, "build = render_build_workbench()", fixed = TRUE)
  expect_match(workbench, "Unsupported Builder workflow stage", fixed = TRUE)
  expect_false(grepl('actionButton(\n      "build"', app, fixed = TRUE))
  expect_false(grepl('uiOutput("actionbar")', app, fixed = TRUE))
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

test_that("release recovery releases worker files and uses the evidenced action", {
  build_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "build.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    build_server,
    "action <- builder_release_recovery_action(recovery)",
    fixed = TRUE
  )
  expect_match(build_server, "restart_worker_protocol(", fixed = TRUE)
  expect_match(
    build_server,
    "builder_coordinator_recover(target, action = action)",
    fixed = TRUE
  )
  expect_false(grepl(
    'builder_coordinator_recover(target, action = "restore")',
    build_server,
    fixed = TRUE
  ))
})

test_that("Build owns output mode and expanded Viewer App settings", {
  crb <- builder_build_options_ui(builder_build_options())
  crb_html <- builder_stage_html(crb)
  expect_match(crb_html, "CRB files only", fixed = TRUE)
  expect_false(grepl("Welcome message", crb_html, fixed = TRUE))

  app <- builder_build_options_ui(builder_build_options(make_app = TRUE))
  app_html <- builder_stage_html(app)
  expect_false(builder_build_options(make_app = TRUE)$app$launch_browser)
  expect_match(app_html, "Viewer app", fixed = TRUE)
  expect_match(app_html, "Welcome message", fixed = TRUE)
  expect_false(grepl("Open App after build", app_html, fixed = TRUE))
  expect_false(grepl("build_launch_browser", app_html, fixed = TRUE))
  expect_match(app_html, "Host", fixed = TRUE)
  expect_match(app_html, "Port", fixed = TRUE)
  expect_match(app_html, "Require login", fixed = TRUE)
  expect_match(app_html, "builder-stage-section", fixed = TRUE)
  expect_match(app_html, "builder-build-options-fields", fixed = TRUE)
  expect_match(app_html, "<details", fixed = TRUE)
  expect_match(app_html, "Connection settings", fixed = TRUE)

  auth_unavailable <- builder_stage_html(builder_build_options_ui(
    builder_build_options(make_app = TRUE),
    auth = list(
      enabled = FALSE,
      account_count = 0L,
      error = NULL,
      available = FALSE,
      reason = paste(
        "Login is unavailable because this required R package is missing or too old:",
        "shinymanager (>= 1.1.0).",
        'Run install.packages("shinymanager"), then restart Builder.'
      )
    )
  ))
  expect_match(auth_unavailable, "Login is unavailable", fixed = TRUE)
  expect_match(auth_unavailable, "shinymanager (&gt;= 1.1.0)", fixed = TRUE)

  unavailable <- builder_stage_html(builder_build_options_ui(
    builder_build_options(),
    app_available = FALSE,
    app_reason = "Install Viewer dependencies."
  ))
  expect_match(unavailable, 'value="app" disabled="disabled"', fixed = TRUE)
  expect_match(unavailable, "builder-app-capability-warning", fixed = TRUE)
  expect_match(unavailable, "Viewer App unavailable", fixed = TRUE)
  expect_match(unavailable, "Install Viewer dependencies.", fixed = TRUE)
  expect_match(unavailable, "CRB-only export is still available.", fixed = TRUE)
  expect_false(grepl("remotes::install_local", unavailable, fixed = TRUE))

  locked <- builder_stage_html(builder_build_options_ui(
    builder_build_options(make_app = TRUE),
    controls_disabled = TRUE
  ))
  expect_match(
    locked,
    'class="builder-build-options-fields" disabled="disabled"',
    fixed = TRUE
  )
})

test_that("Viewer App is the default output when its runtime is available", {
  foundation <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    foundation,
    "build_mode <- reactiveVal(isTRUE(app_capability$available))",
    fixed = TRUE
  )
})

test_that("login state updates only the local authentication controls", {
  build_lines <- readLines(
    builder_profile_inst_path("builder", "server", "build.R"),
    warn = FALSE
  )
  options_output <- builder_app_block(
    build_lines,
    "output$build_output_options <- renderUI({",
    "current_build_options <- function() {"
  )
  build_server <- paste(build_lines, collapse = "\n")

  expect_match(options_output, "isolate(auth_enabled())", fixed = TRUE)
  expect_match(options_output, "isolate(auth_accounts())", fixed = TRUE)
  expect_match(options_output, "isolate(auth_validation())", fixed = TRUE)
  expect_match(options_output, 'uiOutput("build_auth_controls")', fixed = TRUE)
  expect_match(
    build_server,
    "output$build_auth_controls <- renderUI({",
    fixed = TRUE
  )
})

test_that("login dialog is compact and disabling has one state owner", {
  dialog <- paste(
    readLines(
      builder_profile_inst_path("builder", "ui", "review_stage.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  disabled_branch <- regmatches(
    javascript,
    regexpr(
      'if \\(target\\.matches\\("#build_require_login"\\)[\\s\\S]+?var viewerGroupFocus',
      javascript,
      perl = TRUE
    )
  )

  expect_match(dialog, "builder-auth-header", fixed = TRUE)
  expect_match(dialog, "builder-auth-body", fixed = TRUE)
  expect_match(dialog, "builder-auth-footer", fixed = TRUE)
  expect_match(
    dialog,
    "Passwords must contain at least 8 characters.",
    fixed = TRUE
  )
  expect_match(javascript, "rollbackRequireLogin: false", fixed = TRUE)
  expect_match(javascript, "function cancelAuthDialog()", fixed = TRUE)
  expect_match(javascript, "openAuthDialog(target, true)", fixed = TRUE)
  expect_match(
    javascript,
    "authEditor.rollbackRequireLogin = false;\n        closeAuthDialog(true);",
    fixed = TRUE
  )
  expect_length(disabled_branch, 1L)
  expect_false(grepl(
    'send("builder_auth_accounts"',
    disabled_branch,
    fixed = TRUE
  ))
})

test_that("login and connection settings use prominent setting rows", {
  ui <- builder_stage_html(builder_build_options_ui(
    builder_build_options(make_app = TRUE),
    auth = list(
      enabled = TRUE,
      account_count = 1L,
      error = NULL,
      available = TRUE,
      reason = NULL
    )
  ))
  css <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.features.css"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(ui, "builder-auth-account-copy", fixed = TRUE)
  expect_match(ui, "Manage accounts", fixed = TRUE)
  expect_match(css, ".review-auth-controls {", fixed = TRUE)
  expect_match(
    css,
    ".review-auth-controls {\n  display: flex;\n  flex-wrap: wrap;\n  width: 100%;",
    fixed = TRUE
  )
  expect_match(
    css,
    ".builder-app-toggle {\n  display: flex;\n  width: 100%;",
    fixed = TRUE
  )
  expect_match(
    css,
    "padding-block: var(--space-2);\n  padding-right: 4rem;",
    fixed = TRUE
  )
  expect_match(
    css,
    ".builder-app-connection-settings {\n  width: 100%;",
    fixed = TRUE
  )
  expect_match(css, "border: 1px solid var(--c-border);", fixed = TRUE)
  expect_match(css, "min-height: 3.25rem;", fixed = TRUE)
  expect_match(css, "font-size: var(--builder-type-small);", fixed = TRUE)
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

test_that("Build stage renders only the confirmed stored snapshot", {
  app <- paste(builder_app_lines(), collapse = "\n")
  workflow_server <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  workflow_ui_lines <- readLines(
    builder_profile_inst_path("builder", "ui", "workflow.R"),
    warn = FALSE
  )
  build_ui_start <- grep(
    "builder_build_workbench_ui <- function",
    workflow_ui_lines,
    fixed = TRUE
  )
  workflow_ui <- paste(
    workflow_ui_lines[build_ui_start:length(workflow_ui_lines)],
    collapse = "\n"
  )

  expect_match(workflow_server, "state$review_snapshot", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      app,
      gregexpr("render_build_workbench <- function", app, fixed = TRUE)
    )),
    1L
  )
  expect_match(
    workflow_server,
    "builder_workflow_confirmation_matches(state, snapshot)",
    fixed = TRUE
  )
  expect_match(workflow_server, "selected_output()", fixed = TRUE)
  expect_match(workflow_ui, '`data-workflow-stage` = "build"', fixed = TRUE)
  expect_match(workflow_ui, 'builder_stage_header_ui(', fixed = TRUE)
  expect_match(workflow_ui, '"Build your output"', fixed = TRUE)
  expect_false(grepl(
    '"No output folder selected"',
    paste(workflow_ui_lines, collapse = "\n"),
    fixed = TRUE
  ))
  expect_false(grepl(
    'uiOutput("build_stage_controls")',
    workflow_ui,
    fixed = TRUE
  ))
  expect_match(workflow_ui, 'id = "build-stage-status"', fixed = TRUE)
  expect_match(
    workflow_ui,
    'uiOutput("build_stage_status_content")',
    fixed = TRUE
  )
  expect_match(workflow_ui, 'uiOutput("build_stage_footer")', fixed = TRUE)
  expect_false(grepl(
    'actionButton(\n        "build"',
    workflow_ui,
    fixed = TRUE
  ))
  expect_false(grepl("Configure", workflow_ui))

  render_build <- builder_app_block(
    readLines(
      builder_profile_inst_path("builder", "server", "workflow.R"),
      warn = FALSE
    ),
    "render_build_workbench <- function() {",
    "observeEvent(input$back_to_review, {"
  )
  for (volatile in c(
    "result()",
    "build_flow()",
    "protocol()",
    "busy_note()",
    "selected_output()"
  )) {
    expect_false(grepl(volatile, render_build, fixed = TRUE), info = volatile)
  }
})

test_that("group color changes use the existing settings revision path", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, 'input[["core-group_color"]]', fixed = TRUE)
  expect_match(app, "builder_update_color_override", fixed = TRUE)
  expect_match(app, 'input[["core-reset_colors"]]', fixed = TRUE)
  expect_match(app, "builder_reset_color_overrides", fixed = TRUE)
  expect_match(app, "replace_entry(entry)", fixed = TRUE)
  expect_false(grepl("umap_palette|pca_palette|tsne_palette", app))
})

test_that("parameter writes do not eagerly freeze Review or rebuild editors", {
  review_lines <- readLines(
    builder_profile_inst_path("builder", "server", "review.R"),
    warn = FALSE
  )
  review <- paste(review_lines, collapse = "\n")
  dataset_lines <- readLines(
    builder_profile_inst_path("builder", "server", "datasets.R"),
    warn = FALSE
  )
  datasets <- paste(dataset_lines, collapse = "\n")

  snapshot <- builder_app_block(
    review_lines,
    "current_review_snapshot <- reactive({",
    "review_report <- reactive({"
  )
  expect_match(snapshot, "bindEvent(", fixed = TRUE)
  expect_match(snapshot, "dataset_check_marks()", fixed = TRUE)
  expect_match(snapshot, "dataset_revisions()", fixed = TRUE)
  expect_match(
    snapshot,
    "alignment_server$pending_drafts()",
    fixed = TRUE
  )
  expect_match(snapshot, "imports()", fixed = TRUE)
  expect_false(grepl("freeze_plan_for_output", snapshot, fixed = TRUE))
  expect_false(grepl("builder_freeze_plan", snapshot, fixed = TRUE))

  colors <- regmatches(
    datasets,
    regexpr(
      'output\\[\\["core-group_colors"\\]\\] <- renderUI\\(\\{[\\s\\S]+?output\\[\\["core-projection_gallery"',
      datasets,
      perl = TRUE
    )
  )
  expect_match(colors, "group_color_ui_revision()", fixed = TRUE)
  expect_match(colors, "isolate(entry_of(id))", fixed = TRUE)

  metadata <- builder_app_block(
    dataset_lines,
    'output[["core-metadata_preview"]] <- renderUI({',
    'output[["core-group_colors"]] <- renderUI({'
  )
  expect_match(metadata, "group_preview_ui_revision()", fixed = TRUE)
  expect_match(metadata, "isolate(entry_of(id))", fixed = TRUE)

  group_action <- builder_app_block(
    dataset_lines,
    'input[["core-group_action"]],',
    'input[["core-cell_cycle"]],'
  )
  expect_match(group_action, "group_preview_ui_revision(", fixed = TRUE)

  tables <- builder_app_block(
    review_lines,
    'output[["enhance-table_list"]] <- renderUI({',
    'output[["inspect_stage"]] <- renderUI({'
  )
  expect_match(tables, "enhance_table_ui_revision()", fixed = TRUE)
  expect_match(tables, "isolate(entry_of(id))", fixed = TRUE)
})

test_that("parameter writes do not rebuild workflow navigation", {
  workflow_lines <- readLines(
    builder_profile_inst_path("builder", "server", "workflow.R"),
    warn = FALSE
  )
  progress <- builder_app_block(
    workflow_lines,
    "output$workflow_progress <- renderUI({",
    "navigate_workflow_stage <- function(stage) {"
  )

  expect_match(progress, "workflow_has_datasets()", fixed = TRUE)
  expect_false(grepl("store()$datasets", progress, fixed = TRUE))
})

test_that("parameter writes do not rebuild dataset rail utilities", {
  foundation_lines <- readLines(
    builder_profile_inst_path("builder", "server", "foundation.R"),
    warn = FALSE
  )
  projections <- builder_app_block(
    foundation_lines,
    "rail_dataset_count <- reactiveVal(",
    "sets <- function(value) {"
  )
  count_output <- builder_app_block(
    foundation_lines,
    "output$ds_count <- renderText({",
    "output$rail_undo <- renderUI({"
  )
  undo_output <- builder_app_block(
    foundation_lines,
    "output$rail_undo <- renderUI({",
    "## -- the worker process"
  )

  expect_match(projections, "isolate(rail_dataset_count())", fixed = TRUE)
  expect_match(projections, "isolate(rail_undo_available())", fixed = TRUE)
  expect_match(count_output, "rail_dataset_count()", fixed = TRUE)
  expect_false(grepl("store()", count_output, fixed = TRUE))
  expect_false(grepl("imports()", count_output, fixed = TRUE))
  expect_match(undo_output, "rail_undo_available()", fixed = TRUE)
  expect_false(grepl("store()", undo_output, fixed = TRUE))
})

test_that("configure actions use nonce events without browser-side loading patches", {
  javascript <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl(
    'target.closest("#complete_dataset_check")',
    javascript,
    fixed = TRUE
  ))
  expect_match(
    javascript,
    'target.closest("[data-builder-configure-action]")',
    fixed = TRUE
  )
  expect_match(
    javascript,
    "configureActionSequence += 1",
    fixed = TRUE
  )
  expect_false(grepl("setDatasetCheckBusy", javascript, fixed = TRUE))
  expect_false(grepl("builder_dataset_check_state", javascript, fixed = TRUE))
})

test_that("background dataset completion preserves the current viewport", {
  imports <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "imports.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(imports, "current_id <- isolate(current())", fixed = TRUE)
  expect_match(
    imports,
    "focus_configure <- !identical(next_current, current_id)",
    fixed = TRUE
  )
  expect_match(imports, "if (focus_configure) {", fixed = TRUE)
})

test_that("Dataset summary is visible and incomplete settings receive focus", {
  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  summary_start <- regexpr(
    'class = "builder-configure-inspect"',
    review,
    fixed = TRUE
  )[[1L]]
  summary_block <- substr(
    review,
    max(1L, summary_start - 80L),
    summary_start + 180L
  )

  expect_gt(summary_start, 0L)
  expect_false(grepl("tags$details(", summary_block, fixed = TRUE))
  expect_match(summary_block, 'uiOutput("inspect_stage")', fixed = TRUE)
  expect_match(javascript, "focusIncompleteSetting", fixed = TRUE)
  expect_match(
    javascript,
    '"builder_focus_incomplete_setting"',
    fixed = TRUE
  )
})

test_that("the App describes object isolation accurately", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_false(grepl("Objects are read into this process", app, fixed = TRUE))
  expect_match(app, "isolated worker process", fixed = TRUE)
})

test_that("artifact replacement invalidates only the configure surface", {
  foundation <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  review <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "review.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    foundation,
    "configure_workbench_surface <- reactiveVal",
    fixed = TRUE
  )
  expect_match(foundation, "entry$load_state %||% \"loaded\"", fixed = TRUE)
  expect_match(review, "configure_workbench_surface()", fixed = TRUE)
  expect_match(review, "entry <- isolate(entry_of(id))", fixed = TRUE)
})

test_that("unchanged settings do not write state or advance revisions", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  settings_observer <- builder_app_block(
    builder_app_lines(),
    "## -- keep the current entry's settings",
    "## Assay-dependent controls"
  )
  expect_match(
    settings_observer,
    "e <- isolate(entry_of(id))",
    fixed = TRUE
  )
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

test_that("direct and request-bearing Builds reject a replaced same-id snapshot", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  frozen <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-frozen"),
    available_bytes = 2^40
  )
  replacement <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-replacement"),
    available_bytes = 2^40
  )
  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list(`dataset-a` = replacement)
  )
  withr::defer({
    try(builder_worker_stop(worker), silent = TRUE)
    if (isTRUE(.builder_snapshot_owned(frozen))) {
      .builder_snapshot_release(frozen)
    }
    if (isTRUE(.builder_snapshot_owned(replacement))) {
      .builder_snapshot_release(replacement)
    }
  })

  identity <- c(
    list(available = TRUE, snapshot = frozen, source = list(id = "a")),
    frozen
  )
  plan <- list(
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      source_snapshot_identity = identity
    ))
  )
  wait_for_result <- function(worker) {
    deadline <- Sys.time() + 10
    repeat {
      polled <- builder_session_poll(worker, timeout = 100)
      worker <- polled$worker
      if (!is.null(polled$result)) {
        return(list(worker = worker, result = polled$result))
      }
      if (Sys.time() >= deadline) {
        stop(
          "Timed out waiting for the frozen-snapshot rejection.",
          call. = FALSE
        )
      }
    }
  }

  builder_session_build(worker, plan)
  direct <- wait_for_result(worker)
  worker <- direct$worker
  expect_true(direct$result$done)
  expect_match(
    direct$result$value$error,
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )

  protocol <- builder_enqueue(
    builder_request_protocol(worker$epoch),
    builder_command("build", "session", payload = list(id = "build-1"))
  )
  dispatched <- builder_protocol_dispatch(protocol)

  builder_session_build(worker, plan, dispatched$request)
  requested <- wait_for_result(worker)
  polled <- list(worker = requested$worker, result = requested$result)
  worker <- polled$worker

  expect_true(polled$result$done)
  expect_match(
    polled$result$value$error,
    "does not match the frozen BuildPlan",
    fixed = TRUE
  )
  expect_identical(polled$result$value$build_id, "build-1")
})

test_that("a worker rejects runtime drift before executing a Build", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  runtime_file <- file.path(root, "runtime.R")
  writeLines("runtime_value <- 1L", runtime_file, useBytes = TRUE)
  runtime_identity <- builder_runtime_test_env$builder_runtime_file_identity(
    runtime_file
  )
  session_env <- environment(builder_session_build)
  prior_identity <- get0(
    "builder_runtime_file_identity",
    envir = session_env,
    inherits = FALSE
  )
  prior_assert <- get0(
    "builder_runtime_assert_identity",
    envir = session_env,
    inherits = FALSE
  )
  assign(
    "builder_runtime_file_identity",
    builder_runtime_test_env$builder_runtime_file_identity,
    envir = session_env
  )
  assign(
    "builder_runtime_assert_identity",
    builder_runtime_test_env$builder_runtime_assert_identity,
    envir = session_env
  )
  withr::defer({
    if (is.null(prior_identity)) {
      rm("builder_runtime_file_identity", envir = session_env)
    } else {
      assign(
        "builder_runtime_file_identity",
        prior_identity,
        envir = session_env
      )
    }
    if (is.null(prior_assert)) {
      rm("builder_runtime_assert_identity", envir = session_env)
    } else {
      assign(
        "builder_runtime_assert_identity",
        prior_assert,
        envir = session_env
      )
    }
  })
  snapshot <- builder_snapshot_seurat(
    SeuratObject::pbmc_small,
    file.path(root, "dataset-a"),
    available_bytes = 2^40
  )

  worker <- builder_worker_start(
    builder_profile_inst_path("builder"),
    snapshot_root = root,
    snapshot_registry = list(`dataset-a` = snapshot)
  )
  withr::defer({
    try(builder_worker_stop(worker), silent = TRUE)
    if (isTRUE(.builder_snapshot_owned(snapshot))) {
      .builder_snapshot_release(snapshot)
    }
  })

  writeLines("runtime_value <- 2L", runtime_file, useBytes = TRUE)
  plan <- list(
    make_app = FALSE,
    app_auth = list(enabled = FALSE),
    items = list(list(
      id = "dataset-a",
      name = "Dataset A",
      source_snapshot_identity = c(
        list(
          available = TRUE,
          snapshot = snapshot,
          source = list(id = "dataset-a")
        ),
        snapshot
      )
    ))
  )
  builder_session_build(
    worker,
    plan,
    runtime_files = runtime_file,
    runtime_identity = runtime_identity
  )

  deadline <- Sys.time() + 20
  result <- NULL
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      result <- polled$result
      break
    }
    if (Sys.time() >= deadline) {
      stop("Timed out waiting for the runtime-drift rejection.", call. = FALSE)
    }
  }

  expect_true(result$done)
  expect_match(result$value$error, "runtime changed before worker build")
  expect_match(result$value$error, "Nothing was published", fixed = TRUE)
  expect_match(result$value$error, "runtime.R", fixed = TRUE)
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

test_that("worker failures recover or terminate every protocol barrier", {
  app <- paste(builder_app_lines(), collapse = "\n")

  expect_match(app, "builder_protocol_recover", fixed = TRUE)
  expect_match(app, 'identical(got$event, "restart_failed")', fixed = TRUE)
  expect_match(app, "retry_persistent = FALSE", fixed = TRUE)
  expect_match(app, "restart_worker_protocol <- function", fixed = TRUE)
  expect_gte(
    lengths(regmatches(
      app,
      gregexpr("restart_worker_protocol\\(", app, perl = TRUE)
    )),
    4L
  )

  register_failure <- builder_app_block(
    builder_app_lines(),
    "if (inherits(updated_worker, \"try-error\"))",
    "worker(updated_worker)"
  )
  release_failure <- builder_app_block(
    builder_app_lines(),
    "if (inherits(released, \"try-error\"))",
    "worker(released$worker)"
  )
  expect_match(
    app,
    "builder_snapshot_release_transition(",
    fixed = TRUE
  )
  expect_match(register_failure, "restart_worker_protocol(", fixed = TRUE)
  expect_match(release_failure, "restart_worker_protocol(", fixed = TRUE)
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

test_that("the App keeps Build execution private until the workflow reaches it", {
  lines <- builder_app_lines()
  app <- paste(lines, collapse = "\n")
  workbench <- builder_app_block(
    lines,
    "output$workbench <- renderUI({",
    "observeEvent(input$continue_to_review, {"
  )

  expect_false(grepl("builder_build_state", app, fixed = TRUE))
  expect_false(grepl("builder_reduce_build", app, fixed = TRUE))
  expect_false(grepl("observeEvent(input$cancel_build, {", app, fixed = TRUE))
  expect_false(grepl("builder_protocol_cancel", app, fixed = TRUE))
  expect_false(grepl('"cancel_build"', workbench, fixed = TRUE))
  expect_false(grepl('actionButton(\n      "build"', app, fixed = TRUE))
  expect_match(workbench, "build = render_build_workbench()", fixed = TRUE)
})

test_that("session shutdown stops the worker before releasing snapshots", {
  shutdown <- paste(
    readLines(
      builder_profile_inst_path("builder", "server", "foundation.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  shutdown <- substr(
    shutdown,
    regexpr("session$onSessionEnded(function() {", shutdown, fixed = TRUE)[[
      1L
    ]],
    nchar(shutdown)
  )
  stopped <- regexpr("builder_worker_stop", shutdown, fixed = TRUE)[1L]
  confirmed <- regexpr("isTRUE(stopped$stopped)", shutdown, fixed = TRUE)[1L]
  cleanup_safe <- regexpr(
    "isTRUE(stopped$worker$cleanup_safe)",
    shutdown,
    fixed = TRUE
  )[1L]
  released <- regexpr(".builder_snapshot_release", shutdown, fixed = TRUE)[1L]
  settlement_stop <- regexpr(
    "stop_builder_release_settlement()",
    shutdown,
    fixed = TRUE
  )[[1L]]
  release_abort <- regexpr(
    "builder_abort_active_release()",
    shutdown,
    fixed = TRUE
  )[[1L]]

  expect_gt(settlement_stop, 0L)
  expect_lt(settlement_stop, stopped)
  expect_gt(release_abort, 0L)
  expect_gt(stopped, 0L)
  expect_match(
    shutdown,
    "!length(current_worker$snapshot_registry %||% list())",
    fixed = TRUE
  )
  expect_gt(confirmed, stopped)
  expect_gt(cleanup_safe, confirmed)
  expect_gt(released, cleanup_safe)
  expect_gt(release_abort, released)
  expect_match(
    shutdown,
    "unlink(current_worker$snapshot_root, recursive = TRUE, force = TRUE)",
    fixed = TRUE
  )
  expect_match(
    shutdown,
    "stopped <- if (empty_worker)",
    fixed = TRUE
  )
  expect_match(
    shutdown,
    "builder_worker_stop(current_worker, grace_ms = 5000L)",
    fixed = TRUE
  )
  expect_match(
    shutdown,
    "if (isTRUE(settlement_stopped))",
    fixed = TRUE
  )
  expect_false(grepl("on.exit({\n    release <-", shutdown, fixed = TRUE))
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

  shiny::testServer(app_env$server, {
    request <- builder_command("build", "session", payload = list())
    queued <- builder_enqueue(
      builder_request_protocol("monitor-worker"),
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
    build_operation_phase("verify")

    expect_false(schedule_builder_release_settlement_poll(
      .schedule = function(...) stop("queue unavailable", call. = FALSE)
    ))
    expect_identical(result()$state, "recovery_required")
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(protocol()$build_status, "idle")
    expect_identical(
      release_settlement_context()$status,
      "monitor_failed"
    )
    expect_identical(release_settlement_process(), process)
    expect_match(busy_note(), "recovery is required", fixed = TRUE)
  })

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

test_that("terminal build dispatch paths unlock the Builder", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_profile_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this dispatch test.")
  }

  shiny::testServer(app_env$server, {
    request <- builder_command(
      "build",
      "session",
      payload = list(kind = "build")
    )
    queued <- builder_enqueue(
      builder_request_protocol("terminal-worker"),
      request
    )
    dispatched <- builder_protocol_dispatch(queued)
    build_flow(list(stage = "building", plan = NULL))
    busy_note("Starting build worker…")
    build_operation_phase("dispatch")

    expect_false(finish_builder_build_dispatch_failure(
      dispatched$protocol,
      dispatched$request,
      builder_result_failure("Dispatch failed.")
    ))
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_null(busy_note())
    expect_null(build_operation_phase())
    expect_identical(result()$state, "failure")
    expect_identical(protocol()$build_status, "idle")
    expect_null(protocol()$pending)

    build_flow(list(stage = "building", plan = NULL))
    result(NULL)
    settle_failed_builds(
      list(failed = list(dispatched$request)),
      "The worker stopped."
    )
    expect_identical(build_flow(), list(stage = "idle", plan = NULL))
    expect_identical(result()$state, "failure")
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

test_that("crash-recovery restarts boot the replacement in the background", {
  lines <- builder_app_lines()
  restart <- builder_app_block(
    lines,
    "restart_worker_protocol <- function(",
    "await_restarted_worker <- function("
  )
  expect_match(restart, ".async = TRUE,", fixed = TRUE)
  expect_match(restart, "verify_tree = !isTRUE(async)", fixed = TRUE)
  expect_match(
    restart,
    'isTRUE(recovered) && identical(restarted$state, "starting")',
    fixed = TRUE
  )
  expect_match(restart, "await_restarted_worker(", fixed = TRUE)

  poller <- builder_app_block(
    lines,
    'if (identical(got$event, "restarted")) {',
    'if (identical(got$event, "restart_failed")) {'
  )
  expect_match(
    poller,
    'identical(got$worker$state %||% NULL, "starting")',
    fixed = TRUE
  )
  expect_match(poller, "await_restarted_worker(", fixed = TRUE)

  startup <- builder_app_block(
    lines,
    "poll_builder_worker_startup <- function() {",
    "worker_available(TRUE)"
  )
  expect_match(
    startup,
    "!builder_protocol_is_quiescent(current_protocol)",
    fixed = TRUE
  )
  expect_match(startup, "retry_persistent = FALSE", fixed = TRUE)
})
