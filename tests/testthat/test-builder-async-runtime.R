test_that("Builder async runtime owns one bounded callr pool", {
  path <- builder_profile_inst_path("builder", "async.R")
  expect_true(file.exists(path))

  runtime <- new.env(parent = globalenv())
  sys.source(path, envir = runtime)
  on.exit(runtime$builder_async_stop(), add = TRUE)

  task <- runtime$builder_async_submit(quote(value + 1L), list(value = 1L))
  resolved <- NULL
  runtime$builder_async_then(
    task,
    session = NULL,
    on_fulfilled = function(value) resolved <<- value,
    on_rejected = function(error) stop(error)
  )
  deadline <- Sys.time() + 5
  while (is.null(resolved) && Sys.time() < deadline) {
    later::run_now(0.1)
  }

  expect_identical(resolved, 2L)
  expect_identical(runtime$.builder_async$n, 2L)
  expect_identical(runtime$.builder_async$memory, 64L)
})

test_that("long read-only jobs and builds remain event-driven", {
  source_text <- function(...) {
    paste(
      readLines(builder_profile_inst_path("builder", ...), warn = FALSE),
      collapse = "\n"
    )
  }
  function_text <- function(text, start, end) {
    start_at <- regexpr(start, text, fixed = TRUE)[[1L]]
    expect_gt(start_at, 0L)
    remainder <- substr(text, start_at, nchar(text))
    end_at <- regexpr(end, remainder, perl = TRUE)[[1L]]
    expect_gt(end_at, 0L)
    substr(remainder, 1L, end_at - 1L)
  }

  enhancements <- function_text(
    source_text("server", "enhancements.R"),
    "start_enhance_table_inventory <- function",
    "observeEvent\\(input\\[\\[\"enhance-table_files"
  )
  project <- function_text(
    source_text("server", "project.R"),
    "builder_project_start_open <- function",
    "invalidate_builder_project_source_sync <- function"
  )
  build <- function_text(
    source_text("server", "build.R"),
    "start_builder_new_release <- function",
    "show_builder_build_server_path <- function"
  )

  for (text in list(enhancements, project)) {
    expect_match(text, "builder_async_submit", fixed = TRUE)
    expect_match(text, "builder_async_then", fixed = TRUE)
  }
  expect_match(build, "builder_build_output_preflight(parent)", fixed = TRUE)
  expect_match(build, "schedule_selected_output_build(target)", fixed = TRUE)
  build_schedule <- function_text(
    source_text("server", "build.R"),
    "schedule_selected_output_build <- function",
    "start_confirmed_build <- function"
  )
  expect_match(build_schedule, "later::later(", fixed = TRUE)
  expect_match(
    enhancements,
    "builder_table_inventory_owner(",
    fixed = TRUE
  )
  expect_match(
    enhancements,
    "builder_table_inventory_owner_is_current(",
    fixed = TRUE
  )
  expect_lt(
    regexpr(
      "builder_table_inventory_owner_is_current(",
      enhancements,
      fixed = TRUE
    )[[1L]],
    regexpr("add_enhance_table_files(", enhancements, fixed = TRUE)[[1L]]
  )
  expect_false(grepl("callr::r_bg", enhancements, fixed = TRUE))
  expect_false(grepl(
    "builder_project_schedule_open_poll",
    project,
    fixed = TRUE
  ))
  expect_false(grepl("builder_project_poll_open", project, fixed = TRUE))
  expect_false(grepl(
    "builder_poll_build_output_preflight",
    build,
    fixed = TRUE
  ))
})

test_that("async callback failures use the task rejection path", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "async.R"),
    envir = runtime
  )
  on.exit(runtime$builder_async_stop(), add = TRUE)
  rejected <- NULL
  task <- runtime$builder_async_submit(quote(1L))
  runtime$builder_async_then(
    task,
    session = NULL,
    on_fulfilled = function(value) stop("callback failed"),
    on_rejected = function(error) rejected <<- conditionMessage(error)
  )
  deadline <- Sys.time() + 2
  while (is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.05)
  }

  expect_identical(rejected, "callback failed")
})

test_that("a broken rejection callback cannot poison shared delivery", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "async.R"),
    envir = runtime
  )
  on.exit(runtime$builder_async_stop(), add = TRUE)

  make_ready <- function(state, value, fulfilled, rejected) {
    task <- new.env(parent = emptyenv())
    task$state <- state
    task$error <- if (identical(state, "failed")) value else NULL
    task$result <- if (identical(state, "completed")) value else NULL
    task$session <- NULL
    task$on_fulfilled <- fulfilled
    task$on_rejected <- rejected
    task$callbacks_registered <- TRUE
    task$callback_error <- NULL
    class(task) <- "builder_async_task"
    task
  }
  bad <- make_ready(
    "failed",
    simpleError("task failed"),
    identity,
    function(error) stop("rejection callback failed")
  )
  delivered <- NULL
  good <- make_ready(
    "completed",
    42L,
    function(value) delivered <<- value,
    identity
  )
  runtime$.builder_async$tasks <- list(bad, good)

  expect_message(
    runtime$builder_async_poll(),
    "Builder asynchronous rejection callback failed"
  )

  expect_identical(delivered, 42L)
  expect_length(runtime$.builder_async$tasks, 0L)
  expect_identical(
    runtime$.builder_async$last_callback_error$message,
    "rejection callback failed"
  )
  expect_identical(bad$callback_error$phase, "rejection")
})

test_that("stopping invalidates an already scheduled poll", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "async.R"),
    envir = runtime
  )
  on.exit(runtime$builder_async_stop(), add = TRUE)

  runtime$.builder_async$poll_delay <- 0.05
  expect_true(runtime$.builder_async_request_poll())
  expect_false(runtime$builder_async_stop())

  polls <- 0L
  runtime$builder_async_poll <- function() {
    polls <<- polls + 1L
  }
  later::run_now(0.1)

  expect_identical(polls, 0L)
  expect_false(runtime$.builder_async$poll_scheduled)
})

test_that("a failed task kill remains owned until the process is reaped", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "async.R"),
    envir = runtime
  )
  on.exit(runtime$builder_async_stop(), add = TRUE)

  alive <- TRUE
  kills <- 0L
  process <- list(
    is_alive = function() alive,
    kill_tree = function() {
      kills <<- kills + 1L
      invisible(FALSE)
    },
    kill = function() {
      kills <<- kills + 1L
      invisible(FALSE)
    },
    wait = function(timeout) invisible(FALSE)
  )
  task <- new.env(parent = emptyenv())
  task$state <- "running"
  task$process <- process
  task$session <- NULL
  task$cancel_started <- NULL
  task$cancel_deadline <- NULL
  task$cancel_error <- NULL
  class(task) <- "builder_async_task"
  runtime$.builder_async$tasks <- list(task)

  expect_false(runtime$builder_async_cancel(task))
  expect_identical(task$state, "cancelling")
  expect_identical(runtime$.builder_async$tasks, list(task))
  expect_gt(kills, 0L)

  expect_false(runtime$builder_async_stop())
  expect_false(runtime$.builder_async$last_stop$stopped)
  expect_identical(runtime$.builder_async$last_stop$remaining, 1L)
  expect_identical(runtime$.builder_async$tasks, list(task))

  alive <- FALSE
  runtime$builder_async_poll()
  expect_length(runtime$.builder_async$tasks, 0L)
  expect_true(runtime$.builder_async$last_stop$stopped)
})

test_that("fulfillment callback failures are recorded before rejection", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "async.R"),
    envir = runtime
  )
  on.exit(runtime$builder_async_stop(), add = TRUE)

  task <- new.env(parent = emptyenv())
  task$state <- "completed"
  task$error <- NULL
  task$result <- 42L
  task$session <- NULL
  task$on_fulfilled <- function(value) stop("fulfillment callback failed")
  rejected <- NULL
  task$on_rejected <- function(error) rejected <<- conditionMessage(error)
  task$callbacks_registered <- TRUE
  task$callback_error <- NULL
  class(task) <- "builder_async_task"
  runtime$.builder_async$tasks <- list(task)

  expect_message(
    runtime$builder_async_poll(),
    "Builder asynchronous fulfillment callback failed"
  )

  expect_identical(rejected, "fulfillment callback failed")
  expect_identical(task$callback_error$phase, "fulfillment")
  expect_identical(
    runtime$.builder_async$last_callback_error$message,
    "fulfillment callback failed"
  )
})

test_that("a process spawn failure rejects once without poisoning the queue", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "async.R"),
    envir = runtime
  )
  on.exit(runtime$builder_async_stop(), add = TRUE)
  runtime$.builder_async$spawn <- function(...) {
    stop("synthetic spawn failure")
  }

  task <- runtime$builder_async_submit(quote(1L))
  rejected <- NULL
  runtime$builder_async_then(
    task,
    session = NULL,
    on_fulfilled = function(value) stop("unexpected success"),
    on_rejected = function(error) rejected <<- conditionMessage(error)
  )
  later::run_now(0.1)

  expect_identical(rejected, "synthetic spawn failure")
  expect_length(runtime$builder_async_active(), 0L)
  expect_length(runtime$.builder_async$tasks, 0L)
})

test_that("async tasks share one bounded adaptive poll loop", {
  source <- paste(
    readLines(
      builder_profile_inst_path("builder", "async.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, ".builder_async$poll_scheduled", fixed = TRUE)
  expect_match(source, "builder_async_poll <- function()", fixed = TRUE)
  expect_match(source, "min(0.2", fixed = TRUE)
  expect_false(grepl(
    "(^|\\n)[[:space:]]*poll <- function\\(\\)",
    source,
    perl = TRUE
  ))
  expect_false(grepl("later::later(poll, 0.01)", source, fixed = TRUE))
})

test_that("Builder declares only its callr async runtime", {
  description_path <- builder_test_description_path()
  description <- read.dcf(description_path)[1L, ]
  imports <- strsplit(description[["Imports"]], ",", fixed = TRUE)[[1L]]
  imports <- trimws(sub("[[:space:]]*\\(.*$", "", imports))
  expect_true(all(c("callr", "digest", "processx", "ps") %in% imports))
  expect_false(grepl(
    "callr|processx|(^|,)[[:space:]]*ps([[:space:]]|,|$)",
    description[["Suggests"]],
    perl = TRUE
  ))
  # Viewer retains its existing mirai prefetch runtime. Builder owns callr.

  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(app, 'source("async.R", local = TRUE)', fixed = TRUE)
})
