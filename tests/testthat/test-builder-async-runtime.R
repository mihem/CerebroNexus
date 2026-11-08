## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

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
