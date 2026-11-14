## Work that keeps something for the next start waits for the sessions to be
## quiet (R/quiet_queue.R).

reset_quiet_queue <- function() {
  .viewer_quiet$queue <- list()
  .viewer_quiet$polling <- FALSE
  .viewer_quiet$since <- NULL
}

wait_for <- function(condition, seconds) {
  end <- Sys.time() + seconds
  while (!condition() && Sys.time() < end) {
    later::run_now(0.05)
  }
}

test_that("a task waits while a session is busy, then runs once, in order", {
  reset_quiet_queue()
  withr::defer(reset_quiet_queue())
  ran <- character()
  .viewerWhenQuiet(function() ran <<- c(ran, "first"), quiet = 0.6)
  .viewerWhenQuiet(function() stop("a task that fails"), quiet = 0.6)
  .viewerWhenQuiet(function() ran <<- c(ran, "third"), quiet = 0.6)

  ## a session that reports every 0.2 s keeps them waiting
  end <- Sys.time() + 1.5
  while (Sys.time() < end) {
    .viewerSessionActivity()
    later::run_now(0.2)
  }
  expect_identical(ran, character())

  ## quiet: every task runs, a failing one does not stop the next
  wait_for(function() length(ran) == 2L, 5)
  expect_identical(ran, c("first", "third"))
  wait_for(function() !.viewer_quiet$polling, 5)
  expect_false(.viewer_quiet$polling)
  expect_length(.viewer_quiet$queue, 0L)
})

test_that("without a session a task runs once it has waited as long", {
  reset_quiet_queue()
  withr::defer(reset_quiet_queue())
  ran <- FALSE
  queued <- Sys.time()
  .viewerWhenQuiet(function() ran <<- TRUE, quiet = 0.5)
  wait_for(function() ran, 5)
  expect_true(ran)
  expect_gte(as.numeric(difftime(Sys.time(), queued, units = "secs")), 0.5)
})

test_that("the Viewer keeps its page and its document through the queue", {
  launch <- deparse(body(launchCerebro))
  expect_true(any(grepl(".viewerWhenQuiet(function()", launch, fixed = TRUE)))
  ui_cache <- readLines(viewer_test_path("ui_cache.R"))
  expect_true(any(grepl(".viewerWhenQuiet(keep)", ui_cache, fixed = TRUE)))
})
