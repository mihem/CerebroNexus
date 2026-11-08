builder_profile_source_runtime(globalenv())
builder_repo_source("loading.R", local = globalenv())
builder_repo_source("worker.R", local = globalenv())
builder_repo_source("session.R", local = globalenv())

test_that("failed import collection requests one full GC", {
  full <- logical()

  .builder_session_collect_failed_import(function(...) {
    args <- list(...)
    full <<- c(full, isTRUE(args$full))
    invisible(NULL)
  })

  expect_identical(full, TRUE)
})

test_that("session import collects only after failure", {
  response <- NULL
  process <- list(call = function(fun, args) {
    response <<- do.call(fun, args)
    invisible(NULL)
  })
  calls <- 0L
  session_env <- environment(builder_session_load)
  original <- get(".builder_session_collect_failed_import", envir = session_env)
  assign(
    ".builder_session_collect_failed_import",
    function(...) calls <<- calls + 1L,
    envir = session_env
  )
  withr::defer(assign(
    ".builder_session_collect_failed_import",
    original,
    envir = session_env
  ))

  builder_session_load(
    process,
    "bad",
    "unused.rds",
    .importer = function(...) stop("not a Seurat object", call. = FALSE)
  )

  expect_identical(calls, 1L)
  expect_identical(response$error, "not a Seurat object")

  calls <- 0L
  builder_session_load(
    process,
    "good",
    "unused.rds",
    .importer = function(...) list(ok = TRUE)
  )

  expect_identical(calls, 0L)
  expect_null(response$error)

  cancelled <- structure(
    list(message = "cancelled", call = NULL),
    class = c("interrupt", "condition")
  )
  ## Caught right here: an interrupt that reaches testthat stops the run.
  caught <- tryCatch(
    builder_session_load(
      process,
      "cancelled",
      "unused.rds",
      .importer = function(...) stop(cancelled)
    ),
    interrupt = identity
  )
  expect_s3_class(caught, "interrupt")
  expect_identical(calls, 1L)
})

test_that("real worker fully collects after rejecting a non-Seurat RDS", {
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  path <- file.path(root, "not-seurat.rds")
  saveRDS(seq_len(100), path)
  started <- builder_session_start(
    builder_profile_inst_path("builder"),
    snapshot_root = file.path(root, "snapshots")
  )
  expect_null(started$error)
  worker <- started$worker
  withr::defer(try(builder_worker_stop(worker), silent = TRUE))
  marker <- file.path(root, "full-gc-called")
  session_env <- environment(builder_session_load)
  original <- get(".builder_session_collect_failed_import", envir = session_env)
  assign(
    ".builder_session_collect_failed_import",
    local({
      marker_path <- marker
      collect <- original
      function() {
        writeLines("full", marker_path)
        collect()
      }
    }),
    envir = session_env
  )
  withr::defer(assign(
    ".builder_session_collect_failed_import",
    original,
    envir = session_env
  ))

  builder_session_load(worker, "bad", path)
  deadline <- Sys.time() + 10
  repeat {
    polled <- builder_session_poll(worker, timeout = 100)
    worker <- polled$worker
    if (!is.null(polled$result)) {
      break
    }
    if (Sys.time() >= deadline) {
      stop("The non-Seurat import did not finish.", call. = FALSE)
    }
  }

  expect_identical(
    polled$result$value$error,
    "The file contains a integer object, not a Seurat object."
  )
  expect_identical(readLines(marker, warn = FALSE), "full")
})
