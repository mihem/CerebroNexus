## Guard that keeps trajectory outputs from calling getTrajectory() with a method
## that belongs to a previously-loaded dataset. On a dataset switch the Shiny
## input `trajectory_selected_method` keeps its old value (e.g. "monocle2") until
## the selector round-trips; req() on a bare string passes, so getTrajectory()
## throws "Method `monocle2` is not available." for a dataset without that
## method. The pure predicate below is req()-ed at every getTrajectory() site so
## the guard fails cleanly instead.

repo_file <- function(...) {
  parts <- c(...)
  stripped <- if (length(parts) && identical(parts[[1L]], "inst")) {
    parts[-1L]
  } else {
    parts
  }
  # 1) installed package (R CMD check) or load_all-shimmed source location
  if (length(stripped)) {
    p <- system.file(
      do.call(file.path, as.list(stripped)),
      package = "CerebroNexus"
    )
    if (nzchar(p)) {
      return(p)
    }
  }
  # 2) fall back to the source tree (devtools::test_dir run from the repo)
  testthat::test_path("..", "..", ...)
}

load_predicate <- function() {
  env <- new.env()
  sys.source(
    repo_file("inst", "shiny", "v1.4", "utility_functions.R"),
    envir = env
  )
  env[["trajectorySelectionValid"]]
}

test_that("a method/name valid for the current dataset passes", {
  ok <- load_predicate()
  expect_true(ok("monocle2", "traj_A", c("monocle2"), c("traj_A", "traj_B")))
})

test_that("a stale method not in the current dataset fails", {
  ok <- load_predicate()
  # The reproduced bug: input still "monocle2" but the switched-to dataset has
  # no trajectory methods at all.
  expect_false(ok("monocle2", "traj_A", character(0), character(0)))
  expect_false(ok("monocle2", "traj_A", NULL, NULL))
})

test_that("a method present but a name not in it fails", {
  ok <- load_predicate()
  expect_false(ok("monocle2", "gone", c("monocle2"), c("traj_A")))
})

test_that("NULL / empty method or name fails", {
  ok <- load_predicate()
  expect_false(ok(NULL, "traj_A", c("monocle2"), c("traj_A")))
  expect_false(ok("monocle2", NULL, c("monocle2"), c("traj_A")))
  expect_false(ok("", "traj_A", c("monocle2"), c("traj_A")))
})
