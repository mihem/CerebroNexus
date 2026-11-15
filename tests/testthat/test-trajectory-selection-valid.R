## Guard that keeps trajectory outputs from calling getTrajectory() with a method
## that belongs to a previously-loaded dataset. On a dataset switch the Shiny
## input `trajectory_selected_method` keeps its old value (e.g. "monocle2") until
## the selector round-trips; req() on a bare string passes, so getTrajectory()
## throws "Method `monocle2` is not available." for a dataset without that
## method. The pure predicate below is req()-ed at every getTrajectory() site so
## the guard fails cleanly instead.

load_predicate <- function() {
  env <- new.env()
  viewer_runtime_env(env)
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

test_that("Trajectory first frame uses the values its selectors bind with", {
  withr::local_package("shiny")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      viewer_runtime_env(scope)
      getMethodsForTrajectories <- function() c("unknown", "paga", "monocle2")
      getNamesOfTrajectories <- function(method) {
        switch(method, paga = c("p1", "p2"), monocle2 = "m1")
      }
      viewerProjectionFirstFrameMetadata <- function() {
        data.frame(sample = "a", cluster = "b")
      }
      getGroups <- function() c("sample", "cluster")
      getParameters <- function() list(main_group = "cluster")
      viewer_server_load(
        viewer_server_statements("trajectory", "server.R"),
        scope,
        names = c(
          "trajectory_selector_value",
          "trajectory_default_method",
          "trajectory_default_name",
          "trajectory_default_point_color",
          "trajectory_selected_method",
          "trajectory_selected_name",
          "trajectory_point_color"
        )
      )
      sys.source(
        viewer_test_path("trajectory", "select_method_and_name.R"),
        scope
      )
      runs <- 0L
      observe({
        trajectory_selected_method()
        trajectory_selected_name()
        trajectory_point_color()
        runs <<- runs + 1L
      })
    },
    {
      session$flushReact()
      ## Before the selectors reach the browser: what they will bind with.
      expect_identical(trajectory_selected_method(), "paga")
      expect_identical(trajectory_selected_name(), "p1")
      expect_identical(trajectory_point_color(), "cluster")
      html <- output$trajectory_primary_controls_UI$html
      expect_match(html, '<option value="paga" selected>', fixed = TRUE)
      expect_match(html, '<option value="p1" selected>', fixed = TRUE)
      expect_match(html, '<option value="cluster" selected>', fixed = TRUE)
      ## Their binding with those values invalidates nothing.
      runs_before <- scope$runs
      expect_gt(runs_before, 0L)
      session$setInputs(
        trajectory_selected_method = "paga",
        trajectory_selected_name = "p1",
        trajectory_point_color = "cluster"
      )
      expect_identical(scope$runs, runs_before)
    }
  )
})
