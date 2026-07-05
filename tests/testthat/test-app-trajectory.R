# test-app-trajectory.R — shinytest2 integration tests for trajectory module
#
# The bundled app loads four demo data sets; only the fourth
# ("PBMC - Monocle2 trajectory" -> demo_trajectory.crb) carries trajectory
# data, so the Trajectory tab is conditionally inserted only after switching
# to it. The default landing data set (demo_full_tcr_bcr) has no trajectory.

library(shinytest2)

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
inst_dir <- inst_candidates[file.exists(file.path(inst_candidates, "app.R"))][1]
if (is.na(inst_dir)) {
  inst_dir <- system.file(package = "cerebroAppLite")
}
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

trajectory_crb <- "extdata/v1.4/demo_trajectory.crb"

test_that("Trajectory tab appears after switching to the monocle2 data set", {
  app <- AppDriver$new(
    inst_dir,
    name = "trajectory_visible",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  # Default data set has no trajectory data -> tab absent.
  tab_before <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-trajectory"]\') !== null;'
  )
  expect_false(tab_before)

  # Switch to the monocle2 trajectory demo -> conditional tab inserted.
  app$set_inputs(crb_file_selector = trajectory_crb, wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  tab_after <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-trajectory"]\') !== null;'
  )
  expect_true(tab_after)
})

test_that("trajectory module loads without breaking the main app", {
  app <- AppDriver$new(
    inst_dir,
    name = "trajectory_load",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  # Switch to the trajectory demo (501 cells) and confirm the Data info tab
  # still renders its cell count normally.
  app$set_inputs(crb_file_selector = trajectory_crb, wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  cells_box <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("501", cells_box$html))
})
