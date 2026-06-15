# test-app-immune_repertoire.R — shinytest2 integration tests for immune repertoire module

library(shinytest2)

inst_dir <- system.file(package = "cerebroAppLite")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

test_that("immune_repertoire tab hidden with example data (no TCR)", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_hidden", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # Default example.crb has no immune repertoire data — tab should be absent
  tab_absent <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\') === null;'
  )
  expect_true(tab_absent)

  app$stop()
})

test_that("immune_repertoire module loads without breaking main app", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_load", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # Data info tab should still render normally
  cells_box <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("501", cells_box$html))

  app$stop()
})
