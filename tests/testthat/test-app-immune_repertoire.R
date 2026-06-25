# test-app-immune_repertoire.R — shinytest2 integration tests for immune repertoire module
#
# The example dataset now ships with real TCR data, so the immune repertoire
# tab is present by default and its UI can be exercised directly.

library(shinytest2)

inst_dir <- system.file(package = "cerebroAppLite")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

test_that("immune_repertoire tab is present with example data (has TCR)", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_present", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # example.crb carries real TCR data — the conditional tab should appear
  tab_present <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\') !== null;'
  )
  expect_true(tab_present)

  app$stop()
})

test_that("immune_repertoire tab can be opened and renders settings", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_open", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # select the tab
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  # the chain selector (a core settings control) should be populated
  chain_present <- app$get_js(
    'document.querySelector("#ir_chain") !== null;'
  )
  expect_true(chain_present)

  app$stop()
})

test_that("clonal scatter renders without error in default and grouped states", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_scatter", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  err_pat <- "clonalScatter|getlindex|get1index|undefined columns|names.*attribute"

  # default state: example is split into >= 2 samples, scatter should render
  v1 <- app$get_value(output = "ir_plot_clonalScatter")
  expect_false(isTRUE(grepl(err_pat, v1$html, ignore.case = TRUE)))

  # grouped + re-split state (the combination that previously errored)
  app$set_inputs(ir_groupBy = "seurat_clusters", wait_ = FALSE)
  app$set_inputs(ir_sampleCol = "cell_type", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)
  v2 <- app$get_value(output = "ir_plot_clonalScatter")
  expect_false(isTRUE(grepl(err_pat, v2$html, ignore.case = TRUE)))

  app$stop()
})

test_that("immune_repertoire module loads without breaking main app", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_load", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # Data info tab should still render normally (1476 cells in the new example)
  cells_box <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("1,?476", cells_box$html))

  app$stop()
})
