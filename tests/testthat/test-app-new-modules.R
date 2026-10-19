# shinytest2 integration tests for enhanced Viewer modules

skip_if_not_installed("shinytest2")
library(shinytest2)

inst_dir <- system.file(package = "CerebroNexus")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

## Booting the app dominates these recordings: every test below only reads from
## the DOM of the same freshly-loaded example.crb, so a driver per test spent
## almost all its time on startup. They share one driver instead, created on
## first use and stopped when the file finishes.
##
## Sharing is safe here because each test navigates to the tab it asserts on
## rather than relying on where a previous test left the app. Keep that property
## when adding a test: if a new test needs a pristine app (e.g. asserting that
## something has NOT been loaded yet), give it its own AppDriver instead.
shared_driver <- NULL
shared_driver_env <- environment()
shared_app <- function() {
  if (is.null(shared_driver)) {
    local_app_support(inst_dir, envir = shared_driver_env)
    shared_driver <<- AppDriver$new(
      inst_dir,
      name = "new_modules_shared",
      height = 950,
      width = 1619
    )
    withr::defer(shared_driver$stop(), envir = shared_driver_env)
    shared_driver$wait_for_idle(timeout = 20000)
  }
  shared_driver
}

test_that("most_expressed_genes tab navigates and renders table", {
  app <- shared_app()

  # Most expressed genes is a conditionally shown sidebar item. Wait for its
  # menu link to become available, then click it,
  # so the tab activates on a slow CI runner instead of navigating too early.
  app$wait_for_js(
    "document.querySelector('a[href=\"#shiny-tab-mostExpressedGenes\"]') !== null",
    timeout = 20000
  )
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-mostExpressedGenes"]\').click();'
  )
  app$wait_for_idle(timeout = 10000)

  # Group selector renders with expected options
  select_html <- app$get_value(
    output = "most_expressed_genes_select_group_UI"
  )$html
  expect_true(grepl("seurat_clusters", select_html))

  # Table renders without error
  table_html <- app$get_value(
    output = "most_expressed_genes_table_or_text_UI"
  )$html
  expect_false(grepl("no.*available|not.*found|error", tolower(table_html)))
})

test_that("enriched_pathways tab content exists in DOM", {
  app <- shared_app()

  # Output container exists in the DOM
  has_div <- app$get_js(
    'document.getElementById("enriched_pathways_select_method_and_table_UI") !== null;'
  )
  expect_true(has_div)
})

test_that("extra_material tab content exists in DOM", {
  app <- shared_app()

  # Output container exists in the DOM
  has_div <- app$get_js(
    'document.getElementById("extra_material_select_category_and_content_UI") !== null;'
  )
  expect_true(has_div)
})

test_that("all three new tabs are visible in sidebar after data load", {
  app <- shared_app()

  most_expressed <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-mostExpressedGenes"]\') !== null;'
  )
  expect_true(most_expressed)

  enriched_pw <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-enrichedPathways"]\') !== null;'
  )
  expect_true(enriched_pw)

  extra_mat <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-extra_material"]\') !== null;'
  )
  expect_true(extra_mat)
})
