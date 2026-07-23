# test-app-new-modules.R — shinytest2 integration tests for PR2 enhanced modules

library(shinytest2)

inst_dir <- system.file(package = "CerebroNexus")
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

test_that("most_expressed_genes tab navigates and renders table", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "most_expressed_genes",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  # Most expressed genes is now a conditionally + asynchronously inserted sidebar
  # item (insertConditionalTab). Wait for its menu link to appear, then click it,
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
  table_html <- app$get_value(output = "most_expressed_genes_table_UI")$html
  expect_false(grepl("no.*available|not.*found|error", tolower(table_html)))

  app$stop()
})


test_that("enriched_pathways tab content exists in DOM", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "enriched_pathways",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  # Output container exists in the DOM
  has_div <- app$get_js(
    'document.getElementById("enriched_pathways_select_method_and_table_UI") !== null;'
  )
  expect_true(has_div)

  app$stop()
})


test_that("extra_material tab content exists in DOM", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "extra_material",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

  # Output container exists in the DOM
  has_div <- app$get_js(
    'document.getElementById("extra_material_select_category_and_content_UI") !== null;'
  )
  expect_true(has_div)

  app$stop()
})


test_that("all three new tabs are visible in sidebar after data load", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "new_tabs_sidebar",
    height = 950,
    width = 1619
  )
  app$wait_for_idle(timeout = 20000)

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

  app$stop()
})

test_that("insertConditionalTab is defined and wired to conditional tabs", {
  server_file <- file.path(inst_dir, "shiny/v1.4/shiny_server.R")
  skip_if_not(file.exists(server_file))
  content <- paste(readLines(server_file), collapse = "\n")

  # Function is defined with all required parameters
  expect_match(
    content,
    "insertConditionalTab\\s*<-\\s*function\\s*\\(\\s*tab_label\\s*,\\s*tab_name\\s*,\\s*icon_name\\s*,\\s*check_fn",
    perl = TRUE
  )

  # Calls are present for enriched pathways and extra material
  expect_match(
    content,
    "insertConditionalTab\\s*\\([\\s\\S]*?Enriched pathways",
    perl = TRUE
  )
  expect_match(
    content,
    "insertConditionalTab\\s*\\([\\s\\S]*?Extra material",
    perl = TRUE
  )
})

test_that("conditional tab placeholders exist in UI source", {
  ui_file <- file.path(inst_dir, "shiny/v1.4/shiny_UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")

  # Placeholder divs for the two conditional tabs
  expect_match(
    content,
    'sidebar_item_enriched_pathways_placeholder',
    perl = TRUE
  )
  expect_match(content, 'sidebar_item_extra_material_placeholder', perl = TRUE)
})
