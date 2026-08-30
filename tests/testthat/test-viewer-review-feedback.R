viewer_source <- function(...) {
  viewer_root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(viewer_root)) {
    viewer_root <- testthat::test_path("../../inst/viewer")
  }
  path <- file.path(viewer_root, ...)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("Projection defaults to cell type when available", {
  ui <- viewer_source("overview", "UI_projection_main_parameters.R")

  expect_match(ui, '"cell_type" %in% color_choices', fixed = TRUE)
})

test_that("Projection does not offer an unimplemented square option", {
  ui <- viewer_source("overview", "UI_projection_additional_parameters.R")
  css <- viewer_source("www", "custom.css")

  expect_no_match(ui, "overview_projection_keep_square", fixed = TRUE)
  expect_no_match(css, "cerebro-square-option", fixed = TRUE)
})

test_that("Standalone cell views cannot enter linked-view focus", {
  js <- viewer_source("www", "cell_views.js")

  expect_match(js, "function canFocusPanel", fixed = TRUE)
  expect_match(js, "return !singleActive;", fixed = TRUE)
  expect_match(js, "if (!canFocusPanel()) return;", fixed = TRUE)
})

test_that("Settings drawer pickers reserve space for labels and buttons", {
  css <- viewer_source("www", "custom.css")

  expect_match(
    css,
    ".cerebro-settings-drawer .bootstrap-select.form-control {\n  height: auto !important;",
    fixed = TRUE
  )
  expect_match(
    css,
    ".cerebro-settings-drawer .form-group {\n  margin-bottom: 16px;",
    fixed = TRUE
  )
})
