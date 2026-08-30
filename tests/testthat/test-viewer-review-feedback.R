viewer_source <- function(...) {
  viewer_root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(viewer_root)) {
    viewer_root <- testthat::test_path("../../inst/viewer")
  }
  paste(readLines(file.path(viewer_root, ...), warn = FALSE), collapse = "\n")
}

viewer_path <- function(...) {
  viewer_root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(viewer_root)) {
    viewer_root <- testthat::test_path("../../inst/viewer")
  }
  file.path(viewer_root, ...)
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

test_that("Gene expression display and colour modes are linked", {
  display_ui <- viewer_source(
    "gene_expression",
    "UI_projection_genes_separate_panels.R"
  )
  colour_ui <- viewer_source(
    "gene_expression",
    "UI_projection_gene_color_mode.R"
  )

  css <- viewer_source("www", "custom.css")

  expect_match(display_ui, 'label = "Display mode"', fixed = TRUE)
  expect_match(display_ui, '"Mean expression" = "combined"', fixed = TRUE)
  expect_match(display_ui, '"Separate panels" = "separate"', fixed = TRUE)
  expect_match(display_ui, '"RGB co-expression" = "rgb"', fixed = TRUE)
  expect_match(colour_ui, 'label = "Panel colors"', fixed = TRUE)
  expect_match(colour_ui, '"Shared scale" = "shared"', fixed = TRUE)
  expect_match(colour_ui, '"Distinct colors" = "different"', fixed = TRUE)
  expect_match(colour_ui, 'display_mode != "separate"', fixed = TRUE)
  expect_match(
    colour_ui,
    "Choose Separate panels to configure panel colors.",
    fixed = TRUE
  )
  expect_match(css, "@keyframes cerebro-control-unlock", fixed = TRUE)
  expect_match(css, "prefers-reduced-motion: reduce", fixed = TRUE)
})

test_that("Gene expression panels share one global numeric range", {
  env <- new.env(parent = globalenv())
  sys.source(
    viewer_path("gene_expression", "func_color_scale.R"),
    envir = env
  )

  levels <- list(GeneA = c(0, 1), GeneB = c(2, 7))
  expect_equal(env$expressionValueRange(levels), c(0, 7))
  expect_equal(env$expressionValueRange(c(0.2, 0.9)), c(0.2, 0.9))
})

test_that("Different gene colours are stable and visually distinct", {
  root <- system.file("viewer", package = "CerebroNexus")
  if (!nzchar(root)) {
    root <- testthat::test_path("../../inst/viewer")
  }
  env <- new.env(parent = globalenv())
  sys.source(
    file.path(root, "gene_expression", "func_color_scale.R"),
    envir = env
  )

  genes <- paste0("Gene", seq_len(9))
  shared <- env$expressionPanelColorScales(genes, "shared", "Cerebro orange")
  different <- env$expressionPanelColorScales(
    genes,
    "different",
    "Cerebro orange"
  )

  scale_key <- function(scale) paste(capture.output(dput(scale)), collapse = "")
  expect_length(unique(vapply(shared, scale_key, character(1))), 1)
  expect_length(unique(vapply(different, scale_key, character(1))), 9)
  expect_identical(
    different,
    env$expressionPanelColorScales(genes, "different", "Cerebro orange")
  )
})

test_that("Canvas renderer accepts a palette per gene panel", {
  js <- viewer_source("www", "cell_views.js")
  css <- viewer_source("www", "coordviews.css")

  expect_match(js, "data.panel_colorscales", fixed = TRUE)
  expect_match(js, "panelScale", fixed = TRUE)
  expect_match(js, "Shared expression range", fixed = TRUE)
  expect_match(js, "cv-panel-scale", fixed = TRUE)
  expect_match(
    css,
    ".coordviews-page .cv-focus-btn {\n  position: absolute;",
    fixed = TRUE
  )
  expect_match(css, ".coordviews-page .cv-panel-scale", fixed = TRUE)
})
