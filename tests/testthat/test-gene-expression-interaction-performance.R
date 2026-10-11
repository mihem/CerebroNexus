test_that("Gene expression hydrates auxiliary cell data only on interaction", {
  source <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(source, "function requestSingleAux()", fixed = TRUE)
  expect_false(grepl(
    "p.canvas.addEventListener('pointerenter', requestSingleAux)",
    source,
    fixed = TRUE
  ))
  expect_match(source, "requestSingleAux\\(\\);\\s*setHoverCell\\(i\\);")
  expect_false(grepl("scheduleSingleAux();", source, fixed = TRUE))
  expect_false(grepl("}, 5000);", source, fixed = TRUE))
})

test_that("Gene selection settles before expression extraction starts", {
  selected <- paste(
    readLines(
      viewer_test_path("gene_expression", "obj_selected_genes.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    selected,
    "expression_selected_genes_input <- reactive",
    fixed = TRUE
  )
  expect_match(
    selected,
    "expression_selected_genes <- debounceAfterFirst(",
    fixed = TRUE
  )
  expect_match(
    selected,
    "expression_selected_genes_input,\n  200",
    fixed = TRUE
  )
})

test_that("RGB mode reuses mounted selectors without resending all genes", {
  source <- paste(
    readLines(
      viewer_test_path("gene_expression", "UI_projection_input_type.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "copyGenesToRgb", fixed = TRUE)
  expect_match(source, "cerebroExpressionRgbPrefill", fixed = TRUE)
  expect_false(grepl("genes <- sort(getGeneNames())", source, fixed = TRUE))
  expect_false(grepl("choices = genes", source, fixed = TRUE))
})

test_that("Gene projection host is present before dynamic controls bind", {
  ui <- paste(
    readLines(viewer_test_path("gene_expression", "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  parameters <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "obj_projection_parameters_plot.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    ui,
    'cerebroCellViewOutput("expression_projection")',
    fixed = TRUE
  )
  expect_false(grepl('uiOutput("expression_projection_UI")', ui, fixed = TRUE))
  expect_match(parameters, 'input[["coordviews_shared_base"]]', fixed = TRUE)
  expect_match(parameters, "available_projections[[1L]]", fixed = TRUE)
})


test_that("empty Gene selection keeps the same summary without hydrating IDs", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  full <- data.frame(
    cell_barcode = c("a", "b", "c"),
    cluster = c("X", "X", "Y")
  )
  scope$getGroups <- function() "cluster"
  scope$getObservationUnit <- function() list(plural = "cells")
  scope$getParameters <- function() list(main_group = "cluster")
  hydrated <- 0L
  scope$getMetaData <- function() {
    hydrated <<- hydrated + 1L
    full
  }
  scope$viewerProjectionFirstFrameMetadata <- function() full["cluster"]
  selection <- NULL
  scope$expression_projection_selected_cells <- function() selection
  scope$input <- list(expression_projection_to_display = "UMAP")
  scope$output <- list()
  scope$renderUI <- function(expr) eval(substitute(expr), parent.frame())
  # Composition is checked independently; no selection means no color lookup.
  expected <- as.character(scope$cerebroSelectionSummary(
    NULL,
    "UMAP",
    metadata = full
  ))
  sys.source(
    viewer_test_path("gene_expression", "out_number_of_selected_cells.R"),
    scope
  )
  expect_identical(
    as.character(scope$output$expression_number_of_selected_cells),
    expected
  )
  expect_identical(hydrated, 0L)
  selection <- data.frame(selection_key = c("a", "b"))
  scope$reactive_colors <- function() list(cluster = c(X = "red", Y = "blue"))
  expected <- as.character(scope$cerebroSelectionSummary(
    selection,
    "UMAP",
    metadata = full
  ))
  sys.source(
    viewer_test_path("gene_expression", "out_number_of_selected_cells.R"),
    scope
  )
  expect_identical(
    as.character(scope$output$expression_number_of_selected_cells),
    expected
  )
  expect_gt(hydrated, 0L)
})


test_that("Gene hover loads pending IDs without bypassing hover or lifecycle guards", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- testthat::test_path("fixtures", "gene-hover-demand.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cell_views.js")))
    ),
    0L
  )
})
