test_that("gene means reuse canonical rows and retain full-dataset semantics", {
  scope <- new.env(parent = globalenv())
  scope$reactiveVal <- shiny::reactiveVal
  scope$reactive <- shiny::reactive
  scope$observeEvent <- function(...) NULL
  sys.source(
    viewer_test_path("gene_expression", "obj_projection_expression_levels.R"),
    scope
  )
  scope$expressionProjectionDatasetKey <- function() "summary-test"
  reads <- 0L
  scope$viewerExpressionValues <- function(data, cells, genes) {
    reads <<- reads + 1L
    stats::setNames(
      lapply(genes, function(gene) data$expression[gene, cells]),
      genes
    )
  }
  data <- list(
    expression = matrix(
      c(0, 1, 2, 10, 4, 0, 8, 2),
      nrow = 2,
      dimnames = list(c("A", "B"), NULL)
    )
  )
  scope$expressionProjectionCachedRows(data, 1:4, c("A", "B"))
  means <- scope$expressionProjectionGeneMeans(data, c("B", "A"))
  expect_identical(reads, 1L)
  expect_equal(means$expression, unname(rowMeans(data$expression)[c("B", "A")]))
  data$getMeanExpressionForGenes <- function(genes) {
    data.frame(gene = genes, expression = rep(42, length(genes)))
  }
  expect_equal(
    scope$expressionProjectionGeneMeans(data, "uncached")$expression,
    42
  )
  expect_identical(reads, 1L)
})

test_that("many-group density payload stays bounded with exact full-data summaries", {
  skip_if_not_installed("plotly")
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("plotting_functions.R"), scope)
  sys.source(
    viewer_test_path("gene_expression", "func_expression_summary.R"),
    scope
  )
  compact <- scope$compactViolinData
  tables <- list()
  scope$compactViolinData <- function(...) {
    result <- compact(...)
    tables[[length(tables) + 1L]] <<- result
    result
  }
  scope$cerebro_plotly_axis <- function(...) list(...)
  scope$cerebro_plotly_hoverlabel <- function() list()
  scope$cerebro_plotly_theme <- function() list(transparent = "transparent")
  scope$cerebro_plotly_toolbar <- function(plot) plot
  groups <- rep(sprintf("sample-%03d", 1:100), each = 1000)
  values <- rep(0:999, 100)
  series <- lapply(1:3, function(i) {
    list(label = paste0("Gene", i), values = values + i, color = "#ff0000")
  })
  scope$plotExpressionSummary(series, groups, rep("#ff0000", 100))
  expect_lte(sum(vapply(tables, nrow, integer(1))), 65536)
  summary <- attr(tables[[1]], "cerebro_violin_summary")$stats
  expect_equal(summary$n, rep(1000L, 100))
  expect_equal(summary$mean, rep(mean(0:999 + 1), 100))
  expect_equal(summary$median, rep(stats::median(0:999 + 1), 100))
})
