## Execute the production render bodies without the browser serialization layer.
gene_selection_body <- function(file, output) {
  root <- getOption(
    "gene.selection.source",
    viewer_test_path("gene_expression")
  )
  assignment <- viewer_r_assignment(
    file.path(root, file),
    paste0("output.", output)
  )
  body <- assignment[[3]][[2]]
  ## A table is defined as viewerLazyRender(function() DT::renderDataTable(...)).
  if (identical(assignment[[3]][[1]], as.name("viewerLazyRender"))) {
    render <- body[[3]]
    if (is.call(render) && identical(render[[1]], as.name("{"))) {
      render <- render[[2]]
    }
    body <- render[[2]]
  }
  body
}

test_that("Gene summary and table resolve canonical barcodes after deferred identity", {
  env <- viewer_runtime_env()
  env$data_set <- function() NULL
  env$`%>%` <- dplyr::`%>%`
  env$bind_cols <- dplyr::bind_cols
  env$everything <- dplyr::everything
  env$req <- shiny::req
  env$input <- list()
  env$reads <- 0L
  metadata <- data.frame(cell_barcode = c("3", "cell-b", "1", "cell-d"))
  env$getMetaData <- function() {
    env$reads <- env$reads + 1L
    metadata
  }
  env$expression_projection_summary_ready <- function() TRUE
  env$expression_projection_cells_to_show <- function() c(4L, 1L, 3L)
  env$expression_projection_coordinates <- function() {
    data.frame(x = c(1, 1, 2), y = c(3, 3, 4))
  }
  env$expression_projection_data <- function() {
    data.frame(cell_barcode = c("4", "1", "3"))
  }
  env$viewerProjectionFirstFrameMetadata <- function() {
    data.frame(cell_barcode = as.character(1:4))
  }
  env$expression_projection_shown_rows <- function() NULL
  rows <- viewer_r_assignment(
    viewer_test_path("gene_expression", "obj_projection_selected_cells.R"),
    "expression_projection_selected_rows"
  )[[3]][[2]]
  env$expression_projection_selected_rows <- function() eval(rows, env)
  env$expression_summary_data <- function() {
    list(kind = "mean", series = list(list(values = c(4, 0, NA_real_))))
  }
  env$plotExpressionSummary <- function(series, groups, colors) {
    as.character(groups)
  }
  env$prettifyTable <- function(data, ...) data
  env$prepareEmptyTable <- function(data) data
  summary <- gene_selection_body(
    "UI_expression_in_selected_cells.R",
    "expression_in_selected_cells"
  )
  table <- gene_selection_body(
    "UI_table_of_selected_cells.R",
    "expression_details_selected_cells"
  )
  env$selection <- NULL
  env$expression_projection_selected_cells <- function() env$selection
  ## The existing browser gate sends no IDs while identity is pending.
  expect_error(eval(summary, env), class = "shiny.silent.error")
  expect_error(eval(table, env), class = "shiny.silent.error")
  expect_identical(env$reads, 0L)
  ## Numeric-looking real barcode 1 means canonical cell 3, not subset row 1.
  env$selection <- data.frame(selection_key = "1")
  expect_identical(
    eval(summary, env),
    c("not selected", "not selected", "selected")
  )
  result <- eval(table, env)
  expect_identical(result$cell_barcode, "1")
  expect_identical(result$expression_level, NA_real_)
  env$selection <- data.frame(selection_key = c("3", "cell-d"))
  expect_identical(
    eval(summary, env),
    c("selected", "selected", "not selected")
  )
  expect_identical(eval(table, env)$cell_barcode, c("cell-d", "3"))
  ## Full canonical frame uses exactly the same identity, including numeric IDs.
  env$expression_projection_cells_to_show <- function() 1:3
  env$selection <- data.frame(selection_key = "cell-b")
  expect_identical(
    eval(summary, env),
    c("not selected", "selected", "not selected")
  )
  expect_identical(eval(table, env)$cell_barcode, "cell-b")
})

test_that("Gene selected-cell summary leaves out cells the browser filters hide", {
  env <- viewer_runtime_env()
  env$req <- shiny::req
  env$getMetaData <- function() {
    data.frame(cell_barcode = c("a", "b", "c", "d"))
  }
  env$viewerClientSelection <- function(...) FALSE
  env$expression_projection_summary_ready <- function() TRUE
  env$expression_projection_cells_to_show <- function() 1:4
  env$expression_projection_coordinates <- function() {
    data.frame(x = c(1, 2, 3, 4), y = c(5, 6, 7, 8))
  }
  env$expression_projection_data <- function() data.frame(row.names = 1:4)
  env$expression_summary_data <- function() {
    list(kind = "mean", series = list(list(values = c(10, 20, 30, 40))))
  }
  env$plotExpressionSummary <- function(series, groups, colors) {
    list(values = series[[1]]$values, groups = as.character(groups))
  }
  rows <- viewer_r_assignment(
    viewer_test_path("gene_expression", "obj_projection_selected_cells.R"),
    "expression_projection_selected_rows"
  )[[3]][[2]]
  env$expression_projection_selected_rows <- function() eval(rows, env)
  summary <- gene_selection_body(
    "UI_expression_in_selected_cells.R",
    "expression_in_selected_cells"
  )
  env$expression_projection_selected_cells <- function() {
    data.frame(selection_key = c("a", "c"))
  }
  ## The frame holds every cell; the browser shows rows 1, 2 and 4.
  env$expression_projection_shown_rows <- function() c(1L, 2L, 4L)
  expect_identical(
    eval(summary, env),
    list(
      values = c(10, 20, 40),
      groups = c("selected", "not selected", "not selected")
    )
  )
  env$expression_projection_shown_rows <- function() NULL
  expect_identical(
    eval(summary, env)$groups,
    c("selected", "not selected", "selected", "not selected")
  )
  ## Before stable identities, coordinates name the selected cells.
  env$expression_projection_selected_cells <- function() {
    data.frame(x = 3, y = 7, identifier = "3-7")
  }
  expect_identical(
    eval(summary, env)$groups,
    c("not selected", "not selected", "selected", "not selected")
  )
})
