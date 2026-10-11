test_that("trajectory metadata columns preserve exact values, order and row names", {
  expr <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1L]], as.name("<-")) &&
        identical(x[[2L]], as.name("trajectory_cells_reactive"))
    },
    parse(viewer_test_path("trajectory", "server.R"))
  )[[1L]]
  env <- new.env(parent = globalenv())
  metadata <- data.frame(
    cell_barcode = letters[1:5],
    group = factor(c("b", NA, "a", "b", "c"), levels = c("c", "b", "a")),
    value = c(0, NA, 1.23456789012345, 4, 5),
    row.names = letters[1:5]
  )
  index <- c(5L, 2L, 3L, 1L)
  cells <- data.frame(
    DR_1 = c(0, 1, 2, 3),
    DR_2 = c(3, 2, 1, 0),
    pseudotime = c(0, 1, NA, 3),
    state = factor(c("A", "B", "B", "C")),
    row.names = metadata$cell_barcode[index]
  )
  env$trajectory_data_reactive <- function() list(meta = cells)
  env$trajectory_row_index_reactive <- function() index
  env$viewerProjectionFirstFrameMetadata <- function() metadata
  env$viewerPackCurrent <- function() NULL
  env$viewerPackCellBarcodes <- function(...) NULL
  env$getMetaData <- function() metadata
  eval(expr, env)
  expected <- cbind(cells, metadata[index, c("group", "value"), drop = FALSE])
  expected$cell_index <- index
  expected$cell_barcode <- metadata$cell_barcode[index]
  expected <- expected[!is.na(expected$pseudotime), , drop = FALSE]
  actual <- env$trajectory_cells_reactive(
    c("group", "value", "state", "group", "missing"),
    TRUE
  )
  expect_identical(actual, expected)
  expect_identical(actual$cell_barcode, c("e", "b", "a"))
  expect_identical(actual$value, c(5, NA_real_, 0))
  expect_identical(levels(actual$group), levels(metadata$group))
})

test_that("empty Trajectory selection preserves summary without hydrating metadata", {
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
  scope$trajectory_projection_selected_cells <- function() selection
  scope$input <- list(
    trajectory_selected_name = "path",
    trajectory_point_color = "cluster"
  )
  scope$output <- list()
  scope$renderUI <- function(expr) eval(substitute(expr), parent.frame())
  expr <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1L]], as.name("<-")) &&
        identical(
          paste(deparse(x[[2L]]), collapse = ""),
          'output[["trajectory_number_of_selected_cells"]]'
        )
    },
    parse(viewer_test_path("trajectory", "projection_plot.R"))
  )[[1L]]
  expected <- as.character(scope$cerebroSelectionSummary(
    NULL,
    "path",
    "cluster",
    metadata = full
  ))
  eval(expr, scope)
  expect_identical(
    as.character(scope$output$trajectory_number_of_selected_cells),
    expected
  )
  expect_identical(hydrated, 0L)
  selection <- data.frame(selection_key = c("a", "b"))
  expected <- as.character(scope$cerebroSelectionSummary(
    selection,
    "path",
    "cluster",
    metadata = full
  ))
  eval(expr, scope)
  expect_identical(
    as.character(scope$output$trajectory_number_of_selected_cells),
    expected
  )
  expect_identical(hydrated, 1L)
})
