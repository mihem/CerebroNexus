test_that("selected table does not require lazy appearance controls", {
  expr <- viewer_r_assignment(
    viewer_test_path("trajectory", "selected_cells_table.R"),
    "output.trajectory_details_selected_cells_table"
  )
  # Execute the actual output expression until data access, with no appearance
  # input initialized. A silent req failure would stop before this sentinel.
  scope <- new.env(parent = globalenv())
  scope$req <- shiny::req
  scope$output <- list()
  local_mocked_bindings(
    renderDataTable = function(expr, ...) {
      eval(substitute(expr), parent.frame())
    },
    .package = "DT"
  )
  scope$input <- list()
  scope$trajectory_selection_ok <- function() TRUE
  scope$trajectory_projection_selected_cells <- function() {
    stop("selection reached")
  }
  expect_error(eval(expr, scope), "selection reached")
  scope$trajectory_selection_ok <- function() FALSE
  expect_error(eval(expr, scope), class = "shiny.silent.error")
})

test_that("selected table matches barcodes first and coordinates as fallback", {
  expr <- viewer_r_assignment(
    viewer_test_path("trajectory", "selected_cells_table.R"),
    "output.trajectory_details_selected_cells_table"
  )
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  scope$output <- list()
  local_mocked_bindings(
    renderDataTable = function(expr, ...) {
      eval(substitute(expr), parent.frame())
    },
    .package = "DT"
  )
  scope$prettifyTable <- function(table, ...) list(table = table, ...)
  scope$prepareEmptyTable <- function(table) list(empty = table)
  scope$input <- list(
    trajectory_details_selected_cells_table_number_formatting = FALSE,
    trajectory_details_selected_cells_table_color_highlighting = TRUE
  )
  scope$trajectory_selection_ok <- function() TRUE
  scope$viewerProjectionFirstFrameMetadata <- function() {
    data.frame(cell_barcode = "a", group = "A")
  }
  scope$getMetaData <- function() {
    data.frame(cell_barcode = c("a", "b"), group = c("A", "B"))
  }
  cells <- data.frame(
    DR_1 = c(1, 2, 3),
    DR_2 = c(4, 5, 6),
    pseudotime = c(0.1, 0.2, 0.3),
    group = c("A", "B", "A"),
    cell_barcode = c("a", "b", "c"),
    row.names = c("a", "b", "c")
  )
  scope$trajectory_cells_reactive <- function(columns, barcodes = FALSE) {
    expect_true(barcodes)
    expect_identical(columns, c("cell_barcode", "group"))
    cells
  }
  render <- function(selection) {
    scope$trajectory_projection_selected_cells <- function() selection
    eval(expr, scope)
  }

  stable <- render(data.frame(selection_key = c("c", "a"), identifier = "2-5"))
  expect_identical(stable$table, cells[c(1L, 3L), c(5L, 1:4)])
  expect_false(stable$number_formatting)
  expect_true(stable$color_highlighting)
  expect_identical(
    stable$download_file_name,
    "trajectory_details_of_selected_cells"
  )

  fallback <- render(data.frame(selection_key = NA, identifier = "2-5"))
  expect_identical(fallback$table, cells[2L, c(5L, 1:4)])

  empty_columns <- data.frame(cell_barcode = character(), group = character())
  expect_identical(render(NULL)$empty, empty_columns)
  expect_identical(
    render(data.frame(selection_key = "zz", identifier = "1-4"))$empty,
    empty_columns
  )
})
