test_that("selected table does not require lazy appearance controls", {
  expr <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1L]], as.name("<-")) &&
        identical(
          paste(deparse(x[[2L]]), collapse = ""),
          'output[["trajectory_details_selected_cells_table"]]'
        )
    },
    parse(viewer_test_path("trajectory", "selected_cells_table.R"))
  )[[1L]]
  # Execute the actual output expression until data access, with no appearance
  # input initialized. A silent req failure would stop before this sentinel.
  body <- expr[[3L]][[2L]]
  scope <- new.env(parent = globalenv())
  scope$req <- shiny::req
  scope$input <- list()
  scope$trajectory_selection_ok <- function() TRUE
  scope$trajectory_projection_selected_cells <- function() {
    stop("selection reached")
  }
  expect_error(eval(body, scope), "selection reached")
  scope$trajectory_selection_ok <- function() FALSE
  expect_error(eval(body, scope), class = "shiny.silent.error")
})

test_that("hover dictionaries preserve singleton labels and missing values", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  script <- testthat::test_path("fixtures", "single-hover-levels.js")
  result <- system2(
    Sys.which("node"),
    c(shQuote(script), shQuote(viewer_test_path("www", "cell_views.js"))),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})
