test_that("cell inspection requests use stable IDs and reject stale replies", {
  skip_if(Sys.which("node") == "", "Node unavailable")
  result <- system2("node", shQuote(c(
    test_path("fixtures", "cell-inspection.js"),
    viewer_test_path("www", "cell_views.js")
  )), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})

test_that("specialist detail requests resolve the requested cell instead of Linked row order", {
  shiny::testServer(function(input, output, session) {
    scope <- environment()
    sent <- list()
    viewer_loaded_dataset_context <- function() list(generation = 1L)
    viewerDatasetContextEqual <- identical
    cv_saved_view_cells <- function() stop("Must not use Linked view ordering")
    getMetaData <- function() data.frame(cell_barcode = c("A", "B"), cluster = c("one", "two"))
    session$sendCustomMessage <- function(type, message) sent[[length(sent) + 1L]] <<- message
    sys.source(viewer_test_path("core", "cell_inspection.R"), envir = environment())
  }, {
    request <- list(cell = "B", client_index = 0L, view_id = "expression_projection",
      request_id = 1L, dataset_context = list(generation = 1L))
    session$setInputs(coordviews_cell_detail = request)
    expect_identical(scope$sent[[1L]]$cell, "B")
    expect_identical(scope$sent[[1L]]$rows[[1L]]$v, "two")
    expect_identical(scope$sent[[1L]]$client_index, 0L)
    expect_identical(scope$sent[[1L]]$request_id, 1L)
    request$dataset_context$generation <- 0L
    session$setInputs(coordviews_cell_detail = request)
    expect_length(scope$sent, 1L)
    request$dataset_context$generation <- 1L
    request$cell <- "missing"
    session$setInputs(coordviews_cell_detail = request)
    expect_identical(scope$sent[[2L]]$error, "Cell metadata unavailable.")
  })
})
