## A table of a few selected cells, a hover or a click on one cell names those
## cells only: the names are cut out of the Viewer Pack's names file, not made
## for every cell of the data set first.

test_that("the names of some rows are cut out of the Pack, not all read", {
  env <- viewer_runtime_env()
  pack <- list(cell_count = 6L, cache = new.env(parent = emptyenv()))
  asked <- list()
  env$viewerPackCurrent <- function() pack
  env$viewerPackCellBarcodesAt <- function(pack, index) {
    asked[[length(asked) + 1L]] <<- index
    paste0("cell", index)
  }
  env$viewerPackCellBarcodes <- function(...) stop("every name was read")
  env$getMetaData <- function() stop("the metadata was hydrated")

  expect_identical(env$viewerCellBarcodesAt(c(5, 2)), c("cell5", "cell2"))
  expect_identical(asked[[1L]], c(5L, 2L))

  ## Without a Pack, or when the Pack cannot name them, the metadata does.
  env$getMetaData <- function() {
    data.frame(cell_barcode = paste0("meta", 1:6))
  }
  env$viewerPackCellBarcodesAt <- function(pack, index) NULL
  expect_identical(env$viewerCellBarcodesAt(c(5L, 2L)), c("meta5", "meta2"))
  env$viewerPackCurrent <- function() NULL
  expect_identical(env$viewerCellBarcodesAt(3L), "meta3")
})

test_that("a clicked cell's metadata row is taken by position, checked by one name", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sent <- list()
      named <- character()
      viewer_loaded_dataset_context <- function() list(generation = 1L)
      viewerDatasetContextEqual <- identical
      viewerProjectionFirstFrameMetadata <- function() {
        data.frame(cluster = c("one", "two", "three"))
      }
      viewerCellBarcodesAt <- function(rows) {
        named <<- c(named, paste(rows, collapse = ","))
        c("A", "B", "C")[rows]
      }
      getMetaData <- function() {
        data.frame(
          cell_barcode = c("C", "B", "A"),
          cluster = c("by-name-C", "by-name-B", "by-name-A")
        )
      }
      session$sendCustomMessage <- function(type, message) {
        sent[[length(sent) + 1L]] <<- message
      }
      sys.source(
        viewer_test_path("core", "cell_inspection.R"),
        envir = environment()
      )
    },
    {
      request <- list(
        cell = "C",
        client_index = 2L,
        view_id = "linked",
        request_id = 1L,
        dataset_context = list(generation = 1L)
      )
      session$setInputs(coordviews_cell_detail = request)
      ## The row at the browser's index, its name checked, nothing else named.
      expect_identical(scope$sent[[1L]]$rows[[1L]]$v, "three")
      expect_identical(scope$named, "3")

      ## An index that does not hold the cell: found by name, as before.
      request$request_id <- 2L
      request$client_index <- 0L
      session$setInputs(coordviews_cell_detail = request)
      expect_identical(scope$sent[[2L]]$rows[[1L]]$v, "by-name-C")
    }
  )
})
