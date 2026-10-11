test_that("hidden Linked selection UI drops selection and dataset dependencies", {
  expressions <- parse(viewer_test_path("coordinated_views", "server.R"))
  target <- Filter(
    function(x) {
      is.call(x) &&
        identical(x[[1L]], as.name("<-")) &&
        identical(
          paste(deparse(x[[2L]]), collapse = ""),
          'output[["coordviews_selected_cells_UI"]]'
        )
    },
    expressions
  )[[1L]]
  shiny::testServer(
    function(input, output, session) {
      renderUI <- shiny::renderUI
      req <- shiny::req
      work <- new.env(parent = emptyenv())
      work$selection_reads <- work$metadata_reads <- 0L
      coordviews_selected_barcodes <- shiny::reactive({
        input$generation
        work$selection_reads <- work$selection_reads + 1L
        character()
      })
      getMetaData <- function() {
        work$metadata_reads <- work$metadata_reads + 1L
        stop("Empty or hidden selections must not prepare metadata")
      }
      eval(target, environment())
      shiny::outputOptions(
        output,
        "coordviews_selected_cells_UI",
        suspendWhenHidden = FALSE
      )
    },
    {
      session$setInputs(sidebar = "coordinated_views", generation = 1L)
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      expect_identical(work$selection_reads, 1L)
      session$setInputs(sidebar = "geneExpression")
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      session$setInputs(generation = 2L)
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      expect_identical(work$selection_reads, 1L)
      expect_identical(work$metadata_reads, 0L)
      session$setInputs(sidebar = "coordinated_views")
      expect_error(
        output$coordviews_selected_cells_UI,
        class = "shiny.silent.error"
      )
      expect_identical(work$selection_reads, 2L)
    }
  )
})
