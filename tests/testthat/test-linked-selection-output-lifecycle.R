test_that("hidden Linked selection UI drops selection and dataset dependencies", {
  target <- viewer_r_assignment(
    viewer_test_path("coordinated_views", "server.R"),
    "coordviews_selected_cells_ui"
  )
  shiny::testServer(
    function(input, output, session) {
      renderUI <- shiny::renderUI
      req <- shiny::req
      work <- new.env(parent = emptyenv())
      work$selection_reads <- work$metadata_reads <- 0L
      coordviews_selection_present <- shiny::reactive({
        input$generation
        work$selection_reads <- work$selection_reads + 1L
        FALSE
      })
      getMetaData <- viewerProjectionFirstFrameMetadata <- function() {
        work$metadata_reads <- work$metadata_reads + 1L
        stop("Empty or hidden selections must not prepare metadata")
      }
      eval(target, environment())
      output$coordviews_selected_cells_UI <- coordviews_selected_cells_ui
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

test_that("the Linked selection UI has a value before the first selection and builds on it", {
  ## shiny >= 1.9 keeps `.recalculating` on an output until its first value
  ## arrives, so an output without a renderer keeps the page busy for good.
  expressions <- viewer_server_statements("coordinated_views", "server.R")
  shiny::testServer(
    function(input, output, session) {
      renderUI <- shiny::renderUI
      observeEvent <- shiny::observeEvent
      req <- shiny::req
      isolate <- shiny::isolate
      tagList <- shiny::tagList
      fluidRow <- shiny::fluidRow
      selectInput <- shiny::selectInput
      work <- new.env(parent = emptyenv())
      work$metadata_reads <- 0L
      coordviews_selection_present <- shiny::reactive(isTRUE(input$present))
      viewerProjectionFirstFrameMetadata <- function() {
        work$metadata_reads <- work$metadata_reads + 1L
        data.frame(cell_barcode = "a", cluster = "x")
      }
      cerebroBox <- function(title, ...) {
        shiny::tags$div(class = "box", title, ...)
      }
      boxTitle <- function(text) text
      cerebroInfoButton <- function(id) NULL
      cerebroChartOutput <- function(id) shiny::tags$div(id = id)
      viewer_server_load(
        expressions,
        environment(),
        names = c(
          "coordviews_selected_cells_ui",
          "output[['coordviews_selected_cells_UI']]"
        ),
        events = "coordviews_selection_present"
      )
    },
    {
      session$setInputs(sidebar = "coordinated_views", present = FALSE)
      ## A value, not a silent error: the page can settle with nothing selected.
      expect_no_error(output$coordviews_selected_cells_UI)
      expect_identical(work$metadata_reads, 0L)

      session$setInputs(present = TRUE)
      html <- as.character(output$coordviews_selected_cells_UI$html)
      expect_match(html, "Plot of selected cells", fixed = TRUE)
      expect_match(html, "Table of selected cells", fixed = TRUE)
      expect_gte(work$metadata_reads, 1L)
    }
  )
})
