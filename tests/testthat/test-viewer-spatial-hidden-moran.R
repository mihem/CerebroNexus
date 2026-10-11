test_that("hidden Moran cache key does not prepare Spatial data", {
  helpers <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), helpers)
  score_output <- parse(viewer_test_path("spatial", "out_morans_i.R"))[[1L]]
  shiny::testServer(
    function(input, output, session) {
      renderText <- shiny::renderText
      req <- shiny::req
      cachePlot <- helpers$cachePlot
      `%>%` <- dplyr::`%>%`
      work <- new.env(parent = emptyenv())
      work$parameters <- work$reads <- 0L
      spatial_projection_parameters_plot <- function() {
        work$parameters <- work$parameters + 1L
        list(
          plot_type = input$spatial_projection_plot_type,
          projection = "slide",
          feature_to_display = input$spatial_projection_feature_to_display
        )
      }
      getGeneNames <- function() "G"
      getSpatialData <- function(projection) {
        work$reads <- work$reads + 1L
        list(coordinates = data.frame(x = 1:3, y = c(0, 1, 0)))
      }
      spatial_projection_cell_index <- shiny::reactive(1:3)
      viewerExpressionRow <- function(...) c(1, 2, 4)
      data_set <- shiny::reactive(list())
      morans_i <- function(...) 0.125
      viewer_loaded_dataset_context <- shiny::reactive(list(
        epoch = "test",
        dataset_key = "slide",
        generation = 1L
      ))
      viewerDatasetContextToken <- helpers$viewerDatasetContextToken
      eval(score_output, environment())
    },
    {
      session$setInputs(
        sidebar = "overview",
        spatial_projection_plot_type = "ImageFeaturePlot",
        spatial_projection_feature_to_display = "G"
      )
      expect_error(
        output$spatial_projection_morans_i,
        class = "shiny.silent.error"
      )
      expect_identical(work$parameters, 0L)
      expect_identical(work$reads, 0L)
      session$setInputs(
        sidebar = "spatial",
        spatial_projection_plot_type = "ImageDimPlot"
      )
      expect_error(
        output$spatial_projection_morans_i,
        class = "shiny.silent.error"
      )
      expect_identical(work$parameters, 0L)
      session$setInputs(spatial_projection_plot_type = "ImageFeaturePlot")
      expect_identical(output$spatial_projection_morans_i, "0.125")
      expect_identical(work$reads, 1L)
    }
  )
})
