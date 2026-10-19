test_that("feedback reads only small controls without reactive dependencies", {
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      sys.source(viewer_test_path("utility_functions.R"), scope)
      runs <- 0L
      probe <- reactive({
        input$trigger
        runs <<- runs + 1L
        cerebroFeedbackControls("overview_projection", input)
      })
      getMetaData <- function() stop("must not materialize metadata")
    },
    {
      session$setInputs(
        trigger = 1,
        overview_projection_point_color = "sample",
        overview_projection_selected_cells = rep("barcode", 1000),
        overview_projection_group_labels = TRUE
      )
      expect_named(
        probe(),
        c(
          "overview_projection_group_labels",
          "overview_projection_point_color"
        ),
        ignore.order = TRUE
      )
      expect_identical(probe()$overview_projection_group_labels, "true")
      before <- runs
      session$setInputs(overview_projection_point_color = "cluster")
      expect_identical(probe()$overview_projection_point_color, "sample")
      expect_identical(runs, before)
      session$setInputs(trigger = 2)
      expect_identical(probe()$overview_projection_point_color, "cluster")
      expect_identical(
        cerebroFeedbackControls(
          "hla_motif_network",
          input,
          list(hla_min_nodes = 3)
        )$hla_min_nodes,
        "3"
      )
    }
  )
})

test_that("feedback matches painted operations and rejects stale datasets", {
  expect_node_fixture(
    "view-feedback.js",
    viewer_test_path("www", "view-feedback.js"),
    pass = "feedback assertions passed"
  )
})

test_that("Trajectory label updates carry the existing dataset context", {
  shiny::testServer(
    function(input, output, session) {
      sent <- NULL
      scope <- environment()
      trajectory_projection_appearance <- list(group_labels = FALSE)
      trajectory_update_appearance <- function(...) TRUE
      viewerDatasetIdentity <- function() list(fingerprint = "cells")
      viewer_loaded_dataset_context <- function() {
        list(epoch = "session", dataset_key = "A", generation = 3L)
      }
      session$sendCustomMessage <- function(type, message) sent <<- message
      viewer_server_load(
        viewer_server_statements("trajectory", "projection.R"),
        scope,
        events = "trajectory_projection_group_labels"
      )
    },
    {
      session$setInputs(trajectory_projection_group_labels = FALSE)
      expect_identical(
        scope$sent$dataset_context,
        viewer_loaded_dataset_context()
      )
      expect_identical(scope$sent$values$group_labels, FALSE)
    }
  )
})
