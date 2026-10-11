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
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node unavailable")
  result <- system2(
    node,
    shQuote(c(
      test_path("fixtures", "view-feedback.js"),
      viewer_test_path("www", "view-feedback.js")
    )),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(
    paste(result, collapse = "\n"),
    "feedback assertions passed",
    fixed = TRUE
  )
})

test_that("Trajectory label updates carry the existing dataset context", {
  expressions <- as.list(parse(viewer_test_path("trajectory", "projection.R")))
  observer <- Filter(
    function(e) {
      is.call(e) &&
        identical(e[[1]], as.name("observeEvent")) &&
        grepl(
          "trajectory_projection_group_labels",
          paste(deparse(e[[2]]), collapse = ""),
          fixed = TRUE
        )
    },
    expressions
  )[[1]]
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
      eval(observer, environment())
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
