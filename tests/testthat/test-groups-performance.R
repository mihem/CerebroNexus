test_that("Groups reads only the active columns after its viewport gate", {
  composition <- paste(
    readLines(viewer_test_path("groups", "composition.R"), warn = FALSE),
    collapse = "\n"
  )
  metrics <- paste(
    readLines(viewer_test_path("groups", "expression_metrics.R"), warn = FALSE),
    collapse = "\n"
  )
  ui <- paste(
    readLines(viewer_test_path("groups", "UI.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("getMetaData()", composition, fixed = TRUE))
  expect_false(grepl("getMetaData()", metrics, fixed = TRUE))
  expect_match(composition, "groupsMetadataColumns", fixed = TRUE)
  expect_match(metrics, "groupsMetricData", fixed = TRUE)
  expect_match(
    metrics,
    "groups_expression_metrics_render_request",
    fixed = TRUE
  )
  expect_match(metrics, "groups_expression_metrics_tabs", fixed = TRUE)
  expect_match(ui, "IntersectionObserver", fixed = TRUE)
  expect_match(ui, "groups_expression_metrics_render_request", fixed = TRUE)
})

groups_test_function <- function(name) {
  expressions <- parse(viewer_test_path("groups", "composition.R"))
  assignment <- Filter(
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], quote(`<-`)) &&
        identical(as.character(expression[[2L]]), name)
    },
    expressions
  )
  stopifnot(length(assignment) == 1L)
  scope <- new.env(parent = globalenv())
  if (!identical(name, "groupsFormatComposition")) {
    scope$groupsFormatComposition <- groups_test_function(
      "groupsFormatComposition"
    )
  }
  eval(assignment[[1L]], envir = scope)
  scope[[name]]
}

test_that("Groups builds its first-frame controls without a renderUI cascade", {
  composition <- paste(
    readLines(viewer_test_path("groups", "composition.R"), warn = FALSE),
    collapse = "\n"
  )
  controls <- paste(
    readLines(viewer_test_path("groups", "select_group.R"), warn = FALSE),
    collapse = "\n"
  )
  ui <- paste(
    readLines(viewer_test_path("groups", "UI.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("groups_composition_UI", composition, fixed = TRUE))
  expect_false(grepl(
    "groups_by_other_group_output_UI",
    composition,
    fixed = TRUE
  ))
  expect_match(controls, "groups_controls_UI", fixed = TRUE)
  expect_match(ui, "plotlyOutput(\"groups_by_other_group_plot\")", fixed = TRUE)
  expect_match(ui, "groups_plotly_dependencies", fixed = TRUE)
})

test_that("Groups reports passive request-bound Plotly readiness", {
  ui <- paste(
    readLines(viewer_test_path("groups", "UI.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(ui, "plotly_afterplot", fixed = TRUE)
  expect_match(ui, "cerebro:groups-primary-ready", fixed = TRUE)
  expect_match(ui, "datasetFingerprint", fixed = TRUE)
  expect_match(ui, "selectedGroup", fixed = TRUE)
  expect_match(ui, "benchmarkGeneration", fixed = TRUE)
  expect_false(grepl("waitForFirstPlotDraw", ui, fixed = TRUE))
  expect_match(ui, "observer.observe(target)", fixed = TRUE)
  expect_match(ui, "if (!visible || target.dataset.requested", fixed = TRUE)
})

test_that("Groups composition counts factors without sorting every cell", {
  calculate <- groups_test_function("groupsCalculateTableAB")
  metadata <- data.frame(
    major = factor(c("A", "A", "B", "B", NA), levels = c("A", "B")),
    subtype = factor(
      c("a1", "a2", "b1", "b1", "a2"),
      levels = c("a1", "a2", "b1")
    )
  )

  long <- calculate(metadata, "major", "subtype", "long", FALSE)
  expect_identical(as.character(long$major), c("A", "A", "B", NA))
  expect_identical(as.character(long$subtype), c("a1", "a2", "b1", "a2"))
  expect_identical(long$count, c(1L, 1L, 2L, 1L))
  expect_identical(long$total_cell_count, c(2L, 2L, 2L, 1L))

  percent <- calculate(metadata, "major", "subtype", "long", TRUE)
  expect_equal(percent$count, c(0.5, 0.5, 1, 1))

  wide <- calculate(metadata, "major", "subtype", "wide", FALSE)
  expect_named(wide, c("major", "total_cell_count", "a1", "a2", "b1"))
  expect_equal(wide$a1, c(1L, 0L, 0L))
  expect_equal(wide$a2, c(1L, 0L, 1L))
  expect_equal(wide$b1, c(0L, 2L, 0L))
})

test_that("Groups resolves only the colours used by the active plot", {
  composition <- paste(
    readLines(viewer_test_path("groups", "composition.R"), warn = FALSE),
    collapse = "\n"
  )
  metrics <- paste(
    readLines(viewer_test_path("groups", "expression_metrics.R"), warn = FALSE),
    collapse = "\n"
  )
  setup <- paste(
    readLines(viewer_test_path("color_setup.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("reactive_colors()", composition, fixed = TRUE))
  expect_false(grepl("reactive_colors()", metrics, fixed = TRUE))
  expect_match(composition, "reactive_group_colors", fixed = TRUE)
  expect_match(metrics, "reactive_group_colors", fixed = TRUE)
  expect_match(setup, "reactive_group_colors <- function", fixed = TRUE)
})


test_that("single-group controls keep expression metrics usable across dataset changes", {
  withr::local_package("shiny")
  withr::local_package("shinydashboard")
  withr::local_package("magrittr")
  controls <- viewer_test_path("groups", "select_group.R")
  metrics <- viewer_test_path("groups", "expression_metrics.R")
  plots <- viewer_test_path("plotting_functions.R")
  shiny::testServer(
    function(input, output, session) {
      groups <- reactiveVal(c("sample", "condition"))
      getGroups <- function() groups()
      metadata <- data.frame(sample = c("A", "A", "B"), nUMI = c(1, 3, 7))
      viewerProjectionFirstFrameMetadata <- function() metadata
      viewerPackCurrent <- function() NULL
      viewerPackGroupMetric <- function(...) NULL
      groupsMetadataColumns <- function(columns) {
        metadata[, columns, drop = FALSE]
      }
      reactive_group_colors <- function(...) c(A = "red", B = "blue")
      cachePlot <- function(x, ...) x
      cerebroRegisterInfo <- function(...) NULL
      available_crb_files <- list(selected = "fixture")
      sys.source(plots, environment())
      sys.source(controls, environment())
      sys.source(metrics, environment())
    },
    {
      session$flushReact()
      expect_match(
        output$groups_controls_UI$html,
        "groups_by_other_group_second_group"
      )
      groups("sample")
      session$flushReact()
      expect_match(output$groups_controls_UI$html, "groups_selected_group")
      expect_false(grepl(
        "groups_by_other_group_second_group",
        output$groups_controls_UI$html
      ))
      session$setInputs(
        groups_selected_group = "sample",
        groups_expression_metrics_render_request = 1,
        groups_expression_metrics_tabs = "Number of transcripts"
      )
      rendered <- jsonlite::fromJSON(
        output$groups_nUMI_plot,
        simplifyVector = FALSE
      )
      boxes <- Filter(
        function(trace) identical(trace$type, "box"),
        rendered$x$data
      )
      expect_length(boxes, 1)
      expect_equal(unlist(boxes[[1L]]$median), c(2, 7))
      groups(character())
      session$flushReact()
      expect_match(output$groups_controls_UI$html, "No grouping variables")
    }
  )
})
