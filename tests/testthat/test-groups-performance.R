test_that("Groups reads only the active columns after its viewport gate", {
  withr::local_package("magrittr")
  shiny::testServer(
    function(input, output, session) {
      scope <- environment()
      reads <- list()
      colour_reads <- character()
      metadata <- data.frame(
        sample = c("A", "B"),
        nUMI = c(1, 3),
        nGene = c(2, 4),
        unused = 5:6
      )
      getGroups <- function() "sample"
      getMetaData <- reactive_colors <- function() {
        stop("full metadata or colour scan")
      }
      viewerPackCurrent <- function() NULL
      viewerPackGroupMetric <- function(...) NULL
      groupsMetadataColumns <- function(columns) {
        reads[[length(reads) + 1L]] <<- columns
        metadata[, columns, drop = FALSE]
      }
      reactive_group_colors <- function(group) {
        colour_reads <<- c(colour_reads, group)
        c(A = "red", B = "blue")
      }
      cachePlot <- function(x, ...) x
      plotlyViolin <- function(table, colors, ...) {
        force(colors)
        plotly::plot_ly(x = table[[1]], y = table[[2]], type = "bar")
      }
      available_crb_files <- list(selected = "tiny")
      cerebroRegisterInfo <- function(...) NULL
      ## The metric outputs are created in a loop, which viewer_server_load()
      ## skips, so source the whole file.
      sys.source(viewer_test_path("groups", "expression_metrics.R"), scope)
    },
    {
      session$setInputs(
        groups_selected_group = "sample",
        groups_expression_metrics_tabs = "Number of transcripts"
      )
      expect_error(output$groups_nUMI_plot, class = "shiny.silent.error")
      expect_length(scope$reads, 0L)
      expect_length(scope$colour_reads, 0L)
      session$setInputs(groups_expression_metrics_render_request = 1)
      invisible(output$groups_nUMI_plot)
      expect_identical(scope$reads, list(c("sample", "nUMI")))
      session$setInputs(
        groups_expression_metrics_tabs = "Number of expressed genes"
      )
      invisible(output$groups_nGene_plot)
      expect_identical(
        scope$reads,
        list(c("sample", "nUMI"), c("sample", "nGene"))
      )
      expect_identical(scope$colour_reads, c("sample", "sample"))
      session$setInputs(groups_selected_group = "old-dataset-group")
      expect_error(output$groups_nGene_plot, class = "shiny.silent.error")
      expect_length(scope$reads, 2L)
    }
  )
})

groups_test_function <- function(name) {
  assignment <- viewer_r_assignment(
    viewer_test_path("groups", "composition.R"),
    name
  )
  scope <- new.env(parent = globalenv())
  if (!identical(name, "groupsFormatComposition")) {
    scope$groupsFormatComposition <- groups_test_function(
      "groupsFormatComposition"
    )
  }
  eval(assignment, envir = scope)
  scope[[name]]
}

test_that("Groups builds its first-frame controls without a renderUI cascade", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("groups", "UI.R"), scope)
  page <- htmltools::tagQuery(scope$tab_groups)
  expect_equal(page$find("#groups_by_other_group_plot")$length(), 1L)
  dependencies <- htmltools::renderTags(scope$tab_groups)$dependencies
  expect_true(any(vapply(
    dependencies,
    function(x) grepl("plotly", x$name),
    logical(1)
  )))
  shiny::testServer(
    function(input, output, session) {
      getGroups <- function() c("sample", "cluster")
      sys.source(viewer_test_path("groups", "select_group.R"), environment())
    },
    {
      first <- output$groups_controls_UI$html
      expect_match(first, 'id="groups_selected_group"', fixed = TRUE)
      expect_match(
        first,
        'id="groups_by_other_group_second_group"',
        fixed = TRUE
      )
      session$setInputs(groups_selected_group = "cluster")
      expect_identical(output$groups_controls_UI$html, first)
    }
  )
})

test_that("Groups reports passive request-bound Plotly readiness", {
  skip_if_no_node()
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("groups", "UI.R"), scope)
  scripts <- htmltools::tagQuery(scope$tab_groups)$find("script")$selectedTags()
  payload <- withr::local_tempfile(fileext = ".json")
  writeLines(
    jsonlite::toJSON(
      unname(lapply(scripts, function(tag) {
        paste(tag$children, collapse = "\n")
      })),
      auto_unbox = TRUE
    ),
    payload
  )
  expect_node_fixture("groups-readiness.js", payload)
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
