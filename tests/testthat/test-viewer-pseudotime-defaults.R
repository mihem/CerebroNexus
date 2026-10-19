test_that("pseudotime controls consume author defaults and hiding the trend still renders", {
  skip_if_not_installed("plotly")
  sys.source(
    viewer_test_path("core", "viewer_content_contract.R"),
    environment()
  )
  defaults <- builderViewerDisplayDefaults(list(
    trajectory_density_opacity = .3,
    trajectory_show_trend_line = FALSE,
    trajectory_trend_line_width = 5
  ))
  server <- function(input, output, session) {
    current_page_display_defaults <- function(page) {
      viewerPageDisplayDefaults(
        list(viewer_content = list(ds = list(display = defaults))),
        "ds",
        page
      )
    }
    cerebroBox <- function(title, content, ...) shiny::tagList(title, content)
    boxTitle <- shiny::tags$span
    cerebroInfoButton <- function(...) NULL
    cerebroRegisterInfo <- function(...) invisible(NULL)
    trajectory_projection_sent <- function() TRUE
    getVariableToCompareChoices <- function() "score"
    preferences <- list(use_webgl = FALSE)
    cells <- data.frame(
      pseudotime = 1:6,
      score = c(2, 4, 3, 6, 5, 8),
      state = factor(rep("A", 6)),
      cell_barcode = paste0("c", 1:6)
    )
    trajectory_data_reactive <- function() list(meta = cells)
    trajectory_cells_reactive <- function(...) cells
    assignColorsToGroups <- function(...) c(A = "#008080")
    cerebro_plotly_axis <- function(...) list(...)
    cerebro_plotly_hoverlabel <- function() list()
    cerebro_plotly_theme <- function() {
      list(transparent = "transparent", axis = "#222222", accent = "#cc6600")
    }
    cerebro_plotly_toolbar <- identity
    `%>%` <- dplyr::`%>%`
    sys.source(
      viewer_test_path("trajectory", "distribution_along_pseudotime.R"),
      environment()
    )
  }
  shiny::testServer(server, {
    session$setInputs(
      trajectory_distribution_section_visible = TRUE,
      trajectory_selected_method = "method",
      trajectory_selected_name = "path"
    )
    traj <- output$trajectory_distribution_along_pseudotime_UI$html
    attr <- function(doc, id, name) {
      inputs <- regmatches(doc, gregexpr("<input\\b[^>]*>", doc, perl = TRUE))[[
        1L
      ]]
      input <- inputs[grepl(paste0('id="', id, '"'), inputs, fixed = TRUE)]
      stopifnot(length(input) == 1L)
      match <- regmatches(
        input,
        regexec(paste0(' ', name, '="([^"]*)"'), input)
      )[[1L]]
      if (length(match)) match[[2L]] else NA_character_
    }
    expect_true(is.na(attr(
      traj,
      "trajectory_distribution_along_pseudotime_show_trend_line",
      "checked"
    )))
    expect_equal(
      as.numeric(attr(
        traj,
        "trajectory_distribution_along_pseudotime_opacity",
        "data-from"
      )),
      .3
    )
    expect_equal(
      as.numeric(attr(
        traj,
        "trajectory_distribution_along_pseudotime_trend_line_width",
        "data-from"
      )),
      5
    )
    session$setInputs(
      trajectory_distribution_along_pseudotime_select_variable = "score",
      trajectory_distribution_along_pseudotime_opacity = .3,
      trajectory_distribution_along_pseudotime_show_trend_line = FALSE,
      trajectory_distribution_along_pseudotime_trend_line_bandwidth = 7,
      trajectory_distribution_along_pseudotime_trend_line_width = 5,
      trajectory_point_opacity = .8,
      trajectory_point_size = 2
    )
    plot <- jsonlite::fromJSON(
      output$trajectory_distribution_along_pseudotime_plot,
      simplifyVector = FALSE
    )$x
    expect_true(length(plot$data) > 0)
    expect_false(any(vapply(
      plot$data,
      function(trace) identical(trace$name, "Trend line"),
      logical(1)
    )))
    session$setInputs(
      trajectory_distribution_along_pseudotime_show_trend_line = TRUE
    )
    plot <- jsonlite::fromJSON(
      output$trajectory_distribution_along_pseudotime_plot,
      simplifyVector = FALSE
    )$x
    trend <- Filter(
      function(trace) identical(trace$name, "Trend line"),
      plot$data
    )
    expect_length(trend, 1)
    expect_equal(trend[[1]]$line$width, 5)
  })
})
