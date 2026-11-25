##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Distribution along pseudotime.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_distribution_along_pseudotime_UI"]] <- renderUI({
  req(input[["trajectory_distribution_section_visible"]])
  req(trajectory_projection_sent())
  req(
    trajectory_selected_method(),
    trajectory_selected_name()
  )
  display_defaults <- current_page_display_defaults("trajectory")

  fluidRow(
    cerebroBox(
      title = tagList(
        boxTitle("Distribution along pseudotime"),
        cerebroInfoButton(
          "trajectory_distribution_along_pseudotime_info",
          style = "margin-right: 3px"
        ),
        shinyWidgets::dropdownButton(
          tags$div(
            style = "color: black !important;",
            class = "pull-right",
            tagList(
              sliderInput(
                "trajectory_distribution_along_pseudotime_opacity",
                label = "Opacity of density curve",
                min = 0,
                max = 1,
                step = 0.1,
                value = display_defaults$density_opacity
              ),
              hr(),
              shinyWidgets::awesomeCheckbox(
                inputId = "trajectory_distribution_along_pseudotime_show_trend_line",
                label = "Show trend line",
                value = display_defaults$show_trend_line
              ),
              sliderInput(
                "trajectory_distribution_along_pseudotime_trend_line_bandwidth",
                label = "Bandwidth for trend line",
                min = 1,
                max = 50,
                step = 1,
                value = 7
              ),
              sliderInput(
                "trajectory_distribution_along_pseudotime_trend_line_width",
                label = "Width of line",
                min = 1,
                max = 10,
                step = 1,
                value = display_defaults$trend_line_width
              )
            )
          ),
          circle = FALSE,
          icon = icon("cog"),
          inline = TRUE,
          size = "xs"
        )
      ),
      tagList(
        selectInput(
          "trajectory_distribution_along_pseudotime_select_variable",
          label = "Variable to show along pseudotime:",
          choices = c(
            "state",
            getVariableToCompareChoices()
          )
        ),
        cerebroChartOutput("trajectory_distribution_along_pseudotime_plot")
      )
    )
  )
})
if (exists("viewer_hidden_output_options", inherits = TRUE)) {
  outputOptions(
    output,
    "trajectory_distribution_along_pseudotime_UI",
    priority = 10
  )
}

##----------------------------------------------------------------------------##
## Plot.
##----------------------------------------------------------------------------##

output[[
  "trajectory_distribution_along_pseudotime_plot"
]] <- renderCerebroChart({
  ## don't do anything before these inputs are selected
  req(
    trajectory_selected_method(),
    trajectory_selected_name(),
    input[["trajectory_distribution_along_pseudotime_select_variable"]],
    !is.null(input[[
      "trajectory_distribution_along_pseudotime_show_trend_line"
    ]]),
    input[["trajectory_distribution_along_pseudotime_trend_line_bandwidth"]]
  )
  ## Density opacity, trend width and point size/opacity are applied in the
  ## browser as they change (`restyle`, www/cerebro-charts.js); a rebuild picks
  ## up their values.
  look <- isolate(list(
    opacity = input[["trajectory_distribution_along_pseudotime_opacity"]],
    trend_width = input[[
      "trajectory_distribution_along_pseudotime_trend_line_width"
    ]],
    point_size = input[["trajectory_point_size"]],
    point_opacity = input[["trajectory_point_opacity"]]
  ))
  restyle <- list(
    trajectory_distribution_along_pseudotime_opacity = "fill_opacity",
    trajectory_distribution_along_pseudotime_trend_line_width = "trend_width",
    trajectory_point_size = "size",
    trajectory_point_opacity = "opacity"
  )

  withProgress(message = "Generating distribution plot...", value = 0, {
    ## collect trajectory data
    trajectory_data <- trajectory_data_reactive()

    incProgress(0.2, detail = "Processing data...")

    ## grab column name for cell coloring
    color_variable <- input[[
      "trajectory_distribution_along_pseudotime_select_variable"
    ]]
    cells_df <- trajectory_cells_reactive(c(color_variable, "nUMI", "nGene"))

    ## ... cells are colored by a categorical variable; the Y axis will show the
    ##     density of the group along pseudotime
    if (
      is.factor(cells_df[[color_variable]]) ||
        is.character(cells_df[[color_variable]])
    ) {
      ## get colors for groups
      colors_for_groups <- assignColorsToGroups(cells_df, color_variable)

      ## get group levels
      if (is.factor(cells_df[[color_variable]])) {
        group_levels <- levels(cells_df[[color_variable]])
      } else if (is.character(cells_df[[color_variable]])) {
        group_levels <- unique(cells_df[[color_variable]])
      }

      incProgress(0.4, detail = "Calculating densities...")

      ## one density over pseudotime for every group level with 2+ cells
      series <- list()
      for (i in seq_along(group_levels)) {
        pseudotime <- cells_df[["pseudotime"]][
          which(cells_df[[color_variable]] == group_levels[i])
        ]
        if (length(pseudotime) > 1) {
          density <- stats::density(pseudotime, kernel = "gaussian")
          series[[length(series) + 1L]] <- list(
            name = group_levels[i],
            color = colors_for_groups[[i]],
            x = density$x,
            y = density$y
          )
        }
      }

      chart <- cerebroLineChartData(
        series,
        x_title = "Pseudotime",
        y_title = "Density",
        x_range = range(cells_df[["pseudotime"]]),
        fill_opacity = look$opacity %||% 0.5,
        legend_title = color_variable
      )

      ## ... cells should be colored by a numeric variable; instead of showing the
      ##     density by pseudotime, the numeric variable will be shown directly on
      ##     the Y axis
    } else {
      incProgress(0.4, detail = "Preparing scatter plot...")

      ## Per-cell names only for small trajectories: the points themselves
      ## travel as binary columns, the names would not.
      hover <- list()
      if (nrow(cells_df) <= 50000L) {
        hover[["Cell"]] <- as.character(
          viewerCellBarcodesAt(cells_df[["cell_index"]])
        )
      }
      if ("nUMI" %in% colnames(cells_df)) {
        hover[["Transcripts"]] <- cells_df[["nUMI"]]
      }
      if ("nGene" %in% colnames(cells_df)) {
        hover[["Expressed genes"]] <- cells_df[["nGene"]]
      }

      ## smoothened trend line, evaluated on a grid along pseudotime
      trend <- NULL
      if (
        isTRUE(input[[
          "trajectory_distribution_along_pseudotime_show_trend_line"
        ]])
      ) {
        trend_line <- stats::ksmooth(
          cells_df[["pseudotime"]],
          cells_df[[color_variable]],
          "normal",
          input[[
            "trajectory_distribution_along_pseudotime_trend_line_bandwidth"
          ]],
          n.points = 512L
        )
        trend <- list(
          x = trend_line$x,
          y = trend_line$y,
          color = cerebro_plotly_theme()$accent,
          width = look$trend_width
        )
      }

      chart <- cerebroScatterData(
        x = cells_df[["pseudotime"]],
        y = cells_df[[color_variable]],
        groups = cells_df[["state"]],
        colors = assignColorsToGroups(trajectory_data[["meta"]], "state"),
        x_title = "Pseudotime",
        y_title = color_variable,
        group_title = "State",
        size = look$point_size,
        opacity = look$point_opacity,
        hover = hover,
        trend = trend
      )
    }
    chart$restyle <- restyle
    chart
  })
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(
  input,
  "trajectory_distribution_along_pseudotime_info",
  trajectory_distribution_along_pseudotime_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_distribution_along_pseudotime_info <- list(
  title = "Distribution along pseudotime",
  text = HTML(
    "
    This plot shows the distribution of the variable selected above to colour cells by along pseudotime.<br>
    If this is a categorical variable, e.g. 'sample' or 'cluster', you will see a density plot. Subgroups of the selected categorical variable that contain only a single cell will not be shown as no density can be calculated for them.<br>
    In contrast, if you have selected a continuous variable, e.g. nUMI or nGene, cells will be coloured by the state they belong to."
  )
)
