##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
cerebroObserveLocalAppearance("expression_projection", input, session)
expression_projection_parameters_plot <- reactive({
  available_projections <- availableProjections()
  available_trajectory_names <- available_trajectories()
  selected_projection <- input[["expression_projection_to_display"]]
  if (
    is.null(selected_projection) ||
      !selected_projection %in%
        c(
          available_projections,
          available_trajectory_names
        )
  ) {
    shared <- isolate(input[["coordviews_shared_base"]])
    selected_projection <- as.character(shared$projection %||% "")
    if (!selected_projection %in% available_projections) {
      selected_projection <- if (length(available_projections)) {
        available_projections[[1L]]
      } else if (length(available_trajectory_names)) {
        available_trajectory_names[[1L]]
      } else {
        NULL
      }
    }
  }
  req(
    selected_projection,
    !is.null(input[["expression_projection_plotting_order"]]),
    !is.null(preferences[["show_hover_info_in_projections"]]),
    selected_projection %in%
      available_projections ||
      selected_projection %in% available_trajectory_names
  )
  if (selected_projection %in% available_projections) {
    is_trajectory <- FALSE
    range_data <- viewerProjectionFirstFrameCoordinates(selected_projection)
    n_dimensions <- ncol(range_data)
  } else {
    is_trajectory <- TRUE
    # currently, only trajectories with 2 dimensions are supported
    n_dimensions <- 2
    selection <- strsplit(selected_projection, split = " // ")[[1]]
    range_data <- getTrajectory(selection[[1]], selection[[2]])[["meta"]]
  }
  XYranges <- getXYranges(range_data)
  display <- current_page_display_defaults("gene")
  parameters <- list(
    projection = selected_projection,
    plot_order = if (isTRUE(input[["expression_projection_plotting_order"]])) {
      "Highest expression on top"
    } else {
      "Input order"
    },
    n_dimensions = n_dimensions,
    is_trajectory = is_trajectory,
    point_size = isolate(current_scatter_defaults()$point_size),
    point_opacity = isolate(current_scatter_defaults()$point_opacity),
    draw_border = isTRUE(
      isolate(input[["expression_projection_point_border"]]) %||%
        display$draw_border
    ),
    keep_square = isTRUE(
      isolate(input[["expression_projection_keep_square"]]) %||%
        display$keep_square
    ),
    x_range = c(XYranges$x$min, XYranges$x$max),
    y_range = c(XYranges$y$min, XYranges$y$max),
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

##
expression_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['expression_projection_to_display']], {
  expression_projection_parameters_other[['reset_axes']] <- TRUE
})
