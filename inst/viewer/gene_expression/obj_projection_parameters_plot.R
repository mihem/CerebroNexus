##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
expression_projection_parameters_plot <- reactive({
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_plotting_order"]],
    input[["expression_projection_point_size"]],
    input[["expression_projection_point_opacity"]],
    !is.null(input[["expression_projection_point_border"]]),
    !is.null(input[["expression_projection_keep_square"]]),
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]]),
    input[["expression_projection_to_display"]] %in%
      availableProjections() ||
      input[["expression_projection_to_display"]] %in% available_trajectories()
  )
  selected_projection <- input[["expression_projection_to_display"]]
  if (input[["expression_projection_to_display"]] %in% availableProjections()) {
    is_trajectory <- FALSE
    range_data <- getProjection(selected_projection)
    n_dimensions <- ncol(range_data)
  } else {
    is_trajectory <- TRUE
    # currently, only trajectories with 2 dimensions are supported
    n_dimensions <- 2
    selection <- strsplit(selected_projection, split = " // ")[[1]]
    range_data <- getTrajectory(selection[[1]], selection[[2]])[["meta"]]
  }
  XYranges <- getXYranges(range_data)
  parameters <- list(
    projection = selected_projection,
    plot_order = input[["expression_projection_plotting_order"]],
    n_dimensions = n_dimensions,
    is_trajectory = is_trajectory,
    point_size = input[["expression_projection_point_size"]],
    point_opacity = input[["expression_projection_point_opacity"]],
    draw_border = input[["expression_projection_point_border"]],
    keep_square = isTRUE(input[["expression_projection_keep_square"]]),
    x_range = c(XYranges$x$min, XYranges$x$max),
    y_range = c(XYranges$y$min, XYranges$y$max),
    webgl = preferences[["use_webgl"]],
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
