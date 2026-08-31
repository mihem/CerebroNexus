##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
overview_projection_parameters_plot <- reactive({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections(),
    input[["overview_projection_point_color"]],
    input[["overview_projection_point_color"]] %in% colnames(getMetaData()),
    input[["overview_projection_point_size"]],
    input[["overview_projection_point_opacity"]],
    !is.null(input[["overview_projection_point_border"]]),
    !is.null(input[["overview_projection_keep_square"]]),
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  projection_data <- getProjection(input[["overview_projection_to_display"]])
  XYranges <- getXYranges(projection_data)
  parameters <- list(
    projection = input[["overview_projection_to_display"]],
    n_dimensions = ncol(projection_data),
    color_variable = input[["overview_projection_point_color"]],
    point_size = input[["overview_projection_point_size"]],
    point_opacity = input[["overview_projection_point_opacity"]],
    draw_border = input[["overview_projection_point_border"]],
    group_labels = isTRUE(input[["overview_projection_group_labels"]]),
    keep_square = isTRUE(input[["overview_projection_keep_square"]]),
    x_range = c(XYranges$x$min, XYranges$x$max),
    y_range = c(XYranges$y$min, XYranges$y$max),
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

##
overview_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['overview_projection_to_display']], {
  overview_projection_parameters_other[['reset_axes']] <- TRUE
})
