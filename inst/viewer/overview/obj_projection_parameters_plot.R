##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
cerebroObserveLocalAppearance("overview_projection", input, session)
overview_projection_parameters_plot <- reactive({
  metadata <- viewerProjectionFirstFrameMetadata()
  projections <- availableProjections()
  defaults <- viewerProjectionDefaults(
    metadata,
    projections,
    tryCatch(getParameters(), error = function(e) list())
  )
  projection <- input[["overview_projection_to_display"]]
  if (is.null(projection) || !projection %in% projections) {
    projection <- defaults$projection
  }
  color_variable <- input[["overview_projection_point_color"]]
  if (is.null(color_variable) || !color_variable %in% colnames(metadata)) {
    ## What "Colour by" opens with (UI_projection_main_parameters.R): the
    ## first frame is drawn before the control binds, and its first value then
    ## changes nothing.
    color_variable <- viewerColourGroupChoices(metadata)$selected %||%
      defaults$color_variable
  }
  appearance <- isolate(current_scatter_defaults())
  display <- current_page_display_defaults("projection")
  point_size <- appearance$point_size
  point_opacity <- appearance$point_opacity
  group_labels <- isolate(input[["overview_projection_group_labels"]])
  if (is.null(group_labels)) {
    group_labels <- display$group_labels
  }
  req(
    projection,
    color_variable,
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  projection_resource <- viewerProjectionAsset(
    projection,
    overview_projection_cells_to_show()
  )
  projection_data <- if (is.list(projection_resource)) {
    NULL
  } else {
    viewerProjectionFirstFrameCoordinates(projection)
  }
  XYranges <- if (is.null(projection_data)) {
    NULL
  } else {
    getXYranges(projection_data)
  }
  parameters <- list(
    projection = projection,
    n_dimensions = if (is.list(projection_resource)) {
      as.integer(projection_resource$dimensions)
    } else {
      ncol(projection_data)
    },
    color_variable = color_variable,
    point_size = point_size,
    point_opacity = point_opacity,
    draw_border = isTRUE(
      isolate(input[["overview_projection_point_border"]]) %||%
        display$draw_border
    ),
    group_labels = isTRUE(group_labels),
    keep_square = isTRUE(
      isolate(input[["overview_projection_keep_square"]]) %||%
        display$keep_square
    ),
    x_range = if (is.null(XYranges)) {
      list()
    } else {
      c(XYranges$x$min, XYranges$x$max)
    },
    y_range = if (is.null(XYranges)) {
      list()
    } else {
      c(XYranges$y$min, XYranges$y$max)
    },
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

##
overview_projection_parameters_other <- reactiveValues(
  reset_axes = TRUE
)

## Reset the axes when the user picks another projection. The selector binding
## with its first value is not a change: a stale TRUE would reset the view on
## the next colour or filter change.
overview_projection_previous_projection <- NULL
observeEvent(input[['overview_projection_to_display']], {
  projection <- input[['overview_projection_to_display']]
  if (
    !is.null(overview_projection_previous_projection) &&
      !identical(projection, overview_projection_previous_projection)
  ) {
    overview_projection_parameters_other[['reset_axes']] <- TRUE
  }
  overview_projection_previous_projection <<- projection
})
