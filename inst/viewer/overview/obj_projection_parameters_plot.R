##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
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
    color_variable <- defaults$color_variable
  }
  appearance <- current_scatter_defaults()
  point_size <- input[["overview_projection_point_size"]]
  if (is.null(point_size)) {
    point_size <- appearance$point_size
  }
  point_opacity <- input[["overview_projection_point_opacity"]]
  if (is.null(point_opacity)) {
    point_opacity <- appearance$point_opacity
  }
  group_labels <- input[["overview_projection_group_labels"]]
  if (is.null(group_labels)) {
    group_labels <- TRUE
  }
  req(
    projection,
    color_variable,
    !is.null(preferences[["use_webgl"]]),
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
    draw_border = isTRUE(input[["overview_projection_point_border"]]),
    group_labels = isTRUE(group_labels),
    keep_square = isTRUE(input[["overview_projection_keep_square"]]),
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
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

##
overview_projection_parameters_other <- reactiveValues(
  reset_axes = TRUE
)

##
observeEvent(input[['overview_projection_to_display']], {
  overview_projection_parameters_other[['reset_axes']] <- TRUE
})
