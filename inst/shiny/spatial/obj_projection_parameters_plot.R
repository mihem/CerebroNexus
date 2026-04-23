##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
spatial_projection_parameters_plot_raw <- reactive({
  req(
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_to_display"]] %in% availableProjections(),
    input[["spatial_projection_plot_type"]],
    input[["spatial_projection_point_size"]],
    input[["spatial_projection_point_opacity"]],
    !is.null(input[["spatial_projection_point_border"]]),
    input[["spatial_projection_scale_x_manual_range"]],
    input[["spatial_projection_scale_y_manual_range"]],
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  # message('--> trigger "spatial_projection_parameters_plot"')

  plot_type <- input[["spatial_projection_plot_type"]]
  color_variable <- NULL
  feature_to_display <- NULL

  if (plot_type == "ImageDimPlot") {
    color_variable <- input[["spatial_projection_point_color"]]
  } else if (plot_type == "ImageFeaturePlot") {
    feature_to_display <- input[["spatial_projection_feature_to_display"]]
    req(feature_to_display)
    color_variable <- feature_to_display
  }

  background_opacity <- if (is.null(input[["spatial_projection_background_opacity"]])) 1 else input[["spatial_projection_background_opacity"]]

  background_flip_x <- FALSE
  background_flip_y <- FALSE
  background_scale_x <- 1
  background_scale_y <- 1

  if (exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_images_flip_x"]]) &&
      exists("available_crb_files") && !is.null(available_crb_files$selected)) {
    match_idx <- which(available_crb_files$files == available_crb_files$selected)
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (!is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_flip_x"]])) {
        background_flip_x <- Cerebro.options[["spatial_images_flip_x"]][[current_name]]
      }
    }
  }

  if (exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_images_flip_y"]]) &&
      exists("available_crb_files") && !is.null(available_crb_files$selected)) {
    match_idx <- which(available_crb_files$files == available_crb_files$selected)
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (!is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_flip_y"]])) {
        background_flip_y <- Cerebro.options[["spatial_images_flip_y"]][[current_name]]
      }
    }
  }

  if (exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_images_scale_x"]]) &&
      exists("available_crb_files") && !is.null(available_crb_files$selected)) {
    match_idx <- which(available_crb_files$files == available_crb_files$selected)
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (!is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_scale_x"]])) {
        background_scale_x <- Cerebro.options[["spatial_images_scale_x"]][[current_name]]
      }
    }
  }

  if (exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_images_scale_y"]]) &&
      exists("available_crb_files") && !is.null(available_crb_files$selected)) {
    match_idx <- which(available_crb_files$files == available_crb_files$selected)
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (!is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_images_scale_y"]])) {
        background_scale_y <- Cerebro.options[["spatial_images_scale_y"]][[current_name]]
      }
    }
  }

  parameters <- list(
    projection = input[["spatial_projection_to_display"]],
    n_dimensions = ncol(getProjection(input[["spatial_projection_to_display"]])),
    color_variable = color_variable,
    plot_type = plot_type,
    feature_to_display = feature_to_display,
    point_size = input[["spatial_projection_point_size"]],
    point_opacity = input[["spatial_projection_point_opacity"]],
    draw_border = input[["spatial_projection_point_border"]],
    group_labels = input[["spatial_projection_show_group_label"]],
    x_range = input[["spatial_projection_scale_x_manual_range"]],
    y_range = input[["spatial_projection_scale_y_manual_range"]],
    background_image = input[["spatial_projection_background_image"]],
    background_flip_x = background_flip_x,
    background_flip_y = background_flip_y,
    background_scale_x = background_scale_x,
    background_scale_y = background_scale_y,
    background_opacity = background_opacity,
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  # message(str(parameters))
  return(parameters)
})

spatial_projection_parameters_plot <- debounce(spatial_projection_parameters_plot_raw, 500)

##
spatial_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['spatial_projection_to_display']], {
  # message('--> set "spatial: reset_axes"')
  spatial_projection_parameters_other[['reset_axes']] <- TRUE
})
