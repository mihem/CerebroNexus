##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
spatial_projection_parameters_plot_raw <- reactive({
  req(
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    input[["spatial_projection_plot_type"]],
    input[["spatial_projection_point_size"]],
    input[["spatial_projection_point_opacity"]],
    !is.null(input[["spatial_projection_point_border"]]),
    input[["spatial_projection_scale_x_manual_range"]],
    input[["spatial_projection_scale_y_manual_range"]],
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  message(
    '[spatial] params reactive triggered, background = ',
    input[["spatial_projection_background_image"]]
  )

  plot_type <- input[["spatial_projection_plot_type"]]
  color_variable <- NULL
  feature_to_display <- NULL

  if (plot_type == "ImageDimPlot") {
    color_variable <- input[["spatial_projection_point_color"]]
    ## When the loaded .crb is switched, the point-colour dropdown can still hold
    ## a column name from the previous dataset (e.g. Xenium colours by "cluster",
    ## MERFISH by "cell_type"). Colouring by a column the new metadata lacks
    ## makes the downstream dplyr::group_by() error out and the plot freezes on
    ## the old dataset. Fall back to the first available grouping variable (or the
    ## first metadata column) until the dropdown catches up.
    meta_cols <- colnames(getMetaData())
    if (
      is.null(color_variable) ||
        !(color_variable %in% meta_cols)
    ) {
      groups <- getGroups()
      color_variable <- if (length(groups) > 0 && groups[1] %in% meta_cols) {
        groups[1]
      } else {
        meta_cols[1]
      }
    }
  } else if (plot_type == "ImageFeaturePlot") {
    feature_to_display <- input[["spatial_projection_feature_to_display"]]
    req(feature_to_display)
    color_variable <- feature_to_display
  }

  ## Background APPEARANCE (opacity, move, flip, scale, rotate) is deliberately
  ## NOT read here. Those are decoupled from the scatter plot: they flow through
  ## an independent observer -> shinyjs.updateSpatialBackgroundAppearance, which
  ## only re-styles the background <div>. Reading them in this reactive would make
  ## the whole plot re-render (Plotly.react) on every opacity/move tick, which is
  ## exactly the coupling we removed. isolate() the initial opacity so the first
  ## render of a freshly chosen image starts at the current slider value without
  ## creating a reactive dependency on it.
  background_opacity <- isolate({
    if (is.null(input[["spatial_projection_background_opacity"]])) {
      1
    } else {
      input[["spatial_projection_background_opacity"]]
    }
  })

  background_flip_x <- FALSE
  background_flip_y <- FALSE
  background_scale_x <- 1
  background_scale_y <- 1
  background_offset_x <- 0
  background_offset_y <- 0

  ## Resolve a per-dataset `spatial_images_*` preset by the current dataset name.
  ## Returns `fallback` when unset. Used to seed the background transform so the
  ## overlay opens pre-aligned (see the flip/scale blocks below and the offset
  ## block that follows).
  resolve_bg_preset <- function(option_name, fallback) {
    if (
      !exists("Cerebro.options") ||
        is.null(Cerebro.options[[option_name]]) ||
        !exists("available_crb_files") ||
        is.null(available_crb_files$selected)
    ) {
      return(fallback)
    }
    match_idx <- which(
      available_crb_files$files == available_crb_files$selected
    )
    if (length(match_idx) == 0) {
      return(fallback)
    }
    current_name <- names(available_crb_files$files)[match_idx[1]]
    if (
      is.null(current_name) ||
        !(current_name %in% names(Cerebro.options[[option_name]]))
    ) {
      return(fallback)
    }
    val <- Cerebro.options[[option_name]][[current_name]]
    if (is.null(val) || length(val) != 1 || is.na(val)) fallback else val
  }
  background_offset_x <- resolve_bg_preset("spatial_images_offset_x", 0)
  background_offset_y <- resolve_bg_preset("spatial_images_offset_y", 0)

  if (
    exists("Cerebro.options") &&
      !is.null(Cerebro.options[["spatial_images_flip_x"]]) &&
      exists("available_crb_files") &&
      !is.null(available_crb_files$selected)
  ) {
    match_idx <- which(
      available_crb_files$files == available_crb_files$selected
    )
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (
        !is.null(current_name) &&
          current_name %in% names(Cerebro.options[["spatial_images_flip_x"]])
      ) {
        background_flip_x <- Cerebro.options[["spatial_images_flip_x"]][[
          current_name
        ]]
      }
    }
  }

  if (
    exists("Cerebro.options") &&
      !is.null(Cerebro.options[["spatial_images_flip_y"]]) &&
      exists("available_crb_files") &&
      !is.null(available_crb_files$selected)
  ) {
    match_idx <- which(
      available_crb_files$files == available_crb_files$selected
    )
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (
        !is.null(current_name) &&
          current_name %in% names(Cerebro.options[["spatial_images_flip_y"]])
      ) {
        background_flip_y <- Cerebro.options[["spatial_images_flip_y"]][[
          current_name
        ]]
      }
    }
  }

  if (
    exists("Cerebro.options") &&
      !is.null(Cerebro.options[["spatial_images_scale_x"]]) &&
      exists("available_crb_files") &&
      !is.null(available_crb_files$selected)
  ) {
    match_idx <- which(
      available_crb_files$files == available_crb_files$selected
    )
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (
        !is.null(current_name) &&
          current_name %in% names(Cerebro.options[["spatial_images_scale_x"]])
      ) {
        background_scale_x <- Cerebro.options[["spatial_images_scale_x"]][[
          current_name
        ]]
      }
    }
  }

  if (
    exists("Cerebro.options") &&
      !is.null(Cerebro.options[["spatial_images_scale_y"]]) &&
      exists("available_crb_files") &&
      !is.null(available_crb_files$selected)
  ) {
    match_idx <- which(
      available_crb_files$files == available_crb_files$selected
    )
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (
        !is.null(current_name) &&
          current_name %in% names(Cerebro.options[["spatial_images_scale_y"]])
      ) {
        background_scale_y <- Cerebro.options[["spatial_images_scale_y"]][[
          current_name
        ]]
      }
    }
  }

  spatial_data <- getSpatialData(input[["spatial_projection_to_display"]])
  n_dimensions <- ncol(spatial_data$coordinates)

  ## A .crb built from real data may embed the genuine histology image (base64
  ## data: URI) plus its extent in coordinate space. When present it is offered
  ## as a background choice ("__embedded__") and rendered directly, aligned via
  ## its stored bounds — no external file, no manual flip/scale.
  embedded_image <- spatial_data$histology_image
  embedded_bounds <- spatial_data$histology_image_bounds

  ## Normalise the background choice against the CURRENT dataset. When the user
  ## switches from an image-bearing demo (where they picked "__embedded__") to
  ## one without an embedded image (e.g. Xenium -> Slide-seq), the stale
  ## "__embedded__" input value would otherwise leave `background_image` pointing
  ## at an image this dataset does not have, wedging the plot update. Fall back to
  ## no background whenever the embedded image is absent.
  background_image <- input[["spatial_projection_background_image"]]
  if (identical(background_image, "__embedded__") && is.null(embedded_image)) {
    background_image <- "No Background"
  }

  parameters <- list(
    projection = input[["spatial_projection_to_display"]],
    n_dimensions = n_dimensions,
    color_variable = color_variable,
    plot_type = plot_type,
    feature_to_display = feature_to_display,
    point_size = input[["spatial_projection_point_size"]],
    point_opacity = input[["spatial_projection_point_opacity"]],
    draw_border = input[["spatial_projection_point_border"]],
    group_labels = input[["spatial_projection_show_group_label"]],
    x_range = input[["spatial_projection_scale_x_manual_range"]],
    y_range = input[["spatial_projection_scale_y_manual_range"]],
    background_image = background_image,
    embedded_image = embedded_image,
    embedded_bounds = embedded_bounds,
    background_flip_x = background_flip_x,
    background_flip_y = background_flip_y,
    background_scale_x = background_scale_x,
    background_scale_y = background_scale_y,
    background_offset_x = background_offset_x,
    background_offset_y = background_offset_y,
    background_opacity = background_opacity,
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  # message(str(parameters))
  return(parameters)
})

spatial_projection_parameters_plot <- debounce(
  spatial_projection_parameters_plot_raw,
  500
)

##
spatial_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['spatial_projection_to_display']], {
  # message('--> set "spatial: reset_axes"')
  spatial_projection_parameters_other[['reset_axes']] <- TRUE
})
