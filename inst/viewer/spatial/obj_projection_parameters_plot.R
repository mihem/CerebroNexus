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
  } else if (plot_type == "Co-expression (RGB)") {
    ## One gene per channel; any channel may be empty. Require at least one so
    ## the render has something to colour by.
    coexpr_genes <- list(
      r = input[["spatial_projection_coexpr_r"]],
      g = input[["spatial_projection_coexpr_g"]],
      b = input[["spatial_projection_coexpr_b"]]
    )
    blank <- function(x) is.null(x) || !nzchar(x)
    req(
      !(blank(coexpr_genes$r) && blank(coexpr_genes$g) && blank(coexpr_genes$b))
    )
    ## The cell colour comes from the RGB blend, not a metadata column, but the
    ## downstream code still indexes metadata[[color_variable]] — give it a valid
    ## placeholder column so that lookup can't fail on a NULL name.
    color_variable <- colnames(getMetaData())[1]
  }

  ## Background APPEARANCE (opacity, move, flip, scale, rotate) is deliberately
  ## NOT read here. Those are decoupled from the scatter plot: they flow through
  ## an independent observer -> shared background action, which redraws the
  ## existing Canvas without rebuilding its cell payload. Reading them in this
  ## reactive would resend the whole plot on every opacity/move tick. isolate()
  ## the initial opacity so the first
  ## render of a freshly chosen image starts at the current slider value without
  ## creating a reactive dependency on it.
  background_opacity <- isolate({
    if (is.null(input[["spatial_projection_background_opacity"]])) {
      1
    } else {
      input[["spatial_projection_background_opacity"]]
    }
  })

  spatial_data <- getSpatialData(input[["spatial_projection_to_display"]])
  n_dimensions <- ncol(spatial_data$coordinates)
  spatial_name <- input[["spatial_projection_to_display"]]
  dataset <- spatial_dataset_name(
    if (exists("available_crb_files")) available_crb_files$files else NULL,
    if (exists("available_crb_files")) available_crb_files$selected else NULL
  )
  embedded_images <- embedded_spatial_images(spatial_data)
  configured_background_images <- configured_spatial_images(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name
  )
  background_choices <- spatial_background_choices(
    embedded_images,
    configured_background_images
  )
  background_image <- normalize_spatial_background_choice(
    input[["spatial_projection_background_image"]],
    background_choices
  )
  background_descriptor <- resolve_spatial_background(
    background_image,
    embedded_images,
    configured_background_images
  )
  background_identity <- spatial_background_identity(
    dataset,
    spatial_name,
    background_descriptor
  )
  image_label <- if (is.null(background_descriptor)) {
    NULL
  } else {
    background_descriptor$label
  }
  resolve_bg_setting <- function(setting, fallback) {
    if (is.null(image_label)) {
      return(fallback)
    }
    resolve_spatial_image_setting(
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      spatial_name,
      image_label,
      setting,
      fallback
    )
  }
  background_flip_x <- isTRUE(resolve_bg_setting("flip_x", FALSE))
  background_flip_y <- isTRUE(resolve_bg_setting("flip_y", FALSE))
  background_scale_x <- resolve_bg_setting("scale_x", 1)
  background_scale_y <- resolve_bg_setting("scale_y", 1)
  background_offset_x <- resolve_bg_setting("offset_x", 0)
  background_offset_y <- resolve_bg_setting("offset_y", 0)
  background_rotation <- resolve_bg_setting("rotation", 0)

  parameters <- list(
    projection = input[["spatial_projection_to_display"]],
    n_dimensions = n_dimensions,
    color_variable = color_variable,
    plot_type = plot_type,
    feature_to_display = feature_to_display,
    coexpr_r = input[["spatial_projection_coexpr_r"]],
    coexpr_g = input[["spatial_projection_coexpr_g"]],
    coexpr_b = input[["spatial_projection_coexpr_b"]],
    point_size = input[["spatial_projection_point_size"]],
    point_opacity = input[["spatial_projection_point_opacity"]],
    draw_border = input[["spatial_projection_point_border"]],
    group_labels = input[["spatial_projection_show_group_label"]],
    show_region_outlines = isTRUE(
      input[["spatial_projection_show_region_outlines"]]
    ),
    x_range = input[["spatial_projection_scale_x_manual_range"]],
    y_range = input[["spatial_projection_scale_y_manual_range"]],
    background_image = background_image,
    background_descriptor = background_descriptor,
    background_identity = background_identity,
    background_image_allowlist = vapply(
      configured_background_images,
      `[[`,
      character(1),
      "path"
    ),
    background_flip_x = background_flip_x,
    background_flip_y = background_flip_y,
    background_scale_x = background_scale_x,
    background_scale_y = background_scale_y,
    background_offset_x = background_offset_x,
    background_offset_y = background_offset_y,
    background_rotation = background_rotation,
    background_opacity = background_opacity,
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
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
  spatial_projection_parameters_other[['reset_axes']] <- TRUE
})
