##----------------------------------------------------------------------------##
## Collect parameters for projection plot.
##----------------------------------------------------------------------------##
## Hidden Settings inputs bind after the first frame. Keep their defaults in
## server state so that mounting the drawer cannot invalidate the million-cell
## data reactive. Labels, borders and square layout are browser-only appearance
## patches; region outlines remain reactive because they require hull geometry.
spatial_projection_appearance <- reactiveValues(
  group_labels = FALSE,
  draw_border = FALSE,
  keep_square = FALSE
)
spatial_projection_region_outlines <- reactiveVal(FALSE)

## The Spatial shell can request its first frame before its renderUI controls
## have made a browser round trip. Resolve the exact same defaults on the
## server and publish them through a deduplicating reactive value: later input
## bindings with those values are a no-op instead of causing a second 1M frame.
spatial_projection_controls <- reactiveVal(NULL)

observe(
  {
    req(input[["spatial_projection_render_request"]])
    spatial_names <- availableSpatial()
    req(length(spatial_names) > 0L)

    defaults <- current_scatter_defaults()
    projection <- input[["spatial_projection_to_display"]]
    if (is.null(projection) || !(projection %in% spatial_names)) {
      projection <- spatial_names[[1L]]
    }
    plot_type <- input[["spatial_projection_plot_type"]]
    if (is.null(plot_type) || !nzchar(plot_type)) {
      plot_type <- "ImageDimPlot"
    }

    exclude_trivial <- isTRUE(as.logical(
      Cerebro.options[["exclude_trivial_metadata"]]
    ))
    color_choices <- if (exclude_trivial) {
      getGroups()
    } else {
      setdiff(colnames(viewerProjectionFirstFrameMetadata()), "cell_barcode")
    }
    color_variable <- input[["spatial_projection_point_color"]]
    if (is.null(color_variable) || !(color_variable %in% color_choices)) {
      req(length(color_choices) > 0L)
      color_variable <- color_choices[[1L]]
    }

    value_or_default <- function(value, default) {
      if (is.null(value) || !length(value) || is.na(value[[1L]])) {
        default
      } else {
        value[[1L]]
      }
    }
    controls <- list(
      projection = as.character(projection[[1L]]),
      plot_type = as.character(plot_type[[1L]]),
      color_variable = as.character(color_variable[[1L]]),
      feature_to_display = input[["spatial_projection_feature_to_display"]],
      coexpr_r = input[["spatial_projection_coexpr_r"]],
      coexpr_g = input[["spatial_projection_coexpr_g"]],
      coexpr_b = input[["spatial_projection_coexpr_b"]],
      point_size = as.numeric(value_or_default(
        input[["spatial_projection_point_size"]],
        defaults$point_size
      )),
      point_opacity = as.numeric(value_or_default(
        input[["spatial_projection_point_opacity"]],
        defaults$point_opacity
      )),
      percentage_cells_to_show = as.numeric(value_or_default(
        input[["spatial_projection_percentage_cells_to_show"]],
        defaults$percentage_cells_to_show
      ))
    )
    if (!identical(isolate(spatial_projection_controls()), controls)) {
      spatial_projection_controls(controls)
    }
  },
  priority = 1000
)

spatial_update_appearance <- function(name, value) {
  if (identical(spatial_projection_appearance[[name]], value)) {
    return(FALSE)
  }
  spatial_projection_appearance[[name]] <- value
  TRUE
}

spatial_send_appearance <- function(values) {
  identity <- viewerDatasetIdentity()
  req(identity$fingerprint)
  session$sendCustomMessage(
    "cell_view_appearance",
    list(
      id = "spatial_projection",
      dataset_context = viewer_loaded_dataset_context(),
      dataset_fingerprint = identity$fingerprint,
      values = values
    )
  )
}

observeEvent(
  input[["spatial_projection_group_labels"]],
  {
    value <- isTRUE(input[["spatial_projection_group_labels"]])
    if (spatial_update_appearance("group_labels", value)) {
      spatial_send_appearance(list(group_labels = value))
    }
  },
  ignoreNULL = TRUE
)

observeEvent(
  input[["spatial_projection_point_border"]],
  {
    value <- isTRUE(input[["spatial_projection_point_border"]])
    if (spatial_update_appearance("draw_border", value)) {
      spatial_send_appearance(list(draw_border = value))
    }
  },
  ignoreNULL = TRUE
)

observeEvent(
  input[["spatial_projection_keep_square"]],
  {
    value <- isTRUE(input[["spatial_projection_keep_square"]])
    if (spatial_update_appearance("keep_square", value)) {
      spatial_send_appearance(list(keep_square = value))
    }
  },
  ignoreNULL = TRUE
)

observeEvent(
  input[["spatial_projection_show_region_outlines"]],
  {
    value <- isTRUE(input[["spatial_projection_show_region_outlines"]])
    if (!identical(spatial_projection_region_outlines(), value)) {
      spatial_projection_region_outlines(value)
    }
  },
  ignoreNULL = TRUE
)

spatial_projection_parameters_plot <- reactive({
  controls <- spatial_projection_controls()
  req(
    controls,
    controls$projection %in% availableSpatial(),
    controls$plot_type,
    controls$point_size,
    controls$point_opacity,
    !is.null(preferences[["use_webgl"]]),
    !is.null(preferences[["show_hover_info_in_projections"]])
  )
  plot_type <- controls$plot_type
  color_variable <- NULL
  feature_to_display <- NULL

  if (plot_type == "ImageDimPlot") {
    color_variable <- controls$color_variable
    req(
      color_variable,
      color_variable %in% colnames(viewerProjectionFirstFrameMetadata())
    )
  } else if (plot_type == "ImageFeaturePlot") {
    feature_to_display <- controls$feature_to_display
    req(feature_to_display)
    color_variable <- feature_to_display
  } else if (plot_type == "Co-expression (RGB)") {
    ## One gene per channel; any channel may be empty. Require at least one so
    ## the render has something to colour by.
    coexpr_genes <- list(
      r = controls$coexpr_r,
      g = controls$coexpr_g,
      b = controls$coexpr_b
    )
    blank <- function(x) is.null(x) || !nzchar(x)
    req(
      !(blank(coexpr_genes$r) && blank(coexpr_genes$g) && blank(coexpr_genes$b))
    )
    ## The cell colour comes from the RGB blend, not a metadata column, but the
    ## downstream code still indexes metadata[[color_variable]] — give it a valid
    ## placeholder column so that lookup can't fail on a NULL name.
    color_variable <- "cell_index"
  }

  spatial_data <- viewerSpatialFirstFrameData(
    controls$projection
  )
  req(spatial_data, spatial_data$coordinates)
  n_dimensions <- ncol(spatial_data$coordinates)
  spatial_name <- controls$projection
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
  background_preset <- spatialImagePreset(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name,
    image_label
  )
  ## Every new image starts from its own preset. Interactive appearance changes
  ## arrive later as one context-and-image-scoped atomic payload, so stale
  ## dynamic input bindings can never seed a new render.
  background_opacity <- background_preset$opacity

  parameters <- list(
    projection = controls$projection,
    n_dimensions = n_dimensions,
    color_variable = color_variable,
    plot_type = plot_type,
    feature_to_display = feature_to_display,
    coexpr_r = controls$coexpr_r,
    coexpr_g = controls$coexpr_g,
    coexpr_b = controls$coexpr_b,
    point_size = controls$point_size,
    point_opacity = controls$point_opacity,
    draw_border = isolate(spatial_projection_appearance$draw_border),
    group_labels = isolate(spatial_projection_appearance$group_labels),
    keep_square = isolate(spatial_projection_appearance$keep_square),
    show_region_outlines = spatial_projection_region_outlines(),
    x_range = NULL,
    y_range = NULL,
    background_image = background_image,
    background_descriptor = background_descriptor,
    background_identity = background_identity,
    background_image_allowlist = vapply(
      configured_background_images,
      `[[`,
      character(1),
      "path"
    ),
    background_flip_x = background_preset$flipX,
    background_flip_y = background_preset$flipY,
    background_scale_x = background_preset$scaleX,
    background_scale_y = background_preset$scaleY,
    background_offset_x = background_preset$offsetX,
    background_offset_y = background_preset$offsetY,
    background_rotation = background_preset$rotation,
    background_opacity = background_opacity,
    webgl = preferences[["use_webgl"]],
    hover_info = preferences[["show_hover_info_in_projections"]]
  )
  return(parameters)
})

##
spatial_projection_parameters_other <- reactiveValues(
  reset_axes = FALSE
)

##
observeEvent(input[['spatial_projection_to_display']], {
  spatial_projection_parameters_other[['reset_axes']] <- TRUE
})
