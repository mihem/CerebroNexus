##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
spatial_projection_full_extent <- cachePlot(
  reactive({
    projection <- input[["spatial_projection_to_display"]]
    req(projection, projection %in% availableSpatial())
    dataset <- viewerDatasetName(
      available_crb_files$files,
      available_crb_files$selected
    )
    rotation <- spatialPlotRotation(Cerebro.options, dataset, projection)
    coordinates <- rotateSpatialCoordinates(
      getSpatialData(projection)$coordinates,
      rotation
    )
    x_range <- range(coordinates[[1]], na.rm = TRUE)
    y_range <- range(coordinates[[2]], na.rm = TRUE)
    x_margin <- diff(x_range) * 0.02
    y_margin <- diff(y_range) * 0.02
    ranges_are_finite <- all(is.finite(x_range)) && all(is.finite(y_range))
    list(
      rotation = rotation,
      x_range = if (ranges_are_finite) {
        c(x_range[[1]] - x_margin, x_range[[2]] + x_margin)
      },
      y_range = if (ranges_are_finite) {
        c(y_range[[1]] - y_margin, y_range[[2]] + y_margin)
      }
    )
  }),
  viewerDatasetName(
    available_crb_files$files,
    available_crb_files$selected
  ),
  input[["spatial_projection_to_display"]],
  spatialPlotRotation(
    Cerebro.options,
    viewerDatasetName(
      available_crb_files$files,
      available_crb_files$selected
    ),
    input[["spatial_projection_to_display"]]
  )
)

## Hull geometry changes with cells, groups, projection, or rotation—not with
## point styling. Shiny retains this value until one of those inputs changes.
spatial_projection_group_hulls <- reactive({
  if (
    !isTRUE(input[["spatial_projection_show_region_outlines"]]) ||
      !identical(input[["spatial_projection_plot_type"]], "ImageDimPlot")
  ) {
    return(list())
  }
  color_variable <- input[["spatial_projection_point_color"]]
  metadata <- spatial_projection_metadata()
  req(color_variable, color_variable %in% colnames(metadata))
  color_input <- metadata[[color_variable]]
  if (is.numeric(color_input)) {
    return(list())
  }
  coordinates <- rotateSpatialCoordinates(
    spatial_projection_coordinates(),
    spatial_projection_full_extent()$rotation
  )
  if (ncol(coordinates) != 2L) {
    return(list())
  }
  compute_group_hulls(
    coordinates[[1]],
    coordinates[[2]],
    as.character(color_input)
  )
})

spatial_projection_data_to_plot_raw <- reactive({
  metadata <- spatial_projection_metadata()
  coordinates <- spatial_projection_coordinates()
  plot_parameters <- spatial_projection_parameters_plot()
  hover_info <- spatial_projection_hover_info()
  req(
    metadata,
    coordinates,
    plot_parameters,
    reactive_colors(),
    hover_info,
    !isTRUE(hover_info$enabled) ||
      nrow(metadata) == length(hover_info$selection_key)
  )

  plot_type <- plot_parameters$plot_type
  ## Add the expression columns required by the active Spatial mode.
  uses_expression <- identical(plot_type, "Co-expression (RGB)") ||
    identical(plot_type, "ImageFeaturePlot") &&
      !is.null(plot_parameters$feature_to_display)
  if (uses_expression) {
    cells_to_extract <- if ("cell_barcode" %in% colnames(metadata)) {
      metadata$cell_barcode
    } else {
      rownames(metadata)
    }
    gene_names <- getGeneNames()
    if (identical(plot_type, "ImageFeaturePlot")) {
      gene <- plot_parameters$feature_to_display
      if (gene %in% gene_names) {
        expr_values <- viewerExpressionRow(
          data_set(),
          cells_to_extract,
          gene
        )
        if (!is.null(expr_values)) {
          metadata[[gene]] <- expr_values
        }
      }
    } else {
      ## Keep NULL channels in a list so names cannot shift.
      coexpr_genes <- list(
        coexpr_r = plot_parameters$coexpr_r,
        coexpr_g = plot_parameters$coexpr_g,
        coexpr_b = plot_parameters$coexpr_b
      )
      requested_genes <- unique(unlist(coexpr_genes, use.names = FALSE))
      requested_genes <- requested_genes[
        !is.na(requested_genes) &
          nzchar(requested_genes) &
          requested_genes %in% gene_names
      ]
      expression_values <- viewerExpressionValues(
        data_set(),
        cells_to_extract,
        requested_genes
      )
      for (channel in names(coexpr_genes)) {
        gene <- coexpr_genes[[channel]]
        metadata[[channel]] <- NA_real_
        if (!is.null(gene) && gene %in% names(expression_values)) {
          metadata[[channel]] <- expression_values[[gene]]
        }
      }
    }
  }

  ## get colors for groups (if applicable)
  if (
    plot_parameters[['color_variable']] %in%
      colnames(metadata) &&
      is.numeric(metadata[[plot_parameters[['color_variable']]]])
  ) {
    color_assignments <- NULL
  } else {
    color_assignments <- assignColorsToGroups(
      metadata,
      plot_parameters[['color_variable']]
    )
  }

  ## Plot rotation belongs to one exact dataset + spatial entry. Image rotation
  ## is resolved independently from spatial_image_settings.
  extent <- spatial_projection_full_extent()
  rotation_angle <- extent$rotation
  ## Apply rotation to the displayed (subset) coordinates.
  coordinates <- rotateSpatialCoordinates(
    coordinates,
    rotation_angle
  )

  ## Pin the axes to the FULL cell extent, not the currently displayed subset.
  ## Otherwise, changing "Show % of cells" rescales the axes to whatever subset
  ## is plotted and the plot visibly jitters. We compute the range over ALL cells
  ## (in the same rotated frame) and pass it as an explicit x/y range. A small
  ## margin keeps edge points off the frame.
  if (
    is.null(plot_parameters[["x_range"]]) ||
      length(plot_parameters[["x_range"]]) < 2 ||
      is.null(plot_parameters[["y_range"]]) ||
      length(plot_parameters[["y_range"]]) < 2
  ) {
    plot_parameters[["x_range"]] <- extent$x_range
    plot_parameters[["y_range"]] <- extent$y_range
  }

  ## With an explicit full-extent range we must NOT let the JS autorange (which
  ## would refit to the subset). reset_axes is meant to snap back to the full
  ## view on a dataset switch — that is exactly the full-extent range we set, so
  ## keep the fixed range.
  reset_axes <- isolate(spatial_projection_parameters_other[['reset_axes']])
  if (
    length(plot_parameters[["x_range"]]) >= 2 &&
      length(plot_parameters[["y_range"]]) >= 2
  ) {
    reset_axes <- FALSE
  }

  list(
    cells_df = metadata,
    coordinates = coordinates,
    reset_axes = reset_axes,
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    hover_info = hover_info,
    group_hulls = spatial_projection_group_hulls()
  )
})

spatial_projection_render_event <- viewerProjectionEvent(
  "spatial_projection",
  "spatial",
  extra = function() {
    list(
      plot_type = input[["spatial_projection_plot_type"]],
      feature = input[["spatial_projection_feature_to_display"]],
      coexpr_r = input[["spatial_projection_coexpr_r"]],
      coexpr_g = input[["spatial_projection_coexpr_g"]],
      coexpr_b = input[["spatial_projection_coexpr_b"]],
      region_outlines = input[["spatial_projection_show_region_outlines"]],
      background_image = input[["spatial_projection_background_image"]]
    )
  },
  colors = TRUE
)

spatial_projection_data_to_plot <- debounceEventAfterFirst(
  spatial_projection_render_event,
  spatial_projection_data_to_plot_raw,
  150
)
