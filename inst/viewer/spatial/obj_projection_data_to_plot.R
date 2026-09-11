##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
spatial_projection_data_to_plot_raw <- reactive({
  req(
    spatial_projection_metadata(),
    spatial_projection_coordinates(),
    spatial_projection_parameters_plot(),
    reactive_colors(),
    spatial_projection_hover_info(),
    nrow(spatial_projection_metadata()) ==
      length(spatial_projection_hover_info()) ||
      spatial_projection_hover_info() == "none"
  )
  metadata <- spatial_projection_metadata()
  plot_parameters <- spatial_projection_parameters_plot()
  cells_to_extract <- if ("cell_barcode" %in% colnames(metadata)) {
    metadata$cell_barcode
  } else {
    rownames(metadata)
  }

  ## Handle ImageFeaturePlot (add gene expression data)
  if (
    plot_parameters$plot_type == 'ImageFeaturePlot' &&
      !is.null(plot_parameters$feature_to_display)
  ) {
    gene <- plot_parameters$feature_to_display
    if (gene %in% getGeneNames()) {
      expr_values <- viewerExpressionRow(
        data_set(),
        cells_to_extract,
        gene
      )
      if (!is.null(expr_values)) {
        metadata[[gene]] <- expr_values
      }
    }
  }

  ## Co-expression: pull each channel's gene expression into metadata columns
  ## keyed by a stable channel name, so the renderer can blend them onto RGB.
  if (plot_parameters$plot_type == "Co-expression (RGB)") {
    ## Use a list, not c(): an empty channel is NULL, and c() would DROP it and
    ## shift the remaining names, misaligning genes to channels.
    coexpr_genes <- list(
      coexpr_r = plot_parameters$coexpr_r,
      coexpr_g = plot_parameters$coexpr_g,
      coexpr_b = plot_parameters$coexpr_b
    )
    requested_genes <- unique(unlist(coexpr_genes, use.names = FALSE))
    requested_genes <- requested_genes[
      !is.na(requested_genes) &
        nzchar(requested_genes) &
        requested_genes %in% getGeneNames()
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
  current_name <- viewerDatasetName(
    available_crb_files$files,
    available_crb_files$selected
  )
  rotation_angle <- spatialPlotRotation(
    Cerebro.options,
    current_name,
    plot_parameters[["projection"]]
  )
  ## Apply rotation to the displayed (subset) coordinates.
  coordinates <- rotateSpatialCoordinates(
    spatial_projection_coordinates(),
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
    full_coords <- rotateSpatialCoordinates(
      getSpatialData(plot_parameters[["projection"]])$coordinates,
      rotation_angle
    )
    x_full <- range(full_coords[[1]], na.rm = TRUE)
    y_full <- range(full_coords[[2]], na.rm = TRUE)
    x_margin <- diff(x_full) * 0.02
    y_margin <- diff(y_full) * 0.02
    if (all(is.finite(x_full)) && all(is.finite(y_full))) {
      plot_parameters[["x_range"]] <- c(
        x_full[1] - x_margin,
        x_full[2] + x_margin
      )
      plot_parameters[["y_range"]] <- c(
        y_full[1] - y_margin,
        y_full[2] + y_margin
      )
    }
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

  ## return collect data
  to_return <- list(
    cells_df = metadata,
    coordinates = coordinates,
    reset_axes = reset_axes,
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    hover_info = spatial_projection_hover_info()
  )

  return(to_return)
})

spatial_projection_data_to_plot <- debounce(
  spatial_projection_data_to_plot_raw,
  150
)
