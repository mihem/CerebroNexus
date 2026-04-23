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
    nrow(spatial_projection_metadata()) == length(spatial_projection_hover_info()) || spatial_projection_hover_info() == "none"
  )
  metadata <- spatial_projection_metadata()
  plot_parameters <- spatial_projection_parameters_plot()

  ## Handle ImageFeaturePlot (add gene expression data)
  if ( plot_parameters$plot_type == 'ImageFeaturePlot' && !is.null(plot_parameters$feature_to_display) ) {
    gene <- plot_parameters$feature_to_display
    if ( gene %in% getGeneNames() ) {
      # Use cell_barcode column if available, otherwise fallback to rownames
      if ( "cell_barcode" %in% colnames(metadata) ) {
        cells_to_extract <- metadata$cell_barcode
      } else {
        cells_to_extract <- rownames(metadata)
      }
      # Access expression matrix safely
      expression_data <- getExpressionMatrix()
      if (!is.null(expression_data) && gene %in% rownames(expression_data)) {
        expr_values <- as.vector(expression_data[gene, cells_to_extract])
        metadata[[gene]] <- expr_values
      }
    }
  }

  ## get colors for groups (if applicable)
  if (
    plot_parameters[['color_variable']] %in% colnames(metadata) &&
    is.numeric(metadata[[ plot_parameters[['color_variable']] ]])
  ) {
    color_assignments <- NA
  } else {
    color_assignments <- assignColorsToGroups(
      metadata,
      plot_parameters[['color_variable']]
    )
  }

  ## Apply rotation to coordinates if configured
  coordinates <- spatial_projection_coordinates()
  if (exists("Cerebro.options") && !is.null(Cerebro.options[["spatial_plot_rotation"]]) &&
      exists("available_crb_files") && !is.null(available_crb_files$selected)) {
    match_idx <- which(available_crb_files$files == available_crb_files$selected)
    if (length(match_idx) > 0) {
      current_name <- names(available_crb_files$files)[match_idx[1]]
      if (!is.null(current_name) && current_name %in% names(Cerebro.options[["spatial_plot_rotation"]])) {
        rotation_angle <- Cerebro.options[["spatial_plot_rotation"]][[current_name]]
        if (!is.null(rotation_angle) && rotation_angle != 0) {
          theta <- rotation_angle * pi / 180
          cos_theta <- cos(theta)
          sin_theta <- sin(theta)
          x <- coordinates[, 1]
          y <- coordinates[, 2]
          coordinates[, 1] <- x * cos_theta - y * sin_theta
          coordinates[, 2] <- x * sin_theta + y * cos_theta
        }
      }
    }
  }

  ## return collect data
  to_return <- list(
    cells_df = metadata,
    coordinates = coordinates,
    reset_axes = isolate(spatial_projection_parameters_other[['reset_axes']]),
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    hover_info = spatial_projection_hover_info()
  )

  return(to_return)
})

spatial_projection_data_to_plot <- debounce(spatial_projection_data_to_plot_raw, 150)
