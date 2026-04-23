##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
spatial_projection_update_plot <- function(input) {
  ## assign input data to new variables
  metadata          <- input[['cells_df']]
  coordinates       <- input[['coordinates']]
  reset_axes        <- input[['reset_axes']]
  plot_parameters   <- input[['plot_parameters']]
  color_assignments <- input[['color_assignments']]
  hover_info        <- input[['hover_info']]
  color_input       <- metadata[[ plot_parameters[['color_variable']] ]]

  ## get container dimensions
  container_dimensions <- shinyjs::js$getContainerDimensions()
  container_info <- list(
    width  = container_dimensions[['width']],
    height = container_dimensions[['height']]
  )

  ## prepare background image data and bounds if selected
  background_image_data <- NULL
  image_bounds <- list()

  if (!is.null(plot_parameters[['background_image']]) &&
    plot_parameters[['background_image']] != "No Background") {

    img_path <- plot_parameters[['background_image']]

    # Calculate bounds from coordinates
    x_rng <- range(coordinates[[1]], na.rm = TRUE)
    y_rng <- range(coordinates[[2]], na.rm = TRUE)

    if (file.exists(img_path)) {
      image_bounds <- list(
        xmin = x_rng[1],
        xmax = x_rng[2],
        ymin = y_rng[1],
        ymax = y_rng[2]
      )

      # Encode image
      ext <- tolower(tools::file_ext(img_path))
      mime_type <- switch(ext,
        "jpg" = "image/jpeg",
        "jpeg" = "image/jpeg",
        "png" = "image/png",
        "image/jpeg"
      )

      try({
        if (requireNamespace("base64enc", quietly = TRUE)) {
          encoded <- base64enc::base64encode(img_path)
          background_image_data <- paste0("data:", mime_type, ";base64,", encoded)
        }
      })
    }
  }

  ## follow this when the coloring variable is numeric
  if ( is.numeric(color_input) ) {
    ## put together meta data
    output_meta <- list(
      color_type     = 'continuous',
      traces         = plot_parameters[['color_variable']],
      color_variable = plot_parameters[['color_variable']],
      background_image = background_image_data,
      image_bounds     = image_bounds,
      background_flip_x = plot_parameters[['background_flip_x']],
      background_flip_y = plot_parameters[['background_flip_y']],
      background_scale_x = plot_parameters[['background_scale_x']],
      background_scale_y = plot_parameters[['background_scale_y']],
      background_opacity = plot_parameters[['background_opacity']]
    )
    ## put together data
    output_data <- list(
      x             = coordinates[[1]],
      y             = coordinates[[2]],
      color         = color_input,
      point_size    = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line    = list(),
      x_range       = plot_parameters[["x_range"]],
      y_range       = plot_parameters[["y_range"]],
      reset_axes    = reset_axes
    )

    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    ## put together hover info
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = 'empty'
    )
    if ( plot_parameters[["hover_info"]] ) {
      output_hover[['text']] <- unname(hover_info)
    }
    ## send request to update projection to JavaScript functions (2D / 3D)
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      shinyjs::js$updatePlot2DContinuousSpatial(
        output_meta,
        output_data,
        output_hover,
        list(),
        container_info
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      output_data[['z']] <- coordinates[[3]]
      shinyjs::js$updatePlot3DContinuousSpatial(
        output_meta,
        output_data,
        output_hover,
        list(),
        container_info
      )
    }
  ## follow this procedure when coloring variable is not numeric
  } else {
    ## put together meta data
    output_meta <- list(
      color_type = 'categorical',
      traces = list(),
      color_variable     = plot_parameters[['color_variable']],
      background_image   = background_image_data,
      image_bounds       = image_bounds,
      background_flip_x  = plot_parameters[['background_flip_x']],
      background_flip_y  = plot_parameters[['background_flip_y']],
      background_scale_x = plot_parameters[['background_scale_x']],
      background_scale_y = plot_parameters[['background_scale_y']],
      background_opacity = plot_parameters[['background_opacity']]
    )
    ## put together data
    output_data <- list(
      x = list(),
      y = list(),
      z = list(),
      color = list(),
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = list(),
      x_range = plot_parameters[["x_range"]],
      y_range = plot_parameters[["y_range"]],
      reset_axes = reset_axes
    )
    if ( plot_parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(
        color = "rgb(196,196,196)",
        width = 1
      )
    }
    ## put together hover info
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
      text = ifelse(plot_parameters[["hover_info"]], list(), 'test')
    )
    ## prepare trace for each group of the catergorical coloring variable and
    ## send request to update projection to JavaScript function (2D/3D)
    if ( plot_parameters[['n_dimensions']] == 2 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- coordinates[[1]][cells_to_extract]
        output_data[['y']][[i]] <- coordinates[[2]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            metadata[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(coordinates, metadata, 2, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x     = group_centers_df[['x_median']],
        y     = group_centers_df[['y_median']]
      )
      shinyjs::js$updatePlot2DCategoricalSpatial(
        output_meta,
        output_data,
        output_hover,
        output_group_centers,
        container_info
      )
    } else if ( plot_parameters[['n_dimensions']] == 3 ) {
      i <- 1
      for ( j in names(color_assignments) ) {
        output_meta[['traces']][[i]] <- j
        cells_to_extract <- which(color_input==j)
        output_data[['x']][[i]] <- coordinates[[1]][cells_to_extract]
        output_data[['y']][[i]] <- coordinates[[2]][cells_to_extract]
        output_data[['z']][[i]] <- coordinates[[3]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(names(color_assignments)==j)])
        if ( plot_parameters[["hover_info"]] ) {
          hover_info_matched <- match(
            metadata[['cell_barcode']][cells_to_extract],
            names(hover_info)
          )
          output_hover[['text']][[i]] <- unname(hover_info[hover_info_matched])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(coordinates, metadata, 3, plot_parameters[['color_variable']])
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']],
        z = group_centers_df[['z_median']]
      )
      shinyjs::js$updatePlot3DCategoricalSpatial(
        output_meta,
        output_data,
        output_hover,
        output_group_centers,
        container_info
      )
    }
  }
}
