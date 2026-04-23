##----------------------------------------------------------------------------##
## Helper function to prepare background image data
##----------------------------------------------------------------------------##
prepare_background_image <- function(parameters, coordinates) {
  background_image_data <- NULL
  image_bounds <- list()

  if (!is.null(parameters[['background_image']]) &&
      parameters[['background_image']] != "No Background") {

    img_path <- parameters[['background_image']]

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

  return(list(
    background_image_data = background_image_data,
    image_bounds = image_bounds
  ))
}

##----------------------------------------------------------------------------##
## Helper function to prepare projection data for JS
##----------------------------------------------------------------------------##
prepare_projection_data <- function(data, coordinates, parameters, color_assignments, hover_info,
                                    background_image_data = NULL, image_bounds = list()) {

  color_input <- data[[ parameters[['color_variable']] ]]
  reset_axes <- TRUE # Simplified for now, or pass as arg

  ## Numeric coloring
  if ( is.numeric(color_input) ) {
    output_meta <- list(
      color_type         = 'continuous',
      traces             = parameters[['color_variable']],
      color_variable     = parameters[['color_variable']],
      background_image   = background_image_data,
      image_bounds       = image_bounds,
      background_flip_x  = parameters[['background_flip_x']],
      background_flip_y  = parameters[['background_flip_y']],
      background_scale_x = parameters[['background_scale_x']],
      background_scale_y = parameters[['background_scale_y']],
      background_opacity = parameters[['background_opacity']]
    )

    output_data <- list(
      x = coordinates[[1]],
      y = coordinates[[2]],
      color = color_input,
      point_size = parameters[["point_size"]],
      point_opacity = parameters[["point_opacity"]],
      point_line = list(),
      x_range = parameters[["x_range"]],
      y_range = parameters[["y_range"]],
      reset_axes = reset_axes,
      n_dimensions = parameters[['n_dimensions']]
    )

    if ( parameters[['n_dimensions']] == 3 ) {
      output_data[['z']] <- coordinates[[3]]
    }

    if ( parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(color = "rgb(196,196,196)", width = 1)
    }

    output_hover <- list(
      hoverinfo = ifelse(parameters[["hover_info"]], 'text', 'skip'),
      text = if(parameters[["hover_info"]]) unname(hover_info) else 'empty'
    )

  } else {
    ## Categorical coloring
    output_meta <- list(
      color_type         = 'categorical',
      traces             = list(),
      color_variable     = parameters[['color_variable']],
      background_image   = background_image_data,
      image_bounds       = image_bounds,
      background_flip_x  = parameters[['background_flip_x']],
      background_flip_y  = parameters[['background_flip_y']],
      background_scale_x = parameters[['background_scale_x']],
      background_scale_y = parameters[['background_scale_y']],
      background_opacity = parameters[['background_opacity']]
    )

    output_data <- list(
      x = list(),
      y = list(),
      z = list(),
      color = list(),
      point_size = parameters[["point_size"]],
      point_opacity = parameters[["point_opacity"]],
      point_line = list(),
      x_range = parameters[["x_range"]],
      y_range = parameters[["y_range"]],
      reset_axes = reset_axes,
      n_dimensions = parameters[['n_dimensions']]
    )

    if ( parameters[["draw_border"]] ) {
      output_data[['point_line']] <- list(color = "rgb(196,196,196)", width = 1)
    }

    output_hover <- list(
      hoverinfo = ifelse(parameters[["hover_info"]], 'text', 'skip'),
      text = list()
    )

    ## Split by group
    indices_by_group <- split(seq_along(color_input), as.character(color_input))

    # Ensure order matches color assignments
    for ( group_name in names(color_assignments) ) {
      if ( is.null(indices_by_group[[group_name]]) ) next

      idx <- indices_by_group[[group_name]]

      output_meta[['traces']] <- c(output_meta[['traces']], group_name)
      output_data[['x']] <- c(output_data[['x']], list(coordinates[[1]][idx]))
      output_data[['y']] <- c(output_data[['y']], list(coordinates[[2]][idx]))

      if ( parameters[['n_dimensions']] == 3 ) {
        output_data[['z']] <- c(output_data[['z']], list(coordinates[[3]][idx]))
      }

      output_data[['color']] <- c(output_data[['color']], color_assignments[[group_name]])

      if ( parameters[["hover_info"]] ) {
        output_hover[['text']] <- c(output_hover[['text']], list(hover_info[idx]))
      }
    }

    # Labels
    group_centers <- list(x = list(), y = list(), group = list())

    # Check if group_labels is valid
    show_labels <- FALSE
    if (!is.null(parameters[["group_labels"]]) && parameters[["group_labels"]] == TRUE) {
      show_labels <- TRUE
    }

    if ( show_labels ) {
      for ( group_name in names(color_assignments) ) {
        if ( is.null(indices_by_group[[group_name]]) ) next
        idx <- indices_by_group[[group_name]]
        group_centers[['x']] <- c(group_centers[['x']], mean(coordinates[[1]][idx]))
        group_centers[['y']] <- c(group_centers[['y']], mean(coordinates[[2]][idx]))
        group_centers[['group']] <- c(group_centers[['group']], group_name)
      }
    }

    return(list(
      meta = output_meta,
      data = output_data,
      hover = output_hover,
      group_centers = group_centers
    ))
  }

  # Continuous case: return with empty group_centers
  return(list(
    meta = output_meta,
    data = output_data,
    hover = output_hover,
    group_centers = list(x = list(), y = list(), group = list())
  ))
}
