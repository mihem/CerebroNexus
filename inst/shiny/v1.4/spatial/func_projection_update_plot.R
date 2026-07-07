##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
spatial_projection_update_plot <- function(input) {
  ## assign input data to new variables
  metadata <- input[['cells_df']]
  coordinates <- input[['coordinates']]
  reset_axes <- input[['reset_axes']]
  plot_parameters <- input[['plot_parameters']]
  color_assignments <- input[['color_assignments']]
  hover_info <- input[['hover_info']]

  ## Guard against a colour variable that does not exist in THIS dataset's
  ## metadata. When the loaded .crb is switched, plot_parameters (debounced) and
  ## metadata can be momentarily inconsistent — the colour column may still name
  ## a variable from the previous dataset (Xenium "cluster" vs Slide-tags
  ## "cell_type"). A missing column makes the downstream dplyr::group_by() error
  ## and freezes the plot on the old data. Fall back to the first metadata column
  ## so the render always succeeds and self-corrects on the next tick.
  color_variable <- plot_parameters[['color_variable']]
  if (
    is.null(color_variable) ||
      !(color_variable %in% colnames(metadata))
  ) {
    color_variable <- colnames(metadata)[1]
    plot_parameters[['color_variable']] <- color_variable
  }
  color_input <- metadata[[color_variable]]

  ## get container dimensions
  container_dimensions <- shinyjs::js$getContainerDimensions()
  container_info <- list(
    width = container_dimensions[['width']],
    height = container_dimensions[['height']]
  )

  ## prepare background image data and bounds if selected
  background_image_data <- NULL
  image_bounds <- list()

  ## Case 1: the real histology image embedded in the .crb. Its base64 data URI
  ## and coordinate-space bounds travel with the data, so it renders directly and
  ## aligns automatically — no file lookup, no manual flip/scale.
  if (
    !is.null(plot_parameters[['background_image']]) &&
      identical(plot_parameters[['background_image']], "__embedded__") &&
      !is.null(plot_parameters[['embedded_image']])
  ) {
    background_image_data <- plot_parameters[['embedded_image']]
    eb <- plot_parameters[['embedded_bounds']]
    if (is.null(eb)) {
      # fall back to the coordinate range if bounds were not stored
      x_rng <- range(coordinates[[1]], na.rm = TRUE)
      y_rng <- range(coordinates[[2]], na.rm = TRUE)
      eb <- list(
        xmin = x_rng[1],
        xmax = x_rng[2],
        ymin = y_rng[1],
        ymax = y_rng[2]
      )
    }
    image_bounds <- list(
      xmin = eb[["xmin"]],
      xmax = eb[["xmax"]],
      ymin = eb[["ymin"]],
      ymax = eb[["ymax"]],
      img_width = 0,
      img_height = 0
    )
    message("[spatial] using embedded histology image from .crb")
  } else if (
    !is.null(plot_parameters[['background_image']]) &&
      plot_parameters[['background_image']] != "No Background"
  ) {
    img_path <- file.path(
      Cerebro.options[["cerebro_root"]],
      plot_parameters[['background_image']]
    )

    message(
      "[spatial] background_image = ",
      plot_parameters[['background_image']]
    )
    message("[spatial] resolved path  = ", img_path)
    message("[spatial] file.exists    = ", file.exists(img_path))
    message("[spatial] cerebro_root   = ", Cerebro.options[["cerebro_root"]])

    # Calculate bounds from coordinates
    x_rng <- range(coordinates[[1]], na.rm = TRUE)
    y_rng <- range(coordinates[[2]], na.rm = TRUE)
    ext <- tolower(tools::file_ext(img_path))

    if (file.exists(img_path)) {
      # Read JPEG dimensions from header without decompressing
      img_dims <- NULL
      tryCatch(
        {
          if (ext %in% c("jpg", "jpeg")) {
            con <- file(img_path, "rb")
            on.exit(close(con))
            buf <- readBin(con, "raw", 131072) # 128KB for header
            i <- 3 # skip SOI (0xFF 0xD8)
            n <- length(buf)
            # Walk through JPEG markers, skipping variable-length segments
            # until we reach the Start of Frame (0xFF 0xC0-0xC3)
            while (i < n - 8) {
              if (buf[i] != as.raw(0xFF)) {
                i <- i + 1
                next
              }
              marker <- buf[i + 1]
              # SOF markers: image dimensions follow at offset 5-8
              if (marker >= as.raw(0xC0) && marker <= as.raw(0xC3)) {
                h <- readBin(
                  buf[(i + 5):(i + 6)],
                  "integer",
                  size = 2,
                  signed = FALSE,
                  endian = "big"
                )
                w <- readBin(
                  buf[(i + 7):(i + 8)],
                  "integer",
                  size = 2,
                  signed = FALSE,
                  endian = "big"
                )
                img_dims <- c(h, w)
                break
              }
              # Variable-length marker segments (most markers):
              # read 2-byte length, skip that many bytes
              if (
                marker != as.raw(0xD8) &&
                  marker != as.raw(0xD9) &&
                  !(marker >= as.raw(0xD0) && marker <= as.raw(0xD7))
              ) {
                seg_len <- readBin(
                  buf[(i + 2):(i + 3)],
                  "integer",
                  size = 2,
                  signed = FALSE,
                  endian = "big"
                )
                i <- i + 2 + seg_len
              } else {
                i <- i + 2
              }
            }
            if (!is.null(img_dims)) {
              message(
                "[spatial] image native resolution: ",
                img_dims[2],
                " x ",
                img_dims[1],
                " px"
              )
            }
          }
        },
        error = function(e) {
          warning("[spatial] Could not read image dimensions: ", e$message)
        }
      )

      image_bounds <- list(
        xmin = x_rng[1],
        xmax = x_rng[2],
        ymin = y_rng[1],
        ymax = y_rng[2],
        img_width = if (!is.null(img_dims)) img_dims[2] else 0,
        img_height = if (!is.null(img_dims)) img_dims[1] else 0
      )

      # Encode image. SVG carries no raster header, so img_dims stays NULL and
      # the JS side stretches it to the plot bounds via the CSS-background path.
      mime_type <- switch(
        ext,
        "jpg" = "image/jpeg",
        "jpeg" = "image/jpeg",
        "png" = "image/png",
        "svg" = "image/svg+xml",
        "image/jpeg"
      )

      tryCatch(
        {
          if (requireNamespace("base64enc", quietly = TRUE)) {
            encoded <- base64enc::base64encode(img_path)
            background_image_data <- paste0(
              "data:",
              mime_type,
              ";base64,",
              encoded
            )
          } else {
            warning(
              "[spatial] base64enc package not available, cannot encode background image"
            )
          }
        },
        error = function(e) {
          warning("[spatial] Failed to encode background image: ", e$message)
        }
      )
    } else {
      warning("[spatial] Background image file not found: ", img_path)
    }
  }

  ## Axis ranges are a property of the CELLS only — never the background image.
  ## The scatter plot's coordinate system is fixed by the point bounding box (or
  ## the user's manual range); the background is a passenger that the JS maps into
  ## that fixed system via its stored `image_bounds` (data-space extent → pixels).
  ## So we do NOT widen the axes to the image extent here: doing that squashed the
  ## points (the image is larger than the spot bbox, and — combined with the old
  ## scaleanchor lock — it blew the y-axis out to negative values). Selecting a
  ## background must not change the axes at all.
  x_range_out <- plot_parameters[["x_range"]]
  y_range_out <- plot_parameters[["y_range"]]
  using_embedded <-
    identical(plot_parameters[["background_image"]], "__embedded__") &&
    !is.null(plot_parameters[["embedded_image"]]) &&
    length(image_bounds) > 0
  ## Whether an embedded image must be flipped vertically to sit the right way up
  ## depends on how this dataset's point y relates to its image rows, which
  ## differs per platform. It travels with the .crb as `embedded_flip_y` (set at
  ## build time, ground-truth verified per dataset). External spatial_images keep
  ## their own `background_flip_y`.
  background_flip_y <- plot_parameters[["background_flip_y"]]
  if (using_embedded) {
    background_flip_y <- isTRUE(plot_parameters[["embedded_flip_y"]])
  }

  ## follow this when the coloring variable is numeric
  if (is.numeric(color_input)) {
    ## put together meta data
    output_meta <- list(
      color_type = 'continuous',
      traces = plot_parameters[['color_variable']],
      color_variable = plot_parameters[['color_variable']],
      background_image = background_image_data,
      is_embedded = using_embedded,
      image_bounds = image_bounds,
      background_flip_x = plot_parameters[['background_flip_x']],
      background_flip_y = background_flip_y,
      background_scale_x = plot_parameters[['background_scale_x']],
      background_scale_y = plot_parameters[['background_scale_y']],
      background_opacity = plot_parameters[['background_opacity']]
    )
    ## put together data
    output_data <- list(
      x = coordinates[[1]],
      y = coordinates[[2]],
      color = color_input,
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = list(),
      x_range = x_range_out,
      y_range = y_range_out,
      reset_axes = reset_axes
    )

    if (plot_parameters[["draw_border"]]) {
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
    if (plot_parameters[["hover_info"]]) {
      output_hover[['text']] <- unname(hover_info)
    }
    ## send request to update projection to JavaScript functions (2D / 3D)
    if (plot_parameters[['n_dimensions']] == 2) {
      shinyjs::js$updatePlot2DContinuousSpatial(
        output_meta,
        output_data,
        output_hover,
        list(),
        container_info
      )
    } else if (plot_parameters[['n_dimensions']] == 3) {
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
      color_variable = plot_parameters[['color_variable']],
      background_image = background_image_data,
      is_embedded = using_embedded,
      image_bounds = image_bounds,
      background_flip_x = plot_parameters[['background_flip_x']],
      background_flip_y = background_flip_y,
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
      x_range = x_range_out,
      y_range = y_range_out,
      reset_axes = reset_axes
    )
    if (plot_parameters[["draw_border"]]) {
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
    if (plot_parameters[['n_dimensions']] == 2) {
      # Optimization: Group cells by color category to avoid repeated full scans
      cells_by_group <- split(seq_along(color_input), color_input)

      i <- 1
      for (j in names(color_assignments)) {
        output_meta[['traces']][[i]] <- j

        # Get indices for this group (NULL if not present)
        cells_to_extract <- cells_by_group[[j]]

        output_data[['x']][[i]] <- coordinates[[1]][cells_to_extract]
        output_data[['y']][[i]] <- coordinates[[2]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(
          names(color_assignments) == j
        )])

        if (plot_parameters[["hover_info"]]) {
          # Optimization: Direct indexing instead of match()
          # hover_info is already aligned with metadata/color_input
          output_hover[['text']][[i]] <- unname(hover_info[cells_to_extract])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(
        coordinates,
        metadata,
        2,
        plot_parameters[['color_variable']]
      )
      output_group_centers <- list(
        group = group_centers_df[['group']],
        x = group_centers_df[['x_median']],
        y = group_centers_df[['y_median']]
      )
      shinyjs::js$updatePlot2DCategoricalSpatial(
        output_meta,
        output_data,
        output_hover,
        output_group_centers,
        container_info
      )
    } else if (plot_parameters[['n_dimensions']] == 3) {
      # Optimization: Group cells by color category
      cells_by_group <- split(seq_along(color_input), color_input)

      i <- 1
      for (j in names(color_assignments)) {
        output_meta[['traces']][[i]] <- j

        # Get indices for this group
        cells_to_extract <- cells_by_group[[j]]

        output_data[['x']][[i]] <- coordinates[[1]][cells_to_extract]
        output_data[['y']][[i]] <- coordinates[[2]][cells_to_extract]
        output_data[['z']][[i]] <- coordinates[[3]][cells_to_extract]
        output_data[['color']][[i]] <- unname(color_assignments[which(
          names(color_assignments) == j
        )])

        if (plot_parameters[["hover_info"]]) {
          # Optimization: Direct indexing
          output_hover[['text']][[i]] <- unname(hover_info[cells_to_extract])
        }
        i <- i + 1
      }
      group_centers_df <- centerOfGroups(
        coordinates,
        metadata,
        3,
        plot_parameters[['color_variable']]
      )
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
