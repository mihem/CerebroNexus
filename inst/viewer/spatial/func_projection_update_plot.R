##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
## Defense in depth at the file-read boundary: require both an exact allowlist
## match and canonical containment, including after resolving symbolic links.
authorized_spatial_image_path <- function(
  background_image,
  allowlist,
  cerebro_root
) {
  if (
    !is.character(background_image) ||
      length(background_image) != 1L ||
      is.na(background_image) ||
      !is.character(allowlist) ||
      !(background_image %in% allowlist) ||
      !is.character(cerebro_root) ||
      length(cerebro_root) != 1L ||
      is.na(cerebro_root)
  ) {
    return(NULL)
  }

  canonicalize <- function(path) {
    tryCatch(
      suppressWarnings(
        normalizePath(path, winslash = "/", mustWork = TRUE)
      ),
      error = function(error) NULL
    )
  }
  image_path <- canonicalize(file.path(cerebro_root, background_image))
  trusted_roots <- lapply(
    c("spatial-assets", "extdata"),
    function(root) canonicalize(file.path(cerebro_root, root))
  )
  trusted_roots <- Filter(Negate(is.null), trusted_roots)
  if (length(trusted_roots) == 0L || is.null(image_path)) {
    return(NULL)
  }

  if (.Platform$OS.type == "windows") {
    trusted_roots <- lapply(trusted_roots, tolower)
    image_path_comparison <- tolower(image_path)
  } else {
    image_path_comparison <- image_path
  }
  inside_trusted_root <- any(vapply(
    trusted_roots,
    function(root) {
      startsWith(
        image_path_comparison,
        paste0(sub("/+$", "", root), "/")
      )
    },
    logical(1)
  ))
  if (!inside_trusted_root) {
    return(NULL)
  }

  image_path
}

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
  ## a variable from the previous dataset (Xenium "cluster" vs MERFISH
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
  selection_keys <- if ("cell_barcode" %in% colnames(metadata)) {
    as.character(metadata[["cell_barcode"]])
  } else {
    rownames(metadata)
  }

  ## get container dimensions
  container_dimensions <- shinyjs::js$getContainerDimensions()
  container_info <- list(
    width = container_dimensions[['width']],
    height = container_dimensions[['height']]
  )

  ## prepare background image data and bounds if selected
  background_image_data <- NULL
  image_bounds <- list()

  ## The selected descriptor was resolved server-side from the exact current
  ## dataset / spatial / image leaf. Browser input values never contain a path or
  ## data URI and cannot select an image from another entry.
  selected_background <- plot_parameters[["background_descriptor"]]
  ## Compatibility for direct renderer callers that predate source-tagged
  ## selection. The live Viewer always supplies background_descriptor.
  if (
    is.null(selected_background) &&
      is.character(plot_parameters[["background_image"]]) &&
      length(plot_parameters[["background_image"]]) == 1L &&
      plot_parameters[["background_image"]] %in%
        plot_parameters[["background_image_allowlist"]]
  ) {
    selected_background <- list(
      source = "external",
      label = basename(plot_parameters[["background_image"]]),
      path = plot_parameters[["background_image"]],
      bounds = NULL
    )
  }
  if (
    !is.null(selected_background) &&
      identical(selected_background$source, "embedded")
  ) {
    background_image_data <- selected_background$image
    eb <- selected_background$bounds
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
    !is.null(selected_background) &&
      identical(selected_background$source, "external")
  ) {
    img_path <- authorized_spatial_image_path(
      selected_background$path,
      plot_parameters[["background_image_allowlist"]],
      Cerebro.options[["cerebro_root"]]
    )
    if (is.null(img_path)) {
      message("[spatial] rejected unauthorized background image")
    } else {
      message(
        "[spatial] background_image = ",
        selected_background$label
      )
      message("[spatial] resolved path  = ", img_path)
      message("[spatial] file.exists    = ", file.exists(img_path))
      message("[spatial] cerebro_root   = ", Cerebro.options[["cerebro_root"]])

      # Calculate bounds from coordinates
      x_rng <- range(coordinates[[1]], na.rm = TRUE)
      y_rng <- range(coordinates[[2]], na.rm = TRUE)
      ext <- tolower(tools::file_ext(img_path))

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

      explicit_bounds <- selected_background$bounds
      if (is.null(explicit_bounds)) {
        explicit_bounds <- list(
          xmin = x_rng[1],
          xmax = x_rng[2],
          ymin = y_rng[1],
          ymax = y_rng[2]
        )
      }
      image_bounds <- list(
        xmin = explicit_bounds[["xmin"]],
        xmax = explicit_bounds[["xmax"]],
        ymin = explicit_bounds[["ymin"]],
        ymax = explicit_bounds[["ymax"]],
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
    !is.null(selected_background) &&
    identical(selected_background$source, "embedded") &&
    length(image_bounds) > 0
  ## Images render in their native orientation by default. If a dataset needs a
  ## vertical/horizontal flip to align with the points, the user sets it from the
  ## tab's "Flip" checkboxes; both embedded and external images honour the same
  ## `background_flip_y` / `background_flip_x`.
  background_flip_y <- plot_parameters[["background_flip_y"]]

  ## Co-expression: colour each cell by blending up to three genes' expression
  ## onto RGB channels. Reuses the continuous render path, but the per-cell
  ## colour is a pre-computed rgb() string (no colorscale). The channel columns
  ## were populated in obj_projection_data_to_plot.R.
  is_coexpr <- identical(plot_parameters[["plot_type"]], "Co-expression (RGB)")
  if (is_coexpr) {
    rgb_colors <- blend_genes_to_rgb(
      r = if ("coexpr_r" %in% colnames(metadata)) {
        metadata[["coexpr_r"]]
      } else {
        NULL
      },
      g = if ("coexpr_g" %in% colnames(metadata)) {
        metadata[["coexpr_g"]]
      } else {
        NULL
      },
      b = if ("coexpr_b" %in% colnames(metadata)) {
        metadata[["coexpr_b"]]
      } else {
        NULL
      }
    )
    ## One legend entry per populated channel, kept as parallel label/colour
    ## vectors so the JS renders a coloured swatch per channel (red/green/blue)
    ## instead of a single grey blob. An unused channel is dropped from both.
    coexpr_labels <- c(
      if (nzchar(plot_parameters[["coexpr_r"]] %||% "")) {
        paste0("R: ", plot_parameters[["coexpr_r"]])
      },
      if (nzchar(plot_parameters[["coexpr_g"]] %||% "")) {
        paste0("G: ", plot_parameters[["coexpr_g"]])
      },
      if (nzchar(plot_parameters[["coexpr_b"]] %||% "")) {
        paste0("B: ", plot_parameters[["coexpr_b"]])
      }
    )
    coexpr_colors <- c(
      if (nzchar(plot_parameters[["coexpr_r"]] %||% "")) "rgb(255,0,0)",
      if (nzchar(plot_parameters[["coexpr_g"]] %||% "")) "rgb(0,255,0)",
      if (nzchar(plot_parameters[["coexpr_b"]] %||% "")) "rgb(0,0,255)"
    )
    output_meta <- list(
      is_spatial = TRUE,
      color_type = "coexpression",
      traces = as.list(coexpr_labels),
      coexpr_colors = as.list(coexpr_colors),
      color_variable = paste(coexpr_labels, collapse = "  "),
      background_image = background_image_data,
      background_identity = plot_parameters[["background_identity"]],
      is_embedded = using_embedded,
      image_bounds = image_bounds,
      background_flip_x = plot_parameters[["background_flip_x"]],
      background_flip_y = background_flip_y,
      background_scale_x = plot_parameters[["background_scale_x"]],
      background_scale_y = plot_parameters[["background_scale_y"]],
      background_offset_x = plot_parameters[["background_offset_x"]],
      background_offset_y = plot_parameters[["background_offset_y"]],
      background_rotation = plot_parameters[["background_rotation"]],
      background_opacity = plot_parameters[["background_opacity"]]
    )
    output_data <- list(
      x = coordinates[[1]],
      y = coordinates[[2]],
      selection_key = selection_keys,
      color = rgb_colors,
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = list(),
      x_range = x_range_out,
      y_range = y_range_out,
      reset_axes = reset_axes
    )
    if (plot_parameters[["draw_border"]]) {
      output_data[["point_line"]] <- list(color = "rgb(196,196,196)", width = 1)
    }
    output_hover <- list(
      hoverinfo = ifelse(plot_parameters[["hover_info"]], "text", "skip"),
      text = "empty"
    )
    if (plot_parameters[["hover_info"]]) {
      output_hover[["text"]] <- unname(hover_info)
    }
    shinyjs::js$updatePlot2DContinuousSpatial(
      output_meta,
      output_data,
      output_hover,
      list(),
      container_info
    )
    return(invisible(NULL))
  }

  ## follow this when the coloring variable is numeric
  if (is.numeric(color_input)) {
    ## put together meta data
    output_meta <- list(
      is_spatial = TRUE,
      color_type = 'continuous',
      traces = plot_parameters[['color_variable']],
      color_variable = plot_parameters[['color_variable']],
      background_image = background_image_data,
      background_identity = plot_parameters[['background_identity']],
      is_embedded = using_embedded,
      image_bounds = image_bounds,
      background_flip_x = plot_parameters[['background_flip_x']],
      background_flip_y = background_flip_y,
      background_scale_x = plot_parameters[['background_scale_x']],
      background_scale_y = plot_parameters[['background_scale_y']],
      background_offset_x = plot_parameters[['background_offset_x']],
      background_offset_y = plot_parameters[['background_offset_y']],
      background_rotation = plot_parameters[['background_rotation']],
      background_opacity = plot_parameters[['background_opacity']]
    )
    ## put together data
    output_data <- list(
      x = coordinates[[1]],
      y = coordinates[[2]],
      selection_key = selection_keys,
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
      is_spatial = TRUE,
      color_type = 'categorical',
      traces = list(),
      color_variable = plot_parameters[['color_variable']],
      background_image = background_image_data,
      background_identity = plot_parameters[['background_identity']],
      is_embedded = using_embedded,
      image_bounds = image_bounds,
      background_flip_x = plot_parameters[['background_flip_x']],
      background_flip_y = background_flip_y,
      background_scale_x = plot_parameters[['background_scale_x']],
      background_scale_y = plot_parameters[['background_scale_y']],
      background_offset_x = plot_parameters[['background_offset_x']],
      background_offset_y = plot_parameters[['background_offset_y']],
      background_rotation = plot_parameters[['background_rotation']],
      background_opacity = plot_parameters[['background_opacity']]
    )
    ## put together data
    output_data <- list(
      x = list(),
      y = list(),
      z = list(),
      selection_key = list(),
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
        output_data[['selection_key']][[i]] <-
          selection_keys[cells_to_extract]
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
      ## Optional per-group region outline (convex hull), matched to each group's
      ## colour. Computed only when the user turns it on, since hulls overlap
      ## heavily for spatially intermixed groups.
      output_hulls <- list(
        group = list(),
        x = list(),
        y = list(),
        color = list()
      )
      if (isTRUE(plot_parameters[['show_region_outlines']])) {
        hulls <- compute_group_hulls(
          coordinates[[1]],
          coordinates[[2]],
          as.character(color_input)
        )
        h <- 1
        for (g in names(hulls)) {
          col_idx <- which(names(color_assignments) == g)
          if (length(col_idx) == 0) {
            next
          }
          output_hulls[['group']][[h]] <- g
          output_hulls[['x']][[h]] <- hulls[[g]][['x']]
          output_hulls[['y']][[h]] <- hulls[[g]][['y']]
          output_hulls[['color']][[h]] <- unname(color_assignments[col_idx])
          h <- h + 1
        }
      }
      shinyjs::js$updatePlot2DCategoricalSpatial(
        output_meta,
        output_data,
        output_hover,
        output_group_centers,
        container_info,
        output_hulls
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
        output_data[['selection_key']][[i]] <-
          selection_keys[cells_to_extract]
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
