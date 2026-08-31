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

  color_variable <- plot_parameters[['color_variable']]
  color_input <- metadata[[color_variable]]
  selection_keys <- if ("cell_barcode" %in% colnames(metadata)) {
    as.character(metadata[["cell_barcode"]])
  } else {
    rownames(metadata)
  }

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
      ymax = eb[["ymax"]]
    )
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
      # Calculate bounds from coordinates
      x_rng <- range(coordinates[[1]], na.rm = TRUE)
      y_rng <- range(coordinates[[2]], na.rm = TRUE)
      ext <- tolower(tools::file_ext(img_path))

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
        ymax = explicit_bounds[["ymax"]]
      )

      # Encode the image for the shared Canvas renderer.
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
  ## The scatter plot's coordinate system is fixed by the point bounding box;
  ## the background is a passenger that the JS maps into that fixed system via
  ## its stored `image_bounds` (data-space extent → pixels).
  ## So we do NOT widen the axes to the image extent here: doing that squashed the
  ## points (the image is larger than the spot bbox, and — combined with the old
  ## scaleanchor lock — it blew the y-axis out to negative values). Selecting a
  ## background must not change the axes at all.
  x_range_out <- plot_parameters[["x_range"]]
  y_range_out <- plot_parameters[["y_range"]]
  ## Images render in their native orientation by default. If a dataset needs a
  ## vertical/horizontal flip to align with the points, the user sets it from the
  ## tab's "Flip" checkboxes; both embedded and external images honour the same
  ## `background_flip_y` / `background_flip_x`.
  background_flip_y <- plot_parameters[["background_flip_y"]]
  background_meta <- list(
    is_spatial = TRUE,
    background_image = background_image_data,
    background_identity = plot_parameters[["background_identity"]],
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
  point_line <- if (plot_parameters[["draw_border"]]) {
    list(color = "rgb(196,196,196)", width = 1)
  } else {
    list()
  }

  ## Co-expression: colour each cell by blending up to three genes' expression
  ## onto RGB channels. The channel columns were populated in
  ## obj_projection_data_to_plot.R.
  is_coexpr <- identical(plot_parameters[["plot_type"]], "Co-expression (RGB)")
  if (is_coexpr) {
    render_color <- blend_genes_to_rgb(
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
    output_meta <- c(
      background_meta,
      list(
        color_type = "coexpression",
        traces = as.list(coexpr_labels),
        coexpr_colors = as.list(coexpr_colors),
        color_variable = paste(coexpr_labels, collapse = "  "),
        appearance = list(
          group_labels = FALSE,
          draw_border = isTRUE(plot_parameters[["draw_border"]]),
          keep_square = isTRUE(plot_parameters[["keep_square"]])
        )
      )
    )
    output_data <- list(
      x = coordinates[[1]],
      y = coordinates[[2]],
      selection_key = selection_keys,
      color = render_color,
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = point_line,
      x_range = x_range_out,
      y_range = y_range_out,
      reset_axes = reset_axes
    )
    output_hover <- list(
      hoverinfo = if (plot_parameters[["hover_info"]]) "text" else "skip",
      text = if (plot_parameters[["hover_info"]]) {
        unname(hover_info)
      } else {
        "empty"
      }
    )
    cerebroCellViewRender(
      "spatial_projection",
      output_meta,
      output_data,
      output_hover
    )
    return(invisible(NULL))
  }

  n_dimensions <- plot_parameters[["n_dimensions"]]
  payload <- cerebroCellViewScatterPayload(
    coordinates = coordinates,
    color = color_input,
    color_variable = plot_parameters[["color_variable"]],
    selection_keys = selection_keys,
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    group_labels = plot_parameters[["group_labels"]],
    keep_square = plot_parameters[["keep_square"]],
    point_line = point_line,
    x_range = x_range_out,
    y_range = y_range_out,
    reset_axes = reset_axes,
    n_dimensions = n_dimensions,
    color_assignments = color_assignments,
    hover_info = hover_info,
    hover = plot_parameters[["hover_info"]]
  )
  payload[["meta"]] <- c(background_meta, payload[["meta"]])

  output_hulls <- list()
  if (
    !is.numeric(color_input) &&
      n_dimensions == 2 &&
      isTRUE(plot_parameters[["show_region_outlines"]])
  ) {
    hulls <- compute_group_hulls(
      coordinates[[1]],
      coordinates[[2]],
      as.character(color_input)
    )
    present <- intersect(names(hulls), names(color_assignments))
    output_hulls <- list(
      x = unname(lapply(hulls[present], `[[`, "x")),
      y = unname(lapply(hulls[present], `[[`, "y")),
      color = unname(as.list(color_assignments[present]))
    )
  }
  cerebroCellViewRender(
    "spatial_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]],
    extra = list(group_hulls = output_hulls)
  )
}
