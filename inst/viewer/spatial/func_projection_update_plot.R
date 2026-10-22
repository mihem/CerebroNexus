##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
spatial_background_render_payload <- function(
  descriptor,
  allowlist,
  identity,
  preset,
  coordinates,
  cerebro_root
) {
  image_data <- NULL
  image_bounds <- list()
  if (!is.null(descriptor) && identical(descriptor$source, "embedded")) {
    image_data <- viewerEmbeddedImageUrl(
      descriptor$image,
      session = shiny::getDefaultReactiveDomain()
    )
    bounds <- descriptor$bounds
    if (is.null(bounds)) {
      x_range <- range(coordinates[[1]], na.rm = TRUE)
      y_range <- range(coordinates[[2]], na.rm = TRUE)
      bounds <- list(
        xmin = x_range[[1L]],
        xmax = x_range[[2L]],
        ymin = y_range[[1L]],
        ymax = y_range[[2L]]
      )
    }
    image_bounds <- as.list(bounds[c("xmin", "xmax", "ymin", "ymax")])
  } else if (!is.null(descriptor) && identical(descriptor$source, "external")) {
    image_path <- authorized_spatial_image_path(
      descriptor$path,
      allowlist,
      cerebro_root
    )
    if (is.null(image_path)) {
      message("[spatial] rejected unauthorized background image")
    } else {
      bounds <- descriptor$bounds
      if (is.null(bounds)) {
        x_range <- range(coordinates[[1]], na.rm = TRUE)
        y_range <- range(coordinates[[2]], na.rm = TRUE)
        bounds <- list(
          xmin = x_range[[1L]],
          xmax = x_range[[2L]],
          ymin = y_range[[1L]],
          ymax = y_range[[2L]]
        )
      }
      image_bounds <- as.list(bounds[c("xmin", "xmax", "ymin", "ymax")])
      image_data <- viewerPrivateImageUrl(
        image_path,
        session = shiny::getDefaultReactiveDomain()
      )
    }
  }
  list(
    background_image = image_data,
    background_identity = identity,
    image_bounds = image_bounds,
    background_flip_x = preset$flipX,
    background_flip_y = preset$flipY,
    background_scale_x = preset$scaleX,
    background_scale_y = preset$scaleY,
    background_offset_x = preset$offsetX,
    background_offset_y = preset$offsetY,
    background_rotation = preset$rotation,
    background_opacity = preset$opacity
  )
}

spatial_projection_update_plot <- function(input) {
  ## assign input data to new variables
  dataset_context <- input[["dataset_context"]]
  metadata <- input[['cells_df']]
  coordinates <- input[['coordinates']]
  reset_axes <- input[['reset_axes']]
  plot_parameters <- input[['plot_parameters']]
  color_assignments <- input[['color_assignments']]
  geometry_resource <- input[["geometry_resource"]]
  categorical_resource <- input[["categorical_resource"]]
  resource_first <- isTRUE(input[["resource_first"]])

  attach_geometry_resource <- function(data) {
    if (
      is.null(geometry_resource) ||
        !is.atomic(data[["x"]]) ||
        !is.atomic(data[["y"]]) ||
        length(data[["x"]]) != geometry_resource$cells ||
        length(data[["y"]]) != geometry_resource$cells
    ) {
      return(data)
    }
    data[["x"]] <- NULL
    data[["y"]] <- NULL
    data[["projection_resource"]] <- geometry_resource
    data
  }

  color_variable <- plot_parameters[['color_variable']]
  color_input <- if (resource_first) NULL else metadata[[color_variable]]
  selection_keys <- if (resource_first) {
    integer()
  } else {
    seq_len(nrow(metadata))
  }
  build_deferred_aux <- function(selection_rows) {
    cell_index <- if (resource_first) {
      input[["cell_indices"]]
    } else if ("cell_index" %in% colnames(metadata)) {
      metadata[["cell_index"]]
    } else {
      seq_len(nrow(metadata))
    }
    hover <- isTRUE(plot_parameters[["hover_info"]])
    first_frame <- viewerProjectionFirstFrameMetadata()
    columns <- viewerProjectionMetadataColumns(
      first_frame,
      color_variable = NULL,
      hover_info = hover,
      groups = getGroups()
    )
    hover_metadata <- viewerProjectionSubsetRows(
      first_frame,
      cell_index,
      columns
    )
    cell_barcodes <- viewerPackCellBarcodes(viewerPackCurrent(), cell_index)
    if (is.null(cell_barcodes)) {
      cell_barcodes <- getMetaData()[["cell_barcode"]][cell_index]
    }
    cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = cell_barcodes,
      hover_columns = if (hover) {
        cerebroProjectionHoverColumns(hover_metadata)
      } else {
        list()
      },
      hover = hover
    )
  }

  ## The selected descriptor was resolved server-side from the exact current
  ## dataset / spatial / image leaf. Browser input values never contain a path or
  ## data URI and cannot select an image from another entry.
  ## Without a descriptor no image is drawn: `background_image` is only the
  ## selector token and is never read as a path.
  selected_background <- plot_parameters[["background_descriptor"]]
  background_meta <- c(
    list(is_spatial = TRUE),
    spatial_background_render_payload(
      selected_background,
      plot_parameters[["background_image_allowlist"]],
      plot_parameters[["background_identity"]],
      list(
        flipX = plot_parameters[["background_flip_x"]],
        flipY = plot_parameters[["background_flip_y"]],
        scaleX = plot_parameters[["background_scale_x"]],
        scaleY = plot_parameters[["background_scale_y"]],
        offsetX = plot_parameters[["background_offset_x"]],
        offsetY = plot_parameters[["background_offset_y"]],
        rotation = plot_parameters[["background_rotation"]],
        opacity = plot_parameters[["background_opacity"]]
      ),
      coordinates,
      if (exists("Cerebro.options")) {
        Cerebro.options[["cerebro_root"]]
      } else {
        NULL
      }
    )
  )
  ## The Builder viewport describes the camera used while aligning an image,
  ## not the final Viewer camera. Keep the image bounds and transform for exact
  ## alignment, but fit the Viewer to its current cell/ROI scope so a viewport
  ## saved in a differently shaped Builder canvas cannot shrink the plot.
  x_range_out <- plot_parameters[["x_range"]]
  y_range_out <- plot_parameters[["y_range"]]
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
        space_label = plot_parameters[["projection"]],
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
      hoverinfo = "skip",
      text = list(),
      columns = list()
    )
    output_data <- attach_geometry_resource(output_data)
    cerebroCellViewRender(
      "spatial_projection",
      output_meta,
      output_data,
      output_hover,
      deferred_aux = function() {
        build_deferred_aux(output_data[["selection_key"]])
      },
      dataset_context = input[["dataset_context"]],
      feedback = input[["feedback"]]
    )
    return(invisible(NULL))
  }

  n_dimensions <- plot_parameters[["n_dimensions"]]
  payload <- cerebroCellViewScatterPayload(
    coordinates = if (resource_first) list() else coordinates,
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
    hover_columns = list(),
    hover = FALSE,
    space_label = plot_parameters[["projection"]],
    cell_count = input[["cell_count"]],
    coordinate_resource = if (resource_first) geometry_resource else NULL,
    categorical_resource = if (resource_first) {
      categorical_resource
    } else {
      NULL
    }
  )
  if (identical(plot_parameters[["plot_type"]], "ImageFeaturePlot")) {
    settings <- viewerExpressionColorSettings(
      if (exists("Cerebro.options")) Cerebro.options else list(),
      dataset_context$dataset_key
    )
    payload$data$colorscale <- expressionColorScale(settings$palette)
    payload$data$reversescale <- FALSE
  }
  payload[["meta"]] <- c(background_meta, payload[["meta"]])
  if (!resource_first) {
    payload[["data"]] <- attach_geometry_resource(payload[["data"]])
  }

  split_by <- plot_parameters[["split_by"]] %||% ""
  if (nzchar(split_by) && split_by %in% colnames(metadata)) {
    cell_index <- if ("cell_index" %in% colnames(metadata)) {
      metadata[["cell_index"]]
    } else {
      selection_keys
    }
    panel_selection_keys <- if ("cell_barcode" %in% colnames(metadata)) {
      metadata[["cell_barcode"]]
    } else {
      barcodes <- viewerPackCellBarcodes(viewerPackCurrent(), cell_index)
      if (is.null(barcodes)) {
        getMetaData()[["cell_barcode"]][cell_index]
      } else {
        barcodes
      }
    }
    req(
      length(panel_selection_keys) == length(selection_keys),
      !anyNA(panel_selection_keys)
    )
    panel_selection_keys <- as.character(panel_selection_keys)
    panel_labels <- as.character(metadata[[split_by]])
    panel_labels[is.na(panel_labels) | !nzchar(panel_labels)] <- "N/A"
    panel_order <- unique(as.character(
      plot_parameters[["roi_order"]] %||% character()
    ))
    panel_order <- panel_order[
      !is.na(panel_order) & nzchar(panel_order) & panel_order %in% panel_labels
    ]
    panel_order <- c(panel_order, setdiff(unique(panel_labels), panel_order))
    panel_indices <- split(
      seq_along(panel_labels),
      factor(panel_labels, levels = panel_order),
      drop = TRUE
    )
    panel_range <- function(values) {
      extent <- range(values[is.finite(values)], na.rm = TRUE)
      margin <- if (diff(extent) > 0) diff(extent) * 0.02 else 1
      c(extent[[1L]] - margin, extent[[2L]] + margin)
    }
    panel_x_range <- x_range_out %||% panel_range(coordinates[[1]])
    panel_y_range <- y_range_out %||% panel_range(coordinates[[2]])
    hover_info <- input[["hover_info"]] %||% rep("", length(selection_keys))
    hover_names <- names(hover_info)
    panel_hover <- if (
      !is.null(hover_names) && any(!is.na(hover_names) & nzchar(hover_names))
    ) {
      unname(hover_info[match(selection_keys, hover_names)])
    } else {
      unname(hover_info)
    }
    panel_background <- function(label, panel_coordinates) {
      configured <- plot_parameters[["roi_backgrounds"]][[label]]
      if (is.null(configured) || is.null(configured$descriptor)) {
        return(list())
      }
      rendered <- spatial_background_render_payload(
        configured$descriptor,
        configured$image_allowlist,
        configured$identity,
        configured$preset,
        panel_coordinates,
        if (exists("Cerebro.options")) {
          Cerebro.options[["cerebro_root"]]
        } else {
          NULL
        }
      )
      result <- list(
        background_image = rendered$background_image,
        image_bounds = rendered$image_bounds,
        image_identity = rendered$background_identity,
        image_label = configured$descriptor$label,
        image_preset = configured$preset
      )
      result
    }
    payload$data$panels <- lapply(seq_along(panel_indices), function(index) {
      cells <- panel_indices[[index]]
      label <- names(panel_indices)[[index]]
      point_appearance <- plot_parameters[["roi_point_appearance"]][[
        label
      ]] %||%
        list()
      panel_coordinates <- coordinates[cells, , drop = FALSE]
      extent <- plot_parameters[["roi_extents"]][[label]]
      if (
        identical(plot_parameters[["roi_mode"]], "separate") &&
          is.list(extent) &&
          length(extent$x) == 2L &&
          length(extent$y) == 2L &&
          all(is.finite(c(extent$x, extent$y)))
      ) {
        panel_coordinates <- data.frame(x = extent$x, y = extent$y)
      }
      utils::modifyList(
        list(
          id = paste0("split-", index),
          label = label,
          selection_key = panel_selection_keys[cells],
          x = as.numeric(coordinates[[1]][cells]),
          y = as.numeric(coordinates[[2]][cells]),
          hover = panel_hover[cells],
          spatial = TRUE,
          builder_point_size = point_appearance$point_size,
          builder_point_opacity = point_appearance$point_opacity,
          preserve_aspect = identical(
            plot_parameters[["roi_mode"]],
            "separate"
          ),
          x_range = if (identical(plot_parameters[["roi_mode"]], "separate")) {
            panel_range(panel_coordinates[[1L]])
          } else {
            panel_x_range
          },
          y_range = if (identical(plot_parameters[["roi_mode"]], "separate")) {
            panel_range(panel_coordinates[[2L]])
          } else {
            panel_y_range
          }
        ),
        panel_background(label, panel_coordinates)
      )
    })
    payload$data$selection_key <- selection_keys
    if (!is.numeric(color_input)) {
      panel_groups <- as.character(color_input)
      panel_groups[is.na(panel_groups)] <- "(missing)"
      trace_names <- unlist(payload$meta$traces, use.names = FALSE)
      payload$data$group <- panel_groups
      payload$meta$group_colors <- unname(color_assignments[trace_names])
    }
  }

  output_hulls <- list()
  if (
    !is.numeric(color_input) &&
      n_dimensions == 2 &&
      isTRUE(plot_parameters[["show_region_outlines"]])
  ) {
    hulls <- input[["group_hulls"]]
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
    extra = list(group_hulls = output_hulls),
    deferred_aux = function() {
      build_deferred_aux(
        if (resource_first) {
          seq_len(input[["cell_count"]])
        } else {
          payload[["data"]][["selection_key"]]
        }
      )
    },
    dataset_context = input[["dataset_context"]],
    feedback = input[["feedback"]]
  )
}
