## Region outlines of a frame, in the form the browser draws: one polygon per
## coloured group present in the frame.
spatial_projection_output_hulls <- function(
  hulls,
  color_assignments,
  unsupported,
  show
) {
  if (isTRUE(unsupported) || !isTRUE(show) || !length(hulls)) {
    return(list())
  }
  present <- intersect(names(hulls), names(color_assignments))
  list(
    x = unname(lapply(hulls[present], `[[`, "x")),
    y = unname(lapply(hulls[present], `[[`, "y")),
    color = unname(as.list(color_assignments[present]))
  )
}
spatial_projection_drawn_hulls <- list()
## The hash of the cells, positions and panels of the grid frame drawn last
## (NULL for any other frame): a new colouring of the same grid sends only its
## colours.
spatial_grid_drawn_geometry <- NULL
## Counts the frames sent; the batches of a streaming grid stop once it moves
## on (spatial_grid_stream_send()).
spatial_grid_stream_serial <- 0L

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
  ## A new frame ends the batches of a grid still streaming.
  spatial_grid_stream_serial <<- spatial_grid_stream_serial + 1L
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
  aux_cells <- if (resource_first) {
    input[["cell_indices"]]
  } else if ("cell_index" %in% colnames(metadata)) {
    metadata[["cell_index"]]
  } else {
    seq_len(nrow(metadata))
  }
  ## The auxiliary data depend only on these. A colour change keeps them, so
  ## the browser need not download the cells' rows and hover data again. The
  ## key goes out with the auxiliary data, so it is their hash, not two more
  ## arrays of every cell.
  aux_key <- function(selection_rows) {
    rlang::hash(list(
      cells = aux_cells,
      selection_rows = selection_rows,
      hover = isTRUE(plot_parameters[["hover_info"]])
    ))
  }
  build_deferred_aux <- function(selection_rows) {
    cell_index <- aux_cells
    hover <- isTRUE(plot_parameters[["hover_info"]])
    first_frame <- viewerProjectionFirstFrameMetadata()
    columns <- viewerProjectionMetadataColumns(
      first_frame,
      color_variable = NULL,
      hover_info = hover,
      groups = getGroups()
    )
    ## Read only for hover text: the rows of a frame of 3.9 million cells
    ## took 0.3 s even without a column.
    hover_metadata <- if (hover) {
      viewerProjectionSubsetRows(first_frame, cell_index, columns)
    }
    ## The browser takes the names from the cell-name resource when there is
    ## one, so only the canonical rows of the points are sent.
    names_resource <- viewerCellNamesResourceFor(cell_index)
    cell_barcodes <- if (is.null(names_resource)) {
      viewerPackCellBarcodes(viewerPackCurrent(), cell_index) %||%
        getMetaData()[["cell_barcode"]][cell_index]
    }
    aux <- cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = cell_barcodes,
      hover_columns = if (hover) {
        cerebroProjectionHoverColumns(hover_metadata)
      } else {
        list()
      },
      hover = hover,
      names_resource = names_resource,
      cell_rows = cell_index
    )
    ## A section's rows are usually contiguous: cerebroCellViewDeferredAux()
    ## sends their range.
    aux
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
  ## Co-expression: one gene per channel, blended in the browser as on the
  ## Gene expression page (color_type "rgb"): a cell expressing none of them
  ## is grey, and the legend names each channel's gene. Without any gene the
  ## frame is the grey "No gene selected" one below.
  is_coexpr <- identical(
    plot_parameters[["plot_type"]],
    "Co-expression (RGB)"
  ) &&
    !isTRUE(plot_parameters[["no_gene"]])
  if (is_coexpr) {
    channel_values <- function(column) {
      values <- if (column %in% colnames(metadata)) {
        as.numeric(metadata[[column]])
      } else {
        numeric(nrow(metadata))
      }
      values[!is.finite(values)] <- 0
      values
    }
    grid <- plot_parameters[["grid"]]
    output_meta <- c(
      background_meta,
      list(
        color_type = "rgb",
        ## Separate channels: one panel per channel (and section, in the grid).
        rgb_layout = if (
          identical(plot_parameters[["rgb_layout"]], "separate")
        ) {
          "separate"
        } else {
          "composite"
        },
        color_variable = "RGB co-expression",
        show_color_scale = TRUE,
        space_label = if (is.list(grid)) {
          "Section grid"
        } else {
          plot_parameters[["projection"]]
        },
        appearance = list(
          group_labels = FALSE,
          draw_border = isTRUE(plot_parameters[["draw_border"]]),
          keep_square = isTRUE(plot_parameters[["keep_square"]])
        )
      )
    )
    output_data <- list(
      x = as.numeric(coordinates[[1]]),
      y = as.numeric(coordinates[[2]]),
      selection_key = selection_keys,
      rgb = list(
        r = channel_values("coexpr_r"),
        g = channel_values("coexpr_g"),
        b = channel_values("coexpr_b")
      ),
      rgb_genes = list(
        r = plot_parameters[["coexpr_r"]] %||% "",
        g = plot_parameters[["coexpr_g"]] %||% "",
        b = plot_parameters[["coexpr_b"]] %||% ""
      ),
      point_size = plot_parameters[["point_size"]],
      point_opacity = plot_parameters[["point_opacity"]],
      point_line = point_line,
      x_range = x_range_out,
      y_range = y_range_out,
      reset_axes = reset_axes
    )
    if (is.list(grid)) {
      grid$coordinates <- coordinates
      output_data$panels <- c(
        spatial_grid_projection_panel(grid$projection),
        spatial_grid_panels(
          grid,
          plot_parameters[["grid_backgrounds"]] %||% list(),
          if (exists("Cerebro.options")) {
            Cerebro.options[["cerebro_root"]]
          } else {
            NULL
          }
        )
      )
    }
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
      feedback = input[["feedback"]],
      aux_key = aux_key(output_data[["selection_key"]])
    )
    return(invisible(NULL))
  }

  n_dimensions <- plot_parameters[["n_dimensions"]]
  ## A section's frame reads the codes of every cell of the data set and maps
  ## them through its canonical rows (selection_rows below); the payload
  ## checks the codes against the frame's cell count.
  section_codes <- resource_first &&
    is.list(categorical_resource) &&
    !identical(
      as.integer(categorical_resource$cells),
      as.integer(input[["cell_count"]])
    )
  payload_codes <- categorical_resource
  if (section_codes) {
    payload_codes$cells <- as.integer(input[["cell_count"]])
  }
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
      payload_codes
    } else {
      NULL
    }
  )
  if (section_codes) {
    payload$data$categorical_resource <- categorical_resource
  }
  if (
    identical(plot_parameters[["plot_type"]], "ImageFeaturePlot") &&
      !isTRUE(plot_parameters[["no_gene"]])
  ) {
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

  ## A grid: one panel per section over its slice of this frame. The frame's
  ## single colour vector and range serve every panel.
  grid <- plot_parameters[["grid"]]
  if (is.list(grid)) {
    grid$coordinates <- coordinates
    payload$data$panels <- c(
      spatial_grid_projection_panel(grid$projection),
      spatial_grid_panels(
        grid,
        plot_parameters[["grid_backgrounds"]] %||% list(),
        if (exists("Cerebro.options")) {
          Cerebro.options[["cerebro_root"]]
        } else {
          NULL
        }
      )
    )
    ## Slices index the frame's rows, so x, y and keys are flat in frame order
    ## (a small categorical payload nests them by group).
    payload$data$x <- as.numeric(coordinates[[1L]])
    payload$data$y <- as.numeric(coordinates[[2L]])
    payload$data$selection_key <- selection_keys
    if (is.numeric(color_input)) {
      payload$data$color_range <- spatial_grid_color_range(color_input)
    } else {
      trace_names <- unlist(payload$meta$traces, use.names = FALSE)
      ## The payload's codes of the traces, when it has them: matching 3.9
      ## million names again took 0.2 s.
      canonical <- payload$data$canonical_group
      payload$data$group_code <- if (length(canonical) == length(color_input)) {
        as.integer(unclass(canonical))
      } else {
        grid_groups <- as.character(color_input)
        grid_groups[is.na(grid_groups)] <- "(missing)"
        as.integer(match(grid_groups, trace_names) - 1L)
      }
      payload$data$canonical_group <- NULL
      payload$data$color <- NULL
      ## The payload adds "(missing)" with this colour when a cell has none.
      grid_colors <- unname(color_assignments[trace_names])
      grid_colors[is.na(grid_colors)] <- "#7b8794"
      payload$meta$group_colors <- grid_colors
    }
    payload$meta$space_label <- "Section grid"
  }

  split_by <- plot_parameters[["split_by"]] %||% ""
  if (
    !is.list(grid) &&
      nzchar(split_by) &&
      split_by %in% colnames(metadata)
  ) {
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
    ## The other panels follow the column's levels (natural order for
    ## character columns), not the order in which their cells come; missing
    ## values last.
    split_values <- metadata[[split_by]]
    label_order <- if (is.factor(split_values)) {
      levels(split_values)
    } else {
      .runtimeNaturalSort(unique(panel_labels))
    }
    label_order <- c(
      setdiff(label_order[label_order %in% panel_labels], "N/A"),
      setdiff(unique(panel_labels), c(label_order, "N/A")),
      "N/A"
    )
    panel_order <- c(panel_order, setdiff(label_order, panel_order))
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

  output_hulls <- spatial_projection_output_hulls(
    input[["group_hulls"]],
    color_assignments,
    is.numeric(color_input) || n_dimensions != 2,
    plot_parameters[["show_region_outlines"]]
  )
  spatial_projection_drawn_hulls <<- output_hulls
  aux_rows <- if (resource_first) {
    seq_len(input[["cell_count"]])
  } else {
    payload[["data"]][["selection_key"]]
  }
  ## Points in cell order: the frame names its canonical rows (a range).
  if (
    is.null(payload$data$panels) &&
      is.numeric(aux_rows) &&
      identical(as.integer(aux_rows), seq_along(aux_rows))
  ) {
    payload$data$selection_rows <- cerebroRowRange(aux_cells - 1L)
  }
  ## A grid coloured anew over the same cells, positions and panels: only the
  ## colours travel, and the browser keeps what it has (recolorSingle() in
  ## cell_views.js). Sending the 3.9 million positions of the Allen atlas again
  ## took 40 s. A new render request (a remounted canvas, a data set switch)
  ## always gets the whole frame.
  grid_geometry <- if (is.list(grid)) {
    rlang::hash(list(
      context = input[["dataset_context"]],
      x = payload$data$x,
      y = payload$data$y,
      keys = payload$data$selection_key,
      panels = payload$data$panels,
      x_range = payload$data$x_range,
      y_range = payload$data$y_range
    ))
  }
  if (
    !is.null(grid_geometry) &&
      identical(grid_geometry, spatial_grid_drawn_geometry) &&
      !isTRUE(input[["fresh_render"]]) &&
      !isTRUE(reset_axes)
  ) {
    ## A gene expressed in under half the cells travels as those cells and
    ## their values (cerebroSparseColorPacket()): Vip over the 3.9 million
    ## cells of the Allen atlas is 7.4 MB instead of 31.5 MB.
    sparse <- cerebroSparseColorPacket(payload$data$color)
    if (!is.null(sparse)) {
      payload$data$color <- NULL
      payload$data$sparse_color <- sparse
    }
    cerebroCellViewRecolor(
      "spatial_projection",
      payload[["meta"]],
      payload[["data"]],
      dataset_context = input[["dataset_context"]],
      feedback = input[["feedback"]]
    )
    return(invisible(NULL))
  }
  spatial_grid_drawn_geometry <<- grid_geometry
  ## A grid of many cells streams: the sections shown first draw at once, the
  ## others as their batches arrive. Until the last is sent a new colouring
  ## gets a whole frame, since a recolour holds the colours of every cell.
  stream <- if (is.list(grid)) {
    spatial_grid_stream_split(payload$data, spatial_grid_shown_sections())
  }
  deferred_aux <- function() build_deferred_aux(aux_rows)
  if (!is.null(stream)) {
    payload$data <- stream$head
    spatial_grid_drawn_geometry <<- NULL
    stream <- spatial_grid_stream_keys(stream, deferred_aux())
    deferred_aux <- function() stream$head_aux
  }
  render_token <- cerebroCellViewRender(
    "spatial_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]],
    extra = list(group_hulls = output_hulls),
    deferred_aux = deferred_aux,
    dataset_context = input[["dataset_context"]],
    feedback = input[["feedback"]],
    aux_key = aux_key(aux_rows)
  )
  if (!is.null(stream)) {
    spatial_grid_stream_send(
      stream,
      render_token,
      input[["dataset_context"]],
      grid_geometry
    )
  }
  invisible(render_token)
}

## The sections the Sections field shows, in its order (the browser lays the
## grid out by it).
spatial_grid_shown_sections <- function() {
  tryCatch(
    as.character(
      shiny::isolate(
        session$input[["spatial_projection_sections"]]
      ) %||%
        character()
    ),
    error = function(error) character()
  )
}

## Sends batches 2, 3, ... of a streaming grid (spatial_grid_stream_split()),
## each in its own turn of the event loop so that the session answers its
## inputs in between. A newer frame of the page stops them; the browser drops
## the rows of a frame it no longer shows. After the last, a new colouring of
## the grid sends only its colours.
spatial_grid_stream_send <- function(
  stream,
  render_token,
  dataset_context,
  geometry
) {
  serial <- spatial_grid_stream_serial
  context <- viewerDatasetContext(
    dataset_context$epoch,
    dataset_context$dataset_key,
    dataset_context$generation
  )
  send <- function(k) {
    if (
      !identical(serial, spatial_grid_stream_serial) ||
        isTRUE(tryCatch(session$isClosed(), error = function(error) TRUE))
    ) {
      return(invisible(NULL))
    }
    part <- stream$part(k)
    last <- k >= stream$batches
    message <- list(
      id = "spatial_projection",
      dataset_context = context,
      render_token = render_token,
      batch = k,
      done = last,
      section = I(part$section),
      start = I(part$start),
      count = I(part$count)
    )
    for (field in intersect(
      c(
        "x",
        "y",
        "group_code",
        "color",
        "projection",
        "selection_index",
        "selection_key"
      ),
      names(part)
    )) {
      value <- part[[field]]
      message[[field]] <- if (identical(field, "projection")) {
        lapply(value, function(values) I(unname(values)))
      } else if (is.list(value)) {
        value
      } else {
        I(unname(value))
      }
    }
    session$sendBinaryMessage(
      "cell_view_grid_rows",
      cv_wire_pack_message(message)
    )
    if (last) {
      spatial_grid_drawn_geometry <<- geometry
    } else {
      later::later(function() send(k + 1L), 0)
    }
    invisible(NULL)
  }
  ## A frame of one batch (its sections' positions all in files) is sent.
  if (stream$batches < 2L) {
    spatial_grid_drawn_geometry <<- geometry
    return(invisible(NULL))
  }
  later::later(function() send(2L), 0)
}
