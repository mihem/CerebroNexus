expression_projection_render_state <- new.env(parent = emptyenv())
expression_projection_render_state$geometry <- NULL

expressionSparseColor <- function(values, max_density = 0.5) {
  if (!is.numeric(values) || length(values) < 4096L) {
    return(NULL)
  }
  present <- which(is.na(values) | !is.finite(values) | values != 0)
  if (length(present) >= length(values) * max_density) {
    return(NULL)
  }
  list(
    protocol = "sparse-f64-v1",
    length = length(values),
    index = as.integer(present - 1L),
    color = unname(values[present])
  )
}

expressionPackedColor <- function(values) {
  sparse <- expressionSparseColor(values)
  if (!is.null(sparse)) {
    return(sparse)
  }
  list(
    protocol = "dense-f64-v1",
    length = length(values),
    color = unname(values)
  )
}

expressionColorDatasetKey <- function() {
  if (!exists("viewerDatasetIdentity", mode = "function", inherits = TRUE)) {
    return(NULL)
  }
  identity <- tryCatch(viewerDatasetIdentity(), error = function(error) NULL)
  fingerprint <- as.character(identity$pack_fingerprint %||% "")
  if (
    length(fingerprint) != 1L ||
      is.na(fingerprint) ||
      !nzchar(fingerprint)
  ) {
    fingerprint <- tryCatch(
      as.character(available_crb_files$selected),
      error = function(error) ""
    )
  }
  if (
    length(fingerprint) != 1L ||
      is.na(fingerprint) ||
      !nzchar(fingerprint)
  ) {
    fingerprint <- as.character(identity$fingerprint %||% "")
  }
  if (length(fingerprint) != 1L || is.na(fingerprint) || !nzchar(fingerprint)) {
    return(NULL)
  }
  fingerprint
}

expressionColorCacheKeyForGene <- function(gene, cell_indices, n_cells) {
  gene <- as.character(gene %||% "")
  if (
    length(gene) != 1L ||
      is.na(gene) ||
      !nzchar(gene) ||
      length(cell_indices) != n_cells ||
      !identical(as.integer(cell_indices), seq_len(n_cells))
  ) {
    return(NULL)
  }
  fingerprint <- expressionColorDatasetKey()
  if (is.null(fingerprint)) {
    return(NULL)
  }
  paste(fingerprint, gene, n_cells, sep = "::")
}

expressionColorCacheKey <- function(color_settings, cell_indices, n_cells) {
  genes <- as.character(color_settings[["genes"]] %||% character())
  if (length(genes) != 1L) {
    return(NULL)
  }
  expressionColorCacheKeyForGene(genes[[1L]], cell_indices, n_cells)
}

expressionPackedColorKeys <- function(genes, cell_indices, n_cells) {
  vapply(
    genes,
    function(gene) {
      expressionColorCacheKeyForGene(gene, cell_indices, n_cells) %||% ""
    },
    character(1),
    USE.NAMES = TRUE
  )
}

## function to be executed to update figure
expression_projection_update_plot <- function(input) {
  coordinates <- input[['coordinates']]
  reset_axes <- input[['reset_axes']]
  expression_levels <- input[['expression_levels']]
  plot_parameters <- input[['plot_parameters']]
  color_settings <- input[['color_settings']]
  metadata <- input[['metadata']]
  trajectory <- input[['trajectory']]
  display_mode <- input[['display_mode']]
  render_key <- input[["render_key"]]
  cell_indices <- input[['cell_indices']]
  separate_panels <- input[['separate_panels']]
  no_gene_selected <- length(color_settings[["genes"]]) == 0L
  if (no_gene_selected) {
    expression_levels <- numeric()
  }
  if (is.null(cell_indices)) {
    cell_indices <- seq_len(nrow(metadata))
  }
  n_cells <- if (is.null(metadata)) length(cell_indices) else nrow(metadata)
  appearance <- list(
    group_labels = FALSE,
    draw_border = isTRUE(plot_parameters[["draw_border"]]),
    keep_square = isTRUE(plot_parameters[["keep_square"]])
  )
  ## define output_data
  output_data <- list(
    x = coordinates[[1]],
    y = coordinates[[2]],
    color = expression_levels,
    selection_key = seq_len(n_cells),
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    point_line = list(),
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]],
    paint_order = if (
      identical(plot_parameters[["plot_order"]], "Highest expression on top")
    ) {
      "highest"
    } else {
      "natural"
    },
    reset_axes = reset_axes
  )
  output_data[["render_key"]] <- render_key
  if (is.list(expression_levels)) {
    output_data[["color"]] <- NULL
    if (!identical(display_mode, "rgb")) {
      output_data[["packed_colors"]] <- lapply(
        expression_levels,
        expressionPackedColor
      )
      output_data[["packed_color_cache_keys"]] <- expressionPackedColorKeys(
        names(expression_levels),
        cell_indices,
        n_cells
      )
    }
  } else {
    sparse_color <- expressionSparseColor(expression_levels)
    if (!is.null(sparse_color)) {
      output_data[["color"]] <- NULL
      output_data[["sparse_color"]] <- sparse_color
    }
  }
  output_data[["color_cache_key"]] <- expressionColorCacheKey(
    color_settings,
    cell_indices,
    n_cells
  )
  shared_projection <- if (
    exists("viewerSharedProjectionName", mode = "function", inherits = TRUE)
  ) {
    viewerSharedProjectionName(
      plot_parameters[["projection"]],
      length(cell_indices),
      dataset_context = input[["dataset_context"]]
    )
  } else {
    NULL
  }
  projection_asset <- if (
    is.null(shared_projection) &&
      exists("viewerProjectionAsset", mode = "function", inherits = TRUE)
  ) {
    viewerProjectionAsset(plot_parameters[["projection"]], cell_indices)
  } else {
    NULL
  }
  failed_asset <- input[["projection_resource_failed"]]
  if (
    is.list(projection_asset) &&
      !identical(as.character(failed_asset), projection_asset$url)
  ) {
    output_data[["x"]] <- NULL
    output_data[["y"]] <- NULL
    output_data[["projection_resource"]] <- projection_asset
  }
  if (plot_parameters[["draw_border"]]) {
    output_data[['point_line']] <- list(
      color = "rgb(196,196,196)",
      width = 1
    )
  }
  output_data[["colorscale"]] <- expressionColorScale(
    color_settings[["color_scale"]]
  )
  if (
    is.list(expression_levels) &&
      !identical(display_mode, "rgb") &&
      identical(color_settings[["color_mode"]], "different")
  ) {
    output_data[["panel_colorscales"]] <- expressionPanelColorScales(
      names(expression_levels),
      color_settings[["color_mode"]],
      color_settings[["color_scale"]],
      panel_palette = color_settings[["panel_palette"]]
    )
  }
  output_data[["color_range"]] <- color_settings[["color_range"]]
  output_data[["reversescale"]] <- expressionReverseColorScale(
    color_settings[["color_scale"]]
  )
  ## process trajectory data
  trajectory_lines <- list()
  if (plot_parameters[['is_trajectory']]) {
    ## convert trajectory edges to the shared renderer's shape format
    trajectory_edges <- trajectory[['edges']]
    for (i in seq_len(nrow(trajectory_edges))) {
      line <- list(
        type = "line",
        line = list(color = "black", width = 1),
        xref = "x",
        yref = "y",
        x0 = trajectory_edges$source_dim_1[i],
        y0 = trajectory_edges$source_dim_2[i],
        x1 = trajectory_edges$target_dim_1[i],
        y1 = trajectory_edges$target_dim_2[i]
      )
      trajectory_lines <- c(trajectory_lines, list(line))
    }
  }
  output_hover <- list(hoverinfo = "skip", text = list(), columns = list())
  data_snapshot <- if (exists("data_set", mode = "function")) {
    data_set()
  } else {
    NULL
  }
  core_aux <- function() {
    metadata <- if (is.null(data_snapshot)) {
      getMetaData()
    } else {
      data_snapshot$getMetaData()
    }
    core <- cerebroCellViewDeferredAux(
      selection_rows = seq_len(n_cells),
      cell_barcodes = metadata[["cell_barcode"]][cell_indices],
      hover = isTRUE(plot_parameters[["hover_info"]])
    )
    core$hover$pending <- isTRUE(plot_parameters[["hover_info"]])
    core
  }
  deferred_aux <- function() {
    full_metadata <- if (
      !is.null(metadata) && "cell_barcode" %in% colnames(metadata)
    ) {
      metadata
    } else {
      getMetaData()
    }
    hover <- isTRUE(plot_parameters[["hover_info"]])
    columns <- viewerProjectionMetadataColumns(
      full_metadata,
      color_variable = NULL,
      hover_info = hover,
      groups = getGroups()
    )
    deferred_metadata <- viewerProjectionSubsetRows(
      full_metadata,
      cell_indices,
      columns
    )
    hover_columns <- if (hover) {
      cerebroProjectionHoverColumns(deferred_metadata)
    } else {
      list()
    }
    if (hover && plot_parameters[['is_trajectory']]) {
      states <- as.character(trajectory[['meta']]$state)
      states[is.na(states)] <- "NA"
      state_levels <- unique(states)
      hover_columns <- c(
        hover_columns,
        list(
          list(
            label = "State",
            levels = state_levels,
            values = match(states, state_levels) - 1L
          ),
          list(
            label = "Pseudotime",
            format = "fixed",
            digits = 2L,
            values = as.numeric(trajectory[['meta']]$pseudotime)
          )
        )
      )
    }
    cerebroCellViewDeferredAux(
      selection_rows = seq_len(nrow(deferred_metadata)),
      cell_barcodes = deferred_metadata[["cell_barcode"]],
      hover_columns = hover_columns,
      hover = hover
    )
  }
  geometry <- list(
    dataset = if (exists("data_set", mode = "function")) data_set() else NULL,
    coordinates = coordinates,
    plot_parameters = plot_parameters,
    cells = cell_indices,
    trajectory = trajectory,
    separate = isTRUE(separate_panels),
    rgb_layout = if (identical(display_mode, "rgb")) input[["rgb_layout"]] else NULL,
    request = input[["render_request"]]
  )
  send_render <- function(id, meta, data, hover, extra, deferred_aux) {
    can_recolor <- !isTRUE(reset_axes) &&
      !no_gene_selected &&
      !isTRUE(separate_panels) &&
      !(identical(display_mode, "rgb") && identical(input[["rgb_layout"]], "separate")) &&
      length(coordinates) == 2L &&
      identical(expression_projection_render_state$geometry, geometry)
    if (can_recolor) {
      cerebroCellViewRecolor(
        id,
        meta,
        data,
        dataset_context = input[["dataset_context"]],
        feedback = input[["feedback"]]
      )
    } else {
      cerebroCellViewRender(
        id,
        meta,
        data,
        hover,
        extra,
        deferred_aux,
        core_aux,
        dataset_context = input[["dataset_context"]],
        feedback = input[["feedback"]]
      )
    }
    expression_projection_render_state$geometry <- geometry
  }
  if (identical(display_mode, "rgb") && !no_gene_selected) {
    rgb_levels <- expression_levels[c("r", "g", "b")]
    rgb_genes <- color_settings[["rgb_genes"]][c("r", "g", "b")]
    output_data[["packed_rgb"]] <- lapply(rgb_levels, expressionPackedColor)
    output_data[["packed_rgb_cache_keys"]] <- expressionPackedColorKeys(
      unlist(rgb_genes, use.names = TRUE),
      cell_indices,
      n_cells
    )
    output_data[["rgb_genes"]] <- color_settings[["rgb_genes"]]
    send_render(
      "expression_projection",
      list(
        color_type = "rgb",
        rgb_layout = if (identical(input[["rgb_layout"]], "separate")) "separate" else "composite",
        color_variable = "RGB co-expression",
        show_color_scale = TRUE,
        space_label = plot_parameters[["projection"]],
        appearance = appearance
      ),
      output_data,
      output_hover,
      extra = list(shapes = trajectory_lines),
      deferred_aux = deferred_aux
    )
    return(invisible(NULL))
  }
  n_dimensions <- plot_parameters[["n_dimensions"]]
  multi <- n_dimensions == 2 &&
    isTRUE(separate_panels) &&
    is.list(input[["expression_levels"]])
  if (n_dimensions == 3 && is.null(output_data[["projection_resource"]])) {
    output_data[['z']] <- coordinates[[3]]
  }
  if (n_dimensions == 3 || !is.list(input[["expression_levels"]]) || multi) {
    legend_label <- if (no_gene_selected) {
      plot_parameters[["projection"]]
    } else if (is.list(expression_levels)) {
      "Expression"
    } else if (length(color_settings[["genes"]]) == 1) {
      color_settings[["genes"]][[1]]
    } else {
      paste0("Mean expression (", length(color_settings[["genes"]]), " genes)")
    }
    output_data[["shared_zero_color"]] <- no_gene_selected
    send_render(
      "expression_projection",
      list(
        color_type = "continuous",
        color_variable = legend_label,
        show_color_scale = !no_gene_selected,
        space_label = plot_parameters[["projection"]],
        appearance = appearance
      ),
      output_data,
      output_hover,
      extra = list(shapes = trajectory_lines),
      deferred_aux = deferred_aux
    )
  }
}
