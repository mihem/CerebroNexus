##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
overview_projection_update_plot <- function(input) {
  cells_df <- input[["cells_df"]]
  cell_indices <- input[["cell_indices"]]
  coordinates <- input[["coordinates"]]
  reset_axes <- input[["reset_axes"]]
  plot_parameters <- input[["plot_parameters"]]
  color_assignments <- input[["color_assignments"]]
  color_variable <- plot_parameters[["color_variable"]]
  n_dimensions <- plot_parameters[["n_dimensions"]]
  resource_coordinates <- isTRUE(input[["resource_coordinates"]])
  payload <- cerebroCellViewScatterPayload(
    coordinates = coordinates,
    color = cells_df[[color_variable]],
    color_variable = color_variable,
    selection_keys = seq_len(nrow(cells_df)),
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    group_labels = plot_parameters[["group_labels"]],
    keep_square = plot_parameters[["keep_square"]],
    point_line = if (plot_parameters[["draw_border"]]) {
      list(color = "rgb(196,196,196)", width = 1)
    } else {
      list()
    },
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]],
    reset_axes = reset_axes,
    n_dimensions = n_dimensions,
    color_assignments = color_assignments,
    hover_columns = list(),
    hover = FALSE,
    space_label = plot_parameters[["projection"]],
    cell_count = input[["cell_count"]],
    coordinate_resource = if (resource_coordinates) {
      input[["projection_resource"]]
    } else {
      NULL
    },
    categorical_resource = if (resource_coordinates) {
      input[["categorical_resource"]]
    } else {
      NULL
    }
  )
  selection_rows <- if (is.list(input[["categorical_resource"]])) {
    seq_len(input[["cell_count"]])
  } else {
    payload$data$selection_key
  }
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
      selection_rows = selection_rows,
      cell_barcodes = metadata[["cell_barcode"]][cell_indices],
      hover = isTRUE(plot_parameters[["hover_info"]])
    )
    core$hover$pending <- isTRUE(plot_parameters[["hover_info"]])
    core
  }
  deferred_aux <- function() {
    metadata <- getMetaData()
    groups <- getGroups()
    hover_columns <- if (isTRUE(plot_parameters[["hover_info"]])) {
      columns <- viewerProjectionMetadataColumns(
        metadata,
        color_variable,
        hover_info = TRUE,
        groups = groups
      )
      cerebroProjectionHoverColumns(
        viewerProjectionSubsetRows(metadata, cell_indices, columns)
      )
    } else {
      list()
    }
    cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = metadata[["cell_barcode"]][cell_indices],
      hover_columns = hover_columns,
      hover = plot_parameters[["hover_info"]]
    )
  }

  cerebroCellViewRender(
    "overview_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]],
    deferred_aux = deferred_aux,
    core_aux = core_aux,
    dataset_context = input[["dataset_context"]],
    feedback = input[["feedback"]]
  )
}
