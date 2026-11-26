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
    hover <- isTRUE(plot_parameters[["hover_info"]])
    names_resource <- viewerCellNamesResourceFor(cell_indices)
    ## Every cell in canonical order: the rows would be 0..n-1, so the browser
    ## takes the names as they are.
    if (
      is.list(names_resource) &&
        !is.list(selection_rows) &&
        length(cell_indices) == names_resource$cells &&
        identical(as.integer(selection_rows), seq_along(cell_indices)) &&
        identical(as.integer(cell_indices), seq_along(cell_indices))
    ) {
      return(list(
        selection_index_all = TRUE,
        cell_names_resource = names_resource,
        hover = list(
          hoverinfo = if (hover) "text" else "skip",
          columns = list(),
          pending = hover
        )
      ))
    }
    ## With the cell-name resource the canonical rows of the points are sent
    ## and the browser takes their names from it: the metadata is not read.
    cell_barcodes <- if (!is.list(names_resource)) {
      metadata <- if (is.null(data_snapshot)) {
        getMetaData()
      } else {
        data_snapshot$getMetaData()
      }
      metadata[["cell_barcode"]][cell_indices]
    }
    core <- cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = cell_barcodes,
      hover = hover,
      names_resource = names_resource,
      cell_rows = cell_indices
    )
    core$hover$pending <- hover
    core
  }
  ## `keys = FALSE` after core_aux() named the cells: only the hover columns
  ## are built. They come from the first-frame metadata, so a thin CRB is not
  ## hydrated (its cell names read from the H5 file) just for them.
  deferred_aux <- function(keys = TRUE) {
    hover_columns <- if (isTRUE(plot_parameters[["hover_info"]])) {
      metadata <- viewerProjectionFirstFrameMetadata()
      columns <- viewerProjectionMetadataColumns(
        metadata,
        color_variable,
        hover_info = TRUE,
        groups = getGroups()
      )
      cerebroProjectionHoverColumns(
        viewerProjectionSubsetRows(metadata, cell_indices, columns)
      )
    } else {
      list()
    }
    cerebroCellViewDeferredAux(
      selection_rows = selection_rows,
      cell_barcodes = if (keys) viewerCellBarcodesAt(cell_indices),
      hover_columns = hover_columns,
      hover = plot_parameters[["hover_info"]],
      keys = keys
    )
  }

  if (isTRUE(input[["client_percentage"]])) {
    ## The browser applies "% of cells" to this full frame; later slider edits
    ## arrive through the shared scatter defaults without a new frame.
    payload$data$client_percentage <- TRUE
    payload$data$percentage_cells_to_show <- isolate(current_scatter_percentage())
  }
  cerebroCellViewRender(
    "overview_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]],
    deferred_aux = deferred_aux,
    core_aux = core_aux,
    dataset_context = input[["dataset_context"]],
    feedback = input[["feedback"]],
    ## Barcodes depend only on these. A colour or appearance change keeps them,
    ## so the browser need not download every barcode again.
    aux_key = list(
      cell_indices = cell_indices,
      selection_rows = selection_rows,
      hover = isTRUE(plot_parameters[["hover_info"]])
    )
  )
}
