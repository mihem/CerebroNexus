##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
spatial_projection_interaction_data <- function(plot_data) {
  cell_index <- plot_data$cells_df[["cell_index"]]
  metadata <- viewerProjectionSubsetRows(
    getMetaData(),
    cell_index,
    columns = NULL
  )
  cbind(plot_data$coordinates, metadata)
}

spatial_projection_selected_cells <- reactive({
  ## The selection is held persistently on the JS side (see
  ## cell_views.js) and pushed here as {x, y, ids} so it survives plot
  ## parameter changes. Plotly's own plotly_selected event is NOT used, because a
  ## re-render (e.g. changing "Colour by") wipes it while the selection must
  ## stay. The identifier is built the same way the table keys cells (paste0 with
  ## '-'), so downstream filtering is unchanged.
  ## The shared renderer pushes the persistent selection under
  ## <plot_id>_persistent_selection; the spatial plot id is 'spatial_projection'.
  selection <- viewerPersistentSelection(
    input[["spatial_projection_persistent_selection"]],
    viewer_loaded_dataset_context()
  )
  if (is.null(selection)) {
    return(NULL)
  }

  ## Keep the empty selection path independent of million-cell plot data.
  req(spatial_projection_data_to_plot())

  ## Drop cells whose group is currently hidden via the legend, so the count and
  ## the selected-cells panels reflect only visible groups (shared helper in
  ## utility_functions.R). The plotted coordinates come from
  ## spatial_projection_data_to_plot(), keyed the same way as the selection.
  hidden_request <- input[["spatial_projection_hidden_groups"]]
  hidden_groups <- if (
    is.list(hidden_request) &&
      viewerDatasetContextEqual(
        hidden_request[["dataset_context"]],
        viewer_loaded_dataset_context()
      )
  ) {
    as.character(hidden_request[["groups"]] %||% character())
  } else {
    character()
  }
  if (length(hidden_groups) > 0) {
    color_variable <- input[["spatial_projection_point_color"]]
    plot_data <- spatial_projection_data_to_plot()
    metadata <- spatial_projection_interaction_data(plot_data) %>%
      dplyr::rename(X1 = 1, X2 = 2)
    metadata[["selection_key"]] <- if ("cell_barcode" %in% colnames(metadata)) {
      as.character(metadata[["cell_barcode"]])
    } else {
      NULL
    }
    stable_selection <- "selection_key" %in%
      colnames(metadata) &&
      "selection_key" %in% colnames(selection) &&
      any(
        !is.na(selection[["selection_key"]]) &
          nzchar(as.character(selection[["selection_key"]]))
      )
    if (!stable_selection) {
      metadata[["identifier"]] <- paste0(
        metadata[["X1"]],
        "-",
        metadata[["X2"]]
      )
    }
    selection <- filterSelectionByHiddenGroups(
      selection,
      metadata,
      color_variable,
      hidden_groups
    )
    if (is.null(selection) || nrow(selection) == 0) {
      return(NULL)
    }
  }

  selection
})

observeEvent(spatial_projection_selected_cells(), {
  if (
    !is.null(spatial_projection_selected_cells()) &&
      nrow(spatial_projection_selected_cells()) > 0
  ) {
    shinyjs::js$showScrollDownIndicator("Charts generated below ↓")
  } else {
    shinyjs::js$hideScrollDownIndicator()
  }
})
