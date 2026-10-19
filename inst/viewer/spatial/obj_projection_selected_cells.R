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
  ## the selected-cells panels reflect only visible groups. The plotted
  ## coordinates come from spatial_projection_data_to_plot(), keyed the same way
  ## as the selection.
  viewerVisibleSelection(
    selection,
    input[["spatial_projection_hidden_groups"]],
    viewer_loaded_dataset_context(),
    input[["spatial_projection_point_color"]],
    function() {
      spatial_projection_interaction_data(spatial_projection_data_to_plot())
    }
  )
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
