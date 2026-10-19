##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
overview_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated. Isolated: a recolour must not rebuild a large selection (its
  ## coordinate identifiers are formatted for every selected cell); the browser
  ## republishes the selection whenever it changes.
  req(isolate(overview_projection_data_to_plot()))

  ## The selection is held persistently on the JS side (shared
  ## cell_views.js) and pushed here as {x, y, ids} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes
  ## (colour / point size / % of cells). Plotly's volatile plotly_selected event
  ## is NOT used, because a re-render would wipe it. The identifier is built the
  ## same way the table keys cells (paste0 with '-'), so downstream filtering is
  ## unchanged.
  selection <- viewerPersistentSelection(
    input[["overview_projection_persistent_selection"]],
    viewer_loaded_dataset_context()
  )
  if (is.null(selection)) {
    return(NULL)
  }

  ## Drop cells whose group is currently hidden via the legend, so the count and
  ## the selected-cells panels reflect only visible groups. The shared JS pushes
  ## the hidden group names under <plot_id>_hidden_groups; the grouping variable
  ## is whatever the projection is coloured by.
  viewerVisibleSelection(
    selection,
    input[["overview_projection_hidden_groups"]],
    viewer_loaded_dataset_context(),
    input[["overview_projection_point_color"]],
    function() {
      cbind(
        getProjection(input[["overview_projection_to_display"]]),
        getMetaData()
      )
    }
  )
})
