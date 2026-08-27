##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
spatial_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(spatial_projection_data_to_plot())

  ## The selection is held persistently on the JS side (see
  ## projection_scatter.js) and pushed here as {x, y, ids} so it survives plot
  ## parameter changes. Plotly's own plotly_selected event is NOT used, because a
  ## re-render (e.g. changing "Color cells by") wipes it while the selection must
  ## stay. The identifier is built the same way the table keys cells (paste0 with
  ## '-'), so downstream filtering is unchanged.
  ## The shared renderer pushes the persistent selection under
  ## <plot_id>_persistent_selection; the spatial plot id is 'spatial_projection'.
  sel <- input[["spatial_projection_persistent_selection"]]
  if (is.null(sel) || is.null(sel[["x"]]) || length(sel[["x"]]) == 0) {
    return(NULL)
  }
  selection <- data.frame(
    x = as.numeric(sel[["x"]]),
    y = as.numeric(sel[["y"]]),
    identifier = paste0(as.numeric(sel[["x"]]), '-', as.numeric(sel[["y"]])),
    stringsAsFactors = FALSE
  )
  selection[["selection_key"]] <- if (length(sel[["ids"]]) == nrow(selection)) {
    as.character(sel[["ids"]])
  } else {
    selection[["identifier"]]
  }

  ## Drop cells whose group is currently hidden via the legend, so the count and
  ## the selected-cells panels reflect only visible groups (shared helper in
  ## utility_functions.R). The plotted coordinates come from
  ## spatial_projection_data_to_plot(), keyed the same way as the selection.
  hidden_groups <- input[["spatial_projection_hidden_groups"]]
  if (length(hidden_groups) > 0) {
    color_variable <- input[["spatial_projection_point_color"]]
    plot_data <- spatial_projection_data_to_plot()
    metadata <- cbind(plot_data$coordinates, plot_data$cells_df) %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2))
    metadata[["selection_key"]] <- if ("cell_barcode" %in% colnames(metadata)) {
      as.character(metadata[["cell_barcode"]])
    } else {
      metadata[["identifier"]]
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
