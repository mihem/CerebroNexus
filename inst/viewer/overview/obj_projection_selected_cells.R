##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
overview_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(overview_projection_data_to_plot())

  ## The selection is held persistently on the JS side (shared
  ## cell_views.js) and pushed here as {x, y, ids} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes
  ## (colour / point size / % of cells). Plotly's volatile plotly_selected event
  ## is NOT used, because a re-render would wipe it. The identifier is built the
  ## same way the table keys cells (paste0 with '-'), so downstream filtering is
  ## unchanged.
  sel <- input[["overview_projection_persistent_selection"]]
  if (is.null(sel) || is.null(sel[["x"]]) || length(sel[["x"]]) == 0) {
    return(NULL)
  }
  selection <- data.frame(
    x = as.numeric(sel[["x"]]),
    y = as.numeric(sel[["y"]]),
    identifier = paste0(as.numeric(sel[["x"]]), '-', as.numeric(sel[["y"]])),
    stringsAsFactors = FALSE
  )
  if (length(sel[["ids"]]) == nrow(selection)) {
    selection[["selection_key"]] <- as.character(sel[["ids"]])
  }

  ## Drop cells whose group is currently hidden via the legend, so the count and
  ## the selected-cells panels reflect only visible groups. The shared JS pushes
  ## the hidden group names under <plot_id>_hidden_groups; the grouping variable
  ## is whatever the projection is coloured by. Cells are mapped to their group
  ## through an `identifier` (X1-X2) built the same way as the selection key.
  hidden_groups <- input[["overview_projection_hidden_groups"]]
  if (length(hidden_groups) > 0) {
    color_variable <- input[["overview_projection_point_color"]]
    projection <- getProjection(input[["overview_projection_to_display"]])
    metadata <- cbind(projection, getMetaData())
    metadata <- metadata %>%
      dplyr::rename(X1 = 1, X2 = 2) %>%
      dplyr::mutate(identifier = paste0(X1, '-', X2))
    metadata[["selection_key"]] <- if ("cell_barcode" %in% colnames(metadata)) {
      as.character(metadata[["cell_barcode"]])
    } else {
      as.character(seq_len(nrow(metadata)))
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
