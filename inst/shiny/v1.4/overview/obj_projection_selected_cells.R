##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
overview_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(overview_projection_data_to_plot())

  ## The selection is held persistently on the JS side (shared
  ## projection_scatter.js) and pushed here as {x, y} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes
  ## (colour / point size / % of cells). Plotly's volatile plotly_selected event
  ## is NOT used, because a re-render would wipe it. The identifier is built the
  ## same way the table keys cells (paste0 with '-'), so downstream filtering is
  ## unchanged.
  sel <- input[["overview_projection_persistent_selection"]]
  if (is.null(sel) || is.null(sel[["x"]]) || length(sel[["x"]]) == 0) {
    return(NULL)
  }
  data.frame(
    x = as.numeric(sel[["x"]]),
    y = as.numeric(sel[["y"]]),
    identifier = paste0(as.numeric(sel[["x"]]), '-', as.numeric(sel[["y"]])),
    stringsAsFactors = FALSE
  )
})
