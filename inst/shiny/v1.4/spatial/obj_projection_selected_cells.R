##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
spatial_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(spatial_projection_data_to_plot())

  ## The selection is held persistently on the JS side (see
  ## js_projection_update_plot.js) and pushed here as {x, y} so it survives plot
  ## parameter changes. Plotly's own plotly_selected event is NOT used, because a
  ## re-render (e.g. changing "Color cells by") wipes it while the selection must
  ## stay. The identifier is built the same way the table keys cells (paste0 with
  ## '-'), so downstream filtering is unchanged.
  sel <- input[["spatial_persistent_selection"]]
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
