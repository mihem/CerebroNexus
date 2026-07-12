##----------------------------------------------------------------------------##
## Reactive that holds IDs of selected cells (ID is built from position in
## projection).
##----------------------------------------------------------------------------##
expression_projection_selected_cells <- reactive({
  ## make sure plot parameters are set because it means that the plot can be
  ## generated
  req(
    expression_projection_parameters_plot(),
    expression_projection_data()
  )
  # message('--> trigger "expression_projection_selected_cells"')

  ## The selection is held persistently on the JS side (shared
  ## projection_scatter.js) and pushed here as {x, y} under
  ## <plot_id>_persistent_selection, so it survives plot-parameter changes
  ## (colour scale / range / point size). Plotly's volatile plotly_selected
  ## event is NOT used, because a re-render would wipe it. The identifier is
  ## built the same way the table keys cells (paste0 with '-'), so downstream
  ## filtering is unchanged.
  sel <- input[["expression_projection_persistent_selection"]]
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
