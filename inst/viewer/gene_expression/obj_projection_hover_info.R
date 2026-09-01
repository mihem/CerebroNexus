##----------------------------------------------------------------------------##
## Hover info for cells in projection.
##----------------------------------------------------------------------------##
expression_projection_hover_info <- reactive({
  req(
    hover_info_projections(),
    expression_projection_cells_to_show()
  )
  if (
    !is.null(preferences[["show_hover_info_in_projections"]]) &&
      preferences[['show_hover_info_in_projections']] == TRUE
  ) {
    hover_info <- hover_info_projections()[expression_projection_cells_to_show()]
  } else {
    hover_info <- hover_info_projections()
  }
  return(hover_info)
})
