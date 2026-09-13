##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
overview_projection_cells_to_show <- reactive({
  req(input[["overview_projection_percentage_cells_to_show"]])
  viewerProjectionCellIndices("overview_projection")
})
