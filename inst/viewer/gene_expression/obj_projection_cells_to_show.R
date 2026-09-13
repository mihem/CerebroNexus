##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
expression_projection_cells_to_show <- reactive({
  req(input[["expression_projection_percentage_cells_to_show"]])
  viewerProjectionCellIndices("expression_projection")
})
