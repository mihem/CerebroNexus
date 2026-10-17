##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
expression_projection_cells_to_show <- reactive({
  viewerProjectionCellIndices(
    "expression_projection",
    metadata = viewerProjectionFirstFrameMetadata(),
    percentage = current_scatter_percentage()
  )
})
