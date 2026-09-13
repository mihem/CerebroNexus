##----------------------------------------------------------------------------##
## Indices of cells to show in projection.
##----------------------------------------------------------------------------##
spatial_projection_cells_to_show <- reactive({
  req(input[["spatial_projection_percentage_cells_to_show"]])
  meta_data <- getMetaData()
  req(!is.null(meta_data))
  viewerProjectionCellIndices("spatial_projection", meta_data)
})
