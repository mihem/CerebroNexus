##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
spatial_projection_metadata <- reactive({
  req(spatial_projection_cells_to_show())
  parameters <- spatial_projection_parameters_plot()
  ## Inputs can still describe the previous dataset while its hidden controls
  ## are remounted. Use the validated first-frame parameters throughout.
  color_variable <- if (identical(parameters[["plot_type"]], "ImageDimPlot")) {
    parameters[["color_variable"]]
  } else {
    character()
  }
  columns <- unique(Filter(
    nzchar,
    c(
      color_variable,
      parameters$split_by
    )
  ))
  full_metadata <- viewerProjectionFirstFrameMetadata()
  req(all(columns %in% colnames(full_metadata)))
  metadata <- viewerProjectionSubsetRows(
    full_metadata,
    spatial_projection_cells_to_show(),
    columns
  )
  metadata[["cell_index"]] <- spatial_projection_cells_to_show()

  return(metadata)
})
