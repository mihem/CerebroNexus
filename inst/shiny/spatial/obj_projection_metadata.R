##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
spatial_projection_metadata <- reactive({
  req(spatial_projection_cells_to_show())
  # message('--> trigger "spatial_projection_metadata"')
  metadata <- getMetaData()[spatial_projection_cells_to_show(),]

  return(metadata)
})
