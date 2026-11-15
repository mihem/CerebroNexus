##----------------------------------------------------------------------------##
## Coordinates of cells in projection.
##----------------------------------------------------------------------------##
## The rows of the section's coordinates that are drawn, in drawing order, or
## NULL when every row is, in order. The ROI of a drawn cell is read at these
## positions (obj_projection_data_to_plot.R), not by its cell name.
spatial_projection_coordinate_rows <- reactive({
  req(
    spatial_projection_parameters_plot(),
    spatial_projection_cells_to_show()
  )

  parameters <- spatial_projection_parameters_plot()
  cells_to_show <- spatial_projection_cells_to_show()
  req(parameters[["projection"]] %in% availableSpatial())

  canonical_index <- spatial_projection_cell_index()
  coordinates <- viewerSpatialCoordinates(parameters[["projection"]])
  req(coordinates)
  if (
    spatial_coordinates_are_canonical_full_order(
      cells_to_show,
      canonical_index,
      nrow(coordinates)
    )
  ) {
    return(NULL)
  }
  coordinate_rows <- match(cells_to_show, canonical_index)
  req(!anyNA(coordinate_rows))
  coordinate_rows
})

spatial_projection_coordinates <- reactive({
  req(spatial_projection_parameters_plot())
  parameters <- spatial_projection_parameters_plot()
  coordinate_rows <- spatial_projection_coordinate_rows()
  coordinates <- viewerSpatialCoordinates(parameters[["projection"]])
  req(coordinates)
  if (!is.null(coordinate_rows)) {
    coordinates <- coordinates[coordinate_rows, , drop = FALSE]
  }

  return(coordinates)
})
