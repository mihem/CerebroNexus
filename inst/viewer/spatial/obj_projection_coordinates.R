##----------------------------------------------------------------------------##
## Coordinates of cells in projection.
##----------------------------------------------------------------------------##
spatial_projection_coordinates <- reactive({
  projection <- input[["spatial_projection_to_display"]]
  req(projection, projection %in% availableSpatial())
  cells_to_show <- spatial_projection_cells_to_show()
  req(cells_to_show)

  ## `cells_to_show` are row positions into getMetaData(). Spatial coordinates
  ## are stored in .getSpatialData()'s own `common_cells` order (a possibly
  ## reordered subset), so they must NOT be indexed by metadata row positions.
  ## Resolve each requested cell to its barcode, then look the coordinate row up
  ## by barcode so the two tables align by identity, not by position. Rows kept
  ## here stay in `cells_to_show` order to match spatial_projection_metadata().
  meta_data <- getMetaData()
  if ("cell_barcode" %in% colnames(meta_data)) {
    barcodes <- as.character(meta_data[["cell_barcode"]])[cells_to_show]
  } else {
    barcodes <- rownames(meta_data)[cells_to_show]
  }

  spatial_data <- getSpatialData(projection)
  ## Barcodes absent from the coordinate table (metadata cells without a spatial
  ## position) yield NA rows here; they are simply not drawn. Bail out only if
  ## NOTHING matched, which signals a barcode-space mismatch rather than a few
  ## missing spots.
  req(any(barcodes %in% rownames(spatial_data$coordinates)))
  coordinates <- spatial_data$coordinates[barcodes, , drop = FALSE]

  return(coordinates)
})
