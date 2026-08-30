##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  req(overview_projection_cells_to_show())
  cells_df <- getMetaData()[overview_projection_cells_to_show(), ]
  return(cells_df)
})
