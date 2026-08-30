##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
expression_projection_data <- reactive({
  req(expression_projection_cells_to_show())
  cells_df <- getMetaData()[expression_projection_cells_to_show(), ]
  return(cells_df)
})
