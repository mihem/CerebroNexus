##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
overview_projection_data <- reactive({
  cells_df <- getMetaData()[
    overview_projection_cells_to_show(),
    ,
    drop = FALSE
  ]
  return(cells_df)
})
