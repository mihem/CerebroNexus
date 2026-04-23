##----------------------------------------------------------------------------##
## Cell meta data and position in projection.
##----------------------------------------------------------------------------##
spatial_projection_expression <- reactive({
  req(
    spatial_projection_parameters_plot(),
    spatial_projection_cells_to_show()
  )

  parameters    <- spatial_projection_parameters_plot()
  cells_to_show <- spatial_projection_cells_to_show()
  req(parameters[["projection"]] %in% availableProjections())

  expression_data <-getExpressionMatrix()[cells_to_show, ]

  message(
    paste0(
      '[', format(Sys.time(), '%H:%M:%S'), '] Expression data shape: ', paste(dim(expression_data), collapse = 'x')
    )
  )

  print(expression_data[1:5, 1:5])
  return(expression_data)
})
