##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##
output[["spatial_details_selected_cells_table"]] <- DT::renderDataTable({
  req(
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    spatial_projection_data_to_plot()
  )
  meta_data <- getMetaData()
  req(!is.null(meta_data))
  selected_cells <- spatial_projection_selected_cells()
  cells_df <- NULL
  if (!is.null(selected_cells)) {
    plot_data <- spatial_projection_data_to_plot()
    cells_df <- selectedCellRows(
      cbind(plot_data$coordinates, plot_data$cells_df),
      selected_cells,
      missing_key = "identifier"
    )
  }
  selectedCellsDataTable(
    cells_df,
    meta_data,
    input[["spatial_details_selected_cells_table_number_formatting"]],
    input[["spatial_details_selected_cells_table_color_highlighting"]],
    "spatial_details_of_selected_cells"
  )
})
