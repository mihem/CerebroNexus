##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##
output[["overview_details_selected_cells_table"]] <- DT::renderDataTable({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections()
  )
  selected_cells <- overview_projection_selected_cells()
  cells_df <- NULL
  if (!is.null(selected_cells)) {
    cells_df <- cbind(
      getProjection(input[["overview_projection_to_display"]]),
      getMetaData()
    )
    cells_df <- selectedCellRows(cells_df, selected_cells)
  }
  selectedCellsDataTable(
    cells_df,
    getMetaData(),
    input[["overview_details_selected_cells_table_number_formatting"]],
    input[["overview_details_selected_cells_table_color_highlighting"]],
    "overview_details_of_selected_cells"
  )
})
