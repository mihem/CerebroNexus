##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["spatial_selected_cells_table_UI"]] <- renderUI({
  req(spatial_projection_selected_cells())
  cerebroSelectedCellsTableUI(
    "spatial_details_selected_cells_table",
    "Table of selected cells"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_details_selected_cells_table_info"]], {
  showModal(
    modalDialog(
      spatial_details_selected_cells_table_info$text,
      title = spatial_details_selected_cells_table_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
spatial_details_selected_cells_table_info <- cerebroSelectedCellsTableInfo(
  "Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.",
  state = "activated"
)
