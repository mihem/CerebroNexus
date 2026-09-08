##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##
output[["overview_selected_cells_table_UI"]] <- renderUI({
  req(overview_projection_selected_cells())
  cerebroSelectedCellsTableUI(
    "overview_details_selected_cells_table",
    "Table of selected cells"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_details_selected_cells_table_info"]], {
  showModal(
    modalDialog(
      overview_details_selected_cells_table_info$text,
      title = overview_details_selected_cells_table_info$title,
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
overview_details_selected_cells_table_info <- cerebroSelectedCellsTableInfo(
  "Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.",
  state = "activated"
)
