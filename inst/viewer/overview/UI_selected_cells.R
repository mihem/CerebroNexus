output[["overview_selected_cells_plot_UI"]] <- renderUI({
  req(overview_projection_selected_cells())
  choices <- setdiff(colnames(getMetaData()), "cell_barcode")
  cerebroSelectedCellsPlotUI("overview", choices)
})

cerebroRegisterInfo(
  input,
  "overview_details_selected_cells_plot_info",
  cerebroSelectedCellsPlotInfo()
)

output[["overview_selected_cells_table_UI"]] <- renderUI({
  req(overview_projection_selected_cells())
  cerebroSelectedCellsTableUI("overview")
})

cerebroRegisterInfo(
  input,
  "overview_details_selected_cells_table_info",
  cerebroSelectedCellsTableInfo()
)
