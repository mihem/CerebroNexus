overview_projection_info <- cerebroProjectionInfo()
cerebroRegisterInfo(input, "overview_projection_info", overview_projection_info)

output[["overview_details_selected_cells_plot"]] <- plotly::renderPlotly({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections(),
    input[["overview_selected_cells_plot_select_variable"]]
  )
  cells_df <- cbind(
    getProjection(input[["overview_projection_to_display"]]),
    getMetaData()
  )
  cerebroSelectedCellsPlot(
    cells_df,
    overview_projection_selected_cells(),
    input[["overview_selected_cells_plot_select_variable"]],
    fallback_key = "row"
  )
})

output[["overview_details_selected_cells_table"]] <- DT::renderDataTable({
  req(
    input[["overview_projection_to_display"]],
    input[["overview_projection_to_display"]] %in% availableProjections()
  )
  cerebroSelectedCellsTable(
    cbind(
      getProjection(input[["overview_projection_to_display"]]),
      getMetaData()
    ),
    viewerProjectionFirstFrameMetadata(),
    overview_projection_selected_cells(),
    fallback_key = "row",
    number_formatting = input[[
      "overview_details_selected_cells_table_number_formatting"
    ]],
    color_highlighting = input[[
      "overview_details_selected_cells_table_color_highlighting"
    ]],
    download_file_name = "overview_details_of_selected_cells"
  )
})
