spatial_projection_info <- cerebroProjectionInfo()
cerebroRegisterInfo(input, "spatial_projection_info", spatial_projection_info)

output[["spatial_details_selected_cells_plot"]] <- plotly::renderPlotly({
  req(
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    input[["spatial_selected_cells_plot_select_variable"]],
    spatial_projection_data_to_plot()
  )
  cells_df <- spatial_projection_interaction_data(
    spatial_projection_data_to_plot()
  )
  cerebroSelectedCellsPlot(
    cells_df,
    spatial_projection_selected_cells(),
    input[["spatial_selected_cells_plot_select_variable"]],
    fallback_key = "identifier"
  )
})

output[["spatial_details_selected_cells_table"]] <- DT::renderDataTable({
  req(
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_to_display"]] %in% availableSpatial(),
    spatial_projection_data_to_plot()
  )
  meta_data <- getMetaData()
  req(!is.null(meta_data))
  cerebroSelectedCellsTable(
    spatial_projection_interaction_data(spatial_projection_data_to_plot()),
    meta_data,
    spatial_projection_selected_cells(),
    fallback_key = "identifier",
    number_formatting = input[[
      "spatial_details_selected_cells_table_number_formatting"
    ]],
    color_highlighting = input[[
      "spatial_details_selected_cells_table_color_highlighting"
    ]],
    download_file_name = "spatial_details_of_selected_cells"
  )
})
