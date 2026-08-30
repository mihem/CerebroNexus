##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["spatial_number_of_selected_cells"]] <- renderUI({
  cerebroSelectionSummary(
    spatial_projection_selected_cells(),
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_point_color"]]
  )
})

output[["spatial_projection_composition"]] <- renderUI({
  cerebroSelectionSummary(
    spatial_projection_selected_cells(),
    input[["spatial_projection_to_display"]],
    input[["spatial_projection_point_color"]],
    composition = TRUE
  )
})
