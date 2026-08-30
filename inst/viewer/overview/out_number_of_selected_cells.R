##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["overview_number_of_selected_cells"]] <- renderUI({
  cerebroSelectionSummary(
    overview_projection_selected_cells(),
    input[["overview_projection_to_display"]],
    input[["overview_projection_point_color"]]
  )
})

output[["overview_projection_composition"]] <- renderUI({
  cerebroSelectionSummary(
    overview_projection_selected_cells(),
    input[["overview_projection_to_display"]],
    input[["overview_projection_point_color"]],
    composition = TRUE
  )
})
