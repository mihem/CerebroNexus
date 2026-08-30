##----------------------------------------------------------------------------##
## Text showing the number of selected cells.
##----------------------------------------------------------------------------##
output[["expression_number_of_selected_cells"]] <- renderUI({
  cerebroSelectionSummary(
    expression_projection_selected_cells(),
    input[["expression_projection_to_display"]]
  )
})

output[["expression_projection_composition"]] <- renderUI({
  cerebroSelectionSummary(
    expression_projection_selected_cells(),
    input[["expression_projection_to_display"]],
    composition = TRUE
  )
})
