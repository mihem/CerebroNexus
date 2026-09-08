##----------------------------------------------------------------------------##
## Table for details of selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element with toggle switches (for automatic number formatting and
## coloring) and table.
##----------------------------------------------------------------------------##
output[["expression_details_selected_cells_UI"]] <- renderUI({
  cerebroSelectedCellsTableUI(
    "expression_details_selected_cells",
    "Details of selected cells"
  )
})

##----------------------------------------------------------------------------##
## Table with results.
##----------------------------------------------------------------------------##
output[["expression_details_selected_cells"]] <- DT::renderDataTable({
  req(
    expression_projection_data(),
    expression_projection_coordinates(),
    expression_projection_expression_levels()
  )
  selected_cells <- expression_projection_selected_cells()
  cells_df <- NULL
  if (!is.null(selected_cells)) {
    cells_df <- bind_cols(
      expression_projection_coordinates(),
      expression_projection_data()
    )
    expression_levels <- expression_projection_expression_levels()
    cells_df$level <- if (is.list(expression_levels)) {
      Matrix::rowMeans(do.call(cbind, expression_levels))
    } else {
      expression_levels
    }
    cells_df <- selectedCellRows(cells_df, selected_cells) %>%
      dplyr::rename(expression_level = level) %>%
      dplyr::select(
        dplyr::any_of("cell_barcode"),
        expression_level,
        everything()
      )
  }
  selectedCellsDataTable(
    cells_df,
    getMetaData(),
    input[["expression_details_selected_cells_number_formatting"]],
    input[["expression_details_selected_cells_color_highlighting"]],
    "expression_details_of_selected_cells"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_details_selected_cells_info"]], {
  showCerebroInfoModal(expression_details_selected_cells_info)
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_details_selected_cells_info <- cerebroSelectedCellsTableInfo(
  "Table containing (average) expression values of selected genes as well as selected meta data (sample, cluster, number of transcripts, number of expressed genes) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.",
  state = "active"
)
