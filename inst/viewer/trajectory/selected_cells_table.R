##----------------------------------------------------------------------------##
## Tab: Trajectory
##
## Table of selected cells.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## UI element for output.
##----------------------------------------------------------------------------##

output[["trajectory_selected_cells_table_UI"]] <- renderUI({
  req(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]],
    trajectory_projection_selected_cells()
  )
  cerebroSelectedCellsTableUI(
    "trajectory_details_selected_cells_table",
    "Table of selected cells"
  )
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##

output[["trajectory_details_selected_cells_table"]] <- DT::renderDataTable({
  req(
    trajectory_selection_ok(),
    input[["trajectory_percentage_cells_to_show"]],
    input[["trajectory_point_color"]],
    input[["trajectory_point_size"]],
    input[["trajectory_point_opacity"]]
  )
  trajectory_data <- getTrajectory(
    input[["trajectory_selected_method"]],
    input[["trajectory_selected_name"]]
  )
  meta_data <- getMetaData()
  req(!is.null(meta_data))
  cells_df <- mergeTrajectoryWithMetaData(trajectory_data) %>%
    dplyr::filter(!is.na(pseudotime))
  cells_df <- selectedCellRows(
    cells_df,
    trajectory_projection_selected_cells(),
    coordinate_columns = c("DR_1", "DR_2"),
    drop_coordinates = FALSE
  )
  selectedCellsDataTable(
    cells_df,
    meta_data,
    input[["trajectory_details_selected_cells_table_number_formatting"]],
    input[["trajectory_details_selected_cells_table_color_highlighting"]],
    "trajectory_details_of_selected_cells"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

observeEvent(input[["trajectory_details_selected_cells_table_info"]], {
  showCerebroInfoModal(trajectory_details_selected_cells_table_info)
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_details_selected_cells_table_info <- cerebroSelectedCellsTableInfo(
  "Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.",
  state = "active"
)
