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

  cerebroSelectedCellsTableUI("trajectory")
})

##----------------------------------------------------------------------------##
## Table.
##----------------------------------------------------------------------------##

output[["trajectory_details_selected_cells_table"]] <- DT::renderDataTable({
  ## Selection identity is sufficient; appearance controls are initialized
  ## lazily and must not prevent a selected-cell table from rendering.
  req(trajectory_selection_ok())

  selected_cells <- trajectory_projection_selected_cells()
  cells_df <- trajectory_cells_reactive(
    colnames(viewerProjectionFirstFrameMetadata()),
    barcodes = TRUE
  )

  ## The shared table expects the coordinates as its first two columns and
  ## drops them from the output; the trajectory's own DR_1/DR_2 stay in place.
  ## Selections match by barcode, falling back to "DR_1-DR_2" coordinates for
  ## older Plotly event payloads without a stable selection_key.
  cerebroSelectedCellsTable(
    cells_df = cbind(cells_df[, c("DR_1", "DR_2"), drop = FALSE], cells_df),
    empty_data = getMetaData(),
    selection = selected_cells,
    fallback_key = "identifier",
    number_formatting = input[[
      "trajectory_details_selected_cells_table_number_formatting"
    ]],
    color_highlighting = input[[
      "trajectory_details_selected_cells_table_color_highlighting"
    ]],
    download_file_name = "trajectory_details_of_selected_cells"
  )
})

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##

cerebroRegisterInfo(
  input,
  "trajectory_details_selected_cells_table_info",
  trajectory_details_selected_cells_table_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##

trajectory_details_selected_cells_table_info <- list(
  title = "Details of selected cells",
  text = HTML(
    "
    Table containing meta data (some columns may be hidden, check the 'Column visibility' button) for cells selected in the plot using the box or lasso selection tool. If you want the table to contain all cells in the data set, you must select all cells in the plot. The table can be saved to disk in CSV or Excel format for further analysis.
    <h4>Options</h4>
    <b>Automatically format numbers</b><br>
    When active, columns in the table that contain different types of numeric values will be formatted based on what they <u>seem</u> to be. The algorithm will look for integers (no decimal values), percentages, p-values, log-fold changes and apply different formatting schemes to each of them. Importantly, this process does that always work perfectly. If it fails and hinders working with the table, automatic formatting can be deactivated.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <b>Highlight values with colours</b><br>
    Similar to the automatic formatting option, when active, CerebroNexus will look for known columns in the table (those that contain grouping variables), try to interpret column content, and use colours and other stylistic elements to facilitate quick interpretation of the values. If you prefer the table without colours and/or the identification does not work properly, you can simply deactivate this feature.<br>
    <em>This feature does not work on columns that contain 'NA' values.</em><br>
    <br>
    <em>Columns can be re-ordered by dragging their respective header.</em>"
  )
)
