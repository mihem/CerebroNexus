##----------------------------------------------------------------------------##
## Number and composition of the selected cells.
##----------------------------------------------------------------------------##
viewerSelectionSummaryOutputs(
  "spatial_projection",
  "spatial_number_of_selected_cells",
  function() spatial_projection_selected_cells(),
  source = function() {
    spatial_projection_single_section(input, availableSpatial())
  },
  color = function() input[["spatial_projection_point_color"]],
  require_cells = TRUE
)
