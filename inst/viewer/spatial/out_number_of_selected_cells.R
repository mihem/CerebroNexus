##----------------------------------------------------------------------------##
## Number and composition of the selected cells.
##----------------------------------------------------------------------------##
viewerSelectionSummaryOutputs(
  "spatial_projection",
  "spatial_number_of_selected_cells",
  function() spatial_projection_selected_cells(),
  source = function() {
    chosen <- spatial_projection_chosen_sections(input, availableSpatial())
    if (length(chosen) > 1L) {
      ## A grid: the section the browser says the cells were selected in.
      picked <- input[["spatial_projection_persistent_selection"]][["source"]]
      return(
        if (is.character(picked) && length(picked) == 1L && nzchar(picked)) {
          picked
        }
      )
    }
    if (length(chosen)) chosen[[1L]]
  },
  color = function() input[["spatial_projection_point_color"]],
  require_cells = TRUE
)
