##----------------------------------------------------------------------------##
## UI elements to set group filters for the spatial projection.
##
## Implementation lives in inst/shiny/module/group_filters/group_filters_widget.R.
##----------------------------------------------------------------------------##

registerGroupFiltersUI(
  output,
  "spatial_projection",
  getGroups = getGroups,
  getGroupLevels = getGroupLevels,
  render_request = function() {
    input[["spatial_projection_more_render_request"]]
  }
)

registerGroupFiltersInfo(
  input,
  "spatial_projection",
  title = "Group filters for projection",
  text = HTML(
    "Choose the group levels to display. Cells must pass every grouping filter to appear in the projection."
  )
)
