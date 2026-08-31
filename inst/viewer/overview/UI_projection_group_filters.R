##----------------------------------------------------------------------------##
## UI elements to set group filters for the projection.
##----------------------------------------------------------------------------##
registerGroupFiltersUI(
  output,
  "overview_projection",
  getGroups = getGroups,
  getGroupLevels = getGroupLevels
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
registerGroupFiltersInfo(
  input,
  "overview_projection",
  title = "Group filters for projection",
  text = HTML(
    "
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection.
    "
  )
)

##----------------------------------------------------------------------------##
## example for implementation of nested checkboxes with shinyTree for selection
## of group levels to show; works similar to cellxgene; anyway decided against
## it because it creates a new dependency and isn't as aesthetically pleasing as
## the existing solution
##----------------------------------------------------------------------------##
