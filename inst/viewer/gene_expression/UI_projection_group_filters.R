##----------------------------------------------------------------------------##
## UI elements to set group filters.
##----------------------------------------------------------------------------##
registerGroupFiltersUI(
  output,
  "expression_projection",
  getGroups = getGroups,
  getGroupLevels = getGroupLevels
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
registerGroupFiltersInfo(
  input,
  "expression_projection",
  title = "Group filters for gene (set) expression",
  text = HTML(
    "
    The elements in this panel allow you to select which cells should be plotted based on the group(s) they belong to. For each grouping variable, you can activate or deactivate group levels. Only cells that are pass all filters (for each grouping variable) are shown in the projection, the expression by group, and expression by pseudotime (if applicable).
    "
  )
)
