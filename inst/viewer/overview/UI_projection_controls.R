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

registerCellViewPointBorder(output, "overview_projection", function() {
  current_page_display_defaults("projection")
})

##----------------------------------------------------------------------------##
## UI elements with switch to show group labels in projection.
##----------------------------------------------------------------------------##
output[["overview_projection_show_group_label_UI"]] <- renderUI({
  req(input[["overview_projection_point_color"]])
  if (input[["overview_projection_point_color"]] %in% getGroups()) {
    checkboxInput(
      inputId = "overview_projection_group_labels",
      label = "Group labels",
      value = current_page_display_defaults("projection")$group_labels
    )
  }
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_show_group_label_UI",
  suspendWhenHidden = FALSE
)
