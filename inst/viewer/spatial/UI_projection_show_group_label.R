##----------------------------------------------------------------------------##
## UI elements with switch to show group labels in projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_show_group_label_UI"]] <- renderUI({
  if (!identical(input[["spatial_projection_plot_type"]], "ImageDimPlot")) {
    return(NULL)
  }
  req(input[["spatial_projection_point_color"]])
  if (input[["spatial_projection_point_color"]] %in% getGroups()) {
    tagList(
      checkboxInput(
        inputId = "spatial_projection_group_labels",
        label = "Group labels",
        value = TRUE
      ),
      ## Outline each group's spatial region with its convex hull, so the tissue
      ## regions read at a glance. Off by default — hulls overlap heavily when
      ## groups are spatially intermixed, so it's opt-in.
      checkboxInput(
        inputId = "spatial_projection_show_region_outlines",
        label = "Outline group regions (convex hull)",
        value = FALSE
      )
    )
  }
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "spatial_projection_show_group_label_UI",
  suspendWhenHidden = FALSE
)
