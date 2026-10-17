##----------------------------------------------------------------------------##
## UI elements with switch to show group labels in projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_show_group_label_UI"]] <- renderUI({
  req(input[["spatial_projection_more_render_request"]])
  if (!identical(input[["spatial_projection_plot_type"]], "ImageDimPlot")) {
    return(NULL)
  }
  req(input[["spatial_projection_point_color"]])
  if (input[["spatial_projection_point_color"]] %in% getGroups()) {
    checkboxInput(
      inputId = "spatial_projection_group_labels",
      label = "Group labels",
      value = isolate(spatial_projection_appearance$group_labels)
    )
  }
})

observeEvent(
  input[["spatial_projection_group_labels"]],
  {
    ## This is an appearance preference for the whole Spatial page, not a
    ## property of one sample or FOV. Keep it while this session moves among
    ## samples, spatial entries, ROIs, and data sets.
    preferences[["spatial_group_labels"]] <- isTRUE(
      input[["spatial_projection_group_labels"]]
    )
  },
  ignoreNULL = TRUE,
  priority = 100
)

output[["spatial_projection_show_region_outline_UI"]] <- renderUI({
  req(input[["spatial_projection_more_render_request"]])
  if (!identical(input[["spatial_projection_plot_type"]], "ImageDimPlot")) {
    return(NULL)
  }
  req(input[["spatial_projection_point_color"]])
  if (input[["spatial_projection_point_color"]] %in% getGroups()) {
    ## Outline each group's spatial region with its convex hull, so the tissue
    ## regions read at a glance. Off by default because intermixed groups overlap.
    checkboxInput(
      inputId = "spatial_projection_show_region_outlines",
      label = "Outline group regions (convex hull)",
      value = isolate(spatial_projection_region_outlines())
    )
  }
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "spatial_projection_show_group_label_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "spatial_projection_show_region_outline_UI",
  suspendWhenHidden = FALSE
)
