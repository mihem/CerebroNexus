##----------------------------------------------------------------------------##
## UI elements to select X and Y limits in projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_scales_UI"]] <- renderUI({
  if (
    is.null(input[["spatial_projection_to_display"]]) ||
    is.na(input[["spatial_projection_to_display"]]) ||
    input[["spatial_projection_to_display"]] %in% availableProjections() == FALSE
  ) {
    projection_to_display <- availableProjections()[1]
  } else {
    projection_to_display <- input[["spatial_projection_to_display"]]
  }
  ##
  XYranges <- getXYranges(getProjection(projection_to_display))
  ##
  tagList(
    sliderInput(
      "spatial_projection_scale_x_manual_range",
      label = "Range of X axis",
      min = XYranges$x$min,
      max = XYranges$x$max,
      value = c(XYranges$x$min, XYranges$x$max)
    ),
    sliderInput(
      "spatial_projection_scale_y_manual_range",
      label = "Range of Y axis",
      min = XYranges$y$min,
      max = XYranges$y$max,
      value = c(XYranges$y$min, XYranges$y$max)
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "spatial_projection_scales_UI",
  suspendWhenHidden = FALSE
)
