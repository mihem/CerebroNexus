##----------------------------------------------------------------------------##
## UI elements with switch to draw border around cells.
##----------------------------------------------------------------------------##
output[["spatial_projection_point_border_UI"]] <- renderUI({
  shinyWidgets::awesomeCheckbox(
    inputId = "spatial_projection_point_border",
    label = "Draw border around cells",
    value = FALSE
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "spatial_projection_point_border_UI",
  suspendWhenHidden = FALSE
)
