##----------------------------------------------------------------------------##
## UI elements to set color scale.
##----------------------------------------------------------------------------##
output[["expression_projection_color_scale_UI"]] <- renderUI({
  selectInput(
    "expression_projection_color_scale",
    label = "Color scale",
    choices = c(
      "Cerebro orange",
      "YlGnBu",
      "YlOrRd",
      "Blues",
      "Greens",
      "Reds",
      "RdBu",
      "Viridis"
    ),
    selected = "Cerebro orange"
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "expression_projection_color_scale_UI",
  suspendWhenHidden = FALSE
)
