##----------------------------------------------------------------------------##
## UI elements to set main parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_main_parameters_UI"]] <- renderUI({
  defaults <- viewerProjectionDefaults(
    viewerProjectionFirstFrameMetadata(),
    availableProjections(),
    tryCatch(getParameters(), error = function(e) list())
  )
  colour_groups <- viewerColourGroupChoices()
  tagList(
    selectInput(
      "overview_projection_to_display",
      label = "Projection",
      choices = availableProjections(),
      selected = defaults$projection
    ),
    selectInput(
      "overview_projection_point_color",
      label = "Colour by",
      choices = colour_groups$choices,
      selected = colour_groups$selected
    )
  )
})
