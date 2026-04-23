##----------------------------------------------------------------------------##
## Color assignments.
##----------------------------------------------------------------------------##
spatial_projection_color_assignments <- reactive({
  req(
    spatial_projection_metadata(),
    spatial_projection_parameters_plot()
  )
  # message('--> trigger "spatial_projection_color_assignments"')
  colors <- assignColorsToGroups(
    spatial_projection_metadata(),
    spatial_projection_parameters_plot()[['color_variable']]
  )
  # message(str(colors))
  return(colors)
})
