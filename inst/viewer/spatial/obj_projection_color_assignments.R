##----------------------------------------------------------------------------##
## Color assignments.
##----------------------------------------------------------------------------##
spatial_projection_color_assignments <- reactive({
  req(
    spatial_projection_metadata(),
    spatial_projection_parameters_plot()
  )
  colors <- assignColorsToGroups(
    spatial_projection_metadata(),
    spatial_projection_parameters_plot()[['color_variable']]
  )
  return(colors)
})
