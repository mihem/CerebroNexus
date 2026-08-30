##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
overview_projection_data_to_plot_raw <- reactive({
  req(
    overview_projection_data(),
    overview_projection_coordinates(),
    overview_projection_parameters_plot(),
    reactive_colors(),
    overview_projection_hover_info(),
    nrow(overview_projection_data()) ==
      length(overview_projection_hover_info()) ||
      overview_projection_hover_info() == "none"
  )
  cells_df <- overview_projection_data()
  plot_parameters <- overview_projection_parameters_plot()
  color_variable <- plot_parameters[['color_variable']]
  if (is.numeric(cells_df[[color_variable]])) {
    color_assignments <- NULL
  } else {
    color_assignments <- assignColorsToGroups(cells_df, color_variable)
  }

  list(
    cells_df = cells_df,
    coordinates = overview_projection_coordinates(),
    reset_axes = isolate(overview_projection_parameters_other[['reset_axes']]),
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    hover_info = overview_projection_hover_info()
  )
})

overview_projection_data_to_plot <- debounce(
  overview_projection_data_to_plot_raw,
  150
)
