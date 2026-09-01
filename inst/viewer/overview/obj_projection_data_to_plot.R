##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
overview_projection_data_to_plot_raw <- reactive({
  req(
    overview_projection_parameters_plot(),
    reactive_colors()
  )
  cells_df <- overview_projection_data()
  coordinates <- overview_projection_coordinates()
  hover_info <- overview_projection_hover_info()
  req(
    nrow(cells_df) == 0L ||
      nrow(cells_df) == length(hover_info) ||
      hover_info == "none"
  )
  plot_parameters <- overview_projection_parameters_plot()
  color_variable <- plot_parameters[['color_variable']]
  if (nrow(cells_df) == 0L) {
    color_assignments <- character(0)
  } else if (is.numeric(cells_df[[color_variable]])) {
    color_assignments <- NULL
  } else {
    color_assignments <- assignColorsToGroups(cells_df, color_variable)
  }

  list(
    cells_df = cells_df,
    coordinates = coordinates,
    reset_axes = isolate(overview_projection_parameters_other[['reset_axes']]),
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    hover_info = hover_info
  )
})

overview_projection_data_to_plot <- debounce(
  overview_projection_data_to_plot_raw,
  150
)
