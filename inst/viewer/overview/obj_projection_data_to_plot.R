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
    hover_columns = if (isTRUE(plot_parameters[["hover_info"]])) {
      cerebroProjectionHoverColumns(cells_df)
    } else {
      list()
    }
  )
})

overview_projection_data_to_plot <- debounceAfterFirst(
  overview_projection_data_to_plot_raw,
  150
)
