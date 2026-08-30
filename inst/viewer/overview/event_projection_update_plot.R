##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
observeEvent(
  list(
    overview_projection_data_to_plot(),
    input[["overview_projection_render_request"]]
  ),
  {
    data <- overview_projection_data_to_plot()
    req(data)
    overview_projection_update_plot(data)
  }
)
