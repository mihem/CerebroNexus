##----------------------------------------------------------------------------##
## Update projection plot when spatial_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##

observeEvent(
  list(
    spatial_projection_data_to_plot(),
    input[["spatial_projection_render_request"]]
  ),
  {
    data <- spatial_projection_data_to_plot()
    req(data)

    withProgress(message = 'Updating spatial plot...', value = 0.5, {
      spatial_projection_update_plot(data)
    })
  }
)
