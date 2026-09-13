##----------------------------------------------------------------------------##
## Update projection plot when spatial_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##

spatial_projection_started <- reactiveVal(FALSE)
observeEvent(
  input[["spatial_projection_render_request"]],
  {
    spatial_projection_started(TRUE)
  },
  ignoreInit = TRUE
)

observe({
  req(spatial_projection_started())
  data <- spatial_projection_data_to_plot()
  req(data)

  withProgress(message = 'Updating spatial plot...', value = 0.5, {
    spatial_projection_update_plot(data)
  })
})
