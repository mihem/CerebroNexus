##----------------------------------------------------------------------------##
## Update projection plot when spatial_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##

observeEvent(spatial_projection_data_to_plot(), {
  req(spatial_projection_data_to_plot())

  data <- spatial_projection_data_to_plot()
  spatial_projection_update_plot(data)
})
