##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
overview_projection_rendered <- reactiveVal(FALSE)

observe({
  req(input[["overview_projection_render_request"]])
  first_render <- !isolate(overview_projection_rendered())
  data <- if (first_render) {
    overview_projection_data_to_plot_raw()
  } else {
    overview_projection_data_to_plot()
  }
  req(data)
  overview_projection_update_plot(data)
  overview_projection_rendered(TRUE)
})
