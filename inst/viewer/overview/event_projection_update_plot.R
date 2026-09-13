##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
overview_projection_started <- reactiveVal(FALSE)
overview_projection_rendered <- reactiveVal(FALSE)
observeEvent(
  input[["overview_projection_render_request"]],
  {
    overview_projection_started(TRUE)
  },
  ignoreInit = TRUE
)

observe({
  req(overview_projection_started())
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
