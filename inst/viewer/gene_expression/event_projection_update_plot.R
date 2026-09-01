##----------------------------------------------------------------------------##
## Update projection plot when expression_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
observeEvent(
  list(
    expression_projection_data_to_plot(),
    input[["expression_projection_render_request"]]
  ),
  {
    data <- expression_projection_data_to_plot()
    req(data)
    expression_projection_parameters_other[['reset_axes']] <- FALSE
    expression_projection_update_plot(data)
  }
)
