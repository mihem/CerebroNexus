##----------------------------------------------------------------------------##
## Update projection plot when expression_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
expression_projection_started <- reactiveVal(FALSE)
observeEvent(
  input[["expression_projection_render_request"]],
  {
    expression_projection_started(TRUE)
  },
  ignoreInit = TRUE
)

observe({
  req(expression_projection_started())
  data <- expression_projection_data_to_plot()
  req(data)
  expression_projection_parameters_other[['reset_axes']] <- FALSE
  expression_projection_update_plot(data)
})
