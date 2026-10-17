##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
overview_projection_last_render <- NULL
overview_projection_last_feedback_token <- NULL
observe({
  req(identical(input[["sidebar"]], "overview"))
  render_request <- input[["overview_projection_render_request"]]
  req(is.list(render_request), is.list(render_request$dataset_context))
  current_context <- viewer_loaded_dataset_context()
  req(viewerDatasetContextEqual(
    render_request$dataset_context,
    current_context
  ))
  render_request <- input[["overview_projection_render_request"]]
  projection_resource_failed <- input[[
    "overview_projection_projection_resource_failed"
  ]]
  data <- overview_projection_data_to_plot()
  if (!viewerDatasetContextEqual(data$dataset_context, current_context)) {
    data <- overview_projection_data_to_plot_raw()
  }
  req(data, viewerDatasetContextEqual(data$dataset_context, current_context))
  render <- list(
    request = render_request,
    projection_resource_failed = projection_resource_failed,
    data = data[setdiff(names(data), "feedback")]
  )
  if (identical(render, overview_projection_last_render)) {
    cerebroSendViewFeedback(
      "overview_projection",
      overview_projection_last_feedback_token,
      current_context,
      data$feedback
    )
    return()
  }
  overview_projection_last_render <<- render
  data$projection_resource_failed <- projection_resource_failed
  overview_projection_last_feedback_token <<- overview_projection_update_plot(
    data
  )
  overview_projection_parameters_other[["reset_axes"]] <- FALSE
})
