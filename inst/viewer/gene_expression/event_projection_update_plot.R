##----------------------------------------------------------------------------##
## Update projection plot when expression_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
observe({
  render_request <- input[["expression_projection_render_request"]]
  req(is.list(render_request), is.list(render_request$dataset_context))
  current_context <- viewer_loaded_dataset_context()
  req(viewerDatasetContextEqual(
    render_request$dataset_context,
    current_context
  ))
  data <- expression_projection_data_to_plot()
  if (!viewerDatasetContextEqual(data$dataset_context, current_context)) {
    data <- expression_projection_data_to_plot_raw()
  }
  req(
    data,
    identical(data$dataset, data_set()),
    viewerDatasetContextEqual(data$dataset_context, current_context)
  )
  expression_projection_parameters_other[['reset_axes']] <- FALSE
  ## Payload construction consults browser-published transport/cache state.
  ## Those reads are snapshots for this render, not plot dependencies: the
  ## browser republishes shared geometry after every paint, and subscribing to
  ## it here creates an endless render/publish feedback loop.
  expressionProjectionProgressUpdate(0.72, "Transferring expression colours...")
  tryCatch(
    isolate(expression_projection_update_plot(data)),
    error = function(error) {
      expressionProjectionProgressClose()
      stop(error)
    }
  )
  expressionProjectionProgressUpdate(0.9, "Drawing expression colours...")
})
