##----------------------------------------------------------------------------##
## Update projection plot when spatial_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##

spatial_projection_render_context <- reactiveVal(NULL)
spatial_projection_render_request_seen <- reactiveVal(NULL)
spatial_projection_last_render <- NULL

observe({
  render_request <- input[["spatial_projection_render_request"]]
  req(is.list(render_request), is.list(render_request$dataset_context))
  current_context <- viewer_loaded_dataset_context()
  req(
    viewerDatasetContextEqual(
      render_request$dataset_context,
      current_context
    )
  )
  dataset_changed <- !identical(
    viewerDatasetContextToken(current_context),
    isolate(spatial_projection_render_context())
  )
  request_nonce <- render_request$nonce %||% render_request$request_id
  render_requested <- !identical(
    request_nonce,
    isolate(spatial_projection_render_request_seen())
  )
  # Interactive controls remain debounced, but a dataset switch must bypass
  # the previous dataset's delayed value. A newly mounted Canvas also needs a
  # fresh payload even if the hidden-page observer already saw this dataset.
  # Calling the raw reactive keeps this observer invalidated while replacement
  # renderUI inputs settle.
  data <- if (dataset_changed || render_requested) {
    spatial_projection_data_to_plot_raw()
  } else {
    spatial_projection_data_to_plot()
  }
  req(
    data,
    viewerDatasetContextEqual(data$dataset_context, current_context)
  )
  ## The raw value read for a new request keeps this observer subscribed to
  ## it; later re-evaluations with an unchanged frame must not resend it.
  if (
    !dataset_changed &&
      !render_requested &&
      identical(data, spatial_projection_last_render)
  ) {
    return()
  }
  spatial_projection_last_render <<- data

  if (dataset_changed || render_requested) {
    withProgress(message = 'Updating spatial plot...', value = 0.5, {
      isolate(spatial_projection_update_plot(data))
    })
  } else {
    isolate(spatial_projection_update_plot(data))
  }
  spatial_projection_render_context(viewerDatasetContextToken(current_context))
  spatial_projection_render_request_seen(request_nonce)
})
