##----------------------------------------------------------------------------##
## Update projection plot when overview_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##
overview_projection_last_render <- NULL
overview_projection_last_feedback_token <- NULL
## A first frame that was sent before the browser asked for it (below): its
## dataset context and when it was sent, until the browser's request comes.
overview_projection_unrequested <- NULL

## Sends the frame of `data` for `render_request`, or only the feedback for
## the controls when it is the frame sent last. TRUE when a frame was sent.
overview_projection_send <- function(render_request, data, current_context) {
  projection_resource_failed <- input[[
    "overview_projection_projection_resource_failed"
  ]]
  ## reset_axes is cleared after each frame; a later evaluation of the same
  ## data (a control binding with its current value) must not resend it.
  render <- list(
    request = render_request,
    projection_resource_failed = projection_resource_failed,
    data = data[setdiff(names(data), c("feedback", "reset_axes"))]
  )
  ## "% of cells" and the group filters the browser applies change no frame;
  ## their values reach R after the frame was drawn. Re-run on them, so the
  ## drawn frame acknowledges them.
  input[["overview_projection_percentage_cells_to_show"]]
  overview_projection_group_filters()
  if (identical(render, overview_projection_last_render)) {
    cerebroSendViewFeedback(
      "overview_projection",
      overview_projection_last_feedback_token,
      current_context,
      cerebroFeedbackControls("overview_projection", input)
    )
    return(invisible(FALSE))
  }
  overview_projection_last_render <<- render
  data$projection_resource_failed <- projection_resource_failed
  overview_projection_last_feedback_token <<- overview_projection_update_plot(
    data
  )
  overview_projection_parameters_other[["reset_axes"]] <- FALSE
  invisible(TRUE)
}

## The browser asks for its first frame as soon as it hears that the data set
## is loading, before that frame can have reached it. When the frame was sent
## without waiting for the request (below), the request is the one that frame
## answers: it becomes the request of the frame sent last, and the frame is not
## sent again for it. Only the first request after that frame, within seconds
## of it, for its data set, on the first connection, and not after the cell
## names file failed: the browser asks again then, and that request is drawn.
overview_projection_adopt_seconds <- 5
overview_projection_adopt_request <- function(render_request) {
  unrequested <- overview_projection_unrequested
  if (is.null(unrequested)) {
    return(invisible(FALSE))
  }
  overview_projection_unrequested <<- NULL
  last <- overview_projection_last_render
  sync <- isolate(input[["viewer_dataset_context_sync"]])
  connection <- suppressWarnings(as.numeric(
    if (is.list(sync)) sync$nonce %||% 1 else 1
  ))
  adopt <- is.list(last) &&
    is.null(last$request) &&
    viewerDatasetContextEqual(
      render_request$dataset_context,
      unrequested$dataset_context
    ) &&
    isTRUE(
      difftime(Sys.time(), unrequested$sent, units = "secs") <=
        overview_projection_adopt_seconds
    ) &&
    is.null(isolate(input[["cell_view_names_resource_failed"]])) &&
    length(connection) == 1L &&
    isTRUE(connection <= 1)
  if (!adopt) {
    return(invisible(FALSE))
  }
  last["request"] <- list(render_request)
  overview_projection_last_render <<- last
  invisible(TRUE)
}

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
  data <- overview_projection_data_to_plot()
  if (!viewerDatasetContextEqual(data$dataset_context, current_context)) {
    data <- overview_projection_data_to_plot_raw()
  }
  req(data, viewerDatasetContextEqual(data$dataset_context, current_context))
  overview_projection_adopt_request(render_request)
  overview_projection_send(render_request, data, current_context)
})

## The page's first frame, with its default parameters (the values its
## controls report when they bind), as soon as the data set is in. This code
## is loaded when the page is first shown (shiny_server.R); for a session that
## starts on it, that is in the session's first cycle. The browser's request
## for the frame is read only after that cycle, which makes the controls of the
## page and the rest of the session first; with a priority above theirs this
## frame leaves within the cycle, right after the data set has been made.
##
## Only while no frame was sent and no request has come, and only while the
## page is shown: otherwise, and once it has sent its frame, it stops for good,
## and the observer above draws every frame. Not with Linked views' shared
## projection, a frame the browser may have to ask for again.
overview_projection_first_frame <- observe(
  {
    if (
      !identical(input[["sidebar"]], "overview") ||
        !is.null(input[["overview_projection_render_request"]]) ||
        !is.null(overview_projection_last_render) ||
        length(isolate(input[["coordviews_shared_base"]]))
    ) {
      overview_projection_first_frame$destroy()
      return()
    }
    current_context <- viewer_loaded_dataset_context()
    data <- overview_projection_data_to_plot()
    if (!viewerDatasetContextEqual(data$dataset_context, current_context)) {
      data <- overview_projection_data_to_plot_raw()
    }
    req(data, viewerDatasetContextEqual(data$dataset_context, current_context))
    sent <- isolate(overview_projection_send(NULL, data, current_context))
    if (isTRUE(sent)) {
      overview_projection_unrequested <<- list(
        dataset_context = current_context,
        sent = Sys.time()
      )
    }
    overview_projection_first_frame$destroy()
  },
  priority = 900
)
