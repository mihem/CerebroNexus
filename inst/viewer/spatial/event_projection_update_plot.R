##----------------------------------------------------------------------------##
## Update projection plot when spatial_projection_data_to_plot() changes.
##----------------------------------------------------------------------------##

spatial_projection_render_context <- reactiveVal(NULL)
spatial_projection_render_request_seen <- reactiveVal(NULL)
spatial_projection_last_render <- NULL
spatial_projection_last_feedback <- NULL
spatial_projection_last_token <- NULL
## Counts the whole frames asked for, and whether the last is still waiting for
## its turn (spatial_projection_defer()).
spatial_projection_send_serial <- 0L
spatial_projection_send_pending <- FALSE
## Runs `task` in a turn of the event loop of its own; a test that wants the
## frame at once defines this before sourcing the file.
if (!exists("spatial_projection_defer", inherits = FALSE)) {
  spatial_projection_defer <- function(task) later::later(task, 0)
}

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
  ## A new section is one discrete choice: draw it now rather than after the
  ## debounce that gathers slider and filter changes. So is a switch between
  ## one section and the grid.
  scene_fields <- c("projection", "grid_sections", "grid_projection")
  section_changed <- !identical(
    unname(spatial_projection_controls()[scene_fields]),
    unname(spatial_projection_last_render$plot_parameters[scene_fields])
  )
  data <- if (dataset_changed || render_requested || section_changed) {
    spatial_projection_data_to_plot_raw()
  } else {
    spatial_projection_data_to_plot()
  }
  req(
    data,
    viewerDatasetContextEqual(data$dataset_context, current_context)
  )
  ## Group filters the browser applies change no frame: re-run on them, so
  ## the drawn frame acknowledges them.
  spatial_projection_group_filters()
  feedback <- cerebroFeedbackControls("spatial_projection", input)
  ## The raw value read for a new request keeps this observer subscribed to
  ## it; later re-evaluations with an unchanged frame must not resend it.
  ## Control values that change without changing the frame (a filter level
  ## outside the section, a control binding) are acknowledged against the
  ## painted frame once the current value agrees.
  if (
    !dataset_changed &&
      !render_requested &&
      identical(data, spatial_projection_last_render)
  ) {
    current <- tryCatch(
      spatial_projection_data_to_plot_raw(),
      shiny.silent.error = function(error) NULL
    )
    if (
      !identical(feedback, spatial_projection_last_feedback) &&
        !is.null(spatial_projection_last_token) &&
        identical(current, data)
    ) {
      cerebroSendViewFeedback(
        "spatial_projection",
        spatial_projection_last_token,
        current_context,
        feedback
      )
      spatial_projection_last_feedback <<- feedback
    }
    return()
  }
  drawn <- data
  data$feedback <- feedback
  ## A data set switch or a render request draws the whole frame, never only
  ## new colours over what the browser may no longer have. So does any frame
  ## while such a frame is still waiting to be sent.
  whole_frame <- dataset_changed ||
    render_requested ||
    spatial_projection_send_pending
  data$fresh_render <- whole_frame
  ## The switch's current value: plot parameters hold the value from their
  ## last rebuild, and a filter or ROI change rebuilds only the frame.
  data$plot_parameters[["show_region_outlines"]] <- isTRUE(
    isolate(spatial_projection_region_outlines())
  )
  token_before <- .cerebro_cell_view_render_serial
  frame_sent <- function() {
    if (.cerebro_cell_view_render_serial > token_before) {
      spatial_projection_last_token <<- .cerebro_cell_view_render_serial
      spatial_projection_last_feedback <<- feedback
    }
  }

  if (whole_frame) {
    ## The frame is built and sent in a turn of the event loop of its own: the
    ## scene, the controls and the data of this flush go out first, and the
    ## other sessions of the process are answered in between (the Allen atlas
    ## held the process for 0.65 s, 0.17 s of it for the frame). A newer frame
    ## replaces one that has not been sent.
    spatial_projection_send_serial <<- spatial_projection_send_serial + 1L
    spatial_projection_send_pending <<- TRUE
    serial <- spatial_projection_send_serial
    fresh_load <- dataset_changed || render_requested
    spatial_projection_defer(function() {
      if (
        !identical(serial, spatial_projection_send_serial) ||
          isTRUE(tryCatch(session$isClosed(), error = function(error) TRUE))
      ) {
        return(invisible(NULL))
      }
      current <- tryCatch(
        isolate(viewer_loaded_dataset_context()),
        error = function(error) NULL
      )
      sent <- !is.null(current) &&
        viewerDatasetContextEqual(data$dataset_context, current) &&
        tryCatch(
          {
            shiny::withReactiveDomain(session, {
              if (fresh_load) {
                withProgress(
                  message = 'Updating spatial plot...',
                  value = 0.5,
                  {
                    isolate(spatial_projection_update_plot(data))
                  }
                )
              } else {
                isolate(spatial_projection_update_plot(data))
              }
            })
            TRUE
          },
          error = function(error) {
            message(
              "[spatial] the frame could not be sent: ",
              conditionMessage(error)
            )
            FALSE
          }
        )
      spatial_projection_send_pending <<- FALSE
      ## Recorded once sent: a frame that failed is drawn again on the next run.
      if (sent) {
        frame_sent()
      } else {
        spatial_projection_last_render <<- NULL
      }
      invisible(NULL)
    })
    ## The frame is the one the next run is compared with from here on; its
    ## turn comes before any input that the browser answers this flush with.
    spatial_projection_last_render <<- drawn
  } else {
    isolate(spatial_projection_update_plot(data))
    ## Recorded once sent: a frame that failed is drawn again on the next run.
    spatial_projection_last_render <<- drawn
    frame_sent()
  }
  spatial_projection_render_context(viewerDatasetContextToken(current_context))
  spatial_projection_render_request_seen(request_nonce)
})

## The region-outline switch patches the drawn frame's overlay; its cells and
## their GPU data stay as they are.
observeEvent(
  spatial_projection_region_outlines(),
  {
    frame <- spatial_projection_last_render
    req(!is.null(frame), identical(input[["sidebar"]], "spatial"))
    color_variable <- frame$plot_parameters[["color_variable"]]
    numeric_color <- is.data.frame(frame$cells_df) &&
      is.numeric(frame$cells_df[[color_variable]])
    hulls <- spatial_projection_output_hulls(
      spatial_projection_group_hulls(),
      frame$color_assignments,
      numeric_color || !identical(frame$plot_parameters[["n_dimensions"]], 2L),
      spatial_projection_region_outlines()
    )
    if (identical(hulls, spatial_projection_drawn_hulls)) {
      return()
    }
    identity <- viewerDatasetIdentity()
    req(identity$fingerprint)
    session$sendCustomMessage(
      "cell_view_extra",
      list(
        id = "spatial_projection",
        dataset_context = viewer_loaded_dataset_context(),
        dataset_fingerprint = identity$fingerprint,
        extra = list(group_hulls = hulls)
      )
    )
    spatial_projection_drawn_hulls <<- hulls
  },
  ignoreInit = TRUE
)
