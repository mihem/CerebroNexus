##----------------------------------------------------------------------------##
## Spatial background-image CONTROLS.
##
## Split out of obj_projection_parameters_plot.R so the background overlay's
## interactive controls live on their own, separate from the scatter-plot
## parameter collection. Auto-sourced by spatial/server.R via the obj_ prefix,
## in the same `local = TRUE` session scope, so `input`, `session`,
## `available_crb_files` and `Cerebro.options` resolve exactly as before.
##
## Contents:
##   - the decoupled APPEARANCE observer (pushes opacity/move/flip/scale/rotate
##     straight to the background <div>, never re-rendering the scatter plot),
##   - the aspect-ratio lock / single-vs-XY scale mirroring,
##   - Reset (returns to the per-dataset spatial_images_* preset),
##   - slider <-> numeric-box two-way sync.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Background image APPEARANCE — decoupled channel.
##
## opacity / move / flip / scale / rotate are pushed straight to the background
## canvas through the shared cell-view action. This does NOT go through
## spatial_projection_parameters_plot / spatial_projection_update_plot, so the
## scatter plot is never re-rendered when the user nudges the background — the
## dimensional-reduction plot stays a function of its own parameters alone.
##----------------------------------------------------------------------------##
## Session-local overrides, keyed by dataset generation and the complete image
## identity. Never assign into Cerebro.options, CRB data, or image descriptors.
spatial_background_edits <- new.env(parent = emptyenv())
spatial_background_edit_key <- function(identity) {
  as.character(jsonlite::toJSON(list(
    context = viewer_loaded_dataset_context(),
    image = identity[sort(names(identity))]
  ), auto_unbox = TRUE, null = "null"))
}
## Publish only background scope changes. Sampling, colours and point styling
## may invalidate plot parameters, but must not replace mounted image controls.
spatial_background_edit_model <- reactiveVal(NULL)
observe({
  parameters <- spatial_projection_parameters_plot()
  model <- parameters[c("roi_mode", "roi_backgrounds", "background_identity",
                         "background_descriptor", "projection")]
  if (!identical(isolate(spatial_background_edit_model()), model)) {
    spatial_background_edit_model(model)
  }
}, priority = 900)
spatial_background_edit_target <- reactive({
  parameters <- spatial_background_edit_model()
  if (identical(parameters$roi_mode, "separate")) {
    targets <- Filter(function(x) !is.null(x$descriptor), parameters$roi_backgrounds)
    if (!length(targets)) return(NULL)
    selected <- input[["spatial_projection_background_target"]]
    if (is.null(selected) || !selected %in% names(targets)) selected <- names(targets)[[1L]]
    target <- targets[[selected]]
    target$roi <- selected
    return(target)
  }
  if (is.null(parameters$background_identity)) return(NULL)
  list(identity = parameters$background_identity,
       descriptor = parameters$background_descriptor,
       preset = spatial_background_preset(Cerebro.options,
         viewer_current_dataset_key(), input[["spatial_projection_to_display"]],
         parameters$background_descriptor))
})
spatial_background_edit_preset <- function(target) {
  if (is.null(target)) return(NULL)
  edit <- spatial_background_edits[[spatial_background_edit_key(target$identity)]]
  if (is.null(edit)) return(target$preset)
  edit$rotation <- edit$rotate
  edit$rotate <- NULL
  utils::modifyList(target$preset, edit)
}

observeEvent(
  input[["spatial_projection_background_controls"]],
  {
    payload <- input[["spatial_projection_background_controls"]]
    dataset_context <- viewer_loaded_dataset_context()
    req(
      is.list(payload),
      viewerDatasetContextEqual(
        payload[["dataset_context"]],
        dataset_context
      )
    )
    background_identity <- spatial_background_edit_target()$identity
    req(
      !is.null(background_identity),
      spatial_background_identity_equal(
        payload[["background_identity"]],
        background_identity
      ),
      is.list(payload[["values"]])
    )
    values <- payload[["values"]]
    finite_value <- function(name) {
      value <- values[[name]]
      if (
        length(value) == 1L &&
          is.numeric(value) &&
          !is.na(value) &&
          is.finite(value)
      ) {
        return(unname(as.numeric(value)))
      }
      NULL
    }
    logical_value <- function(name) {
      value <- values[[name]]
      if (is.logical(value) && length(value) == 1L && !is.na(value)) {
        return(isTRUE(value))
      }
      NULL
    }

    clean_values <- list(
        opacity = finite_value("opacity"),
        offsetX = finite_value("offsetX"),
        offsetY = finite_value("offsetY"),
        flipX = logical_value("flipX"),
        flipY = logical_value("flipY"),
        scaleX = finite_value("scaleX"),
        scaleY = finite_value("scaleY"),
        rotate = finite_value("rotate")
      )
    clean_values <- Filter(Negate(is.null), clean_values)
    spatial_background_edits[[spatial_background_edit_key(background_identity)]] <- clean_values
    cerebroCellViewBackground(
      id = "spatial_projection",
      values = clean_values,
      dataset_context = dataset_context,
      background_identity = background_identity
    )
  },
  ignoreInit = TRUE
)

##----------------------------------------------------------------------------##
## While locked, the single Scale slider drives both axes. Mirror its value into
## the hidden X/Y sliders so that unlocking later starts from the same scale
## instead of jumping back to whatever the X/Y sliders last held.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_background_scale"]], {
  if (!isTRUE(input[["spatial_projection_background_scale_lock"]])) {
    return()
  }
  v <- input[["spatial_projection_background_scale"]]
  if (is.null(v) || !is.finite(v)) {
    return()
  }
  if (!isTRUE(isolate(input[["spatial_projection_background_scale_x"]]) == v)) {
    updateSliderInput(
      session,
      "spatial_projection_background_scale_x",
      value = v
    )
  }
  if (!isTRUE(isolate(input[["spatial_projection_background_scale_y"]]) == v)) {
    updateSliderInput(
      session,
      "spatial_projection_background_scale_y",
      value = v
    )
  }
})

##----------------------------------------------------------------------------##
## Reset every background-image adjustment to the shared preset.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_background_reset"]], {
  target <- spatial_background_edit_target()
  req(target)
  preset <- target$preset
  values <- preset
  values$rotate <- values$rotation
  values$rotation <- NULL
  spatial_background_edits[[spatial_background_edit_key(target$identity)]] <- values
  cerebroCellViewBackground(id = "spatial_projection", values = values,
    dataset_context = viewer_loaded_dataset_context(), background_identity = target$identity)
  updateSliderInput(
    session,
    "spatial_projection_background_opacity",
    value = preset$opacity
  )
  updateSliderInput(
    session,
    "spatial_projection_background_offset_x",
    value = preset$offsetX
  )
  updateSliderInput(
    session,
    "spatial_projection_background_offset_y",
    value = preset$offsetY
  )
  ## Move, flip and scale all reset to their preset (the shipped alignment),
  ## matching how the UI seeds them, so Reset restores the aligned overlay rather
  ## than a bare image. Scale is single-source now, so it too returns to preset.
  scale_x_reset <- preset$scaleX
  scale_y_reset <- preset$scaleY
  updateCheckboxInput(
    session,
    "spatial_projection_background_scale_lock",
    value = isTRUE(all.equal(scale_x_reset, scale_y_reset) == TRUE)
  )
  updateSliderInput(
    session,
    "spatial_projection_background_scale",
    value = scale_x_reset
  )
  updateSliderInput(
    session,
    "spatial_projection_background_scale_x",
    value = scale_x_reset
  )
  updateSliderInput(
    session,
    "spatial_projection_background_scale_y",
    value = scale_y_reset
  )
  updateSliderInput(
    session,
    "spatial_projection_background_rotate",
    value = preset$rotation
  )
  updateCheckboxInput(
    session,
    "spatial_projection_background_flip_x",
    value = preset$flipX
  )
  updateCheckboxInput(
    session,
    "spatial_projection_background_flip_y",
    value = preset$flipY
  )
})

##----------------------------------------------------------------------------##
## Two-way sync between each authoritative slider and its exact numeric box.
## Each direction updates the OTHER control, guarded by an equality check so the
## two observers cannot ping-pong into an infinite loop.
##----------------------------------------------------------------------------##
local({
  sync_slider_numeric <- function(slider_id, numeric_id) {
    ## slider -> numeric
    observeEvent(input[[slider_id]], {
      new_val <- input[[slider_id]]
      if (
        is.null(new_val) ||
          !is.finite(new_val) ||
          isTRUE(isolate(input[[numeric_id]]) == new_val)
      ) {
        return()
      }
      updateNumericInput(session, numeric_id, value = new_val)
    })
    ## numeric -> slider
    observeEvent(input[[numeric_id]], {
      new_val <- input[[numeric_id]]
      if (
        is.null(new_val) ||
          !is.finite(new_val) ||
          isTRUE(isolate(input[[slider_id]]) == new_val)
      ) {
        return()
      }
      updateSliderInput(session, slider_id, value = new_val)
    })
  }
  sync_slider_numeric(
    "spatial_projection_background_opacity",
    "spatial_projection_background_opacity_num"
  )
  sync_slider_numeric(
    "spatial_projection_background_offset_x",
    "spatial_projection_background_offset_x_num"
  )
  sync_slider_numeric(
    "spatial_projection_background_offset_y",
    "spatial_projection_background_offset_y_num"
  )
  sync_slider_numeric(
    "spatial_projection_background_scale",
    "spatial_projection_background_scale_num"
  )
  sync_slider_numeric(
    "spatial_projection_background_scale_x",
    "spatial_projection_background_scale_x_num"
  )
  sync_slider_numeric(
    "spatial_projection_background_scale_y",
    "spatial_projection_background_scale_y_num"
  )
  sync_slider_numeric(
    "spatial_projection_background_rotate",
    "spatial_projection_background_rotate_num"
  )
})

##----------------------------------------------------------------------------##
## Copy alignment as preset.
##
## Reads the exact current dataset / spatial / image identity and renders one
## canonical spatial_image_settings leaf. An empty selection has no image
## identity, so it deliberately produces no snippet.
##----------------------------------------------------------------------------##
spatial_preset_code <- reactiveVal(NULL)

observeEvent(
  list(
    viewer_loaded_dataset_context(),
    spatial_background_edit_target()$identity
  ),
  spatial_preset_code(NULL),
  ignoreInit = TRUE
)

observeEvent(input[["spatial_projection_background_copy_preset"]], {
  dataset_context <- viewer_loaded_dataset_context()
  spatial_name <- input[["spatial_projection_to_display"]]
  dataset_context <- viewer_loaded_dataset_context()
  dataset <- dataset_context$dataset_key
  target <- spatial_background_edit_target()
  descriptor <- target$descriptor
  background_identity <- target$identity
  if (is.null(background_identity)) {
    spatial_preset_code(NULL)
    return()
  }

  locked <- isTRUE(input[["spatial_projection_background_scale_lock"]])
  if (locked) {
    scale_x <- input[["spatial_projection_background_scale"]]
    scale_y <- scale_x
  } else {
    scale_x <- input[["spatial_projection_background_scale_x"]]
    scale_y <- input[["spatial_projection_background_scale_y"]]
  }

  null_to <- function(value, default) {
    if (is.null(value) || !is.finite(value)) default else value
  }

  code <- format_spatial_preset_code(
    dataset = dataset,
    spatial_name = spatial_name,
    image_label = descriptor$key %||% descriptor$label,
    offset_x = null_to(input[["spatial_projection_background_offset_x"]], 0),
    offset_y = null_to(input[["spatial_projection_background_offset_y"]], 0),
    scale_x = null_to(scale_x, 1),
    scale_y = null_to(scale_y, 1),
    flip_x = isTRUE(input[["spatial_projection_background_flip_x"]]),
    flip_y = isTRUE(input[["spatial_projection_background_flip_y"]]),
    rotation = null_to(input[["spatial_projection_background_rotate"]], 0),
    image_opacity = null_to(
      input[["spatial_projection_background_opacity"]],
      0.5
    )
  )
  spatial_preset_code(code)
  session$sendCustomMessage(
    "spatial_copy_preset",
    list(
      text = code,
      dataset_context = dataset_context,
      background_identity = background_identity
    )
  )
})

output[["spatial_projection_background_preset_code"]] <- renderText({
  req(spatial_preset_code())
  spatial_preset_code()
})
## The code box lives inside a collapsible box + conditionalPanel; without this
## it stays suspended (blank) until the box happens to be open on first render.
outputOptions(
  output,
  "spatial_projection_background_preset_code",
  suspendWhenHidden = FALSE
)

## Drives the conditionalPanel that reveals the code box only after the button
## has produced a snippet.
output[["spatial_projection_background_preset_code_present"]] <- reactive({
  !is.null(spatial_preset_code())
})
outputOptions(
  output,
  "spatial_projection_background_preset_code_present",
  suspendWhenHidden = FALSE
)
