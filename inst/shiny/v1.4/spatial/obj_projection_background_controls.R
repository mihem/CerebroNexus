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
##   - the Move slider <-> numeric-box two-way sync.
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## Background image APPEARANCE — decoupled channel.
##
## opacity / move / flip / scale / rotate are pushed straight to the background
## <div> via shinyjs.updateSpatialBackgroundAppearance. This does NOT go through
## spatial_projection_parameters_plot / spatial_projection_update_plot, so the
## scatter plot is never re-rendered when the user nudges the background — the
## dimensional-reduction plot stays a function of its own parameters alone.
##----------------------------------------------------------------------------##
observe({
  ## Depend on each appearance control. These are the ONLY inputs that reach the
  ## background div directly; everything else about the plot is untouched.
  opacity <- input[["spatial_projection_background_opacity"]]
  offset_x <- input[["spatial_projection_background_offset_x"]]
  offset_y <- input[["spatial_projection_background_offset_y"]]
  flip_x <- input[["spatial_projection_background_flip_x"]]
  flip_y <- input[["spatial_projection_background_flip_y"]]
  rotate <- input[["spatial_projection_background_rotate"]]

  ## Resolve X/Y scale from the lock state: locked -> the single slider drives
  ## both axes; unlocked -> the independent X/Y sliders. Whichever sliders are
  ## hidden may report NULL, so fall back to the visible source.
  locked <- isTRUE(input[["spatial_projection_background_scale_lock"]])
  if (locked) {
    scale_x <- input[["spatial_projection_background_scale"]]
    scale_y <- scale_x
  } else {
    scale_x <- input[["spatial_projection_background_scale_x"]]
    scale_y <- input[["spatial_projection_background_scale_y"]]
  }

  ## Pass NULL for any control that has not been created yet (e.g. before an
  ## image is chosen); the JS side leaves the corresponding style unchanged.
  ## Named arguments — shinyjs packs them into one `params` object that the JS
  ## side unpacks via getParams (positional formals would NOT be spread).
  shinyjs::js$updateSpatialBackgroundAppearance(
    opacity = if (is.null(opacity)) NULL else opacity,
    offsetX = if (is.null(offset_x)) NULL else offset_x,
    offsetY = if (is.null(offset_y)) NULL else offset_y,
    flipX = if (is.null(flip_x)) NULL else isTRUE(flip_x),
    flipY = if (is.null(flip_y)) NULL else isTRUE(flip_y),
    scaleX = if (is.null(scale_x)) NULL else scale_x,
    scaleY = if (is.null(scale_y)) NULL else scale_y,
    rotate = if (is.null(rotate)) NULL else rotate
  )
})

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
## Locking the aspect ratio while X and Y scales differ has no single sensible
## value to collapse to, so it resets scale to 1 (both the locked slider and the
## X/Y sliders). When X and Y already match, locking keeps that shared value.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_background_scale_lock"]], {
  if (!isTRUE(input[["spatial_projection_background_scale_lock"]])) {
    return()
  }
  sx <- input[["spatial_projection_background_scale_x"]]
  sy <- input[["spatial_projection_background_scale_y"]]
  if (
    is.null(sx) ||
      is.null(sy) ||
      !is.finite(sx) ||
      !is.finite(sy) ||
      isTRUE(all.equal(sx, sy) == TRUE)
  ) {
    ## already equal (or not yet initialised) — keep the shared value
    if (!is.null(sx) && is.finite(sx)) {
      updateSliderInput(
        session,
        "spatial_projection_background_scale",
        value = sx
      )
    }
    return()
  }
  updateSliderInput(session, "spatial_projection_background_scale", value = 1)
  updateSliderInput(session, "spatial_projection_background_scale_x", value = 1)
  updateSliderInput(session, "spatial_projection_background_scale_y", value = 1)
})

##----------------------------------------------------------------------------##
## Reset the background-image adjustments back to identity.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_background_reset"]], {
  ## Reset returns to the app-configured default for the current dataset (the
  ## `spatial_images_*` presets), not a hard identity value — otherwise resetting
  ## a pre-aligned overlay would knock it out of alignment. Falls back to the
  ## identity (0 move, 1 scale, FALSE flip) when no preset is set. Same
  ## per-dataset name lookup as the UI seeds it with.
  reset_preset_default <- function(option_name, fallback) {
    if (
      !exists("Cerebro.options") ||
        is.null(Cerebro.options[[option_name]]) ||
        !exists("available_crb_files") ||
        is.null(available_crb_files$selected)
    ) {
      return(fallback)
    }
    idx <- which(available_crb_files$files == available_crb_files$selected)
    if (length(idx) == 0) {
      return(fallback)
    }
    current_name <- names(available_crb_files$files)[idx[1]]
    if (
      is.null(current_name) ||
        !(current_name %in% names(Cerebro.options[[option_name]]))
    ) {
      return(fallback)
    }
    val <- Cerebro.options[[option_name]][[current_name]]
    if (is.null(val) || length(val) != 1 || is.na(val)) fallback else val
  }
  updateSliderInput(
    session,
    "spatial_projection_background_offset_x",
    value = reset_preset_default("spatial_images_offset_x", 0)
  )
  updateSliderInput(
    session,
    "spatial_projection_background_offset_y",
    value = reset_preset_default("spatial_images_offset_y", 0)
  )
  ## Move, flip and scale all reset to their preset (the shipped alignment),
  ## matching how the UI seeds them, so Reset restores the aligned overlay rather
  ## than a bare image. Scale is single-source now, so it too returns to preset.
  scale_x_reset <- reset_preset_default("spatial_images_scale_x", 1)
  scale_y_reset <- reset_preset_default("spatial_images_scale_y", 1)
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
  updateSliderInput(session, "spatial_projection_background_rotate", value = 0)
  updateCheckboxInput(
    session,
    "spatial_projection_background_flip_x",
    value = isTRUE(reset_preset_default("spatial_images_flip_x", FALSE))
  )
  updateCheckboxInput(
    session,
    "spatial_projection_background_flip_y",
    value = isTRUE(reset_preset_default("spatial_images_flip_y", FALSE))
  )
})

##----------------------------------------------------------------------------##
## Two-way sync between each Move slider (coarse drag, authoritative) and its
## numeric box (exact keyboard entry / unit-level nudge). The slider is the value
## the appearance observer above reads; the numeric box only mirrors it. Each
## direction updates the OTHER control, guarded by an equality check so the two
## observers can't ping-pong into an infinite loop.
##----------------------------------------------------------------------------##
local({
  sync_move <- function(slider_id, numeric_id) {
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
  sync_move(
    "spatial_projection_background_offset_x",
    "spatial_projection_background_offset_x_num"
  )
  sync_move(
    "spatial_projection_background_offset_y",
    "spatial_projection_background_offset_y_num"
  )
})
