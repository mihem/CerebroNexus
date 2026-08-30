##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
## Scatter controls deliberately live in a renderUI which is independent of the
## selected background image. This preserves users' point-size, opacity and
## sampling choices while moving among backgrounds.
output[["spatial_projection_scatter_parameters_UI"]] <- renderUI({
  ## Start from a dynamic default sized to the spot count + canvas, falling back
  ## to the fixed default if that can't be computed. A dataset-specific preset
  ## (below) still takes precedence over this when one is configured.
  default_point_size <- tryCatch(
    dynamicPointSize(
      n_points = tryCatch(
        nrow(
          getSpatialData(input[["spatial_projection_to_display"]])$coordinates
        ),
        error = function(e) nrow(getMetaData())
      ),
      plot_width_px = session$clientData[["output_spatial_projection_width"]],
      plot_height_px = session$clientData[["output_spatial_projection_height"]],
      min = preferences[["projection_plot_point_size"]][["min"]],
      max = preferences[["projection_plot_point_size"]][["max"]],
      step = preferences[["projection_plot_point_size"]][["step"]],
      fallback = preferences[["projection_plot_point_size"]][["default"]]
    ),
    error = function(e) {
      preferences[["projection_plot_point_size"]][["default"]]
    }
  )

  if (
    exists("Cerebro.options") &&
      !is.null(Cerebro.options[["point_size"]]) &&
      is.list(Cerebro.options[["point_size"]]) &&
      !is.null(Cerebro.options[["point_size"]][[
        "spatial_projection_point_size"
      ]])
  ) {
    config_val <- Cerebro.options[["point_size"]][[
      "spatial_projection_point_size"
    ]]

    if (is.list(config_val)) {
      if (
        !is.null(available_crb_files$names) &&
          !is.null(available_crb_files$files) &&
          !is.null(available_crb_files$selected)
      ) {
        idx <- which(available_crb_files$files == available_crb_files$selected)
        if (length(idx) > 0) {
          current_name <- available_crb_files$names[idx[1]]
          if (current_name %in% names(config_val)) {
            default_point_size <- config_val[[current_name]]
          }
        }
      }
    } else if (is.numeric(config_val)) {
      default_point_size <- config_val
    }
  }

  tagList(
    sliderInput(
      "spatial_projection_point_size",
      label = "Point size",
      min = preferences[["projection_plot_point_size"]][["min"]],
      max = preferences[["projection_plot_point_size"]][["max"]],
      step = preferences[["projection_plot_point_size"]][["step"]],
      value = default_point_size
    ),
    sliderInput(
      "spatial_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["projection_plot_point_opacity"]][["min"]],
      max = preferences[["projection_plot_point_opacity"]][["max"]],
      step = preferences[["projection_plot_point_opacity"]][["step"]],
      value = 1
    ),
    sliderInput(
      "spatial_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["projection_plot_percentage_cells_to_show"]][[
        "min"
      ]],
      max = preferences[["projection_plot_percentage_cells_to_show"]][[
        "max"
      ]],
      step = preferences[["projection_plot_percentage_cells_to_show"]][[
        "step"
      ]],
      value = 100
    )
  )
})

## The image-specific controls may safely be regenerated when the selected
## image changes: their initial values come from that image's preset.
output[["spatial_projection_background_parameters_UI"]] <- renderUI({
  ## Offset sliders move the background image in DATA units, so their range is
  ## sized to the current dataset's coordinate span (± the larger of x/y span).
  ## That keeps one range usable whether the coordinates run 0–5k (Xenium) or
  ## 0–9k (MERFISH). Falls back to a generous default if coordinates are absent.
  offset_limit <- 5000
  ## Coarse step so each nudge visibly moves the image; a step of 1 was
  ## imperceptible on datasets with a large coordinate span.
  offset_step <- 50
  tryCatch(
    {
      req(
        !is.null(input[["spatial_projection_to_display"]]),
        input[["spatial_projection_to_display"]] %in% availableSpatial()
      )
      sp <- getSpatialData(input[["spatial_projection_to_display"]])
      co <- sp$coordinates
      x <- co[[1]][is.finite(co[[1]])]
      y <- co[[2]][is.finite(co[[2]])]
      if (length(x) > 0 && length(y) > 0) {
        span <- max(diff(range(x)), diff(range(y)))
        if (is.finite(span) && span > 0) {
          offset_limit <- ceiling(span / 100) * 100
          offset_step <- max(50, round(span / 400))
        }
      }
    },
    error = function(e) NULL
  )

  ## Seed appearance from the exact dataset / spatial / image leaf. Because this
  ## renderUI reads the selected source-tagged image key, changing either spatial
  ## entry or image recreates every control with that image's own defaults.
  spatial_name <- input[["spatial_projection_to_display"]]
  dataset <- spatial_dataset_name(
    if (exists("available_crb_files")) available_crb_files$files else NULL,
    if (exists("available_crb_files")) available_crb_files$selected else NULL
  )
  spatial_data <- tryCatch(
    getSpatialData(spatial_name),
    error = function(e) list()
  )
  embedded_images <- embedded_spatial_images(spatial_data)
  external_images <- configured_spatial_images(
    if (exists("Cerebro.options")) Cerebro.options else NULL,
    dataset,
    spatial_name
  )
  selected_descriptor <- resolve_spatial_background(
    input[["spatial_projection_background_image"]],
    embedded_images,
    external_images
  )
  image_label <- if (is.null(selected_descriptor)) {
    NULL
  } else {
    selected_descriptor$label
  }
  preset_default <- function(setting, fallback) {
    if (is.null(image_label)) {
      return(fallback)
    }
    resolve_spatial_image_setting(
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      dataset,
      spatial_name,
      image_label,
      setting,
      fallback
    )
  }
  ## Seed MOVE and FLIP from the preset so the controls honestly reflect the
  ## shipped alignment (checkbox ticked, sliders positioned). Both are read by
  ## the JS as single-source interaction state (dataset.offsetX / dataset.flipY),
  ## so seeding the input is exactly where the preset takes effect — one factor,
  ## no double-application.
  ##
  ## SCALE is now single-source too: the slider(s) are seeded from the preset and
  ## are the only scale the JS applies (no separate dataset factor). The X and Y
  ## scale can differ; a "Lock aspect ratio" checkbox decides whether the user
  ## sees one slider (locked, X drives both) or two (unlocked). Initial lock state
  ## follows the preset: equal x/y -> locked single slider; unequal -> unlocked
  ## with both shown.
  offset_x_default <- preset_default("offset_x", 0)
  offset_y_default <- preset_default("offset_y", 0)
  flip_x_default <- isTRUE(preset_default("flip_x", FALSE))
  flip_y_default <- isTRUE(preset_default("flip_y", FALSE))
  scale_x_default <- preset_default("scale_x", 1)
  scale_y_default <- preset_default("scale_y", 1)
  rotation_default <- preset_default("rotation", 0)
  aspect_locked_default <- isTRUE(
    all.equal(scale_x_default, scale_y_default) == TRUE
  )

  tagList(
    ## Background-image adjustments. Shown only when an image is selected. Every
    ## control here is DECOUPLED from the scatter plot: it re-styles the image
    ## <div> via the independent JS channel and never re-renders the points.
    conditionalPanel(
      condition = paste0(
        "input.spatial_projection_background_image && ",
        "input.spatial_projection_background_image !== 'none'"
      ),
      tags$hr(style = "margin: 16px 0 10px; border-top: 2px solid #ccc;"),
      tags$div(
        style = paste0(
          "font-size: 15px; font-weight: 700; margin-bottom: 8px; ",
          "text-transform: uppercase; letter-spacing: 0.04em; color: #337ab7;"
        ),
        "Background image"
      ),
      sliderInput(
        "spatial_projection_background_opacity",
        label = "Image opacity",
        min = 0,
        max = 1,
        value = 0.6,
        step = 0.05
      ),
      ## Move: slider for coarse dragging + numeric box for exact keyboard entry
      ## and unit-level nudging. The slider (`..._offset_x`) stays the AUTHORITATIVE
      ## input the appearance observer reads; the numeric box (`..._offset_x_num`)
      ## is a two-way mirror synced by an observer in obj_projection_parameters_plot.R.
      tags$label(
        `for` = "spatial_projection_background_offset_x",
        class = "control-label",
        "Move horizontally"
      ),
      tags$div(
        style = "display: flex; gap: 8px; align-items: center;",
        tags$div(
          style = "flex: 1 1 auto;",
          sliderInput(
            "spatial_projection_background_offset_x",
            label = NULL,
            min = -offset_limit,
            max = offset_limit,
            value = offset_x_default,
            step = offset_step
          )
        ),
        tags$div(
          style = "flex: 0 0 90px;",
          numericInput(
            "spatial_projection_background_offset_x_num",
            label = NULL,
            value = offset_x_default,
            step = offset_step
          )
        )
      ),
      tags$label(
        `for` = "spatial_projection_background_offset_y",
        class = "control-label",
        "Move vertically"
      ),
      tags$div(
        style = "display: flex; gap: 8px; align-items: center;",
        tags$div(
          style = "flex: 1 1 auto;",
          sliderInput(
            "spatial_projection_background_offset_y",
            label = NULL,
            min = -offset_limit,
            max = offset_limit,
            value = offset_y_default,
            step = offset_step
          )
        ),
        tags$div(
          style = "flex: 0 0 90px;",
          numericInput(
            "spatial_projection_background_offset_y_num",
            label = NULL,
            value = offset_y_default,
            step = offset_step
          )
        )
      ),
      checkboxInput(
        "spatial_projection_background_scale_lock",
        label = "Lock aspect ratio",
        value = aspect_locked_default
      ),
      ## Locked: one slider drives both axes (X mirrors to Y in the observer).
      conditionalPanel(
        condition = "input.spatial_projection_background_scale_lock",
        sliderInput(
          "spatial_projection_background_scale",
          label = "Scale (about centre)",
          min = 0.2,
          max = 3,
          value = scale_x_default,
          step = 0.05
        )
      ),
      ## Unlocked: independent X / Y scale.
      conditionalPanel(
        condition = "!input.spatial_projection_background_scale_lock",
        sliderInput(
          "spatial_projection_background_scale_x",
          label = "Scale X (about centre)",
          min = 0.2,
          max = 3,
          value = scale_x_default,
          step = 0.05
        ),
        sliderInput(
          "spatial_projection_background_scale_y",
          label = "Scale Y (about centre)",
          min = 0.2,
          max = 3,
          value = scale_y_default,
          step = 0.05
        )
      ),
      sliderInput(
        "spatial_projection_background_rotate",
        label = "Rotate (about centre)",
        min = -180,
        max = 180,
        value = rotation_default,
        step = 1
      ),
      checkboxInput(
        "spatial_projection_background_flip_x",
        label = "Flip horizontally",
        value = flip_x_default
      ),
      checkboxInput(
        "spatial_projection_background_flip_y",
        label = "Flip vertically",
        value = flip_y_default
      ),
      actionButton(
        "spatial_projection_background_reset",
        label = "Reset image",
        icon = icon("undo"),
        width = "100%"
      ),
      ## Turn the current hand-tuned alignment into pasteable Cerebro.options
      ## preset code, so an app can ship this dataset pre-aligned instead of the
      ## user re-nudging it every session. Output appears in the box below.
      actionButton(
        "spatial_projection_background_copy_preset",
        label = "Copy alignment as preset",
        icon = icon("clipboard"),
        width = "100%"
      ),
      conditionalPanel(
        condition = "output.spatial_projection_background_preset_code_present",
        tags$pre(
          style = paste(
            "margin-top: 8px; padding: 8px; font-size: 11px;",
            "white-space: pre-wrap; word-break: break-word;",
            "background: #f6f8fa; border: 1px solid #d9d9d9;",
            "border-radius: 4px;"
          ),
          verbatimTextOutput(
            "spatial_projection_background_preset_code"
          )
        )
      )
    )
  )
})


## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "spatial_projection_scatter_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "spatial_projection_background_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      spatial_projection_additional_parameters_info[["text"]],
      title = spatial_projection_additional_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
# <li><b>Range of X/Y axis (located in dropdown menu above the projection):</b> Set the X/Y axis limits. This is useful when you want to change the aspect ratio of the plot.</li>
spatial_projection_additional_parameters_info <- list(
  title = "Additional parameters for projection",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)
