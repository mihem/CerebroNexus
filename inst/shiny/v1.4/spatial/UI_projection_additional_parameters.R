##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_additional_parameters_UI"]] <- renderUI({
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
      min = preferences[["gene_expression_plot_point_size"]][["min"]],
      max = preferences[["gene_expression_plot_point_size"]][["max"]],
      step = preferences[["gene_expression_plot_point_size"]][["step"]],
      fallback = preferences[["gene_expression_plot_point_size"]][["default"]]
    ),
    error = function(e) {
      preferences[["gene_expression_plot_point_size"]][["default"]]
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
      sp <- getSpatialData(input[["spatial_projection_to_display"]])
      co <- sp$coordinates
      span <- max(
        diff(range(co$x, na.rm = TRUE)),
        diff(range(co$y, na.rm = TRUE))
      )
      if (is.finite(span) && span > 0) {
        offset_limit <- ceiling(span / 100) * 100
        offset_step <- max(50, round(span / 400))
      }
    },
    error = function(e) NULL
  )

  ## Initial background-image adjustments for the CURRENT dataset, if the app was
  ## built with the matching `spatial_images_*` preset. Resolved by dataset name
  ## via `available_crb_files`. These control INITIAL VALUES so that an app can
  ## ship a pre-aligned overlay (move + flip + scale) as the default, instead of
  ## forcing the user to nudge it into place every session. The `fallback` is the
  ## identity value used when no preset applies (0 for move, 1 for scale, FALSE
  ## for flip). NOTE: the JS appearance channel that actually transforms the
  ## background reads these UI inputs, so seeding the inputs here is what makes a
  ## preset take effect — reading the option anywhere else has no visible result.
  preset_default <- function(option_name, fallback) {
    resolve_spatial_image_preset(
      option_name,
      fallback,
      if (exists("Cerebro.options")) Cerebro.options else NULL,
      if (exists("available_crb_files")) available_crb_files$files else NULL,
      if (exists("available_crb_files")) available_crb_files$selected else NULL
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
  offset_x_default <- preset_default("spatial_images_offset_x", 0)
  offset_y_default <- preset_default("spatial_images_offset_y", 0)
  flip_x_default <- isTRUE(preset_default("spatial_images_flip_x", FALSE))
  flip_y_default <- isTRUE(preset_default("spatial_images_flip_y", FALSE))
  scale_x_default <- preset_default("spatial_images_scale_x", 1)
  scale_y_default <- preset_default("spatial_images_scale_y", 1)
  aspect_locked_default <- isTRUE(
    all.equal(scale_x_default, scale_y_default) == TRUE
  )

  tagList(
    sliderInput(
      "spatial_projection_point_size",
      label = "Point size",
      min = preferences[["gene_expression_plot_point_size"]][["min"]],
      max = preferences[["gene_expression_plot_point_size"]][["max"]],
      step = preferences[["gene_expression_plot_point_size"]][["step"]],
      value = default_point_size
    ),
    sliderInput(
      "spatial_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["gene_expression_plot_point_opacity"]][["min"]],
      max = preferences[["gene_expression_plot_point_opacity"]][["max"]],
      step = preferences[["gene_expression_plot_point_opacity"]][["step"]],
      ## Spatial-specific default: fully opaque points (cells sit over a tissue
      ## image, where translucent points read as washed out).
      value = 1
    ),
    sliderInput(
      "spatial_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["gene_expression_plot_percentage_cells_to_show"]][[
        "min"
      ]],
      max = preferences[["gene_expression_plot_percentage_cells_to_show"]][[
        "max"
      ]],
      step = preferences[["gene_expression_plot_percentage_cells_to_show"]][[
        "step"
      ]],
      ## Spatial-specific default: show all cells. Unlike a scRNA-seq UMAP,
      ## where down-sampling barely changes the picture, spatial resolution is
      ## the whole point here — dropping cells visibly degrades the tissue map.
      value = 100
    ),
    ## Background-image adjustments. Shown only when an image is selected. Every
    ## control here is DECOUPLED from the scatter plot: it re-styles the image
    ## <div> via the independent JS channel and never re-renders the points.
    conditionalPanel(
      condition = paste0(
        "input.spatial_projection_background_image && ",
        "input.spatial_projection_background_image !== 'No Background'"
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
        value = 0,
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
  "spatial_projection_additional_parameters_UI",
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
