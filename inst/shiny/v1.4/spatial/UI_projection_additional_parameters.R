##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_additional_parameters_UI"]] <- renderUI({
  default_point_size <- preferences[["gene_expression_plot_point_size"]][[
    "default"
  ]]

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
      ## Spatial-specific default: show half the cells so the plot stays
      ## responsive on large sections while still reading as full tissue.
      value = 50
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
            value = 0,
            step = offset_step
          )
        ),
        tags$div(
          style = "flex: 0 0 90px;",
          numericInput(
            "spatial_projection_background_offset_x_num",
            label = NULL,
            value = 0,
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
            value = 0,
            step = offset_step
          )
        ),
        tags$div(
          style = "flex: 0 0 90px;",
          numericInput(
            "spatial_projection_background_offset_y_num",
            label = NULL,
            value = 0,
            step = offset_step
          )
        )
      ),
      sliderInput(
        "spatial_projection_background_scale",
        label = "Scale (about centre)",
        min = 0.2,
        max = 3,
        value = 1,
        step = 0.05
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
        value = FALSE
      ),
      checkboxInput(
        "spatial_projection_background_flip_y",
        label = "Flip vertically",
        value = FALSE
      ),
      actionButton(
        "spatial_projection_background_reset",
        label = "Reset image",
        icon = icon("undo"),
        width = "100%"
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
