##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
output[["spatial_projection_additional_parameters_UI"]] <- renderUI({

  default_point_size <- preferences[["scatter_plot_point_size"]][["default"]]

  if (
    exists("Cerebro.options") &&
    !is.null(Cerebro.options[["point_size"]]) &&
    is.list(Cerebro.options[["point_size"]]) &&
    !is.null(Cerebro.options[["point_size"]][["spatial_projection_point_size"]])
  ) {
    config_val <- Cerebro.options[["point_size"]][["spatial_projection_point_size"]]

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
      min = preferences[["scatter_plot_point_size"]][["min"]],
      max = preferences[["scatter_plot_point_size"]][["max"]],
      step = preferences[["scatter_plot_point_size"]][["step"]],
      value = default_point_size
    ),
    sliderInput(
      "spatial_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["scatter_plot_point_opacity"]][["min"]],
      max = preferences[["scatter_plot_point_opacity"]][["max"]],
      step = preferences[["scatter_plot_point_opacity"]][["step"]],
      value = preferences[["scatter_plot_point_opacity"]][["default"]]
    ),
    sliderInput(
      "spatial_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["scatter_plot_percentage_cells_to_show"]][["min"]],
      max = preferences[["scatter_plot_percentage_cells_to_show"]][["max"]],
      step = preferences[["scatter_plot_percentage_cells_to_show"]][["step"]],
      value = preferences[["scatter_plot_percentage_cells_to_show"]][["default"]]
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
  text = HTML("
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)
