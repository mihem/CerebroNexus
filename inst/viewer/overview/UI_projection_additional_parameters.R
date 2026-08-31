##----------------------------------------------------------------------------##
## UI elements to set additional parameters for the projection.
##----------------------------------------------------------------------------##
output[["overview_projection_additional_parameters_UI"]] <- renderUI({
  appearance <- current_scatter_defaults()

  tagList(
    sliderInput(
      "overview_projection_point_size",
      label = "Point size",
      min = preferences[["cell_point_size"]][["min"]],
      max = preferences[["cell_point_size"]][["max"]],
      step = preferences[["cell_point_size"]][["step"]],
      value = appearance$point_size
    ),
    sliderInput(
      "overview_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["cell_point_opacity"]][["min"]],
      max = preferences[["cell_point_opacity"]][["max"]],
      step = preferences[["cell_point_opacity"]][["step"]],
      value = appearance$point_opacity
    )
  )
})

output[["overview_projection_data_parameters_UI"]] <- renderUI({
  appearance <- current_scatter_defaults()

  tagList(
    sliderInput(
      "overview_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["cell_percentage_cells_to_show"]][["min"]],
      max = preferences[["cell_percentage_cells_to_show"]][["max"]],
      step = preferences[["cell_percentage_cells_to_show"]][[
        "step"
      ]],
      value = appearance$percentage_cells_to_show
    )
  )
})

## make sure elements are loaded even though the box is collapsed
outputOptions(
  output,
  "overview_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "overview_projection_data_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
observeEvent(input[["overview_projection_additional_parameters_info"]], {
  showModal(
    modalDialog(
      overview_projection_additional_parameters_info[["text"]],
      title = overview_projection_additional_parameters_info[["title"]],
      easyClose = TRUE,
      footer = NULL,
      size = "l"
    )
  )
})

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
overview_projection_additional_parameters_info <- list(
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
