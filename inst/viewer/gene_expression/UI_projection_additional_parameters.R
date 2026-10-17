##----------------------------------------------------------------------------##
## UI elements to set additional plotting parameters.
##----------------------------------------------------------------------------##
output[["expression_projection_additional_parameters_UI"]] <- renderUI({
  viewer_requested_dataset_context()
  appearance <- isolate(current_scatter_defaults())

  tagList(
    sliderInput(
      "expression_projection_point_size",
      label = "Point size",
      min = preferences[["cell_point_size"]][["min"]],
      max = preferences[["cell_point_size"]][["max"]],
      step = preferences[["cell_point_size"]][["step"]],
      value = appearance$point_size
    ),
    sliderInput(
      "expression_projection_point_opacity",
      label = "Point opacity",
      min = preferences[["cell_point_opacity"]][["min"]],
      max = preferences[["cell_point_opacity"]][["max"]],
      step = preferences[["cell_point_opacity"]][["step"]],
      value = appearance$point_opacity
    )
  )
})

output[["expression_projection_data_parameters_UI"]] <- renderUI({
  viewer_requested_dataset_context()
  appearance <- isolate(current_scatter_defaults())

  tagList(
    checkboxInput(
      "expression_projection_plotting_order",
      label = "Highest expression on top",
      value = identical(current_page_display_defaults("gene")$plotting_order,
                        "Highest expression on top")
    ),
    sliderInput(
      "expression_projection_percentage_cells_to_show",
      label = "Show % of cells",
      min = preferences[["cell_percentage_cells_to_show"]][[
        "min"
      ]],
      max = preferences[["cell_percentage_cells_to_show"]][[
        "max"
      ]],
      step = preferences[["cell_percentage_cells_to_show"]][[
        "step"
      ]],
      value = appearance$percentage_cells_to_show
    )
  )
})

## Keep controls available while the settings drawer is hidden.
outputOptions(
  output,
  "expression_projection_additional_parameters_UI",
  suspendWhenHidden = FALSE
)
outputOptions(
  output,
  "expression_projection_data_parameters_UI",
  suspendWhenHidden = FALSE
)

##----------------------------------------------------------------------------##
## Info box that gets shown when pressing the "info" button.
##----------------------------------------------------------------------------##
cerebroRegisterInfo(
  input,
  "expression_projection_additional_parameters_info",
  expression_projection_additional_parameters_info
)

##----------------------------------------------------------------------------##
## Text in info box.
##----------------------------------------------------------------------------##
expression_projection_additional_parameters_info <- list(
  title = "Additional parameters for gene (set) expression",
  text = HTML(
    "
    The elements in this panel allow you to control what and how results are displayed across the whole tab.
    <ul>
      <li><b>Highest expression on top:</b> Off preserves the input cell order, like Seurat FeaturePlot(order = FALSE). On draws cells from low to high expression.</li>
      <li><b>Point size:</b> Controls how large the cells should be.</li>
      <li><b>Point opacity:</b> Controls the transparency of the cells.</li>
      <li><b>Show % of cells:</b> Using the slider, you can randomly remove a fraction of cells from the plot. This can be useful for large data sets and/or computers with limited resources.</li>
    </ul>
    "
  )
)
