##----------------------------------------------------------------------------##
## Object that combines all data required for updating projection plot.
##----------------------------------------------------------------------------##
expression_projection_data_to_plot_raw <- reactive({
  req(
    expression_projection_coordinates(),
    expression_projection_parameters_plot(),
    expression_projection_parameters_color(),
    expression_projection_hover_info(),
    expression_projection_trajectory(),
    nrow(expression_projection_coordinates()) ==
      length(isolate(expression_projection_expression_levels())) ||
      nrow(expression_projection_coordinates()) ==
        length(isolate(expression_projection_expression_levels())[[1]]),
    nrow(expression_projection_coordinates()) ==
      length(expression_projection_hover_info()) ||
      expression_projection_hover_info() == "none",
    !is.null(input[["expression_projection_genes_in_separate_panels"]])
  )
  parameters <- expression_projection_parameters_plot()
  if (parameters[['is_trajectory']]) {
    req(
      nrow(expression_projection_coordinates()) ==
        nrow(expression_projection_trajectory()[['meta']])
    )
  }
  to_return <- list(
    coordinates = expression_projection_coordinates(),
    reset_axes = isolate(expression_projection_parameters_other[[
      'reset_axes'
    ]]),
    expression_levels = expression_projection_expression_levels(),
    plot_parameters = expression_projection_parameters_plot(),
    color_settings = expression_projection_parameters_color(),
    selection_keys = as.character(
      expression_projection_data()[["cell_barcode"]]
    ),
    hover_info = expression_projection_hover_info(),
    trajectory = expression_projection_trajectory(),
    display_mode = input[["expression_projection_genes_in_separate_panels"]],
    separate_panels = identical(
      input[["expression_projection_genes_in_separate_panels"]],
      "separate"
    )
  )
  return(to_return)
})

expression_projection_data_to_plot <- debounce(
  expression_projection_data_to_plot_raw,
  250
)
