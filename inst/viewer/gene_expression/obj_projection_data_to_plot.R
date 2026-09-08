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
    !isTRUE(expression_projection_hover_info()$enabled) ||
      nrow(expression_projection_coordinates()) ==
        length(expression_projection_hover_info()$selection_key),
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

expression_projection_render_event <- viewerProjectionEvent(
  "expression_projection",
  "geneExpression",
  extra = function() {
    list(
      plotting_order = input[["expression_projection_plotting_order"]],
      display_mode = input[["expression_projection_genes_in_separate_panels"]],
      analysis_mode = input[["expression_analysis_mode"]],
      genes = input[["expression_genes_input"]],
      gene_set = input[["expression_select_gene_set"]],
      rgb_r = input[["expression_rgb_gene_r"]],
      rgb_g = input[["expression_rgb_gene_g"]],
      rgb_b = input[["expression_rgb_gene_b"]],
      color_mode = input[["expression_projection_gene_color_mode"]]
    )
  }
)

expression_projection_data_to_plot <- debounceEventAfterFirst(
  expression_projection_render_event,
  expression_projection_data_to_plot_raw,
  250
)
