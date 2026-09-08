##----------------------------------------------------------------------------##
## Object that combines all data required for updating projection plot.
##----------------------------------------------------------------------------##
expression_projection_data_to_plot_raw <- reactive({
  coordinates <- expression_projection_coordinates()
  plot_parameters <- expression_projection_parameters_plot()
  color_settings <- expression_projection_parameters_color()
  hover_info <- expression_projection_hover_info()
  trajectory <- expression_projection_trajectory()
  display_mode <- input[["expression_projection_genes_in_separate_panels"]]
  req(
    coordinates,
    plot_parameters,
    color_settings,
    hover_info,
    trajectory,
    !is.null(display_mode)
  )
  expression_levels <- isolate(expression_projection_expression_levels())
  req(
    nrow(coordinates) == length(expression_levels) ||
      nrow(coordinates) == length(expression_levels[[1]]),
    !isTRUE(hover_info$enabled) ||
      nrow(coordinates) == length(hover_info$selection_key)
  )
  expression_levels <- expression_projection_expression_levels()
  if (plot_parameters[['is_trajectory']]) {
    req(
      nrow(coordinates) == nrow(trajectory[['meta']])
    )
  }
  list(
    coordinates = coordinates,
    reset_axes = isolate(expression_projection_parameters_other[[
      'reset_axes'
    ]]),
    expression_levels = expression_levels,
    plot_parameters = plot_parameters,
    color_settings = color_settings,
    selection_keys = as.character(
      expression_projection_data()[["cell_barcode"]]
    ),
    hover_info = hover_info,
    trajectory = trajectory,
    display_mode = display_mode,
    separate_panels = identical(display_mode, "separate")
  )
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
