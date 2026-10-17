##----------------------------------------------------------------------------##
## Object that combines all data required for updating projection plot.
##----------------------------------------------------------------------------##
expression_projection_data_to_plot_raw <- reactive({
  ## shiny::debounce() below evaluates this eagerly. Off the page it would read
  ## million-cell expression rows for a view nobody is looking at.
  req(identical(input[["sidebar"]], "geneExpression"))
  dataset_context <- viewer_loaded_dataset_context()
  coordinates <- expression_projection_coordinates()
  parameters <- expression_projection_parameters_plot()
  expression_levels <- expression_projection_expression_levels()
  no_gene_selected <- length(
    expression_selected_genes()[["genes_to_display_present"]]
  ) ==
    0L
  ## The primary frame needs only canonical row indices. Full metadata belongs
  ## exclusively to deferred hover/selection auxiliary data, including after a
  ## gene is selected, so never materialize it on the colour-update path.
  metadata <- NULL
  req(
    coordinates,
    parameters,
    expression_projection_parameters_color(),
    expression_projection_trajectory(),
    no_gene_selected ||
      nrow(coordinates) == length(expression_levels) ||
      nrow(coordinates) == length(expression_levels[[1]]),
    !is.null(input[["expression_projection_genes_in_separate_panels"]])
  )
  if (parameters[['is_trajectory']]) {
    req(
      nrow(coordinates) == nrow(expression_projection_trajectory()[['meta']])
    )
  }
  to_return <- list(
    dataset_context = dataset_context,
    feedback = cerebroFeedbackControls("expression_projection", input),
    dataset = data_set(),
    render_request = input[["expression_projection_render_request"]],
    coordinates = coordinates,
    reset_axes = isolate(expression_projection_parameters_other[[
      'reset_axes'
    ]]),
    expression_levels = expression_levels,
    plot_parameters = parameters,
    color_settings = expression_projection_parameters_color(),
    metadata = metadata,
    trajectory = expression_projection_trajectory(),
    display_mode = input[["expression_projection_genes_in_separate_panels"]],
    rgb_layout = input[["expression_rgb_layout"]],
    render_key = expression_projection_render_key(),
    cell_indices = expression_projection_cells_to_show(),
    projection_resource_failed = input[[
      "expression_projection_projection_resource_failed"
    ]],
    separate_panels = identical(
      input[["expression_projection_genes_in_separate_panels"]],
      "separate"
    )
  )
  return(to_return)
})

## This prepared value already contains the million-cell expression vector.
## Use Shiny's debounce directly instead of relaying it through another observer.
expression_projection_data_to_plot <- shiny::debounce(
  expression_projection_data_to_plot_raw,
  50
)
