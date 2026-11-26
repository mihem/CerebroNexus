##----------------------------------------------------------------------------##
## Object that combines all data required for updating projection plot.
##----------------------------------------------------------------------------##
expression_projection_data_to_plot_raw <- reactive({
  ## shiny::debounce() below evaluates this eagerly. Off the page it would read
  ## million-cell expression rows for a view nobody is looking at.
  req(identical(input[["sidebar"]], "geneExpression"))
  dataset_context <- viewer_loaded_dataset_context()
  parameters <- expression_projection_parameters_plot()
  cell_indices <- expression_projection_cells_to_show()
  failed_asset <- input[["expression_projection_projection_resource_failed"]]
  projection_resource <- if (!isTRUE(parameters[["is_trajectory"]])) {
    asset <- viewerProjectionAsset(parameters[["projection"]], cell_indices)
    if (
      is.list(asset) &&
        !identical(as.character(failed_asset), asset$url)
    ) {
      asset
    } else {
      NULL
    }
  } else {
    NULL
  }
  coordinates <- if (is.list(projection_resource)) {
    list()
  } else {
    expression_projection_coordinates()
  }
  n_coord_cells <- if (is.list(projection_resource)) {
    length(cell_indices)
  } else {
    nrow(coordinates)
  }
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
    is.list(projection_resource) || !is.null(coordinates),
    parameters,
    expression_projection_parameters_color(),
    expression_projection_trajectory(),
    no_gene_selected ||
      n_coord_cells == length(expression_levels) ||
      n_coord_cells == length(expression_levels[[1]]),
    !is.null(expression_projection_display_mode())
  )
  if (parameters[['is_trajectory']]) {
    req(
      nrow(coordinates) == nrow(expression_projection_trajectory()[['meta']])
    )
  }
  ## Control values (feedback) are not part of the frame: the sender adds
  ## them, so a control that binds without changing the frame sends none.
  to_return <- list(
    dataset_context = dataset_context,
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
    display_mode = expression_projection_display_mode(),
    rgb_layout = input[["expression_rgb_layout"]],
    render_key = expression_projection_render_key(),
    cell_indices = cell_indices,
    trajectory_rows = expression_projection_trajectory_rows(),
    client_percentage = isTRUE(expression_projection_client_percentage()),
    projection_resource_failed = failed_asset,
    separate_panels = identical(
      expression_projection_display_mode(),
      "separate"
    )
  )
  return(to_return)
})

## This prepared value already contains the million-cell expression vector.
## Apply the first change after a quiet spell immediately (like Projection and
## Spatial), while still coalescing rapid bursts (slider drags / multi-clicks).
expression_projection_active <- function() {
  identical(input[["sidebar"]], "geneExpression")
}
expression_projection_data_to_plot <- debounceAfterFirst(
  expression_projection_data_to_plot_raw,
  50,
  active = expression_projection_active
)
