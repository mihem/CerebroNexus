##----------------------------------------------------------------------------##
## Collect data required to update projection.
##----------------------------------------------------------------------------##
## shiny::debounce() evaluates this eagerly; only the visible page may pay for it.
overview_projection_active <- function() identical(input[["sidebar"]], "overview")
overview_projection_data_to_plot_raw <- reactive({
  req(overview_projection_active())
  dataset_context <- viewer_loaded_dataset_context()
  plot_parameters <- overview_projection_parameters_plot()
  req(plot_parameters)
  cell_indices <- overview_projection_cells_to_show()
  color_variable <- plot_parameters[['color_variable']]
  metadata <- viewerProjectionFirstFrameMetadata()
  color_input <- metadata[[color_variable]]
  if (!length(cell_indices)) {
    color_assignments <- character(0)
  } else if (is.numeric(color_input)) {
    color_assignments <- NULL
  } else if (color_variable %in% getGroups()) {
    color_assignments <- reactive_group_colors(color_variable)
  } else {
    color_assignments <- assignColorsToGroups(metadata, color_variable)
  }
  projection_resource <- viewerProjectionAsset(
    plot_parameters[["projection"]],
    cell_indices
  )
  failed_resource <- input[[
    "overview_projection_projection_resource_failed"
  ]]
  if (
    is.list(projection_resource) &&
      length(failed_resource) == 1L &&
      !is.na(failed_resource) &&
      identical(failed_resource, projection_resource$url)
  ) {
    projection_resource <- NULL
  }
  resource_coordinates <- is.list(projection_resource)
  categorical_resource <- if (
    resource_coordinates && !is.numeric(color_input) && length(cell_indices)
  ) {
    viewerMetadataCodesAsset(color_variable, names(color_assignments))
  } else {
    NULL
  }
  cells_df <- overview_projection_data()
  coordinates <- if (resource_coordinates) {
    list()
  } else {
    overview_projection_coordinates()
  }
  list(
    dataset_context = dataset_context,
    feedback = cerebroFeedbackControls("overview_projection", input),
    cells_df = cells_df,
    cell_indices = cell_indices,
    cell_count = length(cell_indices),
    coordinates = coordinates,
    reset_axes = isolate(overview_projection_parameters_other[['reset_axes']]),
    plot_parameters = plot_parameters,
    color_assignments = color_assignments,
    projection_resource = projection_resource,
    categorical_resource = categorical_resource,
    resource_coordinates = resource_coordinates,
    client_percentage = isTRUE(overview_projection_client_percentage())
  )
})

overview_projection_data_to_plot <- debounceAfterFirst(
  overview_projection_data_to_plot_raw,
  150,
  active = overview_projection_active
)
