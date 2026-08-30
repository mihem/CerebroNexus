##----------------------------------------------------------------------------##
## Function that updates projections.
##----------------------------------------------------------------------------##
overview_projection_update_plot <- function(input) {
  cells_df <- input[["cells_df"]]
  coordinates <- input[["coordinates"]]
  reset_axes <- input[["reset_axes"]]
  plot_parameters <- input[["plot_parameters"]]
  color_assignments <- input[["color_assignments"]]
  hover_info <- input[["hover_info"]]
  color_variable <- plot_parameters[["color_variable"]]
  color_input <- cells_df[[color_variable]]
  n_dimensions <- plot_parameters[["n_dimensions"]]
  selection_keys <- if ("cell_barcode" %in% colnames(cells_df)) {
    as.character(cells_df[["cell_barcode"]])
  } else {
    as.character(seq_len(nrow(cells_df)))
  }

  payload <- cerebroCellViewScatterPayload(
    coordinates = coordinates,
    color = color_input,
    color_variable = color_variable,
    selection_keys = selection_keys,
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    point_line = if (plot_parameters[["draw_border"]]) {
      list(color = "rgb(196,196,196)", width = 1)
    } else {
      list()
    },
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]],
    reset_axes = reset_axes,
    n_dimensions = n_dimensions,
    color_assignments = color_assignments,
    hover_info = hover_info,
    hover = plot_parameters[["hover_info"]]
  )

  cerebroCellViewRender(
    "overview_projection",
    payload[["meta"]],
    payload[["data"]],
    payload[["hover"]]
  )
}
