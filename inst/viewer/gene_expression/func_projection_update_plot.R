## function to be executed to update figure
expression_projection_update_plot <- function(input) {
  coordinates <- input[['coordinates']]
  reset_axes <- input[['reset_axes']]
  expression_levels <- input[['expression_levels']]
  plot_parameters <- input[['plot_parameters']]
  color_settings <- input[['color_settings']]
  selection_keys <- input[['selection_keys']]
  hover_info <- input[['hover_info']]
  trajectory <- input[['trajectory']]
  display_mode <- input[['display_mode']]
  separate_panels <- input[['separate_panels']]
  appearance <- list(
    group_labels = FALSE,
    draw_border = isTRUE(plot_parameters[["draw_border"]]),
    keep_square = isTRUE(plot_parameters[["keep_square"]])
  )
  ## sort cells based on expression (if applicable)
  if (
    plot_parameters[['plot_order']] == 'Highest expression on top' &&
      separate_panels == FALSE &&
      !identical(display_mode, "rgb")
  ) {
    cell_order <- order(expression_levels)
    coordinates <- coordinates[cell_order, ]
    selection_keys <- selection_keys[cell_order]
    hover_info <- hover_info[cell_order]
    if (is.list(expression_levels)) {
      for (i in seq_along(expression_levels)) {
        expression_levels[[i]] <- expression_levels[[i]][cell_order]
      }
    } else {
      expression_levels <- expression_levels[cell_order]
    }
  }
  ## define output_data
  output_data <- list(
    x = coordinates[[1]],
    y = coordinates[[2]],
    color = expression_levels,
    selection_key = selection_keys,
    point_size = plot_parameters[["point_size"]],
    point_opacity = plot_parameters[["point_opacity"]],
    point_line = list(),
    x_range = plot_parameters[["x_range"]],
    y_range = plot_parameters[["y_range"]],
    reset_axes = reset_axes
  )
  if (plot_parameters[["draw_border"]]) {
    output_data[['point_line']] <- list(
      color = "rgb(196,196,196)",
      width = 1
    )
  }
  output_data[["colorscale"]] <- expressionColorScale(
    color_settings[["color_scale"]]
  )
  if (
    is.list(expression_levels) &&
      !identical(display_mode, "rgb") &&
      identical(color_settings[["color_mode"]], "different")
  ) {
    output_data[["panel_colorscales"]] <- expressionPanelColorScales(
      names(expression_levels),
      color_settings[["color_mode"]],
      color_settings[["color_scale"]]
    )
  }
  output_data[["color_range"]] <- color_settings[["color_range"]]
  output_data[["reversescale"]] <- expressionReverseColorScale(
    color_settings[["color_scale"]]
  )
  ## prepare hover info
  output_hover <- list(
    hoverinfo = ifelse(plot_parameters[["hover_info"]], 'text', 'skip'),
    text = 'empty'
  )
  if (plot_parameters[["hover_info"]]) {
    output_hover[['text']] <- unname(hover_info)
  }
  ## process trajectory data
  trajectory_lines <- list()
  if (plot_parameters[['is_trajectory']]) {
    ## fix order of trajectory meta data if cells are sorted by expression
    if (
      plot_parameters[['plot_order']] == 'Highest expression on top' &&
        separate_panels == FALSE &&
        !identical(display_mode, "rgb")
    ) {
      trajectory[['meta']] <- trajectory[['meta']][cell_order, ]
    }
    ## add additional info to hover info
    if (plot_parameters[['hover_info']]) {
      output_hover[['text']] <- glue::glue(
        "{output_hover[['text']]}<br>",
        "<b>State</b>: {trajectory[['meta']]$state}<br>",
        "<b>Pseudotime</b>: {formatC(trajectory[['meta']]$pseudotime, format = 'f', digits = 2)}"
      )
    }
    ## convert trajectory edges to the shared renderer's shape format
    trajectory_edges <- trajectory[['edges']]
    for (i in seq_len(nrow(trajectory_edges))) {
      line <- list(
        type = "line",
        line = list(color = "black", width = 1),
        xref = "x",
        yref = "y",
        x0 = trajectory_edges$source_dim_1[i],
        y0 = trajectory_edges$source_dim_2[i],
        x1 = trajectory_edges$target_dim_1[i],
        y1 = trajectory_edges$target_dim_2[i]
      )
      trajectory_lines <- c(trajectory_lines, list(line))
    }
  }
  if (identical(display_mode, "rgb")) {
    output_data[["rgb"]] <- expression_levels[c("r", "g", "b")]
    output_data[["rgb_genes"]] <- color_settings[["rgb_genes"]]
    cerebroCellViewRender(
      "expression_projection",
      list(
        color_type = "rgb",
        color_variable = "RGB co-expression",
        space_label = plot_parameters[["projection"]],
        appearance = appearance
      ),
      output_data,
      output_hover,
      extra = list(shapes = trajectory_lines)
    )
    return(invisible(NULL))
  }
  n_dimensions <- plot_parameters[["n_dimensions"]]
  multi <- n_dimensions == 2 &&
    isTRUE(separate_panels) &&
    is.list(input[["expression_levels"]])
  if (n_dimensions == 3) {
    output_data[['z']] <- coordinates[[3]]
  }
  if (n_dimensions == 3 || !is.list(input[["expression_levels"]]) || multi) {
    legend_label <- if (is.list(expression_levels)) {
      "Expression"
    } else if (length(color_settings[["genes"]]) == 1) {
      color_settings[["genes"]][[1]]
    } else {
      paste0("Mean expression (", length(color_settings[["genes"]]), " genes)")
    }
    cerebroCellViewRender(
      "expression_projection",
      list(
        color_type = "continuous",
        color_variable = legend_label,
        space_label = plot_parameters[["projection"]],
        appearance = appearance
      ),
      output_data,
      output_hover,
      extra = list(shapes = trajectory_lines)
    )
  }
}
