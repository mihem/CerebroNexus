##----------------------------------------------------------------------------##
## Function to plot expression in trajectory (for export).
##----------------------------------------------------------------------------##
pltExpTrj2DSglPanExp <- function(
  df,
  trajectory_edges,
  point_size,
  point_opacity,
  point_border,
  color_scale,
  color_range,
  x_range,
  y_range
) {
  ##
  if (point_border == TRUE) {
    stroke <- 0.2
  } else {
    stroke <- 0
  }
  ## start building the plot
  plot <- ggplot() +
    geom_point(
      data = df,
      aes(
        x = .data[[colnames(df)[1]]],
        y = .data[[colnames(df)[2]]],
        fill = .data[["level"]]
      ),
      shape = 21,
      size = point_size / 3,
      stroke = stroke,
      color = "#c4c4c4",
      alpha = point_opacity
    ) +
    geom_segment(
      data = trajectory_edges,
      aes(
        source_dim_1,
        source_dim_2,
        xend = target_dim_1,
        yend = target_dim_2
      ),
      size = 0.75,
      linetype = "solid",
      na.rm = TRUE
    ) +
    cerebro_export_theme()
  plot <- plot + expressionFillScale(color_scale, color_range)
  return(plot)
}
