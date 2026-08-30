##----------------------------------------------------------------------------##
## Function to plot expression in single panel in 2D (for export).
##----------------------------------------------------------------------------##
pltExpProj2DSglPanExp <- function(
  df,
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
  ## prepare plot
  plot <- ggplot(
    df,
    aes_q(
      x = as.name(colnames(df)[1]),
      y = as.name(colnames(df)[2]),
      fill = as.name("level")
    )
  ) +
    geom_point(
      shape = 21,
      size = point_size / 3,
      stroke = stroke,
      color = "#c4c4c4",
      alpha = point_opacity
    ) +
    lims(x = x_range, y = y_range) +
    cerebro_export_theme()
  plot <- plot + expressionFillScale(color_scale, color_range)
  return(plot)
}
