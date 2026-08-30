expressionColorScale <- function(name) {
  if (!identical(name, "Cerebro orange")) {
    return(name)
  }
  list(
    list(0, "#aeb5bb"),
    list(0.08, "#f7c89d"),
    list(0.38, "#f49a4c"),
    list(0.7, "#e75f25"),
    list(1, "#9f251f")
  )
}

expressionValueRange <- function(expression_levels) {
  values <- unlist(expression_levels, use.names = FALSE)
  values <- values[is.finite(values)]
  if (!length(values) || all(values == 0)) {
    return(c(0, 1))
  }
  round(range(values), digits = 2)
}

expressionPanelColorScales <- function(genes, mode, shared_scale) {
  if (!length(genes)) {
    return(list())
  }
  if (!identical(mode, "different")) {
    return(stats::setNames(
      rep(list(expressionColorScale(shared_scale)), length(genes)),
      genes
    ))
  }
  high <- c(
    "#b2182b",
    "#2166ac",
    "#1b7837",
    "#762a83",
    "#e08214",
    "#008080",
    "#c51b7d",
    "#7f3b08",
    "#4d4d4d"
  )
  scales <- lapply(seq_along(genes), function(i) {
    colors <- grDevices::colorRampPalette(c("#d9dde0", high[[i]]))(5)
    Map(list, seq(0, 1, length.out = length(colors)), colors)
  })
  stats::setNames(scales, genes)
}

expressionReverseColorScale <- function(name) {
  !identical(name, "Cerebro orange")
}

expressionFillScale <- function(name, limits) {
  guide <- ggplot2::guide_colorbar(
    frame.colour = "black",
    ticks.colour = "black"
  )
  common <- list(
    limits = limits,
    oob = scales::squish,
    name = "Log-normalised\nexpression",
    guide = guide
  )
  if (identical(name, "Cerebro orange")) {
    return(do.call(
      ggplot2::scale_fill_gradientn,
      c(
        list(
          colours = vapply(
            expressionColorScale(name),
            `[[`,
            character(1),
            2L
          ),
          values = vapply(
            expressionColorScale(name),
            `[[`,
            numeric(1),
            1L
          )
        ),
        common
      )
    ))
  }
  if (identical(tolower(name), "viridis")) {
    return(do.call(
      ggplot2::scale_fill_viridis_c,
      c(list(option = "viridis", direction = -1), common)
    ))
  }
  do.call(
    ggplot2::scale_fill_distiller,
    c(list(palette = name, direction = 1), common)
  )
}
