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
