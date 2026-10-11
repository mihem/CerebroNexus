## Pure display settings shared by Builder and Viewer. Missing legacy fields
## resolve here without rewriting a checkpoint's saved configuration.
expressionPaletteChoices <- function() {
  c(
    "Cerebro orange" = "cerebro_orange",
    "Blues" = "blues",
    "Greens" = "greens",
    "Viridis" = "viridis"
  )
}

expressionColorSettings <- function(value = NULL) {
  defaults <- list(
    palette = "cerebro_orange",
    panel_mode = "shared",
    panel_palette = "default"
  )
  choices <- list(
    palette = unname(expressionPaletteChoices()),
    panel_mode = c("shared", "distinct"),
    panel_palette = c("default", "soft")
  )
  if (!is.list(value) || is.object(value)) {
    return(defaults)
  }
  for (field in names(defaults)) {
    item <- value[[field]]
    if (
      is.character(item) &&
        !is.object(item) &&
        length(item) == 1L &&
        !is.na(item) &&
        item %in% choices[[field]]
    ) {
      defaults[[field]] <- item
    }
  }
  defaults
}

expressionColorSettingsValid <- function(value) {
  is.list(value) &&
    !is.object(value) &&
    identical(value, expressionColorSettings(value))
}

expressionColorScale <- function(name) {
  aliases <- expressionPaletteChoices()
  if (name %in% names(aliases)) {
    name <- unname(aliases[[name]])
  }
  colors <- switch(
    name,
    cerebro_orange = c("#aeb5bb", "#f7c89d", "#f49a4c", "#e75f25", "#9f251f"),
    blues = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    greens = c("#f7fcf5", "#c7e9c0", "#74c476", "#238b45", "#00441b"),
    viridis = c("#440154", "#3b528b", "#21918c", "#5ec962", "#fde725"),
    NULL
  )
  if (is.null(colors)) {
    return(name)
  }
  positions <- if (identical(name, "cerebro_orange")) {
    c(0, .08, .38, .7, 1)
  } else {
    seq(0, 1, length.out = length(colors))
  }
  Map(list, positions, colors)
}

expressionPanelColors <- function(palette = "default") {
  if (identical(palette, "soft")) {
    return(c(
      "#b86678",
      "#658eb4",
      "#72a48a",
      "#9980b2",
      "#c69561",
      "#66a6a6",
      "#ad80a0",
      "#ac8b76",
      "#899399"
    ))
  }
  c(
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
}

expressionValueRange <- function(expression_levels) {
  series <- if (is.list(expression_levels)) {
    expression_levels
  } else {
    list(expression_levels)
  }
  ranges <- lapply(series, function(values) {
    if (!length(values)) {
      return(NULL)
    }
    value_range <- suppressWarnings(range(values, finite = TRUE))
    if (any(!is.finite(value_range))) NULL else value_range
  })
  ranges <- Filter(Negate(is.null), ranges)
  if (!length(ranges)) {
    return(c(0, 1))
  }
  low <- min(vapply(ranges, `[[`, numeric(1), 1L))
  high <- max(vapply(ranges, `[[`, numeric(1), 2L))
  if (low == 0 && high == 0) c(0, 1) else round(c(low, high), digits = 2)
}

expressionPanelColorScales <- function(
  genes,
  mode,
  shared_scale,
  panel_palette = "default"
) {
  if (!length(genes)) {
    return(list())
  }
  if (!identical(mode, "different")) {
    return(stats::setNames(
      rep(list(expressionColorScale(shared_scale)), length(genes)),
      genes
    ))
  }
  high <- rep(expressionPanelColors(panel_palette), length.out = length(genes))
  scales <- lapply(seq_along(genes), function(i) {
    colors <- grDevices::colorRampPalette(c("#d9dde0", high[[i]]))(5)
    Map(list, seq(0, 1, length.out = length(colors)), colors)
  })
  stats::setNames(scales, genes)
}

expressionReverseColorScale <- function(name) {
  !name %in% c(names(expressionPaletteChoices()), expressionPaletteChoices())
}

viewerExpressionColorSettings <- function(options, dataset_key) {
  content <- options[["viewer_content"]]
  item <- if (is.list(content)) content[[dataset_key]] else NULL
  expressionColorSettings(
    if (is.list(item)) item[["gene_expression"]] else NULL
  )
}

expressionPanelModeInputId <- function(context) {
  paste0("expression_projection_gene_color_mode_", context$generation)
}
