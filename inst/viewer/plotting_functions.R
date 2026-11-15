##----------------------------------------------------------------------------##
## Shared chart theme (single source of truth for R plots).
##
## Mirrors the Viewer palette used by www/cell_views.js and custom.css.
## tokens. Warm neutrals reused from the app --c-* palette so every plotly /
## ggplot chart reads as the same design system. Keep these three in sync.
##----------------------------------------------------------------------------##
cerebro_plotly_theme <- function() {
  list(
    font = paste0(
      '"Segoe UI Variable", "Segoe UI", -apple-system, BlinkMacSystemFont, ',
      '"Helvetica Neue", Arial, sans-serif'
    ),
    grid = "#ececec", # --chart-grid  (= --c-border)
    axis = "#e0e0e0", # --chart-axis  (= --c-border-2)
    tick = "#6b6b70", # --chart-tick  (= --c-text-2)
    title = "#1c1c1e", # --chart-title (= --c-text)
    signal = "#2f6fd6", # --chart-signal (= --c-blue)
    accent = "#f97316", # --chart-accent (= --c-amber)
    up = "#4c9a6b", # --chart-up   (muted green)
    down = "#c05b5b", # --chart-down (muted brick)
    hover_bg = "rgba(255, 255, 255, 0.95)",
    transparent = "rgba(255, 255, 255, 0)"
  )
}

## Plotly axis list styled like the shared projection axes. `mirror` defaults to
## TRUE (four-sided frame, matches the interactive scatter); pass FALSE for
## static analytic charts (violin / bar) to drop the top+right spines, the
## Nature-style minimalist look.
cerebro_plotly_axis <- function(title = "", mirror = TRUE, ...) {
  th <- cerebro_plotly_theme()
  c(
    list(
      title = title,
      mirror = mirror,
      showline = TRUE,
      zeroline = FALSE,
      gridcolor = th$grid,
      linecolor = th$axis,
      tickfont = list(color = th$tick, family = th$font),
      titlefont = list(color = th$title, family = th$font)
    ),
    list(...)
  )
}

## Plotly hoverlabel styled like the shared projection hover.
cerebro_plotly_hoverlabel <- function() {
  th <- cerebro_plotly_theme()
  list(
    font = list(size = 12, color = th$title, family = th$font),
    bgcolor = th$hover_bg,
    bordercolor = th$grid,
    align = "left"
  )
}

## Shared modebar configuration for native Plotly charts. The custom cell-view
## renderer exposes the same useful actions with its own toolbar.
cerebro_plotly_toolbar <- function(fig) {
  plotly::config(
    plotly::layout(fig, dragmode = "lasso"),
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtons = list(list(
      "select2d",
      "lasso2d",
      "pan2d",
      "zoomIn2d",
      "zoomOut2d",
      "resetScale2d",
      "toImage"
    ))
  )
}

##----------------------------------------------------------------------------##
## Violin summaries, e.g. for expression metrics.
##----------------------------------------------------------------------------##
## stats::quantile(x, probs, type, names = FALSE) for sorted finite `x`, with
## the same arithmetic but without sorting `x` again or formatting names.
sortedQuantile <- function(x, probs, type) {
  n <- length(x)
  floor_fuzzy <- function(np) floor(np * (1 + 4 * .Machine$double.eps))
  if (type == 7L) {
    index <- 1 + (n - 1) * probs
    lo <- floor_fuzzy(index)
    qs <- x[lo]
    i <- which(index > lo & x[ceiling(index)] != qs)
    h <- (index - lo)[i]
    qs[i] <- (1 - h) * qs[i] + h * x[ceiling(index)[i]]
    return(qs)
  }
  ## Type 8: a = b = 1/3; x is padded with its ends as in stats.
  nppm <- 1 / 3 + probs * (n + 1 - 1 / 3 - 1 / 3)
  j <- floor_fuzzy(nppm)
  h <- nppm - j
  h[abs(h) < 4 * .Machine$double.eps] <- 0
  lower <- x[pmin(pmax(j, 1), n)]
  upper <- x[pmin(j + 1, n)]
  qs <- lower
  qs[h == 1] <- upper[h == 1]
  other <- 0 < h & h < 1 & lower != upper
  qs[other] <- ((1 - h) * lower + h * upper)[other]
  qs
}

compactViolinData <- function(
  table,
  metric,
  coloring_variable,
  max_points_per_group = 4096L
) {
  previous <- attr(table, "cerebro_violin_summary", exact = TRUE)
  if (
    is.list(previous) &&
      identical(previous$metric, metric) &&
      identical(previous$group, coloring_variable)
  ) {
    return(table)
  }
  values <- table[[metric]]
  groups <- table[[coloring_variable]]
  valid <- is.finite(values) & !is.na(groups)
  ## drop = TRUE would build a factor again; empty groups are dropped below.
  rows_by_group <- split(which(valid), groups[valid])
  rows_by_group <- rows_by_group[lengths(rows_by_group) > 0L]
  ## Mid-quantiles approximate density on the original measurement scale.
  ## Never shift/rescale observations to force a target mean.
  probs <- (seq_len(max_points_per_group) - .5) / max_points_per_group
  ## Each group is sorted once for its box and its sample.
  summaries <- lapply(rows_by_group, function(rows) {
    x <- values[rows]
    sorted <- sort.int(x, method = "radix")
    n <- length(x)
    q <- sortedQuantile(sorted, c(.25, .5, .75), 7L)
    iqr <- q[3L] - q[1L]
    list(
      stats = list(
        group = as.character(groups[rows[1L]]),
        n = n,
        mean = mean(x),
        q1 = q[1L],
        median = q[2L],
        q3 = q[3L],
        lowerfence = min(sorted[sorted >= q[1L] - 1.5 * iqr]),
        upperfence = max(sorted[sorted <= q[3L] + 1.5 * iqr]),
        minimum = sorted[1L],
        maximum = sorted[n],
        approximate = n > max_points_per_group
      ),
      rows = if (n > max_points_per_group) {
        rep(rows[1L], max_points_per_group)
      } else {
        rows
      },
      values = if (n > max_points_per_group) {
        sortedQuantile(sorted, probs, 8L)
      } else {
        x
      }
    )
  })
  part <- function(name, parts = summaries) lapply(parts, `[[`, name)
  stats <- part("stats")
  result <- table[
    as.integer(unlist(part("rows"), use.names = FALSE)),
    c(coloring_variable, metric),
    drop = FALSE
  ]
  if (length(summaries)) {
    result[[metric]] <- unlist(part("values"), use.names = FALSE)
  }
  rownames(result) <- NULL
  attr(result, "cerebro_violin_summary") <- list(
    version = 1L,
    metric = metric,
    group = coloring_variable,
    stats = if (length(stats)) {
      as.data.frame(
        lapply(
          stats::setNames(nm = names(stats[[1L]])),
          function(name) unlist(part(name, stats), use.names = FALSE)
        ),
        row.names = names(stats)
      )
    } else {
      data.frame()
    }
  )
  result
}

##----------------------------------------------------------------------------##
## Charts drawn in the browser (www/cerebro-charts.js).
##
## The server sends small columnar payloads; the browser draws them as SVG (a
## canvas for scatter points). Hover text, axis formatting, legend toggles and
## downloads happen there, so R only aggregates.
##----------------------------------------------------------------------------##

cerebroChartOutput <- function(outputId, height = NULL) {
  shiny::div(
    id = outputId,
    class = "cerebro-chart",
    style = if (!is.null(height)) {
      paste0("height: ", shiny::validateCssUnit(height), ";")
    }
  )
}

## A Shiny render function whose value is a plain list. It carries a cache hint,
## so cachePlot() (bindCache) caches it like the built-in renderers.
renderCerebroChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  func <- shiny::exprToFunction(expr, env, quoted = TRUE)
  shiny::createRenderFunction(
    func,
    function(value, session, name, ...) value,
    cerebroChartOutput,
    cacheHint = list(label = "renderCerebroChart", userExpr = expr)
  )
}

## Levels that occur in `values`, in factor-level order (sorted otherwise).
cerebroChartLevels <- function(values) {
  values <- values[!is.na(values)]
  if (is.factor(values)) {
    return(levels(droplevels(values)))
  }
  sort(unique(as.character(values)))
}

## Colours in `levels` order; a level without a colour gets a neutral grey.
cerebroChartColors <- function(colors, levels) {
  if (is.null(colors)) {
    colors <- character()
  }
  colors <- unname(colors[match(levels, names(colors))])
  colors[is.na(colors)] <- "#898989"
  I(colors)
}

## Colours of `values` on a colour scale given as list(list(position, colour)),
## stretched over the range of the values like a Plotly marker colour scale.
cerebroScaleColors <- function(values, stops, reverse = FALSE) {
  positions <- vapply(stops, function(stop) as.numeric(stop[[1L]]), 0)
  channels <- grDevices::col2rgb(vapply(
    stops,
    function(stop) as.character(stop[[2L]]),
    ""
  ))
  limits <- range(values, finite = TRUE)
  at <- if (all(is.finite(limits)) && limits[2L] > limits[1L]) {
    (values - limits[1L]) / (limits[2L] - limits[1L])
  } else {
    rep(1, length(values))
  }
  if (isTRUE(reverse)) {
    at <- 1 - at
  }
  mixed <- vapply(
    1:3,
    function(k) stats::approx(positions, channels[k, ], at, rule = 2)$y,
    numeric(length(at))
  )
  grDevices::rgb(matrix(mixed, ncol = 3L), maxColorValue = 255)
}

## Stacked bar chart of a long composition table (calculateTableAB()).
## `values[[i]][j]` is the count (or fraction) of series i in category j.
cerebroBarChartData <- function(table, first, second, colors, percent) {
  percent <- isTRUE(percent)
  keep <- !is.na(table[[first]]) & !is.na(table[[second]])
  categories <- cerebroChartLevels(table[[first]][keep])
  series <- cerebroChartLevels(table[[second]][keep])
  values <- matrix(0, nrow = length(series), ncol = length(categories))
  values[cbind(
    match(as.character(table[[second]][keep]), series),
    match(as.character(table[[first]][keep]), categories)
  )] <- table[["count"]][keep]
  if (percent) {
    values <- signif(values, 6)
  }
  list(
    type = "bar",
    categories = I(categories),
    series = I(series),
    colors = cerebroChartColors(colors, series),
    values = lapply(seq_along(series), function(i) I(values[i, ])),
    percent = percent,
    y_title = if (percent) "Percent of cells" else "Number of cells",
    legend_title = second
  )
}

## One bar per category in its own colour (no legend). `format` is "integer"
## (counts) or "number" (three significant digits).
cerebroCountBarData <- function(
  categories,
  values,
  colors,
  y_title = "Number of cells",
  format = "integer",
  outline = NULL
) {
  categories <- as.character(categories)
  list(
    type = "bar",
    categories = I(categories),
    series = I(y_title),
    category_colors = if (is.null(names(colors))) {
      I(unname(as.character(colors)))
    } else {
      cerebroChartColors(colors, categories)
    },
    values = list(I(as.numeric(values))),
    percent = FALSE,
    format = format,
    outline = outline,
    y_title = y_title
  )
}

## Vertical two-level Sankey: `first` levels on top, `second` levels below.
## Links are zero-based indices into `sources` and `targets`.
cerebroSankeyData <- function(
  table,
  first,
  second,
  colors_first,
  colors_second
) {
  keep <- !is.na(table[[first]]) &
    !is.na(table[[second]]) &
    table[["count"]] > 0
  sources <- cerebroChartLevels(table[[first]][keep])
  targets <- cerebroChartLevels(table[[second]][keep])
  list(
    type = "sankey",
    first_title = first,
    second_title = second,
    sources = I(sources),
    source_colors = cerebroChartColors(colors_first, sources),
    targets = I(targets),
    target_colors = cerebroChartColors(colors_second, targets),
    link_source = I(match(as.character(table[[first]][keep]), sources) - 1L),
    link_target = I(match(as.character(table[[second]][keep]), targets) - 1L),
    link_value = I(as.numeric(table[["count"]][keep]))
  )
}

## Kernel density over [lo, hi], scaled to a maximum of 1 (every violin has
## the same width). Silverman's rule as Plotly uses for violins. `lo` and `hi`
## are the exact range of the group: a compact sample stops short of it.
cerebroViolinDensity <- function(
  values,
  points = 96L,
  lo = min(values),
  hi = max(values)
) {
  n <- length(values)
  empty <- list(lo = lo, hi = hi, density = I(numeric()))
  if (n < 2L || !(hi > lo)) {
    return(empty)
  }
  quartiles <- stats::quantile(values, c(.25, .75), names = FALSE, type = 7)
  spread <- min(stats::sd(values), (quartiles[2L] - quartiles[1L]) / 1.349)
  if (!(spread > 0)) {
    spread <- stats::sd(values)
  }
  bandwidth <- 1.059 * spread * n^(-0.2)
  if (!is.finite(bandwidth) || !(bandwidth > 0)) {
    return(empty)
  }
  density <- stats::density(
    values,
    bw = bandwidth,
    n = points,
    from = lo,
    to = hi
  )$y
  list(lo = lo, hi = hi, density = I(signif(density / max(density), 3)))
}

## Violins from the compact table and its exact summary (compactViolinData());
## boxes and means describe all finite observations. `mode` is "integer",
## "percent" (values in 0-1) or "number". `fill` gives every violin one colour.
cerebroViolinData <- function(
  table,
  metric,
  coloring_variable,
  colors,
  y_title,
  mode,
  points = 96L,
  max_points = 4096L,
  title = NULL,
  fill = NULL
) {
  table <- compactViolinData(table, metric, coloring_variable, max_points)
  summary <- attr(table, "cerebro_violin_summary", exact = TRUE)$stats
  chart <- list(type = "violin", mode = mode, y_title = y_title, title = title)
  if (is.null(summary) || !nrow(summary)) {
    return(c(chart, list(groups = I(character()))))
  }
  groups <- as.character(summary$group)
  rows <- split(
    seq_len(nrow(table)),
    factor(as.character(table[[coloring_variable]]), levels = groups)
  )
  values <- table[[metric]]
  column <- function(name) I(unname(summary[[name]]))
  c(
    chart,
    list(
      groups = I(groups),
      colors = if (is.null(fill)) {
        cerebroChartColors(colors, groups)
      } else {
        I(rep(as.character(fill), length(groups)))
      },
      n = column("n"),
      mean = column("mean"),
      q1 = column("q1"),
      median = column("median"),
      q3 = column("q3"),
      lowerfence = column("lowerfence"),
      upperfence = column("upperfence"),
      minimum = column("minimum"),
      maximum = column("maximum"),
      approximate = column("approximate"),
      violins = unname(lapply(seq_along(groups), function(i) {
        cerebroViolinDensity(
          values[rows[[i]]],
          points,
          summary$minimum[[i]],
          summary$maximum[[i]]
        )
      }))
    )
  )
}

## Violin panels side by side, three per row, on one value axis. Each panel is
## a cerebroViolinData() payload; its `title` labels the panel.
cerebroViolinPanels <- function(panels, y_title, mode) {
  list(
    type = "violin",
    mode = mode,
    y_title = y_title,
    panels = unname(panels)
  )
}

## Filled lines (densities): `series` is a list of list(name, color, x, y).
cerebroLineChartData <- function(
  series,
  x_title,
  y_title,
  x_range = NULL,
  fill_opacity = 0.5,
  legend_title = NULL,
  name_prefix = ""
) {
  list(
    type = "lines",
    x_title = x_title,
    y_title = y_title,
    x_range = if (!is.null(x_range)) I(as.numeric(x_range)),
    fill_opacity = fill_opacity,
    legend_title = legend_title,
    name_prefix = name_prefix,
    series = I(vapply(series, function(s) as.character(s$name), "")),
    colors = I(vapply(series, function(s) as.character(s$color), "")),
    x = unname(lapply(series, function(s) I(signif(s$x, 6)))),
    y = unname(lapply(series, function(s) I(signif(s$y, 4))))
  )
}

## Float32 (or Int32) values as base64: a million points stay a few MB.
cerebroChartBinary <- function(values, integer = FALSE) {
  bytes <- if (integer) {
    writeBin(as.integer(values), raw(), size = 4L, endian = "little")
  } else {
    writeBin(as.numeric(values), raw(), size = 4L, endian = "little")
  }
  jsonlite::base64_enc(bytes)
}

## Points drawn on a canvas, coloured by `groups` (a factor). `hover` is a named
## list of extra per-point columns for the tooltip (numbers, or labels for small
## data). `trend` is an optional list(x, y, color, width) line.
cerebroScatterData <- function(
  x,
  y,
  groups,
  colors,
  x_title,
  y_title,
  group_title = "",
  size = 5,
  opacity = 1,
  hover = list(),
  trend = NULL
) {
  groups <- as.factor(groups)
  levels <- levels(groups)
  codes <- as.integer(groups) - 1L
  codes[is.na(codes)] <- -1L
  numeric_hover <- Filter(is.numeric, hover)
  label_hover <- Filter(Negate(is.numeric), hover)
  list(
    type = "scatter",
    n = length(x),
    x = cerebroChartBinary(x),
    y = cerebroChartBinary(y),
    group = cerebroChartBinary(codes, integer = TRUE),
    levels = I(levels),
    colors = cerebroChartColors(colors, levels),
    x_title = x_title,
    y_title = y_title,
    group_title = group_title,
    size = size,
    opacity = opacity,
    hover_names = I(names(numeric_hover)),
    hover_values = unname(lapply(numeric_hover, cerebroChartBinary)),
    label_names = I(names(label_hover)),
    label_values = unname(lapply(label_hover, function(v) I(as.character(v)))),
    trend = if (!is.null(trend)) {
      list(
        x = I(signif(trend$x, 6)),
        y = I(signif(trend$y, 6)),
        color = trend$color,
        width = trend$width
      )
    }
  )
}
