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
cerebro_plotly_toolbar <- function(
  fig,
  buttons = c(
    "select2d",
    "lasso2d",
    "pan2d",
    "zoomIn2d",
    "zoomOut2d",
    "resetScale2d",
    "toImage"
  )
) {
  if ("lasso2d" %in% buttons) {
    fig <- plotly::layout(fig, dragmode = "lasso")
  }
  plotly::config(
    fig,
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtons = list(as.list(buttons))
  )
}

##----------------------------------------------------------------------------##
## Violin plots with plotly, e.g. for expression metrics.
##----------------------------------------------------------------------------##
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
  rows_by_group <- split(which(valid), groups[valid], drop = TRUE)
  summaries <- lapply(rows_by_group, function(rows) {
    x <- values[rows]
    q <- unname(stats::quantile(x, c(.25, .5, .75), type = 7))
    iqr <- q[3L] - q[1L]
    data.frame(
      group = as.character(groups[rows[1L]]),
      n = length(x),
      mean = mean(x),
      q1 = q[1L],
      median = q[2L],
      q3 = q[3L],
      lowerfence = min(x[x >= q[1L] - 1.5 * iqr]),
      upperfence = max(x[x <= q[3L] + 1.5 * iqr]),
      minimum = min(x),
      maximum = max(x),
      approximate = length(x) > max_points_per_group
    )
  })
  compact <- lapply(rows_by_group, function(rows) {
    if (length(rows) <= max_points_per_group) {
      return(table[rows, c(coloring_variable, metric), drop = FALSE])
    }
    # Mid-quantiles approximate density on the original measurement scale.
    # Never shift/rescale observations to force a target mean.
    sampled <- unname(stats::quantile(
      values[rows],
      (seq_len(max_points_per_group) - .5) / max_points_per_group,
      type = 8
    ))
    result <- table[
      rep(rows[1L], max_points_per_group),
      c(coloring_variable, metric),
      drop = FALSE
    ]
    result[[metric]] <- sampled
    result
  })
  result <- if (length(compact)) {
    do.call(rbind, compact)
  } else {
    table[integer(), c(coloring_variable, metric), drop = FALSE]
  }
  rownames(result) <- NULL
  attr(result, "cerebro_violin_summary") <- list(
    version = 1L,
    metric = metric,
    group = coloring_variable,
    stats = if (length(summaries)) do.call(rbind, summaries) else data.frame()
  )
  result
}

# The density is approximate for large groups; boxes and means always use full
# finite observations. Suppress Plotly's statistics derived from the density.
addExactViolinSummary <- function(plot, table) {
  summary <- attr(table, "cerebro_violin_summary", exact = TRUE)$stats
  if (is.null(summary) || !nrow(summary)) {
    return(plot)
  }
  plotly::add_trace(
    plot,
    inherit = FALSE,
    type = "box",
    x = summary$group,
    q1 = summary$q1,
    median = summary$median,
    q3 = summary$q3,
    lowerfence = summary$lowerfence,
    upperfence = summary$upperfence,
    mean = summary$mean,
    boxmean = TRUE,
    boxpoints = FALSE,
    width = .12,
    fillcolor = "rgba(255,255,255,0.5)",
    line = list(color = "#555555"),
    name = "Exact summary",
    showlegend = FALSE,
    text = paste0(
      "n=",
      summary$n,
      "; min=",
      signif(summary$minimum, 6),
      "; max=",
      signif(summary$maximum, 6),
      ifelse(summary$approximate, "; density: quantile approximation", "")
    ),
    hoverinfo = "all"
  )
}

plotlyViolin <- function(
  table,
  metric,
  coloring_variable,
  colors,
  y_title,
  mode
) {
  if (mode == "percent") {
    y_range <- c(0, 1)
    y_tickformat <- ",.0%"
    y_hoverformat <- ",.1%"
  } else if (mode == "integer") {
    y_range <- NULL
    y_tickformat <- ",.0f"
    y_hoverformat <- ",.0f"
  }
  table <- compactViolinData(table, metric, coloring_variable)
  ##
  plot <- plotly::plot_ly(
    x = table[[coloring_variable]],
    y = table[[metric]],
    type = "violin",
    box = list(
      visible = FALSE
    ),
    meanline = list(
      visible = FALSE
    ),
    color = table[[coloring_variable]],
    colors = colors,
    source = "subset",
    showlegend = FALSE,
    hoverinfo = "y",
    marker = list(
      size = 5
    )
  ) %>%
    addExactViolinSummary(table) %>%
    plotly::layout(
      title = "",
      xaxis = cerebro_plotly_axis(title = "", mirror = FALSE),
      yaxis = cerebro_plotly_axis(
        title = y_title,
        mirror = FALSE,
        range = y_range,
        tickformat = y_tickformat,
        hoverformat = y_hoverformat
      ),
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      dragmode = "lasso",
      hovermode = "compare"
    )

  ##
  return(cerebro_plotly_toolbar(plot))
}

##----------------------------------------------------------------------------##
## Bar chart for composition plots.
##----------------------------------------------------------------------------##
plotlyBarChart <- function(
  table,
  first_grouping_variable,
  second_grouping_variable,
  colors,
  percent
) {
  if (percent == FALSE) {
    y_title <- "Number of cells"
    y_range <- NULL
    y_tickformat <- ",.0f"
    y_hoverformat <- ",.0f"
    hover_info <- glue::glue(
      "<b>{table[[ second_grouping_variable ]]}:</b> ",
      "{formatC(table[['count']], big.mark = ',')}"
    )
    ##
  } else if (percent == TRUE) {
    y_title <- "Percent of cells"
    y_range <- c(0, 1)
    y_tickformat <- ",.0%"
    y_hoverformat <- ".1%"
    hover_info <- glue::glue(
      "<b>{table[[ second_grouping_variable ]]}:</b> ",
      "{format(round(table[['count']]*100, 1), nsmall = 1)}%"
    )
  }
  ## generate plot
  plot <- table %>%
    plotly::plot_ly(
      x = ~ .[[first_grouping_variable]],
      y = ~count,
      type = "bar",
      color = ~ .[[second_grouping_variable]],
      colors = colors,
      hoverinfo = "text",
      hovertext = hover_info,
      textposition = "none"
    ) %>%
    plotly::layout(
      xaxis = cerebro_plotly_axis(title = "", mirror = FALSE),
      yaxis = cerebro_plotly_axis(
        title = y_title,
        mirror = FALSE,
        range = y_range,
        tickformat = y_tickformat,
        hoverformat = y_hoverformat
      ),
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      barmode = "stack",
      hovermode = "compare"
    )

  ##
  return(cerebro_plotly_toolbar(plot))
}

##----------------------------------------------------------------------------##
## Sankey plot for composition plots.
##----------------------------------------------------------------------------##
plotlySankeyPlot <- function(
  table,
  first_grouping_variable,
  second_grouping_variable,
  colors_for_groups
) {
  ## transform factor levels to integers (necessary for plotly)
  table[["source"]] <- as.numeric(table[[1]]) - 1
  table[["target"]] <- as.numeric(table[[2]]) - 1 + length(unique(table[[1]]))
  ## combine all factor levels in a single vector
  all_groups <- c(levels(table[[1]]), levels(table[[2]]))
  ## match color codes to group levels (from both groups)
  colors_for_groups_all <- colors_for_groups[
    names(colors_for_groups) %in% all_groups
  ]
  ## prepare plot
  plot <- plotly::plot_ly(
    type = "sankey",
    orientation = "v",
    valueformat = ".0f",
    node = list(
      label = all_groups,
      hovertemplate = paste0(
        "<b>%{label}</b><br>",
        "%{value:,.0f} cells",
        "<extra></extra>",
        collapse = ""
      ),
      color = colors_for_groups_all,
      pad = 15,
      thickness = 20,
      line = list(
        color = cerebro_plotly_theme()$axis,
        width = 0.5
      )
    ),
    link = list(
      source = table[["source"]],
      target = table[["target"]],
      value = table[[3]],
      hoverinfo = "all",
      hovertemplate = paste0(
        "<b>",
        first_grouping_variable,
        ":</b> %{source.label}<br>",
        "<b>",
        second_grouping_variable,
        ":</b> %{target.label}<br>",
        "<b>Number of cells:</b> %{value:,.0f}",
        "<extra></extra>",
        collapse = ""
      )
    )
  )
  ##
  return(cerebro_plotly_toolbar(plot, "toImage"))
}
