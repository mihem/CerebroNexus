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

## ggplot theme for the static PDF/SVG *exports* (the "export to PDF" buttons).
## Submission-grade defaults: a clean sans base family, hairline warm-neutral
## panel border and gridlines matching the on-screen Viewer palette, so the
## exported figure reads as the same design system as the interactive plot.
## `base_family = ""` lets the device pick its default sans (Helvetica/Arial on
## the common PDF/SVG devices), which keeps exported text editable in Illustrator
## and avoids a hard font dependency.
cerebro_export_theme <- function(base_size = 12) {
  th <- cerebro_plotly_theme()
  ggplot2::theme_bw(base_size = base_size, base_family = "") +
    ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = th$axis, fill = NA),
      panel.grid.major = ggplot2::element_line(colour = th$grid),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = th$tick),
      axis.title = ggplot2::element_text(colour = th$title),
      legend.key = ggplot2::element_blank()
    )
}

##----------------------------------------------------------------------------##
## Violin plots with plotly, e.g. for expression metrics.
##----------------------------------------------------------------------------##
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
  ##
  plot <- table %>%
    plotly::plot_ly(
      x = ~ .[[coloring_variable]],
      y = ~ .[[metric]],
      type = "violin",
      box = list(
        visible = TRUE
      ),
      meanline = list(
        visible = TRUE
      ),
      color = ~ .[[coloring_variable]],
      colors = colors,
      source = "subset",
      showlegend = FALSE,
      hoverinfo = "y",
      marker = list(
        size = 5
      )
    ) %>%
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
      dragmode = "select",
      hovermode = "compare"
    )

  ##
  return(plot)
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
      text = hover_info
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
  return(plot)
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
  return(plot)
}
