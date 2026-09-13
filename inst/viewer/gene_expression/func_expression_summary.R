expressionSummaryMode <- function(display_mode, gene_count, dimensions = 2) {
  if (identical(display_mode, "rgb")) {
    return("rgb")
  }
  if (
    identical(display_mode, "separate") &&
      identical(dimensions, 2L) &&
      gene_count >= 2 &&
      gene_count <= 9
  ) {
    return("separate")
  }
  "mean"
}

expressionSummarySpec <- function(
  display_mode,
  genes,
  rgb_genes = list(),
  dimensions = 2
) {
  genes <- unique(as.character(genes))
  genes <- genes[!is.na(genes) & nzchar(genes)]

  if (length(genes) == 0) {
    return(list(kind = "empty", series = list()))
  }

  display_mode <- expressionSummaryMode(
    display_mode,
    length(genes),
    as.integer(dimensions)
  )

  if (identical(display_mode, "rgb")) {
    channel_colors <- c(r = "#dc2626", g = "#16a34a", b = "#2563eb")
    series <- lapply(names(channel_colors), function(channel) {
      gene <- rgb_genes[[channel]]
      if (is.null(gene) || length(gene) != 1 || !gene %in% genes) {
        return(NULL)
      }
      list(
        label = paste0(toupper(channel), " · ", gene),
        key = channel,
        genes = gene,
        color = unname(channel_colors[[channel]])
      )
    })
    return(list(kind = "rgb", series = Filter(Negate(is.null), series)))
  }

  if (identical(display_mode, "separate") && length(genes) > 1) {
    return(list(
      kind = "separate",
      series = lapply(genes, function(gene) {
        list(label = gene, key = gene, genes = gene, color = NULL)
      })
    ))
  }

  list(
    kind = "mean",
    series = list(list(
      label = if (length(genes) == 1) {
        genes[[1]]
      } else {
        paste0("Mean expression (", length(genes), " genes)")
      },
      key = NULL,
      genes = genes,
      color = NULL
    ))
  )
}

plotExpressionSummary <- function(series, groups, colors) {
  plots <- lapply(series, function(item) {
    plot_data <- data.frame(
      group = groups,
      level = item$values,
      stringsAsFactors = FALSE
    )
    plot_data <- compactViolinData(plot_data, "level", "group")
    if (is.null(item$color)) {
      plot <- plotly::plot_ly(
        plot_data,
        x = ~group,
        y = ~level,
        type = "violin",
        color = ~group,
        colors = colors,
        source = "subset",
        showlegend = FALSE,
        hoverinfo = "y",
        marker = list(size = 5),
        box = list(visible = TRUE),
        meanline = list(visible = TRUE)
      )
    } else {
      plot <- plotly::plot_ly(
        plot_data,
        x = ~group,
        y = ~level,
        type = "violin",
        fillcolor = item$color,
        line = list(color = item$color),
        source = "subset",
        showlegend = FALSE,
        hoverinfo = "y",
        marker = list(size = 5, color = item$color),
        box = list(visible = TRUE),
        meanline = list(visible = TRUE)
      )
    }
    plotly::layout(
      plot,
      title = "",
      xaxis = cerebro_plotly_axis(
        title = if (length(series) == 1) "" else item$label,
        mirror = FALSE
      ),
      yaxis = cerebro_plotly_axis(
        title = "Expression level",
        mirror = FALSE,
        hoverformat = ".2f"
      ),
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      dragmode = "lasso",
      hovermode = "compare"
    )
  })

  plot <- if (length(plots) == 1) {
    plots[[1]]
  } else {
    plotly::subplot(
      plots,
      nrows = ceiling(length(plots) / 3),
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.05
    )
  }
  cerebro_plotly_toolbar(plot)
}
