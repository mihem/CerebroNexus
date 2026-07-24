## ---- Plot-panel height --------------------------------------------------- ##
## Every single-panel IR plot fills the viewport through the shared
## `.cerebro-fill` mechanism (www/fill_height.js), the same one the HLA network
## and the projection pages use — no per-plot height constant to keep in sync
## with the chrome above it. The two hand-measured `calc(100vh - Npx)` constants
## that used to live here are gone. Faceted variants stay on an explicit pixel
## height because they are deliberately tall and meant to scroll.

## Static single plot tab body. `plotly = TRUE` emits an interactive
## plotlyOutput (zoom/pan/hover) instead of a static plotOutput.
##
## Height comes from the app-wide fill mechanism (www/fill_height.js), the SAME
## one the HLA network uses: the `.cerebro-fill` wrapper is sized to the viewport
## minus its live top offset, and the plot renders at height:100% inside it.
## This replaces the old `calc(100vh - 250px)` — a hand-measured chrome constant
## that over-reserved height (so the panel ballooned past the screen on first
## paint, then snapped smaller). One mechanism now, one behaviour, no magic
## number to keep in sync with the chrome.
ir_fill_plot <- function(
  id,
  spinner = TRUE,
  plotly = FALSE,
  height = NULL
) {
  # height = NULL (the common case) -> fill the viewport via the shared
  # `.cerebro-fill` mechanism. An explicit height is for the faceted plots that
  # are DELIBERATELY tall (one panel per sample) and meant to scroll; those are
  # rendered as-is, never fill'd.
  fill <- is.null(height)
  plot <- if (plotly) {
    plotly::plotlyOutput(id, height = if (fill) "100%" else height)
  } else {
    plotOutput(id, height = if (fill) "100%" else height)
  }
  if (spinner) {
    plot <- shinycssloaders::withSpinner(plot)
  }
  if (fill) tags$div(class = "cerebro-fill", plot) else plot
}

## Wrap an already-built output (e.g. a uiOutput whose server side computes a
## facet-aware pixel height) — passed through unchanged for now.
ir_fill_wrap <- function(output) {
  output
}

## ---- Visualizations UI ------------------------------------------------ ##
output$ir_visualizations_UI <- renderUI({
  if (!has_scRepertoire()) {
    return(ir_scRepertoire_missing_ui())
  }
  data <- ir_data()
  if (is.null(data)) {
    return(div(
      class = "alert alert-warning",
      "No immune repertoire data available."
    ))
  }

  ## Priority tabs (per performance-notes order). Abundance leads so the first
  ## view is a common overview plot rather than the sample-comparison Scatter,
  ## which is moved in with the other multi-sample tabs below.
  priority_tabs <- list(
    tabPanel(
      # Clonal expansion overlaid on the cell UMAP — the default landing tab,
      # so the first thing the user sees is where expanded clones sit.
      "Clonal UMAP",
      # Reserve a viewport-proportional placeholder for the spinner so the
      # container does not collapse to the ~400px default and snap up when the
      # plot arrives. 60vh matches the non-faceted plotly's own placeholder (and
      # the projection pages'), so the placeholder and the settled height are
      # close — no visible jump on first open.
      shinycssloaders::withSpinner(
        uiOutput("ir_ui_clonalUMAP"),
        proxy.height = "60vh"
      )
    ),
    tabPanel(
      "Abundance",
      ir_fill_plot("ir_plot_clonalAbundance", plotly = TRUE)
    ),
    tabPanel(
      "Diversity",
      ir_fill_plot("ir_plot_clonalDiversity", plotly = TRUE)
    ),
    tabPanel(
      "Homeostasis",
      ir_fill_plot("ir_plot_clonalHomeostasis", plotly = TRUE)
    ),
    tabPanel(
      "Isotype",
      ir_fill_plot("ir_plot_isotype", plotly = TRUE)
    ),
    # Hidden by default (kept available; renderer/help/param_spec retained).
    # tabPanel(
    #   "SHM Proxy",
    #   ir_fill_plot("ir_plot_shmProxy")
    # ),
    tabPanel(
      "Paired Scatter",
      ir_fill_wrap(shinycssloaders::withSpinner(uiOutput(
        "ir_ui_pairedScatter"
      )))
    ),
    # Hidden by default (kept available; renderer/help/param_spec retained).
    # The clone-definition waterfall is an exploratory tool for choosing a
    # clone-call resolution, not a finalised figure for a reader, so it is not
    # in the default tab strip; uncomment to re-enable.
    # tabPanel(
    #   "Definition",
    #   ir_fill_plot("ir_plot_cloneDefinition", plotly = TRUE)
    # ),
    tabPanel(
      "Clone Sharing",
      ir_fill_plot("ir_plot_cloneSharing", plotly = TRUE)
    )
  )

  ## Remaining tabs. Most are hidden by default to keep the tab strip focused
  ## on the commonly used plots; their renderers, help, and param_spec entries
  ## are retained so any of them can be re-enabled by uncommenting its tabPanel.
  other_tabs <- list(
    # tabPanel(
    #   "Length",
    #   ir_fill_plot("ir_plot_clonalLength")
    # ),
    # tabPanel(
    #   "Proportion",
    #   ir_fill_plot("ir_plot_clonalProportion")
    # ),
    # tabPanel(
    #   "Quant",
    #   ir_fill_plot("ir_plot_clonalQuant")
    # ),
    # tabPanel("Rarefaction", ir_fill_wrap(uiOutput("ir_ui_clonalRarefaction"))),
    # tabPanel("Gene usage", ir_fill_wrap(uiOutput("ir_ui_percentGeneUsage"))),
    # tabPanel("vizGenes", ir_fill_wrap(uiOutput("ir_ui_vizGenes"))),
    # tabPanel("percentGenes", ir_fill_wrap(uiOutput("ir_ui_percentGenes"))),
    # tabPanel("percentVJ", ir_fill_wrap(uiOutput("ir_ui_percentVJ"))),
    # tabPanel("AA %", ir_fill_wrap(uiOutput("ir_ui_percentAA"))),
    # tabPanel(
    #   "Entropy",
    #   ir_fill_plot("ir_plot_positionalEntropy")
    # ),
    # tabPanel("Property", ir_fill_wrap(uiOutput("ir_ui_positionalProperty"))),
    # tabPanel(
    #   # Top motifs now lives in the settings panel (IR_PARAM_SPEC "K-mer").
    #   "K-mer",
    #   ir_fill_wrap(uiOutput("ir_ui_percentKmer"))
    # )
  )

  ## Tabs requiring >= 2 samples
  if (n_samples() >= 2) {
    other_tabs <- c(
      other_tabs,
      list(
        # tabPanel(
        #   "Scatter",
        #   helpText(
        #     "Compares clonotype proportions between the two groups selected",
        #     "below. Use 'Group by' to choose the grouping; the X/Y selectors",
        #     "then pick which two groups to compare."
        #   ),
        #   ir_fill_plot("ir_plot_clonalScatter")
        # ),
        tabPanel(
          "Compare",
          ir_fill_plot("ir_plot_clonalCompare", plotly = TRUE)
        ),
        # tabPanel(
        #   "Overlap",
        #   ir_fill_plot("ir_plot_clonalOverlap")
        # ),
        tabPanel(
          "SizeDist",
          ir_fill_plot("ir_plot_clonalSizeDistribution", plotly = TRUE)
        )
      )
    )
  }

  tabs <- c(priority_tabs, other_tabs)

  # Preserve the user's current tab across rebuilds (when this renderUI
  # re-runs). Without `selected`, the rebuilt tabsetPanel would reset to the
  # first tab (Abundance).
  tab_names <- vapply(tabs, function(t) t$attribs$`data-value`, character(1))
  last_tab <- isolate(ir_last_tab())
  selected_tab <- if (!is.null(last_tab) && last_tab %in% tab_names) {
    last_tab
  } else {
    NULL
  }

  do.call(
    tabsetPanel,
    c(list(id = "ir_tabs", selected = selected_tab), tabs)
  )
})

##------------------------------------------------------------------------##
## Plot renderers
##------------------------------------------------------------------------##

## ---- Clonal UMAP -------------------------------------------------------- ##
## Overlays clone expansion level on the cell projection (UMAP/tSNE). Data is
## built in data.R (ir_clonal_umap_data); here we draw the coloured scatter.
## Point size / opacity come from the generic display options; font size and
## title are applied by safeRenderPlot via ir_apply_display.
## Expansion level is an ordinal magnitude (clone size increasing), so it gets a
## sequential ramp in the app's --c-blue family (matches the projection's
## continuous colourscale): larger (more expanded) clones read as a deeper blue.
## Keys must match the factor levels produced in data.R (IR_CLONE_LABELS).
## Named hcl.colors palette passed to scRepertoire plotting functions and the
## R-side diversity/compare fills. "Harmonic" is a low-saturation QUALITATIVE
## hcl palette (muted gold/green/blue) that reads calm and in-system — the right
## kind of palette for unordered groups. scRepertoire's `palette` arg only
## accepts an hcl.colors palette *name* (a string), so this must stay a name,
## not a vector.
IR_PALETTE <- "Harmonic"

IR_EXPANSION_COLORS <- stats::setNames(
  c("#c7dcf0", "#8bb8de", "#5e9bc7", "#2f6fd6", "#1d4ea0"),
  c(
    "Single (0 < X <= 1)",
    "Small (1 < X <= 5)",
    "Medium (5 < X <= 20)",
    "Large (20 < X <= 100)",
    "Hyperexpanded (100 < X)"
  )
)

## Shared projection look, mirrored for the Clonal UMAP so it reads as the same
## app as the Main / spatial / trajectory projections. Colours/font come from the
## single shared source cerebro_plotly_theme() (mirrors projection_layouts.js and
## the custom.css --chart-* tokens) instead of a private copy. The Clonal UMAP
## keeps its own engine (per-level traces, faceting, grey "Other cells") — only
## the layout styling is aligned.
IR_PROJECTION_FONT <- cerebro_plotly_theme()$font
IR_PROJECTION_STYLE <- cerebro_plotly_theme()

## Axis styled like the shared projection.
ir_projection_axis <- function() {
  list(
    autorange = TRUE,
    mirror = TRUE,
    showline = TRUE,
    zeroline = FALSE,
    gridcolor = IR_PROJECTION_STYLE$grid,
    linecolor = IR_PROJECTION_STYLE$axis,
    tickfont = list(
      color = IR_PROJECTION_STYLE$tick,
      family = IR_PROJECTION_FONT
    ),
    titlefont = list(
      color = IR_PROJECTION_STYLE$title,
      family = IR_PROJECTION_FONT
    )
  )
}

## Render a scRepertoire ggplot as an interactive plotly figure. safeRenderPlot
## still does the heavy lifting (display options, empty-state and error plots,
## silent-error re-raise); we just convert its ggplot result with ggplotly and
## fall back to a plotly message if conversion itself fails. Used by the simple
## bar/point tabs; plots that ggplotly cannot represent well (alluvial, custom
## facet/bootstrap renderers) keep renderPlot.
ir_render_ggplotly <- function(expr, plot_name, tooltip = NULL) {
  p <- safeRenderPlot(expr, plot_name)
  if (!inherits(p, "ggplot")) {
    return(ir_empty_plotly("This plot is not available for the current view."))
  }
  tryCatch(
    {
      # ggplotly() needs an open graphics device to measure text/layout. In the
      # renderPlotly context Shiny has not opened one, so on macOS the default
      # quartz device is requested with a zero size and errors with
      # "invalid quartz() device size". Open a throwaway null PDF device (no
      # platform device, no file) for the duration of the conversion.
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
      # No toWebGL() here: these are bar plots, which have no WebGL trace
      # equivalent — converting them only emits "don't have a WebGL equivalent"
      # and "'scattergl' object don't have 'hoveron'" warnings. WebGL is only
      # worth it for the large point cloud in the Clonal UMAP.
      #
      # `tooltip` restricts the hover to a specific aes (e.g. "text"): without
      # it ggplotly derives the tooltip from every mapped aes, which repeats the
      # fill variable and exposes raw column names (display, Freq).
      fig <- if (is.null(tooltip)) {
        plotly::ggplotly(p)
      } else {
        plotly::ggplotly(p, tooltip = tooltip)
      }
      # Match the projection tabs' modebar (theme "a"): no Plotly logo, curated
      # tools. Clonal UMAP already gets this via the shared scatter engine.
      ir_apply_theme_a_modebar(fig)
    },
    error = function(e) {
      ir_empty_plotly(paste("Plot conversion error:", conditionMessage(e)))
    }
  )
}

## Empty-state plotly figure with a centred message (used when there is nothing
## to draw), so the tab still shows an interactive canvas like the other UMAPs.
ir_empty_plotly <- function(msg) {
  plotly::plotly_empty(type = "scatter", mode = "markers") %>%
    plotly::layout(
      annotations = list(
        text = msg,
        showarrow = FALSE,
        font = list(size = 14, color = "#666666")
      )
    )
}

ir_umap_split_layout <- function(
  n_groups,
  width = NULL,
  height = NULL,
  min_px = 300L
) {
  n_groups <- max(1L, as.integer(n_groups %||% 1L))
  width <- suppressWarnings(as.numeric(width))
  height <- suppressWarnings(as.numeric(height))
  if (length(width) != 1 || is.na(width) || width <= 0) {
    width <- 900
  }
  if (length(height) != 1 || is.na(height) || height <= 0) {
    height <- 650
  }
  max_cols <- max(1L, min(n_groups, floor(width / min_px)))
  candidates <- seq_len(max_cols)
  layouts <- lapply(candidates, function(ncol) {
    nrow <- ceiling(n_groups / ncol)
    draw_height <- max(height, nrow * min_px)
    panel_px <- min(width / ncol, draw_height / nrow)
    fill_ratio <- n_groups / (ncol * nrow)
    data.frame(
      ncol = ncol,
      nrow = nrow,
      panel_px = panel_px,
      score = panel_px * fill_ratio
    )
  })
  layouts <- do.call(rbind, layouts)
  ok <- layouts$panel_px >= min_px
  choices <- if (any(ok)) layouts[ok, , drop = FALSE] else layouts
  best_score <- max(choices$score)
  choices <- choices[choices$score == best_score, , drop = FALSE]
  ncol <- max(choices$ncol)
  nrow <- ceiling(n_groups / ncol)
  draw_height <- max(height, nrow * min_px)
  panel_px <- min(width / ncol, draw_height / nrow)
  list(
    ncol = ncol,
    nrow = nrow,
    width = ceiling(width),
    height = ceiling(nrow * panel_px),
    panel_px = panel_px
  )
}

ir_umap_grouped_data <- function(df, group_by) {
  if (is.null(group_by) || !nzchar(group_by)) {
    return(df)
  }
  md <- tryCatch(getMetaData(), error = function(e) NULL)
  if (
    is.null(md) ||
      !("cell_barcode" %in% colnames(md)) ||
      !(group_by %in% colnames(md))
  ) {
    return(NULL)
  }
  idx <- match(df$barcode, md$cell_barcode)
  group_values <- as.character(md[[group_by]][idx])
  group_levels <- tryCatch(getGroupLevels(group_by), error = function(e) NULL)
  if (is.null(group_levels) || length(group_levels) == 0) {
    group_levels <- unique(group_values)
  }
  group_levels <- group_levels[
    !is.na(group_levels) & nzchar(group_levels) & group_levels %in% group_values
  ]
  df$.umap_group <- factor(group_values, levels = group_levels)
  df <- df[!is.na(df$.umap_group), , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }
  df
}

ir_umap_split_group_count <- function(group_by) {
  md <- tryCatch(getMetaData(), error = function(e) NULL)
  if (
    is.null(md) ||
      is.null(group_by) ||
      !nzchar(group_by) ||
      !("cell_barcode" %in% colnames(md)) ||
      !(group_by %in% colnames(md))
  ) {
    return(1L)
  }
  cells <- tryCatch(ir_umap_cells_to_show(), error = function(e) NULL)
  if (!is.null(cells)) {
    md <- md[md$cell_barcode %in% cells, , drop = FALSE]
  }
  vals <- as.character(md[[group_by]])
  vals <- vals[!is.na(vals) & nzchar(vals)]
  max(1L, length(unique(vals)))
}

ir_umap_client_px <- function(keys, fallback) {
  cd <- session$clientData
  vals <- vapply(
    keys,
    function(key) {
      suppressWarnings(as.numeric(cd[[key]] %||% NA_real_))
    },
    numeric(1)
  )
  vals <- vals[!is.na(vals) & vals > 0]
  if (length(vals) == 0) {
    return(fallback)
  }
  vals[[1]]
}

ir_umap_split_current_layout <- function(group_by) {
  layout <- ir_umap_split_layout(
    ir_umap_split_group_count(group_by),
    width = ir_umap_client_px(
      c(
        "output_ir_plot_clonalUMAP_static_width",
        "output_ir_ui_clonalUMAP_width",
        "output_ir_visualizations_UI_width"
      ),
      fallback = 900
    ),
    height = ir_umap_client_px(
      c(
        "output_ir_ui_clonalUMAP_height",
        "output_ir_visualizations_UI_height"
      ),
      fallback = 650
    )
  )
  layout
}

ir_umap_split_output_height <- function(group_by) {
  layout <- ir_umap_split_current_layout(group_by)
  ceiling(layout$height)
}

ir_clonal_umap_ggplot <- function(df, group_by, point_size, alpha, ncol) {
  bg <- df[is.na(df$expansion), , drop = FALSE]
  fg <- df[!is.na(df$expansion), , drop = FALSE]
  ggplot2::ggplot() +
    ggplot2::geom_point(
      data = bg,
      ggplot2::aes(x = .data$x, y = .data$y),
      colour = "grey85",
      size = point_size,
      alpha = alpha
    ) +
    ggplot2::geom_point(
      data = fg,
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$expansion),
      size = point_size,
      alpha = alpha
    ) +
    ggplot2::facet_wrap(stats::as.formula("~ .umap_group"), ncol = ncol) +
    ggplot2::scale_colour_manual(
      values = IR_EXPANSION_COLORS,
      drop = FALSE,
      name = "Clonotype"
    ) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "UMAP_1", y = "UMAP_2") +
    ggplot2::theme_classic() +
    ggplot2::theme(aspect.ratio = 1)
}

output$ir_ui_clonalUMAP <- renderUI({
  group_by <- ir_param("ir_p_umap_group_by", "")
  if (is.null(group_by) || !nzchar(group_by)) {
    ## Non-faceted: render through the shared projection-scatter engine (same
    ## host div + selection buttons as Main/spatial). The plot itself is drawn
    ## client-side by js$updateClonalUMAP into this plotlyOutput; the empty
    ## bootstrap renderPlotly below only creates the target div.
    return(ir_clonalUMAP_projection_ui())
  }
  ## Faceted: a group_by column is chosen. Faceting needs a multi-panel ggplot,
  ## which the single-canvas shared renderer cannot express, so this variant
  ## stays on the static plotOutput.
  ir_fill_plot(
    "ir_plot_clonalUMAP_static",
    spinner = FALSE,
    height = paste0(ir_umap_split_output_height(group_by), "px")
  )
})

## Shared-projection host for the non-faceted Clonal UMAP: the plotly div the
## shared renderer targets, plus the Clear/Zoom-to-selection buttons (hidden
## until a selection exists). Mirrors overview/UI_projection.R.
ir_clonalUMAP_projection_ui <- function() {
  tagList(
    div(
      class = "cerebro-projection-gate",
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(
          "ir_clonalUMAP_projection",
          width = "auto",
          # 60vh placeholder, same as the overview projection: the shared
          # renderer measures and resizes to the real viewport height, and the
          # projection reveal gate keeps it hidden until then. A placeholder
          # close to the settled height means no visible jump on reveal (the old
          # calc(100vh - 250px) started ~125px too tall and shrank in view).
          height = "60vh"
        ),
        type = 8,
        hide.ui = FALSE
      )
    ),
    div(
      class = "cerebro-selection-actions",
      style = "margin-top: 6px;",
      shinyjs::hidden(
        actionButton(
          inputId = "ir_clonalUMAP_projection_zoom_to_selection",
          label = "Zoom to selection",
          icon = icon("magnifying-glass-plus"),
          class = "btn-xs btn-default"
        )
      ),
      shinyjs::hidden(
        actionButton(
          inputId = "ir_clonalUMAP_projection_clear_selection",
          label = "Clear selection",
          icon = icon("eraser"),
          class = "btn-xs btn-default btn-breathing"
        )
      )
    )
  )
}

## Bootstrap the shared-projection host div. The shared renderer draws into this
## same plotly output via Plotly.react (js$updateClonalUMAP); this empty
## scattergl only creates the target div, exactly like overview/out_projection.R.
output$ir_clonalUMAP_projection <- plotly::renderPlotly({
  plotly::plot_ly(
    type = "scattergl",
    mode = "markers",
    source = "ir_clonalUMAP_projection"
  ) %>%
    plotly::layout(
      xaxis = ir_projection_axis(),
      yaxis = ir_projection_axis()
    )
})

## Draw the non-faceted Clonal UMAP through the shared projection-scatter engine.
## We marshal the same grey "Other cells" background + one-trace-per-expansion-
## level data the old renderPlotly built, but as the meta/data/hover arrays the
## shared render2DCategorical consumes, then hand off to JS. Runs only when no
## grouping column is chosen (the faceted variant uses the static ggplot below).
observe({
  group_by <- ir_param("ir_p_umap_group_by", "")
  ## Faceting is handled by the static ggplot path; nothing to push here.
  if (!is.null(group_by) && nzchar(group_by)) {
    return()
  }

  ## Depend on the host div's reported width so the render RE-FIRES once the div
  ## materialises. The host lives behind renderUI (emitted only when non-faceted),
  ## so unlike the always-present Main plot, the div may not exist when this first
  ## runs. clientData width is populated once the div is in the DOM and sized;
  ## requiring it both registers the dependency and avoids a react on a 0-size or
  ## absent div (which would draw blank or throw). Re-fires on faceted->non-
  ## faceted switches and tab re-open, and harmlessly on resize.
  plot_width <- session$clientData[["output_ir_clonalUMAP_projection_width"]]
  req(!is.null(plot_width) && plot_width > 0)

  receptor <- ir_param("ir_p_umap_receptor")
  projection <- ir_param("ir_p_umap_projection")
  clone_call <- "gene"
  show_all <- isTRUE(ir_param("ir_p_umap_show_all", TRUE))
  cells <- ir_umap_cells_to_show()
  df <- ir_clonal_umap_data(
    projection,
    receptor,
    clone_call,
    show_all = show_all,
    cells = cells
  )
  req(!is.null(df) && nrow(df) > 0)

  dp <- tryCatch(ir_display_params(), error = function(e) list())
  point_size <- suppressWarnings(as.numeric(dp[["ir_d_point_size"]]))
  if (length(point_size) != 1 || is.na(point_size)) {
    point_size <- 1
  }
  alpha <- suppressWarnings(as.numeric(dp[["ir_d_alpha"]]))
  if (length(alpha) != 1 || is.na(alpha)) {
    alpha <- 0.8
  }
  ## plotly marker sizes read larger than ggplot's; scale up so the points are
  ## comparable to the other UMAPs (matches the old renderer).
  marker_size <- point_size * 5

  legend_size <- suppressWarnings(as.numeric(dp[["ir_d_legend_size"]]))
  if (length(legend_size) != 1 || is.na(legend_size) || legend_size <= 0) {
    legend_size <- 12
  }
  ## Map the IR legend-position choice onto the shared renderer's legend modes:
  ## "top" -> the custom top bar (shared default); right/bottom/left -> plotly's
  ## native legend; "none" -> hidden. Default to the top bar for the unified look.
  legend_pos <- dp[["ir_d_legend_pos"]]
  if (!is.character(legend_pos) || length(legend_pos) != 1) {
    legend_pos <- "top"
  }
  legend_position <- switch(
    legend_pos,
    top = "top",
    right = "right",
    bottom = "bottom",
    left = "left",
    none = "none",
    "top"
  )

  ## Grey background = cells without the selected receptor (expansion = NA);
  ## coloured foreground = receptor cells with an expansion level. One trace per
  ## expansion level, in canonical order, so each keeps its turbo colour.
  bg <- df[is.na(df$expansion), , drop = FALSE]
  fg <- df[!is.na(df$expansion), , drop = FALSE]

  traces <- list()
  data_x <- list()
  data_y <- list()
  data_color <- list()
  hover_info <- list()
  hover_text <- list()

  if (nrow(bg) > 0) {
    traces[[length(traces) + 1]] <- "Other cells"
    data_x[[length(data_x) + 1]] <- bg$x
    data_y[[length(data_y) + 1]] <- bg$y
    data_color[[length(data_color) + 1]] <- "#D9D9D9"
    ## Background cells skip hover (per-trace hoverinfo, honoured by shared JS).
    hover_info[[length(hover_info) + 1]] <- "skip"
    hover_text[[length(hover_text) + 1]] <- ""
  }
  for (lvl in names(IR_EXPANSION_COLORS)) {
    sub <- fg[
      !is.na(fg$expansion) & as.character(fg$expansion) == lvl,
      ,
      drop = FALSE
    ]
    if (nrow(sub) == 0) {
      next
    }
    traces[[length(traces) + 1]] <- lvl
    data_x[[length(data_x) + 1]] <- sub$x
    data_y[[length(data_y) + 1]] <- sub$y
    data_color[[length(data_color) + 1]] <- unname(IR_EXPANSION_COLORS[[lvl]])
    hover_info[[length(hover_info) + 1]] <- "text"
    hover_text[[length(hover_text) + 1]] <- paste0(
      sub$barcode,
      "<br>",
      lvl,
      "<br>UMAP_1: ",
      formatC(sub$x, format = "f", digits = 2),
      "<br>UMAP_2: ",
      formatC(sub$y, format = "f", digits = 2)
    )
  }
  req(length(traces) > 0)

  output_meta <- list(
    color_type = "categorical",
    traces = traces,
    color_variable = "expansion",
    legend_position = legend_position,
    legend_font_size = legend_size
  )
  output_data <- list(
    x = data_x,
    y = data_y,
    color = data_color,
    point_size = marker_size,
    point_opacity = alpha,
    point_line = list(),
    reset_axes = TRUE
  )
  output_hover <- list(
    hoverinfo = hover_info,
    text = hover_text
  )

  shinyjs::js$updateClonalUMAP(output_meta, output_data, output_hover)
})

## ---- Clonal UMAP selection buttons (shared-projection engine) ----------- ##
## Delegate to the shared JS clear/zoom for this plot id, mirroring
## overview/event_projection_clear_selection.R.
observeEvent(input[["ir_clonalUMAP_projection_clear_selection"]], {
  shinyjs::js$irClonalUMAPClearSelection()
})
observeEvent(input[["ir_clonalUMAP_projection_zoom_to_selection"]], {
  shinyjs::js$irClonalUMAPZoomToSelection()
})

## Reflect the zoom state on the button (filled "Reset zoom" while zoomed in),
## toggled from the <plot_id>_zoom_state input the shared JS pushes.
observeEvent(
  input[["ir_clonalUMAP_projection_zoom_state"]],
  {
    zoomed <- isTRUE(input[["ir_clonalUMAP_projection_zoom_state"]])
    shinyjs::toggleClass(
      id = "ir_clonalUMAP_projection_zoom_to_selection",
      class = "is-zoomed",
      condition = zoomed
    )
    updateActionButton(
      session,
      "ir_clonalUMAP_projection_zoom_to_selection",
      label = if (zoomed) "Reset zoom" else "Zoom to selection",
      icon = if (zoomed) {
        icon("magnifying-glass-minus")
      } else {
        icon("magnifying-glass-plus")
      }
    )
  },
  ignoreInit = TRUE
)

## Show the selection buttons only while a persistent selection exists. The
## shared JS pushes the selection payload under <plot_id>_persistent_selection.
observe({
  sel <- input[["ir_clonalUMAP_projection_persistent_selection"]]
  has_selection <- !is.null(sel) && length(sel) > 0
  if (has_selection) {
    shinyjs::show("ir_clonalUMAP_projection_clear_selection")
    shinyjs::show("ir_clonalUMAP_projection_zoom_to_selection")
  } else {
    shinyjs::hide("ir_clonalUMAP_projection_clear_selection")
    shinyjs::hide("ir_clonalUMAP_projection_zoom_to_selection")
  }
})

output$ir_plot_clonalUMAP_static <- renderPlot(
  {
    req_plot_space("ir_plot_clonalUMAP_static")
    receptor <- ir_param("ir_p_umap_receptor")
    projection <- ir_param("ir_p_umap_projection")
    group_by <- ir_param("ir_p_umap_group_by", "")
    validate(need(nzchar(group_by), "Choose a grouping column."))
    clone_call <- "gene"
    show_all <- isTRUE(ir_param("ir_p_umap_show_all", TRUE))
    cells <- ir_umap_cells_to_show()
    df <- ir_clonal_umap_data(
      projection,
      receptor,
      clone_call,
      show_all = show_all,
      cells = cells
    )

    safeRenderPlot(
      {
        if (is.null(df) || nrow(df) == 0) {
          return(
            ggplot2::ggplot() +
              ggplot2::annotate(
                "text",
                x = 0,
                y = 0,
                label = paste0(
                  "No clonal UMAP to display.\n",
                  "Needs a cell projection and ",
                  if (is.null(receptor)) "TCR/BCR" else receptor,
                  " clonotypes whose barcodes match the cells."
                ),
                size = 4.5,
                colour = "#666666"
              ) +
              ggplot2::theme_void()
          )
        }
        df <- ir_umap_grouped_data(df, group_by)
        validate(need(
          !is.null(df) && nrow(df) > 0,
          "No cells match this grouping."
        ))
        dp <- tryCatch(ir_display_params(), error = function(e) list())
        point_size <- suppressWarnings(as.numeric(dp[["ir_d_point_size"]]))
        if (length(point_size) != 1 || is.na(point_size)) {
          point_size <- 1
        }
        alpha <- suppressWarnings(as.numeric(dp[["ir_d_alpha"]]))
        if (length(alpha) != 1 || is.na(alpha)) {
          alpha <- 0.8
        }
        n_groups <- length(levels(df$.umap_group))
        layout <- ir_umap_split_layout(
          n_groups,
          width = session$clientData$output_ir_plot_clonalUMAP_static_width,
          height = session$clientData$output_ir_plot_clonalUMAP_static_height
        )
        ir_clonal_umap_ggplot(
          df,
          group_by = group_by,
          point_size = point_size,
          alpha = alpha,
          ncol = layout$ncol
        )
      },
      "clonalUMAP"
    )
  },
  width = function() {
    group_by <- ir_param("ir_p_umap_group_by", "")
    ceiling(ir_umap_split_current_layout(group_by)$width)
  },
  height = function() {
    group_by <- ir_param("ir_p_umap_group_by", "")
    ceiling(ir_umap_split_current_layout(group_by)$height)
  }
) %>%
  ir_bindCache(
    input$ir_p_umap_receptor,
    input$ir_p_umap_projection,
    input$ir_p_umap_show_all,
    input$ir_p_umap_group_by,
    input$ir_d_point_size,
    input$ir_d_alpha
  )

## ---- BCR-specific renderers --------------------------------------------- ##
output$ir_plot_isotype <- plotly::renderPlotly({
  req_plot_space("ir_plot_isotype")
  data <- ir_data()
  req(!is.null(data))
  gb <- ir_params()$groupBy
  group_col <- if (is.null(gb)) "sample" else gb
  p <- bcr_isotype_plot(data, group_col = group_col)
  if (is.null(p)) {
    return(ir_empty_plotly(
      "No BCR isotype data available. Requires BCR data with CTgene column."
    ))
  }
  ir_render_ggplotly(p, "isotype")
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy
  )

output$ir_plot_shmProxy <- renderPlot({
  req_plot_space("ir_plot_shmProxy")
  data <- ir_data()
  req(!is.null(data))
  gb <- ir_params()$groupBy
  group_col <- if (is.null(gb)) "sample" else gb
  safeRenderPlot(
    {
      p <- bcr_shm_proxy_plot(data, group_col = group_col)
      if (is.null(p)) {
        plot.new()
        text(
          0.5,
          0.5,
          "No SHM proxy data available.\nRequires CTnt and CTstrict columns with BCR data.",
          cex = 0.9
        )
      } else {
        # Return the ggplot (not print()) so safeRenderPlot can apply display
        # options and renderPlot prints it once. Printing here too would render
        # twice.
        p
      }
    },
    "shmProxy"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy
  )

## ---- Paired Scatter (generic) ------------------------------------------- ##
ir_sample_meta <- reactive({
  data <- ir_data_annotated()
  if (is.null(data) || length(data) < 2) {
    return(NULL)
  }
  sample_level_meta(data)
})

output$ir_ui_pairedScatter <- renderUI({
  groups <- ir_compare_groups()
  if (length(groups) < 2) {
    return(div(
      class = "alert alert-info",
      "Paired scatter requires at least two groups to compare. Use Compare by to choose a metadata column with multiple levels."
    ))
  }
  meta <- ir_sample_meta()
  choices <- ir_paired_scatter_choices(meta)
  pair_choices <- c("Manual group comparison" = "", choices$compare_candidates)
  pair_mode <- input$ir_pair_compare
  if (is.null(pair_mode) || !(pair_mode %in% pair_choices)) {
    pair_mode <- ""
  }
  x <- input$ir_pair_x_group
  y <- input$ir_pair_y_group
  if (is.null(x) || !(x %in% groups)) {
    x <- groups[1]
  }
  if (is.null(y) || !(y %in% groups) || identical(y, x)) {
    y <- groups[min(2L, length(groups))]
  }

  controls <- list(
    selectInput(
      "ir_pair_compare",
      "Pair by:",
      choices = pair_choices,
      selected = pair_mode,
      selectize = FALSE
    )
  )
  if (identical(pair_mode, "")) {
    controls <- c(
      controls,
      list(
        selectInput(
          "ir_pair_x_group",
          "X group:",
          choices = groups,
          selected = x,
          selectize = FALSE
        ),
        selectInput(
          "ir_pair_y_group",
          "Y group:",
          choices = groups,
          selected = y,
          selectize = FALSE
        )
      )
    )
  } else {
    facet_candidates <- choices$facet_candidates
    default_facet <- ir_paired_scatter_default_facet(
      meta,
      pair_mode,
      facet_candidates
    )
    controls <- c(
      controls,
      list(selectInput(
        "ir_pair_facet",
        "Facet by:",
        choices = c("(none)" = "", facet_candidates),
        selected = default_facet,
        selectize = FALSE
      ))
    )
  }
  tagList(
    # Side-by-side (Pair by / X group / Y group [/ Facet by]); wraps only when
    # the row is too narrow. This panel sits in the wide right-hand column.
    ir_flow_controls_inline(controls),
    shinycssloaders::withSpinner(
      uiOutput("ir_ui_pairedScatter_plot")
    )
  )
})

## Single-panel paired scatter is interactive (plotly); the faceted variant is a
## patchwork grid that plotly cannot lay out cleanly, so it stays a static
## ggplot. The renderUI returns the matching output type for the current mode.
## The single panel fills the viewport through the shared `.cerebro-fill`
## mechanism, same as every other single IR plot — no per-plot height constant.
ir_paired_plotly_output <- function() {
  tags$div(
    class = "cerebro-fill",
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(
        "ir_plot_pairedScatter",
        width = "auto",
        height = "100%"
      ),
      type = 8,
      hide.ui = FALSE
    )
  )
}

output$ir_ui_pairedScatter_plot <- renderUI({
  pair_mode <- input$ir_pair_compare
  # Single-plot case (no compare mode): fill the viewport like every other tab.
  if (is.null(pair_mode) || !nzchar(pair_mode)) {
    return(ir_paired_plotly_output())
  }
  meta <- ir_sample_meta()
  req(!is.null(meta))
  facet_col <- input$ir_pair_facet
  if (is.null(facet_col) || facet_col == "") {
    # Still a single panel — fills the viewport.
    return(ir_paired_plotly_output())
  }
  # Faceted: size by the number of facet rows so panels aren't squashed. This is
  # intentionally a fixed pixel height (can exceed the viewport and scroll),
  # because forcing many facets into one viewport height would flatten them.
  # patchwork grid -> static ggplot output.
  n_facets <- length(unique(meta[[facet_col]]))
  ncol_p <- min(4L, n_facets)
  nrow_p <- ceiling(n_facets / ncol_p)
  h <- max(450, nrow_p * 420)
  plotOutput("ir_plot_pairedScatter_facet", height = paste0(h, "px"))
})

## Build one clonalScatter panel. clonalScatter returns a table with a Var1
## clonotype column, the two group columns, a `class` column (shared / expanded)
## and a `size` column; the point layer maps x/y to internal `x`/`y` columns.
ir_paired_scatter_panel <- function(
  data,
  pars,
  x_axis,
  y_axis,
  title,
  show_title = TRUE
) {
  p <- irn_clonalScatter(
    data,
    cloneCall = pars$cloneCall,
    chain = pars$chain,
    x.axis = x_axis,
    y.axis = y_axis,
    dot.size = ir_param("ir_p_dot_size", "total"),
    graph = ir_param("ir_p_graph", "proportion"),
    exportTable = FALSE,
    palette = IR_PALETTE
  )
  # clonalScatter draws two legends (fill = clone class, size = "Total n").
  # ggplotly cannot lay them out side by side and merges the titles into one
  # unreadable "classTotal n" block. Drop the size legend (the dot area still
  # encodes total n) and keep a single, clearly named clone-class legend.
  p <- p +
    ggplot2::guides(size = "none") +
    ggplot2::labs(fill = "Clone class")
  # The title only labels the faceted patchwork panels. On the single-panel
  # interactive plotly the x/y axes already name the two groups, and ggplotly
  # crams the title into the legend area where it overlaps the entries, so the
  # interactive caller passes show_title = FALSE.
  if (show_title) {
    p <- p + ggplot2::ggtitle(title)
  }
  p
}

## Single-panel paired scatter -> interactive plotly. The default ggplotly hover
## already shows the x/y group values and the clone class.
output$ir_plot_pairedScatter <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_pairedScatter")
  data <- ir_data_annotated()
  req(!is.null(data))
  meta <- ir_sample_meta()
  pars <- ir_params()
  groups <- ir_compare_groups()
  req(length(groups) >= 2)
  pair_mode <- input$ir_pair_compare

  ir_render_ggplotly(
    {
      if (is.null(pair_mode) || !nzchar(pair_mode)) {
        x <- input$ir_pair_x_group
        y <- input$ir_pair_y_group
        validate(
          need(x %in% groups, "Select an X group."),
          need(y %in% groups, "Select a Y group."),
          need(x != y, "Select two different groups.")
        )
        ir_paired_scatter_panel(
          data,
          pars,
          x,
          y,
          paste(x, "vs", y),
          show_title = FALSE
        )
      } else {
        req(!is.null(meta))
        compare_col <- pair_mode
        lvls <- sort(unique(meta[[compare_col]]))
        req(length(lvls) == 2L)
        s_a <- meta$.sample_name[meta[[compare_col]] == lvls[1]][1]
        s_b <- meta$.sample_name[meta[[compare_col]] == lvls[2]][1]
        ir_paired_scatter_panel(
          data,
          pars,
          s_a,
          s_b,
          paste(lvls[1], "vs", lvls[2]),
          show_title = FALSE
        )
      }
    },
    "pairedScatter"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_pair_compare,
    input$ir_pair_x_group,
    input$ir_pair_y_group,
    input$ir_p_graph,
    input$ir_p_dot_size
  )

## Faceted paired scatter -> static patchwork grid (plotly cannot lay out the
## multi-panel grid cleanly, so this variant stays a ggplot).
output$ir_plot_pairedScatter_facet <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_pairedScatter_facet")
  data <- ir_data_annotated()
  req(!is.null(data))
  meta <- ir_sample_meta()
  req(!is.null(meta))
  pars <- ir_params()
  groups <- ir_compare_groups()
  req(length(groups) >= 2)
  compare_col <- input$ir_pair_compare
  req(!is.null(compare_col) && nzchar(compare_col))
  facet_col <- input$ir_pair_facet
  req(!is.null(facet_col) && nzchar(facet_col))

  safeRenderPlot(
    {
      lvls <- sort(unique(meta[[compare_col]]))
      req(length(lvls) == 2L)
      facet_lvls <- unique(meta[[facet_col]])
      panels <- list()
      for (fl in facet_lvls) {
        rows <- meta[meta[[facet_col]] == fl, , drop = FALSE]
        s_a <- rows$.sample_name[rows[[compare_col]] == lvls[1]]
        s_b <- rows$.sample_name[rows[[compare_col]] == lvls[2]]
        if (length(s_a) == 0L || length(s_b) == 0L) {
          next
        }
        tryCatch(
          {
            panels[[length(panels) + 1L]] <- ir_paired_scatter_panel(
              data,
              pars,
              s_a[1],
              s_b[1],
              paste0(fl, ": ", lvls[1], " vs ", lvls[2])
            )
          },
          error = function(e) {
            message("[IR] Paired scatter for ", fl, " failed: ", e$message)
          }
        )
      }
      if (length(panels) == 0L) {
        plot.new()
        text(
          0.5,
          0.5,
          "No valid pairs found for the selected columns.",
          cex = 0.9
        )
      } else {
        ncol_p <- min(4L, length(panels))
        patchwork::wrap_plots(panels, ncol = ncol_p)
      }
    },
    "pairedScatterFacet"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_pair_compare,
    input$ir_pair_facet,
    input$ir_p_graph,
    input$ir_p_dot_size
  )

output$ir_plot_clonalAbundance <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalAbundance")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  ir_render_ggplotly(
    irn_clonalAbundance(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      scale = isTRUE(ir_param("ir_p_scale", FALSE))
    ),
    "clonalAbundance"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_scale,
    input$ir_p_order_by
  )

## Compare -> interactive alluvial. scRepertoire's ggalluvial output does not
## survive ggplotly(), so we take clonalCompare(exportTable = TRUE) (which still
## does all the clone filtering / top.clones / proportion work) and redraw it as
## native Plotly polygons via the compare_helpers.R pure functions. The `area`
## graph mode was dropped: Compare is about tracking clones across groups, which
## is the alluvial, and mixing a plotOutput/plotlyOutput per mode is not worth it.
output$ir_plot_clonalCompare <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalCompare")
  samples <- input$ir_compare_samples
  req(!is.null(samples) && length(samples) >= 2)
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  proportion <- isTRUE(ir_param("ir_p_compare_prop", TRUE))

  tryCatch(
    {
      tab <- irn_clonalCompare(
        data,
        cloneCall = pars$cloneCall,
        chain = pars$chain,
        group.by = pars$groupBy,
        order.by = ir_order_by(),
        samples = samples,
        top.clones = as.numeric(ir_param("ir_p_top_clones", 10)),
        proportion = proportion,
        exportTable = TRUE
      )
      prep <- ir_prepare_compare_alluvial(tab, samples, proportion = proportion)
      if (!isTRUE(prep$ok)) {
        return(ir_empty_plotly(prep$message))
      }
      dp <- tryCatch(ir_display_params(), error = function(e) list())
      legend_pos <- dp[["ir_d_legend_pos"]]
      if (!is.character(legend_pos) || length(legend_pos) != 1) {
        legend_pos <- "right"
      }
      ## The custom "top" bar is a shared-scatter feature; here plain Plotly has
      ## no top-bar mode, so fold "top" into the native right legend.
      if (identical(legend_pos, "top")) {
        legend_pos <- "right"
      }
      title <- dp[["ir_d_title"]]
      ir_compare_alluvial_plotly(
        prep,
        palette = IR_COMPARE_PALETTE,
        theme = cerebro_plotly_theme(),
        hoverlabel = cerebro_plotly_hoverlabel(),
        legend_pos = legend_pos,
        base_size = dp[["ir_d_base_size"]],
        title = if (is.character(title) && nzchar(title)) title else NULL
      )
    },
    error = function(e) {
      ir_empty_plotly(paste("Compare plot error:", conditionMessage(e)))
    }
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_compare_samples,
    input$ir_p_top_clones,
    input$ir_p_compare_prop,
    input$ir_p_order_by
  )

ir_plot_clonal_diversity <- function(
  data,
  clone_call,
  chain,
  group_by,
  metric,
  x_axis,
  n_boots,
  palette = IR_PALETTE
) {
  # scRepertoire 2.6.x can coerce factor x.axis values to numeric positions and
  # its boxplot layer does not explicitly group by x.axis. Use its bootstrap
  # table and redraw the x-axis summary so each metadata level gets its own box.
  # When x_axis is NULL, scRepertoire groups by list element (sample) names and
  # returns a table without an x.axis column — we use the group column itself as
  # the effective x-axis so the plot stays consistent with the non-NULL path.
  plot_data <- lapply(data, function(df) {
    for (col in unique(c(group_by, x_axis))) {
      if (!is.null(col) && col %in% colnames(df)) {
        df[[col]] <- as.character(df[[col]])
      }
    }
    df
  })

  # Build scRepertoire call, omitting x.axis when NULL
  scr_args <- list(
    plot_data,
    cloneCall = clone_call,
    chain = chain,
    group.by = group_by,
    metric = metric,
    n.boots = n_boots,
    return.boots = TRUE,
    exportTable = TRUE,
    palette = palette
  )
  if (!is.null(x_axis)) {
    scr_args[["x.axis"]] <- x_axis
  }
  ob <- ir_order_by()
  if (!is.null(ob)) {
    scr_args[["order.by"]] <- ob
  }
  output_df <- do.call(irn_clonalDiversity, scr_args)

  group_col <- if (is.null(group_by)) "Group" else group_by

  # When x_axis is NULL, scRepertoire returns a table keyed by the group column
  # alone (no x.axis column). Use the group column as the effective x-axis.
  eff_x_axis <- if (is.null(x_axis)) group_col else x_axis
  x_label <- if (is.null(x_axis)) {
    if (is.null(group_by)) "Group" else group_by
  } else {
    x_axis
  }

  validate(
    need(
      eff_x_axis %in% colnames(output_df),
      "X axis / group column is missing from the output table."
    ),
    need(
      group_col %in% colnames(output_df),
      "Selected grouping is not available."
    )
  )

  # Build x-axis levels. When x_axis is NULL, the level order comes from the
  # output table (scRepertoire's natural ordering by list element names).
  if (is.null(x_axis)) {
    x_levels <- unique(as.character(output_df[[group_col]]))
  } else {
    x_levels <- sort(unique(unlist(
      lapply(plot_data, function(df) {
        if (x_axis %in% colnames(df)) {
          as.character(df[[x_axis]])
        } else {
          character(0)
        }
      }),
      use.names = FALSE
    )))
  }
  x_levels <- x_levels[!is.na(x_levels)]
  if (length(x_levels) == 0) {
    x_levels <- unique(as.character(output_df[[eff_x_axis]]))
  }
  output_df[[eff_x_axis]] <- factor(
    as.character(output_df[[eff_x_axis]]),
    levels = x_levels
  )
  output_df[[group_col]] <- factor(as.character(output_df[[group_col]]))

  metric_name <- gsub(
    "(^|[[:space:]])([[:alpha:]])",
    "\\1\\U\\2",
    metric,
    perl = TRUE
  )
  fills <- grDevices::hcl.colors(
    length(levels(output_df[[group_col]])),
    palette
  )
  names(fills) <- levels(output_df[[group_col]])

  ggplot2::ggplot(
    output_df,
    ggplot2::aes(
      x = .data[[eff_x_axis]],
      y = as.numeric(.data[["value"]])
    )
  ) +
    ggplot2::geom_boxplot(
      ggplot2::aes(group = .data[[eff_x_axis]]),
      outlier.alpha = 0,
      fill = "white",
      colour = "#666666"
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(fill = .data[[group_col]]),
      width = 0.18,
      size = 3,
      shape = 21,
      stroke = 0.25,
      colour = "black"
    ) +
    ggplot2::scale_fill_manual(values = fills, name = "Group") +
    ggplot2::labs(
      x = x_label,
      y = paste(metric_name, "Index Score")
    ) +
    ggplot2::theme_classic(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5),
      legend.position = "right"
    )
}

output$ir_plot_clonalDiversity <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalDiversity")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  metric <- ir_param("ir_p_metric", "shannon")
  x_axis <- ir_param("ir_p_x_axis", "")
  x_axis <- if (is.null(x_axis) || !nzchar(x_axis)) NULL else x_axis
  n_boots <- as.numeric(ir_param("ir_p_n_boots", 20))
  if (is.na(n_boots) || n_boots < 1) {
    n_boots <- 20
  }
  ir_render_ggplotly(
    ir_plot_clonal_diversity(
      data = data,
      clone_call = pars$cloneCall,
      chain = pars$chain,
      group_by = pars$groupBy,
      metric = metric,
      x_axis = x_axis,
      n_boots = n_boots,
      palette = IR_PALETTE
    ),
    "clonalDiversity"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_metric,
    input$ir_p_x_axis,
    input$ir_p_n_boots,
    input$ir_p_order_by
  )

output$ir_plot_clonalHomeostasis <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalHomeostasis")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  ## Precomputed scRepertoire table for the by-sample, default-bin view; native
  ## fallback for grouped views, custom bins, or a .crb without precompute.
  pt <- if (
    is.null(pars$groupBy) && identical(ir_clone_size(), IR_CLONE_SIZE_DEFAULT)
  ) {
    ir_pc_lookup("clonalHomeostasis", pars$cloneCall, pars$chain)
  } else {
    NULL
  }
  ir_render_ggplotly(
    if (!is.null(pt)) {
      ir_pc_plot_homeostasis(pt)
    } else {
      irn_clonalHomeostasis(
        data,
        cloneCall = pars$cloneCall,
        chain = pars$chain,
        cloneSize = ir_clone_size(),
        group.by = pars$groupBy,
        order.by = ir_order_by(),
        exportTable = FALSE,
        palette = IR_PALETTE
      )
    },
    "clonalHomeostasis"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_clone_size,
    input$ir_p_order_by
  )

output$ir_plot_clonalLength <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalLength")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  # clonalLength measures CDR3 sequence length, so cloneCall must be a sequence
  # type (nt/aa) — "gene"/"strict" make scRepertoire error ("Please make a
  # selection of the type of CDR3 sequence ... by using cloneCall"). The tab's
  # cloneCall dropdown is restricted to nt/aa, but enforce it here too so a
  # stale "gene" value (update race on tab switch) can't reach scRepertoire.
  clone_call <- if (isTRUE(pars$cloneCall %in% c("nt", "aa"))) {
    pars$cloneCall
  } else {
    "aa"
  }
  scale_on <- isTRUE(ir_param("ir_p_scale", FALSE))
  if (is.null(pars$groupBy)) {
    # No grouping (Group results by = None): a single combined panel with each
    # loaded sample overlaid by colour, i.e. scRepertoire's native plot. Do NOT
    # facet — the export table still carries the list-element (sample) names in
    # `values`, but those are not a user-chosen grouping.
    pt <- ir_pc_lookup("clonalLength", clone_call, pars$chain)
    safeRenderPlot(
      if (!is.null(pt)) {
        ir_pc_plot_length(pt, scale = scale_on)
      } else {
        irn_clonalLength(
          data,
          cloneCall = clone_call,
          chain = pars$chain,
          group.by = NULL,
          order.by = ir_order_by(),
          scale = scale_on,
          exportTable = FALSE,
          palette = IR_PALETTE
        )
      },
      "clonalLength"
    )
  } else {
    # A grouping is selected: scRepertoire overlays the groups in one panel, so
    # take its per-clonotype table and redraw with facet_wrap to give each
    # selected group its own length-distribution panel on a shared axis.
    order_by <- ir_order_by()
    tbl <- irn_clonalLength(
      data,
      cloneCall = clone_call,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = order_by,
      exportTable = TRUE,
      palette = IR_PALETTE
    )
    safeRenderPlot(
      ir_length_facet_plot(
        tbl,
        scale = scale_on,
        group_col = pars$groupBy,
        group_levels = ir_length_group_levels(tbl, pars$groupBy, order_by)
      ),
      "clonalLength"
    )
  }
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_scale,
    input$ir_p_order_by
  )

output$ir_plot_clonalOverlap <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalOverlap")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  ov_method <- ir_param("ir_p_overlap_method", "overlap")
  pt <- if (is.null(pars$groupBy)) {
    ir_pc_lookup("clonalOverlap", pars$cloneCall, pars$chain, ov_method)
  } else {
    NULL
  }
  safeRenderPlot(
    if (!is.null(pt)) {
      ir_pc_plot_overlap(pt)
    } else {
      irn_clonalOverlap(
        data,
        cloneCall = pars$cloneCall,
        chain = pars$chain,
        group.by = pars$groupBy,
        method = ov_method,
        exportTable = FALSE,
        palette = IR_PALETTE
      )
    },
    "clonalOverlap"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_overlap_method
  )

output$ir_plot_clonalProportion <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalProportion")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  csplit <- suppressWarnings(as.numeric(strsplit(
    ir_param("ir_p_clonal_split", "10, 100, 1000, 10000, 30000, 100000"),
    "[,\\s]+"
  )[[1]]))
  csplit <- csplit[!is.na(csplit)]
  if (length(csplit) == 0) {
    csplit <- c(10, 100, 1000, 10000, 30000, 1e+05)
  }
  ## Precompute used scRepertoire's default rank bins; use it only when the user
  ## has not customised the split (else the stored bins would not match).
  default_split <- identical(csplit, c(10, 100, 1000, 10000, 30000, 1e+05))
  pt <- if (is.null(pars$groupBy) && default_split) {
    ir_pc_lookup("clonalProportion", pars$cloneCall, pars$chain)
  } else {
    NULL
  }
  safeRenderPlot(
    if (!is.null(pt)) {
      ir_pc_plot_proportion(pt)
    } else {
      irn_clonalProportion(
        data,
        cloneCall = pars$cloneCall,
        chain = pars$chain,
        group.by = pars$groupBy,
        clonalSplit = csplit,
        exportTable = FALSE,
        palette = IR_PALETTE
      )
    },
    "clonalProportion"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_clonal_split
  )

output$ir_plot_clonalQuant <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalQuant")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  quant_scale <- isTRUE(ir_param("ir_p_scale", FALSE))
  pt <- if (is.null(pars$groupBy)) {
    ir_pc_lookup("clonalQuant", pars$cloneCall, pars$chain)
  } else {
    NULL
  }
  safeRenderPlot(
    if (!is.null(pt)) {
      ir_pc_plot_quant(pt, scale = quant_scale)
    } else {
      irn_clonalQuant(
        data,
        cloneCall = pars$cloneCall,
        chain = pars$chain,
        group.by = pars$groupBy,
        scale = quant_scale,
        exportTable = FALSE,
        palette = IR_PALETTE
      )
    },
    "clonalQuant"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_scale
  )

output$ir_ui_clonalRarefaction <- renderUI({
  # Bootstrap iterations / plot type / Hill number now come from the
  # function-specific param panel (see IR_PARAM_SPEC "Rarefaction").
  # Rarefaction bootstraps are slow — show a spinner while it computes.
  shinycssloaders::withSpinner(
    plotOutput("ir_plot_clonalRarefaction", height = "450px")
  )
})

output$ir_plot_clonalRarefaction <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalRarefaction")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  n_boots <- as.numeric(ir_param("ir_p_rare_n_boots", 20))
  if (is.na(n_boots) || n_boots < 1) {
    n_boots <- 20
  }
  safeRenderPlot(
    ir_quiet_inext(
      irn_clonalRarefaction(
        data,
        cloneCall = pars$cloneCall,
        chain = pars$chain,
        group.by = pars$groupBy,
        plot.type = as.numeric(ir_param("ir_p_rare_plot_type", 1)),
        hill.numbers = as.numeric(ir_param("ir_p_hill_numbers", 0)),
        n.boots = n_boots,
        exportTable = FALSE,
        palette = IR_PALETTE
      )
    ),
    "clonalRarefaction"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_rare_n_boots,
    input$ir_p_rare_plot_type,
    input$ir_p_hill_numbers
  )

output$ir_plot_clonalScatter <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalScatter")
  data <- ir_data()
  req(!is.null(data))
  # clonalScatter compares two groups. x.axis / y.axis are the names of the
  # groups that group.by produces (samples when group.by is None, otherwise the
  # levels of the group.by column). Passing group.by lets scRepertoire regroup
  # the data so the selected x/y refer to those groups.
  x <- input$ir_scatter_x
  y <- input$ir_scatter_y
  # The scatter x/y selectors live in a dynamic renderUI, so on first paint the
  # inputs are not yet registered (NULL). req() silently halts the render until
  # they exist — more reliable than validate() for the first-paint race.
  req(x, y)
  pars <- ir_params()
  chain <- pars$chain
  if (is.null(chain) || !nzchar(chain)) {
    chain <- "both"
  }
  groups <- ir_compare_groups()
  validate(
    need(
      length(groups) >= 2,
      "Clonal scatter needs at least 2 groups to compare. Use 'Group by' to split the data into >= 2 groups."
    ),
    need(
      !is.null(x) && !is.null(y) && nzchar(x) && nzchar(y),
      "Select two groups to compare."
    ),
    need(
      x %in% groups && y %in% groups,
      "Selected groups are not available in the current grouping."
    ),
    need(x != y, "Select two different groups for the scatter comparison.")
  )
  safeRenderPlot(
    irn_clonalScatter(
      data,
      cloneCall = pars$cloneCall,
      chain = chain,
      group.by = pars$groupBy,
      x.axis = x,
      y.axis = y,
      dot.size = ir_param("ir_p_dot_size", "total"),
      graph = ir_param("ir_p_graph", "proportion"),
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "clonalScatter"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_scatter_x,
    input$ir_scatter_y,
    input$ir_p_graph,
    input$ir_p_dot_size
  )

output$ir_plot_clonalSizeDistribution <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalSizeDistribution")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  # clonalSizeDistribution fits a distribution per group (MLE); on the gene/nt/aa
  # clone definitions the fit fails ("initial parameter values are invalid" /
  # "NA/NaN ... sigmau"). The strict clone definition is the stable choice and
  # is also the most appropriate for a clone-size distribution, so enforce it.
  threshold <- as.numeric(ir_param("ir_p_sd_threshold", 1))
  if (is.na(threshold) || threshold < 1) {
    threshold <- 1
  }
  ir_render_ggplotly(
    {
      p <- irn_clonalSizeDistribution(
        data,
        cloneCall = "strict",
        chain = pars$chain,
        group.by = pars$groupBy,
        method = ir_param("ir_p_sd_method", "ward.D2"),
        threshold = threshold,
        exportTable = FALSE
      )
      # clonalSizeDistribution maps color = as.factor(label) internally, so the
      # colour legend title comes through as the raw "as.factor(label)". Rename
      # it to what it actually is (the grouping variable).
      p + ggplot2::labs(colour = "Group")
    },
    "clonalSizeDistribution"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_sd_method,
    input$ir_p_sd_threshold
  )

output$ir_ui_percentGeneUsage <- renderUI({
  h <- ir_plot_height("none")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentGeneUsage",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentGeneUsage <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_percentGeneUsage")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    irn_percentGeneUsage(
      data,
      chain = pars$chain,
      genes = (function() {
        g <- ir_param("ir_p_gu_genes", default_gene_family())
        if (is.null(g) || !nzchar(g)) default_gene_family() else g
      })(),
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      summary.fun = ir_param("ir_p_gu_summary", "percent"),
      plot.type = ir_param("ir_p_gu_plot_type", "heatmap"),
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "percentGeneUsage"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_gu_genes,
    input$ir_p_gu_plot_type,
    input$ir_p_gu_summary,
    input$ir_p_order_by
  )

output$ir_ui_vizGenes <- renderUI({
  h <- ir_plot_height("none")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_vizGenes",
    height = paste0(h, "px")
  ))
})

output$ir_plot_vizGenes <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_vizGenes")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  vg_x <- ir_param("ir_p_vg_x_axis", default_gene_family())
  if (is.null(vg_x) || !nzchar(vg_x)) {
    vg_x <- default_gene_family()
  }
  safeRenderPlot(
    irn_vizGenes(
      data,
      x.axis = vg_x,
      y.axis = NULL,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      plot = ir_param("ir_p_vg_plot", "heatmap"),
      summary.fun = ir_param("ir_p_vg_summary", "percent"),
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "vizGenes"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_vg_x_axis,
    input$ir_p_vg_plot,
    input$ir_p_vg_summary,
    input$ir_p_order_by
  )

output$ir_ui_percentGenes <- renderUI({
  h <- ir_plot_height("none")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentGenes",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentGenes <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_percentGenes")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    irn_percentGenes(
      data,
      chain = specific_chain(),
      gene = ir_param("ir_p_pg_gene", "Vgene"),
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      summary.fun = ir_param("ir_p_pg_summary", "percent"),
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "percentGenes"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_pg_gene,
    input$ir_p_pg_summary,
    input$ir_p_order_by
  )

output$ir_ui_percentVJ <- renderUI({
  h <- ir_plot_height("wrap")
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentVJ",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentVJ <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_percentVJ")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    irn_percentVJ(
      data,
      chain = specific_chain(),
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      summary.fun = ir_param("ir_p_vj_summary", "percent"),
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "percentVJ"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_vj_summary,
    input$ir_p_order_by
  )

output$ir_ui_percentAA <- renderUI({
  ng <- n_groups()
  # facet_grid(group ~ .): ~200px per group, minimum 400
  h <- max(400, ng * 200)
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentAA",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentAA <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_percentAA")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  # Guard aa.length: a non-positive / NA value makes scRepertoire's positional
  # functions error. Fall back to the default when the input is invalid.
  aa_len <- as.numeric(ir_param("ir_p_aa_length", 20))
  if (is.na(aa_len) || aa_len < 1) {
    aa_len <- 20
  }
  pt <- if (is.null(pars$groupBy) && aa_len == 20) {
    ir_pc_lookup("percentAA", pars$chain)
  } else {
    NULL
  }
  safeRenderPlot(
    if (!is.null(pt)) {
      ir_pc_plot_percentAA(pt)
    } else {
      irn_percentAA(
        data,
        chain = pars$chain,
        group.by = pars$groupBy,
        order.by = ir_order_by(),
        aa.length = aa_len,
        exportTable = FALSE,
        palette = IR_PALETTE
      )
    },
    "percentAA"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_aa_length,
    input$ir_p_order_by
  )

output$ir_plot_positionalEntropy <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_positionalEntropy")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  # Guard aa.length (see percentAA): invalid values make scRepertoire error.
  aa_len <- as.numeric(ir_param("ir_p_pe_aa_length", 20))
  if (is.na(aa_len) || aa_len < 1) {
    aa_len <- 20
  }
  safeRenderPlot(
    irn_positionalEntropy(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      aa.length = aa_len,
      method = ir_param("ir_p_pe_method", "norm.entropy"),
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "positionalEntropy"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_pe_aa_length,
    input$ir_p_pe_method,
    input$ir_p_order_by
  )

## ---- Positional Property: facet count per method ---------------------- ##
## Requires immApex; most methods also need the Peptides package.
all_property_facets <- c(
  atchleyFactors = 5,
  crucianiProperties = 3,
  FASGAI = 6,
  kideraFactors = 10,
  MSWHIM = 3,
  ProtFP = 8,
  stScales = 8,
  tScales = 5,
  VHSE = 8,
  zScales = 5
)

available_property_methods <- reactive({
  resolver <- tryCatch(
    getFromNamespace(".aa.property.matrix", "immApex"),
    error = function(e) NULL
  )
  if (is.null(resolver)) {
    return(all_property_facets["atchleyFactors"])
  }
  ok <- vapply(
    names(all_property_facets),
    function(m) {
      tryCatch(
        {
          resolver(m)
          TRUE
        },
        error = function(e) FALSE
      )
    },
    logical(1)
  )
  all_property_facets[ok]
})

output$ir_ui_positionalProperty <- renderUI({
  # Property method is set in the settings panel (IR_PARAM_SPEC "Property").
  avail <- available_property_methods()
  method <- input$ir_property_method
  if (is.null(method) || !method %in% names(avail)) {
    method <- names(avail)[1]
  }
  n_facets <- avail[[method]]
  if (is.null(n_facets)) {
    n_facets <- 5
  }
  # ~120px per facet row, minimum 450
  h <- max(450, n_facets * 120)
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_positionalProperty",
    height = paste0(h, "px")
  ))
})

output$ir_plot_positionalProperty <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_positionalProperty")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  method <- input$ir_property_method
  if (is.null(method)) {
    method <- names(available_property_methods())[1]
  }
  safeRenderPlot(
    irn_positionalProperty(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      method = method,
      aa.length = as.numeric(ir_param("ir_p_pp_aa_length", 20)),
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "positionalProperty"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_property_method,
    input$ir_p_pp_aa_length,
    input$ir_p_order_by
  )

output$ir_ui_percentKmer <- renderUI({
  # Top motifs is set in the settings panel (IR_PARAM_SPEC "K-mer").
  top_m <- as.numeric(ir_param("ir_p_top_motifs", 30))
  if (is.na(top_m) || top_m < 1) {
    top_m <- 30
  }
  h <- max(450, top_m * 20)
  shinycssloaders::withSpinner(plotOutput(
    "ir_plot_percentKmer",
    height = paste0(h, "px")
  ))
})

output$ir_plot_percentKmer <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_percentKmer")
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  top_m <- as.numeric(ir_param("ir_p_top_motifs", 30))
  if (is.na(top_m) || top_m < 1) {
    top_m <- 30
  }
  safeRenderPlot(
    irn_percentKmer(
      data,
      chain = pars$chain,
      cloneCall = pars$cloneCall,
      group.by = pars$groupBy,
      motif.length = as.numeric(ir_param("ir_p_motif_length", 3)),
      min.depth = as.numeric(ir_param("ir_p_min_depth", 3)),
      top.motifs = top_m,
      exportTable = FALSE,
      palette = IR_PALETTE
    ),
    "percentKmer"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_top_motifs,
    input$ir_p_motif_length,
    input$ir_p_min_depth
  )

## ---- Definition: clone-definition resolution waterfall ----------------- ##
## Bars count unique entities at cells -> V -> J -> V+J -> CDR3 -> V+CDR3 ->
## V+J+CDR3. The clone definition is parsed from the CT* columns for the active
## chain (ir_parse_segments); faceted by the active group.by column when chosen.
output$ir_plot_cloneDefinition <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_cloneDefinition")
  ir_render_ggplotly(
    ir_build_definition_plot(
      ir_data_annotated(),
      specific_chain(),
      ir_params()$groupBy
    ),
    "ir_plot_cloneDefinition"
  )
}) %>%
  ir_bindCache(
    input$ir_chain,
    input$ir_groupBy
  )

## ---- Sharing: cross-group clonotype sharing ---------------------------- ##
## Classifies each clonotype (V+J+CDR3 of the active chain) as Private /
## Public(within-group) / Public(cross-group) using the chosen sharing unit and
## the active group.by, then bars the class counts.
output$ir_plot_cloneSharing <- plotly::renderPlotly({
  req(has_scRepertoire())
  req_plot_space("ir_plot_cloneSharing")
  fig <- ir_render_ggplotly(
    ir_build_sharing_plot(
      ir_data_annotated(),
      specific_chain(),
      ir_param("ir_sharing_unit", "sample"),
      ir_params()$groupBy
    ),
    "ir_plot_cloneSharing",
    tooltip = "text"
  )
  # The on-bar count labels come through as their own text-mode traces; stop
  # them showing a second (redundant) tooltip on hover, so only the bars react.
  if (inherits(fig, "plotly") && !is.null(fig$x$data)) {
    fig$x$data <- lapply(fig$x$data, function(tr) {
      if (identical(tr$mode, "text")) {
        tr$hoverinfo <- "skip"
      }
      tr
    })
  }
  fig
}) %>%
  ir_bindCache(
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sharing_unit
  )
