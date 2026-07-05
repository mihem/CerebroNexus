## ---- Plot-panel height helpers ----------------------------------------- ##
## One place controls the height of every IR tab body, instead of repeating a
## `height = 450` literal in each tabPanel. `IR_PLOT_HEIGHT` fills the viewport
## minus the fixed chrome above the plot (top bar, box title, tab strip, help
## panel) and leaves a small gap at the bottom. Using a viewport-relative height
## (rather than a flex `100%` chain) fills the screen while staying safe for both
## plotly and static plotOutput — a percentage height on a flex item with no
## resolved parent height collapses a static plot to zero.
IR_PLOT_HEIGHT <- "calc(100vh - 250px)"

## Static single plot tab body. `plotly = TRUE` emits an interactive
## plotlyOutput (zoom/pan/hover) instead of a static plotOutput.
ir_fill_plot <- function(
  id,
  spinner = TRUE,
  height = IR_PLOT_HEIGHT,
  plotly = FALSE
) {
  plot <- if (plotly) {
    plotly::plotlyOutput(id, height = height)
  } else {
    plotOutput(id, height = height)
  }
  if (spinner) {
    plot <- shinycssloaders::withSpinner(plot)
  }
  plot
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
      ir_fill_plot("ir_plot_clonalUMAP", plotly = TRUE)
    ),
    tabPanel(
      "Abundance",
      ir_fill_plot("ir_plot_clonalAbundance", plotly = TRUE)
    ),
    tabPanel(
      "Diversity",
      ir_fill_plot("ir_plot_clonalDiversity")
    ),
    tabPanel(
      "Homeostasis",
      ir_fill_plot("ir_plot_clonalHomeostasis", plotly = TRUE)
    ),
    tabPanel(
      "Isotype",
      ir_fill_plot("ir_plot_isotype", plotly = TRUE)
    ),
    # Hidden per review (kept available; renderer/help/param_spec retained).
    # tabPanel(
    #   "SHM Proxy",
    #   ir_fill_plot("ir_plot_shmProxy")
    # ),
    tabPanel(
      "Paired Scatter",
      ir_fill_wrap(shinycssloaders::withSpinner(uiOutput(
        "ir_ui_pairedScatter"
      )))
    )
  )

  ## Remaining tabs. Most are hidden per review to keep the tab strip focused
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
          ir_fill_plot("ir_plot_clonalCompare")
        ),
        # tabPanel(
        #   "Overlap",
        #   ir_fill_plot("ir_plot_clonalOverlap")
        # ),
        tabPanel(
          "SizeDist",
          ir_fill_plot("ir_plot_clonalSizeDistribution")
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
## Expansion-level colours: turbo runs cool -> warm, so larger (more expanded)
## clones read as warmer, which matches the ordering. Keys must match the
## factor levels produced in data.R (IR_CLONE_LABELS).
IR_EXPANSION_COLORS <- stats::setNames(
  viridis::turbo(5, begin = 0.05, end = 0.95),
  c(
    "Single (0 < X <= 1)",
    "Small (1 < X <= 5)",
    "Medium (5 < X <= 20)",
    "Large (20 < X <= 100)",
    "Hyperexpanded (100 < X)"
  )
)

## Render a scRepertoire ggplot as an interactive plotly figure. safeRenderPlot
## still does the heavy lifting (display options, empty-state and error plots,
## silent-error re-raise); we just convert its ggplot result with ggplotly and
## fall back to a plotly message if conversion itself fails. Used by the simple
## bar/point tabs; plots that ggplotly cannot represent well (alluvial, custom
## facet/bootstrap renderers) keep renderPlot.
ir_render_ggplotly <- function(expr, plot_name) {
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
      plotly::ggplotly(p)
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

output$ir_plot_clonalUMAP <- plotly::renderPlotly({
  req_plot_space("ir_plot_clonalUMAP")
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

  fig <- tryCatch(
    {
      if (is.null(df) || nrow(df) == 0) {
        ir_empty_plotly(paste0(
          "No clonal UMAP to display. Needs a cell projection and ",
          if (is.null(receptor)) "TCR/BCR" else receptor,
          " clonotypes whose barcodes match the cells."
        ))
      } else {
        dp <- tryCatch(ir_display_params(), error = function(e) list())
        point_size <- suppressWarnings(as.numeric(dp[["ir_d_point_size"]]))
        if (length(point_size) != 1 || is.na(point_size)) {
          point_size <- 1
        }
        alpha <- suppressWarnings(as.numeric(dp[["ir_d_alpha"]]))
        if (length(alpha) != 1 || is.na(alpha)) {
          alpha <- 0.8
        }
        # plotly marker sizes read larger than ggplot's; scale up so the points
        # are comparable to the other UMAPs.
        marker_size <- point_size * 5

        # Grey background = cells without the selected receptor (expansion = NA);
        # coloured foreground = receptor cells with an expansion level.
        bg <- df[is.na(df$expansion), , drop = FALSE]
        fg <- df[!is.na(df$expansion), , drop = FALSE]

        p <- plotly::plot_ly(source = "ir_plot_clonalUMAP")
        if (nrow(bg) > 0) {
          p <- plotly::add_trace(
            p,
            x = bg$x,
            y = bg$y,
            type = "scattergl",
            mode = "markers",
            marker = list(
              size = marker_size,
              color = "#D9D9D9",
              opacity = alpha
            ),
            name = "Other cells",
            hoverinfo = "skip",
            showlegend = TRUE
          )
        }
        if (nrow(fg) > 0) {
          # One trace per expansion level so the legend is clickable and each
          # gets its turbo colour; keep the canonical level order.
          for (lvl in names(IR_EXPANSION_COLORS)) {
            sub <- fg[
              !is.na(fg$expansion) & as.character(fg$expansion) == lvl,
              ,
              drop = FALSE
            ]
            if (nrow(sub) == 0) {
              next
            }
            p <- plotly::add_trace(
              p,
              x = sub$x,
              y = sub$y,
              type = "scattergl",
              mode = "markers",
              marker = list(
                size = marker_size,
                color = IR_EXPANSION_COLORS[[lvl]],
                opacity = alpha
              ),
              name = lvl,
              text = sub$barcode,
              hovertemplate = paste0(
                "%{text}<br>",
                lvl,
                "<br>UMAP_1: %{x:.2f}<br>UMAP_2: %{y:.2f}<extra></extra>"
              ),
              showlegend = TRUE
            )
          }
        }
        title <- dp[["ir_d_title"]]
        legend_size <- suppressWarnings(as.numeric(dp[["ir_d_legend_size"]]))
        if (
          length(legend_size) != 1 || is.na(legend_size) || legend_size <= 0
        ) {
          legend_size <- 12
        }
        legend_pos <- dp[["ir_d_legend_pos"]]
        if (!is.character(legend_pos) || length(legend_pos) != 1) {
          legend_pos <- "right"
        }
        # Map the shared position choices onto plotly's legend orientation/anchor.
        show_legend <- legend_pos != "none"
        legend_cfg <- list(
          itemsizing = "constant",
          font = list(size = legend_size),
          title = list(text = "Clonotype")
        )
        if (legend_pos == "bottom") {
          legend_cfg <- c(
            legend_cfg,
            list(orientation = "h", x = 0, y = -0.15)
          )
        } else if (legend_pos == "top") {
          legend_cfg <- c(
            legend_cfg,
            list(orientation = "h", x = 0, y = 1.1)
          )
        } else if (legend_pos == "left") {
          legend_cfg <- c(legend_cfg, list(x = -0.2))
        }
        plotly::layout(
          p,
          xaxis = list(
            autorange = TRUE,
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          yaxis = list(
            autorange = TRUE,
            mirror = TRUE,
            showline = TRUE,
            zeroline = FALSE
          ),
          showlegend = show_legend,
          legend = legend_cfg,
          title = if (is.character(title) && nzchar(title)) title else NULL
        )
      }
    },
    error = function(e) {
      ir_empty_plotly(paste("Clonal UMAP error:", conditionMessage(e)))
    }
  )
  plotly::toWebGL(fig)
}) %>%
  ir_bindCache(
    input$ir_p_umap_receptor,
    input$ir_p_umap_projection,
    input$ir_p_umap_show_all,
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

output$ir_ui_pairedScatter_plot <- renderUI({
  pair_mode <- input$ir_pair_compare
  if (is.null(pair_mode) || !nzchar(pair_mode)) {
    return(plotOutput("ir_plot_pairedScatter", height = "500px"))
  }
  meta <- ir_sample_meta()
  req(!is.null(meta))
  facet_col <- input$ir_pair_facet
  if (is.null(facet_col) || facet_col == "") {
    h <- 500
  } else {
    n_facets <- length(unique(meta[[facet_col]]))
    ncol_p <- min(4L, n_facets)
    nrow_p <- ceiling(n_facets / ncol_p)
    h <- max(450, nrow_p * 420)
  }
  plotOutput("ir_plot_pairedScatter", height = paste0(h, "px"))
})

output$ir_plot_pairedScatter <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_pairedScatter")
  data <- ir_data_annotated()
  req(!is.null(data))
  meta <- ir_sample_meta()
  pars <- ir_params()
  groups <- ir_compare_groups()
  req(length(groups) >= 2)
  pair_mode <- input$ir_pair_compare
  facet_col <- input$ir_pair_facet

  safeRenderPlot(
    {
      if (is.null(pair_mode) || !nzchar(pair_mode)) {
        x <- input$ir_pair_x_group
        y <- input$ir_pair_y_group
        validate(
          need(x %in% groups, "Select an X group."),
          need(y %in% groups, "Select a Y group."),
          need(x != y, "Select two different groups.")
        )
        p <- scRepertoire::clonalScatter(
          data,
          cloneCall = pars$cloneCall,
          chain = pars$chain,
          group.by = pars$groupBy,
          x.axis = x,
          y.axis = y,
          dot.size = ir_param("ir_p_dot_size", "total"),
          graph = ir_param("ir_p_graph", "proportion"),
          exportTable = FALSE,
          palette = "inferno"
        )
        p <- p + ggplot2::ggtitle(paste(x, "vs", y))
        p
      } else {
        req(!is.null(meta))
        compare_col <- pair_mode
        req(!is.null(compare_col))
        lvls <- sort(unique(meta[[compare_col]]))
        req(length(lvls) == 2L)
        if (!is.null(facet_col) && nzchar(facet_col)) {
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
                p <- scRepertoire::clonalScatter(
                  data,
                  cloneCall = pars$cloneCall,
                  chain = pars$chain,
                  x.axis = s_a[1],
                  y.axis = s_b[1],
                  dot.size = ir_param("ir_p_dot_size", "total"),
                  graph = ir_param("ir_p_graph", "proportion"),
                  exportTable = FALSE,
                  palette = "inferno"
                )
                p <- p +
                  ggplot2::ggtitle(paste0(fl, ": ", lvls[1], " vs ", lvls[2]))
                panels[[length(panels) + 1L]] <- p
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
        } else {
          s_a <- meta$.sample_name[meta[[compare_col]] == lvls[1]][1]
          s_b <- meta$.sample_name[meta[[compare_col]] == lvls[2]][1]
          p <- scRepertoire::clonalScatter(
            data,
            cloneCall = pars$cloneCall,
            chain = pars$chain,
            x.axis = s_a,
            y.axis = s_b,
            dot.size = ir_param("ir_p_dot_size", "total"),
            graph = ir_param("ir_p_graph", "proportion"),
            exportTable = FALSE,
            palette = "inferno"
          )
          p <- p + ggplot2::ggtitle(paste(lvls[1], "vs", lvls[2]))
          p
        }
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
    input$ir_pair_facet,
    input$ir_pair_x_group,
    input$ir_pair_y_group,
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
    scRepertoire::clonalAbundance(
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

output$ir_plot_clonalCompare <- renderPlot({
  req(has_scRepertoire())
  req_plot_space("ir_plot_clonalCompare")
  req(
    !is.null(input$ir_compare_samples) && length(input$ir_compare_samples) >= 2
  )
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  safeRenderPlot(
    scRepertoire::clonalCompare(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      samples = input$ir_compare_samples,
      top.clones = as.numeric(ir_param("ir_p_top_clones", 10)),
      graph = ir_param("ir_p_compare_graph", "alluvial"),
      proportion = isTRUE(ir_param("ir_p_compare_prop", TRUE)),
      exportTable = FALSE,
      palette = "inferno"
    ),
    "clonalCompare"
  )
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy,
    input$ir_compare_samples,
    input$ir_p_top_clones,
    input$ir_p_compare_graph,
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
  palette = "inferno"
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
  output_df <- do.call(scRepertoire::clonalDiversity, scr_args)

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

output$ir_plot_clonalDiversity <- renderPlot({
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
  safeRenderPlot(
    ir_plot_clonal_diversity(
      data = data,
      clone_call = pars$cloneCall,
      chain = pars$chain,
      group_by = pars$groupBy,
      metric = metric,
      x_axis = x_axis,
      n_boots = n_boots,
      palette = "inferno"
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
  ir_render_ggplotly(
    scRepertoire::clonalHomeostasis(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      cloneSize = ir_clone_size(),
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      exportTable = FALSE,
      palette = "inferno"
    ),
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
    safeRenderPlot(
      scRepertoire::clonalLength(
        data,
        cloneCall = clone_call,
        chain = pars$chain,
        group.by = NULL,
        order.by = ir_order_by(),
        scale = scale_on,
        exportTable = FALSE,
        palette = "inferno"
      ),
      "clonalLength"
    )
  } else {
    # A grouping is selected: scRepertoire overlays the groups in one panel, so
    # take its per-clonotype table and redraw with facet_wrap to give each
    # selected group its own length-distribution panel on a shared axis.
    order_by <- ir_order_by()
    tbl <- scRepertoire::clonalLength(
      data,
      cloneCall = clone_call,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = order_by,
      exportTable = TRUE,
      palette = "inferno"
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
  safeRenderPlot(
    scRepertoire::clonalOverlap(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      method = ir_param("ir_p_overlap_method", "overlap"),
      exportTable = FALSE,
      palette = "inferno"
    ),
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
  safeRenderPlot(
    scRepertoire::clonalProportion(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      clonalSplit = csplit,
      exportTable = FALSE,
      palette = "inferno"
    ),
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
  safeRenderPlot(
    scRepertoire::clonalQuant(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      scale = isTRUE(ir_param("ir_p_scale", FALSE)),
      exportTable = FALSE,
      palette = "inferno"
    ),
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
      scRepertoire::clonalRarefaction(
        data,
        cloneCall = pars$cloneCall,
        chain = pars$chain,
        group.by = pars$groupBy,
        plot.type = as.numeric(ir_param("ir_p_rare_plot_type", 1)),
        hill.numbers = as.numeric(ir_param("ir_p_hill_numbers", 0)),
        n.boots = n_boots,
        exportTable = FALSE,
        palette = "inferno"
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
    scRepertoire::clonalScatter(
      data,
      cloneCall = pars$cloneCall,
      chain = chain,
      group.by = pars$groupBy,
      x.axis = x,
      y.axis = y,
      dot.size = ir_param("ir_p_dot_size", "total"),
      graph = ir_param("ir_p_graph", "proportion"),
      exportTable = FALSE,
      palette = "inferno"
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

output$ir_plot_clonalSizeDistribution <- renderPlot({
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
  safeRenderPlot(
    scRepertoire::clonalSizeDistribution(
      data,
      cloneCall = "strict",
      chain = pars$chain,
      group.by = pars$groupBy,
      method = ir_param("ir_p_sd_method", "ward.D2"),
      threshold = threshold,
      exportTable = FALSE
    ),
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
    scRepertoire::percentGeneUsage(
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
      palette = "inferno"
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
    scRepertoire::vizGenes(
      data,
      x.axis = vg_x,
      y.axis = NULL,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      plot = ir_param("ir_p_vg_plot", "heatmap"),
      summary.fun = ir_param("ir_p_vg_summary", "percent"),
      exportTable = FALSE,
      palette = "inferno"
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
    scRepertoire::percentGenes(
      data,
      chain = specific_chain(),
      gene = ir_param("ir_p_pg_gene", "Vgene"),
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      summary.fun = ir_param("ir_p_pg_summary", "percent"),
      exportTable = FALSE,
      palette = "inferno"
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
    scRepertoire::percentVJ(
      data,
      chain = specific_chain(),
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      summary.fun = ir_param("ir_p_vj_summary", "percent"),
      exportTable = FALSE,
      palette = "inferno"
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
  safeRenderPlot(
    scRepertoire::percentAA(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      aa.length = aa_len,
      exportTable = FALSE,
      palette = "inferno"
    ),
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
    scRepertoire::positionalEntropy(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      aa.length = aa_len,
      method = ir_param("ir_p_pe_method", "norm.entropy"),
      exportTable = FALSE,
      palette = "inferno"
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
    scRepertoire::positionalProperty(
      data,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      method = method,
      aa.length = as.numeric(ir_param("ir_p_pp_aa_length", 20)),
      exportTable = FALSE,
      palette = "inferno"
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
    scRepertoire::percentKmer(
      data,
      chain = pars$chain,
      cloneCall = pars$cloneCall,
      group.by = pars$groupBy,
      motif.length = as.numeric(ir_param("ir_p_motif_length", 3)),
      min.depth = as.numeric(ir_param("ir_p_min_depth", 3)),
      top.motifs = top_m,
      exportTable = FALSE,
      palette = "inferno"
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
