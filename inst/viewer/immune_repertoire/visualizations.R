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

## ---- Visualizations UI ------------------------------------------------ ##
output$ir_visualizations_UI <- renderUI({
  if (!has_scRepertoire()) {
    return(ir_scRepertoire_missing_ui())
  }
  # Building the tab strip only needs to know whether repertoire data exists.
  # Do not depend on ir_data(): that reactive follows the active grouping
  # controls so it can join only requested metadata. Rebuilding the whole
  # tabset on every grouping change resets the selected tab before the
  # corresponding controls/output can update.
  if (!ir_repertoire_available()) {
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
      "Clonal projection",
      value = "Clonal UMAP"
      # Drawn by the shared cell-view host in UI.R, grouped or not.
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
      shinycssloaders::withSpinner(uiOutput(
        "ir_ui_pairedScatter"
      ))
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
    # tabPanel("Rarefaction", uiOutput("ir_ui_clonalRarefaction")),
    # tabPanel("Gene usage", uiOutput("ir_ui_percentGeneUsage")),
    # tabPanel("vizGenes", uiOutput("ir_ui_vizGenes")),
    # tabPanel("percentGenes", uiOutput("ir_ui_percentGenes")),
    # tabPanel("percentVJ", uiOutput("ir_ui_percentVJ")),
    # tabPanel("AA %", uiOutput("ir_ui_percentAA")),
    # tabPanel(
    #   "Entropy",
    #   ir_fill_plot("ir_plot_positionalEntropy")
    # ),
    # tabPanel("Property", uiOutput("ir_ui_positionalProperty")),
    # tabPanel(
    #   # Top motifs now lives in the settings panel (IR_PARAM_SPEC "K-mer").
    #   "K-mer",
    #   uiOutput("ir_ui_percentKmer")
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

  shiny::tagAppendAttributes(
    do.call(
      tabsetPanel,
      c(list(id = "ir_tabs", selected = selected_tab), tabs)
    ),
    class = "cerebro-analysis-tabs"
  )
})

##------------------------------------------------------------------------##
## Plot renderers
##------------------------------------------------------------------------##

## ---- Clonal UMAP -------------------------------------------------------- ##
## Overlays clone expansion level on the cell projection (UMAP/tSNE). Data is
## built in data.R (ir_clonal_umap_data); here we draw the coloured scatter.
## Point size / opacity come from the shared scatter display options.
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
      cerebro_plotly_toolbar(fig)
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
  idx <- if (
    "cell_index" %in%
      colnames(df) &&
      all(df$cell_index >= 1L & df$cell_index <= nrow(md))
  ) {
    as.integer(df$cell_index)
  } else {
    match(df$barcode, md$cell_barcode)
  }
  df$barcode <- as.character(md$cell_barcode[idx])
  group_values <- as.character(md[[group_by]][idx])
  group_levels <- tryCatch(getGroupLevels(group_by), error = function(e) NULL)
  if (is.null(group_levels) || length(group_levels) == 0) {
    group_levels <- unique(group_values)
  }
  group_levels <- group_levels[
    !is.na(group_levels) & nzchar(group_levels) & group_levels %in% group_values
  ]
  df$.umap_group <- factor(group_values, levels = group_levels)
  ungrouped <- sum(is.na(df$.umap_group))
  df <- df[!is.na(df$.umap_group), , drop = FALSE]
  if (nrow(df) == 0) {
    return(NULL)
  }
  attr(df, "ungrouped_cells") <- ungrouped
  df
}

## Every group is a panel with its own canvas and full-length arrays, so the
## grouped Clonal projection draws at most as many panels as the gene panels.
IR_CLONAL_UMAP_MAX_PANELS <- 12L

## Decides whether grouped data (from ir_umap_grouped_data) can be drawn as
## panels. `df` is NULL when it cannot; the projection is then drawn without
## grouping, so no earlier picture stays on screen. `notice` (or NULL) says
## why, or how many cells have no group.
ir_clonal_umap_grouping <- function(
  df,
  group_by,
  max_panels = IR_CLONAL_UMAP_MAX_PANELS
) {
  if (is.null(df) || nrow(df) == 0L) {
    return(list(
      df = NULL,
      type = "warning",
      notice = paste0(
        "No cells shown have a value in `",
        group_by,
        "`, so the projection is drawn without grouping."
      )
    ))
  }
  n_groups <- nlevels(droplevels(df$.umap_group))
  if (n_groups > max_panels) {
    return(list(
      df = NULL,
      type = "warning",
      notice = paste0(
        "`",
        group_by,
        "` has ",
        n_groups,
        " groups among the cells shown, more than the ",
        max_panels,
        " panels the Clonal projection draws, so it is drawn without ",
        "grouping. Filter the groups in the settings or group by another column."
      )
    ))
  }
  ungrouped <- attr(df, "ungrouped_cells") %||% 0L
  list(
    df = df,
    type = "message",
    notice = if (ungrouped > 0L) {
      paste0(
        format(ungrouped, big.mark = ","),
        if (ungrouped == 1L) " cell has" else " cells have",
        " no value in `",
        group_by,
        "` and ",
        if (ungrouped == 1L) "is" else "are",
        " not drawn."
      )
    }
  )
}

ir_projection_display_name <- function(projection) {
  key <- tolower(as.character(projection %||% "projection"))
  switch(
    key,
    tsne = "t-SNE",
    umap = "UMAP",
    pca = "PCA",
    as.character(projection %||% "projection")
  )
}

ir_projection_axis_names <- function(projection) {
  label <- toupper(gsub(
    "[^[:alnum:]]+",
    "",
    ir_projection_display_name(projection)
  ))
  paste0(label, "_", 1:2)
}

output[["ir_selection_status_UI"]] <- renderUI({
  if (!has_scRepertoire() || !ir_repertoire_available()) {
    return(NULL)
  }
  active_tab <- input[["ir_tabs"]]
  if (!is.null(active_tab) && !identical(active_tab, "Clonal UMAP")) {
    return(
      div(
        class = "cerebro-selection-status-slot",
        div(
          class = "cerebro-selection-status-guide",
          tags$span(
            class = "cerebro-selection-status-kicker",
            icon("arrow-pointer"),
            "Selection workspace"
          ),
          tags$span(
            class = "cerebro-selection-status-text",
            "Selection is available in Clonal projection."
          )
        )
      )
    )
  }
  cerebroSelectionStatus(
    "ir_clonalUMAP_projection",
    "ir_clonalUMAP_number_of_selected_cells",
    portable = FALSE
  )
})

## Draw the Clonal UMAP through the shared cell-view engine. Without a grouping
## column we marshal a grey "Other cells" background + one trace per expansion
## level as the meta/data/hover arrays the shared renderer consumes. With one,
## every group gets its own panel of the same engine (ir_clonal_umap_panels), so
## the grouped view zooms, hovers and brushes like the ungrouped one and a brush
## in any panel selects exactly the cells drawn there.
## One panel per group, all drawn by the same engine view. `df` has one row per
## cell (x, y, expansion, barcode and the group in `.umap_group`), so every cell
## belongs to exactly one panel and a brush selects the barcodes drawn inside it.
## Cells without the receptor come first and the most expanded clones last, so
## they are painted on top. Panels share axis ranges and are titled with their
## group and the number of cells that carry the receptor.
ir_clonal_umap_panels <- function(df, projection) {
  present <- as.character(df$expansion)
  present[is.na(present)] <- "Other cells"
  order_rows <- order(
    !is.na(df$expansion),
    as.integer(df$expansion),
    na.last = FALSE
  )
  df <- df[order_rows, , drop = FALSE]
  label <- present[order_rows]
  levels <- c(
    if (any(label == "Other cells")) "Other cells",
    intersect(IR_CLONE_LABELS, label)
  )
  colors <- c(
    if (any(label == "Other cells")) "#D9D9D9",
    unname(IR_EXPANSION_COLORS[setdiff(levels, "Other cells")])
  )
  pad <- function(values) {
    span <- diff(range(values, na.rm = TRUE))
    range(values, na.rm = TRUE) + c(-1, 1) * max(span, 1e-6) * 0.04
  }
  x_range <- pad(df$x)
  y_range <- pad(df$y)
  axes <- ir_projection_axis_names(projection)
  keys <- as.character(df$barcode)
  groups <- df$.umap_group
  panels <- lapply(seq_along(levels(groups)), function(index) {
    rows <- which(groups == levels(groups)[[index]])
    if (!length(rows)) {
      return(NULL)
    }
    carriers <- sum(!is.na(df$expansion[rows]))
    list(
      id = paste0("group", index),
      label = paste0(
        levels(groups)[[index]],
        "  ·  ",
        format(carriers, big.mark = ","),
        if (carriers == 1L) " cell" else " cells",
        " with receptor"
      ),
      selection_key = keys[rows],
      x = as.numeric(df$x[rows]),
      y = as.numeric(df$y[rows]),
      x_range = x_range,
      y_range = y_range,
      hover = paste0(
        keys[rows],
        "<br>",
        label[rows],
        "<br>",
        axes[[1]],
        ": ",
        formatC(df$x[rows], format = "f", digits = 2),
        "<br>",
        axes[[2]],
        ": ",
        formatC(df$y[rows], format = "f", digits = 2)
      )
    )
  })
  list(
    keys = keys,
    group = label,
    levels = levels,
    colors = colors,
    panels = panels[!vapply(panels, is.null, logical(1))]
  )
}

ir_clonal_umap_last_request <- reactiveVal(NULL)

observe(
  {
    render_request <- input[["ir_clonalUMAP_projection_render_request"]]
    dataset_context <- viewer_loaded_dataset_context()
    req(
      is.list(render_request),
      viewerDatasetContextEqual(render_request$dataset_context, dataset_context)
    )
    req(render_request)
    req(
      identical(input[["sidebar"]], "immune_repertoire"),
      identical(input[["ir_tabs"]], "Clonal UMAP")
    )
    group_by <- ir_param("ir_p_umap_group_by", "")
    grouped <- !is.null(group_by) && nzchar(group_by)

    ## Receptor and projection arrive from the visible toolbar. Show-all and the
    ## appearance controls live in the unopened settings drawer, so use their
    ## declared server defaults until the user opens and changes them.
    receptor <- ir_param("ir_p_umap_receptor", {
      choices <- ir_receptor_types()
      if (length(choices)) unname(choices[[1L]]) else NULL
    })
    projection <- ir_param("ir_p_umap_projection", {
      choices <- tryCatch(availableProjections(), error = function(e) {
        character()
      })
      if (length(choices)) unname(choices[[1L]]) else NULL
    })
    show_all <- ir_param("ir_p_umap_show_all", TRUE)
    req(receptor, projection)
    group_labels <- input[["ir_clonalUMAP_group_labels"]]
    display <- current_page_display_defaults("ir")
    if (is.null(group_labels)) {
      group_labels <- display$group_labels
    }
    point_border <- isTRUE(
      input[["ir_clonalUMAP_point_border"]] %||% display$draw_border
    )
    keep_square <- isTRUE(
      input[["ir_clonalUMAP_keep_square"]] %||% display$keep_square
    )
    clone_call <- "gene"
    show_all <- isTRUE(show_all)
    cells <- ir_umap_cells_to_show()
    appearance <- current_scatter_defaults()
    point_size <- appearance$point_size
    alpha <- appearance$point_opacity
    percentage <- appearance$percentage_cells_to_show
    identity <- tryCatch(viewerDatasetIdentity(), error = function(e) list())
    request_signature <- list(
      render_request = render_request,
      dataset_fingerprint = as.character(identity$fingerprint %||% ""),
      cell_order_fingerprint = as.character(identity$order_fingerprint %||% ""),
      receptor = as.character(receptor),
      projection = as.character(projection),
      show_all = show_all,
      cells = cells,
      point_size = point_size,
      alpha = alpha,
      percentage = percentage,
      group_labels = isTRUE(group_labels),
      point_border = point_border,
      keep_square = keep_square,
      group_by = if (grouped) as.character(group_by) else ""
    )
    previous_signature <- ir_clonal_umap_last_request()
    if (identical(previous_signature, request_signature)) {
      return()
    }
    if (grouped) {
      df <- ir_clonal_umap_data(
        projection,
        receptor,
        clone_call,
        show_all = show_all,
        cells = cells,
        percentage = percentage
      )
      req(!is.null(df))
      grouping <- ir_clonal_umap_grouping(
        ir_umap_grouped_data(df, group_by),
        group_by
      )
      if (is.null(grouping$notice)) {
        removeNotification("ir-clonal-umap-grouping")
      } else {
        showNotification(
          grouping$notice,
          type = grouping$type,
          id = "ir-clonal-umap-grouping"
        )
      }
      grouped <- !is.null(grouping$df)
    } else {
      removeNotification("ir-clonal-umap-grouping")
    }
    if (grouped) {
      df <- grouping$df
      grouped_view <- ir_clonal_umap_panels(df, projection)
      isolate(
        cerebroCellViewRender(
          "ir_clonalUMAP_projection",
          list(
            color_type = "categorical",
            space_label = paste(
              "Clonal",
              ir_projection_display_name(projection)
            ),
            color_variable = "expansion",
            traces = grouped_view$levels,
            group_colors = grouped_view$colors,
            appearance = list(
              group_labels = FALSE,
              draw_border = point_border,
              keep_square = keep_square
            )
          ),
          list(
            selection_key = grouped_view$keys,
            group = grouped_view$group,
            panels = grouped_view$panels,
            point_size = point_size,
            point_opacity = alpha,
            point_line = if (point_border) {
              list(color = "rgb(196,196,196)", width = 1)
            } else {
              list()
            },
            reset_axes = TRUE
          ),
          list(hoverinfo = "text"),
          dataset_context = dataset_context
        )
      )
      ir_clonal_umap_last_request(request_signature)
      return()
    }
    render_data <- ir_clonal_umap_data(
      projection,
      receptor,
      clone_call,
      show_all = show_all,
      cells = cells,
      percentage = percentage,
      split_output = TRUE
    )
    req(!is.null(render_data), length(render_data$x) > 0L)
    ## Grey background = cells without the selected receptor (expansion = NA);
    ## coloured foreground = receptor cells with an expansion level. One trace per
    ## expansion level, in canonical order, so each keeps its turbo colour.
    trace_codes <- render_data$trace_codes
    traces <- ifelse(
      is.na(trace_codes),
      "Other cells",
      IR_CLONE_LABELS[trace_codes]
    )
    data_color <- ifelse(
      is.na(trace_codes),
      "#D9D9D9",
      unname(IR_EXPANSION_COLORS[IR_CLONE_LABELS[trace_codes]])
    )
    trace_lengths <- lengths(render_data$x)
    axes <- ir_projection_axis_names(projection)
    req(length(traces) > 0)
    deferred_aux <- function() {
      data_keys <- c(
        if (is.na(trace_codes[[1L]])) {
          list(render_data$background_key)
        } else {
          list()
        },
        lapply(
          render_data$coloured_rows,
          function(rows) render_data$coloured_key[rows]
        )
      )
      uses_cell_index <- identical(render_data$key_type, "cell_index")
      key_lengths <- lengths(data_keys)
      if (isTRUE(uses_cell_index)) {
        flat_barcodes <- ir_clonal_umap_barcodes(data.frame(
          cell_index = unlist(data_keys, use.names = FALSE)
        ))
        req(length(flat_barcodes) == sum(key_lengths))
        barcodes_by_trace <- unname(split(
          flat_barcodes,
          rep(seq_along(key_lengths), key_lengths)
        ))
      } else {
        barcodes_by_trace <- lapply(data_keys, as.character)
      }
      req(identical(lengths(barcodes_by_trace), key_lengths))
      hover_info <- ifelse(traces == "Other cells", "skip", "text")
      hover_text <- Map(
        function(barcodes, x, y, label) {
          if (identical(label, "Other cells")) {
            return("")
          }
          paste0(
            barcodes,
            "<br>",
            label,
            "<br>",
            axes[[1]],
            ": ",
            formatC(x, format = "f", digits = 2),
            "<br>",
            axes[[2]],
            ": ",
            formatC(y, format = "f", digits = 2)
          )
        },
        barcodes_by_trace,
        render_data$x,
        render_data$y,
        traces
      )
      list(
        selection_key = barcodes_by_trace,
        hover = list(hoverinfo = as.list(hover_info), text = hover_text)
      )
    }

    output_meta <- list(
      color_type = "categorical",
      space_label = paste("Clonal", ir_projection_display_name(projection)),
      traces = traces,
      color_variable = "expansion",
      appearance = list(
        group_labels = isTRUE(group_labels),
        draw_border = point_border,
        keep_square = keep_square
      )
    )
    output_data <- list(
      x = render_data$x,
      y = render_data$y,
      deferred_selection_lengths = trace_lengths,
      color = as.list(data_color),
      point_size = point_size,
      point_opacity = alpha,
      point_line = if (point_border) {
        list(color = "rgb(196,196,196)", width = 1)
      } else {
        list()
      },
      reset_axes = TRUE
    )
    output_hover <- list(hoverinfo = "skip")

    isolate(
      cerebroCellViewRender(
        "ir_clonalUMAP_projection",
        output_meta,
        output_data,
        output_hover,
        deferred_aux = deferred_aux,
        dataset_context = dataset_context
      )
    )
    ir_clonal_umap_last_request(request_signature)
  },
  priority = 1000
)

## ---- Clonal UMAP selection summaries ----------------------------------- ##
output[["ir_clonalUMAP_number_of_selected_cells"]] <- renderUI({
  sel <- input[["ir_clonalUMAP_projection_persistent_selection"]]
  if (
    !is.null(sel) &&
      (!is.list(sel) ||
        !viewerDatasetContextEqual(
          sel[["dataset_context"]],
          viewer_loaded_dataset_context()
        ))
  ) {
    sel <- NULL
  }
  cerebroSelectionSummary(sel, "Clonal projection")
})

output[["ir_clonalUMAP_projection_composition"]] <- renderUI({
  sel <- input[["ir_clonalUMAP_projection_persistent_selection"]]
  if (
    !is.null(sel) &&
      (!is.list(sel) ||
        !viewerDatasetContextEqual(
          sel[["dataset_context"]],
          viewer_loaded_dataset_context()
        ))
  ) {
    sel <- NULL
  }
  cerebroSelectionSummary(sel, "Clonal projection", composition = TRUE)
})

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
      "Paired scatter needs at least two groups. Choose a multi-group column in Compare by."
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
  dot_size <- ir_param("ir_p_dot_size", "total")
  needed <- unique(c(
    x_axis,
    y_axis,
    if (!identical(dot_size, "total")) dot_size
  ))
  if (length(needed) && all(needed %in% names(data))) {
    data <- data[needed]
  }
  p <- scRepertoire::clonalScatter(
    data,
    cloneCall = pars$cloneCall,
    chain = pars$chain,
    x.axis = x_axis,
    y.axis = y_axis,
    dot.size = dot_size,
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
  req_scRepertoire()
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
  req_scRepertoire()
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
        shiny::validate(shiny::need(
          requireNamespace("patchwork", quietly = TRUE),
          "Install the patchwork package to show faceted paired scatter plots."
        ))
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

ir_clonal_abundance_plotly <- function(counts) {
  if (!nrow(counts)) {
    return(ir_empty_plotly("No clonotypes are available for this selection."))
  }
  counts$hover <- paste0(
    "<b>",
    counts$sample,
    "</b><br>",
    "Abundance: ",
    formatC(counts$abundance, format = "d"),
    "<br>",
    "Clones: ",
    formatC(counts$n_clones, format = "d")
  )
  plot <- plotly::plot_ly(
    counts,
    x = ~abundance,
    y = ~n_clones,
    split = ~sample,
    color = ~sample,
    type = "scatter",
    mode = "lines",
    text = ~hover,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      xaxis = cerebro_plotly_axis("Abundance", mirror = FALSE, type = "log"),
      yaxis = cerebro_plotly_axis(
        "Number of clones",
        mirror = FALSE,
        rangemode = "tozero"
      ),
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      legend = list(title = list(text = "Samples"))
    )
  cerebro_plotly_toolbar(plot)
}

output$ir_plot_clonalAbundance <- plotly::renderPlotly({
  req_plot_space("ir_plot_clonalAbundance")
  pars <- ir_params()
  ## Tab navigation can reach this output before renderUI has bound its
  ## controls. Do not pass NULL chain/cloneCall into scRepertoire during that gap.
  req(pars$chain, pars$cloneCall)
  scale <- isTRUE(ir_param("ir_p_scale", FALSE))
  order_by <- ir_order_by()
  if (
    identical(pars$chain, "both") &&
      is.null(pars$groupBy) &&
      !scale &&
      is.null(order_by)
  ) {
    return(ir_clonal_abundance_plotly(ir_clonal_abundance_counts(
      ir_data(),
      ir_clonecall_col(pars$cloneCall)
    )))
  }
  data <- ir_data()
  req(!is.null(data))
  req_scRepertoire()
  ir_render_ggplotly(
    scRepertoire::clonalAbundance(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      group.by = pars$groupBy,
      order.by = order_by,
      scale = scale
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
  req_scRepertoire()
  req_plot_space("ir_plot_clonalCompare")
  samples <- input$ir_compare_samples
  req(!is.null(samples) && length(samples) >= 2)
  data <- ir_data()
  req(!is.null(data))
  pars <- ir_params()
  proportion <- isTRUE(ir_param("ir_p_compare_prop", TRUE))

  tryCatch(
    {
      tab <- scRepertoire::clonalCompare(
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
      ir_compare_alluvial_plotly(
        prep,
        palette = IR_COMPARE_PALETTE,
        theme = cerebro_plotly_theme(),
        hoverlabel = cerebro_plotly_hoverlabel()
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

output$ir_plot_clonalDiversity <- plotly::renderPlotly({
  req_scRepertoire()
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
  req_plot_space("ir_plot_clonalHomeostasis")
  pars <- ir_params()
  req(pars$chain, pars$cloneCall)
  # Dataset switches can deliver the old grouping before its selector resets.
  if (!is.null(pars$groupBy)) {
    req(pars$groupBy %in% getGroups())
  }
  if (identical(pars$chain, "both") && is.null(ir_order_by())) {
    return(ir_render_ggplotly(
      {
        counts <- if (is.null(pars$groupBy)) {
          ir_homeostasis_counts(ir_data(), ir_clonecall_col(pars$cloneCall))
        } else {
          ir_grouped_homeostasis_counts(
            ir_data(),
            ir_clonecall_col(pars$cloneCall),
            pars$groupBy
          )
        }
        ir_homeostasis_plot(counts, ir_clone_size(), IR_PALETTE)
      },
      "clonalHomeostasis"
    ))
  }
  req_scRepertoire()
  data <- ir_data()
  req(!is.null(data))
  ir_render_ggplotly(
    scRepertoire::clonalHomeostasis(
      data,
      cloneCall = pars$cloneCall,
      chain = pars$chain,
      cloneSize = ir_clone_size(),
      group.by = pars$groupBy,
      order.by = ir_order_by(),
      exportTable = FALSE,
      palette = IR_PALETTE
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
  req_scRepertoire()
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
        palette = IR_PALETTE
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
  req_scRepertoire()
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
      palette = IR_PALETTE
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
  req_scRepertoire()
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
      palette = IR_PALETTE
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
  req_scRepertoire()
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
      palette = IR_PALETTE
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
  req_scRepertoire()
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
  req_scRepertoire()
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
  req_plot_space("ir_plot_clonalSizeDistribution")
  req(input$ir_chain, !is.null(input$ir_groupBy))
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
  row_count <- sum(vapply(data, nrow, integer(1)))
  ir_render_ggplotly(
    if (row_count >= 100000L) {
      # The upstream implementation creates a dense clonotype-by-sample
      # matrix. On the Ren atlas that blocked the only Shiny R process for
      # close to a minute and made every subsequently selected tab appear
      # broken. Cluster exact empirical clone-size profiles instead; this path
      # is linear in repertoire rows and keeps the same group/linkage controls.
      ir_size_profile_plot(
        ir_size_distribution_profile(),
        method = ir_param("ir_p_sd_method", "ward.D2")
      )
    } else {
      req_scRepertoire()
      p <- scRepertoire::clonalSizeDistribution(
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
  req_scRepertoire()
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
  req_scRepertoire()
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
  req_scRepertoire()
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
  req_scRepertoire()
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
  req_scRepertoire()
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
      palette = IR_PALETTE
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
  req_scRepertoire()
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
  req_scRepertoire()
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
  req_scRepertoire()
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
  ## Self-made plot (ir_build_definition_plot in data.R) — does NOT call
  ## scRepertoire, so gate on the cheap availability probe only and never load
  ## the namespace here.
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
  ## Self-made plot (ir_build_sharing_plot in data.R) — does NOT call
  ## scRepertoire, so gate on the cheap availability probe only and never load
  ## the namespace here.
  req(has_scRepertoire())
  req_plot_space("ir_plot_cloneSharing")
  req(input$ir_chain, input$ir_sharing_unit, !is.null(input$ir_groupBy))
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
    # ggplotly ignores the ggplot `legend.position` and drops a horizontal
    # legend along the TOP, where it collided with the plot title. Move it below
    # the plot so the title sits cleanly on its own row.
    fig <- plotly::layout(
      fig,
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.14,
        yanchor = "top"
      )
    )
  }
  fig
}) %>%
  ir_bindCache(
    input$ir_chain,
    input$ir_groupBy,
    input$ir_sharing_unit
  )
