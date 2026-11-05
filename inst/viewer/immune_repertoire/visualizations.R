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
  # Building the tab strip only needs to know whether repertoire data exists:
  # rebuilding the whole tabset resets the selected tab before the
  # corresponding controls/output can update.
  if (!ir_repertoire_available()) {
    return(div(
      class = "alert alert-warning",
      "No immune repertoire data available."
    ))
  }

  ## Every sub-tab, keyed by its value; the App shows the ones the Builder
  ## kept for this dataset (all of them without a choice) that its sample
  ## count supports: Compare and SizeDist need at least two samples.
  tabs <- list(
    "Clonal UMAP" = tabPanel(
      # Clonal expansion overlaid on the cell UMAP — the default landing tab,
      # so the first thing the user sees is where expanded clones sit.
      "Clonal projection",
      value = "Clonal UMAP"
      # Drawn by the shared cell-view host in UI.R, grouped or not.
    ),
    "Abundance" = tabPanel(
      "Abundance",
      ir_fill_plot("ir_plot_clonalAbundance", plotly = TRUE)
    ),
    "Diversity" = tabPanel(
      "Diversity",
      ir_fill_plot("ir_plot_clonalDiversity", plotly = TRUE)
    ),
    "Homeostasis" = tabPanel(
      "Homeostasis",
      ir_fill_plot("ir_plot_clonalHomeostasis", plotly = TRUE)
    ),
    "Isotype" = tabPanel(
      "Isotype",
      ir_fill_plot("ir_plot_isotype", plotly = TRUE)
    ),
    "Paired Scatter" = tabPanel(
      "Paired Scatter",
      shinycssloaders::withSpinner(uiOutput(
        "ir_ui_pairedScatter"
      ))
    ),
    "Clone Sharing" = tabPanel(
      "Clone Sharing",
      ir_fill_plot("ir_plot_cloneSharing", plotly = TRUE)
    ),
    "Compare" = tabPanel(
      "Compare",
      ir_fill_plot("ir_plot_clonalCompare", plotly = TRUE)
    ),
    "SizeDist" = tabPanel(
      "SizeDist",
      ir_fill_plot("ir_plot_clonalSizeDistribution", plotly = TRUE)
    )
  )
  tabs <- unname(tabs[ir_visible_tabs()])

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
  ## Plots built as Plotly traces directly only need the shared toolbar.
  if (inherits(p, "plotly")) {
    return(cerebro_plotly_toolbar(p))
  }
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
      ir_plot_failed()
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
  ## Cells named by canonical row read their group from the first-frame
  ## metadata instead of hydrating the full table; their barcodes are looked
  ## up only when the frame has to carry them (the Clonal projection observer).
  first <- if ("cell_index" %in% colnames(df)) {
    tryCatch(viewerProjectionFirstFrameMetadata(), error = function(e) NULL)
  }
  if (
    is.data.frame(first) &&
      group_by %in% colnames(first) &&
      isTRUE(all(df$cell_index >= 1L & df$cell_index <= nrow(first)))
  ) {
    idx <- as.integer(df$cell_index)
    group_values <- as.character(first[[group_by]][idx])
  } else {
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
  }
  ## Canonical (metadata) row of each cell: the grouped frame can name cells by
  ## row against the cell-name resource instead of carrying every barcode.
  df$.cell_row <- as.integer(idx)
  group_levels <- tryCatch(getGroupLevels(group_by), error = function(e) NULL)
  if (is.null(group_levels) || length(group_levels) == 0) {
    group_levels <- unique(group_values)
  }
  group_levels <- group_levels[
    !is.na(group_levels) & nzchar(group_levels) & group_levels %in% group_values
  ]
  df$.umap_group <- factor(group_values, levels = group_levels)
  ungrouped <- sum(is.na(df$.umap_group))
  if (ungrouped > 0L) {
    df <- df[!is.na(df$.umap_group), , drop = FALSE]
  }
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
## Rows are ordered by group, so each panel is one slice (`start`, 0-based, and
## `count`) of the frame's x, y and keys; the frame carries every cell once.
## Within a panel, cells without the receptor come first and the most expanded
## clones last, so they are painted on top. Panels share axis ranges and are
## titled with their group and the number of cells that carry the receptor.
ir_clonal_umap_panels <- function(df, projection) {
  groups <- df$.umap_group
  order_rows <- order(
    as.integer(groups),
    !is.na(df$expansion),
    as.integer(df$expansion),
    na.last = FALSE
  )
  ## Only the vectors the frame sends are put in panel order, not the frame.
  groups <- groups[order_rows]
  expansion <- df$expansion[order_rows]
  label <- as.character(expansion)
  label[is.na(label)] <- "Other cells"
  x <- as.numeric(df$x[order_rows])
  y <- as.numeric(df$y[order_rows])
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
  x_range <- pad(x)
  y_range <- pad(y)
  counts <- tabulate(as.integer(groups), nbins = nlevels(groups))
  starts <- cumsum(c(0L, counts))[seq_along(counts)]
  panels <- lapply(seq_along(levels(groups)), function(index) {
    if (!counts[[index]]) {
      return(NULL)
    }
    rows <- starts[[index]] + seq_len(counts[[index]])
    carriers <- sum(!is.na(expansion[rows]))
    list(
      id = paste0("group", index),
      label = paste0(
        levels(groups)[[index]],
        "  ·  ",
        format(carriers, big.mark = ","),
        if (carriers == 1L) " cell" else " cells",
        " with receptor"
      ),
      start = as.integer(starts[[index]]),
      count = as.integer(counts[[index]]),
      x_range = x_range,
      y_range = y_range
    )
  })
  list(
    ## NULL when the cells are named by canonical row only.
    keys = if ("barcode" %in% names(df)) as.character(df$barcode[order_rows]),
    rows = if (".cell_row" %in% names(df)) as.integer(df$.cell_row[order_rows]),
    x = x,
    y = y,
    group_code = match(label, levels) - 1L,
    levels = levels,
    colors = colors,
    panels = panels[!vapply(panels, is.null, logical(1))]
  )
}

ir_clonal_umap_last_request <- reactiveVal(NULL)
## Labels, border and square layout of the drawn frame. They are browser
## patches, as on Projection: a change redraws the frame in place instead of
## building and sending it again.
ir_clonal_umap_drawn_appearance <- reactiveVal(NULL)
ir_clonal_umap_appearance <- function(grouped) {
  display <- current_page_display_defaults("ir")
  list(
    group_labels = !grouped &&
      isTRUE(input[["ir_clonalUMAP_group_labels"]] %||% display$group_labels),
    draw_border = isTRUE(
      input[["ir_clonalUMAP_point_border"]] %||% display$draw_border
    ),
    keep_square = isTRUE(
      input[["ir_clonalUMAP_keep_square"]] %||% display$keep_square
    )
  )
}
observeEvent(
  list(
    input[["ir_clonalUMAP_group_labels"]],
    input[["ir_clonalUMAP_point_border"]],
    input[["ir_clonalUMAP_keep_square"]]
  ),
  {
    drawn <- ir_clonal_umap_drawn_appearance()
    req(!is.null(drawn), identical(input[["sidebar"]], "immune_repertoire"))
    values <- ir_clonal_umap_appearance(isTRUE(drawn$grouped))
    if (identical(values, drawn$values)) {
      return()
    }
    identity <- viewerDatasetIdentity()
    req(identity$fingerprint)
    session$sendCustomMessage(
      "cell_view_appearance",
      list(
        id = "ir_clonalUMAP_projection",
        dataset_context = viewer_loaded_dataset_context(),
        dataset_fingerprint = identity$fingerprint,
        values = values
      )
    )
    ir_clonal_umap_drawn_appearance(utils::modifyList(
      drawn,
      list(values = values)
    ))
  },
  ignoreInit = TRUE
)
ir_clonal_umap_data_memo <- viewerMemoOne()

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
      identical(ir_tab(), "Clonal UMAP")
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
    ## Appearance is read without a dependency: labels, border and square are
    ## patched by the observer above, point size and opacity by the browser.
    display_values <- isolate(ir_clonal_umap_appearance(FALSE))
    group_labels <- display_values$group_labels
    point_border <- display_values$draw_border
    keep_square <- display_values$keep_square
    clone_call <- "gene"
    show_all <- isTRUE(show_all)
    cells <- ir_umap_cells_to_show()
    appearance <- current_scatter_defaults()
    point_size <- isolate(appearance$point_size)
    alpha <- isolate(appearance$point_opacity)
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
      percentage = percentage,
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
      ## Name cells by canonical row when the browser can resolve the rows from
      ## the cell-name resource; otherwise the frame carries the barcodes.
      names_resource <- viewerCellNamesResourceFor(grouped_view$rows)
      cell_names <- if (is.list(names_resource)) {
        list(
          selection_index = grouped_view$rows - 1L,
          cell_names_resource = names_resource
        )
      } else {
        list(
          selection_key = grouped_view$keys %||%
            as.character(getMetaData()$cell_barcode[grouped_view$rows])
        )
      }
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
          c(
            cell_names,
            list(
              group_code = grouped_view$group_code,
              x = grouped_view$x,
              y = grouped_view$y,
              panels = grouped_view$panels,
              point_size = point_size,
              point_opacity = alpha,
              point_line = if (point_border) {
                list(color = "rgb(196,196,196)", width = 1)
              } else {
                list()
              },
              reset_axes = TRUE
            )
          ),
          list(hoverinfo = "text"),
          dataset_context = dataset_context
        )
      )
      ir_clonal_umap_last_request(request_signature)
      ir_clonal_umap_drawn_appearance(list(
        grouped = TRUE,
        values = isolate(ir_clonal_umap_appearance(TRUE))
      ))
      return()
    }
    ## Appearance-only changes (labels, border, square, point size/opacity)
    ## reuse the coordinates built for the same data request. The memo stores
    ## only usable results: a failed req() leaves it untouched.
    data_key <- c(
      list(dataset_context = dataset_context),
      request_signature[c(
        "dataset_fingerprint",
        "cell_order_fingerprint",
        "receptor",
        "projection",
        "show_all",
        "cells",
        "percentage"
      )]
    )
    render_data <- ir_clonal_umap_data_memo(data_key, function() {
      out <- ir_clonal_umap_data(
        projection,
        receptor,
        clone_call,
        show_all = show_all,
        cells = cells,
        percentage = percentage,
        split_output = TRUE
      )
      req(!is.null(out), length(out$x) > 0L)
      out
    })
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
      key_lengths <- lengths(data_keys)
      ## The single-view tooltip shows the cell name and the expansion level
      ## from the frame; per-cell hover text would never be displayed.
      hover <- list(
        hoverinfo = as.list(ifelse(traces == "Other cells", "skip", "text"))
      )
      names_resource <- viewerCellNamesResource()
      if (is.list(names_resource)) {
        ## Canonical rows (0-based); the browser has the names.
        return(list(
          selection_index = lapply(data_keys, function(keys) {
            I(as.integer(keys) - 1L)
          }),
          cell_names_resource = names_resource,
          hover = hover
        ))
      }
      flat_barcodes <- ir_clonal_umap_barcodes(data.frame(
        cell_index = unlist(data_keys, use.names = FALSE)
      ))
      req(length(flat_barcodes) == sum(key_lengths))
      barcodes_by_trace <- unname(split(
        flat_barcodes,
        rep(seq_along(key_lengths), key_lengths)
      ))
      list(selection_key = barcodes_by_trace, hover = hover)
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
    ir_clonal_umap_drawn_appearance(list(
      grouped = FALSE,
      values = display_values
    ))
  },
  ## Ahead of the page's controls (priority 0), after the dataset's ready
  ## message (priority 1000) that the browser must see before the frame.
  priority = 100
)

## ---- Clonal UMAP selection summaries ----------------------------------- ##
viewerSelectionSummaryOutputs(
  "ir_clonalUMAP_projection",
  "ir_clonalUMAP_number_of_selected_cells",
  function() {
    selection <- input[["ir_clonalUMAP_projection_persistent_selection"]]
    if (is.list(selection)) {
      viewerPersistentSelection(selection, viewer_loaded_dataset_context())
    }
  },
  source = function() "Clonal projection"
)

## ---- BCR-specific renderers --------------------------------------------- ##
output$ir_plot_isotype <- plotly::renderPlotly({
  req_plot_space("ir_plot_isotype")
  encoded <- ir_encoded()
  req(!is.null(encoded))
  gb <- ir_params()$groupBy
  group_col <- if (is.null(gb)) "sample" else gb
  p <- bcr_isotype_plot(
    encoded,
    ir_row_values(encoded, group_col, joined = !is.null(gb)),
    group_col = group_col
  )
  if (is.null(p)) {
    return(ir_empty_plotly(
      "No BCR isotype data available. Requires BCR data with CTgene column."
    ))
  }
  ir_render_ggplotly(p, "isotype")
}) %>%
  ## Isotypes come from CTgene alone: clone call and chain do not change them.
  ir_bindCache(input$ir_groupBy)

## ---- Paired Scatter (generic) ------------------------------------------- ##
ir_sample_meta <- reactive({
  encoded <- ir_encoded()
  if (is.null(encoded) || length(encoded$samples) < 2) {
    return(NULL)
  }
  sample_level_meta(encoded, ir_cell_metadata(), ir_params()$groupBy)
})

## The group names and sample metadata below read the whole repertoire (seconds
## on a million cells). Only the tabs that compare groups need them, so the
## observers wait for those tabs instead of reading it when the page opens.
ir_pair_tab_shown <- reactive({
  isTRUE(input$ir_tabs %in% c("Paired Scatter", "Compare"))
})

## Whether there are two groups to compare, published on change only: another
## grouping updates the choices of the controls in place (below) instead of
## rebuilding them and the plot container, which drew the plot a second time.
ir_paired_scatter_ready <- reactiveVal(NULL)
observe({
  req(ir_pair_tab_shown())
  viewerPublishChanged(
    ir_paired_scatter_ready,
    length(ir_compare_groups()) >= 2
  )
})
observeEvent(
  if (ir_pair_tab_shown()) ir_sample_meta(),
  {
    choices <- ir_paired_scatter_choices(ir_sample_meta())
    pair_choices <- c(
      "Manual group comparison" = "",
      choices$compare_candidates
    )
    pair_mode <- isolate(input$ir_pair_compare)
    req(!is.null(pair_mode))
    if (!(pair_mode %in% pair_choices)) {
      pair_mode <- ""
    }
    updateSelectInput(
      session,
      "ir_pair_compare",
      choices = pair_choices,
      selected = pair_mode
    )
    facet <- isolate(input$ir_pair_facet)
    if (!is.null(facet)) {
      updateSelectInput(
        session,
        "ir_pair_facet",
        choices = c("(none)" = "", choices$facet_candidates),
        selected = if (facet %in% choices$facet_candidates) facet else ""
      )
    }
  },
  ignoreInit = TRUE
)
observeEvent(
  if (ir_pair_tab_shown()) ir_compare_groups(),
  {
    groups <- ir_compare_groups()
    req(length(groups) >= 2)
    x <- isolate(input$ir_pair_x_group)
    y <- isolate(input$ir_pair_y_group)
    req(!is.null(x), !is.null(y))
    if (!(x %in% groups)) {
      x <- groups[1]
    }
    if (!(y %in% groups) || identical(y, x)) {
      y <- setdiff(groups, x)[[1]]
    }
    updateSelectInput(
      session,
      "ir_pair_x_group",
      choices = groups,
      selected = x
    )
    updateSelectInput(
      session,
      "ir_pair_y_group",
      choices = groups,
      selected = y
    )
  },
  ignoreInit = TRUE
)

output$ir_ui_pairedScatter <- renderUI({
  ready <- ir_paired_scatter_ready()
  req(!is.null(ready))
  groups <- isolate(ir_compare_groups())
  if (!isTRUE(ready)) {
    return(div(
      class = "alert alert-info",
      "Paired scatter needs at least two groups. Choose a multi-group column in Compare by."
    ))
  }
  meta <- isolate(ir_sample_meta())
  choices <- ir_paired_scatter_choices(meta)
  pair_choices <- c("Manual group comparison" = "", choices$compare_candidates)
  pair_mode <- input$ir_pair_compare
  if (is.null(pair_mode) || !(pair_mode %in% pair_choices)) {
    pair_mode <- ""
  }
  ## The groups only preselect the controls: choosing another group must not
  ## rebuild them, and the plot output with them.
  x <- isolate(input$ir_pair_x_group)
  y <- isolate(input$ir_pair_y_group)
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

## Picking the X group as the Y group moves Y to another group, as rebuilding
## the controls used to.
observeEvent(list(input$ir_pair_x_group, input$ir_pair_y_group), {
  x <- input$ir_pair_x_group
  y <- input$ir_pair_y_group
  ## The event list is never NULL, so this runs when the page opens: decide
  ## before reading the groups, which reads the whole repertoire.
  req(!is.null(x), identical(x, y))
  groups <- ir_compare_groups()
  req(length(groups) >= 2)
  updateSelectInput(
    session,
    "ir_pair_y_group",
    selected = setdiff(groups, x)[[1]]
  )
})

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

## One panel of the faceted paired scatter, drawn as clonalScatter draws it
## (square-root axes and a dashed diagonal for proportions, one fill per
## class, dot area for the total cells) from ir_scatter_table().
ir_scatter_ggplot <- function(table, x, y, graph = "proportion", title = NULL) {
  proportion <- !identical(graph, "count")
  table$x_value <- if (proportion) table$x_fraction else table$x_n
  table$y_value <- if (proportion) table$y_fraction else table$y_n
  classes <- sort(unique(table$class))
  p <- ggplot2::ggplot(
    table,
    ggplot2::aes(x = x_value, y = y_value, fill = class)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(size = size),
      shape = 21,
      stroke = 0.25
    ) +
    ggplot2::scale_fill_manual(
      values = grDevices::hcl.colors(length(classes), IR_PALETTE, fixup = TRUE)
    ) +
    ggplot2::guides(size = "none") +
    ggplot2::labs(x = x, y = y, fill = "Clone class", title = title) +
    ggplot2::theme_classic()
  if (proportion) {
    p <- p +
      ggplot2::geom_abline(slope = 1, intercept = 0, alpha = 0.4, lty = 2) +
      ggplot2::scale_x_sqrt() +
      ggplot2::scale_y_sqrt()
  }
  p
}

## clonalScatter(exportTable = TRUE) for two compared units without
## scRepertoire: per clone its cells in X and in Y, their fractions, its class
## and its dot size (cells in both, or in the X or Y group alone). `groups`
## are the grouping values of the rows (NULL: X and Y are samples); clones are
## named by `clone_names(ids)`, else by their ids.
ir_scatter_table <- function(
  encoded,
  clone_col,
  chain,
  groups,
  x,
  y,
  dot_size = "total",
  clone_names = NULL
) {
  counts <- ir_compare_table(
    encoded,
    clone_col,
    chain,
    groups,
    units = c(x, y),
    proportion = FALSE,
    clone_names = clone_names
  )
  clones <- unique(counts$clones)
  cells_in <- function(unit) {
    rows <- counts$Sample == unit
    n <- counts$Count[rows][match(clones, counts$clones[rows])]
    n[is.na(n)] <- 0
    n
  }
  x_n <- cells_in(x)
  y_n <- cells_in(y)
  class <- rep(NA_character_, length(clones))
  class[x_n == 1 & y_n == 0] <- paste0(x, ".singlet")
  class[x_n > 1 & y_n == 0] <- paste0(x, ".expanded")
  class[y_n == 1 & x_n == 0] <- paste0(y, ".singlet")
  class[y_n > 1 & x_n == 0] <- paste0(y, ".expanded")
  class[x_n >= 1 & y_n >= 1] <- "dual.expanded"
  data.frame(
    clone = clones,
    x_n = x_n,
    y_n = y_n,
    x_fraction = x_n / sum(x_n),
    y_fraction = y_n / sum(y_n),
    class = class,
    size = switch(dot_size, x = x_n, y = y_n, x_n + y_n),
    stringsAsFactors = FALSE
  )
}

## The paired scatter drawn as clonalScatter draws it (square-root axes for
## proportions, a dashed diagonal, one colour per class), with one point per
## distinct (x, y, class, size): clones that fall on the same spot are drawn
## once and their number is in the hover.
ir_scatter_plotly <- function(table, x, y, graph = "proportion") {
  proportion <- !identical(graph, "count")
  x_value <- if (proportion) table$x_fraction else table$x_n
  y_value <- if (proportion) table$y_fraction else table$y_n
  ## A spot is fixed by the cells in X, in Y and the dot size (the fractions
  ## and the class follow from them): one integer key per clone.
  code <- function(v) match(v, unique(v))
  x_code <- code(table$x_n)
  y_code <- code(table$y_n)
  key <- x_code +
    (y_code - 1) * max(x_code, 0L) +
    (code(table$size) - 1) * max(x_code, 0L) * max(y_code, 0L)
  first <- !duplicated(key)
  clones <- tabulate(match(key, key[first]))
  points <- data.frame(
    x = x_value[first],
    y = y_value[first],
    class = table$class[first],
    size = table$size[first],
    clones = clones,
    stringsAsFactors = FALSE
  )
  classes <- sort(unique(points$class))
  colours <- grDevices::hcl.colors(length(classes), IR_PALETTE, fixup = TRUE)
  axis_max <- max(points$x, points$y)
  scale <- if (proportion) sqrt else identity
  ticks <- pretty(c(0, axis_max))
  ticks <- ticks[ticks <= axis_max * 1.05]
  span <- range(points$size)
  diameter <- if (diff(span) > 0) {
    6 + 14 * sqrt((points$size - span[[1L]]) / diff(span))
  } else {
    rep(10, nrow(points))
  }
  fmt <- function(v) {
    if (proportion) formatC(v, format = "g", digits = 3) else format(v)
  }
  traces <- Map(
    function(class, colour) {
      index <- which(points$class == class)
      list(
        type = "scatter",
        mode = "markers",
        x = scale(points$x[index]),
        y = scale(points$y[index]),
        name = class,
        marker = list(
          color = colour,
          size = diameter[index],
          line = list(color = "black", width = 0.5)
        ),
        text = paste0(
          "<b>",
          class,
          "</b><br>",
          x,
          ": ",
          fmt(points$x[index]),
          "<br>",
          y,
          ": ",
          fmt(points$y[index]),
          "<br>Total n: ",
          points$size[index],
          "<br>Clones: ",
          points$clones[index]
        ),
        hoverinfo = "text"
      )
    },
    classes,
    colours
  )
  axis <- function(title) {
    cerebro_plotly_axis(
      title,
      mirror = FALSE,
      range = scale(c(0, axis_max * 1.05)),
      tickvals = scale(ticks),
      ticktext = fmt(ticks)
    )
  }
  plot <- plotly::plot_ly()
  plot$x$attrs <- list()
  plot$x$data <- unname(traces)
  plot <- plot %>%
    plotly::layout(
      xaxis = axis(x),
      yaxis = axis(y),
      shapes = if (proportion) {
        list(list(
          type = "line",
          x0 = 0,
          y0 = 0,
          x1 = scale(axis_max * 1.05),
          y1 = scale(axis_max * 1.05),
          line = list(color = "rgba(0,0,0,0.4)", dash = "dash", width = 1)
        ))
      },
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      legend = list(title = list(text = "Clone class"))
    )
  plot
}

## "Point size" (a factor on the total-n diameter) and "Point opacity" of the
## display panel, applied in the browser as the sliders move: no R render, and
## the cached plot stays valid. `look` holds the defaults while no slider exists.
ir_paired_scatter_look_js <- "function (el, x, look) {
  var base = x.data.map(function (t) { return t.marker && t.marker.size; });
  var value = function (id, fallback) {
    var e = document.getElementById(id), v = e ? Number(e.value) : NaN;
    return isFinite(v) && v > 0 ? v : fallback;
  };
  el.__irLook = function () {
    var size = value('ir_d_point_size', look.size);
    Plotly.restyle(el, {
      'marker.size': base.map(function (b) {
        if (b == null) return b;
        return Array.isArray(b) ? b.map(function (v) { return v * size; }) : b * size;
      }),
      'marker.opacity': value('ir_d_alpha', look.opacity)
    });
  };
  el.__irLook();
  if (!window.__irPairedLook) {
    window.__irPairedLook = true;
    $(document).on('change', '#ir_d_point_size, #ir_d_alpha', function () {
      var plot = document.getElementById('ir_plot_pairedScatter');
      if (plot && plot.__irLook) plot.__irLook();
    });
  }
}"

## Single-panel paired scatter -> interactive plotly, counted in the app for
## the two compared groups only.
output$ir_plot_pairedScatter <- plotly::renderPlotly({
  req_plot_space("ir_plot_pairedScatter")
  encoded <- ir_encoded()
  req(!is.null(encoded))
  pars <- ir_params()
  groups <- ir_compare_groups()
  req(length(groups) >= 2)
  pair_mode <- input$ir_pair_compare
  if (is.null(pair_mode) || !nzchar(pair_mode)) {
    ## Groups of the previous grouping, before its controls are rebuilt: keep
    ## the drawn plot until the controls send the new groups.
    x <- input$ir_pair_x_group
    y <- input$ir_pair_y_group
    req(isTRUE(x %in% groups), isTRUE(y %in% groups), cancelOutput = TRUE)
    validate(need(x != y, "Select two different groups."))
    ## X and Y are levels of the grouping column, not samples.
    group_by <- pars$groupBy
  } else {
    meta <- ir_sample_meta()
    req(!is.null(meta))
    lvls <- sort(unique(meta[[pair_mode]]))
    req(length(lvls) == 2L)
    x <- meta$.sample_name[meta[[pair_mode]] == lvls[1]][1]
    y <- meta$.sample_name[meta[[pair_mode]] == lvls[2]][1]
    group_by <- NULL
  }
  ir_render_ggplotly(
    ir_scatter_plotly(
      ir_scatter_table(
        encoded,
        ir_clonecall_col(pars$cloneCall),
        pars$chain,
        ir_row_values(encoded, group_by),
        x,
        y,
        ir_param("ir_p_dot_size", "total")
      ),
      x,
      y,
      ir_param("ir_p_graph", "proportion")
    ),
    "pairedScatter"
  ) %>%
    htmlwidgets::onRender(
      ir_paired_scatter_look_js,
      data = with(
        current_page_display_defaults("ir"),
        list(size = scatter_point_size, opacity = scatter_opacity)
      )
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
  req_plot_space("ir_plot_pairedScatter_facet")
  encoded <- ir_encoded()
  req(!is.null(encoded))
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
            panels[[length(panels) + 1L]] <- ir_scatter_ggplot(
              ir_scatter_table(
                encoded,
                ir_clonecall_col(pars$cloneCall),
                pars$chain,
                NULL,
                s_a[1],
                s_b[1],
                ir_param("ir_p_dot_size", "total")
              ),
              s_a[1],
              s_b[1],
              ir_param("ir_p_graph", "proportion"),
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

## Units in clonalAbundance()'s order: sorted, or with order.by = "alphanumeric"
## by their letters, then by their number.
ir_abundance_units <- function(units, order_by = NULL) {
  units <- sort(unique(as.character(units)))
  if (identical(order_by, "alphanumeric")) {
    units <- units[order(
      gsub("[0-9]", "", units),
      suppressWarnings(as.numeric(gsub("[^0-9]", "", units)))
    )]
  }
  units
}

## clonalAbundance()'s scale = TRUE: the density of log10(abundance) over the
## clones of a unit (bandwidth 0.5, 512 points across every unit's range),
## scaled to a maximum of 1.
ir_abundance_density <- function(counts, units) {
  x <- log10(counts$abundance)
  range <- range(x)
  if (diff(range) == 0) {
    range <- range + c(-0.5, 0.5)
  }
  lapply(units, function(unit) {
    index <- which(counts$sample == unit)
    weights <- counts$n_clones[index]
    density <- stats::density(
      x[index],
      weights = weights / sum(weights),
      bw = 0.5,
      from = range[[1L]],
      to = range[[2L]],
      n = 512L
    )
    list(x = 10^density$x, y = density$y / max(density$y))
  })
}

ir_clonal_abundance_plotly <- function(
  counts,
  scale = FALSE,
  order_by = NULL,
  legend = "Samples",
  prebuild = TRUE
) {
  if (!nrow(counts)) {
    return(ir_empty_plotly("No clonotypes are available for this selection."))
  }
  ## One line per unit, coloured as plot_ly(split = ~sample, color = ~sample)
  ## would: Set2 (RColorBrewer::brewer.pal(max(n, 3), "Set2")) interpolated
  ## over the units. Lines only, as clonalAbundance() draws them: a marker
  ## attribute makes plotly add markers to the mode (19,000 more SVG points
  ## on 187 samples, 0.2 s in the browser). Building the traces directly takes 0.2 s for 187 samples
  ## instead of 1.4 s in plotly_build's attribute mapping.
  samples <- ir_abundance_units(counts$sample, order_by)
  set2 <- c(
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854",
    "#FFD92F",
    "#E5C494",
    "#B3B3B3"
  )
  palette <- set2[seq_len(min(max(length(samples), 3L), length(set2)))]
  colours <- plotly::toRGB(scales::col_factor(palette, levels = samples)(
    samples
  ))
  if (isTRUE(scale)) {
    curves <- ir_abundance_density(counts, samples)
    traces <- Map(
      function(curve, sample, colour) {
        list(
          type = "scatter",
          mode = "lines",
          x = curve$x,
          y = curve$y,
          fill = "tozeroy",
          fillcolor = plotly::toRGB(colour, 0.5),
          line = list(color = "black", width = 0.5),
          text = paste0(
            "<b>",
            sample,
            "</b><br>Abundance: ",
            formatC(curve$x, format = "g", digits = 3),
            "<br>Scaled density: ",
            formatC(curve$y, format = "f", digits = 3)
          ),
          hoverinfo = "text",
          name = sample
        )
      },
      curves,
      samples,
      colours
    )
    y_title <- "Density of clones"
  } else {
    hover <- paste0(
      "<b>",
      counts$sample,
      "</b><br>",
      "Abundance: ",
      formatC(counts$abundance, format = "d"),
      "<br>",
      "Clones: ",
      formatC(counts$n_clones, format = "d")
    )
    rows <- split(
      seq_len(nrow(counts)),
      factor(counts$sample, levels = samples)
    )
    traces <- Map(
      function(index, sample, colour) {
        index <- index[order(counts$abundance[index])]
        list(
          type = "scatter",
          mode = "lines",
          x = counts$abundance[index],
          y = counts$n_clones[index],
          text = hover[index],
          hoverinfo = "text",
          name = sample,
          line = list(color = colour)
        )
      },
      rows,
      samples,
      colours
    )
    y_title <- "Number of clones"
  }
  plot <- plotly::plot_ly()
  plot$x$attrs <- list()
  plot$x$data <- unname(traces)
  plot <- plot %>%
    plotly::layout(
      xaxis = cerebro_plotly_axis("Abundance", mirror = FALSE, type = "log"),
      yaxis = cerebro_plotly_axis(
        y_title,
        mirror = FALSE,
        rangemode = "tozero"
      ),
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      legend = list(title = list(text = legend))
    )
  plot <- cerebro_plotly_toolbar(plot)
  if (prebuild) cerebro_plotly_prebuilt(plot) else plot
}

## The clone-size histogram depends on the clones only; scale and order are
## drawn from it, so changing them does not count the clones again.
ir_abundance_counts <- reactive({
  pars <- ir_params()
  req(pars$chain, pars$cloneCall)
  ir_abundance_counts_for(pars, ir_encoded())
}) %>%
  ir_bindCache(
    input$ir_cloneCall,
    input$ir_chain,
    input$ir_groupBy
  )

output$ir_plot_clonalAbundance <- plotly::renderPlotly({
  req_plot_space("ir_plot_clonalAbundance")
  pars <- ir_params()
  ## Tab navigation can reach this output before renderUI has bound its
  ## controls. Do not count clones for a NULL chain/cloneCall during that gap.
  req(pars$chain, pars$cloneCall)
  ir_clonal_abundance_plotly(
    ir_abundance_counts(),
    scale = isTRUE(ir_param("ir_p_scale", FALSE)),
    order_by = ir_order_by(),
    legend = pars$groupBy %||% "Samples"
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
  req_plot_space("ir_plot_clonalCompare")
  samples <- input$ir_compare_samples
  req(!is.null(samples) && length(samples) >= 2)
  ## Samples of the previous grouping, before its selector is rebuilt: keep the
  ## drawn plot until the selector sends the new groups.
  req(all(samples %in% ir_compare_groups()), cancelOutput = TRUE)
  encoded <- ir_encoded()
  req(!is.null(encoded))
  pars <- ir_params()
  proportion <- isTRUE(ir_param("ir_p_compare_prop", TRUE))

  tryCatch(
    {
      ## clonalCompare(exportTable = TRUE), counted for the compared groups
      ## only. The alluvial below follows the selection order, so order.by
      ## does not change it.
      clone_col <- ir_clonecall_col(pars$cloneCall)
      tab <- ir_compare_table(
        encoded,
        clone_col,
        chain = pars$chain,
        groups = ir_row_values(encoded, pars$groupBy),
        units = samples,
        top_clones = as.numeric(ir_param("ir_p_top_clones", 10)),
        proportion = proportion,
        clone_rank = ir_clone_rank(encoded, clone_col, pars$chain),
        clone_names = function(ids) {
          ir_clone_names(encoded, clone_col, pars$chain, ids)
        }
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
      ## validate()/req() keep their usual placeholder.
      if (inherits(e, "shiny.silent.error")) {
        stop(e)
      }
      ir_plot_failed()
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

## The diversity plot as Plotly traces, without ggplotly: one box per x level
## (quartiles and 1.5 IQR whiskers computed here, as geom_boxplot does) and the
## bootstrap values as points, jittered by a fixed sequence, one trace per group.
ir_diversity_plotly <- function(
  table,
  x_col,
  group_col,
  x_label,
  y_label,
  fills
) {
  levels <- levels(table[[x_col]])
  value <- as.numeric(table[["value"]])
  position <- as.integer(table[[x_col]])
  boxes <- lapply(seq_along(levels), function(i) {
    v <- value[position == i & !is.na(value)]
    if (!length(v)) {
      return(NULL)
    }
    q <- stats::quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
    reach <- 1.5 * (q[[3L]] - q[[1L]])
    c(
      x = i,
      q1 = q[[1L]],
      median = q[[2L]],
      q3 = q[[3L]],
      lower = min(v[v >= q[[1L]] - reach]),
      upper = max(v[v <= q[[3L]] + reach])
    )
  })
  boxes <- do.call(rbind, Filter(Negate(is.null), boxes))
  box_trace <- list(
    type = "box",
    x = boxes[, "x"],
    q1 = boxes[, "q1"],
    median = boxes[, "median"],
    q3 = boxes[, "q3"],
    lowerfence = boxes[, "lower"],
    upperfence = boxes[, "upper"],
    width = 0.75,
    boxpoints = FALSE,
    fillcolor = "white",
    line = list(color = "#666666"),
    hoverinfo = "y",
    showlegend = FALSE
  )
  ## Low-discrepancy offsets in [-0.18, 0.18], stable between redraws.
  jitter <- ((seq_along(value) * 0.6180339887) %% 1 - 0.5) * 0.36
  groups <- as.character(table[[group_col]])
  point_traces <- Map(
    function(group, colour) {
      index <- which(groups == group & !is.na(value) & !is.na(position))
      list(
        type = "scatter",
        mode = "markers",
        x = position[index] + jitter[index],
        y = value[index],
        name = group,
        marker = list(
          color = colour,
          size = 9,
          line = list(color = "black", width = 0.5)
        ),
        text = paste0(
          "<b>",
          group,
          "</b><br>",
          levels[position[index]],
          "<br>",
          y_label,
          ": ",
          formatC(value[index], format = "g", digits = 4)
        ),
        hoverinfo = "text"
      )
    },
    names(fills),
    unname(fills)
  )
  plot <- plotly::plot_ly()
  plot$x$attrs <- list()
  plot$x$data <- c(list(box_trace), unname(point_traces))
  plot %>%
    plotly::layout(
      xaxis = cerebro_plotly_axis(
        x_label,
        mirror = FALSE,
        tickvals = seq_along(levels),
        ticktext = levels,
        range = c(0.4, length(levels) + 0.6)
      ),
      yaxis = cerebro_plotly_axis(y_label, mirror = FALSE),
      hoverlabel = cerebro_plotly_hoverlabel(),
      plot_bgcolor = cerebro_plotly_theme()$transparent,
      paper_bgcolor = cerebro_plotly_theme()$transparent,
      legend = list(title = list(text = "Group"))
    )
}

## `groups` and `x_values` are the grouping and x-axis values of the rows.
ir_plot_clonal_diversity <- function(
  encoded,
  clone_call,
  chain,
  group_by,
  metric,
  x_axis,
  n_boots,
  palette = IR_PALETTE,
  plotly = FALSE,
  order_by = NULL,
  groups = NULL,
  x_values = NULL
) {
  # The bootstrap table as clonalDiversity(return.boots = TRUE) returns it,
  # redrawn so each x-axis level gets its own box. Without x_axis the units
  # (samples, or the levels of group_by) are the x-axis.
  output_df <- ir_diversity_table(
    encoded,
    ir_clonecall_col(clone_call),
    chain,
    group_by,
    x_axis,
    metric,
    n_boots,
    groups,
    x_values
  )

  group_col <- if (is.null(group_by)) "Group" else group_by

  # When x_axis is NULL, the table is keyed by the group column
  # alone. Use the group column as the effective x-axis.
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

  # Build x-axis levels: sorted samples or groups (or x_axis levels), or with
  # order.by = "alphanumeric" by their letters, then by their number.
  if (is.null(x_axis)) {
    x_levels <- sort(unique(as.character(output_df[[group_col]])))
  } else {
    x_levels <- sort(unique(as.character(x_values)))
  }
  x_levels <- x_levels[!is.na(x_levels)]
  if (length(x_levels) == 0) {
    x_levels <- unique(as.character(output_df[[eff_x_axis]]))
  }
  if (identical(order_by, "alphanumeric")) {
    x_levels <- ir_abundance_units(x_levels, order_by)
  }
  output_df[[eff_x_axis]] <- factor(
    as.character(output_df[[eff_x_axis]]),
    levels = x_levels
  )
  ## Without x_axis the group column is the x-axis: keep its order.
  if (!identical(group_col, eff_x_axis)) {
    output_df[[group_col]] <- factor(as.character(output_df[[group_col]]))
  }

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

  if (isTRUE(plotly)) {
    return(ir_diversity_plotly(
      output_df,
      eff_x_axis,
      group_col,
      x_label,
      paste(metric_name, "Index Score"),
      fills
    ))
  }
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
  req_plot_space("ir_plot_clonalDiversity")
  encoded <- ir_encoded()
  req(!is.null(encoded))
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
      encoded,
      clone_call = pars$cloneCall,
      chain = pars$chain,
      group_by = pars$groupBy,
      metric = metric,
      x_axis = x_axis,
      n_boots = n_boots,
      palette = IR_PALETTE,
      plotly = TRUE,
      order_by = ir_order_by(),
      groups = ir_row_values(encoded, pars$groupBy),
      x_values = ir_row_values(encoded, x_axis)
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
  ir_render_ggplotly(
    {
      encoded <- ir_encoded()
      req(!is.null(encoded))
      counts <- ir_homeostasis_counts(
        encoded,
        ir_clonecall_col(pars$cloneCall),
        pars$chain,
        ir_row_values(encoded, pars$groupBy)
      )
      ir_homeostasis_plot(counts, ir_clone_size(), IR_PALETTE, ir_order_by())
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

## Exact empirical clone-size profiles of the strict clones, clustered.
## scRepertoire::clonalSizeDistribution() expands a dense clonotype-by-sample
## matrix before fitting its model, which kept the Shiny R process busy for
## close to a minute on Ren's 194 samples; this is linear in the rows and
## keeps the same group, linkage and threshold controls.
output$ir_plot_clonalSizeDistribution <- plotly::renderPlotly({
  req_plot_space("ir_plot_clonalSizeDistribution")
  req(input$ir_chain, !is.null(input$ir_groupBy))
  ir_render_ggplotly(
    ir_size_profile_plot(
      ir_size_distribution_profile(),
      method = ir_param("ir_p_sd_method", "ward.D2")
    ),
    "clonalSizeDistribution"
  )
}) %>%
  ir_bindCache(
    input$ir_chain,
    input$ir_groupBy,
    input$ir_p_sd_method,
    input$ir_p_sd_threshold
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
  encoded <- ir_encoded()
  req(!is.null(encoded))
  fig <- ir_render_ggplotly(
    ir_build_sharing_plot(
      encoded,
      specific_chain(),
      ir_param("ir_sharing_unit", "sample"),
      ir_params()$groupBy,
      ir_cell_metadata()
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
