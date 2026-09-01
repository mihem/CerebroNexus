## ---- Compare (alluvial) helpers --------------------------------------- ##
## Interactive Plotly replacement for scRepertoire's alluvial clonalCompare.
## scRepertoire draws the alluvial with ggalluvial's GeomFlow/GeomStratum, which
## ggplotly() cannot convert cleanly, so we take clonalCompare(exportTable=TRUE)
## and redraw the plot ourselves as native Plotly polygons: one stacked bar per
## group, smooth ribbons linking a clone between adjacent groups. Keeping the
## data prep and the drawing in pure functions (no reactives) lets them be unit
## tested directly with sys.source(), like the other IR *_helpers.R files.

## Half-width of each group's stacked bar, in x units (groups sit at 1, 2, ...).
IR_COMPARE_BAR_HALF_WIDTH <- 0.16
## Points used to trace one ribbon edge (smoothstep between the two bars).
IR_COMPARE_RIBBON_STEPS <- 40L
## Legend labels longer than this are truncated (hover keeps the full clone).
IR_COMPARE_LEGEND_MAXCHAR <- 46L
## Dark hairline border on each clone rectangle, so adjacent same-hue segments
## (many IGH clones share a green/teal) stay visually separated.
IR_COMPARE_RECT_BORDER <- "#333333"
## Compare uses its OWN palette, not the module-wide IR_PALETTE ("Harmonic").
## Harmonic is a low-saturation gold->green->blue ramp: fine for a handful of
## groups, but with ~20 top clones stacked together the neighbours become
## indistinguishable. "Dark 3" is a qualitative HCL palette spanning the full
## hue circle at higher chroma, so adjacent clones get clearly different hues.
IR_COMPARE_PALETTE <- "Dark 3"

## Give a plotly figure the shared cell-view modebar look ("theme a"):
## hide the Plotly logo and drop the clutter buttons, mirroring the REACT_CONFIG
## in www/cell_views.js so every IR plot's toolbar matches the projection
## tabs. The two custom selection buttons there are tied to the shared scatter
## engine's box-select, which the bar/point IR figures don't have, so only the
## styling is mirrored here. Defined in this helpers file (sourced before
## visualizations.R) so both the Compare renderer and ir_render_ggplotly can use
## it, and so it is present when compare_helpers.R is sys.source()d in tests.
ir_apply_theme_a_modebar <- function(fig) {
  plotly::config(
    fig,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "zoom2d",
      "autoScale2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian",
      "toggleSpikelines"
    )
  )
}

## Turn a clonalCompare(exportTable = TRUE) table into the geometry the Plotly
## renderer draws. Returns a list with:
##   $ok        TRUE if there is anything to draw
##   $message   empty-state text when $ok is FALSE
##   $groups    the ordered group names (x positions 1..n)
##   $value_col "Proportion" or "Count" (whichever the table carried)
##   $clones    the clone levels, in stacking order (bottom -> top)
##   $rects     data.frame: clone, group_index, ymin, ymax, value
##   $ribbons   data.frame: clone, from_index, to_index and the four y bounds
## The table columns are clones | (Proportion|Count) | Sample; the value column
## name depends on the `proportion` argument to clonalCompare, so it is detected
## rather than hard-coded.
ir_prepare_compare_alluvial <- function(tab, groups, proportion = TRUE) {
  empty <- function(msg) {
    list(ok = FALSE, message = msg)
  }
  if (is.null(tab) || !is.data.frame(tab) || nrow(tab) == 0L) {
    return(empty("No clones to compare for the current selection."))
  }
  clone_col <- "clones"
  group_col <- "Sample"
  value_col <- if ("Proportion" %in% colnames(tab)) {
    "Proportion"
  } else if ("Count" %in% colnames(tab)) {
    "Count"
  } else {
    ## Fall back to the first numeric column that is not obviously an index.
    num_cols <- names(which(vapply(tab, is.numeric, logical(1))))
    if (length(num_cols) == 0L) {
      return(empty("Compare table has no numeric value column."))
    }
    num_cols[1]
  }
  if (!all(c(clone_col, group_col) %in% colnames(tab))) {
    return(empty("Compare table is missing its clone/sample columns."))
  }

  tab[[clone_col]] <- as.character(tab[[clone_col]])
  tab[[group_col]] <- as.character(tab[[group_col]])
  tab[[value_col]] <- suppressWarnings(as.numeric(tab[[value_col]]))
  tab <- tab[
    !is.na(tab[[value_col]]) & tab[[value_col]] > 0,
    ,
    drop = FALSE
  ]
  if (nrow(tab) == 0L) {
    return(empty("All selected clones have zero abundance."))
  }

  ## Group order follows the user's selection; drop groups absent from the table
  ## and keep only groups that actually appear, so x positions stay contiguous.
  if (is.null(groups) || length(groups) == 0L) {
    groups <- unique(tab[[group_col]])
  }
  groups <- as.character(groups)
  groups <- groups[groups %in% unique(tab[[group_col]])]
  if (length(groups) < 2L) {
    return(empty("Select at least two groups to compare."))
  }

  ## clone_levels is one shared stacking order used by EVERY column: clones
  ## ranked by total abundance across the shown groups (largest at the bottom),
  ## ties by name. Because all columns stack in the same order, a clone keeps a
  ## consistent vertical band and its ribbons run roughly parallel without
  ## crossing — the alluvial's whole point is tracking one clone across groups,
  ## so ribbon legibility wins over per-column size sorting. It also drives the
  ## colour assignment and the legend order.
  totals <- tapply(tab[[value_col]], tab[[clone_col]], sum)
  clone_levels <- names(sort(totals, decreasing = TRUE))
  ties <- order(-as.numeric(totals[clone_levels]), clone_levels)
  clone_levels <- clone_levels[ties]

  ## Per group: stack clones bottom -> top in the shared clone_levels order, so a
  ## clone keeps a consistent vertical band across the groups where it appears.
  rects <- vector("list", length(groups))
  for (gi in seq_along(groups)) {
    g <- groups[gi]
    sub <- tab[tab[[group_col]] == g, , drop = FALSE]
    vals <- setNames(sub[[value_col]], sub[[clone_col]])
    present <- clone_levels[clone_levels %in% names(vals)]
    v <- as.numeric(vals[present])
    ymax <- cumsum(v)
    ymin <- ymax - v
    rects[[gi]] <- data.frame(
      clone = present,
      group_index = gi,
      ymin = ymin,
      ymax = ymax,
      value = v,
      stringsAsFactors = FALSE
    )
  }
  rects <- do.call(rbind, rects)

  ## Ribbons: for each clone, connect consecutive groups where it is present in
  ## BOTH the source and target group (private-in-one-group clones get no
  ## ribbon, only their rectangle).
  ribbon_rows <- list()
  for (cl in clone_levels) {
    cr <- rects[rects$clone == cl, , drop = FALSE]
    cr <- cr[order(cr$group_index), , drop = FALSE]
    if (nrow(cr) < 2L) {
      next
    }
    for (k in seq_len(nrow(cr) - 1L)) {
      a <- cr[k, ]
      b <- cr[k + 1L, ]
      if (b$group_index != a$group_index + 1L) {
        next # only link directly adjacent groups
      }
      ribbon_rows[[length(ribbon_rows) + 1L]] <- data.frame(
        clone = cl,
        from_index = a$group_index,
        to_index = b$group_index,
        from_ymin = a$ymin,
        from_ymax = a$ymax,
        to_ymin = b$ymin,
        to_ymax = b$ymax,
        stringsAsFactors = FALSE
      )
    }
  }
  ribbons <- if (length(ribbon_rows)) {
    do.call(rbind, ribbon_rows)
  } else {
    data.frame(
      clone = character(0),
      from_index = integer(0),
      to_index = integer(0),
      from_ymin = numeric(0),
      from_ymax = numeric(0),
      to_ymin = numeric(0),
      to_ymax = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    ok = TRUE,
    message = "",
    groups = groups,
    value_col = value_col,
    proportion = isTRUE(proportion),
    clones = clone_levels,
    ## Per-clone total across the shown groups, in clone_levels order — used to
    ## annotate each legend entry with the clone's overall size.
    totals = as.numeric(totals[clone_levels]),
    rects = rects,
    ribbons = ribbons
  )
}

## Deterministic clone -> colour map. Same clone keeps its colour regardless of
## group order; colours come from the shared IR palette direction.
ir_compare_clone_colors <- function(
  clone_levels,
  palette = IR_COMPARE_PALETTE
) {
  n <- length(clone_levels)
  if (n == 0L) {
    return(setNames(character(0), character(0)))
  }
  cols <- grDevices::hcl.colors(n, palette = palette)
  setNames(cols, clone_levels)
}

## Truncate a long clone name for the legend (hover keeps the full name), and
## append the clone's overall size so the legend annotates block magnitude. The
## value is the clone's total across the shown groups: a proportion (3 sig figs)
## or a raw count. `total = NULL` keeps the bare name (used by tests / callers
## that don't have totals).
ir_compare_legend_label <- function(
  clone,
  total = NULL,
  proportion = TRUE,
  maxchar = IR_COMPARE_LEGEND_MAXCHAR
) {
  name <- ifelse(
    nchar(clone) > maxchar,
    paste0(substr(clone, 1, maxchar - 1L), "…"),
    clone
  )
  if (is.null(total) || length(total) != 1L || is.na(total)) {
    return(name)
  }
  val <- if (isTRUE(proportion)) {
    formatC(total, format = "g", digits = 3)
  } else {
    formatC(total, format = "d", big.mark = ",")
  }
  paste0(name, " (", trimws(val), ")")
}

## Smoothstep interpolation used for the ribbon edges (flat tangents at both
## ends give the classic alluvial S-curve).
ir_compare_smoothstep <- function(from, to, t) {
  s <- 3 * t^2 - 2 * t^3
  from + (to - from) * s
}

## Accumulate NA-separated sub-polygons into a single (x, y) path, carrying an
## optional per-vertex hover text so each rectangle can show its own value.
ir_compare_path_builder <- function() {
  xs <- numeric(0)
  ys <- numeric(0)
  txt <- character(0)
  list(
    push = function(px, py, ptxt = NA_character_) {
      if (length(xs)) {
        xs <<- c(xs, NA_real_)
        ys <<- c(ys, NA_real_)
        txt <<- c(txt, NA_character_)
      }
      xs <<- c(xs, px)
      ys <<- c(ys, py)
      txt <<- c(txt, rep(ptxt, length.out = length(px)))
    },
    get = function() list(x = xs, y = ys, text = txt)
  )
}

## Format the hover string for one clone rectangle: full clone name (never
## truncated), its group, and the value in the current mode.
ir_compare_hover_text <- function(clone, group, value, proportion) {
  val <- if (isTRUE(proportion)) {
    paste0("Proportion: ", trimws(formatC(value, format = "g", digits = 3)))
  } else {
    paste0("Count: ", trimws(formatC(value, format = "d", big.mark = ",")))
  }
  paste0(clone, "<br>Group: ", group, "<br>", val)
}

## Closed rectangle polygons for one clone (one per group it appears in). These
## get the dark border, drawn as their own Plotly trace so the outline does not
## also stroke the ribbons. The fill itself carries NO hover text — hovering a
## filled multi-polygon repeats every vertex's text, which is the mess we avoid;
## the hover comes from centre-point anchor markers instead (see below).
ir_compare_clone_rects <- function(prep, clone) {
  hw <- IR_COMPARE_BAR_HALF_WIDTH
  b <- ir_compare_path_builder()
  cr <- prep$rects[prep$rects$clone == clone, , drop = FALSE]
  for (i in seq_len(nrow(cr))) {
    gi <- cr$group_index[i]
    xl <- gi - hw
    xr <- gi + hw
    b$push(
      c(xl, xr, xr, xl, xl),
      c(cr$ymin[i], cr$ymin[i], cr$ymax[i], cr$ymax[i], cr$ymin[i])
    )
  }
  b$get()
}

## One invisible hover anchor at the centre of each rectangle for a clone, each
## carrying a single clean hover string (clone + group + value). This is what
## the user hovers — a marker gives one tooltip per rectangle instead of the
## fill's repeated per-vertex text.
ir_compare_clone_anchors <- function(prep, clone) {
  cr <- prep$rects[prep$rects$clone == clone, , drop = FALSE]
  if (nrow(cr) == 0L) {
    return(list(x = numeric(0), y = numeric(0), text = character(0)))
  }
  x <- cr$group_index
  y <- (cr$ymin + cr$ymax) / 2
  text <- vapply(
    seq_len(nrow(cr)),
    function(i) {
      ir_compare_hover_text(
        clone,
        prep$groups[cr$group_index[i]],
        cr$value[i],
        prep$proportion
      )
    },
    character(1)
  )
  list(x = x, y = y, text = text)
}

## Smooth ribbon polygons linking one clone across adjacent groups. Drawn as a
## separate borderless trace: an outline on a ribbon that changes width reads as
## a crease, and with per-column sorting the ribbons now cross, so keeping them
## outline-free keeps the crossings legible.
ir_compare_clone_ribbons <- function(prep, clone) {
  hw <- IR_COMPARE_BAR_HALF_WIDTH
  b <- ir_compare_path_builder()
  rb <- prep$ribbons[prep$ribbons$clone == clone, , drop = FALSE]
  t <- seq(0, 1, length.out = IR_COMPARE_RIBBON_STEPS)
  for (i in seq_len(nrow(rb))) {
    x_from <- rb$from_index[i] + hw
    x_to <- rb$to_index[i] - hw
    x_seq <- x_from + (x_to - x_from) * t
    lower <- ir_compare_smoothstep(rb$from_ymin[i], rb$to_ymin[i], t)
    upper <- ir_compare_smoothstep(rb$from_ymax[i], rb$to_ymax[i], t)
    ## lower edge left->right, then upper edge right->left, closed.
    px <- c(x_seq, rev(x_seq), x_seq[1])
    py <- c(lower, rev(upper), lower[1])
    b$push(px, py)
  }
  b$get()
}

## Assemble the full Plotly figure from a prepared alluvial. `theme`, `axis`,
## `hoverlabel` are the shared cerebro_plotly_* lists (passed in so this stays a
## pure function testable without the Shiny app's plotting_functions.R).
ir_compare_alluvial_plotly <- function(
  prep,
  palette = IR_COMPARE_PALETTE,
  theme = NULL,
  hoverlabel = NULL,
  legend_pos = "right",
  base_size = NULL,
  title = NULL
) {
  colors <- ir_compare_clone_colors(prep$clones, palette = palette)
  ## Spell out that the block height IS the value, so users don't have to hover
  ## or open the guide to learn what the y-axis measures.
  y_title <- if (isTRUE(prep$proportion)) {
    "Proportion of repertoire (block height)"
  } else {
    "Clone count (block height)"
  }
  font_family <- if (!is.null(theme)) theme$font else NULL
  tick_col <- if (!is.null(theme)) theme$tick else "#6b6b70"
  title_col <- if (!is.null(theme)) theme$title else "#1c1c1e"
  grid_col <- if (!is.null(theme)) theme$grid else "#ececec"
  axis_col <- if (!is.null(theme)) theme$axis else "#e0e0e0"

  fig <- plotly::plot_ly()
  for (ci in seq_along(prep$clones)) {
    cl <- prep$clones[ci]
    cl_total <- if (is.null(prep$totals)) NULL else prep$totals[ci]
    ## Ribbons first (drawn behind), borderless and slightly translucent so a
    ## clone's band stays readable; not in the legend and not hoverable (the
    ## rectangle anchors carry all the hover).
    ribbons <- ir_compare_clone_ribbons(prep, cl)
    if (length(ribbons$x) > 0L) {
      fig <- plotly::add_trace(
        fig,
        x = ribbons$x,
        y = ribbons$y,
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = colors[[cl]],
        line = list(color = colors[[cl]], width = 0),
        legendgroup = cl,
        showlegend = FALSE,
        hoverinfo = "skip",
        opacity = 0.55
      )
    }
    ## Rectangles on top, with a dark hairline border so adjacent same-family
    ## colours stay distinct. This trace carries the clone's legend entry. Its
    ## fill has hover disabled (a filled multi-polygon would repeat the text at
    ## every vertex); the hover lives on the anchor markers below instead.
    rects <- ir_compare_clone_rects(prep, cl)
    if (length(rects$x) == 0L) {
      next
    }
    fig <- plotly::add_trace(
      fig,
      x = rects$x,
      y = rects$y,
      type = "scatter",
      mode = "lines",
      fill = "toself",
      fillcolor = colors[[cl]],
      line = list(color = IR_COMPARE_RECT_BORDER, width = 1),
      name = ir_compare_legend_label(cl, cl_total, prep$proportion),
      legendgroup = cl,
      hoverinfo = "skip",
      opacity = 1
    )
    ## Invisible hover anchors: one marker at each rectangle's centre, each with
    ## a single clean tooltip (clone + group + value). Toggling the clone in the
    ## legend hides these too (same legendgroup).
    anchors <- ir_compare_clone_anchors(prep, cl)
    if (length(anchors$x) > 0L) {
      fig <- plotly::add_trace(
        fig,
        x = anchors$x,
        y = anchors$y,
        type = "scatter",
        mode = "markers",
        marker = list(size = 14, color = colors[[cl]], opacity = 0),
        legendgroup = cl,
        showlegend = FALSE,
        hoverinfo = "text",
        text = anchors$text,
        hoverlabel = list(
          bgcolor = colors[[cl]],
          bordercolor = "#333333",
          font = list(color = "#ffffff", size = 12)
        )
      )
    }
  }

  xaxis <- list(
    title = "",
    tickmode = "array",
    tickvals = seq_along(prep$groups),
    ticktext = prep$groups,
    zeroline = FALSE,
    showgrid = FALSE,
    showline = TRUE,
    linecolor = axis_col,
    tickfont = list(color = tick_col, family = font_family),
    fixedrange = FALSE
  )
  yaxis <- list(
    title = y_title,
    rangemode = "tozero",
    zeroline = FALSE,
    gridcolor = grid_col,
    showline = TRUE,
    linecolor = axis_col,
    tickfont = list(color = tick_col, family = font_family),
    titlefont = list(color = title_col, family = font_family)
  )
  if (!is.null(base_size)) {
    bs <- suppressWarnings(as.numeric(base_size))
    if (length(bs) == 1L && !is.na(bs) && bs > 0) {
      xaxis$tickfont$size <- bs
      yaxis$tickfont$size <- bs
      yaxis$titlefont$size <- bs
    }
  }

  layout_args <- list(
    fig,
    xaxis = xaxis,
    yaxis = yaxis,
    hovermode = "closest",
    showlegend = TRUE,
    legend = list(
      x = if (identical(legend_pos, "bottom")) 0 else 1.02,
      y = if (identical(legend_pos, "bottom")) -0.2 else 1,
      orientation = if (identical(legend_pos, "bottom")) "h" else "v",
      font = list(family = font_family, color = title_col)
    ),
    paper_bgcolor = "rgba(255, 255, 255, 0)",
    plot_bgcolor = "rgba(255, 255, 255, 0)"
  )
  if (!is.null(hoverlabel)) {
    layout_args$hoverlabel <- hoverlabel
  }
  if (!is.null(title) && nzchar(title)) {
    layout_args$title <- list(text = title, font = list(color = title_col))
  }
  if (identical(legend_pos, "none")) {
    layout_args$showlegend <- FALSE
  }
  fig <- do.call(plotly::layout, layout_args)
  ir_apply_theme_a_modebar(fig)
}
