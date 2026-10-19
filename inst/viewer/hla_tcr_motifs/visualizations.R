##----------------------------------------------------------------------------##
## HLA & TCR Motifs — visualization layer
##
## igraph -> shared Canvas payload, plus the parameter and status panels.
## Colour / legend are applied HERE from the already-built
## graph, so changing colour never rebuilds the graph (see data.R).
##----------------------------------------------------------------------------##

## Above this many colour levels a per-cluster legend is unreadable noise, so it
## is suppressed (metadata legends are unaffected).
HLA_MOTIF_MAX_LEGEND_CLUSTERS <- 12

## Carrier-status colouring is a fixed scale, not an arbitrary categorical one:
## the four states always mean the same thing, so they keep the same order and
## hue across alleles and data sets. Carrier/non-carrier are the contrast the eye
## is meant to make; Mixed sits between them; Untyped is deliberately neutral
## grey so absence of evidence never reads as a finding.
HLA_CARRIER_LEVELS <- c("Carrier", "Non-carrier", "Mixed", "Untyped")
HLA_CARRIER_COLORS <- c(
  "Carrier" = "#d6432f",
  "Non-carrier" = "#3b6fb6",
  "Mixed" = "#b07aa1",
  "Untyped" = "#b8bcc4"
)

## MHC context is a fixed scale for the same reason carrier status is: Class I /
## Class II / Mixed / Unknown always mean the same thing. It used to fall through
## to the generic categorical palette, which assigns colours in the order levels
## HAPPEN to appear among the nodes — so Class I could be blue on one data set
## and red on the next, and the scale silently re-meant itself.
##
## The hues are deliberately disjoint from HLA_CARRIER_COLORS. The two axes are
## orthogonal — carrier status is about the DONOR's genotype, MHC context about
## the CELL's lineage — and sharing red/blue/purple across them invited exactly
## the wrong inference ("red here and red there must be related"). Nothing links
## them, so nothing should look linked. Both scales keep the same neutral grey
## for their no-information level, which is the one thing they really do share.
HLA_CONTEXT_LEVELS <- c("Class I", "Class II", "Mixed", "Unknown")
HLA_CONTEXT_COLORS <- c(
  "Class I" = "#e08214",
  "Class II" = "#0f9b8e",
  "Mixed" = "#8a6d3b",
  "Unknown" = "#b8bcc4"
)

## ---- Build Canvas data from a motif igraph ----------------------------- ##
## Node area is proportional to clone_count; colour follows `color_by` (a node attribute) or the
## motif cluster by default. Tooltip shows CDR3, clone size + fraction, motif
## cluster + consensus + MAX MISMATCH (so a transitive component is never
## implied to be all-pairs <= 1), V/J, and the active colour column's
## distribution.
## `carrier_status` is an optional per-node vector computed at RENDER time (see
## hla_node_carrier_status): the HLA allele is a display choice, so it must never
## enter the cached graph. When supplied it becomes the colour attribute.
## `unit_noun` names what a node's size counts ("cell" for single-cell data,
## "analysis unit" for bulk), so the tooltip never claims cells that do not exist.
hla_build_motif_visnet <- function(
  graph,
  color_by = NULL,
  chain = NULL,
  carrier_status = NULL,
  carrier_counts = NULL,
  carrier_allele = NULL,
  unit_noun = "cell",
  legend_mode = "auto",
  lineage_col = NULL,
  node_scale = 1
) {
  snapshot <- is.list(graph) &&
    identical(graph$version, 1L) &&
    is.list(graph$vertex_attrs)
  if (snapshot) {
    va <- graph$vertex_attrs
    n <- length(va$name)
    if (!n) {
      return(NULL)
    }
  } else {
    if (!hla_motif_graph_ok(graph)) {
      return(NULL)
    }
    va <- igraph::vertex_attr(graph)
    n <- igraph::vcount(graph)
  }
  get_attr <- function(nm) if (nm %in% names(va)) va[[nm]] else rep(NA, n)

  # Carrier status is not a graph attribute; splice it in for this render only.
  use_carrier <- identical(color_by, "hla_carrier") &&
    !is.null(carrier_status) &&
    length(carrier_status) == n
  if (use_carrier) {
    va[["hla_carrier"]] <- carrier_status
  }

  color_col <- if (
    !is.null(color_by) && nzchar(color_by) && color_by %in% names(va)
  ) {
    color_by
  } else {
    "cluster"
  }

  cdr3 <- get_attr("cdr3")
  if (all(is.na(cdr3))) {
    cdr3 <- get_attr("name")
  }
  clone_count <- get_attr("clone_count")
  v_gene <- get_attr("v_gene")
  j_gene <- get_attr("j_gene")
  consensus <- get_attr("motif_consensus")
  diameter <- get_attr("motif_max_mismatch")
  topo_cluster <- as.character(get_attr("cluster"))

  # Explicit colour per level so nodes + legend share one palette.
  group_raw <- as.character(get_attr(color_col))
  use_origin <- identical(color_col, "sample_origin")
  use_context <- identical(color_col, "mhc_context")
  use_pair <- identical(color_col, "pair_allele")
  # The pair's levels are allele NAMES, so they change with the picker — but
  # what they MEAN does not: one is the class I side, one the class II side.
  # Order and hue therefore follow the class, not the string.
  pair_levels <- if (use_pair) {
    present <- unique(group_raw[!is.na(group_raw)])
    alleles <- setdiff(present, HLA_PAIR_MIXED_LABEL)
    cls <- vapply(
      alleles,
      function(a) hla_locus_class(hla_allele_locus(a)),
      character(1)
    )
    c(
      alleles[cls == "Class I"],
      alleles[cls == "Class II"],
      intersect(HLA_PAIR_MIXED_LABEL, present)
    )
  } else {
    character(0)
  }
  levels_ord <- if (color_col == "cluster") {
    as.character(sort(unique(suppressWarnings(as.numeric(group_raw)))))
  } else if (use_carrier) {
    # Fixed, meaningful order — the reader compares carrier against non-carrier,
    # so those must not swap places or change hue between alleles.
    intersect(HLA_CARRIER_LEVELS, unique(group_raw))
  } else if (use_context) {
    # Same reasoning: a fixed scale, fixed order, fixed hues across data sets.
    intersect(HLA_CONTEXT_LEVELS, unique(group_raw))
  } else if (use_pair) {
    pair_levels
  } else if (use_origin) {
    # Samples alphabetical, "Shared" last: it is the level the eye should find,
    # and pinning it to the end keeps its slot stable as samples come and go.
    c(
      sort(setdiff(unique(group_raw[!is.na(group_raw)]), HLA_SHARED_LABEL)),
      intersect(HLA_SHARED_LABEL, group_raw)
    )
  } else {
    unique(group_raw[!is.na(group_raw)])
  }
  levels_ord <- levels_ord[!is.na(levels_ord)]
  level_colors <- if (use_carrier) {
    HLA_CARRIER_COLORS[levels_ord]
  } else if (use_context) {
    HLA_CONTEXT_COLORS[levels_ord]
  } else if (use_pair) {
    # Deliberately the SAME hues as MHC context, because it is the same axis:
    # the class I side is the class I colour. Giving the pair its own palette
    # would teach two colour languages for one distinction — a user who learned
    # that orange means class I here would have to unlearn it one scope over.
    stats::setNames(
      vapply(
        levels_ord,
        function(lv) {
          if (identical(lv, HLA_PAIR_MIXED_LABEL)) {
            unname(HLA_CONTEXT_COLORS[["Mixed"]])
          } else {
            unname(HLA_CONTEXT_COLORS[[
              hla_locus_class(hla_allele_locus(lv))
            ]])
          }
        },
        character(1)
      ),
      levels_ord
    )
  } else if (use_origin) {
    # Per-sample hues, then "Shared" in near-black so a CDR3 recurring across
    # samples reads against every sample colour rather than competing with one.
    per_sample <- setdiff(levels_ord, HLA_SHARED_LABEL)
    stats::setNames(
      c(
        unname(hla_distinct_colors(per_sample)),
        rep("#222222", length(intersect(HLA_SHARED_LABEL, levels_ord)))
      ),
      c(per_sample, intersect(HLA_SHARED_LABEL, levels_ord))
    )
  } else {
    hla_distinct_colors(levels_ord)
  }
  node_color <- unname(level_colors[group_raw])
  node_color[is.na(node_color)] <- "grey70"

  # Variable-residue label (letters at the consensus 'x' positions).
  node_label <- vapply(
    seq_len(n),
    function(i) hla_motif_variable_aa(cdr3[i], consensus[i]),
    character(1)
  )

  deg <- if (snapshot) graph$degree else igraph::degree(graph)
  total_cells <- if (snapshot) {
    graph$total_cells
  } else {
    tryCatch(igraph::graph_attr(graph, "total_cells"), error = function(e) {
      NA_real_
    })
  }
  if (length(total_cells) != 1 || is.na(total_cells)) {
    total_cells <- NA_real_
  }
  # Which column holds the lineage is the data set's business, not this
  # renderer's: it is passed in (hla_celltype_col() finds it by what the labels
  # resolve to). Hardcoding "cell_type" here made the lineage line vanish for
  # any object that names its annotation differently.
  lineage_col <- if (is.character(lineage_col) && length(lineage_col) == 1) {
    lineage_col
  } else {
    NA_character_
  }
  # Distribution of the active colour column across a node's cells (only for
  # metadata colouring, and skipped for the lineage column, which has its own
  # line below).
  skip_dist <- c("cluster", if (!is.na(lineage_col)) lineage_col)
  color_dist <- if (!color_col %in% skip_dist) {
    get_attr(paste0(color_col, "_dist"))
  } else {
    rep(NA_character_, n)
  }
  cell_dist <- if (!is.na(lineage_col)) {
    get_attr(paste0(lineage_col, "_dist"))
  } else {
    rep(NA_character_, n)
  }

  # Send tooltip fields as compact columns. Repeating the full HTML template for
  # every node made hover text the second-largest part of the HLA frame. The
  # browser already knows how to expand dictionary-coded categorical columns.
  hover_categorical <- function(label, values) {
    values <- as.character(values)
    missing <- is.na(values) | !nzchar(values)
    levels <- unique(values[!missing])
    if (!length(levels)) {
      return(NULL)
    }
    codes <- match(values, levels) - 1L
    codes[missing] <- NA_integer_
    list(label = label, levels = levels, values = codes)
  }
  hover_numeric <- function(label, values, format = NULL, digits = NULL) {
    values <- suppressWarnings(as.numeric(values))
    if (!length(values) || all(is.na(values))) {
      return(NULL)
    }
    out <- list(label = label, values = values)
    if (!is.null(format)) {
      out$format <- format
    }
    if (!is.null(digits)) {
      out$digits <- digits
    }
    out
  }
  carrier_detail <- if (use_carrier && !is.null(carrier_counts)) {
    sprintf(
      "%d carrier / %d non-carrier / %d untyped %s",
      carrier_counts$n_carrier,
      carrier_counts$n_noncarrier,
      carrier_counts$n_untyped,
      if (identical(unit_noun, "cell")) "sample(s)" else "donor(s)"
    )
  } else {
    rep(NA_character_, n)
  }
  hover_columns <- Filter(
    Negate(is.null),
    list(
      hover_categorical("Motif", topo_cluster),
      hover_categorical("Consensus", consensus),
      hover_numeric("Max mismatch", diameter, "integer"),
      hover_categorical("Variable residue", node_label),
      hover_numeric(
        if (identical(unit_noun, "cell")) "Clone size" else "Analysis units",
        clone_count,
        "integer"
      ),
      if (!is.na(total_cells) && total_cells > 0) {
        hover_numeric(
          "Fraction (%)",
          100 * clone_count / total_cells,
          "fixed",
          1
        )
      },
      hover_numeric("Neighbours", deg, "integer"),
      if (use_carrier) {
        hover_categorical(carrier_allele %||% "HLA carrier", group_raw)
      },
      if (use_carrier) hover_categorical("Carrier counts", carrier_detail),
      if (!is.na(lineage_col)) hover_categorical(lineage_col, cell_dist),
      if (!color_col %in% skip_dist) hover_categorical(color_col, color_dist),
      if (!is.null(chain)) hover_categorical("Chain", rep(chain, n)),
      hover_categorical("V gene", v_gene),
      hover_categorical("J gene", j_gene)
    )
  )

  ## NO `group` column, deliberately. vis-network auto-registers any group it
  ## has not been told about and paints it from its own default palette
  ## (#97C2FC, #FFFF00, #FB7E81, #7BE141, ...), which overrides the per-node
  ## `color` set above: measured at 246 of 430 nodes rendering in vis defaults
  ## instead of their carrier colour, e.g. a "Mixed" node drawn bright yellow.
  ## Nothing here needs the column — the legend is built by hand with
  ## `useGroups = FALSE` and there is no `selectedBy` — so the colour travels on
  ## `color` alone. Re-adding `group` requires a matching visGroups() call.
  nodes <- data.frame(
    id = seq_len(n),
    node_key = as.character(get_attr("name")),
    label = node_label,
    # `size`, not `value`: vis scales `value` linearly onto the radius, which
    # squares the difference the eye reads. See hla_node_radius().
    size = hla_node_radius(clone_count, node_scale),
    color = node_color,
    font.size = 16,
    font.color = "#2a3f5f",
    font.vadjust = -20,
    stringsAsFactors = FALSE
  )

  el <- if (snapshot) graph$edges else igraph::as_edgelist(graph, names = FALSE)
  edges <- if (nrow(el) == 0) {
    data.frame(from = integer(0), to = integer(0), stringsAsFactors = FALSE)
  } else {
    data.frame(from = el[, 1], to = el[, 2], stringsAsFactors = FALSE)
  }

  n_levels <- length(levels_ord)
  # Motif ids are arbitrary — no order, no meaning — so past a dozen the legend
  # is a wall of swatches that map to nothing a reader can use. "auto" suppresses
  # it for THAT colouring only; every other scale is a real one and keeps its
  # key. The user can override either way: "auto" is a default, not a verdict,
  # and someone chasing one motif id out of 18 has a use for the wall.
  hide_legend <- switch(
    legend_mode,
    always = FALSE,
    never = TRUE,
    color_col == "cluster" && n_levels > HLA_MOTIF_MAX_LEGEND_CLUSTERS
  )
  legend <- if (hide_legend) {
    NULL
  } else {
    data.frame(
      label = if (color_col == "cluster") {
        paste("Motif", levels_ord)
      } else {
        levels_ord
      },
      color = unname(level_colors[levels_ord]),
      shape = "dot",
      stringsAsFactors = FALSE
    )
  }

  # Draw coordinates, computed once by igraph when the graph was built (see
  # hla_motif_layout) and carried on the vertices ever since. NULL only for a
  # graph built before this existed, in which case the renderer falls back to
  # letting the browser settle it.
  layout <- if (all(c("layout_x", "layout_y") %in% names(va))) {
    cbind(as.numeric(va[["layout_x"]]), as.numeric(va[["layout_y"]]))
  } else {
    NULL
  }

  list(
    nodes = nodes,
    edges = edges,
    layout = layout,
    hover_columns = hover_columns,
    legend = legend,
    legend_title = if (color_col == "cluster") {
      "Motif cluster"
    } else if (use_carrier) {
      # Name the allele being shown, not the internal column.
      carrier_allele %||% "HLA carrier status"
    } else if (use_pair) {
      # Likewise: the entries already name the alleles, so the title says what
      # the colours ARE. "pair_allele" is this code's word, not the reader's.
      "Candidate allele by lineage"
    } else if (use_context) {
      "MHC context"
    } else {
      color_col
    }
  )
}

## ---- The interactive motif network ------------------------------------ ##
## ---- The drawable network data ---------------------------------------- ##
## Extracted from the renderer so the LEGEND can be drawn outside the canvas
## (see output$hla_legend_ui): both must come from one build, or the key and the
## picture could disagree about what a colour means.
hla_visnet <- reactive({
  if (!hla_has_deps()) {
    return(NULL)
  }
  g <- hla_first_frame_graph_snapshot()
  if (is.null(g)) {
    g <- hla_motif_graph()
  }
  snapshot <- is.list(g) &&
    identical(g$version, 1L) &&
    is.list(g$vertex_attrs)
  # A tripped size guard returns NA carrying a message; surface it as a note
  # (handled by output$hla_motif_note) and draw nothing.
  if (!snapshot && !hla_motif_graph_ok(g)) {
    return(NULL)
  }
  n_nodes <- if (snapshot) length(g$vertex_attrs$name) else igraph::vcount(g)
  if (n_nodes > HLA_MOTIF_MAX_RENDER) {
    return(NULL)
  }
  color_by <- hla_param("hla_color_by", "cluster")
  # Carrier status is derived HERE, from the cached graph's allele-independent
  # `samples_all` attribute, so switching allele re-colours without recomputing
  # a single Hamming distance.
  #
  # The allele is read ONLY when it is actually drawn. Reading it unconditionally
  # would take a reactive dependency on it in every colouring, so picking an
  # allele on the Associations tab would redraw a network that is coloured by
  # motif cluster and does not mention an allele anywhere.
  use_carrier <- identical(color_by, "hla_carrier")
  allele <- if (use_carrier) hla_color_allele() else NULL
  carrier <- NULL
  carrier_cnt <- NULL
  if (use_carrier && !is.null(allele)) {
    sa <- if (snapshot) {
      g$vertex_attrs$samples_all
    } else {
      igraph::vertex_attr(g, "samples_all")
    }
    typing <- hla_active_typing()
    smp <- hla_ir_samples()
    carrier <- hla_node_carrier_status(sa, typing, smp, allele)
    carrier_cnt <- hla_node_carrier_counts(sa, typing, smp, allele)
  }
  hla_build_motif_visnet(
    g,
    color_by = color_by,
    chain = hla_active_chain(),
    carrier_status = carrier,
    carrier_counts = carrier_cnt,
    carrier_allele = allele,
    unit_noun = hla_unit_noun(),
    legend_mode = hla_param("hla_legend_mode", "auto"),
    lineage_col = hla_celltype_col(),
    # Display only. Read here so a size change flows through the SAME colour
    # path: the graph is untouched, so Canvas receives new radii without
    # rebuilding or re-fitting the layout.
    node_scale = hla_param("hla_node_scale", 1)
  )
})

## Live read-out for the min-size slider: how many clusters / nodes the current
## threshold keeps. Reads the same finalized graph the network draws, so it
## always agrees with what is on screen and updates when the slider settles.
output$hla_motif_readout <- renderUI({
  hla_require_active_page()
  req(hla_ready_latch())
  g <- hla_motif_graph_cached()
  if (!hla_motif_graph_ok(g)) {
    return(NULL)
  }
  n_nodes <- igraph::vcount(g)
  cl <- tryCatch(igraph::vertex_attr(g, "cluster"), error = function(e) NULL)
  n_clusters <- if (is.null(cl)) 0L else length(unique(cl))
  tags$div(
    class = "text-muted",
    style = "font-size: 11px; margin: -2px 0 8px;",
    sprintf(
      "%d cluster%s · %d node%s shown",
      n_clusters,
      if (n_clusters == 1) "" else "s",
      n_nodes,
      if (n_nodes == 1) "" else "s"
    )
  )
})

hla_motif_last_render_signature <- reactiveVal(NULL)
hla_motif_last_feedback_token <- NULL

observe(
  {
    req(identical(input[["sidebar"]], "hla_tcr_motifs"))
    req(identical(input[["hla_tabs"]], "Motif Network"))
    render_request <- input[["hla_motif_network_render_request"]]
    dataset_context <- viewer_loaded_dataset_context()
    req(
      is.list(render_request),
      viewerDatasetContextEqual(render_request$dataset_context, dataset_context)
    )
    req(render_request)
    req(hla_active_chain() %in% hla_tcr_chains())
    req(isTRUE(hla_first_frame_graph_matches()) || hla_ready_latch())
    vn <- hla_visnet()
    if (is.null(vn) || is.null(vn$layout)) {
      return()
    }

    from <- as.integer(vn$edges$from)
    to <- as.integer(vn$edges$to)
    point_sizes <- 2 * vn$nodes$size
    render_signature <- list(
      dataset_context = dataset_context,
      dataset = available_crb_files$selected,
      legend_title = vn$legend_title,
      layout = vn$layout,
      node_key = vn$nodes$node_key,
      color = vn$nodes$color,
      point_sizes = point_sizes,
      hover_columns = vn$hover_columns,
      from = from,
      to = to
    )
    feedback <- cerebroFeedbackControls(
      "hla_motif_network",
      input,
      list(
        hla_min_nodes = if (isTRUE(hla_first_frame_graph_matches())) {
          hla_param("hla_min_nodes", hla_default_min_nodes())
        } else {
          hla_min_nodes_debounced()
        }
      )
    )
    # Input widgets and the client request lifecycle initialise independently.
    # Several invalidations can therefore arrive with the same effective graph.
    # Compare the actual frame, not its input controls: controls can move from a
    # server default to the same explicit browser value without changing one pixel.
    if (identical(hla_motif_last_render_signature(), render_signature)) {
      cerebroSendViewFeedback(
        "hla_motif_network",
        hla_motif_last_feedback_token,
        dataset_context,
        feedback
      )
      return()
    }

    hla_motif_last_feedback_token <<- cerebroCellViewRender(
      "hla_motif_network",
      meta = list(
        color_type = "coexpression",
        color_variable = vn$legend_title,
        appearance = list(
          group_labels = FALSE,
          draw_border = TRUE,
          keep_square = FALSE
        ),
        space_label = "Motif network"
      ),
      data = list(
        x = vn$layout[, 1],
        y = vn$layout[, 2],
        selection_key = vn$nodes$node_key,
        color = vn$nodes$color,
        point_sizes = point_sizes,
        point_size = min(20, stats::median(point_sizes)),
        point_opacity = 1,
        reset_axes = FALSE
      ),
      hover = list(
        hoverinfo = "text",
        columns = vn$hover_columns
      ),
      extra = list(
        interaction = list(
          # Dense motif networks are primarily overview figures. Above this size,
          # per-node hit testing and drag interaction are opt-in so the rest of the
          # page stays responsive; the user can re-enable them from the notice.
          static_above = 200L
        ),
        edges = list(
          # The browser already owns the node coordinates. Integer endpoints avoid
          # repeating four double vectors for every edge (the dominant HLA payload).
          from = from,
          to = to
        )
      ),
      dataset_context = dataset_context,
      feedback = feedback
    )
    hla_motif_last_render_signature(render_signature)
  },
  priority = 100
)

## ---- Export: tables + manifest ---------------------------------------- ##
## A screenshot of the network is not a result. This writes what the page shows
## as recomputable tables, plus a manifest carrying the parameters and every
## caveat that applies — so numbers cannot leave the app stripped of the fact
## that, say, the receptors were selected on the association being displayed.
output$hla_export_analysis <- downloadHandler(
  filename = function() {
    sprintf(
      "hla_tcr_motifs_%s_%s.zip",
      gsub("[^A-Za-z0-9]+", "_", hla_active_chain()),
      format(Sys.Date())
    )
  },
  content = function(file) {
    g <- hla_motif_graph()
    tabs <- hla_graph_tables(g)
    motifs <- hla_motif_summary(g)
    typing <- hla_active_typing()
    ir_samples <- hla_ir_samples()
    unit_map <- tryCatch(
      hla_analysis_unit_map(typing, ir_samples),
      error = function(e) NULL
    )
    qc <- tryCatch(attr(typing, "qc"), error = function(e) NULL)

    manifest <- hla_build_manifest(
      dataset = tryCatch(
        data_set()$experiment$experiment_name,
        error = function(e) NA_character_
      ),
      chain = hla_active_chain(),
      input_channel = if (
        !is.null(hla_session_typing()) && nrow(hla_session_typing()) > 0
      ) {
        "session upload"
      } else if (hla_has_typing()) {
        "stored .crb"
      } else {
        "none"
      },
      hla_source_type = if (hla_has_typing()) {
        paste(unique(typing$source_type), collapse = ", ")
      } else {
        NA_character_
      },
      unit_type = if (is.null(unit_map)) {
        NA_character_
      } else {
        paste(unique(unit_map$unit_type), collapse = ", ")
      },
      observation_unit = hla_unit_noun(),
      n_units = length(ir_samples),
      n_nodes = nrow(tabs$nodes),
      n_edges = nrow(tabs$edges),
      n_motifs = nrow(motifs),
      min_nodes = hla_param("hla_min_nodes", hla_default_min_nodes()),
      split_by_v = isTRUE(hla_param("hla_by_v", hla_by_v_default())),
      show_isolated = isTRUE(hla_param("hla_show_isolated", FALSE)),
      allele = hla_color_allele() %||% NA_character_,
      scope = tryCatch(hla_scope_mode(), error = function(e) NA_character_),
      allele_i = tryCatch(
        hla_pair_allele_i() %||% NA_character_,
        error = function(e) NA_character_
      ),
      allele_ii = tryCatch(
        hla_pair_allele_ii() %||% NA_character_,
        error = function(e) NA_character_
      ),
      lineage_column = tryCatch(
        hla_celltype_col() %||% NA_character_,
        error = function(e) NA_character_
      ),
      tcr_selection = tryCatch(
        data_set()$technical_info$tcr_selection %||% NA_character_,
        error = function(e) NA_character_
      ),
      qc_warnings = if (is.data.frame(qc) && nrow(qc) > 0) {
        qc$issue
      } else {
        character(0)
      },
      # The app version comes from the runtime config, not from the installed
      # CerebroNexus package: an exported app must not reference the package at
      # runtime (see the self-containment check in test-smoke-production.R).
      app_version = as.character(
        Cerebro.options[["cerebro_version"]] %||% "unknown"
      )
    )

    # A unique staging directory per download. Concurrent user sessions share
    # one R process, so a fixed tempdir()/hla_export path lets one download's
    # cleanup delete or mix another's files mid-write (corrupt archives, and a
    # cross-session leak). tempfile() is unique per call; on.exit removes the
    # directory once the archive has been written.
    tmp <- tempfile("hla_export_")
    dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::write.csv(
      manifest,
      file.path(tmp, "manifest.csv"),
      row.names = FALSE
    )
    utils::write.csv(tabs$nodes, file.path(tmp, "nodes.csv"), row.names = FALSE)
    utils::write.csv(tabs$edges, file.path(tmp, "edges.csv"), row.names = FALSE)
    utils::write.csv(motifs, file.path(tmp, "motifs.csv"), row.names = FALSE)
    if (hla_has_typing()) {
      utils::write.csv(
        typing,
        file.path(tmp, "hla_typing.csv"),
        row.names = FALSE
      )
    }
    ov <- tryCatch(hla_overlap_table(), error = function(e) NULL)
    if (is.data.frame(ov) && nrow(ov) > 0) {
      utils::write.csv(
        ov,
        file.path(tmp, "allele_overlap.csv"),
        row.names = FALSE
      )
    }
    # utils::zip (base R) rather than the zip package: no new dependency. "-j"
    # junks the temp directory path so the archive holds plain file names.
    files <- list.files(tmp, full.names = TRUE)
    utils::zip(zipfile = file, files = files, flags = "-j -q")
  }
)

## ---- Note under the network (guard messages / empty state) ------------- ##
output$hla_motif_note <- renderUI({
  hla_require_active_page()
  if (!hla_has_deps()) {
    return(tags$p(
      class = "text-muted",
      `data-view-state` = "empty",
      "The motif network requires the 'visNetwork' and 'stringdist' packages."
    ))
  }
  seg <- hla_segments()
  if (is.null(seg) || nrow(seg) == 0) {
    return(tags$p(
      class = "text-muted",
      `data-view-state` = "empty",
      sprintf(
        "No %s cells with a complete V / J / CDR3 to build a network.",
        hla_active_chain()
      )
    ))
  }
  g <- hla_motif_graph()
  guard <- attr(g, "guard")
  if (!is.null(guard)) {
    return(tags$p(class = "text-danger", `data-view-state` = "error", guard))
  }
  if (hla_motif_graph_ok(g) && igraph::vcount(g) > HLA_MOTIF_MAX_RENDER) {
    return(tags$p(
      class = "text-danger",
      `data-view-state` = "error",
      sprintf(
        "The filtered graph has %s nodes; interactive rendering is capped at %s. Increase motif size or filter the repertoire.",
        format(igraph::vcount(g), big.mark = ","),
        format(HLA_MOTIF_MAX_RENDER, big.mark = ",")
      )
    ))
  }
  if (!hla_motif_graph_ok(g)) {
    return(tags$p(
      class = "text-muted",
      `data-view-state` = "empty",
      paste(
        "No CDR3 motif clusters at Hamming distance 1 with the current",
        "settings. Try lowering the minimum motif size or enabling",
        "'Show unconnected CDR3s'."
      )
    ))
  }
  # The definitional "what a node / edge / motif is" text lives in the info guide
  # (the info button in the page header), so it no longer clutters the page below the
  # plot. Keep on the page only the two things a reader must not misread from the
  # screen itself: the node-AREA cap (so sizes are not compared past it), and —
  # when it is actually hidden — why the legend is gone.
  tags$p(
    class = "text-muted",
    style = "font-size: 12px; margin-top: 8px;",
    sprintf(
      paste(
        "Node AREA is proportional to the number of %ss carrying that CDR3, up to",
        "%d; above that every node is the same size — read the tooltip for the",
        "exact count."
      ),
      hla_unit_noun(),
      HLA_NODE_MAX_EXACT
    ),
    if (
      identical(hla_param("hla_color_by", "cluster"), "cluster") &&
        identical(hla_param("hla_legend_mode", "auto"), "auto") &&
        hla_motif_n_clusters() > HLA_MOTIF_MAX_LEGEND_CLUSTERS
    ) {
      tagList(
        tags$br(),
        sprintf(
          paste(
            "The legend is hidden: %d motifs, and motif numbers are arbitrary, so",
            "past %d swatches it maps to nothing. Hover a node for its motif, or",
            "force it on under Additional parameters."
          ),
          hla_motif_n_clusters(),
          HLA_MOTIF_MAX_LEGEND_CLUSTERS
        )
      )
    }
  )
})

## ---- Legend, above the plot -------------------------------------------- ##
## Drawn as flowing HTML rather than visLegend: visLegend can only sit left or
## right and reserves 15% of the canvas whether it needs it or not. Long keys
## wrap above the canvas and the shared fill-height sizing gives the plot the
## remaining vertical space.
output$hla_legend_ui <- renderUI({
  vn <- hla_visnet()
  if (is.null(vn) || is.null(vn$legend) || nrow(vn$legend) == 0) {
    return(NULL)
  }
  items <- lapply(seq_len(nrow(vn$legend)), function(i) {
    tags$span(
      style = paste0(
        "display:inline-flex;align-items:center;gap:5px;",
        "max-width:100%;margin:0 12px 0 0;font-size:11px;color:#33333a;",
        "overflow-wrap:anywhere;"
      ),
      tags$span(
        style = paste0(
          "width:11px;height:11px;border-radius:50%;flex:none;",
          "border:1px solid #333;background:",
          vn$legend$color[i],
          ";"
        )
      ),
      vn$legend$label[i]
    )
  })
  tags$div(
    style = paste0(
      "display:flex;flex-wrap:wrap;align-items:center;gap:4px 0;",
      "min-width:0;max-width:100%;margin:0;"
    ),
    tags$span(
      style = paste0(
        "font-size:11px;font-weight:700;color:#1c1c1e;",
        "margin-right:10px;overflow-wrap:anywhere;"
      ),
      vn$legend_title
    ),
    tags$div(
      style = paste0(
        "display:flex;flex:1 1 auto;flex-wrap:wrap;align-items:center;",
        "min-width:0;gap:4px 0;margin:0;"
      ),
      items
    )
  )
})
