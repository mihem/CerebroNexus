# Frozen 860e07c5 graph/aggregation functions for equivalence tests.
hla_aggregate_cdr3_nodes <- function(
  seg,
  meta_cols = character(0),
  context_col = NULL,
  by_v = FALSE,
  context_summary = hla_context_summary
) {
  if (is.null(seg) || nrow(seg) == 0) {
    return(NULL)
  }
  meta_cols <- intersect(meta_cols, colnames(seg))
  # One group id per row, and the row indices behind each group. Splitting the
  # INDICES, not `seg` itself, is deliberate: splitting the wide data.frame
  # copies every column of every group and cost more than the tallies do.
  keys <- if (isTRUE(by_v)) {
    interaction(seg$v_gene, seg$cdr3, drop = TRUE, lex.order = TRUE)
  } else {
    factor(seg$cdr3)
  }
  g <- as.integer(keys)
  k <- nlevels(keys)
  idx <- split(seq_len(nrow(seg)), keys, drop = FALSE)
  # One representative row per group, for the columns that are constant within
  # it (`cdr3`, and `v_gene` when it is part of the key).
  first_i <- vapply(idx, `[`, integer(1), 1L)
  # `mode` and `_dist` are two readings of ONE tally, so it is computed once.
  summarise <- function(col) {
    t <- hla_group_tally(g, seg[[col]])
    list(mode = hla_group_mode(t, k), dist = hla_group_dist(t, k))
  }

  v_sum <- summarise("v_gene")
  j_sum <- summarise("j_gene")
  # ONE data.frame for every node, not one per node: the constructor is not
  # cheap enough to call thousands of times (it was 47% of aggregation).
  agg <- data.frame(
    node_id = if (isTRUE(by_v)) {
      paste(seg$v_gene[first_i], seg$cdr3[first_i], sep = "::")
    } else {
      seg$cdr3[first_i]
    },
    cdr3 = seg$cdr3[first_i],
    v_gene = v_sum$mode,
    j_gene = j_sum$mode,
    v_gene_dist = v_sum$dist,
    j_gene_dist = j_sum$dist,
    clone_count = lengths(idx),
    # Machine-readable set of the samples this node was seen in, so a
    # renderer can derive per-node HLA carrier status for ANY allele without
    # rebuilding the graph. Kept allele-independent on purpose: the graph is
    # cached on its build parameters, and colouring must never invalidate it.
    samples_all = if ("sample" %in% colnames(seg)) {
      # The tally's values are already sorted and unique within each group.
      t <- hla_group_tally(g, seg$sample)
      out <- rep(NA_character_, k)
      if (length(t$g) > 0) {
        joined <- vapply(split(t$v, t$g), paste, character(1), collapse = ",")
        out[as.integer(names(joined))] <- unname(joined)
      }
      out
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
  for (mc in meta_cols) {
    m_sum <- summarise(mc)
    agg[[mc]] <- m_sum$mode
    agg[[paste0(mc, "_dist")]] <- m_sum$dist
  }
  if (!is.null(context_col) && context_col %in% colnames(seg)) {
    # Not a mode, and not tallyable: `context_summary` is pluggable precisely
    # because a node spanning both values is the finding rather than a tie to
    # break, so it keeps its per-group call. It is cheap (any() / unique()).
    ctx <- as.character(seg[[context_col]])
    agg[[context_col]] <- vapply(
      idx,
      function(i) context_summary(ctx[i]),
      character(1)
    )
    agg[[paste0(context_col, "_dist")]] <- hla_group_dist(
      hla_group_tally(g, ctx),
      k
    )
  }
  rownames(agg) <- NULL
  # Derived from samples_all, so it is allele-independent and cannot disagree
  # with the sample set the tooltip reports.
  agg$sample_origin <- hla_node_sample_origin(agg$samples_all)
  agg
}

hla_build_motif_graph_raw <- function(
  seg,
  by_v = FALSE,
  meta_cols = character(0),
  context_col = NULL,
  context_summary = hla_context_summary
) {
  # A guard trip returns NA (not NULL) carrying a message attribute, because an
  # attribute cannot be set on NULL. Callers treat is.null() OR a "guard" attr
  # as "no graph"; hla_motif_graph_ok() below is the single predicate for that.
  guard <- function(msg) {
    out <- NA
    attr(out, "guard") <- msg
    out
  }
  if (is.null(seg) || nrow(seg) == 0) {
    return(NULL)
  }
  agg <- hla_aggregate_cdr3_nodes(
    seg,
    meta_cols = meta_cols,
    context_col = context_col,
    by_v = by_v,
    context_summary = context_summary
  )
  if (is.null(agg) || nrow(agg) == 0) {
    return(NULL)
  }

  # Total-size guard across all unique CDR3.
  if (nrow(agg) > HLA_MOTIF_MAX_TOTAL) {
    return(guard(sprintf(
      paste(
        "Too many unique CDR3s (%s > %s).",
        "Open Settings > Filters and narrow the active cohort."
      ),
      format(nrow(agg), big.mark = ","),
      format(HLA_MOTIF_MAX_TOTAL, big.mark = ",")
    )))
  }
  # Per-bin guard: the O(k^2) distance matrix is built per length bin (and per
  # V gene when by_v). Both grouping vectors must be length nrow(agg).
  bin_key <- if (by_v) {
    paste(nchar(agg$cdr3), agg$v_gene, sep = "|")
  } else {
    as.character(nchar(agg$cdr3))
  }
  bin_sizes <- table(bin_key)
  if (max(bin_sizes) > HLA_MOTIF_MAX_BIN) {
    return(guard(sprintf(
      paste(
        "A CDR3-length bin has %s unique sequences (> %s).",
        "Open Settings > Filters and narrow the active cohort."
      ),
      format(max(bin_sizes), big.mark = ","),
      format(HLA_MOTIF_MAX_BIN, big.mark = ",")
    )))
  }

  res <- hla_build_motif_groups(agg, by_v = by_v)
  edges <- res$edges
  has_edges <- !is.null(edges) && nrow(edges) > 0

  vertices <- res$motif_df
  vertices$name <- vertices$node_id
  vertices <- vertices[, c("name", setdiff(colnames(vertices), "name"))]
  edge_df <- if (has_edges) {
    edges[, c("from", "to")]
  } else {
    data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
  }
  g <- igraph::graph_from_data_frame(
    edge_df,
    vertices = vertices,
    directed = FALSE
  )
  # Denominator for a node's clone-size fraction shown in tooltips.
  g <- igraph::set_graph_attr(g, "total_cells", nrow(seg))

  # Lay out the connected CORE (degree > 0) ONCE, here. That core is exactly the
  # set the minimum-size threshold filters within: every survivor of any
  # min_nodes >= 2 is one of these nodes. Computing the layout independently of
  # the threshold lets finalize drop small clusters WITHOUT reshuffling the ones
  # that remain. Coordinates travel WITH the graph so a colour change never
  # re-arranges the network.
  core_idx <- which(igraph::degree(g) > 0)
  lx <- rep(NA_real_, igraph::vcount(g))
  ly <- lx
  if (length(core_idx) > 0) {
    core <- igraph::induced_subgraph(g, igraph::V(g)[core_idx])
    xy <- hla_motif_layout(core)
    if (!is.null(xy)) {
      lx[core_idx] <- xy[, 1]
      ly[core_idx] <- xy[, 2]
    }
  }
  # Isolated nodes take no part in a force layout -- they are singletons -- but
  # they get their coordinates HERE as well, on a cheap deterministic grid. That
  # way finalize never has to lay anything out even when show_isolated keeps
  # them: if it did, changing the threshold would re-run the layout and move the
  # connected clusters that did not change.
  placed <- .hla_place_isolated(lx, ly, which(igraph::degree(g) == 0))
  igraph::V(g)$layout_x <- placed$x
  igraph::V(g)$layout_y <- placed$y
  g
}
