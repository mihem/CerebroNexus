# ============================================================================
# Shared CDR3 motif-network core (HLA & TCR Motifs page)
# ============================================================================
# Pure, Shiny-independent functions for turning an immune-repertoire list into
# a CDR3 Hamming-1 motif network. Rebuilt against a fresh contract (see
# tmp/DESIGN-hla-motif-network.md) rather than copied from the archived IR
# implementation; the archive is used only as a regression reference.
#
# Contract highlights enforced here:
#   * Edges join two EQUAL-length CDR3s at Hamming distance exactly 1.
#   * A "motif" is a Hamming-1 connected component; membership can be transitive
#     (A-B=1, B-C=1 keeps A,C together even when A-C>1). The component's MAX
#     MISMATCH (max pairwise Hamming distance) is reported so callers never
#     imply all-pairs distance <= 1. Deliberately NOT called a diameter: on a
#     graph that word means the longest shortest-path in HOPS, which is a
#     different and larger number (measured on the shipped demo: 16 of 20
#     motifs disagree, e.g. max mismatch 6 vs graph diameter 8).
#   * `min_nodes` keeps components with size >= N (NOT > N). Default 2.
#   * Node identity is the unique CDR3 amino-acid string; V/J are kept as
#     distributions, never folded into the node key unless `by_v = TRUE`.
#   * Size guards refuse to build pathologically large distance matrices.
#
# These functions carry an `hla_` prefix and live in the package namespace so
# they are installed, unit-testable, and shared by the Shiny module.
# ============================================================================

#' visNetwork import anchor
#'
#' `visNetwork` is a hard dependency of the HLA & TCR Motifs page, but the
#' renderer lives in `inst/shiny` (runtime), not in `R/`. This roxygen anchor
#' imports a symbol so `R CMD check` sees the Imports entry as used. It defines
#' no runtime behaviour.
#'
#' @importFrom visNetwork renderVisNetwork
#' @keywords internal
#' @name hla_visnetwork_import
NULL

## ---- Chains that define each receptor class --------------------------- ##
HLA_TCR_CHAINS <- c("TRA", "TRB", "TRG", "TRD")
HLA_BCR_CHAINS <- c("IGH", "IGK", "IGL")

## ---- Motif size guards (conservative; widen only with benchmarks) ------ ##
HLA_MOTIF_MAX_BIN <- 2500L # unique CDR3 in one length bin
HLA_MOTIF_MAX_TOTAL <- 20000L # unique CDR3 across all bins
HLA_MOTIF_MAX_RENDER <- 5000L # rendered nodes before physics is disabled

## Label for a CDR3 seen in more than one sample. Cross-sample recurrence is the
## signal an HLA screen rests on (a motif recurring across carriers), so it gets
## its own level rather than being folded into one sample's colour.
HLA_SHARED_LABEL <- "Shared"

#' Detect receptor chains present in an immune-repertoire list
#'
#' Scans the `CTgene` column of all samples for chain tokens.
#'
#' @param data Named list of scRepertoire-style data.frames (one per sample).
#' @return Character vector of detected chains, subset of TRA/TRB/TRG/TRD/
#'   IGH/IGK/IGL, in that canonical order.
#' @keywords internal
hla_detect_chains <- function(data) {
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(character(0))
  }
  all_ct <- unlist(lapply(data, function(df) {
    if ("CTgene" %in% names(df)) as.character(df$CTgene) else character(0)
  }))
  chains <- c(HLA_TCR_CHAINS, HLA_BCR_CHAINS)
  chains[vapply(chains, function(ch) any(grepl(ch, all_ct)), logical(1))]
}

#' Parse V / J / CDR3 for one chain out of scRepertoire CT* columns
#'
#' scRepertoire packs every chain of a cell into single underscore-joined
#' strings (`CTgene`, `CTaa`); this returns one row per cell that HAS the
#' requested chain, with parsed `v_gene` / `j_gene` / `cdr3` plus a combined
#' `clone_vjc = "v;j;cdr3"` clone identity and every metadata column already
#' joined onto the IR data.
#'
#' @param data Named list of IR data.frames (with metadata joined by barcode).
#' @param chain Chain prefix, e.g. "TRB" / "TRA".
#' @return A data.frame with one row per cell carrying `chain`, or NULL.
#' @keywords internal
hla_parse_ir_segments <- function(data, chain) {
  if (is.null(data) || length(data) == 0 || is.null(chain) || !nzchar(chain)) {
    return(NULL)
  }
  # Positional (1-based) slot index of the chain matching `chain` in each cell's
  # underscore-joined CT* string; NA when the chain is absent.
  chain_slot_index <- function(ct_gene_vec) {
    ct_gene_vec <- as.character(ct_gene_vec)
    vapply(
      strsplit(ct_gene_vec, "_", fixed = TRUE),
      function(parts) {
        first <- sub(";.*$", "", parts)
        idx <- which(grepl(chain, first, fixed = TRUE) & first != "NA")
        if (length(idx) == 0) NA_integer_ else idx[1]
      },
      integer(1)
    )
  }
  # Pick each row's slot value from a CT* vector (split once, index per row).
  pick_slot <- function(ct_vec, slot_idx) {
    if (length(ct_vec) == 0) {
      return(character(0))
    }
    split_all <- strsplit(as.character(ct_vec), "_", fixed = TRUE)
    val <- mapply(
      function(parts, i) {
        if (is.na(i) || i > length(parts)) NA_character_ else parts[i]
      },
      split_all,
      slot_idx,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    val[is.na(val) | val == "NA" | !nzchar(val)] <- NA_character_
    val
  }
  first_allele <- function(x) {
    ifelse(is.na(x), NA_character_, sub(";.*$", "", x))
  }

  rows <- lapply(data, function(df) {
    if (is.null(df) || !all(c("barcode", "CTgene", "CTaa") %in% colnames(df))) {
      return(NULL)
    }
    slot_idx <- chain_slot_index(df$CTgene)
    gene_seg <- first_allele(pick_slot(df$CTgene, slot_idx))
    cdr3 <- first_allele(pick_slot(df$CTaa, slot_idx))
    pull_token <- function(prefix) {
      vapply(
        strsplit(gene_seg, ".", fixed = TRUE),
        function(toks) {
          hit <- toks[grepl(paste0("^", chain, prefix), toks)]
          if (length(hit) == 0) NA_character_ else hit[1]
        },
        character(1)
      )
    }
    v_gene <- pull_token("V")
    j_gene <- pull_token("J")
    # V + CDR3 are required (they define the node key and the optional V split);
    # J is OPTIONAL. Requiring it is a paired-single-cell assumption: bulk
    # repertoire sources routinely report only a V family and the CDR3, and
    # dropping those rows would discard the whole sample. A missing J stays NA
    # and simply shows as NA in the J distribution / tooltip.
    keep <- !is.na(v_gene) & !is.na(cdr3) & nzchar(cdr3)
    if (!any(keep)) {
      return(NULL)
    }
    out <- df[keep, , drop = FALSE]
    out$v_gene <- v_gene[keep]
    out$j_gene <- j_gene[keep]
    out$cdr3 <- cdr3[keep]
    out$clone_vjc <- paste(out$v_gene, out$j_gene, out$cdr3, sep = ";")
    out
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    return(NULL)
  }
  # Align columns by the union of all sample columns, NA-filling gaps, so
  # per-cohort metadata columns are never silently dropped.
  all_cols <- unique(unlist(lapply(rows, colnames)))
  rows <- lapply(rows, function(df) {
    miss <- setdiff(all_cols, colnames(df))
    for (col in miss) {
      df[[col]] <- NA
    }
    df[, all_cols, drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Consensus string of equal-length sequences (differing positions -> "x")
#'
#' @param seqs Character vector of equal-length sequences.
#' @return One consensus string; positions that differ across members are "x".
#' @keywords internal
hla_make_consensus <- function(seqs) {
  seqs <- seqs[!is.na(seqs)]
  if (length(seqs) == 0) {
    return(NA_character_)
  }
  if (length(seqs) == 1) {
    return(seqs[1])
  }
  # All members are the same length within a bin; guard anyway.
  if (length(unique(nchar(seqs))) != 1) {
    return(NA_character_)
  }
  m <- do.call(rbind, strsplit(seqs, ""))
  paste0(
    apply(m, 2, function(col) if (length(unique(col)) == 1) col[1] else "x"),
    collapse = ""
  )
}

#' Residue(s) a sequence carries at the consensus "x" (variable) positions
#'
#' Purely a display helper for node labels; never feeds clustering.
#'
#' @param seq A CDR3 sequence.
#' @param cons Its cluster consensus string.
#' @return The residues at the variable positions, or "".
#' @keywords internal
hla_motif_variable_aa <- function(seq, cons) {
  if (is.na(seq) || is.na(cons) || nchar(seq) != nchar(cons)) {
    return("")
  }
  cs <- strsplit(cons, "")[[1]]
  ss <- strsplit(seq, "")[[1]]
  vp <- which(cs == "x")
  if (length(vp) == 0) "" else paste(ss[vp], collapse = "")
}

#' Cluster equal-length CDR3s within one length bin at Hamming distance 1
#'
#' All rows of `df` must share one CDR3 length (caller guarantees this).
#'
#' @param df data.frame with a `cdr3` column (all equal length).
#' @return list(df = df + motif columns, edges = Hamming==1 pairs or NULL).
#' @keywords internal
hla_process_length_group <- function(df) {
  seqs <- df$cdr3
  node_ids <- if ("node_id" %in% colnames(df)) df$node_id else seqs
  n <- length(seqs)

  # Single distance measure (Hamming); edge and adjacency use the SAME
  # threshold (== 1) so component membership and drawn edges never diverge.
  dist_mat <- stringdist::stringdistmatrix(seqs, seqs, method = "hamming")
  adj <- dist_mat == 1
  diag(adj) <- FALSE
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  comps <- igraph::components(g)

  # Max pairwise Hamming distance within the component: how far apart its two
  # most different members are. Reported so a transitive component (A-B=1,
  # B-C=1, A-C=2) is never presented as if every pair were <= 1.
  #
  # This is the diameter of the member SET under the Hamming metric, but it is
  # NOT the graph's diameter (longest shortest-path, counted in hops), which is
  # larger: a hop changes one position and later hops can change it back, so
  # Hamming(u,v) <= hops(u,v). Naming it `diameter` on a network invited exactly
  # the wrong reading, so it is `max_mismatch` everywhere the user can see it.
  diam <- vapply(
    seq_len(comps$no),
    function(k) {
      members <- which(comps$membership == k)
      if (length(members) < 2) {
        return(0L)
      }
      as.integer(max(dist_mat[members, members]))
    },
    integer(1)
  )
  consensus <- vapply(
    seq_len(comps$no),
    function(k) hla_make_consensus(seqs[which(comps$membership == k)]),
    character(1)
  )

  len_label <- nchar(seqs[1])
  df$motif_group <- paste0("M", len_label, "_", comps$membership)
  df$motif_size <- comps$csize[comps$membership]
  df$motif_max_mismatch <- diam[comps$membership]
  df$motif_consensus <- consensus[comps$membership]

  edges <- NULL
  if (n > 1) {
    idx <- which(dist_mat == 1 & upper.tri(dist_mat), arr.ind = TRUE)
    if (nrow(idx) > 0) {
      edges <- data.frame(
        from = node_ids[idx[, 1]],
        to = node_ids[idx[, 2]],
        stringsAsFactors = FALSE
      )
    }
  }
  list(df = df, edges = edges)
}

#' Build motif groups over all length bins (optionally within V gene)
#'
#' Splits unique CDR3s into equal-length bins (Hamming only compares equal
#' lengths), clusters each, and stitches results back together.
#'
#' @param df data.frame with `cdr3` (+ `v_gene` when `by_v`); one row per
#'   unique node key already aggregated by the caller.
#' @param by_v When TRUE, split by (V gene, length) and prefix motif ids by V.
#' @return list(motif_df = per-CDR3 assignment, edges = Hamming==1 pairs or NULL)
#' @keywords internal
hla_build_motif_groups <- function(df, by_v = FALSE) {
  df$cdr3_length <- nchar(df$cdr3)
  split_key <- if (by_v) {
    interaction(df$v_gene, df$cdr3_length, drop = TRUE)
  } else {
    factor(df$cdr3_length)
  }
  results <- lapply(split(df, split_key, drop = TRUE), hla_process_length_group)

  motif_df <- do.call(rbind, lapply(results, `[[`, "df"))
  edge_list <- lapply(results, `[[`, "edges")
  edge_list <- edge_list[!vapply(edge_list, is.null, logical(1))]
  edges <- if (length(edge_list) == 0) NULL else do.call(rbind, edge_list)

  if (by_v) {
    # Same CDR3 in two V bins must not be one node; disambiguate the motif id
    # and (for edges) never join across V. Edges already come from within-bin
    # clustering, so they are V-safe by construction.
    motif_df$motif_group <- paste0(motif_df$v_gene, "::", motif_df$motif_group)
  }
  rownames(motif_df) <- NULL
  list(motif_df = motif_df, edges = edges)
}

#' Tally every group's values in one pass
#'
#' The per-node summaries (`mode` + `_dist`) both need one thing: how often each
#' value occurs within each node. Computing that with `table()` per node per
#' column is what made aggregation ~95% of the graph build — `table()` factors
#' its input and re-pays `match.arg`/`deparse`/`sys.call` on every one of tens of
#' thousands of calls, and `mode` and `_dist` each built their own copy of the
#' identical tally.
#'
#' This does it once per column, for every group at once: one radix `order()`
#' drops into C, and the run boundaries of the sorted `(group, value)` pairs ARE
#' the tally. Sparse by construction — a dense group x level matrix would be
#' 630 columns wide on a cohort like Emerson, nearly all of it zero.
#'
#' Runs come out ALPHABETICAL within each group, which is what makes the
#' tie-breaks below match `sort(table(x))`: see [hla_group_mode()].
#'
#' @param g Integer group id per row (1..K).
#' @param v Values, one per row. NA / "" are dropped (a cell with no label is
#'   absent from the summary, never a level called "NA").
#' @return list(g, v, n): one entry per (group, distinct value) pair.
#' @keywords internal
hla_group_tally <- function(g, v) {
  v <- as.character(v)
  keep <- !is.na(v) & nzchar(v)
  g <- g[keep]
  v <- v[keep]
  if (length(g) == 0) {
    return(list(g = integer(0), v = character(0), n = integer(0)))
  }
  o <- order(g, v, method = "radix")
  gg <- g[o]
  vv <- v[o]
  m <- length(gg)
  # A run starts wherever the (group, value) pair changes.
  new_run <- c(TRUE, gg[-1L] != gg[-m] | vv[-1L] != vv[-m])
  list(g = gg[new_run], v = vv[new_run], n = tabulate(cumsum(new_run)))
}

#' Order a tally by descending count within each group
#'
#' Stable (radix), and [hla_group_tally()] hands over runs already in
#' alphabetical order, so equal counts stay alphabetical. That is precisely what
#' `sort(table(x), decreasing = TRUE)` did: `table()` names its counts by factor
#' level (alphabetical) and R's sort keeps that order among ties.
#'
#' Getting this wrong is silent. Tallying in first-appearance order instead
#' flips roughly a fifth of tied modes, moving node colours and tooltips with no
#' error raised anywhere, so the order is a contract and is tested as one.
#'
#' @param t A [hla_group_tally()] result.
#' @return The same list, reordered.
#' @keywords internal
hla_tally_order <- function(t) {
  o <- order(t$g, -t$n, method = "radix")
  list(g = t$g[o], v = t$v[o], n = t$n[o])
}

#' Modal value per group (ties -> alphabetically first)
#'
#' @param t A [hla_group_tally()] result.
#' @param k Number of groups.
#' @return Character vector of length `k`; NA where the group had no value.
#' @keywords internal
hla_group_mode <- function(t, k) {
  out <- rep(NA_character_, k)
  if (length(t$g) == 0) {
    return(out)
  }
  t <- hla_tally_order(t)
  first <- !duplicated(t$g)
  out[t$g[first]] <- t$v[first]
  out
}

#' Compact "N types: A (5), B (2)" distribution string per group
#'
#' @param t A [hla_group_tally()] result.
#' @param k Number of groups.
#' @return Character vector of length `k`; NA where the group had no value.
#' @keywords internal
hla_group_dist <- function(t, k) {
  out <- rep(NA_character_, k)
  if (length(t$g) == 0) {
    return(out)
  }
  t <- hla_tally_order(t)
  parts <- paste0(t$v, " (", t$n, ")")
  body <- vapply(split(parts, t$g), paste, character(1), collapse = ", ")
  gid <- as.integer(names(body))
  n_types <- tabulate(t$g, nbins = k)[gid]
  out[gid] <- sprintf(
    "%d type%s: %s",
    n_types,
    ifelse(n_types == 1L, "", "s"),
    unname(body)
  )
  out
}

#' Aggregate parsed segments into unique-CDR3 nodes carrying distributions
#'
#' Node key = unique CDR3 amino-acid string, or `(V gene, CDR3)` when `by_v` is
#' TRUE. `clone_count` = number of cells carrying that node key. Categorical
#' metadata columns are summarised as their
#' most-common value (`_mode`) plus a compact "N types: A (5), B (2)"
#' distribution string (`_dist`) so the tooltip can show provenance without
#' collapsing it to a single label.
#'
#' @param seg Output of [hla_parse_ir_segments()].
#' @param meta_cols Character vector of metadata columns to summarise per node.
#' @param context_col Optional name of a per-cell context column. When given,
#'   the node gets a `context_summary` value instead of a plain mode, plus the
#'   usual `_dist` string.
#' @param context_summary How to collapse that column's per-cell values to one
#'   node value. Defaults to [hla_context_summary()] (Class I / Class II /
#'   "Mixed" / "Unknown"). The Class I x Class II pair scope passes
#'   [hla_pair_class_summary()] instead. It is a parameter and not a hardcoded
#'   call because these columns share one property that the plain mode destroys:
#'   a node spanning BOTH values is the finding, not a tie to be broken.
#' @param by_v When TRUE, aggregate with `(v_gene, cdr3)` as the node key.
#' @return A per-node data.frame, or NULL when `seg` is empty.
#' @keywords internal
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

#' Sample of origin per node, collapsing multi-sample nodes to "Shared"
#'
#' A node's `sample` metadata column is summarised as its MODE, which paints a
#' CDR3 seen in three samples with its dominant sample's colour and hides the
#' recurrence entirely. This reports the sample only when the node was seen in
#' exactly one; anything seen in more becomes [HLA_SHARED_LABEL].
#'
#' Cross-sample sharing is not by itself evidence of an HLA association: public
#' CDR3s recur across unrelated donors. It is the observation an association
#' screen starts from, not its conclusion.
#'
#' @param samples_all Character vector of comma-separated sorted sample lists
#'   (the `samples_all` node attribute).
#' @return Character vector: one sample name, "Shared", or NA when untracked.
#' @keywords internal
hla_node_sample_origin <- function(samples_all) {
  if (length(samples_all) == 0) {
    return(character(0))
  }
  vapply(
    strsplit(as.character(samples_all), ",", fixed = TRUE),
    function(s) {
      s <- s[!is.na(s) & nzchar(s)]
      if (length(s) == 0) {
        NA_character_
      } else if (length(s) == 1) {
        s
      } else {
        HLA_SHARED_LABEL
      }
    },
    character(1)
  )
}

## Seed for the motif layout. Fixed so the same graph always draws the same
## picture: a layout that reshuffled between sessions would make two screenshots
## of one analysis impossible to compare, and the page's whole export/manifest
## story rests on a view being reproducible.
HLA_LAYOUT_SEED <- 42L

#' Coordinates for drawing a motif graph, computed in igraph
#'
#' The browser used to do this: `visPhysics(stabilization = 150)` ran a
#' force simulation in JS on every open, which blocked the main thread for ~1.8s
#' on a 430-node graph and drew NOTHING until it finished — so the spinner (which
#' only tracks Shiny's recalculation) had long since vanished, leaving a blank
#' canvas. igraph does the same job in C in ~75ms.
#'
#' `layout_components` rather than a plain force layout, because a motif network
#' is BY CONSTRUCTION a set of disconnected components (that is what a motif is).
#' A force layout has to push those apart with repulsion alone, which is both the
#' slow part and a bad picture — it is what the min-motif-size default exists to
#' avoid ("the layout collapses to a ring"). Laying each component out on its own
#' and packing the results is the shape of the actual data.
#'
#' @param graph A [hla_build_motif_graph()] igraph.
#' @param seed RNG seed; the layout is randomized and must not be.
#' @return A two-column matrix of coordinates, one row per vertex, or NULL.
#' @keywords internal
hla_motif_layout <- function(graph, seed = HLA_LAYOUT_SEED) {
  if (!hla_motif_graph_ok(graph)) {
    return(NULL)
  }
  # Seed locally and put the caller's RNG stream back. set.seed() in a Shiny
  # session is a global side effect: silently re-seeding from here would make
  # every later random draw in that session — in any other tab — follow from
  # this seed. (visNetwork::visIgraphLayout(randomSeed=) does exactly that,
  # which is why the layout is computed here and passed in instead.)
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) {
    get(".Random.seed", envir = globalenv(), inherits = FALSE)
  } else {
    NULL
  }
  on.exit(
    {
      if (had_seed) {
        assign(".Random.seed", old_seed, envir = globalenv())
      } else if (
        exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      ) {
        rm(".Random.seed", envir = globalenv())
      }
    },
    add = TRUE
  )
  set.seed(seed)
  igraph::layout_components(graph, layout = igraph::layout_with_fr)
}

#' Is a motif-graph result a usable igraph?
#'
#' [hla_build_motif_graph()] returns NULL (nothing to draw), an NA carrying a
#' "guard" attribute (a size guard tripped), or an igraph. This is the single
#' predicate for "we have a drawable graph".
#'
#' @param g A return value of [hla_build_motif_graph()].
#' @return TRUE only when `g` is an igraph with at least one vertex.
#' @keywords internal
hla_motif_graph_ok <- function(g) {
  inherits(g, "igraph") && igraph::vcount(g) > 0
}

#' Build the CDR3 Hamming-1 motif igraph from parsed segments
#'
#' Parses nothing itself: takes already-parsed segments, aggregates to unique
#' CDR3 nodes, clusters by Hamming distance 1 (optionally within V gene), and
#' returns an igraph whose vertices carry `cdr3` / `motif_*` attributes + per-
#' node metadata distributions + `clone_count`.
#'
#' @param seg Output of [hla_parse_ir_segments()].
#' @param by_v Split clustering within V gene.
#' @param min_nodes Keep connected components of size >= `min_nodes`. Default 2.
#' @param show_isolated When TRUE, also keep isolated (degree-0) CDR3s as points.
#' @param meta_cols Metadata columns to carry as node distributions.
#' @param context_col Optional per-cell context column; the node gets a
#'   `context_summary` value rather than a mode.
#' @param context_summary Collapse function for `context_col`; see
#'   [hla_aggregate_cdr3_nodes()].
#' @return An igraph object (with a per-node `cluster` attribute and a
#'   `total_cells` graph attribute) or NULL. Attaches attr "guard" with a
#'   message when a size guard tripped (graph is NULL in that case).
#' @keywords internal
hla_build_motif_graph <- function(
  seg,
  by_v = FALSE,
  min_nodes = 2L,
  show_isolated = FALSE,
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
      "Too many unique CDR3s (%s > %s). Filter by sample or group first.",
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
      "A CDR3-length bin has %s unique sequences (> %s). Filter first.",
      format(max(bin_sizes), big.mark = ","),
      format(HLA_MOTIF_MAX_BIN, big.mark = ",")
    )))
  }

  res <- hla_build_motif_groups(agg, by_v = by_v)
  edges <- res$edges
  has_edges <- !is.null(edges) && nrow(edges) > 0
  if (!has_edges && !isTRUE(show_isolated)) {
    return(NULL)
  }

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

  if (!isTRUE(show_isolated)) {
    g <- igraph::induced_subgraph(g, igraph::V(g)[igraph::degree(g) > 0])
    if (igraph::vcount(g) == 0) {
      return(NULL)
    }
    comp <- igraph::components(g)
    keep <- which(comp$csize >= min_nodes)
    if (length(keep) == 0) {
      return(NULL)
    }
    g <- igraph::induced_subgraph(g, igraph::V(g)[comp$membership %in% keep])
  } else {
    comp <- igraph::components(g)
    is_isolated <- igraph::degree(g) == 0
    keep_cluster <- comp$csize[comp$membership] >= min_nodes
    keep <- is_isolated | keep_cluster
    g <- igraph::induced_subgraph(g, igraph::V(g)[keep])
  }
  if (igraph::vcount(g) == 0) {
    return(NULL)
  }
  igraph::V(g)$cluster <- igraph::components(g)$membership
  # Denominator for a node's clone-size fraction shown in tooltips.
  g <- igraph::set_graph_attr(g, "total_cells", nrow(seg))
  # Draw coordinates travel WITH the graph, deliberately. The layout is a
  # function of the graph's structure alone, so it belongs to the thing the
  # caller caches on the build parameters. Computing it at render time instead
  # would redo it on every colour change — and, worse, a colour change would
  # then be free to re-arrange the network, which is not a colour change.
  xy <- hla_motif_layout(g)
  if (!is.null(xy)) {
    igraph::V(g)$layout_x <- xy[, 1]
    igraph::V(g)$layout_y <- xy[, 2]
  }
  g
}
