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
#     (A-B=1, B-C=1 keeps A,C together even when A-C>1). The component diameter
#     is reported so callers never imply all-pairs distance <= 1.
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

#' Detect receptor chains present in an immune-repertoire list
#'
#' Scans the `CTgene` column of the first few samples for chain tokens.
#'
#' @param data Named list of scRepertoire-style data.frames (one per sample).
#' @return Character vector of detected chains, subset of TRA/TRB/TRG/TRD/
#'   IGH/IGK/IGL, in that canonical order.
#' @keywords internal
hla_detect_chains <- function(data) {
  if (is.null(data) || !is.list(data) || length(data) == 0) {
    return(character(0))
  }
  sample_dfs <- data[seq_len(min(length(data), 3))]
  all_ct <- unlist(lapply(sample_dfs, function(df) {
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
    keep <- !is.na(v_gene) & !is.na(j_gene) & !is.na(cdr3) & nzchar(cdr3)
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
  n <- length(seqs)

  # Single distance measure (Hamming); edge and adjacency use the SAME
  # threshold (== 1) so component membership and drawn edges never diverge.
  dist_mat <- stringdist::stringdistmatrix(seqs, seqs, method = "hamming")
  adj <- dist_mat == 1
  diag(adj) <- FALSE
  g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected")
  comps <- igraph::components(g)

  # Component diameter = max pairwise Hamming distance within the component.
  # Reported so a transitive component (A-B=1, B-C=1, A-C=2) is never presented
  # as if every pair were <= 1.
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
  df$motif_diameter <- diam[comps$membership]
  df$motif_consensus <- consensus[comps$membership]

  edges <- NULL
  if (n > 1) {
    idx <- which(dist_mat == 1 & upper.tri(dist_mat), arr.ind = TRUE)
    if (nrow(idx) > 0) {
      edges <- data.frame(
        from = seqs[idx[, 1]],
        to = seqs[idx[, 2]],
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

#' Aggregate parsed segments into unique-CDR3 nodes carrying distributions
#'
#' Node key = unique CDR3 amino-acid string. `clone_count` = number of cells
#' carrying that CDR3. Categorical metadata columns are summarised as their
#' most-common value (`_mode`) plus a compact "N types: A (5), B (2)"
#' distribution string (`_dist`) so the tooltip can show provenance without
#' collapsing it to a single label.
#'
#' @param seg Output of [hla_parse_ir_segments()].
#' @param meta_cols Character vector of metadata columns to summarise per node.
#' @param context_col Optional name of a per-cell MHC-context column (values
#'   "Class I" / "Class II" / "Unknown"). When given, the node gets a
#'   [hla_context_summary()] value (single class, "Mixed", or "Unknown") instead
#'   of a plain mode, plus the usual `_dist` string.
#' @return A per-CDR3 data.frame, or NULL when `seg` is empty.
#' @keywords internal
hla_aggregate_cdr3_nodes <- function(
  seg,
  meta_cols = character(0),
  context_col = NULL
) {
  if (is.null(seg) || nrow(seg) == 0) {
    return(NULL)
  }
  meta_cols <- intersect(meta_cols, colnames(seg))
  mode_val <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) {
      return(NA_character_)
    }
    names(sort(table(as.character(x)), decreasing = TRUE))[1]
  }
  dist_str <- function(x) {
    x <- x[!is.na(x) & nzchar(as.character(x))]
    if (length(x) == 0) {
      return(NA_character_)
    }
    tab <- sort(table(as.character(x)), decreasing = TRUE)
    parts <- paste0(names(tab), " (", as.integer(tab), ")")
    sprintf(
      "%d type%s: %s",
      length(tab),
      if (length(tab) == 1) "" else "s",
      paste(parts, collapse = ", ")
    )
  }
  agg <- do.call(
    rbind,
    lapply(split(seg, seg$cdr3), function(d) {
      row <- data.frame(
        cdr3 = d$cdr3[1],
        v_gene = mode_val(d$v_gene),
        j_gene = mode_val(d$j_gene),
        v_gene_dist = dist_str(d$v_gene),
        j_gene_dist = dist_str(d$j_gene),
        clone_count = nrow(d),
        stringsAsFactors = FALSE
      )
      for (mc in meta_cols) {
        row[[mc]] <- mode_val(d[[mc]])
        row[[paste0(mc, "_dist")]] <- dist_str(d[[mc]])
      }
      if (!is.null(context_col) && context_col %in% colnames(d)) {
        row[[context_col]] <- hla_context_summary(
          as.character(d[[context_col]])
        )
        row[[paste0(context_col, "_dist")]] <- dist_str(d[[context_col]])
      }
      row
    })
  )
  rownames(agg) <- NULL
  agg
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
#' @param context_col Optional per-cell MHC-context column; the node gets a
#'   [hla_context_summary()] value (single class / "Mixed" / "Unknown").
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
  context_col = NULL
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
    context_col = context_col
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
  vertices$name <- vertices$cdr3
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
  g
}
