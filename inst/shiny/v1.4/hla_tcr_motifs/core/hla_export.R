# ============================================================================
# HLA & TCR Motifs — analysis export
# ============================================================================
# A picture of a network is not a result: it cannot be recomputed, diffed or
# audited. These helpers turn what the page shows into tables plus a manifest
# that records exactly how they were produced — which data set, which HLA and
# with what provenance, which filters, and every caveat that applied.
#
# The manifest is the part that matters. Numbers copied out of a screenshot
# lose the fact that (say) the receptors were selected on the very association
# being displayed; the manifest carries that with the data.
# ============================================================================

#' Build the export manifest for one HLA & TCR Motifs view
#'
#' Records the parameters and caveats a reader needs to interpret (or
#' recompute) the exported tables. Pure: takes values, returns a data.frame.
#'
#' @param dataset Name of the loaded data set.
#' @param chain Receptor chain analysed.
#' @param input_channel Where the active HLA came from ("stored .crb" /
#'   "session upload" / "none").
#' @param hla_source_type Provenance of the genotype (genotyped / imputed /
#'   synthetic / unknown).
#' @param unit_type Statistical unit actually used ("donor" / "sample").
#' @param observation_unit What one row of the data set is ("cell" /
#'   "analysis unit").
#' @param n_units,n_nodes,n_edges,n_motifs Counts describing the exported view.
#' @param min_nodes,split_by_v,show_isolated Motif build parameters.
#' @param allele Allele the view was coloured / summarised by, if any.
#' @param scope Network scope in effect ("all" / "allele" / "pair"); with "all"
#'   the whole graph is shown and the allele only re-colours it, so the scope is
#'   needed to know whether the exported nodes are a subset.
#' @param allele_i,allele_ii The two alleles when `scope` is a Class I x II pair,
#'   otherwise NA.
#' @param lineage_column Metadata column the CD4/CD8 lineage was read from, if
#'   any; it determines the class filter that allele / pair scope applied.
#' @param tcr_selection Declared receptor-selection provenance, if any.
#' @param qc_warnings Character vector of QC warnings, if any.
#' @param app_version Package version string.
#' @return A two-column data.frame(field, value).
#' @keywords internal
hla_build_manifest <- function(
  dataset,
  chain,
  input_channel,
  hla_source_type,
  unit_type,
  observation_unit,
  n_units,
  n_nodes,
  n_edges,
  n_motifs,
  min_nodes,
  split_by_v,
  show_isolated,
  allele = NA_character_,
  scope = NA_character_,
  allele_i = NA_character_,
  allele_ii = NA_character_,
  lineage_column = NA_character_,
  tcr_selection = NA_character_,
  qc_warnings = character(0),
  app_version = NA_character_
) {
  na_blank <- function(x) {
    if (length(x) == 0 || is.na(x[1]) || !nzchar(as.character(x[1]))) {
      "(none)"
    } else {
      as.character(x[1])
    }
  }
  fields <- list(
    "generated_at" = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    "CerebroNexus_version" = na_blank(app_version),
    "dataset" = na_blank(dataset),
    "chain" = na_blank(chain),
    "observation_unit" = na_blank(observation_unit),
    "statistical_unit" = na_blank(unit_type),
    "n_analysis_units" = as.character(n_units),
    "hla_input_channel" = na_blank(input_channel),
    "hla_source_type" = na_blank(hla_source_type),
    "hla_allele_shown" = na_blank(allele),
    "network_scope" = na_blank(scope),
    "pair_allele_i" = na_blank(allele_i),
    "pair_allele_ii" = na_blank(allele_ii),
    "lineage_column" = na_blank(lineage_column),
    "edge_rule" = "Hamming distance == 1, equal-length CDR3 only",
    "node_key" = if (isTRUE(split_by_v)) "V gene + CDR3" else "CDR3",
    "minimum_motif_nodes" = as.character(min_nodes),
    "split_by_v_gene" = as.character(isTRUE(split_by_v)),
    "show_unconnected_cdr3" = as.character(isTRUE(show_isolated)),
    "n_nodes" = as.character(n_nodes),
    "n_edges" = as.character(n_edges),
    "n_motifs" = as.character(n_motifs),
    "tcr_selection" = na_blank(tcr_selection),
    # State the ceiling on interpretation IN the export, so a table that leaves
    # the app cannot be read as more than it is.
    "evidence_level" = paste(
      "Descriptive overlap only. No hypothesis test, no p-value.",
      "Alleles shown are candidate co-occurrences, NOT confirmed TCR",
      "restriction."
    ),
    "fraction_denominator" = paste(
      "Fractions are over what THIS data set contains per unit, not the",
      "unit's full repertoire."
    ),
    "qc_warnings" = if (length(qc_warnings) == 0) {
      "(none)"
    } else {
      paste(qc_warnings, collapse = " | ")
    }
  )
  if (identical(tcr_selection, "association-conditioned")) {
    fields[["interpretation_warning"]] <- paste(
      "POSITIVE CONTROL: this data set's receptors were selected using the",
      "HLA association shown, so a carrier/non-carrier contrast here is a",
      "consequence of that selection, not independent evidence."
    )
  }
  if (identical(tcr_selection, "synthetic")) {
    fields[["interpretation_warning"]] <- paste(
      "FABRICATED FIXTURE: this data set's receptor sequences and their HLA",
      "association were both constructed. Nothing exported here is a",
      "measurement, and no contrast in it is evidence of anything."
    )
  }
  data.frame(
    field = names(fields),
    value = unlist(fields, use.names = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Node and edge tables for a motif graph
#'
#' Turns the graph into the two tables an export needs. Vertex attributes are
#' carried through as-is; edges are emitted as CDR3 endpoint pairs so the table
#' is meaningful without the graph object.
#'
#' @param graph A motif igraph from [hla_build_motif_graph()].
#' @return list(nodes = data.frame, edges = data.frame).
#' @keywords internal
hla_graph_tables <- function(graph) {
  if (!hla_motif_graph_ok(graph)) {
    return(list(
      nodes = data.frame(),
      edges = data.frame(from = character(0), to = character(0))
    ))
  }
  nodes <- igraph::as_data_frame(graph, what = "vertices")
  edges <- igraph::as_data_frame(graph, what = "edges")
  list(nodes = nodes, edges = edges)
}

#' Per-motif summary table
#'
#' One row per Hamming-1 connected component: size, consensus and max mismatch.
#' `max_mismatch` is included because a component's membership is transitive, so
#' its members are not all within distance 1 of each other. It is the largest
#' pairwise Hamming distance in the component — NOT the graph's diameter, which
#' counts hops and is larger.
#'
#' @param graph A motif igraph from [hla_build_motif_graph()].
#' @return data.frame(motif_group, n_cdr3, consensus, max_mismatch).
#' @keywords internal
hla_motif_summary <- function(graph) {
  if (!hla_motif_graph_ok(graph)) {
    return(data.frame(
      motif_group = character(0),
      n_cdr3 = integer(0),
      consensus = character(0),
      max_mismatch = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  v <- igraph::as_data_frame(graph, what = "vertices")
  if (!"motif_group" %in% colnames(v)) {
    return(data.frame(
      motif_group = character(0),
      n_cdr3 = integer(0),
      consensus = character(0),
      max_mismatch = integer(0),
      stringsAsFactors = FALSE
    ))
  }
  parts <- split(v, v$motif_group)
  out <- do.call(
    rbind,
    lapply(names(parts), function(g) {
      d <- parts[[g]]
      data.frame(
        motif_group = g,
        n_cdr3 = nrow(d),
        consensus = if ("motif_consensus" %in% colnames(d)) {
          as.character(d$motif_consensus[1])
        } else {
          NA_character_
        },
        max_mismatch = if ("motif_max_mismatch" %in% colnames(d)) {
          as.integer(d$motif_max_mismatch[1])
        } else {
          NA_integer_
        },
        stringsAsFactors = FALSE
      )
    })
  )
  out <- out[order(-out$n_cdr3, out$motif_group), , drop = FALSE]
  rownames(out) <- NULL
  out
}
