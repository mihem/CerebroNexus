# ============================================================================
# Descriptive HLA x TCR-feature overlap (no inferential statistics)
# ============================================================================

#' Resolve sample or donor as the descriptive analysis unit
#'
#' Donor-level collapse is used only when every in-scope sample has exactly one
#' non-empty donor ID. Otherwise all units remain samples; mixed donor/sample
#' counting is deliberately avoided.
#'
#' @param typing Canonical HLA typing table.
#' @param samples In-scope immune-repertoire sample names.
#' @return data.frame(sample, analysis_unit, unit_type).
#' @keywords internal
hla_analysis_unit_map <- function(typing, samples) {
  samples <- unique(as.character(samples))
  if (length(samples) == 0) {
    return(data.frame(
      sample = character(0),
      analysis_unit = character(0),
      unit_type = character(0),
      stringsAsFactors = FALSE
    ))
  }
  in_scope <- if (hla_is_typing_table(typing)) {
    typing[typing$sample %in% samples, , drop = FALSE]
  } else {
    .hla_empty_long()
  }
  donor_by_sample <- vapply(
    samples,
    function(s) {
      donors <- unique(as.character(in_scope$donor_id[
        in_scope$sample == s &
          !is.na(in_scope$donor_id) &
          nzchar(as.character(in_scope$donor_id))
      ]))
      if (length(donors) == 1) donors else NA_character_
    },
    character(1)
  )
  use_donor <- all(!is.na(donor_by_sample))
  data.frame(
    sample = samples,
    analysis_unit = if (use_donor) unname(donor_by_sample) else samples,
    unit_type = if (use_donor) "donor" else "sample",
    stringsAsFactors = FALSE
  )
}

#' Classify each analysis unit for one HLA allele
#'
#' Non-carrier means the allele's locus was typed at BOTH copies and the allele
#' was absent from both. Anything short of that -- no typing at the locus, only
#' one copy called, or typing too coarse to decide -- is untyped: no information
#' either way, and excluded from both groups rather than assumed negative.
#'
#' @param typing Canonical HLA typing table.
#' @param samples In-scope immune-repertoire sample names.
#' @param allele Canonical or normalizable HLA allele.
#' @param unit_map Optional precomputed [hla_analysis_unit_map()] for the same
#'   `typing` and `samples`. It is allele-independent, so a caller looping over
#'   many alleles (e.g. hla_allele_carrier_summary) can build it once and pass it
#'   in instead of rebuilding it per allele. Computed here when `NULL`.
#' @return data.frame(analysis_unit, unit_type, hla_status).
#' @keywords internal
hla_allele_status_by_unit <- function(
  typing,
  samples,
  allele,
  unit_map = NULL
) {
  allele <- hla_normalize_allele(allele)
  if (is.na(allele)) {
    stop("allele must be a recognizable HLA allele", call. = FALSE)
  }
  if (is.null(unit_map)) {
    unit_map <- hla_analysis_unit_map(typing, samples)
  }
  units <- unique(unit_map[, c("analysis_unit", "unit_type"), drop = FALSE])
  units$hla_status <- "untyped"
  if (!hla_is_typing_table(typing) || nrow(typing) == 0 || nrow(units) == 0) {
    return(units)
  }

  in_scope <- typing[typing$sample %in% unit_map$sample, , drop = FALSE]
  sample_to_unit <- stats::setNames(unit_map$analysis_unit, unit_map$sample)
  locus <- hla_allele_locus(allele)
  locus_rows <- in_scope[in_scope$locus == locus, , drop = FALSE]

  # Only a COMPLETELY called locus can put a unit in the comparison group. A
  # unit typed at one copy has an unknown second copy that may yet be the
  # allele, so ruling it out would seed the non-carrier group with possible
  # carriers -- a false-negative bias pointing the same way as the effect being
  # looked for. See hla_locus_call_state for why row count is the only signal.
  call_state <- hla_locus_call_state(typing, unit_map$sample, locus)
  complete_units <- unique(unname(sample_to_unit[
    call_state$sample[call_state$call_state == "complete"]
  ]))

  # Field-wise, not string-equal: typing is never zero-padded, so `A*02` and
  # `A*02:01` are different strings for the same family. See hla_allele_compare.
  cmp <- vapply(
    as.character(locus_rows$allele),
    hla_allele_compare,
    character(1),
    query_allele = allele,
    USE.NAMES = FALSE
  )
  carrier_units <- unique(unname(sample_to_unit[
    locus_rows$sample[cmp == "carrier"]
  ]))
  ambiguous_units <- unique(unname(sample_to_unit[
    locus_rows$sample[cmp == "ambiguous"]
  ]))

  units$hla_status[units$analysis_unit %in% complete_units] <- "non-carrier"
  # Typing too coarse to decide cannot rule the allele OUT, so the unit must not
  # join the comparison group. "untyped" already means "no information either
  # way" and is already excluded from carrier calls, which is exactly right.
  units$hla_status[units$analysis_unit %in% ambiguous_units] <- "untyped"
  # Applied last: one copy that settles it outranks another that does not.
  units$hla_status[units$analysis_unit %in% carrier_units] <- "carrier"
  units
}

#' Per-node HLA carrier status for one allele (render-time, cache-safe)
#'
#' Maps each motif node to the carrier status of the samples it was observed in,
#' for ONE allele. Deliberately a render-time helper: it takes the node's sample
#' set (`samples_all`, an allele-independent node attribute) rather than being
#' baked into the graph, so switching allele re-colours without rebuilding the
#' Hamming graph.
#'
#' A node aggregates observations from possibly several samples, so the status
#' summarises those samples' statuses. The labels describe the TYPED samples
#' only, because an untyped sample carries no information either way:
#'   - "Carrier"      at least one typed carrier and NO typed non-carrier;
#'   - "Non-carrier"  at least one typed non-carrier and NO typed carrier;
#'   - "Mixed"        both a typed carrier and a typed non-carrier;
#'   - "Untyped"      no carrying sample is typed at the allele's locus.
#'
#' A "Carrier" node may therefore also have been seen in untyped samples: it
#' means "no evidence against", not "every sample is a carrier". Because that
#' distinction is invisible in a colour, callers must surface the counts (see
#' [hla_node_carrier_counts()]) rather than let the label stand alone.
#'
#' This is candidate co-occurrence, NOT restriction: a carrier's TCR is not
#' thereby restricted by that allele.
#'
#' @param samples_all Character vector, one entry per node: a comma-separated
#'   sorted sample list (the `samples_all` node attribute).
#' @param typing Canonical HLA typing table.
#' @param samples In-scope immune-repertoire sample names.
#' @param allele Canonical or normalizable HLA allele.
#' @return Character vector, one status per node.
#' @keywords internal
hla_node_carrier_status <- function(samples_all, typing, samples, allele) {
  n <- length(samples_all)
  if (n == 0) {
    return(character(0))
  }
  status <- tryCatch(
    hla_allele_status_by_unit(typing, samples, allele),
    error = function(e) NULL
  )
  if (is.null(status) || nrow(status) == 0) {
    return(rep("Untyped", n))
  }
  # Samples map to analysis units (donor when donor mapping is complete), so
  # resolve each node's samples through the same unit map the tables use.
  unit_map <- hla_analysis_unit_map(typing, samples)
  sample_to_unit <- stats::setNames(unit_map$analysis_unit, unit_map$sample)
  unit_status <- stats::setNames(status$hla_status, status$analysis_unit)

  counts <- hla_node_carrier_counts(samples_all, typing, samples, allele)
  # The label is a thin function of the counts, so the two can never disagree.
  out <- rep("Untyped", n)
  has_c <- counts$n_carrier > 0
  has_n <- counts$n_noncarrier > 0
  out[has_c] <- "Carrier"
  out[has_n] <- "Non-carrier"
  out[has_c & has_n] <- "Mixed"
  out
}

#' Per-node carrier / non-carrier / untyped counts for one allele
#'
#' The counts behind [hla_node_carrier_status()]. A colour alone cannot say
#' whether a "Carrier" node rests on ten carriers or on one carrier and nine
#' untyped samples, so the UI shows these next to the label.
#'
#' @inheritParams hla_node_carrier_status
#' @return data.frame(n_carrier, n_noncarrier, n_untyped), one row per node.
#' @keywords internal
hla_node_carrier_counts <- function(samples_all, typing, samples, allele) {
  n <- length(samples_all)
  empty <- data.frame(
    n_carrier = integer(n),
    n_noncarrier = integer(n),
    n_untyped = integer(n)
  )
  if (n == 0) {
    return(empty[0, , drop = FALSE])
  }
  status <- tryCatch(
    hla_allele_status_by_unit(typing, samples, allele),
    error = function(e) NULL
  )
  if (is.null(status) || nrow(status) == 0) {
    return(empty)
  }
  unit_map <- hla_analysis_unit_map(typing, samples)
  sample_to_unit <- stats::setNames(unit_map$analysis_unit, unit_map$sample)
  unit_status <- stats::setNames(status$hla_status, status$analysis_unit)

  parts <- lapply(samples_all, function(s) {
    if (is.na(s) || !nzchar(s)) {
      return(c(0L, 0L, 0L))
    }
    smp <- strsplit(s, ",", fixed = TRUE)[[1]]
    # Count DISTINCT analysis units, not samples: two samples of one donor are
    # one unit and must not count twice.
    units <- unique(unname(sample_to_unit[smp]))
    units <- units[!is.na(units)]
    st <- unname(unit_status[units])
    c(
      sum(st == "carrier", na.rm = TRUE),
      sum(st == "non-carrier", na.rm = TRUE),
      sum(is.na(st) | st == "untyped")
    )
  })
  m <- do.call(rbind, parts)
  data.frame(
    n_carrier = as.integer(m[, 1]),
    n_noncarrier = as.integer(m[, 2]),
    n_untyped = as.integer(m[, 3])
  )
}

#' Descriptive overlap of one HLA allele with a frozen TCR feature
#'
#' A feature is supplied as its member CDR3 strings (one node or all nodes in a
#' frozen motif component). The function reports per-unit presence plus two
#' fractions. It deliberately performs no hypothesis test.
#'
#' **What the denominators are.** Both fractions are over what the DATA SET
#' contains for that unit — `n_cells` counts rows (observations), not
#' necessarily sequenced cells, and `n_unique_clonotypes` counts the clonotypes
#' present here. When the data set holds a selected subset of receptors (e.g.
#' one assembled from a published HLA association), these are fractions of that
#' subset and are NOT the unit's repertoire breadth or bulk clonal depth. The
#' caller is responsible for naming the unit and disclosing any selection; see
#' `technical_info$tcr_selection`.
#'
#' @param typing Canonical HLA typing table.
#' @param segments Parsed IR segments with `sample` and `cdr3` columns.
#' @param samples In-scope immune-repertoire sample names.
#' @param allele HLA allele to describe.
#' @param feature_cdr3 Character vector of CDR3 members in the frozen feature.
#' @param feature_v_gene Optional V gene per `feature_cdr3`. When supplied, the
#'   frozen feature is matched by `(V gene, CDR3)` rather than CDR3 alone.
#' @return Per-unit descriptive data.frame.
#' @keywords internal
hla_descriptive_feature_overlap <- function(
  typing,
  segments,
  samples,
  allele,
  feature_cdr3,
  feature_v_gene = NULL
) {
  if (
    !is.data.frame(segments) ||
      !all(c("sample", "cdr3") %in% colnames(segments))
  ) {
    stop("segments must contain sample and cdr3 columns", call. = FALSE)
  }
  unit_map <- hla_analysis_unit_map(typing, samples)
  status <- hla_allele_status_by_unit(typing, samples, allele)
  sample_to_unit <- stats::setNames(unit_map$analysis_unit, unit_map$sample)
  seg <- segments[segments$sample %in% unit_map$sample, , drop = FALSE]
  seg$analysis_unit <- unname(sample_to_unit[seg$sample])
  members <- unique(as.character(feature_cdr3))
  if (!is.null(feature_v_gene)) {
    if (
      !("v_gene" %in% colnames(seg)) ||
        length(feature_v_gene) != length(feature_cdr3)
    ) {
      stop(
        "V-specific features need segments$v_gene and one V gene per CDR3",
        call. = FALSE
      )
    }
    member_keys <- unique(paste(feature_v_gene, feature_cdr3, sep = "::"))
  } else {
    member_keys <- NULL
  }

  metrics <- do.call(
    rbind,
    lapply(unique(unit_map$analysis_unit), function(unit) {
      d <- seg[seg$analysis_unit == unit, , drop = FALSE]
      in_feature <- if (is.null(member_keys)) {
        d$cdr3 %in% members
      } else {
        paste(d$v_gene, d$cdr3, sep = "::") %in% member_keys
      }
      clone_keys <- if (is.null(member_keys)) {
        d$cdr3
      } else {
        paste(d$v_gene, d$cdr3, sep = "::")
      }
      n_cells <- nrow(d)
      n_feature_cells <- sum(in_feature)
      n_unique <- length(unique(clone_keys))
      n_feature_unique <- length(unique(clone_keys[in_feature]))
      data.frame(
        analysis_unit = unit,
        feature_present = n_feature_cells > 0,
        n_cells = as.integer(n_cells),
        n_feature_cells = as.integer(n_feature_cells),
        n_unique_clonotypes = as.integer(n_unique),
        n_feature_clonotypes = as.integer(n_feature_unique),
        unique_clonotype_fraction = if (n_unique > 0) {
          n_feature_unique / n_unique
        } else {
          NA_real_
        },
        cell_fraction = if (n_cells > 0) {
          n_feature_cells / n_cells
        } else {
          NA_real_
        },
        stringsAsFactors = FALSE
      )
    })
  )
  out <- merge(
    status,
    metrics,
    by = "analysis_unit",
    all.x = TRUE,
    sort = FALSE
  )
  out <- out[
    match(unique(unit_map$analysis_unit), out$analysis_unit),
    ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}

#' Build a descriptive analysis-unit by HLA-allele carrier matrix
#'
#' Cells are 1 for carrier, 0 for locus-typed non-carrier and NA when that locus
#' is untyped. No statistical operation is applied to the matrix.
#'
#' @param typing Canonical HLA typing table.
#' @param samples In-scope immune-repertoire sample names.
#' @return data.frame with analysis-unit metadata followed by allele columns.
#' @keywords internal
hla_unit_allele_matrix <- function(typing, samples) {
  unit_map <- hla_analysis_unit_map(typing, samples)
  out <- unique(unit_map[, c("analysis_unit", "unit_type"), drop = FALSE])
  if (!hla_is_typing_table(typing) || nrow(typing) == 0 || nrow(out) == 0) {
    return(out)
  }
  alleles <- sort(unique(typing$allele[typing$sample %in% unit_map$sample]))
  for (allele in alleles) {
    status <- hla_allele_status_by_unit(typing, samples, allele)
    values <- c("carrier" = 1L, "non-carrier" = 0L, "untyped" = NA_integer_)
    out[[allele]] <- unname(values[
      status$hla_status[match(out$analysis_unit, status$analysis_unit)]
    ])
  }
  out
}

## ---- Per-allele evidence scope ----------------------------------------- ##

#' Scope segments to one Class I x Class II allele pair
#'
#' The pair view asks a different question from the single-allele scope: not
#' "what do carriers of X look like" but "within the donors who could present on
#' X (class I) or Y (class II), which CDR3s turn up on each side, and which turn
#' up on BOTH".
#'
#' Every kept cell is assigned the ONE allele its own lineage could actually use
#' AND its donor actually carries:
#'   * a Class I (CD8) cell of a donor carrying `allele_i`  -> allele_i
#'   * a Class II (CD4/Treg) cell of a donor carrying `allele_ii` -> allele_ii
#'   * anything else is dropped, including a Class I cell from a donor who
#'     carries only `allele_ii`: that cell has no candidate in this pair, and
#'     keeping it would put a receptor under an allele its donor does not have.
#'
#' Unknown-lineage cells are always dropped: the assignment IS the lineage, so a
#' cell with no lineage cannot claim either side.
#'
#' As with every scope on this page, this is co-occurrence, never restriction: a
#' CDR3 sitting under `allele_i` only means it was seen in a CD8 cell of a donor
#' who carries it.
#'
#' @param seg Parsed segments (needs `sample` and the lineage context column).
#' @param typing Canonical HLA typing table.
#' @param allele_i A Class I allele.
#' @param allele_ii A Class II allele.
#' @param context_col Per-cell MHC-context column. Required: NULL returns NULL.
#' @return `seg` subset to the pair, plus a `pair_allele` column; NULL when the
#'   pair is not analysable (no lineage, same class, unrecognisable alleles).
#' @keywords internal
hla_scope_segments_by_allele_pair <- function(
  seg,
  typing,
  allele_i,
  allele_ii,
  context_col = "mhc_context"
) {
  if (is.null(seg) || nrow(seg) == 0) {
    return(seg)
  }
  # No lineage, no pair: the whole construction rests on knowing which class a
  # cell would present on. Returning an unfiltered graph here would silently
  # answer a question nobody asked.
  if (
    is.null(context_col) ||
      !(context_col %in% colnames(seg)) ||
      !("sample" %in% colnames(seg)) ||
      !hla_is_typing_table(typing)
  ) {
    return(NULL)
  }
  allele_i <- hla_normalize_allele(allele_i)
  allele_ii <- hla_normalize_allele(allele_ii)
  if (is.na(allele_i) || is.na(allele_ii)) {
    return(NULL)
  }
  class_i <- hla_locus_class(hla_allele_locus(allele_i))
  class_ii <- hla_locus_class(hla_allele_locus(allele_ii))
  # The pair is defined BY the two classes. Two alleles of one class would make
  # the lineage split meaningless — every CD8 cell would claim both.
  if (!identical(class_i, "Class I") || !identical(class_ii, "Class II")) {
    return(NULL)
  }

  carriers_i <- hla_carriers_of(typing, allele_i)
  carriers_ii <- hla_carriers_of(typing, allele_ii)
  samples <- as.character(seg$sample)
  ctx <- as.character(seg[[context_col]])

  pair_allele <- rep(NA_character_, nrow(seg))
  pair_allele[samples %in% carriers_i & ctx == "Class I"] <- allele_i
  pair_allele[samples %in% carriers_ii & ctx == "Class II"] <- allele_ii

  out <- seg[!is.na(pair_allele), , drop = FALSE]
  out$pair_allele <- pair_allele[!is.na(pair_allele)]
  rownames(out) <- NULL
  out
}

#' Restrict segments to the cells that could bear on one HLA allele
#'
#' The per-allele view of an HLA screen. Two filters, both necessary:
#'   1. the cell's sample must CARRY the allele (a non-carrier's receptor cannot
#'      be restricted by an allele the donor does not have);
#'   2. the cell's lineage-derived MHC class must MATCH the allele's class — a
#'      class II allele cannot restrict a CD8 cell's receptor, and vice versa.
#' The Hamming graph is then rebuilt on the subset, so an edge never joins a
#' carrier's CDR3 to a non-carrier's. That is the difference from re-colouring a
#' global graph, which leaves such edges in place.
#'
#' Cells whose context is "Unknown" are dropped by the class filter rather than
#' assumed into a class. When the data set has no context column at all (a bulk
#' repertoire has no lineage), only the carrier filter applies — a weaker scope,
#' which the caller must surface rather than present as class-matched.
#'
#' This is a SUBSET, not a test, and it deliberately removes the comparison
#' group: inside it every donor is a carrier, so "this motif recurs across
#' donors" cannot be told apart from an ordinary public TCR. The carrier
#' colouring on the unscoped graph is what supplies that contrast.
#'
#' Note also that a carrier has up to six class I alleles; scoping to one of
#' them keeps ALL of that donor's class-I-restricted receptors, including those
#' restricted by the other five. The scope is candidate co-occurrence, never
#' confirmed restriction.
#'
#' @param seg Parsed segments; needs a `sample` column.
#' @param typing Canonical HLA typing table.
#' @param allele Canonical allele, e.g. `"HLA-A*02:01"`.
#' @param context_col Name of the per-cell MHC-context column
#'   ("Class I"/"Class II"/"Unknown"), or NULL to skip class matching.
#' @return A subset of `seg` (possibly zero rows), or NULL when unusable.
#' @keywords internal
hla_scope_segments_by_allele <- function(
  seg,
  typing,
  allele,
  context_col = "mhc_context"
) {
  if (is.null(seg) || nrow(seg) == 0) {
    return(seg)
  }
  if (
    is.null(allele) ||
      length(allele) != 1L ||
      is.na(allele) ||
      !nzchar(allele) ||
      !hla_is_typing_table(typing) ||
      !("sample" %in% colnames(seg))
  ) {
    return(NULL)
  }
  # Resolution-aware: a donor typed A*02:01 carries A*02, and the scope must not
  # drop them over a string mismatch. Donors too coarsely typed to decide stay
  # out — they are unknown, not carriers.
  carriers <- hla_carriers_of(typing, allele)
  keep <- if (length(carriers) == 0) {
    rep(FALSE, nrow(seg))
  } else {
    as.character(seg$sample) %in% carriers
  }
  cls <- hla_locus_class(hla_allele_locus(allele))
  if (
    !is.null(context_col) &&
      context_col %in% colnames(seg) &&
      cls %in% c("Class I", "Class II")
  ) {
    keep <- keep & !is.na(seg[[context_col]]) & seg[[context_col]] == cls
  }
  out <- seg[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}
