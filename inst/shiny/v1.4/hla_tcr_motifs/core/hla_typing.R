# ============================================================================
# HLA typing: canonical long-table contract + normalization
# ============================================================================
# The single persistence contract for HLA typing is a canonical LONG table (one
# row per sample x locus x copy), carrying data provenance so the app never
# mistakes an uploaded / imputed / synthetic genotype for a directly typed one.
# named-list and wide inputs are accepted only as adapters that normalize INTO
# this long table.
#
# See tmp/DESIGN-hla-motif-network.md section 6 for the rationale (provenance,
# donor != cell, "context not restriction").
#
# These functions are Shiny-independent, installed and unit-testable.
# ============================================================================

## ---- Canonical schema -------------------------------------------------- ##
HLA_TYPING_COLUMNS <- c(
  "sample",
  "donor_id",
  "locus",
  "copy",
  "allele",
  "resolution",
  "source_type",
  "typing_method",
  "source_reference",
  "confidence"
)

## `source_type` describes how the genotype was obtained (evidence quality),
## NOT how it entered the app (that is the input channel: R API / seurat misc /
## upload). Only `genotyped` is eligible for confirmatory downstream analysis.
HLA_SOURCE_TYPES <- c("genotyped", "imputed", "synthetic", "unknown")

## MVP-interpretable loci. Others may be stored but are flagged "stored, not
## interpreted" -- DQ/DP need heterodimer pairing rules; HLA-E is non-classical.
HLA_MVP_LOCI <- c("HLA-A", "HLA-B", "HLA-C", "HLA-DRB1")
HLA_CLASS_I_LOCI <- c("HLA-A", "HLA-B", "HLA-C")
HLA_CLASS_II_LOCI <- c(
  "HLA-DRB1",
  "HLA-DRB3",
  "HLA-DRB4",
  "HLA-DRB5",
  "HLA-DQA1",
  "HLA-DQB1",
  "HLA-DPA1",
  "HLA-DPB1"
)

#' Normalize one raw allele token to canonical `HLA-<locus>*<fields>`
#'
#' Accepts `A*02:01`, `HLA-A*02:01`, or a bare `02:01` when `locus` is supplied.
#' `NNNN` / empty / `NA` become NA (missing). Field resolution is preserved
#' (never auto-padded). Returns NA for anything unrecognisable; the caller logs
#' it as a QC warning rather than silently dropping it.
#'
#' @param x A raw allele token.
#' @param locus Optional locus (e.g. "HLA-A") for bare `02:01` inputs.
#' @return A canonical allele string, or NA_character_.
#' @keywords internal
hla_normalize_allele <- function(x, locus = NULL) {
  if (is.null(x) || length(x) != 1) {
    return(NA_character_)
  }
  x <- trimws(as.character(x))
  if (is.na(x) || !nzchar(x) || toupper(x) %in% c("NA", "NNNN", "NONE", "-")) {
    return(NA_character_)
  }
  x <- toupper(x)
  x <- sub("^HLA-", "", x) # strip any HLA- prefix; re-added below
  # Exactly one '*' separates locus from fields. Two or more (e.g. A*02:01*03)
  # is malformed and must be rejected, not silently truncated to A*02:01.
  if (nchar(x) - nchar(gsub("*", "", x, fixed = TRUE)) > 1L) {
    return(NA_character_)
  }
  # Split "<locus>*<fields>" if present.
  if (grepl("\\*", x)) {
    parts <- strsplit(x, "\\*")[[1]]
    loc <- parts[1]
    fields <- parts[2]
  } else {
    # bare fields like "02:01" need the caller's locus
    if (is.null(locus) || is.na(locus) || !nzchar(locus)) {
      return(NA_character_)
    }
    loc <- sub("^HLA-", "", toupper(trimws(locus)))
    fields <- x
  }
  # Field portion must look like colon-separated numeric fields, optionally with
  # a trailing expression suffix letter (N/L/S/C/A/Q) or official ambiguity
  # group suffix (G/P). Resolution is preserved.
  if (!grepl("^[0-9]{1,3}(:[0-9]{1,3}){0,3}[NLSCAQGP]?$", fields)) {
    return(NA_character_)
  }
  if (!grepl("^[A-Z0-9]+$", loc)) {
    return(NA_character_)
  }
  paste0("HLA-", loc, "*", fields)
}

#' Count the field resolution of a canonical allele (e.g. 2-field)
#' @keywords internal
hla_allele_resolution <- function(allele) {
  if (is.na(allele)) {
    return(NA_character_)
  }
  fields <- sub("^HLA-[A-Z0-9]+\\*", "", allele)
  fields <- sub("[NLSCAQGP]$", "", fields)
  n <- length(strsplit(fields, ":", fixed = TRUE)[[1]])
  paste0(n, "-field")
}

#' Locus of a canonical allele (e.g. "HLA-A")
#' @keywords internal
hla_allele_locus <- function(allele) {
  if (is.na(allele)) {
    return(NA_character_)
  }
  sub("\\*.*$", "", allele)
}

#' MHC class of a locus: "Class I" / "Class II" / "Other"
#' @keywords internal
hla_locus_class <- function(locus) {
  ifelse(
    locus %in% HLA_CLASS_I_LOCI,
    "Class I",
    ifelse(locus %in% HLA_CLASS_II_LOCI, "Class II", "Other")
  )
}

## ---- Long-table normalization ------------------------------------------ ##

# Turn a named list (sample -> allele vector, a la 57.R hla_by_patient) into a
# long data.frame of (sample, locus, allele) before canonicalisation.
.hla_named_list_to_long <- function(x) {
  do.call(
    rbind,
    lapply(names(x), function(s) {
      alleles <- x[[s]]
      if (length(alleles) == 0) {
        return(NULL)
      }
      data.frame(
        sample = s,
        allele_raw = as.character(alleles),
        stringsAsFactors = FALSE
      )
    })
  )
}

# Turn a wide table (sample column + one column per HLA-*_1 / HLA-*_2 slot, a la
# 57.R HLA_typing_v3.xlsx) into long (sample, locus, allele_raw).
.hla_wide_to_long <- function(df) {
  sample_col <- intersect(c("sample", "sample_ID", "patient_id"), colnames(df))
  if (length(sample_col) == 0) {
    stop("wide HLA table needs a 'sample' column", call. = FALSE)
  }
  sample_col <- sample_col[1]
  donor_col <- intersect(c("donor_id", "donor"), colnames(df))
  donor_col <- if (length(donor_col) > 0) donor_col[1] else NULL
  hla_cols <- grep("^HLA-", colnames(df), value = TRUE, ignore.case = TRUE)
  if (length(hla_cols) == 0) {
    stop("wide HLA table needs HLA-* columns", call. = FALSE)
  }
  do.call(
    rbind,
    lapply(hla_cols, function(col) {
      locus <- sub("_[12]$", "", col) # HLA-A_1 -> HLA-A
      data.frame(
        sample = as.character(df[[sample_col]]),
        donor_id = if (is.null(donor_col)) {
          NA_character_
        } else {
          as.character(df[[donor_col]])
        },
        locus_hint = locus,
        allele_raw = as.character(df[[col]]),
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Read an uploaded HLA typing file into a raw data.frame
#'
#' Delimiter is sniffed from the file name (`.tsv` -> tab, otherwise comma), so
#' the name matters even when the bytes live in a temp file, as they do behind
#' a Shiny fileInput.
#'
#' `check.names = FALSE` is the entire reason this is a function. R's default
#' rewrites any column name that is not a syntactic identifier, which turns the
#' documented wide format's `HLA-A_1` into `HLA.A_1` -- and [.hla_wide_to_long]
#' matches columns on `^HLA-`. With the default, the wide upload the Data & QC
#' tab advertises cannot survive its own read: every real wide file dies as
#' "no valid HLA alleles found", pointing the user at their data instead of at
#' this line. Long uploads are unaffected either way (`sample`, `locus`,
#' `allele` are already syntactic).
#'
#' @param path Path to the file on disk.
#' @param name Original file name, used only to pick the delimiter. Defaults to
#'   `path`.
#' @return A data.frame with column names exactly as written in the file.
#' @keywords internal
hla_read_typing_file <- function(path, name = path) {
  if (!file.exists(path)) {
    stop("HLA typing file does not exist: ", path, call. = FALSE)
  }
  if (grepl("\\.tsv$", name, ignore.case = TRUE)) {
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

#' Normalize any accepted HLA input into the canonical long table
#'
#' Accepts:
#'   * a canonical/near-canonical long data.frame (has `sample` + `allele`);
#'   * a wide data.frame (`sample` + `HLA-A_1`, `HLA-A_2`, ... columns);
#'   * a named list (`sample -> c("HLA-A*02:01", ...)`).
#'
#' Output columns are exactly [HLA_TYPING_COLUMNS]. `copy` (1/2) is assigned per
#' (sample, locus) in input order. Unrecognisable alleles are dropped from the
#' table but reported via attribute "qc" (a data.frame of warnings). Provenance
#' defaults to `source_type = "unknown"` with a blocking warning when absent --
#' allele format is never used to guess `genotyped`.
#'
#' @param x One of the accepted inputs.
#' @param source_type One of [HLA_SOURCE_TYPES]; default "unknown".
#' @param typing_method,source_reference Optional provenance strings.
#' @return A canonical long data.frame with a "qc" attribute (warnings df).
#' @export
hla_normalize_typing <- function(
  x,
  source_type = "unknown",
  typing_method = NA_character_,
  source_reference = NA_character_
) {
  qc <- data.frame(
    sample = character(0),
    value = character(0),
    issue = character(0),
    stringsAsFactors = FALSE
  )
  add_qc <- function(sample, value, issue) {
    qc <<- rbind(
      qc,
      data.frame(
        sample = sample,
        value = value,
        issue = issue,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!source_type %in% HLA_SOURCE_TYPES) {
    add_qc(NA, source_type, "unknown source_type; treated as 'unknown'")
    source_type <- "unknown"
  }

  # Dispatch input shape -> a long frame with columns (sample, allele_raw) and
  # optionally locus_hint.
  long <- NULL
  if (is.list(x) && !is.data.frame(x)) {
    long <- .hla_named_list_to_long(x)
    if (!is.null(long)) {
      long$locus_hint <- NA_character_
    }
  } else if (is.data.frame(x)) {
    if (all(c("sample", "allele") %in% colnames(x))) {
      # already long-ish
      long <- data.frame(
        sample = as.character(x$sample),
        donor_id = if ("donor_id" %in% colnames(x)) {
          as.character(x$donor_id)
        } else {
          NA_character_
        },
        locus_hint = if ("locus" %in% colnames(x)) {
          as.character(x$locus)
        } else {
          NA_character_
        },
        allele_raw = as.character(x$allele),
        stringsAsFactors = FALSE
      )
    } else {
      long <- .hla_wide_to_long(x)
    }
  } else {
    stop(
      "HLA typing must be a named list or a data.frame",
      call. = FALSE
    )
  }

  if (is.null(long) || nrow(long) == 0) {
    out <- .hla_empty_long()
    attr(out, "qc") <- qc
    return(out)
  }
  if (!("donor_id" %in% colnames(long))) {
    long$donor_id <- NA_character_
  }

  # Canonicalise each allele; locus comes from the token or the hint.
  canon <- vapply(
    seq_len(nrow(long)),
    function(i) {
      hla_normalize_allele(long$allele_raw[i], locus = long$locus_hint[i])
    },
    character(1)
  )
  bad <- is.na(canon) &
    !is.na(long$allele_raw) &
    !toupper(trimws(long$allele_raw)) %in% c("NA", "NNNN", "", "NONE", "-")
  if (any(bad)) {
    for (i in which(bad)) {
      add_qc(long$sample[i], long$allele_raw[i], "unrecognisable allele")
    }
  }
  keep <- !is.na(canon)
  if (!any(keep)) {
    out <- .hla_empty_long()
    attr(out, "qc") <- qc
    return(out)
  }
  df <- data.frame(
    sample = long$sample[keep],
    donor_id = long$donor_id[keep],
    allele = canon[keep],
    stringsAsFactors = FALSE
  )
  df$locus <- vapply(df$allele, hla_allele_locus, character(1))
  df$resolution <- vapply(df$allele, hla_allele_resolution, character(1))

  # Assign copy 1/2 within (sample, locus) in input order; flag >2 as conflict.
  df$copy <- NA_integer_
  for (key in unique(paste(df$sample, df$locus))) {
    idx <- which(paste(df$sample, df$locus) == key)
    # de-duplicate identical alleles first (keep, but they occupy one copy slot)
    uniq_alleles <- df$allele[idx]
    copy_ids <- seq_along(idx)
    if (length(idx) > 2) {
      add_qc(
        df$sample[idx[1]],
        paste(uniq_alleles, collapse = ","),
        sprintf("> 2 alleles at %s; extra copies flagged", df$locus[idx[1]])
      )
    }
    df$copy[idx] <- copy_ids
  }
  df$copy[df$copy > 2L] <- NA_integer_ # keep row but mark copy unknown

  out <- data.frame(
    sample = df$sample,
    donor_id = df$donor_id,
    locus = df$locus,
    copy = df$copy,
    allele = df$allele,
    resolution = df$resolution,
    source_type = source_type,
    typing_method = typing_method,
    source_reference = source_reference,
    confidence = NA_real_,
    stringsAsFactors = FALSE
  )
  out <- out[, HLA_TYPING_COLUMNS, drop = FALSE]
  rownames(out) <- NULL
  if (identical(source_type, "unknown")) {
    add_qc(
      NA,
      NA,
      "source_type is 'unknown'; descriptive context only, no association"
    )
  }
  attr(out, "qc") <- qc
  out
}

# An empty canonical long table with the right columns/types.
.hla_empty_long <- function() {
  out <- data.frame(
    sample = character(0),
    donor_id = character(0),
    locus = character(0),
    copy = integer(0),
    allele = character(0),
    resolution = character(0),
    source_type = character(0),
    typing_method = character(0),
    source_reference = character(0),
    confidence = numeric(0),
    stringsAsFactors = FALSE
  )
  out[, HLA_TYPING_COLUMNS, drop = FALSE]
}

#' Is a value a valid canonical HLA long table?
#' @param x Candidate object.
#' @return TRUE when `x` is a data.frame with the canonical columns.
#' @keywords internal
hla_is_typing_table <- function(x) {
  is.data.frame(x) && all(HLA_TYPING_COLUMNS %in% colnames(x))
}

#' Validate and clean an already-canonical HLA typing table
#'
#' [hla_is_typing_table()] only checks that the columns exist, so a table can
#' pass it while still carrying an unrecognisable allele, a locus that
#' contradicts its allele, a copy value outside 1/2, or an invalid provenance.
#' `addHLATyping()` used to store such a table verbatim. This re-applies the
#' per-value rules [hla_normalize_typing()] enforces on raw input -- WITHOUT
#' clobbering the table's own provenance columns, which is why it is separate
#' from normalization (normalization stamps a single source_type from its
#' argument and would overwrite genuine per-row provenance).
#'
#' Rows with an unrecognisable allele are dropped; locus and resolution are
#' re-derived from the allele so they cannot contradict it; a copy outside 1/2
#' becomes NA; a source_type outside [HLA_SOURCE_TYPES] becomes "unknown".
#'
#' @param typing A data.frame with the canonical columns.
#' @return A cleaned canonical long table (possibly zero-row).
#' @keywords internal
hla_validate_typing <- function(typing) {
  if (!hla_is_typing_table(typing)) {
    return(.hla_empty_long())
  }
  out <- typing[, HLA_TYPING_COLUMNS, drop = FALSE]
  if (nrow(out) == 0) {
    rownames(out) <- NULL
    return(out)
  }
  allele <- vapply(
    as.character(out$allele),
    hla_normalize_allele,
    character(1),
    USE.NAMES = FALSE
  )
  out <- out[!is.na(allele), , drop = FALSE]
  out$allele <- allele[!is.na(allele)]
  if (nrow(out) == 0) {
    rownames(out) <- NULL
    return(out)
  }
  # Locus and resolution follow from the (now canonical) allele, so a stored
  # locus can never contradict its allele.
  out$locus <- vapply(out$allele, hla_allele_locus, character(1))
  out$resolution <- vapply(out$allele, hla_allele_resolution, character(1))
  # Any of these can arrive as a FACTOR (a stringsAsFactors frame, or a
  # read.csv import). Everything below must therefore go through character
  # first: as.integer() on a factor returns the level INDEX, not the value, so
  # copy "2" could silently become 1; and assigning a string into a factor
  # column that has no such level yields NA, so an invalid provenance would
  # vanish instead of becoming "unknown".
  for (col in c("sample", "donor_id", "typing_method", "source_reference")) {
    out[[col]] <- as.character(out[[col]])
  }
  if (is.factor(out$confidence)) {
    out$confidence <- suppressWarnings(as.numeric(as.character(out$confidence)))
  }
  # A diploid locus has copies 1 and 2; anything else is unknown, not kept as a
  # bogus copy id.
  out$copy <- suppressWarnings(as.integer(as.character(out$copy)))
  out$copy[!out$copy %in% c(1L, 2L)] <- NA_integer_
  # Provenance must be one of the declared source types; unknown/NA is treated
  # as "unknown" rather than silently accepted as evidence.
  out$source_type <- as.character(out$source_type)
  out$source_type[
    is.na(out$source_type) | !out$source_type %in% HLA_SOURCE_TYPES
  ] <- "unknown"
  rownames(out) <- NULL
  out
}

#' Carrier index: which samples carry each allele
#'
#' @param typing A canonical long table.
#' @return Named list `allele -> character vector of samples`, or empty list.
#' @keywords internal
hla_carrier_index <- function(typing) {
  if (!hla_is_typing_table(typing) || nrow(typing) == 0) {
    return(list())
  }
  by_allele <- split(typing$sample, typing$allele)
  lapply(by_allele, function(s) sort(unique(s)))
}

#' Compare a donor's typed allele against a queried allele, field by field
#'
#' Typing arrives at whatever resolution the lab reported and is never
#' zero-padded, so `HLA-A*02` and `HLA-A*02:01` are different STRINGS naming the
#' same molecule family. Exact string matching therefore got both directions
#' wrong, and both errors are false negatives that land people in the comparison
#' group that must not hold them:
#'   * a donor typed `A*02` looked like a NON-carrier of `A*02:01`, although
#'     `A*02:01` is an `A*02` and that donor may well have it;
#'   * a donor typed `A*02:01` looked like a NON-carrier of `A*02`, although it
#'     certainly carries one.
#'
#' Fields are compared as whole tokens, never as text prefixes, so `A*2` never
#' matches `A*24` and the locus must agree first.
#'
#' @param donor_allele One canonical allele from a donor's typing.
#' @param query_allele The canonical allele being asked about.
#' @return "carrier" when the donor's allele IS the query or refines it;
#'   "ambiguous" when the donor's typing is too coarse to decide either way;
#'   "no" when they disagree at a field both report.
#' @keywords internal
hla_allele_compare <- function(donor_allele, query_allele) {
  if (
    length(donor_allele) != 1L ||
      length(query_allele) != 1L ||
      is.na(donor_allele) ||
      is.na(query_allele)
  ) {
    return("no")
  }
  if (
    !identical(hla_allele_locus(donor_allele), hla_allele_locus(query_allele))
  ) {
    return("no")
  }
  fields <- function(x) {
    strsplit(sub("^[^*]*\\*", "", x), ":", fixed = TRUE)[[1]]
  }
  d <- fields(donor_allele)
  q <- fields(query_allele)
  n <- min(length(d), length(q))
  if (n == 0L) {
    return("no")
  }
  if (!identical(d[seq_len(n)], q[seq_len(n)])) {
    return("no")
  }
  if (length(d) >= length(q)) "carrier" else "ambiguous"
}

#' Samples that definitely carry an allele, resolution-aware
#'
#' Unlike [hla_carrier_index()], which keys on the exact allele string, this
#' resolves typing recorded at a finer resolution than the query (a donor typed
#' `A*02:01` does carry `A*02`). Donors whose typing is too coarse to decide are
#' NOT returned: they are unknown, not carriers.
#'
#' @param typing Canonical HLA typing table.
#' @param allele Canonical allele.
#' @return Character vector of sample names.
#' @keywords internal
hla_carriers_of <- function(typing, allele) {
  allele <- hla_normalize_allele(allele)
  if (!hla_is_typing_table(typing) || nrow(typing) == 0 || is.na(allele)) {
    return(character(0))
  }
  hit <- vapply(
    as.character(typing$allele),
    function(a) identical(hla_allele_compare(a, allele), "carrier"),
    logical(1),
    USE.NAMES = FALSE
  )
  sort(unique(as.character(typing$sample[hit])))
}

#' How completely a locus was called, per sample
#'
#' A negative call ("this donor does not carry X") is only valid once BOTH
#' copies of the locus are known: a donor typed `A*01:01` at one copy may still
#' carry `A*02:01` at the other. Sources differ here -- the synthetic fixture
#' writes a homozygote as two identical rows, while published carrier calls
#' (e.g. DeWitt) list positives only and never repeat a homozygote -- and the
#' `copy` column is re-numbered by row order on import, so it cannot tell the
#' two apart. Row count per sample x locus is therefore the only honest signal,
#' and one row has to read as "unknown second copy", not "homozygous".
#'
#' Counted per SAMPLE, never pooled across a donor's samples: two samples with
#' one copy each are two partial calls, not one diploid call.
#'
#' @param typing Canonical HLA typing table.
#' @param samples Sample names to report on.
#' @param locus Locus name, e.g. "HLA-A".
#' @return data.frame(sample, n_copies, call_state) where call_state is
#'   "complete" (>= 2 copies), "partial" (exactly 1) or "absent" (none).
#' @keywords internal
hla_locus_call_state <- function(typing, samples, locus) {
  samples <- unique(as.character(samples))
  out <- data.frame(
    sample = samples,
    n_copies = rep(0L, length(samples)),
    call_state = rep("absent", length(samples)),
    stringsAsFactors = FALSE
  )
  if (
    !hla_is_typing_table(typing) ||
      nrow(typing) == 0 ||
      length(samples) == 0
  ) {
    return(out)
  }
  rows <- typing[
    typing$sample %in% samples & typing$locus == locus,
    ,
    drop = FALSE
  ]
  counts <- table(factor(as.character(rows$sample), levels = samples))
  out$n_copies <- as.integer(counts[samples])
  out$call_state <- ifelse(
    out$n_copies >= 2L,
    "complete",
    ifelse(out$n_copies == 1L, "partial", "absent")
  )
  out
}

## ---- Lineage-derived MHC context -------------------------------------- ##

#' Map a cell-type label to a lineage-derived MHC class context
#'
#' CD8 lineage -> "Class I", CD4 / Treg -> "Class II", everything else ->
#' "Unknown" (never guessed). This is explicitly a lineage-derived CONTEXT, not
#' a confirmed restriction; a coarse "T cells" label yields "Unknown".
#'
#' @param cell_type A character vector of cell-type labels.
#' @return A character vector of "Class I" / "Class II" / "Unknown".
#' @keywords internal
hla_lineage_context <- function(cell_type) {
  x <- as.character(cell_type)
  out <- rep("Unknown", length(x))
  out[grepl("(^|[^A-Za-z])CD8", x, ignore.case = TRUE)] <- "Class I"
  # CD4 or Treg -> Class II; checked after CD8 so a rare "CD4 CD8" label would
  # already be Class I (double-positive is not a conventional restriction).
  is_ii <- grepl("(^|[^A-Za-z])CD4", x, ignore.case = TRUE) |
    grepl("treg", x, ignore.case = TRUE)
  out[is_ii & out == "Unknown"] <- "Class II"
  out
}

## Labels that name an experimental CONDITION rather than a cell lineage. They
## routinely carry a lineage token -- "anti-CD4" is a depleting antibody, not a
## CD4 cell; "CD8_case" is a study arm -- so a column of them scores as a lineage
## column unless they are excluded explicitly.
HLA_CONDITION_PATTERNS <- c(
  "(^|[^a-z])anti[-_ .]?cd", # anti-CD4, anti CD8, antiCD8
  "\u03b1[-_ .]?cd", # the same written with a Greek alpha (escaped: ASCII source)
  "treated|treatment|untreated|vehicle|mock",
  "stimulat|blockade|deplet",
  "(^|[-_ .])(case|control|ctrl)([-_ .]|$)"
)

#' Does a label read as an experimental condition rather than a cell lineage?
#'
#' @param x A character vector of labels.
#' @return A logical vector, TRUE where the label reads as a condition.
#' @keywords internal
hla_is_condition_label <- function(x) {
  x <- as.character(x)
  hits <- lapply(
    HLA_CONDITION_PATTERNS,
    function(p) grepl(p, x, ignore.case = TRUE)
  )
  Reduce(`|`, hits, init = rep(FALSE, length(x)))
}

#' Score how well a column's values read as a CD4/CD8 lineage label
#'
#' Used to INFER the lineage column when a data set does not declare
#' `technical_info$lineage_column`. The score is the share of values that resolve
#' to a real lineage AND do not read as an experimental condition. Counting
#' conditions would let a treatment or study-arm column win the lineage role and
#' silently change which cells the Class I / Class II scope keeps.
#'
#' @param values A character vector of the column's values.
#' @return A share in `[0, 1]`; 0 when nothing qualifies.
#' @keywords internal
hla_lineage_column_score <- function(values) {
  v <- as.character(values)
  v <- v[!is.na(v) & nzchar(v)]
  if (length(v) == 0) {
    return(0)
  }
  mean((hla_lineage_context(v) != "Unknown") & !hla_is_condition_label(v))
}

## The share a candidate column must reach before it may be INFERRED as the
## lineage column. One stray "CD4" among a thousand unrelated values is not
## evidence; a real lineage column resolves a substantial part of its values.
HLA_LINEAGE_MIN_SHARE <- 0.1

#' Summarise a distribution of MHC-context labels to one node summary
#'
#' A CDR3 node carries cells of possibly mixed lineage. Collapse the per-cell
#' contexts to a single node summary: a single non-Unknown class stays that
#' class; both Class I and Class II present -> "Mixed"; only Unknown -> "Unknown".
#'
#' @param contexts A character vector of per-cell context labels.
#' @return One of "Class I" / "Class II" / "Mixed" / "Unknown".
#' @keywords internal
hla_context_summary <- function(contexts) {
  has_i <- any(contexts == "Class I")
  has_ii <- any(contexts == "Class II")
  if (has_i && has_ii) {
    return("Mixed")
  }
  if (has_i) {
    return("Class I")
  }
  if (has_ii) {
    return("Class II")
  }
  "Unknown"
}

## The label for a CDR3 seen in BOTH compartments of a Class I x Class II pair.
## Not "Mixed": this page already uses that word for two other things (a node
## whose donors are part carrier / part non-carrier, and a node spanning both
## lineages), and all three would otherwise read as one concept.
HLA_PAIR_MIXED_LABEL <- "Both classes"

#' Summarise a node's per-cell candidate alleles into one pair class
#'
#' In a Class I x Class II pair scope every cell carries the allele its lineage
#' would present on ([hla_scope_segments_by_allele_pair]). A CDR3 node pools
#' cells, so it can span both compartments: that is the observation the pair
#' network exists to show, and it must not be averaged away -- taking the modal
#' allele would silently report such a node as whichever compartment happened to
#' contribute more cells.
#'
#' @param x Per-cell candidate alleles (NA where none applies).
#' @return The single allele when all cells agree, [HLA_PAIR_MIXED_LABEL] when
#'   both appear, NA when there is nothing to summarise.
#' @keywords internal
hla_pair_class_summary <- function(x) {
  vals <- unique(as.character(x[!is.na(x)]))
  if (length(vals) == 0) {
    return(NA_character_)
  }
  if (length(vals) > 1) {
    return(HLA_PAIR_MIXED_LABEL)
  }
  vals[1]
}

## ---- Descriptive HLA carrier summaries (NO inferential statistics) ----- ##

#' Descriptive per-allele carrier summary over analysis units
#'
#' For each allele in the typing table, count carriers, non-carriers and untyped
#' units. Every call is derived from [hla_allele_status_by_unit()] so the counts
#' match the association tables and network colours exactly: comparison is
#' resolution-aware, and only a completely-typed locus can be a non-carrier -- a
#' one-copy call is untyped, not assumed negative. Complete donor mappings are
#' collapsed to donor; otherwise the units remain samples. This is strictly
#' descriptive: it performs no enrichment test and reports no p-value.
#'
#' @param typing A canonical long table.
#' @param samples Character vector of samples to consider (e.g. the IR samples).
#' @return data.frame(allele, locus, mhc_class, n_carrier, n_noncarrier,
#'   n_untyped, carriers, analysis_unit) ordered by descending carrier count,
#'   or an empty frame. Complete donor mappings are collapsed to donor; otherwise
#'   the function reports sample-level counts.
#' @keywords internal
hla_allele_carrier_summary <- function(typing, samples) {
  empty <- function() {
    data.frame(
      allele = character(0),
      locus = character(0),
      mhc_class = character(0),
      n_carrier = integer(0),
      n_noncarrier = integer(0),
      n_untyped = integer(0),
      carriers = character(0),
      analysis_unit = character(0),
      stringsAsFactors = FALSE
    )
  }
  if (
    !hla_is_typing_table(typing) || nrow(typing) == 0 || length(samples) == 0
  ) {
    return(empty())
  }
  samples <- unique(as.character(samples))
  in_scope <- typing[typing$sample %in% samples, , drop = FALSE]
  # A typing table from another cohort is format-valid and non-empty, so it gets
  # this far and then matches nothing. Without this the allele loop runs zero
  # times, rbind of no frames gives NULL, and the sort below dies on `-NULL`.
  if (nrow(in_scope) == 0) {
    return(empty())
  }
  # Every carrier / non-carrier / untyped call is delegated to
  # hla_allele_status_by_unit(): the SINGLE source of truth the association
  # tables and network colours use too, so the picker counts here can never
  # disagree with what those show for the same sample. That helper is
  # resolution-aware (A*02 vs A*02:01) and only a COMPLETELY called locus can
  # seed the non-carrier group -- a one-copy call is untyped, not assumed
  # negative. This function is now just per-allele tallying and ordering; it
  # holds no carrier logic of its own.
  #
  # The unit map is allele-independent, so build it once and reuse it across
  # every allele rather than letting each status call rebuild it.
  unit_map <- hla_analysis_unit_map(typing, samples)
  alleles <- sort(unique(in_scope$allele))
  out <- do.call(
    rbind,
    lapply(alleles, function(a) {
      status <- hla_allele_status_by_unit(
        typing,
        samples,
        a,
        unit_map = unit_map
      )
      carriers <- sort(status$analysis_unit[status$hla_status == "carrier"])
      locus <- hla_allele_locus(a)
      data.frame(
        allele = a,
        locus = locus,
        mhc_class = hla_locus_class(locus),
        n_carrier = length(carriers),
        n_noncarrier = sum(status$hla_status == "non-carrier"),
        n_untyped = sum(status$hla_status == "untyped"),
        carriers = paste(carriers, collapse = ", "),
        analysis_unit = if (nrow(status) > 0) status$unit_type[1] else "sample",
        stringsAsFactors = FALSE
      )
    })
  )
  out <- out[order(-out$n_carrier, out$allele), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Per-sample allele coverage summary (for the Data & QC tab)
#'
#' @param typing A canonical long table.
#' @return data.frame(sample, n_alleles, loci) or an empty frame.
#' @keywords internal
hla_coverage_by_sample <- function(typing) {
  if (!hla_is_typing_table(typing) || nrow(typing) == 0) {
    return(data.frame(
      sample = character(0),
      n_alleles = integer(0),
      loci = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(
    rbind,
    lapply(split(typing, typing$sample), function(d) {
      data.frame(
        sample = d$sample[1],
        n_alleles = length(unique(d$allele)),
        loci = paste(sort(unique(d$locus)), collapse = ", "),
        stringsAsFactors = FALSE
      )
    })
  )
}
