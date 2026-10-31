##----------------------------------------------------------------------------##
## Stable Builder facts for immune-repertoire and HLA source content.
##
## This file profiles every source independently. Source priority, conversion,
## preservation, and final Viewer-page decisions belong to the frozen BuildPlan.
## Records cross the callr boundary, so payloads remain in the worker and every
## preview in this file has a fixed upper bound.
##----------------------------------------------------------------------------##

.builder_immune_required_columns <- c(
  "barcode",
  "CTgene",
  "CTnt",
  "CTaa",
  "CTstrict"
)

.builder_immune_chain_names <- c(
  "TRA",
  "TRB",
  "TRG",
  "TRD",
  "IGH",
  "IGK",
  "IGL"
)

.builder_immune_tcr_chains <- c("TRA", "TRB", "TRG", "TRD")
.builder_immune_bcr_chains <- c("IGH", "IGK", "IGL")
.builder_immune_preview_limit <- 20L
.builder_immune_preview_character_limit <- 160L
.builder_immune_preview_byte_limit <- 512L

.builder_immune_unique_text <- function(values) {
  if (is.null(values)) {
    return(character())
  }
  values <- as.character(values)
  unique(values[!is.na(values) & nzchar(values)])
}

.builder_immune_bound_text <- function(values) {
  if (!length(values)) {
    return(list(value = character(), truncated_count = 0L))
  }
  values <- as.character(values)
  truncated <- logical(length(values))
  out <- vapply(
    seq_along(values),
    function(index) {
      value <- values[[index]]
      if (is.na(value)) {
        return(NA_character_)
      }
      value <- iconv(value, from = "", to = "UTF-8", sub = "?")
      if (is.na(value)) {
        value <- "?"
      }
      codepoints <- utf8ToInt(value)
      too_many_characters <- length(codepoints) >
        .builder_immune_preview_character_limit
      too_many_bytes <- nchar(value, type = "bytes") >
        .builder_immune_preview_byte_limit
      if (!too_many_characters && !too_many_bytes) {
        return(value)
      }
      truncated[[index]] <<- TRUE
      character_budget <- max(
        0L,
        .builder_immune_preview_character_limit - 3L
      )
      byte_budget <- max(0L, .builder_immune_preview_byte_limit - 3L)
      prefix <- codepoints[seq_len(min(length(codepoints), character_budget))]
      while (
        length(prefix) &&
          nchar(intToUtf8(prefix), type = "bytes") > byte_budget
      ) {
        prefix <- prefix[-length(prefix)]
      }
      paste0(intToUtf8(prefix), "...")
    },
    character(1),
    USE.NAMES = FALSE
  )
  list(value = out, truncated_count = as.integer(sum(truncated)))
}

.builder_immune_preview_info <- function(values) {
  values <- .builder_immune_unique_text(values)
  values <- utils::head(values, .builder_immune_preview_limit)
  .builder_immune_bound_text(values)
}

.builder_immune_unique_summary <- function(values) {
  values <- .builder_immune_unique_text(values)
  count <- length(values)
  shown <- min(count, .builder_immune_preview_limit)
  bounded <- .builder_immune_bound_text(values[seq_len(shown)])
  list(
    count = as.integer(count),
    preview = bounded$value,
    truncated_count = as.integer(
      count - shown + bounded$truncated_count
    )
  )
}

.builder_immune_bounded_frame <- function(value) {
  value <- utils::head(value, .builder_immune_preview_limit)
  truncated_count <- 0L
  for (index in seq_along(value)) {
    column <- .subset2(value, index)
    if (is.factor(column)) {
      column <- as.character(column)
    }
    if (is.character(column)) {
      bounded <- .builder_immune_bound_text(column)
      value[[index]] <- bounded$value
      truncated_count <- truncated_count + bounded$truncated_count
    }
  }
  list(value = value, truncated_count = as.integer(truncated_count))
}

.builder_immune_plain_list <- function(value) {
  identical(typeof(value), "list") &&
    is.null(attr(value, "class", exact = TRUE))
}

.builder_immune_plain_data_frame <- function(value) {
  identical(typeof(value), "list") &&
    identical(attr(value, "class", exact = TRUE), "data.frame")
}

.builder_immune_names_attribute <- function(value) {
  value_names <- attr(value, "names", exact = TRUE)
  if (is.null(value_names)) {
    return(list(ok = TRUE, value = NULL))
  }
  if (
    !identical(typeof(value_names), "character") ||
      !is.null(attr(value_names, "class", exact = TRUE))
  ) {
    return(list(ok = FALSE, value = NULL))
  }
  attributes(value_names) <- NULL
  list(ok = TRUE, value = value_names)
}

.builder_immune_row_names_attribute <- function(value) {
  row_names <- attr(value, "row.names", exact = TRUE)
  if (
    is.null(row_names) ||
      !is.null(attr(row_names, "class", exact = TRUE)) ||
      !typeof(row_names) %in% c("integer", "character")
  ) {
    return(list(ok = FALSE, value = NULL, count = NA_integer_))
  }
  attributes(row_names) <- NULL
  compact <- identical(typeof(row_names), "integer") &&
    length(row_names) == 2L &&
    is.na(row_names[[1L]]) &&
    row_names[[2L]] <= 0L
  count <- if (compact) {
    as.integer(-row_names[[2L]])
  } else {
    as.integer(length(row_names))
  }
  values <- if (compact) {
    as.character(seq_len(count))
  } else {
    as.character(row_names)
  }
  list(ok = TRUE, value = values, count = count)
}

.builder_immune_storage_length <- function(value) {
  if (is.null(value)) {
    return(0L)
  }
  if (!is.null(attr(value, "class", exact = TRUE))) {
    value <- unclass(value)
  }
  length(value)
}

.builder_immune_record <- function(
  detected,
  valid,
  normalized = NULL,
  diagnostics = character(),
  requirements = character(),
  page_candidates = character(),
  ...
) {
  stopifnot(
    is.logical(detected),
    length(detected) == 1L,
    !is.na(detected),
    is.logical(valid),
    length(valid) == 1L,
    !is.na(valid)
  )
  c(
    list(
      detected = detected,
      valid = valid,
      normalized = normalized,
      diagnostics = .builder_immune_unique_text(diagnostics),
      requirements = .builder_immune_unique_text(requirements),
      page_candidates = .builder_immune_unique_text(page_candidates)
    ),
    list(...)
  )
}

.builder_immune_absent_candidate <- function(source_kind) {
  .builder_immune_record(
    detected = FALSE,
    valid = TRUE,
    source_kind = source_kind,
    full_ir_ready = FALSE,
    hla_tcr_ready = FALSE,
    parseable_tcr_chains = character(),
    parseable_tcr_row_count = 0L,
    preview_truncated_count = 0L,
    missing_columns = character(),
    expected_chains = character(),
    unexpected_chains = character(),
    .full_records = character(),
    .motif_records = character(),
    .records = character(),
    .sample_names = character()
  )
}

.builder_immune_text_result <- function(value) {
  value_class <- attr(value, "class", exact = TRUE)
  if (is.null(value_class) && identical(typeof(value), "character")) {
    attributes(value) <- NULL
    return(list(ok = TRUE, value = value, diagnostic = character()))
  }
  if (
    identical(value_class, "factor") ||
      identical(value_class, c("ordered", "factor"))
  ) {
    codes <- unclass(value)
    levels <- attr(value, "levels", exact = TRUE)
    attributes(codes) <- NULL
    if (!identical(typeof(codes), "integer")) {
      return(list(
        ok = FALSE,
        value = NULL,
        diagnostic = "unsafe_factor_codes"
      ))
    }
    if (
      !identical(typeof(levels), "character") ||
        !is.null(attr(levels, "class", exact = TRUE))
    ) {
      return(list(
        ok = FALSE,
        value = NULL,
        diagnostic = "unsafe_factor_levels"
      ))
    }
    attributes(levels) <- NULL
    out <- rep(NA_character_, length(codes))
    usable <- !is.na(codes) & codes >= 1L & codes <= length(levels)
    out[usable] <- levels[codes[usable]]
    return(list(ok = TRUE, value = out, diagnostic = character()))
  }
  list(ok = FALSE, value = NULL, diagnostic = "unsafe_column_class")
}

.builder_immune_text_vector <- function(value) {
  result <- .builder_immune_text_result(value)
  if (isTRUE(result$ok)) result$value else NULL
}

.builder_immune_detect_chains <- function(values) {
  values <- .builder_immune_text_vector(values)
  if (is.null(values)) {
    return(character())
  }
  values <- toupper(values[!is.na(values) & nzchar(values)])
  .builder_immune_chain_names[vapply(
    .builder_immune_chain_names,
    function(chain) {
      pattern <- paste0("(^|[^A-Z0-9])", chain, "[VDJC]")
      any(grepl(pattern, values, perl = TRUE))
    },
    logical(1)
  )]
}

.builder_immune_receptor_types <- function(chains) {
  c(
    if (length(intersect(chains, .builder_immune_tcr_chains))) "TCR",
    if (length(intersect(chains, .builder_immune_bcr_chains))) "BCR"
  )
}

.builder_immune_comparison_records <- function(
  table,
  sample_name,
  columns
) {
  out <- character()
  if (
    !is.character(sample_name) ||
      length(sample_name) != 1L ||
      is.na(sample_name) ||
      !nzchar(sample_name)
  ) {
    return(out)
  }
  for (row in seq_len(nrow(table))) {
    barcode <- table$barcode[[row]]
    if (is.na(barcode) || !nzchar(barcode)) {
      next
    }
    chains <- .builder_immune_detect_chains(table$CTgene[[row]])
    if (!length(chains)) {
      next
    }
    values <- vapply(
      columns,
      function(column) {
        value <- table[[column]][[row]]
        if (is.na(value)) "<NA>" else value
      },
      character(1)
    )
    signature <- paste(c(sample_name, values), collapse = "\u001f")
    keys <- paste(chains, barcode, sep = "\u001f")
    records <- rep(signature, length(keys))
    names(records) <- keys
    out <- c(out, records)
  }
  out
}

.builder_immune_candidate_from_tables <- function(
  payload,
  cells,
  source_kind,
  expected_chains = character()
) {
  requirements <- c(
    "named_sample_tables",
    paste(.builder_immune_required_columns, collapse = ","),
    "dataset_barcode_overlap",
    "recognized_receptor_chain",
    "parseable_tcr_segments_for_hla"
  )
  detected <- .builder_immune_storage_length(payload) > 0L
  if (!detected) {
    out <- .builder_immune_absent_candidate(source_kind)
    out$requirements <- requirements
    out$expected_chains <- expected_chains
    return(out)
  }

  diagnostics <- character()
  missing_columns <- character()
  if (!.builder_immune_plain_list(payload)) {
    return(.builder_immune_record(
      detected = TRUE,
      valid = FALSE,
      diagnostics = if (identical(typeof(payload), "list")) {
        "unsafe_container_class"
      } else {
        "invalid_container"
      },
      requirements = requirements,
      source_kind = source_kind,
      full_ir_ready = FALSE,
      hla_tcr_ready = FALSE,
      parseable_tcr_chains = character(),
      parseable_tcr_row_count = 0L,
      preview_truncated_count = 0L,
      missing_columns = .builder_immune_required_columns,
      expected_chains = expected_chains,
      unexpected_chains = character(),
      .full_records = character(),
      .motif_records = character(),
      .records = character(),
      .sample_names = character()
    ))
  }

  sample_name_attribute <- .builder_immune_names_attribute(payload)
  sample_names <- sample_name_attribute$value
  if (!sample_name_attribute$ok) {
    diagnostics <- c(diagnostics, "unsafe_sample_names")
    sample_names <- rep("", length(payload))
  } else if (is.null(sample_names) || length(sample_names) != length(payload)) {
    diagnostics <- c(diagnostics, "missing_sample_names")
    sample_names <- rep("", length(payload))
  }
  if (anyNA(sample_names) || any(!nzchar(sample_names))) {
    diagnostics <- c(diagnostics, "blank_sample_names")
  }
  nonblank_samples <- sample_names[!is.na(sample_names) & nzchar(sample_names)]
  if (anyDuplicated(nonblank_samples)) {
    diagnostics <- c(diagnostics, "duplicate_sample_names")
  }

  all_barcodes <- character()
  all_chains <- character()
  full_records <- character()
  motif_records <- character()
  motif_payload <- list()
  n_rows <- 0L

  for (index in seq_along(payload)) {
    table <- .subset2(payload, index)
    if (!.builder_immune_plain_data_frame(table)) {
      diagnostics <- c(
        diagnostics,
        if (identical(typeof(table), "list")) {
          "unsafe_sample_table_class"
        } else {
          "invalid_sample_table"
        }
      )
      next
    }
    column_name_attribute <- .builder_immune_names_attribute(table)
    row_name_attribute <- .builder_immune_row_names_attribute(table)
    if (!column_name_attribute$ok) {
      diagnostics <- c(diagnostics, "unsafe_column_names")
      next
    }
    if (!row_name_attribute$ok) {
      diagnostics <- c(diagnostics, "unsafe_row_names")
      next
    }
    column_names <- column_name_attribute$value
    if (
      is.null(column_names) ||
        anyNA(column_names) ||
        any(!nzchar(column_names)) ||
        anyDuplicated(column_names)
    ) {
      diagnostics <- c(diagnostics, "invalid_column_names")
      next
    }
    table_rows <- row_name_attribute$count
    if (!table_rows) {
      diagnostics <- c(diagnostics, "empty_sample_table")
      next
    }
    missing <- setdiff(.builder_immune_required_columns, column_names)
    if (length(missing)) {
      diagnostics <- c(diagnostics, "missing_required_columns")
      missing_columns <- c(missing_columns, missing)
    }

    available <- intersect(.builder_immune_required_columns, column_names)
    normalized <- vector("list", length(available))
    names(normalized) <- available
    valid_columns <- TRUE
    for (column in available) {
      result <- .builder_immune_text_result(.subset2(table, column))
      if (!result$ok) {
        diagnostics <- c(diagnostics, result$diagnostic)
        valid_columns <- FALSE
        break
      }
      if (length(result$value) != table_rows) {
        diagnostics <- c(diagnostics, "invalid_text_column_length")
        valid_columns <- FALSE
        break
      }
      normalized[[column]] <- result$value
    }
    if (!valid_columns) {
      next
    }
    if ("barcode" %in% names(normalized)) {
      barcodes <- normalized$barcode
      if (anyNA(barcodes) || any(!nzchar(barcodes))) {
        diagnostics <- c(diagnostics, "blank_barcodes")
      }
      usable_barcodes <- barcodes[!is.na(barcodes) & nzchar(barcodes)]
      if (anyDuplicated(usable_barcodes)) {
        diagnostics <- c(diagnostics, "duplicate_barcodes")
      }
      all_barcodes <- c(all_barcodes, usable_barcodes)
    } else {
      barcodes <- rep(NA_character_, table_rows)
      usable_barcodes <- character()
    }
    if ("CTgene" %in% names(normalized)) {
      all_chains <- c(
        all_chains,
        .builder_immune_detect_chains(normalized$CTgene)
      )
    }

    motif_columns <- c("barcode", "CTgene", "CTaa")
    if (
      all(motif_columns %in% names(normalized)) &&
        !is.na(sample_names[[index]]) &&
        nzchar(sample_names[[index]])
    ) {
      motif_table <- as.data.frame(
        normalized[motif_columns],
        stringsAsFactors = FALSE
      )
      motif_payload[[length(motif_payload) + 1L]] <- motif_table
      names(motif_payload)[[length(motif_payload)]] <- sample_names[[index]]
      motif_records <- c(
        motif_records,
        .builder_immune_comparison_records(
          motif_table,
          sample_names[[index]],
          motif_columns
        )
      )
    }

    if (length(missing)) {
      next
    }
    normalized <- as.data.frame(
      normalized[.builder_immune_required_columns],
      stringsAsFactors = FALSE
    )
    empty_columns <- .builder_immune_required_columns[-1L][vapply(
      normalized[.builder_immune_required_columns[-1L]],
      function(value) !any(!is.na(value) & nzchar(value)),
      logical(1)
    )]
    if (length(empty_columns)) {
      diagnostics <- c(diagnostics, "empty_required_column")
    }
    n_rows <- n_rows + nrow(normalized)
    full_records <- c(
      full_records,
      .builder_immune_comparison_records(
        normalized,
        sample_names[[index]],
        .builder_immune_required_columns
      )
    )
  }

  if (anyDuplicated(all_barcodes)) {
    diagnostics <- c(diagnostics, "duplicate_barcodes")
  }
  cells <- as.character(cells)
  outside <- setdiff(unique(all_barcodes), unique(cells))
  overlap <- intersect(unique(all_barcodes), unique(cells))
  if (length(outside)) {
    diagnostics <- c(diagnostics, "barcodes_outside_dataset")
  }
  if (!length(overlap)) {
    diagnostics <- c(diagnostics, "no_dataset_barcode_overlap")
  }
  all_chains <- unique(all_chains)
  if (!length(all_chains)) {
    diagnostics <- c(diagnostics, "unrecognized_chain")
  }
  unexpected_chains <- setdiff(all_chains, expected_chains)
  if (length(expected_chains)) {
    if (!length(intersect(all_chains, expected_chains))) {
      diagnostics <- c(diagnostics, "chain_family_mismatch")
    }
    if (length(unexpected_chains)) {
      diagnostics <- c(diagnostics, "unexpected_chain_family")
    }
  } else {
    unexpected_chains <- character()
  }

  parseable_tcr_chains <- character()
  parseable_tcr_row_count <- 0L
  for (chain in intersect(c("TRA", "TRB"), all_chains)) {
    segments <- tryCatch(
      hla_parse_ir_segments(motif_payload, chain),
      error = function(error) NULL
    )
    if (!is.null(segments) && nrow(segments)) {
      parseable_tcr_chains <- c(parseable_tcr_chains, chain)
      parseable_tcr_row_count <- parseable_tcr_row_count + nrow(segments)
    }
  }

  shared_invalid_reasons <- c(
    "invalid_container",
    "unsafe_container_class",
    "unsafe_sample_names",
    "missing_sample_names",
    "blank_sample_names",
    "duplicate_sample_names",
    "invalid_sample_table",
    "unsafe_sample_table_class",
    "unsafe_column_names",
    "unsafe_row_names",
    "invalid_column_names",
    "invalid_text_column_length",
    "unsafe_column_class",
    "unsafe_factor_codes",
    "unsafe_factor_levels",
    "blank_barcodes",
    "duplicate_barcodes",
    "no_dataset_barcode_overlap",
    "chain_family_mismatch",
    "unexpected_chain_family"
  )
  full_invalid_reasons <- c(
    shared_invalid_reasons,
    "missing_required_columns",
    "empty_required_column",
    "unrecognized_chain"
  )
  full_ir_ready <- !length(intersect(
    unique(diagnostics),
    full_invalid_reasons
  ))
  hla_tcr_ready <- length(parseable_tcr_chains) > 0L &&
    !length(intersect(unique(diagnostics), shared_invalid_reasons))
  denominator <- length(unique(all_barcodes))
  overlap_fraction <- if (denominator) length(overlap) / denominator else 0
  full_records <- full_records[!duplicated(names(full_records))]
  motif_records <- motif_records[!duplicated(names(motif_records))]
  sample_preview <- .builder_immune_preview_info(nonblank_samples)
  barcode_preview <- .builder_immune_preview_info(all_barcodes)
  outside_preview <- .builder_immune_preview_info(outside)
  preview_truncated_count <- sum(c(
    sample_preview$truncated_count,
    barcode_preview$truncated_count,
    outside_preview$truncated_count
  ))
  page_candidates <- c(
    if (full_ir_ready) "immune_repertoire",
    if (hla_tcr_ready) "hla_tcr_motifs"
  )

  .builder_immune_record(
    detected = TRUE,
    valid = full_ir_ready,
    normalized = list(
      n_samples = as.integer(length(unique(nonblank_samples))),
      n_rows = as.integer(n_rows),
      barcode_count = as.integer(length(unique(all_barcodes))),
      dataset_overlap_count = as.integer(length(overlap)),
      dataset_overlap_fraction = overlap_fraction,
      sample_names = sample_preview$value,
      sample_count = as.integer(length(unique(nonblank_samples))),
      chains = all_chains,
      receptor_types = .builder_immune_receptor_types(all_chains),
      columns = .builder_immune_required_columns,
      barcode_preview = barcode_preview$value,
      outside_barcode_count = as.integer(length(outside)),
      outside_barcode_preview = outside_preview$value
    ),
    diagnostics = diagnostics,
    requirements = requirements,
    page_candidates = page_candidates,
    attention = any(
      c(
        "empty_sample_table",
        "barcodes_outside_dataset"
      ) %in%
        diagnostics
    ),
    source_kind = source_kind,
    full_ir_ready = full_ir_ready,
    hla_tcr_ready = hla_tcr_ready,
    parseable_tcr_chains = parseable_tcr_chains,
    parseable_tcr_row_count = as.integer(parseable_tcr_row_count),
    preview_truncated_count = as.integer(preview_truncated_count),
    missing_columns = unique(missing_columns),
    expected_chains = expected_chains,
    unexpected_chains = unexpected_chains,
    .full_records = full_records,
    .motif_records = motif_records,
    .records = full_records,
    .sample_names = unique(nonblank_samples)
  )
}

.builder_immune_metadata_candidate <- function(metadata, cells) {
  source_kind <- "metadata"
  requirements <- c(
    "metadata_rows_match_dataset_cells",
    paste(.builder_immune_required_columns[-1L], collapse = ","),
    "sample_key_or_default",
    "recognized_receptor_chain"
  )
  if (!.builder_immune_plain_data_frame(metadata)) {
    out <- .builder_immune_absent_candidate(source_kind)
    out$requirements <- requirements
    return(out)
  }
  core <- .builder_immune_required_columns[-1L]
  invalid_candidate <- function(
    diagnostics,
    missing_columns = core,
    sample_columns = character(),
    sample_column = NULL
  ) {
    out <- .builder_immune_absent_candidate(source_kind)
    out$detected <- TRUE
    out$valid <- FALSE
    out$diagnostics <- .builder_immune_unique_text(diagnostics)
    out$requirements <- requirements
    out$missing_columns <- missing_columns
    out$sample_columns <- sample_columns
    out$sample_column <- sample_column
    out
  }
  column_name_attribute <- .builder_immune_names_attribute(metadata)
  if (!column_name_attribute$ok) {
    return(invalid_candidate("unsafe_column_names"))
  }
  metadata_columns <- column_name_attribute$value
  if (
    is.null(metadata_columns) ||
      anyNA(metadata_columns) ||
      any(!nzchar(metadata_columns)) ||
      anyDuplicated(metadata_columns)
  ) {
    return(invalid_candidate("invalid_column_names"))
  }
  present <- intersect(core, metadata_columns)
  if (!length(present)) {
    out <- .builder_immune_absent_candidate(source_kind)
    out$requirements <- requirements
    return(out)
  }

  row_name_attribute <- .builder_immune_row_names_attribute(metadata)
  if (!row_name_attribute$ok) {
    return(invalid_candidate("unsafe_row_names"))
  }
  metadata_rows <- row_name_attribute$count
  if (metadata_rows != length(cells)) {
    return(invalid_candidate("metadata_row_count_mismatch"))
  }

  missing <- setdiff(core, metadata_columns)
  sample_columns <- intersect(
    c("orig.ident", "sample", "Sample"),
    metadata_columns
  )
  sample_column <- if (length(sample_columns)) sample_columns[[1L]] else NULL
  if (is.null(sample_column)) {
    sample_ids <- rep("Sample_1", metadata_rows)
  } else {
    sample_result <- .builder_immune_text_result(
      .subset2(metadata, sample_column)
    )
    if (!sample_result$ok || length(sample_result$value) != metadata_rows) {
      return(invalid_candidate(
        if (sample_result$ok) {
          "invalid_text_column_length"
        } else {
          sample_result$diagnostic
        },
        missing_columns = missing,
        sample_columns = sample_columns,
        sample_column = sample_column
      ))
    }
    sample_ids <- sample_result$value
  }
  if (anyNA(sample_ids) || any(!nzchar(sample_ids))) {
    return(invalid_candidate(
      "blank_sample_names",
      missing_columns = missing,
      sample_columns = sample_columns,
      sample_column = sample_column
    ))
  }
  barcodes <- row_name_attribute$value
  safe_core <- vector("list", length(present))
  names(safe_core) <- present
  for (column in present) {
    result <- .builder_immune_text_result(.subset2(metadata, column))
    if (!result$ok || length(result$value) != metadata_rows) {
      return(invalid_candidate(
        if (result$ok) {
          "invalid_text_column_length"
        } else {
          result$diagnostic
        },
        missing_columns = missing,
        sample_columns = sample_columns,
        sample_column = sample_column
      ))
    }
    safe_core[[column]] <- result$value
  }
  if ("CTgene" %in% metadata_columns) {
    primary <- safe_core[["CTgene"]]
    keep <- !is.na(primary) & nzchar(primary)
  } else {
    keep <- rep(TRUE, metadata_rows)
  }
  indices <- which(keep)
  if (!length(indices)) {
    return(.builder_immune_record(
      detected = TRUE,
      valid = FALSE,
      diagnostics = c(
        if (length(missing)) "missing_required_columns",
        "no_repertoire_rows"
      ),
      requirements = requirements,
      source_kind = source_kind,
      missing_columns = missing,
      full_ir_ready = FALSE,
      hla_tcr_ready = FALSE,
      parseable_tcr_chains = character(),
      parseable_tcr_row_count = 0L,
      preview_truncated_count = 0L,
      expected_chains = character(),
      unexpected_chains = character(),
      sample_columns = sample_columns,
      sample_column = sample_column,
      .full_records = character(),
      .motif_records = character(),
      .records = character(),
      .sample_names = character()
    ))
  }

  keys <- sample_ids[indices]
  unique_keys <- unique(keys)
  payload <- lapply(unique_keys, function(key) {
    selected <- indices[!is.na(keys) & keys == key]
    table <- data.frame(
      barcode = barcodes[selected],
      stringsAsFactors = FALSE
    )
    for (column in intersect(core, metadata_columns)) {
      value <- safe_core[[column]]
      table[[column]] <- value[selected]
    }
    table
  })
  names(payload) <- unique_keys
  profile <- .builder_immune_candidate_from_tables(
    payload,
    cells,
    source_kind = source_kind
  )
  profile$detected <- TRUE
  profile$requirements <- .builder_immune_unique_text(requirements)
  profile$missing_columns <- unique(c(profile$missing_columns, missing))
  if (length(missing)) {
    profile$diagnostics <- .builder_immune_unique_text(c(
      profile$diagnostics,
      "missing_required_columns"
    ))
    profile$valid <- FALSE
    profile$full_ir_ready <- FALSE
    profile$page_candidates <- if (isTRUE(profile$hla_tcr_ready)) {
      "hla_tcr_motifs"
    } else {
      character()
    }
  }
  if (!is.null(profile$normalized)) {
    profile$normalized$barcode_origin <- "metadata_rownames"
    profile$normalized$sample_column <- sample_column
    profile$normalized$sample_columns <- sample_columns
  }
  profile$sample_columns <- sample_columns
  profile$sample_column <- sample_column
  profile
}

.builder_immune_pair_overlaps <- function(
  candidates,
  record_field = ".records",
  capability = "full_ir"
) {
  active <- names(candidates)[vapply(
    candidates,
    function(candidate) {
      isTRUE(candidate$detected) &&
        length(candidate[[record_field]]) > 0L
    },
    logical(1)
  )]
  if (length(active) < 2L) {
    return(list())
  }
  pairs <- utils::combn(active, 2L, simplify = FALSE)
  lapply(pairs, function(pair) {
    left <- candidates[[pair[[1L]]]][[record_field]]
    right <- candidates[[pair[[2L]]]][[record_field]]
    overlap <- intersect(names(left), names(right))
    exact <- overlap[left[overlap] == right[overlap]]
    divergent <- setdiff(overlap, exact)
    preview_barcodes <- function(keys) {
      sub("^.*\u001f", "", keys)
    }
    overlap_preview <- .builder_immune_preview_info(
      preview_barcodes(overlap)
    )
    divergent_preview <- .builder_immune_preview_info(
      preview_barcodes(divergent)
    )
    list(
      left = pair[[1L]],
      right = pair[[2L]],
      capability = capability,
      n_left = as.integer(length(left)),
      n_right = as.integer(length(right)),
      n_overlap = as.integer(length(overlap)),
      n_exact = as.integer(length(exact)),
      n_divergent = as.integer(length(divergent)),
      equivalent = length(divergent) == 0L &&
        length(exact) == length(left) &&
        length(exact) == length(right),
      overlap_preview = overlap_preview$value,
      divergent_preview = divergent_preview$value,
      preview_truncated_count = as.integer(
        overlap_preview$truncated_count +
          divergent_preview$truncated_count
      )
    )
  })
}

.builder_immune_profile_repertoire <- function(misc, context) {
  cells <- context$cells
  unified <- .builder_immune_candidate_from_tables(
    .subset2(misc, "immune_repertoire"),
    cells,
    source_kind = "unified_misc"
  )
  metadata <- .builder_immune_metadata_candidate(context$metadata, cells)
  legacy_bcr <- .builder_immune_candidate_from_tables(
    .subset2(misc, "bcr_data"),
    cells,
    source_kind = "legacy_bcr",
    expected_chains = .builder_immune_bcr_chains
  )
  legacy_tcr <- .builder_immune_candidate_from_tables(
    .subset2(misc, "tcr_data"),
    cells,
    source_kind = "legacy_tcr",
    expected_chains = .builder_immune_tcr_chains
  )
  candidates <- list(
    unified_misc = unified,
    metadata = metadata,
    legacy_bcr = legacy_bcr,
    legacy_tcr = legacy_tcr
  )
  full_source_overlaps <- .builder_immune_pair_overlaps(
    candidates,
    ".full_records",
    "full_ir"
  )
  motif_source_overlaps <- .builder_immune_pair_overlaps(
    candidates,
    ".motif_records",
    "hla_tcr_motifs"
  )
  detected <- any(vapply(candidates, `[[`, logical(1), "detected"))
  valid_sources <- names(candidates)[vapply(
    candidates,
    function(candidate) {
      isTRUE(candidate$detected) && isTRUE(candidate$full_ir_ready)
    },
    logical(1)
  )]
  tcr_sources <- names(candidates)[vapply(
    candidates,
    function(candidate) {
      isTRUE(candidate$detected) && isTRUE(candidate$hla_tcr_ready)
    },
    logical(1)
  )]
  valid <- !detected || length(valid_sources) > 0L
  content_sources <- union(valid_sources, tcr_sources)
  chains <- unique(unlist(lapply(
    candidates[content_sources],
    function(candidate) {
      candidate$normalized$chains
    }
  )))
  receptor_types <- .builder_immune_receptor_types(chains)
  parseable_tcr_chains <- unique(unlist(lapply(
    candidates[tcr_sources],
    `[[`,
    "parseable_tcr_chains"
  )))
  parseable_tcr_row_count <- sum(vapply(
    candidates[tcr_sources],
    function(candidate) candidate$parseable_tcr_row_count,
    integer(1)
  ))
  diagnostics <- unlist(
    lapply(names(candidates), function(name) {
      values <- candidates[[name]]$diagnostics
      if (!length(values)) {
        return(character())
      }
      paste0(name, ":", values)
    }),
    use.names = FALSE
  )
  page_candidates <- c(
    if (length(valid_sources)) "immune_repertoire",
    if (length(tcr_sources)) "hla_tcr_motifs"
  )
  normalized <- if (length(valid_sources) || length(tcr_sources)) {
    list(
      available_sources = valid_sources,
      available_tcr_sources = tcr_sources,
      chains = chains,
      receptor_types = receptor_types,
      n_sources = as.integer(length(valid_sources)),
      parseable_tcr_chains = parseable_tcr_chains,
      parseable_tcr_row_count = as.integer(parseable_tcr_row_count)
    )
  } else {
    NULL
  }
  preview_truncated_count <- sum(vapply(
    candidates,
    function(candidate) candidate$preview_truncated_count,
    integer(1)
  )) +
    sum(vapply(
      c(full_source_overlaps, motif_source_overlaps),
      function(overlap) overlap$preview_truncated_count,
      integer(1)
    ))
  out <- .builder_immune_record(
    detected = detected,
    valid = valid,
    normalized = normalized,
    diagnostics = diagnostics,
    requirements = c(
      "one_valid_source",
      "source_priority_is_deferred_to_build_plan"
    ),
    page_candidates = page_candidates,
    candidates = candidates,
    source_overlaps = full_source_overlaps,
    full_source_overlaps = full_source_overlaps,
    motif_source_overlaps = motif_source_overlaps,
    available_sources = valid_sources,
    available_tcr_sources = tcr_sources,
    parseable_tcr_chains = parseable_tcr_chains,
    parseable_tcr_row_count = as.integer(parseable_tcr_row_count),
    preview_truncated_count = as.integer(preview_truncated_count),
    selected_source = NULL
  )
  out
}

.builder_hla_shape <- function(value) {
  if (.builder_immune_plain_data_frame(value)) {
    columns <- attr(value, "names", exact = TRUE)
    if (all(HLA_TYPING_COLUMNS %in% columns)) {
      return("canonical_long")
    }
    if (all(c("sample", "allele") %in% columns)) {
      return("long")
    }
    sample_columns <- intersect(
      c("sample", "sample_ID", "patient_id"),
      columns
    )
    hla_columns <- grep("^HLA-", columns, value = TRUE, ignore.case = TRUE)
    if (length(sample_columns) && length(hla_columns)) {
      return("wide")
    }
    return("invalid")
  }
  if (.builder_immune_plain_list(value)) {
    value_names <- attr(value, "names", exact = TRUE)
    names_valid <- !is.null(value_names) &&
      length(value_names) == length(value) &&
      !anyNA(value_names) &&
      all(nzchar(value_names)) &&
      !anyDuplicated(value_names)
    if (names_valid) {
      return("named_list")
    }
  }
  "invalid"
}

.builder_hla_source_type <- function(misc) {
  value <- .subset2(misc, "hla_typing_source_type")
  if (is.null(value)) {
    return(list(value = "unknown", diagnostic = character()))
  }
  value <- .builder_immune_text_vector(value)
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value) ||
      !value %in% HLA_SOURCE_TYPES
  ) {
    return(list(value = "unknown", diagnostic = "invalid_source_type"))
  }
  list(value = unname(value), diagnostic = character())
}

.builder_hla_safe_value <- function(value) {
  if (.builder_immune_plain_list(value)) {
    value_name_attribute <- .builder_immune_names_attribute(value)
    if (!value_name_attribute$ok) {
      return(list(ok = FALSE, diagnostic = "unsafe_sample_names"))
    }
    value_names <- value_name_attribute$value
    out <- vector("list", length(value))
    for (index in seq_along(value)) {
      item <- .builder_immune_text_result(.subset2(value, index))
      if (!item$ok) {
        return(list(ok = FALSE, diagnostic = item$diagnostic))
      }
      out[[index]] <- item$value
    }
    names(out) <- value_names
    return(list(ok = TRUE, value = out, diagnostic = character()))
  }
  if (!.builder_immune_plain_data_frame(value)) {
    return(list(ok = FALSE, diagnostic = "unsafe_container_class"))
  }

  column_name_attribute <- .builder_immune_names_attribute(value)
  if (!column_name_attribute$ok) {
    return(list(ok = FALSE, diagnostic = "unsafe_column_names"))
  }
  columns <- column_name_attribute$value
  if (
    is.null(columns) ||
      anyNA(columns) ||
      any(!nzchar(columns)) ||
      anyDuplicated(columns)
  ) {
    return(list(ok = FALSE, diagnostic = "invalid_column_names"))
  }
  row_name_attribute <- .builder_immune_row_names_attribute(value)
  if (!row_name_attribute$ok) {
    return(list(ok = FALSE, diagnostic = "unsafe_row_names"))
  }
  row_count <- row_name_attribute$count
  safe_columns <- vector("list", length(columns))
  for (index in seq_along(columns)) {
    column <- .subset2(value, index)
    column_class <- attr(column, "class", exact = TRUE)
    if (
      identical(column_class, "factor") ||
        identical(column_class, c("ordered", "factor"))
    ) {
      result <- .builder_immune_text_result(column)
      if (!result$ok) {
        return(list(ok = FALSE, diagnostic = result$diagnostic))
      }
      column <- result$value
    } else if (!is.null(column_class) || !is.atomic(column)) {
      return(list(ok = FALSE, diagnostic = "unsafe_column_class"))
    } else {
      attributes(column) <- NULL
    }
    if (length(column) != row_count) {
      return(list(ok = FALSE, diagnostic = "invalid_column_length"))
    }
    safe_columns[[index]] <- column
  }
  names(safe_columns) <- columns
  safe <- structure(
    safe_columns,
    class = "data.frame",
    row.names = seq_len(row_count)
  )
  list(ok = TRUE, value = safe, diagnostic = character())
}

.builder_hla_empty_qc <- function() {
  data.frame(
    sample = character(),
    value = character(),
    issue = character(),
    stringsAsFactors = FALSE
  )
}

.builder_hla_empty_provenance <- function() {
  list(
    source_types = character(),
    has_unknown = FALSE,
    typing_method_preview = character(),
    source_reference_preview = character(),
    confidence_provided = FALSE,
    association_eligible = FALSE,
    descriptive_only = TRUE
  )
}

.builder_hla_empty_record <- function(..., page_gate) {
  .builder_immune_record(
    ...,
    qc = .builder_hla_empty_qc(),
    qc_count = 0L,
    provenance = .builder_hla_empty_provenance(),
    attention = FALSE,
    page_gate = page_gate,
    unit_mappings = list(),
    preview_truncated_count = 0L
  )
}

.builder_hla_add_qc <- function(qc, sample, value, issue) {
  rbind(
    qc,
    data.frame(
      sample = as.character(sample),
      value = as.character(value),
      issue = as.character(issue),
      stringsAsFactors = FALSE
    )
  )
}

.builder_hla_unit_mappings <- function(table, ir_candidates) {
  if (!nrow(table)) {
    return(list())
  }
  hla_samples <- unique(table$sample[
    !is.na(table$sample) & nzchar(table$sample)
  ])
  active <- names(ir_candidates)[vapply(
    ir_candidates,
    function(candidate) {
      isTRUE(candidate$detected) && isTRUE(candidate$hla_tcr_ready)
    },
    logical(1)
  )]
  out <- lapply(active, function(name) {
    ir_samples <- .builder_immune_unique_text(
      ir_candidates[[name]]$.sample_names
    )
    matched_samples <- intersect(ir_samples, hla_samples)
    unmatched_hla <- setdiff(hla_samples, ir_samples)
    unmatched_ir <- setdiff(ir_samples, hla_samples)
    unit_map <- hla_analysis_unit_map(table, ir_samples)
    unit_types <- unique(unit_map$unit_type)
    unit_type <- if (length(unit_types)) unit_types[[1L]] else "sample"
    matched_preview <- .builder_immune_preview_info(matched_samples)
    unmatched_hla_preview <- .builder_immune_preview_info(unmatched_hla)
    unmatched_ir_preview <- .builder_immune_preview_info(unmatched_ir)
    list(
      source = name,
      ir_sample_count = as.integer(length(ir_samples)),
      hla_sample_count = as.integer(length(hla_samples)),
      matched_sample_count = as.integer(length(matched_samples)),
      unmatched_hla_count = as.integer(length(unmatched_hla)),
      unmatched_ir_count = as.integer(length(unmatched_ir)),
      donor_collapse_eligible = identical(unit_type, "donor"),
      analysis_unit_type = unit_type,
      analysis_unit_count = as.integer(length(unique(unit_map$analysis_unit))),
      matched_preview = matched_preview$value,
      unmatched_hla_preview = unmatched_hla_preview$value,
      unmatched_ir_preview = unmatched_ir_preview$value,
      preview_truncated_count = as.integer(sum(c(
        matched_preview$truncated_count,
        unmatched_hla_preview$truncated_count,
        unmatched_ir_preview$truncated_count
      )))
    )
  })
  names(out) <- active
  out
}

.builder_immune_profile_hla <- function(misc, ir_candidates) {
  value <- .subset2(misc, "hla_typing")
  detected <- .builder_immune_storage_length(value) > 0L
  requirements <- c(
    "accepted_hla_shape",
    "canonical_hla_alleles",
    "explicit_provenance_for_association"
  )
  page_gate <- list(
    role = "supporting",
    opens = FALSE,
    requires = "valid_tcr"
  )
  if (!detected) {
    return(.builder_hla_empty_record(
      detected = FALSE,
      valid = TRUE,
      requirements = requirements,
      shape = "absent",
      page_gate = page_gate
    ))
  }

  safe_value <- .builder_hla_safe_value(value)
  if (!isTRUE(safe_value$ok)) {
    return(.builder_hla_empty_record(
      detected = TRUE,
      valid = FALSE,
      diagnostics = safe_value$diagnostic,
      requirements = requirements,
      shape = "invalid",
      page_gate = page_gate
    ))
  }
  value <- safe_value$value
  shape <- .builder_hla_shape(value)
  if (identical(shape, "invalid")) {
    return(.builder_hla_empty_record(
      detected = TRUE,
      valid = FALSE,
      diagnostics = "invalid_shape",
      requirements = requirements,
      shape = shape,
      page_gate = page_gate
    ))
  }

  source_type <- .builder_hla_source_type(misc)
  diagnostics <- source_type$diagnostic
  qc <- .builder_hla_empty_qc()
  normalized <- tryCatch(
    {
      if (identical(shape, "canonical_long")) {
        before <- nrow(value)
        out <- hla_validate_typing(value)
        removed <- before - nrow(out)
        if (removed > 0L) {
          qc <- .builder_hla_add_qc(
            qc,
            NA_character_,
            as.character(removed),
            "invalid canonical HLA rows were removed"
          )
          diagnostics <- c(diagnostics, "invalid_rows_removed")
        }
        out
      } else {
        out <- hla_normalize_typing(
          value,
          source_type = source_type$value
        )
        got_qc <- attr(out, "qc", exact = TRUE)
        if (is.data.frame(got_qc) && nrow(got_qc)) {
          qc <- got_qc
        }
        out
      }
    },
    error = function(error) NULL
  )
  if (is.null(normalized)) {
    qc_preview <- .builder_immune_bounded_frame(qc)
    return(.builder_immune_record(
      detected = TRUE,
      valid = FALSE,
      diagnostics = c(diagnostics, "normalization_failed"),
      requirements = requirements,
      shape = shape,
      qc = qc_preview$value,
      qc_count = as.integer(nrow(qc)),
      provenance = .builder_hla_empty_provenance(),
      attention = FALSE,
      page_gate = page_gate,
      unit_mappings = list(),
      preview_truncated_count = qc_preview$truncated_count
    ))
  }

  sample_blanks <- is.na(normalized$sample) | !nzchar(normalized$sample)
  if (any(sample_blanks)) {
    diagnostics <- c(diagnostics, "blank_hla_samples")
  }
  source_types <- .builder_immune_unique_text(normalized$source_type)
  has_unknown <- "unknown" %in% source_types || !length(source_types)
  if (has_unknown) {
    diagnostics <- c(diagnostics, "unknown_provenance")
  }
  invalid_copy <- is.na(normalized$copy)
  if (any(invalid_copy)) {
    diagnostics <- c(diagnostics, "ambiguous_copy")
  }
  key <- paste(normalized$sample, normalized$locus, normalized$copy, sep = "\r")
  if (anyDuplicated(key[!is.na(normalized$copy)])) {
    diagnostics <- c(diagnostics, "duplicate_hla_copy")
  }
  valid <- nrow(normalized) > 0L &&
    !any(sample_blanks) &&
    !"duplicate_hla_copy" %in% diagnostics
  if (!nrow(normalized)) {
    diagnostics <- c(diagnostics, "no_valid_alleles")
  }
  attention <- valid &&
    any(
      c(
        "unknown_provenance",
        "ambiguous_copy",
        "invalid_rows_removed",
        "invalid_source_type"
      ) %in%
        diagnostics
    )

  sample_values <- normalized$sample[
    !is.na(normalized$sample) & nzchar(normalized$sample)
  ]
  donor_values <- normalized$donor_id[
    !is.na(normalized$donor_id) & nzchar(normalized$donor_id)
  ]
  preview <- normalized
  attr(preview, "qc") <- NULL
  table_preview <- .builder_immune_bounded_frame(preview)
  qc_preview <- .builder_immune_bounded_frame(qc)
  typing_methods <- .builder_immune_unique_text(normalized$typing_method)
  source_references <- .builder_immune_unique_text(normalized$source_reference)
  confidence <- suppressWarnings(as.numeric(normalized$confidence))
  sample_preview <- .builder_immune_preview_info(sample_values)
  donor_preview <- .builder_immune_preview_info(donor_values)
  allele_preview <- .builder_immune_preview_info(normalized$allele)
  typing_method_preview <- .builder_immune_preview_info(typing_methods)
  source_reference_preview <- .builder_immune_preview_info(source_references)
  locus_summary <- .builder_immune_unique_summary(normalized$locus)
  normalized_summary <- list(
    n_rows = as.integer(nrow(normalized)),
    n_samples = as.integer(length(unique(sample_values))),
    sample_preview = sample_preview$value,
    n_donors = as.integer(length(unique(donor_values))),
    donor_preview = donor_preview$value,
    n_loci = locus_summary$count,
    locus_preview = locus_summary$preview,
    loci_truncated_count = locus_summary$truncated_count,
    n_alleles = as.integer(length(unique(normalized$allele))),
    allele_preview = allele_preview$value,
    columns = HLA_TYPING_COLUMNS,
    table_preview = table_preview$value
  )
  provenance <- list(
    source_types = source_types,
    has_unknown = has_unknown,
    typing_method_preview = typing_method_preview$value,
    source_reference_preview = source_reference_preview$value,
    confidence_provided = any(is.finite(confidence)),
    association_eligible = length(source_types) > 0L &&
      all(source_types == "genotyped"),
    descriptive_only = !(length(source_types) > 0L &&
      all(source_types == "genotyped"))
  )
  unit_mappings <- .builder_hla_unit_mappings(normalized, ir_candidates)
  preview_truncated_count <- sum(c(
    sample_preview$truncated_count,
    donor_preview$truncated_count,
    allele_preview$truncated_count,
    table_preview$truncated_count,
    qc_preview$truncated_count,
    typing_method_preview$truncated_count,
    source_reference_preview$truncated_count,
    locus_summary$truncated_count,
    vapply(
      unit_mappings,
      function(mapping) mapping$preview_truncated_count,
      integer(1)
    )
  ))

  .builder_immune_record(
    detected = TRUE,
    valid = valid,
    normalized = normalized_summary,
    diagnostics = diagnostics,
    requirements = requirements,
    page_candidates = character(),
    shape = shape,
    qc = qc_preview$value,
    qc_count = as.integer(nrow(qc)),
    provenance = provenance,
    attention = attention,
    page_gate = page_gate,
    unit_mappings = unit_mappings,
    preview_truncated_count = as.integer(preview_truncated_count),
    .sample_names = unique(sample_values)
  )
}

.builder_immune_strip_private <- function(record) {
  record$.full_records <- NULL
  record$.motif_records <- NULL
  record$.records <- NULL
  record$.sample_names <- NULL
  record
}

#' Profile immune-repertoire and HLA content without choosing a source.
#'
#' @param object The live Seurat object held by the Builder worker.
#' @param context Stable structural facts from `builder_dataset_profile()`.
#' @return Plain, bounded source records for BuildPlan compilation.
#' @keywords internal
builder_profile_immune_content <- function(object, context) {
  if (!is.list(context) || is.null(context$cells)) {
    stop("Immune content profiling requires dataset cell identities.")
  }
  misc <- tryCatch(methods::slot(object, "misc"), error = function(error) {
    list()
  })
  if (!identical(typeof(misc), "list")) {
    misc <- list()
  } else if (!is.null(attr(misc, "class", exact = TRUE))) {
    misc <- unclass(misc)
  }
  if (is.null(context$metadata)) {
    context$metadata <- tryCatch(
      methods::slot(object, "meta.data"),
      error = function(error) data.frame()
    )
  }
  ir <- .builder_immune_profile_repertoire(misc, context)
  hla <- .builder_immune_profile_hla(misc, ir$candidates)
  ir$candidates <- lapply(ir$candidates, .builder_immune_strip_private)
  hla <- .builder_immune_strip_private(hla)
  list(immune_repertoire = ir, hla = hla)
}
