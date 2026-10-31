##----------------------------------------------------------------------------##
## Conservative recommendations derived only from DatasetProfile v2 facts.
##
## Each decision has one authoritative `value`. Supporting fields explain why
## it was chosen and whether a person must confirm it before a build proceeds.
##----------------------------------------------------------------------------##

.builder_recommend_abort <- function(message) {
  stop(message, call. = FALSE)
}

.builder_recommend_scalar_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_recommend_record <- function(
  value,
  reason,
  confidence,
  requires_confirmation,
  ...
) {
  if (!.builder_recommend_scalar_text(reason)) {
    .builder_recommend_abort("A recommendation reason must be one string.")
  }
  if (
    !is.numeric(confidence) ||
      length(confidence) != 1L ||
      is.na(confidence) ||
      !is.finite(confidence) ||
      confidence < 0 ||
      confidence > 1
  ) {
    .builder_recommend_abort(
      "Recommendation confidence must be between 0 and 1."
    )
  }
  if (
    !is.logical(requires_confirmation) ||
      length(requires_confirmation) != 1L ||
      is.na(requires_confirmation)
  ) {
    .builder_recommend_abort(
      "Recommendation confirmation must be one logical value."
    )
  }
  c(
    list(
      value = value,
      reason = trimws(reason),
      confidence = as.numeric(confidence),
      requires_confirmation = requires_confirmation
    ),
    list(...)
  )
}

.builder_recommend_profile <- function(profile) {
  if (
    !is.list(profile) ||
      !identical(profile$schema_version, 2L) ||
      !is.list(profile$identity) ||
      !is.list(profile$identity$cells) ||
      !is.list(profile$metadata) ||
      !is.list(profile$metadata$columns)
  ) {
    .builder_recommend_abort(
      "Recommendations require stable facts from DatasetProfile v2."
    )
  }
  count <- profile$identity$cells$count
  if (
    !is.numeric(count) ||
      length(count) != 1L ||
      is.na(count) ||
      !is.finite(count) ||
      count < 0 ||
      count > .Machine$integer.max ||
      count != floor(count)
  ) {
    .builder_recommend_abort("DatasetProfile v2 has a malformed cell count.")
  }
  profile
}

.builder_recommend_names <- function(values, label) {
  if (is.null(values)) {
    return(character())
  }
  if (
    !is.character(values) ||
      anyNA(values) ||
      any(!nzchar(trimws(values)))
  ) {
    .builder_recommend_abort(paste(label, "must contain non-empty names."))
  }
  unique(trimws(values))
}

.builder_recommend_normalize_name <- function(name) {
  name <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", name, perl = TRUE)
  name <- tolower(name)
  name <- gsub("[^a-z0-9]+", "_", name)
  gsub("(^_+|_+$)", "", name)
}

.builder_recommend_sensitive_name <- function(name) {
  normalized <- .builder_recommend_normalize_name(name)
  tokens <- strsplit(normalized, "_", fixed = TRUE)[[1L]]
  tokens <- tokens[nzchar(tokens)]
  sensitive_tokens <- c(
    "patient",
    "donor",
    "subject",
    "barcode",
    "email",
    "name",
    "mrn",
    "address",
    "phone",
    "clinical"
  )
  any(tokens %in% sensitive_tokens) ||
    grepl("sample.*id", gsub("_", "", normalized, fixed = TRUE))
}

.builder_recommend_category_name <- function(name) {
  normalized <- .builder_recommend_normalize_name(name)
  allowlist <- c(
    "cell_type",
    "celltype",
    "annotation",
    "seurat_clusters",
    "cluster",
    "cluster_id",
    "group",
    "condition",
    "treatment",
    "batch",
    "phase",
    "ident",
    "label"
  )
  normalized %in% allowlist
}

.builder_recommend_dependencies <- function(dependency_ids, name) {
  values <- dependency_ids[[name]]
  if (is.null(values)) {
    return(character())
  }
  values <- .builder_recommend_names(values, "Dependency ids")
  sort(values, method = "radix")
}

.builder_recommend_metadata_fact <- function(
  fact,
  name,
  n_cells,
  required,
  dependency_ids
) {
  if (
    !is.list(fact) ||
      !.builder_recommend_scalar_text(fact$name) ||
      !identical(fact$name, name) ||
      !is.character(fact$class) ||
      !length(fact$class) ||
      anyNA(fact$class) ||
      !is.logical(fact$supported) ||
      length(fact$supported) != 1L ||
      is.na(fact$supported)
  ) {
    .builder_recommend_abort("DatasetProfile v2 has a malformed metadata fact.")
  }
  counts <- c(fact$non_missing, fact$unique_non_missing)
  if (
    !is.numeric(counts) ||
      length(counts) != 2L ||
      anyNA(counts) ||
      any(!is.finite(counts)) ||
      any(counts < 0) ||
      any(counts > .Machine$integer.max) ||
      any(counts != floor(counts)) ||
      counts[[2L]] > counts[[1L]] ||
      counts[[1L]] > n_cells
  ) {
    .builder_recommend_abort("DatasetProfile v2 has a malformed metadata fact.")
  }

  class <- as.character(fact$class)
  non_missing <- as.integer(fact$non_missing)
  unique_non_missing <- as.integer(fact$unique_non_missing)
  is_required <- name %in% required
  sensitive <- .builder_recommend_sensitive_name(name)
  categorical_name <- .builder_recommend_category_name(name)
  safe_type <- any(class %in% c("factor", "ordered", "character", "logical"))
  numeric_type <- any(class %in% c("integer", "numeric", "double"))
  low_cardinality <- unique_non_missing >= 2L &&
    unique_non_missing <= 50L &&
    unique_non_missing <= n_cells * 0.05
  one_value_per_cell <- non_missing > 0L &&
    unique_non_missing >= non_missing

  retain_in_crb <- isTRUE(fact$supported)
  group_recommended <- FALSE

  disposition <- "excluded"
  effective_included <- FALSE
  confirmation <- FALSE
  preview_allowed <- FALSE
  group_eligible <- FALSE
  confidence <- 1
  reason <- "This column is not a safe low-cardinality export choice."

  if (!isTRUE(fact$supported)) {
    disposition <- if (is_required) "blocking" else "excluded"
    confirmation <- is_required
    reason <- if (is_required) {
      "This required column has an unsupported metadata type."
    } else {
      "This column has an unsupported metadata type."
    }
  } else if (sensitive) {
    disposition <- "attention"
    effective_included <- is_required
    confirmation <- TRUE
    confidence <- 0.5
    reason <- if (is_required) {
      "This required column looks sensitive and needs explicit confirmation."
    } else {
      "This column looks sensitive and is not included automatically."
    }
  } else if (numeric_type && categorical_name && low_cardinality) {
    disposition <- "included"
    effective_included <- TRUE
    preview_allowed <- TRUE
    group_eligible <- TRUE
    group_recommended <- TRUE
    reason <- "This is an explicitly named low-cardinality category."
  } else if (is_required && safe_type && low_cardinality) {
    disposition <- "included"
    effective_included <- TRUE
    preview_allowed <- TRUE
    group_eligible <- TRUE
    group_recommended <- TRUE
    reason <- "This required column is a safe categorical export."
  } else if (is_required) {
    disposition <- "attention"
    effective_included <- TRUE
    confirmation <- TRUE
    confidence <- 0.5
    reason <- paste0(
      "This required column is exportable but needs explicit confirmation."
    )
  } else if (unique_non_missing < 2L) {
    reason <- "This column does not contain two usable categories."
  } else if (one_value_per_cell) {
    reason <- "This column has one distinct value for every non-missing cell."
  } else if (!low_cardinality) {
    disposition <- "attention"
    confirmation <- TRUE
    confidence <- 0.5
    reason <- paste0(
      "This column has ",
      unique_non_missing,
      " values and needs confirmation beyond the automatic category limit."
    )
  } else if (safe_type) {
    disposition <- "included"
    effective_included <- TRUE
    preview_allowed <- TRUE
    group_eligible <- TRUE
    group_recommended <- TRUE
    reason <- "This is a safe low-cardinality categorical column."
  } else if (numeric_type) {
    disposition <- "attention"
    confirmation <- TRUE
    confidence <- 0.5
    reason <- paste0(
      "This low-cardinality numeric column needs categorical confirmation."
    )
  } else {
    reason <- paste0(
      "Numeric metadata is excluded unless its name is explicitly categorical."
    )
  }

  if (isTRUE(fact$supported)) {
    disposition <- "included"
    effective_included <- TRUE
    confirmation <- FALSE
    preview_allowed <- TRUE
    reason <- "Cell metadata is retained automatically for Viewer details and downloads."
  }

  .builder_recommend_record(
    value = disposition,
    reason = reason,
    confidence = confidence,
    requires_confirmation = confirmation,
    name = name,
    class = class,
    non_missing = non_missing,
    unique_non_missing = unique_non_missing,
    disposition = disposition,
    dependency_ids = .builder_recommend_dependencies(dependency_ids, name),
    preview_allowed = preview_allowed,
    effective_included = effective_included,
    retain_in_crb = retain_in_crb,
    group_eligible = group_eligible,
    group_recommended = group_recommended,
    supported = isTRUE(fact$supported),
    forced = is_required,
    sensitive = sensitive,
    required = is_required
  )
}

#' Recommend a conservative metadata export policy.
builder_recommend_metadata <- function(
  profile,
  required = character(),
  dependency_ids = list()
) {
  profile <- .builder_recommend_profile(profile)
  required <- .builder_recommend_names(required, "Required metadata")
  if (!is.list(dependency_ids) || is.object(dependency_ids)) {
    .builder_recommend_abort("Dependency ids must be a named list.")
  }
  if (length(dependency_ids)) {
    if (
      is.null(names(dependency_ids)) ||
        anyNA(names(dependency_ids)) ||
        any(!nzchar(names(dependency_ids)))
    ) {
      .builder_recommend_abort("Dependency ids must be a named list.")
    }
  }
  n_cells <- as.integer(profile$identity$cells$count)
  columns <- profile$metadata$columns
  column_names <- names(columns)
  if (is.null(column_names)) {
    if (length(columns)) {
      .builder_recommend_abort(
        "DatasetProfile v2 metadata columns must be named."
      )
    }
    column_names <- character()
  }
  if (
    anyNA(column_names) ||
      any(!nzchar(column_names)) ||
      anyDuplicated(column_names)
  ) {
    .builder_recommend_abort(
      "DatasetProfile v2 metadata columns are malformed."
    )
  }

  reserved_collision <- "cell_barcode" %in% column_names
  cell_barcode <- .builder_recommend_record(
    value = if (reserved_collision) "blocking" else "included",
    reason = if (reserved_collision) {
      paste0(
        "The metadata name cell_barcode is reserved for mandatory identity; ",
        "rename the metadata column before export."
      )
    } else {
      "Cell barcodes are required to preserve dataset identity."
    },
    confidence = 1,
    requires_confirmation = reserved_collision,
    name = "cell_barcode",
    class = "character",
    non_missing = n_cells,
    unique_non_missing = n_cells,
    disposition = if (reserved_collision) "blocking" else "included",
    dependency_ids = "core.cell_identity",
    preview_allowed = FALSE,
    effective_included = TRUE,
    retain_in_crb = TRUE,
    group_eligible = FALSE,
    group_recommended = FALSE,
    supported = TRUE,
    forced = TRUE,
    sensitive = FALSE,
    required = TRUE
  )
  metadata_names <- setdiff(column_names, "cell_barcode")
  records <- lapply(metadata_names, function(name) {
    .builder_recommend_metadata_fact(
      columns[[name]],
      name,
      n_cells,
      required,
      dependency_ids
    )
  })
  names(records) <- metadata_names

  missing_required <- setdiff(required, c("cell_barcode", column_names))
  for (name in missing_required) {
    records[[name]] <- .builder_recommend_record(
      value = "blocking",
      reason = "This required metadata column is missing.",
      confidence = 1,
      requires_confirmation = TRUE,
      name = name,
      class = "missing",
      non_missing = 0L,
      unique_non_missing = 0L,
      disposition = "blocking",
      dependency_ids = .builder_recommend_dependencies(dependency_ids, name),
      preview_allowed = FALSE,
      effective_included = FALSE,
      retain_in_crb = FALSE,
      group_eligible = FALSE,
      group_recommended = FALSE,
      supported = FALSE,
      forced = TRUE,
      sensitive = .builder_recommend_sensitive_name(name),
      required = TRUE
    )
  }
  records <- c(list(cell_barcode = cell_barcode), records)
  disposition <- vapply(records, `[[`, "", "disposition")
  retained <- names(records)[vapply(records, `[[`, FALSE, "retain_in_crb")]
  group_candidates <- names(records)[vapply(
    records,
    `[[`,
    FALSE,
    "group_eligible"
  )]
  forced <- names(records)[vapply(records, `[[`, FALSE, "forced")]
  included <- retained
  attention <- names(records)[disposition == "attention"]
  excluded <- names(records)[!names(records) %in% retained]
  blocking <- names(records)[disposition == "blocking"]
  requires_confirmation <- length(attention) > 0L || length(blocking) > 0L

  .builder_recommend_record(
    value = included,
    reason = if (length(blocking)) {
      "Required metadata is missing or cannot be exported safely."
    } else if (length(attention)) {
      "Safe metadata is included; attention items need confirmation."
    } else {
      "Only safe low-cardinality metadata is included."
    },
    confidence = if (length(blocking)) {
      0
    } else if (length(attention)) {
      0.7
    } else {
      1
    },
    requires_confirmation = requires_confirmation,
    columns = records,
    included = included,
    retained = retained,
    group_candidates = group_candidates,
    forced = forced,
    attention = attention,
    excluded = excluded,
    blocking = blocking
  )
}


#' Nomenclature choices supported for one confirmed organism.
builder_nomenclature_choices <- function(organism) {
  if (!.builder_recommend_scalar_text(organism)) {
    return(character())
  }
  switch(
    organism,
    hg = c("name", "ensembl", "gencode_v27"),
    mm = c("name", "ensembl", "gencode_vM16"),
    character()
  )
}

#' Validate an organism/nomenclature pair without cross-species coercion.
builder_validate_nomenclature <- function(organism, nomenclature) {
  if (
    !.builder_recommend_scalar_text(nomenclature) ||
      !nomenclature %in% builder_nomenclature_choices(organism)
  ) {
    .builder_recommend_abort(
      "The nomenclature is not valid for the selected organism."
    )
  }
  invisible(nomenclature)
}

.builder_recommend_available <- function(available) {
  capabilities <- "h5"
  blank <- stats::setNames(rep(FALSE, length(capabilities)), capabilities)
  normalize_side <- function(side) {
    values <- blank
    if (!is.list(side) && !(is.logical(side) && !is.null(names(side)))) {
      return(values)
    }
    for (name in capabilities) {
      value <- side[[name]]
      values[[name]] <- is.logical(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        isTRUE(value)
    }
    values
  }
  if (!is.list(available)) {
    return(list(build = blank, viewer = blank))
  }
  list(
    build = normalize_side(available$build),
    viewer = normalize_side(available$viewer)
  )
}

#' Recommend expression storage from bounded matrix facts and capabilities.
builder_recommend_backend <- function(matrix_summary, available) {
  normalized <- .builder_recommend_available(available)
  storage <- if (is.list(matrix_summary)) {
    matrix_summary$storage %||% "memory"
  } else {
    NULL
  }
  valid <- is.list(matrix_summary) &&
    is.numeric(matrix_summary$estimated_bytes) &&
    length(matrix_summary$estimated_bytes) == 1L &&
    !is.na(matrix_summary$estimated_bytes) &&
    is.finite(matrix_summary$estimated_bytes) &&
    matrix_summary$estimated_bytes >= 0 &&
    is.logical(matrix_summary$sparse) &&
    length(matrix_summary$sparse) == 1L &&
    !is.na(matrix_summary$sparse) &&
    .builder_recommend_scalar_text(storage) &&
    storage %in% c("memory", "bpcells", "h5")
  if (!valid) {
    return(.builder_recommend_record(
      value = NULL,
      reason = "Matrix size, sparsity, or storage facts are missing or malformed.",
      confidence = 0,
      requires_confirmation = TRUE,
      available = normalized,
      blocking = "matrix_summary",
      dependency_actions = "Re-inspect the expression matrix."
    ))
  }
  complete <- function(name) {
    isTRUE(normalized$build[[name]]) && isTRUE(normalized$viewer[[name]])
  }
  if (matrix_summary$estimated_bytes <= 256 * 1024^2) {
    return(.builder_recommend_record(
      value = "embedded",
      reason = "The measured snapshot size fits the embedded threshold.",
      confidence = 1,
      requires_confirmation = FALSE,
      available = normalized,
      blocking = character(),
      dependency_actions = character()
    ))
  }
  if (complete("h5")) {
    return(.builder_recommend_record(
      value = "h5",
      reason = "A large matrix uses complete H5 support.",
      confidence = 1,
      requires_confirmation = FALSE,
      available = normalized,
      blocking = character(),
      dependency_actions = character()
    ))
  }
  .builder_recommend_record(
    value = NULL,
    reason = "No non-embedded backend has complete build and Viewer support.",
    confidence = 0,
    requires_confirmation = TRUE,
    available = normalized,
    blocking = "expression_backend",
    dependency_actions = "Enable HDF5 support."
  )
}
