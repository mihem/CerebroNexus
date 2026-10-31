##----------------------------------------------------------------------------##
## Stable source facts for Viewer tables, trajectories, and extra material.
##
## These helpers return bounded summaries only. The source payload remains in
## the worker-owned Seurat object and is never copied into DatasetProfile.
##----------------------------------------------------------------------------##

# Shared records ----

.builder_table_identifier_byte_limit <- 256L
.builder_table_preview_limit <- 20L
.builder_table_preview_character_limit <- 160L
.builder_table_preview_byte_limit <- 256L

.builder_table_bound_preview_text <- function(values) {
  if (!is.character(values) || is.object(values) || isS4(values)) {
    return(list(value = character(), truncated_count = 0L))
  }
  attributes(values) <- NULL
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
      too_many_characters <- nchar(value, type = "chars") >
        .builder_table_preview_character_limit
      too_many_bytes <- nchar(value, type = "bytes") >
        .builder_table_preview_byte_limit
      if (!too_many_characters && !too_many_bytes) {
        return(value)
      }
      truncated[[index]] <<- TRUE
      character_budget <- max(
        0L,
        .builder_table_preview_character_limit - 3L
      )
      byte_budget <- max(0L, .builder_table_preview_byte_limit - 3L)
      prefix <- substr(value, 1L, character_budget)
      while (
        nzchar(prefix) &&
          nchar(prefix, type = "bytes") > byte_budget
      ) {
        prefix <- substr(prefix, 1L, nchar(prefix, type = "chars") - 1L)
      }
      paste0(prefix, "...")
    },
    character(1),
    USE.NAMES = FALSE
  )
  list(
    value = out,
    truncated_count = as.integer(sum(truncated))
  )
}

.builder_table_record <- function(
  detected = FALSE,
  valid = TRUE,
  normalized = NULL,
  diagnostics = character(),
  requirements = character(),
  page_candidates = character()
) {
  diagnostics <- unique(as.character(diagnostics))
  requirements <- unique(as.character(requirements))
  page_candidates <- unique(as.character(page_candidates))
  diagnostics <- diagnostics[!is.na(diagnostics)]
  requirements <- requirements[!is.na(requirements)]
  page_candidates <- page_candidates[
    !is.na(page_candidates) & nzchar(page_candidates)
  ]
  list(
    detected = isTRUE(detected),
    valid = isTRUE(valid),
    normalized = normalized,
    diagnostics = diagnostics,
    requirements = requirements,
    page_candidates = page_candidates
  )
}

.builder_table_plain_list <- function(value) {
  is.list(value) && !is.object(value) && !isS4(value)
}

.builder_table_list_names <- function(value) {
  names <- attr(value, "names", exact = TRUE)
  if (is.null(names)) {
    rep.int("", length(value))
  } else {
    names
  }
}

.builder_table_name_diagnostics <- function(names, label) {
  diagnostics <- character()
  if (is.object(names) || isS4(names)) {
    return(paste0("unsafe_", label, "_names"))
  }
  if (!is.character(names) || anyNA(names) || any(!nzchar(names))) {
    diagnostics <- c(diagnostics, paste0("blank_", label, "_names"))
  }
  if (is.character(names) && anyDuplicated(names)) {
    diagnostics <- c(diagnostics, paste0("duplicate_", label, "_names"))
  }
  if (
    is.character(names) &&
      any(
        !is.na(names) &
          nchar(names, type = "bytes") > .builder_table_identifier_byte_limit
      )
  ) {
    diagnostics <- c(diagnostics, paste0("oversized_", label, "_names"))
  }
  diagnostics
}

.builder_table_context_groups <- function(context) {
  candidates <- context[["groups"]][["candidates"]]
  if (!is.character(candidates)) {
    return(character())
  }
  unique(candidates[!is.na(candidates) & nzchar(candidates)])
}

.builder_table_context_cells <- function(context) {
  cells <- context[["cells"]]
  if (!is.character(cells)) {
    return(character())
  }
  cells[!is.na(cells) & nzchar(cells)]
}

.builder_table_detected <- function(value) {
  if (is.null(value)) {
    return(FALSE)
  }
  if (!.builder_table_plain_list(value)) {
    return(TRUE)
  }
  length(value) > 0L
}

.builder_table_content_preflight <- function(
  value,
  requirements,
  invalid_diagnostic = "unsafe_container",
  max_entries = NULL,
  max_outer = max_entries,
  nested = FALSE,
  name_label = NULL,
  empty_diagnostic = NULL,
  names_before_budget = FALSE,
  container_required = FALSE
) {
  invalid <- function(diagnostic) {
    .builder_table_record(
      detected = TRUE,
      valid = FALSE,
      normalized = list(),
      diagnostics = diagnostic,
      requirements = requirements
    )
  }
  if (isTRUE(container_required) && !.builder_table_plain_list(value)) {
    return(list(record = invalid(invalid_diagnostic), names = character()))
  }
  if (!.builder_table_detected(value)) {
    if (
      .builder_table_plain_list(value) &&
        !length(value) &&
        !is.null(empty_diagnostic)
    ) {
      return(list(record = invalid(empty_diagnostic), names = character()))
    }
    return(list(
      record = .builder_table_record(requirements = requirements),
      names = character()
    ))
  }
  if (!.builder_table_plain_list(value)) {
    return(list(record = invalid(invalid_diagnostic), names = character()))
  }
  entry_names <- .builder_table_list_names(value)
  diagnostics <- if (is.null(name_label)) {
    character()
  } else {
    .builder_table_name_diagnostics(entry_names, name_label)
  }
  if (isTRUE(names_before_budget) && length(diagnostics)) {
    return(list(record = invalid(diagnostics), names = entry_names))
  }
  entry_count <- if (isTRUE(nested)) {
    sum(vapply(
      value,
      function(entries) {
        if (.builder_table_plain_list(entries)) {
          length(entries)
        } else {
          0L
        }
      },
      integer(1)
    ))
  } else {
    length(value)
  }
  if (
    !is.null(max_entries) &&
      (length(value) > max_outer || entry_count > max_entries)
  ) {
    return(list(
      record = invalid("profile_entry_budget_exceeded"),
      names = character()
    ))
  }
  list(
    record = if (length(diagnostics)) invalid(diagnostics) else NULL,
    names = entry_names
  )
}

.builder_table_gene_lists_content <- function(value) {
  requirements <- c("named_gene_lists", "character_genes")
  preflight <- .builder_table_content_preflight(
    value,
    requirements,
    invalid_diagnostic = "invalid_gene_list_container",
    max_entries = 512L
  )
  if (!is.null(preflight$record)) {
    return(preflight$record)
  }
  list_names <- preflight$names
  diagnostics <- .builder_table_name_diagnostics(list_names, "gene_list")
  valid_members <- vapply(
    value,
    function(genes) {
      is.character(genes) &&
        !is.object(genes) &&
        !isS4(genes) &&
        length(genes) > 0L &&
        !anyNA(genes) &&
        all(nzchar(trimws(genes)))
    },
    logical(1)
  )
  if (any(!valid_members)) {
    diagnostics <- c(diagnostics, "invalid_gene_list")
  }
  counts <- if (all(valid_members)) lengths(value) else integer()
  bounded_names <- .builder_table_bound_preview_text(
    utils::head(list_names, .builder_table_preview_limit)
  )
  .builder_table_record(
    detected = TRUE,
    valid = !length(diagnostics),
    normalized = list(
      n_lists = as.integer(length(value)),
      n_genes = as.integer(sum(counts)),
      list_preview = bounded_names$value,
      preview_truncated_count = as.integer(
        max(0L, length(value) - .builder_table_preview_limit) +
          bounded_names$truncated_count
      )
    ),
    diagnostics = unique(diagnostics),
    requirements = requirements
  )
}

.builder_table_misc <- function(object) {
  tryCatch(
    methods::slot(object, "misc"),
    error = function(error) list()
  )
}

# Inert data frames ----

.builder_table_data_frame_class <- function(value) {
  class <- attr(value, "class", exact = TRUE)
  identical(class, "data.frame") ||
    identical(class, c("tbl_df", "tbl", "data.frame"))
}

.builder_table_factor_values <- function(value) {
  class <- attr(value, "class", exact = TRUE)
  if (
    !identical(class, "factor") &&
      !identical(class, c("ordered", "factor"))
  ) {
    return(NULL)
  }
  codes <- unclass(value)
  levels <- attr(value, "levels", exact = TRUE)
  if (
    typeof(codes) != "integer" ||
      !is.character(levels) ||
      is.object(levels) ||
      isS4(levels) ||
      anyNA(levels) ||
      any(!nzchar(levels)) ||
      anyDuplicated(levels) ||
      any(!is.na(codes) & (codes < 1L | codes > length(levels)))
  ) {
    return(NULL)
  }
  result <- rep.int(NA_character_, length(codes))
  present <- !is.na(codes)
  result[present] <- levels[codes[present]]
  result
}

.builder_table_plain_column <- function(value) {
  if (isS4(value) || is.function(value) || is.environment(value)) {
    return(FALSE)
  }
  class <- attr(value, "class", exact = TRUE)
  if (!is.null(class)) {
    return(!is.null(.builder_table_factor_values(value)))
  }
  is.atomic(value) && !is.list(value)
}

.builder_table_row_count <- function(row_names, columns) {
  if (length(columns)) {
    return(length(columns[[1L]]))
  }
  if (is.null(row_names)) {
    return(0L)
  }
  if (
    is.integer(row_names) &&
      length(row_names) == 2L &&
      is.na(row_names[[1L]]) &&
      row_names[[2L]] <= 0L
  ) {
    return(as.integer(-row_names[[2L]]))
  }
  as.integer(length(row_names))
}

.builder_table_column_length <- function(value) {
  if (!is.null(attr(value, "class", exact = TRUE))) {
    value <- unclass(value)
  }
  length(value)
}

.builder_table_data_frame <- function(value) {
  invalid <- list(
    valid = FALSE,
    rows = 0L,
    columns = character(),
    column_types = character(),
    row_names = NULL,
    data = NULL,
    diagnostics = "unsupported_table"
  )
  if (!.builder_table_data_frame_class(value) || typeof(value) != "list") {
    return(invalid)
  }

  data <- unclass(value)
  column_names <- attr(data, "names", exact = TRUE)
  row_names <- attr(data, "row.names", exact = TRUE)
  diagnostics <- character()
  plain_column_names <- is.character(column_names) &&
    !is.object(column_names) &&
    !isS4(column_names)
  if (
    !plain_column_names ||
      length(column_names) != length(data) ||
      anyNA(column_names) ||
      any(!nzchar(column_names))
  ) {
    diagnostics <- c(diagnostics, "blank_table_columns")
  }
  if (plain_column_names && anyDuplicated(column_names)) {
    diagnostics <- c(diagnostics, "duplicate_table_columns")
  }
  oversized_column_names <- plain_column_names &&
    any(
      !is.na(column_names) &
        nchar(column_names, type = "bytes") >
          .builder_table_identifier_byte_limit
    )
  if (oversized_column_names) {
    diagnostics <- c(diagnostics, "oversized_table_column_names")
  }
  if (length(data) > 256L) {
    diagnostics <- c(diagnostics, "profile_column_budget_exceeded")
  }
  safe_columns <- vapply(data, .builder_table_plain_column, logical(1))
  if (length(safe_columns) && !all(safe_columns)) {
    diagnostics <- c(diagnostics, "unsafe_table_columns")
  }
  row_names_safe <- is.null(row_names) ||
    (is.atomic(row_names) && !is.object(row_names) && !isS4(row_names))
  if (!row_names_safe) {
    diagnostics <- c(diagnostics, "unsafe_table_rows")
  }
  lengths <- if (length(data) && all(safe_columns)) {
    vapply(data, .builder_table_column_length, integer(1))
  } else {
    integer()
  }
  if (length(lengths) && length(unique(lengths)) != 1L) {
    diagnostics <- c(diagnostics, "inconsistent_table_columns")
  }
  rows <- if (row_names_safe) {
    .builder_table_row_count(
      row_names,
      if (all(safe_columns)) data else list()
    )
  } else {
    0L
  }
  if (length(lengths) && any(lengths != rows)) {
    diagnostics <- c(diagnostics, "invalid_table_rows")
  }
  column_types <- if (length(data)) {
    vapply(
      data,
      function(column) {
        class <- attr(column, "class", exact = TRUE)
        if (is.null(class)) typeof(column) else paste(class, collapse = "/")
      },
      character(1)
    )
  } else {
    character()
  }
  list(
    valid = !length(diagnostics),
    rows = rows,
    columns = if (plain_column_names && !oversized_column_names) {
      column_names
    } else {
      character()
    },
    column_types = column_types,
    row_names = row_names,
    data = if (!length(diagnostics)) data else NULL,
    diagnostics = unique(diagnostics)
  )
}

.builder_table_column_text <- function(table, name) {
  index <- match(name, table$columns)
  if (is.na(index)) {
    return(NULL)
  }
  value <- table$data[[index]]
  class <- attr(value, "class", exact = TRUE)
  if (is.null(class) && typeof(value) == "character") {
    return(value)
  }
  .builder_table_factor_values(value)
}

.builder_table_column_numeric <- function(table, name) {
  index <- match(name, table$columns)
  if (is.na(index)) {
    return(NULL)
  }
  value <- table$data[[index]]
  if (
    !is.null(attr(value, "class", exact = TRUE)) ||
      !typeof(value) %in% c("integer", "double")
  ) {
    return(NULL)
  }
  unclass(value)
}

.builder_table_column_summary <- function(table, limit = 32L) {
  count <- length(table$columns)
  shown <- min(count, limit)
  list(
    column_count = count,
    columns = utils::head(table$columns, shown),
    column_types = utils::head(unname(table$column_types), shown),
    columns_truncated = count > shown
  )
}

.builder_table_summary <- function(table, groups = character()) {
  group_column <- if (
    length(table$columns) && table$columns[[1L]] %in% groups
  ) {
    table$columns[[1L]]
  } else {
    NULL
  }
  columns <- .builder_table_column_summary(table)
  list(
    kind = "table",
    rows = table$rows,
    column_count = columns$column_count,
    columns = columns$columns,
    column_types = columns$column_types,
    columns_truncated = columns$columns_truncated,
    group_column = group_column,
    group_compatible = !is.null(group_column),
    valid = isTRUE(table$valid) && table$rows > 0L,
    diagnostics = unique(c(
      table$diagnostics,
      if (table$valid && table$rows == 0L) "empty_table" else character()
    ))
  )
}

.builder_table_empty_result <- function(payload, sentinels) {
  if (
    typeof(payload) == "character" &&
      is.null(attr(payload, "class", exact = TRUE)) &&
      length(payload) == 1L &&
      !is.na(payload) &&
      payload %in% sentinels
  ) {
    return(list(
      kind = "empty_result",
      sentinel = unname(payload),
      valid = TRUE,
      diagnostics = character()
    ))
  }
  NULL
}

# Nested result tables ----

.builder_table_nested_content <- function(
  value,
  groups,
  sentinels,
  name_label,
  payload_validator
) {
  preflight <- .builder_table_content_preflight(
    value,
    character(),
    max_entries = 512L,
    max_outer = 64L,
    nested = TRUE,
    name_label = name_label,
    empty_diagnostic = "empty_container",
    names_before_budget = TRUE,
    container_required = TRUE
  )
  if (!is.null(preflight$record)) {
    return(list(
      valid = FALSE,
      normalized = list(),
      diagnostics = preflight$record$diagnostics,
      usable = FALSE
    ))
  }
  outer_names <- preflight$names
  entry_name_diagnostics <- character()
  for (outer_index in seq_along(value)) {
    entries <- value[[outer_index]]
    if (.builder_table_plain_list(entries)) {
      entry_name_diagnostics <- c(
        entry_name_diagnostics,
        .builder_table_name_diagnostics(
          .builder_table_list_names(entries),
          "entry"
        )
      )
    }
  }
  if (length(entry_name_diagnostics)) {
    return(list(
      valid = FALSE,
      normalized = list(),
      diagnostics = unique(entry_name_diagnostics),
      usable = FALSE
    ))
  }

  normalized <- list()
  diagnostics <- character()
  usable <- FALSE
  for (outer_index in seq_along(value)) {
    outer_name <- outer_names[[outer_index]]
    entries <- value[[outer_index]]
    if (!.builder_table_plain_list(entries) || !length(entries)) {
      normalized[[outer_name]] <- list()
      diagnostics <- c(diagnostics, "invalid_nested_container")
      next
    }
    entry_names <- .builder_table_list_names(entries)
    entry_diagnostics <- .builder_table_name_diagnostics(
      entry_names,
      "entry"
    )
    if (length(entry_diagnostics)) {
      normalized[[outer_name]] <- list()
      diagnostics <- c(diagnostics, entry_diagnostics)
      next
    }
    method_entries <- list()
    for (entry_index in seq_along(entries)) {
      entry_name <- entry_names[[entry_index]]
      payload <- entries[[entry_index]]
      empty_result <- .builder_table_empty_result(payload, sentinels)
      summary <- if (!is.null(empty_result)) {
        empty_result$group_compatible <- entry_name %in% groups
        empty_result
      } else {
        payload_validator(payload, entry_name, groups)
      }
      method_entries[[entry_name]] <- summary
      diagnostics <- c(diagnostics, summary$diagnostics)
      usable <- usable ||
        (isTRUE(summary$valid) && isTRUE(summary$group_compatible))
    }
    normalized[[outer_name]] <- method_entries
  }
  list(
    valid = !length(diagnostics) && usable,
    normalized = normalized,
    diagnostics = unique(diagnostics),
    usable = usable
  )
}

.builder_table_marker_payload <- function(payload, entry_name, groups) {
  table <- .builder_table_data_frame(payload)
  summary <- .builder_table_summary(table, groups)
  if (!table$valid) {
    summary$diagnostics <- unique(c(summary$diagnostics, "unsupported_payload"))
  }
  summary
}

.builder_table_marker_content <- function(value, groups) {
  detected <- .builder_table_detected(value)
  if (!detected) {
    return(.builder_table_record(requirements = "named_method_results"))
  }
  content <- .builder_table_nested_content(
    value,
    groups,
    sentinels = "no_markers_found",
    name_label = "method",
    payload_validator = .builder_table_marker_payload
  )
  .builder_table_record(
    detected = TRUE,
    valid = content$valid,
    normalized = content$normalized,
    diagnostics = content$diagnostics,
    requirements = c("named_method_results", "table_or_no_markers_found"),
    page_candidates = if (content$usable) "marker_genes" else character()
  )
}

.builder_table_metric_payload <- function(
  payload,
  group,
  groups,
  metric
) {
  table <- .builder_table_data_frame(payload)
  summary <- .builder_table_summary(table, groups)
  summary$group <- group
  summary$group_compatible <- group %in% groups
  diagnostics <- summary$diagnostics
  if (!summary$group_compatible) {
    diagnostics <- c(diagnostics, "incompatible_group")
  }
  gene <- if (table$valid) {
    .builder_table_column_text(table, "gene")
  } else {
    NULL
  }
  if (is.null(gene)) {
    diagnostics <- c(diagnostics, "missing_gene")
  } else if (anyNA(gene) || any(!nzchar(gene))) {
    diagnostics <- c(diagnostics, "invalid_gene")
  }
  metric_values <- if (table$valid) {
    .builder_table_column_numeric(table, metric)
  } else {
    NULL
  }
  if (is.null(metric_values)) {
    diagnostics <- c(diagnostics, paste0("missing_", metric))
  } else if (any(!is.finite(metric_values))) {
    diagnostics <- c(diagnostics, paste0("non_finite_", metric))
  }
  summary$valid <- !length(diagnostics) && table$rows > 0L
  summary$diagnostics <- unique(diagnostics)
  summary
}

.builder_table_metric_content <- function(
  value,
  groups,
  metric,
  page = character()
) {
  requirements <- c("compatible_group", "gene", metric)
  if (identical(metric, "mean_expr")) {
    requirements <- c(requirements, "most_expressed_genes")
  }
  preflight <- .builder_table_content_preflight(
    value,
    requirements,
    max_entries = 512L,
    name_label = "group"
  )
  if (!is.null(preflight$record)) {
    return(preflight$record)
  }
  entry_names <- preflight$names
  diagnostics <- character()
  normalized <- list()
  usable <- FALSE
  for (index in seq_along(value)) {
    group <- entry_names[[index]]
    summary <- .builder_table_metric_payload(
      value[[index]],
      group,
      groups,
      metric
    )
    normalized[[group]] <- summary
    diagnostics <- c(diagnostics, summary$diagnostics)
    usable <- usable || isTRUE(summary$valid)
  }
  .builder_table_record(
    detected = TRUE,
    valid = !length(diagnostics) && usable,
    normalized = normalized,
    diagnostics = unique(diagnostics),
    requirements = requirements,
    page_candidates = if (usable) page else character()
  )
}

.builder_table_enrichment_payload <- function(payload, group, groups) {
  table <- .builder_table_data_frame(payload)
  summary <- .builder_table_summary(table, groups)
  summary$group <- group
  summary$group_compatible <- group %in% groups
  if (!summary$group_compatible) {
    summary$diagnostics <- unique(c(
      summary$diagnostics,
      "incompatible_group"
    ))
  }
  if (!table$valid) {
    summary$diagnostics <- unique(c(
      summary$diagnostics,
      "unsupported_payload"
    ))
  }
  summary$valid <- !length(summary$diagnostics) && table$rows > 0L
  summary
}

.builder_table_enrichment_content <- function(value, groups) {
  detected <- .builder_table_detected(value)
  requirements <- c("named_method_results", "compatible_group")
  if (!detected) {
    return(.builder_table_record(requirements = requirements))
  }
  content <- .builder_table_nested_content(
    value,
    groups,
    sentinels = c(
      "no_markers_found",
      "no_pathways_found",
      "no_gene_sets_enriched"
    ),
    name_label = "method",
    payload_validator = .builder_table_enrichment_payload
  )
  for (method in names(content$normalized)) {
    for (group in names(content$normalized[[method]])) {
      entry <- content$normalized[[method]][[group]]
      if (!isTRUE(entry$group_compatible)) {
        entry$valid <- FALSE
        entry$diagnostics <- unique(c(
          entry$diagnostics,
          "incompatible_group"
        ))
        content$normalized[[method]][[group]] <- entry
        content$diagnostics <- c(
          content$diagnostics,
          "incompatible_group"
        )
      }
    }
  }
  usable <- any(vapply(
    content$normalized,
    function(method) {
      any(vapply(
        method,
        function(entry) {
          isTRUE(entry$valid) && isTRUE(entry$group_compatible)
        },
        logical(1)
      ))
    },
    logical(1)
  ))
  .builder_table_record(
    detected = TRUE,
    valid = content$valid && usable,
    normalized = content$normalized,
    diagnostics = content$diagnostics,
    requirements = requirements,
    page_candidates = if (usable) "enriched_pathways" else character()
  )
}

# Monocle 2 trajectories ----

.builder_table_explicit_row_names <- function(table) {
  row_names <- table$row_names
  if (
    !is.character(row_names) ||
      is.object(row_names) ||
      isS4(row_names) ||
      length(row_names) != table$rows
  ) {
    return(character())
  }
  row_names
}

.builder_table_trajectory_identity <- function(ids, expected) {
  diagnostics <- character()
  if (!length(ids)) {
    diagnostics <- c(diagnostics, "missing_cell_barcodes")
  }
  if (anyNA(ids) || any(!nzchar(ids))) {
    diagnostics <- c(diagnostics, "blank_cell_barcodes")
  }
  if (anyDuplicated(ids)) {
    diagnostics <- c(diagnostics, "duplicate_cell_barcodes")
  }
  extra <- setdiff(ids[!is.na(ids) & nzchar(ids)], expected)
  if (length(extra)) {
    diagnostics <- c(diagnostics, "extra_cell_barcodes")
  }
  usable <- !length(diagnostics)
  relation <- if (!usable) {
    "invalid"
  } else if (length(ids) == length(expected) && setequal(ids, expected)) {
    "complete"
  } else {
    "subset"
  }
  list(
    valid = usable,
    count = length(ids),
    relation = relation,
    coverage = if (length(expected)) {
      length(intersect(ids, expected)) / length(expected)
    } else {
      0
    },
    order_matches = identical(ids, expected),
    diagnostics = unique(diagnostics)
  )
}

.builder_table_required_columns <- function(table, required, code) {
  missing <- setdiff(required, table$columns)
  if (length(missing)) code else character()
}

.builder_table_numeric_columns_valid <- function(table, columns) {
  values <- lapply(columns, function(name) {
    .builder_table_column_numeric(table, name)
  })
  list(
    numeric = all(vapply(values, Negate(is.null), logical(1))),
    finite = all(vapply(
      values,
      function(value) {
        !is.null(value) && all(is.finite(value))
      },
      logical(1)
    ))
  )
}

## Trajectory methods whose payload is a cell-aligned `meta` (DR_1, DR_2,
## pseudotime, state) plus `edges`: monocle2, and what scanpy / scVelo files
## carry (PAGA, velocity, diffusion pseudotime).
.builder_trajectory_methods <- c("monocle2", "paga", "scvelo", "pseudotime")

## A plain `pseudotime` trajectory has no graph, so its `edges` may be empty.
.builder_table_monocle2 <- function(
  payload,
  expected_cells,
  allow_empty_edges = FALSE
) {
  diagnostics <- character()
  if (!.builder_table_plain_list(payload)) {
    return(list(
      supported = TRUE,
      valid = FALSE,
      cell_relation = "invalid",
      cells = list(count = 0L, coverage = 0),
      meta = list(rows = 0L, columns = character()),
      edges = list(rows = 0L, columns = character()),
      diagnostics = "unsafe_trajectory_payload"
    ))
  }
  component_names <- .builder_table_list_names(payload)
  diagnostics <- c(
    diagnostics,
    .builder_table_name_diagnostics(component_names, "trajectory_component")
  )
  if (length(diagnostics)) {
    return(list(
      supported = TRUE,
      valid = FALSE,
      cell_relation = "invalid",
      cells = list(count = 0L, coverage = 0),
      meta = list(rows = 0L, columns = character()),
      edges = list(rows = 0L, columns = character()),
      diagnostics = unique(diagnostics)
    ))
  }
  required_components <- c("meta", "edges")
  if (length(setdiff(required_components, component_names))) {
    diagnostics <- c(diagnostics, "missing_trajectory_components")
  }
  if (length(setdiff(component_names, required_components))) {
    diagnostics <- c(diagnostics, "unexpected_trajectory_components")
  }

  meta <- if ("meta" %in% component_names) {
    .builder_table_data_frame(payload[[match("meta", component_names)]])
  } else {
    .builder_table_data_frame(NULL)
  }
  edges <- if ("edges" %in% component_names) {
    .builder_table_data_frame(payload[[match("edges", component_names)]])
  } else {
    .builder_table_data_frame(NULL)
  }
  if (!meta$valid || meta$rows == 0L) {
    diagnostics <- c(diagnostics, "invalid_trajectory_meta")
  }
  if (!edges$valid || (edges$rows == 0L && !isTRUE(allow_empty_edges))) {
    diagnostics <- c(diagnostics, "invalid_trajectory_edges")
  }
  meta_required <- c("DR_1", "DR_2", "pseudotime", "state")
  edge_required <- c(
    "source",
    "target",
    "weight",
    "source_dim_1",
    "source_dim_2",
    "target_dim_1",
    "target_dim_2"
  )
  diagnostics <- c(
    diagnostics,
    .builder_table_required_columns(
      meta,
      meta_required,
      "missing_meta_columns"
    ),
    .builder_table_required_columns(
      edges,
      edge_required,
      "missing_edge_columns"
    )
  )

  meta_numeric <- .builder_table_numeric_columns_valid(
    meta,
    c("DR_1", "DR_2", "pseudotime")
  )
  if (!meta_numeric$numeric) {
    diagnostics <- c(diagnostics, "non_numeric_meta")
  } else if (!meta_numeric$finite) {
    diagnostics <- c(diagnostics, "non_finite_meta")
  }
  edge_numeric <- .builder_table_numeric_columns_valid(
    edges,
    c(
      "weight",
      "source_dim_1",
      "source_dim_2",
      "target_dim_1",
      "target_dim_2"
    )
  )
  if (!edge_numeric$numeric) {
    diagnostics <- c(diagnostics, "non_numeric_edges")
  } else if (!edge_numeric$finite) {
    diagnostics <- c(diagnostics, "non_finite_edges")
  }
  source <- .builder_table_column_text(edges, "source")
  target <- .builder_table_column_text(edges, "target")
  if (
    is.null(source) ||
      is.null(target) ||
      anyNA(source) ||
      anyNA(target) ||
      any(!nzchar(source)) ||
      any(!nzchar(target))
  ) {
    diagnostics <- c(diagnostics, "invalid_edge_nodes")
  }
  state_index <- match("state", meta$columns)
  state <- if (!is.na(state_index) && meta$valid) {
    .builder_table_factor_values(meta$data[[state_index]])
  } else {
    NULL
  }
  if (is.null(state) || anyNA(state) || any(!nzchar(state))) {
    diagnostics <- c(diagnostics, "invalid_state_levels")
  }

  identity <- .builder_table_trajectory_identity(
    .builder_table_explicit_row_names(meta),
    expected_cells
  )
  diagnostics <- c(diagnostics, identity$diagnostics)
  meta_columns <- .builder_table_column_summary(meta)
  edge_columns <- .builder_table_column_summary(edges)
  state_counts <- if (!is.null(state)) {
    sort(table(state, useNA = "no"), decreasing = TRUE)
  } else {
    integer()
  }
  state_items <- lapply(seq_along(state_counts), function(index) {
    list(
      value = names(state_counts)[[index]],
      count = as.integer(state_counts[[index]])
    )
  })
  list(
    supported = TRUE,
    valid = !length(diagnostics),
    cell_relation = identity$relation,
    cells = list(
      count = identity$count,
      coverage = identity$coverage,
      order_matches = identity$order_matches
    ),
    meta = c(list(rows = meta$rows), meta_columns),
    edges = c(list(rows = edges$rows), edge_columns),
    states = list(
      count = as.integer(length(state_counts)),
      items = state_items
    ),
    diagnostics = unique(diagnostics)
  )
}

.builder_table_trajectory_content <- function(value, expected_cells) {
  requirements <- c("monocle2", "meta", "edges", "cell_identity")
  preflight <- .builder_table_content_preflight(
    value,
    requirements,
    max_entries = 512L,
    max_outer = 64L,
    nested = TRUE,
    name_label = "method"
  )
  if (!is.null(preflight$record)) {
    return(preflight$record)
  }
  method_names <- preflight$names
  diagnostics <- character()

  normalized <- list()
  usable <- FALSE
  for (method_index in seq_along(value)) {
    method <- method_names[[method_index]]
    entries <- value[[method_index]]
    if (!.builder_table_plain_list(entries) || !length(entries)) {
      normalized[[method]] <- list()
      diagnostics <- c(diagnostics, "invalid_nested_container")
      next
    }
    entry_names <- .builder_table_list_names(entries)
    entry_diagnostics <- .builder_table_name_diagnostics(
      entry_names,
      "trajectory"
    )
    if (length(entry_diagnostics)) {
      normalized[[method]] <- list()
      diagnostics <- c(diagnostics, entry_diagnostics)
      next
    }
    summaries <- list()
    for (entry_index in seq_along(entries)) {
      name <- entry_names[[entry_index]]
      summary <- if (method %in% .builder_trajectory_methods) {
        .builder_table_monocle2(
          entries[[entry_index]],
          expected_cells,
          allow_empty_edges = identical(method, "pseudotime")
        )
      } else {
        list(
          supported = FALSE,
          valid = FALSE,
          cell_relation = "not_checked",
          cells = list(count = 0L, coverage = 0),
          meta = list(rows = 0L, columns = character()),
          edges = list(rows = 0L, columns = character()),
          diagnostics = "unsupported_method"
        )
      }
      summaries[[name]] <- summary
      diagnostics <- c(diagnostics, summary$diagnostics)
      usable <- usable || isTRUE(summary$valid)
    }
    normalized[[method]] <- summaries
  }
  .builder_table_record(
    detected = TRUE,
    valid = !length(diagnostics) && usable,
    normalized = normalized,
    diagnostics = unique(diagnostics),
    requirements = requirements,
    page_candidates = if (usable) "trajectory" else character()
  )
}

builder_trajectory_catalog <- function(trajectory) {
  scalar_or <- function(value, fallback) {
    if (is.null(value) || !length(value)) fallback else value[[1L]]
  }
  normalized <- if (is.list(trajectory)) {
    trajectory$normalized
  } else {
    NULL
  }
  if (!is.list(normalized) || !length(normalized)) {
    return(list())
  }
  catalog <- list()
  for (method_index in seq_along(normalized)) {
    method <- names(normalized)[[method_index]]
    entries <- normalized[[method_index]]
    if (!is.list(entries) || !length(entries)) {
      next
    }
    for (entry_index in seq_along(entries)) {
      name <- names(entries)[[entry_index]]
      summary <- entries[[entry_index]]
      supported <- isTRUE(summary$supported) &&
        method %in% .builder_trajectory_methods
      valid <- isTRUE(summary$valid)
      selectable <- supported && valid
      catalog[[length(catalog) + 1L]] <- list(
        method = method,
        name = name,
        supported = supported,
        valid = valid,
        selectable = selectable,
        cell_count = as.integer(scalar_or(summary$cells$count, 0L)),
        coverage = as.numeric(scalar_or(summary$cells$coverage, 0)),
        state_count = as.integer(scalar_or(summary$states$count, 0L)),
        edge_count = as.integer(scalar_or(summary$edges$rows, 0L)),
        reason = if (selectable) {
          NULL
        } else if (!supported) {
          "Not supported by this Viewer version."
        } else {
          "This trajectory did not pass Viewer compatibility checks."
        }
      )
    }
  }
  catalog
}

# Extra material ----

.builder_table_plot_summary <- function(value) {
  class <- attr(value, "class", exact = TRUE)
  safe_class <- is.character(class) && !is.object(class) && !isS4(class)
  if (safe_class) {
    attributes(class) <- NULL
  } else {
    class <- character()
  }
  allowed <- c(
    "ggplot2::ggplot",
    "ggplot",
    "ggplot2::gg",
    "S7_object",
    "gg"
  )
  storage <- typeof(value)
  supported_storage <- storage %in%
    c("list", "object") ||
    (identical(storage, "S4") && "S7_object" %in% class)
  recognized <- safe_class &&
    !anyNA(class) &&
    "ggplot" %in% class &&
    "gg" %in% class &&
    all(class %in% allowed) &&
    supported_storage
  class_count <- length(class)
  class_shown <- min(class_count, .builder_table_preview_limit)
  class_preview <- .builder_table_bound_preview_text(
    class[seq_len(class_shown)]
  )
  list(
    kind = "plot",
    class_count = as.integer(class_count),
    class_preview = class_preview$value,
    class_truncated_count = as.integer(
      class_count - class_shown + class_preview$truncated_count
    ),
    recognized = recognized,
    valid = recognized,
    diagnostics = if (recognized) character() else "unsupported_plot"
  )
}

.builder_table_extra_category <- function(value, category) {
  preflight <- .builder_table_content_preflight(
    value,
    character(),
    invalid_diagnostic = "invalid_material_category",
    max_entries = 512L,
    name_label = "material",
    empty_diagnostic = "invalid_material_category",
    container_required = TRUE
  )
  if (!is.null(preflight$record)) {
    return(list(
      valid = FALSE,
      normalized = list(),
      diagnostics = preflight$record$diagnostics,
      usable = FALSE
    ))
  }
  entry_names <- preflight$names
  diagnostics <- character()
  normalized <- list()
  usable <- FALSE
  for (index in seq_along(value)) {
    name <- entry_names[[index]]
    summary <- if (identical(category, "tables")) {
      table <- .builder_table_data_frame(value[[index]])
      .builder_table_summary(table)
    } else {
      .builder_table_plot_summary(value[[index]])
    }
    normalized[[name]] <- summary
    diagnostics <- c(diagnostics, summary$diagnostics)
    usable <- usable || isTRUE(summary$valid)
  }
  list(
    valid = !length(diagnostics) && usable,
    normalized = normalized,
    diagnostics = unique(diagnostics),
    usable = usable
  )
}

.builder_table_extra_content <- function(value) {
  requirements <- c("named_tables_or_plots", "unique_names")
  preflight <- .builder_table_content_preflight(
    value,
    requirements,
    name_label = "category"
  )
  if (!is.null(preflight$record)) {
    return(preflight$record)
  }
  # table_index is optional display metadata written by builder_attach_tables,
  # not a material category. Export normalizes it separately from table data.
  category_names <- setdiff(preflight$names, "table_index")
  diagnostics <- character()
  supported <- c("tables", "plots")
  if (length(setdiff(category_names, supported))) {
    diagnostics <- c(diagnostics, "unsupported_material_category")
  }
  normalized <- list()
  usable <- FALSE
  for (category in intersect(supported, unique(category_names))) {
    summary <- .builder_table_extra_category(
      value[[category]],
      category
    )
    normalized[[category]] <- summary$normalized
    diagnostics <- c(diagnostics, summary$diagnostics)
    usable <- usable || summary$usable
  }
  .builder_table_record(
    detected = TRUE,
    valid = !length(diagnostics) && usable,
    normalized = normalized,
    diagnostics = unique(diagnostics),
    requirements = requirements,
    page_candidates = if (usable) "extra_material" else character()
  )
}

# Public assembly ----

builder_profile_table_content <- function(object, context) {
  misc <- .builder_table_misc(object)
  if (!.builder_table_plain_list(misc)) {
    invalid <- .builder_table_record(
      detected = TRUE,
      valid = FALSE,
      normalized = NULL,
      diagnostics = "unsafe_misc_container"
    )
    return(list(
      marker_genes = invalid,
      most_expressed_genes = invalid,
      mean_expression = invalid,
      enriched_pathways = invalid,
      trajectory = invalid,
      gene_lists = invalid,
      extra_material = invalid
    ))
  }
  groups <- .builder_table_context_groups(context)
  cells <- .builder_table_context_cells(context)
  list(
    marker_genes = .builder_table_marker_content(
      misc[["marker_genes"]],
      groups
    ),
    most_expressed_genes = .builder_table_metric_content(
      misc[["most_expressed_genes"]],
      groups,
      metric = "pct",
      page = "most_expressed_genes"
    ),
    mean_expression = .builder_table_metric_content(
      misc[["mean_expression"]],
      groups,
      metric = "mean_expr"
    ),
    enriched_pathways = .builder_table_enrichment_content(
      misc[["enriched_pathways"]],
      groups
    ),
    trajectory = .builder_table_trajectory_content(
      misc[["trajectories"]],
      cells
    ),
    gene_lists = .builder_table_gene_lists_content(misc[["gene_lists"]]),
    extra_material = .builder_table_extra_content(
      misc[["extra_material"]]
    )
  )
}
