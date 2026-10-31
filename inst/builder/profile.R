##----------------------------------------------------------------------------##
## Versioned, structural facts about one Seurat data set.
##
## Every cell-associated component is compared with Cells(object) by barcode.
## Counts and row positions are diagnostics, never identity contracts.
##----------------------------------------------------------------------------##

# Source ----

.builder_profile_abort <- function(code, message) {
  condition <- structure(
    list(message = message, call = NULL, code = code),
    class = c("builder_profile_error", "error", "condition")
  )
  stop(condition)
}

.builder_profile_scalar_text <- function(value, allow_empty = FALSE) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    (isTRUE(allow_empty) || nzchar(value))
}

.builder_profile_plain_data <- function(value, depth = 0L) {
  if (depth > 20L) {
    return(FALSE)
  }
  if (
    is.object(value) ||
      is.function(value) ||
      is.environment(value) ||
      is.language(value) ||
      is.symbol(value) ||
      typeof(value) == "externalptr"
  ) {
    return(FALSE)
  }
  if (is.list(value)) {
    return(all(vapply(
      value,
      .builder_profile_plain_data,
      logical(1),
      depth = depth + 1L
    )))
  }
  is.null(value) || is.atomic(value)
}

builder_profile_source <- function(source) {
  if (
    !is.list(source) ||
      is.object(source) ||
      !.builder_profile_plain_data(source)
  ) {
    .builder_profile_abort(
      "invalid_source",
      "A dataset source must be an inert data record."
    )
  }
  if (
    !.builder_profile_scalar_text(source[["type"]]) ||
      !.builder_profile_scalar_text(source[["location"]])
  ) {
    .builder_profile_abort(
      "invalid_source",
      "A dataset source requires type and location strings."
    )
  }

  clean_text <- function(value) {
    value <- as.character(value)
    attributes(value) <- NULL
    value
  }

  optional_text <- function(name) {
    value <- source[[name]]
    if (is.null(value)) {
      return(NULL)
    }
    if (!.builder_profile_scalar_text(value)) {
      .builder_profile_abort(
        "invalid_source",
        paste("Dataset source", name, "must be a non-empty string.")
      )
    }
    clean_text(value)
  }

  list(
    type = clean_text(source[["type"]]),
    location = clean_text(source[["location"]]),
    fingerprint = optional_text("fingerprint"),
    format = optional_text("format")
  )
}

# Identity ----

.builder_profile_ids <- function(ids) {
  if (is.null(ids)) {
    return(character())
  }
  as.character(ids)
}

builder_axis_identity <- function(ids) {
  ids <- unname(.builder_profile_ids(ids))
  present <- !is.na(ids)
  ids[present] <- enc2utf8(ids[present])
  path <- tempfile("cerebro-builder-axis-")
  connection <- file(path, open = "wb")
  Sys.chmod(path, mode = "0600")
  closed <- FALSE
  on.exit(
    {
      if (!closed) {
        try(close(connection), silent = TRUE)
      }
      if (file.exists(path)) {
        unlink(path, force = TRUE)
      }
    },
    add = TRUE
  )
  # A length-prefixed UTF-8 stream keeps saved identities stable across R versions.
  header <- paste0("CEREBRO_AXIS_IDENTITY_V1:", length(ids), ":")
  writeBin(charToRaw(header), connection)
  if (length(ids)) {
    chunk_size <- 10000L
    for (start in seq.int(1L, length(ids), by = chunk_size)) {
      end <- min(length(ids), start + chunk_size - 1L)
      values <- ids[start:end]
      available <- !is.na(values)
      records <- rep("-1:", length(values))
      if (any(available)) {
        records[available] <- paste0(
          nchar(values[available], type = "bytes"),
          ":",
          values[available]
        )
      }
      writeBin(charToRaw(paste0(records, collapse = "")), connection)
    }
  }
  close(connection)
  closed <- TRUE
  md5 <- unname(as.character(tools::md5sum(path)))
  if (
    length(md5) != 1L ||
      is.na(md5) ||
      !grepl("^[[:xdigit:]]{32}$", md5)
  ) {
    stop("Dataset identity could not be summarized safely.", call. = FALSE)
  }
  list(
    schema_version = 2L,
    count = as.double(length(ids)),
    md5 = tolower(md5)
  )
}

builder_axis_identity_valid <- function(value) {
  is.list(value) &&
    is.numeric(value$schema_version) &&
    length(value$schema_version) == 1L &&
    !is.na(value$schema_version) &&
    is.finite(value$schema_version) &&
    value$schema_version == 2 &&
    is.numeric(value$count) &&
    length(value$count) == 1L &&
    !is.na(value$count) &&
    is.finite(value$count) &&
    value$count >= 0 &&
    value$count == floor(value$count) &&
    is.character(value$md5) &&
    length(value$md5) == 1L &&
    !is.na(value$md5) &&
    grepl("^[[:xdigit:]]{32}$", value$md5)
}

builder_axis_identity_normalize <- function(value) {
  if (!builder_axis_identity_valid(value)) {
    return(NULL)
  }
  list(
    schema_version = 2L,
    count = as.double(value$count),
    md5 = tolower(value$md5)
  )
}

builder_identity_profile <- function(ids, expected = ids) {
  match <- builder_match_cells(ids, expected, mode = "exact")

  list(
    ids = match$ids,
    count = match$count,
    valid = match$valid,
    duplicates = match$duplicates,
    blanks = match$blanks,
    missing = match$missing,
    extra = match$extra,
    order_matches = match$order_matches,
    coverage = match$coverage,
    canonical_ids = match$canonical_ids,
    reorder_index = match$reorder_index
  )
}

builder_feature_profile <- function(ids, expected) {
  profile <- builder_identity_profile(ids, expected)
  usable_ids <- profile$ids[!is.na(profile$ids) & nzchar(profile$ids)]
  folded <- toupper(usable_ids)
  collision_keys <- unique(folded[duplicated(folded)])
  casefold_duplicates <- usable_ids[folded %in% collision_keys]
  structurally_usable <- profile$count > 0L &&
    !length(profile$blanks) &&
    !length(profile$duplicates) &&
    !length(casefold_duplicates) &&
    !length(profile$extra)
  relation <- if (!structurally_usable) {
    "invalid"
  } else if (profile$valid) {
    "complete"
  } else {
    "subset"
  }
  profile$casefold_duplicates <- casefold_duplicates
  profile$relation <- relation
  profile$usable <- !identical(relation, "invalid")
  profile
}

.builder_profile_identity_diagnostics <- function(identity) {
  diagnostics <- character()
  if (length(identity$blanks)) {
    diagnostics <- c(diagnostics, "blank_ids")
  }
  if (length(identity$duplicates)) {
    diagnostics <- c(diagnostics, "duplicate_ids")
  }
  if (length(identity$missing)) {
    diagnostics <- c(diagnostics, "missing_ids")
  }
  if (length(identity$extra)) {
    diagnostics <- c(diagnostics, "extra_ids")
  }
  if (identity$valid && !identity$order_matches) {
    diagnostics <- c(diagnostics, "reordered")
  }
  diagnostics
}

.builder_profile_identity_record <- function(value) {
  is.list(value) &&
    all(
      c("ids", "count", "valid", "canonical_ids", "reorder_index") %in%
        names(value)
    )
}

.builder_profile_compact_identity <- function(
  value,
  exact = FALSE,
  sample_max = 20L
) {
  ids <- value$ids %||% character()
  canonical <- value$canonical_ids %||% ids
  if (isTRUE(exact)) {
    value$axis_identity <- builder_axis_identity(canonical)
  }
  value$sample_ids <- unname(utils::head(ids, sample_max))
  for (field in c(
    "duplicates",
    "blanks",
    "missing",
    "extra",
    "casefold_duplicates"
  )) {
    if (!is.null(value[[field]])) {
      value[[paste0(field, "_count")]] <- as.double(length(value[[field]]))
      value[[field]] <- unname(utils::head(value[[field]], 20L))
    }
  }
  for (field in c("ids", "canonical_ids", "reorder_index")) {
    value[[field]] <- NULL
  }
  value
}

builder_profile_workspace_contract <- function(profile) {
  if (
    !inherits(profile, "builder_dataset_profile") ||
      !is.list(profile$identity)
  ) {
    .builder_profile_abort(
      "invalid_profile",
      "A DatasetProfile v2 record is required for workspace transport."
    )
  }
  if (.builder_profile_identity_record(profile$identity$cells)) {
    profile$identity$cells <- .builder_profile_compact_identity(
      profile$identity$cells,
      exact = TRUE
    )
  }
  if (.builder_profile_identity_record(profile$identity$features)) {
    profile$identity$features <- .builder_profile_compact_identity(
      profile$identity$features,
      exact = TRUE,
      sample_max = 500L
    )
  }
  if (.builder_profile_identity_record(profile$identity$metadata)) {
    profile$identity$metadata <- .builder_profile_compact_identity(
      profile$identity$metadata
    )
  }
  for (assay_index in seq_along(profile$assays)) {
    assay <- profile$assays[[assay_index]]
    for (layer_index in seq_along(assay$layers)) {
      layer <- assay$layers[[layer_index]]
      for (axis in c("cells", "features")) {
        if (.builder_profile_identity_record(layer[[axis]])) {
          layer[[axis]] <- .builder_profile_compact_identity(
            layer[[axis]],
            exact = identical(axis, "features")
          )
        }
      }
      if (is.list(layer$feature_members)) {
        layer$feature_members <- lapply(
          layer$feature_members,
          function(value) {
            if (.builder_profile_identity_record(value)) {
              .builder_profile_compact_identity(value)
            } else {
              value
            }
          }
        )
      }
      assay$layers[[layer_index]] <- layer
    }
    profile$assays[[assay_index]] <- assay
  }
  for (field in c("identity", "column_identity")) {
    if (.builder_profile_identity_record(profile$metadata[[field]])) {
      profile$metadata[[field]] <- .builder_profile_compact_identity(
        profile$metadata[[field]]
      )
    }
  }
  for (index in seq_along(profile$reductions)) {
    reduction <- profile$reductions[[index]]
    if (.builder_profile_identity_record(reduction$cells)) {
      reduction$cells <- .builder_profile_compact_identity(reduction$cells)
    }
    profile$reductions[[index]] <- reduction
  }
  if (is.list(profile$manifest[["dataset_identity"]])) {
    profile$manifest[["dataset_identity"]]$diagnostics <- profile$identity
  }
  if (is.list(profile$manifest[["metadata"]])) {
    profile$manifest[["metadata"]]$diagnostics <- list(
      rows = profile$metadata$identity,
      columns = profile$metadata$column_identity
    )
  }
  for (name in names(profile$reductions)) {
    id <- paste0("reduction:", name)
    if (is.list(profile$manifest[[id]])) {
      profile$manifest[[id]]$diagnostics <- profile$reductions[[name]]
    }
  }
  profile
}

# Assays & layers ----

.builder_profile_layer_root <- function(layer) {
  if (startsWith(layer, "scale.data.")) {
    return("scale.data")
  }
  sub("[.].*$", "", layer)
}

.builder_profile_layer_api <- function() {
  namespace <- asNamespace("SeuratObject")
  required <- c("Layers", "Cells", "Features")
  if (
    !all(vapply(
      required,
      exists,
      logical(1),
      envir = namespace,
      inherits = FALSE
    ))
  ) {
    return(NULL)
  }
  list(
    layers = get("Layers", envir = namespace, inherits = FALSE),
    cells = get("Cells", envir = namespace, inherits = FALSE),
    features = get("Features", envir = namespace, inherits = FALSE)
  )
}

.builder_profile_legacy_layers <- function(assay) {
  candidates <- c("counts", "data", "scale.data")
  candidates[vapply(
    candidates,
    function(layer) {
      if (!layer %in% methods::slotNames(assay)) {
        return(FALSE)
      }
      value <- methods::slot(assay, layer)
      length(dim(value)) == 2L && all(dim(value) > 0L)
    },
    logical(1)
  )]
}

.builder_profile_layer_names <- function(assay, layer_api) {
  if (is.null(layer_api) || inherits(assay, "Assay")) {
    return(.builder_profile_legacy_layers(assay))
  }
  tryCatch(layer_api$layers(assay), error = function(error) character())
}

.builder_profile_layer_ids <- function(assay, layer, what, layer_api) {
  if (is.null(layer_api) || inherits(assay, "Assay")) {
    if (!layer %in% methods::slotNames(assay)) {
      return(character())
    }
    value <- methods::slot(assay, layer)
    return(if (identical(what, "cells")) colnames(value) else rownames(value))
  }
  method <- layer_api[[what]]
  tryCatch(method(assay, layer = layer), error = function(error) character())
}

.builder_profile_physical_layer <- function(
  assay,
  layer,
  expected_cells,
  expected_features,
  layer_api
) {
  cells <- builder_identity_profile(
    .builder_profile_layer_ids(assay, layer, "cells", layer_api),
    expected_cells
  )
  features <- builder_feature_profile(
    .builder_profile_layer_ids(assay, layer, "features", layer_api),
    expected_features
  )
  diagnostics <- c(
    .builder_profile_identity_diagnostics(cells),
    if (!features$count) "no_features" else character(),
    if (!features$usable) {
      "unusable_feature_ids"
    } else if (!features$valid) {
      "partial_feature_coverage"
    } else {
      character()
    }
  )
  list(
    name = layer,
    kind = "physical",
    members = layer,
    cells = cells,
    features = features,
    exportable = cells$valid && features$usable,
    diagnostics = unique(diagnostics)
  )
}

.builder_profile_find_partition <- function(
  expected_cells,
  memberships,
  max_solutions = 2L,
  max_nodes = 100000L,
  max_depth = 128L,
  max_conflict_work = 5000000
) {
  expected_cells <- as.character(expected_cells)
  if (
    !length(expected_cells) ||
      anyNA(expected_cells) ||
      any(!nzchar(expected_cells)) ||
      anyDuplicated(expected_cells)
  ) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list(),
      strategy = "validation"
    ))
  }
  memberships <- lapply(memberships, as.character)
  usable <- vapply(
    memberships,
    function(ids) {
      length(ids) > 0L &&
        length(ids) < length(expected_cells) &&
        !anyNA(ids) &&
        !any(!nzchar(ids)) &&
        !anyDuplicated(ids) &&
        !length(setdiff(ids, expected_cells))
    },
    logical(1)
  )
  memberships <- memberships[usable]
  if (length(memberships) < 2L) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list(),
      strategy = "linear"
    ))
  }
  memberships <- memberships[order(names(memberships), method = "radix")]
  expected_index <- seq_along(expected_cells)
  names(expected_index) <- expected_cells
  encoded <- lapply(memberships, function(ids) {
    unname(expected_index[ids])
  })
  claims <- unlist(encoded, use.names = FALSE)
  if (
    length(claims) == length(expected_cells) &&
      !anyDuplicated(claims) &&
      setequal(claims, seq_along(expected_cells))
  ) {
    solution <- names(memberships)
    return(list(
      status = "unique",
      layers = solution,
      solutions = list(solution),
      strategy = "linear"
    ))
  }

  layer_index <- seq_along(encoded)
  claims_by_layer <- rep.int(layer_index, lengths(encoded))
  cell_to_layers <- split(
    claims_by_layer,
    factor(claims, levels = seq_along(expected_cells))
  )
  conflict_work <- unname(sum(lengths(cell_to_layers)^2))
  if (conflict_work > max_conflict_work) {
    return(list(
      status = "budget_exceeded",
      layers = character(),
      solutions = list(),
      strategy = "search",
      budget_reason = "conflict_work",
      conflict_work = conflict_work,
      nodes = 0L
    ))
  }
  conflicts <- lapply(encoded, function(ids) {
    unique(unlist(cell_to_layers[ids], use.names = FALSE))
  })

  solutions <- list()
  nodes <- 0L
  budget_exceeded <- FALSE
  budget_reason <- NULL
  search <- function(covered, available, chosen, depth = 0L) {
    if (budget_exceeded) {
      return(invisible(NULL))
    }
    nodes <<- nodes + 1L
    if (nodes > max_nodes) {
      budget_exceeded <<- TRUE
      budget_reason <<- "nodes"
      return(invisible(NULL))
    }
    if (length(solutions) >= max_solutions) {
      return(invisible(NULL))
    }
    if (all(covered)) {
      if (length(chosen) >= 2L) {
        solutions[[length(solutions) + 1L]] <<- sort(
          names(memberships)[chosen],
          method = "radix"
        )
      }
      return(invisible(NULL))
    }
    if (depth >= max_depth) {
      budget_exceeded <<- TRUE
      budget_reason <<- "depth"
      return(invisible(NULL))
    }
    pivot <- which(!covered)[1L]
    choices <- intersect(cell_to_layers[[pivot]], available)
    for (choice in choices) {
      ids <- encoded[[choice]]
      remaining <- setdiff(available, conflicts[[choice]])
      next_covered <- covered
      next_covered[ids] <- TRUE
      search(
        next_covered,
        remaining,
        c(chosen, choice),
        depth + 1L
      )
      if (budget_exceeded || length(solutions) >= max_solutions) {
        break
      }
    }
    invisible(NULL)
  }
  search(
    rep(FALSE, length(expected_cells)),
    layer_index,
    integer()
  )
  if (budget_exceeded) {
    return(list(
      status = "budget_exceeded",
      layers = character(),
      solutions = solutions,
      strategy = "search",
      budget_reason = budget_reason,
      conflict_work = conflict_work,
      nodes = nodes
    ))
  }
  if (!length(solutions)) {
    return(list(
      status = "none",
      layers = character(),
      solutions = list(),
      strategy = "search"
    ))
  }
  keys <- vapply(solutions, paste, collapse = "\r", character(1))
  solutions <- solutions[!duplicated(keys)]
  if (length(solutions) == 1L) {
    return(list(
      status = "unique",
      layers = solutions[[1L]],
      solutions = solutions,
      strategy = "search"
    ))
  }
  list(
    status = "ambiguous",
    layers = character(),
    solutions = solutions,
    strategy = "search"
  )
}

.builder_profile_logical_layer <- function(
  root,
  members,
  physical,
  expected_cells,
  expected_features
) {
  direct <- members[
    !grepl(
      ".",
      substring(members, nchar(root) + 2L),
      fixed = TRUE
    )
  ]
  member_cells <- lapply(direct, function(member) {
    physical[[member]]$cells$ids
  })
  names(member_cells) <- direct
  partition <- .builder_profile_find_partition(expected_cells, member_cells)
  selected <- if (identical(partition$status, "unique")) {
    partition$layers
  } else {
    direct
  }
  cells <- builder_identity_profile(
    unlist(member_cells[selected], use.names = FALSE),
    expected_cells
  )
  feature_members <- lapply(selected, function(member) {
    physical[[member]]$features
  })
  names(feature_members) <- selected
  feature_members_usable <- length(feature_members) > 0L &&
    all(vapply(
      feature_members,
      function(features) features$usable,
      logical(1)
    ))
  feature_union <- unique(unlist(
    lapply(
      feature_members,
      function(features) features$ids
    ),
    use.names = FALSE
  ))
  features <- builder_feature_profile(feature_union, expected_features)
  features$usable <- feature_members_usable && features$usable
  feature_keys <- vapply(
    feature_members,
    function(features) paste(sort(features$ids), collapse = "\r"),
    character(1)
  )
  heterogeneous_features <- length(unique(feature_keys)) > 1L
  diagnostics <- .builder_profile_identity_diagnostics(cells)
  if (identical(partition$status, "ambiguous")) {
    diagnostics <- c(diagnostics, "ambiguous_partition")
  } else if (identical(partition$status, "budget_exceeded")) {
    diagnostics <- c(diagnostics, "partition_search_budget_exceeded")
  } else if (identical(partition$status, "none")) {
    diagnostics <- c(diagnostics, "incomplete_partition")
  }
  if (length(setdiff(members, direct))) {
    diagnostics <- c(diagnostics, "nested_candidates_deferred")
  }
  if (!features$usable) {
    diagnostics <- c(diagnostics, "unusable_feature_ids")
  } else if (heterogeneous_features) {
    diagnostics <- c(diagnostics, "incompatible_split_feature_sets")
  } else if (!features$valid) {
    diagnostics <- c(diagnostics, "partial_feature_coverage")
  }
  list(
    name = root,
    kind = "logical",
    members = selected,
    candidate_members = direct,
    nested_candidates = setdiff(members, direct),
    partition_status = partition$status,
    solutions = partition$solutions,
    cells = cells,
    features = features,
    feature_members = feature_members,
    heterogeneous_features = heterogeneous_features,
    exportable = identical(partition$status, "unique") &&
      cells$valid &&
      features$usable &&
      !heterogeneous_features,
    diagnostics = unique(diagnostics)
  )
}

builder_profile_assay <- function(
  assay,
  expected_cells,
  name = NULL,
  expected_features = NULL,
  layer_api = .builder_profile_layer_api()
) {
  if (is.null(expected_features)) {
    expected_features <- tryCatch(
      if (is.null(layer_api)) rownames(assay) else layer_api$features(assay),
      error = function(error) rownames(assay)
    )
  }
  layers <- .builder_profile_layer_names(assay, layer_api)
  physical <- lapply(layers, function(layer) {
    .builder_profile_physical_layer(
      assay,
      layer,
      expected_cells,
      expected_features,
      layer_api
    )
  })
  names(physical) <- layers

  roots <- unique(vapply(layers, .builder_profile_layer_root, character(1)))
  logical <- list()
  for (root in roots) {
    members <- layers[startsWith(layers, paste0(root, "."))]
    if (length(members) < 2L || root %in% layers) {
      next
    }
    logical[[root]] <- .builder_profile_logical_layer(
      root,
      members,
      physical,
      expected_cells,
      expected_features
    )
  }
  profiles <- c(physical, logical)
  exportable <- names(profiles)[vapply(
    profiles,
    function(layer) isTRUE(layer$exportable),
    logical(1)
  )]

  list(
    name = name,
    layers = profiles,
    exportable_layers = exportable,
    exportable = length(exportable) > 0L
  )
}

builder_profile_assays <- function(object, expected_cells) {
  assays <- names(object@assays)
  profiles <- lapply(assays, function(name) {
    assay <- object[[name]]
    expected_features <- tryCatch(
      SeuratObject::Features(assay),
      error = function(error) rownames(assay)
    )
    builder_profile_assay(
      assay,
      expected_cells,
      name,
      expected_features = expected_features
    )
  })
  names(profiles) <- assays
  profiles
}

# Metadata ----

BUILDER_METADATA_SAMPLE_MAX <- 5L
BUILDER_METADATA_LEVEL_MAX <- 12L
BUILDER_METADATA_TEXT_MAX_BYTES <- 120L

.builder_profile_bounded_text <- function(
  value,
  max_bytes = BUILDER_METADATA_TEXT_MAX_BYTES
) {
  if (is.null(value) || !length(value)) {
    value <- ""
  } else {
    value <- value[[1L]]
  }
  value <- enc2utf8(as.character(value))
  attributes(value) <- NULL
  if (nchar(value, type = "bytes") <= max_bytes) {
    return(value)
  }
  suffix <- "..."
  keep <- max(1L, min(nchar(value), max_bytes - nchar(suffix)))
  candidate <- substr(value, 1L, keep)
  while (
    keep > 1L &&
      nchar(candidate, type = "bytes") + nchar(suffix) > max_bytes
  ) {
    keep <- keep - 1L
    candidate <- substr(value, 1L, keep)
  }
  paste0(candidate, suffix)
}

.builder_profile_sample_values <- function(values) {
  values <- utils::head(values, BUILDER_METADATA_SAMPLE_MAX)
  text <- if (is.factor(values) || is.atomic(values)) {
    as.character(values)
  } else {
    rep(NA_character_, length(values))
  }
  text[is.na(text)] <- "N/A"
  text[!nzchar(text)] <- "(blank)"
  unname(vapply(
    text,
    .builder_profile_bounded_text,
    character(1)
  ))
}

.builder_profile_metadata_classification <- function(values, distinct) {
  if (!is.atomic(values) || is.list(values)) {
    return("unsupported")
  }
  if (is.factor(values) || is.character(values) || is.logical(values)) {
    return("categorical")
  }
  if (is.integer(values) && distinct <= 100L) {
    return("categorical")
  }
  if (is.numeric(values) || inherits(values, c("Date", "POSIXt"))) {
    return("continuous")
  }
  "other"
}

.builder_profile_level_counts <- function(values) {
  text <- if (is.factor(values) || is.atomic(values)) {
    as.character(values)
  } else {
    rep(NA_character_, length(values))
  }
  text[is.na(text)] <- "N/A"
  full_counts <- sort(table(text, useNA = "no"), decreasing = TRUE)
  display_names <- names(full_counts)
  display_names[!nzchar(display_names)] <- "(blank)"
  counts <- stats::setNames(as.numeric(full_counts), display_names)
  if (anyDuplicated(display_names)) {
    counts <- tapply(counts, display_names, sum)
  }
  counts <- sort(counts, decreasing = TRUE)
  total <- length(counts)
  shown <- utils::head(counts, BUILDER_METADATA_LEVEL_MAX)
  items <- lapply(seq_along(shown), function(index) {
    list(
      value = .builder_profile_bounded_text(names(shown)[[index]]),
      count = as.integer(shown[[index]])
    )
  })
  summary <- list(
    items = items,
    total = as.integer(total),
    truncated = total > length(shown),
    remaining_count = if (total > length(shown)) {
      as.integer(sum(counts[-seq_along(shown)]))
    } else {
      0L
    }
  )
  attr(summary, "builder_full_counts") <- full_counts
  summary
}

.builder_profile_group_reason_label <- function(reason, distinct) {
  if (is.null(reason)) {
    return(NULL)
  }
  if (identical(reason, "QC metric, not a group")) {
    return("Continuous quality-control values are kept as metadata.")
  }
  if (identical(reason, "only one value")) {
    return("Only one value was detected.")
  }
  if (identical(reason, "one value per cell")) {
    return("Nearly every cell has a different value.")
  }
  if (grepl("values; too many$", reason)) {
    return(paste0(
      "Too many distinct values (",
      distinct,
      ") for a Viewer group."
    ))
  }
  if (identical(reason, "continuous numeric metadata requires review")) {
    return("Continuous numeric values remain available as metadata.")
  }
  if (identical(reason, "unsupported metadata type")) {
    return("This metadata type cannot be used as a Viewer group.")
  }
  if (identical(reason, "metadata column name is missing or duplicated")) {
    return("This column needs a unique name before it can be a Viewer group.")
  }
  "This column remains available as metadata but cannot be a Viewer group."
}

.builder_profile_metadata_catalog_entry <- function(
  values,
  name,
  reason,
  stats = NULL
) {
  if (is.null(stats)) {
    missing <- is.na(values)
    stats <- list(
      distinct = length(unique(values[!missing])),
      missing = sum(missing)
    )
  }
  distinct <- stats$distinct
  missing <- stats$missing
  list(
    name = .builder_profile_bounded_text(name),
    classification = .builder_profile_metadata_classification(
      values,
      distinct
    ),
    group_eligible = is.null(reason),
    group_reason = .builder_profile_group_reason_label(reason, distinct),
    count = as.integer(length(values)),
    distinct_count = as.integer(distinct),
    missing_count = as.integer(missing),
    missing_percentage = if (length(values)) {
      100 * missing / length(values)
    } else {
      0
    },
    sample_values = .builder_profile_sample_values(values),
    level_counts = .builder_profile_level_counts(values)
  )
}

.builder_profile_metadata_column <- function(
  values,
  name,
  stats = NULL,
  full_counts = NULL
) {
  if (!is.null(stats) && !is.null(full_counts)) {
    blanks <- full_counts[names(full_counts) == ""]
    return(list(
      name = name,
      class = class(values),
      storage_type = typeof(values),
      count = length(values),
      missing = stats$missing,
      blanks = if (length(blanks)) as.integer(blanks[[1L]]) else 0L,
      unique = stats$distinct + as.integer(stats$missing > 0L),
      non_missing = length(values) - stats$missing,
      unique_non_missing = stats$distinct,
      supported = is.atomic(values) && !is.list(values)
    ))
  }
  as_text <- if (is.factor(values)) {
    as.character(values)
  } else if (is.atomic(values)) {
    as.character(values)
  } else {
    rep(NA_character_, length(values))
  }
  list(
    name = name,
    class = class(values),
    storage_type = typeof(values),
    count = length(values),
    missing = sum(is.na(values)),
    blanks = sum(!is.na(as_text) & !nzchar(as_text)),
    unique = length(unique(values)),
    non_missing = sum(!is.na(values)),
    unique_non_missing = length(unique(values[!is.na(values)])),
    supported = is.atomic(values) && !is.list(values)
  )
}

.builder_profile_group_reason <- function(
  values,
  name,
  n_cells,
  distinct = NULL
) {
  qc_pattern <- paste0(
    "^(nCount|nFeature|nUMI|nGene)_?|^total[._]?(count|UMI|gene)s?$|",
    "^percent[._]|[._]score$|^S\\.Score$|^G2M\\.Score$"
  )
  if (!is.atomic(values) || is.list(values)) {
    return("unsupported metadata type")
  }
  if (grepl(qc_pattern, name, ignore.case = TRUE)) {
    return("QC metric, not a group")
  }
  if (is.null(distinct)) {
    distinct <- length(unique(values[!is.na(values)]))
  }
  if (distinct < 2L) {
    return("only one value")
  }
  if (distinct >= n_cells) {
    return("one value per cell")
  }
  if (distinct > 100L) {
    return(paste0(distinct, " values; too many"))
  }
  if (
    !(is.factor(values) ||
      is.character(values) ||
      is.logical(values) ||
      is.integer(values))
  ) {
    return("continuous numeric metadata requires review")
  }
  NULL
}

builder_profile_metadata <- function(meta, expected_cells) {
  identity <- builder_identity_profile(rownames(meta), expected_cells)
  column_names <- colnames(meta)
  column_identity <- builder_identity_profile(column_names, column_names)
  columns <- list()

  candidates <- character()
  conversions <- character()
  rejected <- character()
  catalog <- list()
  legacy_columns <- character()
  legacy_distinct <- integer()
  legacy_counts <- list()
  legacy_levels <- list()
  if (!column_identity$valid) {
    keys <- column_names
    invalid_key <- is.na(keys) | !nzchar(keys)
    keys[invalid_key] <- paste0("<column_", which(invalid_key), ">")
    keys <- make.unique(keys)
    rejected <- stats::setNames(
      rep("metadata column name is missing or duplicated", length(keys)),
      keys
    )
    catalog <- lapply(seq_along(column_names), function(index) {
      values <- meta[[index]]
      missing <- is.na(values)
      stats <- list(
        distinct = length(unique(values[!missing])),
        missing = sum(missing)
      )
      entry <- .builder_profile_metadata_catalog_entry(
        values,
        keys[[index]],
        "metadata column name is missing or duplicated",
        stats
      )
      columns[[index]] <<- .builder_profile_metadata_column(
        values,
        column_names[[index]],
        stats,
        attr(entry$level_counts, "builder_full_counts")
      )
      attr(entry$level_counts, "builder_full_counts") <- NULL
      entry
    })
    names(catalog) <- keys
  } else {
    for (index in seq_along(column_names)) {
      name <- column_names[[index]]
      values <- meta[[index]]
      missing <- is.na(values)
      stats <- list(
        distinct = length(unique(values[!missing])),
        missing = sum(missing)
      )
      reason <- .builder_profile_group_reason(
        values,
        name,
        length(expected_cells),
        stats$distinct
      )
      entry <- .builder_profile_metadata_catalog_entry(
        values,
        name,
        reason,
        stats
      )
      full_counts <- attr(entry$level_counts, "builder_full_counts")
      columns[[index]] <- .builder_profile_metadata_column(
        values,
        name,
        stats,
        full_counts
      )
      attr(entry$level_counts, "builder_full_counts") <- NULL
      catalog[[name]] <- entry
      if (is.character(values) || is.factor(values) || is.integer(values)) {
        legacy_columns <- c(legacy_columns, name)
        legacy_distinct[[name]] <- length(full_counts)
        if (length(full_counts) <= 100L) {
          legacy_counts[[name]] <- full_counts
          if (is.factor(values)) {
            levels <- base::levels(values)
            if (anyNA(values) && !"N/A" %in% levels) {
              levels <- c(levels, "N/A")
            }
          } else {
            levels <- names(full_counts)
            if (anyNA(values)) {
              levels <- c(sort(setdiff(levels, "N/A")), "N/A")
            } else {
              levels <- sort(levels)
            }
          }
          legacy_levels[[name]] <- levels
        }
      }
      if (!is.null(reason)) {
        rejected[[name]] <- reason
        next
      }
      candidates <- c(candidates, name)
      if (!is.factor(values)) {
        conversions[[name]] <- paste0(typeof(values), " to factor")
      }
    }
  }
  names(columns) <- column_names

  list(
    identity = identity,
    column_identity = column_identity,
    columns = columns,
    catalog = catalog,
    groups = list(
      candidates = candidates,
      conversions = conversions,
      rejected = rejected,
      legacy = list(
        columns = legacy_columns,
        distinct = legacy_distinct,
        counts = legacy_counts,
        levels = legacy_levels
      )
    )
  )
}

#' Find metadata columns that can safely act as Viewer cell-cycle annotations.
#'
#' Cell-cycle scores are continuous measurements, not phase annotations. Keep
#' this deliberately conservative: a candidate must already be a valid
#' categorical Group and have an explicit phase/cell-cycle name.
builder_cell_cycle_candidate_ids <- function(metadata_catalog) {
  if (!is.list(metadata_catalog) || !length(metadata_catalog)) {
    return(character())
  }
  ids <- names(metadata_catalog)
  if (is.null(ids)) {
    return(character())
  }
  ids[vapply(
    seq_along(ids),
    function(index) {
      id <- ids[[index]]
      column <- metadata_catalog[[index]]
      if (!is.list(column) || !isTRUE(column$group_eligible)) {
        return(FALSE)
      }
      label <- column$name %||% id
      is.character(label) &&
        length(label) == 1L &&
        !is.na(label) &&
        grepl(
          "(^|[._ -])(cell[._ -]?cycle|phase)([._ -]|$)",
          label,
          ignore.case = TRUE,
          perl = TRUE
        )
    },
    logical(1)
  )]
}

# Reductions ----

builder_profile_embeddings <- function(embeddings, expected_cells, name) {
  matrix_like <- is.matrix(embeddings) || is.data.frame(embeddings)
  values <- if (matrix_like) as.matrix(embeddings) else NULL
  ids <- if (matrix_like) rownames(embeddings) else character()
  cells <- builder_identity_profile(ids, expected_cells)
  dimensions <- if (matrix_like) as.integer(ncol(embeddings)) else 0L
  numeric <- matrix_like && is.numeric(values)
  finite <- numeric && all(is.finite(values))
  diagnostics <- .builder_profile_identity_diagnostics(cells)
  if (!matrix_like) {
    diagnostics <- c(diagnostics, "missing_embeddings")
  }
  if (matrix_like && !numeric) {
    diagnostics <- c(diagnostics, "non_numeric")
  }
  if (numeric && !finite) {
    diagnostics <- c(diagnostics, "non_finite")
  }
  if (dimensions < 2L) {
    diagnostics <- c(diagnostics, "fewer_than_two_dimensions")
  }
  structurally_valid <- matrix_like &&
    numeric &&
    finite &&
    dimensions >= 2L &&
    cells$valid
  is_pca <- grepl("pca", name, ignore.case = TRUE)

  list(
    name = name,
    dimensions = dimensions,
    numeric = numeric,
    finite = finite,
    cells = cells,
    structurally_valid = structurally_valid,
    is_pca = is_pca,
    selection_role = if (is_pca) "fallback_only" else "normal",
    exportable = structurally_valid,
    diagnostics = unique(diagnostics)
  )
}

builder_profile_reduction <- function(reduction, expected_cells, name) {
  embeddings <- tryCatch(
    SeuratObject::Embeddings(reduction),
    error = function(error) NULL
  )
  builder_profile_embeddings(embeddings, expected_cells, name)
}

builder_profile_reductions <- function(object, expected_cells) {
  reduction_names <- names(object@reductions)
  profiles <- lapply(reduction_names, function(name) {
    builder_profile_reduction(object[[name]], expected_cells, name)
  })
  names(profiles) <- reduction_names
  profiles
}

builder_profile_projection_catalog <- function(reductions) {
  if (!is.list(reductions) || !length(reductions)) {
    return(list())
  }
  catalog <- lapply(seq_along(reductions), function(index) {
    reduction <- reductions[[index]]
    name <- names(reductions)[[index]]
    lower <- tolower(name)
    kind <- if (grepl("umap", lower, fixed = TRUE)) {
      "umap"
    } else if (
      grepl("tsne", lower, fixed = TRUE) ||
        grepl("t-sne", lower, fixed = TRUE)
    ) {
      "tsne"
    } else if (
      isTRUE(reduction$is_pca) ||
        grepl("pca", lower, fixed = TRUE)
    ) {
      "pca"
    } else {
      "other"
    }
    available <- isTRUE(reduction$exportable) &&
      is.numeric(reduction$dimensions) &&
      length(reduction$dimensions) == 1L &&
      reduction$dimensions >= 2L
    dimensions <- reduction$dimensions
    if (is.null(dimensions) || !length(dimensions)) {
      dimensions <- 0L
    }
    cell_count <- reduction$cells$count
    if (is.null(cell_count) || !length(cell_count)) {
      cell_count <- 0L
    }
    list(
      id = name,
      name = name,
      kind = kind,
      dimensions = as.integer(dimensions[[1L]]),
      cell_count = as.integer(cell_count[[1L]]),
      available = available,
      is_pca = isTRUE(reduction$is_pca),
      reason = if (available) {
        NULL
      } else {
        "This reduction is not a complete two-dimensional Viewer projection."
      }
    )
  })
  names(catalog) <- names(reductions)
  catalog
}

builder_profile_organism <- function(features) {
  features <- .builder_profile_ids(features)
  features <- features[!is.na(features) & nzchar(features)]
  features <- utils::head(features, 200L)
  if (!length(features)) {
    return(list(
      code = "other",
      confidence = 0,
      reason = "No feature identifiers were available for inference."
    ))
  }

  human <- mean(features == toupper(features))
  mouse <- mean(grepl("^[A-Z][a-z]", features))
  if (isTRUE(human > 0.8)) {
    list(
      code = "hg",
      confidence = unname(human),
      reason = "Most sampled feature identifiers use human-style symbols."
    )
  } else if (isTRUE(mouse > 0.6)) {
    list(
      code = "mm",
      confidence = unname(mouse),
      reason = "Most sampled feature identifiers use mouse-style symbols."
    )
  } else {
    list(
      code = "other",
      confidence = unname(max(human, mouse)),
      reason = "Feature identifiers do not strongly match one supported style."
    )
  }
}

# Manifest ----

.builder_profile_manifest_source <- function(source) {
  list(type = source$type, location = source$location)
}

.builder_profile_manifest_entry <- function(
  id,
  source,
  valid,
  summary,
  diagnostics,
  disposition = "preserved",
  pages = character(),
  verifier = NULL
) {
  builder_manifest_entry(
    id = id,
    source = .builder_profile_manifest_source(source),
    status = if (isTRUE(valid)) "valid" else "blocking",
    disposition = if (isTRUE(valid)) disposition else "rejected",
    artifact_scope = "both",
    summary = summary,
    diagnostics = diagnostics,
    compatibility = list(viewer = isTRUE(valid)),
    pages = pages,
    verifier = verifier
  )
}

builder_profile_core_manifest <- function(
  source,
  identity,
  assays,
  metadata,
  reductions
) {
  identity_valid <- identity$cells$valid &&
    identity$cells$count > 0L &&
    identity$features$valid
  expression_valid <- identity_valid &&
    identity$features$count > 0L &&
    any(vapply(assays, function(assay) assay$exportable, logical(1)))
  metadata_valid <- metadata$identity$valid && metadata$column_identity$valid
  group_candidates <- metadata$groups$candidates
  groups_valid <- metadata_valid && length(group_candidates) > 0L
  has_conversion <- any(
    group_candidates %in% names(metadata$groups$conversions)
  )
  usable_reductions <- names(reductions)[vapply(
    reductions,
    function(reduction) reduction$exportable,
    logical(1)
  )]

  entries <- list(
    .builder_profile_manifest_entry(
      "dataset_identity",
      source,
      identity_valid,
      "Dataset cell and feature identities were checked.",
      identity,
      pages = "data_info",
      verifier = "verify_dataset_identity"
    ),
    .builder_profile_manifest_entry(
      "expression",
      source,
      expression_valid,
      "Expression layers were matched to dataset cell barcodes.",
      list(
        assays = names(assays),
        exportable = vapply(
          assays,
          function(assay) assay$exportable,
          logical(1)
        )
      ),
      pages = "gene_expression",
      verifier = "verify_expression"
    ),
    .builder_profile_manifest_entry(
      "metadata",
      source,
      metadata_valid,
      "Metadata rows were matched to dataset cell barcodes.",
      list(
        rows = metadata$identity,
        columns = metadata$column_identity
      ),
      pages = c("data_info", "groups"),
      verifier = "verify_metadata"
    ),
    .builder_profile_manifest_entry(
      "groups",
      source,
      groups_valid,
      "Grouping candidates were classified from metadata.",
      metadata$groups,
      disposition = if (has_conversion) "converted" else "preserved",
      pages = c("groups", "color_management"),
      verifier = "verify_groups"
    )
  )

  for (name in names(reductions)) {
    reduction <- reductions[[name]]
    if (reduction$exportable) {
      entries[[length(entries) + 1L]] <- .builder_profile_manifest_entry(
        paste0("reduction:", name),
        source,
        TRUE,
        if (reduction$is_pca) {
          "PCA is available only as a fallback projection selection."
        } else {
          "Reduction structure and cell barcodes were checked."
        },
        reduction,
        pages = "projection",
        verifier = "verify_reductions"
      )
    } else {
      entries[[length(entries) + 1L]] <- builder_manifest_entry(
        id = paste0("reduction:", name),
        source = .builder_profile_manifest_source(source),
        status = "valid",
        disposition = "rejected",
        artifact_scope = "both",
        summary = "This reduction is unsafe and is excluded from choices.",
        diagnostics = reduction,
        compatibility = list(viewer = FALSE),
        pages = character(),
        verifier = "verify_reductions"
      )
    }
  }

  entries[[length(entries) + 1L]] <- .builder_profile_manifest_entry(
    "projection",
    source,
    length(usable_reductions) > 0L,
    "At least one Viewer-compatible projection is required.",
    list(usable_reductions = usable_reductions),
    pages = "projection",
    verifier = "verify_projection"
  )
  builder_content_manifest(entries)
}

# Assembly ----

builder_dataset_profile <- function(object, source) {
  source <- builder_profile_source(source)
  if (!inherits(object, "Seurat")) {
    .builder_profile_abort(
      "unsupported_object",
      "Dataset profiles currently require a Seurat object."
    )
  }

  cells <- tryCatch(
    SeuratObject::Cells(object),
    error = function(error) character()
  )
  features <- tryCatch(
    SeuratObject::Features(object),
    error = function(error) character()
  )
  identity <- list(
    cells = builder_identity_profile(cells),
    features = builder_feature_profile(features, features),
    metadata = builder_identity_profile(rownames(object@meta.data), cells)
  )
  assays <- builder_profile_assays(object, cells)
  metadata <- builder_profile_metadata(object@meta.data, cells)
  reductions <- builder_profile_reductions(object, cells)
  assay_names <- names(assays)
  default_assay <- tryCatch(
    SeuratObject::DefaultAssay(object),
    error = function(error) if (length(assay_names)) assay_names[[1L]] else NULL
  )
  if (!default_assay %in% assay_names) {
    default_assay <- if (length(assay_names)) assay_names[[1L]] else NULL
  }
  content_context <- builder_profile_content_context(
    source = source,
    cells = cells,
    features = features,
    metadata = object@meta.data,
    assays = assays,
    default_assay = default_assay,
    groups = metadata$groups,
    reductions = reductions
  )
  content <- builder_profile_optional_content(object, content_context)
  object_version <- tryCatch(
    as.character(methods::slot(object, "version")),
    error = function(error) NA_character_
  )
  spatial_fact <- content$spatial$normalized
  spatial_sections <- vapply(
    spatial_fact$sections,
    function(section) section$name,
    character(1)
  )
  spatial_summary <- list(
    section_count = spatial_fact$section_count,
    sections = spatial_sections,
    sections_truncated = spatial_fact$sections_truncated,
    section_names_truncated = spatial_fact$section_names_truncated,
    deferred = TRUE
  )
  manifest <- builder_profile_core_manifest(
    source,
    identity,
    assays,
    metadata,
    reductions
  )

  structure(
    list(
      schema_version = 2L,
      source = source,
      object = list(class = class(object), version = object_version),
      identity = identity,
      assays = assays,
      default_assay = default_assay,
      metadata = list(
        identity = metadata$identity,
        column_identity = metadata$column_identity,
        columns = metadata$columns,
        catalog = metadata$catalog
      ),
      groups = metadata$groups,
      reductions = reductions,
      content = content,
      viewer_content = list(
        metadata = metadata$catalog,
        projections = builder_profile_projection_catalog(reductions),
        trajectories = builder_trajectory_catalog(content$trajectory)
      ),
      spatial = spatial_summary,
      organism = builder_profile_organism(features),
      manifest = manifest
    ),
    class = c("builder_dataset_profile", "list")
  )
}
