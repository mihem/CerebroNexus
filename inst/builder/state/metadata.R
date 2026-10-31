## Builder state: metadata.

.builder_state_metadata_policy_abort <- function(message) {
  .builder_state_abort("invalid_metadata_policy", message)
}

.builder_state_metadata_policy_ids <- function(value, label) {
  if (
    !is.character(value) ||
      anyNA(value) ||
      any(!nzchar(value)) ||
      anyDuplicated(value)
  ) {
    .builder_state_metadata_policy_abort(
      paste(label, "must contain unique non-empty column names.")
    )
  }
  value
}

.builder_state_upgrade_metadata_policy <- function(policy) {
  if (is.null(policy)) {
    return(NULL)
  }
  if (!is.list(policy) || !is.list(policy$columns)) {
    return(policy)
  }
  columns <- policy$columns
  for (id in names(columns)) {
    record <- columns[[id]]
    if (!is.list(record)) {
      next
    }
    if (is.null(record$retain_in_crb)) {
      record$retain_in_crb <- isTRUE(record$effective_included)
    }
    if (is.null(record$group_enabled)) {
      record$group_enabled <- FALSE
    }
    if (is.null(record$forced)) {
      record$forced <- isTRUE(record$required)
    }
    columns[[id]] <- record
  }
  ids <- names(columns) %||% character()
  retained <- ids[vapply(
    columns,
    function(record) is.list(record) && isTRUE(record$retain_in_crb),
    logical(1)
  )]
  groups <- ids[vapply(
    columns,
    function(record) is.list(record) && isTRUE(record$group_enabled),
    logical(1)
  )]
  forced <- ids[vapply(
    columns,
    function(record) is.list(record) && isTRUE(record$forced),
    logical(1)
  )]
  dispositions <- vapply(
    columns,
    function(record) {
      if (is.list(record) && is.character(record$disposition)) {
        record$disposition[[1L]]
      } else {
        ""
      }
    },
    character(1)
  )
  policy$columns <- columns
  if (is.null(policy$retained)) {
    policy$retained <- retained
  }
  if (is.null(policy$groups)) {
    policy$groups <- groups
  }
  if (is.null(policy$forced)) {
    policy$forced <- forced
  }
  if (is.null(policy$included)) {
    policy$included <- retained
  }
  if (is.null(policy$attention)) {
    policy$attention <- ids[dispositions == "attention"]
  }
  if (is.null(policy$excluded)) {
    policy$excluded <- setdiff(ids, retained)
  }
  if (is.null(policy$blocking)) {
    policy$blocking <- ids[dispositions == "blocking"]
  }
  if (is.null(policy$value)) {
    policy$value <- retained
  }
  if (is.null(policy$requires_confirmation)) {
    policy$requires_confirmation <- length(policy$attention) > 0L ||
      length(policy$blocking) > 0L
  }
  policy
}

builder_metadata_policy_set_retained <- function(policy, retained) {
  retained <- unique(as.character(retained))
  policy <- .builder_state_upgrade_metadata_policy(policy)
  for (id in names(policy$columns)) {
    record <- policy$columns[[id]]
    keep <- id %in% retained || isTRUE(record$forced)
    record$retain_in_crb <- keep
    record$group_enabled <- isTRUE(record$group_enabled) && keep
    if (!identical(record$disposition, "blocking")) {
      record$value <- if (keep) "included" else "excluded"
      record$disposition <- record$value
      record$effective_included <- keep
      record$requires_confirmation <- FALSE
    }
    policy$columns[[id]] <- record
  }
  policy[c(
    "retained",
    "included",
    "attention",
    "excluded",
    "blocking",
    "value",
    "requires_confirmation"
  )] <- list(NULL)
  .builder_state_upgrade_metadata_policy(policy)
}

builder_metadata_policy_set_groups <- function(policy, groups) {
  groups <- unique(as.character(groups))
  policy <- .builder_state_upgrade_metadata_policy(policy)
  for (id in names(policy$columns)) {
    record <- policy$columns[[id]]
    record$group_enabled <- id %in% groups
    if (id %in% groups) {
      record$retain_in_crb <- TRUE
      if (!identical(record$disposition, "blocking")) {
        record$value <- "included"
        record$disposition <- "included"
        record$effective_included <- TRUE
        record$requires_confirmation <- FALSE
      }
    }
    policy$columns[[id]] <- record
  }
  policy[c(
    "retained",
    "groups",
    "included",
    "attention",
    "excluded",
    "blocking",
    "value",
    "requires_confirmation"
  )] <- list(NULL)
  .builder_state_upgrade_metadata_policy(policy)
}

.builder_state_metadata_policy_sync_groups <- function(policy, groups) {
  policy <- .builder_state_upgrade_metadata_policy(policy)
  groups <- unique(as.character(groups %||% character()))
  for (id in names(policy$columns)) {
    policy$columns[[id]]$group_enabled <- id %in% groups
  }
  policy$groups <- NULL
  .builder_state_upgrade_metadata_policy(policy)
}

.builder_state_missing_metadata_sentinel <- function(record, id) {
  zero_count <- function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      identical(as.numeric(value), 0)
  }
  is.list(record) &&
    !is.object(record) &&
    !.builder_state_has_reference(record) &&
    identical(record$name, id) &&
    identical(record$class, "missing") &&
    isTRUE(record$required) &&
    identical(record$disposition, "blocking") &&
    identical(record$value, "blocking") &&
    identical(record$effective_included, FALSE) &&
    identical(record$requires_confirmation, TRUE) &&
    zero_count(record$non_missing) &&
    zero_count(record$unique_non_missing)
}

.builder_state_metadata_record_dependencies <- function(record) {
  dependencies <- record$dependency_ids
  if (is.null(dependencies)) {
    return(character())
  }
  if (
    !is.character(dependencies) ||
      anyNA(dependencies) ||
      any(!nzchar(dependencies)) ||
      anyDuplicated(dependencies)
  ) {
    .builder_state_metadata_policy_abort(
      "Metadata dependency ids must be unique non-empty strings."
    )
  }
  dependencies
}

.builder_state_metadata_required_columns <- function(
  entry,
  policy,
  recommendation
) {
  settings <- entry$settings
  legacy_profile <- if (is.list(entry$profile)) entry$profile else list()
  selected <- unlist(
    Filter(
      is.character,
      list(
        settings$groups,
        .builder_state_included_groups(entry),
        settings$default_group,
        settings$cell_cycle_columns,
        .builder_state_or(settings$nUMI, legacy_profile$nUMI),
        .builder_state_or(settings$nGene, legacy_profile$nGene)
      )
    ),
    use.names = FALSE
  )
  selected <- selected[!is.na(selected) & nzchar(selected)]
  dependent <- character()
  for (source in list(policy, recommendation)) {
    columns <- source$columns
    if (!is.list(columns)) {
      next
    }
    ids <- names(columns)
    if (is.null(ids)) {
      next
    }
    for (id in ids) {
      record <- columns[[id]]
      if (
        is.list(record) &&
          (isTRUE(record$required) ||
            length(.builder_state_metadata_record_dependencies(record)))
      ) {
        dependent <- c(dependent, id)
      }
    }
  }
  unique(c(selected, dependent))
}

.builder_state_validate_metadata_dependencies <- function(
  entry,
  policy,
  recommendation
) {
  required <- .builder_state_metadata_required_columns(
    entry,
    policy,
    recommendation
  )
  missing <- required[
    !vapply(
      required,
      function(id) {
        record <- policy$columns[[id]]
        is.list(record) &&
          (isTRUE(record$retain_in_crb) ||
            identical(record$disposition, "blocking"))
      },
      logical(1)
    )
  ]
  if (length(missing)) {
    .builder_state_abort(
      "metadata_dependency_conflict",
      paste0(
        "Final metadata must include selected or dependency-bearing columns: ",
        paste(missing, collapse = ", "),
        "."
      )
    )
  }
  invisible(required)
}

.builder_state_validate_metadata_policy <- function(
  policy,
  profile,
  entry,
  recommendation = NULL,
  validate_dependencies = TRUE
) {
  if (
    !is.list(policy) ||
      is.object(policy) ||
      .builder_state_has_reference(policy) ||
      !is.list(policy$columns) ||
      is.object(policy$columns)
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy must be an inert record."
    )
  }

  column_ids <- names(policy$columns)
  if (is.null(column_ids)) {
    column_ids <- character()
  }
  column_ids <- .builder_state_metadata_policy_ids(
    column_ids,
    "Final metadata policy columns"
  )

  if (
    !is.list(profile) ||
      !is.list(profile$metadata) ||
      !is.list(profile$metadata$columns)
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy requires profiled metadata columns."
    )
  }
  source_columns <- profile$metadata$columns
  source_ids <- names(source_columns)
  if (is.null(source_ids)) {
    source_ids <- character()
  }
  source_ids <- .builder_state_metadata_policy_ids(
    source_ids,
    "Profiled metadata columns"
  )
  expected_ids <- unique(c(
    "cell_barcode",
    setdiff(source_ids, "cell_barcode")
  ))
  missing_ids <- setdiff(expected_ids, column_ids)
  extra_ids <- setdiff(column_ids, expected_ids)
  valid_extra_ids <- extra_ids[vapply(
    extra_ids,
    function(id) {
      .builder_state_missing_metadata_sentinel(
        policy$columns[[id]],
        id
      )
    },
    logical(1)
  )]
  if (
    length(missing_ids) ||
      !setequal(extra_ids, valid_extra_ids)
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy does not match the profiled column set."
    )
  }

  bucket_names <- c("included", "attention", "excluded", "blocking")
  buckets <- lapply(bucket_names, function(name) {
    .builder_state_metadata_policy_ids(
      policy[[name]],
      paste("Final metadata policy", name)
    )
  })
  names(buckets) <- bucket_names
  retained <- .builder_state_metadata_policy_ids(
    policy$retained,
    "Final metadata retained columns"
  )
  groups <- .builder_state_metadata_policy_ids(
    policy$groups,
    "Final metadata Group columns"
  )
  forced <- .builder_state_metadata_policy_ids(
    policy$forced,
    "Final metadata forced columns"
  )
  if (length(intersect(buckets$included, buckets$excluded))) {
    .builder_state_metadata_policy_abort(
      "Included metadata columns cannot also be excluded."
    )
  }

  dispositions <- character(length(column_ids))
  effective <- logical(length(column_ids))
  names(dispositions) <- column_ids
  names(effective) <- column_ids
  allowed <- c("included", "attention", "excluded", "blocking")
  for (id in column_ids) {
    record <- policy$columns[[id]]
    if (
      !is.list(record) ||
        is.object(record) ||
        .builder_state_has_reference(record) ||
        !identical(record$name, id) ||
        !.builder_state_text(record$disposition) ||
        !record$disposition %in% allowed ||
        !identical(record$value, record$disposition) ||
        !is.logical(record$effective_included) ||
        length(record$effective_included) != 1L ||
        is.na(record$effective_included) ||
        !is.logical(record$retain_in_crb) ||
        length(record$retain_in_crb) != 1L ||
        is.na(record$retain_in_crb) ||
        !is.logical(record$group_enabled) ||
        length(record$group_enabled) != 1L ||
        is.na(record$group_enabled) ||
        !is.logical(record$forced) ||
        length(record$forced) != 1L ||
        is.na(record$forced) ||
        !is.logical(record$requires_confirmation) ||
        length(record$requires_confirmation) != 1L ||
        is.na(record$requires_confirmation)
    ) {
      .builder_state_metadata_policy_abort(
        paste("Final metadata policy column", id, "is malformed.")
      )
    }
    .builder_state_metadata_record_dependencies(record)
    expected_confirmation <- record$disposition %in%
      c("attention", "blocking")
    invalid_effective <-
      (identical(record$disposition, "included") &&
        !isTRUE(record$effective_included)) ||
      (identical(record$disposition, "excluded") &&
        isTRUE(record$effective_included))
    if (
      invalid_effective ||
        !identical(
          record$requires_confirmation,
          expected_confirmation
        )
    ) {
      .builder_state_metadata_policy_abort(
        paste("Final metadata policy column", id, "is inconsistent.")
      )
    }
    dispositions[[id]] <- record$disposition
    effective[[id]] <- record$effective_included
  }

  derived_retained <- column_ids[vapply(
    policy$columns,
    `[[`,
    logical(1),
    "retain_in_crb"
  )]
  derived_groups <- column_ids[vapply(
    policy$columns,
    `[[`,
    logical(1),
    "group_enabled"
  )]
  derived_forced <- column_ids[vapply(
    policy$columns,
    `[[`,
    logical(1),
    "forced"
  )]
  derived <- list(
    included = derived_retained,
    attention = column_ids[dispositions == "attention"],
    excluded = setdiff(column_ids, derived_retained),
    blocking = column_ids[dispositions == "blocking"]
  )
  if (length(setdiff(groups, retained))) {
    .builder_state_abort(
      "metadata_dependency_conflict",
      "Every included Group must also be retained as metadata."
    )
  }
  if (
    !all(vapply(
      bucket_names,
      function(name) {
        identical(buckets[[name]], derived[[name]])
      },
      logical(1)
    ))
  ) {
    .builder_state_metadata_policy_abort(
      "Final metadata policy buckets do not match their column records."
    )
  }
  if (
    !setequal(retained, derived_retained) ||
      !setequal(groups, derived_groups) ||
      !setequal(forced, derived_forced)
  ) {
    .builder_state_metadata_policy_abort(
      "Final metadata retention or Group sets do not match their records."
    )
  }
  forced_present <- intersect(forced, expected_ids)
  forced_missing <- forced_present[
    !forced_present %in% retained &
      dispositions[forced_present] != "blocking"
  ]
  if (length(forced_missing)) {
    message <- paste0(
      "Forced metadata must be retained: ",
      paste(forced_missing, collapse = ", "),
      "."
    )
    if ("cell_barcode" %in% forced_missing) {
      .builder_state_metadata_policy_abort(message)
    }
    .builder_state_abort("metadata_dependency_conflict", message)
  }
  if (!identical(policy$value, retained)) {
    .builder_state_metadata_policy_abort(
      "Final metadata policy value must equal its included columns."
    )
  }
  expected_confirmation <- length(buckets$attention) > 0L ||
    length(buckets$blocking) > 0L
  if (
    !is.logical(policy$requires_confirmation) ||
      length(policy$requires_confirmation) != 1L ||
      is.na(policy$requires_confirmation) ||
      !identical(
        policy$requires_confirmation,
        expected_confirmation
      )
  ) {
    .builder_state_metadata_policy_abort(
      "Final metadata policy confirmation state is inconsistent."
    )
  }

  barcode <- policy$columns$cell_barcode
  if (
    !is.list(barcode) ||
      !isTRUE(barcode$required) ||
      identical(barcode$disposition, "excluded") ||
      !isTRUE(barcode$effective_included) ||
      !isTRUE(barcode$retain_in_crb) ||
      !"cell_barcode" %in% retained
  ) {
    .builder_state_metadata_policy_abort(
      "The final metadata policy must include required cell barcodes."
    )
  }
  if (
    "cell_barcode" %in%
      source_ids &&
      !identical(barcode$disposition, "blocking")
  ) {
    .builder_state_metadata_policy_abort(
      "A reserved cell_barcode source collision cannot be downgraded."
    )
  }
  unsafe_included <- source_ids[vapply(
    source_ids,
    function(id) {
      fact <- source_columns[[id]]
      record <- policy$columns[[id]]
      classes <- if (is.list(fact)) fact$class else NULL
      unsafe <- !is.list(fact) ||
        !isTRUE(fact$supported) ||
        !is.character(classes) ||
        any(classes %in% c("list", "data.frame"))
      unsafe && isTRUE(record$retain_in_crb)
    },
    logical(1)
  )]
  if (length(unsafe_included)) {
    .builder_state_metadata_policy_abort(
      paste0(
        "Unsupported metadata columns cannot be included: ",
        paste(unsafe_included, collapse = ", "),
        "."
      )
    )
  }
  if (isTRUE(validate_dependencies)) {
    .builder_state_validate_metadata_dependencies(
      entry,
      policy,
      recommendation
    )
  }
  invisible(policy)
}

.builder_state_effective_metadata_policy <- function(entry, profile) {
  recommendations <- entry$settings$recommendations
  if (
    !is.null(recommendations) &&
      (!is.list(recommendations) ||
        is.object(recommendations) ||
        .builder_state_has_reference(recommendations))
  ) {
    .builder_state_metadata_policy_abort(
      "Metadata recommendations must be an inert record."
    )
  }
  recommendation <- if (is.list(recommendations)) {
    .builder_state_upgrade_metadata_policy(recommendations$metadata)
  } else {
    NULL
  }
  policy <- .builder_state_upgrade_metadata_policy(
    entry$settings$metadata_policy
  )
  selected_groups <- .builder_state_included_groups(entry)
  if (!is.null(policy)) {
    policy <- .builder_state_metadata_policy_sync_groups(
      policy,
      selected_groups
    )
  } else if (!is.null(recommendation)) {
    recommendation <- .builder_state_metadata_policy_sync_groups(
      recommendation,
      selected_groups
    )
  }
  effective <- .builder_state_or(policy, recommendation)
  for (candidate in list(recommendation, policy)) {
    if (
      !is.null(candidate) &&
        (!is.list(candidate) ||
          is.object(candidate) ||
          .builder_state_has_reference(candidate))
    ) {
      .builder_state_metadata_policy_abort(
        "Metadata policy values must be inert records."
      )
    }
  }
  if (is.list(profile)) {
    if (!is.null(recommendation)) {
      .builder_state_validate_metadata_policy(
        recommendation,
        profile,
        entry,
        validate_dependencies = is.null(policy)
      )
    }
    if (!is.null(policy)) {
      .builder_state_validate_metadata_policy(
        policy,
        profile,
        entry,
        recommendation = recommendation
      )
    }
  }
  effective
}

.builder_state_metadata_entry <- function(policy, source) {
  recommendation <- policy
  if (is.null(recommendation)) {
    return(NULL)
  }
  if (
    !is.list(recommendation) ||
      !is.list(recommendation$columns) ||
      !is.character(recommendation$attention) ||
      anyNA(recommendation$attention) ||
      !is.character(recommendation$blocking) ||
      anyNA(recommendation$blocking) ||
      !is.logical(recommendation$requires_confirmation) ||
      length(recommendation$requires_confirmation) != 1L ||
      is.na(recommendation$requires_confirmation)
  ) {
    .builder_state_abort(
      "invalid_metadata_recommendation",
      "Metadata recommendations must use the production recommendation shape."
    )
  }
  attention <- sort(unique(recommendation$attention), method = "radix")
  blocking <- sort(unique(recommendation$blocking), method = "radix")
  included <- .builder_state_or(recommendation$included, character())
  excluded <- .builder_state_or(recommendation$excluded, character())
  if (
    !is.character(included) ||
      anyNA(included) ||
      !is.character(excluded) ||
      anyNA(excluded)
  ) {
    .builder_state_abort(
      "invalid_metadata_recommendation",
      "Metadata recommendations contain invalid included or excluded names."
    )
  }
  evidence <- list(
    detected = TRUE,
    valid = !length(blocking),
    attention = length(attention) > 0L &&
      isTRUE(recommendation$requires_confirmation),
    attention_items = c(
      paste0("review=", attention),
      paste0("include=", sort(unique(included), method = "radix")),
      paste0("exclude=", sort(unique(excluded), method = "radix"))
    ),
    normalized = list(
      included = included,
      excluded = excluded,
      attention = attention,
      blocking = blocking
    ),
    diagnostics = c(
      paste0("attention:", attention),
      paste0("blocking:", blocking)
    ),
    requirements = if (length(attention)) {
      "acknowledge_metadata_attention"
    } else {
      character()
    },
    page_candidates = character()
  )
  status <- if (length(blocking)) {
    "blocking"
  } else if (evidence$attention) {
    "attention"
  } else {
    "valid"
  }
  .builder_state_manifest_record(
    "metadata_policy",
    source,
    status,
    if (identical(status, "blocking")) "rejected" else "preserved",
    character(),
    evidence,
    required_action = if (identical(status, "attention")) {
      .builder_state_attention_action("metadata_policy", evidence)
    } else {
      NULL
    },
    verifier = "verify_metadata_policy"
  )
}

.builder_state_compile_manifest <- function(
  entry,
  profile,
  manifest,
  metadata_policy
) {
  content <- .subset2(profile, "content")
  source <- .builder_state_source(entry, profile)
  additions <- list()
  if (!is.null(content) && !is.list(content)) {
    .builder_state_abort(
      "invalid_content_evidence",
      "Optional content evidence must be a list of inert records."
    )
  }
  if (is.list(content) && length(content)) {
    content_ids <- attr(content, "names", exact = TRUE)
    if (
      is.null(content_ids) ||
        length(content_ids) != length(content) ||
        anyNA(content_ids) ||
        any(!nzchar(content_ids)) ||
        anyDuplicated(content_ids) ||
        any(!content_ids %in% .builder_profile_content_ids())
    ) {
      .builder_state_abort(
        "invalid_content_id",
        "Optional content evidence contains an unexpected capability."
      )
    }
    for (id in content_ids) {
      fact <- .subset2(content, id)
      .builder_state_validate_content_fact(id, fact)
      if (identical(id, "immune_repertoire")) {
        additions <- c(
          additions,
          .builder_state_immune_entries(entry, fact, source)
        )
        next
      }
      additions[[length(additions) + 1L]] <-
        .builder_state_generic_content_entry(entry, id, fact, source)
    }
  }
  metadata <- .builder_state_metadata_entry(metadata_policy, source)
  if (!is.null(metadata)) {
    additions[[length(additions) + 1L]] <- metadata
  }

  existing <- unname(manifest)
  addition_ids <- vapply(
    additions,
    function(value) value$id,
    character(1)
  )
  existing <- Filter(
    function(value) !value$id %in% addition_ids,
    existing
  )
  compiled <- builder_content_manifest(c(existing, additions))
  contract <- builder_viewer_page_contract(compiled)
  always <- contract$always$id
  for (id in names(compiled)) {
    pages <- compiled[[id]]$pages
    compiled[[id]]$page_visible <- any(pages %in% always) ||
      any(pages %in% contract$visible_conditional)
  }
  compiled
}
