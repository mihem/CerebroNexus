## Builder state: content.

.builder_state_revision <- function(value, default = 0L) {
  if (is.null(value)) {
    return(as.integer(default))
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value > .Machine$integer.max ||
      value != floor(value)
  ) {
    .builder_state_abort(
      "invalid_revision",
      "Builder revisions must be non-negative integers."
    )
  }
  as.integer(value)
}

.builder_state_profile <- function(entry) {
  profile <- entry$dataset_profile
  if (is.null(profile) && inherits(entry$profile, "builder_dataset_profile")) {
    profile <- entry$profile
  }
  profile
}

.builder_state_content_available <- function(entry, id) {
  profile <- .builder_state_profile(entry)
  if (is.list(profile)) {
    content <- .subset2(profile, "content")
    fact <- if (is.list(content)) .subset2(content, id) else NULL
    return(
      is.list(fact) &&
        isTRUE(.subset2(fact, "detected")) &&
        isTRUE(.subset2(fact, "valid"))
    )
  }
  any(vapply(
    .builder_state_or(entry$profile$extras, list()),
    function(value) {
      is.list(value) &&
        identical(value$key, id) &&
        isTRUE(value$found)
    },
    logical(1)
  ))
}

.builder_state_source <- function(entry, profile) {
  source <- .builder_state_or(profile$source, entry$source)
  if (
    !is.list(source) ||
      !.builder_state_text(source$type) ||
      !.builder_state_text(source$location)
  ) {
    return(list(
      type = "builder",
      location = .builder_state_or(entry$id, "dataset")
    ))
  }
  list(type = source$type, location = source$location)
}

.builder_state_acknowledgements <- function(entry) {
  acknowledgements <- .builder_state_or(
    entry$acknowledgements,
    .builder_state_or(entry$settings$acknowledgements, character())
  )
  if (!is.character(acknowledgements) || anyNA(acknowledgements)) {
    .builder_state_abort(
      "invalid_acknowledgements",
      "Dataset acknowledgements must be character tokens."
    )
  }
  unique(acknowledgements)
}

.builder_state_fact_logical <- function(value) {
  is.logical(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.null(attributes(value))
}

.builder_state_fact_text <- function(value) {
  .builder_state_text(value) &&
    !is.object(value) &&
    is.null(attributes(value))
}

.builder_state_fact_text_vector <- function(value) {
  is.character(value) &&
    !is.object(value) &&
    !anyNA(value) &&
    is.null(attributes(value))
}

.builder_state_fact_count <- function(value) {
  is.numeric(value) &&
    !is.object(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= 0 &&
    value == floor(value) &&
    is.null(attributes(value))
}

.builder_state_validate_fact_record <- function(fact, id, code) {
  required <- c(
    "detected",
    "valid",
    "normalized",
    "diagnostics",
    "requirements",
    "page_candidates"
  )
  if (
    !.builder_state_plain_record(fact) ||
      !all(required %in% names(fact))
  ) {
    .builder_state_abort(
      code,
      paste("Optional content evidence for", id, "must be an inert record.")
    )
  }
  for (field in c("detected", "valid")) {
    if (!.builder_state_fact_logical(.subset2(fact, field))) {
      .builder_state_abort(
        code,
        paste(
          "Optional content evidence for",
          id,
          "has an invalid",
          field,
          "flag."
        )
      )
    }
  }
  normalized <- .subset2(fact, "normalized")
  if (!is.null(normalized) && !.builder_state_plain_list(normalized)) {
    .builder_state_abort(
      code,
      paste("Optional content evidence for", id, "has invalid normalized data.")
    )
  }
  for (field in c("diagnostics", "requirements", "page_candidates")) {
    if (!.builder_state_fact_text_vector(.subset2(fact, field))) {
      .builder_state_abort(
        code,
        paste(
          "Optional content evidence for",
          id,
          "has invalid",
          field,
          "values."
        )
      )
    }
  }
  attention <- .subset2(fact, "attention")
  if (!is.null(attention) && !.builder_state_fact_logical(attention)) {
    .builder_state_abort(
      code,
      paste(
        "Optional content evidence for",
        id,
        "has an invalid attention flag."
      )
    )
  }
  invisible(fact)
}

.builder_state_validate_immune_candidates <- function(fact) {
  candidates <- .subset2(fact, "candidates")
  if (!.builder_state_plain_record(candidates)) {
    .builder_state_abort(
      "invalid_immune_candidates",
      "Immune-repertoire candidates must be inert named records."
    )
  }
  for (name in names(candidates)) {
    candidate <- .subset2(candidates, name)
    .builder_state_validate_fact_record(
      candidate,
      paste("immune candidate", name),
      "invalid_immune_candidates"
    )
    for (field in c("full_ir_ready", "hla_tcr_ready")) {
      if (!.builder_state_fact_logical(.subset2(candidate, field))) {
        .builder_state_abort(
          "invalid_immune_candidates",
          paste("Immune candidate", name, "has an invalid", field, "flag.")
        )
      }
    }
    source_kind <- .subset2(candidate, "source_kind")
    if (!is.null(source_kind) && !.builder_state_fact_text(source_kind)) {
      .builder_state_abort(
        "invalid_immune_candidates",
        paste("Immune candidate", name, "has an invalid source kind.")
      )
    }
  }
  invisible(candidates)
}

.builder_state_validate_immune_overlaps <- function(fact, candidate_names) {
  for (field in c(
    "source_overlaps",
    "full_source_overlaps",
    "motif_source_overlaps"
  )) {
    overlaps <- .subset2(fact, field)
    if (is.null(overlaps)) {
      next
    }
    if (!.builder_state_plain_list(overlaps)) {
      .builder_state_abort(
        "invalid_immune_overlaps",
        "Immune source overlaps must be inert record lists."
      )
    }
    for (overlap in overlaps) {
      if (!.builder_state_plain_record(overlap)) {
        .builder_state_abort(
          "invalid_immune_overlaps",
          "Each immune source overlap must be an inert record."
        )
      }
      left <- .subset2(overlap, "left")
      right <- .subset2(overlap, "right")
      if (
        !.builder_state_fact_text(left) ||
          !.builder_state_fact_text(right) ||
          identical(left, right) ||
          !left %in% candidate_names ||
          !right %in% candidate_names
      ) {
        .builder_state_abort(
          "invalid_immune_overlaps",
          "Immune source overlaps must identify two known sources."
        )
      }
      for (count in c("n_overlap", "n_divergent")) {
        if (!.builder_state_fact_count(.subset2(overlap, count))) {
          .builder_state_abort(
            "invalid_immune_overlaps",
            paste("Immune source overlap has an invalid", count, "count.")
          )
        }
      }
      equivalent <- .subset2(overlap, "equivalent")
      if (!is.null(equivalent) && !.builder_state_fact_logical(equivalent)) {
        .builder_state_abort(
          "invalid_immune_overlaps",
          "Immune source overlap has an invalid equivalent flag."
        )
      }
    }
  }
  invisible(fact)
}

.builder_state_validate_content_fact <- function(id, fact) {
  .builder_state_validate_fact_record(
    fact,
    id,
    "invalid_content_evidence"
  )
  if (identical(id, "immune_repertoire")) {
    candidates <- .builder_state_validate_immune_candidates(fact)
    .builder_state_validate_immune_overlaps(fact, names(candidates))
  }
  invisible(fact)
}

.builder_state_optional_evidence <- function(fact) {
  list(
    detected = isTRUE(.subset2(fact, "detected")),
    valid = isTRUE(.subset2(fact, "valid")),
    attention = isTRUE(.subset2(fact, "attention")),
    normalized = .builder_state_or(
      .subset2(fact, "normalized"),
      list()
    ),
    diagnostics = .builder_state_or(
      .subset2(fact, "diagnostics"),
      character()
    ),
    requirements = .builder_state_or(
      .subset2(fact, "requirements"),
      character()
    ),
    page_candidates = .builder_state_or(
      .subset2(fact, "page_candidates"),
      character()
    )
  )
}

.builder_state_optional_setting_record <- function(
  entry,
  field,
  code,
  label
) {
  settings <- .subset2(entry, "settings")
  record <- .subset2(settings, field)
  if (is.null(record)) {
    return(NULL)
  }
  if (!.builder_state_plain_record(record)) {
    .builder_state_abort(
      code,
      paste("Optional-content", label, "must be a plain inert record.")
    )
  }
  record
}

.builder_state_content_choice <- function(entry, id) {
  choices <- .builder_state_optional_setting_record(
    entry,
    "content_dispositions",
    "invalid_content_dispositions",
    "dispositions"
  )
  if (is.null(choices)) {
    return(NULL)
  }
  choice_names <- attr(choices, "names", exact = TRUE)
  if (
    is.null(choice_names) ||
      length(choice_names) != length(choices) ||
      anyNA(choice_names) ||
      any(!nzchar(choice_names)) ||
      anyDuplicated(choice_names)
  ) {
    .builder_state_abort(
      "invalid_content_dispositions",
      "Optional-content dispositions must have unique non-empty names."
    )
  }
  choice <- .subset2(choices, id)
  if (is.null(choice)) {
    return(NULL)
  }
  if (
    !.builder_state_fact_text(choice) ||
      !choice %in%
        c(
          "preserved",
          "generated",
          "converted",
          "attached",
          "filtered",
          "stored_only"
        )
  ) {
    .builder_state_abort(
      "invalid_content_disposition",
      "A selected content disposition is not supported."
    )
  }
  choice
}

.builder_state_validate_content_dispositions <- function(entry) {
  choices <- .builder_state_optional_setting_record(
    entry,
    "content_dispositions",
    "invalid_content_dispositions",
    "dispositions"
  )
  if (is.null(choices)) {
    return(invisible(NULL))
  }
  choice_ids <- attr(choices, "names", exact = TRUE)
  if (
    is.null(choice_ids) ||
      length(choice_ids) != length(choices) ||
      anyNA(choice_ids) ||
      any(!nzchar(choice_ids)) ||
      anyDuplicated(choice_ids) ||
      any(
        !choice_ids %in%
          c(
            .builder_profile_content_ids(),
            "hla_tcr_motifs"
          )
      )
  ) {
    .builder_state_abort(
      "invalid_content_dispositions",
      paste0(
        "Optional-content dispositions must have unique known ",
        "capability names."
      )
    )
  }
  for (id in choice_ids) {
    .builder_state_content_choice(entry, id)
  }
  invisible(choices)
}

.builder_state_analysis_ids <- function() {
  c(
    "percent_mt_ribo",
    "most_expressed",
    "marker_genes",
    "enriched_pathways"
  )
}

.builder_state_validate_analyses <- function(selected) {
  if (is.null(selected)) {
    return(character())
  }
  if (
    !is.character(selected) ||
      anyNA(selected) ||
      any(!nzchar(selected)) ||
      anyDuplicated(selected) ||
      any(!selected %in% .builder_state_analysis_ids())
  ) {
    .builder_state_abort(
      "invalid_analyses",
      "Selected analyses must be unique supported analysis ids."
    )
  }
  selected
}

.builder_state_normalize_analyses <- function(
  selected,
  has_marker_genes = FALSE
) {
  order <- .builder_state_analysis_ids()
  selected <- intersect(order, .builder_state_validate_analyses(selected))
  if (
    "enriched_pathways" %in%
      selected &&
      !"marker_genes" %in% selected &&
      !isTRUE(has_marker_genes)
  ) {
    selected <- setdiff(selected, "enriched_pathways")
  }
  selected
}

.builder_state_included_groups <- function(entry) {
  settings <- entry$settings
  recommendations <- settings$recommendations
  group_recommendation <- if (
    is.list(recommendations) &&
      !is.object(recommendations) &&
      is.list(recommendations$groups) &&
      !is.object(recommendations$groups)
  ) {
    recommendations$groups$included
  } else {
    NULL
  }
  .builder_state_or(
    settings$included_groups,
    .builder_state_or(group_recommendation, settings$groups)
  )
}

.builder_state_selected_analyses <- function(entry) {
  .builder_state_normalize_analyses(
    entry$settings$analyses,
    has_marker_genes = .builder_state_content_available(
      entry,
      "marker_genes"
    )
  )
}

.builder_state_generated_content <- function(entry) {
  selected <- .builder_state_selected_analyses(entry)
  map <- c(
    most_expressed = "most_expressed_genes",
    marker_genes = "marker_genes",
    enriched_pathways = "enriched_pathways"
  )
  unique(unname(map[intersect(names(map), selected)]))
}

.builder_state_validate_analysis_dispositions <- function(entry) {
  generated_content <- .builder_state_generated_content(entry)
  generation_capabilities <- c(
    "most_expressed_genes",
    "marker_genes",
    "enriched_pathways"
  )
  choices <- .builder_state_optional_setting_record(
    entry,
    "content_dispositions",
    "invalid_content_dispositions",
    "dispositions"
  )
  choice_ids <- if (is.null(choices)) {
    character()
  } else {
    attr(choices, "names", exact = TRUE)
  }
  for (id in union(generation_capabilities, choice_ids)) {
    choice <- .builder_state_content_choice(entry, id)
    generated <- id %in% generated_content
    if (
      !is.null(choice) &&
        (id %in% generation_capabilities || identical(choice, "generated")) &&
        !identical(generated, identical(choice, "generated"))
    ) {
      .builder_state_abort(
        "analysis_disposition_conflict",
        paste0(
          "Analysis execution and the ",
          id,
          " content disposition disagree."
        )
      )
    }
  }
  invisible(generated_content)
}

.builder_state_generated_page <- function(id) {
  pages <- c(
    most_expressed_genes = "most_expressed_genes",
    marker_genes = "marker_genes",
    enriched_pathways = "enriched_pathways"
  )
  unname(.builder_state_or(pages[[id]], character()))
}

.builder_state_attention_action <- function(id, evidence) {
  signals <- sort(
    unique(c(
      .builder_state_or(evidence$diagnostics, character()),
      .builder_state_or(evidence$attention_items, character())
    )),
    method = "radix"
  )
  signals <- signals[!is.na(signals) & nzchar(signals)]
  if (!length(signals)) {
    signals <- "review"
  }
  list(
    type = "acknowledge",
    token = paste(c("builder", id, "attention-v1", signals), collapse = ":")
  )
}

.builder_state_manifest_record <- function(
  id,
  source,
  status,
  disposition,
  pages,
  evidence,
  artifact_scope = "both",
  required_action = NULL,
  verifier = NULL
) {
  entry <- builder_manifest_entry(
    id = id,
    source = source,
    status = status,
    disposition = disposition,
    artifact_scope = artifact_scope,
    summary = paste("BuildPlan decision for", id),
    diagnostics = list(
      codes = evidence$diagnostics,
      requirements = evidence$requirements,
      normalized = evidence$normalized
    ),
    compatibility = list(
      viewer = status %in%
        c("valid", "attention") &&
        disposition %in% c("preserved", "generated", "converted", "attached")
    ),
    pages = pages,
    required_action = required_action,
    verifier = verifier
  )
  entry$evidence <- evidence
  entry
}

.builder_state_generic_content_entry <- function(entry, id, fact, source) {
  evidence <- .builder_state_optional_evidence(fact)
  choice <- .builder_state_content_choice(entry, id)
  generated <- id %in% .builder_state_generated_content(entry)

  if (!evidence$detected && !generated && is.null(choice)) {
    return(.builder_state_manifest_record(
      id,
      source,
      "not_applicable",
      NA_character_,
      character(),
      evidence,
      verifier = paste0("verify_", id)
    ))
  }

  disposition <- if (!is.null(choice)) {
    choice
  } else if (generated) {
    "generated"
  } else {
    "preserved"
  }
  filtered <- disposition %in% c("filtered", "stored_only")
  valid <- (evidence$detected && evidence$valid) || generated || filtered
  attention <- evidence$detected &&
    evidence$valid &&
    evidence$attention &&
    !filtered
  status <- if (attention) {
    "attention"
  } else if (valid) {
    "valid"
  } else {
    "blocking"
  }
  if (!valid) {
    disposition <- "rejected"
  }
  pages <- if (
    status %in%
      c("valid", "attention") &&
      disposition %in% c("preserved", "generated", "converted", "attached")
  ) {
    if (generated) {
      .builder_state_generated_page(id)
    } else {
      evidence$page_candidates
    }
  } else {
    character()
  }

  .builder_state_manifest_record(
    id,
    source,
    status,
    disposition,
    pages,
    evidence,
    required_action = if (attention) {
      .builder_state_attention_action(id, evidence)
    } else {
      NULL
    },
    verifier = paste0("verify_", id)
  )
}

.builder_state_immune_source <- function(names, candidates, fallback) {
  kinds <- vapply(
    seq_along(candidates),
    function(index) {
      .builder_state_or(
        .subset2(.subset2(candidates, index), "source_kind"),
        names[[index]]
      )
    },
    character(1)
  )
  records <- list(
    unified_misc = list(
      source = list(type = "seurat_slot", location = "@misc$immune_repertoire"),
      disposition = "preserved"
    ),
    metadata = list(
      source = list(type = "seurat_metadata", location = "@meta.data"),
      disposition = "converted"
    ),
    legacy_bcr = list(
      source = list(type = "seurat_slot", location = "@misc$bcr_data"),
      disposition = "converted"
    ),
    legacy_tcr = list(
      source = list(type = "seurat_slot", location = "@misc$tcr_data"),
      disposition = "converted"
    )
  )
  selected <- lapply(kinds, function(kind) {
    .builder_state_or(
      records[[kind]],
      list(source = fallback, disposition = "converted")
    )
  })
  locations <- unique(vapply(
    selected,
    function(record) record$source$location,
    character(1)
  ))
  source <- if (length(selected) == 1L) {
    selected[[1L]]$source
  } else {
    list(
      type = "seurat_slots",
      location = paste(locations, collapse = " + ")
    )
  }
  dispositions <- unique(vapply(
    selected,
    `[[`,
    character(1),
    "disposition"
  ))
  list(
    name = if (length(names) == 1L) names[[1L]] else NULL,
    names = names,
    kind = if (length(kinds) == 1L) kinds[[1L]] else kinds,
    candidate = if (length(candidates) == 1L) candidates[[1L]] else NULL,
    candidates = candidates,
    source = source,
    disposition = if (length(dispositions) == 1L) {
      dispositions[[1L]]
    } else {
      "converted"
    }
  )
}

.builder_state_immune_requested_source <- function(entry, id) {
  sources <- .builder_state_optional_setting_record(
    entry,
    "content_sources",
    "invalid_content_sources",
    "sources"
  )
  if (is.null(sources)) {
    return(NULL)
  }
  source <- .subset2(sources, id)
  if (is.null(source)) {
    return(NULL)
  }
  if (!.builder_state_fact_text(source)) {
    .builder_state_abort(
      "invalid_content_source",
      "A selected optional-content source is invalid."
    )
  }
  source
}

.builder_state_validate_content_sources <- function(entry) {
  sources <- .builder_state_optional_setting_record(
    entry,
    "content_sources",
    "invalid_content_sources",
    "sources"
  )
  if (is.null(sources)) {
    return(invisible(NULL))
  }
  source_ids <- attr(sources, "names", exact = TRUE)
  allowed_ids <- c("immune_repertoire", "hla_tcr_motifs")
  if (
    is.null(source_ids) ||
      length(source_ids) != length(sources) ||
      anyNA(source_ids) ||
      any(!nzchar(source_ids)) ||
      anyDuplicated(source_ids) ||
      any(!source_ids %in% allowed_ids)
  ) {
    .builder_state_abort(
      "invalid_content_sources",
      paste0(
        "Optional-content sources must be a named list of known ",
        "capabilities."
      )
    )
  }
  valid_values <- vapply(
    sources,
    .builder_state_fact_text,
    logical(1)
  )
  if (!all(valid_values)) {
    .builder_state_abort(
      "invalid_content_source",
      "A selected optional-content source is invalid."
    )
  }
  invisible(sources)
}

.builder_state_immune_selection <- function(entry, fact, gate, id, fallback) {
  candidates <- .subset2(fact, "candidates")
  requested <- .builder_state_immune_requested_source(entry, id)
  if (!is.list(candidates) || !length(candidates)) {
    if (!is.null(requested)) {
      return(structure(
        list(reason = "selected_source_is_not_ready"),
        class = "builder_invalid_immune_source"
      ))
    }
    return(NULL)
  }
  candidate_names <- names(candidates)
  if (
    is.null(candidate_names) ||
      anyNA(candidate_names) ||
      any(!nzchar(candidate_names)) ||
      anyDuplicated(candidate_names)
  ) {
    .builder_state_abort(
      "invalid_immune_candidates",
      "Immune-repertoire candidates must have stable source names."
    )
  }
  eligible <- candidate_names[vapply(
    candidates,
    function(candidate) {
      is.list(candidate) &&
        isTRUE(.subset2(candidate, "detected")) &&
        isTRUE(.subset2(candidate, gate)) &&
        (!identical(id, "hla_tcr_motifs") ||
          isTRUE(.subset2(candidate, "full_ir_ready")))
    },
    logical(1)
  )]
  if (!is.null(requested) && !requested %in% eligible) {
    return(structure(
      list(reason = "selected_source_is_not_ready"),
      class = "builder_invalid_immune_source"
    ))
  }
  overlaps <- if (identical(gate, "full_ir_ready")) {
    .builder_state_or(
      .subset2(fact, "full_source_overlaps"),
      .builder_state_or(.subset2(fact, "source_overlaps"), list())
    )
  } else {
    .builder_state_or(.subset2(fact, "motif_source_overlaps"), list())
  }
  divergent <- any(vapply(
    overlaps,
    function(overlap) {
      is.list(overlap) &&
        .subset2(overlap, "left") %in% eligible &&
        .subset2(overlap, "right") %in% eligible &&
        .subset2(overlap, "n_divergent") > 0
    },
    logical(1)
  ))
  incomplete_overlap <- identical(gate, "full_ir_ready") &&
    any(vapply(
      overlaps,
      function(overlap) {
        is.list(overlap) &&
          .subset2(overlap, "left") %in% eligible &&
          .subset2(overlap, "right") %in% eligible &&
          .subset2(overlap, "n_overlap") > 0L &&
          !isTRUE(.subset2(overlap, "equivalent"))
      },
      logical(1)
    ))
  sources_equivalent <- function() {
    eligible_overlaps <- Filter(
      function(overlap) {
        is.list(overlap) &&
          .subset2(overlap, "left") %in% eligible &&
          .subset2(overlap, "right") %in% eligible
      },
      overlaps
    )
    expected_pairs <- utils::combn(
      sort(eligible, method = "radix"),
      2L,
      simplify = FALSE
    )
    pair_key <- function(pair) {
      paste(sort(pair, method = "radix"), collapse = "\u001f")
    }
    observed_pairs <- unique(vapply(
      eligible_overlaps,
      function(overlap) {
        pair_key(c(
          .subset2(overlap, "left"),
          .subset2(overlap, "right")
        ))
      },
      character(1)
    ))
    expected_pair_keys <- vapply(expected_pairs, pair_key, character(1))
    length(observed_pairs) == length(expected_pair_keys) &&
      setequal(observed_pairs, expected_pair_keys) &&
      all(vapply(
        eligible_overlaps,
        function(overlap) isTRUE(.subset2(overlap, "equivalent")),
        logical(1)
      ))
  }
  complementary_legacy <- identical(gate, "full_ir_ready") &&
    setequal(eligible, c("legacy_bcr", "legacy_tcr"))
  unverified <- length(eligible) > 1L &&
    !complementary_legacy &&
    !sources_equivalent()
  decision_reason <- if (divergent) {
    "divergent_source_overlap"
  } else if (incomplete_overlap) {
    "incomplete_source_equivalence"
  } else if (unverified) {
    "unverified_source_equivalence"
  } else {
    NULL
  }
  if (!is.null(decision_reason) && is.null(requested)) {
    return(structure(
      list(reason = decision_reason),
      class = "builder_invalid_immune_source"
    ))
  }
  if (!is.null(decision_reason)) {
    selected <- requested
  } else {
    priority <- c(
      "attachment",
      "unified_misc",
      "metadata",
      "legacy_bcr",
      "legacy_tcr"
    )
    selected <- intersect(priority[seq_len(3L)], eligible)
    if (length(selected)) {
      selected <- selected[[1L]]
    } else {
      selected <- intersect(priority[4:5], eligible)
    }
    if (!length(selected) && length(eligible)) {
      selected <- eligible[[1L]]
    }
    if (!length(selected)) {
      return(NULL)
    }
  }
  .builder_state_immune_source(
    selected,
    candidates[selected],
    fallback
  )
}

.builder_state_immune_disposition <- function(choice, selection) {
  if (is.null(selection)) {
    return(NULL)
  }
  if (inherits(selection, "builder_invalid_immune_source")) {
    return("rejected")
  }
  if (is.null(choice)) {
    return(selection$disposition)
  }
  if (choice %in% c("filtered", "stored_only")) {
    return(choice)
  }
  if (identical(choice, selection$disposition)) {
    return(choice)
  }
  "rejected"
}

.builder_state_immune_evidence <- function(
  fact,
  selection,
  full_selection,
  motif_selection
) {
  evidence <- .builder_state_optional_evidence(fact)
  evidence$full_ir_ready <- !is.null(full_selection) &&
    !inherits(full_selection, "builder_invalid_immune_source")
  evidence$hla_tcr_ready <- !is.null(motif_selection) &&
    !inherits(motif_selection, "builder_invalid_immune_source")
  evidence$selected_sources <- if (
    is.null(selection) ||
      inherits(selection, "builder_invalid_immune_source")
  ) {
    character()
  } else {
    selection$names
  }
  evidence["selected_source"] <- list(
    if (length(evidence$selected_sources) == 1L) {
      evidence$selected_sources[[1L]]
    } else {
      NULL
    }
  )
  evidence$selected_candidates <- if (!length(evidence$selected_sources)) {
    list()
  } else {
    selection$candidates
  }
  selected_attention <- vapply(
    evidence$selected_candidates,
    function(candidate) isTRUE(.subset2(candidate, "attention")),
    logical(1)
  )
  evidence$attention <- any(selected_attention)
  evidence$attention_items <- unique(unlist(
    lapply(
      evidence$selected_candidates[selected_attention],
      function(candidate) {
        .builder_state_or(
          .subset2(candidate, "diagnostics"),
          character()
        )
      }
    ),
    use.names = FALSE
  ))
  evidence["selected_candidate"] <- list(
    if (length(evidence$selected_sources) == 1L) {
      evidence$selected_candidates[[1L]]
    } else {
      NULL
    }
  )
  if (inherits(selection, "builder_invalid_immune_source")) {
    evidence$diagnostics <- unique(c(
      evidence$diagnostics,
      selection$reason
    ))
  }
  evidence
}

.builder_state_immune_entries <- function(entry, fact, source) {
  full_selection <- .builder_state_immune_selection(
    entry,
    fact,
    "full_ir_ready",
    "immune_repertoire",
    source
  )
  motif_selection <- .builder_state_immune_selection(
    entry,
    fact,
    "hla_tcr_ready",
    "hla_tcr_motifs",
    source
  )
  immune_evidence <- .builder_state_immune_evidence(
    fact,
    full_selection,
    full_selection,
    motif_selection
  )
  motif_evidence <- .builder_state_immune_evidence(
    fact,
    motif_selection,
    full_selection,
    motif_selection
  )

  immune_choice <- .builder_state_content_choice(entry, "immune_repertoire")
  immune_filtered <- !is.null(immune_choice) &&
    immune_choice %in% c("filtered", "stored_only")
  motif_choice <- .builder_state_content_choice(entry, "hla_tcr_motifs")
  motif_filtered <- !is.null(motif_choice) &&
    motif_choice %in% c("filtered", "stored_only")
  candidates <- .builder_state_or(.subset2(fact, "candidates"), list())
  raw_motif_ready <- any(vapply(
    candidates,
    function(candidate) {
      is.list(candidate) &&
        isTRUE(.subset2(candidate, "detected")) &&
        isTRUE(.subset2(candidate, "hla_tcr_ready"))
    },
    logical(1)
  ))
  invalid_full_source <- inherits(
    full_selection,
    "builder_invalid_immune_source"
  )
  has_full_source <- !is.null(full_selection) && !invalid_full_source
  invalid_motif_source <- inherits(
    motif_selection,
    "builder_invalid_immune_source"
  )
  has_motif_source <- !is.null(motif_selection) && !invalid_motif_source
  motif_source_not_exportable <- raw_motif_ready &&
    !has_motif_source &&
    !invalid_motif_source
  if (motif_source_not_exportable) {
    motif_evidence$diagnostics <- unique(c(
      motif_evidence$diagnostics,
      "motif_source_not_exportable"
    ))
    if (!motif_filtered) {
      invalid_motif_source <- TRUE
    }
  }
  incompatible_sources <- has_full_source &&
    has_motif_source &&
    !immune_filtered &&
    !motif_filtered &&
    !all(motif_selection$names %in% full_selection$names)
  if (incompatible_sources) {
    immune_evidence$diagnostics <- unique(c(
      immune_evidence$diagnostics,
      "incompatible_immune_source_selection"
    ))
    motif_evidence$diagnostics <- unique(c(
      motif_evidence$diagnostics,
      "incompatible_immune_source_selection"
    ))
    invalid_full_source <- TRUE
    invalid_motif_source <- TRUE
  }
  full_payload_has_motif <- has_full_source &&
    any(vapply(
      full_selection$candidates,
      function(candidate) isTRUE(.subset2(candidate, "hla_tcr_ready")),
      logical(1)
    ))
  incompatible_pages <- (!immune_filtered &&
    motif_filtered &&
    full_payload_has_motif) ||
    (immune_filtered &&
      !motif_filtered &&
      has_motif_source)
  if (incompatible_pages) {
    immune_evidence$diagnostics <- unique(c(
      immune_evidence$diagnostics,
      "incompatible_immune_page_disposition"
    ))
    motif_evidence$diagnostics <- unique(c(
      motif_evidence$diagnostics,
      "incompatible_immune_page_disposition"
    ))
    invalid_full_source <- TRUE
    invalid_motif_source <- TRUE
  }
  immune_attention <- has_full_source &&
    isTRUE(immune_evidence$attention) &&
    !immune_filtered
  visible_immune_choice <- !is.null(immune_choice) && !immune_filtered
  immune_status <- if (
    invalid_full_source ||
      visible_immune_choice &&
        !has_full_source
  ) {
    "blocking"
  } else if (has_full_source || immune_filtered) {
    if (immune_attention) "attention" else "valid"
  } else if (!immune_evidence$detected || raw_motif_ready) {
    "not_applicable"
  } else {
    "blocking"
  }
  immune_disposition <- if (identical(immune_status, "not_applicable")) {
    NA_character_
  } else if (identical(immune_status, "blocking")) {
    "rejected"
  } else if (immune_filtered) {
    immune_choice
  } else {
    .builder_state_immune_disposition(immune_choice, full_selection)
  }
  if (identical(immune_disposition, "rejected")) {
    immune_status <- "blocking"
  }
  immune_visible <- immune_status %in%
    c("valid", "attention") &&
    immune_disposition %in% c("preserved", "converted", "attached")

  motif_attention <- has_motif_source &&
    isTRUE(motif_evidence$attention) &&
    !motif_filtered
  visible_motif_choice <- !is.null(motif_choice) && !motif_filtered
  motif_status <- if (
    invalid_motif_source ||
      visible_motif_choice &&
        !has_motif_source
  ) {
    "blocking"
  } else if (has_motif_source || motif_filtered) {
    if (motif_attention) "attention" else "valid"
  } else {
    "not_applicable"
  }
  motif_disposition <- if (identical(motif_status, "not_applicable")) {
    NA_character_
  } else if (identical(motif_status, "blocking")) {
    "rejected"
  } else if (motif_filtered) {
    motif_choice
  } else {
    .builder_state_immune_disposition(motif_choice, motif_selection)
  }
  if (identical(motif_disposition, "rejected")) {
    motif_status <- "blocking"
  }
  motif_visible <- motif_status %in%
    c("valid", "attention") &&
    motif_disposition %in% c("preserved", "converted", "attached")

  list(
    .builder_state_manifest_record(
      "immune_repertoire",
      if (has_full_source) full_selection$source else source,
      immune_status,
      immune_disposition,
      if (immune_visible) "immune_repertoire" else character(),
      immune_evidence,
      required_action = if (immune_attention) {
        .builder_state_attention_action("immune_repertoire", immune_evidence)
      } else {
        NULL
      },
      verifier = "verify_immune_repertoire"
    ),
    .builder_state_manifest_record(
      "hla_tcr_motifs",
      if (has_motif_source) motif_selection$source else source,
      motif_status,
      motif_disposition,
      if (motif_visible) "hla_tcr_motifs" else character(),
      motif_evidence,
      required_action = if (motif_attention) {
        .builder_state_attention_action("hla_tcr_motifs", motif_evidence)
      } else {
        NULL
      },
      verifier = "verify_hla_tcr_motifs"
    )
  )
}
