builder_immune_source_choices_model <- function(model, manifest) {
  fact <- model$immune_source_fact %||% list()
  candidates <- fact$candidates %||% list()
  if (!is.list(candidates) || !length(candidates)) {
    return(list())
  }
  candidate_ids <- names(candidates)
  if (
    is.null(candidate_ids) ||
      anyNA(candidate_ids) ||
      any(!nzchar(candidate_ids)) ||
      anyDuplicated(candidate_ids)
  ) {
    return(list())
  }
  selected_sources <- model$content_sources %||% list()
  if (!is.list(selected_sources) || is.object(selected_sources)) {
    selected_sources <- list()
  }
  labels <- c(
    unified_misc = "Unified immune repertoire",
    metadata = "Metadata annotations",
    legacy_bcr = "Legacy BCR data",
    legacy_tcr = "Legacy TCR data"
  )
  friendly_label <- function(id) {
    label <- unname(labels[id])
    if (length(label) == 1L && !is.na(label)) {
      return(label)
    }
    value <- gsub("[_.]+", " ", id)
    paste0(toupper(substr(value, 1L, 1L)), substr(value, 2L, nchar(value)))
  }
  ambiguity_codes <- c(
    "divergent_source_overlap",
    "incomplete_source_equivalence",
    "unverified_source_equivalence",
    "incompatible_immune_source_selection"
  )
  pair_key <- function(left, right) {
    paste(sort(c(left, right), method = "radix"), collapse = "\u001f")
  }
  raw_ambiguity <- function(capability, eligible) {
    if (length(eligible) < 2L) {
      return(FALSE)
    }
    if (
      identical(capability, "immune_repertoire") &&
        setequal(eligible, c("legacy_bcr", "legacy_tcr"))
    ) {
      return(FALSE)
    }
    overlaps <- if (identical(capability, "immune_repertoire")) {
      fact$full_source_overlaps %||% fact$source_overlaps %||% list()
    } else {
      fact$motif_source_overlaps %||% list()
    }
    overlaps <- Filter(
      function(overlap) {
        is.list(overlap) &&
          overlap$left %in% eligible &&
          overlap$right %in% eligible
      },
      overlaps
    )
    if (
      any(vapply(
        overlaps,
        function(overlap) (overlap$n_divergent %||% 0L) > 0L,
        logical(1)
      ))
    ) {
      return(TRUE)
    }
    expected <- utils::combn(
      sort(eligible, method = "radix"),
      2L,
      simplify = FALSE
    )
    expected <- vapply(
      expected,
      function(pair) pair_key(pair[[1L]], pair[[2L]]),
      character(1)
    )
    observed <- unique(vapply(
      overlaps,
      function(overlap) pair_key(overlap$left, overlap$right),
      character(1)
    ))
    !setequal(expected, observed) ||
      !all(vapply(
        overlaps,
        function(overlap) isTRUE(overlap$equivalent),
        logical(1)
      ))
  }
  specs <- list(
    immune_repertoire = list(
      gate = "full_ir_ready",
      title = "Immune repertoire source"
    ),
    hla_tcr_motifs = list(
      gate = "hla_tcr_ready",
      title = "HLA & TCR motif source"
    )
  )
  capability_models <- lapply(names(specs), function(capability) {
    spec <- specs[[capability]]
    eligible <- candidate_ids[vapply(
      candidates,
      function(candidate) {
        is.list(candidate) &&
          isTRUE(candidate$detected) &&
          isTRUE(candidate[[spec$gate]]) &&
          (!identical(capability, "hla_tcr_motifs") ||
            isTRUE(candidate$full_ir_ready))
      },
      logical(1)
    )]
    record <- manifest[[capability]] %||% list()
    diagnostics <- record$evidence$diagnostics %||% character()
    list(
      capability = capability,
      eligible = eligible,
      needed = length(eligible) > 0L &&
        !identical(record$status %||% "", "not_applicable") &&
        !(record$disposition %||% "") %in% c("filtered", "stored_only"),
      ambiguous = raw_ambiguity(capability, eligible) ||
        any(ambiguity_codes %in% diagnostics)
    )
  })
  names(capability_models) <- names(specs)
  needed <- names(capability_models)[vapply(
    capability_models,
    `[[`,
    logical(1),
    "needed"
  )]
  ambiguous <- needed[vapply(
    capability_models[needed],
    `[[`,
    logical(1),
    "ambiguous"
  )]
  if (!length(ambiguous)) {
    return(list())
  }
  targets <- if (all(c("immune_repertoire", "hla_tcr_motifs") %in% needed)) {
    c("immune_repertoire", "hla_tcr_motifs")
  } else {
    ambiguous[[1L]]
  }
  eligible <- Reduce(
    intersect,
    lapply(targets, function(capability) {
      capability_models[[capability]]$eligible
    })
  )
  if (!length(eligible)) {
    return(list())
  }
  explicit <- unname(unlist(selected_sources[targets], use.names = FALSE))
  explicit <- explicit[
    is.character(explicit) & !is.na(explicit) & nzchar(explicit)
  ]
  selected <- if (
    length(explicit) == length(targets) &&
      length(unique(explicit)) == 1L &&
      explicit[[1L]] %in% eligible
  ) {
    explicit[[1L]]
  } else {
    ""
  }
  title <- if (length(targets) > 1L) {
    "Immune data source"
  } else {
    specs[[targets[[1L]]]]$title
  }
  choice_labels <- vapply(eligible, friendly_label, character(1))
  list(list(
    capability = targets[[1L]],
    targets = targets,
    title = title,
    choices = stats::setNames(eligible, choice_labels),
    selected = selected,
    resolved = nzchar(selected)
  ))
}

builder_apply_immune_source_choice <- function(entry, selector, value) {
  if (
    !is.list(entry) ||
      !is.list(entry$settings) ||
      !is.list(selector) ||
      !is.character(selector$targets) ||
      !length(selector$targets) ||
      anyNA(selector$targets) ||
      any(!nzchar(selector$targets)) ||
      anyDuplicated(selector$targets) ||
      !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !value %in% unname(selector$choices %||% character())
  ) {
    return(entry)
  }
  sources <- entry$settings$content_sources %||% list()
  if (!is.list(sources) || is.object(sources)) {
    sources <- list()
  }
  for (target in selector$targets) {
    sources[[target]] <- value
  }
  entry$settings$content_sources <- sources
  entry
}

builder_content_action_owner_is_valid <- function(owner) {
  valid_counter <- function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value >= 0 &&
      identical(as.double(value), floor(as.double(value)))
  }
  is.list(owner) &&
    valid_counter(owner$generation) &&
    valid_counter(owner$project_epoch) &&
    builder_stage_has_text(owner$owner_token %||% "")
}

builder_content_action_owner <- function(entry, project_epoch) {
  snapshot <- if (is.list(entry)) entry$snapshot else NULL
  owner <- list(
    generation = if (is.list(entry)) entry$revision else NULL,
    project_epoch = project_epoch,
    owner_token = if (is.list(snapshot)) snapshot$owner_token else NULL
  )
  if (!builder_content_action_owner_is_valid(owner)) {
    return(NULL)
  }
  list(
    generation = as.double(owner$generation),
    project_epoch = as.double(owner$project_epoch),
    owner_token = owner$owner_token
  )
}

builder_content_action_owner_is_current <- function(
  request,
  entry,
  project_epoch
) {
  expected <- builder_content_action_owner(entry, project_epoch)
  if (is.null(expected) || !is.list(request)) {
    return(FALSE)
  }
  received <- list(
    generation = request$generation,
    project_epoch = request$project_epoch,
    owner_token = request$owner_token
  )
  if (!builder_content_action_owner_is_valid(received)) {
    return(FALSE)
  }
  identical(as.double(received$generation), expected$generation) &&
    identical(as.double(received$project_epoch), expected$project_epoch) &&
    identical(received$owner_token, expected$owner_token)
}

builder_acknowledgement_action <- function(
  record,
  acknowledgements,
  dataset,
  id,
  owner = NULL
) {
  action <- record$required_action %||% list()
  token <- action$token %||% ""
  if (
    !identical(record$status %||% "", "attention") ||
      !identical(action$type %||% "", "acknowledge") ||
      !builder_stage_has_text(token) ||
      token %in% acknowledgements ||
      !builder_stage_has_text(dataset %||% "") ||
      !builder_stage_has_text(id %||% "") ||
      !builder_content_action_owner_is_valid(owner)
  ) {
    return(NULL)
  }
  c(
    list(dataset = dataset, capability = id, token = token),
    owner[c("generation", "project_epoch", "owner_token")]
  )
}

builder_acknowledgement_button <- function(
  id,
  action,
  label = "Acknowledge and continue"
) {
  if (!is.list(action)) {
    return(NULL)
  }
  ns <- if (is.null(id)) identity else NS(id)
  tags$button(
    type = "button",
    class = "btn btn-default builder-acknowledgement-action",
    `data-builder-acknowledge` = "true",
    `data-input-id` = ns("acknowledge_content"),
    `data-dataset` = action$dataset,
    `data-capability` = action$capability,
    `data-token` = action$token,
    `data-generation` = action$generation,
    `data-project-epoch` = action$project_epoch,
    `data-owner-token` = action$owner_token,
    label
  )
}

builder_content_disposition_action <- function(
  record,
  dataset,
  id,
  owner = NULL
) {
  if (
    !is.list(record) ||
      !builder_stage_has_text(dataset %||% "") ||
      !builder_stage_has_text(id %||% "") ||
      !builder_content_action_owner_is_valid(owner)
  ) {
    return(NULL)
  }
  disposition <- record$disposition %||% ""
  if (disposition %in% c("filtered", "stored_only")) {
    return(list(
      dataset = dataset,
      capability = id,
      disposition = "auto",
      label = "Include in CRB",
      generation = owner$generation,
      project_epoch = owner$project_epoch,
      owner_token = owner$owner_token
    ))
  }
  if (
    isTRUE(record$evidence$detected) &&
      ((record$status %||% "") %in%
        c("attention", "blocking") ||
        identical(disposition, "rejected"))
  ) {
    return(list(
      dataset = dataset,
      capability = id,
      disposition = "filtered",
      label = "Exclude from CRB",
      generation = owner$generation,
      project_epoch = owner$project_epoch,
      owner_token = owner$owner_token
    ))
  }
  NULL
}

builder_content_disposition_button <- function(id, action) {
  if (!is.list(action)) {
    return(NULL)
  }
  ns <- if (is.null(id)) identity else NS(id)
  tags$button(
    type = "button",
    class = "btn btn-default builder-content-disposition-action",
    `data-builder-content-disposition` = "true",
    `data-input-id` = ns("content_disposition"),
    `data-dataset` = action$dataset,
    `data-capability` = action$capability,
    `data-disposition` = action$disposition,
    `data-generation` = action$generation,
    `data-project-epoch` = action$project_epoch,
    `data-owner-token` = action$owner_token,
    action$label
  )
}

builder_specialized_content_model <- function(model) {
  manifest <- model$content_manifest %||%
    model$analysis_manifest %||%
    model$manifest %||%
    list()
  acknowledgements <- model$content_acknowledgements %||%
    model$analysis_acknowledgements %||%
    model$acknowledgements %||%
    character()
  dataset <- model$id %||% NULL
  action_owner <- model$content_action_owner %||% NULL
  count_value <- function(value) {
    if (
      is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value >= 0
    ) {
      as.integer(value)
    } else {
      0L
    }
  }
  plural <- function(value, singular, plural_form = paste0(singular, "s")) {
    paste0(
      value,
      " ",
      if (identical(value, 1L)) singular else plural_form
    )
  }
  record_state <- function(record) {
    action <- record$required_action %||% list()
    disposition <- record$disposition %||% ""
    acknowledged <- identical(record$status %||% "", "attention") &&
      identical(action$type %||% "", "acknowledge") &&
      (action$token %||% "") %in% acknowledgements
    if (
      disposition %in%
        c("filtered", "stored_only") ||
        identical(record$status %||% "", "not_applicable")
    ) {
      return("not_included")
    }
    if (
      !acknowledged &&
        ((record$status %||% "") %in%
          c("attention", "blocking") ||
          identical(disposition, "rejected"))
    ) {
      return("needs_attention")
    }
    if (
      (identical(record$status %||% "", "valid") || acknowledged) &&
        disposition %in% c("preserved", "generated", "converted", "attached")
    ) {
      return("included")
    }
    "not_included"
  }
  content_item <- function(
    id,
    label,
    metrics,
    message,
    directory = character(),
    attention_message = NULL,
    record_override = NULL
  ) {
    record <- record_override %||% manifest[[id]]
    if (!is.list(record)) {
      return(NULL)
    }
    evidence <- record$evidence %||% list()
    if (!isTRUE(evidence$detected)) {
      return(NULL)
    }
    state <- record_state(record)
    if (identical(state, "needs_attention")) {
      message <- attention_message %||%
        paste(
          "This content cannot be used yet.",
          "Review the source data before building."
        )
    } else if (identical(state, "not_included")) {
      message <- "This content will not be retained in the CRB."
    }
    action_token <- record$required_action$token %||% ""
    action_capability <- names(Filter(
      function(candidate) {
        is.list(candidate) &&
          identical(candidate$required_action$token %||% "", action_token)
      },
      manifest
    ))
    if (
      length(action_capability) != 1L || !builder_stage_has_text(action_token)
    ) {
      action_capability <- id
    }
    list(
      id = id,
      label = label,
      state = state,
      metrics = metrics,
      message = message,
      directory = directory,
      acknowledgement = builder_acknowledgement_action(
        record,
        acknowledgements,
        dataset,
        action_capability,
        action_owner
      ),
      disposition_action = builder_content_disposition_action(
        record,
        dataset,
        action_capability,
        action_owner
      )
    )
  }
  spatial_record <- manifest$spatial %||% list()
  spatial <- spatial_record$evidence$normalized %||% list()
  spatial_sections <- spatial$sections %||% list()
  tissue_image_count <- as.integer(sum(vapply(
    spatial_sections,
    function(section) {
      is.list(section) &&
        is.list(section$raster) &&
        isTRUE(section$raster$present) &&
        isTRUE(section$raster$valid)
    },
    logical(1)
  )))
  spatial_metrics <- c(
    plural(count_value(spatial$section_count), "section"),
    paste(count_value(spatial$valid_section_count), "ready"),
    if (isTRUE(spatial$sections_truncated)) {
      paste0(">=", tissue_image_count, " tissue images in preview")
    } else {
      plural(tissue_image_count, "tissue image")
    }
  )

  trekker_record <- manifest$trekker %||% list()
  trekker <- trekker_record$evidence$normalized %||% list()
  coverage <- trekker$barcode_coverage %||% 0
  if (
    !is.numeric(coverage) ||
      length(coverage) != 1L ||
      is.na(coverage) ||
      !is.finite(coverage)
  ) {
    coverage <- 0
  }
  coverage <- min(1, max(0, coverage))
  trekker_metrics <- c(
    plural(count_value(trekker$cell_count), "cell"),
    plural(count_value(trekker$cluster_count), "cluster"),
    plural(count_value(trekker$field_count), "field"),
    paste0(round(100 * coverage), "% coverage"),
    if (count_value(trekker$moran_count) > 0L) {
      plural(count_value(trekker$moran_count), "Moran result")
    },
    if (count_value(trekker$evidence_count) > 0L) {
      plural(count_value(trekker$evidence_count), "evidence image")
    }
  )

  immune_record <- manifest$immune_repertoire %||% list()
  motif_record <- manifest$hla_tcr_motifs %||% list()
  motif_evidence <- motif_record$evidence %||% list()
  immune_evidence <- immune_record$evidence %||% list()
  immune <- immune_evidence$normalized %||% list()
  selected_immune <- immune_evidence$selected_candidate$normalized %||%
    list()
  chains <- selected_immune$chains %||% immune$chains %||% character()
  immune_diagnostics <- unique(c(
    immune_evidence$diagnostics %||% character(),
    immune_evidence$selected_candidate$diagnostics %||% character(),
    motif_evidence$diagnostics %||% character(),
    motif_evidence$selected_candidate$diagnostics %||% character()
  ))
  immune_attention_message <- if (
    "no_dataset_barcode_overlap" %in% immune_diagnostics
  ) {
    paste(
      "Immune cell barcodes do not match this dataset.",
      "Check barcode prefixes or choose the matching dataset."
    )
  } else if ("barcodes_outside_dataset" %in% immune_diagnostics) {
    paste(
      "Some immune cell barcodes do not match this dataset.",
      "Check barcode prefixes before building."
    )
  } else if ("duplicate_barcodes" %in% immune_diagnostics) {
    "Immune cell barcodes are duplicated. Make them unique before building."
  } else if ("motif_source_not_exportable" %in% immune_diagnostics) {
    paste(
      "TCR motif data was detected, but its repertoire is incomplete.",
      "Add the complete repertoire columns or exclude this content."
    )
  } else if ("incompatible_immune_page_disposition" %in% immune_diagnostics) {
    paste(
      "Immune repertoire and HLA & TCR motifs share one data payload.",
      "Include or exclude the two pages together."
    )
  } else if ("incompatible_immune_source_selection" %in% immune_diagnostics) {
    paste(
      "The detected immune sources cannot support both content types together.",
      "Keep one compatible source or reconcile the source data."
    )
  } else if (
    any(
      c(
        "divergent_source_overlap",
        "incomplete_source_equivalence",
        "unverified_source_equivalence"
      ) %in%
        immune_diagnostics
    )
  ) {
    paste(
      "Multiple immune-data sources disagree.",
      "Review the source data before building."
    )
  } else {
    NULL
  }
  immune_metrics <- c(
    plural(count_value(selected_immune$n_rows %||% immune$n_rows), "record"),
    plural(
      count_value(selected_immune$n_samples %||% immune$n_samples),
      "sample"
    ),
    plural(length(unique(as.character(chains))), "chain")
  )

  immune_display_record <- immune_record
  if (
    identical(record_state(motif_record), "needs_attention") &&
      !identical(record_state(immune_record), "needs_attention")
  ) {
    immune_display_record$status <- motif_record$status
    immune_display_record$disposition <- motif_record$disposition
    immune_display_record$required_action <- motif_record$required_action
    immune_display_record$evidence <- immune_evidence
    immune_display_record$evidence$detected <- isTRUE(
      immune_evidence$detected
    ) ||
      isTRUE(motif_evidence$detected)
  }

  hla_record <- manifest$hla %||% list()
  hla <- hla_record$evidence$normalized %||% list()
  motif_ready <- isTRUE(motif_evidence$detected) &&
    identical(record_state(motif_record), "included") &&
    "hla_tcr_motifs" %in% (motif_record$pages %||% character())
  hla_metrics <- c(
    plural(count_value(hla$n_samples), "sample"),
    plural(count_value(hla$n_loci), "locus", "loci"),
    plural(count_value(hla$n_alleles), "allele")
  )

  gene_lists_record <- manifest$gene_lists %||% list()
  gene_lists <- gene_lists_record$evidence$normalized %||% list()

  extra_record <- manifest$extra_material %||% list()
  extra <- extra_record$evidence$normalized %||% list()
  table_names <- names(extra$tables %||% list()) %||% character()
  plot_names <- names(extra$plots %||% list()) %||% character()
  friendly_name <- function(value) {
    value <- gsub("[_.]+", " ", value)
    if (tolower(value) %in% c("qc", "hla", "tcr", "bcr", "pca", "umap")) {
      return(toupper(value))
    }
    paste0(toupper(substr(value, 1L, 1L)), substr(value, 2L, nchar(value)))
  }
  directory_line <- function(label, values) {
    if (!length(values)) {
      return(character())
    }
    shown <- utils::head(vapply(values, friendly_name, character(1)), 3L)
    suffix <- if (length(values) > length(shown)) {
      paste0(" +", length(values) - length(shown), " more")
    } else {
      ""
    }
    paste0(label, ": ", paste(shown, collapse = ", "), suffix)
  }

  items <- Filter(
    Negate(is.null),
    list(
      content_item(
        "spatial",
        "Spatial",
        spatial_metrics,
        "Available on the Spatial page."
      ),
      content_item(
        "trekker",
        "Trekker",
        trekker_metrics,
        paste(
          "Paired transcriptome and physical coordinates are available",
          "on the Trekker page."
        )
      ),
      content_item(
        "immune_repertoire",
        "Immune repertoire",
        immune_metrics,
        "Available on the Immune repertoire page.",
        attention_message = immune_attention_message,
        record_override = immune_display_record
      ),
      content_item(
        "hla",
        "HLA",
        hla_metrics,
        if (motif_ready) {
          "Supports the HLA & TCR motifs page."
        } else {
          "Supporting HLA typing will be preserved with this dataset."
        }
      ),
      content_item(
        "gene_lists",
        "Gene lists",
        c(
          plural(count_value(gene_lists$n_lists), "list"),
          plural(count_value(gene_lists$n_genes), "gene")
        ),
        "Available in Analysis info."
      ),
      content_item(
        "extra_material",
        "Extra material",
        c(
          plural(length(table_names), "table"),
          plural(length(plot_names), "plot")
        ),
        "Available on the Extra material page.",
        c(
          directory_line("Tables", table_names),
          directory_line("Plots", plot_names)
        )
      )
    )
  )
  states <- if (length(items)) {
    vapply(items, `[[`, character(1), "state")
  } else {
    character()
  }
  included_count <- as.integer(sum(states == "included"))
  attention_count <- as.integer(sum(states == "needs_attention"))
  excluded_count <- as.integer(sum(states == "not_included"))
  summary <- c(
    if (included_count > 0L) paste(included_count, "included"),
    if (attention_count > 0L) {
      paste(
        attention_count,
        if (identical(attention_count, 1L)) {
          "needs attention"
        } else {
          "need attention"
        }
      )
    },
    if (excluded_count > 0L) paste(excluded_count, "not included")
  )
  list(
    items = unname(items),
    total_count = as.integer(length(items)),
    included_count = included_count,
    attention_count = attention_count,
    excluded_count = excluded_count,
    summary = paste(summary, collapse = " · "),
    immune_source_selectors = builder_immune_source_choices_model(
      model,
      manifest
    )
  )
}

builder_specialized_content_ui <- function(model, id = NULL) {
  ns <- if (is.null(id)) identity else NS(id)
  badge <- function(state) {
    switch(
      state,
      included = "Included",
      needs_attention = "Needs attention",
      not_included = "Not included",
      "Available"
    )
  }
  div(
    class = "viewer-specialized-content",
    lapply(model$items %||% list(), function(item) {
      tags$article(
        class = paste(
          "viewer-specialized-item",
          paste0("is-", gsub("_", "-", item$state))
        ),
        div(
          class = "viewer-specialized-head",
          h4(item$label),
          span(class = "viewer-specialized-badge", badge(item$state))
        ),
        p(
          class = "viewer-specialized-metrics",
          paste(item$metrics, collapse = " · ")
        ),
        lapply(item$directory, function(line) {
          p(class = "viewer-specialized-directory", line)
        }),
        p(class = "viewer-specialized-page", item$message),
        div(
          class = "builder-content-actions",
          builder_acknowledgement_button(id, item$acknowledgement),
          builder_content_disposition_button(id, item$disposition_action)
        )
      )
    }),
    lapply(model$immune_source_selectors %||% list(), function(selector) {
      tags$article(
        class = paste(
          "viewer-immune-source-selector",
          if (isTRUE(selector$resolved)) "is-resolved" else ""
        ),
        h4(selector$title),
        p(
          class = "viewer-immune-source-copy",
          "Choose which detected source to retain in the CRB."
        ),
        selectInput(
          ns(paste0("immune_source_", selector$capability)),
          "Use this source",
          choices = if (isTRUE(selector$resolved)) {
            selector$choices
          } else {
            c(
              stats::setNames("", "Choose a source\u2026"),
              selector$choices
            )
          },
          selected = selector$selected,
          width = "100%"
        )
      )
    })
  )
}

builder_analysis_results_model <- function(model) {
  manifest <- model$analysis_manifest %||% model$manifest %||% list()
  acknowledgements <- model$analysis_acknowledgements %||%
    model$acknowledgements %||%
    character()
  dataset <- model$id %||% NULL
  action_owner <- model$content_action_owner %||% NULL
  specs <- list(
    marker_genes = list(
      label = "Marker genes",
      page = "Marker genes",
      nested = TRUE
    ),
    most_expressed_genes = list(
      label = "Most expressed genes",
      page = "Most expressed genes",
      nested = FALSE
    ),
    mean_expression = list(
      label = "Mean expression",
      page = "Most expressed genes",
      nested = FALSE
    ),
    enriched_pathways = list(
      label = "Enriched pathways",
      page = "Enriched pathways",
      nested = TRUE
    )
  )
  named_values <- function(value) {
    if (!is.list(value) || !length(value)) {
      return(character())
    }
    ids <- names(value)
    if (is.null(ids)) character() else ids[nzchar(ids)]
  }
  count_tables <- function(values) {
    if (!is.list(values) || !length(values)) {
      return(0L)
    }
    as.integer(sum(vapply(
      values,
      function(value) is.list(value) && identical(value$kind, "table"),
      logical(1)
    )))
  }
  summarize <- function(id, spec) {
    record <- manifest[[id]]
    if (!is.list(record)) {
      return(NULL)
    }
    evidence <- record$evidence %||% list()
    detected <- isTRUE(evidence$detected)
    disposition <- record$disposition %||% NA_character_
    action <- record$required_action
    acknowledged <- identical(record$status, "attention") &&
      is.list(action) &&
      identical(action$type, "acknowledge") &&
      action$token %in% acknowledgements
    needs_attention <- !acknowledged &&
      (identical(record$status, "attention") ||
        identical(record$status, "blocking") ||
        identical(disposition, "rejected"))
    generated <- identical(disposition, "generated")
    excluded <- disposition %in% c("filtered", "stored_only")
    if (!detected && !generated && !needs_attention && !excluded) {
      return(NULL)
    }
    normalized <- evidence$normalized %||% list()
    if (!is.list(normalized)) {
      normalized <- list()
    }
    if (isTRUE(spec$nested)) {
      method_count <- length(named_values(normalized))
      group_names <- unique(unlist(
        lapply(normalized, named_values),
        use.names = FALSE
      ))
      leaves <- unlist(
        lapply(normalized, function(value) {
          if (is.list(value)) unname(value) else list()
        }),
        recursive = FALSE,
        use.names = FALSE
      )
    } else {
      method_count <- 0L
      group_names <- named_values(normalized)
      leaves <- unname(normalized)
    }
    status <- if (needs_attention) {
      "needs_attention"
    } else if (generated) {
      "will_be_generated"
    } else if (excluded) {
      "not_included"
    } else {
      "existing"
    }
    list(
      id = id,
      label = spec$label,
      page = spec$page,
      page_message = if (needs_attention) {
        paste0(
          "The ",
          spec$page,
          " page stays unavailable until this is fixed."
        )
      } else if (excluded) {
        "This result will not be retained in the CRB."
      } else if (identical(id, "mean_expression")) {
        paste(
          "Used by the Most expressed genes page",
          "when that page is available."
        )
      } else {
        paste0("Shown on the ", spec$page, " page.")
      },
      status = status,
      method_count = as.integer(method_count),
      group_count = as.integer(length(group_names)),
      table_count = count_tables(leaves),
      acknowledgement = builder_acknowledgement_action(
        record,
        acknowledgements,
        dataset,
        id,
        action_owner
      ),
      disposition_action = builder_content_disposition_action(
        record,
        dataset,
        id,
        action_owner
      )
    )
  }
  items <- Filter(
    Negate(is.null),
    Map(summarize, names(specs), specs)
  )
  statuses <- if (length(items)) {
    vapply(items, `[[`, character(1), "status")
  } else {
    character()
  }
  list(
    items = unname(items),
    total_count = as.integer(length(items)),
    existing_count = as.integer(sum(statuses == "existing")),
    generated_count = as.integer(sum(statuses == "will_be_generated")),
    attention_count = as.integer(sum(statuses == "needs_attention")),
    excluded_count = as.integer(sum(statuses == "not_included"))
  )
}

builder_analysis_results_ui <- function(model, id = NULL) {
  plural <- function(value, singular) {
    paste0(value, " ", singular, if (identical(value, 1L)) "" else "s")
  }
  status_label <- function(status) {
    switch(
      status,
      existing = "Existing",
      will_be_generated = "Will be generated",
      needs_attention = "Needs attention",
      not_included = "Not included",
      "Available"
    )
  }
  div(
    class = "viewer-analysis-results",
    lapply(model$items %||% list(), function(item) {
      metrics <- c(
        if (item$method_count > 0L) plural(item$method_count, "method"),
        plural(item$group_count, "group"),
        plural(item$table_count, "table")
      )
      tags$article(
        class = paste("viewer-analysis-result", paste0("is-", item$status)),
        div(
          class = "viewer-analysis-result-head",
          h4(item$label),
          span(
            class = "viewer-analysis-result-status",
            status_label(item$status)
          )
        ),
        if (item$status == "needs_attention") {
          p(
            class = "viewer-analysis-result-action",
            "This result cannot be used yet. Recompute it or review its source."
          )
        } else if (item$status == "will_be_generated") {
          p(
            class = "viewer-analysis-result-metrics",
            "Created during build."
          )
        } else if (item$status == "not_included") {
          p(
            class = "viewer-analysis-result-metrics",
            "Excluded from the CRB."
          )
        } else {
          p(
            class = "viewer-analysis-result-metrics",
            paste(metrics, collapse = " · ")
          )
        },
        p(
          class = "viewer-analysis-result-page",
          item$page_message
        ),
        div(
          class = "builder-content-actions",
          builder_acknowledgement_button(id, item$acknowledgement),
          builder_content_disposition_button(id, item$disposition_action)
        )
      )
    })
  )
}
