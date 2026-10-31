## -------------------------------------------------------------------------
## Builder planning contracts.
##
## The builder UI is only a front end for these decisions. Keeping them in
## small pure helpers makes the expensive worker predictable and lets us test
## edge cases without starting Shiny.
## -------------------------------------------------------------------------

builder_plan_contract_source_runtime <- function(local = parent.frame()) {
  helper <- environment(builder_plan_contract_source_runtime)
  targets <- if (identical(helper, local)) list(local) else list(helper, local)
  for (target in targets) {
    builder_profile_source_runtime(target)
    builder_repo_source("marker_import.R", local = target)
    builder_repo_source("prerequisite.R", local = target)
    builder_repo_source("recommend.R", local = target)
    builder_repo_source("state.R", local = target)
  }
  invisible(local)
}


builder_task6_fact <- function(
  detected = FALSE,
  valid = TRUE,
  page_candidates = character(),
  diagnostics = character(),
  ...
) {
  c(
    list(
      detected = detected,
      valid = valid,
      normalized = list(rows = if (detected) 1L else 0L),
      diagnostics = diagnostics,
      requirements = character(),
      page_candidates = page_candidates
    ),
    list(...)
  )
}

builder_task6_snapshot_identity <- function() {
  structure(
    list(
      path = "/private/builder/snapshot-a",
      object_file = "/private/builder/snapshot-a/object.rds",
      owner_token = "snapshot-owner",
      created_at = as.POSIXct("2026-08-04 12:00:00", tz = "UTC"),
      object_md5 = strrep("a", 32L),
      closure_bytes = 1024
    ),
    class = c("builder_snapshot_identity", "list")
  )
}

builder_task6_asset_claim <- function(source, target, artifact) {
  structure(
    list(
      source = source,
      target = target,
      artifact = artifact
    ),
    class = c("builder_asset_claim", "list")
  )
}

builder_task6_immune_candidate <- function(
  source_kind,
  full_ir_ready,
  hla_tcr_ready = FALSE
) {
  .builder_immune_record(
    detected = TRUE,
    valid = isTRUE(full_ir_ready),
    normalized = list(
      chains = if (hla_tcr_ready) "TRA" else "IGH"
    ),
    page_candidates = c(
      if (full_ir_ready) "immune_repertoire",
      if (hla_tcr_ready) "hla_tcr_motifs"
    ),
    source_kind = source_kind,
    full_ir_ready = isTRUE(full_ir_ready),
    hla_tcr_ready = isTRUE(hla_tcr_ready),
    parseable_tcr_chains = if (hla_tcr_ready) "TRA" else character(),
    parseable_tcr_row_count = if (hla_tcr_ready) 1L else 0L,
    preview_truncated_count = 0L,
    missing_columns = character(),
    expected_chains = character(),
    unexpected_chains = character()
  )
}

builder_task6_immune_fact <- function(candidates) {
  full_sources <- names(candidates)[vapply(
    candidates,
    function(candidate) isTRUE(candidate$full_ir_ready),
    logical(1)
  )]
  motif_sources <- names(candidates)[vapply(
    candidates,
    function(candidate) isTRUE(candidate$hla_tcr_ready),
    logical(1)
  )]
  .builder_immune_record(
    detected = length(candidates) > 0L,
    valid = length(full_sources) > 0L,
    normalized = list(
      available_sources = full_sources,
      available_tcr_sources = motif_sources
    ),
    requirements = c(
      "one_valid_source",
      "source_priority_is_deferred_to_build_plan"
    ),
    page_candidates = c(
      if (length(full_sources)) "immune_repertoire",
      if (length(motif_sources)) "hla_tcr_motifs"
    ),
    candidates = candidates,
    source_overlaps = list(),
    available_sources = full_sources,
    available_tcr_sources = motif_sources,
    selected_source = NULL
  )
}

builder_task6_final_metadata_policy <- function(policy, decisions = list()) {
  for (id in names(decisions)) {
    disposition <- decisions[[id]]
    record <- policy$columns[[id]]
    record$value <- disposition
    record$disposition <- disposition
    if (identical(disposition, "included")) {
      record$effective_included <- TRUE
      record$retain_in_crb <- TRUE
    } else if (identical(disposition, "excluded")) {
      record$effective_included <- FALSE
      record$retain_in_crb <- FALSE
    }
    record$requires_confirmation <- disposition %in%
      c("attention", "blocking")
    policy$columns[[id]] <- record
  }
  dispositions <- vapply(
    policy$columns,
    `[[`,
    character(1),
    "disposition"
  )
  effective <- vapply(
    policy$columns,
    `[[`,
    logical(1),
    "effective_included"
  )
  ids <- names(policy$columns)
  for (id in ids) {
    policy$columns[[id]]$group_enabled <-
      isTRUE(policy$columns[[id]]$group_enabled)
    policy$columns[[id]]$forced <- isTRUE(
      policy$columns[[id]]$forced %||% policy$columns[[id]]$required
    )
  }
  retained <- ids[vapply(
    policy$columns,
    function(record) isTRUE(record$retain_in_crb),
    logical(1)
  )]
  policy$retained <- retained
  policy$groups <- ids[vapply(
    policy$columns,
    function(record) isTRUE(record$group_enabled),
    logical(1)
  )]
  policy$forced <- ids[vapply(
    policy$columns,
    function(record) isTRUE(record$forced),
    logical(1)
  )]
  policy$included <- retained
  policy$attention <- ids[dispositions == "attention"]
  policy$excluded <- setdiff(ids, retained)
  policy$blocking <- ids[dispositions == "blocking"]
  policy$value <- retained
  policy$requires_confirmation <- length(policy$attention) > 0L ||
    length(policy$blocking) > 0L
  policy
}

builder_task6_entry <- function(
  status = "valid",
  immune_detected = NULL,
  full_ir_ready = FALSE,
  hla_tcr_ready = FALSE,
  aggregate_valid = TRUE,
  hla_detected = FALSE,
  hla_valid = TRUE
) {
  if (is.null(immune_detected)) {
    immune_detected <- isTRUE(full_ir_ready) || isTRUE(hla_tcr_ready)
  }
  entry <- builder_minimal_entry("dataset-a", "Dataset A")
  source <- list(
    type = "example",
    location = "task-6",
    fingerprint = "example:task-6:v1",
    format = "Built-in example"
  )
  core <- builder_manifest_entry(
    id = "expression",
    source = source[c("type", "location")],
    status = status,
    disposition = if (identical(status, "blocking")) {
      "rejected"
    } else {
      "preserved"
    },
    artifact_scope = "both",
    pages = "gene_expression"
  )
  candidates <- if (immune_detected) {
    list(
      unified_misc = builder_task6_immune_candidate(
        "unified_misc",
        full_ir_ready,
        hla_tcr_ready
      )
    )
  } else {
    list()
  }
  immune <- builder_task6_immune_fact(candidates)
  immune$valid <- if (immune_detected) aggregate_valid else TRUE
  entry$dataset_profile <- list(
    schema_version = 2L,
    identity = list(
      cells = list(
        count = 100L,
        valid = TRUE,
        canonical_ids = paste0("cell-", seq_len(100L))
      ),
      features = list(
        count = 2L,
        valid = TRUE,
        canonical_ids = c("Gene1", "Gene2")
      )
    ),
    metadata = list(
      columns = list(
        cluster = list(
          name = "cluster",
          class = "factor",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 2L
        ),
        nCount_RNA = list(
          name = "nCount_RNA",
          class = "numeric",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 80L
        ),
        nFeature_RNA = list(
          name = "nFeature_RNA",
          class = "integer",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 70L
        ),
        donor_id = list(
          name = "donor_id",
          class = "character",
          supported = TRUE,
          non_missing = 100L,
          unique_non_missing = 100L
        )
      )
    ),
    source = source,
    manifest = builder_content_manifest(list(core)),
    content = list(
      immune_repertoire = immune,
      hla = builder_task6_fact(
        detected = hla_detected,
        valid = hla_valid
      )
    )
  )
  entry$snapshot_identity <- NULL
  metadata_recommendation <- builder_recommend_metadata(
    entry$dataset_profile,
    required = c("nCount_RNA", "nFeature_RNA"),
    dependency_ids = list(
      nCount_RNA = "core.qc.nUMI",
      nFeature_RNA = "core.qc.nGene"
    )
  )
  metadata_policy <- builder_task6_final_metadata_policy(
    metadata_recommendation,
    list(
      nCount_RNA = "included",
      nFeature_RNA = "included",
      donor_id = "excluded"
    )
  )
  entry$settings$recommendations <- list(
    groups = list(included = "cluster"),
    projections = list(included = "umap"),
    metadata = metadata_recommendation
  )
  entry$settings$default_group <- "cluster"
  entry$settings$default_projection <- "umap"
  entry$settings$metadata_policy <- metadata_policy
  entry$settings$nomenclature <- "name"
  entry$settings$expression_backend <- "embedded"
  entry$settings$sidecars <- character()
  entry$settings$acknowledgements <- character()
  entry
}
