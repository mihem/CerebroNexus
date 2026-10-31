builder_plan_contract_source_runtime(environment())

metadata_retention_entry <- function() {
  entry <- builder_task6_entry()
  entry$dataset_profile$metadata$columns$orig.ident <- list(
    name = "orig.ident",
    class = "factor",
    supported = TRUE,
    non_missing = 100L,
    unique_non_missing = 1L
  )
  recommendation <- builder_recommend_metadata(
    entry$dataset_profile,
    required = c("nCount_RNA", "nFeature_RNA")
  )
  entry$settings$recommendations$metadata <- recommendation
  entry$settings$metadata_policy <- builder_task6_final_metadata_policy(
    recommendation,
    list(nCount_RNA = "included", nFeature_RNA = "included")
  )
  entry
}

test_that("metadata retention is independent from Group selection", {
  entry <- metadata_retention_entry()
  final <- .builder_state_effective_metadata_policy(
    entry,
    entry$dataset_profile
  )

  expect_contains(final$retained, "orig.ident")
  expect_false("orig.ident" %in% final$groups)
})

test_that("metadata retention controls resolve recommendation attention", {
  entry <- metadata_retention_entry()
  recommendation <- entry$settings$recommendations$metadata
  recommendation$columns$patient_id <- recommendation$columns$cluster
  recommendation$columns$patient_id$name <- "patient_id"
  recommendation$columns$patient_id$value <- "attention"
  recommendation$columns$patient_id$disposition <- "attention"
  recommendation$columns$patient_id$effective_included <- FALSE
  recommendation$columns$patient_id$retain_in_crb <- FALSE
  recommendation$columns$patient_id$requires_confirmation <- TRUE
  recommendation$attention <- "patient_id"
  recommendation$excluded <- unique(c(
    recommendation$excluded,
    "patient_id"
  ))
  recommendation$requires_confirmation <- TRUE

  excluded <- builder_metadata_policy_set_retained(
    recommendation,
    recommendation$retained
  )
  expect_identical(
    excluded$columns$patient_id$disposition,
    "excluded"
  )
  expect_false(excluded$columns$patient_id$requires_confirmation)
  expect_false("patient_id" %in% excluded$attention)

  included <- builder_metadata_policy_set_retained(
    recommendation,
    c(recommendation$retained, "patient_id")
  )
  expect_identical(
    included$columns$patient_id$disposition,
    "included"
  )
  expect_true(included$columns$patient_id$effective_included)
  expect_true("patient_id" %in% included$retained)
  expect_false(included$requires_confirmation)
})

test_that("a Group cannot be excluded from retained metadata", {
  entry <- metadata_retention_entry()
  entry$settings$groups <- "cluster"
  entry$settings$included_groups <- "cluster"
  policy <- entry$settings$metadata_policy
  policy$retained <- setdiff(policy$retained, "cluster")
  policy$columns$cluster$retain_in_crb <- FALSE
  entry$settings$metadata_policy <- policy

  error <- tryCatch(
    {
      .builder_state_effective_metadata_policy(
        entry,
        entry$dataset_profile
      )
      NULL
    },
    builder_state_error = identity
  )
  expect_s3_class(error, "builder_state_error")
  expect_identical(error$code, "metadata_dependency_conflict")
})

test_that("legacy metadata policies preserve their prior retention choices", {
  entry <- metadata_retention_entry()
  legacy <- entry$settings$metadata_policy
  legacy$retained <- NULL
  legacy$groups <- NULL
  legacy$forced <- NULL
  for (id in names(legacy$columns)) {
    legacy$columns[[id]]$retain_in_crb <- NULL
    legacy$columns[[id]]$group_enabled <- NULL
    legacy$columns[[id]]$forced <- NULL
  }

  upgraded <- .builder_state_upgrade_metadata_policy(legacy)

  expected <- names(legacy$columns)[vapply(
    legacy$columns,
    function(record) isTRUE(record$effective_included),
    logical(1)
  )]
  expect_setequal(upgraded$retained, expected)
  expect_identical(upgraded$groups, character())
})

test_that("immune pages require one exportable canonical payload", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    motif_only <- builder_task6_entry(
      full_ir_ready = FALSE,
      hla_tcr_ready = TRUE,
      aggregate_valid = TRUE
    )
    motif_state <- builder_dataset_state(motif_only)
    motif_plan <- builder_freeze_plan(list(motif_only), tempdir(), FALSE)
    expect_identical(motif_state$readiness, "blocked")
    expect_identical(
      motif_state$manifest[["hla_tcr_motifs"]]$status,
      "blocking"
    )
    expect_identical(
      motif_state$manifest[["hla_tcr_motifs"]]$disposition,
      "rejected"
    )
    expect_false(
      motif_state$manifest[["hla_tcr_motifs"]]$page_visible
    )
    expect_true(
      "motif_source_not_exportable" %in%
        motif_state$manifest[["hla_tcr_motifs"]]$evidence$diagnostics
    )
    expect_identical(motif_plan$error_code, "blocking_capability")

    bcr_only <- builder_task6_entry(
      full_ir_ready = TRUE,
      hla_tcr_ready = FALSE,
      aggregate_valid = FALSE
    )
    bcr_plan <- builder_freeze_plan(
      list(bcr_only),
      tempdir(),
      FALSE
    )
    expect_null(bcr_plan$error)
    expect_true(
      bcr_plan$manifest[["immune_repertoire"]]$page_visible
    )
    expect_false(
      bcr_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )

    full_tcr <- builder_task6_entry(
      full_ir_ready = TRUE,
      hla_tcr_ready = TRUE,
      aggregate_valid = FALSE
    )
    full_tcr_plan <- builder_freeze_plan(list(full_tcr), tempdir(), FALSE)
    expect_null(full_tcr_plan$error)
    expect_true(
      full_tcr_plan$manifest[["immune_repertoire"]]$page_visible
    )
    expect_true(
      full_tcr_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )

    hla_only <- builder_task6_entry(
      immune_detected = FALSE,
      hla_detected = TRUE,
      hla_valid = TRUE,
      aggregate_valid = FALSE
    )
    hla_plan <- builder_freeze_plan(list(hla_only), tempdir(), FALSE)
    expect_null(hla_plan$error)
    expect_false(
      hla_plan$manifest[["immune_repertoire"]]$page_visible
    )
    expect_false(
      hla_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )
    expect_identical(hla_plan$manifest[["hla"]]$status, "valid")

    filtered_motif <- builder_task6_entry(
      full_ir_ready = FALSE,
      hla_tcr_ready = TRUE
    )
    filtered_motif$settings$content_dispositions <- list(
      hla_tcr_motifs = "filtered"
    )
    filtered_motif_plan <- builder_freeze_plan(
      list(filtered_motif),
      tempdir(),
      FALSE
    )
    expect_null(filtered_motif_plan$error)
    expect_identical(
      filtered_motif_plan$manifest[["hla_tcr_motifs"]]$disposition,
      "filtered"
    )
    expect_false(
      filtered_motif_plan$manifest[["hla_tcr_motifs"]]$page_visible
    )
  })
})

test_that("immune page dispositions cannot contradict their shared payload", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    conflict_cases <- list(
      list(immune_repertoire = "preserved", hla_tcr_motifs = "filtered"),
      list(immune_repertoire = "filtered", hla_tcr_motifs = "preserved")
    )
    for (choices in conflict_cases) {
      entry <- builder_task6_entry(
        full_ir_ready = TRUE,
        hla_tcr_ready = TRUE
      )
      entry$settings$content_dispositions <- choices
      state <- builder_dataset_state(entry)
      plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

      expect_identical(state$readiness, "blocked")
      expect_setequal(
        state$blocking_ids,
        c("immune_repertoire", "hla_tcr_motifs")
      )
      expect_true(all(vapply(
        state$manifest[c("immune_repertoire", "hla_tcr_motifs")],
        function(record) {
          "incompatible_immune_page_disposition" %in%
            record$evidence$diagnostics
        },
        logical(1)
      )))
      expect_identical(plan$error_code, "blocking_capability")
    }

    hidden <- builder_task6_entry(
      full_ir_ready = TRUE,
      hla_tcr_ready = TRUE
    )
    hidden$settings$content_dispositions <- list(
      immune_repertoire = "filtered",
      hla_tcr_motifs = "stored_only"
    )
    hidden_plan <- builder_freeze_plan(list(hidden), tempdir(), FALSE)
    expect_null(hidden_plan$error)
    expect_false(any(vapply(
      hidden_plan$items[[1L]]$manifest[
        c("immune_repertoire", "hla_tcr_motifs")
      ],
      `[[`,
      logical(1),
      "page_visible"
    )))

    bcr <- builder_task6_entry(
      full_ir_ready = TRUE,
      hla_tcr_ready = FALSE
    )
    bcr$settings$content_dispositions <- list(
      hla_tcr_motifs = "filtered"
    )
    bcr_plan <- builder_freeze_plan(list(bcr), tempdir(), FALSE)
    expect_null(bcr_plan$error)
    expect_true(
      bcr_plan$items[[1L]]$manifest[["immune_repertoire"]]$page_visible
    )
    expect_false(
      bcr_plan$items[[1L]]$manifest[["hla_tcr_motifs"]]$page_visible
    )
  })
})

test_that("modern marker evidence owns enrichment normalization", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$content$marker_genes <- .builder_table_record(
      detected = TRUE,
      valid = TRUE,
      normalized = list(methods = "wilcox"),
      page_candidates = "marker_genes"
    )
    entry$dataset_profile$content$enriched_pathways <-
      .builder_table_record()
    entry$settings$analyses <- "enriched_pathways"

    state <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$analyses, "enriched_pathways")
    expect_identical(
      plan$items[[1L]]$analysis_dependency_graph$enriched_pathways$dependencies,
      "marker_genes"
    )
    expect_identical(
      state$manifest[["enriched_pathways"]]$disposition,
      "generated"
    )
    expect_identical(
      state$manifest[["enriched_pathways"]]$pages,
      "enriched_pathways"
    )
    expect_true(state$manifest[["enriched_pathways"]]$page_visible)
  })
})

test_that("generated analyses open their explicit Viewer pages", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$content$most_expressed_genes <-
      .builder_table_record()
    entry$dataset_profile$content$marker_genes <- .builder_table_record()
    entry$dataset_profile$content$enriched_pathways <-
      .builder_table_record()
    entry$settings$analyses <- c(
      "most_expressed",
      "marker_genes",
      "enriched_pathways"
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    expected <- c(
      most_expressed_genes = "most_expressed_genes",
      marker_genes = "marker_genes",
      enriched_pathways = "enriched_pathways"
    )
    for (id in names(expected)) {
      record <- plan$items[[1L]]$manifest[[id]]
      expect_identical(record$disposition, "generated", info = id)
      expect_identical(record$pages, unname(expected[[id]]), info = id)
      expect_true(record$page_visible, info = id)
    }
  })
})

test_that("analysis execution and content dispositions have one authority", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    for (disposition in c(
      "filtered",
      "stored_only",
      "preserved",
      "converted",
      "attached"
    )) {
      conflicting <- builder_task6_entry()
      conflicting$dataset_profile$content$marker_genes <-
        .builder_table_record()
      conflicting$settings$analyses <- "marker_genes"
      conflicting$settings$content_dispositions <- list(
        marker_genes = disposition
      )
      expect_identical(
        builder_freeze_plan(
          list(conflicting),
          tempdir(),
          FALSE
        )$error_code,
        "analysis_disposition_conflict",
        info = disposition
      )
    }

    explicit <- builder_task6_entry()
    explicit$dataset_profile$content$marker_genes <-
      .builder_table_record()
    explicit$settings$analyses <- "marker_genes"
    explicit$settings$content_dispositions <- list(
      marker_genes = "generated"
    )
    plan <- builder_freeze_plan(list(explicit), tempdir(), FALSE)
    expect_null(plan$error)
    expect_identical(plan$items[[1L]]$analyses, "marker_genes")
    expect_identical(
      plan$items[[1L]]$manifest[["marker_genes"]]$disposition,
      "generated"
    )
    expect_identical(
      plan$items[[1L]]$manifest[["marker_genes"]]$pages,
      "marker_genes"
    )

    unselected <- builder_task6_entry()
    unselected$settings$content_dispositions <- list(
      marker_genes = "generated"
    )
    expect_identical(
      builder_freeze_plan(list(unselected), tempdir(), FALSE)$error_code,
      "analysis_disposition_conflict"
    )

    for (disposition in c("preserved", "converted", "attached")) {
      absent <- builder_task6_entry()
      absent$dataset_profile$content$marker_genes <-
        .builder_table_record(detected = FALSE, valid = TRUE)
      absent$settings$content_dispositions <- setNames(
        list(disposition),
        "marker_genes"
      )
      expect_identical(
        builder_freeze_plan(list(absent), tempdir(), FALSE)$error_code,
        "blocking_capability",
        info = disposition
      )
    }

    unsupported <- builder_task6_entry(
      hla_detected = TRUE,
      hla_valid = TRUE
    )
    unsupported$settings$content_dispositions <- list(hla = "generated")
    expect_identical(
      builder_freeze_plan(list(unsupported), tempdir(), FALSE)$error_code,
      "analysis_disposition_conflict"
    )

    stale <- builder_task6_entry()
    stale$dataset_profile$content$marker_genes <-
      .builder_table_record()
    stale$settings$analyses <- "marker_genes"
    stale_marker <- builder_manifest_entry(
      id = "marker_genes",
      source = list(type = "stale", location = "caller-manifest"),
      status = "valid",
      disposition = "preserved",
      artifact_scope = "both",
      pages = character()
    )
    stale$dataset_profile$manifest <- builder_content_manifest(c(
      unname(stale$dataset_profile$manifest),
      list(stale_marker)
    ))
    stale_plan <- builder_freeze_plan(list(stale), tempdir(), FALSE)
    expect_null(stale_plan$error)
    expect_identical(
      stale_plan$items[[1L]]$manifest[["marker_genes"]]$disposition,
      "generated"
    )
    expect_identical(
      stale_plan$items[[1L]]$manifest[["marker_genes"]]$pages,
      "marker_genes"
    )

    forged_core <- builder_task6_entry()
    forged_core$dataset_profile$content$expression <-
      builder_task6_fact()
    expect_identical(
      builder_freeze_plan(
        list(forged_core),
        tempdir(),
        FALSE
      )$error_code,
      "invalid_content_id"
    )
  })
})

test_that("production HLA attention requires a stable acknowledgement", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$dataset_profile$content$hla <- .builder_immune_record(
      detected = TRUE,
      valid = TRUE,
      diagnostics = "unknown_provenance",
      requirements = "explicit_provenance_for_association",
      attention = TRUE,
      provenance = list(has_unknown = TRUE),
      page_gate = list(role = "supporting", opens = FALSE)
    )

    state <- builder_dataset_state(entry)
    repeated <- builder_dataset_state(entry)
    expect_identical(state$readiness, "needs_attention")
    expect_identical(state$attention_ids, "hla")
    expect_identical(state$manifest[["hla"]]$status, "attention")
    expect_identical(
      state$manifest[["hla"]]$required_action$type,
      "acknowledge"
    )
    token <- state$manifest[["hla"]]$required_action$token
    expect_true(builder_has_text(token))
    expect_identical(
      repeated$manifest[["hla"]]$required_action$token,
      token
    )
    expect_identical(
      builder_freeze_plan(list(entry), tempdir(), FALSE)$error_code,
      "attention_capability"
    )

    entry$settings$acknowledgements <- token
    acknowledged <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(acknowledged$readiness, "ready")
    expect_null(plan$error)
  })
})

test_that("reserved barcode collisions remain valid blocking policies", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    profile <- list(
      schema_version = 2L,
      identity = list(
        cells = list(
          count = 4L,
          canonical_ids = paste0("cell-", seq_len(4L))
        )
      ),
      metadata = list(
        columns = list(
          cell_barcode = list(
            name = "cell_barcode",
            class = "character",
            supported = TRUE,
            non_missing = 4L,
            unique_non_missing = 4L
          )
        )
      )
    )
    policy <- builder_recommend_metadata(profile)
    expect_identical(
      policy$columns$cell_barcode$disposition,
      "blocking"
    )
    expect_true(policy$columns$cell_barcode$effective_included)

    entry <- builder_task6_entry()
    entry$dataset_profile$identity$cells <- profile$identity$cells
    entry$dataset_profile$metadata <- profile$metadata
    entry$settings$groups <- "cell_barcode"
    entry$settings$default_group <- "cell_barcode"
    entry$settings$nUMI <- "cell_barcode"
    entry$settings$nGene <- "cell_barcode"
    entry$settings$recommendations$groups$included <- "cell_barcode"
    entry$settings$recommendations$metadata <- policy
    entry$settings$metadata_policy <- policy
    state <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "metadata_policy")
    expect_identical(plan$error_code, "blocking_capability")
  })
})

test_that("final metadata cannot weaken profiled hard blockers", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    collision_profile <- list(
      schema_version = 2L,
      identity = list(
        cells = list(
          count = 4L,
          canonical_ids = paste0("cell-", seq_len(4L))
        )
      ),
      metadata = list(
        columns = list(
          cell_barcode = list(
            name = "cell_barcode",
            class = "character",
            supported = TRUE,
            non_missing = 4L,
            unique_non_missing = 4L
          )
        )
      )
    )
    collision <- builder_recommend_metadata(collision_profile)
    weakened <- builder_task6_final_metadata_policy(
      collision,
      list(cell_barcode = "attention")
    )
    collision_entry <- builder_task6_entry()
    collision_entry$dataset_profile$identity$cells <-
      collision_profile$identity$cells
    collision_entry$dataset_profile$metadata <- collision_profile$metadata
    collision_entry$settings$groups <- "cell_barcode"
    collision_entry$settings$default_group <- "cell_barcode"
    collision_entry$settings$nUMI <- "cell_barcode"
    collision_entry$settings$nGene <- "cell_barcode"
    collision_entry$settings$recommendations$groups$included <-
      "cell_barcode"
    collision_entry$settings$recommendations$metadata <- collision
    collision_entry$settings$metadata_policy <- weakened
    expect_identical(
      builder_freeze_plan(
        list(collision_entry),
        tempdir(),
        FALSE
      )$error_code,
      "invalid_metadata_policy"
    )

    for (bad_class in c("character", "list", "data.frame")) {
      unsafe <- builder_task6_entry()
      unsafe$dataset_profile$metadata$columns$unsafe_column <- list(
        name = "unsafe_column",
        class = bad_class,
        supported = FALSE,
        non_missing = 100L,
        unique_non_missing = 2L
      )
      recommendation <- builder_recommend_metadata(
        unsafe$dataset_profile,
        required = c("nCount_RNA", "nFeature_RNA"),
        dependency_ids = list(
          nCount_RNA = "core.qc.nUMI",
          nFeature_RNA = "core.qc.nGene"
        )
      )
      included <- builder_task6_final_metadata_policy(
        recommendation,
        list(
          nCount_RNA = "included",
          nFeature_RNA = "included",
          donor_id = "excluded",
          unsafe_column = "included"
        )
      )
      unsafe$settings$recommendations$metadata <- recommendation
      unsafe$settings$metadata_policy <- included
      expect_identical(
        builder_freeze_plan(list(unsafe), tempdir(), FALSE)$error_code,
        "invalid_metadata_policy",
        info = bad_class
      )

      fallback <- unsafe
      fallback$settings$metadata_policy <- NULL
      fallback$settings$recommendations$metadata <- included
      expect_identical(
        builder_freeze_plan(list(fallback), tempdir(), FALSE)$error_code,
        "invalid_metadata_policy",
        info = paste("fallback", bad_class)
      )
    }
  })
})

test_that("final metadata covers selections and declared dependencies", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    entry$settings$recommendations$metadata$columns$donor_id$dependency_ids <-
      "content.hla_association"
    entry$settings$metadata_policy$columns$donor_id$dependency_ids <-
      "content.hla_association"
    entry$settings$metadata_policy <- builder_task6_final_metadata_policy(
      entry$settings$metadata_policy,
      list(donor_id = "included")
    )
    expect_null(builder_freeze_plan(list(entry), tempdir(), FALSE)$error)

    included_group <- builder_task6_entry()
    included_group$settings$included_groups <- c("cluster", "donor_id")
    expect_identical(
      builder_freeze_plan(
        list(included_group),
        tempdir(),
        FALSE
      )$error_code,
      "metadata_dependency_conflict"
    )
    included_group$settings$metadata_policy <-
      builder_task6_final_metadata_policy(
        included_group$settings$metadata_policy,
        list(donor_id = "included")
      )
    expect_null(
      builder_freeze_plan(
        list(included_group),
        tempdir(),
        FALSE
      )$error
    )

    for (id in c("cluster", "nCount_RNA", "nFeature_RNA", "donor_id")) {
      missing <- entry
      missing$settings$metadata_policy <-
        builder_task6_final_metadata_policy(
          missing$settings$metadata_policy,
          setNames(list("excluded"), id)
        )
      expect_identical(
        builder_freeze_plan(list(missing), tempdir(), FALSE)$error_code,
        "metadata_dependency_conflict",
        info = id
      )
    }

    fallback <- entry
    fallback$settings$recommendations$metadata <-
      builder_task6_final_metadata_policy(
        fallback$settings$recommendations$metadata,
        list(
          nCount_RNA = "included",
          nFeature_RNA = "included",
          donor_id = "excluded"
        )
      )
    fallback$settings$metadata_policy <- NULL
    expect_identical(
      builder_freeze_plan(list(fallback), tempdir(), FALSE)$error_code,
      "metadata_dependency_conflict"
    )

    required <- builder_task6_entry()
    required_recommendation <- builder_recommend_metadata(
      required$dataset_profile,
      required = c("nCount_RNA", "nFeature_RNA", "donor_id"),
      dependency_ids = list(
        nCount_RNA = "core.qc.nUMI",
        nFeature_RNA = "core.qc.nGene"
      )
    )
    required$settings$recommendations$metadata <- required_recommendation
    required$settings$metadata_policy <-
      builder_task6_final_metadata_policy(
        required_recommendation,
        list(
          nCount_RNA = "included",
          nFeature_RNA = "included",
          donor_id = "excluded"
        )
      )
    required$settings$metadata_policy$columns$donor_id$required <- FALSE
    expect_identical(
      builder_freeze_plan(list(required), tempdir(), FALSE)$error_code,
      "metadata_dependency_conflict"
    )
  })
})

test_that("missing required metadata remains a valid blocking policy", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    profile <- list(
      schema_version = 2L,
      identity = list(
        cells = list(
          count = 4L,
          canonical_ids = paste0("cell-", seq_len(4L))
        )
      ),
      metadata = list(columns = list())
    )
    policy <- builder_recommend_metadata(
      profile,
      required = "missing_required"
    )
    sentinel <- policy$columns$missing_required
    expect_identical(sentinel$class, "missing")
    expect_true(sentinel$required)
    expect_identical(sentinel$disposition, "blocking")
    expect_false(sentinel$effective_included)

    entry <- builder_task6_entry()
    entry$dataset_profile$identity$cells <- profile$identity$cells
    entry$dataset_profile$metadata <- profile$metadata
    entry$settings$groups <- "cell_barcode"
    entry$settings$default_group <- "cell_barcode"
    entry$settings$nUMI <- "cell_barcode"
    entry$settings$nGene <- "cell_barcode"
    entry$settings$recommendations$groups$included <- "cell_barcode"
    entry$settings$recommendations$metadata <- policy
    entry$settings$metadata_policy <- policy
    state <- builder_dataset_state(entry)
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "metadata_policy")
    expect_identical(plan$error_code, "blocking_capability")
  })
})

test_that("final metadata policy owns review and frozen output", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    selected <- builder_task6_entry()
    recommendation <- selected$settings$recommendations$metadata
    final_policy <- selected$settings$metadata_policy
    expected_final_policy <- .builder_state_metadata_policy_sync_groups(
      final_policy,
      .builder_state_included_groups(selected)
    )
    expected_recommendation <- .builder_state_metadata_policy_sync_groups(
      .builder_state_upgrade_metadata_policy(recommendation),
      .builder_state_included_groups(selected)
    )
    expect_identical(
      recommendation$columns$donor_id$disposition,
      "included"
    )
    expect_identical(
      final_policy$columns$donor_id$disposition,
      "excluded"
    )
    expect_false(identical(final_policy, recommendation))
    state <- builder_dataset_state(selected)
    plan <- builder_freeze_plan(
      list(selected),
      tempdir(),
      FALSE
    )
    expect_identical(state$readiness, "ready")
    expect_identical(
      state$manifest[["metadata_policy"]]$evidence$normalized$excluded,
      final_policy$excluded
    )
    expect_null(plan$error)
    expect_identical(
      plan$items[[1L]]$metadata_policy,
      expected_final_policy
    )

    recommendation_only <- builder_task6_entry()
    recommendation_only$settings$metadata_policy <- NULL
    recommendation_plan <- builder_freeze_plan(
      list(recommendation_only),
      tempdir(),
      FALSE
    )
    expect_null(recommendation_plan$error)
    expect_identical(
      recommendation_plan$items[[1L]]$metadata_policy,
      expected_recommendation
    )
  })
})

test_that("ready Marker imports enter BuildPlan without upload metadata", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    group <- (entry$settings$included_groups %||% entry$settings$groups)[[1L]]
    level <- entry$levels[[group]][[1L]]
    source <- builder_marker_import_map_single(
      builder_marker_import_source(
        paste0(level, ".csv"),
        NULL,
        data.frame(gene = "marker-a", score = 5)
      ),
      group,
      level,
      entry$levels[[group]],
      confirmed = TRUE
    )
    source$id <- "source-001"
    source$datapath <- "/private/upload/marker.csv"
    draft <- builder_marker_import_new_draft(
      "marker-import-1",
      "Scanpy Wilcoxon",
      group,
      list(source),
      entry$levels[[group]]
    )
    draft$sources[[1L]] <- source
    draft <- builder_marker_import_refresh_draft(draft)
    draft$ready <- TRUE
    entry$settings$marker_imports <- list(
      `marker-import-1` = draft
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    imports <- plan$items[[1L]]$marker_imports
    expect_length(imports, 1L)
    expect_identical(imports[[1L]]$method, "Scanpy Wilcoxon")
    expect_null(imports[[1L]]$sources[[1L]]$raw_table)
    expect_null(imports[[1L]]$sources[[1L]]$datapath)
  })
})

test_that("final metadata policies must prove their own consistency", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    policy <- entry$settings$metadata_policy

    without_recommendation <- entry
    without_recommendation$settings$recommendations$metadata <- NULL
    expect_null(
      builder_freeze_plan(
        list(without_recommendation),
        tempdir(),
        FALSE
      )$error
    )

    unknown_column <- policy
    unknown_column$columns$invented <- unknown_column$columns$donor_id
    unknown_column$columns$invented$name <- "invented"
    unknown_column <- builder_task6_final_metadata_policy(
      unknown_column
    )

    forged_missing_sentinel <- policy
    forged_missing_sentinel$columns$invented <-
      forged_missing_sentinel$columns$donor_id
    forged_missing_sentinel$columns$invented$name <- "invented"
    forged_missing_sentinel$columns$invented$class <- "missing"
    forged_missing_sentinel$columns$invented$required <- TRUE
    forged_missing_sentinel$columns$invented$non_missing <- 1L
    forged_missing_sentinel$columns$invented$unique_non_missing <- 0L
    forged_missing_sentinel <- builder_task6_final_metadata_policy(
      forged_missing_sentinel,
      list(invented = "blocking")
    )

    missing_source_column <- policy
    missing_source_column$columns$donor_id <- NULL
    missing_source_column <- builder_task6_final_metadata_policy(
      missing_source_column
    )

    overlapping_sets <- policy
    overlapping_sets$included <- c(
      overlapping_sets$included,
      "donor_id"
    )
    overlapping_sets$value <- overlapping_sets$included

    missing_barcode <- policy
    missing_barcode$columns$cell_barcode <- NULL
    missing_barcode <- builder_task6_final_metadata_policy(
      missing_barcode
    )

    excluded_barcode <- builder_task6_final_metadata_policy(
      policy,
      list(cell_barcode = "excluded")
    )

    disposition_drift <- policy
    disposition_drift$columns$donor_id$disposition <- "included"

    effective_drift <- policy
    effective_drift$columns$donor_id$value <- "included"
    effective_drift$columns$donor_id$disposition <- "included"
    effective_drift$columns$donor_id$effective_included <- FALSE
    effective_drift$excluded <- setdiff(
      effective_drift$excluded,
      "donor_id"
    )

    record_value_drift <- policy
    record_value_drift$columns$donor_id$value <- "included"

    record_confirmation_drift <- policy
    record_confirmation_drift$columns$donor_id$requires_confirmation <- TRUE

    aggregate_drift <- policy
    aggregate_drift$excluded <- character()

    policy_value_drift <- policy
    policy_value_drift$value <- "donor_id"

    policy_confirmation_drift <- policy
    policy_confirmation_drift$requires_confirmation <- TRUE

    mutable_record <- policy
    mutable_record$columns$donor_id$mutable <- new.env(parent = emptyenv())

    invalid <- list(
      unknown_column = unknown_column,
      forged_missing_sentinel = forged_missing_sentinel,
      missing_source_column = missing_source_column,
      overlapping_sets = overlapping_sets,
      missing_barcode = missing_barcode,
      excluded_barcode = excluded_barcode,
      disposition_drift = disposition_drift,
      effective_drift = effective_drift,
      record_value_drift = record_value_drift,
      record_confirmation_drift = record_confirmation_drift,
      aggregate_drift = aggregate_drift,
      policy_value_drift = policy_value_drift,
      policy_confirmation_drift = policy_confirmation_drift,
      mutable_record = mutable_record
    )
    for (label in names(invalid)) {
      malformed <- entry
      malformed$settings$metadata_policy <- invalid[[label]]
      expect_identical(
        builder_freeze_plan(
          list(malformed),
          tempdir(),
          FALSE
        )$error_code,
        "invalid_metadata_policy",
        info = label
      )
    }
  })
})
