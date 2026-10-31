builder_plan_contract_source_runtime(environment())

test_that("immune source and gate decisions remain truthful", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    sources <- list(
      unified_misc = list(
        disposition = "preserved",
        location = "@misc$immune_repertoire"
      ),
      metadata = list(
        disposition = "converted",
        location = "@meta.data"
      ),
      legacy_bcr = list(
        disposition = "converted",
        location = "@misc$bcr_data"
      ),
      legacy_tcr = list(
        disposition = "converted",
        location = "@misc$tcr_data"
      )
    )
    for (source_kind in names(sources)) {
      entry <- builder_task6_entry(immune_detected = FALSE)
      candidate <- builder_task6_immune_candidate(
        source_kind,
        full_ir_ready = TRUE,
        hla_tcr_ready = identical(source_kind, "legacy_tcr")
      )
      entry$dataset_profile$content$immune_repertoire <-
        builder_task6_immune_fact(setNames(list(candidate), source_kind))
      plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
      record <- plan$items[[1L]]$manifest[["immune_repertoire"]]

      expect_null(plan$error, info = source_kind)
      expect_identical(
        record$evidence$selected_source,
        source_kind,
        info = source_kind
      )
      expect_identical(
        record$disposition,
        sources[[source_kind]]$disposition,
        info = source_kind
      )
      expect_identical(
        record$source$location,
        sources[[source_kind]]$location,
        info = source_kind
      )
    }

    mixed <- builder_task6_entry(immune_detected = FALSE)
    mixed$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(
        unified_misc = builder_task6_immune_candidate(
          "unified_misc",
          full_ir_ready = FALSE,
          hla_tcr_ready = TRUE
        ),
        metadata = builder_task6_immune_candidate(
          "metadata",
          full_ir_ready = TRUE,
          hla_tcr_ready = FALSE
        )
      ))
    mixed_state <- builder_dataset_state(mixed)
    mixed_plan <- builder_freeze_plan(list(mixed), tempdir(), FALSE)
    expect_identical(mixed_state$readiness, "blocked")
    expect_identical(
      mixed_state$blocking_ids,
      "hla_tcr_motifs"
    )
    expect_true(
      "motif_source_not_exportable" %in%
        mixed_state$manifest[["hla_tcr_motifs"]]$evidence$diagnostics
    )
    expect_identical(mixed_plan$error_code, "blocking_capability")

    motif_only <- builder_task6_entry(immune_detected = FALSE)
    motif_only$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(
        unified_misc = builder_task6_immune_candidate(
          "unified_misc",
          full_ir_ready = FALSE,
          hla_tcr_ready = TRUE
        )
      ))
    state <- builder_dataset_state(motif_only)
    expect_identical(
      state$manifest[["immune_repertoire"]]$status,
      "not_applicable"
    )
    expect_identical(
      state$manifest[["hla_tcr_motifs"]]$status,
      "blocking"
    )
    expect_false(state$manifest[["hla_tcr_motifs"]]$page_visible)

    motif_only$settings$content_dispositions <- list(
      immune_repertoire = "preserved"
    )
    spoofed <- builder_freeze_plan(list(motif_only), tempdir(), FALSE)
    expect_identical(spoofed$error_code, "blocking_capability")
  })
})

test_that("stale explicit immune sources block after candidates disappear", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )

    state <- builder_dataset_state(entry)
    expect_identical(state$readiness, "blocked")
    expect_true("immune_repertoire" %in% state$blocking_ids)
    expect_true(
      "selected_source_is_not_ready" %in%
        state$manifest[["immune_repertoire"]]$evidence$diagnostics
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(plan$error_code, "blocking_capability")
    expect_true(
      "immune_repertoire" %in% plan$details$capability_ids
    )
  })
})

test_that("malformed immune candidate and overlap evidence fails typed", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }
    capture_plan <- function(entry) {
      tryCatch(
        builder_freeze_plan(list(entry), tempdir(), FALSE),
        error = function(error) error
      )
    }
    expect_typed <- function(entry, code) {
      state <- capture_state(entry)
      plan <- capture_plan(entry)
      expect_s3_class(state, "builder_state_error")
      expect_identical(state$code, code)
      expect_s3_class(plan, "builder_plan_failure")
      expect_identical(plan$error_code, code)
    }

    candidate_cases <- list(
      source_kind = c("unified_misc", "metadata"),
      detected = c(TRUE, FALSE),
      full_ir_ready = "yes",
      hla_tcr_ready = NA
    )
    for (field in names(candidate_cases)) {
      malformed <- builder_task6_entry(full_ir_ready = TRUE)
      malformed$dataset_profile$content$immune_repertoire$candidates$unified_misc[[
        field
      ]] <- candidate_cases[[field]]
      expect_typed(malformed, "invalid_immune_candidates")
    }

    overlap_entry <- builder_task6_entry(immune_detected = FALSE)
    overlap_fact <- builder_task6_immune_fact(list(
      unified_misc = builder_task6_immune_candidate(
        "unified_misc",
        full_ir_ready = TRUE
      ),
      metadata = builder_task6_immune_candidate(
        "metadata",
        full_ir_ready = TRUE
      )
    ))
    overlap_fact$source_overlaps <- list(list(
      left = c("unified_misc", "metadata"),
      right = "metadata",
      n_overlap = 1L,
      n_divergent = 1L,
      equivalent = FALSE
    ))
    overlap_entry$dataset_profile$content$immune_repertoire <- overlap_fact
    expect_typed(overlap_entry, "invalid_immune_overlaps")
  })
})

test_that("divergent immune sources require an explicit source choice", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    fact <- builder_task6_immune_fact(list(
      unified_misc = builder_task6_immune_candidate(
        "unified_misc",
        full_ir_ready = TRUE
      ),
      metadata = builder_task6_immune_candidate(
        "metadata",
        full_ir_ready = TRUE
      )
    ))
    fact$source_overlaps <- list(list(
      left = "unified_misc",
      right = "metadata",
      n_overlap = 2L,
      n_exact = 0L,
      n_divergent = 2L,
      overlap_preview = c("cell-a", "cell-b"),
      divergent_preview = c("cell-a", "cell-b"),
      preview_truncated_count = 0L
    ))
    entry$dataset_profile$content$immune_repertoire <- fact

    state <- builder_dataset_state(entry)
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(state$readiness, "blocked")
    expect_identical(state$blocking_ids, "immune_repertoire")
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("selected immune filtering requires a stable acknowledgement", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    candidate <- builder_task6_immune_candidate(
      "unified_misc",
      full_ir_ready = TRUE,
      hla_tcr_ready = TRUE
    )
    candidate$attention <- TRUE
    candidate$diagnostics <- c(
      "empty_sample_table",
      "barcodes_outside_dataset"
    )
    entry$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(unified_misc = candidate))

    state <- builder_dataset_state(entry)
    expect_identical(state$readiness, "needs_attention")
    expect_identical(
      state$attention_ids,
      c("immune_repertoire", "hla_tcr_motifs")
    )
    actions <- lapply(
      state$manifest[state$attention_ids],
      `[[`,
      "required_action"
    )
    expect_true(all(vapply(
      actions,
      function(action) identical(action$type, "acknowledge"),
      logical(1)
    )))

    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "attention_capability")

    entry$settings$acknowledgements <- unique(vapply(
      actions,
      `[[`,
      character(1),
      "token"
    ))
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    expect_setequal(
      plan$items[[1L]]$acknowledgements,
      entry$settings$acknowledgements
    )
    expect_setequal(
      intersect(
        plan$items[[1L]]$viewer_page_expectations$visible_conditional,
        c("immune_repertoire", "hla_tcr_motifs")
      ),
      c("immune_repertoire", "hla_tcr_motifs")
    )
  })
})

test_that("legacy BCR and TCR freeze as one conversion source set", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <-
      builder_task6_immune_fact(list(
        legacy_bcr = builder_task6_immune_candidate(
          "legacy_bcr",
          full_ir_ready = TRUE,
          hla_tcr_ready = FALSE
        ),
        legacy_tcr = builder_task6_immune_candidate(
          "legacy_tcr",
          full_ir_ready = TRUE,
          hla_tcr_ready = TRUE
        )
      ))
    entry$settings$content_sources <- list(
      immune_repertoire = "legacy_tcr"
    )

    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(plan$error)
    immune <- plan$items[[1L]]$manifest[["immune_repertoire"]]
    motif <- plan$items[[1L]]$manifest[["hla_tcr_motifs"]]
    expect_identical(
      immune$evidence$selected_sources,
      c("legacy_bcr", "legacy_tcr")
    )
    expect_null(immune$evidence$selected_source)
    expect_named(
      immune$evidence$selected_candidates,
      c("legacy_bcr", "legacy_tcr")
    )
    expect_identical(immune$disposition, "converted")
    expect_match(immune$source$location, "bcr_data")
    expect_match(immune$source$location, "tcr_data")
    expect_identical(motif$evidence$selected_source, "legacy_tcr")
  })
})

test_that("production complementary legacy chains are not source conflicts", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    object@misc$bcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "IGH",
        suffix = "legacy-bcr"
      )
    )
    object@misc$tcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "TRB",
        suffix = "legacy-tcr"
      )
    )
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire

    expect_true(fact$candidates$legacy_bcr$full_ir_ready)
    expect_true(fact$candidates$legacy_tcr$full_ir_ready)
    expect_false(any(vapply(
      fact$full_source_overlaps,
      function(overlap) overlap$n_divergent > 0L,
      logical(1)
    )))

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    plan <- builder_freeze_plan(list(entry), tempdir(), FALSE)

    expect_null(plan$error)
    immune <- plan$items[[1L]]$manifest[["immune_repertoire"]]
    expect_identical(
      immune$evidence$selected_sources,
      c("legacy_bcr", "legacy_tcr")
    )
    expect_identical(immune$disposition, "converted")
  })
})

test_that("production same-chain disagreements require source selection", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    object@misc$immune_repertoire <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "TRB",
        suffix = "unified"
      )
    )
    object@misc$tcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "TRB",
        suffix = "legacy"
      )
    )
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire
    expect_true(any(vapply(
      fact$full_source_overlaps,
      function(overlap) {
        identical(overlap$left, "unified_misc") &&
          identical(overlap$right, "legacy_tcr") &&
          overlap$n_divergent > 0L
      },
      logical(1)
    )))

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc",
      hla_tcr_motifs = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("production partial full-IR overlap requires source selection", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    object@misc$immune_repertoire <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:2],
        chain = "IGH",
        suffix = "shared"
      )
    )
    object@misc$bcr_data <- list(
      sample_a = builder_immune_fixture_table(
        cells[1:3],
        chain = "IGH",
        suffix = "shared"
      )
    )
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire
    partial <- Filter(
      function(overlap) {
        identical(overlap$left, "unified_misc") &&
          identical(overlap$right, "legacy_bcr")
      },
      fact$full_source_overlaps
    )[[1L]]
    expect_gt(partial$n_overlap, 0L)
    expect_identical(partial$n_divergent, 0L)
    expect_false(partial$equivalent)

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )

    disjoint <- builder_immune_fixture_object()
    disjoint_cells <- SeuratObject::Cells(disjoint)
    disjoint@misc$immune_repertoire <- list(
      sample_a = builder_immune_fixture_table(
        disjoint_cells[[1L]],
        chain = "IGH",
        suffix = "unified"
      )
    )
    disjoint@misc$bcr_data <- list(
      sample_a = builder_immune_fixture_table(
        disjoint_cells[[2L]],
        chain = "IGH",
        suffix = "legacy"
      )
    )
    disjoint_fact <- builder_profile_immune_content(
      disjoint,
      builder_immune_fixture_context(disjoint)
    )$immune_repertoire
    disjoint_entry <- builder_task6_entry(immune_detected = FALSE)
    disjoint_entry$dataset_profile$content$immune_repertoire <-
      disjoint_fact
    disjoint_plan <- builder_freeze_plan(
      list(disjoint_entry),
      tempdir(),
      FALSE
    )
    expect_identical(disjoint_plan$error_code, "blocking_capability")

    disjoint_entry$settings$content_sources <- list(
      immune_repertoire = "unified_misc"
    )
    disjoint_plan <- builder_freeze_plan(
      list(disjoint_entry),
      tempdir(),
      FALSE
    )
    expect_null(disjoint_plan$error)
    expect_identical(
      disjoint_plan$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("equivalent full-IR sources auto-select by priority", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    object <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(object)
    common <- builder_immune_fixture_table(
      cells[1:2],
      chain = "TRB",
      suffix = "same"
    )
    object@misc$immune_repertoire <- list(sample_a = common)
    object@misc$tcr_data <- list(sample_a = common)
    fact <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire
    exact <- Filter(
      function(overlap) {
        identical(overlap$left, "unified_misc") &&
          identical(overlap$right, "legacy_tcr")
      },
      fact$full_source_overlaps
    )[[1L]]
    expect_true(exact$equivalent)

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- fact
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_null(selected$error)
    expect_identical(
      selected$items[[1L]]$manifest[[
        "immune_repertoire"
      ]]$evidence$selected_source,
      "unified_misc"
    )
  })
})

test_that("production motif-only sources cannot bypass exportability", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    motif_table <- function(barcodes, aa) {
      data.frame(
        barcode = barcodes,
        CTgene = rep("TRBV1.TRBJ1", length(barcodes)),
        CTaa = aa,
        stringsAsFactors = FALSE
      )
    }

    divergent <- builder_immune_fixture_object()
    cells <- SeuratObject::Cells(divergent)
    divergent@misc$immune_repertoire <- list(
      sample_a = motif_table(cells[1:2], c("CASS_A", "CASS_B"))
    )
    divergent@meta.data$CTgene <- NA_character_
    divergent@meta.data$CTaa <- NA_character_
    divergent@meta.data[cells[1:2], "CTgene"] <- "TRBV1.TRBJ1"
    divergent@meta.data[cells[1:2], "CTaa"] <- c(
      "DIFFERENT_A",
      "CASS_B"
    )
    divergent_fact <- builder_profile_immune_content(
      divergent,
      builder_immune_fixture_context(divergent)
    )$immune_repertoire
    expect_true(divergent_fact$candidates$unified_misc$hla_tcr_ready)
    expect_true(divergent_fact$candidates$metadata$hla_tcr_ready)
    expect_true(any(vapply(
      divergent_fact$motif_source_overlaps,
      function(overlap) overlap$n_divergent > 0L,
      logical(1)
    )))

    entry <- builder_task6_entry(immune_detected = FALSE)
    entry$dataset_profile$content$immune_repertoire <- divergent_fact
    blocked <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(blocked$error_code, "blocking_capability")

    entry$settings$content_sources <- list(
      hla_tcr_motifs = "unified_misc"
    )
    selected <- builder_freeze_plan(list(entry), tempdir(), FALSE)
    expect_identical(selected$error_code, "blocking_capability")

    disjoint <- builder_immune_fixture_object()
    disjoint$orig.ident <- "sample_a"
    disjoint$sample <- "sample_a"
    disjoint_cells <- SeuratObject::Cells(disjoint)
    disjoint@misc$immune_repertoire <- list(
      sample_a = motif_table(disjoint_cells[[1L]], "CASS_UNIFIED")
    )
    disjoint@meta.data$CTgene <- NA_character_
    disjoint@meta.data$CTaa <- NA_character_
    disjoint@meta.data[disjoint_cells[[2L]], "CTgene"] <-
      "TRBV1.TRBJ1"
    disjoint@meta.data[disjoint_cells[[2L]], "CTaa"] <- "CASS_METADATA"
    disjoint_fact <- builder_profile_immune_content(
      disjoint,
      builder_immune_fixture_context(disjoint)
    )$immune_repertoire
    disjoint_entry <- builder_task6_entry(immune_detected = FALSE)
    disjoint_entry$dataset_profile$content$immune_repertoire <-
      disjoint_fact

    disjoint_plan <- builder_freeze_plan(
      list(disjoint_entry),
      tempdir(),
      FALSE
    )
    expect_identical(disjoint_plan$error_code, "blocking_capability")
    disjoint_entry$settings$content_sources <- list(
      hla_tcr_motifs = "metadata"
    )
    expect_identical(
      builder_freeze_plan(list(disjoint_entry), tempdir(), FALSE)$error_code,
      "blocking_capability"
    )
  })
})

test_that("optional-content facts require inert typed evidence", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }
    capture_plan <- function(entry) {
      tryCatch(
        builder_freeze_plan(list(entry), tempdir(), FALSE),
        error = function(error) error
      )
    }
    expect_invalid_fact <- function(entry) {
      state <- capture_state(entry)
      plan <- capture_plan(entry)
      expect_s3_class(state, "builder_state_error")
      expect_identical(state$code, "invalid_content_evidence")
      expect_s3_class(plan, "builder_plan_failure")
      expect_identical(plan$error_code, "invalid_content_evidence")
    }

    accessor_called <- FALSE
    method_env <- environment(builder_dataset_state)
    method_name <- "$.builder_hostile_fact"
    had_method <- exists(method_name, envir = method_env, inherits = FALSE)
    if (had_method) {
      prior_method <- get(method_name, envir = method_env, inherits = FALSE)
    }
    assign(
      method_name,
      function(value, name) {
        accessor_called <<- TRUE
        stop("HOSTILE_FACT_ACCESSOR_EXECUTED", call. = FALSE)
      },
      envir = method_env
    )
    for (id in c("hla", "marker_genes")) {
      hostile <- builder_task6_entry()
      if (is.null(hostile$dataset_profile$content[[id]])) {
        hostile$dataset_profile$content[[id]] <- builder_task6_fact()
      }
      hostile$dataset_profile$content[[id]] <- structure(
        hostile$dataset_profile$content[[id]],
        class = c("builder_hostile_fact", "list")
      )
      expect_invalid_fact(hostile)
    }
    if (had_method) {
      assign(method_name, prior_method, envir = method_env)
    } else {
      rm(list = method_name, envir = method_env)
    }
    expect_false(accessor_called)

    malformed_fields <- list(
      detected = c(TRUE, FALSE),
      valid = 1,
      normalized = "rows",
      diagnostics = list("boom"),
      requirements = NA_character_,
      page_candidates = list("hla"),
      attention = c(TRUE, FALSE)
    )
    for (field in names(malformed_fields)) {
      malformed <- builder_task6_entry()
      malformed$dataset_profile$content$hla[[field]] <-
        malformed_fields[[field]]
      expect_invalid_fact(malformed)
    }
  })
})

test_that("malformed optional-content settings fail as plan records", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    sources <- builder_task6_entry(full_ir_ready = TRUE)
    sources$settings$content_sources <- "unified_misc"
    expect_identical(
      builder_freeze_plan(list(sources), tempdir(), FALSE)$error_code,
      "invalid_content_sources"
    )

    invalid_names <- list(
      unnamed = list("unified_misc"),
      blank = setNames(list("unified_misc"), ""),
      duplicate = structure(
        list("unified_misc", "metadata"),
        names = c("immune_repertoire", "immune_repertoire")
      ),
      unknown = list(unknown_capability = "unified_misc"),
      known_but_not_selectable = list(marker_genes = "table")
    )
    for (label in names(invalid_names)) {
      malformed <- builder_task6_entry(full_ir_ready = TRUE)
      malformed$settings$content_sources <- invalid_names[[label]]
      expect_identical(
        builder_freeze_plan(
          list(malformed),
          tempdir(),
          FALSE
        )$error_code,
        "invalid_content_sources",
        info = label
      )
    }

    invalid_value <- builder_task6_entry()
    invalid_value$settings$content_sources <- list(
      immune_repertoire = ""
    )
    expect_identical(
      builder_freeze_plan(
        list(invalid_value),
        tempdir(),
        FALSE
      )$error_code,
      "invalid_content_source"
    )

    for (value in c("preserved", "filtered", "stored_only")) {
      unknown <- builder_task6_entry()
      unknown$settings$content_dispositions <- list(
        unknown_capability = value
      )
      state_error <- tryCatch(
        builder_dataset_state(unknown),
        builder_state_error = function(error) error
      )
      expect_s3_class(
        state_error,
        "builder_state_error"
      )
      expect_identical(
        state_error$code,
        "invalid_content_dispositions",
        info = value
      )
      expect_identical(
        builder_freeze_plan(
          list(unknown),
          tempdir(),
          FALSE
        )$error_code,
        "invalid_content_dispositions",
        info = value
      )
    }

    dispositions <- builder_task6_entry()
    dispositions$settings$content_dispositions <- "preserved"
    expect_identical(
      builder_freeze_plan(list(dispositions), tempdir(), FALSE)$error_code,
      "invalid_content_dispositions"
    )
  })
})

test_that("optional-content setting records are inert and non-dispatching", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }
    capture_plan <- function(entry) {
      tryCatch(
        builder_freeze_plan(list(entry), tempdir(), FALSE),
        error = function(error) error
      )
    }
    expect_invalid <- function(entry, code, info) {
      state <- capture_state(entry)
      plan <- capture_plan(entry)
      expect_s3_class(state, "builder_state_error")
      expect_identical(state$code, code, info = info)
      expect_s3_class(plan, "builder_plan_failure")
      expect_identical(plan$error_code, code, info = info)
    }

    accessor_called <- FALSE
    method_env <- environment(builder_dataset_state)
    method_name <- "[[.evil_choices"
    had_method <- exists(method_name, envir = method_env, inherits = FALSE)
    if (had_method) {
      prior_method <- get(method_name, envir = method_env, inherits = FALSE)
    }
    assign(
      method_name,
      function(value, name, ...) {
        accessor_called <<- TRUE
        stop("EVIL_CHOICES_ACCESSOR_EXECUTED", call. = FALSE)
      },
      envir = method_env
    )

    cases <- list(
      content_dispositions = list(
        value = list(hla = "filtered"),
        code = "invalid_content_dispositions"
      ),
      content_sources = list(
        value = list(immune_repertoire = "unified_misc"),
        code = "invalid_content_sources"
      )
    )
    for (field in names(cases)) {
      classed <- builder_task6_entry(full_ir_ready = TRUE)
      classed$settings[[field]] <- structure(
        cases[[field]]$value,
        class = c("evil_choices", "list")
      )
      expect_invalid(classed, cases[[field]]$code, paste(field, "class"))
    }
    expect_false(accessor_called)

    if (had_method) {
      assign(method_name, prior_method, envir = method_env)
    } else {
      rm(list = method_name, envir = method_env)
    }

    for (field in names(cases)) {
      attributed <- builder_task6_entry(full_ir_ready = TRUE)
      record <- cases[[field]]$value
      attr(record, "probe") <- new.env(parent = emptyenv())
      attributed$settings[[field]] <- record
      expect_invalid(
        attributed,
        cases[[field]]$code,
        paste(field, "reference attribute")
      )
    }

    scalar_cases <- list(
      content_dispositions = list(
        id = "hla",
        value = "filtered",
        code = "invalid_content_disposition"
      ),
      content_sources = list(
        id = "immune_repertoire",
        value = "unified_misc",
        code = "invalid_content_source"
      )
    )
    for (field in names(scalar_cases)) {
      scalar_case <- scalar_cases[[field]]
      for (shape in c("class", "attribute")) {
        malformed <- builder_task6_entry(full_ir_ready = TRUE)
        value <- scalar_case$value
        if (identical(shape, "class")) {
          class(value) <- "evil_choice_value"
        } else {
          attr(value, "note") <- "not-plain"
        }
        malformed$settings[[field]] <- setNames(
          list(value),
          scalar_case$id
        )
        expect_invalid(
          malformed,
          scalar_case$code,
          paste(field, "scalar", shape)
        )
      }
    }
  })
})
