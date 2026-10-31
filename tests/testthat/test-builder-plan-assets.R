builder_plan_contract_source_runtime(environment())

test_that("artifact features follow the selected assay layer order", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    entry <- builder_task6_entry()
    default_features <- c("Gene1", "Gene2")
    selected_features <- rev(default_features)
    entry$settings$assay <- "Xenium"
    entry$settings$layer <- "counts"
    entry$dataset_profile$assays <- list(
      SCT = list(
        layers = list(
          data = list(features = list(canonical_ids = default_features))
        )
      ),
      Xenium = list(
        layers = list(
          counts = list(features = list(canonical_ids = selected_features))
        )
      )
    )

    identity <- .builder_plan_artifact_identity(
      entry,
      included_groups = "cluster",
      included_projections = "umap"
    )

    expect_identical(
      identity$features,
      builder_axis_identity(selected_features)
    )
    expect_false(identical(
      identity$features,
      builder_axis_identity(default_features)
    ))
  })
})

test_that("plan input boundaries always return structured failures", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    expect_failure <- function(result, code, info = NULL) {
      expect_s3_class(result, "builder_plan_failure")
      expect_identical(result$error_code, code, info = info)
    }
    capture <- function(...) {
      tryCatch(
        builder_freeze_plan(...),
        error = function(error) error
      )
    }
    capture_state <- function(entry) {
      tryCatch(
        builder_dataset_state(entry),
        error = function(error) error
      )
    }

    invalid_entry <- capture(list(1), tempdir(), FALSE)
    expect_failure(invalid_entry, "invalid_entries", "entry")

    invalid_settings <- builder_task6_entry()
    invalid_settings$settings <- "settings"
    expect_failure(
      capture(list(invalid_settings), tempdir(), FALSE),
      "invalid_entries",
      "settings"
    )

    recommendation_entry <- builder_task6_entry()
    valid_recommendations <- recommendation_entry$settings$recommendations
    recommendation_cases <- list(
      outer_atomic = "recommendations",
      outer_classed = structure(
        valid_recommendations,
        class = "builder_recommendations"
      ),
      groups_atomic = utils::modifyList(
        valid_recommendations,
        list(groups = "groups")
      ),
      projections_atomic = utils::modifyList(
        valid_recommendations,
        list(projections = "projections")
      ),
      groups_included = utils::modifyList(
        valid_recommendations,
        list(groups = list(included = list("cluster")))
      ),
      projections_included = utils::modifyList(
        valid_recommendations,
        list(projections = list(included = 1))
      ),
      groups_classed = valid_recommendations
    )
    recommendation_cases$groups_classed$groups <- structure(
      list(included = "cluster"),
      class = "builder_group_recommendation"
    )
    reference_recommendations <- valid_recommendations
    reference_recommendations$groups$mutable <-
      new.env(parent = emptyenv())
    recommendation_cases$groups_reference <- reference_recommendations
    for (label in names(recommendation_cases)) {
      invalid_recommendations <- builder_task6_entry()
      invalid_recommendations$settings$recommendations <-
        recommendation_cases[[label]]
      state_result <- capture_state(invalid_recommendations)
      expect_s3_class(state_result, "builder_state_error")
      expect_identical(
        state_result$code,
        "invalid_recommendations",
        info = paste("state", label)
      )
      expect_failure(
        capture(list(invalid_recommendations), tempdir(), FALSE),
        "invalid_recommendations",
        paste("plan", label)
      )
    }

    invalid_name <- builder_task6_entry()
    invalid_name$settings$name <- c("Dataset", "Other")
    expect_failure(
      capture(list(invalid_name), tempdir(), FALSE),
      "invalid_dataset_name",
      "name"
    )

    null_analyses <- builder_task6_entry()
    null_analyses$settings$analyses <- NULL
    null_state <- builder_dataset_state(null_analyses)
    expect_identical(null_state$analyses, character())
    null_plan <- builder_freeze_plan(
      list(null_analyses),
      tempdir(),
      FALSE
    )
    expect_null(null_plan$error)
    expect_identical(null_plan$items[[1L]]$analyses, character())

    invalid_analyses <- list(
      list("marker_genes"),
      NA_character_,
      "unknown_analysis",
      c("marker_genes", "marker_genes"),
      new.env(parent = emptyenv())
    )
    for (index in seq_along(invalid_analyses)) {
      entry <- builder_task6_entry()
      entry$settings$analyses <- invalid_analyses[[index]]
      state_result <- tryCatch(
        builder_dataset_state(entry),
        builder_state_error = function(error) error,
        error = function(error) error
      )
      expect_s3_class(
        state_result,
        "builder_state_error"
      )
      expect_identical(
        state_result$code,
        "invalid_analyses",
        info = paste("state analyses", index)
      )
      expect_failure(
        capture(list(entry), tempdir(), FALSE),
        "invalid_analyses",
        paste("analyses", index)
      )
    }

    for (value in list(1, NA, c(TRUE, FALSE))) {
      expect_failure(
        capture(list(builder_task6_entry()), tempdir(), value),
        "invalid_make_app",
        "make_app"
      )
      expect_failure(
        capture(
          list(builder_task6_entry()),
          tempdir(),
          FALSE,
          overwrite = value
        ),
        "invalid_overwrite",
        "overwrite"
      )
    }

    expect_failure(
      capture(
        list(builder_task6_entry()),
        tempdir(),
        FALSE,
        app_options = structure(list(), class = "options")
      ),
      "invalid_app_options",
      "app_options"
    )
    for (value in list(NULL, 1, NA, c(TRUE, FALSE))) {
      expect_failure(
        capture(
          list(builder_task6_entry()),
          tempdir(),
          FALSE,
          app_options = list(show_upload_ui = value)
        ),
        "invalid_app_options",
        "show_upload_ui"
      )
    }
    for (value in list(
      NULL,
      1,
      NA_character_,
      c("a", "b"),
      ""
    )) {
      expect_failure(
        capture(
          list(builder_task6_entry()),
          tempdir(),
          FALSE,
          app_options = list(initial_dataset = value)
        ),
        "invalid_app_options",
        "initial_dataset"
      )
    }
    expect_failure(
      capture(
        list(builder_task6_entry()),
        tempdir(),
        FALSE,
        expected_prior_identity = "prior"
      ),
      "invalid_expected_prior_identity",
      "prior_identity"
    )

    malformed_metadata <- builder_task6_entry()
    malformed_metadata$settings$recommendations$metadata <- "metadata"
    expect_failure(
      capture(list(malformed_metadata), tempdir(), FALSE),
      "invalid_metadata_policy",
      "metadata_recommendation"
    )
  })
})

test_that("backends derive exact private sidecars and release targets", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    out_dir <- file.path(tempdir(), "task6-sidecar-targets")
    h5 <- builder_task6_entry()
    h5$settings$expression_backend <- "h5"
    h5$settings$sidecars <- NULL
    unsupported <- builder_task6_entry()
    unsupported$settings$expression_backend <- "bpcells"
    unsupported$settings$sidecars <- NULL
    expect_identical(
      builder_freeze_plan(list(unsupported), out_dir, FALSE)$error_code,
      "invalid_expression_backend"
    )

    plan <- builder_freeze_plan(list(h5), out_dir, FALSE)
    expect_null(plan$error)
    expected_sidecars <- paste0(
      tools::file_path_sans_ext(plan$items[[1L]]$filename),
      ".h5"
    )
    expect_identical(plan$items[[1L]]$sidecars, expected_sidecars)
    expected_targets <- file.path(
      plan$out_dir,
      c(plan$items[[1L]]$filename, expected_sidecars)
    )
    expect_identical(plan$output_release$targets, expected_targets)
    expect_true(all(expected_sidecars %in% plan$private_assets))

    embedded_mismatch <- builder_task6_entry()
    embedded_mismatch$settings$sidecars <- "forged.h5"
    expect_identical(
      builder_freeze_plan(
        list(embedded_mismatch),
        out_dir,
        FALSE
      )$error_code,
      "invalid_backend_sidecars"
    )

    h5_mismatch <- builder_task6_entry()
    h5_mismatch$settings$expression_backend <- "h5"
    h5_mismatch$settings$sidecars <- "wrong-dataset.h5"
    expect_identical(
      builder_freeze_plan(list(h5_mismatch), out_dir, FALSE)$error_code,
      "invalid_backend_sidecars"
    )
  })
})

test_that("output directories are scalar absolute release authorities", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    for (invalid in list(
      NA_character_,
      c(tempdir(), tempdir()),
      character(),
      42
    )) {
      failed <- builder_freeze_plan(
        list(builder_task6_entry()),
        invalid,
        FALSE
      )
      expect_s3_class(failed, "builder_plan_failure")
      expect_identical(
        failed$error_code,
        "invalid_output_directory"
      )
    }

    blank <- builder_freeze_plan(
      list(builder_task6_entry()),
      "  ",
      FALSE
    )
    expect_identical(blank$error_code, "missing_output_directory")

    root <- withr::local_tempdir()
    dir.create(file.path(root, "segment"))
    dir.create(file.path(root, "release"))
    supplied <- file.path(root, "segment", "..", "release")
    expected <- as.character(fs::path_norm(fs::path_abs(
      fs::path_expand(supplied)
    )))
    plan <- builder_freeze_plan(
      list(builder_task6_entry()),
      supplied,
      FALSE
    )

    expect_null(plan$error)
    expect_identical(plan$out_dir, expected)
    expect_identical(plan$output_release$directory, expected)
    expect_true(all(startsWith(
      plan$output_release$targets,
      paste0(expected, .Platform$file.sep)
    )))
  })
})

test_that("asset manifests encode dedupe and target conflicts", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    duplicate <- builder_task6_entry()
    duplicate$settings$viewer_bundle_assets <- c("readme.html", "readme.html")
    expect_identical(
      builder_freeze_plan(list(duplicate), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )

    for (legacy_key in c("public_assets", "public_asset_claims")) {
      legacy <- builder_task6_entry()
      legacy$settings[[legacy_key]] <- "legacy.html"
      expect_identical(
        builder_freeze_plan(list(legacy), tempdir(), FALSE)$error_code,
        "invalid_entries"
      )
    }

    malformed <- builder_task6_entry()
    malformed$settings$private_assets <- c("", NA_character_)
    expect_identical(
      builder_freeze_plan(list(malformed), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )

    generated_public <- builder_task6_entry()
    generated_public$settings$viewer_bundle_assets <- builder_item_filename(
      generated_public,
      1L,
      1L
    )
    expect_identical(
      builder_freeze_plan(list(generated_public), tempdir(), FALSE)$error_code,
      "asset_scope_conflict"
    )

    intersection <- builder_task6_entry()
    intersection$settings$viewer_bundle_assets <- "shared.txt"
    intersection$settings$private_assets <- "shared.txt"
    scope_conflict <- builder_freeze_plan(
      list(intersection),
      tempdir(),
      FALSE
    )
    expect_identical(scope_conflict$error_code, "asset_scope_conflict")
    expect_match(scope_conflict$error, "Viewer-bundle")
    expect_false(grepl("public", scope_conflict$error, fixed = TRUE))

    shared_claim <- builder_task6_asset_claim(
      source = "/source/histology.png",
      target = "shared.css",
      artifact = "spatial_image"
    )
    first <- builder_task6_entry()
    first$settings$viewer_bundle_assets <- list(shared_claim)
    second <- builder_task6_entry()
    second$id <- "dataset-b"
    second$settings$name <- "Dataset B"
    second$dataset_profile$source$location <- "another-dataset"
    second$dataset_profile$source$fingerprint <- "another-dataset:v1"
    second$settings$viewer_bundle_assets <- list(shared_claim)
    shared <- builder_freeze_plan(list(first, second), tempdir(), FALSE)
    expect_null(shared$error)
    expect_identical(shared$viewer_bundle_assets, "shared.css")
    expect_length(shared$items[[1L]]$viewer_bundle_asset_claims, 1L)
    expect_identical(
      shared$items[[1L]]$viewer_bundle_asset_claims[[1L]]$source,
      "/source/histology.png"
    )
    expect_identical(
      shared$items[[1L]]$viewer_bundle_asset_claims[[1L]]$target,
      "shared.css"
    )
    expect_identical(
      shared$items[[1L]]$viewer_bundle_asset_claims[[1L]]$artifact,
      "spatial_image"
    )

    conflicting <- second
    conflicting$settings$viewer_bundle_assets <- list(builder_task6_asset_claim(
      source = "/other/histology.png",
      target = "shared.css",
      artifact = "spatial_image"
    ))
    expect_identical(
      builder_freeze_plan(
        list(first, conflicting),
        tempdir(),
        FALSE
      )$error_code,
      "asset_target_conflict"
    )

    within_dataset <- builder_task6_entry()
    within_dataset$settings$viewer_bundle_assets <- list(
      shared_claim,
      builder_task6_asset_claim(
        source = "/other/histology.png",
        target = "shared.css",
        artifact = "spatial_image"
      )
    )
    expect_identical(
      builder_freeze_plan(
        list(within_dataset),
        tempdir(),
        FALSE
      )$error_code,
      "asset_target_conflict"
    )

    legacy_first <- builder_task6_entry()
    legacy_first$settings$viewer_bundle_assets <- "legacy.css"
    legacy_second <- builder_task6_entry()
    legacy_second$id <- "dataset-b"
    legacy_second$settings$name <- "Dataset B"
    legacy_second$settings$viewer_bundle_assets <- "legacy.css"
    expect_identical(
      builder_freeze_plan(
        list(legacy_first, legacy_second),
        tempdir(),
        FALSE
      )$error_code,
      "asset_target_conflict"
    )

    valid <- builder_task6_entry()
    valid$settings$viewer_bundle_assets <- "readme.html"
    valid$settings$private_assets <- "audit.json"
    plan <- builder_freeze_plan(list(valid), tempdir(), FALSE)
    expect_null(plan$error)
    expect_identical(plan$viewer_bundle_assets, "readme.html")
    expect_length(plan$items[[1L]]$viewer_bundle_asset_claims, 1L)
    expect_true(length(plan$items[[1L]]$private_asset_claims) >= 1L)
    expect_true(all(
      c("audit.json", plan$items[[1L]]$filename) %in%
        plan$private_assets
    ))
    expect_length(intersect(plan$viewer_bundle_assets, plan$private_assets), 0L)

    recursive_names <- function(value) {
      if (!is.list(value)) {
        return(character())
      }
      c(names(value), unlist(lapply(value, recursive_names), use.names = FALSE))
    }
    expect_false(any(
      c("public_assets", "public_asset_claims") %in% recursive_names(plan)
    ))
  })
})

test_that("typed asset claims normalize before comparison", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    canonical <- builder_task6_asset_claim(
      source = "/source/histology.png",
      target = "shared.css",
      artifact = "spatial_image"
    )
    attributed_artifact <- structure(
      "spatial_image",
      label = "display-only"
    )
    reordered <- structure(
      list(
        artifact = attributed_artifact,
        source = structure(
          "/source/histology.png",
          names = "display-only"
        ),
        target = structure("shared.css", label = "display-only")
      ),
      note = "display-only",
      class = c("builder_asset_claim", "list")
    )

    first <- builder_task6_entry()
    first$settings$viewer_bundle_assets <- list(canonical)
    second <- builder_task6_entry()
    second$id <- "dataset-b"
    second$settings$name <- "Dataset B"
    second$dataset_profile$source$location <- "another-dataset"
    second$dataset_profile$source$fingerprint <- "another-dataset:v1"
    second$settings$viewer_bundle_assets <- list(reordered)

    plan <- builder_freeze_plan(list(first, second), tempdir(), FALSE)
    expect_null(plan$error)
    expect_identical(plan$viewer_bundle_assets, "shared.css")
    expect_length(plan$items[[1L]]$viewer_bundle_asset_claims, 1L)
    expect_identical(
      plan$items[[1L]]$viewer_bundle_asset_claims[[1L]],
      canonical
    )

    unsafe <- builder_task6_entry()
    unsafe_claim <- canonical
    attr(unsafe_claim, "mutable") <- new.env(parent = emptyenv())
    unsafe$settings$viewer_bundle_assets <- list(unsafe_claim)
    expect_identical(
      builder_freeze_plan(list(unsafe), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )

    extra <- builder_task6_entry()
    extra_claim <- canonical
    extra_claim$note <- "unexpected"
    extra$settings$viewer_bundle_assets <- list(extra_claim)
    expect_identical(
      builder_freeze_plan(list(extra), tempdir(), FALSE)$error_code,
      "invalid_asset_manifest"
    )
  })
})

test_that("only owned production snapshots are available to plans", {
  skip_if_not_installed("SeuratObject")

  local({
    builder_repo_source("io.R")
    builder_repo_source("adapters.R")
    builder_repo_source("preview.R")
    builder_repo_source("recommend.R")
    builder_repo_source("plan.R")

    root <- withr::local_tempdir()
    snapshot <- builder_snapshot_seurat(
      builder_immune_fixture_object(),
      file.path(root, "real.snapshot"),
      available_bytes = 2^40
    )
    withr::defer(.builder_snapshot_release(snapshot))
    expect_type(snapshot, "list")
    expect_false(inherits(snapshot, "builder_snapshot_identity"))

    real <- builder_task6_entry()
    real$snapshot <- snapshot
    real_plan <- builder_freeze_plan(list(real), tempdir(), FALSE)
    expect_null(real_plan$error)
    expect_true(
      real_plan$source_snapshot_identities[["dataset-a"]]$available
    )
    expect_identical(
      real_plan$source_snapshot_identities[["dataset-a"]]$object_md5,
      snapshot$object_md5
    )

    forged <- snapshot
    forged$owner_token <- paste0(forged$owner_token, "-forged")
    forged <- structure(
      forged,
      class = c("builder_snapshot_identity", "list")
    )
    forged_entry <- builder_task6_entry()
    forged_entry$snapshot <- forged
    forged_plan <- builder_freeze_plan(
      list(forged_entry),
      tempdir(),
      FALSE
    )
    expect_null(forged_plan$error)
    expect_false(
      forged_plan$source_snapshot_identities[["dataset-a"]]$available
    )

    missing <- builder_task6_entry()
    missing$snapshot_identity <- builder_task6_snapshot_identity()
    missing_plan <- builder_freeze_plan(list(missing), tempdir(), FALSE)
    expect_null(missing_plan$error)
    expect_false(
      missing_plan$source_snapshot_identities[["dataset-a"]]$available
    )

    shadowed <- builder_task6_entry()
    shadowed$snapshot_identity <- builder_task6_snapshot_identity()
    shadowed$snapshot <- snapshot
    shadowed_plan <- builder_freeze_plan(
      list(shadowed),
      tempdir(),
      FALSE
    )
    expect_true(
      shadowed_plan$source_snapshot_identities[["dataset-a"]]$available
    )
    expect_null(
      shadowed_plan$error
    )
    expect_identical(
      shadowed_plan$source_snapshot_identities[["dataset-a"]]$object_md5,
      snapshot$object_md5
    )
  })
})
