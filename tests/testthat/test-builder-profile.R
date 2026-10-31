builder_profile_source_runtime()
builder_repo_source("inspect.R")

test_that("a real small Seurat object produces a versioned typed profile", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  profile <- builder_dataset_profile(
    object,
    builder_profile_source_fixture()
  )
  cells <- SeuratObject::Cells(object)

  expect_s3_class(profile, "builder_dataset_profile")
  expect_identical(profile$schema_version, 2L)
  expect_identical(profile$source$type, "example")
  expect_identical(profile$source$location, "pbmc_small")
  expect_true("Seurat" %in% profile$object$class)
  expect_type(profile$object$version, "character")
  expect_identical(profile$identity$cells$ids, cells)
  expect_identical(profile$identity$cells$count, length(cells))
  expect_true(profile$identity$cells$valid)
  expect_identical(
    profile$identity$features$ids,
    SeuratObject::Features(object)
  )
  expect_identical(profile$identity$metadata$ids, rownames(object@meta.data))
  expect_true(profile$identity$metadata$valid)
  expect_identical(profile$default_assay, "RNA")
  expect_named(profile$assays, names(object@assays))
  expect_named(profile$metadata$columns, colnames(object@meta.data))
  expect_named(profile$reductions, names(object@reductions))
  expect_identical(profile$spatial$sections, character())
  expect_true(profile$spatial$deferred)
  expect_named(profile$organism, c("code", "confidence", "reason"))
  expect_s3_class(profile$manifest, "builder_content_manifest")
})

test_that("workspace profiles replace full axis vectors with compact identities", {
  skip_if_not_installed("SeuratObject")

  object <- builder_profile_pbmc()
  profile <- builder_dataset_profile(
    object,
    builder_profile_source_fixture()
  )
  cells <- SeuratObject::Cells(object)
  features <- SeuratObject::Features(object)
  first_layer_features <- profile$assays[[1L]]$layers[[
    1L
  ]]$features$canonical_ids
  workspace <- builder_profile_workspace_contract(profile)

  expect_s3_class(workspace, "builder_dataset_profile")
  expect_identical(
    workspace$identity$cells$axis_identity,
    builder_axis_identity(cells)
  )
  expect_identical(
    workspace$identity$features$axis_identity,
    builder_axis_identity(features)
  )
  expect_null(workspace$identity$cells$ids)
  expect_null(workspace$identity$cells$canonical_ids)
  expect_null(workspace$identity$cells$reorder_index)
  expect_identical(
    workspace$identity$features$sample_ids,
    utils::head(features, 500L)
  )
  first_layer <- workspace$assays[[1L]]$layers[[1L]]
  expect_null(first_layer$cells$ids)
  expect_null(first_layer$features$ids)
  expect_identical(
    first_layer$features$axis_identity,
    builder_axis_identity(first_layer_features)
  )
  expect_true(all(vapply(
    workspace$reductions,
    function(reduction) is.null(reduction$cells$ids),
    logical(1)
  )))
  expect_null(
    workspace$manifest[["dataset_identity"]]$diagnostics$cells$ids
  )
})

test_that("axis identities preserve order, missing values, and UTF-8 text", {
  ids <- c("cell/一", "line\nbreak", enc2utf8("é"), NA_character_)
  identity <- builder_axis_identity(ids)

  expect_true(builder_axis_identity_valid(identity))
  expect_identical(identity$schema_version, 2L)
  expect_identical(identity$count, 4)
  expect_identical(identity, builder_axis_identity(ids))
  expect_false(identical(identity, builder_axis_identity(rev(ids))))
})

test_that("legacy spatial fields reuse the bounded content profile", {
  skip_if_not_installed("SeuratObject")

  object <- builder_content_spatial_example_object()
  names(object@images) <- strrep("section-name-", 100000L)
  profile <- builder_dataset_profile(
    object,
    builder_profile_source_fixture()
  )
  spatial_fact <- profile$content$spatial$normalized

  expect_identical(profile$spatial$section_count, spatial_fact$section_count)
  expect_identical(
    profile$spatial$sections,
    vapply(spatial_fact$sections, function(section) section$name, character(1))
  )
  expect_identical(
    profile$spatial$section_names_truncated,
    spatial_fact$section_names_truncated
  )
  expect_true(profile$spatial$section_names_truncated)
  expect_false(profile$spatial$sections_truncated)
  expect_lte(nchar(profile$spatial$sections[[1L]], type = "bytes"), 256L)
  expect_lt(as.numeric(object.size(profile$spatial)), 100000)
})

test_that("source records are inert validated data", {
  skip_if_not_installed("SeuratObject")
  object <- builder_profile_pbmc()
  touched <- FALSE

  expect_error(
    builder_dataset_profile(
      object,
      list(
        type = "file",
        location = tempfile(),
        fingerprint = function() {
          touched <<- TRUE
          "unsafe"
        }
      )
    ),
    class = "builder_profile_error"
  )
  expect_false(touched)
  expect_error(
    builder_dataset_profile(object, list(type = "file", location = "")),
    class = "builder_profile_error"
  )

  absolute <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
  profile <- builder_dataset_profile(
    object,
    list(type = "file", location = absolute)
  )
  summaries <- vapply(
    profile$manifest,
    function(entry) entry$summary,
    character(1)
  )
  expect_false(any(grepl(absolute, summaries, fixed = TRUE)))

  touched <- FALSE
  assign(
    "[[.builder_profile_source_trap",
    function(value, index, ...) {
      touched <<- TRUE
      NextMethod("[[")
    },
    envir = .GlobalEnv
  )
  assign(
    "is.na.builder_profile_source_trap",
    function(value) {
      touched <<- TRUE
      NextMethod("is.na")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm(
      "[[.builder_profile_source_trap",
      "is.na.builder_profile_source_trap",
      envir = .GlobalEnv
    ),
    add = TRUE
  )
  classed_source <- structure(
    list(type = "file", location = absolute),
    class = "builder_profile_source_trap"
  )
  expect_error(
    builder_profile_source(classed_source),
    class = "builder_profile_error"
  )
  nested_source <- list(
    type = "file",
    location = absolute,
    fingerprint = structure(
      list("unsafe"),
      class = "builder_profile_source_trap"
    )
  )
  expect_error(
    builder_profile_source(nested_source),
    class = "builder_profile_error"
  )
  scalar_source <- list(
    type = structure("file", class = "builder_profile_source_trap"),
    location = absolute
  )
  expect_error(
    builder_profile_source(scalar_source),
    class = "builder_profile_error"
  )
  attributed_source <- builder_profile_source(list(
    type = structure("file", evil = function() touched <<- TRUE),
    location = structure(absolute, provenance = "local"),
    fingerprint = structure("abc", opaque = TRUE)
  ))
  expect_identical(attributed_source$type, "file")
  expect_identical(attributed_source$location, absolute)
  expect_identical(attributed_source$fingerprint, "abc")
  expect_null(attributes(attributed_source$type))
  expect_null(attributes(attributed_source$location))
  expect_null(attributes(attributed_source$fingerprint))
  expect_false(touched)
})

test_that("metadata identity permits reorder but rejects identity damage", {
  skip_if_not_installed("SeuratObject")
  object <- builder_profile_pbmc()
  cells <- SeuratObject::Cells(object)

  shuffled <- object@meta.data[
    rev(seq_len(nrow(object@meta.data))),
    ,
    drop = FALSE
  ]
  shuffled_profile <- builder_profile_metadata(shuffled, cells)$identity
  expect_true(shuffled_profile$valid)
  expect_false(shuffled_profile$order_matches)
  expect_equal(shuffled_profile$coverage, 1)
  expect_identical(shuffled_profile$canonical_ids, cells)
  expect_identical(
    shuffled_profile$reorder_index,
    match(cells, rownames(shuffled))
  )

  missing <- object@meta.data[-1L, , drop = FALSE]
  missing_profile <- builder_profile_metadata(missing, cells)$identity
  expect_false(missing_profile$valid)
  expect_identical(
    missing_profile$missing,
    cells[1L]
  )

  extra_row <- object@meta.data[1L, , drop = FALSE]
  rownames(extra_row) <- "not_a_dataset_cell"
  extra <- rbind(object@meta.data, extra_row)
  extra_profile <- builder_profile_metadata(extra, cells)$identity
  expect_false(extra_profile$valid)
  expect_identical(
    extra_profile$extra,
    "not_a_dataset_cell"
  )

  duplicated_meta <- object@meta.data
  ids <- rownames(duplicated_meta)
  ids[2L] <- ids[1L]
  attr(duplicated_meta, "row.names") <- ids
  duplicated_profile <- builder_profile_metadata(
    duplicated_meta,
    cells
  )$identity
  expect_false(duplicated_profile$valid)
  expect_identical(
    duplicated_profile$duplicates,
    ids[1L]
  )

  blank_meta <- object@meta.data
  blank_ids <- rownames(blank_meta)
  blank_ids[2L] <- ""
  attr(blank_meta, "row.names") <- blank_ids
  blank_profile <- builder_profile_metadata(blank_meta, cells)$identity
  expect_false(blank_profile$valid)
  expect_identical(blank_profile$blanks, "")
})

test_that("metadata column names are unique non-empty identities", {
  skip_if_not_installed("SeuratObject")
  object <- builder_profile_pbmc()

  duplicate <- object
  duplicate_names <- colnames(duplicate@meta.data)
  duplicate_names[2L] <- duplicate_names[1L]
  colnames(duplicate@meta.data) <- duplicate_names
  expect_true(methods::validObject(duplicate, test = TRUE, complete = TRUE))
  duplicate_profile <- builder_dataset_profile(
    duplicate,
    builder_profile_source_fixture()
  )
  expect_false(duplicate_profile$metadata$column_identity$valid)
  expect_identical(
    duplicate_profile$metadata$column_identity$duplicates,
    duplicate_names[1L]
  )
  expect_identical(
    duplicate_profile$manifest[["metadata"]]$status,
    "blocking"
  )
  expect_identical(
    duplicate_profile$manifest[["groups"]]$status,
    "blocking"
  )

  missing_name <- object
  missing_names <- colnames(missing_name@meta.data)
  missing_names[2L] <- NA_character_
  colnames(missing_name@meta.data) <- missing_names
  expect_true(methods::validObject(
    missing_name,
    test = TRUE,
    complete = TRUE
  ))
  missing_profile <- builder_dataset_profile(
    missing_name,
    builder_profile_source_fixture()
  )
  expect_false(missing_profile$metadata$column_identity$valid)
  expect_true(anyNA(missing_profile$metadata$column_identity$blanks))
  expect_identical(missing_profile$groups$candidates, character())

  blank_meta <- object@meta.data
  blank_names <- colnames(blank_meta)
  blank_names[2L] <- ""
  colnames(blank_meta) <- blank_names
  blank_profile <- builder_profile_metadata(
    blank_meta,
    SeuratObject::Cells(object)
  )
  expect_false(blank_profile$column_identity$valid)
  expect_identical(blank_profile$column_identity$blanks, "")
  expect_identical(blank_profile$groups$candidates, character())
})

test_that("metadata catalog is complete, friendly, and bounded", {
  cells <- paste0("cell-", seq_len(8L))
  metadata <- data.frame(
    cluster = factor(c("A", "A", "B", "B", "C", "C", NA, "A")),
    continuous_score = seq_len(8L) + 0.25,
    cell_identifier = cells,
    nCount_RNA = seq_len(8L) * 100L,
    notes = c(rep(strrep("x", 500L), 5L), "short", "shorter", "last"),
    row.names = cells,
    check.names = FALSE
  )

  profiled <- builder_profile_metadata(metadata, cells)
  catalog <- profiled$catalog

  expect_named(catalog, colnames(metadata), ignore.order = FALSE)
  expect_true(catalog$cluster$group_eligible)
  expect_identical(catalog$cluster$classification, "categorical")
  expect_equal(catalog$cluster$missing_percentage, 12.5)
  expect_identical(catalog$cluster$distinct_count, 3L)
  expect_lte(length(catalog$cluster$level_counts$items), 12L)
  expect_identical(catalog$cluster$level_counts$total, 4L)

  expect_false(catalog$continuous_score$group_eligible)
  expect_identical(catalog$continuous_score$classification, "continuous")
  expect_match(catalog$continuous_score$group_reason, "Continuous")
  expect_false(catalog$cell_identifier$group_eligible)
  expect_match(catalog$cell_identifier$group_reason, "different value")
  expect_false(catalog$nCount_RNA$group_eligible)
  expect_match(catalog$nCount_RNA$group_reason, "quality-control")

  expect_true(all(vapply(
    catalog,
    function(column) length(column$sample_values) <= 5L,
    logical(1)
  )))
  expect_lte(max(nchar(catalog$notes$sample_values, type = "bytes")), 120L)
  expect_false(any(vapply(
    catalog,
    function(column) is.data.frame(column$sample_values),
    logical(1)
  )))
})

test_that("assay layers use exact barcodes rather than equal counts", {
  skip_if_not_installed("SeuratObject")
  fixture <- builder_profile_wrong_assay()
  profile <- builder_profile_assay(
    fixture$assay,
    fixture$expected,
    "ALT"
  )
  layer <- profile$layers$counts

  expect_true(methods::validObject(fixture$assay, test = TRUE, complete = TRUE))
  expect_identical(layer$cells$count, length(fixture$expected))
  expect_false(layer$cells$valid)
  expect_false(layer$exportable)
  expect_identical(layer$cells$missing, "cell6")
  expect_identical(layer$cells$extra, "ghost")
})

test_that("split logical layers require one unique exact partition", {
  skip_if_not_installed("SeuratObject")
  expected <- paste0("cell", seq_len(6L))

  complete_assay <- builder_profile_partition_assay("complete")
  complete <- builder_profile_assay(complete_assay, expected, "SPLIT")
  valid_root <- complete$layers$counts
  expect_true(methods::validObject(
    complete_assay,
    test = TRUE,
    complete = TRUE
  ))
  expect_identical(valid_root$kind, "logical")
  expect_true(valid_root$exportable)
  expect_true(valid_root$cells$valid)
  expect_setequal(valid_root$members, c("counts.one", "counts.two"))
  expect_identical(valid_root$partition_status, "unique")

  overlap_assay <- builder_profile_partition_assay("overlap")
  overlap_root <- builder_profile_assay(
    overlap_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_true(methods::validObject(
    overlap_assay,
    test = TRUE,
    complete = TRUE
  ))
  expect_false(overlap_root$exportable)
  expect_true(length(overlap_root$cells$duplicates) > 0L)
  expect_identical(overlap_root$partition_status, "none")

  missing_assay <- builder_profile_partition_assay("missing")
  missing_root <- builder_profile_assay(
    missing_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_true(methods::validObject(
    missing_assay,
    test = TRUE,
    complete = TRUE
  ))
  expect_false(missing_root$exportable)
  expect_true(length(missing_root$cells$missing) > 0L)

  noise_assay <- builder_profile_partition_assay("noise")
  noise_root <- builder_profile_assay(
    noise_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_true(noise_root$exportable)
  expect_identical(noise_root$partition_status, "unique")
  expect_setequal(noise_root$members, c("data.one", "data.two"))
  expect_false("data.noise" %in% noise_root$members)
  expect_false("dataBackup" %in% noise_root$candidate_members)

  subset_assay <- builder_profile_partition_assay("same_subset")
  subset_root <- builder_profile_assay(
    subset_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_true(subset_root$exportable)
  expect_identical(subset_root$features$relation, "subset")
  expect_identical(subset_root$features$missing, "G5")
  expect_false(subset_root$heterogeneous_features)

  heterogeneous_assay <- builder_profile_partition_assay("heterogeneous")
  heterogeneous_root <- builder_profile_assay(
    heterogeneous_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_false(heterogeneous_root$exportable)
  expect_true(heterogeneous_root$features$valid)
  expect_true(heterogeneous_root$heterogeneous_features)
  expect_equal(heterogeneous_root$features$coverage, 1)
  expect_true(
    "incompatible_split_feature_sets" %in%
      heterogeneous_root$diagnostics
  )
  expect_false(
    "data" %in%
      builder_layer_choices(
        heterogeneous_assay,
        expected_cells = expected
      )
  )

  nested_assay <- builder_profile_partition_assay("nested_only")
  nested_root <- builder_profile_assay(
    nested_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_false(nested_root$exportable)
  expect_identical(nested_root$candidate_members, character())
  expect_setequal(
    nested_root$nested_candidates,
    c("data.imputed.one", "data.imputed.two")
  )
  expect_true("nested_candidates_deferred" %in% nested_root$diagnostics)

  direct_nested_assay <- builder_profile_partition_assay("direct_and_nested")
  direct_nested_root <- builder_profile_assay(
    direct_nested_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_true(direct_nested_root$exportable)
  expect_setequal(
    direct_nested_root$members,
    c("data.one", "data.two")
  )
  expect_setequal(
    direct_nested_root$nested_candidates,
    c("data.imputed.one", "data.imputed.two")
  )

  ambiguous_assay <- builder_profile_partition_assay("ambiguous")
  ambiguous_root <- builder_profile_assay(
    ambiguous_assay,
    expected,
    "SPLIT"
  )$layers$data
  expect_false(ambiguous_root$exportable)
  expect_identical(ambiguous_root$partition_status, "ambiguous")
  expect_length(ambiguous_root$solutions, 2L)
  expect_true("ambiguous_partition" %in% ambiguous_root$diagnostics)

  ambiguous_memberships <- lapply(
    c("data.a", "data.b", "data.c", "data.d"),
    function(layer) SeuratObject::Cells(ambiguous_assay, layer = layer)
  )
  names(ambiguous_memberships) <- c("data.a", "data.b", "data.c", "data.d")
  budgeted <- .builder_profile_find_partition(
    expected,
    ambiguous_memberships,
    max_nodes = 1L
  )
  expect_identical(budgeted$status, "budget_exceeded")
  expect_identical(budgeted$layers, character())

  large_cells <- paste0("cell", seq_len(1200L))
  large_memberships <- as.list(large_cells)
  names(large_memberships) <- paste0("counts.", seq_along(large_cells))
  large_partition <- .builder_profile_find_partition(
    large_cells,
    large_memberships
  )
  expect_identical(large_partition$status, "unique")
  expect_identical(large_partition$strategy, "linear")
  expect_length(large_partition$layers, length(large_cells))

  noisy_cells <- paste0("cell", seq_len(320L))
  noisy_memberships <- as.list(noisy_cells)
  names(noisy_memberships) <- paste0(
    "counts.",
    seq_along(noisy_cells)
  )
  noisy_memberships[["counts.noise"]] <- noisy_cells[[1L]]
  noisy_partition <- .builder_profile_find_partition(
    noisy_cells,
    noisy_memberships
  )
  expect_identical(noisy_partition$status, "budget_exceeded")
  expect_identical(noisy_partition$strategy, "search")
  expect_identical(noisy_partition$layers, character())

  repeated_memberships <- rep(list("c1"), 1000L)
  names(repeated_memberships) <- sprintf(
    "counts.c1.%04d",
    seq_along(repeated_memberships)
  )
  repeated_memberships[["counts.c2"]] <- "c2"
  repeated_budget <- .builder_profile_find_partition(
    c("c1", "c2"),
    repeated_memberships,
    max_nodes = 1L
  )
  expect_identical(repeated_budget$status, "budget_exceeded")
  expect_identical(repeated_budget$strategy, "search")
  expect_lte(repeated_budget$nodes, 2L)

  dense_cells <- paste0("cell", seq_len(100L))
  dense_memberships <- rep(list(dense_cells[seq_len(50L)]), 320L)
  names(dense_memberships) <- sprintf(
    "data.dense.%03d",
    seq_along(dense_memberships)
  )
  dense_budget <- .builder_profile_find_partition(
    dense_cells,
    dense_memberships,
    max_conflict_work = 5000000
  )
  expect_identical(dense_budget$status, "budget_exceeded")
  expect_identical(dense_budget$budget_reason, "conflict_work")
  expect_identical(dense_budget$conflict_work, 5120000)
  expect_identical(dense_budget$nodes, 0L)
})

test_that("reduction identity is exact and shuffled rows remain reorderable", {
  skip_if_not_installed("SeuratObject")
  shuffled_fixture <- builder_profile_embeddings_fixture("shuffled")
  shuffled <- builder_profile_embeddings(
    shuffled_fixture$embeddings,
    shuffled_fixture$expected,
    "umap"
  )
  expect_true(shuffled$exportable)
  expect_true(shuffled$cells$valid)
  expect_false(shuffled$cells$order_matches)
  expect_identical(
    shuffled$cells$reorder_index,
    match(shuffled_fixture$expected, rownames(shuffled_fixture$embeddings))
  )

  failures <- c(
    missing = "missing",
    extra = "extra",
    wrong_same_count = "extra",
    duplicate = "duplicates",
    blank = "blanks"
  )
  for (mode in names(failures)) {
    fixture <- builder_profile_embeddings_fixture(mode)
    reduction <- builder_profile_embeddings(
      fixture$embeddings,
      fixture$expected,
      "umap"
    )
    expect_false(reduction$exportable, info = mode)
    expect_true(
      length(reduction$cells[[failures[[mode]]]]) > 0L,
      info = mode
    )
  }
})

test_that("reductions must be numeric and at least two dimensional", {
  non_numeric_fixture <- builder_profile_embeddings_fixture("non_numeric")
  non_numeric <- builder_profile_embeddings(
    non_numeric_fixture$embeddings,
    non_numeric_fixture$expected,
    "umap"
  )
  expect_false(non_numeric$numeric)
  expect_false(non_numeric$exportable)
  expect_true("non_numeric" %in% non_numeric$diagnostics)

  one_dimensional_fixture <- builder_profile_embeddings_fixture(
    "one_dimension"
  )
  one_dimensional <- builder_profile_embeddings(
    one_dimensional_fixture$embeddings,
    one_dimensional_fixture$expected,
    "umap"
  )
  expect_identical(one_dimensional$dimensions, 1L)
  expect_false(one_dimensional$exportable)
  expect_true("fewer_than_two_dimensions" %in% one_dimensional$diagnostics)

  for (mode in c("na", "nan", "inf")) {
    fixture <- builder_profile_embeddings_fixture(mode)
    reduction <- builder_profile_embeddings(
      fixture$embeddings,
      fixture$expected,
      "umap"
    )
    expect_false(reduction$finite, info = mode)
    expect_false(reduction$exportable, info = mode)
    expect_true("non_finite" %in% reduction$diagnostics, info = mode)
  }
})

test_that("PCA remains a stable fallback fact rather than a frozen selection", {
  skip_if_not_installed("SeuratObject")
  pca_only <- builder_dataset_profile(
    builder_profile_reduction_object("pca"),
    builder_profile_source_fixture()
  )

  expect_true(pca_only$reductions$pca$structurally_valid)
  expect_true(pca_only$reductions$pca$exportable)
  expect_true(pca_only$reductions$pca$is_pca)
  expect_identical(
    pca_only$reductions$pca$selection_role,
    "fallback_only"
  )
  expect_identical(
    pca_only$manifest[["reduction:pca"]]$disposition,
    "preserved"
  )
  expect_identical(pca_only$manifest[["projection"]]$status, "valid")
  expect_identical(
    builder_manifest_readiness(pca_only$manifest)$state,
    "ready"
  )

  mixed <- builder_dataset_profile(
    builder_profile_reduction_object(c("pca", "umap")),
    builder_profile_source_fixture()
  )
  expect_identical(mixed$reductions$pca$selection_role, "fallback_only")
  expect_identical(mixed$reductions$umap$selection_role, "normal")
  expect_true(mixed$reductions$pca$exportable)
  expect_true(mixed$reductions$umap$exportable)
  expect_false(
    "filtered" %in%
      vapply(
        mixed$manifest,
        function(entry) entry$disposition,
        character(1)
      )
  )

  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  expect_false(any(grepl(
    "PCA-named reductions are omitted",
    app,
    fixed = TRUE
  )))
})

test_that("Viewer projection catalog includes every exportable 2-D reduction", {
  skip_if_not_installed("SeuratObject")
  object <- builder_profile_reduction_object(c("pca", "umap", "tsne"))
  profile <- builder_dataset_profile(
    object,
    builder_profile_source_fixture()
  )
  catalog <- profile$viewer_content$projections

  expect_named(catalog, c("pca", "umap", "tsne"), ignore.order = FALSE)
  expect_true(all(vapply(catalog, `[[`, logical(1), "available")))
  expect_identical(catalog$pca$kind, "pca")
  expect_identical(catalog$umap$kind, "umap")
  expect_identical(catalog$tsne$kind, "tsne")
  expect_true(catalog$pca$is_pca)
  expect_identical(catalog$pca$name, "pca")
  expect_identical(catalog$umap$name, "umap")
})

test_that("empty feature identities yield a finite organism inference", {
  inference <- builder_profile_organism(character())

  expect_identical(inference$code, "other")
  expect_identical(inference$confidence, 0)
  expect_true(is.finite(inference$confidence))
  expect_match(inference$reason, "No feature")
})

test_that("core manifest blocks unsafe structure and preserves valid content", {
  skip_if_not_installed("SeuratObject")
  valid <- builder_dataset_profile(
    builder_profile_pbmc(),
    builder_profile_source_fixture()
  )
  expect_identical(valid$manifest[["dataset_identity"]]$status, "valid")
  expect_identical(valid$manifest[["expression"]]$status, "valid")
  expect_identical(valid$manifest[["metadata"]]$status, "valid")
  expect_identical(valid$manifest[["groups"]]$status, "valid")
  expect_identical(valid$manifest[["projection"]]$status, "valid")
  expect_identical(
    valid$manifest[["reduction:pca"]]$disposition,
    "preserved"
  )
  expect_identical(
    valid$manifest[["reduction:tsne"]]$disposition,
    "preserved"
  )
  expect_identical(builder_manifest_readiness(valid$manifest)$state, "ready")

  broken_identity <- valid$identity
  expected_cells <- broken_identity$cells$ids
  broken_identity$cells <- builder_identity_profile(
    c(expected_cells[-length(expected_cells)], "ghost"),
    expected_cells
  )
  identity_manifest <- builder_profile_core_manifest(
    valid$source,
    broken_identity,
    valid$assays,
    builder_profile_metadata(
      builder_profile_pbmc()@meta.data,
      SeuratObject::Cells(builder_profile_pbmc())
    ),
    valid$reductions
  )
  expect_identical(
    identity_manifest[["dataset_identity"]]$status,
    "blocking"
  )
  expect_identical(identity_manifest[["expression"]]$status, "blocking")

  cells <- SeuratObject::Cells(builder_profile_pbmc())
  broken_metadata <- builder_profile_metadata(
    builder_profile_pbmc()@meta.data[-1L, , drop = FALSE],
    cells
  )
  broken_manifest <- builder_profile_core_manifest(
    valid$source,
    valid$identity,
    valid$assays,
    broken_metadata,
    valid$reductions
  )
  expect_identical(broken_manifest[["metadata"]]$status, "blocking")
  expect_identical(broken_manifest[["metadata"]]$disposition, "rejected")
  expect_identical(
    builder_manifest_readiness(broken_manifest)$state,
    "blocked"
  )
})

test_that("an unselected bad reduction does not block a valid projection", {
  skip_if_not_installed("SeuratObject")
  base <- builder_dataset_profile(
    builder_profile_pbmc(),
    builder_profile_source_fixture()
  )
  fixture <- builder_profile_embeddings_fixture("valid")
  good <- builder_profile_embeddings(
    fixture$embeddings,
    fixture$expected,
    "umap"
  )
  bad_fixture <- builder_profile_embeddings_fixture("non_numeric")
  bad <- builder_profile_embeddings(
    bad_fixture$embeddings,
    bad_fixture$expected,
    "broken"
  )

  mixed <- builder_profile_core_manifest(
    base$source,
    base$identity,
    base$assays,
    builder_profile_metadata(
      builder_profile_pbmc()@meta.data,
      SeuratObject::Cells(builder_profile_pbmc())
    ),
    list(umap = good, broken = bad)
  )
  expect_identical(mixed[["reduction:broken"]]$status, "valid")
  expect_identical(mixed[["reduction:broken"]]$disposition, "rejected")
  expect_identical(mixed[["projection"]]$status, "valid")
  expect_identical(builder_manifest_readiness(mixed)$state, "ready")

  only_bad <- builder_profile_core_manifest(
    base$source,
    base$identity,
    base$assays,
    builder_profile_metadata(
      builder_profile_pbmc()@meta.data,
      SeuratObject::Cells(builder_profile_pbmc())
    ),
    list(broken = bad)
  )
  expect_identical(only_bad[["reduction:broken"]]$status, "valid")
  expect_identical(only_bad[["projection"]]$status, "blocking")
  expect_identical(builder_manifest_readiness(only_bad)$state, "blocked")
})

test_that("legacy Assay slots remain profileable without the v5 layer API", {
  skip_if_not_installed("SeuratObject")
  builder_repo_source("inspect.R")
  assay <- builder_profile_legacy_assay()
  expected_cells <- colnames(assay)
  expected_features <- rownames(assay)
  profile <- builder_profile_assay(
    assay,
    expected_cells,
    "RNA",
    expected_features = expected_features,
    layer_api = NULL
  )

  expect_true(methods::validObject(assay, test = TRUE, complete = TRUE))
  expect_true(all(c("counts", "data") %in% names(profile$layers)))
  expect_true(profile$layers$counts$exportable)
  expect_true(profile$layers$data$exportable)
  expect_identical(profile$layers$counts$cells$ids, expected_cells)
  expect_identical(profile$layers$counts$features$ids, expected_features)

  choices <- builder_layer_choices(
    assay,
    expected_cells = expected_cells,
    layer_api = NULL
  )
  expect_true(all(c("counts", "data") %in% choices))

  object <- builder_profile_pbmc()
  prepared <- builder_prepare_export_layer(
    object,
    "RNA",
    "counts",
    layer_api = NULL
  )
  expect_s4_class(prepared, "Seurat")
  expect_identical(SeuratObject::Cells(prepared), SeuratObject::Cells(object))
})

test_that("feature subsets are diagnosed without rejecting valid layers", {
  skip_if_not_installed("SeuratObject")
  cells <- paste0("cell", seq_len(6L))
  features <- paste0("G", seq_len(5L))
  assay <- SeuratObject::CreateAssay5Object(
    counts = builder_profile_matrix(cells, features)
  )
  partial <- SeuratObject::LayerData(assay, layer = "counts")[
    -1L,
    ,
    drop = FALSE
  ]
  SeuratObject::LayerData(assay, layer = "data") <- partial

  profile <- builder_profile_assay(
    assay,
    cells,
    "RNA",
    expected_features = features
  )
  expect_true(profile$layers$counts$features$valid)
  expect_true(profile$layers$counts$exportable)
  expect_false(profile$layers$data$features$valid)
  expect_identical(profile$layers$data$features$missing, "G1")
  expect_true(profile$layers$data$features$usable)
  expect_identical(profile$layers$data$features$relation, "subset")
  expect_true(profile$layers$data$exportable)

  pbmc <- builder_dataset_profile(
    builder_profile_pbmc(),
    builder_profile_source_fixture()
  )
  expect_identical(pbmc$assays$RNA$layers$scale.data$features$count, 20L)
  expect_identical(
    pbmc$assays$RNA$layers$scale.data$features$relation,
    "subset"
  )
  expect_true(pbmc$assays$RNA$layers$scale.data$exportable)
})

test_that("feature identities reject ambiguous or outside gene names", {
  expected <- c("GeneA", "GeneB", "GeneC")

  reordered <- builder_feature_profile(rev(expected), expected)
  expect_identical(reordered$relation, "complete")
  expect_false(reordered$order_matches)
  expect_identical(reordered$reorder_index, c(3L, 2L, 1L))

  case_collision <- builder_feature_profile(
    c("GeneA", "genea"),
    expected
  )
  expect_identical(case_collision$relation, "invalid")
  expect_identical(
    case_collision$casefold_duplicates,
    c("GeneA", "genea")
  )

  outside <- builder_feature_profile(c("GeneA", "Other"), expected)
  expect_identical(outside$relation, "invalid")
  expect_identical(outside$extra, "Other")
})

test_that("each assay uses its own feature universe", {
  skip_if_not_installed("SeuratObject")
  cells <- paste0("cell", seq_len(6L))
  object <- SeuratObject::CreateSeuratObject(
    builder_profile_matrix(cells, paste0("RNA", seq_len(5L)))
  )
  object[["ADT"]] <- SeuratObject::CreateAssay5Object(
    counts = builder_profile_matrix(cells, paste0("PROTEIN", seq_len(3L)))
  )
  profiles <- builder_profile_assays(object, SeuratObject::Cells(object))

  expect_identical(
    profiles$RNA$layers$counts$features$canonical_ids,
    paste0("RNA", seq_len(5L))
  )
  expect_identical(
    profiles$ADT$layers$counts$features$canonical_ids,
    paste0("PROTEIN", seq_len(3L))
  )
  expect_identical(profiles$ADT$layers$counts$features$relation, "complete")
  expect_true(profiles$ADT$layers$counts$exportable)
})

test_that("preparing a logical layer joins only its exact-cover members", {
  skip_if_not_installed("SeuratObject")
  builder_repo_source("inspect.R")
  assay <- builder_profile_partition_assay("noise")
  cells <- paste0("cell", seq_len(6L))
  object <- SeuratObject::CreateSeuratObject(builder_profile_matrix(cells))
  object <- suppressWarnings({
    object[["SPLIT"]] <- assay
    object
  })

  prepared <- builder_prepare_export_layer(object, "SPLIT", "data")
  joined <- SeuratObject::LayerData(prepared[["SPLIT"]], layer = "data")

  expect_identical(colnames(joined), cells)
  expect_identical(rownames(joined), paste0("G", seq_len(5L)))
  expect_false("dataBackup" %in% SeuratObject::Layers(prepared[["SPLIT"]]))
  expect_false("data.noise" %in% SeuratObject::Layers(prepared[["SPLIT"]]))

  heterogeneous <- builder_profile_partition_assay("heterogeneous")
  blocked <- SeuratObject::CreateSeuratObject(builder_profile_matrix(cells))
  blocked <- suppressWarnings({
    blocked[["SPLIT"]] <- heterogeneous
    blocked
  })
  expect_error(
    builder_prepare_export_layer(blocked, "SPLIT", "data"),
    "is not available"
  )

  subset <- builder_profile_partition_assay("same_subset")
  subset_object <- SeuratObject::CreateSeuratObject(
    builder_profile_matrix(cells)
  )
  subset_object <- suppressWarnings({
    subset_object[["SPLIT"]] <- subset
    subset_object
  })
  subset_prepared <- builder_prepare_export_layer(
    subset_object,
    "SPLIT",
    "data"
  )
  subset_joined <- SeuratObject::LayerData(
    subset_prepared[["SPLIT"]],
    layer = "data"
  )
  expect_identical(colnames(subset_joined), cells)
  expect_identical(rownames(subset_joined), paste0("G", seq_len(4L)))

  direct_nested <- builder_profile_partition_assay("direct_and_nested")
  direct_nested_object <- SeuratObject::CreateSeuratObject(
    builder_profile_matrix(cells)
  )
  direct_nested_object <- suppressWarnings({
    direct_nested_object[["SPLIT"]] <- direct_nested
    direct_nested_object
  })
  direct_nested_prepared <- builder_prepare_export_layer(
    direct_nested_object,
    "SPLIT",
    "data"
  )
  expect_identical(
    SeuratObject::Layers(direct_nested_prepared[["SPLIT"]]),
    "data"
  )

  valid_matrix <- builder_profile_matrix(cells)
  expect_error(
    builder_validate_joined_layer(
      valid_matrix[, c(cells[1:5], cells[5]), drop = FALSE],
      cells,
      rownames(valid_matrix)
    ),
    "cell identity"
  )
  expect_error(
    builder_validate_joined_layer(
      valid_matrix[-1L, , drop = FALSE],
      cells,
      rownames(valid_matrix)
    ),
    "feature identity"
  )
})

test_that("legacy describe_seurat fields remain available", {
  skip_if_not_installed("SeuratObject")
  builder_repo_source("inspect.R")
  object <- builder_profile_pbmc()
  described <- describe_seurat(object)

  expect_named(
    described,
    c(
      "n_cells",
      "n_genes",
      "assays",
      "default_assay",
      "assay_profiles",
      "layers",
      "default_layer",
      "group_candidates",
      "group_preselect",
      "group_counts",
      "group_struck",
      "reductions",
      "reduction_preselect",
      "images",
      "spatial_scenes",
      "nUMI",
      "nGene",
      "qc_values",
      "organism_guess",
      "extras",
      "suggested_dir"
    ),
    ignore.order = FALSE
  )
  expect_identical(described$n_cells, ncol(object))
  expect_identical(described$n_genes, nrow(object))
  expect_identical(described$reductions, c("pca", "tsne"))
  expect_identical(described$reduction_preselect, "tsne")

  pca_only <- describe_seurat(builder_profile_reduction_object("pca"))
  expect_identical(pca_only$reduction_preselect, "pca")

  mixed <- describe_seurat(
    builder_profile_reduction_object(c("pca", "umap"))
  )
  expect_identical(mixed$reduction_preselect, "umap")
})

test_that("describe_seurat carries bounded spatial scene labels", {
  skip_if_not_installed("SeuratObject")
  builder_repo_source("inspect.R")
  object <- builder_content_spatial_example_object("xenium-fov")
  object$sample_roi <- rep(c("S1_lesion", "S2_border"), ncol(object) / 2L)

  described <- describe_seurat(object)

  expect_length(described$spatial_scenes, 1L)
  expect_identical(described$spatial_scenes[[1L]]$id, "xenium-fov")
  expect_match(described$spatial_scenes[[1L]]$label, "2 samples", fixed = TRUE)
  expect_match(described$spatial_scenes[[1L]]$label, "2 ROIs", fixed = TRUE)
})

test_that("profile bootstraps from the installed application layout", {
  skip_if_not_installed("SeuratObject")
  isolated <- new.env(parent = baseenv())
  files <- builder_profile_source_runtime(isolated)

  expect_true(all(nzchar(files)))
  expect_true(all(file.exists(files)))
  expect_true(exists("builder_dataset_profile", isolated, inherits = FALSE))
  profile <- isolated$builder_dataset_profile(
    builder_profile_pbmc(),
    builder_profile_source_fixture()
  )
  expect_s3_class(profile, "builder_dataset_profile")
  expect_s3_class(profile$manifest, "builder_content_manifest")
})

test_that("application and worker source the profile after its dependencies", {
  app <- readLines(
    builder_profile_inst_path("builder", "app.R"),
    warn = FALSE
  )
  session <- readLines(
    builder_profile_inst_path("builder", "worker.R"),
    warn = FALSE
  )

  app_contract <- grep("viewer_content_contract[.]R", app, fixed = FALSE)[1L]
  app_manifest <- grep('source\\("manifest[.]R"', app)[1L]
  app_profile <- grep('source\\("profile[.]R"', app)[1L]
  expect_true(app_contract < app_manifest)
  expect_true(app_manifest < app_profile)

  worker_contract <- grep("viewer_content_contract[.]R", session)[1L]
  worker_manifest <- grep(
    'source_utf8\\(file[.]path\\(dir, "manifest[.]R"',
    session
  )[1L]
  worker_profile <- grep(
    'source_utf8\\(file[.]path\\(dir, "profile[.]R"',
    session
  )[
    1L
  ]
  expect_true(worker_contract < worker_manifest)
  expect_true(worker_manifest < worker_profile)
})
