builder_content_immune_source_runtime()

test_that("absent immune and HLA content is valid optional content", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()

  content <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )

  builder_immune_expect_record_contract(content$immune_repertoire)
  builder_immune_expect_record_contract(content$hla)
  expect_false(content$immune_repertoire$detected)
  expect_true(content$immune_repertoire$valid)
  expect_null(content$immune_repertoire$normalized)
  expect_identical(content$immune_repertoire$page_candidates, character())
  expect_identical(content$immune_repertoire$diagnostics, character())
  expect_false(content$hla$detected)
  expect_true(content$hla$valid)
  expect_null(content$hla$normalized)
  expect_identical(content$hla$page_candidates, character())
})

test_that("IR and motif readiness follow separate Viewer contracts", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)

  motif_only <- data.frame(
    barcode = cells[[1L]],
    CTgene = "TRBV1.TRBJ1",
    CTaa = "CASSLGQ",
    stringsAsFactors = FALSE
  )
  object@misc$immune_repertoire <- list(sample_a = motif_only)
  motif_content <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$immune_repertoire
  motif_candidate <- motif_content$candidates$unified_misc
  viewer_segments <- hla_parse_ir_segments(
    object@misc$immune_repertoire,
    "TRB"
  )

  expect_false(motif_candidate$full_ir_ready)
  expect_false(motif_candidate$valid)
  expect_true(motif_candidate$hla_tcr_ready)
  expect_identical(motif_candidate$parseable_tcr_chains, "TRB")
  expect_identical(
    motif_candidate$parseable_tcr_row_count,
    as.integer(nrow(viewer_segments))
  )
  expect_identical(motif_candidate$page_candidates, "hla_tcr_motifs")
  expect_identical(motif_content$page_candidates, "hla_tcr_motifs")

  complete_but_unusable <- builder_immune_fixture_table(cells[[1L]], "TRB")
  complete_but_unusable$CTaa <- NA_character_
  object@misc$immune_repertoire <- list(sample_a = complete_but_unusable)
  unusable_content <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$immune_repertoire
  unusable_candidate <- unusable_content$candidates$unified_misc

  expect_null(hla_parse_ir_segments(object@misc$immune_repertoire, "TRB"))
  expect_false(unusable_candidate$full_ir_ready)
  expect_false(unusable_candidate$hla_tcr_ready)
  expect_identical(unusable_candidate$parseable_tcr_chains, character())
  expect_identical(unusable_candidate$parseable_tcr_row_count, 0L)
  expect_identical(unusable_candidate$page_candidates, character())
  expect_identical(unusable_content$page_candidates, character())
})

test_that("unified, metadata, and legacy repertoire remain separate facts", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  object@misc$immune_repertoire <- list(
    unified = builder_immune_fixture_table(cells[1:2], "TRB", "unified")
  )
  object@misc$bcr_data <- list(
    legacy_b = builder_immune_fixture_table(cells[3:4], "IGH", "legacy_b")
  )
  object@misc$tcr_data <- list(
    legacy_t = builder_immune_fixture_table(cells[5:6], "TRA", "legacy_t")
  )
  for (column in c("CTgene", "CTnt", "CTaa", "CTstrict")) {
    object@meta.data[[column]] <- NA_character_
  }
  metadata_ir <- builder_immune_fixture_table(cells[7:8], "TRB", "metadata")
  object@meta.data[cells[7:8], c("CTgene", "CTnt", "CTaa", "CTstrict")] <-
    metadata_ir[, c("CTgene", "CTnt", "CTaa", "CTstrict")]
  context <- builder_immune_fixture_context(object)
  context$metadata <- object@meta.data

  content <- builder_profile_immune_content(object, context)
  ir <- content$immune_repertoire

  expect_true(ir$detected)
  expect_true(ir$valid)
  expect_named(
    ir$candidates,
    c("unified_misc", "metadata", "legacy_bcr", "legacy_tcr")
  )
  expect_true(all(vapply(ir$candidates, `[[`, logical(1), "detected")))
  expect_true(all(vapply(ir$candidates, `[[`, logical(1), "valid")))
  expect_identical(ir$selected_source, NULL)
  expect_setequal(ir$normalized$available_sources, names(ir$candidates))
  expect_setequal(ir$normalized$chains, c("TRA", "TRB", "IGH"))
  expect_setequal(
    ir$page_candidates,
    c("immune_repertoire", "hla_tcr_motifs")
  )
  lapply(ir$candidates, builder_immune_expect_record_contract)
})

test_that("repertoire validation records samples, chains, and cell identity", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  object@misc$immune_repertoire <- list(
    sample_a = builder_immune_fixture_table(cells[1:3], "TRB"),
    sample_b = builder_immune_fixture_table(cells[4:5], "IGH")
  )

  ir <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$immune_repertoire
  unified <- ir$candidates$unified_misc

  expect_true(unified$valid)
  expect_identical(unified$normalized$n_samples, 2L)
  expect_identical(unified$normalized$n_rows, 5L)
  expect_identical(unified$normalized$barcode_count, 5L)
  expect_identical(unified$normalized$dataset_overlap_count, 5L)
  expect_equal(unified$normalized$dataset_overlap_fraction, 1)
  expect_identical(unified$normalized$sample_names, c("sample_a", "sample_b"))
  expect_setequal(unified$normalized$chains, c("TRB", "IGH"))
  expect_setequal(unified$normalized$receptor_types, c("TCR", "BCR"))
  expect_identical(unified$diagnostics, character())
})

test_that("malformed repertoire candidates fail closed with precise reasons", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  cases <- list(
    duplicate_samples = {
      value <- list(
        builder_immune_fixture_table(cells[1], "TRB"),
        builder_immune_fixture_table(cells[2], "TRB")
      )
      names(value) <- c("same", "same")
      value
    },
    blank_sample = {
      value <- list(builder_immune_fixture_table(cells[1], "TRB"))
      names(value) <- ""
      value
    },
    missing_column = list(
      sample_a = builder_immune_fixture_table(cells[1], "TRB")[, -2]
    ),
    duplicate_barcode = list(
      sample_a = builder_immune_fixture_table(c(cells[1], cells[1]), "TRB")
    ),
    outside_dataset = list(
      sample_a = builder_immune_fixture_table("ghost", "TRB")
    ),
    unknown_chain = list(
      sample_a = builder_immune_fixture_table(cells[1], "XYZ")
    )
  )
  reasons <- c(
    duplicate_samples = "duplicate_sample_names",
    blank_sample = "blank_sample_names",
    missing_column = "missing_required_columns",
    duplicate_barcode = "duplicate_barcodes",
    outside_dataset = "barcodes_outside_dataset",
    unknown_chain = "unrecognized_chain"
  )

  for (name in names(cases)) {
    candidate_object <- object
    candidate_object@misc$immune_repertoire <- cases[[name]]
    got <- builder_profile_immune_content(
      candidate_object,
      builder_immune_fixture_context(candidate_object)
    )$immune_repertoire$candidates$unified_misc
    expect_true(got$detected, info = name)
    expect_false(got$valid, info = name)
    expect_true(reasons[[name]] %in% got$diagnostics, info = name)
  }
})

test_that("repertoire profiling follows upstream row filtering semantics", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  empty_sample <- builder_immune_fixture_table(cells[[1L]], "TRB")[0, ]
  object@misc$immune_repertoire <- list(
    empty_sample = empty_sample,
    sample_a = builder_immune_fixture_table(
      c(cells[[1L]], "outside-cell"),
      "TRB",
      "partial"
    )
  )

  candidate <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$immune_repertoire$candidates$unified_misc

  expect_true(candidate$valid)
  expect_true(candidate$full_ir_ready)
  expect_true(candidate$attention)
  expect_contains(candidate$diagnostics, "empty_sample_table")
  expect_contains(candidate$diagnostics, "barcodes_outside_dataset")
  expect_identical(candidate$normalized$dataset_overlap_count, 1L)
  expect_identical(candidate$normalized$outside_barcode_count, 1L)

  expect_warning(
    normalized <- CerebroNexus::addImmuneRepertoire(object, verbose = FALSE),
    "removed 1 repertoire row"
  )
  expect_identical(names(normalized@misc$immune_repertoire), "sample_a")
  expect_identical(
    normalized@misc$immune_repertoire$sample_a$barcode,
    cells[[1L]]
  )
})

test_that("serialized custom classes never dispatch during content profiling", {
  skip_if_not_installed("SeuratObject")
  touched <- character()
  `[[.builder_evil_misc` <- function(x, ...) {
    touched <<- c(touched, "misc-bracket")
    NextMethod()
  }
  names.builder_evil_misc <- function(x) {
    touched <<- c(touched, "misc-names")
    NextMethod()
  }
  `[[.builder_evil_table` <- function(x, ...) {
    touched <<- c(touched, "table-bracket")
    NextMethod()
  }
  dim.builder_evil_table <- function(x) {
    touched <<- c(touched, "table-dim")
    NextMethod()
  }
  as.data.frame.builder_evil_table <- function(x, ...) {
    touched <<- c(touched, "table-data-frame")
    NextMethod()
  }
  `[[.builder_evil_hla_list` <- function(x, ...) {
    touched <<- c(touched, "hla-list-bracket")
    NextMethod()
  }
  names.builder_evil_hla_list <- function(x) {
    touched <<- c(touched, "hla-list-names")
    NextMethod()
  }
  length.builder_evil_hla_list <- function(x) {
    touched <<- c(touched, "hla-list-length")
    NextMethod()
  }
  `[[.builder_evil_hla_frame` <- function(x, ...) {
    touched <<- c(touched, "hla-frame-bracket")
    NextMethod()
  }
  dim.builder_evil_hla_frame <- function(x) {
    touched <<- c(touched, "hla-frame-dim")
    NextMethod()
  }
  as.character.builder_evil_hla_column <- function(x, ...) {
    touched <<- c(touched, "hla-column-character")
    NextMethod()
  }

  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  object@misc$immune_repertoire <- list(
    sample_a = builder_immune_fixture_table(cells[1], "TRB")
  )
  unsafe_misc <- object@misc
  class(unsafe_misc) <- "builder_evil_misc"
  invisible(unsafe_misc[["immune_repertoire"]])
  expect_identical(touched, "misc-bracket")
  touched <- character()
  methods::slot(object, "misc", check = FALSE) <- unsafe_misc
  outer <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )
  expect_true(outer$immune_repertoire$candidates$unified_misc$valid)
  expect_identical(touched, character())

  object <- builder_immune_fixture_object()
  unsafe_table <- builder_immune_fixture_table(cells[1], "TRB")
  class(unsafe_table) <- c("builder_evil_table", "data.frame")
  invisible(dim(unsafe_table))
  expect_identical(touched, "table-dim")
  touched <- character()
  object@misc$immune_repertoire <- list(sample_a = unsafe_table)
  table_profile <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$immune_repertoire$candidates$unified_misc
  expect_false(table_profile$valid)
  expect_contains(table_profile$diagnostics, "unsafe_sample_table_class")
  expect_identical(touched, character())

  object <- builder_immune_fixture_object()
  hla_list <- list(sample_a = "HLA-A*02:01")
  class(hla_list) <- "builder_evil_hla_list"
  invisible(length(hla_list))
  expect_identical(touched, "hla-list-length")
  touched <- character()
  invisible(hla_list[[1L]])
  expect_identical(touched, "hla-list-bracket")
  touched <- character()
  object@misc$hla_typing <- hla_list
  hla_list_profile <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_false(hla_list_profile$valid)
  expect_contains(hla_list_profile$diagnostics, "unsafe_container_class")
  expect_identical(touched, character())

  object <- builder_immune_fixture_object()
  hla_frame <- data.frame(
    sample = "sample_a",
    allele = "HLA-A*02:01",
    stringsAsFactors = FALSE
  )
  class(hla_frame) <- c("builder_evil_hla_frame", "data.frame")
  invisible(dim(hla_frame))
  expect_identical(touched, "hla-frame-dim")
  touched <- character()
  object@misc$hla_typing <- hla_frame
  hla_frame_profile <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_false(hla_frame_profile$valid)
  expect_contains(hla_frame_profile$diagnostics, "unsafe_container_class")
  expect_identical(touched, character())

  object <- builder_immune_fixture_object()
  hla_column <- c("HLA-A*02:01")
  class(hla_column) <- "builder_evil_hla_column"
  invisible(as.character(hla_column))
  expect_identical(touched, "hla-column-character")
  touched <- character()
  object@misc$hla_typing <- structure(
    list(sample = "sample_a", allele = hla_column),
    class = "data.frame",
    row.names = 1L
  )
  hla_column_profile <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_false(hla_column_profile$valid)
  expect_contains(hla_column_profile$diagnostics, "unsafe_column_class")
  expect_identical(touched, character())
})

test_that("attribute-level classes fail closed without S3 dispatch", {
  skip_if_not_installed("SeuratObject")
  touched <- character()
  length.builder_attr_bomb <- function(x) {
    touched <<- c(touched, "length")
    NextMethod()
  }
  as.character.builder_attr_bomb <- function(x, ...) {
    touched <<- c(touched, "as.character")
    NextMethod()
  }
  anyNA.builder_attr_bomb <- function(x, recursive = FALSE) {
    touched <<- c(touched, "anyNA")
    NextMethod()
  }
  `[.builder_attr_bomb` <- function(x, ...) {
    touched <<- c(touched, "bracket")
    NextMethod()
  }

  expect_inert <- function(payload, expected_diagnostic) {
    object <- builder_immune_fixture_object()
    methods::slot(object, "misc")$immune_repertoire <- payload
    got <- builder_profile_immune_content(
      object,
      builder_immune_fixture_context(object)
    )$immune_repertoire$candidates$unified_misc
    expect_false(got$valid)
    expect_contains(got$diagnostics, expected_diagnostic)
    expect_identical(touched, character())
  }

  table <- builder_immune_fixture_table("cell1", "TRB")
  payload <- list(table)
  attr(payload, "names") <- structure(
    "sample_a",
    class = "builder_attr_bomb"
  )
  expect_inert(payload, "unsafe_sample_names")

  table <- builder_immune_fixture_table("cell1", "TRB")
  attr(table, "names") <- structure(
    names(table),
    class = "builder_attr_bomb"
  )
  expect_inert(list(sample_a = table), "unsafe_column_names")

  table <- builder_immune_fixture_table("cell1", "TRB")
  table[["barcode"]] <- structure(
    table[["barcode"]],
    class = "builder_attr_bomb"
  )
  expect_inert(list(sample_a = table), "unsafe_column_class")

  table <- builder_immune_fixture_table("cell1", "TRB")
  gene <- factor(table[["CTgene"]])
  attr(gene, "levels") <- structure(
    attr(gene, "levels", exact = TRUE),
    class = "builder_attr_bomb"
  )
  table[["CTgene"]] <- gene
  expect_inert(list(sample_a = table), "unsafe_factor_levels")

  object <- builder_immune_fixture_object()
  metadata <- object@meta.data
  for (column in c("CTgene", "CTnt", "CTaa", "CTstrict")) {
    metadata[[column]] <- "TRBV1.TRBJ1"
  }
  attr(metadata, "row.names") <- structure(
    attr(metadata, "row.names", exact = TRUE),
    class = "builder_attr_bomb"
  )
  context <- builder_immune_fixture_context(object)
  context$metadata <- metadata
  metadata_got <- builder_profile_immune_content(
    object,
    context
  )$immune_repertoire$candidates$metadata
  expect_false(metadata_got$valid)
  expect_contains(metadata_got$diagnostics, "unsafe_row_names")
  expect_identical(touched, character())

  object <- builder_immune_fixture_object()
  hla <- data.frame(
    sample = "sample_a",
    allele = "HLA-A*02:01",
    stringsAsFactors = FALSE
  )
  attr(hla, "names") <- structure(
    names(hla),
    class = "builder_attr_bomb"
  )
  object@misc$hla_typing <- hla
  hla_names <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_false(hla_names$valid)
  expect_contains(hla_names$diagnostics, "unsafe_column_names")
  expect_identical(touched, character())

  object <- builder_immune_fixture_object()
  hla <- data.frame(
    sample = "sample_a",
    allele = "HLA-A*02:01",
    stringsAsFactors = FALSE
  )
  attr(hla, "row.names") <- structure(
    attr(hla, "row.names", exact = TRUE),
    class = "builder_attr_bomb"
  )
  object@misc$hla_typing <- hla
  hla_rows <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_false(hla_rows$valid)
  expect_contains(hla_rows$diagnostics, "unsafe_row_names")
  expect_identical(touched, character())
})

test_that("metadata repertoire exposes conversion facts without choosing it", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  for (column in c("CTgene", "CTnt", "CTaa", "CTstrict")) {
    object@meta.data[[column]] <- NA_character_
  }
  rows <- cells[c(1L, 3L, 4L)]
  table <- builder_immune_fixture_table(rows, "TRB", "metadata")
  object@meta.data[rows, c("CTgene", "CTnt", "CTaa", "CTstrict")] <-
    table[, c("CTgene", "CTnt", "CTaa", "CTstrict")]
  context <- builder_immune_fixture_context(object)
  context$metadata <- object@meta.data

  candidate <- builder_profile_immune_content(
    object,
    context
  )$immune_repertoire$candidates$metadata

  expect_true(candidate$detected)
  expect_true(candidate$valid)
  expect_identical(candidate$normalized$barcode_origin, "metadata_rownames")
  expect_identical(candidate$normalized$sample_column, "orig.ident")
  expect_setequal(candidate$normalized$sample_names, c("sample_a", "sample_b"))
  expect_identical(candidate$normalized$n_rows, 3L)
  expect_identical(candidate$normalized$dataset_overlap_count, 3L)
})

test_that("partial metadata repertoire is detected but is not convertible", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  object@meta.data$CTgene <- rep("TRBV1.TRBJ1", nrow(object@meta.data))
  context <- builder_immune_fixture_context(object)
  context$metadata <- object@meta.data

  candidate <- builder_profile_immune_content(
    object,
    context
  )$immune_repertoire$candidates$metadata

  expect_true(candidate$detected)
  expect_false(candidate$valid)
  expect_contains(candidate$diagnostics, "missing_required_columns")
  expect_setequal(
    candidate$missing_columns,
    c("CTnt", "CTaa", "CTstrict")
  )
})

test_that("immune comparisons separate exact and complementary chains", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  common <- builder_immune_fixture_table(cells[1:2], "TRB", "same")
  object@misc$immune_repertoire <- list(sample_a = common)
  object@misc$tcr_data <- list(sample_a = common)
  object@misc$bcr_data <- list(
    sample_a = builder_immune_fixture_table(cells[2:3], "IGH", "different")
  )

  ir <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$immune_repertoire
  overlaps <- ir$source_overlaps
  exact <- Filter(
    function(x) {
      identical(x$left, "unified_misc") &&
        identical(x$right, "legacy_tcr")
    },
    overlaps
  )[[1L]]
  divergent <- Filter(
    function(x) {
      identical(x$left, "unified_misc") &&
        identical(x$right, "legacy_bcr")
    },
    overlaps
  )[[1L]]

  expect_identical(exact$n_overlap, 2L)
  expect_identical(exact$n_exact, 2L)
  expect_identical(exact$n_divergent, 0L)
  expect_true(exact$equivalent)
  expect_identical(divergent$n_overlap, 0L)
  expect_identical(divergent$n_exact, 0L)
  expect_identical(divergent$n_divergent, 0L)
  expect_false(divergent$equivalent)
  expect_identical(ir$selected_source, NULL)
})

test_that("source overlap treats reassigned samples as divergence", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  common <- builder_immune_fixture_table(cells[[1L]], "TRB", "same")
  object@misc$immune_repertoire <- list(sample_a = common)
  object@misc$tcr_data <- list(sample_b = common)

  overlaps <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$immune_repertoire$source_overlaps
  comparison <- Filter(
    function(x) {
      identical(x$left, "unified_misc") &&
        identical(x$right, "legacy_tcr")
    },
    overlaps
  )[[1L]]

  expect_identical(comparison$n_overlap, 1L)
  expect_identical(comparison$n_exact, 0L)
  expect_identical(comparison$n_divergent, 1L)
  expect_false(comparison$equivalent)
  expect_identical(comparison$divergent_preview, cells[[1L]])
})

test_that("HLA named-list input normalizes with explicit provenance", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  object@misc$hla_typing <- list(
    sample_a = c("HLA-A*02:01", "HLA-A*11:01"),
    sample_b = "HLA-B*08:01"
  )
  object@misc$hla_typing_source_type <- "genotyped"

  hla <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla

  builder_immune_expect_record_contract(hla)
  expect_true(hla$detected)
  expect_true(hla$valid)
  expect_identical(hla$shape, "named_list")
  expect_identical(hla$normalized$n_rows, 3L)
  expect_true(hla_is_typing_table(hla$normalized$table_preview))
  expect_identical(nrow(hla$normalized$table_preview), 3L)
  expect_identical(hla$provenance$source_types, "genotyped")
  expect_false(hla$provenance$has_unknown)
  expect_true(hla$provenance$association_eligible)
  expect_false(hla$provenance$descriptive_only)
  expect_false(hla$attention)
  expect_identical(hla$page_candidates, character())
  expect_identical(hla$page_gate$role, "supporting")
  expect_false(hla$page_gate$opens)
  expect_identical(hla$page_gate$requires, "valid_tcr")
})

test_that("HLA wide and canonical inputs use the shared HLA core", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  wide <- data.frame(
    sample = c("sample_a", "sample_b"),
    `HLA-A_1` = c("02:01", "01:01"),
    `HLA-A_2` = c("11:01", "03:01"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  object@misc$hla_typing <- wide
  object@misc$hla_typing_source_type <- "imputed"
  wide_profile <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_true(wide_profile$valid)
  expect_identical(wide_profile$shape, "wide")
  expect_setequal(
    wide_profile$normalized$table_preview$allele,
    c(
      "HLA-A*02:01",
      "HLA-A*01:01",
      "HLA-A*11:01",
      "HLA-A*03:01"
    )
  )
  expect_identical(wide_profile$provenance$source_types, "imputed")

  canonical <- wide_profile$normalized$table_preview
  canonical$source_type[1L] <- "not-a-source"
  object@misc$hla_typing <- canonical
  object@misc$hla_typing_source_type <- "genotyped"
  canonical_profile <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_true(canonical_profile$valid)
  expect_identical(canonical_profile$shape, "canonical_long")
  expect_contains(canonical_profile$provenance$source_types, "unknown")
  expect_true(canonical_profile$provenance$has_unknown)
  expect_true(canonical_profile$attention)
  expect_contains(canonical_profile$diagnostics, "unknown_provenance")
})

test_that("unknown HLA provenance is attention, not inferred evidence", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  object@misc$hla_typing <- data.frame(
    sample = "sample_a",
    allele = "HLA-A*02:01",
    stringsAsFactors = FALSE
  )

  hla <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla

  expect_true(hla$valid)
  expect_identical(hla$shape, "long")
  expect_identical(hla$normalized$table_preview$source_type, "unknown")
  expect_true(hla$provenance$has_unknown)
  expect_true(hla$attention)
  expect_contains(hla$diagnostics, "unknown_provenance")
  expect_true(any(grepl("unknown", hla$qc$issue, fixed = TRUE)))
})

test_that("malformed HLA is retained as an invalid source fact", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  object@misc$hla_typing <- data.frame(
    sample = "sample_a",
    allele = "banana",
    stringsAsFactors = FALSE
  )
  object@misc$hla_typing_source_type <- "genotyped"

  hla <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla

  expect_true(hla$detected)
  expect_false(hla$valid)
  expect_true(hla_is_typing_table(hla$normalized$table_preview))
  expect_identical(hla$normalized$n_rows, 0L)
  expect_identical(nrow(hla$normalized$table_preview), 0L)
  expect_contains(hla$diagnostics, "no_valid_alleles")
  expect_identical(hla$page_candidates, character())
})

test_that("HLA-only data remains supporting and cannot open the motif page", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  object@misc$hla_typing <- list(sample_a = "HLA-A*02:01")
  object@misc$hla_typing_source_type <- "genotyped"

  content <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )

  expect_false(content$immune_repertoire$detected)
  expect_identical(content$immune_repertoire$page_candidates, character())
  expect_true(content$hla$valid)
  expect_identical(content$hla$page_candidates, character())
  expect_false(content$hla$page_gate$opens)
  expect_identical(content$hla$page_gate$requires, "valid_tcr")
})

test_that("HLA mappings use exact Viewer sample identity", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  object@misc$immune_repertoire <- list(
    sample_exact = builder_immune_fixture_table(cells[1], "TRB", "one"),
    donor_alias = builder_immune_fixture_table(cells[2], "TRB", "two"),
    sample_conflict = builder_immune_fixture_table(cells[3], "TRB", "three"),
    donor_conflict = builder_immune_fixture_table(cells[4], "TRB", "four")
  )
  object@misc$hla_typing <- data.frame(
    sample = c("sample_exact", "raw_alias", "sample_conflict", "nowhere"),
    donor_id = c("donor_zero", "donor_alias", "donor_conflict", "absent"),
    allele = c(
      "HLA-A*02:01",
      "HLA-A*03:01",
      "HLA-A*11:01",
      "HLA-A*24:02"
    ),
    stringsAsFactors = FALSE
  )
  object@misc$hla_typing_source_type <- "genotyped"

  content <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )
  mapping <- content$hla$unit_mappings$unified_misc
  viewer_map <- hla_analysis_unit_map(
    content$hla$normalized$table_preview,
    names(object@misc$immune_repertoire)
  )

  expect_identical(mapping$matched_sample_count, 2L)
  expect_identical(mapping$unmatched_hla_count, 2L)
  expect_identical(mapping$unmatched_ir_count, 2L)
  expect_false(mapping$donor_collapse_eligible)
  expect_identical(mapping$analysis_unit_type, "sample")
  expect_identical(mapping$analysis_unit_count, 4L)
  expect_setequal(mapping$matched_preview, c("sample_exact", "sample_conflict"))
  expect_setequal(mapping$unmatched_hla_preview, c("raw_alias", "nowhere"))
  expect_setequal(
    mapping$unmatched_ir_preview,
    c("donor_alias", "donor_conflict")
  )
  expect_identical(
    mapping$analysis_unit_type,
    unique(viewer_map$unit_type)
  )
  expect_identical(
    mapping$analysis_unit_count,
    as.integer(length(unique(viewer_map$analysis_unit)))
  )
})

test_that("exact HLA matches can collapse complete samples to donors", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  object@misc$immune_repertoire <- list(
    sample_a = builder_immune_fixture_table(cells[1], "TRB", "one"),
    sample_b = builder_immune_fixture_table(cells[2], "TRB", "two")
  )
  object@misc$hla_typing <- data.frame(
    sample = c("sample_a", "sample_b"),
    donor_id = c("donor_one", "donor_one"),
    allele = c("HLA-A*02:01", "HLA-A*03:01"),
    stringsAsFactors = FALSE
  )
  object@misc$hla_typing_source_type <- "genotyped"

  hla <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  mapping <- hla$unit_mappings$unified_misc
  viewer_map <- hla_analysis_unit_map(
    hla$normalized$table_preview,
    names(object@misc$immune_repertoire)
  )

  expect_identical(mapping$matched_sample_count, 2L)
  expect_true(mapping$donor_collapse_eligible)
  expect_identical(mapping$analysis_unit_type, "donor")
  expect_identical(mapping$analysis_unit_count, 1L)
  expect_identical(mapping$analysis_unit_type, unique(viewer_map$unit_type))
  expect_identical(
    mapping$analysis_unit_count,
    as.integer(length(unique(viewer_map$analysis_unit)))
  )
  expect_identical(hla$page_candidates, character())
})

test_that("the deterministic all-content fixture profiles real IR and HLA", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_all_modalities()
  expect_s4_class(object, "Seurat")
  context <- builder_immune_fixture_context(object)

  content <- builder_profile_immune_content(object, context)

  expect_true(content$immune_repertoire$detected)
  expect_true(content$immune_repertoire$valid)
  expect_true(content$immune_repertoire$candidates$unified_misc$valid)
  expect_identical(
    content$immune_repertoire$candidates$unified_misc$normalized$n_samples,
    2L
  )
  expect_identical(
    content$immune_repertoire$candidates$unified_misc$normalized$n_rows,
    300L
  )
  expect_contains(
    content$immune_repertoire$candidates$unified_misc$normalized$chains,
    "TRA"
  )
  expect_true(content$hla$detected)
  expect_true(content$hla$valid)
  expect_identical(content$hla$normalized$n_rows, 12L)
  expect_identical(nrow(content$hla$normalized$table_preview), 12L)
  expect_false(content$hla$attention)
  expect_identical(content$hla$provenance$source_types, "synthetic")
  expect_false(content$hla$provenance$association_eligible)
  expect_true(content$hla$provenance$descriptive_only)
})

test_that("tracked Viewer IR and HLA payloads satisfy Builder contracts", {
  demo <- builder_immune_fixture_viewer_demo()
  skip_if(is.null(demo), "tracked HLA/TCR Viewer demo is unavailable")
  repertoire <- demo[["immune_repertoire"]]
  cells <- unique(unlist(lapply(repertoire, `[[`, "barcode")))
  candidate <- .builder_immune_candidate_from_tables(
    repertoire,
    cells,
    source_kind = "tracked_viewer_demo"
  )
  hla <- .builder_immune_profile_hla(
    list(hla_typing = demo[["hla_typing"]]),
    list(unified_misc = candidate)
  )

  expect_true(candidate$full_ir_ready)
  expect_true(candidate$hla_tcr_ready)
  expect_setequal(
    candidate$parseable_tcr_chains,
    intersect(hla_detect_chains(repertoire), c("TRA", "TRB"))
  )
  expect_true(hla$valid)
  expect_identical(hla$normalized$n_rows, 14L)
  expect_identical(hla$page_candidates, character())
})

test_that("every returned preview string has character and byte bounds", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object()
  cells <- SeuratObject::Cells(object)
  long_sample <- paste0("sample_", strrep("界", 10000L))
  repertoire <- list(builder_immune_fixture_table(cells[[1L]], "TRB"))
  names(repertoire) <- long_sample
  object@misc$immune_repertoire <- repertoire

  n_hla <- 20L
  object@misc$hla_typing <- data.frame(
    sample = paste0("sample_", seq_len(n_hla)),
    donor_id = NA_character_,
    locus = "HLA-A",
    copy = 1L,
    allele = "HLA-A*02:01",
    resolution = "2-field",
    source_type = "genotyped",
    typing_method = "fixture",
    source_reference = vapply(
      seq_len(n_hla),
      function(index) paste0(index, strrep("界", 10000L)),
      character(1)
    ),
    confidence = 1,
    stringsAsFactors = FALSE
  )

  content <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )
  ir_candidate <- content$immune_repertoire$candidates$unified_misc
  hla_preview <- content$hla$normalized$table_preview
  preview_text <- c(
    ir_candidate$normalized$sample_names,
    unlist(
      hla_preview[vapply(hla_preview, is.character, logical(1))],
      use.names = FALSE
    )
  )
  preview_text <- preview_text[!is.na(preview_text)]

  expect_gt(ir_candidate$preview_truncated_count, 0L)
  expect_gt(content$hla$preview_truncated_count, 0L)
  expect_true(all(
    nchar(preview_text, type = "chars") <=
      .builder_immune_preview_character_limit
  ))
  expect_true(all(
    nchar(preview_text, type = "bytes") <= .builder_immune_preview_byte_limit
  ))
  expect_lt(as.numeric(object.size(content)), 200000)

  object <- builder_immune_fixture_object()
  object@misc$hla_typing <- data.frame(
    sample = "sample_a",
    allele = paste0("not-an-allele-", strrep("界", 10000L)),
    stringsAsFactors = FALSE
  )
  qc_hla <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )$hla
  expect_gt(qc_hla$preview_truncated_count, 0L)
  expect_true(all(
    nchar(qc_hla$qc$value, type = "chars") <=
      .builder_immune_preview_character_limit
  ))
  expect_true(all(
    nchar(qc_hla$qc$value, type = "bytes") <= .builder_immune_preview_byte_limit
  ))
})

test_that("immune and HLA normalized facts stay bounded for large inputs", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object(120L)
  cells <- SeuratObject::Cells(object)
  object@misc$immune_repertoire <- list(
    sample_a = builder_immune_fixture_table(cells, "TRB", "large")
  )
  hla_rows <- 80L
  object@misc$hla_typing <- data.frame(
    sample = paste0("sample_", seq_len(hla_rows)),
    allele = rep("HLA-A*02:01", hla_rows),
    stringsAsFactors = FALSE
  )
  object@misc$hla_typing_source_type <- "genotyped"

  content <- builder_profile_immune_content(
    object,
    builder_immune_fixture_context(object)
  )

  ir_summary <- content$immune_repertoire$candidates$unified_misc$normalized
  expect_identical(ir_summary$n_rows, 120L)
  expect_lte(length(ir_summary$barcode_preview), 20L)
  expect_false("barcodes" %in% names(ir_summary))
  expect_false("tables" %in% names(ir_summary))
  expect_identical(content$hla$normalized$n_rows, hla_rows)
  expect_lte(nrow(content$hla$normalized$table_preview), 20L)
  expect_lte(length(content$hla$normalized$sample_preview), 20L)
  expect_lt(as.numeric(object.size(content)), 200000)
})

test_that("HLA locus summaries stay bounded for many valid loci", {
  n_loci <- 2000L
  loci <- paste0("HLA-L", seq_len(n_loci))
  typing <- data.frame(
    sample = rep("sample_a", n_loci),
    donor_id = NA_character_,
    locus = loci,
    copy = 1L,
    allele = paste0(loci, "*02:01"),
    resolution = "2-field",
    source_type = "genotyped",
    typing_method = "fixture",
    source_reference = NA_character_,
    confidence = 1,
    stringsAsFactors = FALSE
  )

  hla <- .builder_immune_profile_hla(
    list(hla_typing = typing),
    list()
  )

  expect_true(hla$valid)
  expect_identical(hla$normalized$n_loci, n_loci)
  expect_type(hla$normalized$locus_preview, "character")
  expect_length(hla$normalized$locus_preview, 20L)
  expect_lte(length(hla$normalized$locus_preview), 20L)
  expect_identical(hla$normalized$loci_truncated_count, 1980L)
  expect_false("loci" %in% names(hla$normalized))
  expect_gte(hla$preview_truncated_count, 1980L)
  expect_lt(as.numeric(object.size(hla)), 200000)
})
