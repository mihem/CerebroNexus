## Import speed changes must not change what an import produces. Each test
## compares the fast path with the implementation it replaced.

builder_repo_source("io.R", local = globalenv())
builder_profile_source_runtime(local = globalenv())
builder_repo_source("inspect.R", local = globalenv())
builder_repo_source("adapters.R", local = globalenv())

## The row-by-row implementation the vectorised one replaced.
.reference_comparison_records <- function(table, sample_name, columns) {
  out <- character()
  if (
    !is.character(sample_name) ||
      length(sample_name) != 1L ||
      is.na(sample_name) ||
      !nzchar(sample_name)
  ) {
    return(out)
  }
  for (row in seq_len(nrow(table))) {
    barcode <- table$barcode[[row]]
    if (is.na(barcode) || !nzchar(barcode)) {
      next
    }
    chains <- .builder_immune_detect_chains(table$CTgene[[row]])
    if (!length(chains)) {
      next
    }
    values <- vapply(
      columns,
      function(column) {
        value <- table[[column]][[row]]
        if (is.na(value)) "<NA>" else value
      },
      character(1)
    )
    signature <- paste(c(sample_name, values), collapse = "\u001f")
    keys <- paste(chains, barcode, sep = "\u001f")
    records <- rep(signature, length(keys))
    names(records) <- keys
    out <- c(out, records)
  }
  out
}

.speed_receptor_table <- function(n = 40L, seed = 7L) {
  set.seed(seed)
  genes <- c(
    "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1",
    "TRBV5.TRBJ2",
    "IGHV1.IGHJ4_IGKV3.IGKJ1",
    "trgv9.trgj1_trdv2.trdj1",
    "NA_TRBV7.TRBJ2",
    "unparseable",
    "",
    NA_character_,
    "IGLV2.IGLJ1",
    "XTRAV1"
  )
  data.frame(
    barcode = c(
      sample(c(paste0("cell", seq_len(n)), "", NA), n, replace = TRUE)
    ),
    CTgene = sample(genes, n, replace = TRUE),
    CTnt = sample(c("TGTGCC", NA_character_, "TGCAGC"), n, replace = TRUE),
    CTaa = sample(c("CASSL_CAVR", NA_character_, "CASRéF"), n, TRUE),
    CTstrict = sample(c("a;b", "c;d", NA_character_), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("immune comparison records match the row-by-row records", {
  columns_full <- .builder_immune_required_columns
  columns_motif <- c("barcode", "CTgene", "CTaa")
  for (seed in 1:6) {
    table <- .speed_receptor_table(n = 25L * seed, seed = seed)
    for (columns in list(columns_full, columns_motif)) {
      expect_identical(
        .builder_immune_comparison_records(table, "sample_a", columns),
        .reference_comparison_records(table, "sample_a", columns)
      )
    }
  }
  table <- .speed_receptor_table(30L, 11L)
  for (sample_name in list(NA_character_, "", c("a", "b"), 1)) {
    expect_identical(
      .builder_immune_comparison_records(table, sample_name, columns_full),
      .reference_comparison_records(table, sample_name, columns_full)
    )
  }
  empty <- table[0, ]
  expect_identical(
    .builder_immune_comparison_records(empty, "s", columns_full),
    .reference_comparison_records(empty, "s", columns_full)
  )
  no_chain <- table
  no_chain$CTgene <- "none"
  expect_identical(
    .builder_immune_comparison_records(no_chain, "s", columns_full),
    character()
  )
  single <- table[!is.na(table$barcode) & nzchar(table$barcode), ][1L, ]
  single$CTgene <- "TRAV1_TRBV2"
  expect_identical(
    .builder_immune_comparison_records(single, "s", columns_motif),
    .reference_comparison_records(single, "s", columns_motif)
  )
  factor_genes <- table
  factor_genes$CTgene <- factor(factor_genes$CTgene)
  expect_identical(
    .builder_immune_comparison_records(factor_genes, "s", columns_motif[-2L]),
    .reference_comparison_records(factor_genes, "s", columns_motif[-2L])
  )
})

test_that("a TCR dataset profile matches the row-by-row records", {
  skip_if_not_installed("SeuratObject")
  object <- builder_immune_fixture_object(n = 60L)
  cells <- SeuratObject::Cells(object)
  tables <- lapply(split(cells, object$sample), function(barcodes) {
    data.frame(
      barcode = barcodes,
      CTgene = rep(
        c("TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1", "TRBV5.TRBJ2", NA),
        length.out = length(barcodes)
      ),
      CTnt = NA_character_,
      CTaa = paste0("CASS", seq_along(barcodes), "F"),
      CTstrict = paste0("strict", seq_along(barcodes)),
      stringsAsFactors = FALSE
    )
  })
  object@misc$tcr_data <- tables
  object$CTgene <- rep(
    c("TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1", NA, "TRBV9.TRBJ1"),
    length.out = length(cells)
  )
  object$CTaa <- paste0("CASS", seq_along(cells))
  object$CTnt <- NA_character_
  object$CTstrict <- paste0("s", seq_along(cells))
  source <- builder_profile_source_fixture()
  fast <- builder_dataset_profile(object, source)
  calls <- 0L
  testthat::local_mocked_bindings(
    .builder_immune_comparison_records = function(...) {
      calls <<- calls + 1L
      .reference_comparison_records(...)
    },
    .env = globalenv()
  )
  reference <- builder_dataset_profile(object, source)
  expect_gt(calls, 0L)
  expect_identical(fast, reference)
})

.speed_v5_object <- function() {
  counts <- Matrix::Matrix(
    matrix(
      rpois(200L, 2),
      nrow = 10L,
      dimnames = list(paste0("g", 1:10), paste0("c", 1:20))
    ),
    sparse = TRUE
  )
  object <- SeuratObject::CreateSeuratObject(
    SeuratObject::CreateAssay5Object(counts = counts, data = log1p(counts))
  )
  SeuratObject::LayerData(object[["RNA"]], layer = "scale.data") <-
    as.matrix(log1p(counts))
  object
}

test_that("layer checks read in-memory v5 layers without copying them", {
  skip_if_not_installed("SeuratObject")
  object <- .speed_v5_object()
  copies <- 0L
  counting <- function(...) {
    copies <<- copies + 1L
    SeuratObject::LayerData(...)
  }
  expect_identical(
    .builder_snapshot_layer_contracts(object, .layer_data = counting),
    list()
  )
  expect_identical(copies, 0L)
  expect_identical(.builder_stored_layer(object[["RNA"]], "missing"), NULL)
  expect_identical(
    .builder_stored_layer(object[["RNA"]], "counts"),
    methods::slot(object[["RNA"]], "layers")$counts
  )

  ## Both paths classify every layer the same way.
  for (assay in names(object@assays)) {
    for (layer in SeuratObject::Layers(object[[assay]])) {
      stored <- .builder_stored_layer(object[[assay]], layer)
      viewed <- suppressWarnings(SeuratObject::LayerData(
        object[[assay]],
        layer = layer
      ))
      expect_identical(class(stored), class(viewed))
    }
  }

  profile <- describe_seurat(object)
  fast <- .builder_expression_matrix_summary(object, profile)
  testthat::local_mocked_bindings(
    .builder_stored_layer = function(...) NULL,
    .env = globalenv()
  )
  expect_identical(fast, .builder_expression_matrix_summary(object, profile))
  expect_identical(fast, list(sparse = TRUE, storage = "memory"))
})

test_that("a BPCells layer is still refused through its stored class", {
  skip_if_not_installed("SeuratObject")
  object <- .speed_v5_object()
  testthat::local_mocked_bindings(
    .builder_stored_layer = function(assay_object, layer_name) {
      structure(list(), class = "IterableMatrix")
    },
    .env = globalenv()
  )
  expect_error(
    .builder_snapshot_layer_contracts(object),
    "BPCells matrix"
  )
})
