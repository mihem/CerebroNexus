make_bpcells_cerebro <- function(root) {
  cells <- paste0("cell-", seq_len(4L))
  counts <- matrix(
    c(0, 1, 4, 2, 0, 5, 3, 6, 0, 7, 8, 9),
    nrow = 3L,
    dimnames = list(paste0("gene-", seq_len(3L)), cells)
  )
  sparse <- methods::as(Matrix::Matrix(counts, sparse = TRUE), "CsparseMatrix")
  sidecar <- file.path(root, "expression.bpcells")
  BPCells::write_matrix_dir(
    methods::as(sparse, "IterableMatrix"),
    dir = sidecar
  )

  object <- Cerebro$new()
  object$expression <- BPCells::open_matrix_dir(sidecar)
  object$setExpressionBackend("bpcells", basename(sidecar))
  object$meta_data <- data.frame(
    cell_barcode = cells,
    sample = c("a", "a", "b", "b"),
    nUMI = as.double(colSums(counts)),
    nGene = as.double(colSums(counts > 0)),
    stringsAsFactors = FALSE
  )
  object$projections <- list(
    umap = data.frame(
      UMAP_1 = seq_along(cells),
      UMAP_2 = rev(seq_along(cells)),
      row.names = cells
    )
  )
  list(object = object, cells = cells, counts = counts, sidecar = sidecar)
}

expect_cerebro_fields <- function(object, fixture) {
  expect_identical(object$getCellNames(), fixture$cells)
  expect_identical(object$meta_data$cell_barcode, fixture$cells)
  expect_identical(rownames(object$getProjection("umap")), fixture$cells)
  expect_equal(as.matrix(object$expression), fixture$counts)
}

test_that("legacy RDS CRBs rebuild the current Cerebro class", {
  path <- tempfile(fileext = ".crb")
  object <- Cerebro$new()
  object$expression <- matrix(
    1:4,
    nrow = 2L,
    dimnames = list(c("gene-1", "gene-2"), c("cell-1", "cell-2"))
  )
  saveRDS(object, path)

  restored <- readCerebro(path)
  expect_s3_class(restored, "Cerebro")
  expect_equal(restored$expression, object$expression)
  expect_identical(
    restored$getExpressionBackend(),
    list(
      type = "embedded",
      location = NULL
    )
  )
})

test_that("bundled legacy demo CRBs remain readable", {
  demo_dir <- file.path(viewer_app_test_path(), "extdata", "examples")
  demos <- list.files(
    demo_dir,
    pattern = "\\.crb$",
    full.names = TRUE
  )

  expect_length(demos, 9L)
  for (path in demos) {
    expect_true(inherits(readCerebro(path), "Cerebro"), info = basename(path))
  }
})

test_that("thin RDS and qs2 CRBs share one validated BPCells sidecar", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  fixture <- make_bpcells_cerebro(root)
  rds <- file.path(root, "thin-rds.crb")
  qs <- file.path(root, "thin-qs2.crb")
  saveCerebro(fixture$object, rds, codec = "rds")
  saveCerebro(fixture$object, qs)

  payload <- .readCerebroPayload(rds)
  expect_null(payload$expression)
  expect_false("cell_barcode" %in% names(payload$meta_data))
  expect_type(payload$meta_data$nUMI, "integer")
  expect_type(payload$meta_data$nGene, "integer")
  expect_identical(payload$crb_schema$version, 2L)
  expect_identical(payload$crb_schema$n_cells, 4L)
  expect_identical(payload$crb_schema$projection_rownames, "umap")
  expect_match(
    payload$cell_fingerprint,
    "^md5-cell-set-v1:[0-9a-f]{32}$"
  )
  expect_identical(
    readBin(qs, "raw", n = 4L),
    as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))
  )

  rds_object <- readCerebro(rds)
  qs_object <- readCerebro(qs)
  expect_cerebro_fields(rds_object, fixture)
  expect_cerebro_fields(qs_object, fixture)

  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = runtime
  )
  runtime_object <- runtime$read_cerebro_file(qs)
  runtime_object <- runtime$.attachExternalExpression(runtime_object, qs)
  expect_identical(runtime_object$meta_data, qs_object$meta_data)
  expect_identical(runtime_object$projections, qs_object$projections)
  expect_equal(as.matrix(runtime_object$expression), fixture$counts)

  invalid <- .readCerebroPayload(rds)
  invalid$crb_schema$cell_names_md5 <- paste(rep("0", 32L), collapse = "")
  invalid_path <- file.path(root, "invalid-checksum.crb")
  saveRDS(invalid, invalid_path)
  expect_error(readCerebro(invalid_path), "cell-name index does not match")

  wrong_count <- .readCerebroPayload(rds)
  wrong_count$crb_schema$n_cells <- 3L
  wrong_count_path <- file.path(root, "invalid-cell-count.crb")
  saveRDS(wrong_count, wrong_count_path)
  expect_error(readCerebro(wrong_count_path), "cell count does not match")

  wrong_fingerprint <- .readCerebroPayload(rds)
  wrong_fingerprint$cell_fingerprint <- paste0(
    "md5-cell-set-v1:",
    paste(rep("0", 32L), collapse = "")
  )
  wrong_fingerprint_path <- file.path(root, "invalid-cell-fingerprint.crb")
  saveRDS(wrong_fingerprint, wrong_fingerprint_path)
  expect_error(
    readCerebro(wrong_fingerprint_path),
    "cell fingerprint does not match"
  )
  runtime_fingerprint <- runtime$read_cerebro_file(wrong_fingerprint_path)
  expect_error(
    runtime$.attachExternalExpression(
      runtime_fingerprint,
      wrong_fingerprint_path
    ),
    "cell fingerprint does not match"
  )

  other_cells <- paste0("other-cell-", seq_len(4L))
  other_sidecar <- file.path(root, "other-expression.bpcells")
  other_counts <- fixture$counts
  colnames(other_counts) <- other_cells
  BPCells::write_matrix_dir(
    methods::as(
      methods::as(Matrix::Matrix(other_counts, sparse = TRUE), "CsparseMatrix"),
      "IterableMatrix"
    ),
    dir = other_sidecar
  )
  wrong_sidecar <- .readCerebroPayload(rds)
  wrong_sidecar$expression_backend$location <- basename(other_sidecar)
  wrong_sidecar$crb_schema$cell_names_md5 <- .bpcellsCellNamesChecksum(
    other_sidecar
  )
  wrong_sidecar_path <- file.path(root, "same-count-wrong-cells.crb")
  saveRDS(wrong_sidecar, wrong_sidecar_path)
  expect_error(
    readCerebro(wrong_sidecar_path),
    "cell fingerprint does not match"
  )

  version_one <- .readCerebroPayload(rds)
  version_one$crb_schema$version <- 1L
  version_one$crb_schema$n_cells <- NULL
  version_one_path <- file.path(root, "schema-v1.crb")
  saveRDS(version_one, version_one_path)
  expect_cerebro_fields(readCerebro(version_one_path), fixture)

  missing_root <- file.path(root, "missing-sidecar")
  dir.create(missing_root)
  missing_sidecar <- file.path(missing_root, basename(rds))
  file.copy(rds, missing_sidecar)
  expect_error(readCerebro(missing_sidecar), "sidecar is missing")

  converted <- file.path(root, "converted-rds.crb")
  convertCerebro(qs, converted, codec = "rds")
  expect_cerebro_fields(readCerebro(converted), fixture)

  other_root <- file.path(root, "converted")
  cross_directory <- file.path(other_root, "converted-rds.crb")
  expect_error(
    convertCerebro(qs, cross_directory, codec = "rds"),
    "must be in the same directory"
  )
  expect_false(file.exists(cross_directory))

  convertCerebro(qs)
  expect_cerebro_fields(readCerebro(qs), fixture)
})
