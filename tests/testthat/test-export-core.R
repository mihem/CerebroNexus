## Tests for the format-independent export core (R/export_core.R)
##
## Nothing here touches Seurat or SingleCellExperiment: the core takes plain R
## data, which is exactly what a new input-format adapter has to provide.

core_fixture <- function(n_cells = 30L, n_genes = 12L) {
  set.seed(1)
  cells <- paste0("cell", seq_len(n_cells))
  genes <- paste0("gene", seq_len(n_genes))
  counts <- Matrix::rsparsematrix(
    n_genes,
    n_cells,
    density = 0.4,
    rand.x = function(n) rpois(n, 3) + 1
  )
  dimnames(counts) <- list(genes, cells)
  meta <- data.frame(
    cluster = rep(c("a", "b", NA), length.out = n_cells),
    nCount = Matrix::colSums(counts),
    nFeature = Matrix::colSums(counts > 0),
    extra = seq_len(n_cells),
    row.names = cells,
    stringsAsFactors = FALSE
  )
  embeddings <- list(
    umap = data.frame(
      UMAP_1 = rnorm(n_cells),
      UMAP_2 = rnorm(n_cells),
      row.names = cells
    ),
    pca = data.frame(
      PC_1 = rnorm(n_cells),
      PC_2 = rnorm(n_cells),
      row.names = cells
    )
  )
  list(
    cells = cells,
    genes = genes,
    counts = counts,
    meta = meta,
    embeddings = embeddings
  )
}

run_core_export <- function(fx, file, mode = "embedded", misc = list()) {
  stage <- .createPrivateExportStage(file)
  on.exit(unlink(stage, recursive = TRUE, force = TRUE), add = TRUE)
  export <- .exportInit("core test", "hg", misc, verbose = FALSE)
  expression_data <- .exportWriteExpression(
    export,
    fx$counts,
    expression_matrix_mode = mode,
    use_delayed_array = FALSE,
    export_stage_dir = stage,
    final_file = file,
    verbose = FALSE
  )
  .addExportExperiment(export, misc)
  .addExportMetaData(
    export,
    fx$meta,
    fx$cells,
    groups = "cluster",
    nUMI = "nCount",
    nGene = "nFeature",
    cell_cycle = NULL,
    add_all_meta_data = TRUE,
    verbose = FALSE
  )
  .addExportProjections(
    export,
    names(fx$embeddings),
    function(name) fx$embeddings[[name]],
    verbose = FALSE
  )
  .exportAddMiscResults(
    export,
    misc,
    groups = "cluster",
    cells = fx$cells,
    verbose = FALSE
  )
  suppressMessages(.exportFinalize(
    export,
    file,
    stage,
    mode,
    codec = "qs2",
    viewer_options = .viewerPackOptions("never", 500000L)
  ))
  invisible(expression_data)
}

test_that("core writes a readable .crb from plain data, without any object class", {
  fx <- core_fixture()
  file <- file.path(withr::local_tempdir(), "plain.crb")
  run_core_export(fx, file)

  expect_true(file.exists(file))
  obj <- readCerebro(file)
  expect_identical(obj$getCellNames(), fx$cells)
  expect_identical(obj$getGeneNames(), fx$genes)
  expect_identical(obj$getExperiment()$experiment_name, "core test")
  expect_true("cluster" %in% obj$getGroups())
  ## NA in a grouping column becomes an explicit "N/A" level, not a dropped cell.
  expect_true("N/A" %in% obj$getGroupLevels("cluster"))
  ## Existing rule, kept by the refactor: PCA is dropped when a non-PCA
  ## reduction (here UMAP) is available.
  expect_identical(obj$availableProjections(), "umap")
})

test_that("core h5 mode writes the TENx sidecar next to the .crb", {
  skip_if_not_installed("HDF5Array")
  fx <- core_fixture()
  file <- file.path(withr::local_tempdir(), "plain_h5.crb")
  run_core_export(fx, file, mode = "h5")

  expect_true(file.exists(file.path(dirname(file), "plain_h5.h5")))
  obj <- readCerebro(file)
  expect_identical(obj$getExpressionBackend()$type, "h5")
  expect_equal(
    unname(as.matrix(obj$getExpressionBlock(fx$genes[1:3]))),
    unname(as.matrix(fx$counts[1:3, ])),
    ignore_attr = TRUE
  )
})

test_that("core argument checks reject bad output specs before any work", {
  expect_error(
    .exportCheckOutputArgs(c("a.crb", "b.crb"), "embedded", FALSE, FALSE),
    "one non-empty output path"
  )
  expect_error(
    .exportCheckOutputArgs(file.path(tempdir(), "x.rds"), "h5", FALSE, FALSE),
    "to end in .crb"
  )
  expect_error(
    .validateExportColumns(
      c("a", "b"),
      groups = c("a", "zzz"),
      nUMI = "a",
      nGene = "b",
      cell_cycle = NULL
    ),
    "zzz"
  )
  expect_error(
    .validateExportColumns(
      c("a"),
      groups = "a",
      nUMI = "missing",
      nGene = "a",
      cell_cycle = NULL
    ),
    "number of transcripts"
  )
})

test_that("core projections keep the PCA-only fallback and the empty-input error", {
  fx <- core_fixture()
  export <- Cerebro$new()
  expect_error(
    .addExportProjections(export, character(), identity, verbose = FALSE),
    "No dimensional reductions"
  )
  expect_warning(
    .addExportProjections(
      export,
      "pca",
      function(name) fx$embeddings[[name]],
      verbose = FALSE
    ),
    "Only PCA"
  )
  expect_identical(export$availableProjections(), "pca")
})

test_that("exported metadata keeps the chosen QC columns and cell barcodes", {
  meta <- data.frame(
    nCount_RNA = c(10, 20),
    nFeature_RNA = c(3, 4),
    nUMI = c(-1, -1),
    nGene = c(-2, -2),
    cell_barcode = c("x", "y"),
    group = factor(c("a", "b")),
    row.names = c("c1", "c2")
  )
  captured <- NULL
  export <- list(
    setMetaData = function(value) captured <<- value,
    addGroup = function(...) invisible(NULL),
    setCellCycle = function(...) invisible(NULL)
  )
  .addExportMetaData(
    export,
    meta,
    c("c1", "c2"),
    "group",
    "nCount_RNA",
    "nFeature_RNA",
    character(),
    TRUE,
    FALSE
  )
  expect_identical(captured$cell_barcode, c("c1", "c2"))
  expect_identical(captured$nUMI, c(10, 20))
  expect_identical(captured$nGene, c(3, 4))
  expect_false(any(
    c("nUMI.1", "nGene.1", "cell_barcode.1") %in% colnames(captured)
  ))
})

## An h5 export offered an earlier sidecar -------------------------------------

h5_reuse_export <- function(fx, sidecars = list()) {
  file <- file.path(
    withr::local_tempdir(.local_envir = parent.frame()),
    "x.crb"
  )
  withr::local_options(
    cerebro.expression_sidecars = sidecars,
    .local_envir = parent.frame()
  )
  run_core_export(fx, file, mode = "h5")
  list(
    file = file,
    sidecar = file.path(dirname(file), "x.h5")
  )
}

test_that("an h5 export takes an earlier sidecar written from the same matrix", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  fx <- core_fixture()
  first <- h5_reuse_export(fx)
  md5 <- unname(tools::md5sum(first$sidecar))
  expect_match(
    rhdf5::h5readAttributes(first$sidecar, "expression")$content_digest,
    "^[[:xdigit:]]{32}$"
  )

  ## The matrix is not transposed and written a second time.
  testthat::local_mocked_bindings(
    .writeTENxExpression = function(...) {
      stop("the matrix must not be written again")
    }
  )
  second <- h5_reuse_export(fx, list(list(path = first$sidecar, md5 = md5)))
  expect_identical(unname(tools::md5sum(second$sidecar)), md5)
  obj <- readCerebro(second$file)
  expect_identical(obj$getExpressionBackend()$md5, md5)
  expect_equal(
    unname(as.matrix(obj$getExpressionBlock(fx$genes[1:3]))),
    unname(as.matrix(fx$counts[1:3, ])),
    ignore_attr = TRUE
  )
})

test_that("an earlier sidecar is not taken for another matrix or a changed file", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  fx <- core_fixture()
  first <- h5_reuse_export(fx)
  md5 <- unname(tools::md5sum(first$sidecar))
  candidate <- list(list(path = first$sidecar, md5 = md5))

  ## Count the times the matrix is written; every export below must write it.
  writes <- 0L
  write_matrix <- .writeTENxExpression
  testthat::local_mocked_bindings(
    .writeTENxExpression = function(...) {
      writes <<- writes + 1L
      write_matrix(...)
    }
  )
  expect_block <- function(exported, counts) {
    expect_equal(
      unname(as.matrix(readCerebro(exported$file)$getExpressionBlock(
        fx$genes[1:3]
      ))),
      unname(as.matrix(counts[1:3, ])),
      ignore_attr = TRUE
    )
  }

  ## Another matrix: the sidecar is written again, from that matrix.
  other <- fx
  other$counts@x <- other$counts@x + 1
  rewritten <- h5_reuse_export(other, candidate)
  expect_identical(writes, 1L)
  expect_block(rewritten, other$counts)

  ## The recorded MD5 no longer describes the file (it was modified).
  tampered <- withr::local_tempfile(fileext = ".h5")
  file.copy(first$sidecar, tampered)
  connection <- file(tampered, "r+b")
  seek(connection, 2048)
  writeBin(as.raw(0:7), connection)
  close(connection)
  retaken <- h5_reuse_export(fx, list(list(path = tampered, md5 = md5)))
  expect_identical(writes, 2L)
  expect_block(retaken, fx$counts)

  ## A sidecar without a digest, a missing file and a malformed record.
  plain <- withr::local_tempfile(fileext = ".h5")
  HDF5Array::writeTENxMatrix(
    Matrix::t(fx$counts),
    plain,
    group = "expression",
    level = 0L
  )
  bad <- list(
    list(path = plain, md5 = unname(tools::md5sum(plain))),
    list(path = file.path(tempdir(), "missing.h5"), md5 = md5),
    list(path = first$sidecar, md5 = "not-a-md5"),
    "not a record"
  )
  for (index in seq_along(bad)) {
    written <- h5_reuse_export(fx, list(bad[[index]]))
    expect_identical(writes, 2L + index)
    expect_block(written, fx$counts)
  }
})

test_that("the digest of an expression matrix follows its content and writer", {
  fx <- core_fixture()
  digest <- .expressionContentDigest(fx$counts, 0L)
  expect_match(digest, "^[[:xdigit:]]{32}$")
  expect_identical(.expressionContentDigest(fx$counts, 0L), digest)
  expect_false(identical(.expressionContentDigest(fx$counts, 6L), digest))
  expect_false(identical(.expressionContentDigest(fx$counts, NULL), digest))
  changed <- fx$counts
  changed@x[[1L]] <- changed@x[[1L]] + 1
  expect_false(identical(.expressionContentDigest(changed, 0L), digest))
  renamed <- fx$counts
  rownames(renamed)[[1L]] <- "other"
  expect_false(identical(.expressionContentDigest(renamed, 0L), digest))
  expect_null(.expressionContentDigest(as.matrix(fx$counts), 0L))
})

test_that("an h5 export that is offered no sidecars does not hash the matrix", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  testthat::local_mocked_bindings(
    .expressionContentDigest = function(...) {
      stop("a plain export must not hash the matrix")
    }
  )
  exported <- h5_reuse_export(core_fixture(), sidecars = NULL)
  expect_null(
    rhdf5::h5readAttributes(exported$sidecar, "expression")$content_digest
  )
  expect_true(file.exists(exported$sidecar))
})
