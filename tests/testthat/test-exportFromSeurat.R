## Tests for exportFromSeurat()
##
## Uses the bundled example Seurat object (inst/extdata/v1.4/pbmc_seurat.rds).
## All tests require Seurat; skipped gracefully when it is not installed.

skip_if_not_installed("Seurat")

## ---------------------------------------------------------------------------
## Load and prepare the shared object once for the whole file
## ---------------------------------------------------------------------------

pbmc_path <- system.file(
  "extdata/v1.4/pbmc_seurat.rds",
  package = "cerebroAppLite"
)
if (!nzchar(pbmc_path)) {
  pbmc_path <- testthat::test_path("../../inst/extdata/v1.4/pbmc_seurat.rds")
}
obj_raw <- readRDS(pbmc_path)

## Convenience: shared valid call args (no file — added per test)
valid_args <- list(
  object = obj_raw,
  experiment_name = "PBMC test",
  organism = "hg",
  groups = c("sample", "seurat_clusters"),
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA"
)

## ---------------------------------------------------------------------------
## Input validation
## ---------------------------------------------------------------------------

test_that("exportFromSeurat: rejects non-Seurat object", {
  expect_error(
    exportFromSeurat(
      object = list(),
      file = tempfile(fileext = ".crb"),
      experiment_name = "test",
      organism = "hg",
      groups = "sample",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA"
    ),
    regexp = "must be of class 'Seurat'"
  )
})

test_that("exportFromSeurat: rejects missing group column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$groups <- "nonexistent_col"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "Some group columns could not be found"
  )
})

test_that("exportFromSeurat: rejects missing nUMI column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$nUMI <- "missing_nUMI"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "not found in meta data"
  )
})

test_that("exportFromSeurat: rejects missing nGene column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$nGene <- "missing_nGene"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "not found in meta data"
  )
})

test_that("exportFromSeurat: rejects missing assay", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$assay <- "SCT"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "could not be found in provided Seurat"
  )
})

test_that("exportFromSeurat: rejects missing cell_cycle column", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$cell_cycle <- "no_such_phase_col"
  expect_error(
    do.call(exportFromSeurat, args),
    regexp = "Some cell cycle columns could not be found"
  )
})

## ---------------------------------------------------------------------------
## Happy-path integration test
## ---------------------------------------------------------------------------

test_that("exportFromSeurat: produces a valid .crb file from pbmc_seurat.rds", {
  outf <- tempfile(fileext = ".crb")
  args <- valid_args
  args$file <- outf

  expect_no_error(do.call(exportFromSeurat, args))

  ## file must exist and be non-empty
  expect_true(file.exists(outf))
  expect_gt(file.size(outf), 0)

  ## load and inspect the Cerebro object
  cerebro <- readRDS(outf)
  expect_true(inherits(cerebro, "Cerebro_v1.3"))

  ## experiment metadata
  exp <- cerebro$getExperiment()
  expect_equal(exp$experiment_name, "PBMC test")
  expect_equal(exp$organism, "hg")

  ## groups
  groups <- cerebro$getGroups()
  expect_true("sample" %in% groups)
  expect_true("seurat_clusters" %in% groups)

  ## group levels
  expect_true("pbmc_1" %in% cerebro$getGroupLevels("sample"))
  expect_true("pbmc_2" %in% cerebro$getGroupLevels("sample"))

  ## cell count preserved
  expect_equal(nrow(cerebro$getMetaData()), ncol(obj_raw))

  ## projections: UMAP should be present
  projs <- cerebro$availableProjections()
  expect_true(any(grepl("umap|UMAP", projs, ignore.case = TRUE)))

  ## expression matrix: genes x cells
  expr <- cerebro$expression
  expect_false(is.null(expr))
  expect_equal(ncol(expr), ncol(obj_raw))
  expect_equal(nrow(expr), nrow(obj_raw))
})

## ---------------------------------------------------------------------------
## h5 backend round-trip
## ---------------------------------------------------------------------------

test_that("exportFromSeurat: h5 backend writes a sibling .h5 with the
           example.h5 schema and round-trips bit-exact through the runtime
           attach reader", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("Matrix")

  out_dir <- file.path(tempdir(), paste0("h5_rt_", as.integer(Sys.time())))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  outf <- file.path(out_dir, "trip.crb")
  h5_path <- file.path(out_dir, "trip.h5")

  args <- valid_args
  args$file <- outf
  args$expression_matrix_mode <- "h5"
  args$verbose <- FALSE

  expect_no_error(do.call(exportFromSeurat, args))
  expect_true(file.exists(outf))
  expect_true(file.exists(h5_path))

  ## crb side: backend tag points at the sibling .h5
  cerebro <- readRDS(outf)
  be <- cerebro$getExpressionBackend()
  expect_equal(be$type, "h5")
  expect_equal(be$location, "trip.h5")

  ## h5 side: must contain the 6 datasets under /expression/ that
  ## example.h5 carries.
  ls_df <- rhdf5::h5ls(h5_path)
  under_expr <- ls_df$name[ls_df$group == "/expression"]
  for (ds in c("data", "indices", "indptr", "shape", "genes", "barcodes")) {
    expect_true(ds %in% under_expr, info = paste("missing /expression/", ds))
  }

  ## round-trip via the runtime reader logic (mirrors
  ## .attachExternalExpression's h5 branch): on-disk is cells x genes,
  ## internal layout is genes x cells.
  data <- as.numeric(rhdf5::h5read(h5_path, "/expression/data"))
  indices <- as.integer(rhdf5::h5read(h5_path, "/expression/indices"))
  indptr <- as.integer(rhdf5::h5read(h5_path, "/expression/indptr"))
  shape <- as.integer(rhdf5::h5read(h5_path, "/expression/shape"))
  gns <- as.character(rhdf5::h5read(h5_path, "/expression/genes"))
  bcs <- as.character(rhdf5::h5read(h5_path, "/expression/barcodes"))
  rhdf5::H5close()

  m_disk <- Matrix::sparseMatrix(
    i = indices + 1L,
    p = indptr,
    x = data,
    dims = c(shape[1], shape[2]),
    index1 = TRUE
  )
  m_int <- methods::as(Matrix::t(m_disk), "CsparseMatrix")
  rownames(m_int) <- bcs
  colnames(m_int) <- gns

  ## bit-exact reconstruction of the input matrix
  orig <- SeuratObject::GetAssayData(obj_raw, layer = "data")
  expect_equal(dim(m_int), dim(orig))
  expect_setequal(rownames(m_int), rownames(orig))
  expect_setequal(colnames(m_int), colnames(orig))
  delta <- max(abs(
    as.matrix(m_int[rownames(orig), colnames(orig)]) - as.matrix(orig)
  ))
  expect_equal(delta, 0)
})

test_that("exportFromSeurat: h5 mode errors clearly when rhdf5 is missing", {
  skip_if(requireNamespace("rhdf5", quietly = TRUE))
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$expression_matrix_mode <- "h5"
  expect_error(do.call(exportFromSeurat, args), regexp = "rhdf5")
})
