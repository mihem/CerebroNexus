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
  package = "CerebroNexus"
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

test_that("exportFromSeurat: h5 mode writes a TENxMatrix-compatible sibling
           and keeps crb$expression NULL so saveRDS does not embed the
           matrix; round-trips bit-exact via lazy HDF5Array::TENxMatrix", {
  skip_if_not_installed("HDF5Array")
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

  ## crb side: expression stays NULL (no in-memory dgCMatrix payload, so
  ## saveRDS does not embed the matrix and the .crb stays small) and the
  ## backend tag points at the sibling .h5.
  cerebro <- readRDS(outf)
  expect_null(
    cerebro$expression,
    label = "crb$expression must be NULL so saveRDS does not embed the matrix"
  )
  be <- cerebro$getExpressionBackend()
  expect_equal(be$type, "h5")
  expect_equal(be$location, "trip.h5")

  ## h5 side: TENxMatrix-readable. No direct rhdf5 dependency.
  m <- HDF5Array::TENxMatrix(h5_path, group = "expression")
  expect_s4_class(m, "TENxMatrix")
  ## On-disk layout is cells × genes (TENx column-favoured, optimised for
  ## per-gene column reads). Cerebro's internal layout is genes × cells.
  m_internal <- t(m)
  expect_s4_class(m_internal, "DelayedMatrix")

  ## bit-exact round-trip vs the input matrix
  orig <- SeuratObject::GetAssayData(obj_raw, layer = "data")
  expect_equal(dim(m_internal), dim(orig))
  expect_setequal(rownames(m_internal), rownames(orig))
  expect_setequal(colnames(m_internal), colnames(orig))
  realised <- as.matrix(m_internal[rownames(orig), colnames(orig)])
  delta <- max(abs(realised - as.matrix(orig)))
  expect_equal(delta, 0)
})

test_that("exportFromSeurat: h5 mode errors clearly when HDF5Array is missing", {
  skip_if(requireNamespace("HDF5Array", quietly = TRUE))
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$expression_matrix_mode <- "h5"
  expect_error(do.call(exportFromSeurat, args), regexp = "HDF5Array")
})

test_that("h5 attach is lazy: .attachExternalExpression returns a DelayedMatrix
           seed, not an eagerly materialised dgCMatrix (low RAM, instant attach)", {
  skip_if_not_installed("HDF5Array")

  ## source the runtime attach helper from inst/ — it's a Shiny utility,
  ## not part of the package namespace
  inst_util <- system.file(
    "shiny/v1.4/utility_functions.R",
    package = "CerebroNexus"
  )
  if (!nzchar(inst_util)) {
    inst_util <- testthat::test_path(
      "../../inst/shiny/v1.4/utility_functions.R"
    )
  }
  ## load only the symbol we need into a fresh env to avoid namespace pollution
  attach_env <- new.env(parent = globalenv())
  source(inst_util, local = attach_env, echo = FALSE)
  skip_if_not(
    is.function(attach_env$.attachExternalExpression),
    ".attachExternalExpression not found in utility_functions.R"
  )

  out_dir <- file.path(tempdir(), paste0("h5_attach_", as.integer(Sys.time())))
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  outf <- file.path(out_dir, "trip.crb")

  args <- valid_args
  args$file <- outf
  args$expression_matrix_mode <- "h5"
  args$verbose <- FALSE
  do.call(exportFromSeurat, args)

  cerebro <- readRDS(outf)
  expect_null(cerebro$expression)

  attached <- attach_env$.attachExternalExpression(cerebro, outf)

  ## the attach must NOT materialise a dgCMatrix in RAM — that defeats the
  ## entire point of the h5 backend (Roman Hillje's vignette
  ## `create_expression_matrix_in_h5_format.Rmd`).
  expect_false(
    inherits(attached$expression, "dgCMatrix"),
    info = "h5 attach must stay lazy; got an in-memory dgCMatrix"
  )
  expect_s4_class(attached$expression, "DelayedMatrix")

  ## but it should still expose Cerebro's genes × cells layout
  orig <- SeuratObject::GetAssayData(obj_raw, layer = "data")
  expect_equal(nrow(attached$expression), nrow(orig))
  expect_equal(ncol(attached$expression), ncol(orig))
  expect_setequal(rownames(attached$expression), rownames(orig))
  expect_setequal(colnames(attached$expression), colnames(orig))
})
