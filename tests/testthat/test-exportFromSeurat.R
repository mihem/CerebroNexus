## Tests for exportFromSeurat()
##
## Uses the bundled example Seurat object (inst/extdata/examples/pbmc_seurat.rds).
## All tests require Seurat; skipped gracefully when it is not installed.

skip_if_not_installed("Seurat")

## ---------------------------------------------------------------------------
## Load and prepare the shared object once for the whole file
## ---------------------------------------------------------------------------

pbmc_path <- system.file(
  "extdata/examples/pbmc_seurat.rds",
  package = "CerebroNexus"
)
if (!nzchar(pbmc_path)) {
  pbmc_path <- testthat::test_path(
    "../../inst/extdata/examples/pbmc_seurat.rds"
  )
}
obj_raw <- readRDS(pbmc_path)

## Convenience: shared valid call args (no file — added per test)
valid_args <- list(
  object = obj_raw,
  experiment_name = "PBMC test",
  organism = "hg",
  groups = c("sample", "seurat_clusters"),
  nUMI = "nCount_RNA",
  nGene = "nFeature_RNA",
  codec = "rds"
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
  expect_true(inherits(cerebro, "Cerebro"))

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

test_that("exportFromSeurat: the finalize hook completes the object before it is written", {
  outf <- tempfile(fileext = ".crb")
  args <- valid_args
  args$file <- outf
  seen <- NULL
  args$.finalize_export <- function(export) {
    seen <<- class(export)
    export$addParameters("hooked", "yes")
  }

  do.call(exportFromSeurat, args)

  expect_true("Cerebro" %in% seen)
  expect_identical(readRDS(outf)$getParameters()$hooked, "yes")

  failing <- tempfile(fileext = ".crb")
  args$file <- failing
  args$.finalize_export <- function(export) stop("hook failed", call. = FALSE)
  expect_error(do.call(exportFromSeurat, args), "hook failed")
  expect_false(file.exists(failing))
})

test_that("exportFromSeurat names each part of the export as it starts", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  parts <- character()
  args$.on_progress <- function(part) parts <<- c(parts, part)

  do.call(exportFromSeurat, args)

  ## A small dataset gets no Viewer Pack, so that part is not reported.
  expect_identical(
    parts,
    setdiff(.exportProgressParts, "writing the Viewer Pack")
  )

  args$file <- tempfile(fileext = ".crb")
  args$viewer_binary <- "always"
  parts <- character()
  do.call(exportFromSeurat, args)
  expect_identical(parts, .exportProgressParts)
})

test_that("the Viewer Pack of an export agrees with the CRB that is read back", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("qs2")
  ## The pack is built and checked against the object the export holds in
  ## memory. validateViewerPack() reads the file, so it sees any difference
  ## between that object and what is written, for the h5 export that takes the
  ## .publishCerebroExport() path and for either codec.
  for (codec in c("rds", "qs2")) {
    args <- valid_args
    args$file <- file.path(withr::local_tempdir(), "dataset.crb")
    args$expression_matrix_mode <- "h5"
    args$codec <- codec
    args$viewer_binary <- "always"
    do.call(exportFromSeurat, args)

    expect_identical(.cerebroPayloadCodec(args$file), codec, info = codec)
    pack <- .viewerPackPath(args$file)
    expect_true(dir.exists(pack), info = codec)
    expect_true(
      validateViewerPack(args$file, pack, full = TRUE)$valid,
      info = codec
    )
  }
})

test_that("exportFromSeurat coalesces missing and existing N/A group levels", {
  object <- obj_raw
  values <- rep(c("A", "N/A", NA_character_), length.out = ncol(object))
  object$character_with_na <- values
  object$factor_with_na <- factor(values, levels = c("A", "N/A"))
  outf <- tempfile(fileext = ".crb")
  args <- valid_args
  args$object <- object
  args$file <- outf
  args$groups <- c("character_with_na", "factor_with_na")

  expect_no_error(do.call(exportFromSeurat, args))

  cerebro <- readRDS(outf)
  expect_identical(
    cerebro$getGroupLevels("character_with_na"),
    c("A", "N/A")
  )
  expect_identical(
    cerebro$getGroupLevels("factor_with_na"),
    c("A", "N/A")
  )
  expect_false(anyNA(cerebro$getMetaData()$character_with_na))
  expect_false(anyNA(cerebro$getMetaData()$factor_with_na))
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

  args <- valid_args
  args$file <- outf
  args$expression_matrix_mode <- "h5"
  args$verbose <- FALSE

  expect_no_error(do.call(exportFromSeurat, args))
  expect_true(file.exists(outf))

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
  expect_identical(be$location, "trip.h5")
  h5_path <- file.path(out_dir, be$location)
  expect_true(file.exists(h5_path))

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

test_that("exportFromSeurat: h5 mode writes a thin v3 CRB whose cell names live in the H5 file", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")

  out_dir <- withr::local_tempdir()
  outf <- file.path(out_dir, "thin.crb")
  args <- valid_args
  args$file <- outf
  args$expression_matrix_mode <- "h5"
  args$verbose <- FALSE
  do.call(exportFromSeurat, args)

  cells <- colnames(obj_raw)
  h5_path <- file.path(out_dir, "thin.h5")
  expect_setequal(.h5CellNames(h5_path), cells)
  expect_setequal(.h5GeneNames(h5_path), rownames(obj_raw))
  expect_identical(.h5Dim(h5_path), as.integer(c(length(cells), nrow(obj_raw))))

  payload <- readRDS(outf)
  schema <- payload$crb_schema
  expect_identical(schema$version, 3L)
  expect_named(
    schema,
    c("version", "cell_names_hash", "projection_rownames", "n_cells"),
    ignore.order = TRUE
  )
  expect_identical(schema$n_cells, length(cells))
  expect_identical(
    schema$cell_names_hash,
    .cerebroCellNamesHash(.h5CellNames(h5_path))
  )
  expect_false("cell_barcode" %in% names(payload$meta_data))
  expect_null(payload$expression)

  restored <- readCerebro(outf)
  expect_identical(restored$meta_data$cell_barcode, .h5CellNames(h5_path))
  expect_identical(colnames(restored$expression), .h5CellNames(h5_path))
  expect_s4_class(restored$expression, "DelayedMatrix")
})

test_that("exportFromSeurat: an unknown expression mode is rejected", {
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$expression_matrix_mode <- "retired-mode"
  expect_error(do.call(exportFromSeurat, args), "should be one of")
  expect_false(file.exists(args$file))
})

test_that("exportFromSeurat: h5 mode errors clearly when HDF5Array is missing", {
  skip_if(requireNamespace("HDF5Array", quietly = TRUE))
  args <- valid_args
  args$file <- tempfile(fileext = ".crb")
  args$expression_matrix_mode <- "h5"
  expect_error(do.call(exportFromSeurat, args), regexp = "HDF5Array")
})

## Deflate level and stored size of `expression/data`. rhdf5 does not report
## the level, so it is read with h5dump when that is on the PATH.
h5_data_filter <- function(path) {
  fid <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDONLY")
  on.exit(rhdf5::H5Fclose(fid), add = TRUE)
  did <- rhdf5::H5Dopen(fid, "expression/data")
  on.exit(rhdf5::H5Dclose(did), add = TRUE, after = FALSE)
  plist <- rhdf5::H5Dget_create_plist(did)
  on.exit(rhdf5::H5Pclose(plist), add = TRUE, after = FALSE)
  filters <- vapply(
    seq_len(rhdf5::H5Pget_nfilters(plist)),
    function(i) rhdf5::H5Pget_filter(plist, i)[[2]],
    character(1)
  )
  level <- NA_integer_
  if (nzchar(Sys.which("h5dump"))) {
    dump <- system2(
      "h5dump",
      c("-p", "-H", "-d", "/expression/data", shQuote(path)),
      stdout = TRUE
    )
    hit <- regmatches(dump, regexpr("LEVEL [0-9]+", dump))
    if (length(hit)) level <- as.integer(sub("LEVEL ", "", hit[[1]]))
  }
  list(
    filters = filters,
    level = level,
    stored = rhdf5::H5Dget_storage_size(did)
  )
}

test_that("exportFromSeurat: h5 data is written at gzip level 0 by default", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")

  out_dir <- withr::local_tempdir()
  export_h5 <- function(name, ...) {
    args <- valid_args
    args$file <- file.path(out_dir, paste0(name, ".crb"))
    args$expression_matrix_mode <- "h5"
    do.call(exportFromSeurat, c(args, list(...)))
    file.path(out_dir, paste0(name, ".h5"))
  }
  default <- h5_data_filter(export_h5("default"))
  expect_true("deflate" %in% default$filters)

  level6 <- h5_data_filter(export_h5("level6", h5_compression_level = 6))
  expect_true("deflate" %in% level6$filters)
  expect_lt(level6$stored, default$stored / 2)

  skip_if(
    is.na(default$level),
    "needs h5dump to read the deflate level"
  )
  expect_identical(default$level, 0L)
  expect_identical(level6$level, 6L)
})

test_that("exportFromSeurat: rejects an invalid h5_compression_level", {
  for (level in list(-1L, 10L, 1.5, NA_integer_, c(0L, 1L), "6")) {
    args <- valid_args
    args$file <- tempfile(fileext = ".crb")
    args$expression_matrix_mode <- "h5"
    args$h5_compression_level <- level
    expect_error(
      do.call(exportFromSeurat, args),
      "`h5_compression_level` must be one integer from 0 to 9"
    )
  }
})

test_that("h5 attach is lazy: .attachExternalExpression returns a DelayedMatrix
           seed, not an eagerly materialised dgCMatrix (low RAM, instant attach)", {
  skip_if_not_installed("HDF5Array")

  ## The runtime attach helper is a Viewer utility, not part of the package
  ## namespace.
  attach_env <- viewer_runtime_env()
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

test_that("exportFromSeurat keeps the position of the arguments of 5.1.3", {
  released <- c(
    "object",
    "assay",
    "slot",
    "file",
    "experiment_name",
    "organism",
    "groups",
    "main_group",
    "cell_cycle",
    "nUMI",
    "nGene",
    "add_all_meta_data",
    "use_delayed_array",
    "expression_matrix_mode",
    "h5_compression_level",
    "codec",
    "viewer_binary",
    "viewer_binary_threshold",
    "spatial_images",
    "verbose"
  )
  arguments <- names(formals(exportFromSeurat))
  expect_identical(arguments[seq_along(released)], released)
  expect_true(all(
    c("immune_sidecar", "immune_sidecar_threshold") %in%
      arguments[-seq_along(released)]
  ))
})
