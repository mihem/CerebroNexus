## Tests for the native h5ad (AnnData) route:
##   .h5ad -> readH5adAsSCE() -> convertH5adToCerebro() -> .crb
##
## The fixtures in fixtures/anndata/ were written by the real anndata/scanpy
## (see make_fixtures.py there); the tests need neither Python nor anndata.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- anndata_test_fixture
read_h5ad <- function(path, ...) {
  suppressWarnings(suppressMessages(readH5adAsSCE(path, ...)))
}
read_fixture <- function(name, ...) read_h5ad(fixture(name), ...)
assay_x <- function(sce, name = "X") SummarizedExperiment::assay(sce, name)

test_that("a CSR h5ad is read as a genes x cells SCE with the right assays", {
  sce <- read_fixture("small_csr.h5ad")
  expect_s4_class(sce, "SingleCellExperiment")
  expect_identical(dim(sce), c(60L, 120L))
  expect_identical(SummarizedExperiment::assayNames(sce), c("X", "counts"))
  expect_identical(rownames(sce)[1:3], c("CD3D", "CD3E", "MS4A1"))
  expect_identical(colnames(sce)[1:2], c("cell000", "cell001"))

  counts <- assay_x(sce, "counts")
  x <- assay_x(sce)
  expect_s4_class(x, "dgCMatrix")
  ## counts are the integers they were written as
  expect_true(all(counts@x == round(counts@x)))
  ## self-checking truth: X is log1p of counts normalised to 1e4
  expected <- log1p(t(t(as.matrix(counts)) / colSums(as.matrix(counts))) * 1e4)
  expect_equal(unname(as.matrix(x)), unname(expected), tolerance = 1e-5)
})

test_that("CSC and dense layouts read identically to CSR", {
  csr <- assay_x(read_fixture("small_csr.h5ad"))
  csc <- assay_x(read_fixture("small_csc.h5ad"))
  dense <- assay_x(read_fixture("small_dense.h5ad"))
  expect_equal(as.matrix(csc), as.matrix(csr))
  expect_equal(as.matrix(dense), as.matrix(csr))
  expect_s4_class(csc, "dgCMatrix")
  expect_s4_class(dense, "dgCMatrix")
  ## the dense fixture carries no layers
  expect_identical(
    SummarizedExperiment::assayNames(read_fixture("small_dense.h5ad")),
    "X"
  )
})

test_that("obs columns of every AnnData encoding are decoded", {
  meta <- SummarizedExperiment::colData(read_fixture("small_csr.h5ad"))
  expect_true(is.factor(meta$cell_type))
  expect_identical(levels(meta$cell_type), c("T cell", "B cell", "Monocyte"))
  expect_identical(
    as.character(meta$cell_type[c(1, 41, 81)]),
    c("T cell", "B cell", "Monocyte")
  )
  expect_true(is.factor(meta$donor))
  expect_identical(levels(meta$donor), c("d0", "d1", "d2"))
  ## categorical with missing values: NaN on every 4th cell
  expect_identical(sum(is.na(meta$batch_note)), 30L)
  expect_true(is.na(meta$batch_note[[1]]))
  ## bool array
  expect_type(meta$is_flagged, "logical")
  expect_identical(sum(meta$is_flagged), 24L)
  ## nullable integer: missing on every 7th cell
  expect_type(meta$n_flag, "integer")
  expect_identical(sum(is.na(meta$n_flag)), 18L)
  expect_identical(meta$n_flag[[2]], 1L)
  expect_type(meta$total_counts, "double")
})

test_that("obsm becomes reducedDims: names mapped, 1-column entries skipped", {
  sce <- read_fixture("small_csr.h5ad")
  expect_setequal(SingleCellExperiment::reducedDimNames(sce), c("UMAP", "PCA"))
  umap <- SingleCellExperiment::reducedDim(sce, "UMAP")
  expect_identical(dim(umap), c(120L, 2L))
  expect_identical(rownames(umap), colnames(sce))
  expect_equal(unname(umap[1, ]), c(5, 0), tolerance = 1e-12)
  expect_identical(ncol(SingleCellExperiment::reducedDim(sce, "PCA")), 5L)
})

test_that("scanpy rank_genes_groups becomes one marker table per groupby", {
  markers <- read_fixture("small_csr.h5ad")@metadata$marker_genes
  expect_identical(names(markers), "scanpy: t-test")
  expect_identical(names(markers[[1]]), "cell_type")
  table <- markers[[1]][["cell_type"]]
  expect_identical(names(table)[1:2], c("cell_type", "gene"))
  expect_setequal(unique(table$cell_type), c("T cell", "B cell", "Monocyte"))
  expect_true(all(
    c("score", "avg_log2FC", "p_val", "p_val_adj") %in% names(table)
  ))
  expect_identical(nrow(table), 24L)
  ## the planted marker genes lead their own cell type
  top <- function(type) table$gene[table$cell_type == type][1:2]
  expect_true(all(top("T cell") %in% c("CD3D", "CD3E")))
  expect_true(all(top("B cell") %in% c("MS4A1", "CD79A")))
  expect_true(all(top("Monocyte") %in% c("LYZ", "S100A8")))
  ## files without scanpy results carry no marker payload
  expect_null(read_fixture("small_csc.h5ad")@metadata$marker_genes)
})

test_that("unsorted CSR indices are repaired, not rejected", {
  copy <- file.path(withr::local_tempdir(), "unsorted.h5ad")
  file.copy(fixture("small_csr.h5ad"), copy)
  indptr <- as.vector(rhdf5::h5read(copy, "X/indptr"))
  indices <- as.vector(rhdf5::h5read(copy, "X/indices"))
  data <- as.vector(rhdf5::h5read(copy, "X/data"))
  first <- seq(indptr[[1]] + 1L, indptr[[2]])
  skip_if(length(first) < 2L, "first cell has fewer than two non-zeros")
  indices[first] <- rev(indices[first])
  data[first] <- rev(data[first])
  rhdf5::h5write(as.integer(indices), copy, "X/indices")
  rhdf5::h5write(as.numeric(data), copy, "X/data")
  expect_equal(
    as.matrix(assay_x(read_h5ad(copy))),
    as.matrix(assay_x(read_fixture("small_csr.h5ad")))
  )
})

test_that("readH5adAsSCE reports bad input clearly", {
  dir <- withr::local_tempdir()
  expect_error(readH5adAsSCE(file.path(dir, "nope.h5ad")), "not found")
  junk <- file.path(dir, "junk.h5ad")
  writeLines("not hdf5", junk)
  expect_error(readH5adAsSCE(junk), "not a readable HDF5")
  expect_error(
    readH5adAsSCE(fixture("small_csr.h5ad"), use_raw = TRUE),
    "no `raw/X`"
  )
})

test_that("convertH5adToCerebro exports the whole file, QC columns found by name", {
  out <- withr::local_tempdir()
  messages <- character()
  path <- withCallingHandlers(
    suppressWarnings(convertH5adToCerebro(
      fixture("small_csr.h5ad"),
      out,
      groups = c("cell_type", "donor", "batch_note"),
      viewer_binary = "never",
      verbose = FALSE
    )),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("Computed missing QC column", messages)))
  expect_identical(basename(path), "cerebro_small_csr.crb")
  obj <- readCerebro(path)
  expect_identical(obj$getExperiment()$experiment_name, "small_csr")
  expect_identical(nrow(obj$getMetaData()), 120L)
  expect_setequal(obj$getGroups(), c("cell_type", "donor", "batch_note"))
  expect_true("N/A" %in% obj$getGroupLevels("batch_note"))
  ## scanpy's total_counts / n_genes_by_counts were used as nUMI / nGene
  meta <- obj$getMetaData()
  counts <- as.matrix(assay_x(read_fixture("small_csr.h5ad"), "counts"))
  expect_equal(unname(meta$nUMI), unname(colSums(counts)), tolerance = 1e-5)
  expect_equal(unname(meta$nGene), unname(colSums(counts > 0)))
  ## UMAP only: PCA is dropped when a non-PCA embedding exists
  expect_identical(obj$availableProjections(), "UMAP")
  expect_identical(obj$getMethodsForMarkerGenes(), "scanpy: t-test")
  expect_identical(obj$getGroupsWithMarkerGenes("scanpy: t-test"), "cell_type")
  expect_setequal(obj$getGroupsWithMostExpressedGenes(), obj$getGroups())
})

test_that("convertH5adToCerebro picks layers and backends, rejects wrong calls", {
  skip_if_not_installed("HDF5Array")
  out <- withr::local_tempdir()
  path <- suppressMessages(suppressWarnings(convertH5adToCerebro(
    fixture("small_csr.h5ad"),
    out,
    assay = "counts",
    groups = "cell_type",
    expression_matrix_mode = "h5",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_true(file.exists(file.path(out, "cerebro_small_csr.h5")))
  obj <- readCerebro(path)
  counts <- as.matrix(assay_x(read_fixture("small_csr.h5ad"), "counts"))
  expect_equal(
    unname(as.matrix(obj$getExpressionBlock(c("CD3D", "LYZ")))),
    unname(counts[c("CD3D", "LYZ"), ])
  )

  expect_error(
    convertH5adToCerebro(fixture("small_csr.h5ad"), out),
    "Character/factor columns in colData"
  )
  expect_error(
    convertH5adToCerebro(
      fixture("small_csr.h5ad"),
      out,
      assay = "nope",
      groups = "cell_type"
    ),
    "Available: X, counts"
  )
  rds <- file.path(out, "x.rds")
  saveRDS(1, rds)
  expect_error(
    convertH5adToCerebro(rds, out, groups = "x"),
    "must be a .h5ad \\(or MuData .h5mu\\) file"
  )
})

test_that("convertSCEToCerebro accepts an .h5ad path directly", {
  out <- withr::local_tempdir()
  path <- suppressMessages(suppressWarnings(convertSCEToCerebro(
    fixture("small_csc.h5ad"),
    out,
    assay = "X",
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(basename(path), "cerebro_small_csc.crb")
})

test_that("a metadata-only read keeps every axis and annotation but no values", {
  full <- read_fixture("small_csr.h5ad")
  shell <- read_fixture("small_csr.h5ad", .metadata_only = TRUE)

  expect_identical(dim(shell), dim(full))
  expect_identical(dimnames(shell), dimnames(full))
  expect_identical(
    SummarizedExperiment::assayNames(shell),
    SummarizedExperiment::assayNames(full)
  )
  expect_identical(
    as.data.frame(SummarizedExperiment::colData(shell)),
    as.data.frame(SummarizedExperiment::colData(full))
  )
  expect_identical(
    SingleCellExperiment::reducedDimNames(shell),
    SingleCellExperiment::reducedDimNames(full)
  )
  for (name in SummarizedExperiment::assayNames(shell)) {
    placeholder <- assay_x(shell, name)
    expect_s4_class(placeholder, "dgCMatrix")
    expect_identical(length(placeholder@x), 0L)
  }
  deferred <- S4Vectors::metadata(shell)$.cerebro_deferred_h5ad
  expect_identical(
    deferred$file,
    normalizePath(fixture("small_csr.h5ad"), winslash = "/")
  )
  expect_identical(deferred$assays, SummarizedExperiment::assayNames(full))
  expect_identical(c(deferred$n_genes, deferred$n_cells), dim(full))
  expect_null(S4Vectors::metadata(full)$.cerebro_deferred_h5ad)
})

## Variants of small_csr.h5ad: X alone (log-normalised), or the integer counts
## moved into X with no layers.
h5ad_variant <- function(kind) {
  path <- withr::local_tempfile(
    fileext = ".h5ad",
    .local_envir = parent.frame()
  )
  file.copy(fixture("small_csr.h5ad"), path)
  handle <- rhdf5::H5Fopen(path)
  on.exit(rhdf5::H5Fclose(handle), add = TRUE)
  if (identical(kind, "x_counts")) {
    rhdf5::H5Ldelete(handle, "X")
    rhdf5::H5Lmove(handle, "layers/counts", handle, "X")
  } else {
    rhdf5::H5Ldelete(handle, "layers/counts")
  }
  path
}

test_that("a converted metadata-only read matches the full conversion", {
  paths <- list(
    with_counts = fixture("small_csr.h5ad"),
    ## a CSC X does not matter while the counts layer is CSR
    csc_x = fixture("small_csc.h5ad"),
    x_only = h5ad_variant("x_only"),
    x_counts = h5ad_variant("x_counts")
  )
  for (case in names(paths)) {
    full <- suppressMessages(convertSCEToSeurat(read_h5ad(paths[[case]])))
    shell <- suppressMessages(convertSCEToSeurat(
      read_h5ad(paths[[case]], .metadata_only = TRUE)
    ))
    assay <- SeuratObject::DefaultAssay(full)
    expect_identical(SeuratObject::DefaultAssay(shell), assay, info = case)
    expect_identical(dimnames(shell), dimnames(full), info = case)
    expect_identical(
      sort(SeuratObject::Layers(shell[[assay]])),
      sort(SeuratObject::Layers(full[[assay]])),
      info = case
    )
    expect_identical(
      colnames(shell@meta.data),
      colnames(full@meta.data),
      info = case
    )
    for (column in grep(
      "^n(Count|Feature)_",
      colnames(full@meta.data),
      value = TRUE
    )) {
      expect_equal(
        shell@meta.data[[column]],
        full@meta.data[[column]],
        tolerance = 1e-10,
        info = paste(case, column)
      )
    }
    expect_identical(
      names(shell@reductions),
      names(full@reductions),
      info = case
    )
    deferred <- shell@misc$.cerebro_deferred_h5ad
    expect_null(deferred$cell_totals)
    expect_true(all(deferred$stored_values > 0))
  }
  ## X alone is classified like the full read samples it.
  x_counts <- read_h5ad(paths$x_counts, .metadata_only = TRUE)
  expect_true(S4Vectors::metadata(x_counts)$.cerebro_deferred_h5ad$x_is_counts)
  x_only <- read_h5ad(paths$x_only, .metadata_only = TRUE)
  expect_false(S4Vectors::metadata(x_only)$.cerebro_deferred_h5ad$x_is_counts)
})

test_that("layouts a metadata-only read cannot stand in for are refused", {
  expect_error(
    read_h5ad(fixture("small_dense.h5ad"), .metadata_only = TRUE),
    class = "cerebro_h5ad_metadata_ineligible"
  )
  ## An h5ad with no assays is ineligible rather than failing with a subscript error.
  ctx <- .h5adContext(fixture("small_csr.h5ad"))
  expect_error(
    .h5adDeferredFacts(ctx, "X", character(), data.frame(), 1L, 1L),
    class = "cerebro_h5ad_metadata_ineligible"
  )
  ## A non-CSR layer (e.g. dense X with obs totals) is never recorded as a
  ## streamable CSR layer_source, so the Builder hydrates it in full instead.
  dense_ctx <- .h5adContext(fixture("small_dense.h5ad"))
  obs_with_totals <- data.frame(
    total_counts = c(10, 20),
    n_genes_by_counts = c(2, 3)
  )
  facts <- .h5adDeferredFacts(
    dense_ctx,
    "X",
    "logcounts",
    obs_with_totals,
    2L,
    2L
  )
  expect_null(facts$layer_sources$data)
})

test_that("streamed cell totals equal the column sums of the matrix", {
  ctx <- .h5adContext(fixture("small_csr.h5ad"))
  sce <- read_h5ad(fixture("small_csr.h5ad"))
  counts <- assay_x(sce, "counts")
  source <- .h5adStreamSource(
    ctx$file,
    "layers/counts",
    ncol(counts),
    nrow(counts)
  )
  withr::local_options(cerebro.h5ad_stream_block = 97)
  totals <- .h5adCellTotals(source, nrow(counts))
  expect_equal(totals$count, unname(Matrix::colSums(counts)))
  expect_equal(totals$features, unname(Matrix::colSums(counts > 0)))
})

test_that("streamed cell totals add up repeated and unsorted genes first", {
  path <- withr::local_tempfile(fileext = ".h5")
  rhdf5::h5createFile(path)
  rhdf5::h5createGroup(path, "X")
  ## cell 1: gene 2 twice (+3, -3) and gene 0; cell 2: empty; cell 3: unsorted
  indptr <- c(0L, 3L, 3L, 6L)
  indices <- c(2L, 0L, 2L, 4L, 1L, 3L)
  data <- c(3, 1, -3, 2, 5, 0)
  rhdf5::h5write(indptr, path, "X/indptr")
  rhdf5::h5write(indices, path, "X/indices")
  rhdf5::h5write(data, path, "X/data")
  source <- .h5adStreamSource(path, "X", n_obs = 3, n_vars = 5)
  expected <- Matrix::sparseMatrix(
    i = indices + 1L,
    j = rep.int(1:3, diff(indptr)),
    x = data,
    dims = c(5L, 3L)
  )
  for (block in c(1, 100)) {
    withr::local_options(cerebro.h5ad_stream_block = block)
    totals <- .h5adCellTotals(source, 5L)
    expect_equal(totals$count, unname(Matrix::colSums(expected)))
    expect_equal(totals$features, unname(Matrix::colSums(expected > 0)))
  }
})

test_that("convertSCEToSeurat matches CreateSeuratObject identities and metadata", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("SeuratObject")
  ## Case 1: <=100 unique prefixes -> orig.ident uses the prefix before "_"
  cells_small <- c("s1_1", "s1_2", "s2_1", "s2_2")
  mat_small <- Matrix::Matrix(
    c(1, 0, 2, 3, 0, 1, 0, 4),
    nrow = 2L,
    ncol = 4L,
    sparse = TRUE,
    dimnames = list(c("G1", "G2"), cells_small)
  )
  sce_small <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = mat_small),
    colData = S4Vectors::DataFrame(
      group = c("A", "A", "B", "B"),
      row.names = cells_small
    )
  )
  seu_small <- suppressMessages(convertSCEToSeurat(sce_small))
  ref_small <- suppressWarnings(SeuratObject::CreateSeuratObject(
    counts = SeuratObject::CreateAssay5Object(counts = mat_small),
    meta.data = data.frame(
      group = c("A", "A", "B", "B"),
      row.names = cells_small
    )
  ))
  expect_true(methods::validObject(seu_small))
  expect_identical(seu_small$orig.ident, ref_small$orig.ident)
  expect_identical(
    SeuratObject::Idents(seu_small),
    SeuratObject::Idents(ref_small)
  )
  expect_identical(seu_small@meta.data, ref_small@meta.data)

  ## Case 2: >100 unique prefixes -> orig.ident falls back to "SeuratProject"
  cells_many <- paste0("p", seq_len(105L), "_1")
  mat_many <- Matrix::Matrix(
    1,
    nrow = 2L,
    ncol = 105L,
    sparse = TRUE,
    dimnames = list(c("G1", "G2"), cells_many)
  )
  sce_many <- SingleCellExperiment::SingleCellExperiment(
    assays = list(counts = mat_many),
    colData = S4Vectors::DataFrame(
      group = rep("A", 105L),
      row.names = cells_many
    )
  )
  seu_many <- suppressMessages(convertSCEToSeurat(sce_many))
  expect_true(methods::validObject(seu_many))
  expect_identical(levels(seu_many$orig.ident), "SeuratProject")
  expect_identical(
    unname(as.character(seu_many$orig.ident)),
    rep("SeuratProject", 105L)
  )
})
