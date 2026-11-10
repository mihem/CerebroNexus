## Layouts and options found in real public files (pbmc3k, scVelo pancreas,
## CELLxGENE, squidpy MERFISH, scirpy MuData): pre-0.7 AnnData, .h5mu, Ensembl
## IDs, several tissue sections, and the marker table shape.
##
## The legacy file below is written by hand with rhdf5 to the structure seen in
## the real pre-0.7 files (anndata cannot write that layout any more). The
## modern fixtures were written by the real anndata, see
## fixtures/anndata/make_fixtures.py.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- anndata_test_fixture
quiet <- function(x) suppressWarnings(suppressMessages(x))
read_h5ad <- function(path, ...) quiet(readH5adAsSCE(path, ...))

## ---------------------------------------------------------------------------
## pre-0.7 layout
## ---------------------------------------------------------------------------

write_legacy_h5ad <- function(path) {
  set.seed(3)
  n_obs <- 12L
  n_vars <- 6L
  cells <- paste0("c", seq_len(n_obs))
  genes <- c("CD3D", "MS4A1", "LYZ", "GZMB", "PPBP", "ACTB")
  x <- matrix(round(runif(n_vars * n_obs, 0, 4), 3), n_vars, n_obs) # genes x cells in R
  raw <- Matrix::sparseMatrix(
    i = c(1, 3, 2, 4, 6, 5, 1, 2),
    j = c(1, 1, 2, 3, 4, 5, 6, 12),
    x = c(5, 2, 3, 1, 9, 4, 6, 7),
    dims = c(n_vars + 2L, n_obs)
  )
  rhdf5::h5createFile(path)
  rhdf5::h5write(x, path, "X") # a dense (cells x genes) dataset on disk
  rhdf5::h5write(
    data.frame(
      index = cells,
      n_genes = seq_len(n_obs),
      louvain = rep(0:2, each = 4L),
      stringsAsFactors = FALSE
    ),
    path,
    "obs"
  )
  rhdf5::h5write(
    data.frame(
      index = genes,
      n_cells = seq_len(n_vars),
      stringsAsFactors = FALSE
    ),
    path,
    "var"
  )
  rhdf5::h5createGroup(path, "obsm")
  rhdf5::h5write(
    rbind(seq_len(n_obs), rev(seq_len(n_obs))) * 1,
    path,
    "obsm/X_umap"
  )
  ## raw.X (dot, not slash) is a CSR group written with the h5sparse attributes
  csr <- methods::as(Matrix::t(raw), "RsparseMatrix")
  rhdf5::h5createGroup(path, "raw.X")
  rhdf5::h5write(as.numeric(csr@x), path, "raw.X/data")
  rhdf5::h5write(as.integer(csr@j), path, "raw.X/indices")
  rhdf5::h5write(as.integer(csr@p), path, "raw.X/indptr")
  .h5adPutAttrs(
    path,
    "raw.X",
    scalars = list(h5sparse_format = "csr"),
    arrays = list(h5sparse_shape = c(n_obs, n_vars + 2L))
  )
  rhdf5::h5write(
    data.frame(index = c(genes, "EXTRA1", "EXTRA2"), stringsAsFactors = FALSE),
    path,
    "raw.var"
  )
  ## categorical labels live in uns[<column>_categories]
  rhdf5::h5createGroup(path, "uns")
  rhdf5::h5write(c("CD4 T", "B", "Monocyte"), path, "uns/louvain_categories")
  ## rank_genes_groups names its columns by group code, not by label
  rhdf5::h5createGroup(path, "uns/rank_genes_groups")
  top <- data.frame(
    `0` = c("CD3D", "ACTB"),
    `1` = c("MS4A1", "ACTB"),
    `2` = c("LYZ", "ACTB"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  score <- data.frame(
    `0` = c(9, 1),
    `1` = c(8, 1),
    `2` = c(7, 1),
    check.names = FALSE
  )
  rhdf5::h5write(top, path, "uns/rank_genes_groups/names")
  rhdf5::h5write(score, path, "uns/rank_genes_groups/scores")
  rhdf5::h5createGroup(path, "uns/rank_genes_groups/params")
  rhdf5::h5write("louvain", path, "uns/rank_genes_groups/params/groupby")
  rhdf5::h5write("t-test", path, "uns/rank_genes_groups/params/method")
  path
}

test_that("a pre-0.7 h5ad (compound obs/var, uns categories, raw.X) is read", {
  file <- write_legacy_h5ad(file.path(withr::local_tempdir(), "legacy.h5ad"))
  sce <- read_h5ad(file)

  expect_identical(dim(sce), c(6L, 12L))
  expect_identical(colnames(sce)[1:2], c("c1", "c2"))
  expect_identical(
    rownames(sce),
    c("CD3D", "MS4A1", "LYZ", "GZMB", "PPBP", "ACTB")
  )
  ## integer codes become labelled factors via uns/louvain_categories
  louvain <- SummarizedExperiment::colData(sce)$louvain
  expect_s3_class(louvain, "factor")
  expect_identical(levels(louvain), c("CD4 T", "B", "Monocyte"))
  expect_identical(
    as.character(louvain[c(1, 5, 9)]),
    c("CD4 T", "B", "Monocyte")
  )
  expect_identical(SummarizedExperiment::colData(sce)$n_genes, 1:12)
  expect_identical(
    rownames(SingleCellExperiment::reducedDim(sce, "UMAP")),
    colnames(sce)
  )
  expect_identical(
    dim(SingleCellExperiment::reducedDim(sce, "UMAP")),
    c(12L, 2L)
  )
  ## a dense (cells x genes) X is read as genes x cells
  x <- SummarizedExperiment::assay(sce, "X")
  expect_identical(dim(x), c(6L, 12L))
})

test_that("legacy raw.X / raw.var are used by use_raw", {
  file <- write_legacy_h5ad(file.path(
    withr::local_tempdir(),
    "legacy_raw.h5ad"
  ))
  raw <- read_h5ad(file, use_raw = TRUE)
  expect_identical(dim(raw), c(8L, 12L))
  expect_identical(rownames(raw)[7:8], c("EXTRA1", "EXTRA2"))
  m <- as.matrix(SummarizedExperiment::assay(raw, "X"))
  expect_identical(m["CD3D", "c1"], 5)
  expect_identical(m["LYZ", "c1"], 2)
  expect_identical(m["MS4A1", "c12"], 7)
})

test_that("legacy rank_genes_groups maps group codes back to labels", {
  file <- write_legacy_h5ad(file.path(
    withr::local_tempdir(),
    "legacy_markers.h5ad"
  ))
  markers <- read_h5ad(file)@metadata$marker_genes
  expect_identical(names(markers), "scanpy: t-test")
  table <- markers[[1]][["louvain"]]
  expect_identical(names(table)[1:2], c("louvain", "gene"))
  expect_setequal(unique(table$louvain), c("CD4 T", "B", "Monocyte"))
  expect_identical(table$gene[table$louvain == "CD4 T"][[1]], "CD3D")
  expect_identical(table$gene[table$louvain == "B"][[1]], "MS4A1")
  expect_identical(table$score[table$louvain == "Monocyte"][[1]], 7)
})

test_that("a pre-0.7 h5ad converts end to end", {
  file <- write_legacy_h5ad(file.path(
    withr::local_tempdir(),
    "legacy_e2e.h5ad"
  ))
  out <- withr::local_tempdir()
  path <- quiet(convertH5adToCerebro(
    file,
    out,
    groups = "louvain",
    viewer_binary = "never",
    verbose = FALSE
  ))
  obj <- readCerebro(path)
  expect_identical(obj$getGroupLevels("louvain"), c("B", "CD4 T", "Monocyte"))
  expect_identical(obj$getGroupsWithMarkerGenes("scanpy: t-test"), "louvain")
})

## ---------------------------------------------------------------------------
## MuData (.h5mu)
## ---------------------------------------------------------------------------

test_that("a MuData file is read through one modality", {
  file <- fixture("small_mudata.h5mu")
  gex <- read_h5ad(file)
  expect_identical(dim(gex), c(60L, 120L))
  expect_identical(
    SummarizedExperiment::assayNames(gex),
    c("X", "counts")
  )
  expect_identical(rownames(gex)[1:2], c("CD3D", "CD3E"))

  prot <- read_h5ad(file, modality = "prot")
  expect_identical(dim(prot), c(4L, 120L))
  expect_identical(rownames(prot), paste0("P", 0:3))

  expect_error(
    read_h5ad(file, modality = "nope"),
    "not found; the file has: gex, prot"
  )
  expect_error(
    read_h5ad(fixture("small_csr.h5ad"), modality = "gex"),
    "only for MuData"
  )
})

test_that("a MuData file converts to a .crb", {
  out <- withr::local_tempdir()
  path <- quiet(convertH5adToCerebro(
    fixture("small_mudata.h5mu"),
    out,
    modality = "gex",
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  ))
  expect_identical(basename(path), "cerebro_small_mudata.crb")
  expect_identical(nrow(readCerebro(path)$getMetaData()), 120L)
})

## ---------------------------------------------------------------------------
## gene names
## ---------------------------------------------------------------------------

test_that("Ensembl var_names are replaced by symbols from var, and the index is kept", {
  expect_message(
    sce <- readH5adAsSCE(fixture("small_ensembl.h5ad")),
    "Using var column `feature_name`"
  )
  expect_identical(rownames(sce)[1:3], c("CD3D", "CD3E", "MS4A1"))
  expect_match(
    SummarizedExperiment::rowData(sce)$var_index[[1]],
    "^ENSG[0-9]{11}$"
  )
  ## explicit choice, and no switch when the index is already symbols
  explicit <- read_h5ad(fixture("small_ensembl.h5ad"), gene_names = "gene_ids")
  expect_match(rownames(explicit)[[1]], "^ENSG[0-9]{5}$")
  plain <- read_h5ad(fixture("small_csr.h5ad"))
  expect_identical(rownames(plain)[1:2], c("CD3D", "CD3E"))
  expect_error(
    read_h5ad(fixture("small_csr.h5ad"), gene_names = "missing_column"),
    "is not a column of var"
  )
})

test_that("converted CELLxGENE-style data is searchable by symbol", {
  out <- withr::local_tempdir()
  path <- quiet(convertH5adToCerebro(
    fixture("small_ensembl.h5ad"),
    out,
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  ))
  expect_true(all(c("CD3D", "LYZ") %in% readCerebro(path)$getGeneNames()))
})

## ---------------------------------------------------------------------------
## spatial coordinates and sections
## ---------------------------------------------------------------------------

test_that("obsm['spatial'] fills the Spatial tab, one entry per section when asked", {
  out <- withr::local_tempdir()
  one <- readCerebro(quiet(convertH5adToCerebro(
    fixture("small_spatial.h5ad"),
    file.path(out, "one"),
    groups = c("cell_type", "section"),
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(names(one$spatial), "spatial")
  expect_identical(nrow(one$getSpatialData("spatial", FALSE)$coordinates), 120L)

  split <- readCerebro(quiet(convertH5adToCerebro(
    fixture("small_spatial.h5ad"),
    file.path(out, "split"),
    groups = c("cell_type", "section"),
    spatial_split_by = "section",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(names(split$spatial), c("spatial [s1]", "spatial [s2]"))
  s1 <- split$getSpatialData("spatial [s1]", FALSE)
  expect_identical(nrow(s1$coordinates), 60L)
  expect_identical(dim(s1$expression), c(60L, 60L))
  expect_true(all(is.finite(s1$coordinates$x)))
  ## each section holds exactly its own cells
  meta <- split$getMetaData()
  expect_setequal(
    rownames(s1$coordinates),
    meta$cell_barcode[meta$section == "s1"]
  )
})

test_that("numeric-looking sections sort numerically", {
  sce <- read_h5ad(fixture("small_spatial.h5ad"))
  sce$bregma <- rep(c(-29, -5, 11, 1), length.out = ncol(sce))
  export <- Cerebro$new()
  expression <- SummarizedExperiment::assay(sce, "X")
  export$setMetaData(data.frame(cell_barcode = colnames(sce)))
  .exportAddSpatialFromEmbeddings(
    export,
    list(spatial = SingleCellExperiment::reducedDim(sce, "spatial")),
    expression_data = expression,
    assay = "X",
    layer = "X",
    split = stats::setNames(as.character(sce$bregma), colnames(sce))
  )
  expect_identical(
    names(export$spatial),
    c("spatial [-29]", "spatial [-5]", "spatial [1]", "spatial [11]")
  )
})

test_that("spatial_split_by must be a colData column", {
  sce <- read_h5ad(fixture("small_spatial.h5ad"))
  expect_error(
    quiet(exportFromSCE(
      sce,
      assay = "X",
      file = file.path(withr::local_tempdir(), "x.crb"),
      experiment_name = "x",
      organism = "hg",
      groups = "cell_type",
      nUMI = "total_counts",
      nGene = "n_genes_by_counts",
      spatial_split_by = "nope",
      viewer_binary = "never"
    )),
    "must be a column of colData"
  )
})

## ---------------------------------------------------------------------------
## marker tables and helpful errors
## ---------------------------------------------------------------------------

marker_csv <- function(clusters) {
  path <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      cluster = rep(clusters, each = 2L),
      gene = rep(c("CD3D", "LYZ"), times = length(clusters)),
      avg_log2FC = seq_len(2L * length(clusters)),
      stringsAsFactors = FALSE
    ),
    path,
    row.names = FALSE
  )
  path
}

test_that("a marker file whose cluster column matches a group becomes one table", {
  out <- withr::local_tempdir()
  obj <- readCerebro(quiet(convertSCEToCerebro(
    quiet(readH5adAsSCE(fixture("small_csr.h5ad"))),
    out,
    assay = "X",
    groups = c("cell_type", "donor"),
    marker_file = marker_csv(c("T cell", "B cell", "Monocyte")),
    marker_method = "my method",
    viewer_binary = "never",
    verbose = FALSE,
    .source_name = "x.h5ad"
  )))
  ## one table named after the grouping variable, its first column renamed to it
  expect_identical(obj$getGroupsWithMarkerGenes("my method"), "cell_type")
  table <- obj$getMarkerGenes("my method", "cell_type")
  expect_identical(names(table)[[1]], "cell_type")
  expect_setequal(unique(table$cell_type), c("T cell", "B cell", "Monocyte"))
})

test_that("a marker file matching no group keeps the per-cluster split", {
  out <- withr::local_tempdir()
  obj <- readCerebro(quiet(convertSCEToCerebro(
    quiet(readH5adAsSCE(fixture("small_csr.h5ad"))),
    out,
    assay = "X",
    groups = c("cell_type", "donor"),
    marker_file = marker_csv(c("alpha", "beta")),
    marker_method = "other",
    viewer_binary = "never",
    verbose = FALSE,
    .source_name = "x.h5ad"
  )))
  expect_setequal(obj$getGroupsWithMarkerGenes("other"), c("alpha", "beta"))
})

test_that("a missing reduction is reported with a way out", {
  sce <- read_h5ad(fixture("small_csr.h5ad"))
  SingleCellExperiment::reducedDims(sce) <- NULL
  expect_error(
    quiet(exportFromSCE(
      sce,
      assay = "X",
      file = file.path(withr::local_tempdir(), "x.crb"),
      experiment_name = "x",
      organism = "hg",
      groups = "cell_type",
      nUMI = "total_counts",
      nGene = "n_genes_by_counts",
      viewer_binary = "never"
    )),
    "No dimensional reductions available.*scater::runUMAP"
  )
})

## ---------------------------------------------------------------------------
## further modalities / alternative experiments as extra features
## ---------------------------------------------------------------------------

test_that("extra_modalities reads a MuData modality as an alternative experiment", {
  path <- anndata_test_fixture("small_mudata.h5mu")
  sce <- suppressWarnings(suppressMessages(readH5adAsSCE(
    path,
    modality = "gex",
    extra_modalities = "prot"
  )))
  expect_identical(SingleCellExperiment::altExpNames(sce), "prot")
  expect_equal(dim(SingleCellExperiment::altExp(sce, "prot")), c(4, 120))
  expect_identical(
    colnames(SingleCellExperiment::altExp(sce, "prot")),
    colnames(sce)
  )
  expect_error(
    readH5adAsSCE(path, modality = "gex", extra_modalities = "nope"),
    "not found"
  )
  expect_error(
    readH5adAsSCE(
      anndata_test_fixture("small_csr.h5ad"),
      extra_modalities = "prot"
    ),
    "only for MuData"
  )
})

test_that("extra features reach the .crb as <experiment>:<feature>", {
  path <- anndata_test_fixture("small_mudata.h5mu")
  crb <- readCerebro(suppressWarnings(suppressMessages(convertH5adToCerebro(
    path,
    withr::local_tempdir(),
    modality = "gex",
    extra_modalities = "prot",
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  ))))
  genes <- crb$getGeneNames()
  expect_equal(length(genes), 60 + 4)
  expect_identical(genes[grepl("^prot:", genes)], paste0("prot:P", 0:3))
  plain <- readCerebro(suppressWarnings(suppressMessages(convertH5adToCerebro(
    path,
    withr::local_tempdir(),
    modality = "gex",
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  ))))
  expect_equal(length(plain$getGeneNames()), 60)
  ## the main genes are untouched
  expect_equal(
    as.matrix(crb$getExpressionMatrix(genes = plain$getGeneNames()[1:3])),
    as.matrix(plain$getExpressionMatrix(genes = plain$getGeneNames()[1:3]))
  )
})

test_that("a SingleCellExperiment's altExp can be added by name, and an unknown one is refused", {
  sce <- suppressWarnings(suppressMessages(readH5adAsSCE(
    anndata_test_fixture("small_mudata.h5mu"),
    modality = "gex",
    extra_modalities = "prot"
  )))
  out <- withr::local_tempdir()
  crb <- readCerebro(suppressWarnings(suppressMessages(convertSCEToCerebro(
    sce,
    out,
    assay = "X",
    groups = "cell_type",
    nUMI = "nUMI",
    nGene = "nGene",
    extra_features = "prot",
    viewer_binary = "never",
    verbose = FALSE,
    .source_name = "x.rds"
  ))))
  expect_equal(sum(grepl("^prot:", crb$getGeneNames())), 4)
  expect_error(
    suppressMessages(convertSCEToCerebro(
      sce,
      withr::local_tempdir(),
      assay = "X",
      groups = "cell_type",
      nUMI = "nUMI",
      nGene = "nGene",
      extra_features = "ADT",
      viewer_binary = "never",
      verbose = FALSE,
      .source_name = "x.rds"
    )),
    "not found"
  )
})

test_that("a CSC matrix with unsorted row indices is rebuilt in the right orientation", {
  skip_if_not_installed("rhdf5")
  ## cells x genes (4 x 3), stored by gene (CSC) with the rows of each column out of order
  truth <- matrix(
    c(0, 5, 0, 0, 7, 0, 0, 9, 1, 0, 2, 3),
    nrow = 4L,
    dimnames = NULL
  )
  path <- withr::local_tempfile(fileext = ".h5ad")
  rhdf5::h5createFile(path)
  rhdf5::h5createGroup(path, "X")
  data <- c(5, 7, 9, 3, 1, 2) # rows of column 1: 1; column 2: 1, 3 (reversed); column 3: 3, 2, 0 (reversed)
  indices <- c(1L, 3L, 1L, 3L, 2L, 0L)
  indptr <- c(0L, 1L, 3L, 6L)
  rhdf5::h5write(data, path, "X/data")
  rhdf5::h5write(indices, path, "X/indices")
  rhdf5::h5write(indptr, path, "X/indptr")
  file <- rhdf5::H5Fopen(path)
  group <- rhdf5::H5Gopen(file, "X")
  rhdf5::h5writeAttribute("csc_matrix", group, "encoding-type")
  rhdf5::h5writeAttribute(c(4L, 3L), group, "shape")
  rhdf5::H5Gclose(group)
  rhdf5::H5Fclose(file)
  expected <- matrix(0, 4L, 3L)
  expected[2L, 1L] <- 5
  expected[c(4L, 2L), 2L] <- c(7, 9)
  expected[c(4L, 3L, 1L), 3L] <- c(3, 1, 2)
  ctx <- .h5adContext(path)
  got <- .h5adReadMatrix(ctx, "X", n_obs = 4L, n_vars = 3L)
  ## the reader returns genes x cells
  expect_identical(dim(got), c(3L, 4L))
  expect_equal(as.matrix(got), t(expected), ignore_attr = TRUE)
})

test_that("repeated entries of a CSR matrix are added up and unsorted ones kept", {
  skip_if_not_installed("rhdf5")
  ## cells x genes (2 x 3), CSR: cell 1 has gene 2, gene 0 and gene 2 again
  path <- withr::local_tempfile(fileext = ".h5ad")
  rhdf5::h5createFile(path)
  rhdf5::h5createGroup(path, "X")
  rhdf5::h5write(c(1, 2, 4, 8), path, "X/data")
  rhdf5::h5write(c(2L, 0L, 2L, 1L), path, "X/indices")
  rhdf5::h5write(c(0L, 3L, 4L), path, "X/indptr")
  file <- rhdf5::H5Fopen(path)
  group <- rhdf5::H5Gopen(file, "X")
  rhdf5::h5writeAttribute("csr_matrix", group, "encoding-type")
  rhdf5::h5writeAttribute(c(2L, 3L), group, "shape")
  rhdf5::H5Gclose(group)
  rhdf5::H5Fclose(file)
  got <- .h5adReadMatrix(.h5adContext(path), "X", n_obs = 2L, n_vars = 3L)
  expected <- matrix(c(2, 0, 5, 0, 8, 0), nrow = 3L)
  expect_equal(as.matrix(got), expected, ignore_attr = TRUE)
})
