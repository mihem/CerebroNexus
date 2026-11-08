## Tests for exportToH5ad(): .crb -> .h5ad
##
## The format-level checks read the HDF5 structure directly, so they hold
## without Python. The file was also opened with the real anndata/scanpy while
## developing this (29 checks over the Seurat, SCE and scanpy routes).

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- anndata_test_fixture
quiet <- function(x) suppressWarnings(suppressMessages(x))

## A .crb built from the real-anndata fixture, which has every column type.
make_crb <- function(
  mode = "embedded",
  dir = withr::local_tempdir(.local_envir = parent.frame())
) {
  quiet(convertH5adToCerebro(
    fixture("small_csr.h5ad"),
    dir,
    groups = c("cell_type", "donor", "batch_note"),
    expression_matrix_mode = mode,
    viewer_binary = "never",
    verbose = FALSE
  ))
}

test_that("a .crb exports to an h5ad that reads back as the same data", {
  crb <- make_crb()
  out <- file.path(withr::local_tempdir(), "out.h5ad")
  expect_identical(quiet(exportToH5ad(crb, out)), out)
  expect_true(file.exists(out))

  back <- quiet(readH5adAsSCE(out))
  source <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  expect_identical(dim(back), dim(source))
  expect_identical(colnames(back), colnames(source))
  expect_identical(rownames(back), rownames(source))
  expect_equal(
    as.matrix(SummarizedExperiment::assay(back, "X")),
    as.matrix(SummarizedExperiment::assay(source, "X"))
  )
  meta <- SummarizedExperiment::colData(back)
  expect_identical(as.character(meta$cell_type), as.character(source$cell_type))
  expect_identical(as.character(meta$donor), as.character(source$donor))
  ## Cerebro's convention: a missing group value becomes the explicit "N/A"
  expect_identical(sum(meta$batch_note == "N/A"), sum(is.na(source$batch_note)))
  expect_equal(
    SingleCellExperiment::reducedDim(back, "UMAP"),
    SingleCellExperiment::reducedDim(source, "UMAP"),
    ignore_attr = TRUE
  )
  ## nUMI / nGene are the scanpy columns the conversion picked
  expect_equal(meta$nUMI, source$total_counts, tolerance = 1e-5)
})

test_that("the file follows the AnnData on-disk format", {
  out <- file.path(withr::local_tempdir(), "format.h5ad")
  quiet(exportToH5ad(make_crb(), out))
  attrs <- function(path) rhdf5::h5readAttributes(out, path)

  expect_identical(attrs("/")[["encoding-type"]], "anndata")
  x <- attrs("X")
  expect_identical(x[["encoding-type"]], "csr_matrix")
  expect_identical(as.integer(x$shape), c(120L, 60L))
  indptr <- as.vector(rhdf5::h5read(out, "X/indptr"))
  expect_length(indptr, 121L)
  expect_identical(indptr[[1]], 0L)
  expect_length(as.vector(rhdf5::h5read(out, "X/indices")), tail(indptr, 1))

  obs <- attrs("obs")
  expect_identical(obs[["encoding-type"]], "dataframe")
  expect_identical(obs[["_index"]], "_index")
  expect_true(all(
    c("cell_type", "donor", "batch_note") %in% obs[["column-order"]]
  ))
  expect_false("cell_barcode" %in% obs[["column-order"]])
  expect_identical(attrs("obs/cell_type")[["encoding-type"]], "categorical")
  ## Cerebro sorts the levels of a character grouping variable
  expect_identical(
    as.vector(rhdf5::h5read(out, "obs/cell_type/categories")),
    c("B cell", "Monocyte", "T cell")
  )
  ## missing categorical values are code -1
  expect_identical(
    sum(as.vector(rhdf5::h5read(out, "obs/batch_note/codes")) < 0L),
    0L
  )

  ## anndata reads a bool only from an HDF5 enum named FALSE/TRUE; rhdf5
  ## returns such an enum as a factor with exactly those levels
  flagged <- rhdf5::h5read(out, "obs/is_flagged")
  expect_s3_class(flagged, "factor")
  expect_identical(levels(flagged), c("FALSE", "TRUE"))
  expect_identical(sum(flagged == "TRUE"), 24L)
  ## nullable integer: values + enum mask
  expect_identical(attrs("obs/n_flag")[["encoding-type"]], "nullable-integer")
  mask <- rhdf5::h5read(out, "obs/n_flag/mask")
  expect_identical(levels(mask), c("FALSE", "TRUE"))
  expect_identical(sum(mask == "TRUE"), 18L)

  ## cells x dims embeddings (an R k x n matrix lands as HDF5 shape (n, k))
  umap <- rhdf5::h5read(out, "obsm/X_umap")
  expect_identical(dim(umap), c(2L, 120L))
  expect_identical(attrs("obsm/X_umap")[["encoding-type"]], "array")
  expect_identical(attrs("var")[["_index"]], "_index")
  expect_identical(attrs("uns")[["encoding-type"]], "dict")
})

test_that("every expression backend exports the same X", {
  skip_if_not_installed("HDF5Array")
  reference <- quiet(readH5adAsSCE(fixture("small_csr.h5ad")))
  for (mode in c("embedded", "h5")) {
    out <- file.path(withr::local_tempdir(), paste0(mode, ".h5ad"))
    quiet(exportToH5ad(make_crb(mode), out))
    expect_equal(
      as.matrix(SummarizedExperiment::assay(quiet(readH5adAsSCE(out)), "X")),
      as.matrix(SummarizedExperiment::assay(reference, "X")),
      info = mode
    )
  }
})

test_that("marker tables survive a Cerebro -> h5ad -> SCE trip", {
  out <- file.path(withr::local_tempdir(), "markers.h5ad")
  quiet(exportToH5ad(make_crb(), out))
  markers <- quiet(readH5adAsSCE(out))@metadata$marker_genes
  expect_identical(names(markers), "scanpy: t-test")
  table <- markers[[1]][["cell_type"]]
  original <- quiet(readH5adAsSCE(fixture(
    "small_csr.h5ad"
  )))@metadata$marker_genes[[1]][["cell_type"]]
  expect_identical(table$gene, original$gene)
  expect_equal(table$score, original$score)
  expect_identical(
    as.character(table$cell_type),
    as.character(original$cell_type)
  )
})

test_that("a Cerebro object is accepted and unsupported content is named", {
  skip_if(
    !nzchar(system.file(
      "extdata/examples/demo_full_tcr_bcr.crb",
      package = "CerebroNexus"
    )) &&
      !file.exists(testthat::test_path(
        "../../inst/extdata/examples/demo_full_tcr_bcr.crb"
      ))
  )
  path <- system.file(
    "extdata/examples/demo_full_tcr_bcr.crb",
    package = "CerebroNexus"
  )
  if (!nzchar(path)) {
    path <- testthat::test_path(
      "../../inst/extdata/examples/demo_full_tcr_bcr.crb"
    )
  }
  object <- readCerebro(path)
  out <- file.path(withr::local_tempdir(), "demo.h5ad")
  expect_message(
    suppressWarnings(exportToH5ad(object, out)),
    "Not exported to h5ad: .*immune repertoire"
  )
  back <- quiet(readH5adAsSCE(out))
  expect_identical(colnames(back), object$getCellNames())
})

test_that("existing files are protected and replaced only by a finished export", {
  crb <- make_crb()
  dir <- withr::local_tempdir()
  out <- file.path(dir, "keep.h5ad")
  quiet(exportToH5ad(crb, out))
  expect_error(exportToH5ad(crb, out), "overwrite = TRUE")

  before <- tools::md5sum(out)
  ## a failing export must leave the existing file and no stage files behind
  expect_error(
    exportToH5ad(file.path(dir, "missing.crb"), out, overwrite = TRUE),
    "not found"
  )
  expect_identical(unname(tools::md5sum(out)), unname(before))
  expect_error(
    exportToH5ad(1, out, overwrite = TRUE),
    "must be a .crb path or a Cerebro object"
  )
  expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), "keep.h5ad")

  quiet(exportToH5ad(crb, out, overwrite = TRUE))
  expect_true(file.exists(out))
  expect_setequal(list.files(dir, all.files = TRUE, no.. = TRUE), "keep.h5ad")
  expect_error(exportToH5ad(crb, dir), "directory")
  expect_error(exportToH5ad(crb, c("a", "b")), "one non-empty output path")
})
