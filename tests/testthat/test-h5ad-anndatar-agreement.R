## The h5ad reader against a reference ----------------------------------------
##
## readH5adAsSCE() is written from the AnnData on-disk specification. The check
## against earlier output of this package shows no regression, but not that it
## agrees with a reference, so the plain fixtures are also read with anndataR
## (the scverse reader) and the matrices, names and obs columns are compared.
## Skipped where anndataR is not installed.

anndata_fixture <- function(name) {
  anndata_test_fixture(paste0(name, ".h5ad"))
}

read_quietly <- function(expr) {
  suppressMessages(suppressWarnings(expr))
}

test_that("readH5adAsSCE() agrees with anndataR on the plain fixtures", {
  skip_if_not_installed("anndataR")
  skip_if_not_installed("SingleCellExperiment")
  ## anndataR keeps the var index as row names; for a file with a gene symbol
  ## column this reader names the rows by the symbols (see small_ensembl).
  names_by_symbol <- "small_ensembl"
  fixtures <- c(
    "small_csr",
    "small_csc",
    "small_dense",
    "small_ensembl",
    "small_spatial",
    "small_visium",
    "small_pseudotime",
    "small_trajectory",
    "small_zarr_twin"
  )
  for (name in fixtures) {
    file <- anndata_fixture(name)
    reference <- read_quietly(
      anndataR::read_h5ad(file, as = "SingleCellExperiment")
    )
    ours <- read_quietly(readH5adAsSCE(file))

    expect_identical(dim(ours), dim(reference), info = name)
    expect_identical(colnames(ours), colnames(reference), info = name)
    if (!name %in% names_by_symbol) {
      expect_identical(rownames(ours), rownames(reference), info = name)
    }

    shared <- intersect(
      SummarizedExperiment::assayNames(reference),
      SummarizedExperiment::assayNames(ours)
    )
    expect_true(length(shared) > 0L, info = name)
    for (assay in shared) {
      expect_equal(
        as.matrix(SummarizedExperiment::assay(ours, assay)),
        as.matrix(SummarizedExperiment::assay(reference, assay)),
        ignore_attr = TRUE,
        info = paste(name, assay)
      )
    }

    reference_obs <- as.data.frame(SummarizedExperiment::colData(reference))
    our_obs <- as.data.frame(SummarizedExperiment::colData(ours))
    expect_setequal(names(our_obs), names(reference_obs))
    for (column in intersect(names(our_obs), names(reference_obs))) {
      expect_equal(
        as.character(our_obs[[column]]),
        as.character(reference_obs[[column]]),
        info = paste(name, column)
      )
    }
  }
})
