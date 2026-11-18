## A matrix with more than 2^31 - 1 non-zero values cannot be a dgCMatrix, so the
## h5 route streams an h5ad X into the expression file instead of reading it.
## The threshold, the blocks of cells and the memory budget are options, so that
## a small fixture takes the same path in many blocks and passes.

skip_if_not_installed("rhdf5")
skip_if_not_installed("HDF5Array")
skip_if_not_installed("SingleCellExperiment")

quiet <- function(x) suppressWarnings(suppressMessages(x))

stream_convert <- function(dir, ...) {
  convertH5adToCerebro(
    anndata_test_fixture("small_csr.h5ad"),
    dir,
    groups = c("cell_type", "donor"),
    expression_matrix_mode = "h5",
    viewer_binary = "never",
    ...
  )
}

read_sidecar <- function(dir) {
  file <- list.files(dir, pattern = "[.]h5$", full.names = TRUE)
  expect_length(file, 1L)
  methods::as(
    HDF5Array::TENxMatrix(file, group = "expression"),
    "dgCMatrix"
  )
}

test_that("a streamed matrix gives the same expression file and summaries", {
  plain_dir <- withr::local_tempdir()
  plain <- quiet(stream_convert(plain_dir, verbose = FALSE))

  withr::local_options(
    cerebro.h5ad_stream_nnz = 0,
    cerebro.h5ad_stream_budget = 40,
    cerebro.h5ad_stream_block = 25
  )
  streamed_dir <- withr::local_tempdir()
  expect_message(
    streamed <- suppressWarnings(stream_convert(streamed_dir, verbose = TRUE)),
    "Streaming the expression matrix"
  )

  expect_equal(read_sidecar(streamed_dir), read_sidecar(plain_dir))
  a <- readCerebro(plain)
  b <- readCerebro(streamed)
  expect_equal(b$mean_expression, a$mean_expression)
  expect_equal(b$most_expressed_genes, a$most_expressed_genes)
  expect_identical(b$getExpressionBackend()$type, "h5")
})

test_that("the blocks and passes do not change what is written", {
  reference <- withr::local_tempdir()
  quiet(stream_convert(reference, verbose = FALSE))
  expected <- read_sidecar(reference)
  withr::local_options(cerebro.h5ad_stream_nnz = 0)
  for (setting in list(
    list(budget = 2^29, block = 2^26),
    list(budget = 1, block = 1),
    list(budget = 97, block = 13)
  )) {
    withr::local_options(
      cerebro.h5ad_stream_budget = setting$budget,
      cerebro.h5ad_stream_block = setting$block
    )
    dir <- withr::local_tempdir()
    quiet(stream_convert(dir, verbose = FALSE))
    expect_equal(read_sidecar(dir), expected)
  }
})
