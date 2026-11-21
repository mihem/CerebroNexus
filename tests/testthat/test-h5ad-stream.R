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
    cerebro.h5ad_stream_block = 25,
    cerebro.h5ad_stream_workers = 2
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

test_that("entries naming a cell and a gene twice are added up", {
  ## A CSR row may repeat a gene; the entries add up, as they do in a dgCMatrix.
  ## Every stored entry is split into two halves at the same coordinate, which
  ## is the same matrix.
  doubled <- withr::local_tempfile(fileext = ".h5ad")
  file.copy(anndata_test_fixture("small_csr.h5ad"), doubled)
  indptr <- as.vector(rhdf5::h5read(doubled, "X/indptr"))
  indices <- as.vector(rhdf5::h5read(doubled, "X/indices"))
  data <- as.vector(rhdf5::h5read(doubled, "X/data"))
  for (name in c("indptr", "indices", "data")) {
    rhdf5::h5delete(doubled, paste0("X/", name))
  }
  twice <- rep(seq_along(data), each = 2L)
  rhdf5::h5write(as.integer(indptr * 2L), doubled, "X/indptr")
  rhdf5::h5write(as.integer(indices[twice]), doubled, "X/indices")
  rhdf5::h5write(data[twice] / 2, doubled, "X/data")

  reference <- withr::local_tempdir()
  expected <- quiet(stream_convert(reference, verbose = FALSE))

  withr::local_options(
    cerebro.h5ad_stream_nnz = 0,
    cerebro.h5ad_stream_budget = 40,
    cerebro.h5ad_stream_block = 25,
    cerebro.h5ad_stream_workers = 1
  )
  streamed_dir <- withr::local_tempdir()
  streamed <- quiet(convertH5adToCerebro(
    doubled,
    streamed_dir,
    groups = c("cell_type", "donor"),
    expression_matrix_mode = "h5",
    viewer_binary = "never"
  ))
  expect_equal(read_sidecar(streamed_dir), read_sidecar(reference))
  a <- readCerebro(expected)
  b <- readCerebro(streamed)
  expect_equal(b$mean_expression, a$mean_expression)
  expect_equal(b$most_expressed_genes, a$most_expressed_genes)
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

## The writer on its own, against HDF5Array's writer, on a matrix stored by cell
## (an AnnData CSR; the genes of a cell in reverse order, as an unsorted source
## may have them), with ranges and blocks that do not divide the matrix evenly,
## in one process and in two.
## `float32`: the values stored as 32-bit floats, as scanpy writes them; the
## block files then keep 4 bytes a value.
stream_fixture <- function(float32 = FALSE) {
  set.seed(2)
  genes_by_cells <- abs(Matrix::rsparsematrix(40L, 700L, 0.1))
  genes_by_cells@x <- round(genes_by_cells@x * 10) + 1
  dimnames(genes_by_cells) <- list(paste0("g", 1:40), paste0("c", 1:700))
  expected <- withr::local_tempfile(
    fileext = ".h5",
    .local_envir = parent.frame()
  )
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::t(genes_by_cells), "CsparseMatrix"),
    expected,
    group = "expression",
    level = 0L
  )
  by_cell <- methods::as(Matrix::t(genes_by_cells), "RsparseMatrix")
  order <- unlist(lapply(seq_len(nrow(by_cell)), function(row) {
    first <- by_cell@p[[row]] + 1L
    last <- by_cell@p[[row + 1L]]
    if (last >= first) rev(first:last) else integer()
  }))
  h5ad <- withr::local_tempfile(
    fileext = ".h5ad",
    .local_envir = parent.frame()
  )
  rhdf5::h5createFile(h5ad)
  rhdf5::h5createGroup(h5ad, "raw")
  rhdf5::h5createGroup(h5ad, "raw/X")
  rhdf5::h5write(by_cell@j[order], h5ad, "raw/X/indices")
  if (float32) {
    rhdf5::h5createDataset(
      h5ad,
      "raw/X/data",
      dims = length(order),
      H5type = "H5T_IEEE_F32LE",
      chunk = length(order)
    )
  }
  rhdf5::h5write(by_cell@x[order], h5ad, "raw/X/data")
  rhdf5::h5write(as.integer(by_cell@p), h5ad, "raw/X/indptr")
  list(matrix = genes_by_cells, expected = expected, h5ad = h5ad)
}

test_that("the streaming writer matches writeTENxMatrix on a cell-major source", {
  fixture <- stream_fixture()
  ## the buckets of the block files: one, uneven ones, one per gene; their
  ## compression where R has zstd, and none
  settings <- list(
    list(workers = 1L, buckets = 256L, codec = "none"),
    list(workers = 2L, buckets = 7L, codec = .h5adStreamCodec()),
    list(workers = 2L, buckets = 1L, codec = "none"),
    list(workers = 3L, buckets = 40L, codec = .h5adStreamCodec()),
    list(workers = 2L, buckets = 7L, codec = .h5adStreamCodec(), f32 = TRUE)
  )
  for (setting in settings) {
    if (isTRUE(setting$f32)) {
      fixture <- stream_fixture(float32 = TRUE)
    }
    workers <- setting$workers
    withr::local_options(
      cerebro.h5ad_stream_block = 250,
      cerebro.h5ad_stream_workers = workers,
      cerebro.h5ad_stream_buckets = setting$buckets,
      cerebro.h5ad_stream_codec = setting$codec
    )
    written <- withr::local_tempfile(fileext = ".h5")
    source <- .h5adStreamSource(fixture$h5ad, "raw/X", 700, 40)
    expect_identical(source$value_size, if (isTRUE(setting$f32)) 4L else 8L)
    .h5adStreamWriteTENx(
      source,
      written,
      group = "expression",
      dimnames = list(colnames(fixture$matrix), rownames(fixture$matrix)),
      level = 0L,
      budget = 150
    )
    for (dataset in c(
      "data",
      "indices",
      "indptr",
      "shape",
      "genes",
      "barcodes"
    )) {
      expect_identical(
        as.vector(rhdf5::h5read(written, paste0("expression/", dataset))),
        as.vector(rhdf5::h5read(
          fixture$expected,
          paste0("expression/", dataset)
        )),
        info = paste(
          workers,
          "worker(s),",
          setting$buckets,
          "bucket(s),",
          setting$codec,
          ":",
          dataset
        )
      )
    }
    ## values stored as 32-bit floats stay 32-bit floats in the file
    handle <- rhdf5::H5Fopen(written, flags = "H5F_ACC_RDONLY")
    data <- rhdf5::H5Dopen(handle, "expression/data")
    expect_identical(
      rhdf5::H5Tget_size(rhdf5::H5Dget_type(data)),
      if (isTRUE(setting$f32)) 4L else 8L
    )
    rhdf5::H5Dclose(data)
    rhdf5::H5Fclose(handle)
    expect_length(
      list.files(dirname(written), pattern = "^h5-stream-"),
      0L
    )
  }
})

test_that("the group sums over blocks and workers match the whole matrix", {
  fixture <- stream_fixture()
  source <- .h5adStreamSource(fixture$h5ad, "raw/X", 700, 40)
  members <- Matrix::sparseMatrix(
    i = 1:700,
    j = rep(1:3, length.out = 700),
    x = 1,
    dims = c(700L, 3L)
  )
  expected_sums <- as.matrix(fixture$matrix %*% members)
  positive <- fixture$matrix
  positive@x <- as.numeric(positive@x > 0)
  expected_positive <- as.matrix(positive %*% members)
  for (workers in c(1L, 2L, 3L)) {
    withr::local_options(
      cerebro.h5ad_stream_block = 120,
      cerebro.h5ad_stream_workers = workers
    )
    scratch <- withr::local_tempdir()
    withr::local_options(cerebro.h5ad_stream_scratch = scratch)
    sums <- .h5adStreamGroupSums(source, members)
    expect_equal(sums$sums, expected_sums, ignore_attr = TRUE)
    expect_equal(sums$positive, expected_positive, ignore_attr = TRUE)
    ## the writer takes the blocks the sums left, without reading the source
    expect_false(is.null(source$cache$split))
    written <- withr::local_tempfile(fileext = ".h5")
    .h5adStreamWriteTENx(
      source,
      written,
      group = "expression",
      dimnames = list(colnames(fixture$matrix), rownames(fixture$matrix)),
      level = 0L,
      budget = 150
    )
    expect_null(source$cache$split)
    expect_length(list.files(scratch), 0L)
    for (dataset in c("data", "indices", "indptr")) {
      expect_identical(
        as.vector(rhdf5::h5read(written, paste0("expression/", dataset))),
        as.vector(rhdf5::h5read(
          fixture$expected,
          paste0("expression/", dataset)
        )),
        info = paste(workers, "worker(s):", dataset)
      )
    }
  }
})
