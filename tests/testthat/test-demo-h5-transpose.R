## Slow and starts worker processes: CI only.
skip_on_cran()

demo_h5_helper <- function() {
  testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "h5_transposed_write.R"
  )
}

demo_h5_fixture <- function() {
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
  list(matrix = genes_by_cells, expected = expected)
}

expect_same_h5 <- function(written, expected, info = NULL) {
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
      as.vector(rhdf5::h5read(expected, paste0("expression/", dataset))),
      info = paste(info, dataset)
    )
  }
}

test_that("the demo H5 writer matches writeTENxMatrix on a cell-major source", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  skip_if_not(file.exists(demo_h5_helper()))
  env <- new.env(parent = globalenv())
  sys.source(demo_h5_helper(), envir = env)
  fixture <- demo_h5_fixture()

  ## chunks and gene blocks that do not divide the matrix evenly, one process
  ## and two
  for (workers in c(1L, 2L)) {
    written <- withr::local_tempfile(fileext = ".h5")
    env$.demoWriteH5Transposed(
      fixture$matrix,
      written,
      chunk_cells = 250L,
      block_nnz = 150,
      workers = workers,
      verbose = FALSE
    )
    expect_same_h5(
      written,
      fixture$expected,
      info = paste(workers, "worker(s)")
    )
    expect_identical(.h5CellNames(written), colnames(fixture$matrix))
    expect_identical(.h5GeneNames(written), rownames(fixture$matrix))
    expect_length(
      list.files(dirname(written), pattern = "^h5-transpose-"),
      0L
    )
  }
  expect_identical(
    as.matrix(HDF5Array::TENxMatrix(written, group = "expression")),
    as.matrix(HDF5Array::TENxMatrix(fixture$expected, group = "expression"))
  )
})

test_that("the demo H5 writer reads an AnnData CSR matrix from its arrays", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  skip_if_not(file.exists(demo_h5_helper()))
  env <- new.env(parent = globalenv())
  sys.source(demo_h5_helper(), envir = env)
  fixture <- demo_h5_fixture()

  ## cells x genes stored by cell; the genes of a cell are in reverse order,
  ## as an unsorted source may have them
  by_cell <- methods::as(Matrix::t(fixture$matrix), "RsparseMatrix")
  order <- unlist(lapply(seq_len(nrow(by_cell)), function(row) {
    first <- by_cell@p[[row]] + 1L
    last <- by_cell@p[[row + 1L]]
    if (last >= first) rev(first:last) else integer()
  }))
  h5ad <- withr::local_tempfile(fileext = ".h5ad")
  rhdf5::h5createFile(h5ad)
  rhdf5::h5createGroup(h5ad, "raw")
  rhdf5::h5createGroup(h5ad, "raw/X")
  rhdf5::h5write(by_cell@j[order], h5ad, "raw/X/indices")
  rhdf5::h5write(by_cell@x[order], h5ad, "raw/X/data")
  rhdf5::h5write(as.integer(by_cell@p), h5ad, "raw/X/indptr")
  file_id <- rhdf5::H5Fopen(h5ad)
  group_id <- rhdf5::H5Gopen(file_id, "raw/X")
  rhdf5::h5writeAttribute("csr_matrix", group_id, "encoding-type")
  rhdf5::H5Gclose(group_id)
  rhdf5::H5Fclose(file_id)

  source <- env$.demoAnnDataCsrSource(h5ad, "raw/X")
  expect_false(is.null(source))
  expect_null(env$.demoAnnDataCsrSource(h5ad, "raw"))
  for (workers in c(1L, 2L)) {
    written <- withr::local_tempfile(fileext = ".h5")
    env$.demoWriteH5Transposed(
      fixture$matrix,
      written,
      source = source,
      chunk_cells = 250L,
      block_nnz = 150,
      workers = workers,
      verbose = FALSE
    )
    expect_same_h5(
      written,
      fixture$expected,
      info = paste(workers, "worker(s)")
    )
  }
})
