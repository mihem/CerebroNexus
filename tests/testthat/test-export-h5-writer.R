## The H5 expression sidecar is written from the arrays of a dgCMatrix. The
## file must be the one HDF5Array::writeTENxMatrix() writes: the Viewer reads
## it as a TENxMatrix and its speed depends on the chunk layout.

h5_dataset_layout <- function(path, dataset) {
  file <- rhdf5::H5Fopen(path, flags = "H5F_ACC_RDONLY")
  on.exit(rhdf5::H5Fclose(file), add = TRUE)
  handle <- rhdf5::H5Dopen(file, dataset)
  on.exit(rhdf5::H5Dclose(handle), add = TRUE, after = FALSE)
  properties <- rhdf5::H5Dget_create_plist(handle)
  on.exit(rhdf5::H5Pclose(properties), add = TRUE, after = FALSE)
  type <- rhdf5::H5Dget_type(handle)
  list(
    chunk = rhdf5::H5Pget_chunk(properties),
    filters = rhdf5::H5Pget_nfilters(properties),
    class = rhdf5::H5Tget_class(type),
    size = rhdf5::H5Tget_size(type)
  )
}

test_that("the H5 expression writer matches writeTENxMatrix", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  set.seed(3)
  cells_by_genes <- abs(Matrix::rsparsematrix(700L, 40L, 0.1))
  cells_by_genes@x <- round(cells_by_genes@x * 10) + 1
  ## a gene nobody expresses, first, last and in the middle
  cells_by_genes[, c(1L, 17L, 40L)] <- 0
  cells_by_genes <- Matrix::drop0(cells_by_genes)
  dimnames(cells_by_genes) <- list(paste0("c", 1:700), paste0("g", 1:40))

  for (level in list(0L, 6L)) {
    expected <- withr::local_tempfile(fileext = ".h5")
    HDF5Array::writeTENxMatrix(
      cells_by_genes,
      expected,
      group = "expression",
      level = level
    )
    ## slices that do not divide the arrays evenly
    for (slice in c(2^24, 97)) {
      written <- withr::local_tempfile(fileext = ".h5")
      .writeTENxExpression(
        cells_by_genes,
        written,
        group = "expression",
        level = level,
        slice = slice
      )
      for (dataset in c(
        "data",
        "indices",
        "indptr",
        "shape",
        "genes",
        "barcodes"
      )) {
        name <- paste0("expression/", dataset)
        info <- paste("level", level, "slice", slice, dataset)
        expect_identical(
          as.vector(rhdf5::h5read(written, name)),
          as.vector(rhdf5::h5read(expected, name)),
          info = info
        )
        expect_identical(
          h5_dataset_layout(written, name),
          h5_dataset_layout(expected, name),
          info = info
        )
      }
      expect_identical(
        as.matrix(HDF5Array::TENxMatrix(written, group = "expression")),
        as.matrix(cells_by_genes)
      )
    }
  }
})

test_that("the H5 expression writer handles a matrix without values", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  empty <- Matrix::sparseMatrix(
    i = integer(),
    j = integer(),
    x = numeric(),
    dims = c(5L, 3L),
    dimnames = list(paste0("c", 1:5), paste0("g", 1:3))
  )
  written <- withr::local_tempfile(fileext = ".h5")
  .writeTENxExpression(empty, written, group = "expression", level = 0L)
  expect_identical(
    as.matrix(HDF5Array::TENxMatrix(written, group = "expression")),
    as.matrix(empty)
  )
})

test_that("a matrix that is not a dgCMatrix goes through writeTENxMatrix", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  dense <- matrix(
    c(0, 2, 0, 1, 0, 3),
    nrow = 3L,
    dimnames = list(paste0("c", 1:3), paste0("g", 1:2))
  )
  written <- withr::local_tempfile(fileext = ".h5")
  .writeTENxExpression(
    DelayedArray::DelayedArray(dense),
    written,
    group = "expression",
    level = 0L
  )
  expect_identical(
    as.matrix(HDF5Array::TENxMatrix(written, group = "expression")),
    dense
  )
})
