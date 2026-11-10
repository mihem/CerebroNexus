## Starts fresh R processes: CI only.
skip_on_cran()

test_that("BPCells and HDF5Array work in either namespace load order", {
  skip_if_not_installed("callr")
  packages <- c("BPCells", "HDF5Array")
  for (package in packages) {
    skip_if(
      !nzchar(system.file(package = package)),
      paste(package, "is not installed")
    )
  }
  expected <- matrix(c(0, 2, 3, 0, 5, 6), nrow = 2L)
  for (order in list(packages, rev(packages))) {
    root <- withr::local_tempdir()
    actual <- callr::r(
      function(order, root, expected) {
        for (package in order) {
          loadNamespace(package)
        }
        h5 <- HDF5Array::writeHDF5Array(
          expected,
          filepath = file.path(root, "matrix.h5"),
          name = "expression"
        )
        BPCells::write_matrix_dir(
          mat = Matrix::Matrix(expected, sparse = TRUE),
          dir = file.path(root, "matrix.bpcells")
        )
        bpcells <- BPCells::open_matrix_dir(file.path(root, "matrix.bpcells"))
        list(h5 = as.matrix(h5), bpcells = as.matrix(bpcells))
      },
      args = list(order = order, root = root, expected = expected)
    )
    expect_equal(actual$h5, expected, info = paste(order, collapse = " → "))
    expect_equal(
      actual$bpcells,
      expected,
      info = paste(order, collapse = " → ")
    )
  }
})
