## Starts a separate R process, like test-expression-matrix-first-call.R: CI only.
skip_on_cran()

test_that("the expression matrix of a read CRB has dimensions in a fresh session", {
  skip_if_not_installed("callr")
  crb <- system.file("extdata/examples/example.crb", package = "CerebroNexus")
  skip_if_not(nzchar(crb), "example.crb not installed")
  ## A fresh process, so no earlier test has loaded Matrix.
  result <- callr::r(
    function(file) {
      object <- CerebroNexus::readCerebro(file)
      ## nrow() first: is() or any Matrix call would load Matrix itself.
      rows <- nrow(object$expression)
      cols <- ncol(object$expression)
      list(
        sparse = identical(attr(class(object$expression), "package"), "Matrix"),
        rows = rows,
        cols = cols,
        cells = nrow(object$meta_data)
      )
    },
    args = list(file = crb),
    libpath = .libPaths()
  )
  skip_if_not(isTRUE(result$sparse), "example.crb has no sparse expression")
  expect_gt(result$rows, 0L)
  expect_identical(result$cols, result$cells)
})
