# Guards for the pure planning/ordering logic in tests/bench/benchmark/core.R.
#
# Only the functions that need no network are exercised here. The benchmark tree
# is .Rbuildignore'd, so these skip when running against a built tarball.

bench_lib <- file.path("..", "bench", "benchmark", "core.R")

skip_unless_bench <- function() {
  testthat::skip_if_not(
    file.exists(bench_lib),
    "benchmark tree not present (expected when checking a built package)"
  )
  testthat::skip_if_not_installed("rhdf5")
  testthat::skip_if_not_installed("Matrix")
}

test_that("chunk plans cover the requested cells without overlapping", {
  skip_unless_bench()
  source(bench_lib, local = TRUE)

  plan <- bench_plan_chunks(1000000, 40000, n_chunks = 4L)
  expect_equal(sum(plan$size), 40000)
  expect_equal(nrow(plan), 4L)
  expect_true(all(plan$start >= 1))
  # Every chunk must end inside the file.
  expect_true(all(plan$start + plan$size - 1 <= 1000000))
  # And chunks must not run into each other.
  ends <- plan$start + plan$size - 1
  expect_true(all(plan$start[-1] > ends[-length(ends)]))
  # Starts should be spread out rather than bunched at the head.
  expect_gt(diff(range(plan$start)), 100000)
})

test_that("chunk plans degrade gracefully at the edges", {
  skip_unless_bench()
  source(bench_lib, local = TRUE)

  # Asking for more cells than exist yields the whole file, once.
  plan <- bench_plan_chunks(5000, 10000, n_chunks = 4L)
  expect_equal(sum(plan$size), 5000)
  expect_equal(plan$start[1], 1)

  # Tiny requests collapse to a single contiguous run rather than splitting
  # into chunks smaller than a thousand cells.
  plan <- bench_plan_chunks(1000000, 500, n_chunks = 8L)
  expect_equal(nrow(plan), 1L)
  expect_equal(sum(plan$size), 500)

  # Uneven divisions still add up.
  plan <- bench_plan_chunks(1000000, 4003, n_chunks = 4L)
  expect_equal(sum(plan$size), 4003)
})

test_that("sampled feature names match Seurat normalization", {
  skip_unless_bench()
  source(bench_lib, local = TRUE)

  genes <- c("Metazoa_SRP.34", "A|B", "A_B", "A-B")
  expect_identical(
    bench_seurat_feature_names(genes),
    c("Metazoa-SRP.34", "A-B", "A-B.1", "A-B.2")
  )
})

test_that("within-column ordering is classified correctly", {
  skip_unless_bench()
  source(bench_lib, local = TRUE)

  # Three columns of four entries each.
  p <- c(0L, 4L, 8L, 12L)
  asc <- c(1L, 5L, 9L, 20L, 2L, 3L, 7L, 8L, 0L, 1L, 2L, 3L)
  expect_equal(bench_column_order(asc, p), "ascending")

  # The 10x 1.3M-neuron file looks like this.
  expect_equal(bench_column_order(rev(asc), p), "descending")

  mixed <- c(1L, 5L, 9L, 20L, 8L, 7L, 3L, 2L, 0L, 1L, 2L, 3L)
  expect_equal(bench_column_order(mixed, p), "mixed")

  # A matrix with no column holding more than one entry is trivially ordered.
  expect_equal(bench_column_order(c(3L, 1L), c(0L, 1L, 2L)), "ascending")
})

test_that("reversing a descending matrix preserves its contents", {
  skip_unless_bench()
  source(bench_lib, local = TRUE)

  # Reference matrix, ascending within columns.
  set.seed(1)
  ref <- Matrix::rsparsematrix(20, 6, density = 0.4, rand.x = function(n) {
    seq_len(n)
  })
  ref <- methods::as(ref, "CsparseMatrix")

  # Rebuild it the way the 10x file stores it: descending within each column.
  i_desc <- unlist(lapply(seq_len(ncol(ref)), function(j) {
    rev(ref@i[(ref@p[j] + 1):ref@p[j + 1]])
  }))
  x_desc <- unlist(lapply(seq_len(ncol(ref)), function(j) {
    rev(ref@x[(ref@p[j] + 1):ref@p[j + 1]])
  }))
  expect_equal(bench_column_order(i_desc, ref@p), "descending")

  # This mirrors the correction bench_read_subset() applies: one wholesale
  # reverse, absorbed by reversing the pointers, which flips column order.
  i_fixed <- rev(i_desc)
  x_fixed <- rev(x_desc)
  p_fixed <- cumsum(c(0, rev(diff(ref@p))))
  got <- new(
    "dgCMatrix",
    i = i_fixed,
    p = as.integer(p_fixed),
    x = x_fixed,
    Dim = dim(ref),
    Dimnames = list(NULL, NULL)
  )

  # Same matrix, columns back to front.
  expect_equal(
    as.matrix(got),
    as.matrix(ref)[, rev(seq_len(ncol(ref)))],
    ignore_attr = TRUE
  )
})
