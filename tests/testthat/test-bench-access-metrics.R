bench_protocol <- file.path("..", "bench", "benchmark", "core.R")
bench_access_metrics <- bench_protocol

skip_unless_bench_access <- function() {
  testthat::skip_if_not(
    file.exists(bench_protocol) && file.exists(bench_access_metrics),
    "benchmark tree not present (expected when checking a built package)"
  )
  testthat::skip_if_not_installed("Matrix")
}

test_that("query plans stratify genes and fingerprint source values", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  m <- Matrix::sparseMatrix(
    i = c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4),
    j = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4),
    x = seq_len(10),
    dims = c(4, 4),
    dimnames = list(paste0("g", 1:4), paste0("c", 1:4))
  )
  plan <- bench_build_query_plan(m, n_genes = 3L)

  expect_equal(plan$schema_version, 2L)
  expect_equal(plan$n_cells, 4L)
  expect_equal(nrow(plan$panel), 3L)
  expect_equal(plan$panel$role[1], "first")
  expect_identical(plan$subset_cells, 4:1)
  expect_identical(
    plan$reference_row_fingerprint,
    bench_numeric_fingerprint(m[plan$panel$gene[1], ])
  )
  expect_identical(
    plan$reference_block_fingerprint,
    bench_numeric_fingerprint(as.matrix(m[plan$panel$gene, , drop = FALSE]))
  )
  expect_identical(
    plan$reference_subset_block_fingerprint,
    bench_numeric_fingerprint(
      as.matrix(m[plan$panel$gene, plan$subset_cells, drop = FALSE])
    )
  )
})

test_that("Linux peak RSS reads the process high-water mark", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  status <- tempfile("proc-status-")
  on.exit(unlink(status), add = TRUE)
  writeLines(c("Name:\tR", "VmRSS:\t1024 kB", "VmHWM:\t4096 kB"), status)

  expect_equal(bench_peak_rss_mb(status), 4)
  expect_true(is.na(bench_peak_rss_mb(paste0(status, "-missing"))))
})

test_that("the first backend call is the timed fresh-process query", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  rows <- list(
    median = c(0, 2, 0, 4),
    sparse = c(1, 0, 0, 0),
    dense = c(1, 2, 3, 4)
  )
  panel <- data.frame(
    gene = c("median", "sparse", "dense"),
    nnz = c(2, 1, 4),
    role = c("first", "hot", "hot"),
    stringsAsFactors = FALSE
  )
  plan <- list(
    schema_version = 2L,
    panel = panel,
    subset_cells = c(4L, 2L),
    reference_row_fingerprint = bench_numeric_fingerprint(rows$median),
    reference_block_fingerprint = bench_numeric_fingerprint(
      rbind(rows$median, rows$sparse, rows$dense)
    ),
    reference_subset_row_fingerprint = bench_numeric_fingerprint(
      rows$median[c(4L, 2L)]
    ),
    reference_subset_block_fingerprint = bench_numeric_fingerprint(
      rbind(rows$median, rows$sparse, rows$dense)[, c(4L, 2L), drop = FALSE]
    )
  )

  calls <- character()
  obj <- new.env(parent = emptyenv())
  obj$getExpressionRow <- function(gene, cells = NULL) {
    calls <<- c(
      calls,
      paste0("row:", gene, if (is.null(cells)) ":all" else ":subset")
    )
    values <- rows[[gene]]
    if (is.null(cells)) values else values[cells]
  }
  obj$getExpressionBlock <- function(genes, cells = NULL) {
    calls <<- c(calls, paste0("block:", paste(genes, collapse = ",")))
    values <- do.call(rbind, rows[genes])
    if (is.null(cells)) values else values[, cells, drop = FALSE]
  }
  timer <- function(fn) list(seconds = 0.01, value = fn())

  got <- bench_measure_backend(
    obj,
    plan,
    hot_iterations = 2L,
    timer = timer
  )

  expect_equal(calls[1], "row:median:all")
  expect_equal(got$first_query_secs, 0.01)
  expect_equal(got$n_hot, 4L)
  expect_equal(got$correctness, "OK")
  expect_identical(got$row_fingerprint, plan$reference_row_fingerprint)
  expect_identical(got$block_fingerprint, plan$reference_block_fingerprint)
  expect_equal(got$subset_row_secs, 0.01)
  expect_equal(got$subset_block_secs, 0.01)
  expect_identical(
    got$subset_block_fingerprint,
    plan$reference_subset_block_fingerprint
  )
})

test_that("backend measurements fail closed on wrong values", {
  skip_unless_bench_access()
  source(bench_protocol, local = TRUE)
  source(bench_access_metrics, local = TRUE)

  obj <- new.env(parent = emptyenv())
  obj$getExpressionRow <- function(gene, cells = NULL) c(1, 2)
  obj$getExpressionBlock <- function(genes, cells = NULL) {
    matrix(c(1, 2), nrow = 1)
  }
  plan <- list(
    schema_version = 2L,
    panel = data.frame(
      gene = "g1",
      nnz = 2,
      role = "first",
      stringsAsFactors = FALSE
    ),
    subset_cells = 2:1,
    reference_row_fingerprint = "wrong-row",
    reference_block_fingerprint = "wrong-block",
    reference_subset_row_fingerprint = "wrong-subset-row",
    reference_subset_block_fingerprint = "wrong-subset-block"
  )

  expect_error(
    bench_measure_backend(
      obj,
      plan,
      hot_iterations = 1L,
      timer = function(fn) list(seconds = 0, value = fn())
    ),
    "first-query fingerprint mismatch"
  )
})

test_that("fingerprints materialize BPCells blocks canonically", {
  skip_unless_bench_access()
  testthat::skip_if_not_installed("BPCells")
  source(bench_protocol, local = TRUE)

  matrix_dir <- tempfile("bench-bpcells-")
  on.exit(unlink(matrix_dir, recursive = TRUE), add = TRUE)
  sparse <- Matrix::Matrix(
    matrix(c(0, 1, 2, 0, 3, 4), nrow = 2),
    sparse = TRUE
  )
  dimnames(sparse) <- list(c("g1", "g2"), c("c1", "c2", "c3"))
  BPCells::write_matrix_dir(
    methods::as(sparse, "IterableMatrix"),
    matrix_dir
  )
  block <- BPCells::open_matrix_dir(matrix_dir)[1:2, , drop = FALSE]

  expect_identical(
    bench_numeric_fingerprint(block),
    bench_numeric_fingerprint(as.matrix(block))
  )
})
