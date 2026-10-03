bench_root_full <- normalizePath(file.path("..", "bench"), mustWork = FALSE)
full_source_lib <- file.path(
  bench_root_full,
  "benchmark",
  "core.R"
)
storage_backends_file <- file.path(
  bench_root_full,
  "benchmark",
  "storage_backends.R"
)

skip_unless_full_source_deps <- function() {
  for (package in c("BPCells", "HDF5Array", "rhdf5", "CerebroNexus")) {
    testthat::skip_if_not_installed(package)
  }
}

full_source_fixture <- function(root) {
  m <- Matrix::Matrix(
    matrix(c(1, 0, 2, 0, 3, 0, 4, 0, 5, 6, 0, 7), nrow = 3),
    sparse = TRUE
  )
  m <- methods::as(m, "dgCMatrix")
  rownames(m) <- paste0("g", seq_len(nrow(m)))
  colnames(m) <- paste0("c", seq_len(ncol(m)))
  iterable <- BPCells::write_matrix_memory(m)
  tenx <- file.path(root, "fixture-10x.h5")
  anndata <- file.path(root, "fixture.h5ad")
  BPCells::write_matrix_10x_hdf5(iterable, tenx, type = "double")
  BPCells::write_matrix_anndata_hdf5(iterable, anndata, group = "X")
  list(matrix = m, tenx = tenx, anndata = anndata)
}

test_that("full-source adapters stay lazy and preserve orientation", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("full-source-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- full_source_fixture(root)

  tenx <- bench_open_full_source(list(kind = "tenx"), fixture$tenx)
  anndata <- bench_open_full_source(list(kind = "h5ad"), fixture$anndata)

  expect_s4_class(tenx, "IterableMatrix")
  expect_s4_class(anndata, "IterableMatrix")
  expect_equal(dim(tenx), dim(fixture$matrix))
  expect_equal(dim(anndata), dim(fixture$matrix))
  expect_identical(dimnames(tenx), dimnames(fixture$matrix))
  expect_identical(dimnames(anndata), dimnames(fixture$matrix))
  expect_error(
    bench_open_full_source(list(kind = "matrix"), fixture$matrix),
    "10x or AnnData"
  )
})

test_that("embedded scale materialization uses the same source prefix", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("scale-materialized-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- full_source_fixture(root)

  materialized <- bench_materialize_source_tier(
    list(kind = "tenx"),
    fixture$tenx,
    3L
  )

  expect_s4_class(materialized, "dgCMatrix")
  expect_equal(
    as.matrix(materialized),
    as.matrix(fixture$matrix[, seq_len(3L), drop = FALSE])
  )
  expect_identical(
    dimnames(materialized),
    dimnames(fixture$matrix[, seq_len(3L), drop = FALSE])
  )
})

test_that("full-source writers round-trip both runtime backends", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("full-writer-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- full_source_fixture(root)
  source_matrix <- bench_open_full_source(list(kind = "tenx"), fixture$tenx)

  bpcells_path <- file.path(root, "matrix.bpcells")
  h5_path <- file.path(root, "matrix.h5")
  bench_write_full_backend(source_matrix, "bpcells", bpcells_path)
  bench_write_full_backend(source_matrix, "h5", h5_path)

  bpcells <- BPCells::open_matrix_dir(bpcells_path)
  h5 <- DelayedArray::t(HDF5Array::TENxMatrix(h5_path, group = "expression"))
  expect_equal(as.matrix(bpcells), as.matrix(fixture$matrix))
  expect_equal(as.matrix(h5), as.matrix(fixture$matrix))
  expect_identical(dimnames(bpcells), dimnames(fixture$matrix))
  expect_identical(dimnames(h5), dimnames(fixture$matrix))
  expect_identical(BPCells::storage_order(bpcells), "row")
})

test_that("full-source H5 writer stays on the lazy BPCells path", {
  testthat::skip_if_not(
    file.exists(storage_backends_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  body <- paste(readLines(storage_backends_file, warn = FALSE), collapse = "\n")

  expect_match(body, "BPCells::write_matrix_10x_hdf5", fixed = TRUE)
  expect_match(body, "rhdf5::H5Lmove", fixed = TRUE)
})

test_that("full-source query plan and portable shell use bounded expression", {
  skip_unless_full_source_deps()
  source(full_source_lib, local = TRUE)
  root <- tempfile("full-shell-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- full_source_fixture(root)
  source_matrix <- bench_open_full_source(list(kind = "tenx"), fixture$tenx)

  plan <- bench_build_lazy_query_plan(source_matrix, n_genes = 3L)
  expect_equal(nrow(plan$panel), 3L)
  expect_identical(plan$n_cells, ncol(fixture$matrix))
  expect_identical(plan$n_genes, nrow(fixture$matrix))

  sibling <- file.path(root, "bench.bpcells")
  crb <- file.path(root, "bench.crb")
  bench_write_full_backend(source_matrix, "bpcells", sibling)
  obj <- bench_make_full_shell(
    source_matrix,
    backend = "bpcells",
    location = basename(sibling),
    source_name = "fixture",
    organism = "mm10",
    run_id = "run-1"
  )
  CerebroNexus::saveCerebro(obj, crb)
  payload <- CerebroNexus:::.readCerebroPayload(crb)
  loaded <- CerebroNexus::readCerebro(crb)

  expect_null(payload$expression)
  expect_identical(payload$crb_schema$version, 2L)
  expect_equal(nrow(loaded$getMetaData()), ncol(fixture$matrix))
  expect_identical(loaded$getMetaData()$cell_barcode, colnames(fixture$matrix))
  expect_true(all(c("nUMI", "nGene") %in% names(loaded$getMetaData())))
  expect_equal(nrow(loaded$getProjection("benchmark")), ncol(fixture$matrix))
  expect_identical(loaded$getExperiment()$organism, "mm10")
  expect_identical(loaded$getExpressionBackend()$location, "bench.bpcells")

  metrics <- bench_measure_backend(loaded, plan, hot_iterations = 1L)
  expect_identical(metrics$correctness, "OK")

  h5_sibling <- file.path(root, "bench.h5")
  h5_crb <- file.path(root, "bench-h5.crb")
  bench_write_full_backend(source_matrix, "h5", h5_sibling)
  h5_obj <- bench_make_full_shell(
    source_matrix,
    backend = "h5",
    location = basename(h5_sibling),
    source_name = "fixture",
    organism = "mm10",
    run_id = "run-1"
  )
  CerebroNexus::saveCerebro(h5_obj, h5_crb)
  h5_attached <- CerebroNexus::readCerebro(h5_crb)
  h5_metrics <- bench_measure_backend(h5_attached, plan, hot_iterations = 1L)
  expect_identical(h5_metrics$correctness, "OK")

  mismatched_h5 <- bench_make_full_shell(
    source_matrix,
    backend = "h5",
    location = basename(h5_sibling),
    source_name = "fixture",
    organism = "mm10",
    run_id = "run-1"
  )
  mismatched_h5$meta_data$cell_barcode <- rev(
    mismatched_h5$meta_data$cell_barcode
  )
  mismatched_crb <- file.path(root, "mismatched-h5.crb")
  CerebroNexus::saveCerebro(mismatched_h5, mismatched_crb)
  expect_error(
    CerebroNexus::readCerebro(mismatched_crb),
    "same cells in the same order"
  )
})
