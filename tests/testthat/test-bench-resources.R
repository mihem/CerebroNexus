bench_resources <- file.path("..", "bench", "benchmark", "core.R")

skip_unless_bench_resources <- function() {
  testthat::skip_if_not(
    file.exists(bench_resources),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("resource planning honours an explicit disk-space override", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)

  expect_identical(
    bench_free_disk_bytes(tempdir(), override = "12345"),
    12345
  )
})

test_that("resource planning explains safe and unsafe memory tiers", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)
  inventory <- data.frame(
    source = c("mouse", "human"),
    nnz_per_cell = c(2010, 4112),
    source_bytes = c(4.2e9, 14.2e9)
  )
  plan <- data.frame(
    source = c("mouse", "mouse", "human", "human"),
    n_cells = c(150e3, 400e3, 50e3, 150e3)
  )
  got <- bench_assess_resources(
    inventory,
    plan,
    memory_mb = 32768,
    vector_limit_mb = 32768,
    free_disk_bytes = 100e9
  )

  expect_true(got$safe[got$source == "mouse" & got$n_cells == 150e3])
  expect_false(got$safe[got$source == "mouse" & got$n_cells == 400e3])
  expect_true(got$safe[got$source == "human" & got$n_cells == 50e3])
  expect_false(got$safe[got$source == "human" & got$n_cells == 150e3])
  expect_match(
    got$reason[got$source == "human" & got$n_cells == 150e3],
    "memory"
  )
})

test_that("resource planning checks disk and sparse-index limits", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)
  inventory <- data.frame(
    source = "fixture",
    nnz_per_cell = 5000,
    source_bytes = 20e9
  )
  plan <- data.frame(source = "fixture", n_cells = c(100e3, 500e3))

  disk <- bench_assess_resources(
    inventory,
    plan[1, , drop = FALSE],
    memory_mb = 131072,
    vector_limit_mb = 131072,
    free_disk_bytes = 10e9
  )
  expect_false(disk$safe)
  expect_match(disk$reason, "disk")

  index <- bench_assess_resources(
    inventory,
    plan[2, , drop = FALSE],
    memory_mb = 262144,
    vector_limit_mb = 262144,
    free_disk_bytes = 100e9
  )
  expect_false(index$safe)
  expect_match(index$reason, "32-bit sparse index")
})

test_that("unsafe plans fail unless explicitly overridden", {
  skip_unless_bench_resources()
  source(bench_resources, local = TRUE)
  assessment <- data.frame(
    source = "fixture",
    n_cells = 1,
    safe = FALSE,
    reason = "estimated memory exceeds safe budget"
  )
  expect_error(bench_require_safe_plan(assessment), "unsafe benchmark plan")
  expect_true(bench_require_safe_plan(assessment, allow_unsafe = TRUE))
})

test_that("resource checker rejects unsafe plans before a run", {
  skip_unless_bench_resources()
  bench_root <- normalizePath(file.path("..", "bench"))
  stage <- tempfile("bench-resource-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  inventory_path <- file.path(stage, "data_inventory.csv")
  plan_path <- file.path(stage, "run_plan.csv")
  manifest_path <- file.path(stage, "run_manifest.csv")
  output_path <- file.path(stage, "resource_check.csv")
  utils::write.csv(
    data.frame(
      source = "fixture",
      nnz_per_cell = 5000,
      source_bytes = 1e9
    ),
    inventory_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(source = "fixture", n_cells = 500e3),
    plan_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      key = c("memory_mb", "r_vector_limit_mb"),
      value = c("32768", "32768")
    ),
    manifest_path,
    row.names = FALSE
  )
  output <- suppressWarnings(bench_system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "benchmark", "cli.R"),
      "resources",
      inventory_path,
      plan_path,
      manifest_path,
      output_path
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("BENCH_ROOT=", bench_root),
      "BENCH_FREE_DISK_BYTES=100000000000"
    )
  ))
  expect_false(is.null(attr(output, "status")))
  expect_true(file.exists(output_path))
  expect_match(paste(output, collapse = "\n"), "unsafe benchmark plan")
})

test_that("resource checker accepts an unlimited R vector heap", {
  skip_unless_bench_resources()
  bench_root <- normalizePath(file.path("..", "bench"))
  stage <- tempfile("bench-resource-unlimited-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  inventory_path <- file.path(stage, "data_inventory.csv")
  plan_path <- file.path(stage, "run_plan.csv")
  manifest_path <- file.path(stage, "run_manifest.csv")
  output_path <- file.path(stage, "resource_check.csv")
  utils::write.csv(
    data.frame(
      source = "fixture",
      nnz_per_cell = 100,
      source_bytes = 1e6
    ),
    inventory_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(source = "fixture", n_cells = 1000),
    plan_path,
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      key = c("memory_mb", "r_vector_limit_mb"),
      value = c("32768", "Inf")
    ),
    manifest_path,
    row.names = FALSE
  )
  output <- suppressWarnings(bench_system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "benchmark", "cli.R"),
      "resources",
      inventory_path,
      plan_path,
      manifest_path,
      output_path
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = c(
      paste0("BENCH_ROOT=", bench_root),
      "BENCH_FREE_DISK_BYTES=100000000000"
    )
  ))
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  expect_true(file.exists(output_path))
})
