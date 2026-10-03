bench_protocol <- file.path("..", "bench", "benchmark", "core.R")
bench_reporting <- bench_protocol
bench_root <- normalizePath(file.path("..", "bench"), mustWork = FALSE)

skip_unless_bench_reporting <- function() {
  testthat::skip_if_not(
    file.exists(bench_protocol) && file.exists(bench_reporting),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("metric summaries retain independent-repeat uncertainty", {
  skip_unless_bench_reporting()
  source(bench_protocol, local = TRUE)
  source(bench_reporting, local = TRUE)

  x <- data.frame(
    source = rep("fixture", 6),
    n_cells = rep(1000, 6),
    backend = rep(c("embedded", "h5"), each = 3),
    status = "OK",
    seconds = c(3, 1, 2, 0.3, 0.1, 0.2),
    rss_mb = c(30, 10, 20, 6, 4, 5)
  )
  got <- bench_summarise_metrics(
    x,
    group = c("source", "n_cells", "backend"),
    metrics = c("seconds", "rss_mb")
  )

  embedded <- got[got$backend == "embedded", ]
  expect_equal(embedded$seconds_median, 2)
  expect_equal(embedded$seconds_min, 1)
  expect_equal(embedded$seconds_max, 3)
  expect_equal(embedded$seconds_n, 3L)
  expect_equal(embedded$rss_mb_median, 20)
})

test_that("metric summaries ignore failed rows but not missing values", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  x <- data.frame(
    backend = c("h5", "h5", "h5"),
    status = c("OK", "OK", "FAILED(query)"),
    seconds = c(1, NA, 100)
  )
  got <- bench_summarise_metrics(x, "backend", "seconds")
  expect_equal(got$seconds_median, 1)
  expect_equal(got$seconds_n, 1L)
  expect_equal(got$rows_n, 2L)
})

test_that("interval formatting exposes range and sample count", {
  skip_unless_bench_reporting()
  source(bench_reporting, local = TRUE)

  expect_equal(
    bench_format_interval(2, 1, 3, 3, digits = 1),
    "2.0 [1.0-3.0], n=3"
  )
  expect_equal(
    bench_format_interval(NA, NA, NA, 0, digits = 1),
    "--"
  )
})

test_that("evidence labels prevent quick runs from sounding definitive", {
  skip_unless_bench_reporting()
  source(bench_protocol, local = TRUE)
  source(bench_reporting, local = TRUE)

  expect_match(
    bench_evidence_notice(bench_profile("quick")),
    "Exploratory"
  )
  expect_match(
    bench_evidence_notice(bench_profile("scale")),
    "Benchmark evidence"
  )
})

test_that("report and plots consume repeated benchmark rows", {
  skip_unless_bench_reporting()
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("patchwork")

  result_dir <- tempfile("bench-report-fixture-")
  out_dir <- tempfile("bench-report-plots-")
  dir.create(result_dir)
  dir.create(out_dir)
  on.exit(unlink(c(result_dir, out_dir), recursive = TRUE), add = TRUE)

  backends <- c("embedded", "bpcells", "h5")
  exports <- expand.grid(
    backend = backends,
    export_repeat = 1:3,
    stringsAsFactors = FALSE
  )
  exports$run_id <- "test-run"
  exports$profile <- "scale"
  exports$source <- "fixture"
  exports$label <- "fixture"
  exports$n_cells <- 1000
  exports$n_genes <- 100
  exports$nnz <- 10000
  exports$order_position <- rep(1:3, 3)
  exports$status <- "OK"
  exports$read_secs <- 1
  exports$seurat_secs <- 1
  exports$export_secs <- rep(c(3, 1, 2), 3)
  exports$crb_mb <- 1
  exports$sibling_mb <- 1
  exports$total_mb <- rep(c(3, 1, 2), 3)
  exports$rss_mb <- 10
  exports$r_peak_mb <- 20
  exports$peak_rss_mb <- 24
  exports$query_plan_fingerprint <- "plan"
  utils::write.csv(
    exports,
    file.path(result_dir, "10_export.csv"),
    row.names = FALSE
  )

  access <- exports[
    rep(seq_len(nrow(exports)), each = 2),
    c(
      "run_id",
      "profile",
      "source",
      "n_cells",
      "backend",
      "export_repeat",
      "order_position"
    )
  ]
  access$access_repeat <- rep(1:2, nrow(exports))
  access$status <- "OK"
  access$load_secs <- 0.1
  access$attach_secs <- 0.2
  access$rss_mb <- rep(c(30, 10, 20), 6)
  access$peak_rss_mb <- access$rss_mb + 5
  access$first_query_secs <- 0.03
  access$hot_p50_secs <- rep(c(0.03, 0.01, 0.02), 6)
  access$hot_p95_secs <- access$hot_p50_secs * 1.2
  access$block_secs <- rep(c(0.3, 0.1, 0.2), 6)
  access$n_hot <- 10L
  access$correctness <- "OK"
  access$row_fingerprint <- access$reference_row_fingerprint <- "row"
  access$block_fingerprint <- access$reference_block_fingerprint <- "block"
  access$query_plan_fingerprint <- "plan"
  utils::write.csv(
    access,
    file.path(result_dir, "20_access.csv"),
    row.names = FALSE
  )

  utils::write.csv(
    data.frame(
      label = "fixture",
      n_cells = 1000,
      n_genes = 100,
      nnz = 10000,
      nnz_per_cell = 10,
      dgc_gb_full = 0.001,
      dgc_representable = TRUE
    ),
    file.path(result_dir, "00_probe.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = character(),
      profile = character(),
      source = character(),
      n_cells = numeric(),
      backend = character(),
      export_repeat = integer(),
      order_position = integer(),
      stage = character(),
      exit_code = integer()
    ),
    file.path(result_dir, "crashes.csv"),
    row.names = FALSE
  )
  manifest <- c(
    run_id = "test-run",
    profile = "scale",
    git_sha = paste(rep("a", 40), collapse = ""),
    generated_at = "2026-08-04",
    git_branch = "test",
    git_dirty = "false",
    package_version = "3.2.0",
    r_version = R.version.string,
    os = "test",
    cpu = "test",
    logical_cores = "1",
    memory_mb = "1024",
    r_vector_limit_mb = "32768"
  )
  utils::write.csv(
    data.frame(key = names(manifest), value = unname(manifest)),
    file.path(result_dir, "run_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = "test-run",
      source = "fixture",
      url = "https://example.test",
      bytes = 123,
      sha256 = paste(rep("b", 64), collapse = "")
    ),
    file.path(result_dir, "source_manifest.csv"),
    row.names = FALSE
  )

  env <- paste0("BENCH_ROOT=", normalizePath(file.path("..", "bench")))
  report_status <- bench_system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "benchmark", "cli.R"),
      "report",
      result_dir
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  )
  expect_null(
    attr(report_status, "status"),
    info = paste(report_status, collapse = "\n")
  )
  report <- readLines(file.path(result_dir, "summary.md"), warn = FALSE)
  expect_true(any(grepl("Benchmark evidence", report, fixed = TRUE)))
  expect_true(any(grepl("n=3", report, fixed = TRUE)))
  expect_true(any(grepl("peak process RSS MB", report, fixed = TRUE)))

  plot_status <- bench_system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "benchmark", "cli.R"),
      "figure",
      result_dir,
      out_dir
    ),
    stdout = TRUE,
    stderr = TRUE,
    env = env
  )
  expect_null(
    attr(plot_status, "status"),
    info = paste(plot_status, collapse = "\n")
  )
  expect_true(file.exists(file.path(
    out_dir,
    "expression_backend_benchmark_overview.png"
  )))
  expect_true(file.exists(file.path(
    out_dir,
    "expression_backend_benchmark_ceiling.png"
  )))
})

test_that("benchmark figure labels distinct expression workloads", {
  skip_unless_bench_reporting()
  script <- readLines(
    file.path(bench_root, "benchmark", "cli_report.R"),
    warn = FALSE
  )
  source <- paste(script, collapse = "\n")

  expect_match(source, "Interactive single-gene latency", fixed = TRUE)
  expect_match(source, "Marker-panel block latency", fixed = TRUE)
  expect_match(source, "warmed expression lookup", fixed = TRUE)
  expect_match(source, 'plot_annotation(tag_levels = "A")', fixed = TRUE)
})

test_that("scale-limit summaries use embedded exports only", {
  skip_unless_bench_reporting()
  source <- paste(
    readLines(
      file.path(bench_root, "benchmark", "cli_report.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(source, 'exports$backend == "embedded"', fixed = TRUE)
})
