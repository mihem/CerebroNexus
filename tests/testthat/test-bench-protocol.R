skip_if_no_bench()

bench_protocol <- file.path("..", "bench", "lib", "protocol.R")

test_that("benchmark profiles separate smoke, review, and article evidence", {
  source(bench_protocol, local = TRUE)

  expect_equal(bench_profile("quick")$export_repeats, 1L)
  expect_false(bench_profile("quick")$include_scale_tiers)
  expect_equal(bench_profile("quick")$comparison_tier_mode, "smallest")
  expect_equal(bench_profile("standard")$export_repeats, 3L)
  expect_true(bench_profile("standard")$include_scale_tiers)
  expect_equal(bench_profile("publication")$access_repeats, 2L)
  expect_equal(bench_profile("publication")$query_genes, 12L)
  expect_equal(bench_profile("publication")$hot_iterations, 3L)
  expect_lte(
    (bench_profile("publication")$query_genes - 1L) *
      bench_profile("publication")$hot_iterations,
    36L
  )
  expect_false(bench_profile("standard")$article_eligible)
  expect_true(bench_profile("publication")$article_eligible)
  expect_true(bench_profile("stress")$include_scale_tiers)
  expect_false(bench_profile("stress")$article_eligible)
  expect_false(bench_profile("publication")$include_scale_tiers)
  expect_error(bench_profile("unknown"), "unknown benchmark profile")
})

test_that("comparison schedules balance backend order positions", {
  source(bench_protocol, local = TRUE)

  specs <- list(
    fixture = list(
      tiers = c(1000, 2000),
      comparison_tiers = 1000
    )
  )
  schedule <- bench_schedule(specs, "standard", sources = "fixture")

  repeated <- schedule[schedule$n_cells == 1000, ]
  expect_equal(nrow(repeated), 9L)
  position_counts <- table(repeated$backend, repeated$order_position)
  expect_equal(dim(position_counts), c(3L, 3L))
  expect_true(all(position_counts == 1L))
  expect_true(all(repeated$access_repeats == 1L))

  scale <- schedule[schedule$n_cells == 2000, ]
  expect_equal(nrow(scale), 3L)
  expect_equal(unique(scale$export_repeat), 1L)
  expect_false(any(scale$comparison))
})

test_that("quick schedules run only the smallest comparison tier", {
  source(bench_protocol, local = TRUE)

  specs <- list(
    fixture = list(
      tiers = c(1000, 2000, 4000),
      comparison_tiers = c(1000, 2000)
    )
  )
  schedule <- bench_schedule(specs, "quick", sources = "fixture")
  expect_equal(unique(schedule$n_cells), 1000)
  expect_equal(nrow(schedule), 3L)
})

test_that("query panels are deterministic and span expression density", {
  source(bench_protocol, local = TRUE)

  genes <- paste0("g", seq_len(12))
  nnz <- c(0, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144)
  panel_a <- bench_stratified_gene_panel(genes, nnz, n_genes = 5L)
  panel_b <- bench_stratified_gene_panel(genes, nnz, n_genes = 5L)

  expect_identical(panel_a, panel_b)
  expect_equal(nrow(panel_a), 5L)
  expect_false(any(panel_a$nnz == 0))
  expect_equal(range(panel_a$nnz), c(1, 144))
  expect_equal(panel_a$role[1], "first")
  expect_lte(abs(panel_a$nnz[1] - stats::median(nnz[nnz > 0])), 5)
  expect_error(
    bench_stratified_gene_panel(genes, rep(0, length(genes))),
    "no expressed genes"
  )
})

test_that("numeric fingerprints are stable and value-sensitive", {
  source(bench_protocol, local = TRUE)

  x <- matrix(c(0, 1.25, 2.5, NA_real_), nrow = 2)
  expect_identical(bench_numeric_fingerprint(x), bench_numeric_fingerprint(x))
  expect_false(identical(
    bench_numeric_fingerprint(x),
    bench_numeric_fingerprint(x + 1)
  ))
  expect_false(identical(
    bench_numeric_fingerprint(x),
    bench_numeric_fingerprint(as.numeric(x))
  ))
})

test_that("result validation rejects missing and incorrect measurements", {
  source(bench_protocol, local = TRUE)

  specs <- list(
    fixture = list(tiers = 1000, comparison_tiers = 1000)
  )
  schedule <- bench_schedule(specs, "quick", sources = "fixture")
  exports <- transform(
    schedule,
    status = "OK",
    run_id = "run-1"
  )
  access <- do.call(
    rbind,
    lapply(seq_len(nrow(schedule)), function(i) {
      data.frame(
        source = schedule$source[i],
        n_cells = schedule$n_cells[i],
        backend = schedule$backend[i],
        export_repeat = schedule$export_repeat[i],
        access_repeat = 1L,
        correctness = "OK",
        row_fingerprint = "same-row",
        reference_row_fingerprint = "same-row",
        block_fingerprint = "same-block",
        reference_block_fingerprint = "same-block",
        stringsAsFactors = FALSE
      )
    })
  )

  expect_true(bench_validate_results(
    schedule,
    exports,
    access,
    crashes = data.frame(),
    profile = bench_profile("quick")
  ))

  expect_error(
    bench_validate_results(
      schedule,
      exports[-1, ],
      access,
      crashes = data.frame(),
      profile = bench_profile("quick")
    ),
    "missing export outcome"
  )

  broken <- access
  broken$row_fingerprint[1] <- "wrong"
  expect_error(
    bench_validate_results(
      schedule,
      exports,
      broken,
      crashes = data.frame(),
      profile = bench_profile("quick")
    ),
    "fingerprint mismatch"
  )
})

test_that("only publication profiles may back the user-facing article", {
  source(bench_protocol, local = TRUE)

  expect_error(
    bench_require_article_profile(bench_profile("standard")),
    "publication profile"
  )
  expect_true(bench_require_article_profile(bench_profile("publication")))
})

test_that("access crashes do not masquerade as duplicate export outcomes", {
  source(bench_protocol, local = TRUE)

  specs <- list(fixture = list(tiers = 1000, comparison_tiers = 1000))
  schedule <- bench_schedule(specs, "quick", "fixture")
  exports <- transform(schedule, status = "OK", run_id = "run-1")
  access <- data.frame(
    source = schedule$source[-1],
    n_cells = schedule$n_cells[-1],
    backend = schedule$backend[-1],
    export_repeat = schedule$export_repeat[-1],
    access_repeat = 1L,
    correctness = "OK",
    row_fingerprint = "row",
    reference_row_fingerprint = "row",
    block_fingerprint = "block",
    reference_block_fingerprint = "block",
    stringsAsFactors = FALSE
  )
  crashes <- transform(
    schedule[1, ],
    stage = "access-1",
    exit_code = 9L
  )

  expect_error(
    bench_validate_results(
      schedule,
      exports,
      access,
      crashes,
      bench_profile("quick")
    ),
    "missing access measurement"
  )
})
