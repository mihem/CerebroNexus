bench_protocol <- file.path("..", "bench", "benchmark", "core.R")

skip_unless_bench_protocol <- function() {
  testthat::skip_if_not(
    file.exists(bench_protocol),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("benchmark profiles separate smoke, review, and release evidence", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)

  expect_equal(bench_profile("quick")$export_repeats, 1L)
  expect_false(bench_profile("quick")$include_scale_tiers)
  expect_equal(bench_profile("quick")$comparison_tier_mode, "smallest")
  expect_equal(bench_profile("standard")$export_repeats, 3L)
  expect_true(bench_profile("standard")$include_scale_tiers)
  expect_equal(bench_profile("full")$export_repeats, 5L)
  expect_equal(bench_profile("full")$access_repeats, 2L)
  expect_equal(bench_profile("full")$query_genes, 12L)
  expect_equal(bench_profile("full")$hot_iterations, 3L)
  expect_lte(
    (bench_profile("full")$query_genes - 1L) *
      bench_profile("full")$hot_iterations,
    36L
  )
  expect_false(bench_profile("standard")$evidence_grade)
  expect_true(bench_profile("full")$evidence_grade)
  expect_equal(bench_profile("scale")$export_repeats, 5L)
  expect_equal(bench_profile("scale")$access_repeats, 2L)
  expect_true(bench_profile("scale")$include_scale_tiers)
  expect_true(bench_profile("scale")$evidence_grade)
  expect_equal(bench_profile("full")$export_repeats, 5L)
  expect_true(bench_profile("stress")$include_scale_tiers)
  expect_false(bench_profile("stress")$evidence_grade)
  expect_false(bench_profile("full")$include_scale_tiers)
  expect_equal(bench_profile("panel_c2")$canonical_name, "full")
  expect_error(bench_profile("unknown"), "unknown benchmark profile")
})

test_that("comparison schedules balance backend order positions", {
  skip_unless_bench_protocol()
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
  skip_unless_bench_protocol()
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

test_that("default sources share the complete scale grid", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)

  defaults <- BENCH_SOURCES[
    !vapply(
      BENCH_SOURCES,
      function(source) isTRUE(source$opt_in),
      logical(1)
    )
  ]
  expect_true(all(vapply(
    defaults,
    function(source) {
      identical(
        source$comparison_tiers,
        c(1e3, 10e3, 50e3, 100e3, 500e3, 1e6)
      )
    },
    logical(1)
  )))
})

test_that("preview is the exact three-backend scale bridge", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)
  source(bench_protocol, local = TRUE)

  schedule <- bench_fixed_schedule(BENCH_SOURCES, "preview")

  expect_equal(nrow(schedule), 18L)
  expect_setequal(unique(schedule$backend), c("embedded", "bpcells", "h5"))
  expect_equal(
    unique(schedule$n_cells[schedule$source == "mouse_brain_e18"]),
    400e3
  )
  expect_equal(
    unique(schedule$n_cells[schedule$source == "human_pfc_hbcc"]),
    300e3
  )
  expect_false(any(schedule$n_cells == 800e3))
  expect_true(all(schedule$access_repeats == 2L))
})

test_that("full is the exact two-backend full-source schedule", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)
  source(bench_protocol, local = TRUE)

  schedule <- bench_fixed_schedule(BENCH_SOURCES, "full")

  expect_equal(nrow(schedule), 20L)
  expect_setequal(unique(schedule$backend), c("bpcells", "h5"))
  expect_equal(
    unique(schedule$n_cells[schedule$source == "mouse_brain_e18"]),
    1306127
  )
  expect_equal(
    unique(schedule$n_cells[schedule$source == "human_pfc_hbcc"]),
    1486324
  )
  expect_true(all(schedule$access_repeats == 2L))
  expect_error(
    bench_fixed_schedule(BENCH_SOURCES, "unknown"),
    "preview or full"
  )
})

test_that("scale profile runs embedded through 500k", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)
  source(bench_protocol, local = TRUE)

  schedule <- bench_scale_schedule(BENCH_SOURCES)
  expected_tiers <- c(1e3, 10e3, 50e3, 100e3, 500e3, 1e6)

  expect_equal(nrow(schedule), 2L * (5L * 3L + 2L) * 5L)
  expect_setequal(
    unique(schedule$source),
    c(
      "mouse_brain_e18",
      "human_pfc_hbcc"
    )
  )
  expect_setequal(unique(schedule$backend), c("embedded", "bpcells", "h5"))
  expect_identical(sort(unique(schedule$n_cells)), expected_tiers)
  expect_true(all(schedule$n_cells[schedule$backend == "embedded"] <= 500e3))
  expect_false(any(
    schedule$n_cells == 1e6 & schedule$backend == "embedded"
  ))
  expect_setequal(
    unique(schedule$backend[schedule$n_cells <= 500e3]),
    c("embedded", "bpcells", "h5")
  )
  expect_setequal(
    unique(schedule$backend[schedule$n_cells == 1e6]),
    c("bpcells", "h5")
  )
  expect_true(all(schedule$export_repeat %in% seq_len(5L)))
  expect_true(all(schedule$access_repeats == 2L))
})

test_that("scale profile accepts failed embedded measurements only", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)

  specs <- list(fixture = list(tiers = 1000, comparison_tiers = 1000))
  schedule <- bench_schedule(
    specs,
    "scale",
    sources = "fixture"
  )
  access_for <- function(indices) {
    do.call(
      rbind,
      lapply(indices, function(i) {
        do.call(
          rbind,
          lapply(seq_len(schedule$access_repeats[i]), function(j) {
            data.frame(
              source = schedule$source[i],
              n_cells = schedule$n_cells[i],
              backend = schedule$backend[i],
              export_repeat = schedule$export_repeat[i],
              access_repeat = j,
              status = "OK",
              correctness = "OK",
              row_fingerprint = "row",
              reference_row_fingerprint = "row",
              block_fingerprint = "block",
              reference_block_fingerprint = "block",
              stringsAsFactors = FALSE
            )
          })
        )
      })
    )
  }

  exports <- transform(schedule, status = "OK", run_id = "run-1")
  exports$status[exports$backend == "embedded"] <- "FAILED(export): limit"
  access <- access_for(which(exports$status == "OK"))

  expect_true(bench_validate_results(
    schedule,
    exports,
    access,
    profile = bench_profile("scale")
  ))

  exports$status <- "OK"
  access <- access_for(seq_len(nrow(schedule)))
  optional <- access$backend == "embedded"
  access$status[optional] <- "FAILED(correctness/access): unsupported"
  access$correctness[optional] <- NA_character_
  access$row_fingerprint[optional] <- NA_character_
  access$reference_row_fingerprint[optional] <- NA_character_
  access$block_fingerprint[optional] <- NA_character_
  access$reference_block_fingerprint[optional] <- NA_character_
  expect_true(bench_validate_results(
    schedule,
    exports,
    access,
    profile = bench_profile("scale")
  ))

  access$status[access$backend == "bpcells"][1] <- "FAILED(access): error"
  expect_error(
    bench_validate_results(
      schedule,
      exports,
      access,
      profile = bench_profile("scale")
    ),
    "access process failed"
  )

  exports$status[exports$backend == "bpcells"][1] <- "FAILED(build): limit"
  expect_error(
    bench_validate_results(
      schedule,
      exports,
      access,
      profile = bench_profile("scale")
    ),
    "comparison tier did not complete every required backend"
  )
})

test_that("query panels are deterministic and span expression density", {
  skip_unless_bench_protocol()
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
  skip_unless_bench_protocol()
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
  skip_unless_bench_protocol()
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
        status = "OK",
        correctness = "OK",
        row_fingerprint = "same-row",
        reference_row_fingerprint = "same-row",
        block_fingerprint = "same-block",
        reference_block_fingerprint = "same-block",
        subset_row_fingerprint = "same-subset-row",
        reference_subset_row_fingerprint = "same-subset-row",
        subset_block_fingerprint = "same-subset-block",
        reference_subset_block_fingerprint = "same-subset-block",
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
  broken$status[1] <- "FAILED(query)"
  expect_error(
    bench_validate_results(
      schedule,
      exports,
      broken,
      crashes = data.frame(),
      profile = bench_profile("quick")
    ),
    "access process failed"
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

test_that("only evidence-grade profiles may back the user-facing report", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)

  expect_error(
    bench_require_evidence_profile(bench_profile("standard")),
    "evidence-grade benchmark profile"
  )
  expect_true(bench_require_evidence_profile(bench_profile("full")))
})

test_that("access crashes do not masquerade as duplicate export outcomes", {
  skip_unless_bench_protocol()
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
    status = "OK",
    correctness = "OK",
    row_fingerprint = "row",
    reference_row_fingerprint = "row",
    block_fingerprint = "block",
    reference_block_fingerprint = "block",
    subset_row_fingerprint = "subset-row",
    reference_subset_row_fingerprint = "subset-row",
    subset_block_fingerprint = "subset-block",
    reference_subset_block_fingerprint = "subset-block",
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

test_that("result keys canonicalize scientific and integer cell counts", {
  skip_unless_bench_protocol()
  source(bench_protocol, local = TRUE)

  scientific <- data.frame(
    source = "fixture",
    n_cells = 1e5,
    backend = "bpcells",
    export_repeat = 1L
  )
  integer <- data.frame(
    source = "fixture",
    n_cells = 100000L,
    backend = "bpcells",
    export_repeat = 1
  )

  expect_identical(.bench_result_key(scientific), .bench_result_key(integer))
  expect_identical(
    .bench_result_key(scientific),
    "fixture|100000|bpcells|1"
  )
})
