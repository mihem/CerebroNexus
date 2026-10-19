skip_if_no_bench()

bench_root <- normalizePath(
  file.path("..", "bench"),
  mustWork = FALSE
)

test_that("sweep publishes after validation and cleans up on every exit", {
  skip_if(
    .Platform$OS.type != "unix" || Sys.which("bash") == "",
    "requires bash"
  )
  root <- tempfile("sweep-process-")
  dir.create(file.path(root, "tests", "bench"), recursive = TRUE)
  dir.create(file.path(root, "bin"))
  dir.create(file.path(root, "scratch"))
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  script <- file.path(root, "tests", "bench", "run_sweep.sh")
  writeLines(readLines(file.path(bench_root, "run_sweep.sh")), script)
  for (tool in c("Rscript", "R", "curl", "rm")) {
    target <- file.path(root, "bin", tool)
    writeLines(readLines(test_path("fixtures", "bench-shell.sh")), target)
    Sys.chmod(target, "0755")
  }
  log <- file.path(root, "calls")
  withr::local_envvar(c(
    PATH = paste(
      file.path(root, "bin"),
      Sys.getenv("PATH"),
      sep = .Platform$path.sep
    ),
    BENCH_TEST_ROOT = root,
    BENCH_TEST_LOG = log,
    BENCH_PROFILE = "publication",
    BENCH_RUN_ID = "test",
    BENCH_SCRATCH_PARENT = file.path(root, "scratch"),
    BENCH_KEEP = "0"
  ))
  expected <- c(
    "01_inspect_data.R",
    "02_record_environment.R",
    "03_plan_runs.R",
    "04_check_resources.R",
    "install",
    "10_export_backend.R",
    "20_measure_backend.R",
    "30_check_measurements.R",
    "40_write_report.R",
    "41_draw_figures.R",
    "50_check_outputs.R",
    "60_publish_results.R"
  )
  for (mode in c("success", "failed-validation", "interrupted")) {
    writeLines(character(), log)
    withr::with_envvar(
      c(
        BENCH_TEST_STOP = if (mode == "failed-validation") {
          "50_check_outputs.R"
        } else {
          ""
        },
        BENCH_TEST_SIGNAL = if (mode == "interrupted") {
          "02_record_environment.R"
        } else {
          ""
        }
      ),
      {
        result <- suppressWarnings(system2(
          "bash",
          shQuote(script),
          stdout = TRUE,
          stderr = TRUE
        ))
      }
    )
    calls <- readLines(log)
    stages <- calls[!startsWith(calls, "remove ")]
    if (mode == "success") {
      expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
      expect_identical(stages, expected)
    } else if (mode == "failed-validation") {
      expect_false("60_publish_results.R" %in% stages)
      expect_equal(attr(result, "status"), 1)
    } else {
      expect_false("60_publish_results.R" %in% stages)
    }
    removals <- sub("^remove ", "", calls[startsWith(calls, "remove ")])
    scratch_removals <- removals[
      dirname(removals) == file.path(root, "scratch")
    ]
    expect_length(scratch_removals, 1L)
    expect_length(list.files(file.path(root, "scratch")), 0L)
    expect_true(file.exists(script))
  }
})

run_bench_rscript <- function(script, args = character(), env = character()) {
  out <- tempfile("bench-cli-stdout-")
  err <- tempfile("bench-cli-stderr-")
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- system2_env(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root, "src", script), args),
    stdout = out,
    stderr = err,
    env = c(paste0("BENCH_ROOT=", bench_root), env)
  )
  list(
    status = status,
    stdout = readLines(out, warn = FALSE),
    stderr = readLines(err, warn = FALSE)
  )
}

test_that("schedule CLI emits a complete quick-profile grid", {
  result <- tempfile(fileext = ".csv")
  on.exit(unlink(result), add = TRUE)

  run <- run_bench_rscript(
    "03_plan_runs.R",
    result,
    env = "BENCH_PROFILE=quick"
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  schedule <- utils::read.csv(result, stringsAsFactors = FALSE)
  expect_named(
    schedule,
    c(
      "profile",
      "source",
      "n_cells",
      "comparison",
      "export_repeat",
      "order_position",
      "backend",
      "access_repeats"
    )
  )
  expect_equal(nrow(schedule), 6L)
  expect_setequal(unique(schedule$backend), c("embedded", "bpcells", "h5"))
})

test_that("manifest CLI records source revision and runtime", {
  result <- tempfile(fileext = ".csv")
  on.exit(unlink(result), add = TRUE)

  run <- run_bench_rscript(
    "02_record_environment.R",
    result,
    env = c("BENCH_PROFILE=quick", "BENCH_RUN_ID=test-run")
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  manifest <- utils::read.csv(result, stringsAsFactors = FALSE)
  values <- stats::setNames(manifest$value, manifest$key)
  expect_equal(values[["run_id"]], "test-run")
  expect_equal(values[["profile"]], "quick")
  expect_match(values[["git_sha"]], "^[0-9a-f]{40}$")
  expect_match(values[["r_version"]], "^R version")
  expect_true(nzchar(values[["cpu"]]))
})

test_that("validator CLI accepts complete results and rejects drift", {
  stage <- tempfile("bench-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  source(file.path(bench_root, "lib", "protocol.R"), local = TRUE)
  specs <- list(fixture = list(tiers = 1000, comparison_tiers = 1000))
  schedule <- bench_schedule(specs, "quick", "fixture")
  exports <- transform(schedule, status = "OK", run_id = "run-1")
  access <- do.call(
    rbind,
    lapply(seq_len(nrow(schedule)), function(i) {
      data.frame(
        run_id = "run-1",
        profile = "quick",
        source = schedule$source[i],
        n_cells = schedule$n_cells[i],
        backend = schedule$backend[i],
        export_repeat = schedule$export_repeat[i],
        access_repeat = 1L,
        correctness = "OK",
        row_fingerprint = "row",
        reference_row_fingerprint = "row",
        block_fingerprint = "block",
        reference_block_fingerprint = "block",
        stringsAsFactors = FALSE
      )
    })
  )
  utils::write.csv(
    schedule,
    file.path(stage, "05_schedule.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    exports,
    file.path(stage, "10_export.csv"),
    row.names = FALSE
  )
  utils::write.csv(access, file.path(stage, "20_access.csv"), row.names = FALSE)
  utils::write.csv(
    data.frame(
      key = c("run_id", "profile", "git_sha"),
      value = c("run-1", "quick", paste(rep("a", 40), collapse = ""))
    ),
    file.path(stage, "run_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      source = "fixture",
      n_cells = 1000,
      safe = TRUE,
      reason = "safe"
    ),
    file.path(stage, "resource_check.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      run_id = "run-1",
      source = "fixture",
      url = "https://example.test/fixture.h5",
      bytes = 123,
      sha256 = paste(rep("b", 64), collapse = ""),
      stringsAsFactors = FALSE
    ),
    file.path(stage, "source_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(
      source = character(),
      n_cells = numeric(),
      backend = character(),
      export_repeat = integer(),
      stage = character(),
      exit_code = integer()
    ),
    file.path(stage, "crashes.csv"),
    row.names = FALSE
  )

  ok <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_equal(ok$status, 0L, info = paste(ok$stderr, collapse = "\n"))

  unlink(file.path(stage, "resource_check.csv"))
  missing_resources <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(missing_resources$status, 0L))
  expect_match(
    paste(missing_resources$stderr, collapse = "\n"),
    "resource_check.csv"
  )
  utils::write.csv(
    data.frame(
      source = "fixture",
      n_cells = 1000,
      safe = TRUE,
      reason = "safe"
    ),
    file.path(stage, "resource_check.csv"),
    row.names = FALSE
  )

  access$row_fingerprint[1] <- "wrong"
  utils::write.csv(access, file.path(stage, "20_access.csv"), row.names = FALSE)
  bad <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(bad$status, 0L))
  expect_match(paste(bad$stderr, collapse = "\n"), "fingerprint mismatch")

  access$row_fingerprint[1] <- "row"
  utils::write.csv(access, file.path(stage, "20_access.csv"), row.names = FALSE)
  source_manifest <- utils::read.csv(
    file.path(stage, "source_manifest.csv"),
    stringsAsFactors = FALSE
  )
  source_manifest$sha256 <- "not-a-checksum"
  utils::write.csv(
    source_manifest,
    file.path(stage, "source_manifest.csv"),
    row.names = FALSE
  )
  bad_source <- run_bench_rscript(
    "30_check_measurements.R",
    stage,
    env = "BENCH_PROFILE=quick"
  )
  expect_false(identical(bad_source$status, 0L))
  expect_match(paste(bad_source$stderr, collapse = "\n"), "source SHA-256")
})
