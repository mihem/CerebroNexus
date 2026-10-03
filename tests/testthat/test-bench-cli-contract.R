bench_root <- normalizePath(file.path("..", "bench"), mustWork = FALSE)

skip_unless_bench_cli <- function() {
  testthat::skip_if_not(
    file.exists(file.path(bench_root, "benchmark", "cli.R")),
    "benchmark tree not present (expected when checking a built package)"
  )
}

run_bench_command <- function(command, args = character(), env = character()) {
  out <- tempfile("bench-cli-stdout-")
  err <- tempfile("bench-cli-stderr-")
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- bench_system2(
    file.path(R.home("bin"), "Rscript"),
    c(file.path(bench_root, "benchmark", "cli.R"), command, args),
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

test_that("benchmark has one flat module set and one CLI entry point", {
  skip_unless_bench_cli()

  expect_true(file.exists(file.path(bench_root, "benchmark", "core.R")))
  expect_true(file.exists(file.path(bench_root, "benchmark", "cli.R")))
  expect_true(file.exists(file.path(bench_root, "benchmark", "run.sh")))
  expect_true(all(file.exists(file.path(
    bench_root,
    "benchmark",
    c(
      "sources.R", "protocol.R", "metrics.R", "storage_backends.R",
      "reporting.R", "resources.R", "cli_prepare.R", "cli_measure.R",
      "cli_report.R", "cli_evidence.R"
    )
  ))))
  expect_false(file.exists(file.path(bench_root, "run_scale.sh")))
  expect_false(dir.exists(file.path(bench_root, "config")))
  expect_false(dir.exists(file.path(bench_root, "lib")))
  expect_false(dir.exists(file.path(bench_root, "fixtures")))
  expect_true(file.exists(file.path(bench_root, "acceptance", "policy.R")))
  expect_true(file.exists(file.path(bench_root, "acceptance", "evaluator.R")))
  expect_false(dir.exists(file.path(bench_root, "src")))
  expect_false(file.exists(file.path(bench_root, "run_sweep.sh")))
})

test_that("runner keeps isolated processes and safe cleanup", {
  skip_unless_bench_cli()
  runner <- paste(
    readLines(file.path(bench_root, "benchmark", "run.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(runner, "trap - EXIT INT TERM", fixed = TRUE)
  expect_match(runner, 'if [ "${BENCH_KEEP:-0}" != "1" ]; then', fixed = TRUE)
  expect_match(runner, "BENCH_RESULT_ROOT", fixed = TRUE)
  expect_match(runner, "R_ENVIRON_USER=/dev/null", fixed = TRUE)
  expect_match(runner, "R_PROFILE_USER=/dev/null", fixed = TRUE)
  expect_match(runner, 'R_LIBS_USER="$SCRATCH/r-user-library"', fixed = TRUE)
  expect_match(runner, "bench_fetch_source()", fixed = TRUE)

  stages <- c(
    " inspect ",
    " environment ",
    " plan ",
    " query-plan ",
    ' "$build_command" ',
    " access ",
    " validate ",
    " report ",
    " figure ",
    " evidence ",
    " check ",
    " publish "
  )
  positions <- vapply(
    stages,
    function(stage) {
      regexpr(stage, runner, fixed = TRUE)[1L]
    },
    integer(1)
  )
  expect_true(all(positions > 0L))
})

test_that("one public launcher runs both benchmark profiles in the background", {
  skip_unless_bench_cli()
  launcher <- paste(
    readLines(file.path(bench_root, "benchmark", "run.sh"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(launcher, 'ACTION="${1:-run}"', fixed = TRUE)
  expect_match(launcher, "status)", fixed = TRUE)
  expect_match(launcher, "full|scale)", fixed = TRUE)
  expect_match(
    launcher,
    'nohup "$SCRIPT" _worker "$RUN_TARGET"',
    fixed = TRUE
  )
  expect_match(launcher, '"$SCRIPT" _profile', fixed = TRUE)
  expect_match(launcher, "resume)", fixed = TRUE)
  full <- regexpr(
    "run_profile full full",
    launcher,
    fixed = TRUE
  )[1L]
  scale <- regexpr(
    "run_profile scale scale",
    launcher,
    fixed = TRUE
  )[1L]
  expect_gt(full, 0L)
  expect_gt(scale, full)
})

test_that("schedule CLI includes embedded through 500k", {
  skip_unless_bench_cli()
  csv <- tempfile(fileext = ".csv")
  tsv <- tempfile(fileext = ".tsv")
  on.exit(unlink(c(csv, tsv)), add = TRUE)

  run <- run_bench_command(
    "plan",
    c(csv, tsv),
    env = "BENCH_PROFILE=scale"
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  schedule <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_setequal(
    unique(schedule$n_cells),
    c(1e3, 10e3, 50e3, 100e3, 500e3, 1e6)
  )
  expect_setequal(
    unique(schedule$n_cells[schedule$backend == "embedded"]),
    c(1e3, 10e3, 50e3, 100e3, 500e3)
  )
  expect_false(any(schedule$backend == "embedded" & schedule$n_cells > 500e3))
  expect_false(any(grepl("[eE][+-]", readLines(tsv, warn = FALSE))))
})

test_that("manifest CLI records the run identity", {
  skip_unless_bench_cli()
  result <- tempfile(fileext = ".csv")
  on.exit(unlink(result), add = TRUE)

  run <- run_bench_command(
    "environment",
    result,
    env = c(
      "BENCH_PROFILE=quick",
      "BENCH_STUDY_ID=test-study",
      "BENCH_RUN_ID=test-run",
      "BENCH_THREADS=3"
    )
  )
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  manifest <- utils::read.csv(result, stringsAsFactors = FALSE)
  values <- stats::setNames(manifest$value, manifest$key)
  expect_equal(values[["study_id"]], "test-study")
  expect_equal(values[["run_id"]], "test-run")
  expect_equal(values[["benchmark_threads"]], "3")
  expect_match(values[["git_sha"]], "^[0-9a-f]{40}$")
  cli <- paste(
    unlist(lapply(
      c("cli_prepare.R", "cli_measure.R"),
      function(file) {
        readLines(file.path(bench_root, "benchmark", file), warn = FALSE)
      }
    )),
    collapse = "\n"
  )
  expect_match(cli, ":(exclude,glob)tests/bench/results/**", fixed = TRUE)
  expect_false(grepl("<<-", cli, fixed = TRUE))
  expect_match(
    cli,
    "build = fingerprint_for(successful_exports)",
    fixed = TRUE
  )
  expect_match(
    cli,
    "access = fingerprint_for(successful_access)",
    fixed = TRUE
  )
})

test_that("source cache reuses only verified files", {
  skip_unless_bench_cli()
  testthat::skip_on_os("windows")
  helper <- file.path(bench_root, "benchmark", "run.sh")
  root <- tempfile("bench-cache-")
  origin <- file.path(root, "origin", "fixture.h5")
  cache <- file.path(root, "cache")
  scratch <- file.path(root, "scratch")
  dir.create(dirname(origin), recursive = TRUE)
  dir.create(scratch)
  writeBin(charToRaw("benchmark-fixture"), origin)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)

  command <- file.path(root, "fetch.sh")
  writeLines(
    c(
      "set -euo pipefail",
      sprintf("source %s", shQuote(helper)),
      sprintf("export BENCH_SOURCE_CACHE=%s", shQuote(cache)),
      sprintf(
        "bench_fetch_source %s %d %s",
        shQuote(paste0("file://", normalizePath(origin))),
        file.size(origin),
        shQuote(scratch)
      )
    ),
    command
  )
  first <- system2("bash", command, stdout = TRUE, stderr = TRUE)
  expect_null(attr(first, "status"), info = paste(first, collapse = "\n"))
  unlink(origin)
  second <- system2("bash", command, stdout = TRUE, stderr = TRUE)
  expect_null(attr(second, "status"), info = paste(second, collapse = "\n"))
})

test_that("benchmark sources pin SHA-256 values", {
  skip_unless_bench_cli()
  source(file.path(bench_root, "benchmark", "core.R"), local = TRUE)
  pinned <- vapply(
    BENCH_SOURCES[c("mouse_brain_e18", "human_pfc_hbcc")],
    `[[`,
    character(1),
    "expected_sha256"
  )
  expect_true(all(grepl("^[0-9a-f]{64}$", pinned)))
})
