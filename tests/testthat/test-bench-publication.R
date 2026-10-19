bench_root <- normalizePath(file.path("..", "bench"), mustWork = FALSE)

skip_unless_bench_publication <- function() {
  testthat::skip_if_not(
    file.exists(file.path(bench_root, "src", "60_publish_results.R")),
    "benchmark tree not present (expected when checking a built package)"
  )
}

run_publisher <- function(stage, target, run_id, fail_at = "") {
  out <- tempfile("bench-publish-stdout-")
  err <- tempfile("bench-publish-stderr-")
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- system2_env(
    file.path(R.home("bin"), "Rscript"),
    c(
      file.path(bench_root, "src", "60_publish_results.R"),
      stage,
      target,
      run_id
    ),
    stdout = out,
    stderr = err,
    env = paste0("BENCH_PUBLISH_FAIL_AT=", fail_at)
  )
  list(
    status = status,
    stdout = readLines(out, warn = FALSE),
    stderr = readLines(err, warn = FALSE)
  )
}

test_that("failed publication preserves the prior current result", {
  skip_unless_bench_publication()
  root <- tempfile("bench-results-")
  stage <- tempfile("bench-stage-")
  dir.create(file.path(root, "runs", "old-run"), recursive = TRUE)
  dir.create(stage)
  on.exit(unlink(c(root, stage), recursive = TRUE), add = TRUE)
  writeLines("old-run", file.path(root, "CURRENT"))
  writeLines("old evidence", file.path(root, "runs", "old-run", "summary.md"))
  writeLines("new evidence", file.path(stage, "summary.md"))

  failed <- run_publisher(stage, root, "new-run", fail_at = "before-pointer")
  expect_false(identical(failed$status, 0L))
  expect_equal(readLines(file.path(root, "CURRENT")), "old-run")
  expect_equal(
    readLines(file.path(root, "runs", "old-run", "summary.md")),
    "old evidence"
  )

  recovered <- run_publisher(stage, root, "new-run")
  expect_equal(
    recovered$status,
    0L,
    info = paste(recovered$stderr, collapse = "\n")
  )
  expect_equal(readLines(file.path(root, "CURRENT")), "new-run")
  expect_equal(
    readLines(file.path(root, "runs", "new-run", "summary.md")),
    "new evidence"
  )
  expect_true(dir.exists(file.path(root, "runs", "old-run")))
})

test_that("publisher rejects unsafe and conflicting run identities", {
  skip_unless_bench_publication()
  root <- tempfile("bench-results-")
  stage <- tempfile("bench-stage-")
  dir.create(root)
  dir.create(stage)
  on.exit(unlink(c(root, stage), recursive = TRUE), add = TRUE)
  writeLines("evidence", file.path(stage, "summary.md"))

  unsafe <- run_publisher(stage, root, "../escape")
  expect_false(identical(unsafe$status, 0L))
  expect_match(paste(unsafe$stderr, collapse = "\n"), "unsafe run id")

  first <- run_publisher(stage, root, "same-run")
  expect_equal(first$status, 0L)
  writeLines("different", file.path(stage, "summary.md"))
  conflict <- run_publisher(stage, root, "same-run")
  expect_false(identical(conflict$status, 0L))
  expect_match(
    paste(conflict$stderr, collapse = "\n"),
    "conflicting existing run"
  )
})

test_that("output checker requires report and publication figures", {
  skip_unless_bench_publication()
  stage <- tempfile("bench-output-stage-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  utils::write.csv(
    data.frame(key = "profile", value = "publication"),
    file.path(stage, "run_manifest.csv"),
    row.names = FALSE
  )
  checker <- file.path(bench_root, "src", "50_check_outputs.R")

  missing <- suppressWarnings(system2_env(
    file.path(R.home("bin"), "Rscript"),
    c(checker, stage),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("BENCH_ROOT=", bench_root)
  ))
  expect_false(is.null(attr(missing, "status")))
  expect_match(paste(missing, collapse = "\n"), "missing staged output")

  writeLines("report", file.path(stage, "summary.md"))
  dir.create(file.path(stage, "figures"))
  writeLines(
    "png",
    file.path(
      stage,
      "figures",
      "expression_backend_benchmark_overview.png"
    )
  )
  writeLines(
    "png",
    file.path(
      stage,
      "figures",
      "expression_backend_benchmark_ceiling.png"
    )
  )
  complete <- system2_env(
    file.path(R.home("bin"), "Rscript"),
    c(checker, stage),
    stdout = TRUE,
    stderr = TRUE,
    env = paste0("BENCH_ROOT=", bench_root)
  )
  expect_null(attr(complete, "status"), info = paste(complete, collapse = "\n"))
})
