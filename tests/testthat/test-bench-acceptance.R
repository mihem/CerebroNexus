bench_acceptance <- file.path("..", "bench", "acceptance", "evaluator.R")

skip_unless_bench_acceptance <- function() {
  testthat::skip_if_not(
    file.exists(bench_acceptance),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("latency tolerance uses relative margin plus floor", {
  skip_unless_bench_acceptance()
  source(bench_acceptance, local = TRUE)
  latency <- list(rel = 0.05, floor = 50)

  expect_equal(acceptance_tolerance_limit(1000, latency), 1100)
  expect_equal(acceptance_tolerance_limit(100, latency), 155)

  pass <- acceptance_metric_verdict(1000, 1100, latency)
  expect_equal(pass$verdict, "PASS")
  fail <- acceptance_metric_verdict(1000, 1100.1, latency)
  expect_equal(fail$verdict, "FAIL")
  expect_equal(fail$limit, 1100)
  expect_equal(fail$delta, 100.1)

  expect_equal(acceptance_metric_verdict(NA_real_, 10, latency)$verdict, "INVALID")
  expect_equal(acceptance_metric_verdict(10, NA_real_, latency)$verdict, "INVALID")
})

test_that("memory tolerance reports at L1 and gates at L2", {
  skip_unless_bench_acceptance()
  source(bench_acceptance, local = TRUE)
  memory <- list(rel = 0.15, floor = 64, l1_verdict = "REPORT")

  expect_equal(acceptance_metric_verdict(1000, 5000, memory, level = "L1")$verdict, "REPORT")
  expect_equal(acceptance_metric_verdict(1000, 5000, memory, level = "L2")$verdict, "FAIL")
  expect_equal(acceptance_metric_verdict(1000, 1200, memory, level = "L2")$verdict, "PASS")
  expect_equal(acceptance_metric_verdict(1000, 1214, memory, level = "L2")$verdict, "PASS")
  expect_equal(acceptance_metric_verdict(1000, 1215, memory, level = "L2")$verdict, "FAIL")
})

test_that("budget crossings are hard failures", {
  skip_unless_bench_acceptance()
  source(bench_acceptance, local = TRUE)

  expect_equal(acceptance_budget_verdict(1000, 900, 2000)$verdict, "PASS")
  expect_equal(acceptance_budget_verdict(1999, 2001, 2000)$verdict, "FAIL")
  expect_equal(acceptance_budget_verdict(2001, 1999, 2000)$verdict, "REPORT")
  expect_equal(acceptance_budget_verdict(2001, 2100, 2000)$verdict, "TRACK")
  expect_equal(acceptance_budget_verdict(NA_real_, 100, 2000)$verdict, "INVALID")
  expect_identical(acceptance_budget_verdict(1999, 2001, 2000)$baseline_state, TRUE)
  expect_identical(acceptance_budget_verdict(1999, 2001, 2000)$candidate_state, FALSE)
})

test_that("headline improvement needs ten percent or a justification", {
  skip_unless_bench_acceptance()
  source(bench_acceptance, local = TRUE)

  expect_equal(acceptance_headline_improvement(1000, 800), 0.2)
  expect_true(is.na(acceptance_headline_improvement(NA_real_, 1)))

  expect_equal(acceptance_headline_verdict(c(0.2, -0.1)), "PASS")
  expect_equal(acceptance_headline_verdict(c(0.05, -0.1)), "FAIL")
  expect_equal(acceptance_headline_verdict(c(0.05, -0.1), justified = TRUE), "REPORT")
  expect_equal(acceptance_headline_verdict(numeric()), "FAIL")
})

test_that("overall exit code prefers invalid evidence over failure", {
  skip_unless_bench_acceptance()
  source(bench_acceptance, local = TRUE)

  verdicts <- data.frame(verdict = c("PASS", "REPORT", "TRACK"))
  expect_equal(acceptance_exit_code(verdicts), 0L)
  verdicts <- data.frame(verdict = c("PASS", "FAIL"))
  expect_equal(acceptance_exit_code(verdicts), 1L)
  verdicts <- data.frame(verdict = c("FAIL", "INVALID"))
  expect_equal(acceptance_exit_code(verdicts), 2L)
})

bench_acceptance_fixtures <- file.path("..", "bench", "acceptance", "fixtures")

skip_unless_bench_fixtures <- function() {
  testthat::skip_if_not(
    dir.exists(bench_acceptance_fixtures),
    "acceptance fixtures not present (expected when checking a built package)"
  )
}

test_that("CRB adapter emits normalized CRB metrics", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  rows <- acceptance_read_table(file.path(bench_acceptance_fixtures, "crb_sample.csv"))

  long <- acceptance_extract_crb(rows, "thin_rds", "latest")
  expect_named(
    long,
    c("metric_id", "scope", "visit", "role", "value", "unit",
      "tolerance_class", "correctness", "budget")
  )
  expect_setequal(unique(long$metric_id),
                  c("size_mib", "write_median_ms", "decode_median_ms", "hydrated_median_ms"))
  expect_equal(long$value[long$metric_id == "write_median_ms" & long$role == "baseline"], 231)
  expect_equal(long$value[long$metric_id == "write_median_ms" & long$role == "candidate"], 238)
  expect_true(all(long$correctness))
})

test_that("hot-path adapter pairs ms and allocation columns", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  rows <- acceptance_read_table(file.path(bench_acceptance_fixtures, "hot_paths_sample.tsv"))

  long <- acceptance_extract_hot_paths(rows, "thin_crb_ms", "latest_ms")
  expect_true(all(long$correctness))
  expect_equal(
    long$value[long$metric_id == "RGB expression_alloc_mib" & long$role == "candidate"],
    226.1
  )
  expect_equal(
    long$value[long$metric_id == "RGB expression_ms" & long$role == "baseline"],
    12498
  )
  expect_equal(unique(long$tolerance_class[grepl("_alloc_mib$", long$metric_id)]), "allocation")
  expect_equal(unique(long$tolerance_class[grepl("_ms$", long$metric_id)]), "latency")
})

test_that("hot-path adapter flags unequal checks", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  rows <- acceptance_read_table(file.path(bench_acceptance_fixtures, "hot_paths_sample.tsv"))
  rows$check[[1L]] <- "different"

  long <- acceptance_extract_hot_paths(rows, "thin_crb_ms", "latest_ms")
  expect_false(any(long$correctness))
})

test_that("bundle adapter reports build and JSON sizes", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  rows <- acceptance_read_table(file.path(bench_acceptance_fixtures, "bundle_sample.tsv"))

  long <- acceptance_extract_bundle(rows, "thin_crb", "latest")
  expect_setequal(unique(long$metric_id), c("build_ms", "encode_ms", "object_mib", "json_mib"))
  expect_equal(unique(long$tolerance_class[long$metric_id == "build_ms"]), "latency")
  expect_equal(unique(long$tolerance_class[long$metric_id == "json_mib"]), "bytes")
  expect_true(all(long$correctness))
})

test_that("renderer adapter keeps backend scope and flags GPU errors", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  rows <- acceptance_read_table(file.path(bench_acceptance_fixtures, "renderer_sample.csv"))

  long <- acceptance_extract_renderer(rows, "candidate")
  expect_equal(unique(long$scope), "webgpu")
  expect_true(all(long$correctness))
  expect_equal(long$value[long$metric_id == "firstFrameMs"], 42)
  expect_equal(long$tolerance_class[long$metric_id == "imageBytes"], "bytes")

  rows$gpuError[[1L]] <- 1
  long <- acceptance_extract_renderer(rows, "candidate")
  expect_false(any(long$correctness))
})

test_that("startup adapter normalizes every phase", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  baseline <- acceptance_read_table(
    file.path(bench_acceptance_fixtures, "startup_baseline.csv"))
  candidate <- acceptance_read_table(
    file.path(bench_acceptance_fixtures, "startup_candidate.csv"))

  long <- rbind(
    acceptance_extract_startup(baseline, "baseline"),
    acceptance_extract_startup(candidate, "candidate")
  )
  expect_setequal(
    unique(long$metric_id),
    c("library_ms", "app_construct_ms", "server_listen_ms", "browser_load_ms",
      "load_to_data_ms", "browser_to_data_ms", "process_to_data_ms")
  )
  expect_true(all(long$tolerance_class == "latency"))
  expect_equal(nrow(long), 42L)
})

test_that("pages adapter pairs visits and carries budgets", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  rows <- acceptance_read_table(file.path(bench_acceptance_fixtures, "pages_sample.tsv"))

  long <- acceptance_extract_pages(rows, "baseline", "candidate")
  expect_setequal(
    unique(long$metric_id),
    c("primary_ready_ms", "r_peak_rss_mib", "chrome_peak_rss_mib",
      "js_heap_used_mib", "websocket_received_bytes")
  )
  expect_true(all(long$correctness))
  budget <- unique(long$budget[long$metric_id == "primary_ready_ms"])
  expect_setequal(budget, c(2000, 3000, 500))
  expect_equal(unique(long$tolerance_class[long$metric_id == "r_peak_rss_mib"]), "memory")

  rows$correctness_pass[[1L]] <- "FALSE"
  long <- acceptance_extract_pages(rows, "baseline", "candidate")
  expect_false(any(long$correctness))
})

test_that("marker and hash helpers work on real files", {
  skip_unless_bench_fixtures()
  source(bench_acceptance, local = TRUE)
  marker <- file.path(bench_acceptance_fixtures, "correctness_pass.txt")
  expect_true(acceptance_marker_pass(marker, "1M CRB all contract: PASS"))
  expect_false(acceptance_marker_pass(marker, "1M CRB all contract: FAIL"))
  expect_false(acceptance_marker_pass(tempfile(), "anything"))
  expect_match(acceptance_sha256(marker), "^[0-9a-f]{64}$")
})

bench_acceptance_config <- file.path("..", "bench", "acceptance", "policy.R")

skip_unless_bench_config <- function() {
  testthat::skip_if_not(
    file.exists(bench_acceptance_config),
    "benchmark tree not present (expected when checking a built package)"
  )
}

test_that("acceptance config pins the documented tolerances and samples", {
  skip_unless_bench_config()
  source(bench_acceptance_config, local = TRUE)

  expect_equal(ACCEPTANCE_CONFIG$tolerances$latency$rel, 0.05)
  expect_equal(ACCEPTANCE_CONFIG$tolerances$latency$floor, 50)
  expect_equal(ACCEPTANCE_CONFIG$tolerances$allocation$rel, 0.15)
  expect_equal(ACCEPTANCE_CONFIG$tolerances$memory$rel, 0.15)
  expect_equal(ACCEPTANCE_CONFIG$tolerances$memory$floor, 64)
  expect_equal(ACCEPTANCE_CONFIG$tolerances$memory$l1_verdict, "REPORT")
  expect_equal(ACCEPTANCE_CONFIG$budgets$fresh_ms, 2000)
  expect_equal(ACCEPTANCE_CONFIG$budgets$fresh_special_ms, 3000)
  expect_equal(ACCEPTANCE_CONFIG$budgets$repeat_ms, 500)
  expect_equal(ACCEPTANCE_CONFIG$sample_min$crb, 5L)
  expect_equal(ACCEPTANCE_CONFIG$sample_min$pages_l2, 5L)
})

test_that("acceptance config pins every machine-judged branch", {
  skip_unless_bench_config()
  source(bench_acceptance_config, local = TRUE)

  expected <- list(
    pr0 = c("69893a2b", "99d305c0", "crb"),
    pr1 = c("99d305c0", "35128c51", "hot_paths"),
    pr2 = c("35128c51", "5ad9ed40", "renderer"),
    pr3 = c("5ad9ed40", "8f38ee56", "startup"),
    pr4 = c("8f38ee56", "a78b5454", "pages"),
    pr5 = c("a78b5454", "258cee14", "pages")
  )
  for (branch in names(expected)) {
    entry <- ACCEPTANCE_CONFIG$branches[[branch]]
    expect_false(is.null(entry), info = branch)
    expect_equal(entry$parent, expected[[branch]][[1L]], info = branch)
    expect_equal(entry$candidate, expected[[branch]][[2L]], info = branch)
    expect_equal(entry$layer, expected[[branch]][[3L]], info = branch)
    expect_true(entry$machine, info = branch)
    expect_true(length(entry$headline) >= 1L && length(entry$headline) <= 3L,
                info = branch)
  }
  expect_false(ACCEPTANCE_CONFIG$branches$pr6$machine)
  expect_false(ACCEPTANCE_CONFIG$branches$pr7$machine)
  expect_equal(ACCEPTANCE_CONFIG$branches$pr6$candidate, "88930fb5")
  expect_equal(ACCEPTANCE_CONFIG$branches$pr7$candidate, "17b04c92")
})

test_that("acceptance config registers every adapter's required file roles", {
  skip_unless_bench_config()
  source(bench_acceptance_config, local = TRUE)

  expected_roles <- list(
    crb = c("crb", "correctness"),
    hot_paths = c("hot_paths", "bundle"),
    renderer = c("renderer_baseline", "renderer_candidate"),
    startup = c("startup_baseline", "startup_candidate"),
    pages = c("pages")
  )
  for (layer in names(expected_roles)) {
    expect_setequal(ACCEPTANCE_CONFIG$layers[[layer]]$file_roles,
                    expected_roles[[layer]])
    expect_gte(
      length(ACCEPTANCE_CONFIG$layers[[layer]]$commands$windows),
      if (layer == "pages") 1L else length(expected_roles[[layer]])
    )
    expect_gte(
      length(ACCEPTANCE_CONFIG$layers[[layer]]$commands$mac),
      if (layer == "pages") 1L else length(expected_roles[[layer]])
    )
  }
})

temp_run_root <- function(prefix) {
  root <- tempfile(prefix)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  root
}

write_run_dir <- function(root, config, files) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  config_path <- file.path(root, "run-config.tsv")
  utils::write.table(config, config_path, sep = "\t", quote = FALSE,
                     row.names = FALSE, col.names = TRUE)
  utils::write.table(files, file.path(root, "files.tsv"), sep = "\t",
                     quote = FALSE, row.names = FALSE, col.names = TRUE)
  config_path
}

base_run_config <- function() {
  data.frame(
    branch = "pr3", layer = "startup", platform = "windows", level = "L1",
    profile = "quick", mode = "timing", rounds = 3,
    candidate_label = "v2_lazy", candidate_sha = "8f38ee56",
    baseline_label = "v1_eager", baseline_sha = "5ad9ed40",
    baseline_column = "", candidate_column = "",
    fixture_path = "", fixture_sha256 = "", git_dirty = "FALSE",
    created_utc = "2026-09-22T00:00:00Z", stringsAsFactors = FALSE
  )
}

base_run_files <- function(root, baseline_path, candidate_path) {
  source(bench_acceptance, local = TRUE)
  marker <- file.path(root, "correctness.txt")
  writeLines("startup contract: PASS", marker)
  data.frame(
    role = c("baseline", "candidate", "both"),
    file_role = c("startup_baseline", "startup_candidate", "correctness"),
    relative_path = c(basename(baseline_path), basename(candidate_path),
                      "correctness.txt"),
    sha256 = c(acceptance_sha256(baseline_path),
               acceptance_sha256(candidate_path),
               acceptance_sha256(marker)),
    stringsAsFactors = FALSE
  )
}

test_that("run-dir provenance mismatches invalidate the evidence", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-run-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "startup_baseline.csv"), root)
  file.copy(file.path(bench_acceptance_fixtures, "startup_candidate.csv"), root)
  config <- base_run_config()
  files <- base_run_files(root, "startup_baseline.csv", "startup_candidate.csv")
  write_run_dir(root, config, files)
  run <- acceptance_load_run_dir(root)

  expect_length(
    acceptance_validate_run(run, "pr3", "windows")$problems, 0L)

  bad <- config
  bad$candidate_sha <- "00000000"
  write_run_dir(root, bad, files)
  run <- acceptance_load_run_dir(root)
  expect_true(any(grepl("candidate_sha", acceptance_validate_run(run, "pr3", "windows")$problems)))

  bad <- config
  bad$rounds <- 1
  write_run_dir(root, bad, files)
  run <- acceptance_load_run_dir(root)
  expect_true(any(grepl("rounds", acceptance_validate_run(run, "pr3", "windows")$problems)))

  bad <- config
  bad$level <- "L2"
  bad$profile <- "quick"
  write_run_dir(root, bad, files)
  run <- acceptance_load_run_dir(root)
  problems <- acceptance_validate_run(run, "pr3", "windows")$problems
  expect_true(any(grepl("evidence", problems)))

  bad_files <- files
  bad_files$sha256[[1L]] <- strrep("0", 64)
  write_run_dir(root, config, bad_files)
  run <- acceptance_load_run_dir(root)
  problems <- acceptance_validate_run(run, "pr3", "windows")$problems
  expect_true(any(grepl("sha256", problems)))
})

test_that("L2 rejects a dirty worktree and too few rounds", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-l2-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "startup_baseline.csv"), root)
  file.copy(file.path(bench_acceptance_fixtures, "startup_candidate.csv"), root)
  config <- base_run_config()
  config$level <- "L2"
  config$profile <- "evidence"
  config$rounds <- 2
  config$git_dirty <- "TRUE"
  files <- base_run_files(root, "startup_baseline.csv", "startup_candidate.csv")
  write_run_dir(root, config, files)
  run <- acceptance_load_run_dir(root)

  problems <- acceptance_validate_run(run, "pr3", "windows")$problems
  expect_true(any(grepl("rounds", problems)))
  expect_true(any(grepl("worktree", problems)))
})

test_that("L2 startup requires five rounds", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-startup-l2-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "startup_baseline.csv"), root)
  file.copy(file.path(bench_acceptance_fixtures, "startup_candidate.csv"), root)
  files <- base_run_files(root, "startup_baseline.csv", "startup_candidate.csv")
  config <- base_run_config()
  config$level <- "L2"
  config$profile <- "evidence"
  config$rounds <- 4
  write_run_dir(root, config, files)
  run <- acceptance_load_run_dir(root)
  expect_true(any(grepl("rounds", acceptance_validate_run(run, "pr3", "windows")$problems)))

  config$rounds <- 5
  write_run_dir(root, config, files)
  run <- acceptance_load_run_dir(root)
  expect_false(any(grepl("rounds", acceptance_validate_run(run, "pr3", "windows")$problems)))
})

test_that("fixture hashes and uppercase file hashes are enforced", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-fixture-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "startup_baseline.csv"), root)
  file.copy(file.path(bench_acceptance_fixtures, "startup_candidate.csv"), root)
  files <- base_run_files(root, "startup_baseline.csv", "startup_candidate.csv")
  fixture <- file.path(root, "correctness.txt")
  config <- base_run_config()
  config$fixture_path <- fixture
  config$fixture_sha256 <- strrep("0", 64)
  write_run_dir(root, config, files)
  run <- acceptance_load_run_dir(root)
  expect_true(any(grepl("fixture sha256",
                        acceptance_validate_run(run, "pr3", "windows")$problems)))

  config$fixture_sha256 <- acceptance_sha256(fixture)
  write_run_dir(root, config, files)
  run <- acceptance_load_run_dir(root)
  expect_false(any(grepl("fixture sha256",
                         acceptance_validate_run(run, "pr3", "windows")$problems)))

  files$sha256 <- toupper(files$sha256)
  write_run_dir(root, config, files)
  run <- acceptance_load_run_dir(root)
  expect_false(any(grepl("sha256 mismatch",
                         acceptance_validate_run(run, "pr3", "windows")$problems)))
})

test_that("judge pairs medians and exits 0 on a clean fixture run", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-judge-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "startup_baseline.csv"), root)
  file.copy(file.path(bench_acceptance_fixtures, "startup_candidate.csv"), root)
  files <- base_run_files(root, "startup_baseline.csv", "startup_candidate.csv")
  write_run_dir(root, base_run_config(), files)

  judged <- acceptance_judge(root, "pr3", "windows", ACCEPTANCE_CONFIG)
  expect_equal(judged$exit_code, 0L, info = paste(judged$problems, collapse = "; "))
  expect_true(all(judged$verdicts$verdict %in% c("PASS", "REPORT", "TRACK")))
  load_row <- judged$verdicts[
    judged$verdicts$metric_id == "load_to_data_ms" &
      judged$verdicts$verdict == "PASS", , drop = FALSE]
  expect_equal(nrow(load_row), 1L)
})

write_pages_run <- function(root, pages, config = base_run_config()) {
  source(bench_acceptance, local = TRUE)
  path <- file.path(root, "raw.tsv")
  utils::write.table(pages, path, sep = "\t", quote = FALSE, row.names = FALSE)
  config$branch <- "pr4"
  config$layer <- "pages"
  config$candidate_sha <- "a78b5454"
  config$baseline_sha <- "8f38ee56"
  config$candidate_label <- "candidate"
  config$baseline_label <- "baseline"
  files <- data.frame(role = "both", file_role = "pages",
                      relative_path = "raw.tsv",
                      sha256 = acceptance_sha256(path), stringsAsFactors = FALSE)
  write_run_dir(root, config, files)
}

expand_pages <- function(pages, n) {
  keys <- unique(pages[c("candidate", "page", "visit")])
  out <- list()
  for (i in seq_len(nrow(keys))) {
    part <- pages[
      pages$candidate == keys$candidate[[i]] & pages$page == keys$page[[i]] &
        pages$visit == keys$visit[[i]], , drop = FALSE]
    part <- part[rep(seq_len(nrow(part)), length.out = n), , drop = FALSE]
    part$elapsed_ms <- part$elapsed_ms + seq_len(nrow(part)) - 1
    out[[length(out) + 1L]] <- part
  }
  do.call(rbind, out)
}

test_that("judge exits 1 on a page budget crossing inside tolerance", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-pages-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  pages <- acceptance_read_table(file.path(bench_acceptance_fixtures, "pages_sample.tsv"))
  pages$elapsed_ms[pages$candidate == "baseline" & pages$page == "groups" &
                     pages$visit == "first"] <- 1990
  pages$elapsed_ms[pages$candidate == "candidate" & pages$page == "groups" &
                     pages$visit == "first"] <- 2010
  write_pages_run(root, pages)

  judged <- acceptance_judge(root, "pr4", "windows", ACCEPTANCE_CONFIG)
  expect_equal(judged$exit_code, 1L)
  crossing <- judged$verdicts[
    judged$verdicts$rule == "budget" & judged$verdicts$verdict == "FAIL", ,
    drop = FALSE]
  expect_true(nrow(crossing) >= 1L)
  headroom <- judged$verdicts[
    judged$verdicts$metric_id == "primary_ready_ms" &
      judged$verdicts$scope == "groups" & judged$verdicts$visit == "first",
    , drop = FALSE]
  expect_equal(headroom$rule, "budget")
})

test_that("one false correctness row rejects the page run", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-pages-bad-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  pages <- acceptance_read_table(file.path(bench_acceptance_fixtures, "pages_sample.tsv"))
  pages$correctness_pass[[1L]] <- "FALSE"
  write_pages_run(root, pages)

  judged <- acceptance_judge(root, "pr4", "windows", ACCEPTANCE_CONFIG)
  expect_equal(judged$exit_code, 1L)
  correctness <- judged$verdicts[judged$verdicts$rule == "correctness", , drop = FALSE]
  expect_true(nrow(correctness) >= 1L)
  expect_true(all(correctness$verdict == "FAIL"))
})

test_that("waivers rewrite only the configured failing metric", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  verdicts <- data.frame(
    metric_id = c("a", "b"), scope = c("s", "s"), verdict = c("FAIL", "FAIL"),
    rule = c("tolerance", "correctness"),
    stringsAsFactors = FALSE)
  config <- list(waivers = list(
    list(branch = "pr3", metric_id = "a", scope = "s"),
    list(branch = "pr3", metric_id = "b", scope = "s")
  ))
  expect_equal(acceptance_apply_waivers(verdicts, "pr3", config)$verdict,
               c("WAIVED", "FAIL"))
  expect_equal(acceptance_apply_waivers(verdicts, "pr4", config)$verdict,
               c("FAIL", "FAIL"))

  root <- temp_run_root("acceptance-pages-waive-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  pages <- acceptance_read_table(file.path(bench_acceptance_fixtures, "pages_sample.tsv"))
  pages$elapsed_ms[pages$candidate == "baseline" & pages$page == "groups" &
                     pages$visit == "first"] <- 1990
  pages$elapsed_ms[pages$candidate == "candidate" & pages$page == "groups" &
                     pages$visit == "first"] <- 2010
  write_pages_run(root, pages)
  waived <- ACCEPTANCE_CONFIG
  waived$waivers <- list(list(branch = "pr4", metric_id = "primary_ready_ms",
                              scope = "groups", visit = "first"))
  judged <- acceptance_judge(root, "pr4", "windows", waived)
  expect_equal(judged$exit_code, 0L)
  expect_true(any(judged$verdicts$verdict == "WAIVED"))
})

test_that("a failed correctness contract exits 1, not 2", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-crb-bad-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "crb_sample.csv"), root)
  writeLines("1M CRB all contract: FAIL",
             file.path(root, "correctness.txt"))
  config <- base_run_config()
  config$branch <- "pr0"
  config$layer <- "crb"
  config$candidate_sha <- "99d305c0"
  config$baseline_sha <- "69893a2b"
  config$candidate_label <- "latest"
  config$baseline_label <- "thin_rds"
  config$rounds <- 5
  files <- data.frame(
    role = c("both", "both"),
    file_role = c("crb", "correctness"),
    relative_path = c("crb_sample.csv", "correctness.txt"),
    sha256 = c(acceptance_sha256(file.path(root, "crb_sample.csv")),
               acceptance_sha256(file.path(root, "correctness.txt"))),
    stringsAsFactors = FALSE)
  write_run_dir(root, config, files)

  judged <- acceptance_judge(root, "pr0", "windows", ACCEPTANCE_CONFIG)
  expect_equal(judged$exit_code, 1L)
  expect_true(any(grepl("^correctness:", judged$verdicts$metric_id)))

  writeLines("1M CRB all contract: FAIL (expected PASS)",
             file.path(root, "correctness.txt"))
  files$sha256[[2L]] <- acceptance_sha256(file.path(root, "correctness.txt"))
  write_run_dir(root, config, files)
  judged <- acceptance_judge(root, "pr0", "windows", ACCEPTANCE_CONFIG)
  expect_equal(judged$exit_code, 1L)
})

test_that("malformed run-config is rejected with a clear error", {
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  root <- temp_run_root("acceptance-badcfg-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines(c("branch\tpr5", "layer\tpages"),
             file.path(root, "run-config.tsv"))
  writeLines("role\tfile_role\trelative_path\tsha256",
             file.path(root, "files.tsv"))
  expect_error(acceptance_load_run_dir(root), "missing fields")
})

test_that("self-declared rounds cannot exceed observed harness rounds", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-observed-rounds-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  rows <- acceptance_read_table(
    file.path(bench_acceptance_fixtures, "crb_sample.csv")
  )
  rows$rounds <- 1L
  utils::write.csv(rows, file.path(root, "crb.csv"), row.names = FALSE)
  writeLines(
    "1M CRB all contract: PASS",
    file.path(root, "correctness.txt")
  )
  config <- base_run_config()
  config$branch <- "pr0"
  config$layer <- "crb"
  config$candidate_sha <- "99d305c0"
  config$baseline_sha <- "69893a2b"
  config$candidate_label <- "latest"
  config$baseline_label <- "thin_rds"
  config$rounds <- 5L
  files <- data.frame(
    role = c("both", "both"),
    file_role = c("crb", "correctness"),
    relative_path = c("crb.csv", "correctness.txt"),
    sha256 = c(
      acceptance_sha256(file.path(root, "crb.csv")),
      acceptance_sha256(file.path(root, "correctness.txt"))
    ),
    stringsAsFactors = FALSE
  )
  write_run_dir(root, config, files)

  judged <- acceptance_judge(root, "pr0", "windows", ACCEPTANCE_CONFIG)
  expect_equal(judged$exit_code, 2L)
  expect_true(any(grepl("observed rounds mismatch", judged$problems)))
})

test_that("run-dir keeps absolute file paths intact", {
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  root <- temp_run_root("acceptance-abs-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  external <- file.path(root, "outside.tsv")
  writeLines("x", external)
  files <- data.frame(role = "both", file_role = "pages",
                      relative_path = external, sha256 = "",
                      stringsAsFactors = FALSE)
  write_run_dir(root, base_run_config(), files)
  run <- acceptance_load_run_dir(root)
  expect_equal(run$files$path[[1L]], external)
})

test_that("thin paired samples invalidate pages evidence", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-thin-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  pages <- acceptance_read_table(file.path(bench_acceptance_fixtures, "pages_sample.tsv"))
  thin <- rbind(
    pages[pages$candidate == "baseline" & pages$page == "groups" &
            pages$visit == "first", , drop = FALSE][1:2, , drop = FALSE],
    pages[pages$candidate == "candidate" & pages$page == "groups" &
            pages$visit == "first", , drop = FALSE][1:2, , drop = FALSE]
  )
  write_pages_run(root, thin)
  judged <- acceptance_judge(root, "pr4", "windows", ACCEPTANCE_CONFIG)
  expect_equal(judged$exit_code, 2L)
  expect_true(any(grepl("insufficient", judged$problems)))
})

test_that("waivers respect the visit field", {
  skip_unless_bench_acceptance()
  source(bench_acceptance, local = TRUE)
  verdicts <- data.frame(
    metric_id = c("m", "m"), scope = c("s", "s"),
    visit = c("first", "repeat"), verdict = c("FAIL", "FAIL"),
    rule = c("budget", "correctness"),
    stringsAsFactors = FALSE)
  config <- list(waivers = list(list(branch = "pr4", metric_id = "m",
                                     scope = "s", visit = "first")))
  out <- acceptance_apply_waivers(verdicts, "pr4", config)
  expect_equal(out$verdict, c("WAIVED", "FAIL"))
})

test_that("reports surface waivers and config metadata", {
  skip_unless_bench_acceptance()
  source(bench_acceptance, local = TRUE)
  judged <- list(
    exit_code = 0L, problems = character(),
    verdicts = data.frame(
      metric_id = "primary_ready_ms", scope = "groups", visit = "first",
      unit = "ms", baseline_median = 1, candidate_median = 2, delta = 1,
      delta_pct = 1, limit = 1, rule = "budget", verdict = "WAIVED",
      stringsAsFactors = FALSE))
  md <- acceptance_markdown(judged, "pr4", "windows",
                            context = list(version = "1.0.0",
                                           config_sha256 = "abc"))
  expect_true(any(grepl("PASS (WAIVED)", md, fixed = TRUE)))
  expect_true(any(grepl("Waivers", md, fixed = TRUE)))
  expect_true(any(grepl("config_version: 1.0.0", md, fixed = TRUE)))
  expect_true(any(grepl("config_sha256: abc", md, fixed = TRUE)))
})

test_that("memory metrics report at L1 and fail at L2", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-memory-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  pages <- acceptance_read_table(file.path(bench_acceptance_fixtures, "pages_sample.tsv"))
  pages$r_peak_rss_kib[pages$candidate == "candidate"] <- 5000000
  write_pages_run(root, pages)

  l1 <- acceptance_judge(root, "pr4", "windows", ACCEPTANCE_CONFIG)
  rss <- l1$verdicts[l1$verdicts$metric_id == "r_peak_rss_mib", , drop = FALSE]
  expect_true(all(rss$verdict == "REPORT"))

  config <- base_run_config()
  config$branch <- "pr4"
  config$layer <- "pages"
  config$candidate_sha <- "a78b5454"
  config$baseline_sha <- "8f38ee56"
  config$candidate_label <- "candidate"
  config$baseline_label <- "baseline"
  config$level <- "L2"
  config$profile <- "evidence"
  config$rounds <- 5
  write_pages_run(root, expand_pages(pages, 5), config)
  l2 <- acceptance_judge(root, "pr4", "windows", ACCEPTANCE_CONFIG)
  expect_equal(l2$exit_code, 1L)
  rss <- l2$verdicts[l2$verdicts$metric_id == "r_peak_rss_mib", , drop = FALSE]
  expect_true(all(rss$verdict == "FAIL"))
})

run_acceptance_cli <- function(args) {
  script <- file.path("..", "bench", "acceptance", "check.R")
  path_flag <- match(c("--run-dir", "--write-run-dir"), args, nomatch = 0L)
  path_flag <- path_flag[path_flag > 0L][1L]
  if (length(path_flag) && !is.na(path_flag)) {
    args <- c(args, "--results-root", dirname(args[[path_flag + 1L]]))
  }
  out <- tempfile("acceptance-cli-out-")
  err <- tempfile("acceptance-cli-err-")
  on.exit(unlink(c(out, err)), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(script, args),
    stdout = out, stderr = err
  )
  list(status = status, stdout = readLines(out, warn = FALSE),
       stderr = readLines(err, warn = FALSE))
}

test_that("plan prints pinned revisions and commands", {
  skip_unless_bench_config()
  run <- run_acceptance_cli(c("plan", "pr3", "--platform", "windows"))
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  text <- paste(run$stdout, collapse = "\n")
  expect_match(text, "8f38ee56", fixed = TRUE)
  expect_match(text, "5ad9ed40", fixed = TRUE)
  expect_match(text, "startup", fixed = TRUE)
  expect_match(text, "$env:CEREBRO_STARTUP_REPEATS", fixed = TRUE)
  expect_match(text, "<baseline.csv>", fixed = TRUE)
  expect_match(text, "<candidate.csv>", fixed = TRUE)
  expect_match(text, "expected raw files", fixed = TRUE)
  expect_match(text, "budget targets", fixed = TRUE)
})

test_that("plan can scaffold a run-dir", {
  skip_unless_bench_config()
  root <- temp_run_root("acceptance-plan-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  run <- run_acceptance_cli(c("plan", "pr3", "--platform", "windows",
                              "--write-run-dir", root))
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  expect_true(file.exists(file.path(root, "run-config.tsv")))
  expect_true(file.exists(file.path(root, "files.tsv")))
})

test_that("judge CLI writes reports and returns the library exit code", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)

  root <- temp_run_root("acceptance-cli-run-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "startup_baseline.csv"), root)
  file.copy(file.path(bench_acceptance_fixtures, "startup_candidate.csv"), root)
  files <- base_run_files(root, "startup_baseline.csv", "startup_candidate.csv")
  write_run_dir(root, base_run_config(), files)

  run <- run_acceptance_cli(c("judge", "pr3", "--platform", "windows",
                              "--run-dir", root))
  expect_equal(run$status, 0L, info = paste(run$stderr, collapse = "\n"))
  expect_true(file.exists(file.path(root, "acceptance.tsv")))
  expect_true(file.exists(file.path(root, "acceptance.md")))
  report <- readLines(file.path(root, "acceptance.md"), warn = FALSE)
  expect_true(any(grepl("PASS", report)))
  expect_true(any(grepl("headline", report)))

  bad <- base_run_config()
  bad$candidate_sha <- "00000000"
  write_run_dir(root, bad, files)
  run <- run_acceptance_cli(c("judge", "pr3", "--platform", "windows",
                              "--run-dir", root))
  expect_equal(run$status, 2L)
  expect_true(any(grepl("candidate_sha", run$stdout)))
})

test_that("judge CLI emits json on request", {
  skip_unless_bench_fixtures()
  skip_unless_bench_config()
  source(bench_acceptance, local = TRUE)
  source(bench_acceptance_config, local = TRUE)
  root <- temp_run_root("acceptance-cli-json-")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  file.copy(file.path(bench_acceptance_fixtures, "startup_baseline.csv"), root)
  file.copy(file.path(bench_acceptance_fixtures, "startup_candidate.csv"), root)
  files <- base_run_files(root, "startup_baseline.csv", "startup_candidate.csv")
  write_run_dir(root, base_run_config(), files)

  run <- run_acceptance_cli(c("judge", "pr3", "--platform", "windows",
                              "--run-dir", root, "--json"))
  expect_equal(run$status, 0L)
  expect_equal(length(run$stdout), 1L)
  expect_match(paste(run$stdout, collapse = ""), '"exit_code":0', fixed = TRUE)
})
