ci_test_plan_api <- new.env(parent = globalenv())
runner_path <- test_path("..", "..", "scripts", "run-test-shard.R")
if (file.exists(runner_path)) {
  sys.source(runner_path, envir = ci_test_plan_api)
}

test_that("the CI plan classifies every test exactly once", {
  expect_true(file.exists(runner_path))
  skip_if_not(file.exists(runner_path))
  plan <- ci_test_plan_api$ci_test_plan(test_path())
  discovered <- sort(list.files(
    test_path(),
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = FALSE
  ))
  assigned <- c(plan$logic, plan$process_sensitive, plan$browser)

  expect_setequal(assigned, discovered)
  expect_false(anyDuplicated(assigned) > 0L)
  expect_identical(
    plan$browser,
    c(
      "test-app-immune_repertoire.R",
      "test-app-inst.R",
      "test-app-new-modules.R",
      "test-app-trajectory.R",
      "test-app-viewport-layout.R",
      "test-smoke-production.R"
    )
  )
})

test_that("known process-sensitive tests are isolated when present", {
  expect_true(file.exists(runner_path))
  skip_if_not(file.exists(runner_path))
  root <- tempfile("ci-test-plan-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  browser <- c(
    "test-app-immune_repertoire.R",
    "test-app-inst.R",
    "test-app-new-modules.R",
    "test-app-trajectory.R",
    "test-app-viewport-layout.R",
    "test-smoke-production.R"
  )
  file.create(file.path(
    root,
    c("test-plain.R", "test-builder-worker.R", browser)
  ))
  plan <- ci_test_plan_api$ci_test_plan(root)

  expect_identical(plan$process_sensitive, "test-builder-worker.R")
  expect_identical(plan$browser, browser)
  expect_identical(plan$logic, "test-plain.R")
})

test_that("stable hash sharding is lossless and insertion-stable", {
  expect_true(file.exists(runner_path))
  skip_if_not(file.exists(runner_path))
  files <- paste0("test-", letters[1:8], ".R")
  assigned <- ci_test_plan_api$ci_test_shards(files, 3L)
  expanded <- ci_test_plan_api$ci_test_shards(
    c(files, "test-unrelated-new-file.R"),
    3L
  )
  shard_of <- function(name, groups) {
    which(vapply(groups, function(group) name %in% group, logical(1)))
  }

  expect_setequal(unlist(assigned, use.names = FALSE), files)
  expect_false(anyDuplicated(unlist(assigned, use.names = FALSE)) > 0L)
  for (file in files) {
    expect_identical(shard_of(file, assigned), shard_of(file, expanded))
  }
})

test_that("the shard runner validates arguments and permits empty groups", {
  expect_true(file.exists(runner_path))
  skip_if_not(file.exists(runner_path))
  plan <- list(
    logic = c("test-a.R", "test-b.R"),
    process_sensitive = character(),
    browser = character()
  )

  expect_identical(
    ci_test_plan_api$ci_test_shard_files(
      plan,
      "process-sensitive",
      shard = 1L,
      shards = 1L
    ),
    character()
  )
  expect_error(
    ci_test_plan_api$ci_test_shard_files(
      plan,
      "logic",
      shard = 1.5,
      shards = 2L
    ),
    "shard"
  )
  expect_error(
    ci_test_plan_api$ci_parse_args(c("--strategy", "weighted")),
    "Unknown argument"
  )
})

test_that("browser references stay in the explicit browser group", {
  files <- list.files(
    test_path(),
    pattern = "^test-[[:alnum:]_.-]+[.]R$",
    full.names = TRUE
  )
  files <- files[basename(files) != "test-ci-test-plan.R"]
  browser_references <- basename(files[vapply(
    files,
    function(file) {
      any(grepl("shinytest2|AppDriver", readLines(file, warn = FALSE)))
    },
    logical(1)
  )])

  expect_setequal(
    browser_references,
    ci_test_plan_api$ci_browser_test_files()
  )
})

test_that("precheck isolates shards and reports every failure", {
  precheck_path <- test_path("..", "..", "scripts", "precheck.sh")
  expect_true(file.exists(precheck_path))
  skip_if_not(file.exists(precheck_path))
  precheck <- paste(readLines(precheck_path, warn = FALSE), collapse = "\n")

  expect_match(precheck, "mktemp -d", fixed = TRUE)
  expect_match(precheck, "export TMPDIR=", fixed = TRUE)
  expect_match(precheck, "wait \"${shard_pids[$index]}\"", fixed = TRUE)
  expect_match(precheck, "failures+=(", fixed = TRUE)
  expect_match(precheck, "run_parallel_group process-sensitive 1", fixed = TRUE)
  expect_match(precheck, "set -uo pipefail", fixed = TRUE)
})

test_that("docs validation is separate from full code precheck", {
  precheck_path <- test_path("..", "..", "scripts", "precheck.sh")
  expect_true(file.exists(precheck_path))
  skip_if_not(file.exists(precheck_path))
  precheck <- paste(readLines(precheck_path, warn = FALSE), collapse = "\n")
  full_section <- sub(
    "^[\\s\\S]*?  full\\)",
    "",
    precheck,
    perl = TRUE
  )
  full_section <- sub("  docs\\)[\\s\\S]*$", "", full_section, perl = TRUE)
  docs_section <- sub("^[\\s\\S]*?  docs\\)", "", precheck, perl = TRUE)

  expect_false(grepl("pkgdown::", full_section, fixed = TRUE))
  expect_match(docs_section, "pkgdown::build_site", fixed = TRUE)
})

test_that("CI executes the shared plan and waits for every matrix shard", {
  workflow_path <- test_path(
    "..",
    "..",
    ".github",
    "workflows",
    "R-tests.yaml"
  )
  workflow <- paste(readLines(workflow_path, warn = FALSE), collapse = "\n")

  expect_match(workflow, "fail-fast: false", fixed = TRUE)
  expect_match(workflow, "group: process-sensitive", fixed = TRUE)
  expect_match(workflow, "Rscript scripts/run-test-shard.R", fixed = TRUE)
  expect_match(workflow, "--group \"${{ matrix.group }}\"", fixed = TRUE)
  expect_match(workflow, "needs: [tests]", fixed = TRUE)
  expect_match(workflow, "needs.tests.result", fixed = TRUE)
})

test_that("R CMD check does not execute the full test suite again", {
  workflow <- paste(
    readLines(
      test_path("..", "..", ".github", "workflows", "R-cmd-check.yaml"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(workflow, "args = c('--no-tests')", fixed = TRUE)
})

test_that("pkgdown validates pull requests but deploys only master", {
  workflow <- paste(
    readLines(
      test_path("..", "..", ".github", "workflows", "pkgdown.yaml"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    workflow,
    "if: github.event_name == 'push' && github.ref == 'refs/heads/master'",
    fixed = TRUE
  )
})
