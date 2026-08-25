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
