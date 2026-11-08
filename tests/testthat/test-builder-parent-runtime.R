builder_repo_source("prerequisite.R")
builder_repo_source("app_bundle.R")

test_that("parent artifact checks resolve the source-tree CRB runtime", {
  source_root <- withr::local_tempdir()
  dir.create(file.path(source_root, "R"))
  writeLines(
    c(
      ".readCerebroPayload <- function(path) paste0('read:', path)",
      ".spatialMoleculeBackend <- function(object, path) list(path = path)"
    ),
    file.path(source_root, "R", "cerebro_io.R"),
    useBytes = TRUE
  )

  runtime <- .builder_app_runtime_function(
    ".readCerebroPayload",
    .source_root = source_root,
    .namespace = function() NULL
  )

  expect_identical(runtime("fixture.crb"), "read:fixture.crb")
  expect_true(is.function(.builder_app_runtime_function(
    ".spatialMoleculeBackend",
    .source_root = source_root,
    .namespace = function() NULL
  )))
})

test_that("parent artifact checks fail closed for incomplete source runtime", {
  source_root <- withr::local_tempdir()
  dir.create(file.path(source_root, "R"))

  expect_error(
    .builder_app_runtime_function(
      ".readCerebroPayload",
      .source_root = source_root,
      .namespace = function() NULL
    ),
    "source runtime is incomplete",
    fixed = TRUE
  )
})
