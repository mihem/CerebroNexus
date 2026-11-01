builder_repo_source("prerequisite.R", local = globalenv())

test_that("Builder version prefers the active source checkout", {
  root <- withr::local_tempdir()
  writeLines(
    c("Package: CerebroNexus", "Version: 9.8.7"),
    file.path(root, "DESCRIPTION")
  )

  expect_identical(
    builder_runtime_package_version(
      source_root = root,
      .installed_version = function() "1.0.0"
    ),
    "9.8.7"
  )
})

test_that("Builder version falls back to installed package metadata", {
  expect_identical(
    builder_runtime_package_version(
      source_root = NULL,
      .installed_version = function() "5.0.0"
    ),
    "5.0.0"
  )
  expect_null(builder_runtime_package_version(
    source_root = NULL,
    .installed_version = function() stop("not installed")
  ))
})
