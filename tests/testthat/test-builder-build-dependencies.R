builder_repo_source("prerequisite.R")

test_that("Build dependency preflight loads only frozen plan capabilities", {
  plan <- list(
    items = list(list(
      expression_backend = "memory",
      reused_artifact = NULL,
      source_snapshot_identity = list(
        snapshot = list(serialization = "qs2")
      )
    )),
    app_auth = list(enabled = FALSE)
  )
  loaded <- character()

  capability <- builder_build_dependency_capability(
    plan,
    .load = function(package) {
      loaded <<- c(loaded, package)
      TRUE
    },
    .version = function(package) base::package_version("99.0.0")
  )

  expect_true(capability$available)
  expect_identical(
    loaded,
    c(
      builder_build_base_package_requirements(),
      "Seurat",
      "SeuratObject",
      "qs2"
    )
  )
  expect_false("scRepertoire" %in% loaded)
  expect_false(any(c("biomaRt", "GSVA") %in% loaded))
})

test_that("Build dependency preflight keeps selected optional capabilities", {
  plan <- list(
    items = list(list(
      expression_backend = "h5",
      reused_artifact = NULL,
      source_snapshot_identity = list(
        snapshot = list(serialization = "qs")
      )
    )),
    app_auth = list(enabled = TRUE)
  )
  requirements <- builder_build_package_requirements(plan)

  expect_true(all(
    c(
      "HDF5Array (>= 1.18.1)",
      "qs",
      "shinymanager (>= 1.1.0)",
      "digest"
    ) %in%
      requirements
  ))
})
