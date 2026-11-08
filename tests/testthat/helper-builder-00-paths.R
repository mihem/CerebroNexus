builder_test_inst_path <- function(...) {
  relative <- file.path(...)
  vapply(
    relative,
    function(item) {
      path <- testthat::test_path("..", "..", "inst", item)
      if (!file.exists(path)) {
        path <- system.file(item, package = "CerebroNexus")
      }
      path
    },
    character(1),
    USE.NAMES = FALSE
  )
}

## Large demo files are left out of the package tarball (see .Rbuildignore) and
## downloaded on request; tests that read them skip when they are missing.
skip_if_inst_missing <- function(...) {
  paths <- c(...)
  testthat::skip_if_not(
    all(file.exists(builder_test_inst_path(paths))),
    paste(
      "Not in the package tarball:",
      paste(basename(paths), collapse = ", ")
    )
  )
}

## The anndata test fixtures are left out of the package tarball as well.
anndata_test_fixture <- function(name) {
  path <- testthat::test_path("fixtures", "anndata", name)
  testthat::skip_if_not(
    file.exists(path),
    "The anndata fixtures are not in the package tarball"
  )
  path
}

skip_if_builder_examples_missing <- function() {
  skip_if_inst_missing(
    "builder/fixtures/trekker_spatial/trekker_4_tissues.qs2",
    "builder/fixtures/spatial_alignment/visium_breast_cancer.rds"
  )
}

builder_profile_inst_path <- builder_test_inst_path
builder_table_inst_path <- builder_test_inst_path
builder_content_immune_inst_path <- builder_test_inst_path
builder_content_spatial_inst_path <- builder_test_inst_path
builder_spatial_test_inst_path <- builder_test_inst_path
spatial_contract_inst_path <- builder_test_inst_path

builder_test_description_path <- function() {
  path <- testthat::test_path("..", "..", "DESCRIPTION")
  if (!file.exists(path)) {
    path <- system.file(
      "DESCRIPTION",
      package = "CerebroNexus",
      mustWork = TRUE
    )
  }
  path
}
