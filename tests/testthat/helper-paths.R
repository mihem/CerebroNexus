## Paths into inst/. Helpers load alphabetically, so this file comes first and
## later helpers can source viewer files at load time.

## The source tree's copy when it exists (devtools::test(), test_local()),
## otherwise the installed package's (R CMD check).
inst_test_path <- function(...) {
  source_path <- testthat::test_path("..", "..", "inst", ...)
  if (file.exists(source_path)) {
    return(source_path)
  }

  system.file(..., package = "CerebroNexus")
}

viewer_test_path <- function(...) {
  inst_test_path("viewer", ...)
}

## tests/bench/ is .Rbuildignore'd, so benchmark test files skip as a whole
## when checking a built package.
skip_if_no_bench <- function() {
  skip_if_not(
    dir.exists(testthat::test_path("..", "bench")),
    "benchmark tree not present (expected when checking a built package)"
  )
}

viewer_app_test_path <- function() {
  source_path <- testthat::test_path("..", "..", "inst")
  if (file.exists(file.path(source_path, "app.R"))) {
    return(source_path)
  }

  system.file(package = "CerebroNexus")
}
