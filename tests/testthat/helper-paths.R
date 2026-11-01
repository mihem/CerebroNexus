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

## Sources the Viewer runtime helpers into `envir` in the order shiny_server.R
## loads them, so a test does not have to know which files utility_functions.R
## depends on. `root` is a viewer directory, e.g. a generated app's. Returns
## `envir`.
viewer_runtime_env <- function(
  envir = new.env(parent = globalenv()),
  root = viewer_test_path()
) {
  for (file in c(
    "plotting_functions.R",
    "core/viewer_pack.R",
    "core/crb_contract.R",
    "msigdb.R",
    "crb_cache.R",
    "utility_functions.R"
  )) {
    sys.source(file.path(root, file), envir = envir)
  }
  invisible(envir)
}

viewer_app_test_path <- function() {
  source_path <- testthat::test_path("..", "..", "inst")
  if (file.exists(file.path(source_path, "app.R"))) {
    return(source_path)
  }

  system.file(package = "CerebroNexus")
}

## The background R processes of the Builder (a build, the browser tests) start
## with library(CerebroNexus) and read the viewer files of the *installed*
## package. A checkout run with load_all() has none, or an older one, so those
## tests wait for an installation that has the file they need.
skip_without_installed_package <- function() {
  testthat::skip_if_not(
    nzchar(base::system.file(
      "viewer",
      "core",
      "crb_contract.R",
      package = "CerebroNexus"
    )),
    "an installed CerebroNexus with the Viewer files is needed"
  )
}
