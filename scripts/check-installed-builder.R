#!/usr/bin/env Rscript

## Smoke-test resources through the installed-package layout. Source-tree tests
## cannot detect system.file() or archive omissions reliably.

package_root <- system.file(package = "CerebroNexus")
if (!nzchar(package_root) || !dir.exists(package_root)) {
  stop("The installed CerebroNexus package is unavailable.", call. = FALSE)
}

builder_root <- system.file("builder", package = "CerebroNexus")
viewer_root <- system.file("viewer", package = "CerebroNexus")
if (!dir.exists(builder_root) || !dir.exists(viewer_root)) {
  stop("The installed Builder or Viewer runtime is missing.", call. = FALSE)
}

runtime <- new.env(parent = globalenv())
source(
  file.path(builder_root, "io.R"),
  local = runtime,
  encoding = "UTF-8"
)

fixtures <- c(
  file.path(
    "builder",
    "fixtures",
    "complete_viewer",
    "complete_viewer_data.rds"
  ),
  file.path("builder", "fixtures", "spatial_alignment", "arrow_spatial.rds"),
  file.path("builder", "fixtures", "trekker_spatial", "trekker_4_tissues.qs2")
)
resolved <- vapply(fixtures, runtime$.builder_example_path, character(1))
if (any(!nzchar(resolved)) || any(!file.exists(resolved))) {
  stop(
    "One or more installed Builder gallery fixtures cannot be resolved.",
    call. = FALSE
  )
}

cat("Installed Builder layout is valid.\n")
