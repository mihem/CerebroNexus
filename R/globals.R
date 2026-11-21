## Suppress R CMD check NOTEs about undefined global variables.
## - `ui`, `server`, `cerebro_www_prefix` and the `viewer...` functions are
##   created by the Viewer files launchCerebro() sources into its environment.
## - `group` is used as a column name in dplyr pipelines (tibble).
## - `.` is used in magrittr/dplyr pipe expressions.
## - `paths` and `task` are named arguments of mirai::everywhere() in
##   .h5adStreamRun(); the expression sees them in the worker, not here.
utils::globalVariables(c(
  "ui",
  "server",
  "cerebro_www_prefix",
  "viewerSharedResourceHeaders",
  "viewerFirstViewPublishKept",
  "viewerSharedResourceAdopt",
  ".reserveCrbHeap",
  "viewerEarlyServerAdopt",
  "viewerLaunchPrototypes",
  "viewerUiCacheKey",
  "viewerDocumentRestore",
  "viewerDocumentServe",
  "viewerDocumentCapture",
  "viewerStartTab",
  "group",
  "paths",
  "task",
  "."
))

#' @importFrom rlang %||%
#' @noRd
NULL

## Stops with a BiocManager install hint when Bioconductor packages from
## Suggests are missing. `reason` reads like "to run gene set enrichment".
.checkBiocInstalled <- function(packages, reason) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing) == 0L) {
    return(invisible(TRUE))
  }
  stop(
    "The Bioconductor package",
    if (length(missing) > 1L) "s " else " ",
    paste0("'", missing, "'", collapse = ", "),
    if (length(missing) > 1L) " are" else " is",
    " required ",
    reason,
    ". Install with BiocManager::install(",
    paste(deparse(missing), collapse = ""),
    ").",
    call. = FALSE
  )
}

## lapply() with a progress bar when the suggested pbapply package is there.
.progressLapply <- function(X, FUN) {
  if (requireNamespace("pbapply", quietly = TRUE)) {
    return(pbapply::pblapply(X, FUN))
  }
  lapply(X, FUN)
}
