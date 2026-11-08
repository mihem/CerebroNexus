## Suppress R CMD check NOTEs about undefined global variables.
## - `ui` and `server` are created by source(..., local = TRUE) inside
##   launchCerebro().
## - `group` is used as a column name in dplyr pipelines (tibble).
## - `.` is used in magrittr/dplyr pipe expressions.
## - `Cerebro.options` is assigned in .GlobalEnv intentionally (global option
##   store used by the sourced Viewer modules).
utils::globalVariables(c("ui", "server", "group", ".", "Cerebro.options"))

#' @importFrom rlang %||%
#' @noRd
NULL

## TRUE for one string that is neither missing nor empty.
.isString <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

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
