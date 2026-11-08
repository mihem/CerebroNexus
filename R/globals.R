## Suppress R CMD check NOTEs about undefined global variables.
## - `ui` and `server` are created by source(..., local = TRUE) inside
##   launchCerebro().
## - `group` is used as a column name in dplyr pipelines (tibble).
## - `.` is used in magrittr/dplyr pipe expressions.
utils::globalVariables(c("ui", "server", "group", "."))

#' @importFrom rlang %||%
#' @noRd
NULL

## TRUE for one string that is neither missing nor empty.
.isString <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}
