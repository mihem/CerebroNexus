#' Print a progress message prefixed with the current time.
#'
#' @param ... Scalar message parts, pasted together as in \code{message()}.
#' @keywords internal
#' @noRd
.logStep <- function(...) {
  message('[', format(Sys.time(), '%H:%M:%S'), '] ', ...)
}
