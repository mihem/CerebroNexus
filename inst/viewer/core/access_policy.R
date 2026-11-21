## Pure startup policy, shared with start.R before the package is loaded.
viewerAccessRequired <- function(mode, require_token, host) {
  if (
    !is.null(require_token) &&
      (!is.logical(require_token) ||
        length(require_token) != 1L ||
        is.na(require_token))
  ) {
    stop("'require_token' must be NULL, TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(require_token)) {
    return(require_token)
  }
  loopback <- is.character(host) &&
    length(host) == 1L &&
    !is.na(host) &&
    grepl("^(localhost|127([.][0-9]{1,3}){3}|::1|[[]::1[]])$", tolower(host))
  identical(mode, "open") && loopback
}
