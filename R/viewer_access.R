## Per-app credentials, never a process-wide option: two Viewer objects must
## not share a secret. Reuse the Builder's OS-backed credential generator.
.viewerAccessRequired <- function(mode, require_token, host) {
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
  identical(mode, "open") && .builderHostIsLoopback(host)
}

.viewerAccessAllowed <- function(query, token) {
  if (is.null(token)) {
    return(TRUE)
  }
  if (is.null(query)) {
    query <- ""
  }
  if (!is.character(query) || length(query) != 1L || is.na(query)) {
    return(FALSE)
  }
  supplied <- tryCatch(
    shiny::parseQueryString(query)[["token"]],
    error = function(error) NULL
  )
  if (!is.character(supplied) || length(supplied) != 1L || is.na(supplied)) {
    return(FALSE)
  }
  supplied <- charToRaw(enc2utf8(supplied))
  expected <- charToRaw(enc2utf8(token))
  length(supplied) == length(expected) &&
    sum(as.integer(xor(supplied, expected))) == 0L
}

## Keep dataset/view queries and fragments intact when opening a deep link.
.viewerAccessUrl <- function(url, token) {
  if (is.null(token)) {
    return(url)
  }
  fragment <- regmatches(url, regexpr("#.*$", url))
  if (!length(fragment)) {
    fragment <- ""
  }
  url <- sub("#.*$", "", url)
  query_start <- regexpr("?", url, fixed = TRUE)[[1L]]
  fields <- character()
  if (query_start > 0L) {
    fields <- strsplit(substring(url, query_start + 1L), "&", fixed = TRUE)[[
      1L
    ]]
    fields <- fields[nzchar(fields) & !grepl("^token(=|$)", fields)]
    url <- substring(url, 1L, query_start - 1L)
  }
  fields <- c(fields, paste0("token=", token))
  paste0(url, "?", paste(fields, collapse = "&"), fragment)
}
