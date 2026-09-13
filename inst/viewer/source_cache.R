## Parsed Viewer code is immutable in installed/generated apps. Cache only the
## parsed expressions; every session still evaluates them in its own scope.
.viewer_source_cache <- new.env(parent = emptyenv())

viewerSource <- function(path, envir = parent.frame()) {
  path <- normalizePath(path, mustWork = TRUE)
  info <- file.info(path)
  fingerprint <- c(size = info$size, mtime = as.numeric(info$mtime))
  cached <- .viewer_source_cache[[path]]
  if (is.null(cached) || !identical(cached$fingerprint, fingerprint)) {
    cached <- list(
      fingerprint = fingerprint,
      expressions = parse(file = path, keep.source = FALSE)
    )
    .viewer_source_cache[[path]] <- cached
  }
  invisible(eval(cached$expressions, envir = envir))
}
