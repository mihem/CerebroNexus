## Skips that behave the same on Linux CI and on Windows/macOS development
## machines.

## Returns the node path so callers that pass it to system2() need not look it
## up again. Node.js checks run in CI, not on CRAN.
skip_if_no_node <- function() {
  skip_on_cran()
  node <- Sys.which("node")
  skip_if(!nzchar(node), "node not on PATH")
  invisible(unname(node))
}

## shinytest2 skips by itself when Chrome is missing; direct chromote sessions
## do not, so they would error on a machine without Chrome.
skip_if_no_chrome <- function() {
  skip_on_cran()
  skip_if_not_installed("chromote")
  skip_if(is.null(chromote::find_chrome()), "Chrome unavailable")
}
