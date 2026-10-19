## Skips and process helpers that behave the same on Linux CI and on
## Windows/macOS development machines.

## Returns the node path so callers that pass it to system2() need not look it
## up again.
skip_if_no_node <- function() {
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

## system2(env = ) prepends NAME=value to the command line, which only a POSIX
## shell understands: on Windows the pair reaches the program as an argument.
## Set the variables in this process for the duration of the call instead.
system2_env <- function(command, args = character(), env = character(), ...) {
  split <- regexpr("=", env, fixed = TRUE)
  vars <- substring(env, split + 1L)
  names(vars) <- substring(env, 1L, split - 1L)
  withr::with_envvar(vars, system2(command, args, ...))
}
