#' Null-coalescing operator
#'
#' Equivalent to \code{rlang::`\%||\%`} / \code{shiny::`\%||\%`}; defined here
#' so the package works even when older Shiny (< 1.5) is installed.
#'
#' @param a Value to test.
#' @param b Fallback value used when \code{a} is \code{NULL}.
#' @return \code{a} if not \code{NULL}, otherwise \code{b}.
#' @keywords internal
#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b
