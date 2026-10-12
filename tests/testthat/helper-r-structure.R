# Inspect R calls without depending on whitespace, comments or quote style.
r_test_calls <- function(source, name = NULL) {
  if (is.character(source)) {
    source <- parse(text = source, keep.source = FALSE)
  }
  result <- list()
  visit <- function(node) {
    if (
      is.call(node) &&
        (is.null(name) || identical(node[[1L]], as.name(name)))
    ) {
      result[[length(result) + 1L]] <<- node
    }
    if (is.call(node) || is.expression(node) || is.pairlist(node)) {
      for (i in seq_along(node)) {
        if (!identical(node[[i]], quote(expr = ))) visit(node[[i]])
      }
    }
  }
  visit(source)
  result
}

r_test_normalize <- function(node) {
  if (is.call(node) || is.expression(node) || is.pairlist(node)) {
    for (i in seq_along(node)) {
      if (!identical(node[[i]], quote(expr = ))) {
        node[i] <- list(r_test_normalize(node[[i]]))
      }
    }
    # Air may add braces around a single branch expression.
    if (
      is.call(node) && identical(node[[1L]], as.name("{")) && length(node) == 2L
    ) {
      return(node[[2L]])
    }
  }
  node
}

r_test_has_call <- function(source, expected) {
  expected <- r_test_normalize(expected)
  any(vapply(
    r_test_calls(source),
    function(node) {
      identical(r_test_normalize(node), expected)
    },
    logical(1)
  ))
}

r_test_observer <- function(source, call_name, marker) {
  found <- Filter(
    function(node) r_test_has_call(node, marker),
    r_test_calls(source, call_name)
  )
  testthat::expect_length(found, 1L)
  if (length(found) != 1L) {
    stop("Expected one observer containing the marker")
  }
  found[[1L]]
}
