## The packages a quiet session loads one at a time (shiny_server.R,
## viewer_package_load_order()): a package after the packages its NAMESPACE
## imports, as loadNamespace() loads them, and none that only a DESCRIPTION
## names (packages used through `pkg::`, which the Viewer never loads).

viewer_server_function <- function(name) {
  found <- NULL
  walk <- function(expr) {
    if (!is.null(found) || !is.call(expr)) {
      return(invisible(NULL))
    }
    if (
      identical(expr[[1L]], as.name("<-")) &&
        identical(expr[[2L]], as.name(name))
    ) {
      found <<- expr[[3L]]
      return(invisible(NULL))
    }
    for (part in as.list(expr)) {
      walk(part)
    }
  }
  for (expr in parse(viewer_test_path("shiny_server.R"), keep.source = FALSE)) {
    walk(expr)
  }
  found
}

test_that("idle package loading follows NAMESPACE imports, not DESCRIPTION", {
  skip_if_not_installed("DT")
  env <- new.env(parent = globalenv())
  ## As in a session that has loaded nothing yet.
  env$loadedNamespaces <- function() "base"
  for (name in c(
    "viewer_package_imports",
    "viewer_package_imports_read",
    "viewer_package_load_order"
  )) {
    definition <- viewer_server_function(name)
    expect_false(is.null(definition), label = name)
    assign(name, eval(definition, env), envir = env)
  }
  order <- env$viewer_package_load_order("DT")
  expect_identical(order[[length(order)]], "DT")
  imports <- env$viewer_package_imports("DT")
  base <- rownames(installed.packages(priority = "base"))
  expect_true(all(setdiff(imports, order) %in% base))
  expect_true(all(match(intersect(imports, order), order) < length(order)))
  ## DT's DESCRIPTION names these for its `pkg::` uses (or suggests them
  ## through its imports'); loadNamespace("DT") loads none of them.
  expect_false(any(c("rmarkdown", "knitr", "tinytex") %in% order))
})
