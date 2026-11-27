site_root <- function() {
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  testthat::skip_if_not(
    file.exists(file.path(root, "_pkgdown.yml")) &&
      dir.exists(file.path(root, "vignettes")),
    "current source tree is not present in the installed-package layout"
  )
  root
}

has_article <- function(root, page) {
  stem <- sub("[.]html$", "", basename(page))
  any(file.exists(file.path(
    root,
    c("vignettes", file.path("vignettes", "articles")),
    paste0(stem, ".Rmd")
  )))
}

test_that("every pkgdown redirect leaves a retired page and lands on a real one", {
  root <- site_root()
  lines <- readLines(file.path(root, "_pkgdown.yml"), warn = FALSE)
  pairs <- regmatches(
    lines,
    regexec('^ +- [[]"([^"]+)", "([^"]+)"[]]', lines)
  )
  pairs <- Filter(length, pairs)
  expect_gt(length(pairs), 0L)
  for (pair in pairs) {
    old <- pair[[2L]]
    new <- pair[[3L]]
    expect_false(has_article(root, old), info = paste("redirect shadows", old))
    ok <- switch(
      new,
      "articles/index.html" = TRUE,
      "news/index.html" = file.exists(file.path(root, "NEWS.md")),
      has_article(root, new)
    )
    expect_true(ok, info = paste("dead target", new))
  }
})

test_that("the Cerebro class overview lists exactly the slots of the class", {
  root <- site_root()
  lines <- readLines(
    file.path(root, "vignettes", "overview_of_cerebro_class.Rmd"),
    warn = FALSE
  )
  branch <- "^[\u251c\u2514]\u2500\u2500 ([A-Za-z_]+)"
  top <- Filter(length, regmatches(lines, regexec(branch, lines)))
  expect_setequal(vapply(top, `[[`, "", 2L), names(Cerebro$public_fields))
})
