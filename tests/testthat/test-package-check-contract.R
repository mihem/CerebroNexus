source_file <- function(...) {
  testthat::test_path("..", "..", ...)
}

skip_if_not_source_tree <- function() {
  skip_if_not(
    file.exists(source_file(".Rbuildignore")),
    "static source-tree contract"
  )
}

test_that("development-only directories are excluded from package builds", {
  skip_if_not_source_tree()
  patterns <- readLines(source_file(".Rbuildignore"), warn = FALSE)
  for (path in c(
    ".claude",
    ".loci",
    ".playwright-mcp",
    ".sisyphus",
    ".superpowers",
    "tests/node_modules"
  )) {
    expect_true(
      any(vapply(
        patterns,
        grepl,
        logical(1),
        x = path,
        perl = TRUE,
        ignore.case = TRUE
      )),
      info = path
    )
  }
})

test_that("retired share databases stay out of Git and package builds", {
  skip_if_not_source_tree()
  path <- "inst/private-data/linked-view-shares.sqlite"
  patterns <- readLines(source_file(".Rbuildignore"), warn = FALSE)
  expect_true(any(vapply(
    patterns,
    grepl,
    logical(1),
    x = path,
    perl = TRUE,
    ignore.case = TRUE
  )))
  ignored <- system2(
    "git",
    c(
      "-C",
      shQuote(source_file()),
      "check-ignore",
      "--no-index",
      shQuote(path)
    ),
    stdout = TRUE
  )
  expect_identical(ignored, path)
})

test_that("pkgdown output directory has no tracked source files", {
  skip_if_not_source_tree()
  tracked_docs <- system2(
    "git",
    c("-C", shQuote(source_file()), "ls-files", "--", "docs"),
    stdout = TRUE
  )

  expect_length(tracked_docs, 0L)
})

test_that("package and exported-app branding use the current identity", {
  description_path <- source_file("DESCRIPTION")
  if (!file.exists(description_path)) {
    description_path <- system.file("DESCRIPTION", package = "CerebroNexus")
  }
  description <- read.dcf(description_path)

  expect_identical(unname(description[1, "Package"]), "CerebroNexus")
  expect_match(description[1, "URL"], "mihem/CerebroNexus", fixed = TRUE)
  expect_identical(
    formals(createShinyApp)$welcome_message,
    "Welcome to CerebroNexus!"
  )
})

test_that("self-contained app vignette never purls interactive runApp calls", {
  skip_if_not_source_tree()
  output <- tempfile(fileext = ".R")
  on.exit(unlink(output), add = TRUE)
  knitr::purl(
    source_file(
      "vignettes",
      "articles",
      "create_a_self_contained_shiny_app.Rmd"
    ),
    output = output,
    quiet = TRUE
  )
  calls <- unlist(lapply(parse(output), r_test_calls), recursive = FALSE)
  names <- vapply(
    calls,
    function(call) paste(deparse(call[[1]]), collapse = ""),
    character(1)
  )
  expect_false(any(names %in% c("runApp", "shiny::runApp")))
})

test_that("later remains declared because bundled runtime code uses it", {
  skip_if_not_source_tree()
  description <- read.dcf(source_file("DESCRIPTION"), fields = "Imports")[[1]]
  namespace <- readLines(source_file("NAMESPACE"), warn = FALSE)
  expect_match(description, "later")
  expect_true("importFrom(later,later)" %in% namespace)
})
