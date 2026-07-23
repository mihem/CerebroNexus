repo_file <- function(...) {
  installed <- system.file(..., package = "CerebroNexus")
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", "inst", ...)
}

test_that("About renders the version supplied by app configuration", {
  about_source <- paste(
    readLines(repo_file("shiny", "v1.4", "about", "server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    about_source,
    'Cerebro.options[["cerebro_version"]]',
    fixed = TRUE
  )
  expect_false(grepl('version <- "2.1.1"', about_source, fixed = TRUE))
  expect_false(grepl("packageVersion", about_source, fixed = TRUE))
})

test_that("the source demo supplies its version without a package lookup", {
  app_source <- paste(
    readLines(repo_file("app.R"), warn = FALSE),
    collapse = "\n"
  )
  expected_version <- as.character(utils::packageVersion("CerebroNexus"))

  expect_match(
    app_source,
    paste0('"cerebro_version" = "', expected_version, '"'),
    fixed = TRUE
  )
  expect_false(grepl("packageVersion", app_source, fixed = TRUE))
})
