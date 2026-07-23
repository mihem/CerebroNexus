read_bundled_source <- function(...) {
  path <- system.file(..., package = "CerebroNexus")
  if (!nzchar(path)) {
    path <- testthat::test_path("..", "..", "inst", ...)
  }
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

test_that("launchers do not replace the process-wide Shiny error handler", {
  launcher <- paste(deparse(body(launchCerebroV1.4)), collapse = "\n")
  bundled_app <- read_bundled_source("app.R")

  expect_false(grepl("options(shiny.error", launcher, fixed = TRUE))
  expect_false(grepl("options(shiny.error", bundled_app, fixed = TRUE))
  expect_false(grepl("cerebro-errors.log", launcher, fixed = TRUE))
  expect_false(grepl("cerebro-errors.log", bundled_app, fixed = TRUE))
})

test_that("server startup and dataset loading fail with the original error", {
  server <- read_bundled_source("shiny", "v1.4", "shiny_server.R")

  expect_false(grepl("try_source <-", server, fixed = TRUE))
  expect_false(grepl("tab module failed to load", server, fixed = TRUE))
  expect_false(grepl("failed to load dataset", server, fixed = TRUE))
})
