cv_repo_file <- function(...) {
  parts <- c(...)
  installed <- system.file(
    do.call(file.path, as.list(parts[-1L])),
    package = "CerebroNexus"
  )
  if (nzchar(installed)) {
    return(installed)
  }
  testthat::test_path("..", "..", ...)
}

test_that("the Viewer loads shared linked-view geometry helpers", {
  ui_file <- cv_repo_file("inst", "viewer", "shiny_UI.R")
  geom_file <- cv_repo_file("inst", "viewer", "www", "cv-geom.js")
  trekker_file <- cv_repo_file("inst", "viewer", "www", "trekker.js")
  app_source <- paste(
    readLines(ui_file, warn = FALSE),
    collapse = "\n"
  )

  expect_true(file.exists(geom_file))
  expect_match(
    app_source,
    'cerebro_js("cv-geom.js", defer = TRUE)',
    fixed = TRUE
  )

  trekker_source <- paste(
    readLines(trekker_file, warn = FALSE),
    collapse = "\n"
  )
  expect_match(trekker_source, "CBGeom.inPoly", fixed = TRUE)
})
