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
  cell_views_file <- cv_repo_file("inst", "viewer", "www", "cell_views.js")
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

  cell_views_source <- paste(
    readLines(cell_views_file, warn = FALSE),
    collapse = "\n"
  )
  expect_match(cell_views_source, "CBGeom.inPoly", fixed = TRUE)
})
