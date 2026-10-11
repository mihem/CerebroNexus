test_that("Indexed point picking matches the full scan across views and filters", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- testthat::test_path("fixtures", "point-grid.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cv-geom.js")))
    ),
    0L
  )
})
