test_that("Supplement repaint waits for the restored final workspace", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- testthat::test_path("fixtures", "supplement-paint.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cell_views.js")))
    ),
    0L
  )
})
