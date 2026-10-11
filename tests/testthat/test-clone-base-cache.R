test_that("Clonal base caching invalidates when its rendered content changes", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- testthat::test_path("fixtures", "clone-base-cache.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cell_views.js")))
    ),
    0L
  )
})
