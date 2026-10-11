test_that("deferred image ownership ends with its dataset generation", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_identical(
    system2(
      "node",
      c(
        shQuote(testthat::test_path("fixtures", "deferred-asset-lifetime.js")),
        shQuote(viewer_test_path("www", "cell_views.js")),
        shQuote(viewer_test_path("www", "dataset-context.js"))
      )
    ),
    0L
  )
})
