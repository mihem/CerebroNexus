test_that("missing spatial coordinates do not become selectable points at zero", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_identical(
    system2(
      "node",
      c(
        shQuote(testthat::test_path(
          "fixtures",
          "spatial-missing-coordinates.js"
        )),
        shQuote(viewer_test_path("www", "cell_views.js"))
      )
    ),
    0L
  )
})
