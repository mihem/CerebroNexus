test_that("shared cluster labels are text without background boxes", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  expect_identical(
    system2(
      "node",
      c(
        shQuote(test_path("fixtures", "group-label-text.js")),
        shQuote(viewer_test_path("www", "cell_views.js"))
      )
    ),
    0L
  )
})
