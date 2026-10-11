test_that("Linked pinned tooltip preserves deferred groups and loaded values", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- testthat::test_path("fixtures", "linked-pinned-deferred.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cell_views.js")))
    ),
    0L
  )
})
