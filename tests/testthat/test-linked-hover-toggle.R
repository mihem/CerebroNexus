test_that("Linked hover can be disabled without disabling selection or Gene hover", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- testthat::test_path("fixtures", "linked-hover-toggle.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cell_views.js")))
    ),
    0L
  )
})
