test_that("shared renderer restores every page and retains late Linked supplements", {
  skip_if_no_node()
  output <- system2(
    "node",
    c(
      shQuote(testthat::test_path("fixtures", "cell-view-navigation.js")),
      shQuote(viewer_test_path("www", "cell_views.js"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})
