test_that("background image rotates in data space with the cells", {
  skip_if_no_node()
  output <- system2(
    "node",
    c(
      shQuote(testthat::test_path("fixtures", "background-rotation.js")),
      shQuote(viewer_test_path("www", "cell_views.js"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
})
