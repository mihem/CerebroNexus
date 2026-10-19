test_that("shared lifecycle and background models preserve view semantics", {
  node <- skip_if_no_node()
  result <- system2(
    node,
    shQuote(c(
      test_path("fixtures", "viewer-shared-models.js"),
      dirname(viewer_test_path("www", "cell_views.js"))
    )),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(paste(result, collapse = "\n"), "PASS backgrounds", fixed = TRUE)
})
