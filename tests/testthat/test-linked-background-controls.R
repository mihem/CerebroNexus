test_that("Linked background settings isolate ROI images and deferred state", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node unavailable")
  result <- system2(node, shQuote(c(
    test_path("fixtures", "linked-background-controls.js"),
    viewer_test_path("www", "cell_views.js")
  )), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(paste(result, collapse = "\n"), "PASS linked background", fixed = TRUE)
})
