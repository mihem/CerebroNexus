test_that("Linked views defaults to individual spatial scenes and preserves edits", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node is unavailable")
  result <- system2(
    node,
    shQuote(c(
      test_path("fixtures", "linked-spatial-defaults.js"),
      viewer_test_path("www", "cell_views.js")
    )),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(
    paste(result, collapse = "\n"),
    "PASS individual-scene defaults",
    fixed = TRUE
  )
})
