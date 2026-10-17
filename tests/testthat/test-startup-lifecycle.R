test_that("startup waits for the active page and releases errors and empty results", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node is unavailable")
  result <- system2(node, shQuote(c(test_path("fixtures", "startup-lifecycle.js"),
    viewer_test_path("www", "startup.js"))), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(paste(result, collapse = "\n"), "PASS startup readiness", fixed = TRUE)
})
