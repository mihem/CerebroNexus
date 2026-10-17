test_that("gene search refresh is atomic and rejects late selections and choices", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node is unavailable")
  result <- system2(node, shQuote(c(test_path("fixtures", "gene-selector.js"),
    viewer_test_path("www", "gene-selector.js"))), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
})
