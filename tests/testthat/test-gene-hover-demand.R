test_that("Gene hover metadata is requested on demand and retires stale work", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "node is unavailable")
  result <- system2(
    node,
    c(
      shQuote(test_path("fixtures", "gene-hover-demand.js")),
      shQuote(viewer_test_path("www", "cell_views.js"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_true(any(grepl("PASS pending Gene IDs", result, fixed = TRUE)))
})
