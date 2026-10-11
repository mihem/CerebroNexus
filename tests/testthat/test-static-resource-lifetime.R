test_that("static resource caches release old datasets and reject late eviction", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  output <- system2(
    "node",
    c(
      shQuote(testthat::test_path("fixtures/static-resource-lifetime.js")),
      shQuote(viewer_test_path("www", "cell_views.js"))
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(output, "status"), info = paste(output, collapse = "\n"))
  result <- jsonlite::fromJSON(tail(output, 1L))
  expect_true(result$pass)
  expect_true(result$lateFailureSafe)
  expect_equal(result$rows$retainedBytes, rep(8000000, 10L))
  expect_equal(result$afterInvalidationEntries, 0L)
})
