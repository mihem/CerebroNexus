test_that("Spatial background accepts slider and native input events", {
  skip_if_no_node()
  expect_identical(
    system2(
      "node",
      c(
        shQuote(test_path("fixtures", "spatial-background-events.js")),
        shQuote(viewer_test_path("spatial", "js_page_helpers.js"))
      )
    ),
    0L
  )
})
