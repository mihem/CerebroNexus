test_that("GPU renderers upload only attributes whose arrays changed", {
  skip_if_no_node()
  runner <- testthat::test_path("fixtures", "gpu-attribute-upload.js")
  expect_identical(
    system2(
      "node",
      c(shQuote(runner), shQuote(viewer_test_path("www", "cell_points_gpu.js")))
    ),
    0L
  )
})
