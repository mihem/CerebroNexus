test_that("the million-cell renderer is loaded before cell views", {
  renderer <- viewer_test_path("www", "cell_points_gpu.js")
  expect_true(file.exists(renderer))
  renderer_source <- paste(readLines(renderer, warn = FALSE), collapse = "\n")
  expect_match(renderer_source, "backend: 'webgpu'", fixed = TRUE)

  ui <- paste(
    readLines(viewer_test_path("shiny_UI.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(ui, 'cerebro_js("cell_points_gpu.js")', fixed = TRUE)
  expect_lt(
    regexpr("cell_points_gpu.js", ui, fixed = TRUE)[1],
    regexpr("cell_views.js", ui, fixed = TRUE)[1]
  )

  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(engine, "CerebroPointRenderer.create", fixed = TRUE)
  expect_match(engine, "setData", fixed = TRUE)
})
