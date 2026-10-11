test_that("Spatial resolves stable keys before legacy CRB paths", {
  source(viewer_test_path("spatial", "func_spatial_helpers.R"), local = TRUE)
  source(viewer_test_path("utility_functions.R"), local = TRUE)
  files <- c(first = "shared.crb", second = "shared.crb")
  expect_identical(spatial_dataset_name(files, "second"), "second")
  expect_identical(spatial_dataset_name(files, "shared.crb"), "first")
  expect_null(spatial_dataset_name(files, "missing"))
  expect_null(spatial_dataset_name(files, NA_character_))
  expect_null(spatial_dataset_name(files, character()))
  options <- list(spatial_plot_rotation = list(second = c(fov = 90)))
  expect_equal(
    spatialPlotRotation(options, spatial_dataset_name(files, "second"), "fov"),
    90
  )
  expect_equal(
    spatialPlotRotation(options, spatial_dataset_name(files, "first"), "fov"),
    0
  )
})
