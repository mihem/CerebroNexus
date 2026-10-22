test_that("Spatial resolves the dataset id, never the CRB path", {
  source(viewer_test_path("spatial", "func_spatial_helpers.R"), local = TRUE)
  viewer_runtime_env(environment())
  files <- c(first = "shared.crb", second = "shared.crb")
  expect_identical(spatial_dataset_name(files, "second"), "second")
  expect_null(spatial_dataset_name(files, "shared.crb"))
  expect_null(spatial_dataset_name(c(first = "a.crb"), "a.crb"))
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
