sys.source(
  viewer_test_path("core", "viewer_content_contract.R"),
  envir = environment()
)
sys.source(viewer_test_path("utility_functions.R"), envir = environment())
sys.source(
  viewer_test_path("spatial", "func_spatial_helpers.R"),
  envir = environment()
)

expected_spatial_image_target <- function(
  dataset,
  spatial_name,
  image_label,
  filename
) {
  CerebroNexus:::.spatialImageBundleTarget(
    dataset,
    spatial_name,
    image_label,
    filename
  )
}
