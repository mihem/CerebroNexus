spatial_helpers_file <- file.path(
  "..",
  "..",
  "inst",
  "viewer",
  "spatial",
  "func_spatial_helpers.R"
)
utility_helpers_file <- file.path(
  "..",
  "..",
  "inst",
  "viewer",
  "utility_functions.R"
)
viewer_contract_file <- file.path(
  "..",
  "..",
  "inst",
  "viewer",
  "core",
  "viewer_content_contract.R"
)

if (!file.exists(spatial_helpers_file)) {
  spatial_helpers_file <- system.file(
    "viewer/spatial/func_spatial_helpers.R",
    package = "CerebroNexus"
  )
}
if (!file.exists(utility_helpers_file)) {
  utility_helpers_file <- system.file(
    "viewer/utility_functions.R",
    package = "CerebroNexus"
  )
}
if (!file.exists(viewer_contract_file)) {
  viewer_contract_file <- system.file(
    "viewer/core/viewer_content_contract.R",
    package = "CerebroNexus"
  )
}

sys.source(viewer_contract_file, envir = environment())
sys.source(utility_helpers_file, envir = environment())
sys.source(spatial_helpers_file, envir = environment())

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
