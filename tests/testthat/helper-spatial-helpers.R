spatial_helpers_file <- file.path(
  "..",
  "..",
  "inst",
  "shiny",
  "v1.4",
  "spatial",
  "func_spatial_helpers.R"
)

if (!file.exists(spatial_helpers_file)) {
  spatial_helpers_file <- system.file(
    "shiny/v1.4/spatial/func_spatial_helpers.R",
    package = "CerebroNexus"
  )
}

sys.source(spatial_helpers_file, envir = environment())
