#!/usr/bin/env Rscript
# 20 — Shiny app generation (embedded crbs)
#
# Builds a traditional Cerebro Shiny app from the .crb files produced by
# 10_convert_embedded.R.
#
# Depends on: result/10_convert_embedded/*.crb
# Output: result/20_app_embedded/

pkg_root <- file.path(dirname(getwd()), "..")
devtools::load_all(pkg_root)

result_dir <- "result/20_app_embedded"

if (dir.exists(result_dir)) {
  unlink(file.path(result_dir, "*"), recursive = TRUE, force = TRUE)
} else {
  dir.create(result_dir, recursive = TRUE)
}

## ── Single-dataset version (for quick smoke testing) ────────────────────────
createShinyApp(
  result_dir = result_dir,
  cerebro_data = c(
    `Myeloid_snRNAseq` = "result/10_convert_embedded/cerebro_Dura_Mater_-_Myeloid_cells_snRNaseq.crb"),
  colors = list(
    `Myeloid_snRNAseq` = list(
      `condition` = c(Ctrl = "black", MS = "#3d70b5"),
      `cluster` = c(
        "inflamMono" = "#75ae2b",
        "CAM"        = "#ca8e15",
        "IFN-CAM"    = "#ec67a2",
        "Granulo"    = "#cf6ea8",
        "Mast"       = "#e26aa5"))
  ),
  point_size = list(
    `overview_projection_point_size` = 2),
  cerebro_options = list(exclude_trivial_metadata = TRUE),
  port = 8082,
  max_request_size = 10000,
  crb_pick_smallest_file = FALSE,
  show_upload_ui = FALSE,
  welcome_message = "<h2 style='text-align: center; margin-top: 0px'><strong>Human Dura Atlas</strong></h2>
      <p style='text-align: center'>Welcome to the Human Dura Atlas (single dataset smoke test)</p>"
)

## ── Multi-dataset version (uncomment when multi-crb is stable) ──────────────
# colors <- list(
#   `Myeloid_snRNAseq` = list(
#     `condition` = c(Ctrl = "black", MS = "#3d70b5"),
#     `cluster` = c(
#       "inflamMono" = "#75ae2b",
#       "CAM"        = "#ca8e15",
#       "IFN-CAM"    = "#ec67a2",
#       "Granulo"    = "#cf6ea8",
#       "Mast"       = "#e26aa5")
#   ),
#   `Ctrl-Fibro_spatialseq` = list(
#     `cluster` = c(
#       "duraFibro1-3" = "#66c69b",
#       "duraFibro3"   = "#ee756d",
#       "bordFibro"    = "#87be4d",
#       "duraFibro4"   = "#d49005")
#   ),
#   `MS-Fibro_spatialseq` = list(
#     `cluster` = c(
#       "duraFibro1-3" = "#66c69b",
#       "duraFibro3"   = "#ee756d",
#       "bordFibro"    = "#87be4d",
#       "duraFibro4"   = "#d49005")
#   )
# )
#
# createShinyApp(
#   result_dir = result_dir,
#   cerebro_data = c(
#     `Myeloid_snRNAseq`      = "result/10_convert_embedded/cerebro_Dura_Mater_-_Myeloid_cells_snRNaseq.crb",
#     `Ctrl-Fibro_spatialseq` = "result/10_convert_embedded/cerebro_Ctrl_Dura_Mater_-_Fibroblasts_spatialseq.crb",
#     `MS-Fibro_spatialseq`   = "result/10_convert_embedded/cerebro_MS_Dura_Mater_-_Fibroblasts_spatialseq.crb",
#     `TCR-BCR`               = "result/10_convert_embedded/cerebro_PBMC_1002_Post_TCR_BCR.crb"),
#   spatial_plot_rotation  = list(`Ctrl-Fibro_spatialseq` = -61, `MS-Fibro_spatialseq` = 205),
#   spatial_images = list(
#     `Ctrl-Fibro_spatialseq` = c("result/10_convert_embedded/Xenium_Ctrl_ROI_HE.jpg"),
#     `MS-Fibro_spatialseq`   = c("result/10_convert_embedded/Xenium_MS_ROI_HE.jpg")),
#   spatial_images_flip_x  = list(`Ctrl-Fibro_spatialseq` = FALSE, `MS-Fibro_spatialseq` = FALSE),
#   spatial_images_flip_y  = list(`Ctrl-Fibro_spatialseq` = TRUE,  `MS-Fibro_spatialseq` = TRUE),
#   spatial_images_scale_x = list(`Ctrl-Fibro_spatialseq` = 0.9,   `MS-Fibro_spatialseq` = 0.95),
#   spatial_images_scale_y = list(`Ctrl-Fibro_spatialseq` = 1,     `MS-Fibro_spatialseq` = 1.45),
#   point_size = list(
#     `overview_projection_point_size`   = 2,
#     `trajectory_point_size`            = NULL,
#     `expression_projection_point_size` = NULL,
#     `spatial_projection_point_size`    = 17),
#   colors = colors,
#   cerebro_options = list(exclude_trivial_metadata = TRUE),
#   variable_to_compare = TRUE,
#   port = 8082,
#   max_request_size = 10000,
#   enable_auth = FALSE,
#   admin_user = "admin",
#   admin_pass = "admin#123",
#   auth_passphrase = "123123",
#   crb_pick_smallest_file = FALSE,
#   show_upload_ui = FALSE,
#   welcome_message = "<h2 style='text-align: center; margin-top: 0px'><strong>Human Dura Atlas</strong></h3>
#       <p style='text-align: center'>Welcome to the Human Dura Atlas</p>"
# )
