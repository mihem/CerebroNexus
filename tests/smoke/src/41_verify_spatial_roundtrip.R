#!/usr/bin/env Rscript
# 41 - Spatial module round-trip (group D)
#
# Confirms that:
#   * exportFromSeurat persists spatial coordinates + expression for FOV /
#     Visium / Xenium assays into the .crb (Cerebro_v1.3$spatial),
#   * the current inst/shiny/v1.4 UI entrypoint can be source()d standalone,
#   * the Ctrl/MS Fibro crbs from 10_convert_embedded.R carry availableSpatial()
#     non-empty -- the exact precondition the dynamic Spatial-tab insertion
#     in shiny_server.R reacts to.

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(shiny)
})

result_dir <- "result/41_verify_spatial_roundtrip"
unlink(result_dir, recursive = TRUE, force = TRUE)
dir.create(result_dir, recursive = TRUE)

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop("None of the expected files exist:\n", paste(paths, collapse = "\n"), call. = FALSE)
  }
  hit
}

## --- 1. crb spatial round-trip ---
crbs <- c(
  ctrl = first_existing(c(
    "result/10_convert_embedded/cerebro_Ctrl_Dura_Mater_-_Fibroblasts_spatialseq.crb",
    "result/10_convert_embedded/cerebro_10.3.1_sc_Ctrl_duraFibro_both_integrated_renamed.crb"
  )),
  ms   = first_existing(c(
    "result/10_convert_embedded/cerebro_MS_Dura_Mater_-_Fibroblasts_spatialseq.crb",
    "result/10_convert_embedded/cerebro_10.3.1_sc_MS_duraFibro_both_integrated_renamed.crb"
  ))
)
stopifnot(all(file.exists(crbs)))

for (lab in names(crbs)) {
  ds <- readRDS(crbs[[lab]])
  stopifnot(inherits(ds, "Cerebro_v1.3"))
  imgs <- ds$availableSpatial()
  stopifnot(length(imgs) >= 1)
  cat(sprintf("  [%s] availableSpatial() = %s\n", lab, paste(imgs, collapse = ",")))
  d <- ds$getSpatialData(imgs[1])
  stopifnot(is.list(d), "coordinates" %in% names(d), "expression" %in% names(d))
  cat(sprintf("           coords %dx%d, expr %dx%d\n",
              nrow(d$coordinates), ncol(d$coordinates),
              nrow(d$expression),  ncol(d$expression)))
}

## --- 2. v1.4 UI entrypoint source-only check ---
shiny_root <- system.file("shiny/v1.4", package = "cerebroAppLite")
env <- new.env()
env$Cerebro.options <- list(cerebro_root = system.file(package = "cerebroAppLite"))
source(file.path(shiny_root, "utility_functions.R"), local = env)
source(file.path(shiny_root, "shiny_UI.R"), local = env)
stopifnot(inherits(env$ui, c("shiny.tag", "shiny.tag.list")))
cat("  shiny_UI.R sourced; ui class =", class(env$ui)[1], "\n")

writeLines("ok", file.path(result_dir, "PASS"))
cat("[41] spatial round-trip + module source check passed.\n")
