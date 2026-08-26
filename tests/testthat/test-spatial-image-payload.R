valid_spatial_image_payload <- function() {
  list(
    histology_image = "data:image/png;base64,AA==",
    histology_image_bounds = c(
      xmin = 0,
      xmax = 100,
      ymin = 0,
      ymax = 80
    )
  )
}

test_that("spatial image payload validation accepts a contained FOV image", {
  payload <- valid_spatial_image_payload()
  coordinates <- data.frame(x = c(10, 90), y = c(5, 75))

  expect_identical(
    .validateCerebroSpatialImage(payload, "spatial_fov", coordinates),
    list(histology_images = list(`Tissue background` = payload))
  )
})

test_that("spatial image collections match uniquely named Seurat images", {
  payloads <- list(
    spatial_fov = list(
      `H&E` = valid_spatial_image_payload(),
      DAPI = valid_spatial_image_payload()
    )
  )

  expect_identical(
    .validateCerebroSpatialImages(payloads, "spatial_fov"),
    payloads
  )
  expect_null(.validateCerebroSpatialImages(NULL, "spatial_fov"))
  expect_error(
    .validateCerebroSpatialImages(
      list(unknown_fov = valid_spatial_image_payload()),
      "spatial_fov"
    ),
    "unknown_fov.*not present"
  )
  expect_error(
    .validateCerebroSpatialImages(
      structure(list(list()), names = "unknown_fov"),
      "spatial_fov"
    ),
    "unknown_fov.*not present"
  )
  expect_error(
    .validateCerebroSpatialImages(
      structure(list(list()), names = ""),
      "spatial_fov"
    ),
    "non-empty"
  )
})

test_that("Seurat spatial image payloads accept the legacy shorthand", {
  payload <- valid_spatial_image_payload()

  normalized <- .validateCerebroSpatialImages(
    list(spatial_fov = payload),
    "spatial_fov"
  )

  expect_named(normalized$spatial_fov, "Tissue background")
  expect_identical(normalized$spatial_fov[["Tissue background"]], payload)
})

test_that("legacy misc list bounds canonicalize in fixed numeric order", {
  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds <- list(
    ymax = 80,
    xmin = 0,
    ymin = 0,
    xmax = 100
  )
  coordinates <- data.frame(x = c(10, 90), y = c(5, 75))

  declared <- .validateCerebroSpatialImages(
    list(spatial_fov = payload),
    "spatial_fov"
  )
  expect_named(declared$spatial_fov, "Tissue background")

  normalized <- .validateCerebroSpatialImage(
    payload,
    "spatial_fov",
    coordinates
  )
  bounds <- normalized$histology_images[["Tissue background"]][[
    "histology_image_bounds"
  ]]
  expect_identical(
    bounds,
    c(xmin = 0, xmax = 100, ymin = 0, ymax = 80)
  )
})

test_that("histology_image is a valid canonical image label", {
  payload <- valid_spatial_image_payload()
  declared <- list(
    spatial_fov = list(histology_image = payload)
  )

  normalized <- .validateCerebroSpatialImages(declared, "spatial_fov")
  expect_named(normalized$spatial_fov, "histology_image")
  expect_identical(normalized$spatial_fov$histology_image, payload)

  coordinates <- data.frame(x = c(10, 90), y = c(5, 75))
  expect_identical(
    .validateCerebroSpatialImage(
      list(histology_image = payload),
      "spatial_fov",
      coordinates
    ),
    list(histology_images = list(histology_image = payload))
  )
})

test_that("spatial image payload validation rejects malformed images", {
  payload <- valid_spatial_image_payload()
  coordinates <- data.frame(x = c(10, 90), y = c(5, 75))

  payload$histology_image <- "not-a-data-uri"
  expect_error(
    .validateCerebroSpatialImage(payload, "spatial_fov", coordinates),
    "spatial_fov.*data:image"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds <- c(
    left = 0,
    right = 100,
    top = 0,
    bottom = 80
  )
  expect_error(
    .validateCerebroSpatialImage(payload, "spatial_fov", coordinates),
    "xmin.*xmax.*ymin.*ymax"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds[["xmax"]] <- Inf
  expect_error(
    .validateCerebroSpatialImage(payload, "spatial_fov", coordinates),
    "finite"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds[["xmin"]] <- 100
  expect_error(
    .validateCerebroSpatialImage(payload, "spatial_fov", coordinates),
    "xmin.*less than.*xmax"
  )
})

test_that("spatial image payload validation rejects unusable coordinates", {
  payload <- valid_spatial_image_payload()

  expect_error(
    .validateCerebroSpatialImage(
      payload,
      "spatial_fov",
      data.frame(row = 10, column = 20)
    ),
    "numeric.*x.*y"
  )
  expect_no_error(
    .validateCerebroSpatialImage(
      payload,
      "spatial_fov",
      data.frame(x = 101, y = 10)
    )
  )
  expect_error(
    .validateCerebroSpatialImage(
      payload,
      "spatial_fov",
      data.frame(x = NA_real_, y = 10)
    ),
    "finite"
  )
})

test_that("exportFromSeurat preserves a declared FOV image payload", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")

  object <- make_synthetic_spatial_seurat(n_cells = 12, n_genes = 10, seed = 7)
  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds <- c(
    xmin = 0,
    xmax = 100,
    ymin = 0,
    ymax = 100
  )
  object@misc$cerebro_spatial_images <- list(
    fov = list(`Tissue stain` = payload)
  )
  output <- tempfile(fileext = ".crb")

  exportFromSeurat(
    object = object,
    assay = "Spatial",
    slot = "data",
    file = output,
    experiment_name = "Synthetic FOV image",
    organism = "mouse",
    groups = c("seurat_clusters", "cell_type_final"),
    nUMI = "nCount_Spatial",
    nGene = "nFeature_Spatial",
    verbose = FALSE
  )

  spatial <- readRDS(output)$getSpatialData("fov")
  expect_identical(
    spatial$histology_images[["Tissue stain"]],
    payload
  )
})

test_that("exportFromSeurat embeds named path images for multiple FOVs", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")

  object <- make_synthetic_spatial_seurat(n_cells = 12, n_genes = 10, seed = 8)
  fov <- object@images$fov
  object@images <- list(sliceA = fov, sliceB = fov, sliceC = fov)
  image_dir <- tempfile("spatial-export-images-")
  dir.create(image_dir)
  png_path <- file.path(image_dir, "tissue.png")
  jpeg_path <- file.path(image_dir, "nuclei.jpg")
  svg_path <- file.path(image_dir, "markers.svg")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), png_path)
  writeBin(as.raw(c(0xff, 0xd8, 0xff)), jpeg_path)
  writeLines("<svg xmlns='http://www.w3.org/2000/svg'/>", svg_path)
  output <- tempfile(fileext = ".crb")

  exportFromSeurat(
    object = object,
    assay = "Spatial",
    slot = "data",
    file = output,
    experiment_name = "Synthetic named images",
    organism = "mouse",
    groups = c("seurat_clusters", "cell_type_final"),
    nUMI = "nCount_Spatial",
    nGene = "nFeature_Spatial",
    spatial_images = list(
      sliceA = c(`H&E` = png_path, DAPI = jpeg_path),
      sliceB = list(
        IF = list(
          path = svg_path,
          bounds = c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
        )
      )
    ),
    verbose = FALSE
  )

  crb <- readRDS(output)
  slice_a <- crb$getSpatialData("sliceA")
  slice_b <- crb$getSpatialData("sliceB")
  slice_c <- crb$getSpatialData("sliceC")
  expect_named(slice_a$histology_images, c("H&E", "DAPI"))
  expect_match(
    slice_a$histology_images[["H&E"]]$histology_image,
    "^data:image/png;base64,"
  )
  expect_match(
    slice_a$histology_images$DAPI$histology_image,
    "^data:image/jpeg;base64,"
  )
  expect_named(slice_b$histology_images, "IF")
  expect_match(
    slice_b$histology_images$IF$histology_image,
    "^data:image/svg\\+xml;base64,"
  )
  expect_identical(
    slice_b$histology_images$IF$histology_image_bounds,
    c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  )
  expect_identical(slice_c$histology_images, list())
})

test_that("convertSeuratToCerebro forwards spatial_images without mutation", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")

  object <- make_synthetic_spatial_seurat(n_cells = 8, n_genes = 6, seed = 9)
  image_path <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), image_path)
  declared <- list(fov = c(stain = image_path))
  original_misc <- object@misc
  received <- new.env(parent = emptyenv())
  received$spatial_images <- NULL
  testthat::local_mocked_bindings(
    exportFromSeurat = function(..., spatial_images = NULL) {
      received$spatial_images <- spatial_images
      invisible(NULL)
    },
    .package = "CerebroNexus"
  )
  withr::local_options(cerebro.quiet_runtime = TRUE)

  expect_silent(
    convertSeuratToCerebro(
      seurat_file = object,
      result_dir = tempfile("spatial-wrapper-"),
      assay = "Spatial",
      slot = "data",
      experiment_name = "Forward images",
      organism = "mouse",
      groups = c("seurat_clusters", "cell_type_final"),
      nUMI = "nCount_Spatial",
      nGene = "nFeature_Spatial",
      add_most_expressed_genes = FALSE,
      spatial_images = declared,
      verbose = FALSE
    )
  )

  expect_identical(received$spatial_images, declared)
  expect_identical(object@misc, original_misc)
})
