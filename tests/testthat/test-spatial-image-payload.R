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

tissue_image <- function(payload) list(`Tissue background` = payload)

normalize_test_spatial_image <- function(images, image_name, coordinates) {
  list(
    histology_images = .normalizeEmbeddedSpatialImages(
      images,
      coordinates,
      paste0("Spatial image payload `", image_name, "`")
    )
  )
}

test_that("spatial image payload validation accepts a contained FOV image", {
  payload <- valid_spatial_image_payload()
  coordinates <- data.frame(x = c(10, 90), y = c(5, 75))

  expect_identical(
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      coordinates
    ),
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

test_that("a Seurat spatial image payload without labels is rejected", {
  payload <- valid_spatial_image_payload()

  expect_error(
    .validateCerebroSpatialImages(
      list(spatial_fov = payload),
      "spatial_fov"
    ),
    "`spatial_fov`.*singular `histology_image`.*no longer accepted.*list\\(<label>"
  )
  payload$histology_image_bounds <- NULL
  expect_error(
    .validateCerebroSpatialImages(
      list(spatial_fov = payload),
      "spatial_fov"
    ),
    "no longer accepted"
  )
})

test_that("list-valued image bounds are rejected", {
  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds <- list(
    ymax = 80,
    xmin = 0,
    ymin = 0,
    xmax = 100
  )

  expect_error(
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      data.frame(x = c(10, 90), y = c(5, 75))
    ),
    "bounds must contain exactly"
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
    normalize_test_spatial_image(
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
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      coordinates
    ),
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
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      coordinates
    ),
    "xmin.*xmax.*ymin.*ymax"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds[["xmax"]] <- Inf
  expect_error(
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      coordinates
    ),
    "finite"
  )

  payload <- valid_spatial_image_payload()
  payload$histology_image_bounds[["xmin"]] <- 100
  expect_error(
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      coordinates
    ),
    "xmin.*less than.*xmax"
  )
})

test_that("spatial image payload validation rejects unusable coordinates", {
  payload <- valid_spatial_image_payload()

  expect_error(
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      data.frame(row = 10, column = 20)
    ),
    "numeric.*x.*y"
  )
  expect_error(
    normalize_test_spatial_image(
      tissue_image(payload),
      "spatial_fov",
      data.frame(x = 101, y = 10)
    ),
    "outside.*bounds"
  )
  expect_error(
    normalize_test_spatial_image(
      tissue_image(payload),
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
    codec = "rds",
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
    codec = "rds",
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

  expect_identical(received$spatial_images, declared)
  expect_identical(object@misc, original_misc)
})

test_that("exportFromSeurat rejects Seurat v4 VisiumV1 images only", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("SeuratObject")

  object <- make_synthetic_spatial_seurat(n_cells = 12, n_genes = 10, seed = 10)
  coordinates <- data.frame(
    x = seq_len(12),
    y = rev(seq_len(12)),
    row.names = colnames(object)
  )
  v4_images <- list(
    SlideSeq = methods::new(
      "SlideSeq",
      coordinates = coordinates,
      assay = "Spatial",
      key = "puck_"
    ),
    VisiumV1 = methods::new(
      "VisiumV1",
      image = array(0, c(2L, 2L, 3L)),
      scale.factors = structure(
        list(spot = 1, fiducial = 1, hires = 1, lowres = 1),
        class = "scalefactors"
      ),
      coordinates = data.frame(
        tissue = 1L,
        row = 1L,
        col = 1L,
        imagerow = coordinates$y,
        imagecol = coordinates$x,
        row.names = colnames(object)
      ),
      spot.radius = 1,
      assay = "Spatial",
      key = "slice_"
    )
  )

  ## Seurat 5 still builds SlideSeq (ReadSlideSeq, LoadCurioSeeker) and
  ## STARmap images, so only VisiumV1 is rejected.
  slide_seq <- object
  slide_seq@images <- list(puck = v4_images$SlideSeq)
  expect_no_error(.assertCurrentSpatialImages(slide_seq))
  slide_seq_file <- tempfile(fileext = ".crb")
  on.exit(unlink(slide_seq_file), add = TRUE)
  exportFromSeurat(
    codec = "rds",
    object = slide_seq,
    assay = "Spatial",
    slot = "data",
    file = slide_seq_file,
    experiment_name = "Seurat SlideSeq image",
    organism = "mouse",
    groups = c("seurat_clusters", "cell_type_final"),
    nUMI = "nCount_Spatial",
    nGene = "nFeature_Spatial",
    verbose = FALSE
  )
  exported <- readCerebro(slide_seq_file)
  expect_true("puck" %in% exported$availableSpatial())
  puck <- exported$getSpatialData("puck", hydrate_molecules = FALSE)
  expect_setequal(rownames(puck$coordinates), colnames(object))

  for (class_name in "VisiumV1") {
    v4_object <- object
    v4_object@images <- list(legacy = v4_images[[class_name]])
    output <- tempfile(fileext = ".crb")

    expect_error(
      exportFromSeurat(
        codec = "rds",
        object = v4_object,
        assay = "Spatial",
        slot = "data",
        file = output,
        experiment_name = "Seurat v4 image",
        organism = "mouse",
        groups = c("seurat_clusters", "cell_type_final"),
        nUMI = "nCount_Spatial",
        nGene = "nFeature_Spatial",
        verbose = FALSE
      ),
      paste0("`legacy`.*Seurat v4 `", class_name, "`.*UpdateSeuratObject"),
      info = class_name
    )
    expect_false(file.exists(output), info = class_name)
  }

  ## An object without spatial images is not affected.
  plain <- object
  plain@images <- list()
  expect_no_error(.assertCurrentSpatialImages(plain))
  expect_no_error(.assertCurrentSpatialImages(object))
})
