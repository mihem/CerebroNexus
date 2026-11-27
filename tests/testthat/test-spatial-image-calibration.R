## Tissue pictures placed from their calibration (issue #281): pixel size,
## origin and orientation, the same rule for every import that knows them.

test_that("a scaled picture covers the full-resolution extent and opens flipped", {
  placement <- .spatialImageScaled(width = 80, height = 60, scale = 0.05)
  expect_equal(
    placement$histology_image_bounds,
    c(xmin = 0, xmax = 1600, ymin = 0, ymax = 1200)
  )
  ## the Viewer draws row 0 at ymax; spot rows count downwards
  expect_identical(
    placement$histology_alignment,
    list(
      geometry = "live",
      viewer_setting = list(flip_x = FALSE, flip_y = TRUE)
    )
  )
  expect_error(.spatialImageScaled(80, 60, 0), "must be positive")
  expect_error(.spatialImageScaled(80, 60, NA_real_), "must be positive")
})

test_that("orientation follows where the first pixel lands", {
  ## row 0 already at the top of the data: no flip
  upright <- .spatialImagePlacement(c(10, 100), c(110, 0))
  expect_equal(
    upright$histology_image_bounds,
    c(xmin = 10, xmax = 110, ymin = 0, ymax = 100)
  )
  expect_false(upright$histology_alignment$viewer_setting$flip_y)
  expect_false(upright$histology_alignment$viewer_setting$flip_x)
  ## column 0 on the right: mirrored horizontally
  mirrored <- .spatialImagePlacement(c(110, 0), c(10, 100))
  expect_true(mirrored$histology_alignment$viewer_setting$flip_x)
  expect_true(mirrored$histology_alignment$viewer_setting$flip_y)
  expect_error(
    .spatialImagePlacement(c(0, 0), c(0, 10)),
    "two distinct, finite corners"
  )
})

## A Visium object as Seurat builds it from a Space Ranger folder: spots in
## full-resolution pixels, the lowres picture and both scale factors.
make_visium_seurat <- function(dir, width = 80L, height = 60L) {
  spatial <- file.path(dir, "spatial")
  dir.create(spatial, recursive = TRUE, showWarnings = FALSE)
  picture <- array(0.9, dim = c(height, width, 3L))
  png::writePNG(picture, file.path(spatial, "tissue_lowres_image.png"))
  writeLines(
    paste0(
      '{"spot_diameter_fullres": 10, "fiducial_diameter_fullres": 15, ',
      '"tissue_hires_scalef": 0.2, "tissue_lowres_scalef": 0.05}'
    ),
    file.path(spatial, "scalefactors_json.json")
  )
  n <- 12L
  positions <- data.frame(
    barcode = sprintf("S%02d-1", seq_len(n)),
    in_tissue = 1L,
    array_row = seq_len(n),
    array_col = seq_len(n),
    pxl_row_in_fullres = round(seq(100, 1100, length.out = n)),
    pxl_col_in_fullres = round(seq(1500, 100, length.out = n))
  )
  utils::write.csv(
    positions,
    file.path(spatial, "tissue_positions.csv"),
    row.names = FALSE
  )
  image <- suppressWarnings(Seurat::Read10X_Image(
    spatial,
    image.name = "tissue_lowres_image.png"
  ))
  counts <- methods::as(
    matrix(
      stats::rpois(10L * n, 3),
      nrow = 10L,
      dimnames = list(paste0("Gene", seq_len(10L)), positions$barcode)
    ),
    "CsparseMatrix"
  )
  object <- SeuratObject::CreateSeuratObject(counts, assay = "Spatial")
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object$cluster <- factor(rep(c("A", "B"), length.out = n))
  image <- image[SeuratObject::Cells(object)]
  SeuratObject::DefaultAssay(image) <- "Spatial"
  object[["slice1"]] <- image
  object
}

export_visium <- function(object, ...) {
  file <- tempfile(fileext = ".crb")
  suppressWarnings(suppressMessages(exportFromSeurat(
    object,
    file = file,
    assay = "Spatial",
    slot = "data",
    experiment_name = "visium",
    organism = "hg",
    groups = "cluster",
    nUMI = "nCount_Spatial",
    nGene = "nFeature_Spatial",
    projections = character(),
    ...
  )))
  readCerebro(file)$getSpatialData("slice1", hydrate_expression = FALSE)
}

test_that("a Seurat Visium section brings its own picture, placed by its scale factor", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")
  object <- make_visium_seurat(withr::local_tempdir())
  spatial <- export_visium(object)
  ## Seurat's x is the pixel column, y the pixel row
  expect_equal(range(spatial$coordinates$x), c(100, 1500))
  expect_equal(range(spatial$coordinates$y), c(100, 1100))

  expect_identical(names(spatial$histology_images), "lowres")
  picture <- spatial$histology_images$lowres
  expect_equal(
    picture$histology_image_bounds,
    c(xmin = 0, xmax = 1600, ymin = 0, ymax = 1200)
  )
  expect_true(picture$histology_alignment$viewer_setting$flip_y)
  pixels <- png::readPNG(jsonlite::base64_dec(sub(
    "^data:image/png;base64,",
    "",
    picture$histology_image
  )))
  expect_identical(dim(pixels)[1:2], c(60L, 80L))
})

test_that("declared pictures, FALSE and coordinate transforms keep the object's picture out", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  object <- make_visium_seurat(dir)

  expect_length(
    export_visium(object, spatial_images = FALSE)$histology_images,
    0L
  )

  declared <- file.path(dir, "declared.png")
  png::writePNG(array(0.2, dim = c(10L, 10L, 3L)), declared)
  images <- export_visium(
    object,
    spatial_images = list(slice1 = c(Declared = declared))
  )$histology_images
  expect_identical(names(images), "Declared")
  expect_null(images$Declared$histology_alignment)

  rotated <- export_visium(
    object,
    spatial_coordinate_transforms = list(
      slice1 = list(rotation_degrees = 90, scale = 1)
    )
  )
  expect_length(rotated$histology_images, 0L)
})

test_that("a hires picture uses the hires factor, and a picture that misses the spots is left out", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("png")
  object <- make_visium_seurat(withr::local_tempdir())
  image <- object[["slice1"]]
  ## 640 x 480 is longer than Space Ranger's 600 px lowres picture
  image@image <- array(0.9, dim = c(480L, 640L, 3L))
  placed <- .seuratVisiumImage(image, SeuratObject::GetTissueCoordinates(image))
  expect_identical(names(placed), "hires")
  expect_equal(
    placed$hires$histology_image_bounds,
    c(xmin = 0, xmax = 3200, ymin = 0, ymax = 2400)
  )

  image@image <- array(0.9, dim = c(20L, 20L, 3L))
  expect_message(
    expect_null(.seuratVisiumImage(
      image,
      SeuratObject::GetTissueCoordinates(image)
    )),
    "does not cover the spots"
  )
})
