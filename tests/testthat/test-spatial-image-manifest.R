spatial_manifest_coordinates <- function() {
  data.frame(x = c(10, 90), y = c(5, 75))
}

spatial_manifest_payload <- function(uri = "data:image/png;base64,AA==") {
  list(
    histology_image = uri,
    histology_image_bounds = c(
      xmin = 0,
      xmax = 100,
      ymin = 0,
      ymax = 80
    )
  )
}

spatial_manifest_data <- function(images = list()) {
  list(
    coordinates = spatial_manifest_coordinates(),
    expression = matrix(1:4, nrow = 2),
    histology_images = images
  )
}

test_that("Cerebro stores multiple named spatial images canonically", {
  crb <- Cerebro$new()
  images <- list(
    `H&E` = spatial_manifest_payload(),
    DAPI = spatial_manifest_payload("data:image/jpeg;base64,AQ==")
  )

  crb$addSpatialData("section 1", spatial_manifest_data(images))

  stored <- crb$getSpatialData("section 1")
  expect_named(stored$histology_images, c("H&E", "DAPI"))
  expect_identical(stored$histology_images, images)
  expect_null(stored[["histology_image"]])
  expect_null(stored[["histology_image_bounds"]])
})

test_that("Cerebro accepts coordinates-only spatial entries", {
  crb <- Cerebro$new()
  crb$addSpatialData("coordinates", spatial_manifest_data(list()))

  expect_identical(crb$getSpatialData("coordinates")$histology_images, list())
})

test_that("legacy multi-path app spatial images receive distinct labels", {
  catalogs <- list(Dataset = list(section = character()))
  root <- withr::local_tempdir()
  paths <- file.path(root, c("first.png", "second.png"))
  lapply(paths, writeLines, text = "IMAGE")
  normalized <- .normalizeAppSpatialImages(
    list(Dataset = paths),
    catalogs
  )

  expect_named(
    normalized$Dataset$section,
    c("Tissue background 1", "Tissue background 2")
  )
  expect_identical(
    unname(unlist(normalized$Dataset$section, use.names = FALSE)),
    paths
  )
})

test_that("spatial image labels must be non-empty and unique", {
  crb <- Cerebro$new()
  empty_label <- structure(list(spatial_manifest_payload()), names = "")
  duplicate_labels <- structure(
    list(spatial_manifest_payload(), spatial_manifest_payload()),
    names = c("DAPI", "DAPI")
  )

  expect_error(
    crb$addSpatialData("empty", spatial_manifest_data(empty_label)),
    "Spatial data `empty`.*<empty>"
  )
  expect_error(
    crb$addSpatialData("duplicate", spatial_manifest_data(duplicate_labels)),
    "Spatial data `duplicate`.*duplicate.*DAPI"
  )
})

test_that("serialized spatial getter is self-contained without package helpers", {
  crb <- Cerebro$new()
  crb$spatial$legacy <- list(
    coordinates = spatial_manifest_coordinates(),
    expression = matrix(1:4, nrow = 2),
    histology_image = "data:image/png;base64,AA==",
    histology_image_bounds = list(
      ymax = 80,
      xmin = 0,
      ymin = 0,
      xmax = 100
    )
  )
  crb$spatial$canonical <- spatial_manifest_data(
    list(DAPI = spatial_manifest_payload())
  )
  method_environment <- environment(crb$getSpatialData)
  parent.env(method_environment) <- baseenv()
  path <- tempfile(fileext = ".crb")
  saveRDS(crb, path)

  isolated <- readRDS(path)
  legacy <- isolated$getSpatialData("legacy")
  canonical <- isolated$getSpatialData("canonical")

  expect_named(legacy$histology_images, "Tissue background")
  expect_identical(
    legacy$histology_images[["Tissue background"]]$histology_image,
    "data:image/png;base64,AA=="
  )
  expect_identical(
    legacy$histology_images[["Tissue background"]]$histology_image_bounds,
    c(xmin = 0, xmax = 100, ymin = 0, ymax = 80)
  )
  expect_named(canonical$histology_images, "DAPI")
  expect_identical(canonical$histology_images$DAPI, spatial_manifest_payload())
})

test_that("spatial image manifests reject malformed payloads", {
  crb <- Cerebro$new()

  invalid_uri <- list(DAPI = spatial_manifest_payload("not-a-data-uri"))
  expect_error(
    crb$addSpatialData("invalid URI", spatial_manifest_data(invalid_uri)),
    "DAPI.*data:image"
  )

  invalid_bounds <- spatial_manifest_payload()
  invalid_bounds$histology_image_bounds <- c(
    left = 0,
    right = 100,
    top = 0,
    bottom = 80
  )
  expect_error(
    crb$addSpatialData(
      "invalid bounds",
      spatial_manifest_data(list(DAPI = invalid_bounds))
    ),
    "xmin.*xmax.*ymin.*ymax"
  )

  outside <- spatial_manifest_payload()
  outside$histology_image_bounds[["xmax"]] <- 50
  expect_error(
    crb$addSpatialData("outside", spatial_manifest_data(list(DAPI = outside))),
    "outside.*bounds"
  )
})

test_that("missing spatial image bounds are derived from coordinates", {
  payload <- spatial_manifest_payload()
  payload$histology_image_bounds <- NULL

  normalized <- .normalizeEmbeddedSpatialImages(
    list(DAPI = payload),
    spatial_manifest_coordinates(),
    "section 1"
  )

  expect_identical(
    normalized$DAPI$histology_image_bounds,
    c(xmin = 10, xmax = 90, ymin = 5, ymax = 75)
  )
})

test_that("legacy singular spatial images normalize on read", {
  crb <- Cerebro$new()
  crb$spatial$legacy <- list(
    coordinates = spatial_manifest_coordinates(),
    expression = matrix(1:4, nrow = 2),
    histology_image = "data:image/png;base64,AA==",
    histology_image_bounds = c(xmin = 0, xmax = 100, ymin = 0, ymax = 80)
  )

  normalized <- crb$getSpatialData("legacy")

  expect_named(normalized$histology_images, "Tissue background")
  expect_identical(
    normalized$histology_images[["Tissue background"]]$histology_image,
    "data:image/png;base64,AA=="
  )
  expect_null(normalized[["histology_image"]])
  expect_null(normalized[["histology_image_bounds"]])
})

test_that("legacy fixture list bounds normalize to a numeric vector", {
  for (fixture in "demo_spatial_merfish.crb") {
    path <- system.file(
      "extdata",
      "examples",
      fixture,
      package = "CerebroNexus"
    )
    skip_if(
      path == "" || !file.exists(path),
      message = paste(fixture, "missing")
    )
    crb <- readRDS(path)
    spatial_name <- crb$availableSpatial()[[1L]]

    normalized <- .normalizeSpatialDataImages(
      crb$spatial[[spatial_name]],
      spatial_name
    )
    bounds <- normalized$histology_images[["Tissue background"]][[
      "histology_image_bounds"
    ]]

    expect_identical(typeof(bounds), "double", info = fixture)
    expect_identical(
      names(bounds),
      c("xmin", "xmax", "ymin", "ymax"),
      info = fixture
    )
  }
})

test_that("Xenium colour demo loads images from files", {
  path <- system.file(
    "extdata",
    "examples",
    "demo_spatial_xenium.crb",
    package = "CerebroNexus"
  )
  skip_if(path == "" || !file.exists(path), message = "Xenium fixture missing")

  crb <- readRDS(path)
  expect_named(crb$spatial, c("fov", "fov_colour"))

  fov <- crb$getSpatialData("fov")
  colour <- crb$getSpatialData("fov_colour")
  expect_identical(fov$histology_images, list())
  expect_identical(colour$histology_images, list())
  expect_identical(rownames(colour$coordinates), rownames(fov$coordinates))

  expect_identical(
    unname(colour$coordinates$x),
    unname(-fov$coordinates$y)
  )
  expect_identical(
    unname(colour$coordinates$y),
    unname(fov$coordinates$x)
  )

  image_dir <- system.file(
    "extdata",
    "examples",
    "spatial",
    "xenium",
    package = "CerebroNexus"
  )
  expect_true(dir.exists(image_dir))
  expect_true(all(file.exists(file.path(
    image_dir,
    c("dapi.png", "pink_stain_90.png", "fluorescent_yellow_90.png")
  ))))

  dapi <- png::readPNG(file.path(image_dir, "dapi.png"))
  pink <- png::readPNG(file.path(image_dir, "pink_stain_90.png"))
  yellow <- png::readPNG(file.path(image_dir, "fluorescent_yellow_90.png"))
  expect_identical(dim(pink)[1:2], rev(dim(dapi)[1:2]))
  expect_identical(dim(yellow), dim(pink))
})

test_that("canonical manifests reject legacy list bounds", {
  payload <- spatial_manifest_payload()
  payload$histology_image_bounds <- as.list(payload$histology_image_bounds)

  expect_error(
    .normalizeEmbeddedSpatialImages(
      list(DAPI = payload),
      spatial_manifest_coordinates(),
      "Spatial data `canonical`"
    ),
    "bounds must contain exactly"
  )
})

test_that("canonical spatial image manifests take precedence over legacy fields", {
  data <- spatial_manifest_data(list(DAPI = spatial_manifest_payload()))
  data$histology_image <- "not-a-data-uri"
  data$histology_image_bounds <- c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)

  normalized <- .normalizeSpatialDataImages(data, "section 1")

  expect_named(normalized$histology_images, "DAPI")
  expect_null(normalized[["histology_image"]])
  expect_null(normalized[["histology_image_bounds"]])
})

test_that("spatial image paths normalize labels, descriptors, and file errors", {
  image_dir <- tempfile("spatial-image-paths-")
  dir.create(image_dir)
  png_path <- file.path(image_dir, "tissue.png")
  jpeg_path <- file.path(image_dir, "nuclei.jpeg")
  svg_path <- file.path(image_dir, "markers.svg")
  tiff_path <- file.path(image_dir, "unsupported.tiff")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), png_path)
  writeBin(as.raw(c(0xff, 0xd8, 0xff)), jpeg_path)
  writeLines("<svg xmlns='http://www.w3.org/2000/svg'/>", svg_path)
  writeBin(as.raw(c(0x49, 0x49)), tiff_path)

  normalized <- .normalizeSpatialImagePaths(
    list(
      sliceA = c(`H&E` = png_path, DAPI = jpeg_path),
      sliceB = list(
        IF = list(
          path = svg_path,
          bounds = c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
        )
      )
    ),
    c("sliceA", "sliceB", "sliceC"),
    "`spatial_images`"
  )

  expect_named(normalized$sliceA, c("H&E", "DAPI"))
  expect_identical(normalized$sliceA[["H&E"]], list(path = png_path))
  expect_identical(normalized$sliceA$DAPI, list(path = jpeg_path))
  expect_identical(normalized$sliceB$IF$path, svg_path)

  expect_error(
    .normalizeSpatialImagePaths(
      list(unknown = c(stain = png_path)),
      c("sliceA", "sliceB"),
      "`spatial_images`"
    ),
    "unknown.*not present"
  )
  expect_error(
    .normalizeSpatialImagePaths(
      list(sliceA = structure(png_path, names = "")),
      "sliceA",
      "`spatial_images`"
    ),
    "spatial `sliceA`.*<empty>"
  )
  expect_error(
    .normalizeSpatialImagePaths(
      list(sliceA = structure(c(png_path, jpeg_path), names = c("IF", "IF"))),
      "sliceA",
      "`spatial_images`"
    ),
    "spatial `sliceA`.*duplicate.*IF"
  )
  expect_error(
    .normalizeSpatialImagePaths(
      list(sliceA = c(stain = file.path(image_dir, "missing.png"))),
      "sliceA",
      "`spatial_images`"
    ),
    "does not exist"
  )
  expect_error(
    .normalizeSpatialImagePaths(
      list(sliceA = c(stain = image_dir)),
      "sliceA",
      "`spatial_images`"
    ),
    "regular file"
  )
  expect_error(
    .normalizeSpatialImagePaths(
      list(sliceA = c(stain = tiff_path)),
      "sliceA",
      "`spatial_images`"
    ),
    "png.*jpg.*jpeg.*svg"
  )
  expect_error(
    .normalizeSpatialImagePaths(
      list(
        sliceA = list(
          stain = list(
            path = png_path,
            bounds = c(xmin = 0, xmax = 0, ymin = 0, ymax = 100)
          )
        )
      ),
      "sliceA",
      "`spatial_images`"
    ),
    "xmin.*less than.*xmax"
  )
})

test_that("argument and misc spatial images cannot claim the same label", {
  coordinates <- list(
    sliceA = data.frame(x = c(0, 100), y = c(0, 100))
  )
  misc <- list(
    sliceA = list(`H&E` = spatial_manifest_payload())
  )
  argument <- list(
    sliceA = list(`H&E` = list(path = tempfile(fileext = ".png")))
  )
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), argument$sliceA[["H&E"]]$path)

  expect_error(
    .mergeSpatialImageDeclarations(misc, argument, coordinates),
    "sliceA.*H&E.*both"
  )
})
