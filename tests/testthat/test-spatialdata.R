## Zarr stores: plain AnnData (v2 and v3) and SpatialData.
##
## The fixtures were written by the real anndata and zarr-python (see
## fixtures/anndata/make_zarr_fixtures.py). small_sd_cells.zarr's shapes are
## written by make_zarr_shapes.R: the point of instance id i is (10 + i, 200 - 2 i).

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- anndata_test_fixture
quiet <- function(x) suppressWarnings(suppressMessages(x))

## ---------------------------------------------------------------------------
## plain AnnData as Zarr
## ---------------------------------------------------------------------------

test_that("a Zarr v3 and a Zarr v2 AnnData read the same as the .h5ad of the same data", {
  twin <- quiet(readH5adAsSCE(fixture("small_zarr_twin.h5ad")))
  for (store in c("small_v3.zarr", "small_v2.zarr")) {
    sce <- quiet(readH5adAsSCE(fixture(store)))
    expect_identical(dim(sce), dim(twin), info = store)
    expect_identical(colnames(sce), colnames(twin), info = store)
    expect_identical(rownames(sce), rownames(twin), info = store)
    expect_equal(
      as.matrix(SummarizedExperiment::assay(sce, "X")),
      as.matrix(SummarizedExperiment::assay(twin, "X")),
      info = store
    )
    a <- as.data.frame(SummarizedExperiment::colData(sce))
    b <- as.data.frame(SummarizedExperiment::colData(twin))
    expect_identical(names(a), names(b), info = store)
    for (column in names(b)) {
      expect_equal(a[[column]], b[[column]], info = paste(store, column))
    }
    expect_equal(
      SingleCellExperiment::reducedDim(sce, "UMAP"),
      SingleCellExperiment::reducedDim(twin, "UMAP"),
      info = store
    )
  }
})

test_that("a Zarr store converts to a .crb", {
  crb <- readCerebro(quiet(convertH5adToCerebro(
    fixture("small_v3.zarr"),
    withr::local_tempdir(),
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_equal(nrow(crb$getMetaData()), 60)
  expect_equal(length(crb$getGeneNames()), 30)
})

test_that("a Blosc chunk of another format version says so", {
  store <- withr::local_tempdir()
  dir.create(file.path(store, "x"))
  writeLines(
    '{"zarr_format":3,"node_type":"array","shape":[4],"data_type":"int32","chunk_grid":{"name":"regular","configuration":{"chunk_shape":[4]}},"chunk_key_encoding":{"name":"default","configuration":{"separator":"/"}},"fill_value":0,"codecs":[{"name":"bytes","configuration":{"endian":"little"}},{"name":"blosc","configuration":{}}]}',
    file.path(store, "x", "zarr.json")
  )
  dir.create(file.path(store, "x", "c"))
  writeBin(as.raw(1:16), file.path(store, "x", "c", "0"))
  writeLines(
    '{"zarr_format":3,"node_type":"group","attributes":{}}',
    file.path(store, "zarr.json")
  )
  ctx <- CerebroNexus:::.zarrContext(store)
  expect_error(CerebroNexus:::.zarrRead(ctx, "x"), "Blosc format version 1")
})

## ---------------------------------------------------------------------------
## SpatialData
## ---------------------------------------------------------------------------

test_that("a SpatialData table with obsm['spatial'] keeps its coordinates and sections", {
  sce <- quiet(readSpatialDataAsSCE(fixture("small_sd_visium.zarr")))
  expect_identical(dim(sce), c(30L, 54L))
  expect_identical(
    unname(SummarizedExperiment::colData(sce)$library_id),
    c(rep("sec1", 30), rep("sec2", 24))
  )
  coordinates <- SingleCellExperiment::reducedDim(sce, "spatial")
  expect_identical(dim(coordinates), c(54L, 2L))
  expect_true(all(coordinates > 0 & coordinates < 400))
})

test_that("pictures are placed in the shape coordinates of their section", {
  skip_if_not_installed("png")
  sce <- quiet(readSpatialDataAsSCE(fixture("small_sd_visium.zarr")))
  images <- sce@metadata$spatial_images
  expect_identical(names(images), c("sec1", "sec2"))
  expect_identical(names(images$sec1), "hires")
  ## the picture is 36 px wide and 40 px high; the shapes are scaled 0.1 into the
  ## coordinate system the picture sits in, so it spans 360 x 400 shape units
  expect_equal(
    images$sec1$hires$histology_image_bounds,
    c(xmin = 0, xmax = 360, ymin = 0, ymax = 400)
  )
  expect_equal(
    images$sec2$hires$histology_image_bounds,
    c(xmin = 0, xmax = 320, ymin = 0, ymax = 360)
  )
  array <- png::readPNG(base64enc::base64decode(sub(
    "^data:image/png;base64,",
    "",
    images$sec1$hires$histology_image
  )))
  expect_identical(dim(array)[1:2], c(40L, 36L))
  expect_null(
    quiet(readSpatialDataAsSCE(
      fixture("small_sd_visium.zarr"),
      spatial_images = FALSE
    ))@metadata$spatial_images
  )
})

test_that("cell coordinates come from the shapes the table annotates, matched by instance id", {
  skip_if_not_installed("arrow")
  sce <- quiet(readSpatialDataAsSCE(fixture("small_sd_cells.zarr")))
  coordinates <- SingleCellExperiment::reducedDim(sce, "spatial")
  ids <- SummarizedExperiment::colData(sce)$spot_id
  ## instance ids run backwards through the table on purpose
  expect_equal(as.integer(ids[1:3]), c(49L, 48L, 47L))
  expect_equal(unname(coordinates[, 1]), 10 + as.numeric(ids))
  expect_equal(unname(coordinates[, 2]), 200 - 2 * as.numeric(ids))
  expect_null(sce@metadata$spatial_images)
  expect_false("library_id" %in% colnames(SummarizedExperiment::colData(sce)))
})

test_that("a SpatialData store converts to a .crb with one section per region and its picture", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("png")
  path <- quiet(convertSpatialDataToCerebro(
    fixture("small_sd_visium.zarr"),
    withr::local_tempdir(),
    groups = c("cell_type", "library_id"),
    viewer_binary = "never",
    verbose = FALSE
  ))
  crb <- readCerebro(path)
  expect_identical(names(crb$spatial), c("spatial [sec1]", "spatial [sec2]"))
  expect_equal(
    nrow(crb$getSpatialData("spatial [sec1]", FALSE)$coordinates),
    30
  )
  expect_identical(
    names(crb$getSpatialData("spatial [sec2]", FALSE)$histology_images),
    "hires"
  )

  cells <- readCerebro(quiet(convertSpatialDataToCerebro(
    fixture("small_sd_cells.zarr"),
    withr::local_tempdir(),
    groups = "cell_type",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(names(cells$spatial), "spatial")
  expect_equal(nrow(cells$getSpatialData("spatial", FALSE)$coordinates), 50)
})

test_that("bad SpatialData input is refused with a clear message", {
  expect_error(
    readSpatialDataAsSCE(fixture("small_csr.h5ad")),
    "SpatialData .zarr directory"
  )
  expect_error(readSpatialDataAsSCE(fixture("small_v3.zarr")), "no tables/")
  expect_error(
    readSpatialDataAsSCE(fixture("small_sd_cells.zarr"), table = "nope"),
    "not found"
  )
})

test_that("WKB geometries give the centre of points and the centroid of polygons", {
  point <- c(
    as.raw(1L),
    writeBin(1L, raw(), size = 4L),
    writeBin(c(3, 4), raw(), size = 8L)
  )
  expect_equal(CerebroNexus:::.sdWkbCentre(point), c(x = 3, y = 4))
  ## a 2 x 2 square with its corner at the origin, as a polygon
  ring <- matrix(c(0, 0, 2, 0, 2, 2, 0, 2, 0, 0), ncol = 2, byrow = TRUE)
  polygon <- c(
    as.raw(1L),
    writeBin(3L, raw(), size = 4L),
    writeBin(1L, raw(), size = 4L),
    writeBin(5L, raw(), size = 4L),
    writeBin(as.vector(t(ring)), raw(), size = 8L)
  )
  expect_equal(CerebroNexus:::.sdWkbCentre(polygon), c(x = 1, y = 1))
})

test_that("NGFF transformations compose in order", {
  axes <- c("x", "y")
  scale <- list(list(type = "scale", scale = c(2, 3)))
  expect_equal(CerebroNexus:::.sdAffine(scale, axes)[1:2, 1:2], diag(c(2, 3)))
  both <- list(
    list(type = "scale", scale = c(2, 2)),
    list(type = "translation", translation = c(10, 20))
  )
  ## scale first, then translate: (1, 1) -> (2, 2) -> (12, 22)
  expect_equal(
    drop(CerebroNexus:::.sdAffine(both, axes) %*% c(1, 1, 1))[1:2],
    c(12, 22)
  )
  expect_null(CerebroNexus:::.sdAffine(list(list(type = "bijection")), axes))
})
