## What an AnnData file carries for the eye: scanpy's category colours
## (uns[<column>_colors]) and Visium tissue images (uns["spatial"]).
##
## small_visium.h5ad was written by the real anndata from the tiny 10x example in
## R's SpatialExperiment (see fixtures/anndata/make_fixtures.py). Its images are
## shrunk 4x with the scale factor shrunk the same, so an image still spans
## 11287 px by 11757 px in full-resolution spot coordinates.

skip_if_not_installed("rhdf5")
skip_if_not_installed("SingleCellExperiment")

fixture <- anndata_test_fixture
quiet <- function(x) suppressWarnings(suppressMessages(x))
read_h5ad <- function(path, ...) quiet(readH5adAsSCE(path, ...))

## ---------------------------------------------------------------------------
## category colours
## ---------------------------------------------------------------------------

test_that("uns colours become a level -> colour vector per categorical column", {
  ## the fixture's own uns has rank_genes_groups; add the colours next to it
  path <- file.path(withr::local_tempdir(), "colors.h5ad")
  file.copy(fixture("small_csr.h5ad"), path)
  levels <- levels(SummarizedExperiment::colData(read_h5ad(path))$cell_type)
  rhdf5::h5write(
    c("#1F77B4", "#FF7F0E", "ForestGreen"),
    path,
    "uns/cell_type_colors",
    variableLengthString = TRUE
  )
  colors <- read_h5ad(path)@metadata$group_colors
  expect_identical(names(colors), "cell_type")
  expect_identical(names(colors$cell_type), levels)
  ## named colours are normalised to #RRGGBB
  expect_identical(
    unname(colors$cell_type),
    c("#1F77B4", "#FF7F0E", "#228B22")
  )
  ## a file with no colours has no payload
  expect_null(read_h5ad(fixture("small_csr.h5ad"))@metadata$group_colors)
})

test_that("colours with the wrong length or invalid values are ignored", {
  path <- file.path(withr::local_tempdir(), "bad_colors.h5ad")
  file.copy(fixture("small_csr.h5ad"), path)
  rhdf5::h5write(
    c("#111111", "#222222"),
    path,
    "uns/cell_type_colors",
    variableLengthString = TRUE
  )
  rhdf5::h5write(
    c("#111111", "not-a-colour", "#333333"),
    path,
    "uns/donor_colors",
    variableLengthString = TRUE
  )
  expect_null(read_h5ad(path)@metadata$group_colors)
})

test_that("colours reach the .crb for exported groups, and follow a rename", {
  path <- file.path(withr::local_tempdir(), "colors_convert.h5ad")
  file.copy(fixture("small_csr.h5ad"), path)
  rhdf5::h5write(
    c("#AA0000", "#00AA00", "#0000AA"),
    path,
    "uns/cell_type_colors",
    variableLengthString = TRUE
  )
  expected <- stats::setNames(
    c("#AA0000", "#00AA00", "#0000AA"),
    levels(SummarizedExperiment::colData(read_h5ad(path))$cell_type)
  )

  out <- withr::local_tempdir()
  crb <- readCerebro(quiet(convertH5adToCerebro(
    path,
    file.path(out, "a"),
    groups = c("cell_type", "donor"),
    viewer_binary = "never",
    verbose = FALSE
  )))
  stored <- crb$getTechnicalInfo()$group_colors
  expect_identical(names(stored), "cell_type")
  expect_identical(stored$cell_type[names(expected)], expected)

  renamed <- readCerebro(quiet(convertH5adToCerebro(
    path,
    file.path(out, "b"),
    groups = c("cell_type", "donor"),
    groups_naming = list(cell_type = "Cell type"),
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(names(renamed$getTechnicalInfo()$group_colors), "Cell type")

  ## only groups that are exported keep their colours
  only_donor <- readCerebro(quiet(convertH5adToCerebro(
    path,
    file.path(out, "c"),
    groups = "donor",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_null(only_donor$getTechnicalInfo()$group_colors)
})

test_that("exportToH5ad writes the colours back as uns[<column>_colors]", {
  path <- file.path(withr::local_tempdir(), "colors_roundtrip.h5ad")
  file.copy(fixture("small_csr.h5ad"), path)
  rhdf5::h5write(
    c("#AA0000", "#00AA00", "#0000AA"),
    path,
    "uns/cell_type_colors",
    variableLengthString = TRUE
  )
  crb <- quiet(convertH5adToCerebro(
    path,
    withr::local_tempdir(),
    groups = c("cell_type", "donor"),
    viewer_binary = "never",
    verbose = FALSE
  ))
  out <- file.path(withr::local_tempdir(), "back.h5ad")
  quiet(exportToH5ad(crb, out))
  written <- as.vector(rhdf5::h5read(out, "uns/cell_type_colors"))
  categories <- as.vector(rhdf5::h5read(out, "obs/cell_type/categories"))
  ## one colour per category, in the order of the categories
  expect_length(written, length(categories))
  expect_identical(
    stats::setNames(written, categories)[c("B cell", "Monocyte", "T cell")],
    quiet(readCerebro(crb))$getTechnicalInfo()$group_colors$cell_type[
      c("B cell", "Monocyte", "T cell")
    ]
  )
  ## and reading that file gives the colours again
  expect_identical(
    unname(read_h5ad(out)@metadata$group_colors$cell_type),
    written
  )
})

test_that("the Viewer takes a data set's colours as defaults", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("color_config.R"), envir = env)
  info <- list(
    group_colors = list(
      cluster = c(a = "#112233", b = "#445566", z = "#778899", bad = "red")
    )
  )
  ## only levels the group has, and only valid #RRGGBB
  expect_identical(
    env$stored_group_colors(info, "cluster", c("a", "b", "c", "bad")),
    c(a = "#112233", b = "#445566")
  )
  expect_null(env$stored_group_colors(info, "other", c("a", "b")))
  expect_null(env$stored_group_colors(NULL, "cluster", c("a", "b")))
  expect_null(env$stored_group_colors(list(), "cluster", "a"))
  expect_null(env$stored_group_colors(info, "cluster", "none"))
})

## ---------------------------------------------------------------------------
## Visium tissue images
## ---------------------------------------------------------------------------

decode_png <- function(uri) {
  skip_if_not_installed("png")
  png::readPNG(base64enc::base64decode(sub("^data:image/png;base64,", "", uri)))
}

test_that("Visium images are embedded per library with their extent in spot space", {
  skip_if_not_installed("png")
  images <- read_h5ad(fixture("small_visium.h5ad"))@metadata$spatial_images
  expect_identical(names(images), c("section1", "section2"))
  ## auto: hires where there is one, otherwise lowres
  expect_identical(names(images$section1), "hires")
  expect_identical(names(images$section2), "lowres")

  hires <- images$section1$hires
  scale <- 0.051033426 / 4 * 2
  expect_equal(
    hires$histology_image_bounds,
    c(xmin = 0, xmax = 288 / scale, ymin = 0, ymax = 300 / scale),
    tolerance = 1e-6
  )
  ## the extent is that of the full-resolution picture: 11287 x 11757 px
  expect_equal(hires$histology_image_bounds[["xmax"]], 11287, tolerance = 1e-3)
  expect_equal(
    images$section2$lowres$histology_image_bounds[["ymax"]],
    11757,
    tolerance = 1e-3
  )
  expect_identical(hires$image_label, "hires")

  ## a real PNG of the stored size, in its own orientation
  array <- decode_png(hires$histology_image)
  expect_identical(dim(array)[1:2], c(300L, 288L))
  expect_identical(
    dim(decode_png(images$section2$lowres$histology_image))[1:2],
    c(150L, 144L)
  )
})

test_that("uint8 and float images give the same picture", {
  skip_if_not_installed("png")
  images <- read_h5ad(
    fixture("small_visium.h5ad"),
    spatial_images = c("hires", "lowres")
  )@metadata$spatial_images
  expect_setequal(names(images$section1), c("hires", "lowres"))
  hires <- decode_png(images$section1$hires$histology_image)
  lowres <- decode_png(images$section1$lowres$histology_image)
  ## hires is the 2x pixel-doubled low-res, stored as uint8: values in [0, 1]
  expect_true(all(hires >= 0 & hires <= 1))
  expect_equal(
    hires[seq(1, 300, 2), seq(1, 288, 2), 1],
    lowres[,, 1],
    tolerance = 1 / 255 + 1e-6
  )
  ## section 2 has no hires: asking for it gives nothing for that library
  expect_null(images$section2$hires)
})

test_that("spatial_images can switch the images off or pick a label", {
  expect_null(
    read_h5ad(
      fixture("small_visium.h5ad"),
      spatial_images = FALSE
    )@metadata$spatial_images
  )
  low <- read_h5ad(
    fixture("small_visium.h5ad"),
    spatial_images = "lowres"
  )@metadata$spatial_images
  expect_identical(names(low$section1), "lowres")
  expect_identical(names(low$section2), "lowres")
  ## a file without tissue images has none
  expect_null(read_h5ad(fixture("small_spatial.h5ad"))@metadata$spatial_images)
})

test_that("several libraries convert to one section each, with their own image", {
  out <- withr::local_tempdir()
  msgs <- character()
  path <- withCallingHandlers(
    suppressWarnings(convertH5adToCerebro(
      fixture("small_visium.h5ad"),
      out,
      groups = c("library_id", "layer"),
      viewer_binary = "never",
      verbose = FALSE
    )),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl('spatial_split_by = "library_id"', msgs, fixed = TRUE)))
  obj <- readCerebro(path)
  expect_identical(
    names(obj$spatial),
    c("spatial [section1]", "spatial [section2]")
  )
  s1 <- obj$getSpatialData("spatial [section1]", FALSE)
  s2 <- obj$getSpatialData("spatial [section2]", FALSE)
  expect_identical(nrow(s1$coordinates), 50L)
  expect_identical(nrow(s2$coordinates), 49L)
  expect_identical(names(s1$histology_images), "hires")
  expect_identical(names(s2$histology_images), "lowres")
  ## the spots lie inside the image they are drawn on
  b <- s1$histology_images$hires$histology_image_bounds
  expect_true(all(
    s1$coordinates$x >= b[["xmin"]] & s1$coordinates$x <= b[["xmax"]]
  ))
  expect_true(all(
    s1$coordinates$y >= b[["ymin"]] & s1$coordinates$y <= b[["ymax"]]
  ))
  ## coordinates are the file's (column, row) in full-resolution pixels
  source <- read_h5ad(fixture("small_visium.h5ad"))
  spatial <- SingleCellExperiment::reducedDim(source, "spatial")
  expect_equal(
    sort(s1$coordinates$x),
    sort(unname(spatial[source$library_id == "section1", 1]))
  )
})

test_that("a single library's image is attached to the single spatial entry", {
  source <- read_h5ad(fixture("small_visium.h5ad"))
  one <- source[, source$library_id == "section1"]
  one@metadata$spatial_images <- one@metadata$spatial_images["section1"]
  out <- withr::local_tempdir()
  path <- quiet(convertSCEToCerebro(
    one,
    out,
    assay = "X",
    groups = "layer",
    nUMI = "nUMI",
    nGene = "nGene",
    viewer_binary = "never",
    verbose = FALSE,
    .source_name = "one.h5ad"
  ))
  obj <- readCerebro(path)
  expect_identical(names(obj$spatial), "spatial")
  expect_identical(
    names(obj$getSpatialData("spatial", FALSE)$histology_images),
    "hires"
  )
})

test_that("images of several libraries are not guessed onto one unsplit entry", {
  source <- read_h5ad(fixture("small_visium.h5ad"))
  export <- Cerebro$new()
  expression <- SummarizedExperiment::assay(source, "X")
  export$setMetaData(data.frame(cell_barcode = colnames(source)))
  expect_message(
    .exportAddSpatialFromEmbeddings(
      export,
      list(spatial = SingleCellExperiment::reducedDim(source, "spatial")),
      expression_data = expression,
      assay = "X",
      layer = "X",
      images = source@metadata$spatial_images
    ),
    "tissue images for 2 libraries"
  )
  expect_length(export$spatial$spatial$histology_images, 0L)
})

## ---------------------------------------------------------------------------
## writing it back
## ---------------------------------------------------------------------------

test_that("exportToH5ad writes spatial coordinates and tissue images back", {
  skip_if_not_installed("png")
  crb <- quiet(convertH5adToCerebro(
    fixture("small_visium.h5ad"),
    withr::local_tempdir(),
    groups = c("library_id", "layer"),
    viewer_binary = "never",
    verbose = FALSE
  ))
  out <- file.path(withr::local_tempdir(), "visium_back.h5ad")
  quiet(exportToH5ad(crb, out))
  back <- read_h5ad(out)
  source <- read_h5ad(fixture("small_visium.h5ad"))

  ## coordinates: every cell, same values, same order as the file it came from
  expect_identical(colnames(back), colnames(source))
  expect_equal(
    unname(SingleCellExperiment::reducedDim(back, "spatial")),
    unname(SingleCellExperiment::reducedDim(source, "spatial"))
  )

  ## images: one per library, same extent in spot space and same pixels
  expect_identical(
    names(back@metadata$spatial_images),
    c("section1", "section2")
  )
  for (library in c("section1", "section2")) {
    a <- back@metadata$spatial_images[[library]]
    b <- source@metadata$spatial_images[[library]]
    expect_identical(names(a), names(b))
    label <- names(b)[[1L]]
    expect_equal(
      a[[label]]$histology_image_bounds,
      b[[label]]$histology_image_bounds,
      tolerance = 1e-6
    )
    expect_equal(
      decode_png(a[[label]]$histology_image),
      decode_png(b[[label]]$histology_image),
      tolerance = 1 / 255 + 1e-6
    )
  }
  ## scanpy needs the spot diameter to draw a spatial plot
  expect_true(
    rhdf5::h5read(
      out,
      "uns/spatial/section1/scalefactors/spot_diameter_fullres"
    ) >
      0
  )
})
