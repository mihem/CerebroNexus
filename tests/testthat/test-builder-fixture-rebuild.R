test_that("spatial Builder fixtures rebuild offline with stable content and images", {
  script <- test_path("../../data-raw/build_builder_spatial_fixtures.R")
  skip_if_not(file.exists(script), "Regeneration scripts are repository-only")
  for (package in c("Seurat", "SeuratObject", "qs2", "jpeg", "png", "future")) {
    skip_if_not_installed(package)
  }
  first <- withr::local_tempdir()
  second <- withr::local_tempdir()
  for (output in c(first, second)) {
    callr::rscript(
      normalizePath(script),
      cmdargs = c("--output", output),
      libpath = .libPaths()
    )
  }
  files <- c(
    "spatial_alignment/arrow_spatial.rds",
    "spatial_alignment/arrow_background.png",
    "trekker_spatial/trekker_4_tissues.qs2",
    sprintf("trekker_spatial/tissue_%02d_he.jpg", 1:4)
  )
  expect_setequal(list.files(first, recursive = TRUE), files)
  # Compare bytes within one toolchain; cross-version numerical/codec equality is not promised.
  expect_identical(
    unname(tools::md5sum(file.path(first, files))),
    unname(tools::md5sum(file.path(second, files)))
  )
  arrow <- readRDS(file.path(first, files[[1]]))
  trekker <- qs2::qs_read(file.path(first, files[[3]]))
  expect_equal(dim(arrow), c(30L, 159L))
  expect_equal(dim(trekker), c(200L, 5020L))
  expect_identical(colnames(arrow), paste0("ArrowCell", 1:159))
  expect_identical(colnames(trekker), sprintf("SYNTH-%05d", 1:5020))
  expect_identical(SeuratObject::Images(arrow), "arrow_fov")
  expect_identical(SeuratObject::Images(trekker), "slice1")
  expect_s4_class(trekker[["slice1"]], "SlideSeq")
  expect_setequal(
    SeuratObject::Reductions(trekker),
    c("pca", "umap", "SPATIAL")
  )
  expect_identical(as.integer(table(trekker$tissue_id)), c(rep(1250L, 4), 20L))
  coords <- SeuratObject::GetTissueCoordinates(arrow[["arrow_fov"]])
  expect_equal(
    unname(as.matrix(coords[, c("x", "y")])),
    unname(SeuratObject::Embeddings(arrow, "umap") * 100)
  )
  expect_equal(range(coords$x), c(0, 800))
  expect_equal(range(coords$y), c(0, 500))
  spatial <- SeuratObject::Embeddings(trekker, "SPATIAL")
  slide <- trekker[["slice1"]]@coordinates
  expect_identical(rownames(slide), colnames(trekker))
  expect_equal(slide$x, unname(-spatial[, 2]))
  expect_equal(slide$y, unname(spatial[, 1]))
  expression <- SeuratObject::LayerData(trekker, layer = "counts")
  expect_s4_class(expression, "dgCMatrix")
  expect_true(all(Matrix::colSums(expression) > 0))
  expect_gt(
    mean(expression["PECAM1", trekker$cell_type == "endothelial"]),
    5 * mean(expression["PECAM1", trekker$cell_type != "endothelial"])
  )
  expect_identical(
    dim(png::readPNG(file.path(first, files[[2]])))[1:2],
    c(500L, 800L)
  )
  expect_identical(
    dim(jpeg::readJPEG(file.path(first, files[[4]])))[1:2],
    c(728L, 1064L)
  )
  published_root <- test_path("../../inst/builder/fixtures")
  published_arrow <- readRDS(file.path(published_root, files[[1]]))
  published_trekker <- qs2::qs_read(file.path(published_root, files[[3]]))
  for (pair in list(
    list(arrow, published_arrow),
    list(trekker, published_trekker)
  )) {
    expect_equal(
      SeuratObject::LayerData(pair[[1]], layer = "counts"),
      SeuratObject::LayerData(pair[[2]], layer = "counts"),
      tolerance = 0
    )
    expect_identical(colnames(pair[[1]]), colnames(pair[[2]]))
  }
  expect_identical(trekker$cell_type, published_trekker$cell_type)
  expect_identical(trekker$tissue_id, published_trekker$tissue_id)
  expect_equal(
    SeuratObject::Embeddings(trekker, "SPATIAL"),
    SeuratObject::Embeddings(published_trekker, "SPATIAL"),
    tolerance = 0
  )
  expect_equal(
    SeuratObject::GetTissueCoordinates(arrow[["arrow_fov"]]),
    SeuratObject::GetTissueCoordinates(published_arrow[["arrow_fov"]]),
    tolerance = 0
  )
})
