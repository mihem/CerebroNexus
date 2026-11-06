test_that("the Trekker Builder fixture rebuilds offline with stable content and images", {
  script <- test_path("../../data-raw/build_builder_spatial_fixtures.R")
  skip_if_not(file.exists(script), "Regeneration scripts are repository-only")
  for (package in c(
    "Seurat",
    "SeuratObject",
    "qs2",
    "jpeg",
    "future",
    "callr"
  )) {
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
    "trekker_spatial/trekker_4_tissues.qs2",
    sprintf("trekker_spatial/tissue_%02d_he.jpg", 1:4)
  )
  expect_setequal(list.files(first, recursive = TRUE), files)
  # Compare bytes within one toolchain; cross-version numerical/codec equality is not promised.
  expect_identical(
    unname(tools::md5sum(file.path(first, files))),
    unname(tools::md5sum(file.path(second, files)))
  )
  trekker <- qs2::qs_read(file.path(first, files[[1]]))
  expect_equal(dim(trekker), c(200L, 5020L))
  expect_identical(colnames(trekker), sprintf("SYNTH-%05d", 1:5020))
  expect_identical(SeuratObject::Images(trekker), "slice1")
  expect_s4_class(trekker[["slice1"]], "SlideSeq")
  expect_setequal(
    SeuratObject::Reductions(trekker),
    c("pca", "umap", "SPATIAL")
  )
  expect_identical(as.integer(table(trekker$tissue_id)), c(rep(1250L, 4), 20L))
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
    dim(jpeg::readJPEG(file.path(first, files[[2]])))[1:2],
    c(728L, 1064L)
  )
  published_root <- test_path("../../inst/builder/fixtures")
  published_trekker <- qs2::qs_read(file.path(published_root, files[[1]]))
  expect_equal(
    SeuratObject::LayerData(trekker, layer = "counts"),
    SeuratObject::LayerData(published_trekker, layer = "counts"),
    tolerance = 0
  )
  expect_identical(colnames(trekker), colnames(published_trekker))
  expect_identical(trekker$cell_type, published_trekker$cell_type)
  expect_identical(trekker$tissue_id, published_trekker$tissue_id)
  expect_equal(
    SeuratObject::Embeddings(trekker, "SPATIAL"),
    SeuratObject::Embeddings(published_trekker, "SPATIAL"),
    tolerance = 0
  )
})

## The Spatial alignment example is a crop of a real 10x Visium section. Its
## inputs (a 28 MB matrix and a 10 MB image archive, 10x Genomics, CC BY 4.0)
## are not committed, so this rebuild runs only when
## CEREBRO_VISIUM_BREAST_DIR names a directory holding the two published
## files (see data-raw/builder_fixtures.md).
test_that("the Visium Builder fixture rebuilds from the 10x files", {
  script <- test_path("../../data-raw/build_builder_visium_fixture.R")
  skip_if_not(file.exists(script), "Regeneration scripts are repository-only")
  source_dir <- Sys.getenv("CEREBRO_VISIUM_BREAST_DIR")
  skip_if_not(nzchar(source_dir), "CEREBRO_VISIUM_BREAST_DIR is not set")
  find <- function(pattern) {
    found <- list.files(
      source_dir,
      pattern = pattern,
      recursive = TRUE,
      full.names = TRUE
    )
    skip_if_not(length(found) == 1L, paste("No single", pattern))
    found
  }
  matrix <- find("_filtered_feature_bc_matrix[.]h5$")
  spatial <- find("_spatial[.]tar[.]gz$")
  for (package in c(
    "Seurat",
    "SeuratObject",
    "rhdf5",
    "jpeg",
    "png",
    "callr"
  )) {
    skip_if_not_installed(package)
  }
  first <- withr::local_tempdir()
  second <- withr::local_tempdir()
  for (output in c(first, second)) {
    callr::rscript(
      normalizePath(script),
      cmdargs = c("--matrix", matrix, "--spatial", spatial, "--output", output),
      libpath = .libPaths()
    )
  }
  files <- c(
    "spatial_alignment/visium_breast_cancer.rds",
    "spatial_alignment/visium_breast_cancer_he.jpg"
  )
  expect_setequal(list.files(first, recursive = TRUE), files)
  expect_identical(
    unname(tools::md5sum(file.path(first, files))),
    unname(tools::md5sum(file.path(second, files)))
  )
  rebuilt <- readRDS(file.path(first, files[[1]]))
  published_root <- test_path("../../inst/builder/fixtures")
  published <- readRDS(file.path(published_root, files[[1]]))
  expect_equal(dim(rebuilt), c(300L, 405L))
  expect_identical(colnames(rebuilt), colnames(published))
  expect_identical(rownames(rebuilt), rownames(published))
  expect_equal(
    SeuratObject::LayerData(rebuilt, layer = "counts"),
    SeuratObject::LayerData(published, layer = "counts"),
    tolerance = 0
  )
  expect_equal(
    SeuratObject::GetTissueCoordinates(rebuilt[["visium_breast"]]),
    SeuratObject::GetTissueCoordinates(published[["visium_breast"]]),
    tolerance = 0
  )
  expect_identical(
    dim(jpeg::readJPEG(file.path(first, files[[2]]))),
    dim(jpeg::readJPEG(file.path(published_root, files[[2]])))
  )
})
