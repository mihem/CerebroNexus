## Optional demo preparation: tiny vendor-format inputs, no network, Java or
## real microscopy. The Bio-Formats read itself is checked by hand against the
## real image; its pure steps (level, contrast window, compositing) are here.

xenium_prime_env <- function() {
  scripts <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    c("download_helpers.R", "prepare_viewer_xenium_prime_data.R")
  )
  skip_if_not(all(file.exists(scripts)), "repository-only demo preparation")
  env <- new.env(parent = globalenv())
  for (script in scripts) {
    sys.source(script, env)
  }
  env
}

xenium_prime_analysis <- function(root, cells = c("c2", "c1")) {
  paths <- c(
    "analysis/clustering/gene_expression_graphclust/clusters.csv",
    "analysis/umap/gene_expression_2_components/projection.csv"
  )
  for (path in paths) {
    dir.create(file.path(root, dirname(path)), recursive = TRUE)
  }
  utils::write.csv(
    data.frame(Barcode = cells, Cluster = seq_along(cells)),
    file.path(root, paths[[1L]]),
    row.names = FALSE
  )
  utils::write.csv(
    data.frame(Barcode = cells, UMAP_1 = c(2, 1), UMAP_2 = c(20, 10)),
    file.path(root, paths[[2L]]),
    row.names = FALSE
  )
  archive <- file.path(root, "analysis.tar.gz")
  withr::with_dir(
    root,
    utils::tar(archive, files = paths, compression = "gzip", tar = "internal")
  )
  archive
}

test_that("Prime 5K reads published analysis by cell name, retaining unassigned cells", {
  env <- xenium_prime_env()
  root <- withr::local_tempdir()
  archive <- xenium_prime_analysis(root)
  analysis <- env$.viewerXeniumPrimeAnalysis(archive, root, c("c1", "c2", "c3"))
  expect_identical(
    as.character(analysis$cluster),
    c("Cluster 2", "Cluster 1", "Unassigned")
  )
  expect_identical(rownames(analysis$umap), c("c1", "c2", "c3"))
  expect_equal(analysis$umap$Dim_1, c(1, 2, NA))
  expect_equal(analysis$umap$Dim_2, c(10, 20, NA))
  expect_error(
    env$.viewerXeniumPrimeAnalysis(archive, root, "c1"),
    "analysis and matrix cells disagree"
  )
})

test_that("Prime 5K reads gene counts and physical coordinates in matrix order", {
  skip_if_not_installed("rhdf5")
  env <- xenium_prime_env()
  root <- withr::local_tempdir()
  h5 <- file.path(root, "counts.h5")
  rhdf5::h5createFile(h5)
  rhdf5::h5createGroup(h5, "matrix")
  rhdf5::h5createGroup(h5, "matrix/features")
  counts <- Matrix::sparseMatrix(
    i = c(1, 3, 2, 3),
    j = c(1, 1, 2, 2),
    x = c(2, 9, 3, 8),
    dims = c(3L, 3L),
    dimnames = list(c("Gene", "Gene", "Negative"), c("c1", "c2", "c3"))
  )
  write <- function(value, key) {
    rhdf5::h5write(value, h5, paste0("matrix/", key))
  }
  write(dim(counts), "shape")
  write(counts@i, "indices")
  write(counts@p, "indptr")
  write(counts@x, "data")
  write(colnames(counts), "barcodes")
  write(rownames(counts), "features/name")
  write(
    c("Gene Expression", "Gene Expression", "Negative Control Probe"),
    "features/feature_type"
  )
  csv <- file.path(root, "cells.csv.gz")
  connection <- gzfile(csv, "wt")
  utils::write.csv(
    data.frame(
      cell_id = c("c3", "c2", "c1"),
      x_centroid = c(3, 2, 1),
      y_centroid = c(30, 20, 10),
      cell_area = 100,
      nucleus_area = 50
    ),
    connection,
    row.names = FALSE
  )
  close(connection)
  files <- c(matrix = h5, cells = csv, analysis = xenium_prime_analysis(root))
  result <- env$.viewerXeniumPrimeRead(files, root)
  expect_identical(dim(result$counts), c(2L, 3L))
  expect_identical(rownames(result$counts), c("Gene", "Gene.1"))
  expect_identical(colnames(result$counts), c("c1", "c2", "c3"))
  expect_identical(result$metadata$cell_barcode, c("c1", "c2", "c3"))
  expect_equal(result$metadata$nUMI, c(2, 3, 0))
  expect_equal(result$metadata$nGene, c(1, 1, 0))
  expect_equal(result$coordinates$x, 1:3)
  expect_equal(result$coordinates$y, c(-10, -20, -30))
  expect_identical(
    as.character(result$metadata$cluster),
    c("Cluster 2", "Cluster 1", "Unassigned")
  )
})

test_that("the Prime 5K pyramid level is the smallest one that is large enough", {
  env <- xenium_prime_env()
  sizes <- cbind(
    x = c(34154, 17077, 8538, 4269, 2134, 1067),
    y = c(23912, 11956, 5978, 2989, 1494, 747)
  )
  expect_identical(env$.viewerXeniumPrimeLevel(sizes, 4096L), 4L)
  expect_identical(env$.viewerXeniumPrimeLevel(sizes, 3000L), 4L)
  expect_identical(env$.viewerXeniumPrimeLevel(sizes, 4270L), 3L)
  ## Nothing is large enough: take the finest level there is.
  expect_identical(env$.viewerXeniumPrimeLevel(sizes, 100000L), 1L)
})

test_that("a Prime 5K stain is stretched over its positive signal only", {
  env <- xenium_prime_env()
  expect_equal(env$.viewerXeniumPrimeWindow(c(0, 0, 0)), c(0, 1))
  expect_equal(env$.viewerXeniumPrimeWindow(c(0, 7, 7)), c(0, 7))
  expect_equal(env$.viewerXeniumPrimeWindow(c(NA, 0, 5)), c(0, 5))
  signal <- c(rep(0, 1000), seq(1, 2000, length.out = 5000))
  window <- env$.viewerXeniumPrimeWindow(signal)
  ## Zeros are background, so they do not drag the window down.
  expect_gt(window[[1L]], 1)
  expect_lt(window[[2L]], 2000)
  expect_gt(window[[2L]], window[[1L]])
})

test_that("the four Prime 5K stains are drawn in their own colours and added", {
  env <- xenium_prime_env()
  stains <- env$.viewerXeniumPrimeStains()
  expect_identical(nrow(stains), 4L)
  rgb_of <- function(colour) unname(grDevices::col2rgb(colour)[, 1L]) / 255
  ## One landmark per stain (columns 1-4), and one pixel with all four.
  planes <- array(0, dim = c(2L, 5L, 4L))
  for (stain in 1:4) {
    planes[1L, stain, stain] <- 1000
  }
  planes[1L, 5L, ] <- 1000
  composite <- env$.viewerXeniumPrimeCompose(planes, stains)
  expect_identical(dim(composite$rgb), c(2L, 5L, 3L))
  for (stain in 1:4) {
    expect_equal(
      composite$rgb[1L, stain, ],
      rgb_of(stains$colour[[stain]]),
      info = stains$name[[stain]]
    )
  }
  expect_equal(composite$rgb[2L, 1L, ], c(0, 0, 0))
  expect_equal(
    composite$rgb[1L, 5L, ],
    pmin(rowSums(vapply(stains$colour, rgb_of, numeric(3L))), 1)
  )
  expect_true(all(composite$rgb >= 0 & composite$rgb <= 1))
  expect_error(
    env$.viewerXeniumPrimeCompose(planes[,, 1:3], stains),
    "one plane per stain"
  )
})

test_that("Prime 5K preparation publishes atomically, caches all assets, and stamps the recipe", {
  env <- xenium_prime_env()
  root <- withr::local_tempdir()
  paths <- env$.viewerXeniumPrimePaths(root)
  dir.create(dirname(paths$crb), recursive = TRUE)
  writeLines("previous", paths$crb)
  env$.demoRequire <- function(...) NULL
  env$.demoDownload <- function(...) NULL
  env$.viewerXeniumPrimeBuildObject <- function(paths, ...) {
    writeLines("partial", paths$crb)
    stop("simulated failure")
  }
  prepare <- function() env$prepareViewerXeniumPrimeDemoData(root)
  expect_error(prepare(), "simulated failure")
  expect_identical(readLines(paths$crb), "previous")
  expect_length(list.files(paths$root, pattern = "stage-"), 0L)
  calls <- 0L
  env$.viewerXeniumPrimeBuildObject <- function(paths, work, recipe) {
    calls <<- calls + 1L
    writeLines("new", paths$crb)
    file.create(paths$sidecar, paths$image, paths$image_info)
    env$.demoStampWrite(paths$stamp, recipe)
  }
  expect_identical(prepare(), paths$crb)
  expect_identical(prepare(), paths$crb)
  expect_identical(calls, 1L)
  unlink(paths$image)
  expect_identical(prepare(), paths$crb)
  expect_identical(calls, 2L)
  ## The recipe is a hash of the script's functions, so editing the compositor
  ## (or anything else in it) prepares the data set again.
  env$.viewerXeniumPrimeRecipe <- "an edited script"
  expect_identical(prepare(), paths$crb)
  expect_identical(calls, 3L)
})

test_that("Prime 5K asks for its packages before the 13 GB download", {
  env <- xenium_prime_env()
  root <- withr::local_tempdir()
  downloaded <- FALSE
  env$.demoDownload <- function(...) downloaded <<- TRUE
  env$.demoRequire <- function(packages, demo) {
    expect_true("RBioFormats" %in% packages)
    stop("missing packages", call. = FALSE)
  }
  expect_error(env$prepareViewerXeniumPrimeDemoData(root), "missing packages")
  expect_false(downloaded)
})

test_that("a Prime 5K build opens its H5 expression and keeps missing UMAP cells spatially", {
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("png")
  env <- xenium_prime_env()
  root <- withr::local_tempdir()
  paths <- env$.viewerXeniumPrimePaths(root)
  dir.create(dirname(paths$crb), recursive = TRUE)
  cells <- c("c1", "c2", "c3")
  counts <- Matrix::sparseMatrix(
    i = c(1, 2),
    j = c(1, 2),
    x = c(2, 3),
    dims = c(2L, 3L),
    dimnames = list(c("A", "B"), cells)
  )
  section <- list(
    counts = counts,
    metadata = data.frame(
      cell_barcode = cells,
      sample = factor("coronal"),
      cluster = factor(c("Cluster 1", "Cluster 2", "Unassigned")),
      nUMI = c(2, 3, 0),
      nGene = c(1, 1, 0),
      row.names = cells
    ),
    coordinates = data.frame(x = 1:3, y = -(1:3), row.names = cells),
    umap = data.frame(
      Dim_1 = c(1, 2, NA),
      Dim_2 = c(2, 1, NA),
      row.names = cells
    )
  )
  env$.viewerXeniumPrimeExtract <- function(...) c(channel0 = "unused.ome.tif")
  env$.viewerXeniumPrimeRead <- function(...) section
  env$.viewerXeniumPrimeMorphology <- function(ome, png_file, info_file, ...) {
    expect_identical(ome, "unused.ome.tif")
    png::writePNG(array(0, c(4, 4, 3)), png_file)
    jsonlite::write_json(
      list(bounds = list(xmin = 0, xmax = 4, ymin = -4, ymax = 0)),
      info_file,
      auto_unbox = TRUE
    )
  }
  env$.viewerXeniumPrimeBuildObject(paths, root, "test-recipe")
  object <- readCerebro(paths$crb)
  expect_identical(object$getExpressionBackend()$type, "h5")
  expect_identical(object$getMetaData()$cell_barcode, cells)
  expect_equal(
    as.matrix(object$getExpressionMatrix()),
    as.matrix(env$.demoLogNormalize(counts)),
    ignore_attr = TRUE
  )
  expect_identical(rownames(object$getProjection("umap")), cells)
  expect_true(all(is.na(object$getProjection("umap")["c3", ])))
  spatial <- object$getSpatialData("coronal")
  expect_identical(rownames(spatial$coordinates), cells)
  expect_equal(spatial$coordinates$y, -(1:3))
  image <- spatial$histology_images[["Four-stain morphology"]]
  expect_match(image$histology_image, "^data:image/png;base64,")
  expect_equal(
    unname(image$histology_image_bounds[c("xmin", "xmax", "ymin", "ymax")]),
    c(0, 4, -4, 0)
  )
  ## The Viewer builds its Spatial background list from exactly this payload.
  embedded <- embedded_spatial_images(spatial)
  expect_named(embedded, "Four-stain morphology")
  pack <- buildViewerPack(paths$crb, viewer_binary = "always")
  expect_true(validateViewerPack(paths$crb, full = TRUE)$valid)
  expect_true(dir.exists(pack))
})

test_that("the optional Prime 5K menu entry adds the dataset with readable cell dots", {
  env <- new.env(parent = baseenv())
  sys.source(viewer_test_path("million_cell_demo.R"), env)
  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = 5,
    point_opacity = 1,
    percentage_cells_to_show = 100,
    spatial_images = list(Small = list(fov = c(HE = "existing.png")))
  )
  expect_identical(env$viewerAddXeniumPrimeDemo(original, ""), original)
  root <- withr::local_tempdir()
  crb <- file.path(root, "prime.crb")
  file.create(crb, file.path(root, "prime.h5"))
  configured <- env$viewerAddXeniumPrimeDemo(original, crb)
  label <- "Mouse brain (Xenium Prime 5K)"
  expect_identical(names(configured$crb_file_to_load), c("Small", label))
  ## The microscopy is embedded in the CRB: nothing outside the app's own
  ## folders is configured, since the Viewer refuses to serve such files.
  expect_identical(configured$spatial_images, original$spatial_images)
  expect_null(configured$spatial_image_settings)
  appearance <- configured$viewer_content[[
    label
  ]]$spatial_point_appearance$coronal
  expect_identical(appearance, list(point_size = 2, point_opacity = 0.9))
  ## 63,000 cells: dots that are neither the million-cell defaults nor opaque
  ## blobs, so the clusters read clearly and the tissue shows between them.
  expect_equal(unname(configured$point_size[label]), 2)
  expect_equal(unname(configured$point_opacity[label]), 0.8)
  unlink(file.path(root, "prime.h5"))
  expect_error(
    env$viewerAddXeniumPrimeDemo(original, crb),
    "CEREBRO_XENIUM_PRIME_DEMO_CRB must name a .crb with an adjacent .h5 file."
  )
})
