## Optional demo preparation: tiny vendor-format inputs, no network or real
## microscopy needed by the R tests. The Python image helper has its own tests.

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

xenium_prime_image_script <- function() {
  testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_xenium_prime_image.py"
  )
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

test_that("Prime 5K preparation publishes atomically, caches all assets, and stamps the image recipe", {
  env <- xenium_prime_env()
  root <- withr::local_tempdir()
  paths <- env$.viewerXeniumPrimePaths(root)
  dir.create(dirname(paths$crb), recursive = TRUE)
  writeLines("previous", paths$crb)
  env$.demoRequire <- function(...) NULL
  env$system2 <- function(...) 0L
  env$.demoDownload <- function(...) NULL
  env$.viewerXeniumPrimeBuildObject <- function(paths, ...) {
    writeLines("partial", paths$crb)
    stop("simulated failure")
  }
  prepare <- function() {
    env$prepareViewerXeniumPrimeDemoData(
      root,
      python = "test-python",
      image_script = xenium_prime_image_script()
    )
  }
  expect_error(prepare(), "simulated failure")
  expect_identical(readLines(paths$crb), "previous")
  expect_length(list.files(paths$root, pattern = "stage-"), 0L)
  calls <- 0L
  env$.viewerXeniumPrimeBuildObject <- function(
    paths,
    work,
    python,
    image_script,
    recipe
  ) {
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
  ## Editing only the compositor must invalidate the prepared cache too.
  changed <- tempfile(fileext = ".py")
  withr::defer(unlink(changed))
  writeLines(c(readLines(xenium_prime_image_script()), "# changed"), changed)
  env$prepareViewerXeniumPrimeDemoData(
    root,
    python = "test-python",
    image_script = changed
  )
  expect_identical(calls, 3L)
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
  env$system2 <- function(...) {
    png::writePNG(array(0, c(4, 4, 3)), paths$image)
    jsonlite::write_json(
      list(bounds = list(xmin = 0, xmax = 4, ymin = -4, ymax = 0)),
      paths$image_info,
      auto_unbox = TRUE
    )
    0L
  }
  env$.viewerXeniumPrimeBuildObject(
    paths,
    root,
    "test-python",
    xenium_prime_image_script(),
    "test-recipe"
  )
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

test_that("the optional Prime 5K menu entry adds the dataset and a faint cell overlay", {
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
  expect_identical(appearance, list(point_size = 1, point_opacity = 0.1))
  unlink(file.path(root, "prime.h5"))
  expect_error(
    env$viewerAddXeniumPrimeDemo(original, crb),
    "CEREBRO_XENIUM_PRIME_DEMO_CRB must name a .crb with an adjacent .h5 file."
  )
})
