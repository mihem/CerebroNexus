utils_env <- new.env(parent = globalenv())
sys.source(viewer_test_path("utility_functions.R"), envir = utils_env)

test_that("Viewer resolves per-section Spatial point appearance", {
  options <- list(
    viewer_content = list(
      "Dataset A" = list(
        spatial_point_appearance = list(
          "fov-a" = list(point_opacity = 0.42, point_size = 8)
        )
      )
    )
  )

  expect_identical(
    utils_env$spatialPointAppearance(options, "Dataset A", "fov-a"),
    list(point_opacity = 0.42, point_size = 8)
  )
  expect_null(utils_env$spatialPointAppearance(options, "Dataset A", "missing"))
})


test_that("Builder preflight reuse retains the master's dataset catalog", {
  root <- withr::local_tempdir()
  path <- file.path(root, "small.crb")
  object <- Cerebro$new()
  object$setExpressionBackend(type = "embedded")
  object$setMetaData(data.frame(cell_barcode = c("a", "b"), sample = "sample"))
  saveRDS(object, path)
  data <- c(dataset = path)
  preflight <- .preflightBundleData(data)
  expect_equal(preflight$dataset_catalog$dataset$cells, 2L)
  withr::defer(.clearBundlePreflightCache())
  expect_true(.cacheBundlePreflightData(
    data,
    preflight,
    list(dataset = .bundlePreflightFingerprint(path))
  ))
  expect_identical(
    .preflightBundleData(
      data,
      read_object = function(...) stop("verification should be reused")
    ),
    preflight
  )
})

test_that("Builder secure credentials do not depend on R's random seed", {
  withr::local_seed(7)
  first <- .builderRandomBytes(32L)
  set.seed(7)
  second <- .builderRandomBytes(32L)
  expect_type(first, "raw")
  expect_length(first, 32L)
  expect_false(identical(first, second))
  for (bad in list(0, -1, NA_real_, Inf, 0.5, "32", c(1, 2))) {
    expect_error(.builderRandomBytes(bad), "byte count")
  }
})

test_that("Builder MD5 identities retain their existing byte-level format", {
  bytes <- charToRaw("abc")
  expect_identical(
    digest::digest(bytes, algo = "md5", serialize = FALSE),
    "900150983cd24fb0d6963f7d28e17f72"
  )
  builder_repo_source("io.R", local = environment())
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  destination <- file.path(root, "copy")
  writeBin(bytes, source)
  result <- builder_copy_file_verified(
    source,
    destination,
    expected_md5 = "900150983cd24fb0d6963f7d28e17f72",
    buffer_bytes = 2L
  )
  expect_identical(result$md5, "900150983cd24fb0d6963f7d28e17f72")
  expect_identical(readBin(destination, "raw", n = 10L), bytes)
})

test_that("a verified copy hashes while copying instead of rereading", {
  builder_repo_source("io.R", local = environment())
  skip_if(is.null(.builder_md5_stream()))
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  bytes <- as.raw(sample(0:255, 300000L, replace = TRUE))
  writeBin(bytes, source)
  expected <- unname(tools::md5sum(source))
  progress <- list()
  testthat::local_mocked_bindings(
    md5sum = function(files) stop("the copy must not be reread"),
    .package = "tools"
  )

  result <- builder_copy_file_verified(
    source,
    file.path(root, "copy"),
    expected_md5 = expected,
    buffer_bytes = 65536L,
    on_progress = function(done, total) {
      progress[[length(progress) + 1L]] <<- c(done, total)
    }
  )

  expect_identical(result$md5, expected)
  expect_identical(result$bytes, 300000)
  expect_identical(readBin(result$path, "raw", n = 400000L), bytes)
  expect_length(progress, 5L)
  expect_identical(progress[[5L]], c(300000, 300000))
  expect_true(all(diff(vapply(progress, `[[`, numeric(1), 1L)) > 0))
  expect_error(
    builder_copy_file_verified(
      source,
      file.path(root, "mismatch"),
      expected_md5 = strrep("0", 32L)
    ),
    "does not match"
  )
  expect_false(file.exists(file.path(root, "mismatch")))
})

test_that("a verified copy falls back to hashing the file", {
  builder_repo_source("io.R", local = environment())
  .builder_md5_stream <- function() NULL
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  writeBin(charToRaw("abc"), source)
  result <- builder_copy_file_verified(source, file.path(root, "copy"))
  expect_identical(result$md5, "900150983cd24fb0d6963f7d28e17f72")
})

test_that("only files under the session temp folder count as owned uploads", {
  builder_repo_source("io.R", local = environment())
  temp_root <- withr::local_tempdir()
  other <- withr::local_tempdir()
  upload_dir <- file.path(temp_root, "shiny-upload")
  dir.create(upload_dir)
  upload <- file.path(upload_dir, "0.qs2")
  outside <- file.path(other, "dataset.qs2")
  writeBin(charToRaw("x"), upload)
  writeBin(charToRaw("x"), outside)

  expect_true(builder_upload_transport_owned(upload, temp_root = temp_root))
  expect_false(builder_upload_transport_owned(outside, temp_root = temp_root))
  expect_false(builder_upload_transport_owned(
    upload_dir,
    temp_root = temp_root
  ))
  expect_false(builder_upload_transport_owned(
    file.path(upload_dir, "missing.qs2"),
    temp_root = temp_root
  ))
  expect_false(builder_upload_transport_owned(temp_root, temp_root = temp_root))
  expect_false(builder_upload_transport_owned(NA_character_))
  expect_false(builder_upload_transport_owned(c(upload, upload)))
})

test_that("export spatial projections use shared x and y axis semantics", {
  expect_true(exists(".spx_export_projection_coordinates", mode = "function"))
  if (!exists(".spx_export_projection_coordinates", mode = "function")) {
    return(invisible())
  }

  cells <- c("cell-b", "cell-a")
  visium <- data.frame(
    imagerow = c(200, 100),
    imagecol = c(20, 10),
    row.names = cells
  )
  projection <- .spx_export_projection_coordinates(visium)
  expect_identical(names(projection), c("x", "y"))
  expect_identical(projection$x, visium$imagecol)
  expect_identical(projection$y, visium$imagerow)
  expect_identical(rownames(projection), cells)

  standardized <- data.frame(
    x = visium$imagecol,
    y = visium$imagerow,
    imagerow = visium$imagerow,
    imagecol = visium$imagecol,
    row.names = cells
  )
  expect_identical(
    .spx_export_projection_coordinates(standardized),
    standardized[, c("x", "y"), drop = FALSE]
  )
  expect_null(.spx_export_projection_coordinates(data.frame(foo = 1, bar = 2)))

  core_path <- system.file(
    "viewer/core/spatial_coordinate_contract.R",
    package = "CerebroNexus"
  )
  builder_path <- system.file("builder/spatial.R", package = "CerebroNexus")
  if (!nzchar(core_path)) {
    core_path <- builder_test_inst_path(
      "viewer",
      "core",
      "spatial_coordinate_contract.R"
    )
  }
  if (!nzchar(builder_path)) {
    builder_path <- builder_test_inst_path(
      "builder",
      "spatial.R"
    )
  }
  runtime <- new.env(parent = baseenv())
  sys.source(core_path, envir = runtime)
  sys.source(builder_path, envir = runtime)
  normalized <- runtime$builder_spatial_contract(
    visium,
    cells = rev(cells),
    barcodes = rownames(visium),
    source = "seurat_image"
  )$preview
  projection <- projection[normalized$cell_barcode, , drop = FALSE]
  expect_identical(projection$x, normalized$x)
  expect_identical(projection$y, normalized$y)
})

test_that("Xenium segmentation normalizes to the cell-boundary contract", {
  skip_if_not_installed("SeuratObject")
  object <- builder_content_spatial_example_object("xenium-fov")
  expect_null(CerebroNexus:::.getSpatialBoundaries(
    object,
    "xenium-fov",
    colnames(object)
  ))
  image <- object@images[[1L]]
  boundaries <- methods::slot(image, "boundaries")
  cells <- colnames(object)[1:2]
  boundaries$segmentation <- SeuratObject::CreateSegmentation(data.frame(
    x = c(0, 1, 1, 0, 2, 3, 3, 2),
    y = c(0, 0, 1, 1, 2, 2, 3, 3),
    cell = rep(cells, each = 4L)
  ))
  methods::slot(image, "boundaries") <- boundaries
  methods::slot(image, "molecules") <- list(
    RNA = SeuratObject::CreateMolecules(data.frame(
      x = c(0.25, 0.75, 2.25, 2.75),
      y = c(0.25, 0.75, 2.25, 2.75),
      gene = c("Gene1", "Gene2", "Gene1", "Gene2")
    ))
  )
  object@images[[1L]] <- image

  result <- CerebroNexus:::.getSpatialBoundaries(
    object,
    "xenium-fov",
    colnames(object)
  )

  expect_named(result, c("cell_barcode", "x", "y", "part"))
  expect_setequal(unique(result$cell_barcode), cells)
  expect_true(all(is.finite(result$x)) && all(is.finite(result$y)))
  expect_true(all(result$part == "1"))

  molecules <- CerebroNexus:::.getSpatialMolecules(
    object,
    "xenium-fov",
    max_molecules = 3L
  )
  expect_named(
    molecules,
    c("data", "genes", "total_count", "exported_count", "truncated")
  )
  expect_named(molecules$data, c("gene", "x", "y"))
  expect_identical(molecules$total_count, 4L)
  expect_identical(molecules$exported_count, 3L)
  expect_true(molecules$truncated)

  methods::slot(image, "molecules") <- list(
    first = SeuratObject::CreateMolecules(data.frame(
      x = 1:4,
      y = 11:14,
      gene = paste0("A", 1:4)
    )),
    second = SeuratObject::CreateMolecules(data.frame(
      x = 5:8,
      y = 15:18,
      gene = paste0("B", 1:4)
    ))
  )
  object@images[[1L]] <- image
  molecules <- CerebroNexus:::.getSpatialMolecules(
    object,
    "xenium-fov",
    max_molecules = 5L
  )
  keep <- unique(round(seq.int(1L, 8L, length.out = 5L)))
  expected_genes <- paste0(rep(c("A", "B"), each = 4L), 1:4)[keep]
  expect_identical(molecules$data$gene, expected_genes)
  expect_identical(molecules$data$x, as.numeric(1:8)[keep])
  expect_identical(molecules$data$y, as.numeric(11:18)[keep])
  expect_identical(molecules$total_count, 8L)
  expect_identical(molecules$exported_count, 5L)
  expect_true(molecules$truncated)

  large_coordinates <- data.frame(
    x = seq_len(120L),
    y = seq_len(120L) + 1000,
    gene = rep(c("GeneB", "GeneA"), each = 60L)
  )
  large <- SeuratObject::CreateMolecules(large_coordinates)
  complete <- SeuratObject::GetTissueCoordinates(large)
  sampled <- CerebroNexus:::.spx_sample_large_molecules(list(large), 4L)
  keep <- unique(round(seq.int(1L, nrow(complete), length.out = 4L)))
  expect_identical(
    sampled$data,
    data.frame(
      gene = complete$molecule[keep],
      x = complete$x[keep],
      y = complete$y[keep],
      stringsAsFactors = FALSE
    )
  )
  expect_identical(sampled$total_count, 120L)
  expect_identical(sampled$exported_count, 4L)
  expect_true(sampled$truncated)

  path <- tempfile(fileext = ".crb")
  on.exit(unlink(path), add = TRUE)
  exportFromSeurat(
    object = object,
    assay = "RNA",
    slot = "counts",
    file = path,
    experiment_name = "Xenium boundaries",
    organism = "hg",
    groups = c("sample", "seurat_clusters"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    projections = "umap",
    spatial_coordinate_transforms = list(
      `xenium-fov` = list(rotation_degrees = 90, scale = 2)
    ),
    verbose = FALSE
  )
  stored <- readCerebro(path)$getSpatialData("xenium-fov")
  expect_named(stored$boundaries, c("cell_barcode", "x", "y", "part"))
  expect_setequal(unique(stored$boundaries$cell_barcode), cells)
  pivot <- stored$coordinate_transform$pivot
  expect_equal(
    stored$boundaries$x,
    pivot[["x"]] - 2 * (result$y - pivot[["y"]]),
    tolerance = 1e-10
  )
  expect_equal(
    stored$boundaries$y,
    pivot[["y"]] + 2 * (result$x - pivot[["x"]]),
    tolerance = 1e-10
  )
  expect_named(
    stored$molecules,
    c("data", "genes", "total_count", "exported_count", "truncated")
  )
  expect_equal(
    stored$molecules$data$x,
    pivot[["x"]] - 2 * (as.numeric(11:18) - pivot[["y"]]),
    tolerance = 1e-10
  )
})
