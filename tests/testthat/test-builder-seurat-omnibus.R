builder_omnibus_env <- new.env(parent = globalenv())
sys.source(
  builder_profile_inst_path("builder", "prerequisite.R"),
  envir = builder_omnibus_env
)
builder_omnibus_env$builder_source_utf8(
  builder_profile_inst_path("builder", "io.R"),
  envir = builder_omnibus_env
)

test_that("Spatial alignment demo restores the Visium H&E and transform", {
  skip_if_not_installed("jpeg")
  builder_omnibus_env$builder_source_utf8(
    builder_profile_inst_path("builder", "extras.R"),
    envir = builder_omnibus_env
  )
  record <- builder_omnibus_env$builder_example_catalog()$spatial_alignment
  loaded <- record$make()

  expect_null(loaded$error)
  expect_identical(record$provenance, "real")
  expect_match(record$detail, "10x Genomics, CC BY 4.0", fixed = TRUE)
  expect_s4_class(loaded$object, "Seurat")
  expect_identical(SeuratObject::Images(loaded$object), "visium_breast")
  expect_true("umap" %in% SeuratObject::Reductions(loaded$object))
  expect_length(record$histology_images, 1L)
  image_path <- file.path(
    dirname(record$serialized_path),
    record$histology_images$visium_breast_cancer_he$path
  )
  expect_true(file.exists(image_path))
  he <- jpeg::readJPEG(image_path)
  width <- as.numeric(ncol(he))
  height <- as.numeric(nrow(he))

  configured <- builder_omnibus_env$builder_example_configure_entry(
    list(id = record$id, settings = list()),
    record,
    loaded$object
  )
  image <- configured$settings$images$visium_breast[["H&E"]]
  ## The example opens registered: spots are the H&E crop's pixel
  ## positions, so the image spans its own pixel extent and the preset is
  ## flip y only.
  expect_identical(image$rotation, 0)
  expect_identical(image$flip_y, TRUE)
  expect_identical(image$flip_x, FALSE)
  expect_identical(c(image$dx, image$dy, image$scale), c(0, 0, 1))
  expect_identical(
    image$base_bounds,
    list(xmin = 0, xmax = width, ymin = 0, ymax = height)
  )
  expect_identical(image$image_opacity, 0.9)
  expect_identical(image$point_opacity, 0.6)
  expect_identical(image$point_size, 7)
  expect_null(configured$settings$spatial_coordinate_transforms)
  expect_identical(
    configured$settings$spatial_point_appearance$visium_breast,
    list(point_opacity = 0.6, point_size = 7)
  )

  ## In the generated Viewer every spot's pixel lands on the spot: the
  ## settings it receives, drawn as cell_views.js drawImage() does (bounds
  ## rotated about the origin, flips and scale after the rotation).
  setting <- builder_omnibus_env$builder_alignment_viewer_setting(image)
  b <- image$base_bounds
  a <- setting$rotation * pi / 180
  rot <- function(x, y) cbind(cos(a) * x - sin(a) * y, sin(a) * x + cos(a) * y)
  cells <- SeuratObject::GetTissueCoordinates(
    loaded$object[["visium_breast"]]
  )
  p <- rot(
    b$xmin + cells$x / width * (b$xmax - b$xmin),
    b$ymax - cells$y / height * (b$ymax - b$ymin)
  )
  cr <- rot((b$xmin + b$xmax) / 2, (b$ymin + b$ymax) / 2)
  d <- c(
    setting$scale_x * if (isTRUE(setting$flip_x)) -1 else 1,
    setting$scale_y * if (isTRUE(setting$flip_y)) -1 else 1
  )
  drawn <- cbind(
    setting$offset_x + cr[1] + d[1] * (p[, 1] - cr[1]),
    setting$offset_y + cr[2] + d[2] * (p[, 2] - cr[2])
  )
  expect_equal(unname(drawn), unname(cbind(cells$x, cells$y)), tolerance = 1e-9)

  ## The coordinates really are the H&E's: the stain under each spot
  ## (colour saturation of the spot's pixels) follows the spot's expression
  ## cluster far better than under a flipped or shifted registration.
  saturation <- do.call(pmax, lapply(1:3, function(k) he[,, k])) -
    do.call(pmin, lapply(1:3, function(k) he[,, k]))
  clusters <- loaded$object$seurat_clusters[cells$cell]
  explained <- function(x, y) {
    stain <- vapply(
      seq_along(x),
      function(i) {
        rows <- pmin(pmax(round(y[[i]]) + (-6:6), 1L), height)
        columns <- pmin(pmax(round(x[[i]]) + (-6:6), 1L), width)
        mean(saturation[rows, columns])
      },
      numeric(1)
    )
    summary(stats::lm(stain ~ clusters))$r.squared
  }
  registered <- explained(cells$x, cells$y)
  expect_gt(registered, 0.45)
  for (wrong in list(
    list(width - cells$x, cells$y),
    list(cells$x, height - cells$y),
    list(width - cells$x, height - cells$y),
    list(cells$x + 30, cells$y)
  )) {
    expect_gt(registered - explained(wrong[[1]], wrong[[2]]), 0.1)
  }
})

test_that("omnibus Trekker payload satisfies the Builder content contract", {
  for (relative in c(
    "viewer/core/spatial_coordinate_contract.R",
    "builder/spatial.R",
    "builder/content_spatial.R"
  )) {
    builder_omnibus_env$builder_source_utf8(
      builder_profile_inst_path(relative),
      envir = builder_omnibus_env
    )
  }
  object <- builder_omnibus_env$builder_example_catalog()$complete_viewer_data$make()$object
  profile <- builder_omnibus_env$builder_profile_trekker_payload(
    object@misc$trekker,
    list(
      cells = SeuratObject::Cells(object),
      features = SeuratObject::Features(object)
    )
  )

  expect_true(profile$valid, info = paste(profile$diagnostics, collapse = ", "))
  expect_identical(profile$diagnostics, character())
  expect_identical(profile$page_candidates, "trekker")
})

test_that("gallery examples leave tissue images for manual upload", {
  for (relative in c("extras.R", "marker_import.R")) {
    builder_omnibus_env$builder_source_utf8(
      builder_profile_inst_path("builder", relative),
      envir = builder_omnibus_env
    )
  }
  record <- builder_omnibus_env$builder_example_catalog()$complete_viewer_data
  object <- record$make()$object
  entry <- list(
    id = record$id,
    levels = list(cell_type = levels(object$cell_type)),
    settings = list()
  )

  configured <- builder_omnibus_env$builder_example_configure_entry(
    entry,
    record,
    object
  )

  expect_named(configured$settings$marker_imports, "fixture_sidecars")
  marker <- configured$settings$marker_imports$fixture_sidecars
  expect_identical(marker$method, "Fixture sidecars")
  expect_identical(marker$group, "cell_type")
  expect_length(marker$sources, 5L)
  expect_true(marker$validation$ready)

  expect_named(
    configured$settings$tables,
    c(
      "supplementary_clinical",
      "supplementary_tables · Clinical",
      "supplementary_tables · QC"
    )
  )
  expect_identical(
    unname(vapply(
      configured$settings$tables,
      `[[`,
      character(1),
      "file_name"
    )),
    c(
      "supplementary_clinical.csv",
      "supplementary_tables.xlsx",
      "supplementary_tables.xlsx"
    )
  )
  expect_length(configured$settings$images %||% list(), 0L)
  expect_identical(configured$settings$cell_cycle_columns, "Phase")
  expect_identical(
    configured$settings$included_trajectories,
    list(monocle2 = "lineage")
  )
  expect_identical(
    configured$settings$default_trajectory,
    list(method = "monocle2", name = "lineage")
  )

  trekker <- builder_omnibus_env$builder_example_catalog()$trekker_spatial
  trekker_entry <- list(settings = list(images = list()))
  expect_identical(
    builder_omnibus_env$builder_example_configure_entry(
      trekker_entry,
      trekker,
      trekker$make()$object
    ),
    trekker_entry
  )
})

test_that("complete fixture regenerates with stable semantic content", {
  script <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "build_builder_fixtures.R"
  )
  skip_if_not(file.exists(script), "Regeneration scripts are repository-only")
  skip_if_not_installed("readxl")
  skip_if_not_installed("jpeg")
  skip_if_not_installed("png")
  skip_if_not_installed("writexl")
  record <- builder_omnibus_env$builder_example_catalog()$complete_viewer_data
  fixture_dir <- dirname(record$serialized_path)
  repo <- normalizePath(file.path(fixture_dir, "..", "..", "..", ".."))
  output <- withr::local_tempdir()
  command_output <- withr::with_dir(repo, {
    system2(
      "Rscript",
      c("--encoding=UTF-8", "data-raw/build_builder_fixtures.R", output),
      stdout = TRUE,
      stderr = TRUE
    )
  })
  status <- attr(command_output, "status")
  expect_true(is.null(status) || identical(status, 0L))

  current <- readRDS(record$serialized_path)
  regenerated <- readRDS(file.path(output, "complete_viewer_data.rds"))
  expect_equal(current@meta.data, regenerated@meta.data)
  expect_equal(
    SeuratObject::LayerData(current, layer = "counts"),
    SeuratObject::LayerData(regenerated, layer = "counts")
  )
  expect_equal(
    lapply(current@reductions, SeuratObject::Embeddings),
    lapply(regenerated@reductions, SeuratObject::Embeddings)
  )
  expect_equal(current@misc, regenerated@misc)
  for (fov in SeuratObject::Images(current)) {
    expect_equal(
      SeuratObject::GetTissueCoordinates(current[[fov]]),
      SeuratObject::GetTissueCoordinates(regenerated[[fov]])
    )
  }

  delimited <- c(
    "marker_t_cells.csv",
    "marker_b_cells.tsv",
    "supplementary_clinical.csv"
  )
  for (name in delimited) {
    separator <- if (identical(tools::file_ext(name), "tsv")) "\t" else ","
    expect_equal(
      utils::read.delim(
        file.path(fixture_dir, name),
        sep = separator,
        check.names = FALSE
      ),
      utils::read.delim(
        file.path(output, name),
        sep = separator,
        check.names = FALSE
      )
    )
  }
  for (name in c("marker_panels.xlsx", "supplementary_tables.xlsx")) {
    sheets <- readxl::excel_sheets(file.path(fixture_dir, name))
    expect_identical(
      sheets,
      readxl::excel_sheets(file.path(output, name))
    )
    for (sheet in sheets) {
      expect_equal(
        readxl::read_excel(file.path(fixture_dir, name), sheet = sheet),
        readxl::read_excel(file.path(output, name), sheet = sheet)
      )
    }
  }
  images <- grep(
    "[.](png|jpg)$",
    record$expected_supporting_content,
    value = TRUE
  )
  lossless <- images[tolower(tools::file_ext(images)) == "png"]
  expect_identical(
    unname(tools::md5sum(file.path(fixture_dir, lossless))),
    unname(tools::md5sum(file.path(output, lossless)))
  )
  for (name in setdiff(images, lossless)) {
    reference <- jpeg::readJPEG(file.path(fixture_dir, name))
    regenerated <- jpeg::readJPEG(file.path(output, name))
    difference <- abs(reference - regenerated)
    expect_identical(dim(reference), dim(regenerated), info = name)
    expect_lte(max(difference), 0.05, label = name)
    expect_lte(mean(difference), 0.002, label = name)
  }
})
