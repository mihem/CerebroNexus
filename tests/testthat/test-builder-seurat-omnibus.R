builder_omnibus_env <- new.env(parent = globalenv())
sys.source(
  builder_profile_inst_path("builder", "prerequisite.R"),
  envir = builder_omnibus_env
)
builder_omnibus_env$builder_source_utf8(
  builder_profile_inst_path("builder", "io.R"),
  envir = builder_omnibus_env
)

test_that("Builder gallery offers all packaged examples through one action", {
  catalog <- builder_omnibus_env$builder_example_catalog()
  directory <- builder_omnibus_env$builder_example_directory()

  expect_identical(
    names(catalog),
    c("complete_viewer_data", "trekker_spatial", "spatial_alignment")
  )
  expect_identical(
    unname(vapply(directory, `[[`, character(1), "id")),
    "complete_viewer_data"
  )
  expect_identical(
    vapply(directory[[1L]]$examples, `[[`, character(1), "id"),
    names(catalog)
  )
  expect_identical(directory[[1L]]$label, "Load example datasets")
  expect_identical(catalog$complete_viewer_data$label, "Complete Viewer demo")
  expect_identical(catalog$complete_viewer_data$provenance, "synthetic")
  expect_match(
    catalog$complete_viewer_data$serialized_path,
    "complete_viewer/complete_viewer_data\\.rds$"
  )
  expect_identical(catalog$trekker_spatial$label, "Trekker spatial demo")
  expect_identical(catalog$trekker_spatial$provenance, "synthetic")
  expect_match(
    catalog$trekker_spatial$serialized_path,
    "trekker_spatial/trekker_4_tissues\\.qs2$"
  )
  expect_identical(
    catalog$spatial_alignment$label,
    "Spatial alignment demo"
  )
  expect_identical(catalog$spatial_alignment$provenance, "synthetic")
  expect_match(
    catalog$spatial_alignment$serialized_path,
    "spatial_alignment/arrow_spatial\\.rds$"
  )
  expect_false(anyDuplicated(names(catalog)) > 0L)
})

test_that("Spatial alignment demo restores the arrow image and transform", {
  builder_omnibus_env$builder_source_utf8(
    builder_profile_inst_path("builder", "extras.R"),
    envir = builder_omnibus_env
  )
  record <- builder_omnibus_env$builder_example_catalog()$spatial_alignment
  loaded <- record$make()

  expect_null(loaded$error)
  expect_s4_class(loaded$object, "Seurat")
  expect_identical(SeuratObject::Images(loaded$object), "arrow_fov")
  expect_true("umap" %in% SeuratObject::Reductions(loaded$object))
  expect_length(record$histology_images, 1L)
  expect_true(file.exists(file.path(
    dirname(record$serialized_path),
    record$histology_images$arrow_background$path
  )))

  configured <- builder_omnibus_env$builder_example_configure_entry(
    list(id = record$id, settings = list()),
    record,
    loaded$object
  )
  image <- configured$settings$images$arrow_fov[["Arrow background"]]
  ## The example opens registered: cells are the background's pixel
  ## positions, so the preset is flip y only.
  expect_identical(image$rotation, 0)
  expect_identical(image$flip_y, TRUE)
  expect_identical(image$flip_x, FALSE)
  expect_identical(c(image$dx, image$dy, image$scale), c(0, 0, 1))
  expect_identical(
    image$base_bounds,
    list(xmin = 0, xmax = 800, ymin = 0, ymax = 500)
  )
  expect_identical(image$image_opacity, 0.65)
  expect_identical(image$point_opacity, 0.6)
  expect_identical(image$point_size, 7)
  expect_null(configured$settings$spatial_coordinate_transforms)
  expect_identical(
    configured$settings$spatial_point_appearance$arrow_fov,
    list(point_opacity = 0.6, point_size = 7)
  )

  ## In the generated Viewer every cell's pixel lands on the cell: the
  ## settings it receives, drawn as cell_views.js drawImage() does (bounds
  ## rotated about the origin, flips and scale after the rotation).
  setting <- builder_omnibus_env$builder_alignment_viewer_setting(image)
  b <- image$base_bounds
  a <- setting$rotation * pi / 180
  rot <- function(x, y) cbind(cos(a) * x - sin(a) * y, sin(a) * x + cos(a) * y)
  cells <- SeuratObject::GetTissueCoordinates(loaded$object[["arrow_fov"]])
  p <- rot(
    b$xmin + cells$x / 800 * (b$xmax - b$xmin),
    b$ymax - cells$y / 500 * (b$ymax - b$ymin)
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
})

test_that("Trekker fixture carries one SlideSeq section and four H&E images", {
  record <- builder_omnibus_env$builder_example_catalog()$trekker_spatial
  loaded <- record$make()

  expect_null(loaded$error)
  expect_s4_class(loaded$object, "Seurat")
  expect_identical(SeuratObject::Images(loaded$object), "slice1")
  expect_true("SPATIAL" %in% SeuratObject::Reductions(loaded$object))
  expect_length(record$histology_images, 4L)
  expect_true(all(vapply(
    record$histology_images,
    function(image) identical(image$fov_ids, "slice1"),
    logical(1)
  )))
  expect_true(all(file.exists(file.path(
    dirname(record$serialized_path),
    vapply(record$histology_images, `[[`, character(1), "path")
  ))))
})

test_that("complete fixture models every Viewer data family", {
  object <- builder_omnibus_env$builder_example_catalog()$complete_viewer_data$make()$object

  expect_s4_class(object, "Seurat")
  expect_s4_class(
    SeuratObject::LayerData(object, layer = "counts"),
    "sparseMatrix"
  )
  expect_true(all(
    c(
      "patient_id",
      "section_id",
      "fov_id",
      "sample_id",
      "condition",
      "cell_type",
      "cluster",
      "region",
      "Phase",
      "nCount_RNA",
      "nFeature_RNA"
    ) %in%
      colnames(object[[]])
  ))
  expect_setequal(
    unique(as.character(object$patient_id)),
    c(
      "patient_a",
      "patient_b",
      "patient_c"
    )
  )
  expect_setequal(SeuratObject::Reductions(object), c("pca", "umap", "tsne"))

  fovs <- c(
    "section_a_1_fov_1",
    "section_a_2_fov_1",
    "section_b_1_fov_1",
    "section_b_2_fov_1",
    "section_b_3_fov_1",
    "section_c_1_fov_1"
  )
  expect_setequal(SeuratObject::Images(object), fovs)
  section_map <- unique(data.frame(
    patient_id = as.character(object$patient_id),
    section_id = as.character(object$section_id),
    stringsAsFactors = FALSE
  ))
  expect_identical(
    as.integer(table(section_map$patient_id)),
    c(2L, 3L, 1L)
  )
  expect_true(all(vapply(
    split(object$section_id, object$fov_id),
    function(x) length(unique(x)) == 1L,
    logical(1)
  )))
  expect_true(all(vapply(
    split(object$patient_id, object$section_id),
    function(x) length(unique(x)) == 1L,
    logical(1)
  )))
  for (fov in fovs) {
    fov_cells <- SeuratObject::Cells(object[[fov]])
    expect_gt(length(fov_cells), 0L)
    expect_true(all(fov_cells %in% colnames(object)))
    expect_true(all(as.character(object$fov_id[fov_cells]) == fov))
    coordinates <- SeuratObject::GetTissueCoordinates(object, image = fov)
    expect_true(all(c("x", "y") %in% colnames(coordinates)))
    expect_true(all(is.finite(coordinates$x)))
    expect_true(all(is.finite(coordinates$y)))
    expect_gt(length(unique(coordinates$x)), 1L)
    expect_gt(length(unique(coordinates$y)), 1L)
    expect_identical(
      unique(as.character(object$section_id[fov_cells])),
      sub("_fov_[0-9]+$", "", fov)
    )
  }
  coordinate_system <- object@misc$spatial_coordinate_system
  expect_setequal(names(coordinate_system), fovs)
  expect_true(all(vapply(
    coordinate_system,
    function(item) {
      identical(item$unit, "micron") &&
        identical(item$origin, "top-left") &&
        identical(item$x_direction, "right") &&
        identical(item$y_direction, "down")
    },
    logical(1)
  )))

  expected_misc <- c(
    "gene_lists",
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectories",
    "extra_material",
    "immune_repertoire",
    "hla_typing"
  )
  expect_true(all(expected_misc %in% names(object@misc)))
  expect_setequal(
    names(object@misc$immune_repertoire),
    unique(as.character(object$sample_id))
  )
  expect_identical(object@misc$hla_typing_source_type, "synthetic")
  expect_s3_class(object@misc$extra_material$plots$overview, "ggplot")
  expect_true(all(
    c("meta", "edges") %in%
      names(object@misc$trajectories$monocle2$lineage)
  ))

  trekker <- object@misc$trekker
  expect_type(trekker, "list")
  expect_true(length(trekker$barcodes) > 0L)
  expect_identical(anyDuplicated(trekker$barcodes), 0L)
  expect_true(all(trekker$barcodes %in% colnames(object)))
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

test_that("omnibus fixture declares section-owned multi-image histology sidecars", {
  record <- builder_omnibus_env$builder_example_catalog()$complete_viewer_data
  expected <- c(
    "section_a_1_he.png",
    "section_a_1_dapi.png",
    "section_a_2_he.png",
    "section_a_2_dapi.png",
    "section_b_1_he.png",
    "section_b_1_if.jpg",
    "section_b_1_pas.png"
  )

  expect_setequal(
    unname(vapply(record$histology_images, `[[`, character(1), "path")),
    expected
  )
  expect_setequal(
    names(record$histology_images),
    tools::file_path_sans_ext(expected)
  )
  expect_true(all(mapply(
    function(id, image) identical(image$id, id),
    names(record$histology_images),
    record$histology_images,
    USE.NAMES = FALSE
  )))
  images_by_fov <- split(
    names(record$histology_images),
    unlist(lapply(record$histology_images, `[[`, "fov_ids"), use.names = FALSE)
  )
  expect_identical(
    images_by_fov[["section_a_1_fov_1"]],
    c("section_a_1_he", "section_a_1_dapi")
  )
  expect_identical(
    images_by_fov[["section_a_2_fov_1"]],
    c("section_a_2_he", "section_a_2_dapi")
  )
  expect_identical(
    images_by_fov[["section_b_1_fov_1"]],
    c("section_b_1_he", "section_b_1_if", "section_b_1_pas")
  )
  expect_false("section_c_1_fov_1" %in% names(images_by_fov))
  expect_true(all(file.exists(file.path(
    dirname(record$serialized_path),
    vapply(record$histology_images, `[[`, character(1), "path")
  ))))
  expected_sidecars <- c(
    "marker_t_cells.csv",
    "marker_b_cells.tsv",
    "marker_panels.xlsx",
    "supplementary_clinical.csv",
    "supplementary_tables.xlsx",
    expected
  )
  expect_setequal(record$expected_supporting_content, expected_sidecars)
  expect_setequal(
    setdiff(
      list.files(dirname(record$serialized_path)),
      basename(record$serialized_path)
    ),
    expected_sidecars
  )
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

test_that("complete fixture generator defaults to its named directory", {
  script_path <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "build_builder_fixtures.R"
  )
  skip_if_not(
    file.exists(script_path),
    "Regeneration scripts are repository-only"
  )
  script <- paste(
    readLines(
      script_path,
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    script,
    'file.path("inst", "builder", "fixtures", "complete_viewer")',
    fixed = TRUE
  )
})
