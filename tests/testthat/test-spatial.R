# test-spatial.R — Tests for the spatial data backend + Shiny tab
#
# Scope: the backend data layer (Session A) and the interactive Spatial Shiny
# tab wiring (Session B). Backend contract tests come first; the module-parse
# and UI/server wiring guards follow.

shiny_root <- system.file("shiny/v1.4", package = "CerebroNexus")
# demo_spatial.crb is the synthetic Xenium demo that carries spatial data;
# the other bundled demos (PBMC sets, trajectory) have no spatial field.
spatial_crb <- system.file(
  "extdata/v1.4/demo_spatial.crb",
  package = "CerebroNexus"
)

test_that("demo_spatial.crb exposes spatial data via class methods", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readRDS(spatial_crb)
  spatial <- crb$availableSpatial()
  expect_true(is.character(spatial))
  expect_true(length(spatial) > 0)
})

test_that("demo_spatial.crb spatial data is accessible and complete", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readRDS(spatial_crb)
  spatial <- crb$availableSpatial()
  skip_if(length(spatial) == 0)
  data <- crb$getSpatialData(spatial[1])
  expect_true(is.list(data))
  expect_true(all(c("coordinates", "expression") %in% names(data)))
  expect_true(is.data.frame(data$coordinates))
  expect_true(nrow(data$coordinates) > 0)
  # exportFromSeurat crops coordinates to a 2D projection for plotting.
  expect_true(ncol(data$coordinates) >= 2)
  expect_true(nrow(data$expression) > 0)
  expect_true(ncol(data$expression) > 0)
})

test_that("getSpatialData errors on unknown spatial entry", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readRDS(spatial_crb)
  expect_error(crb$getSpatialData("__not_a_real_image__"))
})

test_that("spatial accessor methods are defined on the class", {
  cls <- Cerebro_v1.3
  for (m in c("addSpatialData", "getSpatialData", "availableSpatial")) {
    expect_true(is.function(cls$public_methods[[m]]), info = m)
  }
})

test_that("addSpatialData validates its input structure", {
  # A malformed entry (missing coordinates/expression) must be rejected so the
  # class contract getSpatialData() relies on cannot be violated silently.
  cls_text <- paste(
    deparse(Cerebro_v1.3$public_methods$addSpatialData),
    collapse = "\n"
  )
  expect_match(cls_text, "coordinates", fixed = TRUE)
  expect_match(cls_text, "expression", fixed = TRUE)
})

test_that("spatial utility wrappers are defined in the app scope", {
  # The Spatial tab (Session B) calls these free functions. They were missing
  # from dev and must be present before the module is mounted. Cross-line-
  # tolerant regex per project convention (air may reflow).
  util_src <- paste(
    readLines(file.path(shiny_root, "utility_functions.R")),
    collapse = "\n"
  )
  for (fn in c(
    "availableSpatial",
    "getSpatialData",
    "serverSideGeneSelector"
  )) {
    expect_match(
      util_src,
      paste0(fn, "[\\s]{0,3}<-[\\s]{0,3}function"),
      perl = TRUE,
      info = fn
    )
  }
})

test_that("exportFromSeurat carries the spatial extraction path", {
  # Guard that the spatial export block survived the port: exportFromSeurat must
  # reference the internal .getSpatialData() extractor and stash results via
  # addSpatialData(). Reading the deparsed function body is robust to air reflow.
  fn_text <- paste(deparse(exportFromSeurat), collapse = "\n")
  expect_match(fn_text, ".getSpatialData", fixed = TRUE)
  expect_match(fn_text, "addSpatialData", fixed = TRUE)
})

##----------------------------------------------------------------------------##
## Session B: Shiny tab wiring guards.
##----------------------------------------------------------------------------##

test_that("all spatial module files parse without errors", {
  spatial_dir <- file.path(shiny_root, "spatial")
  skip_if_not(dir.exists(spatial_dir), message = "spatial module missing")
  mod_files <- list.files(spatial_dir, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(mod_files) > 0)
  for (fpath in mod_files) {
    expect_no_error(parse(file = fpath))
  }
})

test_that("ImageFeaturePlot reaches getExpressionMatrix as a Cerebro method", {
  # getExpressionMatrix / getMeanExpressionForCells are Cerebro_v1.3 R6 methods,
  # not bare functions — they must be called through data_set()$. A bare
  # getExpressionMatrix(...) crashed the ImageFeaturePlot (gene-coloured) path
  # with "could not find function". Guard every expression-method call in the
  # spatial module against the bare form.
  spatial_dir <- file.path(shiny_root, "spatial")
  skip_if_not(dir.exists(spatial_dir), message = "spatial module missing")
  methods <- c("getExpressionMatrix", "getMeanExpressionForCells")
  for (fpath in list.files(spatial_dir, pattern = "\\.R$", full.names = TRUE)) {
    src <- paste(readLines(fpath), collapse = "\n")
    for (m in methods) {
      # a call to the method NOT immediately preceded by `$`
      bare <- gregexpr(
        paste0("(^|[^$[:alnum:]_.])", m, "\\("),
        src,
        perl = TRUE
      )[[1]]
      expect_true(
        bare[1] == -1,
        info = paste0(
          "bare ",
          m,
          "() in ",
          basename(fpath),
          " — use data_set()$"
        )
      )
    }
  }
})

test_that("plot update guards against a colour variable absent from metadata", {
  # Switching the loaded .crb can leave the point-colour dropdown holding a
  # column from the previous dataset (e.g. Xenium "cluster" vs MERFISH
  # "cell_type"). Colouring by a missing column makes the downstream
  # dplyr::group_by() error and freezes the plot on the old data. The render
  # function must fall back to a valid metadata column. Assert the guard survives
  # (cross-line tolerant per project convention).
  fpath <- file.path(shiny_root, "spatial", "func_projection_update_plot.R")
  skip_if_not(file.exists(fpath), message = "spatial update module missing")
  src <- paste(readLines(fpath), collapse = "\n")
  expect_match(
    src,
    "color_variable[\\s\\S]{0,80}%in%[\\s\\S]{0,20}colnames\\(metadata\\)",
    perl = TRUE
  )
  expect_match(
    src,
    "color_variable[\\s\\S]{0,40}<-[\\s\\S]{0,40}colnames\\(metadata\\)\\[1\\]",
    perl = TRUE
  )
})

test_that("group_filters widget the spatial tab depends on is present", {
  # spatial/UI_projection_group_filters.R calls registerGroupFiltersUI() and
  # registerGroupFiltersInfo(); those are only defined in the shared module,
  # which must be shipped and sourced or the tab errors on mount.
  widget <- file.path(
    shiny_root,
    "module",
    "group_filters",
    "group_filters_widget.R"
  )
  skip_if_not(file.exists(widget))
  widget_src <- paste(readLines(widget), collapse = "\n")
  for (fn in c("registerGroupFiltersUI", "registerGroupFiltersInfo")) {
    expect_match(
      widget_src,
      paste0(fn, "[\\s]{0,3}<-[\\s]{0,3}function"),
      perl = TRUE,
      info = fn
    )
  }
})

test_that("spatial UI defines correct tabName", {
  ui_file <- file.path(shiny_root, "spatial", "UI.R")
  skip_if_not(file.exists(ui_file))
  content <- paste(readLines(ui_file), collapse = "\n")
  expect_match(content, 'tabName\\s*=\\s*"spatial"', perl = TRUE)
})

test_that("Spatial tab is wired into the app UI and server", {
  # Guard the integration points so a future refactor that drops the wiring
  # (module present but never mounted) fails loudly. Cross-line-tolerant regex
  # per project convention (air may reflow).
  ui_src <- paste(
    readLines(file.path(shiny_root, "shiny_UI.R")),
    collapse = "\n"
  )
  expect_match(ui_src, "spatial/UI\\.R")
  expect_match(ui_src, "tab_spatial")
  expect_match(ui_src, "sidebar_item_spatial_placeholder")

  server_src <- paste(
    readLines(file.path(shiny_root, "shiny_server.R")),
    collapse = "\n"
  )
  expect_match(server_src, "spatial/server\\.R")
  expect_match(server_src, "group_filters/group_filters_widget\\.R")
  expect_match(
    server_src,
    'insertConditionalTab\\([\\s\\S]{0,80}"spatial"',
    perl = TRUE
  )
})

##----------------------------------------------------------------------------##
## Spatial background image: createShinyApp production channel + demo wiring.
##----------------------------------------------------------------------------##

test_that("createShinyApp accepts the spatial_images parameters", {
  # Guard the production API surface: every spatial_images* arg must be part of
  # the formals so downstream users can pass histology backgrounds and their
  # per-dataset alignment defaults (flip / scale / move / rotate).
  args <- names(formals(createShinyApp))
  for (a in c(
    "spatial_images",
    "spatial_images_flip_x",
    "spatial_images_flip_y",
    "spatial_images_scale_x",
    "spatial_images_scale_y",
    "spatial_images_offset_x",
    "spatial_images_offset_y",
    "spatial_plot_rotation"
  )) {
    expect_true(a %in% args, info = a)
  }
})

test_that("createShinyApp bundles a spatial image and writes the option", {
  # End-to-end exercise of the new side-copy + option-write path: a matched
  # spatial image must be copied into the bundle and its stored path rewritten
  # to the portable data/<file> form inside cerebro_config.rds.
  skip_if_not(file.exists(spatial_crb))
  img <- tempfile(fileext = ".png")
  # 1x1 transparent PNG is enough; the copy path does not decode the image.
  writeBin(
    as.raw(c(
      0x89,
      0x50,
      0x4e,
      0x47,
      0x0d,
      0x0a,
      0x1a,
      0x0a,
      0x00,
      0x00,
      0x00,
      0x0d,
      0x49,
      0x48,
      0x44,
      0x52,
      0x00,
      0x00,
      0x00,
      0x01,
      0x00,
      0x00,
      0x00,
      0x01,
      0x08,
      0x06,
      0x00,
      0x00,
      0x00,
      0x1f,
      0x15,
      0xc4,
      0x89,
      0x00,
      0x00,
      0x00,
      0x0a,
      0x49,
      0x44,
      0x41,
      0x54,
      0x78,
      0x9c,
      0x63,
      0x00,
      0x01,
      0x00,
      0x00,
      0x05,
      0x00,
      0x01,
      0x0d,
      0x0a,
      0x2d,
      0xb4,
      0x00,
      0x00,
      0x00,
      0x00,
      0x49,
      0x45,
      0x4e,
      0x44,
      0xae,
      0x42,
      0x60,
      0x82
    )),
    img
  )
  out_dir <- file.path(tempdir(), paste0("cerebro_spatial_", Sys.getpid()))
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(suppressMessages(
    createShinyApp(
      cerebro_data = c("Xenium demo" = spatial_crb),
      result_dir = out_dir,
      spatial_images = c("Xenium demo" = img),
      launch_browser = FALSE,
      verbose = FALSE
    )
  ))

  cfg_path <- file.path(out_dir, "cerebro_config.rds")
  expect_true(file.exists(cfg_path))
  cfg <- readRDS(cfg_path)
  expect_true(!is.null(cfg[["spatial_images"]]))
  # path rewritten to bundle-relative data/<file>
  stored <- cfg[["spatial_images"]][["Xenium demo"]]
  expect_match(stored, "^data/", perl = TRUE)
  # and the image really landed in the bundle
  expect_true(file.exists(file.path(out_dir, stored)))
})

test_that("createShinyApp drops unmatched spatial_images with a warning", {
  # A spatial_images entry whose name matches no dataset must be ignored (not
  # errored) so a typo never blocks app generation.
  skip_if_not(file.exists(spatial_crb))
  img <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), img)
  out_dir <- file.path(
    tempdir(),
    paste0("cerebro_spatial_unmatched_", Sys.getpid())
  )
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  expect_warning(
    suppressMessages(
      createShinyApp(
        cerebro_data = c("Xenium demo" = spatial_crb),
        result_dir = out_dir,
        spatial_images = c("no_such_dataset" = img),
        launch_browser = FALSE,
        verbose = FALSE
      )
    ),
    "No matching names"
  )
  cfg <- readRDS(file.path(out_dir, "cerebro_config.rds"))
  expect_null(cfg[["spatial_images"]])
})

test_that("Visium ships its H&E as an EXTERNAL image, not embedded", {
  # Visium deliberately demonstrates the external-image path: the H&E lives in a
  # standalone PNG loaded via `spatial_images`, and the .crb carries NO embedded
  # image (unlike MERFISH/Xenium). This keeps the .crb small and exercises the
  # spatial_images code path as a live example.
  png <- system.file(
    "extdata/v1.4/demo_spatial_visium_he.png",
    package = "CerebroNexus"
  )
  skip_if(png == "" || !file.exists(png), message = "visium H&E png missing")
  expect_gt(file.info(png)$size, 0)

  crb_path <- system.file(
    "extdata/v1.4/demo_spatial_visium.crb",
    package = "CerebroNexus"
  )
  skip_if(
    crb_path == "" || !file.exists(crb_path),
    message = "visium crb missing"
  )
  crb <- readRDS(crb_path)
  sd <- crb$getSpatialData(crb$availableSpatial()[1])
  expect_null(sd$histology_image)

  # app.R must wire the external image via spatial_images for the Visium dataset
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(app_src, "spatial_images", fixed = TRUE)
  expect_match(app_src, "demo_spatial_visium_he\\.png", perl = TRUE)
})

test_that("bundled real demos embed a genuine tissue image in the .crb", {
  # MERFISH and Xenium carry their REAL histology image (DAPI) inside the .crb
  # under `histology_image`, with coordinate-space bounds, so the Spatial tab
  # renders the true tissue background out of the box. (Visium uses an external
  # image — tested above; Slide-seq carries no image — tested below.)
  for (f in c(
    "demo_spatial_merfish",
    "demo_spatial_xenium"
  )) {
    path <- system.file(
      file.path("extdata/v1.4", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readRDS(path)
    sd <- crb$getSpatialData(crb$availableSpatial()[1])
    expect_true(is.character(sd$histology_image), info = f)
    expect_match(sd$histology_image, "^data:image/", info = f)
    b <- sd$histology_image_bounds
    expect_true(
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(b)),
      info = f
    )
    # cells must fall inside the image's coordinate-space extent
    coords <- sd$coordinates
    expect_true(
      min(coords$x) >= b$xmin &&
        max(coords$x) <= b$xmax &&
        min(coords$y) >= b$ymin &&
        max(coords$y) <= b$ymax,
      info = f
    )
  }
})

##----------------------------------------------------------------------------##
## Real multi-platform demos: each shipped .crb (Visium / Slide-seq v2 / MERFISH
## / Xenium) must load with a usable spatial slot. These are built
## from genuine public data by data-raw/build_spatial_demos.R.
##----------------------------------------------------------------------------##

real_spatial_demos <- c(
  visium = "extdata/v1.4/demo_spatial_visium.crb",
  slideseq = "extdata/v1.4/demo_spatial_slideseq.crb",
  merfish = "extdata/v1.4/demo_spatial_merfish.crb",
  xenium = "extdata/v1.4/demo_spatial_xenium.crb"
)

test_that("each real spatial demo exposes coordinates with x/y", {
  for (nm in names(real_spatial_demos)) {
    path <- system.file(real_spatial_demos[[nm]], package = "CerebroNexus")
    skip_if(
      path == "" || !file.exists(path),
      message = paste0(nm, " demo missing")
    )
    crb <- readRDS(path)
    images <- crb$availableSpatial()
    expect_true(length(images) > 0, info = nm)
    sd <- crb$getSpatialData(images[1])
    expect_true(all(c("coordinates", "expression") %in% names(sd)), info = nm)
    coords <- sd$coordinates
    expect_true(all(c("x", "y") %in% colnames(coords)), info = nm)
    expect_true(nrow(coords) > 0, info = nm)
    # coordinates and expression must share cells so the tab can colour points
    expect_true(
      length(intersect(rownames(coords), colnames(sd$expression))) > 0,
      info = nm
    )
    # x/y must be finite numerics, not all-NA
    expect_true(any(is.finite(coords$x)) && any(is.finite(coords$y)), info = nm)
  }
})

test_that("real spatial demos are wired into the bundled dropdown", {
  # The three technology-labelled demos must appear in app.R's crb_file_to_load
  # so the switcher offers them. Cross-line-tolerant per project convention.
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  for (f in real_spatial_demos) {
    expect_match(
      app_src,
      gsub(".", "\\.", basename(f), fixed = TRUE),
      perl = TRUE
    )
  }
  # the labels must name the technology in brackets
  expect_match(app_src, "Visium")
  expect_match(app_src, "Slide-seq")
  expect_match(app_src, "MERFISH")
  expect_match(app_src, "Xenium")
})

test_that("image-free demo (Slide-seq) carries no histology image", {
  # This platform records positions, not a tissue photo, so a genuine
  # absence of `histology_image` is the CORRECT state, not a build regression.
  for (f in c("demo_spatial_slideseq")) {
    path <- system.file(
      file.path("extdata/v1.4", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readRDS(path)
    sd <- crb$getSpatialData(crb$availableSpatial()[1])
    expect_null(sd$histology_image, info = f)
  }
})

test_that("embedded image demos store the image natively with no flip flag", {
  # Embedded images are stored in their native orientation; there is no per-.crb
  # render-flip flag (removed — display alignment is a user control in the tab).
  # Guard that the image is present and no stale flip flag lingers.
  for (f in c("demo_spatial_xenium", "demo_spatial_merfish")) {
    path <- system.file(
      file.path("extdata/v1.4", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readRDS(path)
    sd <- crb$getSpatialData(crb$availableSpatial()[1])
    expect_false(is.null(sd$histology_image), info = f)
    expect_null(sd$histology_image_flip_y, info = f)
  }
})

test_that("app.R ships the Visium H&E overlay pre-aligned", {
  # The bundled Visium demo opens with its H&E overlay already aligned to the
  # points, so users see a correct overlay without nudging it. app.R therefore
  # sets the per-dataset alignment presets (move + scale + a vertical flip that
  # this dataset needs); the Spatial tab still lets users adjust or Reset.
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(app_src, "\"spatial_images_flip_y\"", fixed = TRUE)
  expect_match(app_src, "\"spatial_images_offset_x\"", fixed = TRUE)
  expect_match(app_src, "\"spatial_images_scale_x\"", fixed = TRUE)
})

##----------------------------------------------------------------------------##
## Regression: .getSpatialData must tolerate a coordinate source that carries an
## NA-named / blank-named column (Slide-seq GetTissueCoordinates returns such a
## frame). Before the as_df sanitiser this crashed with
## "undefined columns selected".
##----------------------------------------------------------------------------##

test_that(".getSpatialData tolerates a real Slide-seq object (NA-named coord col)", {
  # The ssHippo GetTissueCoordinates frame carries a column literally named NA,
  # which used to crash the extractor with "undefined columns selected". This is
  # the exact object behind demo_spatial_slideseq.crb. Skipped unless the source
  # data package is installed (it is not a hard test dependency).
  skip_if_not_installed("Seurat")
  skip_if_not_installed("ssHippo.SeuratData")

  suppressWarnings(suppressMessages(
    utils::data("ssHippo", package = "ssHippo.SeuratData")
  ))
  obj <- get("ssHippo")
  obj <- suppressWarnings(Seurat::UpdateSeuratObject(obj))
  set.seed(1)
  obj <- subset(obj, cells = sample(colnames(obj), 200))

  # confirm the pathological column really is present in the raw source
  tc <- Seurat::GetTissueCoordinates(obj)
  expect_true(any(is.na(colnames(tc))))

  extractor <- getFromNamespace(".getSpatialData", "CerebroNexus")
  res <- extractor(obj, image = "image", layer = "counts", assay = "Spatial")
  expect_true(all(c("x", "y") %in% colnames(res$coordinates)))
  expect_true(nrow(res$coordinates) > 0)
  # the NA-named column must not have leaked through the sanitiser
  expect_false(any(is.na(colnames(res$coordinates))))
})
