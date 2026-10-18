# test-spatial.R — Tests for the spatial data backend + Shiny tab
#
# Scope: the backend data layer (Session A) and the interactive Spatial Shiny
# tab wiring (Session B). Backend contract tests come first; the module-parse
# and UI/server wiring guards follow.

shiny_root <- viewer_test_path()
# demo_spatial.crb is the synthetic Xenium demo that carries spatial data;
# the other bundled demos (PBMC sets, trajectory) have no spatial field.
spatial_crb <- system.file(
  "extdata/examples/demo_spatial.crb",
  package = "CerebroNexus"
)

test_that("demo_spatial.crb exposes spatial data via class methods", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readCerebro(spatial_crb)
  spatial <- crb$availableSpatial()
  expect_true(is.character(spatial))
  expect_true(length(spatial) > 0)
})

test_that("demo_spatial.crb spatial data is accessible and complete", {
  skip_if_not(file.exists(spatial_crb))
  crb <- readCerebro(spatial_crb)
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
  crb <- readCerebro(spatial_crb)
  expect_error(crb$getSpatialData("__not_a_real_image__"))
})

test_that("spatial accessor methods are defined on the class", {
  cls <- Cerebro
  for (m in c("addSpatialData", "getSpatialData", "availableSpatial")) {
    expect_true(is.function(cls$public_methods[[m]]), info = m)
  }
})

test_that("addSpatialData validates its input structure", {
  object <- Cerebro$new()
  for (entry in list(
    list(),
    list(coordinates = data.frame(x = 1, y = 2)),
    list(expression = matrix(1))
  )) {
    expect_error(
      object$addSpatialData("malformed", entry),
      "coordinates|expression"
    )
  }
  valid <- list(coordinates = data.frame(x = 1, y = 2), expression = matrix(1))
  object$addSpatialData("valid", valid)
  expect_identical(
    object$getSpatialData("valid")$coordinates,
    valid$coordinates
  )
  expect_identical(object$getSpatialData("valid")$expression, valid$expression)
})

test_that("exportFromSeurat carries the spatial extraction path", {
  skip_if_not_installed("Seurat")
  object <- make_synthetic_spatial_seurat(n_cells = 8, n_genes = 6)
  out <- withr::local_tempdir()
  path <- convert_synthetic_to_crb(object, out, "spatial export")
  exported <- readCerebro(path)
  expect_identical(exported$availableSpatial(), "fov")
  coordinates <- exported$getSpatialData("fov")$coordinates
  expected <- SeuratObject::GetTissueCoordinates(object[["fov"]])
  expect_equal(coordinates$x, expected$x)
  expect_equal(coordinates$y, expected$y)
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

test_that("spatial coordinate reuse requires exact canonical full order", {
  canonical <- seq_len(6L)

  expect_true(spatial_coordinates_are_canonical_full_order(
    canonical,
    canonical,
    6L
  ))
  expect_false(spatial_coordinates_are_canonical_full_order(
    canonical[-3L],
    canonical,
    6L
  ))
  expect_false(spatial_coordinates_are_canonical_full_order(
    rev(canonical),
    canonical,
    6L
  ))
  expect_false(spatial_coordinates_are_canonical_full_order(
    canonical,
    c(2L, 1L, 3L, 4L, 5L, 6L),
    6L
  ))
  expect_false(spatial_coordinates_are_canonical_full_order(
    as.numeric(canonical),
    canonical,
    6L
  ))
  expect_false(spatial_coordinates_are_canonical_full_order(
    canonical,
    canonical,
    5L
  ))
})

test_that("validated spatial coordinates reuse canonical storage with fallback", {
  assignment <- viewer_r_assignment(
    viewer_test_path("utility_functions.R"),
    "viewerSpatialCoordinates"
  )
  coordinates <- data.frame(x = 1:3, y = 4:6)
  dataset <- new.env(parent = emptyenv())
  class(dataset) <- c("Cerebro", "R6")
  dataset$spatial <- list(slice = list(coordinates = coordinates))
  calls <- 0L
  environment <- new.env(parent = baseenv())
  environment$data_set <- function() dataset
  environment$getSpatialData <- function(name) {
    calls <<- calls + 1L
    list(coordinates = coordinates)
  }
  eval(assignment, envir = environment)

  expect_identical(environment$viewerSpatialCoordinates("slice"), coordinates)
  expect_identical(calls, 0L)

  dataset$spatial$slice$coordinates <- data.frame(a = 1:3, b = 4:6)
  expect_identical(environment$viewerSpatialCoordinates("slice"), coordinates)
  expect_identical(calls, 1L)
})

test_that("spatial coordinates retain subset and dataset-switch fallbacks", {
  coordinates <- list(
    million = data.frame(
      x = 1:6,
      y = 11:16,
      row.names = paste0("million-", 1:6)
    ),
    alternate = data.frame(
      x = 101:106,
      y = 111:116,
      row.names = paste0("alternate-", 1:6)
    )
  )
  server <- function(input, output, session) {
    selected <- shiny::reactiveVal("million")
    cells <- shiny::reactiveVal(seq_len(6L))
    canonical_index <- shiny::reactiveVal(seq_len(6L))
    spatial_projection_parameters_plot <- shiny::reactive(list(
      projection = selected()
    ))
    spatial_projection_cells_to_show <- shiny::reactive(cells())
    spatial_projection_cell_index <- shiny::reactive(canonical_index())
    availableSpatial <- function() names(coordinates)
    viewerSpatialCoordinates <- function(name) coordinates[[name]]
    sys.source(
      viewer_test_path("spatial", "obj_projection_coordinates.R"),
      envir = environment()
    )
  }

  shiny::testServer(server, {
    expect_identical(spatial_projection_coordinates(), coordinates$million)

    cells(seq_len(3L))
    session$flushReact()
    expect_identical(
      spatial_projection_coordinates(),
      coordinates$million[seq_len(3L), , drop = FALSE]
    )

    selected("alternate")
    canonical_index(c(2L, 1L, 3L, 4L, 5L, 6L))
    cells(seq_len(6L))
    session$flushReact()
    expect_identical(
      spatial_projection_coordinates(),
      coordinates$alternate[c(2L, 1L, 3L, 4L, 5L, 6L), , drop = FALSE]
    )

    selected("million")
    canonical_index(seq_len(6L))
    cells(seq_len(6L))
    session$flushReact()
    expect_identical(spatial_projection_coordinates(), coordinates$million)
  })
})

test_that("Moran's I reads Spatial data only after ImageFeaturePlot is requested", {
  shiny::testServer(
    function(input, output, session) {
      spatial_reads <- shiny::reactiveVal(0L)
      spatial_projection_parameters_plot <- shiny::reactive({
        list(
          plot_type = input[["spatial_projection_plot_type"]],
          feature_to_display = input[["spatial_projection_feature_to_display"]],
          projection = "synthetic_grid"
        )
      })
      getGeneNames <- function() "GeneA"
      getSpatialData <- function(name) {
        spatial_reads(shiny::isolate(spatial_reads()) + 1L)
        list(coordinates = data.frame(x = seq_len(7L), y = seq_len(7L)))
      }
      spatial_projection_cell_index <- shiny::reactive(paste0(
        "cell",
        seq_len(7L)
      ))
      viewerExpressionRow <- function(data, cells, gene) seq_along(cells)
      data_set <- shiny::reactive(list())
      morans_i <- function(x, y, values, k = 6) 0.5
      cachePlot <- function(x, ...) x
      available_crb_files <- shiny::reactiveValues(selected = "dataset.crb")
      viewer_loaded_dataset_context <- shiny::reactive(list(
        epoch = "spatial-test",
        dataset_key = "dataset.crb",
        generation = 1L
      ))
      viewerDatasetContextToken <- function(context) {
        paste(
          context$epoch,
          context$dataset_key,
          context$generation,
          sep = ":"
        )
      }

      sys.source(
        file.path(shiny_root, "spatial", "out_morans_i.R"),
        envir = environment()
      )
    },
    {
      session$setInputs(
        sidebar = "spatial",
        spatial_projection_plot_type = "ImageDimPlot",
        spatial_projection_feature_to_display = "GeneA"
      )
      expect_error(
        output[["spatial_projection_morans_i"]],
        class = "shiny.silent.error"
      )
      expect_identical(spatial_reads(), 0L)

      session$setInputs(spatial_projection_plot_type = "ImageFeaturePlot")
      expect_identical(output[["spatial_projection_morans_i"]], "0.500")
      expect_identical(spatial_reads(), 1L)
    }
  )
})

test_that("spatial UI defines correct tabName", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("spatial", "UI.R"), scope)
  page <- htmltools::tagQuery(scope$tab_spatial)
  expect_equal(page$filter("#shiny-tab-spatial")$length(), 1L)
})

##----------------------------------------------------------------------------##
## Spatial background image: createShinyApp production channel + demo wiring.
##----------------------------------------------------------------------------##

test_that("createShinyApp preserves legacy and nested spatial settings APIs", {
  args <- names(formals(createShinyApp))
  expect_true("spatial_images" %in% args)
  expect_true("spatial_image_settings" %in% args)
  expect_true("spatial_plot_rotation" %in% args)
  expect_false("..." %in% args)
  expect_true(all(
    c(
      "spatial_images_flip_x",
      "spatial_images_flip_y",
      "spatial_images_scale_x",
      "spatial_images_scale_y",
      "spatial_images_offset_x",
      "spatial_images_offset_y"
    ) %in%
      args
  ))
})

test_that("createShinyApp migrates legacy spatial settings into its bundle", {
  skip_if_not(file.exists(spatial_crb))
  img <- system.file(
    "extdata/examples/demo_spatial_visium_he.png",
    package = "CerebroNexus"
  )
  skip_if_not(file.exists(img))
  out_dir <- file.path(
    tempdir(),
    paste0("cerebro_spatial_legacy_", Sys.getpid())
  )
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  suppressWarnings(suppressMessages(
    createShinyApp(
      cerebro_data = c("Legacy spatial" = spatial_crb),
      result_dir = out_dir,
      spatial_images = c("Legacy spatial" = img),
      spatial_images_flip_x = c("Legacy spatial" = TRUE),
      spatial_images_flip_y = c("Legacy spatial" = FALSE),
      spatial_images_scale_x = c("Legacy spatial" = 0.9),
      spatial_images_scale_y = c("Legacy spatial" = 1.1),
      spatial_images_offset_x = c("Legacy spatial" = 12),
      spatial_images_offset_y = c("Legacy spatial" = -8),
      spatial_plot_rotation = c("Legacy spatial" = 37),
      launch_browser = FALSE,
      verbose = FALSE
    )
  ))

  spatial_name <- readCerebro(spatial_crb)$availableSpatial()[[1L]]
  cfg <- readRDS(file.path(out_dir, "cerebro_config.rds"))
  preset <- cfg[["spatial_image_settings"]][["Legacy spatial"]][[
    spatial_name
  ]][["Tissue background"]]
  expect_identical(preset[["flip_x"]], TRUE)
  expect_identical(preset[["flip_y"]], FALSE)
  expect_equal(preset[["scale_x"]], 0.9)
  expect_equal(preset[["scale_y"]], 1.1)
  expect_equal(preset[["offset_x"]], 12)
  expect_equal(preset[["offset_y"]], -8)
  expect_equal(
    cfg[["spatial_plot_rotation"]][["Legacy spatial"]][[spatial_name]],
    37
  )
})

test_that("createShinyApp bundles a spatial image and writes the option", {
  # End-to-end exercise of the new side-copy + option-write path: a matched
  # spatial image must be copied into the bundle and its stored path rewritten
  # to the spatial-assets/<file> form inside cerebro_config.rds.
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
  # path rewritten to the bundle-relative spatial asset directory
  spatial_name <- readCerebro(spatial_crb)$availableSpatial()[[1L]]
  stored <- cfg[["spatial_images"]][["Xenium demo"]][[spatial_name]][[
    "Tissue background"
  ]]
  expect_match(stored, "^spatial-assets/", perl = TRUE)
  # and the image really landed in the bundle
  expect_true(file.exists(file.path(out_dir, stored)))
})

test_that("createShinyApp rejects unmatched spatial_images", {
  skip_if_not(file.exists(spatial_crb))
  img <- tempfile(fileext = ".png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), img)
  out_dir <- file.path(
    tempdir(),
    paste0("cerebro_spatial_unmatched_", Sys.getpid())
  )
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  expect_error(
    suppressMessages(
      createShinyApp(
        cerebro_data = c("Xenium demo" = spatial_crb),
        result_dir = out_dir,
        spatial_images = c("no_such_dataset" = img),
        launch_browser = FALSE,
        verbose = FALSE
      )
    ),
    "dataset `no_such_dataset` is not present"
  )
  expect_false(dir.exists(out_dir))
})

test_that("Visium ships its H&E as an EXTERNAL image, not embedded", {
  # Visium deliberately demonstrates the external-image path: the H&E lives in a
  # standalone PNG loaded via `spatial_images`, and the .crb carries NO embedded
  # image (unlike MERFISH/Xenium). This keeps the .crb small and exercises the
  # spatial_images code path as a live example.
  png <- system.file(
    "extdata/examples/demo_spatial_visium_he.png",
    package = "CerebroNexus"
  )
  skip_if(png == "" || !file.exists(png), message = "visium H&E png missing")
  expect_gt(file.info(png)$size, 0)

  crb_path <- system.file(
    "extdata/examples/demo_spatial_visium.crb",
    package = "CerebroNexus"
  )
  skip_if(
    crb_path == "" || !file.exists(crb_path),
    message = "visium crb missing"
  )
  crb <- readCerebro(crb_path)
  spatial_name <- crb$availableSpatial()[1]
  sd <- .normalizeSpatialDataImages(
    crb$getSpatialData(spatial_name),
    spatial_name
  )
  expect_identical(sd$histology_images, list())

  configured <- viewer_demo_options()$spatial_images[["Mouse brain (Visium)"]][[
    spatial_name
  ]]
  expect_true(any(vapply(
    configured,
    function(path) basename(path) == "demo_spatial_visium_he.png",
    logical(1)
  )))

  renderer <- new.env(parent = globalenv())
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "spatial",
      "func_spatial_helpers.R"
    ),
    envir = renderer
  )
  configured_image <- "extdata/examples/demo_spatial_visium_he.png"
  expect_identical(
    renderer$authorized_spatial_image_path(
      configured_image,
      configured_image,
      system.file(package = "CerebroNexus")
    ),
    normalizePath(png, winslash = "/", mustWork = TRUE)
  )
})

test_that("renderer uses selected descriptor bounds without changing cell axes", {
  skip_if_not_installed("base64enc")
  root <- withr::local_tempdir()
  dir.create(file.path(root, "spatial-assets"))
  image_path <- file.path(root, "spatial-assets", "atlas.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), image_path)

  renderer <- new.env(parent = globalenv())
  renderer$Cerebro.options <- list(cerebro_root = root)
  renderer$viewerExpressionColorSettings <- function(...) list()
  renderer$expressionColorScale <- function(...) list()
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "utility_functions.R"
    ),
    envir = renderer
  )
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "gene_expression",
      "func_color_scale.R"
    ),
    envir = renderer
  )
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "spatial",
      "func_spatial_helpers.R"
    ),
    envir = renderer
  )
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "spatial",
      "func_projection_update_plot.R"
    ),
    envir = renderer
  )
  rendered <- NULL
  renderer$cerebroCellViewRender <- function(id, meta, data, ...) {
    rendered <<- c(list(meta = meta, data = data), list(...))
  }
  dataset_context <- list(
    epoch = "spatial-render-test",
    dataset_key = "atlas",
    generation = 1
  )

  params <- list(
    color_variable = "score",
    background_image = "external::Atlas",
    background_descriptor = list(
      source = "external",
      label = "Atlas",
      path = "spatial-assets/atlas.png",
      bounds = c(xmin = -10, xmax = 110, ymin = -20, ymax = 120)
    ),
    background_identity = list(
      dataset = "Atlas",
      spatial_name = "section",
      source = "external",
      label = "Atlas"
    ),
    background_image_allowlist = "spatial-assets/atlas.png",
    n_dimensions = 2,
    x_range = c(0, 100),
    y_range = c(10, 90),
    background_flip_x = FALSE,
    background_flip_y = FALSE,
    background_scale_x = 1,
    background_scale_y = 1,
    background_offset_x = 0,
    background_offset_y = 0,
    background_rotation = 37,
    background_opacity = 1,
    plot_type = "ImageFeaturePlot",
    point_size = 5,
    point_opacity = 1,
    draw_border = FALSE,
    hover_info = FALSE
  )
  call_renderer <- function(plot_parameters) {
    renderer$spatial_projection_update_plot(list(
      dataset_context = dataset_context,
      cells_df = data.frame(score = c(1, 2)),
      coordinates = data.frame(x = c(20, 80), y = c(30, 70)),
      reset_axes = FALSE,
      color_assignments = character(),
      group_hulls = list(),
      hover_columns = list(),
      plot_parameters = plot_parameters
    ))
  }

  call_renderer(params)
  expect_identical(rendered$dataset_context, dataset_context)
  expect_identical(
    unlist(rendered$meta$image_bounds[c("xmin", "xmax", "ymin", "ymax")]),
    c(xmin = -10, xmax = 110, ymin = -20, ymax = 120)
  )
  expect_identical(rendered$data$x_range, c(0, 100))
  expect_identical(rendered$data$y_range, c(10, 90))
  expect_identical(as.numeric(rendered$data$x), c(20, 80))
  expect_identical(as.numeric(rendered$data$y), c(30, 70))
  expect_identical(rendered$meta$background_rotation, 37)
  expect_identical(
    rendered$meta$background_identity,
    params$background_identity
  )

  params$background_descriptor$bounds <- NULL
  call_renderer(params)
  expect_identical(
    unlist(rendered$meta$image_bounds[c("xmin", "xmax", "ymin", "ymax")]),
    c(xmin = 20, xmax = 80, ymin = 30, ymax = 70)
  )
  expect_identical(rendered$data$x_range, c(0, 100))
  expect_identical(rendered$data$y_range, c(10, 90))

  params$background_image <- "none"
  params$background_descriptor <- NULL
  call_renderer(params)
  expect_null(rendered$meta$background_image)
  expect_identical(rendered$data$x_range, c(0, 100))
  expect_identical(rendered$data$y_range, c(10, 90))
  expect_identical(as.numeric(rendered$data$x), c(20, 80))
  expect_identical(as.numeric(rendered$data$y), c(30, 70))
})

test_that("external spatial images are streamed from session-private files", {
  renderer <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = renderer
  )
  image <- tempfile(fileext = ".png")
  withr::defer(unlink(image))
  bytes <- c(
    as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)),
    as.raw(1:12)
  )
  writeBin(bytes, image)
  callback <- NULL
  session <- new.env(parent = emptyenv())
  session$userData <- new.env(parent = emptyenv())
  session$registerDataObj <- function(name, data, handler) {
    callback <<- function() handler(data, NULL)
    paste0("session/", name)
  }

  first <- renderer$viewerPrivateImageUrl(image, session)
  second <- renderer$viewerPrivateImageUrl(image, session)
  response <- callback()

  expect_identical(first, second)
  expect_match(first, "^session/cerebro-image-")
  expect_identical(response$status, 200L)
  expect_identical(response$content_type, "image/png")
  expect_identical(
    response$content,
    list(
      file = normalizePath(image, winslash = "/", mustWork = TRUE),
      owned = FALSE
    )
  )
  expect_false(startsWith(first, "data:"))

  Sys.sleep(1.1)
  writeBin(c(bytes, as.raw(0)), image)
  changed <- callback()
  expect_identical(changed$status, 404L)
})

test_that("spatial first frames replace only exact flat geometry with an asset", {
  renderer <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = renderer)
  sys.source(
    viewer_test_path("spatial", "func_spatial_helpers.R"),
    envir = renderer
  )
  sys.source(
    viewer_test_path("spatial", "func_projection_update_plot.R"),
    envir = renderer
  )
  rendered <- NULL
  renderer$cerebroCellViewRender <- function(id, meta, data, ...) {
    rendered <<- list(id = id, meta = meta, data = data)
  }
  n <- 4096L
  resource <- list(
    protocol = "spatial-geometry-v1",
    url = "spatial.bin",
    cells = n,
    dimensions = 2L,
    dtype = "float32"
  )
  parameters <- list(
    color_variable = "group",
    background_descriptor = NULL,
    background_identity = NULL,
    background_image = "none",
    background_image_allowlist = character(),
    n_dimensions = 2L,
    x_range = c(0, n),
    y_range = c(0, n),
    background_flip_x = FALSE,
    background_flip_y = FALSE,
    background_scale_x = 1,
    background_scale_y = 1,
    background_offset_x = 0,
    background_offset_y = 0,
    background_rotation = 0,
    background_opacity = 1,
    plot_type = "ImageDimPlot",
    point_size = 1,
    point_opacity = 1,
    draw_border = FALSE,
    group_labels = TRUE,
    keep_square = FALSE,
    show_region_outlines = FALSE,
    hover_info = FALSE,
    projection = "slice"
  )
  value <- list(
    cells_df = data.frame(group = rep(c("A", "B"), length.out = n)),
    coordinates = data.frame(x = seq_len(n), y = -seq_len(n)),
    reset_axes = FALSE,
    color_assignments = c(A = "#111111", B = "#222222"),
    group_hulls = list(),
    geometry_resource = resource,
    plot_parameters = parameters
  )

  renderer$spatial_projection_update_plot(value)
  expect_identical(rendered$data$projection_resource, resource)
  expect_null(rendered$data[["x"]])
  expect_null(rendered$data[["y"]])
  expect_identical(length(rendered$data$canonical_group), n)

  value$geometry_resource$cells <- n - 1L
  renderer$spatial_projection_update_plot(value)
  expect_null(rendered$data$projection_resource)
  expect_identical(length(rendered$data[["x"]]), n)
  expect_identical(length(rendered$data[["y"]]), n)

  categories <- list(
    protocol = "canonical-metadata-codes-v1",
    url = "group.codes.bin",
    cells = n,
    dtype = "uint8",
    levels = c("A", "B"),
    code_map = c(-1L, 0L, 1L)
  )
  value$cells_df <- NULL
  value$coordinates <- NULL
  value$geometry_resource <- resource
  value$categorical_resource <- categories
  value$resource_first <- TRUE
  value$cell_count <- n
  value$cell_indices <- seq_len(n)
  renderer$spatial_projection_update_plot(value)
  expect_identical(rendered$data$projection_resource, resource)
  expect_identical(rendered$data$categorical_resource, categories)
  expect_null(rendered$data[["x"]])
  expect_null(rendered$data[["y"]])
  expect_null(rendered$data[["canonical_group"]])
})

test_that("Spatial first-frame data bypasses the legacy full accessor", {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  coordinates <- data.frame(x = 1:3, y = 4:6)
  stored <- list(
    coordinates = coordinates,
    histology_images = list(),
    molecules = structure(list(), class = "large-unrelated-payload")
  )
  dataset <- structure(list(spatial = list(slice = stored)), class = "Cerebro")
  runtime$data_set <- function() dataset
  runtime$getSpatialData <- function(name) {
    stop("legacy accessor should not run")
  }

  result <- runtime$viewerSpatialFirstFrameData("slice")
  expect_identical(result$coordinates, coordinates)
  expect_identical(result$histology_images, list())
  expect_false("molecules" %in% names(result))
})

test_that("Spatial geometry catalog is registered but fetched only on page request", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  sys.source(viewer_test_path("spatial", "func_spatial_helpers.R"), scope)
  scope$Cerebro.options <- list()
  scope$available_crb_files <- list(
    files = c(tiny = "tiny.crb"),
    selected = "tiny.crb"
  )
  scope$viewerPackCurrent <- function() {
    list(
      manifest = list(
        spatial_frames = data.frame(name = c("sliceA", "sliceB", "sliceA"))
      )
    )
  }
  scope$viewerPackSpatialIndex <- function(pack, name) {
    if (name == "sliceA") 1:4 else 5:8
  }
  requested <- list()
  scope$viewerSpatialGeometryAsset <- function(name, indices, rotation) {
    requested[[name]] <<- indices
    if (name == "sliceA") list(url = "sliceA.bin", cells = 4L) else NULL
  }
  viewer_server_load(
    viewer_server_statements("shiny_server.R", server = TRUE),
    scope,
    names = "viewerSpatialGeometryCatalog"
  )
  expect_equal(
    scope$viewerSpatialGeometryCatalog(),
    list(list(url = "sliceA.bin", cells = 4L))
  )
  expect_equal(requested, list(sliceA = 1:4, sliceB = 5:8))
  scope$viewerPackCurrent <- function() NULL
  expect_length(scope$viewerSpatialGeometryCatalog(), 0L)
})

test_that("empty spatial selection returns before million-cell plot data", {
  calls <- new.env(parent = emptyenv())
  calls$plot_data <- 0L
  server <- function(input, output, session) {
    dataset_context <- list(epoch = "test", dataset_key = "a", generation = 1)
    viewer_loaded_dataset_context <- function() dataset_context
    viewerDatasetContextEqual <- identical
    spatial_projection_data_to_plot <- reactive({
      calls$plot_data <- calls$plot_data + 1L
      list(
        cells_df = data.frame(cell_index = 1L),
        coordinates = data.frame(
          x = 1,
          y = 2
        )
      )
    })
    getMetaData <- function() data.frame(cell_barcode = "cell-1")
    viewerProjectionSubsetRows <- function(data, index, columns = NULL) data
    filterSelectionByHiddenGroups <- function(...) {
      stop(
        "hidden-group filtering should not run in this contract"
      )
    }
    viewer_server_load(
      viewer_server_statements("spatial", "obj_projection_selected_cells.R"),
      environment(),
      names = "spatial_projection_selected_cells"
    )
  }

  shiny::testServer(server, {
    expect_null(shiny::isolate(spatial_projection_selected_cells()))
    expect_identical(calls$plot_data, 0L)

    session$setInputs(
      spatial_projection_persistent_selection = list(
        x = 1,
        y = 2,
        ids = "cell-1",
        dataset_context = dataset_context
      )
    )
    selection <- shiny::isolate(spatial_projection_selected_cells())
    expect_identical(calls$plot_data, 1L)
    expect_identical(selection$selection_key, "cell-1")
    ## A complete stable ID needs no coordinate identifier.
    expect_null(selection$identifier)
  })
})

test_that("multi-spatial main UI preserves sliceB and uses its image choices", {
  main_ui <- viewer_test_path(
    "spatial",
    "UI_projection_main_parameters.R"
  )
  atlas <- list(
    sliceA = list(
      coordinates = data.frame(x = 1:2, y = 3:4),
      histology_images = list(
        `H&E` = list(histology_image = "data:image/png;base64,HE")
      )
    ),
    sliceB = list(
      coordinates = data.frame(x = 101:102, y = 203:204),
      histology_images = list(
        IF = list(histology_image = "data:image/png;base64,IF")
      )
    )
  )
  server <- function(input, output, session) {
    dataset_context <- list(
      epoch = "spatial-test-session",
      dataset_key = "Atlas",
      generation = 1
    )
    viewer_loaded_dataset_context <- function() dataset_context
    viewer_current_dataset_key <- function() dataset_context$dataset_key
    viewer_dataset_request <- function() {
      list(dataset_context = dataset_context)
    }
    data_set <- function() TRUE
    availableSpatial <- function() names(atlas)
    getSpatialData <- function(name) atlas[[name]]
    getMetaData <- function() data.frame(group = c("a", "b"))
    viewerProjectionFirstFrameMetadata <- getMetaData
    getGroups <- function() "group"
    serverSideGeneSelector <- function(...) invisible(NULL)
    Cerebro.options <- list(
      spatial_images = list(
        Atlas = list(
          sliceA = c(DAPI = "spatial-assets/Atlas/sliceA/dapi.png"),
          sliceB = c(MIBI = "spatial-assets/Atlas/sliceB/mibi.png")
        )
      )
    )
    available_crb_files <- list(
      files = c(Atlas = "atlas.crb"),
      selected = "atlas.crb"
    )
    sys.source(main_ui, envir = environment())
  }

  shiny::testServer(server, {
    session$setInputs(
      spatial_projection_to_display = "sliceB",
      spatial_projection_more_render_request = 1
    )
    session$flushReact()
    main_html <- as.character(
      output$spatial_projection_scene_UI$html
    )
    background_html <- as.character(
      output$spatial_projection_background_selector_UI$html
    )
    expect_match(main_html, 'value="sliceB" selected', fixed = TRUE)
    expect_match(background_html, "embedded::IF", fixed = TRUE)
    expect_match(background_html, "external::MIBI", fixed = TRUE)
    expect_false(grepl("embedded::H&amp;E", background_html, fixed = TRUE))
    expect_false(grepl("external::DAPI", background_html, fixed = TRUE))
    expect_identical(getSpatialData("sliceB")$coordinates$x, 101:102)

    ## Switching FOV updates only the scene selectors; the plot type and gene
    ## pickers stay mounted.
    controls_html <- as.character(
      output$spatial_projection_main_parameters_UI$html
    )
    session$setInputs(spatial_projection_plot_type = "ImageFeaturePlot")
    session$setInputs(spatial_projection_to_display = "sliceA")
    session$flushReact()
    expect_identical(
      as.character(output$spatial_projection_main_parameters_UI$html),
      controls_html
    )
    expect_match(
      as.character(output$spatial_projection_scene_UI$html),
      'value="sliceA" selected',
      fixed = TRUE
    )
  })
})

test_that("spatial gene selectors initialize only for their visible plot type", {
  main_ui <- viewer_test_path(
    "spatial",
    "UI_projection_main_parameters.R"
  )
  selectors <- new.env(parent = emptyenv())
  server <- function(input, output, session) {
    data_set <- function() TRUE
    availableSpatial <- function() "sliceA"
    viewerProjectionFirstFrameMetadata <- function() data.frame(group = "a")
    getGroups <- function() "group"
    serverSideGeneSelector <- function(
      session,
      input_id,
      extra_triggers,
      active,
      ...
    ) {
      selectors[[input_id]] <- active
      invisible(NULL)
    }
    sys.source(main_ui, envir = environment())
  }

  shiny::testServer(server, {
    session$setInputs(
      sidebar = "spatial",
      spatial_projection_plot_type = "ImageDimPlot"
    )
    session$flushReact()
    expect_false(shiny::isolate(
      selectors[["spatial_projection_feature_to_display"]]()
    ))
    for (channel in c("r", "g", "b")) {
      expect_false(shiny::isolate(
        selectors[[paste0("spatial_projection_coexpr_", channel)]]()
      ))
    }

    session$setInputs(spatial_projection_plot_type = "ImageFeaturePlot")
    expect_true(shiny::isolate(
      selectors[["spatial_projection_feature_to_display"]]()
    ))
    for (channel in c("r", "g", "b")) {
      expect_false(shiny::isolate(
        selectors[[paste0("spatial_projection_coexpr_", channel)]]()
      ))
    }

    session$setInputs(spatial_projection_plot_type = "Co-expression (RGB)")
    expect_false(shiny::isolate(
      selectors[["spatial_projection_feature_to_display"]]()
    ))
    for (channel in c("r", "g", "b")) {
      expect_true(shiny::isolate(
        selectors[[paste0("spatial_projection_coexpr_", channel)]]()
      ))
    }
  })
})

test_that("bundled real demos embed a genuine tissue image in the .crb", {
  # MERFISH carries its real DAPI inside the CRB. Visium and Xenium use external
  # files; Slide-seq carries no image.
  for (f in "demo_spatial_merfish") {
    path <- system.file(
      file.path("extdata/examples", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readCerebro(path)
    spatial_name <- crb$availableSpatial()[1]
    sd <- .normalizeSpatialDataImages(
      crb$getSpatialData(spatial_name),
      spatial_name
    )
    expect_named(sd$histology_images, "Tissue background", info = f)
    image <- sd$histology_images[["Tissue background"]]
    expect_match(image$histology_image, "^data:image/", info = f)
    b <- image$histology_image_bounds
    expect_true(
      all(c("xmin", "xmax", "ymin", "ymax") %in% names(b)),
      info = f
    )
    # cells must fall inside the image's coordinate-space extent
    coords <- sd$coordinates
    expect_true(
      min(coords$x) >= b[["xmin"]] &&
        max(coords$x) <= b[["xmax"]] &&
        min(coords$y) >= b[["ymin"]] &&
        max(coords$y) <= b[["ymax"]],
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
  visium = "extdata/examples/demo_spatial_visium.crb",
  slideseq = "extdata/examples/demo_spatial_slideseq.crb",
  merfish = "extdata/examples/demo_spatial_merfish.crb",
  xenium = "extdata/examples/demo_spatial_xenium.crb"
)

test_that("each real spatial demo exposes coordinates with x/y", {
  for (nm in names(real_spatial_demos)) {
    path <- system.file(real_spatial_demos[[nm]], package = "CerebroNexus")
    skip_if(
      path == "" || !file.exists(path),
      message = paste0(nm, " demo missing")
    )
    crb <- readCerebro(path)
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
  files <- viewer_demo_options()$crb_file_to_load
  expect_true(all(basename(real_spatial_demos) %in% basename(files)))
  for (technology in c("Visium", "Slide-seq", "MERFISH", "Xenium")) {
    expect_true(any(grepl(technology, names(files), fixed = TRUE)))
  }
})

test_that("image-free demo (Slide-seq) carries no histology image", {
  # This platform records positions, not a tissue photo, so a genuine
  # an empty `histology_images` manifest is the correct state.
  for (f in c("demo_spatial_slideseq")) {
    path <- system.file(
      file.path("extdata/examples", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readCerebro(path)
    spatial_name <- crb$availableSpatial()[1]
    sd <- .normalizeSpatialDataImages(
      crb$getSpatialData(spatial_name),
      spatial_name
    )
    expect_identical(sd$histology_images, list(), info = f)
  }
})

test_that("embedded image demos store the image natively with no flip flag", {
  # Embedded images are stored in their native orientation; there is no per-.crb
  # render-flip flag (removed — display alignment is a user control in the tab).
  # Guard that the image is present and no stale flip flag lingers.
  for (f in "demo_spatial_merfish") {
    path <- system.file(
      file.path("extdata/examples", paste0(f, ".crb")),
      package = "CerebroNexus"
    )
    skip_if(path == "" || !file.exists(path), message = paste0(f, " missing"))
    crb <- readCerebro(path)
    spatial_name <- crb$availableSpatial()[1]
    sd <- .normalizeSpatialDataImages(
      crb$getSpatialData(spatial_name),
      spatial_name
    )
    expect_named(sd$histology_images, "Tissue background", info = f)
    expect_null(sd$histology_image_flip_y, info = f)
  }
})

test_that("app.R ships the Visium H&E overlay pre-aligned", {
  options <- viewer_demo_options()
  preset <- spatialImagePreset(
    options,
    "Mouse brain (Visium)",
    "anterior1",
    "Tissue background"
  )
  expect_identical(preset$offsetX, 600)
  expect_identical(preset$scaleX, 1.55)
  expect_true(preset$flipY)
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
  obj <- suppressWarnings(subset(obj, cells = sample(colnames(obj), 200)))

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

test_that("standalone Viewer utilities provide their null fallback operator", {
  expressions <- parse(file.path(shiny_root, "utility_functions.R"))
  assignment <- Filter(
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name("%||%"))
    },
    as.list(expressions)
  )
  expect_length(assignment, 1L)
  runtime <- new.env(parent = baseenv())
  eval(assignment[[1L]], runtime)
  expect_identical(runtime$`%||%`(NULL, "fallback"), "fallback")
  expect_identical(runtime$`%||%`("value", "fallback"), "value")
})

test_that("cell boundary payloads are validated and capped by complete cells", {
  boundaries <- data.frame(
    cell_barcode = rep(c("cell-1", "cell-2"), each = 4L),
    x = c(0, 1, 1, 0, 2, 3, 3, 2),
    y = c(0, 0, 1, 1, 2, 2, 3, 3),
    part = "1",
    stringsAsFactors = FALSE
  )

  payload <- spatial_cell_boundaries(boundaries, c("cell-1", "cell-2"), 4L)
  expect_named(payload, c("cell_barcode", "part", "x", "y"))
  expect_identical(unique(payload$cell_barcode), "cell-1")
  expect_length(payload$x, 4L)
  expect_identical(spatial_cell_boundaries(list(), "cell-1"), list())
  expect_identical(
    spatial_cell_boundaries(boundaries, "cell-1", Inf),
    list()
  )
})

test_that("spatial hierarchy resolves sample and ROI values by barcode", {
  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    sample = c("S1", "S1", "S2"),
    roi_id = c("R1", "R2", "R3"),
    stringsAsFactors = FALSE
  )

  sample <- spatial_metadata_facet(metadata, c("c3", "c1"), "sample")
  roi <- spatial_metadata_facet(
    metadata,
    c("c3", "c1"),
    c("roi", "roi_id")
  )

  expect_identical(sample$field, "sample")
  expect_identical(sample$by_cell, c(c3 = "S2", c1 = "S1"))
  expect_identical(roi$values, c("R3", "R1"))
})

test_that("ROI rotation pivots use the selected sample and ROI field", {
  metadata <- data.frame(
    cell_barcode = c("a1", "a2", "b1", "b2"),
    sample = c("A", "A", "B", "B"),
    cell_type = c("T", "B", "T", "B"),
    sample_roi = "lesion",
    stringsAsFactors = FALSE
  )
  coordinates <- data.frame(
    x = c(0, 2, 100, 102),
    y = 0,
    row.names = metadata$cell_barcode
  )

  context <- spatial_roi_transform_context(
    coordinates,
    metadata,
    sample_value = "A"
  )

  expect_identical(context$scoped_cells, c("a1", "a2"))
  expect_identical(
    context$roi_by_cell,
    stats::setNames(
      rep("lesion", 4L),
      metadata$cell_barcode
    )
  )
  expect_equal(context$pivots$lesion, c(x = 1, y = 0))
})

test_that("spatial sampling is scoped to the selected FOV, sample, and ROI", {
  cells_file <- file.path(
    system.file("viewer", package = "CerebroNexus"),
    "spatial",
    "obj_projection_cells_to_show.R"
  )
  metadata <- data.frame(
    cell_barcode = paste0("c", 1:5),
    sample = c("S1", "S1", "S1", "S2", "S2"),
    roi_id = c("R1", "R1", "R2", "R1", "R1"),
    cluster = "A",
    stringsAsFactors = FALSE
  )
  rownames(metadata) <- metadata$cell_barcode
  atlas <- list(
    fov1 = list(
      coordinates = data.frame(
        x = 1:3,
        y = 4:6,
        row.names = paste0("c", 1:3)
      )
    ),
    fov2 = list(
      coordinates = data.frame(
        x = 7:8,
        y = 9:10,
        row.names = paste0("c", 4:5)
      )
    )
  )
  server <- function(input, output, session) {
    getGroups <- function() character()
    getMetaData <- function() metadata
    getSpatialData <- function(name) atlas[[name]]
    availableSpatial <- function() names(atlas)
    viewerSpatialCoordinates <- function(name) atlas[[name]]$coordinates
    viewerPackCurrent <- function() NULL
    viewerPackSpatialIndex <- function(...) NULL
    viewerProjectionFirstFrameMetadata <- function() metadata
    spatial_projection_selected_sample <- function() {
      input$spatial_projection_sample %||% ""
    }
    spatial_projection_controls <- reactive(list(
      projection = input$spatial_projection_to_display,
      percentage_cells_to_show = input$spatial_projection_percentage_cells_to_show
    ))
    spatial_projection_parameters_plot <- reactive(list(
      projection = input$spatial_projection_to_display,
      sample = input$spatial_projection_sample,
      roi = input$spatial_projection_roi
    ))
    sys.source(cells_file, envir = environment())
  }

  shiny::testServer(server, {
    session$setInputs(
      spatial_projection_percentage_cells_to_show = 100,
      spatial_projection_to_display = "fov1",
      spatial_projection_sample = "S1",
      spatial_projection_roi = "R1"
    )
    expect_setequal(spatial_projection_cells_to_show(), 1:2)

    # Dynamic controls disappear when the next data set has no matching sample
    # or ROI field, but Shiny can retain their previous input values. Those
    # stale values must not filter every cell out of the new spatial scene.
    session$setInputs(
      spatial_projection_to_display = "fov2",
      spatial_projection_sample = "S1",
      spatial_projection_roi = "missing"
    )
    expect_setequal(spatial_projection_cells_to_show(), 4:5)
  })
})

test_that("molecule overlays accept one gene and cap browser points", {
  molecules <- list(
    data = data.frame(
      gene = c("Gene1", "Gene2", "Gene1", "Gene1"),
      x = c(1, 2, 3, 4),
      y = c(5, 6, 7, 8),
      stringsAsFactors = FALSE
    ),
    genes = c("Gene1", "Gene2"),
    total_count = 4L,
    exported_count = 4L,
    truncated = FALSE
  )

  overlay <- spatial_molecule_overlay(molecules, "Gene1", 2L)
  expect_named(overlay, c("x", "y"))
  expect_length(overlay$x, 2L)
  expect_identical(spatial_molecule_overlay(molecules, "missing"), list())
  expect_identical(
    spatial_molecule_overlay(molecules, "Gene1", scoped = TRUE),
    list()
  )

  ## Molecules without a gene label are dropped, not returned as NA rows.
  molecules$data$gene[[2L]] <- NA_character_
  overlay <- spatial_molecule_overlay(molecules, "Gene1")
  expect_identical(overlay$x, c(1, 3, 4))
})

test_that("ROI pivots use column centres for matrix and data frame input", {
  coordinates <- cbind(x = c(0, 10, 100, 120), y = c(0, 20, 200, 240))
  rois <- c("A", "A", "B", "B")
  expected <- list(A = c(x = 5, y = 10), B = c(x = 110, y = 220))
  expect_identical(spatialRoiPivots(coordinates, rois), expected)
  expect_identical(
    spatialRoiPivots(as.data.frame(coordinates), rois),
    expected
  )
})
