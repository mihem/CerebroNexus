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
  # A malformed entry (missing coordinates/expression) must be rejected so the
  # class contract getSpatialData() relies on cannot be violated silently.
  cls_text <- paste(
    deparse(Cerebro$public_methods$addSpatialData),
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
  expressions <- parse(viewer_test_path("utility_functions.R"))
  assignment <- Filter(
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name("viewerSpatialCoordinates"))
    },
    as.list(expressions)
  )[[1L]]
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

test_that("background-image selection only recreates image calibration controls", {
  # The scatter controls retain user-selected values while moving between
  # backgrounds. Keep them in their own renderUI so that the selected image
  # can safely seed the calibration controls without recreating the point-size,
  # point-opacity, or sampling sliders.
  ui_file <- file.path(
    shiny_root,
    "spatial",
    "UI_projection_additional_parameters.R"
  )
  src <- paste(readLines(ui_file), collapse = "\n")

  expect_match(
    src,
    'output\\[\\["spatial_projection_scatter_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI',
    perl = TRUE
  )
  expect_match(
    src,
    'output\\[\\["spatial_projection_background_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI',
    perl = TRUE
  )
  expect_match(
    src,
    'input\\[\\["spatial_projection_more_render_request"\\]\\]',
    perl = TRUE
  )

  scatter_src <- sub(
    '^[\\s\\S]*?output\\[\\["spatial_projection_scatter_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI\\(\\{',
    "",
    src,
    perl = TRUE
  )
  scatter_src <- sub(
    'output\\[\\["spatial_projection_background_parameters_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI[\\s\\S]*$',
    "",
    scatter_src,
    perl = TRUE
  )
  expect_false(
    grepl(
      'input\\[\\["spatial_projection_background_image"\\]\\]',
      scatter_src,
      fixed = FALSE
    )
  )

  projection_ui <- paste(
    readLines(file.path(shiny_root, "spatial", "UI.R")),
    collapse = "\n"
  )
  expect_match(
    projection_ui,
    'uiOutput\\("spatial_projection_scatter_parameters_UI"\\)'
  )
  expect_no_match(
    projection_ui,
    'uiOutput\\("spatial_projection_background_select_UI"\\)'
  )
  main_parameters_ui <- paste(
    readLines(file.path(
      shiny_root,
      "spatial",
      "UI_projection_main_parameters.R"
    )),
    collapse = "\n"
  )
  expect_match(
    main_parameters_ui,
    'output\\[\\["spatial_projection_background_selector_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI',
    perl = TRUE
  )
  selector_source <- sub(
    '^[\\s\\S]*?output\\[\\["spatial_projection_background_selector_UI"\\]\\][[:space:]]*<-[[:space:]]*renderUI\\(\\{',
    "",
    main_parameters_ui,
    perl = TRUE
  )
  selector_source <- sub(
    "serverSideGeneSelector[\\s\\S]*$",
    "",
    selector_source,
    perl = TRUE
  )
  selector_request <- regexpr(
    'identical(input[["sidebar"]], "spatial")',
    selector_source,
    fixed = TRUE
  )[[1]]
  selector_spatial_read <- regexpr(
    "getSpatialData(current_spatial)",
    selector_source,
    fixed = TRUE
  )[[1]]
  ## Sidebar navigation must not reconstruct a mounted selector. Shiny
  ## suspends this output while hidden instead.
  expect_identical(selector_request, -1L)
  expect_gt(selector_spatial_read, 0)
  expect_match(main_parameters_ui, "suspendWhenHidden = TRUE", fixed = TRUE)
  expect_match(
    projection_ui,
    'uiOutput\\("spatial_projection_background_selector_UI"\\)'
  )
  expect_match(
    projection_ui,
    'uiOutput\\("spatial_projection_background_parameters_UI"\\)'
  )
  expect_match(
    projection_ui,
    'class = "spatial-image-controls"',
    fixed = TRUE
  )
  additional_ui <- paste(
    readLines(
      file.path(shiny_root, "spatial", "UI_projection_additional_parameters.R")
    ),
    collapse = "\n"
  )
  for (class_name in c(
    "cerebro-settings-content",
    "cerebro-settings-full",
    "spatial-image-offset-row",
    "spatial-image-checks",
    "spatial-image-actions"
  )) {
    expect_match(additional_ui, class_name, fixed = TRUE)
  }
  expect_no_match(additional_ui, "cerebroSettingsSection(", fixed = TRUE)
  expect_match(additional_ui, '"Opacity"', fixed = TRUE)
  expect_no_match(additional_ui, '"Image opacity"', fixed = TRUE)
  expect_match(additional_ui, 'paste0(id, "_num")', fixed = TRUE)
  expect_match(
    additional_ui,
    'slider_number_input(\n        "spatial_projection_background_opacity"',
    fixed = TRUE
  )
  for (obsolete_class in c(
    "spatial-image-control-group",
    "spatial-image-transform-group",
    "spatial-image-scale-pair",
    "spatial-image-flips"
  )) {
    expect_no_match(additional_ui, obsolete_class, fixed = TRUE)
  }
  settings_css <- paste(
    readLines(file.path(shiny_root, "www", "custom.css")),
    collapse = "\n"
  )
  expect_match(settings_css, ".spatial-image-offset-row {", fixed = TRUE)
  expect_match(
    settings_css,
    ".spatial-image-offset-slider .irs-grid",
    fixed = TRUE
  )
  expect_match(settings_css, "grid-template-columns: 1fr;", fixed = TRUE)
  expect_match(settings_css, "margin-top: 12px;", fixed = TRUE)
  expect_match(
    settings_css,
    ".cerebro-settings-section {",
    fixed = TRUE
  )
  expect_no_match(
    settings_css,
    ".spatial-image-controls .shiny-panel-conditional {\n  display: grid;",
    fixed = TRUE
  )
  expect_match(projection_ui, '"Appearance"', fixed = TRUE)
  expect_match(projection_ui, '"Background image"', fixed = TRUE)
})


test_that("ImageFeaturePlot reaches getExpressionMatrix as a Cerebro method", {
  # getExpressionMatrix / getMeanExpressionForCells are Cerebro R6 methods,
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

test_that("hidden Moran's I exits before reading Spatial data", {
  morans_source <- paste(
    readLines(file.path(shiny_root, "spatial", "out_morans_i.R"), warn = FALSE),
    collapse = "\n"
  )
  projection_ui <- paste(
    readLines(file.path(shiny_root, "spatial", "UI.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    projection_ui,
    "input.spatial_projection_plot_type == 'ImageFeaturePlot'",
    fixed = TRUE
  )
  expect_match(
    projection_ui,
    'textOutput("spatial_projection_morans_i", inline = TRUE)',
    fixed = TRUE
  )
  early_guard <- regexpr(
    'req(identical(input[["spatial_projection_plot_type"]], "ImageFeaturePlot"))',
    morans_source,
    fixed = TRUE
  )[[1]]
  parameter_read <- regexpr(
    "plot_parameters <- spatial_projection_parameters_plot()",
    morans_source,
    fixed = TRUE
  )[[1]]
  spatial_read <- regexpr(
    "spatial_data <- getSpatialData(",
    morans_source,
    fixed = TRUE
  )[[1]]

  expect_gt(early_guard, 0)
  expect_gt(parameter_read, early_guard)
  expect_gt(spatial_read, parameter_read)
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
  expect_match(ui_src, 'conditionalSidebarItem\\("Spatial", "spatial"')

  server_src <- paste(
    readLines(file.path(shiny_root, "shiny_server.R")),
    collapse = "\n"
  )
  expect_match(server_src, "spatial/server\\.R")
  expect_match(server_src, "group_filters/group_filters_widget\\.R")
  expect_match(
    server_src,
    'toggleConditionalTab\\([\\s\\S]{0,80}"spatial"',
    perl = TRUE
  )
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

test_that("Spatial UI seeds and resets every field from the shared image preset", {
  ui <- paste(
    readLines(
      file.path(shiny_root, "spatial", "UI_projection_additional_parameters.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  controls <- paste(
    readLines(
      file.path(shiny_root, "spatial", "obj_projection_background_controls.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    ui,
    "spatial_projection_background_opacity[\\s\\S]{0,200}preset\\$opacity",
    perl = TRUE
  )
  expect_match(
    controls,
    "spatial_projection_background_opacity[\\s\\S]{0,120}value = preset\\$opacity",
    perl = TRUE
  )
  for (numeric_id in c(
    "spatial_projection_background_opacity_num",
    "spatial_projection_background_scale_num",
    "spatial_projection_background_scale_x_num",
    "spatial_projection_background_scale_y_num",
    "spatial_projection_background_rotate_num"
  )) {
    expect_match(controls, numeric_id, fixed = TRUE)
  }
  expect_match(controls, '"spatial_copy_preset"', fixed = TRUE)
  page_js <- paste(
    readLines(file.path(shiny_root, "spatial", "js_page_helpers.js")),
    collapse = "\n"
  )
  expect_match(page_js, "navigator.clipboard.writeText", fixed = TRUE)
  expect_match(page_js, "document.execCommand('copy')", fixed = TRUE)
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

  # app.R must wire the external image via spatial_images for the Visium dataset
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(app_src, "spatial_images", fixed = TRUE)
  expect_match(app_src, "demo_spatial_visium_he\\.png", perl = TRUE)

  renderer <- new.env(parent = globalenv())
  sys.source(
    file.path(
      system.file("viewer", package = "CerebroNexus"),
      "spatial",
      "func_projection_update_plot.R"
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

test_that("external spatial image encoding is cached by file version", {
  renderer <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("spatial", "func_projection_update_plot.R"),
    envir = renderer
  )
  image <- tempfile(fileext = ".png")
  withr::defer(unlink(image))
  writeBin(as.raw(1:4), image)
  calls <- 0L
  encode <- function(path) {
    calls <<- calls + 1L
    paste0("encoded-", file.info(path)$size)
  }

  first <- renderer$spatialBackgroundDataUri(image, encode)
  second <- renderer$spatialBackgroundDataUri(image, encode)
  writeBin(as.raw(1:5), image)
  third <- renderer$spatialBackgroundDataUri(image, encode)

  expect_identical(first, second)
  expect_false(identical(second, third))
  expect_identical(calls, 2L)
})

test_that("spatial hull geometry is prepared outside the renderer", {
  data_flow <- paste(
    readLines(viewer_test_path("spatial", "obj_projection_data_to_plot.R")),
    collapse = "\n"
  )
  renderer <- paste(
    readLines(viewer_test_path("spatial", "func_projection_update_plot.R")),
    collapse = "\n"
  )

  expect_match(
    data_flow,
    "spatial_projection_group_hulls <- reactive({",
    fixed = TRUE
  )
  expect_match(
    data_flow,
    "group_hulls = if (resource_first) list() else spatial_projection_group_hulls()",
    fixed = TRUE
  )
  expect_no_match(renderer, "compute_group_hulls(", fixed = TRUE)
})

test_that("hidden Spatial appearance controls do not rebuild primary data", {
  parameters <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_parameters_plot.R"
    )),
    collapse = "\n"
  )
  data_flow <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_data_to_plot.R"
    )),
    collapse = "\n"
  )
  controls <- paste(
    unlist(lapply(
      c(
        "UI_projection_point_border.R",
        "UI_projection_show_group_label.R"
      ),
      function(file) {
        readLines(viewer_test_path("spatial", file), warn = FALSE)
      }
    )),
    collapse = "\n"
  )
  projection_ui <- paste(
    readLines(viewer_test_path("spatial", "UI.R")),
    collapse = "\n"
  )

  expect_match(
    parameters,
    "spatial_projection_appearance <- reactiveValues(",
    fixed = TRUE
  )
  expect_match(parameters, '"cell_view_appearance"', fixed = TRUE)
  expect_match(
    parameters,
    "draw_border = isolate(spatial_projection_appearance$draw_border)",
    fixed = TRUE
  )
  expect_match(
    parameters,
    "group_labels = isolate(spatial_projection_appearance$group_labels)",
    fixed = TRUE
  )
  expect_match(
    parameters,
    "keep_square = isolate(spatial_projection_appearance$keep_square)",
    fixed = TRUE
  )
  expect_match(
    parameters,
    "show_region_outlines = spatial_projection_region_outlines()",
    fixed = TRUE
  )
  expect_match(
    data_flow,
    "!isTRUE(spatial_projection_region_outlines())",
    fixed = TRUE
  )
  expect_match(
    controls,
    'req(input[["spatial_projection_more_render_request"]])',
    fixed = TRUE
  )
  expect_match(
    projection_ui,
    'uiOutput("spatial_projection_keep_square_UI")',
    fixed = TRUE
  )
})

test_that("first Spatial frame uses deduplicated server-side control defaults", {
  parameters <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_parameters_plot.R"
    )),
    collapse = "\n"
  )
  cells <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_cells_to_show.R"
    )),
    collapse = "\n"
  )

  expect_match(
    parameters,
    "spatial_projection_controls <- reactiveVal(NULL)",
    fixed = TRUE
  )
  expect_match(
    parameters,
    'req(input[["spatial_projection_render_request"]])',
    fixed = TRUE
  )
  expect_match(
    parameters,
    "if (!identical(isolate(spatial_projection_controls()), controls))",
    fixed = TRUE
  )
  expect_match(parameters, "}, priority = 1000)", fixed = TRUE)
  expect_match(
    parameters,
    "controls <- spatial_projection_controls()",
    fixed = TRUE
  )
  expect_match(
    cells,
    "percentage <- controls$percentage_cells_to_show",
    fixed = TRUE
  )
})

test_that("shared Canvas owns spatial background identity and appearance", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js")),
    collapse = "\n"
  )
  controls <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_background_controls.R"
    )),
    collapse = "\n"
  )
  background_ui <- paste(
    readLines(viewer_test_path(
      "spatial",
      "UI_projection_additional_parameters.R"
    )),
    collapse = "\n"
  )
  page_helpers <- paste(
    readLines(viewer_test_path("spatial", "js_page_helpers.js")),
    collapse = "\n"
  )
  parameters <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_parameters_plot.R"
    )),
    collapse = "\n"
  )

  expect_match(engine, "JSON.stringify(meta.background_identity)", fixed = TRUE)
  expect_match(
    engine,
    "rotation: Number(meta.background_rotation) || 0",
    fixed = TRUE
  )
  expect_match(engine, "function updateSingleBackground", fixed = TRUE)
  expect_match(engine, "stashImgState(space)", fixed = TRUE)
  expect_match(controls, "cerebroCellViewBackground(", fixed = TRUE)
  expect_match(
    controls,
    "dataset_context = dataset_context",
    fixed = TRUE
  )
  expect_match(
    controls,
    "background_identity = background_identity",
    fixed = TRUE
  )
  expect_match(
    controls,
    'input[["spatial_projection_background_controls"]]',
    fixed = TRUE
  )
  expect_match(
    controls,
    "spatial_background_identity_equal(",
    fixed = TRUE
  )
  expect_match(
    background_ui,
    'id = "spatial_projection_background_control_scope"',
    fixed = TRUE
  )
  expect_match(background_ui, "data-background-identity", fixed = TRUE)
  expect_match(
    page_helpers,
    "spatial_projection_background_controls",
    fixed = TRUE
  )
  expect_match(
    page_helpers,
    "dataset_context: datasetContext",
    fixed = TRUE
  )
  expect_match(
    page_helpers,
    "background_identity: backgroundIdentity",
    fixed = TRUE
  )
  expect_match(
    parameters,
    "background_opacity <- background_preset$opacity",
    fixed = TRUE
  )
  expect_no_match(
    parameters,
    'isolate(\n    input[["spatial_projection_background_opacity"]]',
    fixed = TRUE
  )
  expect_match(controls, "dataset_context = dataset_context", fixed = TRUE)
  expect_match(
    controls,
    "background_identity = background_identity",
    fixed = TRUE
  )
  expect_match(page_helpers, "stillCurrent(message)", fixed = TRUE)

  renderer_src <- paste(
    readLines(viewer_test_path("spatial", "func_projection_update_plot.R")),
    collapse = "\n"
  )
  assignments <- gregexpr(
    "background_identity = plot_parameters",
    renderer_src,
    fixed = TRUE
  )[[1L]]
  expect_length(assignments[assignments > 0L], 1L)
  expect_match(
    renderer_src,
    'payload[["meta"]] <- c(background_meta, payload[["meta"]])',
    fixed = TRUE
  )
})

test_that("spatial first frames replace only exact flat geometry with an asset", {
  renderer <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = renderer)
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

test_that("bounded Spatial backgrounds remain eligible for resource first frames", {
  data_flow <- paste(
    readLines(viewer_test_path("spatial", "obj_projection_data_to_plot.R")),
    collapse = "\n"
  )
  expect_match(
    data_flow,
    "background_geometry_ready <- is.null(background_descriptor)",
    fixed = TRUE
  )
  expect_match(
    data_flow,
    "is.atomic(background_bounds) || is.list(background_bounds)",
    fixed = TRUE
  )
})

test_that("Spatial geometry catalog is registered but fetched only on page request", {
  server_source <- paste(
    readLines(viewer_test_path("shiny_server.R")),
    collapse = "\n"
  )
  expect_match(
    server_source,
    "viewerSpatialGeometryCatalog <- function()",
    fixed = TRUE
  )
  expect_match(server_source, '"cell_view_resource_catalog"', fixed = TRUE)
  expect_match(
    server_source,
    "viewerPackSpatialIndex(pack, name)",
    fixed = TRUE
  )
  browser_source <- paste(
    readLines(viewer_test_path("www", "cell_views.js")),
    collapse = "\n"
  )
  click_prefetch <- regexpr(
    "prefetchRegisteredSingleResource(id);",
    browser_source,
    fixed = TRUE
  )[[1L]]
  render_request <- regexpr(
    "Shiny.setInputValue(id + '_render_request'",
    browser_source,
    fixed = TRUE
  )[[1L]]
  expect_gt(click_prefetch, 0L)
  expect_gt(render_request, click_prefetch)
  expect_match(
    browser_source,
    "Number(percentage.value) !== 100",
    fixed = TRUE
  )
  expect_match(
    browser_source,
    "if (!boxes[j].checked) return false;",
    fixed = TRUE
  )
  expect_match(
    server_source,
    "default_resource_name = default_resource_name",
    fixed = TRUE
  )
  spatial_catalog <- regexpr(
    'id = "spatial_projection"',
    server_source,
    fixed = TRUE
  )[[1L]]
  overview_catalog <- regexpr(
    'id = "overview_projection"',
    server_source,
    fixed = TRUE
  )[[1L]]
  expect_gt(spatial_catalog, 0L)
  expect_gt(overview_catalog, spatial_catalog)
  expect_match(
    browser_source,
    "return singleDefaultResourceNames.has(id);",
    fixed = TRUE
  )
  expect_match(
    browser_source,
    "if (!name) name = singleDefaultResourceNames.get(id);",
    fixed = TRUE
  )
  expect_match(
    browser_source,
    "if (singleRequests.has(singleRequestKey(message.id, message.dataset_context))) {",
    fixed = TRUE
  )
  expect_false(grepl(
    "cell_view_resource_prefetch",
    browser_source,
    fixed = TRUE
  ))
})

test_that("empty spatial selection returns before million-cell plot data", {
  selected_source <- paste(
    readLines(viewer_test_path(
      "spatial",
      "obj_projection_selected_cells.R"
    )),
    collapse = "\n"
  )
  selection_read <- regexpr(
    'selection <- viewerPersistentSelection(',
    selected_source,
    fixed = TRUE
  )[[1L]]
  empty_return <- regexpr(
    'if (is.null(selection)) return(NULL)',
    selected_source,
    fixed = TRUE
  )[[1L]]
  plot_data_read <- regexpr(
    "req(spatial_projection_data_to_plot())",
    selected_source,
    fixed = TRUE
  )[[1L]]
  expect_gt(selection_read, 0L)
  expect_gt(empty_return, selection_read)
  expect_gt(plot_data_read, empty_return)

  summary_source <- paste(
    readLines(viewer_test_path(
      "spatial",
      "out_number_of_selected_cells.R"
    )),
    collapse = "\n"
  )
  expect_gte(
    lengths(regmatches(
      summary_source,
      gregexpr(
        "req(!is.null(selection), nrow(selection) > 0L)",
        summary_source,
        fixed = TRUE
      )
    )),
    2L
  )

  calls <- new.env(parent = emptyenv())
  calls$plot_data <- 0L
  selected_lines <- readLines(viewer_test_path(
    "spatial",
    "obj_projection_selected_cells.R"
  ))
  observer_line <- grep("^observeEvent\\(", selected_lines)[[1L]]
  selected_reactive_source <- paste(
    selected_lines[seq_len(observer_line - 1L)],
    collapse = "\n"
  )
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
    eval(parse(text = selected_reactive_source), envir = environment())
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
    expect_identical(selection$identifier, "1-2")
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
      output$spatial_projection_main_parameters_UI$html
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
  # The bundled Visium demo opens with its H&E overlay already aligned to the
  # points, so users see a correct overlay without nudging it. app.R therefore
  # sets the per-dataset alignment presets (move + scale + a vertical flip that
  # this dataset needs); the Spatial tab still lets users adjust or Reset.
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(app_src, "\"spatial_image_settings\"", fixed = TRUE)
  expect_match(app_src, "offset_x = 600", fixed = TRUE)
  expect_match(app_src, "scale_x = 1.55", fixed = TRUE)
  expect_match(app_src, "flip_y = TRUE", fixed = TRUE)
})

test_that("app.R configures MERFISH plot and image rotations separately", {
  app_src <- paste(
    readLines(system.file("app.R", package = "CerebroNexus")),
    collapse = "\n"
  )
  expect_match(
    app_src,
    '"fov" = 90',
    fixed = TRUE
  )
  expect_match(
    app_src,
    '"Tissue background" = list(rotation = 90)',
    fixed = TRUE
  )
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


test_that("Spatial background picker is in the toolbar and labels default off", {
  shell <- paste(
    readLines(viewer_test_path("spatial", "UI.R")),
    collapse = "\n"
  )
  selector <- paste(
    readLines(viewer_test_path("spatial", "UI_projection_main_parameters.R")),
    collapse = "\n"
  )
  parameters <- paste(
    readLines(viewer_test_path("spatial", "obj_projection_parameters_plot.R")),
    collapse = "\n"
  )
  drawer <- regexpr("cerebroSettingsDrawer(", shell, fixed = TRUE)[[1L]]
  picker <- regexpr(
    'uiOutput("spatial_projection_background_selector_UI")',
    shell,
    fixed = TRUE
  )[[1L]]
  expect_gt(picker, 0L)
  expect_lt(picker, drawer)
  picker_source <- strsplit(
    selector,
    'output[["spatial_projection_background_selector_UI"]] <- renderUI({',
    fixed = TRUE
  )[[1L]][[2L]]
  picker_source <- strsplit(
    picker_source,
    "serverSideGeneSelector(",
    fixed = TRUE
  )[[1L]][[1L]]
  expect_no_match(
    picker_source,
    "spatial_projection_more_render_request",
    fixed = TRUE
  )
  expect_match(parameters, "group_labels = FALSE", fixed = TRUE)
  expect_match(
    parameters,
    'spatial_update_appearance("group_labels", value)',
    fixed = TRUE
  )
})
