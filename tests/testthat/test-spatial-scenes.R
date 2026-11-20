test_that("Viewer defaults to the exact single ROI or separate multi-ROI layout", {
  expect_identical(
    spatial_default_roi_selection(c("ROI1", "ROI2")),
    "__separate__"
  )
  expect_identical(spatial_default_roi_selection("ROI1"), "ROI1")
  expect_identical(spatial_default_roi_selection(character()), "__all__")
})

## The Spatial main controls as the server renders them for six cells in one
## FOV: two samples (one with `one_sample`), two ROIs with two images each and
## a categorical cell type that the cells of a sample split into.
spatial_scene_controls_server <- function(input, output, session) {
  ## The helpers read the data set and metadata through this scope.
  viewer_runtime_env(environment())
  sys.source(
    viewer_test_path("spatial", "func_spatial_helpers.R"),
    envir = environment()
  )
  data_set <- shiny::reactiveVal("spatial-scenes-controls")
  cells <- paste0("cell-", 1:6)
  images <- list()
  for (roi in c("A", "B")) {
    for (name in c("first", "second")) {
      images[[paste(roi, name, sep = "-")]] <- list(
        histology_image = paste0("data:image/png;base64,", roi, name),
        histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
        roi_field = "roi",
        roi_value = roi
      )
    }
  }
  availableSpatial <- function() "fov"
  getSpatialData <- function(name) {
    list(
      coordinates = data.frame(x = 1:6, y = 1:6, row.names = cells),
      histology_images = images
    )
  }
  viewerProjectionFirstFrameMetadata <- function() {
    data.frame(
      cell_barcode = cells,
      sample = if (isTRUE(input$one_sample)) {
        "s1"
      } else {
        rep(c("s1", "s2"), c(4, 2))
      },
      roi = rep(c("A", "B"), 3),
      cell_type = rep(c("T", "B"), 3)
    )
  }
  getMetaData <- viewerProjectionFirstFrameMetadata
  getGroups <- function() "cell_type"
  serverSideGeneSelector <- function(...) invisible(NULL)
  viewer_current_dataset_key <- function() "atlas"
  sys.source(
    viewer_test_path("spatial", "UI_projection_main_parameters.R"),
    envir = environment()
  )
}

test_that("Spatial controls offer split-by, reset choices and per-ROI images", {
  shiny::testServer(spatial_scene_controls_server, {
    main <- output$spatial_projection_main_parameters_UI$html
    ## The scene controls are a sibling output (UI.R), not one nested here
    ## that waits for this output to bind.
    expect_false(grepl('id="spatial_projection_scene_UI"', main, fixed = TRUE))
    expect_match(main, 'class="spatial-image-dim-controls"', fixed = TRUE)
    expect_match(main, 'id="spatial_projection_split_by"', fixed = TRUE)
    ## "None" resets the split; the cell type is offered because the cells of
    ## the selected sample split into it.
    expect_match(
      main,
      '<option value="__none__" selected>None</option>',
      fixed = TRUE
    )
    expect_match(
      main,
      '<option value="cell_type">cell_type</option>',
      fixed = TRUE
    )

    scene <- output$spatial_projection_scene_UI$html
    expect_match(scene, 'id="spatial_projection_sample"', fixed = TRUE)
    expect_false(grepl("All samples", scene, fixed = TRUE))
    expect_match(scene, 'value="__separate__"', fixed = TRUE)

    session$setInputs(spatial_projection_roi = "__separate__")
    picker <- output$spatial_projection_background_selector_UI$html
    expect_match(
      picker,
      'id="spatial_projection_roi_background_images"',
      fixed = TRUE
    )
    expect_match(picker, '<optgroup label="A">', fixed = TRUE)
    expect_match(picker, 'multiple="multiple"', fixed = TRUE)
    expect_match(picker, '"remove_button"', fixed = TRUE)
    expect_match(picker, '"onItemAdd"', fixed = TRUE)
    expect_false(grepl("maxItems", picker, fixed = TRUE))

    ## One sample needs no Sample selector.
    session$setInputs(one_sample = TRUE)
    data_set("spatial-scenes-controls-one-sample")
    session$flushReact()
    scene <- output$spatial_projection_scene_UI$html
    expect_false(grepl("spatial_projection_sample", scene, fixed = TRUE))
    expect_match(scene, 'id="spatial_projection_sections"', fixed = TRUE)
  })
})

test_that("Spatial draws split-by panels on the shared interactive Canvas", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("spatial", "UI.R"), scope)
  page <- htmltools::tagQuery(scope$tab_spatial)
  expect_equal(page$find("#spatial_projection_cell_view_host")$length(), 1L)
  ## Sample/FOV/ROI controls render in the first output flush, before the
  ## main controls.
  primary <- page$find(".cerebro-viz-primary")$children()
  expect_identical(
    primary$selectedTags()[[1L]]$attribs$id,
    "spatial_projection_scene_UI"
  )
  expect_identical(
    primary$selectedTags()[[2L]]$attribs$id,
    "spatial_projection_main_parameters_UI"
  )
  expect_false(grepl(
    "spatial_projection_split_plot",
    as.character(scope$tab_spatial),
    fixed = TRUE
  ))
})

## The split and ROI settings reach the frame and the renderer reads them per
## panel; no other test follows them through these files.
test_that("split and ROI settings reach the Spatial frame and each panel", {
  root <- viewer_test_path("spatial")
  read <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
  parameters <- read(file.path(root, "obj_projection_parameters_plot.R"))
  expect_match(parameters, "split_by", fixed = TRUE)
  expect_match(parameters, "roi_point_appearance", fixed = TRUE)
  expect_match(parameters, "spatialRoiSetting", fixed = TRUE)
  expect_match(
    read(file.path(root, "obj_projection_data_to_plot.R")),
    "spatial_roi_extents\\([[:space:]]*transformed_full,[[:space:]]*scoped_roi",
    perl = TRUE
  )

  engine <- read(file.path(root, "..", "www", "cell_views.js"))
  for (wiring in c(
    "space.xRange = panel.x_range",
    "space.yRange = panel.y_range",
    "space.builder_point_size = panel.builder_point_size",
    "space.builder_point_opacity = panel.builder_point_opacity",
    "pointSizeEdited = !panelPointSize",
    "pointOpacityEdited = !panelPointOpacity",
    "panel.background_image || meta.background_image",
    "JSON.stringify(panel.image_identity)"
  )) {
    expect_match(engine, wiring, fixed = TRUE)
  }
})

test_that("Separate ROI panels carry their saved point appearance", {
  renderer <- viewer_runtime_env()
  sys.source(
    viewer_test_path("spatial", "func_spatial_helpers.R"),
    envir = renderer
  )
  sys.source(
    viewer_test_path("spatial", "func_projection_update_plot.R"),
    envir = renderer
  )
  rendered <- NULL
  renderer$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover = list(),
    extra = list(),
    dataset_context,
    ...
  ) {
    rendered <<- list(meta = meta, data = data, extra = extra)
  }
  parameters <- list(
    color_variable = "cluster",
    split_by = "sample_roi",
    background_descriptor = NULL,
    background_identity = NULL,
    background_image_allowlist = character(),
    background_flip_x = FALSE,
    background_flip_y = FALSE,
    background_scale_x = 1,
    background_scale_y = 1,
    background_offset_x = 0,
    background_offset_y = 0,
    background_rotation = 0,
    background_opacity = 1,
    n_dimensions = 2,
    x_range = c(0, 10),
    y_range = c(0, 10),
    plot_type = "ImageDimPlot",
    point_size = 5,
    point_opacity = 1,
    draw_border = FALSE,
    group_labels = FALSE,
    keep_square = FALSE,
    show_region_outlines = FALSE,
    hover_info = FALSE,
    roi_mode = "separate",
    roi_order = c("border", "lesion"),
    roi_backgrounds = list(),
    roi_point_appearance = list(
      border = list(point_opacity = 0.35, point_size = 9),
      lesion = list(point_opacity = 0.65, point_size = 7)
    )
  )

  renderer$spatial_projection_update_plot(list(
    dataset_context = list(
      epoch = "spatial-scenes-test",
      dataset_key = "scene-a",
      generation = 1
    ),
    cells_df = data.frame(
      cell_barcode = c("cell-1", "cell-2"),
      cluster = factor(c("C1", "C2")),
      sample_roi = c("lesion", "border")
    ),
    coordinates = data.frame(x = c(2, 8), y = c(3, 7)),
    reset_axes = FALSE,
    plot_parameters = parameters,
    color_assignments = c(C1 = "#111111", C2 = "#eeeeee"),
    group_hulls = list(),
    hover_columns = list(),
    hover_info = character(),
    cell_boundaries = list(),
    molecule_points = list()
  ))

  expect_identical(
    lapply(rendered$data$panels, `[[`, "builder_point_size"),
    list(9, 7)
  )
  expect_identical(
    lapply(rendered$data$panels, `[[`, "builder_point_opacity"),
    list(0.35, 0.65)
  )
})

test_that("Separate ROI panels use full extents for camera and background", {
  extents <- spatial_roi_extents(
    data.frame(
      x = c(0, 10, 100, 200),
      y = c(20, 40, 300, 500)
    ),
    c("lesion", "lesion", "border", "border")
  )
  expect_identical(
    extents,
    list(
      lesion = list(x = c(0, 10), y = c(20, 40)),
      border = list(x = c(100, 200), y = c(300, 500))
    )
  )

  renderer <- viewer_runtime_env()
  sys.source(
    viewer_test_path("spatial", "func_spatial_helpers.R"),
    envir = renderer
  )
  sys.source(
    viewer_test_path("spatial", "func_projection_update_plot.R"),
    envir = renderer
  )
  rendered <- NULL
  renderer$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover = list(),
    extra = list(),
    dataset_context,
    ...
  ) {
    rendered <<- list(meta = meta, data = data, extra = extra)
  }
  preset <- list(
    flipX = FALSE,
    flipY = FALSE,
    scaleX = 1,
    scaleY = 1,
    offsetX = 0,
    offsetY = 0,
    rotation = 0,
    opacity = 1
  )
  roi_background <- function(label) {
    list(
      descriptor = list(
        source = "embedded",
        label = label,
        image = paste0("data:image/png;base64,", label),
        bounds = NULL
      ),
      identity = list(roi = label),
      preset = preset,
      image_allowlist = character()
    )
  }
  parameters <- list(
    color_variable = "cluster",
    split_by = "sample_roi",
    background_descriptor = NULL,
    background_identity = NULL,
    background_image_allowlist = character(),
    background_flip_x = FALSE,
    background_flip_y = FALSE,
    background_scale_x = 1,
    background_scale_y = 1,
    background_offset_x = 0,
    background_offset_y = 0,
    background_rotation = 0,
    background_opacity = 1,
    n_dimensions = 2,
    x_range = c(0, 200),
    y_range = c(20, 500),
    plot_type = "ImageDimPlot",
    point_size = 5,
    point_opacity = 1,
    draw_border = FALSE,
    group_labels = FALSE,
    keep_square = FALSE,
    show_region_outlines = FALSE,
    hover_info = FALSE,
    roi_mode = "separate",
    roi_order = c("border", "lesion"),
    roi_backgrounds = list(
      border = roi_background("border"),
      lesion = roi_background("lesion")
    ),
    roi_point_appearance = list(),
    roi_extents = extents
  )

  renderer$spatial_projection_update_plot(list(
    dataset_context = list(
      epoch = "spatial-scenes-test",
      dataset_key = "scene-b",
      generation = 1
    ),
    cells_df = data.frame(
      cell_barcode = c("cell-1", "cell-2"),
      cluster = factor(c("C1", "C2")),
      sample_roi = c("lesion", "border")
    ),
    coordinates = data.frame(x = c(5, 150), y = c(30, 400)),
    reset_axes = FALSE,
    plot_parameters = parameters,
    color_assignments = c(C1 = "#111111", C2 = "#eeeeee"),
    group_hulls = list(),
    hover_columns = list(),
    hover_info = character(),
    cell_boundaries = list(),
    molecule_points = list()
  ))

  expect_identical(
    lapply(rendered$data$panels, `[[`, "x_range"),
    list(c(98, 202), c(-0.2, 10.2))
  )
  expect_identical(
    lapply(rendered$data$panels, `[[`, "y_range"),
    list(c(296, 504), c(19.6, 40.4))
  )
  expect_identical(
    lapply(rendered$data$panels, function(panel) {
      unlist(panel$image_bounds, use.names = FALSE)
    }),
    list(c(100, 200, 300, 500), c(0, 10, 20, 40))
  )
})

test_that("spatial split choices reuse safe categorical metadata", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", 1:6),
    numeric_group = rep(1:2, each = 3),
    cell_type = rep(c("T", "B"), 3),
    condition = factor(rep(c("control", "treated"), each = 3)),
    passed_qc = rep(c(TRUE, FALSE), 3),
    nUMI = seq_len(6),
    constant = "same",
    identifier = paste0("id-", 1:6),
    stringsAsFactors = FALSE
  )

  expect_identical(
    spatial_split_columns(
      metadata,
      metadata$cell_barcode,
      groups = "numeric_group"
    ),
    c("numeric_group", "cell_type", "condition", "passed_qc")
  )
})

test_that("spatial categorical columns have no arbitrary level cap", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", 1:30),
    cluster = factor(c(paste0("C", 1:24), paste0("C", 1:6))),
    unregistered = factor(c(paste0("U", 1:13), rep("U1", 17))),
    stringsAsFactors = FALSE
  )

  expect_identical(
    spatial_split_columns(
      metadata,
      metadata$cell_barcode,
      groups = "cluster"
    ),
    c("cluster", "unregistered")
  )
})

test_that("unfinished molecule overlay controls are not exposed", {
  controls <- paste(
    readLines(viewer_test_path(
      "spatial",
      "UI_projection_additional_parameters.R"
    )),
    collapse = "\n"
  )
  expect_no_match(controls, "spatial_projection_show_molecules", fixed = TRUE)
  expect_no_match(
    controls,
    "spatial_projection_show_cell_boundaries",
    fixed = TRUE
  )
})

test_that("Separate ROIs groups and normalizes one background per ROI", {
  rois <- paste0("ROI", 1:4)
  images <- list()
  for (roi in rois) {
    for (suffix in c("A", "B")) {
      label <- paste(roi, suffix, sep = "-")
      images[[label]] <- list(
        histology_image = paste0("data:image/png;base64,", label),
        histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
        roi_field = "sample_roi",
        roi_value = roi
      )
    }
  }
  groups <- spatial_roi_background_groups(
    list(histology_images = images),
    options = NULL,
    dataset = "dataset",
    spatial_name = "fov",
    roi_values = c(rois, "ROI-without-image")
  )

  expect_named(groups, rois)
  expect_true(all(vapply(
    groups,
    function(group) {
      identical(
        names(group$choices),
        c(
          paste0(group$roi, "-A"),
          paste0(group$roi, "-B")
        )
      )
    },
    logical(1)
  )))

  selected <- c(
    groups$ROI1$tokens[["embedded::ROI1-B"]],
    groups$ROI3$tokens[["embedded::ROI3-A"]],
    groups$ROI3$tokens[["embedded::ROI3-B"]],
    "invalid"
  )
  expect_identical(
    spatial_roi_background_selections(groups, selected),
    list(
      ROI1 = "embedded::ROI1-B",
      ROI2 = "none",
      ROI3 = "embedded::ROI3-A",
      ROI4 = "none"
    )
  )
  expect_identical(
    spatial_roi_background_selections(groups, selected[-4L]),
    list(
      ROI1 = "embedded::ROI1-B",
      ROI2 = "none",
      ROI3 = "embedded::ROI3-A",
      ROI4 = "none"
    )
  )
  expect_identical(
    spatial_roi_background_selections(groups, NULL),
    list(
      ROI1 = "none",
      ROI2 = "none",
      ROI3 = "none",
      ROI4 = "none"
    )
  )
  expect_identical(
    spatial_roi_background_selections(groups, character()),
    list(
      ROI1 = "none",
      ROI2 = "none",
      ROI3 = "none",
      ROI4 = "none"
    )
  )
})
