test_that("spatial scene choices show only FOV identifiers", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", 1:4),
    sample = c("S1", "S1", "S2", "S2"),
    sample_roi = c("S1_lesion", "S1_border", "S2_normal", "S2_normal"),
    stringsAsFactors = FALSE
  )
  rownames(metadata) <- metadata$cell_barcode
  spatial_data <- list(
    `fov-a` = list(
      coordinates = matrix(
        1:4,
        ncol = 2L,
        dimnames = list(c("cell-1", "cell-2"), c("x", "y"))
      )
    ),
    `fov-b` = list(
      coordinates = matrix(
        5:8,
        ncol = 2L,
        dimnames = list(c("cell-3", "cell-4"), c("x", "y"))
      )
    )
  )

  choices <- spatial_scene_choices(
    c("fov-a", "fov-b"),
    spatial_data,
    metadata
  )

  expect_identical(unname(choices), c("fov-a", "fov-b"))
  expect_identical(names(choices), c("fov-a", "fov-b"))
})

test_that("Viewer defaults to the exact single ROI or separate multi-ROI layout", {
  expect_identical(
    spatial_default_roi_selection(c("ROI1", "ROI2")),
    "__separate__"
  )
  expect_identical(spatial_default_roi_selection("ROI1"), "ROI1")
  expect_identical(spatial_default_roi_selection(character()), "__all__")
})

test_that("Viewer keeps split-by on the shared interactive Canvas", {
  root <- viewer_test_path("spatial")
  controls <- paste(
    readLines(file.path(root, "UI_projection_main_parameters.R"), warn = FALSE),
    collapse = "\n"
  )
  layout <- paste(
    readLines(file.path(root, "UI.R"), warn = FALSE),
    collapse = "\n"
  )
  parameters <- paste(
    readLines(
      file.path(root, "obj_projection_parameters_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  data_flow <- paste(
    readLines(
      file.path(root, "obj_projection_data_to_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(controls, "spatial_projection_split_by", fixed = TRUE)
  expect_match(
    layout,
    'cerebroCellViewOutput("spatial_projection")',
    fixed = TRUE
  )
  expect_no_match(layout, "spatial_projection_split_plot", fixed = TRUE)
  expect_match(parameters, "split_by", fixed = TRUE)
  expect_match(parameters, "roi_point_appearance", fixed = TRUE)
  expect_match(parameters, "spatialRoiSetting", fixed = TRUE)
  expect_match(
    data_flow,
    "spatial_roi_extents\\([[:space:]]*transformed_full,[[:space:]]*scoped_roi",
    perl = TRUE
  )

  engine <- paste(
    readLines(file.path(root, "..", "www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(engine, "space.xRange = panel.x_range", fixed = TRUE)
  expect_match(engine, "space.yRange = panel.y_range", fixed = TRUE)
  expect_match(
    engine,
    "space.builder_point_size = panel.builder_point_size",
    fixed = TRUE
  )
  expect_match(
    engine,
    "space.builder_point_opacity = panel.builder_point_opacity",
    fixed = TRUE
  )
  expect_match(engine, "pointSizeEdited = !panelPointSize", fixed = TRUE)
  expect_match(engine, "pointOpacityEdited = !panelPointOpacity", fixed = TRUE)
  expect_match(
    engine,
    "panel.background_image || meta.background_image",
    fixed = TRUE
  )
  expect_match(engine, "JSON.stringify(panel.image_identity)", fixed = TRUE)

  expect_match(
    controls,
    "spatial_projection_roi_background_images",
    fixed = TRUE
  )
  expect_match(controls, "grouped_choices", fixed = TRUE)
  expect_match(controls, "multiple = TRUE", fixed = TRUE)
  expect_match(controls, 'plugins = list("remove_button")', fixed = TRUE)
  expect_match(controls, "onItemAdd", fixed = TRUE)
  expect_false(grepl("maxItems", controls, fixed = TRUE))
  expect_match(
    controls,
    "!length(roi_background_groups) && !length(background_choices)",
    fixed = TRUE
  )
})

test_that("Separate ROI panels carry their saved point appearance", {
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

test_that("optional Spatial selectors retain their reset choices", {
  controls <- paste(
    readLines(viewer_test_path("spatial", "UI_projection_main_parameters.R")),
    collapse = "\n"
  )

  expect_match(controls, "length(scene$sample_values) > 1L", fixed = TRUE)
  expect_false(grepl('"All samples"', controls, fixed = TRUE))
  expect_match(controls, "spatial_split_columns(", fixed = TRUE)
  expect_match(controls, 'class = "spatial-image-dim-controls"', fixed = TRUE)
  expect_match(
    controls,
    'spatial_split_none_value <- "__none__"',
    fixed = TRUE
  )
  expect_match(controls, '"None" = spatial_split_none_value', fixed = TRUE)
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
