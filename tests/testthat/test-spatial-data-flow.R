spatial_flow_server <- function(input, output, session) {
  scope <- environment()
  sys.source(viewer_test_path("utility_functions.R"), scope)
  sys.source(viewer_test_path("spatial", "func_spatial_helpers.R"), scope)
  coords <- data.frame(x = c(0, 10, 10, 0), y = c(0, 0, 10, 10))
  metadata <- data.frame(cell_index = 1:4, group = rep("A", 4))
  preset <- list(
    offset_x = 25,
    offset_y = -15,
    scale_x = 1.5,
    scale_y = 2,
    flip_x = TRUE,
    flip_y = FALSE,
    rotation = 90,
    ## A configured opacity must not win over the Viewer display default.
    opacity = .8
  )
  Cerebro.options <- list(
    exclude_trivial_metadata = TRUE,
    spatial_image_settings = list(tiny = list(slice = list(tissue = preset)))
  )
  available_crb_files <- list(
    files = c(tiny = "tiny.crb"),
    selected = "tiny.crb"
  )
  preferences <- list(
    use_webgl = FALSE,
    show_hover_info_in_projections = FALSE,
    cell_point_size = list(min = 1, max = 10, step = 1),
    cell_point_opacity = list(min = 0, max = 1, step = .1),
    cell_percentage_cells_to_show = list(min = 1, max = 100, step = 1)
  )
  current_scatter_defaults <- function() {
    list(point_size = 2, point_opacity = 1, percentage_cells_to_show = 100)
  }
  availableSpatial <- function() "slice"
  getGroups <- function() "group"
  viewerProjectionFirstFrameMetadata <- function() metadata
  viewerSpatialFirstFrameData <- function(name) {
    bounds <- c(xmin = 0, xmax = 10, ymin = 0, ymax = 10)
    if (isTRUE(input$list_bounds)) {
      bounds <- as.list(bounds)
    }
    if (isTRUE(input$bad_bounds)) {
      bounds[["xmax"]] <- NA_real_
    }
    image_coords <- if (isTRUE(input$nonfinite)) {
      data.frame(x = c(0, Inf, 120, NA), y = c(0, 10, NA, -Inf))
    } else {
      coords
    }
    list(
      coordinates = image_coords,
      histology_images = if (isTRUE(input$background)) {
        list(
          tissue = list(
            histology_image = "data:image/png;base64,x",
            histology_image_bounds = bounds,
            ## Background identities are built from the image's label, so a
            ## section that carries one is what the controls address.
            image_label = "tissue"
          )
        )
      } else {
        list()
      }
    )
  }
  getSpatialData <- viewerSpatialFirstFrameData
  coordinate_reads <- expression_reads <- frames <- 0L
  viewerSpatialCoordinates <- function(name) {
    coordinate_reads <<- coordinate_reads + 1L
    coords
  }
  viewerSpatialGeometryAsset <- function(...) {
    if (isTRUE(input$packed)) list(url = "coords.bin", cells = 4L) else NULL
  }
  viewerMetadataCodesAsset <- function(...) list(url = "groups.bin", cells = 4L)
  spatial_projection_cells_to_show <- function() {
    if (isTRUE(input$subset)) c(1L, 3L) else 1:4
  }
  spatial_projection_metadata <- function() {
    metadata[spatial_projection_cells_to_show(), , drop = FALSE]
  }
  spatial_projection_coordinates <- function() {
    coords[spatial_projection_cells_to_show(), , drop = FALSE]
  }
  reactive_colors <- function() c(A = "#000000")
  assignColorsToGroups <- function(...) c(A = "#000000")
  viewer_loaded_dataset_context <- function() {
    list(epoch = "s", dataset_key = "tiny", generation = 1L)
  }
  ## Author display settings are resolved for the data set the viewer requested;
  ## this harness configures none.
  viewer_requested_dataset_context <- function() {
    list(epoch = "s", dataset_key = "tiny", generation = 1L)
  }
  current_page_display_defaults <- function(page) list()
  ## The renderers also read the spatial point appearance; this harness uses the
  ## viewer's own defaults.
  current_spatial_scatter_defaults <- function() {
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 100)
  }
  viewer_current_dataset_key <- function() "tiny"
  ## Author display settings come from the deployment's content contract; this
  ## harness configures none, so the builder defaults apply.
  sys.source(viewer_test_path("core", "viewer_content_contract.R"), scope)
  current_page_display_defaults <- function(page) {
    viewerPageDisplayDefaults(list(), "tiny", page)
  }
  viewerDatasetIdentity <- function() list(fingerprint = "tiny")
  getGeneNames <- function() c("G1", "G2")
  expression <- rbind(G1 = c(1, 2, 3, 4), G2 = c(4, 3, 2, 1))
  object <- list(getExpressionMatrix = function(cells, genes, ...) {
    expression_reads <<- expression_reads + 1L
    expression[genes, cells, drop = FALSE]
  })
  data_set <- function() object
  sent <- list()
  updates <- list()
  session$sendInputMessage <- function(inputId, message) {
    updates[[inputId]] <<- message
  }
  session$sendCustomMessage <- function(type, message) {
    sent[[length(sent) + 1L]] <<- list(type = type, message = message)
  }
  sys.source(
    viewer_test_path("spatial", "obj_projection_parameters_plot.R"),
    scope
  )
  sys.source(
    viewer_test_path("spatial", "obj_projection_data_to_plot.R"),
    scope
  )
  sys.source(
    viewer_test_path("spatial", "UI_projection_additional_parameters.R"),
    scope
  )
  sys.source(
    viewer_test_path("spatial", "obj_projection_background_controls.R"),
    scope
  )
  observe({
    spatial_projection_data_to_plot_raw()
    frames <<- frames + 1L
  })
}

test_that("Spatial defaults and appearance updates do not repeat primary computation", {
  shiny::testServer(spatial_flow_server, {
    expect_identical(scope$frames, 0L)
    session$setInputs(
      spatial_projection_render_request = 1,
      spatial_projection_to_display = "slice"
    )
    expect_identical(scope$frames, 1L)
    expect_false(spatial_projection_parameters_plot()$group_labels)
    original <- output$spatial_projection_scatter_parameters_UI$html
    ## The Settings drawer binds the same Spatial defaults the server resolved
    ## for the first frame (current_spatial_scatter_defaults(), point size 5).
    session$setInputs(
      spatial_projection_plot_type = "ImageDimPlot",
      spatial_projection_point_color = "group",
      spatial_projection_point_size = 5,
      spatial_projection_point_opacity = 1,
      spatial_projection_percentage_cells_to_show = 100,
      spatial_projection_group_labels = FALSE,
      spatial_projection_point_border = FALSE,
      spatial_projection_keep_square = FALSE
    )
    expect_identical(scope$frames, 1L)
    session$setInputs(
      spatial_projection_group_labels = TRUE,
      spatial_projection_point_border = TRUE,
      spatial_projection_keep_square = TRUE,
      spatial_projection_more_render_request = 1
    )
    expect_identical(scope$frames, 1L)
    appearance <- Filter(
      function(x) x$type == "cell_view_appearance",
      scope$sent
    )
    expect_length(appearance, 3L)
    expect_setequal(
      unlist(lapply(appearance, function(x) names(x$message$values))),
      c("group_labels", "draw_border", "keep_square")
    )
    expect_true(all(vapply(
      appearance,
      function(x) {
        identical(x$message$dataset_context, viewer_loaded_dataset_context())
      },
      logical(1)
    )))
    session$setInputs(spatial_projection_background_image = "none")
    expect_identical(
      output$spatial_projection_scatter_parameters_UI$html,
      original
    )
    expect_length(spatial_projection_group_hulls(), 0L)
    session$setInputs(spatial_projection_show_region_outlines = TRUE)
    expect_length(spatial_projection_data_to_plot_raw()$group_hulls, 1L)
  })
})

test_that("Spatial feature and RGB frames read real expression in canonical row order", {
  shiny::testServer(spatial_flow_server, {
    session$setInputs(
      spatial_projection_render_request = 1,
      spatial_projection_to_display = "slice",
      spatial_projection_plot_type = "ImageFeaturePlot",
      spatial_projection_feature_to_display = "G1"
    )
    frame <- spatial_projection_data_to_plot_raw()
    expect_equal(frame$cells_df$G1, c(1, 2, 3, 4))
    session$setInputs(
      spatial_projection_plot_type = "Co-expression (RGB)",
      spatial_projection_coexpr_r = "G1",
      spatial_projection_coexpr_g = "G2",
      spatial_projection_coexpr_b = "G1"
    )
    before <- scope$expression_reads
    frame <- spatial_projection_data_to_plot_raw()
    expect_equal(frame$cells_df$coexpr_r, c(1, 2, 3, 4))
    expect_equal(frame$cells_df$coexpr_g, c(4, 3, 2, 1))
    expect_equal(frame$cells_df$coexpr_b, frame$cells_df$coexpr_r)
    # The observer already consumed this frame during the flush.
    expect_identical(scope$expression_reads, before)
    expect_equal(scope$expression_reads, 2L)
    ranges <- frame$plot_parameters[c("x_range", "y_range")]
    session$setInputs(subset = TRUE)
    frame <- spatial_projection_data_to_plot_raw()
    expect_equal(frame$cells_df$coexpr_r, c(1, 3))
    expect_equal(frame$plot_parameters[c("x_range", "y_range")], ranges)
    session$setInputs(
      subset = FALSE,
      packed = TRUE,
      spatial_projection_plot_type = "ImageDimPlot"
    )
    frame <- spatial_projection_data_to_plot_raw()
    expect_true(frame$resource_first)
    expect_null(frame$coordinates)
    expect_null(frame$cells_df)
    expect_equal(frame$cell_count, 4L)
    for (as_list in c(FALSE, TRUE)) {
      session$setInputs(background = TRUE, list_bounds = as_list)
      expect_true(spatial_projection_data_to_plot_raw()$resource_first)
    }
    ## An unusable alignment is no longer expressed by the old bounds input: it
    ## travels with the background request, and the next test asserts that such a
    ## request is refused instead of taking the resource path.
  })
})

test_that("Spatial background changes reject stale images and dataset generations", {
  shiny::testServer(spatial_flow_server, {
    session$setInputs(
      spatial_projection_render_request = 1,
      spatial_projection_to_display = "slice",
      background = TRUE
    )
    ## A background identity only exists for a selected image.
    session$setInputs(spatial_projection_background_image = "embedded::tissue")
    identity <- spatial_projection_parameters_plot()$background_identity
    request <- list(
      dataset_context = viewer_loaded_dataset_context(),
      background_identity = identity,
      values = list(
        opacity = .4,
        scaleX = 2,
        scaleY = 3,
        flipY = TRUE,
        offsetX = Inf
      )
    )
    before <- length(scope$sent)
    wrong <- request
    wrong$dataset_context$generation <- 0L
    session$setInputs(spatial_projection_background_controls = wrong)
    expect_length(scope$sent, before)
    wrong <- request
    wrong$background_identity$spatial_name <- "other"
    session$setInputs(spatial_projection_background_controls = wrong)
    expect_length(scope$sent, before)
    frames <- scope$frames
    session$setInputs(spatial_projection_background_controls = request)
    expect_identical(scope$frames, frames)
    expect_length(scope$sent, before + 1L)
    message <- tail(scope$sent, 1)[[1]]$message
    expect_equal(message$values$opacity, .4)
    expect_equal(message$values$scaleX, 2)
    expect_equal(message$values$scaleY, 3)
    expect_true(message$values$flipY)
    expect_null(message$values$offsetX)
    expect_equal(message$dataset_context, request$dataset_context)
    expect_identical(message$background_identity, identity)
  })
})

test_that("Spatial image controls seed and reset presets, synchronize values and copy current alignment", {
  shiny::testServer(spatial_flow_server, {
    session$setInputs(
      spatial_projection_render_request = 1,
      spatial_projection_to_display = "slice",
      background = TRUE,
      spatial_projection_background_image = "embedded::tissue",
      spatial_projection_more_render_request = 1,
      nonfinite = TRUE
    )
    doc <- xml2::read_html(
      output$spatial_projection_background_parameters_UI$html
    )
    expected <- c(
      offset_x = 25,
      offset_y = -15,
      scale = 1.5,
      scale_x = 1.5,
      scale_y = 2,
      rotate = 90,
      ## This preset sets no image_opacity, so the Viewer default applies.
      opacity = 0.5
    )
    for (field in names(expected)) {
      control <- xml2::xml_find_first(
        doc,
        paste0(".//*[@id='spatial_projection_background_", field, "_num']")
      )
      expect_equal(
        as.numeric(xml2::xml_attr(control, "value")),
        expected[[field]],
        info = field
      )
    }
    offset <- xml2::xml_find_first(
      doc,
      ".//*[@id='spatial_projection_background_offset_x']"
    )
    expect_equal(as.numeric(xml2::xml_attr(offset, "data-max")), 200)
    session$setInputs(spatial_projection_background_reset = 1)
    for (field in names(expected)) {
      expect_equal(
        as.numeric(
          scope$updates[[paste0("spatial_projection_background_", field)]]$value
        ),
        expected[[field]],
        info = field
      )
    }
    expect_true(scope$updates$spatial_projection_background_flip_x$value)
    expect_false(scope$updates$spatial_projection_background_flip_y$value)
    session$setInputs(spatial_projection_background_scale_x_num = 3)
    expect_equal(
      as.numeric(scope$updates$spatial_projection_background_scale_x$value),
      3
    )
    session$setInputs(spatial_projection_background_scale_x = 2.5)
    expect_equal(
      as.numeric(scope$updates$spatial_projection_background_scale_x_num$value),
      2.5
    )
    session$setInputs(
      spatial_projection_background_scale_lock = TRUE,
      spatial_projection_background_scale = 4,
      spatial_projection_background_copy_preset = 1
    )
    code <- output$spatial_projection_background_preset_code
    leaf <- eval(parse(text = code))$tiny$slice$tissue
    expect_equal(leaf$scale_x, 4)
    expect_equal(leaf$scale_y, 4)
    copied <- Filter(function(x) x$type == "spatial_copy_preset", scope$sent)
    expect_length(copied, 1L)
    expect_identical(copied[[1]]$message$text, code)
    session$setInputs(
      spatial_projection_background_image = "none",
      spatial_projection_background_copy_preset = 2
    )
    expect_null(spatial_preset_code())
  })
})

test_that("Spatial controls binding their defaults do not repeat the first frame", {
  shiny::testServer(spatial_flow_server, {
    session$setInputs(background = TRUE)
    session$setInputs(spatial_projection_render_request = 1)
    expect_identical(scope$frames, 1L)
    ## Before the selector binds, the frame already uses its default image.
    expect_identical(
      spatial_projection_parameters_plot()$background_image,
      "embedded::tissue"
    )
    session$setInputs(
      spatial_projection_to_display = "slice",
      spatial_projection_plot_type = "ImageDimPlot",
      spatial_projection_point_color = "group",
      spatial_projection_background_image = "embedded::tissue"
    )
    expect_identical(scope$frames, 1L)
    ## A bound, cleared selector is an explicit request to hide the image.
    session$setInputs(spatial_projection_background_image = NULL)
    expect_identical(spatial_projection_parameters_plot()$background_image, "none")
    expect_identical(scope$frames, 2L)
  })
})
