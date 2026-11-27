spatial_flow_server <- function(input, output, session) {
  scope <- environment()
  viewer_runtime_env(scope)
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
  ## A factor colours a section from the codes of every cell (section_frame).
  viewerProjectionFirstFrameMetadata <- function() {
    if (isTRUE(input$factor_group)) {
      transform(metadata, group = factor(group))
    } else {
      metadata
    }
  }
  viewerSpatialFirstFrameData <- function(name, name_cells = TRUE) {
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
  ## The geometry of the drawn cells (`section`: rows 2 and 3 of four).
  viewerSpatialGeometryAsset <- function(...) {
    if (isTRUE(input$packed)) {
      list(
        url = "coords.bin",
        cells = length(spatial_projection_cells_to_show())
      )
    }
  }
  viewerMetadataCodesAsset <- function(name, levels = NULL) {
    list(
      url = "groups.bin",
      cells = 4L,
      levels = I(levels),
      code_map = I(c(-1L, seq_along(levels) - 1L))
    )
  }
  spatial_projection_cells_to_show <- function() {
    if (isTRUE(input$section)) {
      2:3
    } else if (isTRUE(input$subset)) {
      c(1L, 3L)
    } else {
      1:4
    }
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
  object <- list(
    getExpressionMatrix = function(cells, genes, ...) {
      expression_reads <<- expression_reads + 1L
      expression[genes, cells, drop = FALSE]
    },
    getExpressionRow = function(gene, cells = NULL) {
      expression_reads <<- expression_reads + 1L
      expression[gene, cells]
    }
  )
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
    viewer_test_path("spatial", "obj_projection_background_copy.R"),
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
    frames <- scope$frames
    session$setInputs(spatial_projection_show_region_outlines = TRUE)
    ## Outlines are an overlay patched onto the drawn frame: they are built,
    ## and the frame is not.
    expect_length(spatial_projection_group_hulls(), 1L)
    expect_identical(scope$frames, frames)
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

test_that("a section of several draws from the Viewer Pack with its own legend", {
  shiny::testServer(spatial_flow_server, {
    session$setInputs(
      spatial_projection_render_request = 1,
      spatial_projection_to_display = "slice",
      spatial_projection_plot_type = "ImageDimPlot",
      packed = TRUE,
      section = TRUE,
      factor_group = TRUE
    )
    frame <- spatial_projection_data_to_plot_raw()
    ## Neither metadata nor coordinates: the browser reads the section's
    ## geometry and maps the codes of all four cells through rows 2 and 3.
    expect_true(frame$resource_first)
    expect_null(frame$cells_df)
    expect_null(frame$coordinates)
    expect_identical(frame$cell_indices, 2:3)
    expect_identical(frame$cell_count, 2L)
    expect_identical(frame$categorical_resource$cells, 4L)
    expect_identical(as.character(frame$categorical_resource$levels), "A")
    expect_identical(
      as.integer(frame$categorical_resource$code_map),
      c(-1L, 0L)
    )

    ## A character column without assigned colours is coloured by the levels
    ## the section has: R builds that frame.
    session$setInputs(factor_group = FALSE)
    frame <- spatial_projection_data_to_plot_raw()
    expect_false(frame$resource_first)
    expect_identical(nrow(frame$cells_df), 2L)
  })
})

test_that("The client-owned background editor copies the alignment it holds as a preset", {
  shiny::testServer(spatial_flow_server, {
    session$setInputs(
      spatial_projection_render_request = 1,
      spatial_projection_to_display = "slice",
      background = TRUE
    )
    session$setInputs(spatial_projection_background_image = "embedded::tissue")
    identity <- spatial_projection_parameters_plot()$background_identity
    state <- list(
      opacity = .6,
      offsetX = 12,
      offsetY = -3,
      scaleX = 2,
      scaleY = 2,
      flipX = FALSE,
      flipY = TRUE,
      rotate = 15
    )
    session$setInputs(
      spatial_projection_background_copy = list(
        identity = identity,
        state = state,
        nonce = 1
      )
    )
    copied <- Filter(
      function(x) x$type == "cerebro_background_preset_code",
      scope$sent
    )
    expect_length(copied, 1L)
    leaf <- eval(parse(text = copied[[1]]$message$text))$tiny$slice$tissue
    expect_equal(leaf$scale_x, 2)
    expect_equal(leaf$offset_x, 12)
    expect_true(leaf$flip_y)
    expect_equal(leaf$rotation, 15)
    ## An alignment made for another data set is not turned into code.
    other <- identity
    other$dataset <- "another"
    session$setInputs(
      spatial_projection_background_copy = list(
        identity = other,
        state = state,
        nonce = 2
      )
    )
    expect_length(
      Filter(
        function(x) x$type == "cerebro_background_preset_code",
        scope$sent
      ),
      1L
    )
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
    expect_identical(
      spatial_projection_parameters_plot()$background_image,
      "none"
    )
    expect_identical(scope$frames, 2L)
  })
})

test_that("a Spatial frame draws the current region-outline switch", {
  server <- function(input, output, session) {
    context <- list(epoch = "test", dataset_key = "A", generation = 1L)
    viewer_loaded_dataset_context <- function() context
    viewerDatasetContextEqual <- function(a, b) identical(a, b)
    viewerDatasetContextToken <- function(context) context$generation
    cerebroFeedbackControls <- function(id, input) list()
    cerebroSendViewFeedback <- function(...) invisible(NULL)
    spatial_projection_group_filters <- function() list()
    ## Plot parameters keep the value of their last rebuild; the switch has
    ## been turned on since, and a filter rebuilt only the frame.
    spatial_projection_region_outlines <- reactiveVal(TRUE)
    frame <- list(
      dataset_context = context,
      plot_parameters = list(show_region_outlines = FALSE)
    )
    spatial_projection_data_to_plot_raw <- reactive(frame)
    spatial_projection_data_to_plot <- spatial_projection_data_to_plot_raw
    spatial_projection_controls <- function() list()
    .cerebro_cell_view_render_serial <- 0L
    drawn <- list()
    spatial_projection_update_plot <- function(data) {
      drawn[[length(drawn) + 1L]] <<- data$plot_parameters
      .cerebro_cell_view_render_serial <<- .cerebro_cell_view_render_serial + 1L
    }
    ## The frame is sent in its own turn of the event loop; here at once.
    spatial_projection_defer <- function(task) task()
    sys.source(
      viewer_test_path("spatial", "event_projection_update_plot.R"),
      envir = environment()
    )
    session$userData$state <- function() list(drawn = drawn)
  }
  shiny::testServer(server, {
    request <- function(nonce) {
      list(
        nonce = nonce,
        dataset_context = list(
          epoch = "test",
          dataset_key = "A",
          generation = 1L
        )
      )
    }
    session$setInputs(sidebar = "spatial")
    session$setInputs(spatial_projection_render_request = request(1))
    state <- session$userData$state()
    expect_length(state$drawn, 1L)
    expect_true(state$drawn[[1]]$show_region_outlines)
  })
})

test_that("a Spatial frame is sent in a turn of its own, and a newer one replaces it", {
  server <- function(input, output, session) {
    context <- list(epoch = "test", dataset_key = "A", generation = 1L)
    current_context <- reactiveVal(context)
    viewer_loaded_dataset_context <- function() current_context()
    viewerDatasetContextEqual <- function(a, b) identical(a, b)
    viewerDatasetContextToken <- function(context) context$generation
    cerebroFeedbackControls <- function(id, input) list()
    cerebroSendViewFeedback <- function(...) invisible(NULL)
    spatial_projection_group_filters <- function() list()
    spatial_projection_region_outlines <- reactiveVal(FALSE)
    colour <- reactiveVal("a")
    spatial_projection_data_to_plot_raw <- reactive(
      list(
        dataset_context = current_context(),
        plot_parameters = list(colour = colour())
      )
    )
    spatial_projection_data_to_plot <- spatial_projection_data_to_plot_raw
    spatial_projection_controls <- function() list()
    .cerebro_cell_view_render_serial <- 0L
    drawn <- list()
    spatial_projection_update_plot <- function(data) {
      drawn[[length(drawn) + 1L]] <<- list(
        colour = data$plot_parameters$colour,
        whole = data$fresh_render
      )
      .cerebro_cell_view_render_serial <<- .cerebro_cell_view_render_serial + 1L
    }
    ## Tasks wait for the test to give them their turn.
    waiting <- list()
    spatial_projection_defer <- function(task) {
      waiting[[length(waiting) + 1L]] <<- task
    }
    sys.source(
      viewer_test_path("spatial", "event_projection_update_plot.R"),
      envir = environment()
    )
    session$userData$state <- function() {
      list(drawn = drawn, waiting = length(waiting))
    }
    session$userData$turn <- function() {
      tasks <- waiting
      waiting <<- list()
      for (task in tasks) {
        task()
      }
    }
    session$userData$colour <- colour
    session$userData$context <- current_context
  }
  shiny::testServer(server, {
    request <- function(nonce, generation = 1L) {
      list(
        nonce = nonce,
        dataset_context = list(
          epoch = "test",
          dataset_key = "A",
          generation = generation
        )
      )
    }
    session$setInputs(sidebar = "spatial")
    session$setInputs(spatial_projection_render_request = request(1))
    ## Nothing is sent in the flush that asked for it.
    expect_length(session$userData$state()$drawn, 0L)
    expect_identical(session$userData$state()$waiting, 1L)
    ## A frame that comes before its turn is whole as well, and replaces it.
    session$userData$colour("b")
    session$flushReact()
    expect_identical(session$userData$state()$waiting, 2L)
    session$userData$turn()
    drawn <- session$userData$state()$drawn
    expect_length(drawn, 1L)
    expect_identical(drawn[[1]]$colour, "b")
    expect_true(drawn[[1]]$whole)
    ## Once sent, a new colouring is a frame of its own, sent at once.
    session$userData$colour("c")
    session$flushReact()
    expect_length(session$userData$state()$drawn, 2L)
    expect_identical(session$userData$state()$drawn[[2]]$colour, "c")
    expect_false(session$userData$state()$drawn[[2]]$whole)
    ## A data set that has gone by its turn is not drawn, and is drawn anew.
    session$setInputs(spatial_projection_render_request = request(2))
    expect_identical(session$userData$state()$waiting, 1L)
    session$userData$context(
      list(epoch = "test", dataset_key = "B", generation = 2L)
    )
    session$userData$turn()
    expect_length(session$userData$state()$drawn, 2L)
  })
})

test_that("point styling without spatial controls does not stop the session", {
  source <- paste(
    readLines(
      viewer_test_path("spatial", "obj_projection_parameters_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    source,
    "!is\\.list\\(previous\\)[\\s\\S]{0,80}return\\([\\s\\S]{0,160}modifyList\\(previous",
    perl = TRUE
  )
})
