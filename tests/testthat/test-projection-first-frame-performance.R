projection_first_frame_indices <- function(metadata, percentage = 100) {
  scope <- new.env(parent = globalenv())
  scope$input <- list(test_percentage_cells_to_show = percentage)
  viewer_runtime_env(scope)
  scope$getGroups <- function() character()
  scope$viewerProjectionCellIndices("test", metadata)
}

test_that("Spatial metadata uses the new dataset's validated colour before controls rebind", {
  shiny::testServer(
    function(input, output, session) {
      metadata <- shiny::reactiveVal(data.frame(cell_type = c("T", "B")))
      parameters <- shiny::reactiveVal(list(
        plot_type = "ImageDimPlot",
        color_variable = "cell_type",
        split_by = ""
      ))
      spatial_projection_parameters_plot <- parameters
      spatial_projection_cells_to_show <- function() 1:2
      viewer_runtime_env(environment())
      viewerProjectionFirstFrameMetadata <- metadata
      sys.source(
        viewer_test_path("spatial", "obj_projection_metadata.R"),
        environment()
      )
    },
    {
      session$setInputs(
        spatial_projection_plot_type = "ImageDimPlot",
        spatial_projection_point_color = "cell_type"
      )
      expect_identical(spatial_projection_metadata()$cell_type, c("T", "B"))
      metadata(data.frame(cluster = c("C1", "C14")))
      parameters(list(
        plot_type = "ImageDimPlot",
        color_variable = "cluster",
        split_by = ""
      ))
      session$flushReact()
      expect_identical(input$spatial_projection_point_color, "cell_type")
      expect_identical(spatial_projection_metadata()$cluster, c("C1", "C14"))
      expect_identical(spatial_projection_metadata()$cell_index, 1:2)
    }
  )
})

test_that("a full projection preserves canonical row order", {
  metadata <- data.frame(
    cell_barcode = paste0("cell-", seq_len(8L)),
    state = factor(rep(c("A", "B"), 4L))
  )

  set.seed(42L)
  expect_identical(
    projection_first_frame_indices(metadata),
    seq_len(nrow(metadata))
  )
})

test_that("projection payload selects only required metadata columns", {
  scope <- viewer_runtime_env()
  metadata <- data.frame(
    cell_barcode = c("c1", "c2"),
    major_type = factor(c("T", "B")),
    sample = factor(c("s1", "s2")),
    nUMI = 1:2,
    nGene = 2:3,
    unused = c("x", "y")
  )

  expect_identical(
    scope$viewerProjectionMetadataColumns(
      metadata,
      "major_type",
      hover_info = FALSE,
      groups = c("major_type", "sample")
    ),
    c("cell_barcode", "major_type")
  )
  expect_identical(
    scope$viewerProjectionMetadataColumns(
      metadata,
      "major_type",
      hover_info = TRUE,
      groups = c("major_type", "sample")
    ),
    c("cell_barcode", "major_type", "nUMI", "nGene", "sample")
  )
})

test_that("projection first frame excludes cell identities and hover metadata", {
  scope <- viewer_runtime_env()
  metadata <- data.frame(
    cell_barcode = c("c1", "c2"),
    major_type = factor(c("T", "B")),
    nUMI = 1:2,
    nGene = 2:3
  )

  expect_identical(
    scope$viewerProjectionFirstFrameColumns(metadata, "major_type"),
    "major_type"
  )
})

test_that("thin CRB first frames do not force cell-barcode hydration", {
  scope <- viewer_runtime_env()
  object <- new.env(parent = emptyenv())
  metadata <- data.frame(group = factor(c("A", "B")))
  projection <- data.frame(x = 1:2, y = 3:4)
  attr(object, "cerebro_projection_first_frame") <- list(
    meta_data = metadata,
    canonical_projections = "umap",
    projections = list(umap = projection)
  )
  scope$data_set <- function() object

  expect_identical(scope$viewerProjectionFirstFrameMetadata(), metadata)
  expect_identical(
    scope$viewerProjectionFirstFrameCoordinates("umap"),
    projection
  )
})

test_that("first-frame cache is optional outside a loaded dataset", {
  scope <- viewer_runtime_env()
  scope$data_set <- function() NULL

  expect_null(scope$viewerProjectionFirstFrameCache())
})

test_that("projection auxiliary data is built only after it is requested", {
  scope <- viewer_runtime_env()
  calls <- 0L
  pending <- list(
    id = "overview_projection",
    wire_token = 7L,
    build = function() {
      calls <<- calls + 1L
      list(
        selection_key = list(c("c2", "c4"), c("c1", "c3")),
        hover = list(hoverinfo = "skip")
      )
    }
  )

  expect_identical(calls, 0L)
  message <- scope$cerebroCellViewResolveDeferredAux(pending)
  expect_identical(calls, 1L)
  expect_null(message$build)
  expect_identical(message$id, "overview_projection")
  expect_identical(message$wire_token, 7L)
  expect_identical(
    message$selection_key,
    list(c("c2", "c4"), c("c1", "c3"))
  )
})

test_that("projection auxiliary rows preserve plotted group order", {
  scope <- viewer_runtime_env()
  auxiliary <- scope$cerebroCellViewDeferredAux(
    selection_rows = list(c(2L, 4L), c(1L, 3L)),
    cell_barcodes = c("c1", "c2", "c3", "c4"),
    hover_columns = list(list(
      label = "Transcripts",
      format = "integer",
      values = c(10L, 20L, 30L, 40L)
    )),
    hover = TRUE
  )

  expect_identical(
    lapply(auxiliary$selection_key, as.character),
    list(c("c2", "c4"), c("c1", "c3"))
  )
  expect_identical(
    lapply(auxiliary$hover$columns[[1L]]$values, as.integer),
    list(c(20L, 40L), c(10L, 30L))
  )
})

test_that("auxiliary rows refer to the cell-name resource when there is one", {
  scope <- viewer_runtime_env()
  resource <- list(protocol = "canonical-cell-names-v1", cells = 9L)
  auxiliary <- scope$cerebroCellViewDeferredAux(
    selection_rows = c(2L, 1L),
    cell_barcodes = NULL,
    hover = FALSE,
    names_resource = resource,
    cell_rows = c(7L, 3L)
  )
  expect_null(auxiliary$selection_key)
  expect_identical(unclass(auxiliary$selection_index), c(2L, 6L))
  expect_identical(auxiliary$cell_names_resource, resource)
  expect_identical(auxiliary$hover$hoverinfo, "skip")
})

test_that("contiguous auxiliary rows travel as their range", {
  scope <- viewer_runtime_env()
  resource <- list(protocol = "canonical-cell-names-v1", cells = 9L)
  auxiliary <- scope$cerebroCellViewDeferredAux(
    selection_rows = 1:4,
    cell_barcodes = NULL,
    hover = FALSE,
    names_resource = resource,
    cell_rows = 3:6
  )
  expect_identical(
    auxiliary$selection_index,
    list(kind = "range", start = 2L, length = 4L)
  )
  grouped <- scope$cerebroCellViewDeferredAux(
    selection_rows = list(1:2, 3:4),
    cell_barcodes = NULL,
    hover = FALSE,
    names_resource = resource,
    cell_rows = 3:6
  )
  expect_identical(lapply(grouped$selection_index, unclass), list(2:3, 4:5))
})

test_that("unmounted group filters preserve canonical rows without level scans", {
  scope <- new.env(parent = globalenv())
  scope$input <- list(test_percentage_cells_to_show = 100)
  viewer_runtime_env(scope)
  level_calls <- 0L
  scope$getGroups <- function() c("state", "sample")
  scope$getGroupLevels <- function(group) {
    level_calls <<- level_calls + 1L
    levels(metadata[[group]])
  }
  metadata <- data.frame(
    state = factor(c("A", "B", "A")),
    sample = factor(c("s1", "s1", "s2"))
  )

  expect_identical(
    scope$viewerProjectionCellIndices("test", metadata),
    seq_len(nrow(metadata))
  )
  expect_identical(level_calls, 0L)
})

test_that("gene-expression deferred hover selects rows and columns first", {
  scope <- new.env(parent = globalenv())
  scope$expressionColorScale <- function(...) NULL
  scope$expressionReverseColorScale <- function(...) FALSE
  scope$expressionPanelColorScales <- function(...) NULL
  scope$getGroups <- function() "group"
  scope$getMetaData <- function() {
    data.frame(
      cell_barcode = c("c1", "c2", "c3"),
      nUMI = 1:3,
      group = factor(c("A", "B", "A")),
      unused = c("x", "y", "z")
    )
  }
  scope$viewerProjectionMetadataColumns <- function(
    metadata,
    color_variable,
    hover_info,
    groups
  ) {
    intersect(c("cell_barcode", "nUMI", groups), colnames(metadata))
  }
  scope$viewerProjectionSubsetRows <- function(table, indices, columns) {
    table[indices, columns, drop = FALSE]
  }
  scope$cerebroProjectionHoverColumns <- function(table) {
    scope$hover_input <- table
    list()
  }
  scope$cerebroCellViewDeferredAux <- function(
    selection_rows,
    cell_barcodes,
    hover_columns,
    hover,
    keys = TRUE
  ) {
    list(selection_rows = selection_rows, cell_barcodes = cell_barcodes)
  }
  scope$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    extra,
    deferred_aux,
    ...
  ) {
    scope$aux <- deferred_aux()
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = scope
  )

  scope$expression_projection_update_plot(list(
    coordinates = data.frame(x = 1:2, y = 3:4),
    reset_axes = FALSE,
    expression_levels = c(0, 1),
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = TRUE,
      point_size = 1,
      point_opacity = 1,
      x_range = c(1, 2),
      y_range = c(3, 4),
      plot_order = "Natural",
      is_trajectory = FALSE,
      hover_info = TRUE,
      projection = "umap",
      n_dimensions = 2
    ),
    color_settings = list(
      color_scale = "Viridis",
      color_range = c(0, 1),
      color_mode = "same",
      genes = "CD3D"
    ),
    metadata = data.frame(row.names = 1:2),
    trajectory = list(),
    display_mode = "single",
    cell_indices = c(3L, 1L),
    separate_panels = FALSE
  ))

  expect_named(scope$hover_input, c("cell_barcode", "nUMI", "group"))
  expect_identical(scope$hover_input$cell_barcode, c("c3", "c1"))
  expect_identical(scope$aux$selection_rows, 1:2)
  expect_identical(scope$aux$cell_barcodes, c("c3", "c1"))
})

test_that("cached H5 gene names do not force the expression backend", {
  scope <- viewer_runtime_env()
  slow_calls <- 0L
  object <- new.env(parent = emptyenv())
  class(object) <- c("Cerebro", "R6")
  object$getGeneNames <- function() {
    slow_calls <<- slow_calls + 1L
    "slow"
  }
  attr(object, "cerebro_projection_first_frame") <- list(
    gene_names = c("CD3D", "NKG7")
  )
  scope$data_set <- function() object

  expect_identical(scope$getGeneNames(), c("CD3D", "NKG7"))
  expect_identical(slow_calls, 0L)
})

test_that("thin H5 first-frame cache includes its small gene index", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root, save = FALSE)
  scope <- viewer_runtime_env()
  object <- new.env(parent = emptyenv())
  object$meta_data <- data.frame(group = factor(c("A", "B", "A", "B")))
  object$projections <- list(umap = data.frame(x = 1:4, y = 4:1))
  schema <- list(
    version = 3L,
    n_cells = 4L,
    cell_names_hash = rlang::hash(enc2utf8(fixture$cells)),
    projection_rownames = "umap"
  )

  object <- scope$.deferThinCrbHydration(
    object,
    "dataset.crb",
    fixture$h5,
    schema
  )
  cache <- attr(object, "cerebro_projection_first_frame", exact = TRUE)
  expect_identical(cache$gene_names, fixture$genes)
  expect_identical(cache$canonical_projections, "umap")
  expect_identical(cache$meta_data$group, factor(c("A", "B", "A", "B")))
})

test_that("projection can render before its dynamic controls bind", {
  scope <- viewer_runtime_env()
  metadata <- data.frame(
    cell_barcode = c("c1", "c2"),
    sample = factor(c("a", "b")),
    cell_type = factor(c("T", "B"))
  )

  expect_identical(
    scope$viewerProjectionDefaults(
      metadata,
      projections = c("tsne", "umap"),
      parameters = list(main_group = "sample")
    ),
    list(projection = "tsne", color_variable = "sample")
  )
  expect_identical(
    scope$viewerProjectionDefaults(
      metadata,
      projections = "umap",
      parameters = list(main_group = "missing")
    )$color_variable,
    "cell_type"
  )
})

test_that("a new projection mount redraws unchanged data", {
  server <- function(input, output, session) {
    renders <- 0L
    overview_projection_parameters_other <- reactiveValues(reset_axes = TRUE)
    context <- list(epoch = "test", dataset_key = "A", generation = 1L)
    viewer_loaded_dataset_context <- function() context
    viewerDatasetContextEqual <- function(a, b) identical(a, b)
    overview_projection_data_to_plot <- reactive({
      input[["late_bound_control"]]
      ## As in the page code: the flag is read without a dependency and is
      ## cleared after the first frame, so a late binding sees FALSE.
      list(
        value = 1L,
        dataset_context = context,
        reset_axes = isolate(overview_projection_parameters_other[[
          "reset_axes"
        ]])
      )
    })
    overview_projection_data_to_plot_raw <- overview_projection_data_to_plot
    overview_projection_group_filters <- function() list()
    cerebroFeedbackControls <- function(...) list()
    cerebroSendViewFeedback <- function(...) invisible(NULL)
    overview_projection_update_plot <- function(data) {
      renders <<- renders + 1L
    }
    sys.source(
      viewer_test_path("overview", "event_projection_update_plot.R"),
      envir = environment()
    )
    session$userData$render_count <- function() renders
  }

  shiny::testServer(server, {
    session$setInputs(
      sidebar = "coordinated_views",
      overview_projection_render_request = list(
        nonce = 1,
        dataset_context = context
      ),
      late_bound_control = 1
    )
    expect_identical(session$userData$render_count(), 0L)

    session$setInputs(sidebar = "overview")
    expect_identical(session$userData$render_count(), 1L)

    session$setInputs(late_bound_control = 2)
    expect_identical(session$userData$render_count(), 1L)

    session$setInputs(
      overview_projection_render_request = list(
        nonce = 2,
        dataset_context = context
      )
    )
    expect_identical(session$userData$render_count(), 2L)
    stale <- context
    stale$generation <- 0L
    session$setInputs(
      overview_projection_render_request = list(
        nonce = 3,
        dataset_context = stale
      )
    )
    expect_identical(session$userData$render_count(), 2L)
  })
})

test_that("only another projection resets the Projection axes", {
  server <- function(input, output, session) {
    cerebroObserveLocalAppearance <- function(...) invisible(NULL)
    sys.source(
      viewer_test_path("overview", "obj_projection_parameters_plot.R"),
      envir = environment()
    )
    session$userData$flags <- overview_projection_parameters_other
  }

  shiny::testServer(server, {
    flags <- session$userData$flags
    flags[["reset_axes"]] <- FALSE
    ## The selector binding with its first value: the view stays as it is.
    session$setInputs(overview_projection_to_display = "umap")
    expect_false(isolate(flags[["reset_axes"]]))
    session$setInputs(overview_projection_to_display = "tsne")
    expect_true(isolate(flags[["reset_axes"]]))
  })
})

test_that("full ordered projection rows bypass row subsetting", {
  scope <- viewer_runtime_env()
  table <- data.frame(
    x = 1:4,
    y = 5:8,
    unused = 9:12,
    row.names = paste0("cell-", 1:4)
  )

  expect_identical(
    scope$viewerProjectionSubsetRows(
      table,
      seq_len(nrow(table)),
      c("x", "y")
    ),
    table[, c("x", "y"), drop = FALSE]
  )
  expect_identical(
    scope$viewerProjectionSubsetRows(table, c(4L, 2L), c("x", "y")),
    table[c(4L, 2L), c("x", "y"), drop = FALSE]
  )
  expect_identical(
    scope$viewerProjectionSubsetRows(table, seq_len(nrow(table)), NULL),
    table
  )
})

test_that("thin CRBs discard duplicate metadata row names", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(
    root,
    values = matrix(
      seq_len(12L),
      nrow = 3L,
      dimnames = list(paste0("gene-", 1:3), paste0("cell-", 1:4))
    ),
    stem = "compact"
  )
  cells <- fixture$cells
  crb <- fixture$crb

  payload <- .readCerebroPayload(crb)
  expect_identical(.row_names_info(payload$meta_data, 1L), -4L)
  expect_false("cell_barcode" %in% names(payload$meta_data))
  expect_identical(.row_names_info(payload$projections$umap, 1L), -4L)

  restored <- readCerebro(crb)
  expect_identical(.row_names_info(restored$meta_data, 1L), -4L)
  expect_identical(restored$meta_data$cell_barcode, cells)

  runtime <- viewer_runtime_env()
  runtime_object <- runtime$read_cerebro_file(crb)
  runtime_object <- runtime$.attachExternalExpression(runtime_object, crb)
  expect_identical(.row_names_info(runtime_object$meta_data, 1L), -4L)
  expect_identical(runtime_object$meta_data$cell_barcode, cells)
  expect_true(rlang::env_binding_are_lazy(runtime_object, "expression"))
})

test_that("noncanonical thin projections cannot bypass barcode alignment", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  cells <- c("1", "2", "3")
  fixture <- viewer_h5_fixture(
    root,
    values = matrix(
      seq_len(6L),
      nrow = 2L,
      dimnames = list(c("g1", "g2"), cells)
    ),
    save = FALSE
  )
  scope <- viewer_runtime_env()
  object <- new.env(parent = emptyenv())
  object$meta_data <- data.frame(group = c("A", "B", "C"))
  object$projections <- list(
    umap = data.frame(x = c(30, 20, 10), y = 3:1, row.names = c("3", "2", "1"))
  )
  schema <- list(
    version = 3L,
    n_cells = 3L,
    projection_rownames = character(),
    cell_names_hash = rlang::hash(enc2utf8(cells))
  )
  scope$.deferThinCrbHydration(object, "data.crb", fixture$h5, schema)
  scope$data_set <- function() object
  scope$getProjection <- function(name) object$projections[[name]]
  scope$getMetaData <- function() object$meta_data
  expect_identical(scope$availableProjections(), "umap")
  aligned <- scope$viewerProjectionFirstFrameCoordinates("umap")
  expect_identical(rownames(aligned), cells)
  expect_identical(aligned$x, c(10, 20, 30))
})

test_that("thin CRB cell names come from the H5 file and stay lazy", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root, save = FALSE)
  scope <- viewer_runtime_env()
  schema <- list(
    version = 3L,
    n_cells = 4L,
    cell_names_hash = rlang::hash(enc2utf8(fixture$cells)),
    projection_rownames = "umap"
  )

  expect_identical(scope$.runtimeH5Names(fixture$h5, "genes"), fixture$cells)
  expect_identical(scope$.runtimeH5Names(fixture$h5, "barcodes"), fixture$genes)
  expect_identical(scope$.runtimeH5CellCount(fixture$h5), 4L)
  expect_identical(
    scope$.checkThinCrbCells(fixture$cells, schema, "d.crb"),
    fixture$cells
  )
  expect_error(
    scope$.checkThinCrbCells(rev(fixture$cells), schema, "d.crb"),
    "H5 cell names do not match CRB 'd.crb'"
  )

  new_thin <- function() {
    object <- new.env(parent = emptyenv())
    object$meta_data <- data.frame(group = factor(c("A", "B", "A", "B")))
    object$projections <- list(umap = data.frame(x = 1:4, y = 4:1))
    object
  }

  ## Cell names are read, hashed and attached only when first touched.
  hydrated <- scope$.deferThinCrbHydration(
    new_thin(),
    "d.crb",
    fixture$h5,
    schema
  )
  expect_true(all(rlang::env_binding_are_lazy(
    hydrated,
    c("meta_data", "projections")
  )))
  expect_identical(hydrated$meta_data$cell_barcode, fixture$cells)
  expect_identical(rownames(hydrated$projections$umap), fixture$cells)

  ## A wrong hash fails when metadata is first touched, not at attach.
  wrong_hash <- schema
  wrong_hash$cell_names_hash <- strrep("0", 32L)
  deferred <- scope$.deferThinCrbHydration(
    new_thin(),
    "d.crb",
    fixture$h5,
    wrong_hash
  )
  cache <- attr(deferred, "cerebro_projection_first_frame", exact = TRUE)
  expect_identical(cache$gene_names, fixture$genes)
  expect_error(deferred$meta_data, "H5 cell names do not match CRB 'd.crb'")

  ## A wrong cell count fails at attach.
  wrong_count <- schema
  wrong_count$n_cells <- 5L
  expect_error(
    scope$.deferThinCrbHydration(new_thin(), "d.crb", fixture$h5, wrong_count),
    "cell count does not match its H5 sidecar"
  )
})

test_that("attaching a thin H5 CRB reads the schema, not the matrix", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root)
  scope <- viewer_runtime_env()
  object <- scope$read_cerebro_file(fixture$crb)
  object <- scope$.attachExternalExpression(object, fixture$crb)
  scope$data_set <- function() object
  lazy <- c("meta_data", "projections", "expression")

  expect_true(all(rlang::env_binding_are_lazy(object, lazy)))
  expect_identical(scope$getNumberOfCells(), 4L)
  expect_identical(scope$getGeneNames(), fixture$genes)
  expect_true(all(rlang::env_binding_are_lazy(object, lazy)))
  expect_identical(object$meta_data$cell_barcode, fixture$cells)
  expect_true(rlang::env_binding_are_lazy(object, "expression"))
  expect_identical(
    dimnames(object$expression),
    list(fixture$genes, fixture$cells)
  )
})

test_that("attaching a thin H5 CRB does not load HDF5Array", {
  ## Slow, starts an R process.
  skip_on_cran()
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root)

  state <- callr::r(
    function(utility, crb) {
      scope <- new.env(parent = globalenv())
      sys.source(
        file.path(dirname(utility), "core", "crb_contract.R"),
        envir = scope
      )
      sys.source(utility, envir = scope)
      loaded <- function() "HDF5Array" %in% loadedNamespaces()
      state <- list(start = loaded())
      object <- scope$read_cerebro_file(crb)
      object <- scope$.attachExternalExpression(object, crb)
      state$attached <- loaded()
      scope$data_set <- function() object
      state$cells <- scope$getNumberOfCells()
      state$genes <- scope$getGeneNames()
      state$after_names <- loaded()
      state$barcodes <- object$meta_data$cell_barcode
      state$after_metadata <- loaded()
      state$dim <- dim(object$expression)
      state$after_expression <- loaded()
      state
    },
    list(viewer_test_path("utility_functions.R"), fixture$crb)
  )

  expect_false(state$start)
  expect_false(state$attached)
  expect_identical(state$cells, 4L)
  expect_identical(state$genes, fixture$genes)
  expect_false(state$after_names)
  expect_identical(state$barcodes, fixture$cells)
  expect_false(state$after_metadata)
  expect_identical(state$dim, c(3L, 4L))
  expect_true(state$after_expression)
})

test_that("H5 expression rows are read without loading HDF5Array", {
  ## Slow, starts an R process.
  skip_on_cran()
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("callr")
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root)

  state <- callr::r(
    function(utility, crb) {
      scope <- new.env(parent = globalenv())
      sys.source(
        file.path(dirname(utility), "core", "crb_contract.R"),
        envir = scope
      )
      sys.source(utility, envir = scope)
      object <- scope$.attachExternalExpression(
        scope$read_cerebro_file(crb),
        crb
      )
      state <- list(
        count = scope$viewerExpressionCellCount(object),
        row = scope$viewerExpressionRow(object, NULL, "g2"),
        subset = scope$viewerExpressionRow(object, c(4L, 2L), "g3"),
        values = scope$viewerExpressionValues(object, 2:3, c("g3", "g1")),
        loaded = "HDF5Array" %in% loadedNamespaces(),
        lazy = rlang::env_binding_are_lazy(object, "expression")
      )
      state$missing <- tryCatch(
        scope$viewerExpressionRow(object, NULL, "absent"),
        error = function(error) "error"
      )
      state
    },
    list(viewer_test_path("utility_functions.R"), fixture$crb)
  )

  expect_identical(state$count, 4L)
  expect_identical(state$row, unname(fixture$values["g2", ]))
  expect_identical(state$subset, unname(fixture$values["g3", c(4L, 2L)]))
  expect_identical(
    state$values,
    list(
      g3 = unname(fixture$values["g3", 2:3]),
      g1 = unname(fixture$values["g1", 2:3])
    )
  )
  expect_false(state$loaded)
  expect_true(state$lazy)
  ## A gene the file does not have still fails through the R6 accessor.
  expect_identical(state$missing, "error")
})

test_that("a thin CRB without its H5 file names the recovery step", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root)
  scope <- viewer_runtime_env()
  object <- scope$read_cerebro_file(fixture$crb)
  unlink(fixture$h5)

  expect_error(
    scope$.attachExternalExpression(object, fixture$crb),
    "Expected h5 file at .*fixture[.]h5.*Did the [.]h5 sibling get moved"
  )
})

test_that("thin CRB schemas other than version 3 are retired", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  fixture <- viewer_h5_fixture(root)
  scope <- viewer_runtime_env()
  object <- scope$read_cerebro_file(fixture$crb)
  object$crb_schema$version <- 2L

  expect_error(
    scope$.attachExternalExpression(object, fixture$crb),
    "uses a retired format: it uses thin-CRB schema version 2.*convertCerebro"
  )
})
