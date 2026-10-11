projection_first_frame_indices <- function(metadata, percentage = 100) {
  scope <- new.env(parent = globalenv())
  scope$input <- list(test_percentage_cells_to_show = percentage)
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  scope$getGroups <- function() character()
  scope$viewerProjectionCellIndices("test", metadata)
}

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
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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

  projection_source <- paste(
    readLines(
      viewer_test_path("overview", "obj_projection_data.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    projection_source,
    "viewerProjectionFirstFrameColumns",
    fixed = TRUE
  )
})

test_that("thin CRB first frames do not force cell-barcode hydration", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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

  update_source <- paste(
    readLines(
      viewer_test_path("overview", "func_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  deferred_at <- regexpr(
    "deferred_aux <- function",
    update_source,
    fixed = TRUE
  )
  metadata_at <- regexpr(
    "metadata <- getMetaData()",
    update_source,
    fixed = TRUE
  )
  expect_gt(metadata_at, deferred_at)
})

test_that("first-frame cache is optional outside a loaded dataset", {
  scope <- new.env(parent = globalenv())
  scope$data_set <- NULL
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)

  expect_null(scope$viewerProjectionFirstFrameCache())
})

test_that("projection auxiliary data is built only after it is requested", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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

test_that("trajectory first frame defers identities and hover", {
  source <- paste(
    readLines(
      viewer_test_path("trajectory", "projection_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "selection_keys = seq_len(nrow(cells_df))", fixed = TRUE)
  expect_match(source, "deferred_aux <- function()", fixed = TRUE)
  expect_match(source, "deferred_aux = deferred_aux", fixed = TRUE)
})

test_that("spatial first frame defers identities and hover", {
  source <- paste(
    readLines(
      viewer_test_path("spatial", "func_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    source,
    "selection_keys <- if (resource_first)",
    fixed = TRUE
  )
  expect_match(
    source,
    "build_deferred_aux <- function(selection_rows)",
    fixed = TRUE
  )
  expect_match(source, "deferred_aux = function()", fixed = TRUE)

  flow <- paste(
    vapply(
      c(
        "obj_projection_cells_to_show.R",
        "obj_projection_coordinates.R",
        "obj_projection_metadata.R"
      ),
      function(file) {
        paste(
          readLines(viewer_test_path("spatial", file), warn = FALSE),
          collapse = "\n"
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
  expect_match(flow, "spatial_projection_cell_index", fixed = TRUE)
  expect_match(flow, "viewerProjectionFirstFrameMetadata", fixed = TRUE)
  expect_match(flow, "viewerPackSpatialIndex", fixed = TRUE)
})

test_that("gene-expression first frame defers identities and hover", {
  update_source <- paste(
    readLines(
      viewer_test_path("gene_expression", "func_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    update_source,
    "selection_key = seq_len(n_cells)",
    fixed = TRUE
  )
  expect_match(update_source, "deferred_aux <- function()", fixed = TRUE)
  expect_match(
    update_source,
    '"cell_barcode" %in% colnames(metadata)',
    fixed = TRUE
  )
  expect_match(update_source, "getMetaData()", fixed = TRUE)
  expect_match(
    update_source,
    "viewerProjectionMetadataColumns",
    fixed = TRUE
  )
  expect_match(
    update_source,
    "viewerProjectionSubsetRows",
    fixed = TRUE
  )
  expect_match(update_source, "deferred_aux = deferred_aux", fixed = TRUE)
  expect_match(update_source, "shared_zero_color", fixed = TRUE)
  expression_source <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "obj_projection_expression_levels.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    expression_source,
    "if (length(genes_present) == 0) {\n    ## No gene",
    fixed = TRUE
  )
  expect_match(
    expression_source,
    "expression_levels <- numeric()",
    fixed = TRUE
  )

  sources <- vapply(
    c(
      "obj_projection_cells_to_show.R",
      "obj_projection_parameters_plot.R",
      "obj_projection_coordinates.R",
      "obj_projection_data.R"
    ),
    function(file) {
      paste(
        readLines(
          viewer_test_path("gene_expression", file),
          warn = FALSE
        ),
        collapse = "\n"
      )
    },
    character(1)
  )
  expect_match(
    sources[["obj_projection_cells_to_show.R"]],
    "viewerProjectionFirstFrameMetadata",
    fixed = TRUE
  )
  expect_match(
    sources[["obj_projection_parameters_plot.R"]],
    "viewerProjectionFirstFrameCoordinates",
    fixed = TRUE
  )
  expect_match(
    sources[["obj_projection_coordinates.R"]],
    "viewerProjectionFirstFrameCoordinates",
    fixed = TRUE
  )
  expect_match(
    sources[["obj_projection_data.R"]],
    "viewerProjectionFirstFrameMetadata",
    fixed = TRUE
  )

  selector_source <- paste(
    readLines(
      viewer_test_path("gene_expression", "UI_projection_select_projection.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(selector_source, "coordviews_shared_base", fixed = TRUE)
  expect_match(selector_source, "selected = selected_projection", fixed = TRUE)
})

test_that("unmounted group filters preserve canonical rows without level scans", {
  scope <- new.env(parent = globalenv())
  scope$input <- list(test_percentage_cells_to_show = 100)
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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

test_that("Gene group filters mount only after the settings drawer opens", {
  source <- paste(
    readLines(
      viewer_test_path("gene_expression", "UI_projection_group_filters.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "render_request = function()", fixed = TRUE)
  expect_match(
    source,
    'input[["expression_projection_more_render_request"]]',
    fixed = TRUE
  )
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
    hover
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

test_that("cached BPCells gene names do not force the expression backend", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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

  server_source <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  list_start <- regexpr(
    "list_of_genes <- reactive({",
    server_source,
    fixed = TRUE
  )
  expect_gt(list_start, 0L)
  expect_match(
    substr(server_source, list_start, list_start + 180L),
    "getGeneNames()",
    fixed = TRUE
  )
})

test_that("thin BPCells first-frame cache includes its small gene index", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  sidecar <- tempfile("bpcells-gene-index-")
  dir.create(sidecar)
  on.exit(unlink(sidecar, recursive = TRUE), add = TRUE)
  writeLines(c("cell-1", "cell-2"), file.path(sidecar, "col_names"))
  writeLines(c("CD3D", "NKG7"), file.path(sidecar, "row_names"))
  object <- new.env(parent = emptyenv())
  object$meta_data <- data.frame(group = factor(c("A", "B")))
  object$projections <- list(umap = data.frame(x = 1:2, y = 3:4))
  schema <- list(
    version = 2L,
    n_cells = 2L,
    cell_names_md5 = unname(tools::md5sum(file.path(sidecar, "col_names"))),
    projection_rownames = "umap"
  )

  object <- scope$.deferThinCrbHydration(
    object,
    "dataset.crb",
    sidecar,
    schema
  )
  cache <- attr(object, "cerebro_projection_first_frame", exact = TRUE)
  expect_identical(cache$gene_names, c("CD3D", "NKG7"))
})

test_that("projection rendering has one first-frame debounce entry point", {
  event_source <- paste(
    readLines(
      viewer_test_path("overview", "event_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    event_source,
    "data <- overview_projection_data_to_plot()",
    fixed = TRUE
  )
  expect_false(grepl(
    "overview_projection_rendered",
    event_source,
    fixed = TRUE
  ))
  expect_match(
    event_source,
    "if (!viewerDatasetContextEqual(data$dataset_context, current_context))",
    fixed = TRUE
  )
})

test_that("projection can render before its dynamic controls bind", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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

  parameter_source <- paste(
    readLines(
      viewer_test_path("overview", "obj_projection_parameters_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(parameter_source, "viewerProjectionDefaults", fixed = TRUE)
  expect_match(parameter_source, "current_scatter_defaults", fixed = TRUE)
  expect_false(grepl(
    '!is.null(input[["overview_projection_group_labels"]])',
    parameter_source,
    fixed = TRUE
  ))
  expect_match(parameter_source, "reset_axes = TRUE", fixed = TRUE)
})

test_that("a new projection mount redraws unchanged data", {
  server <- function(input, output, session) {
    renders <- 0L
    context <- list(epoch = "test", dataset_key = "A", generation = 1L)
    viewer_loaded_dataset_context <- function() context
    viewerDatasetContextEqual <- function(a, b) identical(a, b)
    overview_projection_data_to_plot <- reactive({
      input[["late_bound_control"]]
      list(value = 1L, dataset_context = context)
    })
    overview_projection_data_to_plot_raw <- overview_projection_data_to_plot
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

test_that("full ordered projection rows bypass row subsetting", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
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

  coordinate_source <- paste(
    readLines(
      viewer_test_path("overview", "obj_projection_coordinates.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    coordinate_source,
    "viewerProjectionSubsetRows",
    fixed = TRUE
  )
})

test_that("thin CRBs discard duplicate metadata row names", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  cells <- paste0("cell-", seq_len(4L))
  counts <- matrix(
    seq_len(12L),
    nrow = 3L,
    dimnames = list(paste0("gene-", seq_len(3L)), cells)
  )
  sparse <- methods::as(Matrix::Matrix(counts, sparse = TRUE), "CsparseMatrix")
  sidecar <- file.path(root, "expression.bpcells")
  BPCells::write_matrix_dir(methods::as(sparse, "IterableMatrix"), sidecar)

  object <- Cerebro$new()
  object$expression <- BPCells::open_matrix_dir(sidecar)
  object$setExpressionBackend("bpcells", basename(sidecar))
  object$meta_data <- data.frame(
    cell_barcode = cells,
    sample = factor(c("a", "a", "b", "b")),
    row.names = cells
  )
  object$projections <- list(
    umap = data.frame(
      UMAP_1 = seq_along(cells),
      UMAP_2 = rev(seq_along(cells)),
      row.names = cells
    )
  )
  crb <- file.path(root, "compact.crb")
  saveCerebro(object, crb)

  payload <- .readCerebroPayload(crb)
  expect_identical(.row_names_info(payload$meta_data, 1L), -4L)

  restored <- readCerebro(crb)
  expect_identical(.row_names_info(restored$meta_data, 1L), -4L)
  expect_identical(restored$meta_data$cell_barcode, cells)

  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  runtime_object <- runtime$read_cerebro_file(crb)
  runtime_object <- runtime$.attachExternalExpression(runtime_object, crb)
  expect_identical(.row_names_info(runtime_object$meta_data, 1L), -4L)
  expect_identical(runtime_object$meta_data$cell_barcode, cells)
  expect_true(rlang::env_binding_are_lazy(runtime_object, "expression"))
})


test_that("noncanonical thin projections cannot bypass barcode alignment", {
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), scope)
  sidecar <- tempfile("thin-shuffled-")
  dir.create(sidecar)
  on.exit(unlink(sidecar, recursive = TRUE), add = TRUE)
  cells <- c("1", "2", "3")
  writeLines(cells, file.path(sidecar, "col_names"))
  object <- new.env(parent = emptyenv())
  object$meta_data <- data.frame(group = c("A", "B", "C"))
  object$projections <- list(
    umap = data.frame(x = c(30, 20, 10), y = 3:1, row.names = c("3", "2", "1"))
  )
  schema <- list(
    version = 2L,
    n_cells = 3L,
    projection_rownames = character(),
    cell_names_md5 = unname(tools::md5sum(file.path(sidecar, "col_names")))
  )
  scope$.deferThinCrbHydration(object, "data.crb", sidecar, schema)
  scope$data_set <- function() object
  scope$getProjection <- function(name) object$projections[[name]]
  scope$getMetaData <- function() object$meta_data
  expect_identical(scope$availableProjections(), "umap")
  aligned <- scope$viewerProjectionFirstFrameCoordinates("umap")
  expect_identical(rownames(aligned), cells)
  expect_identical(aligned$x, c(10, 20, 30))
})
