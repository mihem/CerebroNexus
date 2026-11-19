test_that("the 1M demo is opt-in and validates its H5 file", {
  helper <- viewer_test_path("million_cell_demo.R")
  expect_true(file.exists(helper))
  env <- new.env(parent = baseenv())
  sys.source(helper, envir = env)

  original <- list(
    crb_file_to_load = c(Small = "small.crb"),
    point_size = c(Small = 5),
    point_opacity = c(Small = 1),
    percentage_cells_to_show = 100
  )
  expect_identical(env$viewerAddMillionCellDemo(original, ""), original)

  root <- tempfile("cerebro-1m-demo-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- file.path(root, "mouse.crb")
  file.create(crb)
  file.create(file.path(root, "mouse.h5"))

  configured <- env$viewerAddMillionCellDemo(original, crb)
  label <- "10x E18 mouse brain (1.3M)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$point_size[label]), 1)
  expect_equal(unname(configured$point_opacity[label]), 0.5)
  expect_equal(unname(configured$percentage_cells_to_show[label]), 100)
  expect_null(configured$expression_point_size)

  unlink(file.path(root, "mouse.h5"))
  expect_error(
    env$viewerAddMillionCellDemo(original, crb),
    "CEREBRO_1M_DEMO_CRB must name a .crb with an adjacent .h5 file.",
    fixed = TRUE
  )
  dir.create(file.path(root, "mouse.h5"))
  expect_error(
    env$viewerAddMillionCellDemo(original, crb),
    "adjacent .h5 file",
    fixed = TRUE
  )
  unlink(file.path(root, "mouse.h5"), recursive = TRUE)
  file.create(file.path(root, "mouse.h5"))
  expect_error(
    env$viewerAddMillionCellDemo(original, file.path(root, "missing.crb")),
    "adjacent .h5 file",
    fixed = TRUE
  )
})

test_that("the 1M preparation keeps unique gene symbols", {
  script <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_1m_data.R"
  )
  skip_if_not(
    file.exists(script),
    "demo tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  expect_identical(
    env$.viewer1mUniqueGeneSymbols(c("Cd3e", "Cd3e", "Ms4a1"), 3L),
    c("Cd3e", "Cd3e.1", "Ms4a1")
  )
  expect_error(
    env$.viewer1mUniqueGeneSymbols(c("Cd3e", ""), 2L),
    "non-empty"
  )

  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    seurat_clusters = c("0", "1", "2"),
    stringsAsFactors = FALSE
  )
  projection <- rbind(
    c3 = c(1, 3),
    c1 = c(0, 1),
    c2 = c(2, 2)
  )
  object <- list(
    getGeneNames = function() c("Sox2", "Dcx", "Rbfox3"),
    getMeanExpressionForCells = function(cells, genes) {
      scores <- list(
        Sox2 = c(c1 = 10, c2 = 2, c3 = 0.5),
        Dcx = c(c1 = 1, c2 = 10, c3 = 2),
        Rbfox3 = c(c1 = 0.5, c2 = 2, c3 = 10)
      )
      values <- vapply(
        genes,
        function(gene) scores[[gene]][cells],
        numeric(length(cells))
      )
      if (is.null(dim(values))) values else rowMeans(values)
    }
  )
  trajectory <- env$.viewer1mMarkerGuidedTrajectory(
    object,
    metadata,
    projection
  )

  expect_identical(rownames(trajectory$meta), metadata$cell_barcode)
  expect_true(all(diff(trajectory$meta$pseudotime) > 0))
  expect_identical(
    levels(trajectory$meta$state),
    c("Neural progenitor", "Neuroblast", "Maturing neuron")
  )
  expect_equal(nrow(trajectory$edges), 2L)
  expect_equal(
    unname(unlist(trajectory$edges[1, ])),
    c(0, 1, 2, 2)
  )
  expect_identical(trajectory$provenance$type, "marker_guided")
  expect_identical(trajectory$provenance$version, 2L)
  expect_identical(trajectory$provenance$markers$early, "Sox2")
  expect_identical(trajectory$provenance$markers$late, "Rbfox3")

  branching <- env$.viewer1mMarkerGuidedEdges(data.frame(
    state = c(
      "Neural progenitor",
      "Neuroblast",
      "Excitatory neuron",
      "Inhibitory neuron"
    ),
    DR_1 = c(0, 1, 2, 2),
    DR_2 = c(0, 0, 1, -1),
    pseudotime = c(0, 0.5, 1, 1)
  ))
  terminal_edges <- branching[branching$target_dim_1 == 2, , drop = FALSE]
  expect_equal(nrow(terminal_edges), 2L)
  expect_equal(terminal_edges$source_dim_1, c(1, 1))
  expect_equal(terminal_edges$source_dim_2, c(0, 0))
  expect_equal(sort(terminal_edges$target_dim_2), c(-1, 1))
})

test_that("the 1M demo adds the marker-guided path and main group", {
  script <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_1m_data.R"
  )
  skip_if_not(
    file.exists(script),
    "demo tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    seurat_clusters = c("0", "1", "2"),
    row.names = c("c1", "c2", "c3"),
    stringsAsFactors = FALSE
  )
  projection <- cbind(x = c(0, 2, 1), y = c(1, 2, 3))
  rownames(projection) <- metadata$cell_barcode
  parameters <- list()
  object <- new.env(parent = emptyenv())
  object$trajectories <- list()
  object$getParameters <- function() parameters
  object$addParameters <- function(field, content) {
    parameters[[field]] <<- content
  }
  object$getMethodsForTrajectories <- function() names(object$trajectories)
  object$getNamesOfTrajectories <- function(method) {
    names(object$trajectories[[method]])
  }
  object$addTrajectory <- function(method, name, trajectory) {
    object$trajectories[[method]][[name]] <- trajectory
  }
  object$availableProjections <- function() "umap"
  object$getMetaData <- function() metadata
  object$getProjection <- function(name) projection
  object$getGeneNames <- function() c("Sox2", "Dcx", "Rbfox3")
  object$getMeanExpressionForCells <- function(cells, genes) {
    scores <- list(
      Sox2 = c(c1 = 10, c2 = 2, c3 = 0.5),
      Dcx = c(c1 = 1, c2 = 10, c3 = 2),
      Rbfox3 = c(c1 = 0.5, c2 = 2, c3 = 10)
    )
    values <- vapply(
      genes,
      function(gene) scores[[gene]][cells],
      numeric(length(cells))
    )
    if (is.null(dim(values))) values else rowMeans(values)
  }

  expect_true(env$.viewer1mEnrichDemoObject(object))
  expect_identical(parameters$main_group, "seurat_clusters")
  expect_named(object$trajectories$marker_guided, "E18_neurogenesis")
  expect_false(env$.viewer1mEnrichDemoObject(object))
})

test_that("the Viewer filters methods at trajectory selection boundaries", {
  utility <- viewer_runtime_env()
  expect_identical(
    utility$viewerSupportedTrajectoryMethods(
      c(
        "unsupported",
        "marker_guided",
        "monocle2",
        "illustrative",
        "published_pseudotime"
      )
    ),
    c("marker_guided", "monocle2", "illustrative", "published_pseudotime")
  )
})

test_that("Viewer spatial readers leave molecule sidecars dormant", {
  scope <- viewer_runtime_env()
  dataset <- new.env(parent = emptyenv())
  class(dataset) <- c("Cerebro", "R6")
  dataset$spatial <- list(
    slice = list(
      coordinates = data.frame(x = 1:2, y = 3:4),
      expression = matrix(numeric(), nrow = 0L, ncol = 2L)
    )
  )
  dataset$getSpatialData <- function(name, hydrate_molecules) {
    expect_false(hydrate_molecules)
    dataset$spatial[[name]]
  }
  scope$data_set <- function() dataset
  expect_identical(
    scope$getSpatialData("slice")$coordinates,
    dataset$spatial$slice$coordinates
  )
})

test_that("million-cell hover stays columnar until the browser needs it", {
  utility <- viewer_runtime_env()
  columns <- utility$cerebroProjectionHoverColumns(
    data.frame(
      cell_barcode = c("cell-1", "cell-2"),
      nUMI = c(1234, 9),
      nGene = c(321, 4),
      cluster = c("B", "A")
    ),
    groups = "cluster"
  )

  expect_identical(
    vapply(columns, `[[`, character(1), "label"),
    c("Transcripts", "Expressed genes", "cluster")
  )
  expect_identical(columns[[3L]]$levels, c("B", "A"))
  expect_identical(columns[[3L]]$values, c(0L, 1L))
})

test_that("sparse linked attributes expand into typed canonical vectors", {
  skip_if_no_node()
  source <- viewer_test_path("www", "cell_views_state.js")
  output <- run_node_script(c(
    "const fs = require('fs');",
    "global.window = {};",
    js_eval_file(source),
    "const values = window.CBViewState.expandSparse(",
    "  new Int32Array([1, 4]), new Int16Array([7, 9]), 6, -1);",
    "console.log(JSON.stringify(Array.from(values)));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output),
    c(-1L, 7L, -1L, -1L, 9L, -1L)
  )
})

test_that("shared browser base reuses only matching dataset projections", {
  skip_if_no_node()
  source <- viewer_test_path("www", "cell_views_state.js")
  output <- run_node_script(c(
    "const fs = require('fs'); global.window = {};",
    js_eval_file(source),
    "const S = window.CBViewState;",
    "const first = S.sharedBase(null, 'dataset-a', 3, 'order-a');",
    "first.projections.umap = {x:new Float32Array(3),y:new Float32Array(3)};",
    "const hit = S.sharedBase(first, 'dataset-a', 3, 'order-a');",
    "const changedDataset = S.sharedBase(first, 'dataset-b', 3, 'order-a');",
    "const changedCount = S.sharedBase(first, 'dataset-a', 4, 'order-a');",
    "const changedOrder = S.sharedBase(first, 'dataset-a', 3, 'order-b');",
    "console.log(JSON.stringify({hit:hit===first,dataset:changedDataset!==first,",
    "count:changedCount!==first,order:changedOrder!==first,",
    "projection:hit.projections.umap===first.projections.umap}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      hit = TRUE,
      dataset = TRUE,
      count = TRUE,
      order = TRUE,
      projection = TRUE
    )
  )
})

test_that("specialist lifecycle owns request and auxiliary state", {
  skip_if_no_node()
  source <- viewer_test_path("www", "cell_views_state.js")
  output <- run_node_script(c(
    "const fs = require('fs'); global.window = {};",
    js_eval_file(source),
    "const L = window.CBViewState.specialistLifecycle, timings = {};",
    "L.begin(timings, 'spatial', true);",
    "const view = {data:{wire_token:7}};",
    "const requested = L.requestAux(view);",
    "const duplicate = L.requestAux(view);",
    "const rejected = L.acceptAux(view, {wire_token:8});",
    "const accepted = L.acceptAux(view, {wire_token:7});",
    "const cached = L.begin(timings, 'spatial', false);",
    "console.log(JSON.stringify({generation:cached.generation,",
    "cached:cached.cached,requested,duplicate,rejected,accepted,",
    "pending:view._auxPending,token:view._auxToken}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      generation = 2L,
      cached = TRUE,
      requested = 7L,
      duplicate = NULL,
      rejected = FALSE,
      accepted = TRUE,
      pending = NULL,
      token = 7L
    )
  )
})

test_that("canonical mapping centralizes identity, subset, and index order", {
  skip_if_no_node()
  source <- viewer_test_path("www", "cell_views_state.js")
  output <- run_node_script(c(
    "const fs = require('fs'); global.window = {};",
    js_eval_file(source),
    "const M = window.CBViewState.canonicalMapping;",
    "const identity = {cell_count:4,",
    " cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),",
    " cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),",
    " pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};",
    "const resource = {cells:4,dataset_fingerprint:identity.cell_fingerprint,",
    " cell_order_fingerprint:identity.cell_order_fingerprint,",
    " pack_dataset_fingerprint:identity.pack_dataset_fingerprint};",
    "const include = M.subsetContract({protocol:'canonical-subset-v1',",
    " kind:'include_uint32',canonical_cells:4,cells:2,index_base:0,",
    " dtype:'uint32',url:'subset.bin',bytes:8},4,2);",
    "const exclude = M.subsetContract({protocol:'canonical-subset-v1',",
    " kind:'exclude_uint32',canonical_cells:4,cells:2,index_base:0,",
    " dtype:'uint32',url:'subset.bin',bytes:8},4,2);",
    "const included = [], excluded = [];",
    "M.forEachIndex(include,new Uint32Array([3,1]),i=>included.push(i));",
    "M.forEachIndex(exclude,new Uint32Array([1,3]),i=>excluded.push(i));",
    "console.log(JSON.stringify({identity:M.resourceIdentityMatches(resource,identity),",
    "badIdentity:M.resourceIdentityMatches({...resource,cells:3},identity),",
    "includeValid:M.indicesMatch(include,new Uint32Array([3,1])),",
    "duplicate:M.indicesMatch(include,new Uint32Array([1,1])),",
    "excludeValid:M.indicesMatch(exclude,new Uint32Array([1,3])),",
    "unsorted:M.indicesMatch(exclude,new Uint32Array([3,1])),included,excluded}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      identity = TRUE,
      badIdentity = FALSE,
      includeValid = TRUE,
      duplicate = FALSE,
      excludeValid = TRUE,
      unsorted = FALSE,
      included = list(3L, 1L),
      excluded = list(0L, 2L)
    )
  )
})

test_that("resource descriptors share shape and identity validation", {
  skip_if_no_node()
  source <- viewer_test_path("www", "cell_views_state.js")
  output <- run_node_script(c(
    "const fs=require('fs'); global.window={};",
    js_eval_file(source),
    "const R=window.CBViewState.resourceDescriptor;",
    "const identity={cell_fingerprint:'md5-cell-set-v1:'+'a'.repeat(32),",
    "cell_order_fingerprint:'md5-cell-order-v1:'+'b'.repeat(32),",
    "pack_dataset_fingerprint:'md5-crb-v1:'+'c'.repeat(32)};",
    "const resource={protocol:'spatial-geometry-v1',url:'g.bin',cells:4,",
    "dimensions:2,dtype:'float32',bytes:32,",
    "dataset_fingerprint:identity.cell_fingerprint,",
    "cell_order_fingerprint:identity.cell_order_fingerprint,",
    "pack_dataset_fingerprint:identity.pack_dataset_fingerprint};",
    "const valid=R.validate(resource,{protocol:'spatial-geometry-v1',",
    "cells:4,dimensions:2,dtype:'float32',requireUrl:true,identity});",
    "console.log(JSON.stringify({valid:!!valid,bytes:valid.bytes,",
    "shape:!!R.validate({...resource,dimensions:3},{cells:4,dimensions:2}),",
    "identity:!!R.validate({...resource,dataset_fingerprint:'bad'},",
    "{cells:4,identity}),width:R.codeWidth({dtype:'uint16'})}));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(valid = TRUE, bytes = 32L, shape = FALSE, identity = FALSE, width = 2L)
  )
})

test_that("telemetry snapshots normalize shared transport phases", {
  skip_if_no_node()
  source <- viewer_test_path("www", "cell_views_state.js")
  output <- run_node_script(c(
    "const fs=require('fs'); global.window={};",
    js_eval_file(source),
    "const T=window.CBViewState.telemetry;",
    "const metric=T.transport({server_prepare_ms:'12.5',",
    "server_resource_ms:'3',server_bundle_ms:'bad',sent_at_ms:900},",
    "{bytes:42,decodeMs:4,requestToBinaryMs:8},true,1000);",
    "const snapshot=T.snapshot(metric,{decodeToDrawMs:6});",
    "console.log(JSON.stringify(snapshot));"
  ))

  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      bytes = 42L,
      serverPrepareMs = 12.5,
      serializeTransferMs = 100L,
      decodeMs = 4L,
      serverResourceMs = 3L,
      serverBundleMs = NULL,
      requestToBinaryMs = 8L,
      decodeToDrawMs = 6L
    )
  )
})

test_that("Mouse 1M marker-guided trajectory keeps its neural-lineage contract", {
  artifact <- Sys.getenv("CEREBRO_MOUSE_1M_CRB")
  skip_if(!nzchar(artifact) || !file.exists(artifact))
  magic <- readBin(artifact, "raw", n = 4L)
  object <- if (identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))) {
    qs2::qs_read(artifact)
  } else {
    readRDS(artifact)
  }
  trajectory <- object$getTrajectory("marker_guided", "E18_neurogenesis")

  expect_identical(nrow(object$getMetaData()), 1306127L)
  expect_lt(nrow(trajectory$meta), nrow(object$getMetaData()))
  expect_false(anyNA(trajectory$meta$pseudotime))
  expect_false(anyNA(trajectory$meta$DR_1))
  expect_false(anyNA(trajectory$meta$DR_2))
})

test_that("Gene projection delegates paint order without copying cell vectors", {
  runtime <- new.env(parent = globalenv())
  captured <- new.env(parent = emptyenv())
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$getGroups <- function() character()
  runtime$viewerProjectionMetadataColumns <- function(
    metadata,
    color_variable,
    hover_info,
    groups
  ) {
    "cell_barcode"
  }
  runtime$viewerProjectionSubsetRows <- function(table, indices, columns) {
    table[indices, columns, drop = FALSE]
  }
  runtime$cerebroCellViewDeferredAux <- function(
    selection_rows,
    cell_barcodes,
    hover_columns,
    hover,
    keys = TRUE
  ) {
    list(selection_key = cell_barcodes[selection_rows])
  }
  runtime$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    extra,
    deferred_aux,
    core_aux = NULL,
    dataset_context = NULL,
    feedback = NULL
  ) {
    captured$dataset_context <- dataset_context
    captured$core_aux <- core_aux
    captured$data <- data
    captured$deferred_aux <- deferred_aux
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  input <- list(
    coordinates = data.frame(x = c(3, 1, 2), y = c(6, 4, 5)),
    dataset_context = list(
      epoch = "session",
      dataset_key = "dataset-a",
      generation = 1
    ),
    reset_axes = FALSE,
    expression_levels = c(30, 10, 20),
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = TRUE,
      plot_order = "Highest expression on top",
      point_size = 1,
      point_opacity = 0.5,
      x_range = c(1, 3),
      y_range = c(4, 6),
      is_trajectory = FALSE,
      hover_info = FALSE,
      projection = "UMAP",
      n_dimensions = 2L
    ),
    color_settings = list(
      color_scale = "Viridis",
      color_mode = "same",
      color_range = NULL,
      genes = "GeneA"
    ),
    metadata = data.frame(cell_barcode = c("c3", "c1", "c2")),
    trajectory = list(),
    display_mode = "single",
    separate_panels = FALSE
  )

  runtime$expression_projection_update_plot(input)

  expect_identical(captured$data$x, c(3, 1, 2))
  expect_identical(captured$data$color, c(30, 10, 20))
  expect_identical(captured$data$selection_key, 1:3)
  expect_identical(
    as.character(captured$deferred_aux()$selection_key),
    c("c3", "c1", "c2")
  )
  expect_identical(captured$data$paint_order, "highest")
  expect_identical(captured$dataset_context, input$dataset_context)
  input$plot_parameters$plot_order <- "Random"
  runtime$expression_projection_update_plot(input)
  expect_identical(captured$data$paint_order, "natural")
})

test_that("Gene no-gene primary frame defers metadata until auxiliary data", {
  runtime <- new.env(parent = globalenv())
  captured <- new.env(parent = emptyenv())
  runtime$expressionColorScale <- function(...) "scale"
  runtime$expressionReverseColorScale <- function(...) FALSE
  runtime$getMetaData <- function() {
    data.frame(cell_barcode = c("c3", "c1", "c2"))
  }
  runtime$getGroups <- function() character()
  runtime$viewerProjectionMetadataColumns <- function(
    metadata,
    color_variable,
    hover_info,
    groups
  ) {
    "cell_barcode"
  }
  runtime$viewerProjectionSubsetRows <- function(table, indices, columns) {
    table[indices, columns, drop = FALSE]
  }
  runtime$cerebroCellViewDeferredAux <- function(
    selection_rows,
    cell_barcodes,
    hover_columns,
    hover,
    keys = TRUE
  ) {
    list(selection_key = cell_barcodes[selection_rows])
  }
  runtime$cerebroCellViewRender <- function(
    id,
    meta,
    data,
    hover,
    extra,
    deferred_aux,
    core_aux = NULL,
    dataset_context = NULL,
    feedback = NULL
  ) {
    captured$dataset_context <- dataset_context
    captured$core_aux <- core_aux
    captured$data <- data
    captured$meta <- meta
    captured$deferred_aux <- deferred_aux
  }
  sys.source(
    viewer_test_path("gene_expression", "func_projection_update_plot.R"),
    envir = runtime
  )
  input <- list(
    coordinates = data.frame(x = c(3, 1, 2), y = c(6, 4, 5)),
    reset_axes = FALSE,
    expression_levels = numeric(),
    plot_parameters = list(
      draw_border = FALSE,
      keep_square = TRUE,
      plot_order = "Highest expression on top",
      point_size = 1,
      point_opacity = 0.5,
      x_range = c(1, 3),
      y_range = c(4, 6),
      is_trajectory = FALSE,
      hover_info = FALSE,
      projection = "UMAP",
      n_dimensions = 2L
    ),
    color_settings = list(
      color_scale = "Viridis",
      color_mode = "same",
      color_range = c(0, 1),
      genes = character()
    ),
    metadata = NULL,
    trajectory = list(),
    display_mode = "single",
    cell_indices = 1:3,
    separate_panels = FALSE
  )

  runtime$expression_projection_update_plot(input)

  expect_identical(captured$data$selection_key, 1:3)
  expect_true(captured$data$shared_zero_color)
  expect_identical(captured$meta$color_variable, "UMAP")
  expect_false(captured$meta$show_color_scale)
  expect_identical(
    as.character(captured$deferred_aux()$selection_key),
    c("c3", "c1", "c2")
  )
})

test_that("the 1M preparation uses CEREBRO_LARGE_CACHE like the Ren one", {
  script <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_1m_data.R"
  )
  skip_if_not(
    file.exists(script),
    "demo tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)

  withr::local_envvar(CEREBRO_LARGE_CACHE = "/data/large")
  expect_identical(
    env$.viewer1mPaths()$crb,
    "/data/large/1m/cerebro-v3/cerebro_mouse_brain_1m.crb"
  )
  ## an explicit argument still wins
  expect_identical(
    env$.viewer1mPaths("/other")$source,
    "/other/1m/source/1M_neurons_filtered_gene_bc_matrices_h5.h5"
  )
})

test_that("a mouse brain built by an earlier release is not taken for the current one", {
  script <- testthat::test_path(
    "..",
    "..",
    "data-raw",
    "large-demo",
    "prepare_viewer_1m_data.R"
  )
  skip_if_not(
    file.exists(script),
    "demo tree not present (expected when checking a built package)"
  )
  env <- new.env(parent = globalenv())
  sys.source(script, envir = env)
  root <- withr::local_tempdir()
  paths <- env$.viewer1mPaths(root)
  complete <- function() {
    env$.viewer1mComplete(paths$crb, paths$sidecar, paths$stamp)
  }

  ## version 1 kept its files in `cerebro`, without a stamp
  old <- file.path(root, "1m", "cerebro")
  dir.create(old, recursive = TRUE)
  file.create(file.path(
    old,
    c("cerebro_mouse_brain_1m.crb", "cerebro_mouse_brain_1m.h5")
  ))
  expect_false(identical(dirname(paths$crb), old))
  expect_false(complete())

  ## files without the stamp, or with another version, are not finished
  dir.create(dirname(paths$crb), recursive = TRUE)
  file.create(c(paths$crb, paths$sidecar))
  expect_false(complete())
  writeLines("1", paths$stamp)
  expect_false(complete())
  writeLines(as.character(env$.viewer1mVersion), paths$stamp)
  expect_true(complete())
  file.remove(paths$sidecar)
  expect_false(complete())
})
