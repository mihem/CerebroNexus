test_that("the 1M demo is opt-in and validates its sidecar", {
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
  dir.create(file.path(root, "mouse.bpcells"))
  file.create(file.path(root, "mouse.bpcells", "shape"))

  configured <- env$viewerAddMillionCellDemo(original, crb)
  label <- "10x E18 mouse brain (1M)"
  expect_identical(
    unname(configured$crb_file_to_load[label]),
    normalizePath(crb)
  )
  expect_equal(unname(configured$point_size[label]), 1)
  expect_equal(unname(configured$point_opacity[label]), 0.5)
  expect_equal(unname(configured$percentage_cells_to_show[label]), 100)
  expect_equal(unname(configured$expression_point_size[label]), 2)
  expect_equal(unname(configured$expression_point_opacity[label]), 1)

  unlink(file.path(root, "mouse.bpcells", "shape"))
  expect_error(
    env$viewerAddMillionCellDemo(original, crb),
    "adjacent non-empty .bpcells"
  )
})

test_that("the 1M preparation keeps unique gene symbols", {
  script <- testthat::test_path("..", "bench", "prepare_viewer_1m_data.R")
  skip_if_not(
    file.exists(script),
    "benchmark tree not present (expected when checking a built package)"
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

test_that("the 1M demo replaces the illustrative path with marker guidance", {
  script <- testthat::test_path("..", "bench", "prepare_viewer_1m_data.R")
  skip_if_not(
    file.exists(script),
    "benchmark tree not present (expected when checking a built package)"
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
  object$trajectories <- list(
    illustrative = list(UMAP_cluster_path = list())
  )
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
  expect_false("illustrative" %in% names(object$trajectories))
  expect_named(object$trajectories$marker_guided, "E18_neurogenesis")
  expect_false(env$.viewer1mEnrichDemoObject(object))
})

test_that("the Viewer filters methods at trajectory selection boundaries", {
  files <- c(
    viewer_test_path("shiny_server.R"),
    viewer_test_path("trajectory", "select_method_and_name.R")
  )
  for (file in files) {
    expect_match(
      paste(readLines(file, warn = FALSE), collapse = "\n"),
      "viewerSupportedTrajectoryMethods(",
      fixed = TRUE,
      info = file
    )
  }
  utility <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility)
  expect_identical(
    utility$viewerSupportedTrajectoryMethods(
      c("unsupported", "marker_guided", "monocle2", "illustrative")
    ),
    c("marker_guided", "monocle2", "illustrative")
  )
})

test_that("optional Viewer tabs use compact catalog capabilities", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  fields <- c(
    markerGenes = "marker_genes",
    mostExpressedGenes = "most_expressed_genes",
    enrichedPathways = "enriched_pathways",
    extra_material = "extra_material",
    trajectory = "trajectory",
    spatial = "spatial",
    trekker = "trekker"
  )

  for (tab in names(fields)) {
    expect_match(
      server,
      sprintf(
        'toggleConditionalTab\\([\\s\\S]{0,300}"%s"[\\s\\S]{0,300}catalog_field = "%s"',
        tab,
        fields[[tab]]
      ),
      perl = TRUE,
      info = tab
    )
  }
})

test_that("Viewer spatial readers leave molecule sidecars dormant", {
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )
  linked <- paste(
    readLines(
      viewer_test_path("coordinated_views", "bundle.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    utility,
    'getter(name, hydrate_molecules = FALSE)',
    fixed = TRUE
  )
  expect_match(
    linked,
    'crb$getSpatialData(nm, hydrate_molecules = FALSE)',
    fixed = TRUE
  )

  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = scope)
  legacy <- new.env(parent = emptyenv())
  class(legacy) <- c("Cerebro", "R6")
  legacy$spatial <- list(
    slice = list(
      coordinates = data.frame(x = 1:2, y = 3:4),
      expression = matrix(numeric(), nrow = 0L, ncol = 2L)
    )
  )
  legacy$getSpatialData <- function(name) legacy$spatial[[name]]
  scope$data_set <- function() legacy

  expect_identical(
    scope$getSpatialData("slice")$coordinates,
    legacy$spatial$slice$coordinates
  )
})

test_that("million-cell hover stays columnar until the browser needs it", {
  utility <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = utility)
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

test_that("specialist pages do not request the full linked bundle", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(engine, "function singlePayloadBundle", fixed = TRUE)
  expect_match(engine, "var vis = linkedVis;", fixed = TRUE)
  expect_no_match(engine, "linkedVis || !!singleId", fixed = TRUE)
})

test_that("specialist pages bind every render request to a dataset context", {
  pages <- list(
    list(
      c("overview", "event_projection_update_plot.R"),
      "overview_projection_render_request"
    ),
    list(
      c("gene_expression", "event_projection_update_plot.R"),
      "expression_projection_render_request"
    ),
    list(
      c("spatial", "event_projection_update_plot.R"),
      "spatial_projection_render_request"
    ),
    list(
      c("trajectory", "projection_plot.R"),
      "trajectory_projection_render_request"
    )
  )

  for (page in pages) {
    source <- paste(
      readLines(do.call(viewer_test_path, as.list(page[[1]])), warn = FALSE),
      collapse = "\n"
    )
    expect_match(
      source,
      sprintf('input[["%s"]]', page[[2]]),
      fixed = TRUE,
      info = page[[2]]
    )
  }

  hla <- paste(
    readLines(
      viewer_test_path("hla_tcr_motifs", "visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    hla,
    'render_request <- input[["hla_motif_network_render_request"]]',
    fixed = TRUE
  )
  expect_match(hla, "req(render_request)", fixed = TRUE)
})

test_that("specialist tab clicks request the primary frame without polling", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(engine, "function requestSingleView(id)", fixed = TRUE)
  expect_match(engine, "function tabSingleViewId(link)", fixed = TRUE)
  expect_match(
    engine,
    "requestSingleView(tabSingleViewId(tabLink));",
    fixed = TRUE
  )
  expect_match(engine, "setTimeout(function () {", fixed = TRUE)
  expect_match(engine, "requestSingleView(singleId);", fixed = TRUE)
  expect_match(engine, "setInterval(reportVisibility, 250);", fixed = TRUE)
})

test_that("Spatial primary rendering ignores shared-base writeback", {
  source <- paste(
    readLines(
      viewer_test_path("spatial", "event_projection_update_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    source,
    "isolate(spatial_projection_update_plot(data))",
    fixed = TRUE
  )
})

test_that("specialist page switches restore panels without staggered fades", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    engine,
    "if (reappearing && !singleActive) fadeInPane(p.pane);",
    fixed = TRUE
  )
})

test_that("specialist pages use binary transport when the browser supports it", {
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(utility, "cv_wire_pack_message", fixed = TRUE)
  expect_match(utility, '"cell_view_binary"', fixed = TRUE)
  expect_match(engine, "'cell_view_binary'", fixed = TRUE)
  expect_match(engine, "!ArrayBuffer.isView(data.color)", fixed = TRUE)
})

test_that("canonical projections use validated static assets with wire fallback", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  overview <- paste(
    c(
      readLines(
        viewer_test_path("overview", "func_projection_update_plot.R"),
        warn = FALSE
      ),
      readLines(
        viewer_test_path("overview", "event_projection_update_plot.R"),
        warn = FALSE
      ),
      readLines(
        viewer_test_path("overview", "obj_projection_data_to_plot.R"),
        warn = FALSE
      )
    ),
    collapse = "\n"
  )
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, "viewerProjectionAsset <- function", fixed = TRUE)
  expect_match(server, "viewerStaticResourcePrefix <- function", fixed = TRUE)
  expect_match(
    server,
    "viewerCanonicalResourceIdentity <- function",
    fixed = TRUE
  )
  expect_equal(
    lengths(gregexpr("shiny::addResourcePath", server, fixed = TRUE)),
    1L
  )
  expect_equal(
    lengths(gregexpr("shiny::removeResourcePath", server, fixed = TRUE)),
    1L
  )
  expect_equal(
    lengths(gregexpr(
      "viewerCanonicalResourceIdentity(pack)",
      server,
      fixed = TRUE
    )),
    4L
  )
  expect_no_match(server, "viewer_projection_prefixes", fixed = TRUE)
  expect_no_match(server, "viewer_metadata_prefixes", fixed = TRUE)
  expect_no_match(server, "viewer_trajectory_prefixes", fixed = TRUE)
  expect_match(server, "tools::md5sum(file)", fixed = TRUE)
  expect_match(server, "shiny::addResourcePath", fixed = TRUE)
  expect_match(server, "shiny::removeResourcePath", fixed = TRUE)
  expect_match(
    overview,
    'input[["projection_resource"]]',
    fixed = TRUE
  )
  expect_match(
    overview,
    "overview_projection_projection_resource_failed",
    fixed = TRUE
  )
  expect_match(engine, "hydrateSingleProjectionResource", fixed = TRUE)
  expect_match(engine, "fetchValidatedProjectionResource", fixed = TRUE)
  expect_match(engine, "response.arrayBuffer()", fixed = TRUE)
  expect_match(engine, "projection_resource_failed", fixed = TRUE)
  parameters <- paste(
    readLines(
      viewer_test_path("overview", "obj_projection_parameters_plot.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    parameters,
    "projection_resource <- viewerProjectionAsset(",
    fixed = TRUE
  )
  expect_match(
    parameters,
    "XYranges <- if (is.null(projection_data)) NULL else getXYranges",
    fixed = TRUE
  )
  expect_match(server, "viewerProjectionCatalog <- function", fixed = TRUE)
  expect_match(server, 'id = "overview_projection"', fixed = TRUE)
  expect_match(
    overview,
    "resource_coordinates = resource_coordinates",
    fixed = TRUE
  )
  expect_match(overview, "categorical_resource", fixed = TRUE)
  expect_match(server, "category_resources", fixed = TRUE)
  expect_match(engine, "prefetchRegisteredSingleCategory", fixed = TRUE)
  expect_match(
    engine,
    "groupResource.protocol === 'canonical-metadata-codes-v1'",
    fixed = TRUE
  )
})

test_that("canonical projection fetches enforce the full dataset identity", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  source <- viewer_test_path("www", "cell_views.js")
  state_source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      sprintf(
        "global.window = {}; eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_source, quote = '"')
      ),
      "const start = source.indexOf('  function canonicalResourceIdentityMatches');",
      "const end = source.indexOf('  function fetchProjectionSubsetResource', start);",
      "let calls = 0;",
      "function fetchProjectionResource(resource, expectedCells) {",
      "  calls += 1;",
      "  if (resource.url === 'broken.bin') {",
      "    return Promise.reject(new Error('resource fetch failed'));",
      "  }",
      "  return Promise.resolve({url:resource.url, cells:expectedCells});",
      "}",
      "eval(source.slice(start, end));",
      "const fingerprints = {",
      "  dataset:'md5-cell-set-v1:' + 'a'.repeat(32),",
      "  order:'md5-cell-order-v1:' + 'b'.repeat(32),",
      "  pack:'md5-crb-v1:' + 'c'.repeat(32)",
      "};",
      "const message = {dataset_identity:{",
      "  cell_fingerprint:fingerprints.dataset,",
      "  cell_order_fingerprint:fingerprints.order,",
      "  pack_dataset_fingerprint:fingerprints.pack,",
      "  cell_count:3",
      "}};",
      "const canonical = {",
      "  protocol:'canonical-projection-v1', dtype:'float32',",
      "  url:'canonical.bin', cells:3,",
      "  dataset_fingerprint:fingerprints.dataset,",
      "  cell_order_fingerprint:fingerprints.order,",
      "  pack_dataset_fingerprint:fingerprints.pack",
      "};",
      "async function rejected(resource) {",
      "  try {",
      "    await fetchValidatedProjectionResource(resource, message, 3);",
      "    return false;",
      "  } catch (error) {",
      "    return error.message === 'Canonical projection identity mismatch';",
      "  }",
      "}",
      "(async () => {",
      "  const canonicalResult = await fetchValidatedProjectionResource(canonical, message, 3);",
      "  const legacyResult = await fetchValidatedProjectionResource({url:'legacy.bin'}, message, 3);",
      "  const datasetMismatch = await rejected({...canonical, dataset_fingerprint:'md5-cell-set-v1:' + 'd'.repeat(32)});",
      "  const orderMismatch = await rejected({...canonical, cell_order_fingerprint:'md5-cell-order-v1:' + 'd'.repeat(32)});",
      "  const packMismatch = await rejected({...canonical, pack_dataset_fingerprint:'md5-crb-v1:' + 'd'.repeat(32)});",
      "  const countMismatch = await rejected({...canonical, cells:2});",
      "  let fetchFailurePreserved = false;",
      "  try {",
      "    await fetchValidatedProjectionResource({...canonical, url:'broken.bin'}, message, 3);",
      "  } catch (error) {",
      "    fetchFailurePreserved = error.message === 'resource fetch failed';",
      "  }",
      "  console.log(JSON.stringify({calls, canonicalResult, legacyResult, datasetMismatch, orderMismatch, packMismatch, countMismatch, fetchFailurePreserved}));",
      "})().catch(error => { console.error(error); process.exit(1); });"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      calls = 3L,
      canonicalResult = list(url = "canonical.bin", cells = 3L),
      legacyResult = list(url = "legacy.bin", cells = 3L),
      datasetMismatch = TRUE,
      orderMismatch = TRUE,
      packMismatch = TRUE,
      countMismatch = TRUE,
      fetchFailurePreserved = TRUE
    )
  )
})

test_that("spatial geometry fetches require exact dataset identity", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  source <- viewer_test_path("www", "cell_views.js")
  state_source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      sprintf(
        "global.window = {}; eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_source, quote = '"')
      ),
      "const start = source.indexOf('  function canonicalResourceIdentityMatches');",
      "const end = source.indexOf('  function fetchProjectionSubsetResource', start);",
      "let calls = 0;",
      "function fetchProjectionResource(resource, expectedCells) {",
      "  calls += 1; return Promise.resolve({url:resource.url, cells:expectedCells});",
      "}",
      "eval(source.slice(start, end));",
      "const identity = {",
      "  cell_count:5,",
      "  cell_fingerprint:'md5-cell-set-v1:' + 'a'.repeat(32),",
      "  cell_order_fingerprint:'md5-cell-order-v1:' + 'b'.repeat(32),",
      "  pack_dataset_fingerprint:'md5-crb-v1:' + 'c'.repeat(32)",
      "};",
      "const message = {dataset_identity:identity};",
      "const resource = {",
      "  protocol:'spatial-geometry-v1', spatial_name:'slice',",
      "  url:'spatial.bin', cells:3, dimensions:2, dtype:'float32',",
      "  dataset_fingerprint:identity.cell_fingerprint,",
      "  cell_order_fingerprint:identity.cell_order_fingerprint,",
      "  pack_dataset_fingerprint:identity.pack_dataset_fingerprint",
      "};",
      "async function rejected(value, cells) {",
      "  try { await fetchValidatedSingleGeometryResource(value, message, cells); return false; }",
      "  catch (error) { return error.message === 'Spatial geometry identity mismatch'; }",
      "}",
      "(async () => {",
      "  const valid = await fetchValidatedSingleGeometryResource(resource, message, 3);",
      "  const datasetMismatch = await rejected({...resource, dataset_fingerprint:'md5-cell-set-v1:' + 'd'.repeat(32)}, 3);",
      "  const localMismatch = await rejected(resource, 2);",
      "  const shapeMismatch = await rejected({...resource, dimensions:3}, 3);",
      "  console.log(JSON.stringify({calls, valid, datasetMismatch, localMismatch, shapeMismatch}));",
      "})().catch(error => { console.error(error); process.exit(1); });"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      calls = 1L,
      valid = list(url = "spatial.bin", cells = 3L),
      datasetMismatch = TRUE,
      localMismatch = TRUE,
      shapeMismatch = TRUE
    )
  )
})

test_that("canonical subsets bind projection and local cell counts", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  source <- viewer_test_path("www", "cell_views.js")
  state_source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const start = source.indexOf('  function resourceCacheKey');",
      "const end = source.indexOf('  function materializeProjectionSubset', start);",
      "let calls = 0;",
      "var projectionSubsetResourceCache = new Map();",
      "global.window = {fetch:async () => {",
      "  calls += 1;",
      "  return {ok:true, arrayBuffer:async () => new Uint32Array([0, 2]).buffer};",
      "}};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_source, quote = '"')
      ),
      "eval(source.slice(start, end));",
      "const include = {protocol:'canonical-subset-v1', kind:'include_uint32', canonical_cells:3, cells:2, index_base:0, dtype:'uint32', url:'subset.bin', bytes:8, checksum:'abc'};",
      "const identity = {protocol:'canonical-subset-v1', kind:'identity', canonical_cells:3, cells:3, index_base:0, dtype:'uint32', bytes:0, checksum:''};",
      "async function rejected(resource, canonicalCells, cells) {",
      "  try {",
      "    await fetchProjectionSubsetResource(resource, canonicalCells, cells);",
      "    return false;",
      "  } catch (error) {",
      "    return error.message === 'Projection subset contract mismatch';",
      "  }",
      "}",
      "(async () => {",
      "  const canonicalMismatch = await rejected(include, 4, 2);",
      "  const localMismatch = await rejected(include, 3, 1);",
      "  const valid = await fetchProjectionSubsetResource(include, 3, 2);",
      "  const cached = await fetchProjectionSubsetResource(include, 3, 2);",
      "  const identityResult = await fetchProjectionSubsetResource(identity, 3, 3);",
      "  console.log(JSON.stringify({calls, canonicalMismatch, localMismatch, validKind:valid.kind, validIndices:Array.from(valid.indices), cachedHit:cached.cacheHit, identityCacheHit:identityResult.cacheHit}));",
      "})().catch(error => { console.error(error); process.exit(1); });"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      calls = 1L,
      canonicalMismatch = TRUE,
      localMismatch = TRUE,
      validKind = "include_uint32",
      validIndices = list(0L, 2L),
      cachedHit = TRUE,
      identityCacheHit = TRUE
    )
  )
})

test_that("trajectory frame resources bind every static asset to dataset identity", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  source <- viewer_test_path("www", "cell_views.js")
  state_source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const start = source.indexOf('  function canonicalResourceIdentityMatches');",
      "const end = source.indexOf('  function hydrateSingleProjectionResource', start);",
      "var projectionSubsetResourceCache = new Map();",
      "function fetchProjectionResource() {}",
      "function cacheSharedProjection() {}",
      "global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_source, quote = '"')
      ),
      "window.CBViewState.sharedProjection = () => null;",
      "function sharedBase() { return {}; }",
      "eval(source.slice(start, end));",
      "const identity = {",
      "  cell_count:3,",
      "  cell_fingerprint:'md5-cell-set-v1:' + 'a'.repeat(32),",
      "  cell_order_fingerprint:'md5-cell-order-v1:' + 'b'.repeat(32),",
      "  pack_dataset_fingerprint:'md5-crb-v1:' + 'c'.repeat(32)",
      "};",
      "const projection = {",
      "  protocol:'canonical-projection-v1', projection_name:'UMAP',",
      "  url:'projection.bin', cells:3, dtype:'float32',",
      "  dataset_fingerprint:identity.cell_fingerprint,",
      "  cell_order_fingerprint:identity.cell_order_fingerprint,",
      "  pack_dataset_fingerprint:identity.pack_dataset_fingerprint",
      "};",
      "const frame = {",
      "  protocol:'trajectory-frame-v1', cells:2, canonical_cells:3,",
      "  geometry_kind:'canonical_projection', projection:projection,",
      "  subset:{protocol:'canonical-subset-v1', kind:'exclude_uint32'},",
      "  state:{url:'state.bin', cells:2, dtype:'uint8'},",
      "  dataset_fingerprint:identity.cell_fingerprint,",
      "  cell_order_fingerprint:identity.cell_order_fingerprint,",
      "  pack_dataset_fingerprint:identity.pack_dataset_fingerprint",
      "};",
      "const message = {dataset_identity:identity};",
      "function rejected(value, expectedCells) {",
      "  try { trajectoryFrameResources(value, message, expectedCells); return false; }",
      "  catch (error) { return /Trajectory/.test(error.message); }",
      "}",
      "const valid = trajectoryFrameResources(frame, message, 2);",
      "console.log(JSON.stringify({",
      "  validProjection:valid.projection.url,",
      "  validState:valid.state.url,",
      "  datasetMismatch:rejected({...frame, dataset_fingerprint:'md5-cell-set-v1:' + 'd'.repeat(32)}, 2),",
      "  canonicalMismatch:rejected({...frame, canonical_cells:4}, 2),",
      "  localMismatch:rejected(frame, 1),",
      "  missingSubset:rejected({...frame, subset:null}, 2),",
      "  projectionMismatch:rejected({...frame, projection:{...projection, cell_order_fingerprint:'md5-cell-order-v1:' + 'd'.repeat(32)}}, 2)",
      "}));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      validProjection = "projection.bin",
      validState = "state.bin",
      datasetMismatch = TRUE,
      canonicalMismatch = TRUE,
      localMismatch = TRUE,
      missingSubset = TRUE,
      projectionMismatch = TRUE
    )
  )
})

test_that("Linked projection hydration uses the primary bundle identity", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  source <- viewer_test_path("www", "cell_views.js")
  state_source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const identityStart = source.indexOf('  function canonicalResourceIdentityMatches');",
      "const identityEnd = source.indexOf('  function fetchProjectionSubsetResource', identityStart);",
      "const hydrateStart = source.indexOf('  function linkedBundleResourceIdentity');",
      "const hydrateEnd = source.indexOf('  var metadataCodesResourceCache', hydrateStart);",
      "let calls = 0;",
      "function fetchProjectionResource(resource, expectedCells) {",
      "  calls += 1;",
      "  return Promise.resolve({coordinates:{x:new Float32Array(expectedCells), y:new Float32Array(expectedCells), bytes:24}, fetchMs:1, downloadMs:1, decodeMs:1});",
      "}",
      sprintf(
        "global.window = {}; eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_source, quote = '"')
      ),
      "eval(source.slice(identityStart, identityEnd));",
      "eval(source.slice(hydrateStart, hydrateEnd));",
      "const fingerprints = {",
      "  dataset:'md5-cell-set-v1:' + 'a'.repeat(32),",
      "  order:'md5-cell-order-v1:' + 'b'.repeat(32),",
      "  pack:'md5-crb-v1:' + 'c'.repeat(32)",
      "};",
      "function bundle(resource) { return {",
      "  n:3, default_projection:'UMAP',",
      "  dataset_fingerprint:fingerprints.dataset,",
      "  canonical_order_id:fingerprints.order,",
      "  pack_dataset_fingerprint:fingerprints.pack,",
      "  projections:{UMAP:{projection_resource:resource}}",
      "}; }",
      "const resource = {",
      "  protocol:'canonical-projection-v1', dtype:'float32',",
      "  url:'projection.bin', cells:3, projection_name:'UMAP',",
      "  dataset_fingerprint:fingerprints.dataset,",
      "  cell_order_fingerprint:fingerprints.order,",
      "  pack_dataset_fingerprint:fingerprints.pack",
      "};",
      "async function rejected(value) {",
      "  try {",
      "    await hydrateLinkedProjectionResource(bundle(value));",
      "    return false;",
      "  } catch (error) {",
      "    return error.message === 'Canonical projection identity mismatch';",
      "  }",
      "}",
      "(async () => {",
      "  const canonical = await hydrateLinkedProjectionResource(bundle(resource));",
      "  const legacy = await hydrateLinkedProjectionResource(bundle({url:'legacy.bin', cells:3, dtype:'float32'}));",
      "  const datasetMismatch = await rejected({...resource, dataset_fingerprint:'md5-cell-set-v1:' + 'd'.repeat(32)});",
      "  const orderMismatch = await rejected({...resource, cell_order_fingerprint:'md5-cell-order-v1:' + 'd'.repeat(32)});",
      "  const packMismatch = await rejected({...resource, pack_dataset_fingerprint:'md5-crb-v1:' + 'd'.repeat(32)});",
      "  console.log(JSON.stringify({calls, canonicalBytes:canonical.projectionBytes, legacyBytes:legacy.projectionBytes, datasetMismatch, orderMismatch, packMismatch}));",
      "})().catch(error => { console.error(error); process.exit(1); });"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      calls = 2L,
      canonicalBytes = 24L,
      legacyBytes = 24L,
      datasetMismatch = TRUE,
      orderMismatch = TRUE,
      packMismatch = TRUE
    )
  )
})

test_that("Linked canonical metadata validates its dataset identity", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  skip_if_not_installed("jsonlite")
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(server, 'protocol = "canonical-metadata-codes-v1"', fixed = TRUE)
  expect_match(server, "dataset_fingerprint = as.character", fixed = TRUE)
  expect_match(server, "cell_order_fingerprint = as.character", fixed = TRUE)
  expect_match(server, "pack_dataset_fingerprint = as.character", fixed = TRUE)

  source <- viewer_test_path("www", "cell_views.js")
  state_source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const identityStart = source.indexOf('  function canonicalResourceIdentityMatches');",
      "const identityEnd = source.indexOf('  function canonicalProjectionIdentityMatches', identityStart);",
      "const fetchStart = source.indexOf('  function fetchValidatedMetadataCodesResource');",
      "const fetchEnd = source.indexOf('  function hydrateLinkedMetadataResource', fetchStart);",
      "let calls = 0;",
      "function fetchMetadataCodesResource(resource, expectedCells) {",
      "  calls += 1;",
      "  return Promise.resolve({url:resource.url, cells:expectedCells});",
      "}",
      sprintf(
        "global.window = {}; eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(state_source, quote = '"')
      ),
      "eval(source.slice(identityStart, identityEnd));",
      "eval(source.slice(fetchStart, fetchEnd));",
      "const identity = {",
      "  cell_count:3,",
      "  cell_fingerprint:'md5-cell-set-v1:' + 'a'.repeat(32),",
      "  cell_order_fingerprint:'md5-cell-order-v1:' + 'b'.repeat(32),",
      "  pack_dataset_fingerprint:'md5-crb-v1:' + 'c'.repeat(32)",
      "};",
      "const resource = {",
      "  protocol:'canonical-metadata-codes-v1', url:'metadata.bin',",
      "  cells:3, dtype:'uint8',",
      "  dataset_fingerprint:identity.cell_fingerprint,",
      "  cell_order_fingerprint:identity.cell_order_fingerprint,",
      "  pack_dataset_fingerprint:identity.pack_dataset_fingerprint",
      "};",
      "async function rejected(value) {",
      "  try {",
      "    await fetchValidatedMetadataCodesResource(value, 3, identity);",
      "    return false;",
      "  } catch (error) {",
      "    return error.message === 'Canonical metadata identity mismatch';",
      "  }",
      "}",
      "(async () => {",
      "  const canonical = await fetchValidatedMetadataCodesResource(resource, 3, identity);",
      "  const legacy = await fetchValidatedMetadataCodesResource({url:'legacy.bin'}, 3, identity);",
      "  const datasetMismatch = await rejected({...resource, dataset_fingerprint:'md5-cell-set-v1:' + 'd'.repeat(32)});",
      "  const orderMismatch = await rejected({...resource, cell_order_fingerprint:'md5-cell-order-v1:' + 'd'.repeat(32)});",
      "  const packMismatch = await rejected({...resource, pack_dataset_fingerprint:'md5-crb-v1:' + 'd'.repeat(32)});",
      "  const countMismatch = await rejected({...resource, cells:2});",
      "  console.log(JSON.stringify({calls, canonical, legacy, datasetMismatch, orderMismatch, packMismatch, countMismatch}));",
      "})().catch(error => { console.error(error); process.exit(1); });"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      calls = 2L,
      canonical = list(url = "metadata.bin", cells = 3L),
      legacy = list(url = "legacy.bin", cells = 3L),
      datasetMismatch = TRUE,
      orderMismatch = TRUE,
      packMismatch = TRUE,
      countMismatch = TRUE
    )
  )
})

test_that("trajectory first frames expose paired static geometry and state", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, "viewerTrajectoryFrameAsset <- function", fixed = TRUE)
  expect_match(
    server,
    "viewerPackTrajectoryFrame(pack, method, name)",
    fixed = TRUE
  )
  expect_match(
    server,
    'viewerStaticResourcePrefix(\n      "trajectory"',
    fixed = TRUE
  )
  expect_match(engine, "fetchCategoricalResource", fixed = TRUE)
  expect_match(
    engine,
    "data.canonical_group = groupResult.values",
    fixed = TRUE
  )
  expect_match(engine, "categorical_resource", fixed = TRUE)
})

test_that("Gene projection streams canonical geometry with wire fallback", {
  data_source <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "obj_projection_data_to_plot.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  render_source <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "func_projection_update_plot.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  event_source <- paste(
    readLines(
      viewer_test_path(
        "gene_expression",
        "event_projection_update_plot.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    data_source,
    "expression_projection_projection_resource_failed",
    fixed = TRUE
  )
  expect_match(
    data_source,
    "metadata <- NULL",
    fixed = TRUE
  )
  expect_no_match(data_source, "expression_projection_data()", fixed = TRUE)
  expect_match(render_source, "viewerProjectionAsset(", fixed = TRUE)
  expect_match(render_source, "viewerSharedProjectionName(", fixed = TRUE)
  expect_match(
    event_source,
    "isolate(expression_projection_update_plot(data))",
    fixed = TRUE
  )
  expect_match(render_source, "projection_resource", fixed = TRUE)
  expect_match(render_source, "failed_asset", fixed = TRUE)
  expect_match(render_source, "output_data[[\"x\"]] <- NULL", fixed = TRUE)
  expect_match(render_source, "output_data[[\"y\"]] <- NULL", fixed = TRUE)
})

test_that("specialist bundles carry the saved dataset fingerprint", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const start = source.indexOf('  function singlePayloadCells');",
      "const end = source.indexOf('  function alignSingleCoordinates', start);",
      "global.window = {cerebroSavedViewDataset:{cell_fingerprint:'stale'}};",
      "eval(source.slice(start, end));",
      "const payload = {dataset_context:{epoch:'test',dataset_key:'a',generation:1},datasetIdentity:{cell_fingerprint:'md5-cell-set-v1:0123456789abcdef0123456789abcdef'},data:{n:2,selection_key:['c1','c2']}};",
      "const present = singlePayloadBundle('overview', payload).dataset_fingerprint;",
      "delete window.cerebroSavedViewDataset;",
      "const missing = singlePayloadBundle('overview', {data:payload.data,dataset_context:payload.dataset_context}).dataset_fingerprint;",
      "console.log(JSON.stringify({present:present,missing:missing}));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      present = "md5-cell-set-v1:0123456789abcdef0123456789abcdef",
      missing = ""
    )
  )
})

test_that("sparse linked attributes expand into typed canonical vectors", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      "global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(source, quote = '"')
      ),
      "const values = window.CBViewState.expandSparse(",
      "  new Int32Array([1, 4]), new Int16Array([7, 9]), 6, -1);",
      "console.log(JSON.stringify(Array.from(values)));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output),
    c(-1L, 7L, -1L, -1L, 9L, -1L)
  )
})

test_that("shared browser base reuses only matching dataset projections", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs'); global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(source, quote = '"')
      ),
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
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
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

test_that("specialist lifecycle owns request, ready, and auxiliary state", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs'); global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(source, quote = '"')
      ),
      "const L = window.CBViewState.specialistLifecycle, timings = {};",
      "const first = L.begin(timings, 'spatial', true); first.requestAtMs = 10;",
      "L.update(timings, 'spatial', {bytes: 42});",
      "L.activate(timings, 'spatial', 20);",
      "const ready = L.ready(timings, 'spatial', 35);",
      "const view = {data:{wire_token:7}};",
      "const requested = L.requestAux(view);",
      "const duplicate = L.requestAux(view);",
      "const rejected = L.acceptAux(view, {wire_token:8});",
      "const accepted = L.acceptAux(view, {wire_token:7});",
      "const cached = L.begin(timings, 'spatial', false);",
      "console.log(JSON.stringify({generation:cached.generation,",
      "cached:cached.cached,bytes:ready.bytes,latency:ready.requestToReadyMs,",
      "activation:ready.activationStartedAtMs,requested,duplicate,rejected,accepted,",
      "pending:view._auxPending,token:view._auxToken}));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(
      generation = 2L,
      cached = TRUE,
      bytes = 42L,
      latency = 25L,
      activation = 20L,
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
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs'); global.window = {};",
      sprintf(
        "eval(fs.readFileSync(%s, 'utf8'));",
        encodeString(source, quote = '"')
      ),
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
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
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
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs=require('fs'); global.window={};",
      sprintf(
        "eval(fs.readFileSync(%s,'utf8'));",
        encodeString(source, quote = '"')
      ),
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
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
  expect_equal(attr(output, "status"), NULL)
  expect_identical(
    jsonlite::fromJSON(output, simplifyVector = FALSE),
    list(valid = TRUE, bytes = 32L, shape = FALSE, identity = FALSE, width = 2L)
  )
})

test_that("telemetry snapshots normalize shared transport phases", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views_state.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs=require('fs'); global.window={};",
      sprintf(
        "eval(fs.readFileSync(%s,'utf8'));",
        encodeString(source, quote = '"')
      ),
      "const T=window.CBViewState.telemetry;",
      "const metric=T.transport({server_prepare_ms:'12.5',",
      "server_resource_ms:'3',server_bundle_ms:'bad',sent_at_ms:900},",
      "{bytes:42,decodeMs:4,requestToBinaryMs:8},true,1000);",
      "const snapshot=T.snapshot(metric,{decodeToDrawMs:6});",
      "console.log(JSON.stringify(snapshot));"
    ),
    runner
  )

  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)
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

test_that("decorated 2-D cell views remain eligible for WebGPU", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function gpuCandidate\\(p\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "const GPU_MIN_CELLS = 4096, D = {n: 1000000};",
      "const spaceById = {",
      "trajectory: {_unit: {nz: false}, trajectory: true},",
      "spatial: {_unit: {nz: false}, background_scope: 'spatial',",
      "  _axisSpec: {}, hulls: [{x:[0,1,0],y:[0,0,1]}]},",
      "sized: {_unit: {nz: false}, pointSizes: new Float32Array(1000000)},",
      "threeD: {_unit: {nz: true}}};",
      "eval(fn);",
      "if (!gpuCandidate({gpu: {}, spaceId: 'trajectory'})) process.exit(1);",
      "if (!gpuCandidate({gpu: {}, spaceId: 'spatial'})) process.exit(2);",
      "if (gpuCandidate({gpu: {}, spaceId: 'sized'})) process.exit(3);",
      "if (gpuCandidate({gpu: {}, spaceId: 'threeD'})) process.exit(4);"
    ),
    runner
  )

  expect_identical(system2("node", runner), 0L)
})

test_that("GPU cell views split underlay, points and interaction overlay", {
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  stylesheet <- paste(
    readLines(viewer_test_path("www", "coordviews.css"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(javascript, "function attachUnderlay(p)", fixed = TRUE)
  expect_match(javascript, "function attachInteractionLayer(p)", fixed = TRUE)
  expect_match(javascript, "function drawHoverOverlay(p)", fixed = TRUE)
  expect_match(javascript, "drawHoverAll();", fixed = TRUE)
  expect_no_match(
    javascript,
    "hoverCell = i;\n    drawAll();",
    fixed = TRUE
  )
  expect_match(
    javascript,
    "if (needsUnderlay(p)) attachUnderlay(p)",
    fixed = TRUE
  )
  expect_match(javascript, "drawImage(p, underlay)", fixed = TRUE)
  expect_match(javascript, "drawHulls(p, underlay)", fixed = TRUE)
  expect_match(javascript, "drawTrajectory(p, underlay)", fixed = TRUE)
  expect_match(javascript, "panel.underlayCanvas", fixed = TRUE)
  expect_match(javascript, "function resizePanelLayers", fixed = TRUE)
  expect_match(javascript, "function setGpuLayerVisible", fixed = TRUE)
  expect_match(javascript, "function exportPanelLayers", fixed = TRUE)
  expect_no_match(javascript, "cv-gpu-overlay", fixed = TRUE)
  expect_match(stylesheet, "canvas.cv-underlay-layer", fixed = TRUE)
  expect_match(stylesheet, "canvas.cv-interaction-layer", fixed = TRUE)
  expect_match(stylesheet, "canvas.cv-gpu-layer { z-index: 1; }", fixed = TRUE)
  expect_match(stylesheet, "z-index: 2; background: transparent", fixed = TRUE)
})

test_that("single Canvas views accept per-point sizes", {
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(utility, '"point_sizes"', fixed = TRUE)
  expect_match(javascript, "space.pointSizes", fixed = TRUE)
  expect_match(javascript, "pointSizes[i]", fixed = TRUE)
})

test_that("single Canvas trajectories accept compact endpoint edges", {
  javascript <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  hla <- paste(
    readLines(
      viewer_test_path("hla_tcr_motifs", "visualizations.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(javascript, "columns.from && columns.to", fixed = TRUE)
  expect_match(javascript, "sp.x[from]", fixed = TRUE)
  expect_match(hla, "from <- as.integer(vn$edges$from)", fixed = TRUE)
  expect_match(hla, "to <- as.integer(vn$edges$to)", fixed = TRUE)
  expect_match(hla, "from = from", fixed = TRUE)
  expect_match(hla, "to = to", fixed = TRUE)
  expect_no_match(hla, "x0 = vn$layout[from, 1]", fixed = TRUE)
})

test_that("gene controls load transcriptome choices server-side", {
  source <- paste(
    readLines(
      viewer_test_path("gene_expression", "UI_projection_input_type.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  utility <- paste(
    readLines(viewer_test_path("utility_functions.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_no_match(source, "list_of_genes()", fixed = TRUE)
  expect_match(source, "serverSideGeneSelector(", fixed = TRUE)
  expect_no_match(source, "retry = FALSE", fixed = TRUE)
  expect_match(utility, "selected <- isolate(input[[input_id]])", fixed = TRUE)
})

test_that("the real Viewer benchmark accepts the Canvas baseline", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_million_cell_viewer.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(
    readLines(benchmark_file, warn = FALSE),
    collapse = "\n"
  )

  expect_match(benchmark, "has_gpu_renderer <- file.exists", fixed = TRUE)
  expect_match(
    benchmark,
    "CEREBRO_VIEWER_BENCH_OVERVIEW_ONLY",
    fixed = TRUE
  )
  expect_match(benchmark, "canvas[id^=\\\"cv-cv-\\\"]", fixed = TRUE)
  expect_match(benchmark, "backend:'canvas2d'", fixed = TRUE)
  expect_match(benchmark, "cerebroLinkedViewsState", fixed = TRUE)
  expect_match(benchmark, "linked_ready_ms", fixed = TRUE)
  expect_match(benchmark, "gene_ready_ms", fixed = TRUE)
  expect_match(benchmark, "rgb_ready_ms", fixed = TRUE)
})

test_that("the Ren Linked benchmark records visual and interactive readiness", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_ren_linked_transport.R"
  )
  skip_if_not(file.exists(benchmark_file), "benchmark tree not present")
  benchmark <- paste(readLines(benchmark_file, warn = FALSE), collapse = "\n")

  expect_match(benchmark, "primary_logical_ms", fixed = TRUE)
  expect_match(benchmark, "complete_logical_ms", fixed = TRUE)
  expect_match(benchmark, "visual_ready_ms", fixed = TRUE)
  expect_match(benchmark, "interactive_ready_ms", fixed = TRUE)
  expect_match(benchmark, "cerebroLinkedViewsState.visualReady()", fixed = TRUE)
  expect_match(benchmark, "wait_for_idle", fixed = TRUE)
})

test_that("the page benchmark has a publication-grade contract", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_viewer_1m_pages.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(readLines(benchmark_file, warn = FALSE), collapse = "\n")

  expect_no_match(benchmark, "requestAnimationFrame", fixed = TRUE)
  expect_no_match(benchmark, "!isTRUE(first) ||", fixed = TRUE)
  expect_match(benchmark, "cerebro:specialist-state", fixed = TRUE)
  expect_match(benchmark, "cerebro:linkedviews-ready", fixed = TRUE)
  expect_match(
    benchmark,
    'setdiff(\n  chromote::get_chrome_args(),\n  "--disable-gpu"',
    fixed = TRUE
  )
  expect_match(
    benchmark,
    "cerebroLinkedViewsState.primaryReady()",
    fixed = TRUE
  )
  expect_match(benchmark, "completion_ready", fixed = TRUE)
  expect_match(benchmark, "primary_ready_ms", fixed = TRUE)
  expect_match(benchmark, "settled_ms", fixed = TRUE)
  expect_match(benchmark, "dataset_load_ms", fixed = TRUE)
  expect_match(
    benchmark,
    '"if(!completionRequired){"',
    fixed = TRUE
  )
  expect_no_match(
    benchmark,
    'document.documentElement.classList.contains',
    fixed = TRUE
  )
  expect_match(benchmark, "benchmark_mode", fixed = TRUE)
  expect_match(benchmark, "projection_asset_bytes", fixed = TRUE)
  expect_match(benchmark, "e.detail?.primaryReady===true", fixed = TRUE)
  expect_match(benchmark, "run_observation", fixed = TRUE)
  expect_match(benchmark, "browser_debug_port_error <- function", fixed = TRUE)
  expect_match(benchmark, "start_app_driver <- function", fixed = TRUE)
  expect_match(benchmark, "startup_attempts = startup$attempts", fixed = TRUE)
  expect_match(benchmark, "startup_errors = paste(startup$errors", fixed = TRUE)
  expect_match(
    benchmark,
    "Chrome debugging port not open after [0-9]+ seconds",
    fixed = TRUE
  )
  expect_match(benchmark, "arm_and_click_page <- function", fixed = TRUE)
  expect_no_match(benchmark, "arm_page <- function", fixed = TRUE)
  expect_match(benchmark, "const generation", fixed = TRUE)
  expect_match(benchmark, "performance.now()", fixed = TRUE)
  expect_match(benchmark, "e.timeStamp >= clickStart", fixed = TRUE)
  expect_match(benchmark, "eventAt!==null", fixed = TRUE)
  expect_match(benchmark, "const linked=hasCurrentEvent", fixed = TRUE)
  expect_match(
    benchmark,
    "readyValue===null||readyValue===undefined",
    fixed = TRUE
  )
  expect_match(
    benchmark,
    "open_page(app, page, require_event = TRUE)",
    fixed = TRUE
  )
  expect_match(benchmark, "requires_ready_event(", fixed = TRUE)
  expect_match(benchmark, "ready_event_required", fixed = TRUE)
  expect_no_match(benchmark, "Network$enable", fixed = TRUE)
  expect_no_match(benchmark, "Network$webSocketFrame", fixed = TRUE)
  expect_no_match(benchmark, "frame_payload_bytes", fixed = TRUE)
  expect_match(benchmark, "Shiny.shinyapp.$socket", fixed = TRUE)
  expect_match(benchmark, "TextEncoder", fixed = TRUE)
  expect_match(benchmark, "ArrayBuffer.isView", fixed = TRUE)
  expect_match(benchmark, "data instanceof Blob", fixed = TRUE)
  expect_match(benchmark, "removeEventListener('message'", fixed = TRUE)
  expect_match(benchmark, "socket.send=meter.originalSend", fixed = TRUE)
  canvas_spec <- sub(
    "(?s).*?(canvas_page <- function.*?)(?=\\n\\npages <- list).*",
    "\\1",
    benchmark,
    perl = TRUE
  )
  expect_no_match(canvas_spec, "shiny-busy", fixed = TRUE)
  expect_match(
    canvas_spec,
    "detail?.datasetFingerprint===",
    fixed = TRUE
  )
  expect_match(
    canvas_spec,
    "window.__cerebroPageBenchExpectedFingerprint",
    fixed = TRUE
  )
  expect_no_match(canvas_spec, "datasetFingerprint.length>0", fixed = TRUE)
  fingerprint_pattern <- "^md5-cell-set-v1:[0-9a-f]{32}$"
  expect_true(grepl(
    fingerprint_pattern,
    "md5-cell-set-v1:0123456789abcdef0123456789abcdef"
  ))
  expect_false(grepl(fingerprint_pattern, "0:811c9dc5:85ebca77"))
  for (name in c(
    "overview",
    "gene_expression",
    "immune_repertoire",
    "trajectory",
    "hla",
    "spatial"
  )) {
    expect_match(
      benchmark,
      paste0(name, " = canvas_page("),
      fixed = TRUE,
      info = name
    )
  }
  coordinated_spec <- substring(
    benchmark,
    regexpr("coordinated_views = page(", benchmark, fixed = TRUE)
  )
  expect_no_match(coordinated_spec, "wait_idle", fixed = TRUE)
  expect_match(
    coordinated_spec,
    "e.detail?.datasetFingerprint===expectedFingerprint",
    fixed = TRUE
  )
  atomic_click <- sub(
    "(?s).*?(arm_and_click_page <- function.*?)(?=\\n\\npage_available).*",
    "\\1",
    benchmark,
    perl = TRUE
  )
  expect_equal(
    lengths(regmatches(atomic_click, gregexpr("app\\$run_js", atomic_click))),
    1L
  )
  expect_match(
    benchmark,
    "(?s)coordinated_views = page\\(.*?required = TRUE.*?ready_event = ",
    perl = TRUE
  )
  expect_match(
    benchmark,
    'results$status == "ok" & results$performance_applicable & !results$pass',
    fixed = TRUE
  )
  expect_equal(
    lengths(gregexpr(
      "expected_points = expected_cells",
      benchmark,
      fixed = TRUE
    )),
    2L
  )
  expect_match(
    benchmark,
    "expected_points = trajectory_contract$renderable_rows[[1L]]",
    fixed = TRUE
  )
  expect_match(
    benchmark,
    "spatial_contract$renderable_rows[[1L]]",
    fixed = TRUE
  )
  expect_match(benchmark, "VIEWER_TRAJECTORY_METHOD", fixed = TRUE)
  expect_match(benchmark, "VIEWER_TRAJECTORY_NAME", fixed = TRUE)
  immune_spec <- regmatches(
    benchmark,
    regexpr(
      "immune_repertoire = canvas_page\\([\\s\\S]{0,300}?\\n  \\),",
      benchmark,
      perl = TRUE
    )
  )
  expect_length(immune_spec, 1L)
  expect_no_match(immune_spec, "expected_points", fixed = TRUE)
  expect_match(benchmark, "VIEWER_EXPECTED_CELLS", fixed = TRUE)
  expect_match(benchmark, "VIEWER_GENE_PRIME_OVERVIEW", fixed = TRUE)
  expect_match(benchmark, "VIEWER_BENCH_SKIP_VISUAL_CHECK", fixed = TRUE)
  expect_match(benchmark, "VIEWER_VISITS_ONLY", fixed = TRUE)
  expect_match(benchmark, "plot.data.length>0", fixed = TRUE)
  expect_match(benchmark, "groups_metric_available", fixed = TRUE)
  expect_match(benchmark, "metadata_names", fixed = TRUE)
  expect_match(benchmark, "groups_by_other_group_plot", fixed = TRUE)
  expect_match(benchmark, "cerebro:groups-primary-ready", fixed = TRUE)
  expect_match(benchmark, "load_benchmark_dataset <- function", fixed = TRUE)
  expect_match(
    benchmark,
    "cerebro_benchmark_dataset_load_request",
    fixed = TRUE
  )
  expect_match(
    benchmark,
    "detail?.benchmarkGeneration===generation",
    fixed = TRUE
  )
  expect_match(
    benchmark,
    "detail?.datasetFingerprint===expectedFingerprint",
    fixed = TRUE
  )
  expect_match(benchmark, "if (isTRUE(memory_mode))", fixed = TRUE)
  expect_match(
    benchmark,
    'identical(benchmark_mode, "timing")',
    fixed = TRUE
  )
  expect_match(
    benchmark,
    'identical(profile, "publication") &&\n    identical(benchmark_mode, "timing")',
    fixed = TRUE
  )
  expect_match(benchmark, "state?.summary?.()", fixed = TRUE)
  expect_match(benchmark, "link.offsetParent !== null", fixed = TRUE)
  expect_match(benchmark, "app$get_screenshot", fixed = TRUE)
  expect_match(benchmark, "png::readPNG", fixed = TRUE)
  protocol <- paste(
    readLines(
      testthat::test_path("..", "bench", "viewer_1m_page_protocol.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  benchmark_contract <- paste(benchmark, protocol, sep = "\n")

  for (field in c(
    "candidate_git_sha",
    "artifact_sha256",
    "host",
    "r_version",
    "package_version",
    "chrome_version",
    "r_peak_rss_kib",
    "chrome_peak_rss_kib",
    "js_heap_used_bytes",
    "websocket_sent_payload_bytes",
    "websocket_received_payload_bytes",
    "websocket_received_at_ready_bytes",
    "websocket_post_ready_received_bytes",
    "shared_projection_primed",
    "performance_ms",
    "primary_ready_ms",
    "settled_ms",
    "dataset_load_ms",
    "benchmark_mode",
    "profiler_instrumented",
    "specialist_event_kind",
    "specialist_generation",
    "render_request_sent",
    "cached_activation",
    "geometry_reused",
    "click_to_request_ms",
    "server_prepare_ms",
    "serialize_transfer_ms",
    "binary_decode_ms",
    "projection_fetch_ms",
    "build_spaces_ms",
    "pre_draw_ms",
    "first_draw_ms",
    "activation_ms",
    "request_to_ready_ms",
    "click_to_ready_ms",
    "primary_payload_bytes",
    "primary_payload_count",
    "primary_payload_meter_bytes",
    "aux_payload_count",
    "aux_payload_bytes",
    "correctness_pass",
    "rendered_point_count",
    "expected_point_count",
    "trajectory_method",
    "trajectory_name",
    "expected_trajectory_method",
    "expected_trajectory_name",
    "correctness_detail",
    "navigator_gpu",
    "renderer_backend",
    "renderer_adapter",
    "renderer_context_lost",
    "renderer_error",
    "visible_pixel_count",
    "visible_pixels_pass",
    "requires_webgpu",
    "performance_applicable"
  )) {
    expect_match(benchmark_contract, field, fixed = TRUE, info = field)
  }
})

test_that("page-ready events carry request-bound correctness identity", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(engine, "benchmarkGeneration", fixed = TRUE)
  expect_match(engine, "renderedPointCount", fixed = TRUE)
  expect_match(engine, "datasetFingerprint", fixed = TRUE)
  expect_match(engine, "page: 'coordinated_views'", fixed = TRUE)
  expect_match(engine, "reportWorkspaceReady('cached')", fixed = TRUE)
})

test_that("dataset switches clear specialist caches before the next render", {
  engine <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(
    engine,
    "shiny:inputchanged.cerebroDatasetSwitch",
    fixed = TRUE
  )
  expect_match(
    engine,
    "event.name !== 'crb_file_selector'",
    fixed = TRUE
  )
  expect_match(engine, "resetSingleViews();", fixed = TRUE)
  expect_match(engine, "lastVis = null;", fixed = TRUE)
})

test_that("trajectory benchmark contract follows the selected trajectory", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  skip_if_not(file.exists(protocol_file), "benchmark tree not present")
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)
  object <- new.env(parent = emptyenv())
  object$getTrajectory <- function(method, name) {
    expect_identical(method, "marker_guided")
    expect_identical(name, "E18_neurogenesis")
    list(
      meta = data.frame(
        DR_1 = 1:5,
        DR_2 = 6:10,
        pseudotime = c(0, 1, NA, 3, 4)
      )
    )
  }
  artifact <- tempfile(fileext = ".crb")
  saveRDS(object, artifact)

  contract <- protocol$benchmark_trajectory_contract(
    artifact,
    "marker_guided",
    "E18_neurogenesis"
  )

  expect_identical(contract$metadata_rows, 5L)
  expect_identical(contract$renderable_rows, 4L)
  expect_identical(contract$excluded_pseudotime_rows, 1L)
  expect_identical(contract$method, "marker_guided")
  expect_identical(contract$name, "E18_neurogenesis")
})

test_that("Spatial benchmark contract follows finite coordinate rows", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  skip_if_not(file.exists(protocol_file), "benchmark tree not present")
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)
  object <- new.env(parent = emptyenv())
  object$availableSpatial <- function() "synthetic_grid"
  object$getSpatialData <- function(name, hydrate_molecules = TRUE) {
    expect_identical(name, "synthetic_grid")
    expect_false(hydrate_molecules)
    list(coordinates = data.frame(x = 1:5, y = 6:10))
  }
  artifact <- tempfile(fileext = ".crb")
  saveRDS(object, artifact)

  contract <- protocol$benchmark_spatial_contract(artifact)

  expect_identical(contract$name, "synthetic_grid")
  expect_identical(contract$renderable_rows, 5L)
})

test_that("Mouse 1M marker-guided trajectory keeps its neural-lineage contract", {
  artifact <- Sys.getenv("CEREBRO_MOUSE_1M_CRB")
  skip_if(!nzchar(artifact) || !file.exists(artifact))
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)
  object <- protocol$read_benchmark_crb_payload(artifact)
  trajectory <- object$getTrajectory("marker_guided", "E18_neurogenesis")

  expect_identical(nrow(object$getMetaData()), 1000000L)
  expect_identical(nrow(trajectory$meta), 981785L)
  expect_identical(1000000L - nrow(trajectory$meta), 18215L)
  expect_false(anyNA(trajectory$meta$pseudotime))
  expect_false(anyNA(trajectory$meta$DR_1))
  expect_false(anyNA(trajectory$meta$DR_2))
  expect_setequal(
    trajectory$provenance$excluded_clusters,
    c("2", "9", "20", "23", "29", "30")
  )
})

test_that("benchmark TSV output escapes controls and validates columns", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  skip_if_not(file.exists(protocol_file), "benchmark tree not present")
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)
  output <- tempfile(fileext = ".tsv")
  rows <- data.frame(
    status = "error",
    error = "first line\nsecond\tfield\rreturn",
    correctness_detail = "{\n\t\"reason\": \"bad\"\n}",
    stringsAsFactors = FALSE
  )

  expect_silent(protocol$write_validated_tsv(rows, output))

  physical_lines <- readLines(output, warn = FALSE)
  expect_length(physical_lines, 2L)
  expect_true(all(
    vapply(
      physical_lines,
      protocol$tsv_field_count,
      integer(1)
    ) ==
      ncol(rows)
  ))
  expect_match(
    physical_lines[[2L]],
    "first line\\nsecond\\tfield\\rreturn",
    fixed = TRUE
  )
  expect_match(
    physical_lines[[2L]],
    "{\\n\\t\"reason\": \"bad\"\\n}",
    fixed = TRUE
  )
})

test_that("benchmark provenance identifies sidecars and Viewer Packs", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  skip_if_not(file.exists(protocol_file), "benchmark tree not present")
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)
  root <- tempfile("benchmark-artifacts-")
  dir.create(root)
  sidecar <- file.path(root, "fixture.bpcells")
  pack <- file.path(root, "fixture.viewer")
  dir.create(sidecar)
  dir.create(pack)
  writeLines(c("cell-1", "cell-2"), file.path(sidecar, "col_names"))
  writeBin(as.raw(c(1L, 2L, 3L)), file.path(sidecar, "val_data"))
  writeLines('{"schema_version":1}', file.path(pack, "manifest.json"))
  object <- new.env(parent = emptyenv())
  object$expression_backend <- list(
    type = "bpcells",
    location = basename(sidecar)
  )
  crb <- file.path(root, "fixture.crb")
  saveRDS(object, crb)

  manifest <- protocol$benchmark_artifact_provenance(crb)

  expect_identical(manifest$bpcells_sidecar, normalizePath(sidecar))
  expect_match(manifest$bpcells_sidecar_sha256, "^[0-9a-f]{64}$")
  expect_identical(
    manifest$viewer_pack_manifest,
    normalizePath(file.path(pack, "manifest.json"))
  )
  expect_match(manifest$viewer_pack_manifest_sha256, "^[0-9a-f]{64}$")
})

test_that("the page benchmark schedule and budgets are balanced", {
  protocol_file <- testthat::test_path(
    "..",
    "bench",
    "viewer_1m_page_protocol.R"
  )
  skip_if_not(
    file.exists(protocol_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  protocol <- new.env(parent = baseenv())
  sys.source(protocol_file, envir = protocol)

  schedule <- protocol$build_balanced_schedule(
    c("baseline", "candidate"),
    c("overview", "trajectory"),
    5L
  )
  units <- split(
    schedule,
    interaction(schedule$round, schedule$page, schedule$visit, drop = TRUE)
  )
  expect_true(all(vapply(
    units,
    function(unit) setequal(unit$candidate, c("baseline", "candidate")),
    logical(1)
  )))
  page_visits <- split(
    schedule,
    interaction(schedule$page, schedule$visit, drop = TRUE)
  )
  expect_true(all(vapply(
    page_visits,
    function(rows) {
      positions <- table(rows$candidate, rows$candidate_position)
      max(positions) - min(positions) <= 1L
    },
    logical(1)
  )))

  expect_error(
    protocol$validate_page_profile("publication", 4L),
    "at least 5 rounds"
  )
  expect_silent(protocol$validate_page_profile("publication", 5L))
  expect_true(protocol$page_budget_pass(1999, 2000))
  expect_false(protocol$page_budget_pass(2000, 2000))
  expect_true(protocol$requires_ready_event(
    "coordinated_views",
    "first",
    warmed = FALSE
  ))
  expect_true(protocol$requires_ready_event(
    "coordinated_views",
    "repeat",
    warmed = FALSE
  ))
  expect_true(protocol$requires_ready_event(
    "coordinated_views",
    "repeat",
    warmed = TRUE
  ))
  expect_true(protocol$requires_ready_event(
    "trajectory",
    "repeat",
    warmed = TRUE
  ))
  expect_silent(protocol$validate_benchmark_mode("timing"))
  expect_silent(protocol$validate_benchmark_mode("memory"))
  expect_error(protocol$validate_benchmark_mode("mixed"), "timing or memory")
})

test_that("the cold-start benchmark measures an installed Viewer", {
  benchmark_file <- testthat::test_path(
    "..",
    "bench",
    "benchmark_million_cell_startup.R"
  )
  skip_if_not(
    file.exists(benchmark_file),
    "benchmark tree not present (expected when checking a built package)"
  )
  benchmark <- paste(
    readLines(benchmark_file, warn = FALSE),
    collapse = "\n"
  )

  expect_match(benchmark, "library(CerebroNexus)", fixed = TRUE)
  expect_no_match(benchmark, "load_all", fixed = TRUE)
  expect_match(benchmark, "library_ms", fixed = TRUE)
  expect_match(benchmark, "browser_load_ms", fixed = TRUE)
  expect_match(benchmark, "load_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "browser_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "process_to_data_ms", fixed = TRUE)
  expect_match(benchmark, "CEREBRO_STARTUP_GATE_LABEL", fixed = TRUE)
  expect_match(
    benchmark,
    "CEREBRO_STARTUP_MAX_PROCESS_TO_DATA_MS",
    fixed = TRUE
  )
  expect_match(benchmark, "observed_ms >= gate_ms", fixed = TRUE)
})

test_that("optional page servers load only when their page becomes visible", {
  server <- paste(
    readLines(viewer_test_path("shiny_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server, "deferred_viewer_server_files <- c(", fixed = TRUE)
  expect_match(server, '"marker_genes/server.R"', fixed = TRUE)
  expect_match(server, '"color_management/server.R"', fixed = TRUE)
  expect_match(
    server,
    "load_deferred_viewer_server <- function(server_file)",
    fixed = TRUE
  )
  expect_match(
    server,
    'load_deferred_viewer_server("coordinated_views/server.R")',
    fixed = TRUE
  )
  expect_match(
    server,
    'server_file <- unname(deferred_viewer_server_files[input[["sidebar"]]])',
    fixed = TRUE
  )
  expect_match(
    server,
    "if (length(server_file) && !is.na(server_file))",
    fixed = TRUE
  )
  expect_no_match(
    server,
    'deferred_viewer_server_files[[input[["sidebar"]]]]',
    fixed = TRUE
  )
  expect_match(server, "ignoreInit\\s*=\\s*FALSE\\s*\\)")
  expect_no_match(server, "preload_deferred_viewer_server", fixed = TRUE)
  expect_no_match(server, "later::later(", fixed = TRUE)
  expect_no_match(
    server,
    "for (server_file in deferred_viewer_server_files)",
    fixed = TRUE
  )
  expect_match(server, "envir = server_scope", fixed = TRUE)
})

test_that("continuous colours keep stable paint order without comparison sort", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function quantisedOrder\\(vals, span\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "eval(fn);",
      "const values = [2, null, 1, 2, NaN, 0, 1];",
      "process.stdout.write(JSON.stringify(Array.from(quantisedOrder(values, 2))));"
    ),
    runner
  )
  output <- system2("node", runner, stdout = TRUE, stderr = TRUE)

  expect_equal(attr(output, "status"), NULL)
  expect_identical(jsonlite::fromJSON(output), c(1L, 4L, 5L, 2L, 6L, 0L, 3L))
})

test_that("WebGPU RGB colours match the existing blend without CSS allocation", {
  skip_if(Sys.which("node") == "", "node not on PATH")
  source <- viewer_test_path("www", "cell_views.js")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    c(
      "const fs = require('fs');",
      sprintf(
        "const source = fs.readFileSync(%s, 'utf8');",
        encodeString(source, quote = '"')
      ),
      "const fn = source.match(/function rgbGpuColor\\(r, g, b\\) \\{[\\s\\S]*?\\n  \\}/)[0];",
      "const RGB_MIN=28, RGB_GREY_RGB=[217,219,222]; eval(fn);",
      "function old(r,g,b){const m=Math.max(r,g,b);let out;",
      "if(m<=RGB_MIN)out=RGB_GREY_RGB;else if(r>RGB_MIN&&g>RGB_MIN&&b>RGB_MIN)out=[0,0,0];",
      "else{const t=m/255;out=[r,g,b].map((v,i)=>Math.round(RGB_GREY_RGB[i]+(v/m*255-RGB_GREY_RGB[i])*t));}",
      "return out[0]|out[1]<<8|out[2]<<16|(m>RGB_MIN?0x1000000:0);}",
      "for(let r=0;r<256;r+=17)for(let g=0;g<256;g+=17)for(let b=0;b<256;b+=17)",
      "if(rgbGpuColor(r,g,b)!==old(r,g,b))process.exit(1);"
    ),
    runner
  )
  status <- system2("node", runner)

  expect_identical(status, 0L)
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
    hover
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
    hover
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
  expect_identical(
    as.character(captured$deferred_aux()$selection_key),
    c("c3", "c1", "c2")
  )
})

test_that("hidden group filters activate after startup rendering", {
  source <- paste(
    readLines(
      viewer_test_path(
        "module",
        "group_filters",
        "group_filters_widget.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(source, "domain$onFlushed(", fixed = TRUE)
  expect_match(source, "delay = 0.5", fixed = TRUE)
  expect_match(source, "suspendWhenHidden = FALSE", fixed = TRUE)
})
