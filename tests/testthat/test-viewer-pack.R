viewer_pack_fixture <- function(
  path,
  n = 3L,
  immune = FALSE,
  trajectory = FALSE,
  trajectory_match = FALSE,
  spatial = FALSE,
  groups = FALSE
) {
  object <- Cerebro$new()
  cells <- sprintf("cell-%03d", seq_len(n))
  object$setMetaData(data.frame(
    cell_barcode = cells,
    group = factor(rep(c("A", "B"), length.out = n)),
    score = seq_len(n) / 10,
    nUMI = seq_len(n) * 10,
    nGene = seq_len(n) * 2,
    stringsAsFactors = FALSE
  ))
  if (isTRUE(groups)) {
    object$addGroup("group", c("A", "B"))
  }
  object$addProjection(
    "umap",
    data.frame(
      x = seq_len(n),
      y = -seq_len(n),
      row.names = cells
    )
  )
  if (isTRUE(immune)) {
    object$addImmuneRepertoire(list(
      sample_1 = data.frame(
        barcode = cells,
        CTgene = rep("TRBV1.TRBJ1", n),
        CTaa = rep("CASSQ", n),
        CTnt = rep("TGTGCC", n),
        CTstrict = rep("TRBV1.TRBJ1;CASSQ", n),
        stringsAsFactors = FALSE
      )
    ))
  }
  if (isTRUE(trajectory)) {
    selected <- if (isTRUE(trajectory_match)) cells[-1L] else cells[c(2L, n)]
    selected_index <- match(selected, cells)
    object$addTrajectory(
      "monocle2",
      "subset",
      list(
        meta = data.frame(
          DR_1 = if (isTRUE(trajectory_match)) {
            selected_index
          } else {
            c(1, 2)
          },
          DR_2 = if (isTRUE(trajectory_match)) {
            -selected_index
          } else {
            c(3, 4)
          },
          pseudotime = seq(0, 1, length.out = length(selected)),
          state = rep(c("1", "2"), length.out = length(selected)),
          row.names = selected
        ),
        edges = data.frame(
          source_dim_1 = 1,
          source_dim_2 = 3,
          target_dim_1 = 2,
          target_dim_2 = 4
        )
      )
    )
  }
  if (isTRUE(spatial)) {
    selected <- cells[c(3L, 1L)]
    object$addSpatialData(
      "slice",
      list(
        coordinates = data.frame(
          x = c(30, 10),
          y = c(3, 1),
          row.names = selected
        ),
        expression = matrix(
          numeric(),
          nrow = 0L,
          ncol = length(selected),
          dimnames = list(character(), selected)
        ),
        histology_images = list()
      )
    )
  }
  saveCerebro(object, path, codec = "rds")
  path
}

test_that("packed projections and first frames align shuffled cell identities", {
  root <- tempfile("pack-order-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- viewer_pack_fixture(file.path(root, "data.crb"))
  object <- readCerebro(crb)
  expected <- object$projections$umap
  object$projections$umap <- expected[3:1, , drop = FALSE]
  saveCerebro(object, crb, codec = "rds")
  pack <- buildViewerPack(crb, viewer_binary = "always")
  expect_true(validateViewerPack(crb)$valid)
  actual <- matrix(
    readBin(
      file.path(pack, "projections/001.bin"),
      numeric(),
      n = 6,
      size = 4,
      endian = "little"
    ),
    ncol = 2,
    byrow = TRUE
  )
  expect_equal(unname(actual), unname(as.matrix(expected)))
  scope <- viewer_runtime_env()
  scope$data_set <- function() NULL
  scope$getProjection <- function(name) object$projections[[name]]
  scope$getMetaData <- function() object$getMetaData()
  expect_identical(
    scope$viewerProjectionFirstFrameCoordinates("umap"),
    expected
  )
  for (ids in list(NULL, c("x", "y", "z"), c("1", "1", "3"))) {
    bad <- as.matrix(expected)
    rownames(bad) <- ids
    object$projections$umap <- bad
    saveCerebro(object, crb, codec = "rds")
    expect_error(
      buildViewerPack(crb, viewer_binary = "always", overwrite = TRUE),
      "cell identities"
    )
    expect_error(
      scope$viewerProjectionFirstFrameCoordinates("umap"),
      "cell identities"
    )
  }
})

test_that("Viewer Pack gating is dataset-level", {
  root <- tempfile("viewer-pack-gating-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 3L)

  expect_null(buildViewerPack(
    crb,
    viewer_binary = "auto",
    viewer_binary_threshold = 4L
  ))
  expect_false(dir.exists(file.path(root, "dataset.viewer")))
  expect_null(buildViewerPack(
    crb,
    viewer_binary = "never",
    viewer_binary_threshold = 1L
  ))

  pack <- buildViewerPack(
    crb,
    viewer_binary = "auto",
    viewer_binary_threshold = 3L
  )
  expect_identical(
    normalizePath(pack, winslash = "/", mustWork = FALSE),
    normalizePath(
      file.path(root, "dataset.viewer"),
      winslash = "/",
      mustWork = FALSE
    )
  )
  expect_true(validateViewerPack(crb, pack)$valid)

  unlink(pack, recursive = TRUE)
  expect_identical(
    normalizePath(
      buildViewerPack(
        crb,
        viewer_binary = "always",
        viewer_binary_threshold = 999L
      ),
      winslash = "/",
      mustWork = FALSE
    ),
    normalizePath(pack, winslash = "/", mustWork = FALSE)
  )
})

test_that("Viewer Pack stores canonical trajectory row indexes", {
  root <- tempfile("viewer-pack-trajectory-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    trajectory = TRUE
  )
  buildViewerPack(crb, viewer_binary = "always")
  object <- readCerebro(crb)
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  descriptor <- runtime$viewerPackOpen(crb)

  expect_identical(
    runtime$viewerPackTrajectoryIndex(descriptor, "monocle2", "subset"),
    c(2L, 4L)
  )
  expect_false(exists(
    "common/cell_order.qs2",
    envir = descriptor$cache,
    inherits = FALSE
  ))
  meta <- object$getTrajectory("monocle2", "subset")$meta
  runtime$viewerPackDropTrajectoryNames(descriptor, object)
  compact <- object$getTrajectory("monocle2", "subset")$meta
  expect_false(is.character(attr(compact, "row.names")))
  expect_identical(unname(as.list(compact)), unname(as.list(meta)))

  frame <- runtime$viewerPackTrajectoryFrame(
    descriptor,
    "monocle2",
    "subset"
  )
  expect_identical(frame$cells, 2L)
  expect_identical(frame$state_dtype, "uint8")
  geometry <- readBin(
    file.path(descriptor$path, frame$geometry_path),
    what = numeric(),
    n = 4L,
    size = 4L,
    endian = "little"
  )
  expect_equal(geometry, c(1, 3, 2, 4))
  state_codes <- readBin(
    file.path(descriptor$path, frame$state_codes_path),
    what = integer(),
    n = 2L,
    size = 1L,
    signed = FALSE,
    endian = "little"
  )
  expect_identical(state_codes, c(0L, 1L))
  expect_identical(
    runtime$viewerPackTrajectoryEdges(descriptor, "monocle2", "subset"),
    data.frame(
      source_dim_1 = 1,
      source_dim_2 = 3,
      target_dim_1 = 2,
      target_dim_2 = 4
    )
  )
  expect_identical(
    as.character(jsonlite::read_json(
      file.path(descriptor$path, frame$state_dictionary_path),
      simplifyVector = TRUE
    )),
    c("1", "2")
  )
})

test_that("Viewer Pack links exact trajectory geometry to canonical projection", {
  root <- tempfile("viewer-pack-trajectory-canonical-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 5L,
    trajectory = TRUE,
    trajectory_match = TRUE
  )
  buildViewerPack(crb, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  descriptor <- runtime$viewerPackOpen(crb)
  frame <- runtime$viewerPackTrajectoryFrame(
    descriptor,
    "monocle2",
    "subset"
  )

  expect_identical(frame$geometry_kind, "canonical_projection")
  expect_identical(frame$projection_name, "umap")
  expect_identical(frame$subset_kind, "exclude_uint32")
  expect_false(file.exists(file.path(
    descriptor$path,
    "trajectory/001.geometry.bin"
  )))
  excluded <- readBin(
    file.path(descriptor$path, frame$subset_path),
    what = integer(),
    n = 1L,
    size = 4L,
    endian = "little"
  )
  expect_identical(excluded, 0L)
})

test_that("validated Viewer Pack descriptors are reused with session-local caches", {
  root <- tempfile("viewer-pack-descriptor-cache-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 4L)
  pack_path <- buildViewerPack(crb, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  cache <- new.env(parent = emptyenv())

  first <- runtime$viewerPackOpenCached(crb, cache)
  assign("session-value", TRUE, envir = first$cache)
  second <- runtime$viewerPackOpenCached(crb, cache)

  expect_identical(second$manifest, first$manifest)
  expect_false(identical(second$cache, first$cache))
  expect_false(exists("session-value", envir = second$cache, inherits = FALSE))

  manifest <- file.path(pack_path, "manifest.json")
  writeLines("{invalid", manifest, useBytes = TRUE)
  expect_null(runtime$viewerPackOpenCached(crb, cache))
})

test_that("Viewer Pack assets are hashed once per file version", {
  root <- tempfile("viewer-pack-md5-cache-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    trajectory = TRUE
  )
  buildViewerPack(crb, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  hashed <- 0L
  md5sum <- tools::md5sum
  testthat::local_mocked_bindings(
    md5sum = function(files) {
      hashed <<- hashed + length(files)
      md5sum(files)
    },
    .package = "tools"
  )
  path <- file.path("trajectory", "cell_index.qs2")
  for (session in 1:2) {
    descriptor <- runtime$viewerPackOpen(crb)
    expect_false(is.null(runtime$viewerPackReadAsset(descriptor, path)))
  }
  ## The CRB itself (once per open) and the asset once.
  expect_identical(hashed, 3L)
})

test_that("Viewer Pack stores canonical spatial row indexes", {
  root <- tempfile("viewer-pack-spatial-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    spatial = TRUE
  )
  buildViewerPack(crb, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  descriptor <- runtime$viewerPackOpen(crb)

  expect_identical(
    runtime$viewerPackSpatialIndex(descriptor, "slice"),
    c(3L, 1L)
  )
  frame <- runtime$viewerPackSpatialFrame(descriptor, "slice")
  expect_identical(frame$cells, 2L)
  expect_identical(frame$dimensions, 2L)
  geometry <- readBin(
    file.path(descriptor$path, frame$geometry_path),
    what = numeric(),
    n = 4L,
    size = 4L,
    endian = "little"
  )
  expect_equal(geometry, c(30, 3, 10, 1))
  expect_false(exists(
    "common/cell_order.qs2",
    envir = descriptor$cache,
    inherits = FALSE
  ))
})

test_that("Viewer Pack stores compact group metric frames", {
  root <- tempfile("viewer-pack-groups-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 10000L,
    groups = TRUE
  )
  buildViewerPack(crb, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  descriptor <- runtime$viewerPackOpen(crb)

  packed <- runtime$viewerPackGroupMetric(descriptor, "group", "nUMI")
  expect_named(packed, c("group", "nUMI"))
  expect_identical(nrow(packed), 8192L)
  expect_identical(levels(packed$group), c("A", "B"))
  expect_true("groups" %in% descriptor$manifest$modules)
  expect_null(runtime$viewerPackGroupMetric(descriptor, "group", "missing"))
})

test_that("saveCerebro forwards dataset-level Viewer Pack policy", {
  root <- tempfile("viewer-pack-save-")
  dir.create(root)
  object <- Cerebro$new()
  cells <- sprintf("cell-%03d", 1:3)
  object$setMetaData(data.frame(
    cell_barcode = cells,
    group = c("A", "A", "B"),
    stringsAsFactors = FALSE
  ))
  object$addProjection(
    "umap",
    data.frame(x = 1:3, y = 3:1, row.names = cells)
  )
  crb <- file.path(root, "dataset.crb")

  saveCerebro(
    object,
    crb,
    codec = "rds",
    viewer_binary_threshold = 3L
  )

  expect_true(validateViewerPack(crb)$valid)
})

test_that("Viewer Pack manifest validates canonical identity and assets", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("stringdist")
  root <- tempfile("viewer-pack-manifest-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    immune = TRUE
  )
  pack <- buildViewerPack(crb, viewer_binary = "always")
  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )

  expect_identical(manifest$schema_version, 1L)
  expect_identical(manifest$n_cells, 4L)
  expect_match(manifest$dataset_fingerprint, "^md5-crb-v1:[0-9a-f]{32}$")
  expect_match(
    manifest$cell_order_fingerprint,
    "^md5-cell-order-v1:[0-9a-f]{32}$"
  )
  expect_true(all(
    c(
      "common",
      "projections",
      "metadata",
      "immune",
      "hla_tcr"
    ) %in%
      manifest$modules
  ))
  expect_identical(manifest$immune_receptors, "TCR")
  expect_identical(
    unlist(manifest$capabilities, use.names = TRUE),
    c(
      marker_genes = FALSE,
      most_expressed_genes = FALSE,
      enriched_pathways = FALSE,
      extra_material = FALSE,
      trajectory = FALSE,
      spatial = FALSE,
      trekker = FALSE
    )
  )
  expect_true(length(manifest$assets) >= 4L)
  expect_true(all(file.exists(file.path(pack, manifest$assets$path))))
  expect_true(all(manifest$assets$bytes > 0))
  expect_true(all(grepl("^[0-9a-f]{32}$", manifest$assets$checksum)))
  expect_true(validateViewerPack(crb, pack, full = TRUE)$valid)
})

test_that("Viewer Pack rejects corruption and preserves the CRB fallback", {
  root <- tempfile("viewer-pack-corrupt-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 3L)
  pack <- buildViewerPack(crb, viewer_binary = "always")
  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )
  asset <- file.path(pack, manifest$assets$path[[1L]])
  writeBin(as.raw(1:3), asset)

  checked <- validateViewerPack(crb, pack, full = TRUE)
  expect_false(checked$valid)
  expect_match(checked$reason, "checksum|size")
  expect_null(readViewerPack(crb, pack))

  expect_error(
    buildViewerPack(crb, viewer_binary = "always"),
    "already exists"
  )
  rebuilt <- buildViewerPack(
    crb,
    viewer_binary = "always",
    overwrite = TRUE
  )
  expect_true(validateViewerPack(crb, rebuilt, full = TRUE)$valid)
})

test_that("Viewer Pack runtime loads HLA assets with exact fallback", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("stringdist")
  root <- tempfile("viewer-pack-runtime-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    immune = TRUE
  )
  pack <- buildViewerPack(crb, viewer_binary = "always")
  object <- readCerebro(crb)
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("core", "viewer_pack.R"),
    envir = runtime
  )

  descriptor <- runtime$viewerPackOpen(crb)
  expect_type(descriptor, "list")
  expect_false(exists(
    ".cell_order_valid",
    envir = descriptor$cache,
    inherits = FALSE
  ))
  expected_annotated <- hla_annotate_ir_metadata(
    object$getImmuneRepertoire(),
    object$getMetaData()
  )
  expected_segments <- hla_parse_ir_segments(expected_annotated, "TRB")
  expected_segments$cell_index <- match(
    as.character(expected_segments$barcode),
    object$getMetaData()$cell_barcode
  )
  expected_segments <- expected_segments[setdiff(
    names(expected_segments),
    c("CTgene", "CTnt", "CTaa", "CTstrict", "clone_vjc")
  )]
  expect_identical(
    runtime$viewerPackHlaSegments(descriptor, "TRB"),
    expected_segments
  )
  first_frame <- runtime$viewerPackHlaFirstFrame(descriptor, "TRB")
  expect_identical(first_frame$version, 2L)
  expect_identical(first_frame$filter_groups, "sample")
  expect_identical(first_frame$filter_levels$sample, "sample_1")
  expect_identical(first_frame$initial_samples, "sample_1")
  expect_identical(first_frame$segments$sample, rep("sample_1", 4L))
  expect_false(first_frame$by_v)
  expect_identical(first_frame$node_meta_cols, "sample")
  expect_null(first_frame$lineage_col)
  expect_s3_class(first_frame$graph_raw, "igraph")
  expect_identical(first_frame$default_min_nodes, 2L)
  expect_true(
    is.null(first_frame$graph_snapshot) || is.list(first_frame$graph_snapshot)
  )
  if (!is.null(first_frame$graph_snapshot)) {
    expect_identical(first_frame$graph_snapshot$version, 1L)
    expect_type(first_frame$graph_snapshot$vertex_attrs, "list")
    expect_equal(ncol(first_frame$graph_snapshot$edges), 2L)
  }
  expect_false(exists(
    ".cell_order_valid",
    envir = descriptor$cache,
    inherits = FALSE
  ))
  expect_false(exists(
    "common/cell_order.qs2",
    envir = descriptor$cache,
    inherits = FALSE
  ))
  expect_identical(
    runtime$viewerPackCellBarcodes(descriptor),
    sprintf("cell-%03d", 1:4)
  )

  immune <- runtime$viewerPackImmuneIndex(descriptor, "TCR")
  expect_identical(immune$cell_index, 1:4)
  expect_identical(immune$clone, rep("TRBV1.TRBJ1", 4L))
  expect_identical(immune$ctaa, rep("CASSQ", 4L))
  expect_identical(immune$expansion, rep(2L, 4L))

  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )
  first_path <- manifest$assets$path[
    grepl("hla_tcr/TRB[.]first[.]qs2$", manifest$assets$path)
  ]
  expect_length(first_path, 1L)
  writeBin(as.raw(1:3), file.path(pack, first_path))
  expect_null(runtime$viewerPackHlaFirstFrame(
    runtime$viewerPackOpen(crb),
    "TRB"
  ))

  trb <- manifest$assets$path[grepl("hla_tcr/TRB[.]qs2$", manifest$assets$path)]
  writeBin(as.raw(1:3), file.path(pack, trb))
  expect_null(runtime$viewerPackHlaSegments(
    runtime$viewerPackOpen(crb),
    "TRB"
  ))
})

test_that("Viewer Pack runtime fails closed on incompatible identity", {
  root <- tempfile("viewer-pack-identity-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 3L)
  pack <- buildViewerPack(crb, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("core", "viewer_pack.R"),
    envir = runtime
  )
  manifest_path <- file.path(pack, "manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)

  manifest$schema_version <- 999L
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    dataframe = "rows"
  )
  expect_null(runtime$viewerPackOpen(crb))

  manifest$schema_version <- 1L
  manifest$dataset_fingerprint <- "md5-crb-v1:00000000000000000000000000000000"
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    dataframe = "rows"
  )
  expect_null(runtime$viewerPackOpen(crb))
})

test_that("a pack of another schema is invalid and is rebuilt over", {
  root <- tempfile("viewer-pack-schema-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 3L)
  pack <- buildViewerPack(crb, viewer_binary = "always")
  manifest_path <- file.path(pack, "manifest.json")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  expect_false(any(
    c("projection_identity_version", "violin_summary_version") %in%
      names(manifest)
  ))
  manifest$schema_version <- 0L
  jsonlite::write_json(
    manifest,
    manifest_path,
    auto_unbox = TRUE,
    dataframe = "rows"
  )
  result <- validateViewerPack(crb)
  expect_false(result$valid)
  expect_match(result$reason, "schema")
  expect_null(readViewerPack(crb))
  expect_error(buildViewerPack(crb, viewer_binary = "always"), "already exists")
  buildViewerPack(crb, viewer_binary = "always", overwrite = TRUE)
  expect_true(validateViewerPack(crb)$valid)
})

test_that("Viewer Packs open from the CRB file alone", {
  root <- tempfile("viewer-pack-runtime-")
  dir.create(root)
  crb <- viewer_pack_fixture(file.path(root, "dataset.crb"), n = 4L)
  buildViewerPack(crb, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("core", "viewer_pack.R"),
    envir = runtime
  )

  descriptor <- runtime$viewerPackOpen(crb)

  expect_type(descriptor, "list")
  expect_true(isTRUE(descriptor$canonical_order))
  expect_identical(descriptor$cell_count, 4L)
  expect_null(descriptor$cells)
  expect_identical(
    runtime$viewerPackCellBarcodes(descriptor),
    sprintf("cell-%03d", 1:4)
  )
})

test_that("Viewer Pack IR indexes first-frame coordinates without barcodes", {
  root <- tempfile("viewer-pack-ir-first-frame-")
  dir.create(root)
  crb <- viewer_pack_fixture(
    file.path(root, "dataset.crb"),
    n = 4L,
    immune = TRUE
  )
  buildViewerPack(crb, viewer_binary = "always")
  object <- readCerebro(crb)
  coordinates <- unname(object$getProjection("umap"))

  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("core", "viewer_pack.R"),
    envir = runtime
  )
  descriptor <- runtime$viewerPackOpen(crb)
  expect_null(descriptor$cells)
  expect_true(exists(
    "viewerPackCellBarcodes",
    envir = runtime,
    inherits = FALSE
  ))

  projection_calls <- 0L
  scope <- new.env(parent = globalenv())
  ## Shared runtime helpers such as viewerMemoOne(); the mocks below win.
  viewer_runtime_env(scope)
  scope$reactive <- function(x) function() eval(substitute(x))
  scope$reactiveVal <- function(...) function(...) NULL
  scope$req <- function(...) invisible(NULL)
  scope$observeEvent <- function(...) invisible(NULL)
  scope$observe <- function(...) invisible(NULL)
  scope$`%||%` <- function(a, b) if (is.null(a)) b else a
  scope$getImmuneRepertoire <- function(...) NULL
  scope$getImmuneRepertoireSummary <- function(...) list(available = TRUE)
  scope$getMetaData <- function(...) stop("barcode hydration was forced")
  scope$availableProjections <- function(...) "umap"
  scope$getProjection <- function(...) {
    projection_calls <<- projection_calls + 1L
    stop("full projection hydration was forced")
  }
  scope$viewerProjectionFirstFrameCoordinates <- function(name) coordinates
  scope$viewerPackCurrent <- function() descriptor
  scope$viewerPackImmuneEncoded <- runtime$viewerPackImmuneEncoded
  scope$data_set <- function() list()
  scope$input <- list()
  scope$session <- list()
  sys.source(
    viewer_test_path("clone_contract.R"),
    envir = scope,
    keep.source = FALSE
  )
  sys.source(
    viewer_test_path("immune_repertoire", "encode.R"),
    envir = scope,
    keep.source = FALSE
  )
  sys.source(
    viewer_test_path("immune_repertoire", "data.R"),
    envir = scope,
    keep.source = FALSE
  )

  out <- scope$ir_clonal_umap_data(
    "umap",
    "TCR",
    show_all = FALSE
  )

  expect_identical(projection_calls, 0L)
  expect_equal(out$x, coordinates[, 1L])
  expect_equal(out$y, coordinates[, 2L])
  expect_identical(out$cell_index, 1:4)
  expect_true(all(!is.na(out$expansion)))
  expect_false("barcode" %in% names(out))
  if (exists("viewerPackCellBarcodes", envir = runtime, inherits = FALSE)) {
    expect_identical(
      runtime$viewerPackCellBarcodes(descriptor, out$cell_index),
      sprintf("cell-%03d", 1:4)
    )
  }
})

test_that("packed violin summaries survive serialization without altering statistics", {
  root <- tempfile("pack-violin-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- viewer_pack_fixture(
    file.path(root, "data.crb"),
    n = 10000L,
    groups = TRUE
  )
  object <- readCerebro(crb)
  object$meta_data$nUMI <- rep(1:10, 1000)
  object$meta_data$nUMI[10000] <- 1e9
  saveCerebro(object, crb, codec = "rds")
  buildViewerPack(crb, viewer_binary = "always")
  scope <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), scope)
  pack <- scope$viewerPackOpen(crb)
  value <- scope$viewerPackGroupMetric(pack, "group", "nUMI")
  expected <- CerebroNexus:::.viewerPackCompactViolinData(
    object$getMetaData(),
    "group",
    "nUMI"
  )
  expect_identical(value, expected)
  expect_null(scope$viewerPackGroupMetric(NULL, "group", "nUMI"))
})

test_that("a Pack without this release's immune encoding is outdated", {
  current <- .viewerPackImmuneEncoder()$IR_ENCODING_VERSION
  expect_false(.viewerPackImmuneOutdated(list(immune_receptors = character())))
  expect_false(.viewerPackImmuneOutdated(list(
    immune_receptors = "TCR",
    immune_encoding = current
  )))
  ## Built before the encoding (no entry), or with another version.
  expect_true(.viewerPackImmuneOutdated(list(immune_receptors = "TCR")))
  expect_true(.viewerPackImmuneOutdated(list(
    immune_receptors = "TCR",
    immune_encoding = current + 1L
  )))
  ## No repertoire, nothing to encode.
  expect_false(.viewerPackImmuneOutdated(list(
    immune_receptors = list(),
    immune_encoding = 0L
  )))
})

test_that("a Viewer Pack beside a thin H5 CRB is checked through its recorded cell hash", {
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  cells <- h5_cells()
  counts <- h5_counts(cells)
  viewer_h5_matrix(counts, file.path(root, "expression.h5"))
  object <- Cerebro$new()
  object$setExpressionBackend("h5", "expression.h5")
  object$meta_data <- data.frame(
    cell_barcode = cells,
    sample = c("a", "a", "b", "b"),
    nUMI = as.double(colSums(counts)),
    nGene = as.double(colSums(counts > 0)),
    stringsAsFactors = FALSE
  )
  object$projections <- list(
    umap = data.frame(
      UMAP_1 = seq_along(cells),
      UMAP_2 = rev(seq_along(cells)),
      row.names = cells
    )
  )
  crb <- file.path(root, "thin.crb")
  saveCerebro(object, crb)
  pack <- buildViewerPack(crb, viewer_binary = "always")
  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )
  payload <- .readCerebroPayload(crb)

  expect_true(.viewerPackMatchesThinCrb(crb, pack, manifest, payload))
  expect_true(.validateViewerPack(crb, pack, TRUE, payload)$valid)
  expect_true(.bundleViewerPackValid(crb, payload))

  ## An embedded CRB has no recorded hash, so it is read again and still valid.
  embedded <- viewer_pack_fixture(file.path(root, "embedded.crb"), n = 3L)
  embedded_pack <- buildViewerPack(embedded, viewer_binary = "always")
  expect_true(.bundleViewerPackValid(embedded, .readCerebroPayload(embedded)))

  ## A pack whose cell order differs from the CRB's cells is rejected.
  qs2::qs_save(rev(cells), file.path(pack, "common", "cell_order.qs2"))
  expect_false(.viewerPackMatchesThinCrb(crb, pack, manifest, payload))
  expect_false(.bundleViewerPackValid(crb, payload))
})

test_that("a Viewer Pack is built without reading the CRB more than once", {
  root <- tempfile("pack-reads-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  crb <- viewer_pack_fixture(file.path(root, "data.crb"))
  object <- readCerebro(crb)
  reads <- 0L
  read_payload <- .readCerebroPayload
  testthat::local_mocked_bindings(
    .readCerebroPayload = function(file) {
      reads <<- reads + 1L
      read_payload(file)
    }
  )

  ## buildViewerPack() reads the CRB to build the pack and checks the pack
  ## against the object it has read.
  buildViewerPack(crb, viewer_binary = "always")
  expect_identical(reads, 1L)

  ## A writer has the object it serialized and hands it over.
  reads <- 0L
  saveCerebro(object, crb, codec = "rds", viewer_binary = "always")
  expect_identical(reads, 0L)
  payload <- read_payload(crb)
  pack <- .viewerPackPath(crb)
  expect_true(
    .validateViewerPack(crb, pack, full = TRUE, payload = payload)$valid
  )
  expect_identical(reads, 0L)

  ## The public check has nothing but the file.
  expect_true(validateViewerPack(crb)$valid)
  expect_identical(reads, 1L)

  ## A payload is still checked like a file that was read: a pack of other
  ## cells does not pass because the object came from memory.
  other <- viewer_pack_fixture(file.path(root, "other.crb"), n = 5L)
  expect_false(
    .validateViewerPack(
      crb,
      pack,
      full = TRUE,
      payload = read_payload(other)
    )$valid
  )
})
