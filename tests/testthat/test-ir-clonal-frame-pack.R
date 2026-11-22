## The Viewer Pack's default Clonal projection frames (R/viewer_pack.R,
## .viewerPackImmuneClonalFrames()) hold the cells, order and traces of the
## frame the Immune repertoire page builds from the encoding.

ir_clonal_pack_fixture <- function(path) {
  n <- 5000L
  cells <- sprintf("cell-%04d", seq_len(n))
  object <- Cerebro$new()
  object$setMetaData(data.frame(
    cell_barcode = cells,
    group = factor(rep(c("A", "B"), length.out = n)),
    stringsAsFactors = FALSE
  ))
  object$addProjection(
    "umap",
    data.frame(x = seq_len(n) / 7, y = -seq_len(n) / 3, row.names = cells)
  )
  ## Clones of 150, 50, 10, 3 and 1 cells: every expansion level.
  clone <- c(
    rep(1L, 150L),
    rep(2:11, each = 50L),
    rep(12:111, each = 10L),
    rep(112:461, each = 3L),
    462:761
  )
  ## The rows in another order than the cells, a cell's second row and a
  ## barcode of no cell.
  order <- c(seq(2L, 3000L, 2L), seq(1L, 2999L, 2L))
  tcr <- data.frame(
    barcode = c(cells[order], cells[[7L]], "stray"),
    clone = c(clone[order], 999L, 998L)
  )
  object$addImmuneRepertoire(list(
    sample_1 = data.frame(
      barcode = c(tcr$barcode, cells[4001:4010]),
      CTgene = c(
        paste0("TRAV1.TRAJ1_TRBV", tcr$clone, ".TRBJ1"),
        rep("IGHV1.IGHJ1.IGHM_IGKV1.IGKJ1.IGKC", 10L)
      ),
      CTaa = c(paste0("CAV_CASS", tcr$clone), rep("CAR_CQQ", 10L)),
      CTnt = c(paste0("tg_tt", tcr$clone), rep("gc_ga", 10L)),
      CTstrict = c(
        paste0("TRAV1.TRAJ1;tg_TRBV", tcr$clone, ".TRBJ1;tt", tcr$clone),
        rep("IGHV1.IGHJ1.IGHM;gc_IGKV1.IGKJ1.IGKC;ga", 10L)
      ),
      stringsAsFactors = FALSE
    )
  ))
  saveCerebro(object, path, codec = "rds")
  list(path = path, cells = cells, order = order)
}

ir_clonal_pack_scope <- function(descriptor, coordinates) {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  scope <- new.env(parent = globalenv())
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
  scope$viewerProjectionFirstFrameCoordinates <- function(name) coordinates
  scope$viewerPackCurrent <- function() descriptor
  scope$viewerPackImmuneEncoded <- runtime$viewerPackImmuneEncoded
  scope$data_set <- function() list()
  scope$input <- list()
  scope$session <- list()
  for (file in list(
    "clone_contract.R",
    c("immune_repertoire", "encode.R"),
    c("immune_repertoire", "param_spec.R"),
    c("immune_repertoire", "data.R")
  )) {
    sys.source(
      do.call(viewer_test_path, as.list(file)),
      envir = scope,
      keep.source = FALSE
    )
  }
  ## The runtime's Pack readers win over the stubs of utility_functions.R.
  for (name in c(
    "viewerPackCanonical",
    "viewerPackImmuneClonalFrame",
    "viewerPackImmuneEncoded"
  )) {
    assign(name, get(name, envir = runtime), envir = scope)
  }
  eval(
    viewer_r_assignment(
      viewer_test_path("immune_repertoire", "visualizations.R"),
      "IR_EXPANSION_COLORS"
    ),
    scope
  )
  scope
}

test_that("the Pack's Clonal frame is the frame the page builds", {
  skip_if_not_installed("qs2")
  root <- tempfile("viewer-pack-ir-clonal-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- ir_clonal_pack_fixture(file.path(root, "dataset.crb"))
  pack <- buildViewerPack(fixture$path, viewer_binary = "always")
  manifest <- jsonlite::read_json(
    file.path(pack, "manifest.json"),
    simplifyVector = TRUE
  )
  ## Clonal frames arrived with revision 2; later revisions keep them.
  expect_gte(manifest$format_revision, 2L)
  expect_setequal(manifest$immune_clonal_frames$receptor, c("TCR", "BCR"))

  object <- readCerebro(fixture$path)
  coordinates <- unname(as.matrix(object$getProjection("umap")))
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  descriptor <- runtime$viewerPackOpen(fixture$path)
  scope <- ir_clonal_pack_scope(descriptor, coordinates)
  read_frame <- function(receptor, pack = descriptor) {
    runtime$viewerPackImmuneClonalFrame(
      pack,
      receptor,
      "CTgene",
      version = scope$IR_CLONAL_FRAME_VERSION,
      bins = scope$IR_CLONE_BINS,
      max_background = 100000L
    )
  }

  for (receptor in c("TCR", "BCR")) {
    out <- scope$ir_clonal_umap_data(
      "umap",
      receptor,
      show_all = TRUE,
      split_output = TRUE
    )
    frame <- read_frame(receptor)
    expect_false(is.null(frame))
    rows <- readBin(
      frame$subset$file,
      "integer",
      n = frame$cells,
      size = 4L,
      endian = "little"
    )
    codes <- as.integer(readBin(frame$codes$file, "raw", n = frame$cells))
    expect_identical(
      rows + 1L,
      c(
        out$background_key,
        unlist(lapply(out$coloured_rows, function(at) out$coloured_key[at]))
      )
    )
    expect_identical(
      frame$trace_codes,
      ifelse(is.na(out$trace_codes), 0L, out$trace_codes)
    )
    expect_identical(codes, rep(seq_along(out$x) - 1L, lengths(out$x)))
  }

  ## The frame built from the encoding: the receptor's cells in the order of
  ## their first rows, every other cell as the background.
  tcr <- scope$ir_clonal_umap_data(
    "umap",
    "TCR",
    show_all = TRUE,
    split_output = TRUE
  )
  expect_identical(tcr$coloured_key, fixture$order)
  expect_identical(tcr$background_key, 3001:5000)
  expect_identical(tcr$trace_codes, c(NA, 1:5))
  expect_identical(lengths(tcr$x), c(2000L, 300L, 1050L, 1000L, 500L, 150L))
  bcr <- read_frame("BCR")
  expect_identical(bcr$trace_codes, c(0L, 3L))
  expect_identical(bcr$cells, 5000L)

  ## The descriptor the page sends instead of the coordinates and rows.
  scope$viewerProjectionAsset <- function(name) {
    list(
      protocol = "canonical-projection-v1",
      url = "projection/001.bin",
      cells = 5000L,
      dimensions = 2L
    )
  }
  scope$viewerStaticResourcePrefix <- function(kind, directory) "clonal"
  scope$viewerCanonicalResourceIdentity <- function(pack) {
    list(dataset_fingerprint = "md5-cell-set-v1:x")
  }
  resource <- scope$ir_clonal_frame_resource("umap", "TCR")
  expect_identical(resource$resource$protocol, "trajectory-frame-v1")
  expect_identical(resource$resource$cells, 5000L)
  expect_identical(resource$resource$canonical_cells, 5000L)
  expect_identical(resource$resource$subset$kind, "include_uint32")
  expect_identical(
    resource$resource$subset$url,
    "clonal/TCR.CTgene.subset.bin"
  )
  expect_identical(resource$resource$state$url, "clonal/TCR.CTgene.codes.bin")
  expect_identical(
    as.character(resource$traces),
    c("Other cells", scope$IR_CLONE_LABELS)
  )
  expect_identical(
    as.character(resource$colors),
    c("#D9D9D9", unname(scope$IR_EXPANSION_COLORS))
  )
  expect_identical(resource$resource$dataset_fingerprint, "md5-cell-set-v1:x")
  expect_identical(
    scope$ir_clonal_frame_rows(resource) + 1L,
    c(
      tcr$background_key,
      unlist(lapply(tcr$coloured_rows, function(at) tcr$coloured_key[at]))
    )
  )
  ## A file the browser could not read, or a 3-D projection: built in R.
  expect_null(scope$ir_clonal_frame_resource(
    "umap",
    "TCR",
    failed = "clonal/TCR.CTgene.codes.bin"
  ))
  scope$viewerProjectionAsset <- function(name) list(dimensions = 3L)
  expect_null(scope$ir_clonal_frame_resource("umap", "TCR"))
})

test_that("a Pack without valid Clonal frames is read as before", {
  skip_if_not_installed("qs2")
  root <- tempfile("viewer-pack-ir-clonal-old-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  fixture <- ir_clonal_pack_fixture(file.path(root, "dataset.crb"))
  pack <- buildViewerPack(fixture$path, viewer_binary = "always")
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  encoder <- new.env(parent = globalenv())
  sys.source(viewer_test_path("clone_contract.R"), envir = encoder)
  sys.source(viewer_test_path("immune_repertoire", "encode.R"), envir = encoder)
  read_frame <- function(
    pack,
    version = encoder$IR_CLONAL_FRAME_VERSION,
    bins = encoder$CEREBRO_CLONE_BINS,
    max_background = 100000L
  ) {
    runtime$viewerPackImmuneClonalFrame(
      pack,
      "TCR",
      "CTgene",
      version = version,
      bins = bins,
      max_background = max_background
    )
  }
  descriptor <- runtime$viewerPackOpen(fixture$path)
  expect_false(is.null(read_frame(descriptor)))
  ## A Pack built before the frames (format revision 1) has none.
  old <- descriptor
  old$manifest$immune_clonal_frames <- NULL
  old$manifest$format_revision <- NULL
  expect_null(read_frame(old))
  ## Another frame version, other bins or another background bound.
  expect_null(read_frame(descriptor, version = 2L))
  expect_null(read_frame(descriptor, bins = c(0, 1, 10, Inf)))
  expect_null(read_frame(descriptor, max_background = 5000L))
  ## A changed file fails its checksum (in a fresh process cache of checksums:
  ## the file keeps its size, and may keep its mtime).
  codes <- file.path(pack, "immune", "clonal", "TCR.CTgene.codes.bin")
  bytes <- readBin(codes, "raw", n = file.size(codes))
  bytes[[1L]] <- as.raw(5L)
  writeBin(bytes, codes)
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "viewer_pack.R"), envir = runtime)
  expect_null(read_frame(runtime$viewerPackOpen(fixture$path)))
})
