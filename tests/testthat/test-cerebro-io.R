## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

skip_if_no_h5 <- function() {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  skip_if_not_installed("Matrix")
  skip_if_not_installed("qs2")
}

h5_cells <- function(prefix = "cell-") paste0(prefix, seq_len(4L))

h5_counts <- function(cells = h5_cells()) {
  matrix(
    c(0, 1, 4, 2, 0, 5, 3, 6, 0, 7, 8, 9),
    nrow = 3L,
    dimnames = list(paste0("gene-", seq_len(3L)), cells)
  )
}

## The matrix is stored cells x genes with the call exportFromSeurat() makes, so
## /expression/genes holds the cell names and /expression/barcodes the genes.
write_h5_matrix <- function(counts, path) {
  if (file.exists(path)) {
    file.remove(path)
  }
  cells_by_genes <- methods::as(
    Matrix::Matrix(t(counts), sparse = TRUE),
    "CsparseMatrix"
  )
  HDF5Array::writeTENxMatrix(
    cells_by_genes,
    path,
    group = "expression",
    level = 0L
  )
  invisible(path)
}

make_h5_cerebro <- function(root, stem = "expression") {
  cells <- h5_cells()
  counts <- h5_counts(cells)
  h5 <- write_h5_matrix(counts, file.path(root, paste0(stem, ".h5")))

  object <- Cerebro$new()
  object$setExpressionBackend("h5", basename(h5))
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
  list(
    object = object,
    cells = cells,
    genes = rownames(counts),
    counts = counts,
    h5 = h5
  )
}

make_embedded_cerebro <- function(cells = h5_cells()) {
  object <- Cerebro$new()
  object$meta_data <- data.frame(
    cell_barcode = cells,
    sample = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  object$setExpression(h5_counts(cells))
  object
}

immune_tables <- function(cells = h5_cells()) {
  list(
    a = data.frame(
      barcode = cells[1:2],
      CTgene = c(
        "TRAV1.TRAJ1.TRAC_TRBV1.TRBJ1.TRBC1",
        "IGHV1.IGHJ1.IGHM_IGKV1.IGKJ1.IGKC"
      ),
      stringsAsFactors = FALSE
    ),
    b = data.frame(
      barcode = cells[3:4],
      CTgene = c("TRBV2.TRBJ2.TRBC2", "IGLV1.IGLJ1.IGLC1"),
      stringsAsFactors = FALSE
    )
  )
}

expect_cerebro_fields <- function(object, fixture) {
  expect_identical(object$getCellNames(), fixture$cells)
  expect_identical(object$meta_data$cell_barcode, fixture$cells)
  expect_identical(rownames(object$getProjection("umap")), fixture$cells)
  expect_s4_class(object$expression, "DelayedMatrix")
  expect_identical(colnames(object$expression), fixture$cells)
  expect_identical(rownames(object$expression), fixture$genes)
  expect_equal(as.matrix(object$expression), fixture$counts)
}

## A serialized Cerebro object as an older release wrote it. `drop` removes
## bindings the old class did not have, `extra` adds or overrides fields (the
## class no longer declares bcr_data / tcr_data, so they can only be added to a
## hand-built payload).
legacy_payload <- function(extra = list(), drop = character(), cls = NULL) {
  source <- Cerebro$new()
  payload <- new.env(parent = emptyenv())
  for (binding in setdiff(ls(source, all.names = TRUE), drop)) {
    payload[[binding]] <- source[[binding]]
  }
  for (field in names(extra)) {
    payload[[field]] <- extra[[field]]
  }
  class(payload) <- if (is.null(cls)) class(source) else cls
  lockEnvironment(payload, bindings = FALSE)
  payload
}

runtime_env <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  runtime
}

## ---------------------------------------------------------------------------
## H5 fixture
## ---------------------------------------------------------------------------

test_that("the H5 helpers read the cell and gene names of a cells x genes file", {
  skip_if_no_h5()
  fixture <- make_h5_cerebro(withr::local_tempdir())

  expect_identical(.h5CellNames(fixture$h5), fixture$cells)
  expect_identical(.h5GeneNames(fixture$h5), fixture$genes)
  expect_identical(.h5Dim(fixture$h5), c(4L, 3L))
  expect_identical(
    .cerebroCellNamesHash(fixture$cells),
    rlang::hash(enc2utf8(fixture$cells))
  )
  expect_match(.cerebroCellNamesHash(fixture$cells), "^[[:xdigit:]]{32}$")
})

## ---------------------------------------------------------------------------
## Thin schema v3
## ---------------------------------------------------------------------------

test_that("legacy-free RDS CRBs rebuild the current Cerebro class", {
  path <- tempfile(fileext = ".crb")
  object <- Cerebro$new()
  object$setExpression(matrix(
    1:4,
    nrow = 2L,
    dimnames = list(c("gene-1", "gene-2"), c("cell-1", "cell-2"))
  ))
  saveRDS(object, path)

  restored <- readCerebro(path)
  expect_s3_class(restored, "Cerebro")
  expect_equal(restored$expression, object$expression)
  expect_identical(
    restored$getExpressionBackend(),
    list(type = "embedded", location = NULL)
  )
})

test_that("thin v3 CRBs keep the cell names once, in the H5 file", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  rds <- file.path(root, "thin-rds.crb")
  qs <- file.path(root, "thin-qs2.crb")
  h5_before <- unname(tools::md5sum(fixture$h5))
  saveCerebro(fixture$object, rds, codec = "rds")
  saveCerebro(fixture$object, qs)

  ## both codecs share the one H5 file and saving leaves it alone
  expect_setequal(
    list.files(root),
    c("expression.h5", "thin-rds.crb", "thin-qs2.crb")
  )
  expect_identical(unname(tools::md5sum(fixture$h5)), h5_before)
  expect_identical(
    readBin(qs, "raw", n = 4L),
    as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))
  )
  expect_false(identical(
    readBin(rds, "raw", n = 4L),
    as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))
  ))

  for (path in c(rds, qs)) {
    payload <- .readCerebroPayload(path)
    expect_null(payload$expression)
    expect_identical(
      payload$expression_backend,
      list(type = "h5", location = "expression.h5")
    )
    expect_false("cell_barcode" %in% names(payload$meta_data))
    expect_type(payload$meta_data$nUMI, "integer")
    expect_type(payload$meta_data$nGene, "integer")
    expect_identical(payload$meta_data$sample, c("a", "a", "b", "b"))
    ## the projection keeps its row order only
    expect_true(.row_names_info(payload$projections$umap) < 0L)
    expect_identical(payload$projections$umap$UMAP_1, 1:4)

    schema <- payload$crb_schema
    expect_named(
      schema,
      c("version", "cell_names_hash", "projection_rownames", "n_cells"),
      ignore.order = TRUE
    )
    expect_identical(schema$version, 3L)
    expect_identical(schema$n_cells, 4L)
    expect_identical(schema$projection_rownames, "umap")
    expect_identical(
      schema$cell_names_hash,
      rlang::hash(enc2utf8(fixture$cells))
    )
    expect_identical(
      payload$cell_fingerprint,
      "md5-cell-set-v1:e4f597e835d59a5ae4da3f346939b09e"
    )
  }

  for (path in c(rds, qs)) {
    restored <- readCerebro(path)
    expect_cerebro_fields(restored, fixture)
    expect_identical(restored$meta_data$sample, c("a", "a", "b", "b"))
    expect_identical(restored$crb_schema$version, 3L)
  }

  ## the input object is not modified by saving it
  expect_identical(fixture$object$meta_data$cell_barcode, fixture$cells)
  expect_identical(rownames(fixture$object$projections$umap), fixture$cells)
})

test_that("only projections whose rownames are the cells are compacted", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  fixture$object$projections$other <- data.frame(
    X_1 = 1:4 + 0,
    X_2 = 4:1 + 0,
    row.names = paste0("point-", 1:4)
  )
  path <- file.path(root, "projections.crb")
  saveCerebro(fixture$object, path)

  payload <- .readCerebroPayload(path)
  expect_identical(payload$crb_schema$projection_rownames, "umap")
  expect_identical(
    rownames(payload$projections$other),
    paste0("point-", 1:4)
  )
  restored <- readCerebro(path)
  expect_identical(rownames(restored$getProjection("umap")), fixture$cells)
  expect_identical(
    rownames(restored$getProjection("other")),
    paste0("point-", 1:4)
  )
})

test_that("an H5-backed object must match its H5 file when it is saved", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  fixture$object$meta_data$cell_barcode <- rev(fixture$cells)

  expect_error(
    saveCerebro(fixture$object, file.path(root, "mismatch.crb")),
    "same cells in the same order"
  )
  expect_false(file.exists(file.path(root, "mismatch.crb")))
})

test_that("convertCerebro moves a thin CRB between codecs next to its H5 file", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  qs <- file.path(root, "thin-qs2.crb")
  saveCerebro(fixture$object, qs)

  converted <- file.path(root, "converted-rds.crb")
  convertCerebro(qs, converted, codec = "rds")
  expect_false(identical(
    readBin(converted, "raw", n = 4L),
    as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))
  ))
  expect_cerebro_fields(readCerebro(converted), fixture)
  expect_identical(.readCerebroPayload(converted)$crb_schema$version, 3L)

  ## the same directory spelled differently is still the same directory
  if (identical(.Platform$OS.type, "windows")) {
    spelled <- file.path(
      normalizePath(root, winslash = "\\"),
      "spelled.crb"
    )
    convertCerebro(qs, spelled, codec = "rds")
    expect_cerebro_fields(readCerebro(spelled), fixture)
  }

  other_root <- file.path(root, "elsewhere")
  cross_directory <- file.path(other_root, "converted-rds.crb")
  expect_error(
    convertCerebro(qs, cross_directory, codec = "rds"),
    "must be in the same directory"
  )
  expect_false(file.exists(cross_directory))

  convertCerebro(qs)
  expect_cerebro_fields(readCerebro(qs), fixture)
  expect_identical(
    readBin(qs, "raw", n = 4L),
    as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))
  )
  convertCerebro(qs, codec = "rds")
  expect_cerebro_fields(readCerebro(qs), fixture)
})

## ---------------------------------------------------------------------------
## Integrity of a thin CRB against its H5 file
## ---------------------------------------------------------------------------

test_that("a swapped or reordered H5 file is rejected", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  path <- file.path(root, "thin.crb")
  saveCerebro(fixture$object, path)
  expect_cerebro_fields(readCerebro(path), fixture)

  ## same size, other cell names
  write_h5_matrix(h5_counts(h5_cells("other-")), fixture$h5)
  expect_error(readCerebro(path), "H5 cell names do not match")

  ## same cells in another order
  order <- c(2L, 1L, 3L, 4L)
  write_h5_matrix(fixture$counts[, order], fixture$h5)
  expect_error(readCerebro(path), "H5 cell names do not match")

  ## fewer cells
  write_h5_matrix(fixture$counts[, 1:3], fixture$h5)
  expect_error(readCerebro(path), "H5 cell names do not match")

  write_h5_matrix(fixture$counts, fixture$h5)
  expect_cerebro_fields(readCerebro(path), fixture)
})

test_that("a thin CRB with a wrong cell count or malformed schema is rejected", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  path <- file.path(root, "thin.crb")
  saveCerebro(fixture$object, path)

  broken <- function(edit) {
    payload <- .currentCerebroCopy(.readCerebroPayload(path))
    edit(payload)
    target <- tempfile(tmpdir = root, fileext = ".crb")
    saveRDS(payload, target)
    target
  }

  wrong_count <- broken(function(p) p$crb_schema$n_cells <- 5L)
  expect_error(readCerebro(wrong_count), "cell count")

  extra_field <- broken(function(p) p$crb_schema$cell_names <- "expression")
  expect_error(readCerebro(extra_field), "unsupported CRB schema")

  double_version <- broken(function(p) p$crb_schema$version <- 3)
  expect_error(readCerebro(double_version), "retired format")

  bad_hash <- broken(function(p) {
    p$crb_schema$cell_names_hash <- strrep("0", 32L)
  })
  expect_error(readCerebro(bad_hash), "H5 cell names do not match")

  short_metadata <- broken(function(p) p$meta_data <- p$meta_data[1:3, ])
  expect_error(readCerebro(short_metadata), "incompatible cell metadata")

  missing_projection <- broken(function(p) p$projections <- list())
  expect_error(readCerebro(missing_projection), "missing projection")

  embedded <- broken(function(p) {
    p$expression_backend <- list(type = "embedded", location = NULL)
  })
  expect_error(readCerebro(embedded), "requires an H5 expression backend")
})

test_that("an H5 CRB written without a schema still reads", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  ## 5.1.x kept the cell names in the CRB: meta_data$cell_barcode and the
  ## projection rownames, with no crb_schema.
  legacy <- .currentCerebroCopy(fixture$object)
  expect_null(legacy$crb_schema)
  path <- file.path(root, "h5-without-schema.crb")
  saveRDS(legacy, path)

  expect_cerebro_fields(readCerebro(path), fixture)
  expect_null(readCerebro(path)$crb_schema)

  runtime <- runtime_env()
  runtime_object <- runtime$read_cerebro_file(path)
  runtime_object <- runtime$.attachExternalExpression(runtime_object, path)
  expect_true(rlang::env_binding_are_lazy(runtime_object, "expression"))
  expect_false(rlang::env_binding_are_lazy(runtime_object, "meta_data"))
  expect_identical(runtime_object$meta_data$cell_barcode, fixture$cells)

  ## converting it thins the CRB against the same H5 file
  convertCerebro(path)
  payload <- .readCerebroPayload(path)
  expect_identical(payload$crb_schema$version, 3L)
  expect_false("cell_barcode" %in% names(payload$meta_data))
  expect_cerebro_fields(readCerebro(path), fixture)
})

test_that("a missing H5 file is reported as a missing sidecar", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  path <- file.path(root, "thin.crb")
  saveCerebro(fixture$object, path)

  lonely <- file.path(root, "copied")
  dir.create(lonely)
  file.copy(path, file.path(lonely, "thin.crb"))
  expect_error(
    readCerebro(file.path(lonely, "thin.crb")),
    "expression sidecar is missing"
  )

  runtime <- runtime_env()
  expect_error(
    runtime$.attachExternalExpression(
      runtime$read_cerebro_file(file.path(lonely, "thin.crb")),
      file.path(lonely, "thin.crb")
    ),
    "Expected h5 file"
  )
})

## ---------------------------------------------------------------------------
## Viewer (runtime) side of a thin CRB
## ---------------------------------------------------------------------------

test_that("the Viewer hydrates a thin CRB lazily and checks it against the H5 file", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  qs <- file.path(root, "thin.crb")
  saveCerebro(fixture$object, qs)
  expected <- readCerebro(qs)

  runtime <- runtime_env()
  runtime_object <- runtime$read_cerebro_file(qs)
  runtime_object <- runtime$.attachExternalExpression(runtime_object, qs)
  expect_true(rlang::env_binding_are_lazy(runtime_object, "meta_data"))
  expect_true(rlang::env_binding_are_lazy(runtime_object, "projections"))
  expect_true(rlang::env_binding_are_lazy(runtime_object, "expression"))
  expect_identical(runtime$.runtimeCerebroCellCount(runtime_object), 4L)
  expect_true(rlang::env_binding_are_lazy(runtime_object, "meta_data"))
  first_frame <- attr(runtime_object, "cerebro_projection_first_frame")
  expect_identical(first_frame$gene_names, fixture$genes)
  expect_identical(first_frame$canonical_projections, "umap")

  expect_identical(runtime_object$meta_data, expected$meta_data)
  expect_identical(runtime_object$projections, expected$projections)
  expect_equal(as.matrix(runtime_object$expression), fixture$counts)

  ## a wrong cell count fails when the object is attached
  payload <- .currentCerebroCopy(.readCerebroPayload(qs))
  payload$crb_schema$n_cells <- 5L
  count_path <- file.path(root, "count.crb")
  saveRDS(payload, count_path)
  expect_error(
    runtime$.attachExternalExpression(
      runtime$read_cerebro_file(count_path),
      count_path
    ),
    "cell count"
  )

  ## a foreign H5 file fails when the metadata is first read
  write_h5_matrix(h5_counts(h5_cells("other-")), fixture$h5)
  foreign <- runtime$.attachExternalExpression(
    runtime$read_cerebro_file(qs),
    qs
  )
  expect_true(rlang::env_binding_are_lazy(foreign, "meta_data"))
  expect_error(foreign$meta_data, "H5 cell names do not match")
})

test_that("Data Info reads the thin cell count without forcing metadata", {
  runtime <- runtime_env()
  object <- new.env(parent = emptyenv())
  object$crb_schema <- list(version = 3L, n_cells = 4L)
  delayedAssign(
    "meta_data",
    stop("metadata was forced"),
    assign.env = object
  )

  expect_identical(runtime$.runtimeCerebroCellCount(object), 4L)
  expect_true(rlang::env_binding_are_lazy(object, "meta_data"))
})

## ---------------------------------------------------------------------------
## Retired formats
## ---------------------------------------------------------------------------

## Each entry is a CRB that older releases wrote and this version does not read.
retired_cerebros <- function(root) {
  cells <- h5_cells()
  embedded <- list(type = "embedded", location = NULL)
  tcr <- list(
    a = data.frame(
      barcode = cells[1:2],
      CTgene = rep("TRAV1.TRAJ1.TRAC_TRBV1.TRBJ1.TRBC1", 2L),
      stringsAsFactors = FALSE
    ),
    b = data.frame(
      barcode = cells[3L],
      CTgene = "TRBV2.TRBJ2.TRBC2",
      stringsAsFactors = FALSE
    )
  )
  bcr <- list(
    b = data.frame(
      barcode = cells[4L],
      CTgene = "IGHV1.IGHJ1.IGHM_IGKV1.IGKJ1.IGKC",
      stringsAsFactors = FALSE
    )
  )
  meta_data <- data.frame(
    cell_barcode = cells,
    sample = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )
  counts <- h5_counts(cells)
  base <- list(
    expression = counts,
    meta_data = meta_data,
    expression_backend = embedded
  )
  with_base <- function(...) utils::modifyList(base, list(...))
  ## Spatial entries as older releases stored them.
  fov_entry <- function(...) {
    c(
      list(
        coordinates = data.frame(
          x = c(10, 20, 30, 40),
          y = c(5, 15, 25, 35),
          row.names = cells
        ),
        expression = counts
      ),
      list(...)
    )
  }
  tissue <- "data:image/png;base64,AA=="
  molecules <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6),
    gene = c("A", "B", "A"),
    stringsAsFactors = FALSE
  )
  old_getter <- function(name) NULL
  environment(old_getter) <- baseenv()

  entries <- list(
    no_descriptor = list(
      payload = legacy_payload(list(
        expression = counts,
        meta_data = meta_data
      )),
      reason = "no expression-backend descriptor",
      upgrades = TRUE
    ),
    absent_descriptor = list(
      payload = legacy_payload(
        list(expression = counts, meta_data = meta_data),
        drop = c("expression_backend", "getExpressionBackend")
      ),
      reason = "no expression-backend descriptor",
      upgrades = TRUE
    ),
    schema_v1 = list(
      payload = legacy_payload(with_base(
        crb_schema = list(version = 1L, cell_names = "expression")
      )),
      reason = "schema version 1",
      upgrades = FALSE
    ),
    schema_v2 = list(
      payload = legacy_payload(with_base(
        crb_schema = list(version = 2L, n_cells = 4L)
      )),
      reason = "schema version 2",
      upgrades = FALSE
    ),
    bcr_tcr_data = list(
      payload = legacy_payload(with_base(tcr_data = tcr, bcr_data = bcr)),
      reason = "bcr_data/tcr_data",
      upgrades = TRUE
    ),
    retired_immune = list(
      payload = legacy_payload(with_base(
        immune_repertoire_backend = list(
          type = "retired-file",
          file = "immune_repertoire.qs2",
          md5 = strrep("a", 32L),
          samples = "a",
          chains = "TRB"
        )
      )),
      reason = "retired layout",
      upgrades = FALSE
    ),
    pre_spatial = list(
      payload = legacy_payload(
        with_base(),
        drop = c(
          "spatial",
          "addSpatialData",
          "availableSpatial",
          "getSpatialData"
        ),
        cls = c(paste0("Cerebro_v1", ".3"), "R6")
      ),
      reason = "predates spatial support",
      upgrades = TRUE
    ),
    singular_image = list(
      payload = legacy_payload(with_base(
        spatial = list(
          fov = fov_entry(
            histology_image = tissue,
            histology_image_bounds = list(
              ymax = 40,
              xmin = 0,
              ymin = 0,
              xmax = 100
            )
          )
        )
      )),
      reason = "spatial entry 'fov' stores a singular histology_image",
      upgrades = TRUE
    ),
    singular_bounds_only = list(
      payload = legacy_payload(with_base(
        spatial = list(
          fov = fov_entry(
            histology_images = list(
              DAPI = list(histology_image = tissue)
            ),
            histology_image_bounds = c(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
          )
        )
      )),
      reason = "spatial entry 'fov' stores a singular histology_image",
      upgrades = TRUE
    ),
    embedded_molecules = list(
      payload = legacy_payload(with_base(
        spatial = list(
          fov = fov_entry(molecules = molecules)
        )
      )),
      reason = "spatial entry 'fov' embeds its molecule table",
      upgrades = TRUE
    ),
    eager_spatial_getter = list(
      payload = legacy_payload(c(
        with_base(spatial = list(fov = fov_entry())),
        list(getSpatialData = old_getter)
      )),
      reason = "predates deferred spatial molecule loading",
      upgrades = TRUE
    ),
    pre_hla = list(
      payload = legacy_payload(
        with_base(),
        drop = c("hla_typing", "addHLATyping", "getHLATyping")
      ),
      reason = "predates HLA typing support",
      upgrades = TRUE
    ),
    pre_trekker = list(
      payload = legacy_payload(
        with_base(),
        drop = c("trekker", "addTrekker", "getTrekker")
      ),
      reason = "predates Trekker support",
      upgrades = TRUE
    ),
    pre_enriched_pathways = list(
      payload = legacy_payload(
        with_base(),
        drop = "getMethodsForEnrichedPathways"
      ),
      reason = "predates the enriched-pathways page",
      upgrades = TRUE
    )
  )
  for (name in names(entries)) {
    path <- file.path(root, paste0(name, ".crb"))
    saveRDS(entries[[name]]$payload, path)
    entries[[name]]$path <- path
  }
  ## A backend type this version does not know is not a retired format with a
  ## named reason: the readers report an invalid descriptor and convertCerebro()
  ## cannot repair it.
  unknown_backend <- file.path(root, "unknown_backend.crb")
  saveRDS(
    legacy_payload(with_base(
      expression_backend = list(type = "retired-store", location = "x.retired"),
      expression = NULL
    )),
    unknown_backend
  )
  list(
    entries = entries,
    unknown_backend = unknown_backend,
    tcr = tcr,
    bcr = bcr,
    counts = counts
  )
}

test_that("every retired format is rejected with a pointer to convertCerebro", {
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  runtime <- runtime_env()

  for (name in names(retired$entries)) {
    entry <- retired$entries[[name]]
    expect_error(
      readCerebro(entry$path),
      "uses a retired format",
      info = name
    )
    expect_error(
      readCerebro(entry$path),
      "convertCerebro",
      info = name
    )
    expect_error(
      readCerebro(entry$path),
      entry$reason,
      fixed = TRUE,
      info = name
    )
    expect_error(
      .assertCurrentCerebroFile(.readCerebroPayload(entry$path), entry$path),
      paste0("'", basename(entry$path), "' uses a retired format"),
      fixed = TRUE,
      info = name
    )
    ## the Viewer refuses the same files with the same message
    expect_error(
      runtime$.attachExternalExpression(
        runtime$read_cerebro_file(entry$path),
        entry$path
      ),
      "uses a retired format.*convertCerebro",
      info = name
    )
    ## and the package and the Viewer name the same reason
    stored <- .readCerebroPayload(entry$path)
    expect_identical(
      runtime$.runtimeLegacyReason(stored),
      .legacyCerebroReason(stored),
      info = name
    )
  }
})

test_that("only a spatial entry in a retired form is gated", {
  root <- withr::local_tempdir()
  path <- file.path(root, "spatial.crb")
  runtime <- runtime_env()
  coordinates <- data.frame(x = c(1, 2), y = c(3, 4))
  object <- Cerebro$new()
  object$setExpressionBackend("embedded")
  object$addSpatialData(
    "fov",
    list(
      coordinates = coordinates,
      expression = matrix(numeric(), nrow = 0L, ncol = 2L),
      histology_images = list(
        DAPI = list(histology_image = "data:image/png;base64,AA==")
      ),
      molecules = data.frame(x = 1, y = 2, gene = "A")
    )
  )
  saveCerebro(object, path)
  stored <- .readCerebroPayload(path)

  ## a canonical image and a molecule sidecar reference are current
  expect_s3_class(stored$spatial$fov$molecules, "CerebroSpatialMoleculeRef")
  expect_null(.legacySpatialReason(stored))
  expect_null(runtime$.runtimeLegacySpatialReason(stored))
  expect_s3_class(readCerebro(path), "Cerebro")

  ## an in-memory object keeps table molecules until it is saved; only a file
  ## read from disk is checked
  expect_s3_class(object$spatial$fov$molecules, "data.frame")
  expect_s3_class(stored$spatial$fov$molecules, "CerebroSpatialMoleculeRef")
})

test_that("current formats are not mistaken for retired ones", {
  root <- withr::local_tempdir()
  embedded <- make_embedded_cerebro()
  path <- file.path(root, "embedded.crb")
  saveCerebro(embedded, path)
  expect_null(.legacyCerebroReason(.readCerebroPayload(path)))
  expect_s3_class(readCerebro(path), "Cerebro")
})

test_that("the package and the Viewer require the same accessors", {
  expect_identical(
    runtime_env()$.runtimeRequiredCerebroMethods(),
    .requiredCerebroMethods
  )
  ## every required accessor is a method of the current class
  expect_true(all(
    names(.requiredCerebroMethods) %in% names(Cerebro$public_methods)
  ))
})

test_that("the Viewer gate also runs from the process-wide function cache", {
  cache <- new.env(parent = globalenv())
  sys.source(viewer_test_path("source_cache.R"), envir = cache)
  scope <- new.env(parent = globalenv())
  withr::local_options(cerebro.quiet_runtime = TRUE)
  cache$viewerSource(viewer_test_path("utility_functions.R"), scope)

  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  current <- file.path(root, "current.crb")
  saveCerebro(make_embedded_cerebro(), current)
  expect_null(scope$.runtimeLegacyReason(.readCerebroPayload(current)))
  expect_match(
    scope$.runtimeLegacyReason(
      .readCerebroPayload(retired$entries$pre_hla$path)
    ),
    "predates HLA typing support"
  )
})

test_that("convertCerebro upgrades what can be repaired without the source", {
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)

  for (name in names(retired$entries)) {
    entry <- retired$entries[[name]]
    if (!entry$upgrades) {
      next
    }
    expect_error(readCerebro(entry$path), "retired format", info = name)
    expect_no_error(convertCerebro(entry$path))

    converted <- readCerebro(entry$path)
    expect_identical(
      converted$getExpressionBackend(),
      list(type = "embedded", location = NULL),
      info = name
    )
    expect_equal(converted$expression, retired$counts, info = name)
    expect_false(
      exists("tcr_data", envir = converted, inherits = FALSE),
      info = name
    )
    expect_false(
      exists("bcr_data", envir = converted, inherits = FALSE),
      info = name
    )
    for (accessor in names(.requiredCerebroMethods)) {
      expect_true(
        is.function(converted[[accessor]]),
        info = paste(name, accessor)
      )
    }
    expect_equal(nrow(converted$getHLATyping()), 0L, info = name)
    expect_null(converted$getTrekker(), info = name)
    expect_identical(
      converted$getMethodsForEnrichedPathways(),
      NULL,
      info = name
    )
    expect_null(.legacyCerebroReason(.readCerebroPayload(entry$path)))
  }

  ## the receptors of bcr_data / tcr_data end up in the unified repertoire
  converted <- readCerebro(retired$entries$bcr_tcr_data$path)
  repertoire <- converted$getImmuneRepertoire()
  expect_named(repertoire, c("a", "b"))
  expect_equal(nrow(repertoire$a), 2L)
  expect_equal(nrow(repertoire$b), 2L)
  expect_setequal(repertoire$b$barcode, c("cell-3", "cell-4"))
  expect_named(converted$getTCR(), c("a", "b"))
  expect_equal(nrow(converted$getTCR()$a), 2L)
  expect_identical(converted$getTCR()$b$barcode, "cell-3")
  expect_named(converted$getBCR(), "b")
  expect_identical(converted$getBCR()$b$barcode, "cell-4")
})

test_that("convertCerebro repairs singular spatial images", {
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  bounds <- c(xmin = 0, xmax = 100, ymin = 0, ymax = 40)

  convertCerebro(retired$entries$singular_image$path)
  converted <- readCerebro(retired$entries$singular_image$path)
  stored <- converted$spatial$fov
  expect_false(any(
    c("histology_image", "histology_image_bounds") %in% names(stored)
  ))
  expect_named(stored$histology_images, "Tissue background")
  expect_identical(
    stored$histology_images[["Tissue background"]]$histology_image,
    "data:image/png;base64,AA=="
  )
  ## list-valued bounds become the numeric vector of the current form
  expect_identical(
    stored$histology_images[["Tissue background"]]$histology_image_bounds,
    bounds
  )
  expect_identical(
    converted$getSpatialData("fov")$histology_images,
    stored$histology_images
  )

  ## a canonical manifest wins over stray singular fields, which are dropped
  convertCerebro(retired$entries$singular_bounds_only$path)
  converted <- readCerebro(retired$entries$singular_bounds_only$path)
  expect_named(converted$spatial$fov$histology_images, "DAPI")
  expect_false("histology_image_bounds" %in% names(converted$spatial$fov))
})

test_that("a singular spatial image that cannot be normalized stops convertCerebro", {
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  path <- retired$entries$singular_image$path
  payload <- .readCerebroPayload(path)
  entry <- payload$spatial$fov
  entry$histology_image <- "not-a-data-uri"
  broken <- legacy_payload(list(
    expression = retired$counts,
    meta_data = payload$meta_data,
    expression_backend = payload$expression_backend,
    spatial = list(fov = entry)
  ))
  broken_path <- file.path(root, "broken.crb")
  saveRDS(broken, broken_path)
  before <- unname(tools::md5sum(broken_path))

  expect_error(convertCerebro(broken_path), "Tissue background.*data:image")
  expect_identical(unname(tools::md5sum(broken_path)), before)
})

test_that("convertCerebro moves embedded spatial molecules into the sidecar", {
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  path <- retired$entries$embedded_molecules$path
  molecules <- .readCerebroPayload(path)$spatial$fov$molecules
  expect_s3_class(molecules, "data.frame")
  expect_error(readCerebro(path), "embeds its molecule table")

  convertCerebro(path)
  expect_s3_class(
    .readCerebroPayload(path)$spatial$fov$molecules,
    "CerebroSpatialMoleculeRef"
  )
  expect_true(dir.exists(file.path(root, "embedded_molecules.spatial")))
  converted <- readCerebro(path)
  expect_s3_class(converted$spatial$fov$molecules, "CerebroSpatialMoleculeRef")
  expect_identical(converted$getSpatialData("fov")$molecules, molecules)
})

test_that("convertCerebro refuses a CRB whose storage it cannot read or repair", {
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)

  paths <- c(
    unknown_backend = retired$unknown_backend,
    schema_v1 = retired$entries$schema_v1$path,
    schema_v2 = retired$entries$schema_v2$path,
    retired_immune = retired$entries$retired_immune$path
  )
  for (name in names(paths)) {
    path <- paths[[name]]
    before <- unname(tools::md5sum(path))
    expect_error(
      convertCerebro(path),
      "cannot read or repair",
      info = name
    )
    expect_error(convertCerebro(path), "Re-export", info = name)
    expect_identical(unname(tools::md5sum(path)), before, info = name)
  }
})

test_that("readCerebro rejects an expression backend it does not know", {
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  expect_error(
    readCerebro(retired$unknown_backend),
    "expression-backend descriptor is invalid"
  )
})

## ---------------------------------------------------------------------------
## Immune repertoire sidecar
## ---------------------------------------------------------------------------

test_that("immune_sidecar = \"always\" writes the repertoire next to the CRB", {
  skip_if_not_installed("qs2")

  for (codec in c("qs2", "rds")) {
    root <- withr::local_tempdir()
    object <- make_embedded_cerebro()
    object$addImmuneRepertoire(immune_tables())
    path <- file.path(root, "dataset.crb")
    saveCerebro(object, path, codec = codec, immune_sidecar = "always")

    sidecar <- file.path(root, paste0("dataset.immune.", codec))
    expect_true(file.exists(sidecar), info = codec)
    expect_setequal(
      list.files(root),
      c("dataset.crb", basename(sidecar))
    )

    payload <- .readCerebroPayload(path)
    expect_length(payload$immune_repertoire, 0L)
    descriptor <- payload$immune_repertoire_backend
    expect_identical(descriptor$type, "file", info = codec)
    expect_identical(descriptor$file, basename(sidecar), info = codec)
    expect_identical(descriptor$md5, unname(tools::md5sum(sidecar)))
    expect_identical(descriptor$samples, c("a", "b"))
    expect_identical(descriptor$chains, c("TRA", "TRB", "IGH", "IGK", "IGL"))
    expect_false("root" %in% names(descriptor))
    expect_setequal(
      names(descriptor),
      c("type", "file", "md5", "samples", "chains")
    )
    ## the input object keeps its repertoire
    expect_length(object$immune_repertoire, 2L)

    restored <- readCerebro(path)
    expect_length(restored$immune_repertoire, 0L)
    expect_identical(
      normalizePath(restored$immune_repertoire_backend$root),
      normalizePath(root)
    )
    expect_identical(restored$getImmuneRepertoire(), immune_tables())
    expect_identical(restored$immune_repertoire, immune_tables())
  }
})

test_that("the immune sidecar is checked when the repertoire is loaded", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "dataset.crb")
  saveCerebro(object, path, immune_sidecar = "always")
  sidecar <- file.path(root, "dataset.immune.qs2")
  original <- readBin(sidecar, "raw", file.info(sidecar)$size)

  ## reading the CRB does not read the repertoire, so a corrupt file shows up
  ## only when it is first needed
  writeBin(c(original[-1L], as.raw(0L)), sidecar)
  restored <- readCerebro(path)
  expect_error(restored$getImmuneRepertoire(), "checksum does not match")
  expect_error(restored$getTCR(), "checksum does not match")
  expect_length(restored$immune_repertoire, 0L)

  writeBin(original, sidecar)
  expect_identical(restored$getImmuneRepertoire(), immune_tables())

  ## a sidecar that vanishes after the CRB was read
  restored <- readCerebro(path)
  file.remove(sidecar)
  expect_error(restored$getImmuneRepertoire(), "sidecar is missing")

  ## and one that is missing when the CRB is read
  expect_error(readCerebro(path), "immune repertoire sidecar is missing")

  ## a CRB copied without its sidecar
  writeBin(original, sidecar)
  alone <- file.path(withr::local_tempdir(), "dataset.crb")
  file.copy(path, alone)
  expect_error(readCerebro(alone), "immune repertoire sidecar is missing")
})

test_that("a malformed immune sidecar descriptor is rejected", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "dataset.crb")
  saveCerebro(object, path, immune_sidecar = "always")

  edit <- function(change) {
    payload <- .currentCerebroCopy(.readCerebroPayload(path))
    descriptor <- payload$immune_repertoire_backend
    payload$immune_repertoire_backend <- change(descriptor)
    target <- tempfile(tmpdir = root, fileext = ".crb")
    saveRDS(payload, target)
    target
  }
  expect_error(
    readCerebro(edit(function(d) utils::modifyList(d, list(md5 = "abc")))),
    "descriptor is invalid"
  )
  expect_error(
    readCerebro(edit(function(d) {
      utils::modifyList(d, list(file = "../x.qs2"))
    })),
    "descriptor is invalid"
  )
  expect_error(
    readCerebro(edit(function(d) {
      utils::modifyList(d, list(type = "directory"))
    })),
    "retired format"
  )
})

test_that("immune_sidecar = \"auto\" externalises only a large H5-backed repertoire", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  fixture$object$addImmuneRepertoire(immune_tables())
  sidecar <- file.path(root, "h5.immune.qs2")

  ## below the default threshold the repertoire stays in the CRB
  saveCerebro(fixture$object, file.path(root, "h5.crb"))
  expect_false(file.exists(sidecar))
  expect_length(
    .readCerebroPayload(file.path(root, "h5.crb"))$immune_repertoire,
    2L
  )

  ## the threshold counts repertoire rows and is inclusive
  saveCerebro(
    fixture$object,
    file.path(root, "h5.crb"),
    immune_sidecar_threshold = 5L
  )
  expect_false(file.exists(sidecar))
  saveCerebro(
    fixture$object,
    file.path(root, "h5.crb"),
    immune_sidecar_threshold = 4L
  )
  expect_true(file.exists(sidecar))
  payload <- .readCerebroPayload(file.path(root, "h5.crb"))
  expect_length(payload$immune_repertoire, 0L)
  expect_identical(payload$immune_repertoire_backend$file, "h5.immune.qs2")
  restored <- readCerebro(file.path(root, "h5.crb"))
  expect_cerebro_fields(restored, fixture)
  expect_identical(restored$getImmuneRepertoire(), immune_tables())
  file.remove(sidecar)

  ## "never" keeps it embedded whatever the size
  saveCerebro(
    fixture$object,
    file.path(root, "h5.crb"),
    immune_sidecar = "never",
    immune_sidecar_threshold = 1L
  )
  expect_false(file.exists(sidecar))
  expect_length(
    .readCerebroPayload(file.path(root, "h5.crb"))$immune_repertoire,
    2L
  )

  ## "always" also applies to an H5-backed CRB
  saveCerebro(
    fixture$object,
    file.path(root, "h5.crb"),
    immune_sidecar = "always"
  )
  expect_true(file.exists(sidecar))
})

test_that("auto never moves the repertoire of an embedded CRB out of the file", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "embedded.crb")

  saveCerebro(object, path, immune_sidecar_threshold = 1L)
  expect_identical(list.files(root), "embedded.crb")
  payload <- .readCerebroPayload(path)
  expect_length(payload$immune_repertoire, 2L)
  expect_null(payload$immune_repertoire_backend)

  saveCerebro(object, path, immune_sidecar = "never")
  expect_identical(list.files(root), "embedded.crb")

  ## nothing to externalise, nothing written
  saveCerebro(make_embedded_cerebro(), path, immune_sidecar = "always")
  expect_identical(list.files(root), "embedded.crb")
})

test_that("saveCerebro validates the immune sidecar options", {
  object <- make_embedded_cerebro()
  path <- file.path(withr::local_tempdir(), "options.crb")

  expect_error(
    saveCerebro(object, path, immune_sidecar = "sometimes"),
    "should be one of"
  )
  expect_error(
    saveCerebro(object, path, immune_sidecar_threshold = -1),
    "non-negative"
  )
  expect_error(
    saveCerebro(object, path, immune_sidecar_threshold = c(1, 2)),
    "non-negative"
  )
  expect_false(file.exists(path))
})

test_that("saving an object that already has a sidecar reuses it", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  fixture$object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "dataset.crb")
  saveCerebro(fixture$object, path, immune_sidecar = "always")
  sidecar <- file.path(root, "dataset.immune.qs2")
  md5 <- unname(tools::md5sum(sidecar))
  info <- file.info(sidecar)

  ## a re-save keeps the descriptor and does not rewrite the file
  restored <- readCerebro(path)
  saveCerebro(restored, path, immune_sidecar = "always")
  expect_identical(unname(tools::md5sum(sidecar)), md5)
  expect_identical(.readCerebroPayload(path)$immune_repertoire_backend$md5, md5)
  expect_identical(file.info(sidecar)$mtime, info$mtime)

  ## also once the repertoire was loaded into the object
  restored$getImmuneRepertoire()
  expect_length(restored$immune_repertoire, 2L)
  saveCerebro(restored, path, immune_sidecar = "never")
  expect_length(.readCerebroPayload(path)$immune_repertoire, 0L)
  expect_identical(unname(tools::md5sum(sidecar)), md5)
  expect_identical(
    readCerebro(path)$getImmuneRepertoire(),
    immune_tables()
  )

  ## and when it is saved under another name in the same directory
  copy <- file.path(root, "copy.crb")
  saveCerebro(readCerebro(path), copy)
  expect_false(file.exists(file.path(root, "copy.immune.qs2")))
  expect_identical(
    .readCerebroPayload(copy)$immune_repertoire_backend$file,
    "dataset.immune.qs2"
  )
  expect_identical(readCerebro(copy)$getImmuneRepertoire(), immune_tables())

  ## convertCerebro carries the sidecar along
  convertCerebro(path, codec = "rds")
  expect_identical(unname(tools::md5sum(sidecar)), md5)
  expect_identical(readCerebro(path)$getImmuneRepertoire(), immune_tables())
})

test_that("replacing a CRB replaces its own immune sidecar", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "dataset.crb")
  sidecar <- file.path(root, "dataset.immune.qs2")
  saveCerebro(object, path, immune_sidecar = "always")
  first <- unname(tools::md5sum(sidecar))

  changed <- immune_tables()
  changed$a$CTgene[[1L]] <- "TRGV1.TRGJ1.TRGC1"
  object$addImmuneRepertoire(changed)
  saveCerebro(object, path, immune_sidecar = "always")

  expect_false(identical(unname(tools::md5sum(sidecar)), first))
  expect_identical(readCerebro(path)$getImmuneRepertoire(), changed)
  expect_setequal(
    list.files(root, all.files = TRUE, no.. = TRUE),
    c(
      "dataset.crb",
      "dataset.immune.qs2"
    )
  )
})

## ---------------------------------------------------------------------------
## getBCR() / getTCR() derive from the unified repertoire
## ---------------------------------------------------------------------------

test_that("getTCR and getBCR split the unified repertoire by chain", {
  object <- Cerebro$new()
  expect_identical(object$getTCR(), list())
  expect_identical(object$getBCR(), list())

  object$addImmuneRepertoire(list(
    mixed = data.frame(
      barcode = paste0("c", 1:5),
      CTgene = c(
        "TRAV1.TRAJ1.TRAC_TRBV1.TRBJ1.TRBC1",
        "IGHV1.IGHJ1.IGHM_IGKV1.IGKJ1.IGKC",
        "TRGV1.TRGJ1.TRGC1_TRDV1.TRDJ1.TRDC",
        "IGLV1.IGLJ1.IGLC1",
        NA
      ),
      stringsAsFactors = FALSE
    ),
    tcr_only = data.frame(
      barcode = c("t1", "t2"),
      CTgene = c("TRBV5.TRBJ1.TRBC1", "TRAV9.TRAJ9.TRAC"),
      stringsAsFactors = FALSE
    ),
    bcr_only = data.frame(
      barcode = "b1",
      CTgene = "IGHV3.IGHJ3.IGHG1_IGKV3.IGKJ3.IGKC",
      stringsAsFactors = FALSE
    ),
    no_gene_column = data.frame(barcode = "x1", stringsAsFactors = FALSE),
    empty = data.frame(
      barcode = character(),
      CTgene = character(),
      stringsAsFactors = FALSE
    )
  ))

  tcr <- object$getTCR()
  expect_named(tcr, c("mixed", "tcr_only"))
  expect_identical(tcr$mixed$barcode, c("c1", "c3"))
  expect_identical(tcr$tcr_only$barcode, c("t1", "t2"))

  bcr <- object$getBCR()
  expect_named(bcr, c("mixed", "bcr_only"))
  expect_identical(bcr$mixed$barcode, c("c2", "c4"))
  expect_identical(bcr$bcr_only$barcode, "b1")

  ## the unified repertoire itself is untouched
  expect_named(
    object$getImmuneRepertoire(),
    c("mixed", "tcr_only", "bcr_only", "no_gene_column", "empty")
  )
  expect_identical(nrow(object$getImmuneRepertoire()$mixed), 5L)
})

test_that("getTCR and getBCR read a file-backed repertoire lazily", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "dataset.crb")
  saveCerebro(object, path, immune_sidecar = "always")

  restored <- readCerebro(path)
  expect_length(restored$immune_repertoire, 0L)
  expect_named(restored$getTCR(), c("a", "b"))
  expect_identical(restored$getTCR()$a$barcode, "cell-1")
  expect_identical(restored$getTCR()$b$barcode, "cell-3")
  expect_named(restored$getBCR(), c("a", "b"))
  expect_identical(restored$getBCR()$a$barcode, "cell-2")
  expect_identical(restored$getBCR()$b$barcode, "cell-4")
})

test_that("the Cerebro class no longer stores bcr_data or tcr_data", {
  object <- Cerebro$new()
  expect_false(any(c("bcr_data", "tcr_data") %in% names(Cerebro$public_fields)))
  expect_false(any(
    c("addBCRData", "addTCRData") %in% names(Cerebro$public_methods)
  ))
  expect_false(exists("bcr_data", envir = object, inherits = FALSE))
  expect_false(exists("tcr_data", envir = object, inherits = FALSE))
  expect_error(
    object$setExpressionBackend("retired-store", "x.retired"),
    "one of"
  )
})

## ---------------------------------------------------------------------------
## Spatial molecules and bundled files
## ---------------------------------------------------------------------------

test_that("spatial molecules are loaded one FOV at a time from a sidecar", {
  skip_if_not_installed("qs2")
  root <- withr::local_tempdir()
  path <- file.path(root, "spatial.crb")
  molecules <- data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6),
    gene = c("A", "B", "A")
  )
  object <- Cerebro$new()
  object$spatial <- list(
    fov1 = list(
      coordinates = data.frame(x = c(0, 10), y = c(0, 10)),
      expression = matrix(numeric(), nrow = 0L, ncol = 2L),
      molecules = molecules
    )
  )

  saveCerebro(object, path)
  payload <- .readCerebroPayload(path)
  expect_s3_class(
    payload$spatial$fov1$molecules,
    "CerebroSpatialMoleculeRef"
  )
  expect_true(dir.exists(file.path(root, "spatial.spatial")))

  restored <- readCerebro(path)
  expect_s3_class(
    restored$spatial$fov1$molecules,
    "CerebroSpatialMoleculeRef"
  )
  summary <- restored$getSpatialData("fov1", hydrate_molecules = FALSE)
  expect_s3_class(summary$molecules, "CerebroSpatialMoleculeRef")
  expect_s3_class(
    restored$spatial$fov1$molecules,
    "CerebroSpatialMoleculeRef"
  )
  expect_identical(restored$getSpatialData("fov1")$molecules, molecules)
  expect_identical(restored$spatial$fov1$molecules, molecules)

  runtime <- new.env(parent = globalenv())
  sys.source(viewer_test_path("utility_functions.R"), envir = runtime)
  runtime_object <- runtime$read_cerebro_file(path)
  runtime_object <- runtime$.attachSpatialMoleculeBackend(runtime_object, path)
  expect_s3_class(
    runtime_object$spatial$fov1$molecules,
    "CerebroSpatialMoleculeRef"
  )
  expect_identical(runtime_object$getSpatialData("fov1")$molecules, molecules)
})

test_that("bundled demo CRBs use the current qs2 codec", {
  demo_dir <- file.path(viewer_app_test_path(), "extdata", "examples")
  demos <- list.files(
    demo_dir,
    pattern = "\\.crb$",
    full.names = TRUE
  )

  expect_length(demos, 9L)
  for (path in demos) {
    connection <- file(path, open = "rb")
    magic <- readBin(connection, "raw", n = 4L)
    close(connection)
    expect_identical(
      magic,
      as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)),
      info = basename(path)
    )
    expect_true(inherits(readCerebro(path), "Cerebro"), info = basename(path))
  }
})

## ---------------------------------------------------------------------------
## Gate parity, failed saves and malformed descriptors
## ---------------------------------------------------------------------------

test_that("the package and the Viewer agree on which CRBs are retired", {
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  runtime <- runtime_env()

  current <- file.path(root, "current.crb")
  saveCerebro(make_embedded_cerebro(), current)
  paths <- c(
    vapply(retired$entries, function(entry) entry$path, character(1)),
    unknown_backend = retired$unknown_backend,
    current = current
  )
  for (name in names(paths)) {
    object <- .readCerebroPayload(paths[[name]])
    expect_identical(
      .legacyCerebroReason(object),
      runtime$.runtimeLegacyReason(object),
      info = name
    )
  }

  ## a malformed (active) descriptor is left to the readers by both
  payload <- new.env(parent = emptyenv())
  makeActiveBinding("expression_backend", function() NULL, payload)
  payload$availableSpatial <- function() character()
  expect_identical(
    .legacyCerebroReason(payload),
    runtime$.runtimeLegacyReason(payload)
  )
})

test_that("the package and the Viewer read the same CRB schema and H5 names", {
  skip_if_no_h5()
  runtime <- runtime_env()
  fixture <- make_h5_cerebro(withr::local_tempdir())
  expect_identical(
    .h5CellNames(fixture$h5),
    runtime$.runtimeH5Names(fixture$h5, "genes")
  )
  expect_identical(
    .h5GeneNames(fixture$h5),
    runtime$.runtimeH5Names(fixture$h5, "barcodes")
  )

  valid <- list(
    version = 3L,
    cell_names_hash = .cerebroCellNamesHash(fixture$cells),
    projection_rownames = character(),
    n_cells = 4L
  )
  schemas <- list(
    valid = valid,
    v2 = utils::modifyList(valid, list(version = 2L)),
    short_hash = utils::modifyList(valid, list(cell_names_hash = "abc")),
    extra_field = c(valid, list(other = 1L)),
    missing_field = valid[c("version", "n_cells")]
  )
  for (name in names(schemas)) {
    object <- legacy_payload(list(crb_schema = schemas[[name]]))
    package_result <- tryCatch(
      .readCrbSchema(object, "x.crb"),
      error = function(error) "error"
    )
    viewer_result <- tryCatch(
      runtime$.readRuntimeCrbSchema(object, "x.crb"),
      error = function(error) "error"
    )
    expect_identical(package_result, viewer_result, info = name)
  }
})

test_that("a failed save leaves no staged immune sidecar behind", {
  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  testthat::local_mocked_bindings(
    .stageSpatialMolecules = function(...) stop("spatial staging failed"),
    .package = "CerebroNexus"
  )
  expect_error(
    saveCerebro(
      object,
      file.path(root, "dataset.crb"),
      immune_sidecar = "always"
    ),
    "spatial staging failed"
  )
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("convertCerebro rejects a malformed expression backend", {
  root <- withr::local_tempdir()
  path <- file.path(root, "junk.crb")
  saveRDS(
    legacy_payload(list(
      expression = h5_counts(h5_cells()),
      meta_data = data.frame(cell_barcode = h5_cells()),
      expression_backend = "junk"
    )),
    path
  )
  before <- unname(tools::md5sum(path))
  expect_error(
    convertCerebro(path),
    "unsupported expression-backend descriptor"
  )
  expect_identical(unname(tools::md5sum(path)), before)
})

test_that("saveCerebro removes a sidecar the new CRB no longer uses", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "dataset.crb")

  saveCerebro(object, path, immune_sidecar = "always")
  expect_setequal(list.files(root), c("dataset.crb", "dataset.immune.qs2"))

  ## the repertoire moves back into the CRB
  saveCerebro(object, path, immune_sidecar = "never")
  expect_identical(list.files(root), "dataset.crb")
  expect_identical(readCerebro(path)$getImmuneRepertoire(), immune_tables())

  ## another codec replaces the file instead of adding a second one
  saveCerebro(object, path, immune_sidecar = "always")
  saveCerebro(object, path, codec = "rds", immune_sidecar = "always")
  expect_setequal(list.files(root), c("dataset.crb", "dataset.immune.rds"))
  expect_identical(readCerebro(path)$getImmuneRepertoire(), immune_tables())

  ## the same codec keeps the file
  saveCerebro(object, path, codec = "rds", immune_sidecar = "always")
  expect_setequal(list.files(root), c("dataset.crb", "dataset.immune.rds"))
})

test_that("saveCerebro keeps a sidecar its CRB still points to", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  path <- file.path(root, "dataset.crb")
  saveCerebro(object, path, immune_sidecar = "always")

  ## a file-backed repertoire keeps its sidecar, whatever the mode
  restored <- readCerebro(path)
  saveCerebro(restored, path, immune_sidecar = "never")
  expect_true(file.exists(file.path(root, "dataset.immune.qs2")))
  expect_identical(readCerebro(path)$getImmuneRepertoire(), immune_tables())

  ## a copy shares it, and saving the copy removes nothing of the original
  copy <- file.path(root, "copy.crb")
  saveCerebro(readCerebro(path), copy)
  saveCerebro(readCerebro(path), copy)
  expect_true(file.exists(file.path(root, "dataset.immune.qs2")))
  expect_identical(readCerebro(copy)$getImmuneRepertoire(), immune_tables())

  ## moving the repertoire back is explicit, and the copy still needs the file
  moved <- readCerebro(path)
  moved$addImmuneRepertoire(moved$getImmuneRepertoire())
  saveCerebro(moved, path, immune_sidecar = "never")
  expect_length(.readCerebroPayload(path)$immune_repertoire, 2L)
  expect_true(file.exists(file.path(root, "dataset.immune.qs2")))
  expect_identical(readCerebro(copy)$getImmuneRepertoire(), immune_tables())

  ## once nothing points to it any more, it goes
  saveCerebro(moved, copy, immune_sidecar = "never")
  saveCerebro(moved, path, immune_sidecar = "always")
  saveCerebro(moved, path, immune_sidecar = "never")
  expect_false(file.exists(file.path(root, "dataset.immune.qs2")))
})

test_that("saveCerebro never removes a sidecar another CRB owns", {
  skip_if_not_installed("qs2")

  root <- withr::local_tempdir()
  object <- make_embedded_cerebro()
  object$addImmuneRepertoire(immune_tables())
  saveCerebro(object, file.path(root, "a.crb"), immune_sidecar = "always")
  saveCerebro(object, file.path(root, "b.crb"), immune_sidecar = "always")

  saveCerebro(object, file.path(root, "b.crb"), immune_sidecar = "never")
  expect_setequal(
    list.files(root),
    c("a.crb", "a.immune.qs2", "b.crb")
  )
})

test_that("the Viewer checks the H5 cell names before it reads expression", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  path <- file.path(root, "thin.crb")
  saveCerebro(fixture$object, path)
  runtime <- runtime_env()
  attach <- function() {
    runtime$.attachExternalExpression(runtime$read_cerebro_file(path), path)
  }

  expect_equal(as.matrix(attach()$expression), fixture$counts)

  ## another data set with as many cells opens, but its expression is refused
  write_h5_matrix(h5_counts(h5_cells("other-")), fixture$h5)
  foreign <- attach()
  expect_error(foreign$expression, "H5 cell names do not match")
})

test_that("the cell names are read once for hydration and expression", {
  skip_if_no_h5()

  root <- withr::local_tempdir()
  fixture <- make_h5_cerebro(root)
  path <- file.path(root, "thin.crb")
  saveCerebro(fixture$object, path)
  runtime <- runtime_env()
  reads <- 0L
  original <- runtime$.runtimeH5Names
  runtime$.runtimeH5Names <- function(sidecar, dataset) {
    if (identical(dataset, "genes")) {
      reads <<- reads + 1L
    }
    original(sidecar, dataset)
  }
  object <- runtime$.attachExternalExpression(
    runtime$read_cerebro_file(path),
    path
  )
  expect_identical(reads, 0L)
  object$meta_data
  expect_identical(reads, 1L)
  expect_equal(as.matrix(object$expression), fixture$counts)
  expect_identical(reads, 1L)

  ## expression alone reads them once as well
  other <- runtime$.attachExternalExpression(
    runtime$read_cerebro_file(path),
    path
  )
  reads <- 0L
  other$expression
  other$expression
  other$meta_data
  expect_identical(reads, 2L)
})

test_that("a retired format that convertCerebro cannot repair says to re-export", {
  root <- withr::local_tempdir()
  retired <- retired_cerebros(root)
  runtime <- runtime_env()

  for (name in names(retired$entries)) {
    entry <- retired$entries[[name]]
    ## the same advice from the package and from the Viewer
    advice <- if (isTRUE(entry$upgrades)) {
      "Convert it once"
    } else {
      "re-export it from the source"
    }
    wrong <- if (isTRUE(entry$upgrades)) {
      "re-export it from the source"
    } else {
      "Convert it once"
    }
    attach <- function() {
      runtime$.attachExternalExpression(
        runtime$read_cerebro_file(entry$path),
        entry$path
      )
    }
    for (read in list(function() readCerebro(entry$path), attach)) {
      message <- tryCatch(read(), error = conditionMessage)
      expect_match(message, advice, fixed = TRUE, info = name)
      expect_false(grepl(wrong, message, fixed = TRUE), info = name)
      expect_match(message, "convertCerebro", fixed = TRUE, info = name)
    }
  }
})
