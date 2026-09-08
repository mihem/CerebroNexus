## Tests for Seurat v5 split (layered) objects.
##
## `split(assay, f = ...)` produces one layer per group, named `<root>.<suffix>`
## where the suffix is whatever the splitting factor's levels are -- sample names
## far more often than integers. Layer resolution used to recognise only the
## numeric form, so an object split by sample name was never joined and the
## export silently kept the first sample's layer alone.
##
## All tests require Seurat; skipped gracefully when it is not installed.

skip_if_not_installed("Seurat")
skip_if_not_installed("SeuratObject")

## `split()` on an assay, and layers as a concept, arrived with Seurat 5. The
## package still declares Seurat (>= 3.0.0), so an older environment has to skip
## rather than fail on a fixture it cannot build.
skip_if_not(
  utils::compareVersion(
    as.character(utils::packageVersion("Seurat")),
    "5.0.0"
  ) >=
    0,
  "split layers require Seurat >= 5"
)

## ---------------------------------------------------------------------------
## Fixtures
## ---------------------------------------------------------------------------

## A minimal normalised object with a UMAP, split by the factor given in `by`.
## `by` is a plain vector: pass characters for `counts.s1`-style layers and
## integers for the legacy `counts.1` form.
make_split_object <- function(by, n_genes = 60, n_cells = 20) {
  set.seed(42)
  counts <- matrix(
    rpois(n_genes * n_cells, 3),
    nrow = n_genes,
    ncol = n_cells,
    dimnames = list(
      paste0("g", seq_len(n_genes)),
      paste0("c", seq_len(n_cells))
    )
  )
  obj <- Seurat::CreateSeuratObject(
    counts = methods::as(counts, "CsparseMatrix")
  )
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj$sample <- rep(by, length.out = n_cells)
  obj$cluster <- factor(rep(c("c1", "c2"), length.out = n_cells))
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = matrix(
      rnorm(n_cells * 2),
      ncol = 2,
      dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2"))
    ),
    key = "UMAP_",
    assay = "RNA"
  )
  obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)
  obj
}

add_custom_partition <- function(
  object,
  root,
  sample_names = c("s1", "s2"),
  offsets = c(100, 200)
) {
  joined <- SeuratObject::JoinLayers(object[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")
  pieces <- vector("list", length(sample_names))
  memberships <- vector("list", length(sample_names))
  layer_names <- paste0(root, ".", sample_names)

  for (i in seq_along(sample_names)) {
    sample_cells <- colnames(object)[
      as.character(object$sample) == sample_names[[i]]
    ]
    piece <- joined_data[, sample_cells, drop = FALSE]
    piece@x <- piece@x + offsets[[i]]
    SeuratObject::LayerData(
      object[["RNA"]],
      layer = layer_names[[i]]
    ) <- piece
    pieces[[i]] <- piece
    memberships[[i]] <- sample_cells
  }

  names(memberships) <- layer_names
  expected <- do.call(cbind, pieces)
  expected <- expected[, colnames(object), drop = FALSE]

  list(
    object = object,
    expected = expected,
    memberships = memberships
  )
}

make_incomplete_requested_root <- function() {
  object <- make_split_object(c("s1", "s2"))
  partial_data <- SeuratObject::LayerData(
    object[["RNA"]],
    layer = "data.s1"
  )
  for (layer in grep(
    "^data",
    SeuratObject::Layers(object[["RNA"]]),
    value = TRUE
  )) {
    suppressWarnings(
      SeuratObject::LayerData(object[["RNA"]], layer = layer) <- NULL
    )
  }
  SeuratObject::LayerData(
    object[["RNA"]],
    layer = "data.imputed"
  ) <- partial_data
  object
}

export_args <- function(object, file, ...) {
  c(
    list(
      object = object,
      assay = "RNA",
      experiment_name = "split layers",
      organism = "mm",
      groups = c("sample", "cluster"),
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      file = file,
      verbose = FALSE
    ),
    list(...)
  )
}

## ---------------------------------------------------------------------------
## Layer resolution on split objects
## ---------------------------------------------------------------------------

test_that("a sample-name-split object resolves to the joined `data` layer", {
  obj <- make_split_object(c("s1", "s2"))
  expect_setequal(
    SeuratObject::Layers(obj[["RNA"]]),
    c("counts.s1", "counts.s2", "data.s1", "data.s2")
  )

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )

  ## every cell, not just the first sample's half
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))

  ## normalised values, not counts standing in for them
  expect_true(any(matrix_out@x %% 1 != 0))
})

test_that("the legacy numeric split form keeps resolving as before", {
  obj <- make_split_object(c(1L, 2L))
  expect_setequal(
    SeuratObject::Layers(obj[["RNA"]]),
    c("counts.1", "counts.2", "data.1", "data.2")
  )

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_true(any(matrix_out@x %% 1 != 0))
})

test_that("a full-cell dotted custom layer is not treated as split", {
  obj <- make_split_object(c("s1", "s2"))
  obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")

  expect_no_message(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "counts",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE,
      verbose = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))
})

test_that("a full-cell custom layer is excluded from a real split join", {
  obj <- make_split_object(c("s1", "s2"))
  obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")
  imputed_data <- joined_data
  imputed_data@x <- imputed_data@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- imputed_data
  obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)

  expect_no_error(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))
  expect_equal(matrix_out, joined_data)
})

test_that("Seurat-prefix custom layers are excluded from a real split join", {
  for (custom_name in c("data_imputed", "dataBackup")) {
    obj <- make_split_object(c("s1", "s2"))
    obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
    joined_data <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")
    custom_data <- joined_data
    custom_data@x <- custom_data@x + 100
    SeuratObject::LayerData(
      obj[["RNA"]],
      layer = custom_name
    ) <- custom_data
    obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)

    expect_no_error(
      matrix_out <- .getExpressionMatrix(
        seurat = obj,
        assay = "RNA",
        slot = "data",
        join_samples = TRUE,
        allow_cross_semantic_fallback = TRUE
      )
    )
    expect_equal(matrix_out, joined_data, info = custom_name)
  }
})

test_that("an unrelated partial layer does not hide a real split partition", {
  obj <- make_split_object(c("s1", "s2"))
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")
  custom_cells <- c(
    colnames(obj)[obj$sample == "s1"][1:3],
    colnames(obj)[obj$sample == "s2"][1:3]
  )
  custom_data <- joined_data[, custom_cells, drop = FALSE]
  custom_data@x <- custom_data@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- custom_data

  expect_no_error(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))
  expect_equal(matrix_out, joined_data)
})

test_that("a requested custom root resolves its complete split partition", {
  obj <- make_split_object(c("s1", "s2"))
  fixture <- add_custom_partition(obj, "ambient")
  obj <- fixture$object

  for (layer in names(fixture$memberships)) {
    expect_identical(
      SeuratObject::Cells(obj[["RNA"]], layer = layer),
      fixture$memberships[[layer]]
    )
  }

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "ambient",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )
  expect_equal(matrix_out, fixture$expected)
  expect_setequal(colnames(matrix_out), colnames(obj))
})

test_that("a requested custom root may itself contain a dot", {
  obj <- make_split_object(c("s1", "s2"))
  fixture <- add_custom_partition(
    obj,
    "ambient.corrected",
    offsets = c(300, 400)
  )
  obj <- fixture$object

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "ambient.corrected",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )
  expect_equal(matrix_out, fixture$expected)
})

test_that("a dotted requested root is matched literally, not as a regex", {
  obj <- make_split_object(c("s1", "s2"))
  requested <- add_custom_partition(
    obj,
    "ambient.corrected",
    offsets = c(300, 400)
  )
  obj <- requested$object
  collision <- add_custom_partition(
    obj,
    "ambientXcorrected",
    offsets = c(500, 600)
  )
  obj <- collision$object

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "ambient.corrected",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )
  expect_equal(matrix_out, requested$expected)
  expect_false(isTRUE(all.equal(matrix_out, collision$expected)))
})

test_that("an exact custom root takes precedence over its split siblings", {
  obj <- make_split_object(c("s1", "s2"))
  fixture <- add_custom_partition(obj, "ambient")
  obj <- fixture$object
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  exact <- SeuratObject::LayerData(joined, layer = "data")
  exact@x <- exact@x + 700
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "ambient"
  ) <- exact

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "ambient",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )
  expect_equal(matrix_out, exact)
  expect_false(isTRUE(all.equal(matrix_out, fixture$expected)))
})

test_that("an ambiguous real Assay5 partition names both covers and is refused", {
  obj <- make_split_object(c("s1", "s2"))
  fixture <- add_custom_partition(obj, "ambient")
  obj <- fixture$object
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")
  batch_memberships <- list(
    "ambient.batch_a" = colnames(obj)[seq(1, ncol(obj), by = 2)],
    "ambient.batch_b" = colnames(obj)[seq(2, ncol(obj), by = 2)]
  )

  for (i in seq_along(batch_memberships)) {
    piece <- joined_data[, batch_memberships[[i]], drop = FALSE]
    piece@x <- piece@x + 500 + i
    SeuratObject::LayerData(
      obj[["RNA"]],
      layer = names(batch_memberships)[[i]]
    ) <- piece
  }

  err <- tryCatch(
    {
      .getExpressionMatrix(
        seurat = obj,
        assay = "RNA",
        slot = "ambient",
        join_samples = TRUE,
        allow_cross_semantic_fallback = TRUE
      )
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )

  expect_false(is.na(err))
  expect_true(grepl("more than one valid cell partition", err, fixed = TRUE))
  expect_true(grepl("ambient.s1", err, fixed = TRUE))
  expect_true(grepl("ambient.s2", err, fixed = TRUE))
  expect_true(grepl("ambient.batch_a", err, fixed = TRUE))
  expect_true(grepl("ambient.batch_b", err, fixed = TRUE))
})

test_that("an incomplete real Assay5 partition is refused", {
  obj <- make_split_object(c("s1", "s2"))
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")
  sample_a <- colnames(obj)[as.character(obj$sample) == "s1"]
  sample_b <- colnames(obj)[as.character(obj$sample) == "s2"]
  memberships <- list(
    "ambient.s1" = sample_a[-1],
    "ambient.s2" = sample_b
  )

  for (i in seq_along(memberships)) {
    piece <- joined_data[, memberships[[i]], drop = FALSE]
    SeuratObject::LayerData(
      obj[["RNA"]],
      layer = names(memberships)[[i]]
    ) <- piece
  }

  err <- tryCatch(
    {
      .getExpressionMatrix(
        seurat = obj,
        assay = "RNA",
        slot = "ambient",
        join_samples = TRUE,
        allow_cross_semantic_fallback = FALSE
      )
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  expect_false(is.na(err))
  expect_true(grepl("No unique disjoint partition", err, fixed = TRUE))
  expect_true(grepl("ambient.s1", err, fixed = TRUE))
  expect_true(grepl("ambient.s2", err, fixed = TRUE))
})

test_that("a full custom prefix is not substituted for an absent exact root", {
  obj <- make_split_object(c("s1", "s2"))
  obj[["RNA"]] <- SeuratObject::JoinLayers(obj[["RNA"]])
  imputed <- SeuratObject::LayerData(obj[["RNA"]], layer = "data")
  imputed@x <- imputed@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "data.imputed"
  ) <- imputed
  suppressWarnings(
    SeuratObject::LayerData(obj[["RNA"]], layer = "data") <- NULL
  )

  err <- tryCatch(
    {
      .getExpressionMatrix(
        seurat = obj,
        assay = "RNA",
        slot = "data",
        join_samples = TRUE,
        allow_cross_semantic_fallback = FALSE
      )
      NA_character_
    },
    error = function(e) conditionMessage(e)
  )
  expect_false(is.na(err))
  expect_true(grepl("Exact layer `data` is absent", err, fixed = TRUE))
  expect_true(grepl("data.imputed", err, fixed = TRUE))
})

test_that("a missing split root is not partially read when joins are disabled", {
  obj <- make_split_object(c("s1", "s2"))

  expect_error(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = FALSE,
      allow_cross_semantic_fallback = FALSE
    ),
    "join_samples = FALSE",
    fixed = TRUE
  )
})

test_that("cross-semantic fallback resolves a split replacement root", {
  obj <- make_split_object(c("s1", "s2"))
  for (layer in grep(
    "^data\\.",
    SeuratObject::Layers(obj[["RNA"]]),
    value = TRUE
  )) {
    suppressWarnings(
      SeuratObject::LayerData(obj[["RNA"]], layer = layer) <- NULL
    )
  }

  expect_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    ),
    "falling back to `counts`",
    fixed = TRUE
  )
  expect_equal(ncol(matrix_out), ncol(obj))
  expect_setequal(colnames(matrix_out), colnames(obj))
})

test_that("incomplete requested-prefix noise does not block compatibility", {
  obj <- make_incomplete_requested_root()

  expect_warning(
    resolution <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE,
      return_resolution = TRUE
    ),
    "falling back to `counts`",
    fixed = TRUE
  )

  expect_identical(resolution$requested, "data")
  expect_identical(resolution$resolved, "counts")
  expect_equal(ncol(resolution$data), ncol(obj))
  expect_setequal(colnames(resolution$data), colnames(obj))
})

test_that("strict incomplete-root errors carry actionable diagnostics", {
  obj <- make_incomplete_requested_root()

  err <- tryCatch(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    ),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl(
    "Layer cell counts: data.imputed=10",
    err,
    fixed = TRUE
  ))
  expect_true(grepl("Missing assay cells (10):", err, fixed = TRUE))
  expect_true(grepl(
    "SeuratObject::Cells",
    err,
    fixed = TRUE
  ))
})

test_that("resolving a partition leaves every caller layer unchanged", {
  obj <- make_split_object(c("s1", "s2"))
  joined <- SeuratObject::JoinLayers(obj[["RNA"]])
  joined_data <- SeuratObject::LayerData(joined, layer = "data")
  custom_data <- joined_data
  custom_data@x <- custom_data@x + 100
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "dataBackup"
  ) <- custom_data

  before_names <- SeuratObject::Layers(obj[["RNA"]])
  before_memberships <- setNames(
    lapply(
      before_names,
      function(layer) {
        SeuratObject::Cells(obj[["RNA"]], layer = layer)
      }
    ),
    before_names
  )
  before_data <- setNames(
    lapply(
      before_names,
      function(layer) {
        SeuratObject::LayerData(obj[["RNA"]], layer = layer)
      }
    ),
    before_names
  )

  expect_no_error(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data",
      join_samples = TRUE,
      allow_cross_semantic_fallback = FALSE
    )
  )

  expect_identical(SeuratObject::Layers(obj[["RNA"]]), before_names)
  for (layer in before_names) {
    expect_identical(
      SeuratObject::Cells(obj[["RNA"]], layer = layer),
      before_memberships[[layer]],
      info = layer
    )
    expect_equal(
      SeuratObject::LayerData(obj[["RNA"]], layer = layer),
      before_data[[layer]],
      info = layer
    )
  }
})

## ---------------------------------------------------------------------------
## Export entry point
## ---------------------------------------------------------------------------

test_that("a sample-split object exports every cell in embedded mode", {
  obj <- make_split_object(c("s1", "s2"))
  out_dir <- withr::local_tempdir()

  crb <- file.path(out_dir, "embedded.crb")
  expect_no_error(do.call(
    exportFromSeurat,
    export_args(obj, crb, expression_matrix_mode = "embedded")
  ))

  cerebro <- readRDS(crb)
  expect_equal(ncol(cerebro$expression), ncol(obj))
  expect_equal(nrow(cerebro$getMetaData()), ncol(obj))
  ## column order must match the meta data the export builds from Cells()
  expect_identical(colnames(cerebro$expression), SeuratObject::Cells(obj))
})

test_that("conversion can hand one validated layer resolution to export", {
  obj <- make_split_object(c("s1", "s2"))
  resolution <- .getExpressionMatrix(
    seurat = obj,
    assay = "RNA",
    slot = "data",
    join_samples = TRUE,
    allow_cross_semantic_fallback = TRUE,
    return_resolution = TRUE
  )
  expect_identical(resolution$assay, "RNA")

  ## Remove the physical data layers after resolving them. If export ignores
  ## the handoff and resolves again, compatibility fallback selects raw counts;
  ## the call still succeeds, so the stored scientific values are the signal.
  for (layer in grep(
    "^data\\.",
    SeuratObject::Layers(obj[["RNA"]]),
    value = TRUE
  )) {
    suppressWarnings(
      SeuratObject::LayerData(obj[["RNA"]], layer = layer) <- NULL
    )
  }

  crb <- file.path(withr::local_tempdir(), "handoff.crb")
  expect_no_error(do.call(
    exportFromSeurat,
    export_args(
      obj,
      crb,
      expression_matrix_mode = "embedded",
      .expression_resolution = resolution
    )
  ))

  expect_equal(readRDS(crb)$expression, resolution$data)
})

test_that("a sample-split object exports every cell in h5 mode", {
  ## The external backends are where a truncated matrix used to slip through
  ## unnoticed: they leave `self$expression` NULL, so nothing compared the
  ## matrix to the meta data and the .crb described more cells than it had.
  skip_if_not_installed("HDF5Array")

  obj <- make_split_object(c("s1", "s2"))
  out_dir <- withr::local_tempdir()

  crb <- file.path(out_dir, "external.crb")
  expect_no_error(do.call(
    exportFromSeurat,
    export_args(obj, crb, expression_matrix_mode = "h5")
  ))

  ## the sibling is stored cells x genes
  on_disk <- HDF5Array::TENxMatrix(
    file.path(out_dir, "external.h5"),
    group = "expression"
  )
  expect_equal(nrow(on_disk), ncol(obj))
  expect_equal(nrow(readRDS(crb)$getMetaData()), ncol(obj))
})

test_that("a sample-split object exports every cell in bpcells mode", {
  skip_if_not_installed("BPCells")

  obj <- make_split_object(c("s1", "s2"))
  out_dir <- withr::local_tempdir()

  crb <- file.path(out_dir, "external.crb")
  expect_no_error(do.call(
    exportFromSeurat,
    export_args(obj, crb, expression_matrix_mode = "bpcells")
  ))

  matrix_dir <- file.path(out_dir, "external.bpcells")
  expect_true(dir.exists(matrix_dir))
  on_disk <- BPCells::open_matrix_dir(dir = matrix_dir)
  expect_equal(ncol(on_disk), ncol(obj))
  expect_equal(nrow(readRDS(crb)$getMetaData()), ncol(obj))
})

test_that("a layer asked for by name is not joined away underneath the caller", {
  ## Joining consumes the split layers, so a request for one of them by name
  ## and a join cannot both happen. The substitution would be invisible: the
  ## joined `data` matches the request's own semantic root, so not even the
  ## cross-semantic warning fires, and a caller asking for one sample would
  ## silently receive every sample.
  obj <- make_split_object(c("s1", "s2"))
  cells_in_s2 <- colnames(obj)[obj$sample == "s2"]

  expect_no_warning(
    matrix_out <- .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "data.s2",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    )
  )

  expect_equal(ncol(matrix_out), length(cells_in_s2))
  expect_setequal(colnames(matrix_out), cells_in_s2)
})

test_that("a disk-backed assay is refused with the reason, not just a class", {
  ## Reading a BPCells- or DelayedArray-backed assay is not supported. That is
  ## unchanged here, but the refusal used to read `Received: RenameDims` and
  ## nothing else, which says neither what happened nor what to do -- and
  ## invites confusion with `expression_matrix_mode = "bpcells"`, which is
  ## about how the .crb stores its matrix, not how the source object holds it.
  skip_if_not_installed("BPCells")

  matrix_dir <- file.path(withr::local_tempdir(), "counts")
  dense <- matrix(
    rpois(30 * 12, 3),
    nrow = 30,
    dimnames = list(paste0("g", 1:30), paste0("c", 1:12))
  )
  BPCells::write_matrix_dir(
    methods::as(methods::as(dense, "CsparseMatrix"), "IterableMatrix"),
    dir = matrix_dir
  )
  obj <- Seurat::CreateSeuratObject(
    counts = BPCells::open_matrix_dir(dir = matrix_dir)
  )
  obj$sample <- rep(c("s1", "s2"), length.out = 12)
  obj[["RNA"]] <- split(obj[["RNA"]], f = obj$sample)

  err <- tryCatch(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "counts",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("lives on disk", err, fixed = TRUE))
  expect_true(grepl("dgCMatrix", err, fixed = TRUE))
  expect_true(grepl("expression_matrix_mode", err, fixed = TRUE))

  ## The advice has to be safe on the object it is given. Converting only the
  ## requested layer would settle for one `counts.*` layer and leave the object
  ## holding a single sample -- advice that reproduces the bug this file is
  ## about is worse than no advice.
  advice_lines <- strsplit(err, "\n", fixed = TRUE)[[1]]
  code_start <- grep("^  (for \\(|layer <- )", advice_lines)[1]
  prose_start <- grep(
    "^(Convert every layer|Note that)",
    advice_lines
  )
  code_end <- min(prose_start[prose_start > code_start]) - 1L
  advice_code <- paste(
    sub("^  ", "", advice_lines[code_start:code_end]),
    collapse = "\n"
  )

  advice_env <- new.env(parent = globalenv())
  advice_env$seurat <- obj
  expect_no_warning(
    eval(parse(text = advice_code), envir = advice_env)
  )

  materialised <- .getExpressionMatrix(
    seurat = advice_env$seurat,
    assay = "RNA",
    slot = "counts",
    join_samples = TRUE,
    allow_cross_semantic_fallback = TRUE
  )
  expect_equal(ncol(materialised), ncol(dense))
  expect_setequal(colnames(materialised), colnames(dense))
})

test_that("every selected split layer is checked for disk-backed storage", {
  skip_if_not_installed("BPCells")

  obj <- make_split_object(c("s1", "s2"))
  disk_layer <- SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "counts.s2"
  )
  matrix_dir <- file.path(withr::local_tempdir(), "counts-s2")
  BPCells::write_matrix_dir(
    methods::as(disk_layer, "IterableMatrix"),
    dir = matrix_dir
  )
  SeuratObject::LayerData(
    obj[["RNA"]],
    layer = "counts.s2"
  ) <- BPCells::open_matrix_dir(dir = matrix_dir)

  err <- tryCatch(
    .getExpressionMatrix(
      seurat = obj,
      assay = "RNA",
      slot = "counts",
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE
    ),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl("lives on disk", err, fixed = TRUE))
  expect_true(grepl("counts.s2", err, fixed = TRUE))
})

test_that("exporting one named split layer fails on the cell count, clearly", {
  ## Honouring the request above means the export then holds a matrix covering
  ## one sample, which is exactly what the cell-count check exists to refuse --
  ## and it names the cause instead of blaming the meta data.
  obj <- make_split_object(c("s1", "s2"))

  err <- tryCatch(
    do.call(
      exportFromSeurat,
      export_args(
        obj,
        file.path(withr::local_tempdir(), "partial.crb"),
        slot = "data.s2"
      )
    ),
    error = function(e) conditionMessage(e)
  )
  expect_true(grepl("JoinLayers", err, fixed = TRUE))
  expect_true(grepl(as.character(ncol(obj)), err, fixed = TRUE))
})

test_that("a partial expression matrix is rejected identically in every mode", {
  ## An assay can hold a layer that covers only part of the object -- here by
  ## dropping one sample's counts and every complete compatibility replacement.
  ## The export has to refuse it rather than pair it with a full meta data table.
  obj <- make_split_object(c("s1", "s2"))
  suppressWarnings(
    SeuratObject::LayerData(obj[["RNA"]], layer = "counts.s2") <- NULL
  )
  for (layer in grep(
    "^data",
    SeuratObject::Layers(obj[["RNA"]]),
    value = TRUE
  )) {
    suppressWarnings(
      SeuratObject::LayerData(obj[["RNA"]], layer = layer) <- NULL
    )
  }

  out_dir <- file.path(tempdir(), "split_layers_partial")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  modes <- c("embedded", "h5")
  if (!requireNamespace("HDF5Array", quietly = TRUE)) {
    modes <- "embedded"
  }

  messages <- vapply(
    modes,
    function(mode) {
      args <- export_args(
        obj,
        file.path(out_dir, paste0(mode, ".crb")),
        slot = "counts",
        expression_matrix_mode = mode
      )
      tryCatch(
        {
          suppressWarnings(do.call(exportFromSeurat, args))
          NA_character_
        },
        error = function(e) conditionMessage(e)
      )
    },
    character(1)
  )

  expect_false(any(is.na(messages)))
  expect_true(all(grepl("JoinLayers", messages, fixed = TRUE)))
  expect_true(all(grepl(as.character(ncol(obj)), messages, fixed = TRUE)))
  ## the same defect must not report itself differently depending on where the
  ## matrix was headed
  expect_equal(length(unique(messages)), 1L)
})

test_that("convertSeuratToCerebro propagates export failures", {
  obj <- make_split_object(c("s1", "s2"))
  out_dir <- withr::local_tempdir()

  expect_error(
    convertSeuratToCerebro(
      seurat_file = obj,
      result_dir = out_dir,
      assay = "RNA",
      slot = "data.s1",
      experiment_name = "partial split",
      organism = "mm",
      groups = c("sample", "cluster"),
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      add_most_expressed_genes = FALSE,
      verbose = FALSE
    ),
    "Error processing <Seurat:partial split>",
    fixed = TRUE
  )
  expect_length(list.files(out_dir, pattern = "\\.crb$"), 0L)
})
