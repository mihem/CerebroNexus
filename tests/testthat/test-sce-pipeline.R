## Tests for the SingleCellExperiment route:
##   SCE (object / .rds / .qs2) -> convertSCEToCerebro() -> exportFromSCE()
##   -> .crb (+ .h5 / .bpcells sidecar)

skip_if_not_installed("SingleCellExperiment")

sce_path <- system.file(
  "extdata/examples/pbmc_SCE.rds",
  package = "CerebroNexus"
)
if (!nzchar(sce_path)) {
  sce_path <- testthat::test_path("../../inst/extdata/examples/pbmc_SCE.rds")
}
sce_raw <- suppressWarnings(readRDS(sce_path))

export_sce <- function(file, ..., object = sce_raw) {
  suppressMessages(suppressWarnings(exportFromSCE(
    object = object,
    file = file,
    experiment_name = "PBMC SCE",
    organism = "hg",
    groups = c("sample", "cluster"),
    viewer_binary = "never",
    ...
  )))
}

test_that("exportFromSCE writes an embedded .crb that round-trips the assay", {
  file <- file.path(withr::local_tempdir(), "sce.crb")
  export_sce(file)

  obj <- readCerebro(file)
  expect_identical(obj$getCellNames(), colnames(sce_raw))
  expect_identical(obj$getGeneNames(), rownames(sce_raw))
  expect_setequal(obj$getGroups(), c("sample", "cluster"))
  ## PCA is dropped when UMAP is available (same rule as the Seurat route).
  expect_identical(obj$availableProjections(), "UMAP")
  genes <- rownames(sce_raw)[1:5]
  expect_equal(
    unname(as.matrix(obj$getExpressionBlock(genes))),
    unname(as.matrix(SummarizedExperiment::assay(sce_raw, "logcounts")[
      genes,
    ]))
  )
})

test_that("exportFromSCE supports the h5 backend", {
  skip_if_not_installed("HDF5Array")
  dir_h5 <- withr::local_tempdir()
  export_sce(file.path(dir_h5, "sce.crb"), expression_matrix_mode = "h5")
  expect_true(file.exists(file.path(dir_h5, "sce.h5")))
  obj <- readCerebro(file.path(dir_h5, "sce.crb"))
  expect_identical(obj$getExpressionBackend()$type, "h5")
  genes <- rownames(sce_raw)[1:5]
  expect_equal(
    unname(as.matrix(obj$getExpressionBlock(genes))),
    unname(as.matrix(SummarizedExperiment::assay(sce_raw, "logcounts")[
      genes,
    ])),
    tolerance = 1e-6
  )
  ## nothing but the CRB and its sidecar is left beside the output
  expect_setequal(list.files(dir_h5), c("sce.crb", "sce.h5"))
})

test_that("exportFromSCE converts non-CsparseMatrix assays for external backends", {
  skip_if_not_installed("HDF5Array")
  sce <- sce_raw
  SummarizedExperiment::assay(sce, "logcounts") <- methods::as(
    methods::as(SummarizedExperiment::assay(sce, "logcounts"), "CsparseMatrix"),
    "TsparseMatrix"
  )
  dir <- withr::local_tempdir()
  export_sce(
    file.path(dir, "t.crb"),
    expression_matrix_mode = "h5",
    object = sce
  )
  expect_true(file.exists(file.path(dir, "t.h5")))
})

test_that("exportFromSCE validates its inputs before writing anything", {
  dir <- withr::local_tempdir()
  f <- file.path(dir, "x.crb")
  expect_error(
    export_sce(f, object = list()),
    "must be of class 'SingleCellExperiment'"
  )
  expect_error(
    suppressMessages(exportFromSCE(
      sce_raw,
      file = f,
      experiment_name = "x",
      organism = "hg",
      groups = c("sample", "nope")
    )),
    "nope"
  )
  expect_error(export_sce(f, projections = "tSNE"), "dimensional reductions")
  expect_error(export_sce(f, assay = "missing"), "could not be found")
  expect_error(
    export_sce(file.path(dir, "x.rds"), expression_matrix_mode = "h5"),
    "to end in .crb"
  )
  ## an SCE without cell names cannot be matched to its meta data: rejected,
  ## not exported
  sce_bad <- sce_raw
  colnames(sce_bad) <- NULL
  expect_error(export_sce(f, object = sce_bad), "unique, non-empty cell names")
  expect_length(list.files(dir), 0L)
})

test_that("convertSCEToCerebro converts from a path and names the output", {
  out <- withr::local_tempdir()
  path <- suppressMessages(suppressWarnings(convertSCEToCerebro(
    sce_file = sce_path,
    result_dir = out,
    groups = c("sample", "cluster"),
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(basename(path), "cerebro_pbmc_SCE.crb")
  expect_true(file.exists(path))
  obj <- readCerebro(path)
  expect_identical(obj$getExperiment()$experiment_name, "pbmc_SCE")
  ## most-expressed genes / mean expression are computed per group
  expect_setequal(
    obj$getGroupsWithMostExpressedGenes(),
    c("sample", "cluster")
  )
  expect_setequal(obj$getGroupsWithMeanExpression(), c("sample", "cluster"))
})

test_that("convertSCEToCerebro renames groups and accepts an in-memory object", {
  out <- withr::local_tempdir()
  path <- suppressMessages(suppressWarnings(convertSCEToCerebro(
    sce_file = sce_raw,
    result_dir = out,
    experiment_name = "In memory",
    groups = c("sample", "cluster"),
    groups_naming = list(cluster = "cell_type"),
    main_group = "cluster",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(basename(path), "cerebro_In_memory.crb")
  obj <- readCerebro(path)
  expect_true("cell_type" %in% obj$getGroups())
  expect_false("cluster" %in% obj$getGroups())
  expect_true("cell_type" %in% obj$getGroupsWithMostExpressedGenes())
  ## the original object is untouched
  expect_true("cluster" %in% names(SummarizedExperiment::colData(sce_raw)))
})

test_that("convertSCEToCerebro computes missing nUMI / nGene from counts", {
  sce <- sce_raw
  SummarizedExperiment::colData(sce)$nUMI <- NULL
  SummarizedExperiment::colData(sce)$nGene <- NULL
  out <- withr::local_tempdir()
  expect_message(
    path <- suppressWarnings(convertSCEToCerebro(
      sce,
      out,
      groups = "cluster",
      viewer_binary = "never",
      verbose = FALSE
    )),
    "Computed missing QC column"
  )
  meta <- readCerebro(path)$getMetaData()
  counts <- SummarizedExperiment::assay(sce, "counts")
  expect_equal(unname(meta$nUMI), unname(as.numeric(Matrix::colSums(counts))))
  expect_equal(
    unname(meta$nGene),
    unname(as.numeric(Matrix::colSums(counts > 0)))
  )
})

test_that("convertSCEToCerebro reads .qs2 files and reports bad input clearly", {
  skip_if_not_installed("qs2")
  dir <- withr::local_tempdir()
  qs_file <- file.path(dir, "pbmc.qs2")
  qs2::qs_save(sce_raw, qs_file)
  path <- suppressMessages(suppressWarnings(convertSCEToCerebro(
    qs_file,
    dir,
    groups = "cluster",
    viewer_binary = "never",
    verbose = FALSE
  )))
  expect_identical(basename(path), "cerebro_pbmc.crb")

  expect_error(
    convertSCEToCerebro(sce_raw, dir, groups = NULL),
    "Character/factor columns in colData: .*cluster"
  )
  expect_error(
    convertSCEToCerebro(file.path(dir, "nope.rds"), dir, groups = "cluster"),
    "sce_file not found"
  )
  bad <- file.path(dir, "bad.rds")
  saveRDS(1:3, bad)
  expect_error(
    convertSCEToCerebro(bad, dir, groups = "cluster"),
    "not a SingleCellExperiment"
  )
  txt <- file.path(dir, "x.loom")
  file.create(txt)
  expect_error(
    convertSCEToCerebro(txt, dir, groups = "cluster"),
    "Unsupported sce_file format"
  )
})

test_that("an SCE .crb carries metadata() payloads through the shared core", {
  sce <- sce_raw
  sce@metadata$marker_genes <- list(
    "Diff. Expression" = list(
      "1" = data.frame(gene = c("A", "B"), avg_log2FC = c(1, 2))
    )
  )
  sce@metadata$parameters <- list(note = "from SCE metadata")
  file <- file.path(withr::local_tempdir(), "meta.crb")
  export_sce(file, object = sce)
  obj <- readCerebro(file)
  expect_identical(obj$getMethodsForMarkerGenes(), "Diff. Expression")
  expect_identical(obj$getParameters()$note, "from SCE metadata")
})

test_that("provided most expressed genes skip non-tables without shifting the rest", {
  table <- data.frame(gene = "A", pct = 1)
  expect_warning(
    result <- .convertAddProvidedMostExpressed(
      list(),
      list(a = table, b = "x", c = table),
      verbose = FALSE
    ),
    "not a data.frame"
  )
  expect_identical(names(result$misc$most_expressed_genes), c("a", "c"))
  expect_warning(
    unnamed <- .convertAddProvidedMostExpressed(
      list(),
      list(table, "x", table),
      verbose = FALSE
    ),
    "not a data.frame"
  )
  expect_identical(
    names(unnamed$misc$most_expressed_genes),
    c("unknown1", "unknown2")
  )
})

test_that("group summaries equal the per-level percentages and means", {
  set.seed(1)
  counts <- Matrix::rsparsematrix(8, 12, 0.4)
  counts@x <- abs(counts@x)
  dimnames(counts) <- list(paste0("g", 1:8), paste0("c", 1:12))
  meta <- data.frame(
    grp = rep(c("b", "a", NA, "c"), 3),
    row.names = colnames(counts),
    stringsAsFactors = FALSE
  )
  misc <- .convertComputeGroupSummaries(list(), meta, counts, "grp", FALSE)

  expected_levels <- c("b", "a", "c")
  pct <- misc$most_expressed_genes$grp
  avg <- misc$mean_expression$grp
  expect_identical(unique(pct$cluster), expected_levels)
  expect_identical(names(pct), c("cluster", "gene", "pct"))
  expect_identical(names(avg), c("cluster", "gene", "mean_expr"))
  for (level in expected_levels) {
    cells <- rownames(meta)[which(meta$grp == level)]
    subset <- counts[, cells, drop = FALSE]
    want_pct <- Matrix::rowSums(subset > 0) / length(cells) * 100
    want_avg <- Matrix::rowMeans(subset)
    got_pct <- pct[pct$cluster == level, ]
    got_avg <- avg[avg$cluster == level, ]
    expect_identical(got_pct$gene, names(want_pct)[order(-want_pct)])
    expect_equal(got_pct$pct, as.numeric(want_pct[order(-want_pct)]))
    expect_identical(got_avg$gene, names(want_avg)[order(-want_avg)])
    expect_equal(got_avg$mean_expr, as.numeric(want_avg[order(-want_avg)]))
  }
  empty <- .convertComputeGroupSummaries(
    list(),
    data.frame(grp = rep(NA_character_, 12), row.names = colnames(counts)),
    counts,
    "grp",
    FALSE
  )
  expect_identical(empty$most_expressed_genes$grp, data.frame())
})
