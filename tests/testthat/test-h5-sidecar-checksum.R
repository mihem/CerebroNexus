h5_checksum_export <- function(dir) {
  object <- readRDS(system.file(
    "extdata",
    "examples",
    "pbmc_seurat.rds",
    package = "CerebroNexus",
    mustWork = TRUE
  ))
  crb <- file.path(dir, "pbmc.crb")
  exportFromSeurat(
    object,
    assay = "RNA",
    slot = "data",
    file = crb,
    experiment_name = "pbmc",
    organism = "hg",
    groups = "seurat_clusters",
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    expression_matrix_mode = "h5",
    verbose = FALSE
  )
  list(crb = crb, h5 = list.files(dir, pattern = "\\.h5$", full.names = TRUE))
}

## The same cell and gene names, other values: a valid TENx H5 file.
h5_checksum_rewrite <- function(h5, factor = 2) {
  matrix <- methods::as(
    as.matrix(HDF5Array::TENxMatrix(h5, group = "expression")),
    "CsparseMatrix"
  )
  matrix@x <- matrix@x * factor
  unlink(h5)
  HDF5Array::writeTENxMatrix(matrix, h5, group = "expression", level = 0L)
}

h5_checksum_runtime <- function() {
  source_file <- viewer_test_path("utility_functions.R")
  if (!file.exists(source_file)) {
    source_file <- system.file(
      "viewer/utility_functions.R",
      package = "CerebroNexus"
    )
  }
  exprs <- parse(source_file, keep.source = FALSE)
  wanted <- ".verifyRuntimeH5Checksum"
  env <- new.env(parent = globalenv())
  for (expr in exprs) {
    if (
      is.call(expr) &&
        identical(expr[[1L]], as.name("<-")) &&
        as.character(expr[[2L]]) %in% wanted
    ) {
      eval(expr, env)
    }
  }
  env
}

test_that("an H5 export records the checksum of its sidecar", {
  skip_if_not_installed("HDF5Array")
  out <- h5_checksum_export(withr::local_tempdir())
  crb <- readCerebro(out$crb)
  backend <- crb$getExpressionBackend()
  expect_identical(backend$type, "h5")
  expect_identical(backend$md5, unname(tools::md5sum(out$h5)))
  expect_identical(dim(crb$getExpressionMatrix()), c(230L, 80L))
})

test_that("readCerebro refuses an H5 sidecar with other values", {
  skip_if_not_installed("HDF5Array")
  out <- h5_checksum_export(withr::local_tempdir())
  h5_checksum_rewrite(out$h5)
  expect_error(
    readCerebro(out$crb),
    "The h5 expression sidecar checksum does not match CRB 'pbmc.crb'."
  )
})

test_that("the expression-backend checksum is validated", {
  object <- Cerebro$new()
  expect_error(
    object$setExpressionBackend("h5", "x.h5", md5 = "not-a-checksum"),
    "`md5` must be one MD5 checksum"
  )
  expect_error(
    object$setExpressionBackend("embedded", md5 = strrep("a", 32)),
    "`md5` must be one MD5 checksum"
  )
  object$setExpressionBackend("h5", "x.h5")
  expect_null(object$getExpressionBackend()$md5)
})

test_that("the Viewer refuses an H5 sidecar with other values and hashes once", {
  skip_if_not_installed("HDF5Array")
  runtime <- h5_checksum_runtime()
  out <- h5_checksum_export(withr::local_tempdir())
  md5 <- unname(tools::md5sum(out$h5))
  expect_invisible(runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb))
  expect_length(ls(runtime$.runtime_h5_verified), 1L)
  ## CRBs written before the checksum was recorded are accepted.
  expect_invisible(runtime$.verifyRuntimeH5Checksum(out$h5, NULL, out$crb))
  h5_checksum_rewrite(out$h5)
  expect_error(
    runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb),
    "The h5 expression sidecar checksum does not match CRB 'pbmc.crb'."
  )
  expect_error(
    runtime$.verifyRuntimeH5Checksum(out$h5, "xyz", out$crb),
    "expression-backend descriptor is invalid"
  )
})

test_that("process-wide Viewer helpers call only process-wide Viewer helpers", {
  cache <- new.env()
  sys.source(viewer_test_path("source_cache.R"), envir = cache)
  process <- cache$.viewer_process_function_names
  exprs <- parse(viewer_test_path("utility_functions.R"), keep.source = FALSE)
  defined <- list()
  for (expr in exprs) {
    if (
      is.call(expr) &&
        identical(expr[[1L]], as.name("<-")) &&
        is.symbol(expr[[2L]]) &&
        is.call(expr[[3L]]) &&
        identical(expr[[3L]][[1L]], as.name("function"))
    ) {
      defined[[as.character(expr[[2L]])]] <- eval(expr[[3L]], baseenv())
    }
  }
  for (name in intersect(process, names(defined))) {
    called <- intersect(
      codetools::findGlobals(defined[[name]], merge = FALSE)$functions,
      names(defined)
    )
    expect_true(
      all(called %in% process),
      info = paste(
        name,
        "calls",
        paste(setdiff(called, process), collapse = ", ")
      )
    )
  }
})
