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

h5_checksum_runtime <- function(background = FALSE) {
  source_file <- viewer_test_path("utility_functions.R")
  if (!file.exists(source_file)) {
    source_file <- system.file(
      "viewer/utility_functions.R",
      package = "CerebroNexus"
    )
  }
  exprs <- parse(source_file, keep.source = FALSE)
  wanted <- c(".runtimeH5ChecksumRecord", ".verifyRuntimeH5Checksum")
  env <- new.env(parent = globalenv())
  ## the Viewer's way to start a background process
  if (background) {
    sys.source(viewer_test_path("crb_cache.R"), env)
  }
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

test_that("createShinyApp refuses an H5 sidecar with other values", {
  skip_if_not_installed("HDF5Array")
  dir <- withr::local_tempdir()
  out <- h5_checksum_export(dir)
  h5_checksum_rewrite(out$h5)
  app <- file.path(dir, "app")
  expect_error(
    createShinyApp(
      cerebro_data = c(pbmc = out$crb),
      result_dir = app,
      launch_browser = FALSE,
      verbose = FALSE
    ),
    "The h5 expression sidecar checksum does not match CRB 'pbmc.crb'."
  )
  expect_false(dir.exists(app))
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

## A directory outside the session's temporary directory, as a user's data is.
h5_checksum_user_dir <- function(envir = parent.frame()) {
  withr::local_tempdir(tmpdir = dirname(tempdir()), .local_envir = envir)
}

h5_checksum_kept <- function() {
  list.files(file.path(
    tools::R_user_dir("CerebroNexus", "cache"),
    "h5-verified"
  ))
}

test_that("a verified H5 file is not hashed again by the next process", {
  skip_if_not_installed("HDF5Array")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  ## a file of the session's temporary directory is remembered by the process
  temporary <- h5_checksum_export(withr::local_tempdir())
  md5 <- unname(tools::md5sum(temporary$h5))
  runtime <- h5_checksum_runtime()
  runtime$.verifyRuntimeH5Checksum(temporary$h5, md5, temporary$crb)
  expect_length(h5_checksum_kept(), 0L)

  out <- h5_checksum_export(h5_checksum_user_dir())
  md5 <- unname(tools::md5sum(out$h5))
  expect_output(
    runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb),
    "Verified h5 checksum"
  )
  expect_length(h5_checksum_kept(), 1L)
  ## another process: nothing is hashed, so nothing is reported
  after <- h5_checksum_runtime()
  expect_silent(after$.verifyRuntimeH5Checksum(out$h5, md5, out$crb))
  ## readCerebro() looks at the same file
  record <- .expressionSidecarRecord(out$h5, md5)
  expect_identical(basename(record$kept), h5_checksum_kept())
  ## what is kept is this version of the file with this checksum
  expect_output(
    expect_error(
      after$.verifyRuntimeH5Checksum(out$h5, strrep("0", 32), out$crb),
      "checksum does not match"
    ),
    "Verified h5 checksum"
  )
  h5_checksum_rewrite(out$h5)
  expect_output(
    expect_error(
      h5_checksum_runtime()$.verifyRuntimeH5Checksum(out$h5, md5, out$crb),
      "checksum does not match"
    ),
    "Verified h5 checksum"
  )
  expect_error(readCerebro(out$crb), "checksum does not match")
  expect_length(h5_checksum_kept(), 1L)
})

test_that("readCerebro keeps what it verified for the next process", {
  skip_if_not_installed("HDF5Array")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  out <- h5_checksum_export(h5_checksum_user_dir())
  md5 <- unname(tools::md5sum(out$h5))
  readCerebro(out$crb)
  expect_length(h5_checksum_kept(), 1L)
  expect_silent(
    h5_checksum_runtime()$.verifyRuntimeH5Checksum(out$h5, md5, out$crb)
  )
})

test_that("the overview of a data set is read without hashing its H5 file", {
  skip_if_not_installed("HDF5Array")
  out <- h5_checksum_export(withr::local_tempdir())
  payload <- .readCerebroPayload(out$crb)
  local_mocked_bindings(
    .verifyExpressionSidecar = function(...) stop("the file was hashed")
  )
  ## launchCerebro() reads this for every data set before its first session
  info <- .readBundleDatasetInfo(payload, "pbmc", out$crb)
  expect_identical(info$overview$genes, 230L)
  expect_identical(.readBundleBackend(out$crb, verify = FALSE)$type, "h5")
  ## createShinyApp() still refuses a replaced file when it builds
  expect_error(.readBundleBackend(out$crb), "the file was hashed")
})

test_that("a deferred check is made by a background process and waited for", {
  skip_on_cran()
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("mirai")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  runtime <- h5_checksum_runtime(background = TRUE)
  out <- h5_checksum_export(h5_checksum_user_dir())
  md5 <- unname(tools::md5sum(out$h5))
  task <- suppressMessages(
    runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb, defer = TRUE)
  )
  expect_s3_class(task, "mirai")
  ## asked again while it runs: the same task, not a second process
  expect_identical(
    runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb, defer = TRUE),
    task
  )
  ## the check before the first read has the task's answer
  expect_output(
    expect_true(runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb)),
    "Verified h5 checksum"
  )
  expect_identical(task$data, md5)
  expect_true(
    runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb, defer = TRUE)
  )
  ## the background process kept what it found
  expect_length(h5_checksum_kept(), 1L)

  ## other values: nothing is refused until the answer is asked for
  h5_checksum_rewrite(out$h5)
  task <- suppressMessages(
    runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb, defer = TRUE)
  )
  expect_s3_class(task, "mirai")
  expect_output(
    expect_error(
      runtime$.verifyRuntimeH5Checksum(out$h5, md5, out$crb),
      "The h5 expression sidecar checksum does not match CRB 'pbmc.crb'."
    ),
    "Verified h5 checksum"
  )
  expect_length(h5_checksum_kept(), 1L)
})

test_that("the Viewer opens a data set before its H5 file is hashed", {
  skip_on_cran()
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("mirai")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  runtime <- viewer_runtime_env()
  gene <- rownames(readRDS(system.file(
    "extdata",
    "examples",
    "pbmc_seurat.rds",
    package = "CerebroNexus",
    mustWork = TRUE
  )))[[1L]]
  read <- function(out) {
    object <- suppressMessages(runtime$get_or_load_crb(out$crb))
    pending <- attr(object, "cerebro_h5_cells_pending", exact = TRUE)
    list(
      object = object,
      pending = pending(),
      row = function() {
        capture.output(
          row <- suppressMessages(
            runtime$viewerExpressionRow(object, NULL, gene)
          )
        )
        row
      }
    )
  }
  good <- read(h5_checksum_export(h5_checksum_user_dir()))
  ## optional work waits for the background process instead of the session
  expect_true(good$pending)
  expect_length(good$row(), 80L)
  expect_false(attr(good$object, "cerebro_h5_cells_pending", exact = TRUE)())

  ## a file with other values: the data set opens, its expression is refused
  out <- h5_checksum_export(h5_checksum_user_dir())
  h5_checksum_rewrite(out$h5)
  other <- read(out)
  expect_error(other$row(), "checksum does not match CRB 'pbmc.crb'")
  expect_error(other$row(), "checksum does not match CRB 'pbmc.crb'")
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
