## expression_matrix_mode = "auto" (the default) writes the matrix to an H5 file
## for more than 200,000 cells (`cerebro.h5_cell_threshold`) and keeps it in the
## CRB otherwise; "embedded" and "h5" stay what they say.

test_that("auto is h5 above the cell threshold and embedded up to it", {
  expect_identical(.resolveExpressionMatrixMode("auto", 200001), "h5")
  expect_identical(.resolveExpressionMatrixMode("auto", 200000), "embedded")
  expect_identical(.resolveExpressionMatrixMode("auto", 10), "embedded")
  expect_identical(.resolveExpressionMatrixMode("embedded", 1e7), "embedded")
  expect_identical(.resolveExpressionMatrixMode("h5", 10), "h5")
  withr::local_options(cerebro.h5_cell_threshold = 5)
  expect_identical(.resolveExpressionMatrixMode("auto", 6), "h5")
  withr::local_options(cerebro.h5_cell_threshold = "many")
  expect_error(.resolveExpressionMatrixMode("auto", 6), "one number")
})

test_that("the export functions default to auto", {
  for (f in list(
    exportFromSeurat,
    exportFromSCE,
    convertSeuratToCerebro,
    convertSCEToCerebro,
    convertH5adToCerebro
  )) {
    expect_identical(eval(formals(f)$expression_matrix_mode)[[1L]], "auto")
  }
})

test_that("an export above the threshold writes an H5 file by default", {
  skip_if_not_installed("SingleCellExperiment")
  skip_if_not_installed("HDF5Array")
  path <- system.file("extdata/examples/pbmc_SCE.rds", package = "CerebroNexus")
  if (!nzchar(path)) {
    path <- testthat::test_path("../../inst/extdata/examples/pbmc_SCE.rds")
  }
  sce <- readRDS(path)
  export <- function(threshold) {
    withr::local_options(cerebro.h5_cell_threshold = threshold)
    dir <- withr::local_tempdir(.local_envir = parent.frame())
    file <- file.path(dir, "auto.crb")
    suppressMessages(suppressWarnings(exportFromSCE(
      object = sce,
      file = file,
      experiment_name = "sce",
      organism = "hg",
      groups = c("sample", "cluster"),
      viewer_binary = "never"
    )))
    list(dir = dir, crb = readCerebro(file))
  }
  above <- export(ncol(sce) - 1)
  expect_identical(above$crb$getExpressionBackend()$type, "h5")
  expect_length(list.files(above$dir, pattern = "[.]h5$"), 1L)
  below <- export(ncol(sce))
  expect_identical(below$crb$getExpressionBackend()$type, "embedded")
  expect_length(list.files(below$dir, pattern = "[.]h5$"), 0L)
})

test_that("an h5 choice of auto is named when the file cannot take it", {
  withr::local_options(cerebro.h5_cell_threshold = 5)
  expect_error(
    .exportCheckOutputArgs("out.rds", "h5", FALSE, FALSE, auto = TRUE),
    "auto\\\" chose \\\"h5\\\" for more than 5 cells"
  )
  expect_error(
    .exportCheckOutputArgs("out.rds", "h5", FALSE, FALSE),
    "Received: out.rds"
  )
})
