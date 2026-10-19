## Regression tests for exportFromSCE().

skip_if_not_installed("SingleCellExperiment")

sce_example <- function() {
  path <- system.file("extdata/examples/pbmc_SCE.rds", package = "CerebroNexus")
  if (!nzchar(path)) {
    path <- testthat::test_path("../../inst/extdata/examples/pbmc_SCE.rds")
  }
  readRDS(path)
}

export_sce <- function(object, ...) {
  file <- tempfile(fileext = ".crb")
  on.exit(unlink(file), add = TRUE)
  suppressMessages(exportFromSCE(
    object = object,
    file = file,
    experiment_name = "sce",
    organism = "hg",
    groups = c("sample", "cluster"),
    codec = "rds",
    viewer_binary = "never",
    ...
  ))
  readRDS(file)
}

test_that("exportFromSCE exports a PCA-only object", {
  sce <- sce_example()
  SingleCellExperiment::reducedDims(sce) <- list(
    PCA = SingleCellExperiment::reducedDim(sce, "PCA")
  )

  expect_warning(
    crb <- export_sce(sce),
    "Only PCA as dimensional reduction found"
  )
  expect_identical(crb$availableProjections(), "PCA")
  expect_identical(
    crb$getProjection("PCA"),
    as.data.frame(SingleCellExperiment::reducedDim(sce, "PCA"))
  )
})
