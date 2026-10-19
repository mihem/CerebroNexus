skip_if_not_installed("Seurat")

test_that("bundled PBMC Seurat input converts into a standalone Shiny app", {
  seurat_file <- system.file(
    "extdata/examples/pbmc_seurat.rds",
    package = "CerebroNexus"
  )
  expect_true(nzchar(seurat_file))

  root <- withr::local_tempdir()
  conversion_dir <- file.path(root, "conversion")
  app_dir <- file.path(root, "app")

  convertSeuratToCerebro(
    seurat_file = seurat_file,
    result_dir = conversion_dir,
    assay = "RNA",
    slot = "data",
    experiment_name = "PBMC example",
    organism = "Human",
    groups = c("sample", "seurat_clusters"),
    groups_naming = list("seurat_clusters" = "cluster"),
    verbose = FALSE
  )

  crb_files <- list.files(
    conversion_dir,
    pattern = "[.]crb$",
    full.names = TRUE
  )
  expect_length(crb_files, 1L)

  createShinyApp(
    cerebro_data = c("PBMC example" = crb_files),
    result_dir = app_dir,
    launch_browser = FALSE,
    verbose = FALSE
  )

  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(file.exists(file.path(app_dir, "cerebro_config.rds")))
  expect_true(dir.exists(file.path(app_dir, "private-data")))
})

test_that("convertSeuratToCerebro skips non-data.frame most_expressed_genes", {
  seurat <- readRDS(system.file(
    "extdata/examples/pbmc_seurat.rds",
    package = "CerebroNexus"
  ))
  result_dir <- withr::local_tempdir()
  table <- data.frame(gene = "MS4A1", pct = 1)

  ## A skipped element in the middle used to shift the later indices and fail
  ## with "subscript out of bounds".
  expect_warning(
    suppressMessages(convertSeuratToCerebro(
      seurat,
      result_dir = result_dir,
      experiment_name = "meg",
      groups = c("sample", "seurat_clusters"),
      most_expressed_genes = list(sample = table, "not a table", table),
      codec = "rds",
      verbose = TRUE
    )),
    "Element 2 in most_expressed_genes is not a data.frame"
  )
  crb <- readRDS(file.path(result_dir, "cerebro_meg.crb"))
  expect_identical(crb$most_expressed_genes, list(sample = table))
})
