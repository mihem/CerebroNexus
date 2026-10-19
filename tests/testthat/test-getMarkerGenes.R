## getMarkerGenes() on the bundled PBMC example.

skip_if_not_installed("Seurat")

test_that("getMarkerGenes runs without an organism", {
  pbmc <- readRDS(system.file(
    "extdata/examples/pbmc_seurat.rds",
    package = "CerebroNexus"
  ))
  messages <- character()

  ## `organism` defaults to NULL, which used to fail in `if (logical(0))`.
  ## The Viewer helpers loaded for other tests define their own
  ## getMarkerGenes(), hence the namespace.
  withCallingHandlers(
    result <- suppressWarnings(CerebroNexus::getMarkerGenes(
      object = pbmc,
      groups = "seurat_clusters",
      min_pct = 0.1,
      thresh_p_val = 1,
      verbose = FALSE
    )),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(any(grepl(
    "No information about genes on cell surface",
    messages,
    fixed = TRUE
  )))
  markers <- result@misc$marker_genes$cerebro_seurat$seurat_clusters
  expect_s3_class(markers, "data.frame")
  expect_identical(colnames(markers)[1:2], c("seurat_clusters", "gene"))
  expect_false("on_cell_surface" %in% colnames(markers))
})
