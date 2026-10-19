## performGeneSetEnrichmentAnalysis() on the bundled PBMC example.

skip_if_not_installed("Seurat")
skip_if_not_installed("GSVA")
skip_if_not_installed("qvalue")

gsea_pbmc <- function() {
  readRDS(system.file(
    "extdata/examples/pbmc_seurat.rds",
    package = "CerebroNexus"
  ))
}
gsea_gmt <- system.file("extdata/example_gene_set.gmt", package = "CerebroNexus")

test_that("a single-level group is skipped with a clean message", {
  messages <- character()
  withCallingHandlers(
    result <- performGeneSetEnrichmentAnalysis(
      object = gsea_pbmc(),
      GMT_file = gsea_gmt,
      groups = "orig.ident"
    ),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  skipped <- grep("Only one group level found", messages, value = TRUE)
  expect_length(skipped, 1L)
  expect_match(
    skipped,
    "for group `orig.ident`. Will skip this group and proceed to next.\n$"
  )
  expect_null(result@misc$enriched_pathways$cerebro_GSVA$orig.ident)
})
