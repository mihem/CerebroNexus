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

test_that("enriched gene sets list their genes without stray spaces", {
  result <- suppressMessages(performGeneSetEnrichmentAnalysis(
    object = gsea_pbmc(),
    GMT_file = gsea_gmt,
    groups = "seurat_clusters",
    thresh_p_val = 1,
    thresh_q_val = 1
  ))
  table <- result@misc$enriched_pathways$cerebro_GSVA$seurat_clusters
  gene_sets <- .read_GMT_file(gsea_gmt)
  expected <- vapply(gene_sets$genesets, paste, character(1), collapse = ",")
  names(expected) <- gene_sets$geneset.names

  expect_s3_class(table, "data.frame")
  expect_identical(
    colnames(table),
    c(
      "seurat_clusters",
      "name",
      "description",
      "length",
      "genes",
      "enrichment_score",
      "p_value",
      "q_value"
    )
  )
  expect_identical(table$genes, unname(expected[table$name]))
  expect_identical(
    table$length,
    lengths(gene_sets$genesets)[match(table$name, gene_sets$geneset.names)]
  )
})
