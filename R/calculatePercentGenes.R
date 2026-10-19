#' @title
#' Calculate percentage of transcripts of gene list.
#'
#' @description
#' Get percentage of transcripts of gene list compared to all transcripts per
#' cell.
#'
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
#' @param genes List(s) of genes.
#'
#' @return
#' List of lists containing the percentages of expression for each provided
#' gene list.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
#'   package = "CerebroNexus"))
#' pbmc <- calculatePercentGenes(
#'   object = pbmc,
#'   assay = 'RNA',
#'   genes = list('example' = c('FCN1','CD3D'))
#' )
#'
#' @importFrom Matrix colSums
#'
#' @export
#'
calculatePercentGenes <- function(
  object,
  assay = 'RNA',
  genes
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  .checkSeuratAvailable()
  .checkSeuratObject(object)
  .checkSeuratAssay(object, assay)

  ## This consumer requires raw counts and therefore never crosses semantic
  ## classes.
  counts_matrix <- .getValidatedExpression(object, assay, "counts")$data

  ## for every supplied gene list, get the genes that are present in the data
  ## set and calculate the percentage of transcripts that they account for
  pbapply::pblapply(genes, function(x) {
    genes_here <- intersect(x, rownames(counts_matrix))
    Matrix::colSums(counts_matrix[genes_here, , drop = FALSE]) /
      Matrix::colSums(counts_matrix)
  })
}
