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

  .validateSeuratInputs(object, assay)

  ## Resolve split Seurat v5 assays through the shared layer resolver. This
  ## consumer requires raw counts and therefore never crosses semantic classes.
  counts_resolution <- .getExpressionMatrix(
    seurat = object,
    assay = assay,
    slot = "counts",
    join_samples = TRUE,
    allow_cross_semantic_fallback = FALSE,
    return_resolution = TRUE
  )
  counts_matrix <- .validate_expression_cells(
    expression_data = counts_resolution$data,
    object_cells = colnames(object),
    assay = assay,
    requested_layer = counts_resolution$requested,
    resolved_layer = counts_resolution$resolved
  )

  ## check if `counts` matrix exist in provided assay
  if (is.null(counts_matrix) || nrow(counts_matrix) == 0) {
    stop(
      paste0(
        '`counts` matrix could not be found in `',
        assay,
        '` assay slot of the provided Seurat object.'
      ),
      call. = FALSE
    )
  }

  ##--------------------------------------------------------------------------##
  ## get for every supplied gene list, get the genes that are present in the
  ## data set and calculate the percentage of transcripts that they account for
  ##--------------------------------------------------------------------------##

  pct_fun <- function(x) {
    genes_here <- intersect(x, rownames(counts_matrix))
    if (length(genes_here) == 1) {
      counts_matrix[genes_here, ] /
        Matrix::colSums(counts_matrix)
    } else {
      Matrix::colSums(counts_matrix[genes_here, ]) /
        Matrix::colSums(counts_matrix)
    }
  }
  result <- if (requireNamespace("pbapply", quietly = TRUE)) {
    pbapply::pblapply(genes, pct_fun)
  } else {
    lapply(genes, pct_fun)
  }

  ##--------------------------------------------------------------------------##
  ## return list with results
  ##--------------------------------------------------------------------------##
  return(result)
}
