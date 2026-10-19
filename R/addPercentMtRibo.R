#' @title
#' Add percentage of mitochondrial and ribosomal transcripts.
#'
#' @description
#' Get percentage of transcripts of gene list compared to all transcripts per
#' cell.
#'
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
#' @param organism Organism, can be either human ('hg') or mouse ('mm'). Genes
#' need to annotated as gene symbol, e.g. MKI67 (human) / Mki67 (mouse).
#' @param gene_nomenclature Define if genes are saved by their name ('name'),
#' ENSEMBL ID ('ensembl') or GENCODE ID ('gencode_v27', 'gencode_vM16').
#'
#' @return
#' Seurat object with two new meta data columns containing the percentage of
#' mitochondrial and ribosomal gene expression for each cell.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
#'   package = "CerebroNexus"))
#' pbmc <- addPercentMtRibo(
#'   object = pbmc,
#'   assay = 'RNA',
#'   organism = 'hg',
#'   gene_nomenclature = 'name'
#' )
#'
#' @import dplyr
#' @importFrom utils read.delim
#'
#' @export
#'
addPercentMtRibo <- function(
  object,
  assay = 'RNA',
  organism,
  gene_nomenclature
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  .checkSeuratAvailable()
  .checkSeuratObject(object)

  ## check if organism is supported
  supported_organisms <- c('hg', 'mm')
  if (!(organism %in% supported_organisms)) {
    stop(
      paste0(
        "User-specified organism ('",
        organism,
        "') not in list of supported organisms: ",
        paste(supported_organisms, collapse = ', ')
      )
    )
  }

  ## check if nomenclature is supported
  supported_nomenclatures <- c('name', 'ensembl', 'gencode_v27', 'gencode_vM16')
  if (!(gene_nomenclature %in% supported_nomenclatures)) {
    stop(
      paste0(
        "User-specified gene nomenclature ('",
        gene_nomenclature,
        "') not in list of supported nomenclatures: ",
        paste(supported_nomenclatures, collapse = ', ')
      )
    )
  }

  ## check if provided assay exists
  if ((assay %in% names(object@assays) == FALSE)) {
    stop(
      paste0(
        'Assay slot `',
        assay,
        '` could not be found in provided Seurat ',
        'object.'
      ),
      call. = FALSE
    )
  }
  ## This consumer requires raw counts and therefore never crosses semantic
  ## classes.
  counts_matrix <- .getValidatedExpression(object, assay, "counts")$data

  if (is.null(object@misc$gene_lists)) {
    object@misc$gene_lists <- list()
  }

  ## percentage of the transcripts of the mitochondrial and ribosomal genes
  ## (gene lists in extdata) that are present in the data set
  for (kind in c('mitochondrial', 'ribosomal')) {
    short <- if (kind == 'mitochondrial') 'mt' else 'ribo'
    genes <- utils::read.delim(
      system.file(
        paste0(
          'extdata/genes_',
          short,
          '_',
          organism,
          '_',
          gene_nomenclature,
          '.tsv.gz'
        ),
        package = 'CerebroNexus'
      ),
      sep = "\t",
      header = FALSE,
      stringsAsFactors = FALSE
    )[[1]]
    genes <- intersect(genes, rownames(counts_matrix))
    if (length(genes) > 0) {
      object@misc$gene_lists[[paste0(kind, '_genes')]] <- genes
      .logStep(
        'Calculate percentage of ',
        length(genes),
        ' ',
        kind,
        ' transcript(s) present in the data set...'
      )
      values <- Matrix::colSums(counts_matrix[genes, , drop = FALSE]) /
        Matrix::colSums(counts_matrix)
    } else {
      object@misc$gene_lists[[paste0(kind, '_genes')]] <- paste0(
        'no_',
        kind,
        '_genes_found'
      )
      .logStep('No ', kind, ' genes found in data set.')
      values <- 0
    }
    object[[paste0('percent_', short)]] <- values
  }

  return(object)
}
