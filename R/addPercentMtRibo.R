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

  .validateSeuratInputs(object)

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

  ##--------------------------------------------------------------------------##
  ## load mitochondrial and ribosomal gene lists from extdata
  ##--------------------------------------------------------------------------##

  genes_mt <- utils::read.delim(
    system.file(
      paste0(
        'extdata/genes_mt_',
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
  ) %>%
    dplyr::select(1) %>%
    t() %>%
    as.vector()
  genes_ribo <- utils::read.delim(
    system.file(
      paste0(
        'extdata/genes_ribo_',
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
  ) %>%
    dplyr::select(1) %>%
    t() %>%
    as.vector()

  ##--------------------------------------------------------------------------##
  ## keep only genes that are present in data set
  ##--------------------------------------------------------------------------##

  if (!(assay %in% names(object@assays))) {
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
  if (is.null(counts_matrix) || nrow(counts_matrix) == 0) {
    stop(
      paste0(
        '`counts` matrix could not be found in `',
        assay,
        '` assay slot.'
      ),
      call. = FALSE
    )
  }
  genes_mt_here <- intersect(genes_mt, rownames(counts_matrix))
  genes_ribo_here <- intersect(genes_ribo, rownames(counts_matrix))

  ##--------------------------------------------------------------------------##
  ## prepare slot in Seurat object to store gene lists (if it doesn't already
  ## exist)
  ##--------------------------------------------------------------------------##

  if (is.null(object@misc$gene_lists)) {
    object@misc$gene_lists <- list()
  }

  ##--------------------------------------------------------------------------##
  ## calculate mitochondrial gene expression
  ##--------------------------------------------------------------------------##

  if (length(genes_mt_here) > 0) {
    object@misc$gene_lists$mitochondrial_genes <- genes_mt_here
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Calculate percentage of ',
        length(genes_mt_here),
        ' mitochondrial transcript(s) present in the data set...'
      )
    )
    values_mt <- Matrix::colSums(
      counts_matrix[genes_mt_here, , drop = FALSE]
    ) /
      Matrix::colSums(counts_matrix)
  } else {
    object@misc$gene_lists$mitochondrial_genes <- 'no_mitochondrial_genes_found'
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] No mitochondrial genes found in data set.'
      )
    )
    values_mt <- 0
  }

  ##--------------------------------------------------------------------------##
  ## calculate ribosomal gene expression
  ##--------------------------------------------------------------------------##

  if (length(genes_ribo_here) > 0) {
    object@misc$gene_lists$ribosomal_genes <- genes_ribo_here
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Calculate percentage of ',
        length(genes_ribo_here),
        ' ribosomal transcript(s) present in the data set...'
      )
    )
    values_ribo <- Matrix::colSums(
      counts_matrix[genes_ribo_here, , drop = FALSE]
    ) /
      Matrix::colSums(counts_matrix)
  } else {
    object@misc$gene_lists$ribosomal_genes <- 'no_ribosomal_genes_found'
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] No ribosomal genes found in data set.'
      )
    )
    values_ribo <- 0
  }

  ##--------------------------------------------------------------------------##
  ## add results to Seurat object
  ##--------------------------------------------------------------------------##

  object$percent_mt <- values_mt
  object$percent_ribo <- values_ribo

  ##--------------------------------------------------------------------------##
  ##
  ##--------------------------------------------------------------------------##
  return(object)
}
