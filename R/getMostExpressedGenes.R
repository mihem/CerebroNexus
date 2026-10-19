#' @title
#' Get most expressed genes for specified grouping variables in Seurat object.
#'
#' @description
#' This function calculates the most expressed genes for one or multiple
#' grouping variables in the meta data of the provided Seurat object.
#'
#' @param object Seurat object.
#' @param assay Assay to pull transcripts counts from; defaults to 'RNA'.
#' @param groups Grouping variables (columns) in \code{object@meta.data} for
#' which most expressed genes should be calculated; defaults to NULL.
#'
#' @return
#' Seurat object with most expressed genes stored for every group level of the
#' specified groups stored in \code{object@misc$most_expressed_genes}.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
#'   package = "CerebroNexus"))
#' pbmc <- getMostExpressedGenes(
#'   object = pbmc,
#'   assay = 'RNA',
#'   groups = c('sample','seurat_clusters')
#' )
#'
#' @import dplyr
#' @importFrom Matrix rowSums
#' @importFrom rlang .data :=
#' @importFrom tibble tibble
#'
#' @export
#'
getMostExpressedGenes <- function(
  object,
  assay = 'RNA',
  groups = NULL
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

  .checkSeuratGroups(object, groups)

  if (is.null(object@misc$most_expressed_genes)) {
    object@misc$most_expressed_genes <- list()
  }

  ## top 100 genes by share of all transcripts in `cells`; NULL means all cells
  top_genes <- function(level, cells = NULL) {
    counts <- if (is.null(cells)) {
      counts_matrix
    } else {
      counts_matrix[, cells, drop = FALSE]
    }
    transcripts_counts_per_gene <- Matrix::rowSums(counts)
    transcripts_percent_per_gene <- sort(
      transcripts_counts_per_gene / sum(transcripts_counts_per_gene),
      decreasing = TRUE
    )
    tibble::tibble(
      group = level,
      gene = names(transcripts_percent_per_gene)[1:100],
      pct = transcripts_percent_per_gene[1:100]
    )
  }

  ## get most expressed genes for each group level in every group
  for (current_group in groups) {
    group_levels <- .seuratGroupLevels(
      object@meta.data[[current_group]],
      current_group,
      ' cell(s) without group assignment  (NA) for `'
    )

    ## ... if only 1 group level is present, use all cells
    if (length(group_levels) == 1) {
      .logStep(
        'Group `',
        current_group,
        '` contains only one subgroup. Will calculate most expressed genes ',
        'across all cells of this group...'
      )
      results <- list(top_genes(group_levels))
    } else if (length(group_levels) > 1) {
      .logStep(
        'Get most expressed genes for ',
        length(group_levels),
        ' groups in `',
        current_group,
        '`...'
      )
      results <- pbapply::pblapply(group_levels, function(x) {
        top_genes(
          x,
          rownames(object@meta.data)[which(
            object@meta.data[[current_group]] == x
          )]
        )
      })
    } else {
      next
    }

    ## merge tables with results and factorize group levels
    object@misc[["most_expressed_genes"]][[current_group]] <- do.call(
      rbind,
      results
    ) %>%
      dplyr::mutate(group = factor(group, levels = group_levels)) %>%
      dplyr::rename(!!current_group := group)
  }

  return(object)
}
