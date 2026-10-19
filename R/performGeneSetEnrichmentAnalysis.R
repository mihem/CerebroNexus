#' @title
#' Perform gene set enrichment analysis with GSVA.
#'
#' @description
#' This function calculates enrichment scores, p- and q-value statistics for
#' provided gene sets for specified groups of cells in given Seurat object using
#' gene set variation analysis (GSVA). Calculation of p- and q-values for gene
#' sets is performed as done in "Evaluation of methods to assign cell type
#' labels to cell clusters from single-cell RNA-sequencing data", Diaz-Mejia et
#' al., F1000Research (2019).
#'
#' @param object Seurat object.
#' @param assay Assay to pull counts from; defaults to 'RNA'. Only relevant in
#' Seurat v3.0 or higher since the concept of assays wasn't implemented before.
#' @param GMT_file Path to GMT file containing the gene sets to be tested.
#' The Broad Institute provides many gene sets which can be downloaded:
#' http://software.broadinstitute.org/gsea/msigdb/index.jsp
#' @param groups Grouping variables (columns) in object@meta.data for which
#' gene set enrichment analysis should be performed
#' @param name Name of list that should be used to store the results in
#' object@misc$enriched_pathways$<name>; defaults to 'cerebro_GSVA'.
#' @param thresh_p_val Threshold for p-value, defaults to 0.05.
#' @param thresh_q_val Threshold for q-value, defaults to 0.1.
#' @param ... Further parameters passed to `GSVA::gsvaParam()` (GSVA >= 2.0) or `GSVA::gsva()` (GSVA < 2.0).
#'
#' @return
#' Seurat object with GSVA results for the specified grouping variables
#' stored in object@misc$enriched_pathways$<name>
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
#'   package = "CerebroNexus"))
#' example_gene_set <- system.file("extdata/example_gene_set.gmt",
#'   package = "CerebroNexus")
#' pbmc <- performGeneSetEnrichmentAnalysis(
#'   object = pbmc,
#'   GMT_file = example_gene_set,
#'   groups = c('sample','seurat_clusters'),
#'   thresh_p_val = 0.05,
#'   thresh_q_val = 0.1
#' )
#'
#' @import dplyr
#' @importFrom Matrix colMeans colSums rowSums t
#' @importFrom rlang .data :=
#' @importFrom tibble tibble
#'
#' @export
#'
performGeneSetEnrichmentAnalysis <- function(
  object,
  assay = 'RNA',
  GMT_file,
  groups = NULL,
  name = 'cerebro_GSVA',
  thresh_p_val = 0.05,
  thresh_q_val = 0.1,
  ...
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  .checkSeuratAvailable()
  .checkSeuratObject(object)
  .checkSeuratAssay(object, assay)

  ## GSVA requires normalized data and therefore never falls back to raw counts.
  data_matrix <- .getValidatedExpression(object, assay, "data")$data

  ## check if specified GMT files exists
  if (!file.exists(GMT_file)) {
    stop("Specified GMT file with gene sets cannot be found.", call. = FALSE)
  }

  .checkSeuratGroups(object, groups)

  ## check if specified p-value thresholds are between 0 and 1
  if (thresh_p_val < 0 | thresh_p_val > 1) {
    stop(
      "Specified threshold for p-value must be between 0 and 1.",
      call. = FALSE
    )
  }

  ## check if specified q-value thresholds are between 0 and 1
  if (thresh_q_val < 0 | thresh_q_val > 1) {
    stop(
      "Specified threshold for q-value must be between 0 and 1.",
      call. = FALSE
    )
  }

  ##---------------------------------------------------------------------------#
  ## preparation
  ##---------------------------------------------------------------------------#

  ## load gene sets from GMT file
  .logStep('Loading gene sets...')
  gene_sets <- .read_GMT_file(GMT_file)

  ## get names of gene sets
  names(gene_sets$genesets) <- gene_sets$geneset.names

  ## make tibble that contains name and description for each set
  ## with the number of genes and the collapsed gene list of each set
  gene_sets_tibble <- tibble::tibble(
    name = gene_sets$geneset.names,
    description = gene_sets$geneset.description,
    length = unname(lengths(gene_sets$genesets)),
    genes = unname(vapply(gene_sets$genesets, paste, '', collapse = ','))
  )

  ## log message
  .logStep('Loaded ', length(gene_sets$genesets), ' gene sets from GMT file.')

  ## check which genes are not expressed in any cell
  expressed_genes <- Matrix::rowSums(data_matrix)
  expressed_genes <- which(expressed_genes != 0)

  ## extract transcript counts for expressed genes from Seurat object
  .logStep(
    'Extracting transcript counts ',
    'from `data` slot of `',
    assay,
    '` assay...'
  )
  matrix_full <- data_matrix[expressed_genes, ]

  ##---------------------------------------------------------------------------#
  ## perform gene set enrichment analysis for each group
  ##---------------------------------------------------------------------------#

  ## create slot for results
  object@misc$enriched_pathways[[name]] <- list()

  ##
  for (i in seq_along(groups)) {
    ## get current group
    current_group <- groups[i]

    group_levels <- .seuratGroupLevels(
      object@meta.data[[current_group]],
      current_group
    )

    ## log message
    message(
      glue::glue(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Performing analysis for ',
        '{length(group_levels)} subgroups of group `{current_group}`...'
      )
    )

    ## check number of group levels
    ## ... if only 1 group level is present, show warning and move to next
    ##     grouping variable
    if (length(group_levels) == 1) {
      .logStep(
        'Only one group level found ',
        'for group `',
        current_group,
        '`. Will skip this group and proceed ',
        'to next.'
      )
      results <- NULL

      ## ... more than 1 group level is available
    } else if (length(group_levels) > 1) {
      ## mean expression of each gene in each group level
      matrix_mean_by_group <- future.apply::future_sapply(
        group_levels,
        function(x) {
          cells_in_this_group <- which(object@meta.data[[current_group]] == x)
          Matrix::rowMeans(matrix_full[, cells_in_this_group, drop = FALSE])
        },
        USE.NAMES = TRUE,
        simplify = TRUE,
        future.globals = FALSE
      )

      ## get enrichment score for each gene set in every cell group
      ## GSVA >= 2.0 uses a param-object API; fall back to old API for older versions
      if (utils::packageVersion("GSVA") >= "2.0") {
        gsva_param <- GSVA::gsvaParam(
          exprData = matrix_mean_by_group,
          geneSets = gene_sets$genesets,
          ...
        )
        enrichment_scores <- GSVA::gsva(gsva_param)
      } else {
        enrichment_scores <- GSVA::gsva(
          expr = matrix_mean_by_group,
          gset.idx.list = gene_sets$genesets,
          ...
        )
      }

      ## log message
      # message(
      #   paste0(
      #     '[', format(Sys.time(), '%H:%M:%S'), '] Filtering results based on ',
      #     'specified thresholds...'
      #   )
      # )

      ## create empty tibble for results
      results <- tibble::tibble(
        group = character(),
        name = character(),
        enrichment_score = numeric(),
        p_value = numeric(),
        q_value = numeric()
      )

      ## calculate statistics for enrichment scores and filter those which don't
      ## pass the specified tresholds
      for (i in seq_len(ncol(enrichment_scores))) {
        ## collect enrichment scores
        temp_results <- tibble::tibble(
          group = colnames(matrix_mean_by_group)[i],
          name = rownames(enrichment_scores),
          enrichment_score = enrichment_scores[, i]
        )

        ## check how many gene sets are available
        ## ... fewer than 2 gene sets available
        if (nrow(temp_results) < 2) {
          ## log message
          .logStep(
            'Only 1 gene set ',
            'available, therefore p- and q-values cannot be calculated.'
          )

          ## p- and q-value cannot be computed, add NA instead
          temp_results <- temp_results %>%
            dplyr::mutate(p_value = NA, q_value = NA)

          ## ... at least 2 gene sets are available
        } else if (nrow(temp_results) >= 2) {
          ## log message
          # message(
          #   paste0(
          #     '[', format(Sys.time(), '%H:%M:%S'), '] Filtering results based ',
          #     'on specified thresholds...'
          #   )
          # )

          ## calculate p- and q-values
          temp_p_values <- stats::pnorm(
            -abs(scale(temp_results$enrichment_score)[, 1])
          )
          temp_q_values <- suppressWarnings(
            qvalue::qvalue(temp_p_values, pi0 = 1, ties = min)$lfdr
          )

          ## assign p- and q-values and filter gene sets based on the results
          temp_results <- temp_results %>%
            dplyr::mutate(
              p_value = temp_p_values,
              q_value = temp_q_values
            ) %>%
            dplyr::filter(
              .data$p_value <= thresh_p_val,
              .data$q_value <= thresh_q_val
            )
        }

        ## merge and sort results
        results <- dplyr::bind_rows(results, temp_results) %>%
          dplyr::arrange(.data$q_value)
      }

      ## - add description, number of genes and list of genes to results
      ## - put columns in right order
      ## - rename group column
      results <- dplyr::left_join(results, gene_sets_tibble, by = 'name') %>%
        dplyr::select(c(
          'group',
          'name',
          'description',
          'length',
          'genes',
          'enrichment_score',
          'p_value',
          'q_value'
        )) %>%
        dplyr::rename(!!current_group := .data$group)

      ## factorize group column
      group_levels_with_results <- group_levels[
        group_levels %in% results[[current_group]]
      ]
      results[[current_group]] <- factor(
        results[[current_group]],
        levels = group_levels_with_results
      )

      ## print number of enriched gene sets
      message(
        glue::glue(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] {nrow(results)} gene sets ',
          'passed the thresholds across all subgroups of group `{current_group}`.'
        )
      )

      ## test if any gene sets passed the filtering
      if (nrow(results) == 0) {
        results <- 'no_gene_sets_enriched'
      }

      ## add results to Seurat object
      object@misc[["enriched_pathways"]][[name]][[current_group]] <- results
    }
  }

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(object)
}
