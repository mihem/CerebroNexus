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
#' pbmc <- readRDS(system.file("extdata/pbmc_seurat.rds",
#'   package = "cerebroAppLite"))
#' pbmc <- getMostExpressedGenes(
#'   object = pbmc,
#'   assay = 'RNA',
#'   groups = c('sample','seurat_clusters')
#' )
#'
#' @import dplyr
#' @importFrom Matrix rowSums
#' @importFrom pbapply pblapply
#' @importFrom rlang .data
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

  ## check if Seurat is installed
  if ( !requireNamespace("Seurat", quietly = TRUE) ) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check that Seurat package is at least v3.0
  if ( utils::packageVersion('Seurat') < "3" ) {
    stop(
      paste0(
        "The installed Seurat package is of version `", utils::packageVersion('Seurat'),
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }

  ## check if provided object is of class "Seurat"
  if ( class(object) != "Seurat" ) {
    stop(
      paste0(
        "Provided object is of class `", class(object), "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }

  ## check version of Seurat object and stop if it is lower than 3
  if ( object@version < "3" ) {
    stop(
      paste0(
        "Provided Seurat object has version `", object@version, "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }

  ## check if provided assay exists
  if ( assay %in% names(object@assays) == FALSE ) {
    stop(
      paste0(
        'Specified assay slot `', assay, '` could not be found in provided Seurat object.'
      ),
      call. = FALSE
    )
  }

  ## check if `counts` matrix exist in provided assay
  if ( is.null(object@assays[[assay]]@counts) ) {
    stop(
      paste0(
        '`counts` matrix could not be found in `', assay, '` assay slot of the provided Seurat object.'
      ),
      call. = FALSE
    )
  }

  ## check if provided groups are present in meta data
  if ( any(which(groups %in% colnames(object@meta.data) == FALSE)) ) {
    missing_groups <- groups[which(groups %in% colnames(object@meta.data) == FALSE)]
    stop(
      paste0(
        "Group(s) `", paste0(missing_groups, collapse = '`, `'), "` were not ",
        "found in meta data of provided Seurat object. Only grouping variables ",
        "that are present in the meta data can be used."
      ),
      call. = FALSE
    )
  }

  ## check if provided groups are factors or characters
  for (group in groups) {
    if ( !is.factor(object@meta.data[[group]]) && !is.character(object@meta.data[[group]]) ) {
      stop(
        paste0(
          "Group `", group, "` is neither a factor nor a character vector. ",
          "Please convert it to one of these types before running this function."
        ),
        call. = FALSE
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## create slot for results in Seurat object if not already existing
  ##--------------------------------------------------------------------------##

  if ( is.null(object@misc$most_expressed_genes) ) {
    object@misc$most_expressed_genes <- list()
  }

  ##--------------------------------------------------------------------------##
  ## get most expressed genes for each group level in every group
  ##--------------------------------------------------------------------------##

  ## get counts matrix once to avoid repeated slot access
  counts_matrix <- object@assays[[assay]]@counts

  ##
  for ( i in seq_along(groups) ) {

    current_group <- groups[i]

    ## collect group levels
    ## ... column contains factors
    if ( is.factor(object@meta.data[[ current_group ]]) ) {
      ## get factor levels
      group_levels <- levels(object@meta.data[[ current_group ]])
    ## ... column contains characters
    } else if ( is.character(object@meta.data[[ current_group ]]) ) {
      ## get unique entries in column
      group_levels <- unique(object@meta.data[[ current_group ]])
      ## check for NA values
      if ( any(is.na(group_levels)) ) {
        ## get number of cells with NA as group assignment
        number_of_cells_without_group_assignment <- sum(is.na(object@meta.data[[ current_group ]]))
        ## remove NA entries from group levels
        group_levels <- stats::na.omit(group_levels)
        ## issue warning to user
        warning(
          paste0(
            'Found ', number_of_cells_without_group_assignment,
            ' cell(s) without group assignment (NA) for `', current_group,
            '`. These cells will be ignored during the analysis.'
          ),
          call. = FALSE
        )
      }
    }

    ## check number of group levels
    if ( length(group_levels) == 0 ) {
      next
    }

    message(
      paste0(
        '[', format(Sys.time(), '%H:%M:%S'), '] Get most expressed genes for ',
        length(group_levels), ' group(s) in `', current_group, '`...'
      )
    )

    results <- pbapply::pblapply(group_levels, function(x) {
      ## get names of cells belonging to current group level
      cells_of_current_group_level <- rownames(object@meta.data)[ which(object@meta.data[[ current_group ]] == x) ]

      ## subset transcript count matrix
      transcript_count_matrix <- counts_matrix[, cells_of_current_group_level]

      ## calculate sums for all genes
      if ( is.vector(transcript_count_matrix) ) {
        transcripts_counts_per_gene <- transcript_count_matrix
      } else {
        transcripts_counts_per_gene <- Matrix::rowSums(transcript_count_matrix)
      }

      ## calculate transcript count across all cells of current group level
      total_transcript_count <- sum(transcripts_counts_per_gene)

      if ( total_transcript_count == 0 ) {
        return(tibble::tibble(group = x, gene = character(0), pct = numeric(0)))
      }

      ## transform transcript counts per gene to percentage of all transcripts
      transcripts_percent_per_gene <- transcripts_counts_per_gene / total_transcript_count

      ## sort percentage values decreasingly
      transcripts_percent_per_gene <- sort(transcripts_percent_per_gene, decreasing = TRUE)

      ## get top 100 (safely)
      top_n <- head(transcripts_percent_per_gene, 100)

      ## build data frame with results
      tibble::tibble(
          group = x,
          gene = names(top_n),
          pct = as.numeric(top_n)
        )
    })

    ## merge tables with results and factorize group levels
    most_expressed_genes <- do.call(rbind, results) %>%
      dplyr::mutate(group = factor(group, levels = group_levels)) %>%
      dplyr::rename(!!current_group := group)

    ## add results to Seurat object
    object@misc[["most_expressed_genes"]][[ current_group ]] <- most_expressed_genes
  }

  ##--------------------------------------------------------------------------##
  ## return Seurat object
  ##--------------------------------------------------------------------------##
  return(object)
}
