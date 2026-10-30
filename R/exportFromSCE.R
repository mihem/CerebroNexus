#' @title
#' Export SingleCellExperiment (SCE) object to Cerebro.
#'
#' @description
#' This function allows to export a \code{SingleCellExperiment} (\code{SCE})
#' object to visualize in Cerebro.
#'
#' @param object \code{SingleCellExperiment} (\code{SCE}) object.
#' @param assay Assay to pull expression values from; defaults to
#' \code{logcounts}. It is recommended to use sparse data (such as
#' log-transformed or raw counts) instead of dense data (such as the 'scaled'
#' slot) to avoid performance bottlenecks in the Cerebro interface.
#' @param file Where to save the output.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. \code{hg} (human), \code{mm} (mouse), etc.
#' @param groups Names of grouping variables in meta data
#' (\code{SingleCellExperiment::colData(object)}), e.g. \code{c("sample","cluster")}; at least one
#' must be provided; defaults to \code{NULL}.
#' @param cell_cycle Names of columns in meta data (\code{SingleCellExperiment::colData(object)}) that#
#' contain cell cycle information, e.g. \code{c("Phase")}; defaults to
#' \code{NULL}.
#' @param nUMI Column in \code{SingleCellExperiment::colData(object)} that contains information about
#' number of transcripts per cell; defaults to \code{nUMI}.
#' @param nGene Column in \code{SingleCellExperiment::colData(object)} that contains information about
#' number of expressed genes per cell; defaults to \code{nGene}.
#' @param add_all_meta_data If set to \code{TRUE}, all further meta data columns
#' will be extracted as well.
#' @param use_delayed_array When set to \code{TRUE}, the expression matrix will
#' be stored as an \code{RleMatrix} (see \code{DelayedArray} package). This can
#' be useful for very large data sets, as the matrix won't be loaded into memory
#' and instead values will be read from the disk directly, at the cost of
#' performance. Note that it is necessary to install the \code{DelayedArray}
#' package. If set to \code{FALSE} (default), the expression matrix will be
#' copied from the input object as is. It is recommended to use a sparse format,
#' such as \code{dgCMatrix} from the \code{Matrix} package.
#' @param codec Serialization codec for the CRB payload. Defaults to
#' \code{"qs2"}; use \code{"rds"} when direct compatibility with
#' \code{readRDS()} is required.
#' @param viewer_binary Build the derived Viewer Pack \code{"auto"}matically
#' for large datasets, \code{"always"}, or \code{"never"}.
#' @param viewer_binary_threshold Dataset-level cell threshold used by
#' \code{viewer_binary = "auto"}.
#' @param verbose Set this to \code{TRUE} if you want additional log messages;
#' defaults to \code{FALSE}.
#'
#' @return
#' No data returned.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/examples/pbmc_SCE.rds",
#'   package = "CerebroNexus"))
#' exportFromSCE(
#'   object = pbmc,
#'   file = file.path(tempdir(), 'pbmc_SCE.crb'),
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   groups = c('sample','cluster'),
#'   nUMI = 'nUMI',
#'   nGene = 'nGene',
#'   use_delayed_array = FALSE,
#'   verbose = TRUE
#' )
#'
#' @import dplyr
#' @importFrom methods as
#' @importFrom rlang .data
#'
#' @export
#'
exportFromSCE <- function(
  object,
  assay = 'logcounts',
  file,
  experiment_name,
  organism,
  groups,
  cell_cycle = NULL,
  nUMI = 'nUMI',
  nGene = 'nGene',
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  verbose = FALSE
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  codec <- match.arg(codec)

  ## check if provided object is of class "SingleCellExperiment"
  if (!inherits(object, "SingleCellExperiment")) {
    stop(
      paste0(
        "Provided object is of class `",
        class(object),
        "` but must be of class 'SingleCellExperiment'."
      ),
      call. = FALSE
    )
  }

  ## check if SingleCellExperiment is installed
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    stop(
      "The 'SingleCellExperiment' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  meta <- SingleCellExperiment::colData(object)
  .validateExportColumns(names(meta), groups, nUMI, nGene, cell_cycle)

  ## check if provided assay exists
  if ((assay %in% names(SummarizedExperiment::assays(object)) == FALSE)) {
    stop(
      glue::glue(
        'Specified assay `{assay}` could not be found in provided SCE object.'
      ),
      call. = FALSE
    )
  }

  export <- .newCerebroExport(experiment_name, organism, verbose)

  ## the assay was checked above
  export$setExpression(.delayedExportExpression(
    SummarizedExperiment::assay(object, assay),
    use_delayed_array,
    verbose
  ))

  extras <- object@metadata
  .addExportExperiment(export, extras)
  .addExportMetaData(
    export,
    meta,
    colnames(object),
    groups,
    nUMI,
    nGene,
    cell_cycle,
    add_all_meta_data,
    verbose
  )
  .addExportProjections(
    export,
    names(SingleCellExperiment::reducedDims(object)),
    function(name) SingleCellExperiment::reducedDims(object)[[name]],
    verbose
  )
  .addExportGroupTables(
    extras$trees,
    'object@metadata$trees',
    'Extracting trees...',
    export$addTree,
    verbose
  )
  .addExportGroupTables(
    extras$most_expressed_genes,
    'object@metadata$most_expressed_genes',
    'Extracting tables of most expressed genes...',
    export$addMostExpressedGenes,
    verbose
  )
  .addExportMethodTables(
    extras$marker_genes,
    'object@metadata$marker_genes',
    'Extracting tables of marker genes...',
    export$addMarkerGenes,
    verbose
  )
  .addExportMethodTables(
    extras$enriched_pathways,
    'object@metadata$enriched_pathways',
    'Extracting pathway enrichment results...',
    export$addEnrichedPathways,
    verbose
  )
  .addExportTrajectories(export, extras$trajectories, verbose)
  .addExportExtraMaterial(export, extras$extra_material, verbose)
  .printExportOverview(export)

  ## check if output directory exists and create it if not
  if (!file.exists(dirname(file))) {
    .logStep('Creating output directory...')
    dir.create(dirname(file), showWarnings = FALSE)
  }
  .logStep('Saving Cerebro object to: ', file)
  saveCerebro(
    export,
    file,
    codec = codec,
    viewer_binary = viewer_binary,
    viewer_binary_threshold = viewer_binary_threshold
  )
  ## saveCerebro() stops when the file is not written
  .logStep('Done!')
}
