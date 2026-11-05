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
#' such as \code{dgCMatrix} from the \code{Matrix} package. Ignored when
#' \code{expression_matrix_mode} is set to an external backend.
#' @param expression_matrix_mode How to persist the expression matrix. One of
#' \code{"embedded"} (default) or \code{"h5"}; identical to
#' \code{\link{exportFromSeurat}}. The \code{"h5"} backend writes a sibling
#' \code{<stem>.h5} file next to the \code{.crb}, and \code{file} must then
#' end in \code{.crb}.
#' @param codec Serialization codec for the CRB payload. Defaults to
#' \code{"qs2"}; use \code{"rds"} when direct compatibility with
#' \code{readRDS()} is required.
#' @param viewer_binary Build the derived Viewer Pack \code{"auto"}matically
#' for large datasets, \code{"always"}, or \code{"never"}.
#' @param viewer_binary_threshold Dataset-level cell threshold used by
#' \code{viewer_binary = "auto"}.
#' @param verbose Set this to \code{TRUE} if you want additional log messages;
#' defaults to \code{FALSE}.
#' @param main_group Optional entry of \code{groups} to use as the main group.
#' @param spatial_reductions Optional character vector naming the
#' \code{reducedDims(object)} that hold spatial point coordinates (cells x >= 2).
#' By default a reduction named \code{spatial} (any case) is used, which is how
#' AnnData \code{obsm["spatial"]} arrives; they fill the Spatial tab.
#' @param spatial_split_by Optional \code{colData} column. When given, the
#' spatial coordinates are divided by its values into one section per level
#' (e.g. the tissue sections of a MERFISH or multi-slice Visium data set that
#' share one coordinate frame), selectable in the Spatial tab.
#' @param projections Optional character vector naming the
#' \code{reducedDims(object)} to export. By default every non-PCA reduction is
#' exported (PCA only when it is the sole reduction).
#'
#' @details
#' The export is written by the same format-independent core as
#' \code{\link{exportFromSeurat}}: the CRB and any sidecar are built in a
#' private stage and published together, and \code{object@metadata} entries
#' (trees, markers, pathways, immune repertoire, HLA typing, extra material, ...)
#' are read with the same keys as \code{object@misc} on the Seurat side.
#' Two entries are specific to this route. \code{metadata(object)$group_colors}, a
#' list named by grouping variable of named colour vectors (level to
#' \code{"#RRGGBB"}), becomes the groups' default colours in the Viewer.
#' \code{metadata(object)$spatial_images}, a list named by library of images
#' (each \code{list(histology_image = <base64 data URI>,
#' histology_image_bounds = c(xmin, xmax, ymin, ymax))}), is drawn behind the
#' spatial coordinates: the single library's images go to the single spatial
#' entry, and with \code{spatial_split_by} each section takes the images of the
#' library named like its level. Both are filled in by
#' \code{\link{readH5adAsSCE}} from scanpy's \code{uns}.
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
  expression_matrix_mode = c("embedded", "h5"),
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  verbose = FALSE,
  main_group = NULL,
  projections = NULL,
  spatial_reductions = NULL,
  spatial_split_by = NULL
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  expression_matrix_mode <- match.arg(expression_matrix_mode)
  codec <- match.arg(codec)
  viewer_options <- .viewerPackOptions(viewer_binary, viewer_binary_threshold)
  .exportCheckOutputArgs(
    file,
    expression_matrix_mode,
    use_delayed_array,
    verbose
  )

  ## check if SingleCellExperiment is installed
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    stop(
      "The 'SingleCellExperiment' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## check if provided object is of class "SingleCellExperiment"
  if (!inherits(object, "SingleCellExperiment")) {
    stop(
      paste0(
        "Provided object is of class `",
        paste(class(object), collapse = ", "),
        "` but must be of class 'SingleCellExperiment'."
      ),
      call. = FALSE
    )
  }

  meta <- SingleCellExperiment::colData(object)
  available_reductions <- names(SingleCellExperiment::reducedDims(object))
  if (
    !is.null(projections) &&
      (!is.character(projections) ||
        anyNA(projections) ||
        any(!nzchar(projections)) ||
        anyDuplicated(projections) ||
        any(!projections %in% available_reductions))
  ) {
    stop(
      "`projections` must name unique dimensional reductions in the object.",
      call. = FALSE
    )
  }

  .validateExportColumns(
    names(meta),
    groups,
    nUMI,
    nGene,
    cell_cycle,
    main_group
  )

  ## check if provided assay exists
  if ((assay %in% names(SummarizedExperiment::assays(object)) == FALSE)) {
    stop(
      glue::glue(
        'Specified assay `{assay}` could not be found in provided SCE object.'
      ),
      call. = FALSE
    )
  }

  ## Stage every output artefact beside the final destination; nothing is
  ## published until the whole object has been built (see exportFromSeurat()).
  final_file <- file
  export_stage_dir <- .createPrivateExportStage(final_file)
  on.exit(
    unlink(export_stage_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## initialize Cerebro object
  ##--------------------------------------------------------------------------##
  export <- .exportInit(experiment_name, organism, object@metadata, verbose)

  ##--------------------------------------------------------------------------##
  ## add transcript counts
  ##--------------------------------------------------------------------------##
  expression_data <- tryCatch(
    SummarizedExperiment::assay(object, i = assay),
    error = function(e) {
      stop(
        "Assay `",
        assay,
        "` could not be read: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  ## Same guard as the Seurat path: the matrix must carry exactly the cells the
  ## meta data describes, in that order.
  cells <- colnames(object)
  expression_data <- .validate_expression_cells(
    expression_data = expression_data,
    object_cells = cells,
    assay = assay,
    requested_layer = assay,
    resolved_layer = assay
  )
  ## The external backends expect a dgCMatrix, a dense matrix or a delayed
  ## matrix; other Matrix classes (e.g. triplet or row-compressed) are converted.
  if (
    inherits(expression_data, "Matrix") &&
      !inherits(expression_data, "dgCMatrix")
  ) {
    expression_data <- methods::as(expression_data, "CsparseMatrix")
  }

  expression_data <- .exportWriteExpression(
    export = export,
    expression_data = expression_data,
    expression_matrix_mode = expression_matrix_mode,
    use_delayed_array = use_delayed_array,
    export_stage_dir = export_stage_dir,
    final_file = final_file,
    verbose = verbose
  )

  .addExportExperiment(export, object@metadata)

  .addExportMetaData(
    export = export,
    meta = meta,
    cells = cells,
    groups = groups,
    nUMI = nUMI,
    nGene = nGene,
    cell_cycle = cell_cycle,
    add_all_meta_data = add_all_meta_data,
    main_group = main_group,
    verbose = verbose
  )

  .addExportProjections(
    export,
    if (is.null(projections)) available_reductions else projections,
    function(name) SingleCellExperiment::reducedDims(object)[[name]],
    verbose,
    explicit = !is.null(projections)
  )

  .exportAddMiscResults(
    export = export,
    misc = object@metadata,
    groups = groups,
    cells = cells,
    verbose = verbose
  )

  ## Point coordinates (MERFISH, Slide-seq, AnnData obsm["spatial"]) held as a
  ## reduction named "spatial" also feed the Spatial tab.
  if (!is.null(spatial_split_by) && !spatial_split_by %in% names(meta)) {
    stop(
      "`spatial_split_by` must be a column of colData: ",
      spatial_split_by,
      call. = FALSE
    )
  }
  if (is.null(spatial_reductions)) {
    spatial_reductions <- available_reductions[
      tolower(available_reductions) == "spatial"
    ]
  } else if (!all(spatial_reductions %in% available_reductions)) {
    stop(
      "`spatial_reductions` must name reducedDims of the object.",
      call. = FALSE
    )
  }
  if (length(spatial_reductions)) {
    .exportAddSpatialFromEmbeddings(
      export = export,
      embeddings = stats::setNames(
        lapply(spatial_reductions, function(name) {
          SingleCellExperiment::reducedDim(object, name)
        }),
        spatial_reductions
      ),
      expression_data = expression_data,
      assay = assay,
      layer = assay,
      images = object@metadata$spatial_images,
      split = if (!is.null(spatial_split_by)) {
        stats::setNames(
          as.character(meta[[spatial_split_by]]),
          cells
        )
      },
      verbose = verbose
    )
  }

  .exportFinalize(
    export = export,
    final_file = final_file,
    export_stage_dir = export_stage_dir,
    expression_matrix_mode = expression_matrix_mode,
    codec = codec,
    viewer_options = viewer_options
  )
}
