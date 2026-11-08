#' @keywords internal
#' @noRd
.readSCEFile <- function(sce_file) {
  if (!file.exists(sce_file)) {
    stop("sce_file not found: ", sce_file, call. = FALSE)
  }
  if (.zarrIsStore(sce_file)) {
    return(
      if (length(.sdTables(sce_file))) {
        readSpatialDataAsSCE(sce_file)
      } else {
        readH5adAsSCE(sce_file)
      }
    )
  }
  ext <- tolower(tools::file_ext(sce_file))
  switch(
    ext,
    rds = readRDS(sce_file),
    qs2 = {
      if (!requireNamespace("qs2", quietly = TRUE)) {
        stop("Reading .qs2 files requires the 'qs2' package.", call. = FALSE)
      }
      qs2::qs_read(sce_file)
    },
    h5ad = ,
    h5mu = readH5adAsSCE(sce_file),
    stop(
      "Unsupported sce_file format: .",
      ext,
      ". Use .rds (saved with saveRDS), .qs2 (saved with qs2::qs_save) or ",
      ".h5ad (AnnData).",
      call. = FALSE
    )
  )
}

#' Fill the nUMI / nGene columns when the object does not carry them.
#'
#' Seurat objects always have nCount_*/nFeature_*; an SCE only has such columns
#' after a QC step (e.g. scuttle::addPerCellQC). When a column is absent it is
#' computed from the \code{counts} assay (or the requested assay without
#' \code{counts}) rather than failing the whole conversion.
#' @keywords internal
#' @noRd
.sceEnsureQcColumns <- function(object, nUMI, nGene, assay, verbose = FALSE) {
  meta <- SummarizedExperiment::colData(object)
  need_umi <- !nUMI %in% names(meta)
  need_gene <- !nGene %in% names(meta)
  if (!need_umi && !need_gene) {
    return(object)
  }
  source_assay <- if ("counts" %in% SummarizedExperiment::assayNames(object)) {
    "counts"
  } else {
    assay
  }
  counts <- SummarizedExperiment::assay(object, source_assay)
  if (need_umi) {
    meta[[nUMI]] <- as.numeric(Matrix::colSums(counts))
  }
  if (need_gene) {
    meta[[nGene]] <- if (inherits(counts, "dgCMatrix")) {
      ## positive entries per column without building counts > 0
      running <- c(0, cumsum(counts@x > 0))
      as.numeric(diff(running[counts@p + 1L]))
    } else {
      as.numeric(Matrix::colSums(counts > 0))
    }
  }
  message(
    "[INFO] Computed missing QC column(s) ",
    paste(
      c(
        if (need_umi) paste0("`", nUMI, "`"),
        if (need_gene) paste0("`", nGene, "`")
      ),
      collapse = " and "
    ),
    " from assay `",
    source_assay,
    "`."
  )
  SummarizedExperiment::colData(object) <- meta
  object
}

#' Convert a SingleCellExperiment object to a Cerebro file
#'
#' @description
#' One-step route from a \code{SingleCellExperiment} (SCE) object, or a file
#' holding one, to a \code{.crb} file that \code{\link{createShinyApp}} can
#' serve. It is the SCE counterpart of \code{\link{convertSeuratToCerebro}}:
#' both run the same preparation (group validation and renaming, marker tables,
#' most-expressed genes and mean expression) and then hand over to an exporter
#' built on the same core, so every storage option and feature of the Seurat
#' route is available here.
#'
#' @param sce_file A \code{SingleCellExperiment} object, or the path to a
#'   \code{.rds} (\code{saveRDS()}) or \code{.qs2} (\code{qs2::qs_save()}) file
#'   holding one, or to an \code{.h5ad} file (read with
#'   \code{\link{readH5adAsSCE}}; see \code{\link{convertH5adToCerebro}}).
#'   There is no SCE-specific file extension: an SCE is an ordinary R object, so
#'   it is stored with whichever of these serializers was used.
#' @param result_dir Directory the \code{.crb} is written to (created if
#'   missing). The file is named \code{cerebro_<basename>.crb}.
#' @param extra_features Alternative experiments (\code{altExp}, for example
#'   antibody counts \code{"ADT"}) to add to the expression data as extra
#'   features: their names, or \code{TRUE} for all. Features are named
#'   \code{<experiment>:<feature>}. They are taken from the assay of the same name
#'   as \code{assay} when it exists, otherwise from \code{X}, \code{counts} or
#'   the first assay, so check that this is on the same scale as the genes.
#' @param assay Name of the assay to export; default \code{"logcounts"}.
#' @param experiment_name Experiment name stored in the Cerebro object. Defaults
#'   to the file's base name, or \code{"SingleCellExperiment"} for an in-memory
#'   object.
#' @param organism Organism label (e.g. \code{"Human"}, \code{"Mouse"}).
#' @param groups Character vector of \code{colData} columns to use as grouping
#'   variables. Required; an error lists the usable columns when omitted.
#' @param groups_naming Named list renaming grouping variables
#'   (\code{old = "new"}); default \code{NULL}.
#' @param max_group_levels Groups with more unique values than this are
#'   excluded; default \code{100}.
#' @param nUMI,nGene \code{colData} columns holding transcripts and expressed
#'   genes per cell. When a column is missing it is computed from the
#'   \code{counts} assay (or from \code{assay} if there is no \code{counts}).
#' @param add_all_meta_data,use_delayed_array,expression_matrix_mode,codec,verbose
#'   As in \code{\link{exportFromSCE}}.
#' @param cell_cycle,main_group,projections As in \code{\link{exportFromSCE}}.
#' @param viewer_binary,viewer_binary_threshold As in
#'   \code{\link{exportFromSCE}}.
#' @param marker_file Optional marker gene table (\code{.csv}, \code{.tsv},
#'   \code{.txt}, \code{.tab}).
#' @param marker_method Label for the marker table; default
#'   \code{"Diff. Expression"}.
#' @param add_most_expressed_genes Compute the most expressed genes and mean
#'   expression per group; default \code{TRUE}.
#' @param most_expressed_genes Optional pre-computed most-expressed-genes
#'   table(s); see \code{\link{convertSeuratToCerebro}}.
#'
#' @param spatial_split_by Optional \code{colData} column dividing the spatial
#'   coordinates into one section per level; see \code{\link{exportFromSCE}}.
#' @param .source_name Internal. Base name used for the output file and the
#'   default experiment name when \code{sce_file} is an object that was read
#'   from a file by a caller (see \code{\link{convertH5adToCerebro}}).
#'
#' @return The path of the written \code{.crb}, invisibly.
#'
#' @details
#' Immune repertoire and HLA typing stored in \code{metadata(sce)} are exported
#' like their \code{@misc} counterparts on a Seurat object. Attaching BCR/TCR
#' files (\code{bcr_file}/\code{tcr_file}) is not available for SCE yet.
#'
#' @examplesIf requireNamespace("SingleCellExperiment", quietly = TRUE)
#' \donttest{
#' convertSCEToCerebro(
#'   sce_file = system.file("extdata/examples/pbmc_SCE.rds",
#'     package = "CerebroNexus"),
#'   result_dir = file.path(tempdir(), "output"),
#'   experiment_name = "PBMC (SCE)",
#'   organism = "Human",
#'   groups = c("sample", "cluster")
#' )
#' }
#'
#' @seealso \code{\link{exportFromSCE}}, \code{\link{convertSeuratToCerebro}}
#'
#' @export
convertSCEToCerebro <- function(
  sce_file,
  result_dir,
  assay = "logcounts",
  experiment_name = NULL,
  organism = "Human",
  groups = NULL,
  groups_naming = NULL,
  max_group_levels = 100,
  nUMI = "nUMI",
  nGene = "nGene",
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "h5"),
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  verbose = TRUE,
  cell_cycle = NULL,
  main_group = NULL,
  projections = NULL,
  marker_file = NULL,
  marker_method = "Diff. Expression",
  add_most_expressed_genes = TRUE,
  most_expressed_genes = NULL,
  spatial_split_by = NULL,
  extra_features = NULL,
  .source_name = NULL
) {
  expression_matrix_mode <- match.arg(expression_matrix_mode)
  codec <- match.arg(codec)
  viewer_binary <- match.arg(viewer_binary)
  if (!requireNamespace("SingleCellExperiment", quietly = TRUE)) {
    stop(
      "The 'SingleCellExperiment' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  if (inherits(sce_file, "SingleCellExperiment")) {
    sce <- sce_file
  } else if (is.character(sce_file) && length(sce_file) == 1L) {
    sce <- .readSCEFile(sce_file)
    if (!inherits(sce, "SingleCellExperiment")) {
      stop(
        "`",
        sce_file,
        "` holds an object of class `",
        paste(class(sce), collapse = ", "),
        "`, not a SingleCellExperiment.",
        call. = FALSE
      )
    }
  } else {
    stop(
      "`sce_file` must be a SingleCellExperiment or one file path.",
      call. = FALSE
    )
  }

  if (is.null(groups) || !length(groups)) {
    meta <- SummarizedExperiment::colData(sce)
    usable <- names(meta)[vapply(
      names(meta),
      function(column) {
        is.character(meta[[column]]) || is.factor(meta[[column]])
      },
      logical(1)
    )]
    stop(
      "`groups` is required. Character/factor columns in colData: ",
      if (length(usable)) paste(usable, collapse = ", ") else "(none)",
      call. = FALSE
    )
  }
  if (!assay %in% SummarizedExperiment::assayNames(sce)) {
    stop(
      "Specified assay `",
      assay,
      "` could not be found in provided SCE object. Available: ",
      paste(SummarizedExperiment::assayNames(sce), collapse = ", "),
      call. = FALSE
    )
  }

  sce <- .sceEnsureQcColumns(sce, nUMI, nGene, assay, verbose)
  if (!is.null(extra_features) && !isFALSE(extra_features)) {
    sce <- .sceAppendAltExps(sce, assay, extra_features, verbose)
  }

  ## Tissue images of several Visium libraries each belong to their own section
  ## (scanpy/squidpy name them in obs["library_id"]): split by it unless told
  ## otherwise, or the images cannot be shown on their sections.
  if (
    is.null(spatial_split_by) &&
      length(sce@metadata$spatial_images) > 1L &&
      "library_id" %in% names(SummarizedExperiment::colData(sce))
  ) {
    spatial_split_by <- "library_id"
    message(
      "[INFO] Several tissue-image libraries: using spatial_split_by = ",
      "\"library_id\"."
    )
  }

  # Same preparation as the Seurat route ---------------------------------------
  validated <- .convertValidateGroups(
    SummarizedExperiment::colData(sce),
    groups,
    max_group_levels,
    verbose
  )
  SummarizedExperiment::colData(sce) <- validated$meta
  groups <- validated$groups

  renamed <- .convertRenameGroups(
    SummarizedExperiment::colData(sce),
    sce@metadata,
    groups,
    groups_naming
  )
  SummarizedExperiment::colData(sce) <- renamed$meta
  sce@metadata <- renamed$misc
  groups <- renamed$groups
  if (!is.null(main_group) && !main_group %in% groups) {
    ## The user names the group they see; follow a rename of it.
    renamed_main <- unlist(groups_naming)[main_group]
    if (!is.null(renamed_main) && !is.na(renamed_main)) {
      main_group <- unname(renamed_main)
    }
  }

  sce@metadata <- .convertAttachMarkers(
    sce@metadata,
    marker_file,
    marker_method,
    verbose,
    group_values = stats::setNames(
      lapply(groups, function(group) {
        unique(as.character(SummarizedExperiment::colData(sce)[[group]]))
      }),
      groups
    )
  )

  provided <- .convertAddProvidedMostExpressed(
    sce@metadata,
    most_expressed_genes,
    verbose
  )
  sce@metadata <- provided$misc
  if (provided$provided) {
    add_most_expressed_genes <- FALSE
  }

  if (add_most_expressed_genes) {
    if (verbose) {
      message(paste0(
        "[",
        format(Sys.time(), "%H:%M:%S"),
        "] Calculating most expressed genes and mean expression for each group..."
      ))
    }
    expr_matrix <- .validate_expression_cells(
      expression_data = SummarizedExperiment::assay(sce, assay),
      object_cells = colnames(sce),
      assay = assay,
      requested_layer = assay,
      resolved_layer = assay
    )
    sce@metadata <- .convertComputeGroupSummaries(
      sce@metadata,
      SummarizedExperiment::colData(sce),
      expr_matrix,
      groups,
      verbose
    )
    rm(expr_matrix)
  }

  # Output name -----------------------------------------------------------------
  if (is.character(sce_file)) {
    base_name <- tools::file_path_sans_ext(basename(sce_file))
    source_label <- basename(sce_file)
  } else if (!is.null(.source_name)) {
    base_name <- tools::file_path_sans_ext(basename(.source_name))
    source_label <- basename(.source_name)
  } else {
    base_name <- gsub(
      "[^A-Za-z0-9._-]",
      "_",
      if (is.null(experiment_name)) "SingleCellExperiment" else experiment_name
    )
    source_label <- "<SingleCellExperiment>"
  }
  if (is.null(experiment_name)) {
    experiment_name <- if (is.character(sce_file) || !is.null(.source_name)) {
      base_name
    } else {
      "SingleCellExperiment"
    }
  }
  file_name <- paste0("cerebro_", base_name, ".crb")
  out_file <- file.path(result_dir, file_name)

  tryCatch(
    {
      exportFromSCE(
        sce,
        assay = assay,
        file = out_file,
        experiment_name = experiment_name,
        organism = organism,
        groups = groups,
        cell_cycle = cell_cycle,
        nUMI = nUMI,
        nGene = nGene,
        add_all_meta_data = add_all_meta_data,
        use_delayed_array = use_delayed_array,
        expression_matrix_mode = expression_matrix_mode,
        codec = codec,
        viewer_binary = viewer_binary,
        viewer_binary_threshold = viewer_binary_threshold,
        verbose = verbose,
        main_group = main_group,
        projections = projections,
        spatial_split_by = spatial_split_by
      )
      message("Successfully exported: ", file_name)
    },
    error = function(e) {
      stop(
        "Error processing ",
        source_label,
        ": ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  invisible(out_file)
}

#' Add alternative experiments to the expression data as extra features.
#'
#' Returns a SingleCellExperiment with one assay, named `assay`: the main
#' features followed by the features of each experiment, named
#' "<experiment>:<feature>". Cell data, reduced dimensions and metadata are kept.
#' @keywords internal
#' @noRd
.sceAppendAltExps <- function(sce, assay, which, verbose = FALSE) {
  available <- SingleCellExperiment::altExpNames(sce)
  if (isTRUE(which)) {
    which <- available
  }
  which <- as.character(which)
  if (!length(which)) {
    return(sce)
  }
  unknown <- setdiff(which, available)
  if (length(unknown)) {
    stop(
      "Alternative experiment `",
      unknown[[1L]],
      "` not found",
      if (length(available)) {
        paste0("; the object has: ", paste(available, collapse = ", "))
      } else {
        ""
      },
      ".",
      call. = FALSE
    )
  }
  as_sparse <- function(x) {
    if (!methods::is(x, "dgCMatrix")) {
      x <- methods::as(
        methods::as(methods::as(x, "dMatrix"), "generalMatrix"),
        "CsparseMatrix"
      )
    }
    x
  }
  combined <- rbind(
    as_sparse(SummarizedExperiment::assay(sce, assay)),
    .sceAltExpRows(sce, assay, which, verbose)
  )
  result <- SingleCellExperiment::SingleCellExperiment(
    assays = stats::setNames(list(combined), assay),
    colData = SummarizedExperiment::colData(sce),
    reducedDims = SingleCellExperiment::reducedDims(sce)
  )
  result@metadata <- sce@metadata
  result
}

#' The features of alternative experiments as rows to append to the main
#' features: named "<experiment>:<feature>", one matrix, cells in the order of
#' `sce`. Taken from the assay of the same name as `assay` when it exists, then
#' from X, counts or the first assay.
#' @keywords internal
#' @noRd
.sceAltExpRows <- function(sce, assay, which, verbose = FALSE) {
  as_sparse <- function(x) {
    if (!methods::is(x, "dgCMatrix")) {
      x <- methods::as(
        methods::as(methods::as(x, "dMatrix"), "generalMatrix"),
        "CsparseMatrix"
      )
    }
    x
  }
  pieces <- list()
  for (name in which) {
    alt <- SingleCellExperiment::altExp(sce, name)
    names_in_alt <- SummarizedExperiment::assayNames(alt)
    source <- intersect(c(assay, "X", "counts"), names_in_alt)
    source <- if (length(source)) source[[1L]] else names_in_alt[[1L]]
    values <- as_sparse(SummarizedExperiment::assay(alt, source))
    dimnames(values) <- list(paste0(name, ":", rownames(alt)), colnames(sce))
    pieces[[length(pieces) + 1L]] <- values
    if (verbose) {
      message(
        "[INFO] Added ",
        nrow(values),
        " feature(s) of `",
        name,
        "` (assay `",
        source,
        "`)."
      )
    }
  }
  do.call(rbind, pieces)
}
