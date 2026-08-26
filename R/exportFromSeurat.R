.setExportArtifactMode <- function(path, mode, label) {
  if (identical(.Platform$OS.type, "windows")) {
    return(invisible(TRUE))
  }
  expected <- bitwAnd(as.integer(as.octmode(mode)), 511L)
  changed <- suppressWarnings(Sys.chmod(path, mode = as.octmode(expected)))
  actual <- bitwAnd(as.integer(file.info(path)$mode), 511L)
  if (!isTRUE(changed) || is.na(actual) || actual != expected) {
    stop(
      "Failed to set private permissions on ",
      label,
      ": ",
      path,
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.createPrivateExportStage <- function(final_file) {
  final_dir <- dirname(final_file)
  if (!dir.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(final_dir)) {
    stop("Failed to create output directory: ", final_dir, call. = FALSE)
  }

  stage <- tempfile(
    pattern = paste0(".", basename(final_file), "-stage-"),
    tmpdir = final_dir
  )
  if (!dir.create(stage, mode = "0700", showWarnings = FALSE)) {
    stop("Failed to create the export staging directory.", call. = FALSE)
  }
  tryCatch(
    .setExportArtifactMode(stage, "0700", "the export staging directory"),
    error = function(error) {
      unlink(stage, recursive = TRUE, force = TRUE)
      stop(error)
    }
  )
  stage
}

.exportSidecarName <- function(final_file, expression_matrix_mode) {
  suffix <- switch(
    expression_matrix_mode,
    h5 = ".h5",
    bpcells = ".bpcells",
    stop("Only external backends have a sidecar name.", call. = FALSE)
  )
  stem <- tools::file_path_sans_ext(basename(final_file))
  paste0(stem, suffix)
}

.validatePortableExportBasename <- function(final_file) {
  name <- basename(final_file)
  stem <- tools::file_path_sans_ext(name)
  windows_stem <- toupper(trimws(
    sub("\\..*$", "", name),
    which = "right"
  ))
  invalid_windows_name <-
    windows_stem %in%
    c("CON", "PRN", "AUX", "NUL") ||
    grepl("^(COM|LPT)[1-9]$", windows_stem)
  if (
    !nzchar(stem) ||
      name %in% c(".", "..") ||
      grepl("[[:cntrl:]<>:\"/\\\\|?*]", name) ||
      grepl("[. ]$", name) ||
      invalid_windows_name
  ) {
    stop(
      "External exports require a portable file name; reserved names, an empty ",
      "stem, control characters, and Windows-invalid punctuation are not ",
      "allowed: ",
      name,
      call. = FALSE
    )
  }
  invisible(name)
}

.validateExportSidecarName <- function(location) {
  if (
    !is.character(location) ||
      length(location) != 1L ||
      is.na(location) ||
      !nzchar(location) ||
      location %in% c(".", "..") ||
      grepl("[/\\\\]", location)
  ) {
    stop(
      "The expression sidecar location must be one relative file name.",
      call. = FALSE
    )
  }
  invisible(location)
}

.readPublishedExportBackend <- function(final_file) {
  if (!file.exists(final_file) || dir.exists(final_file)) {
    return(NULL)
  }
  object <- tryCatch(readRDS(final_file), error = function(error) NULL)
  if (
    !is.environment(object) ||
      !any(grepl("^Cerebro", class(object))) ||
      !exists("expression_backend", envir = object, inherits = FALSE) ||
      bindingIsActive("expression_backend", object) ||
      isTRUE(rlang::env_binding_are_lazy(object, "expression_backend"))
  ) {
    return(NULL)
  }

  backend <- object[["expression_backend"]]
  if (
    !is.list(backend) ||
      !is.character(backend$type) ||
      length(backend$type) != 1L ||
      is.na(backend$type) ||
      !(backend$type %in% c("embedded", "h5", "bpcells"))
  ) {
    return(NULL)
  }
  if (identical(backend$type, "embedded")) {
    return(list(type = "embedded", location = NULL))
  }
  invalid_location <- tryCatch(
    {
      .validateExportSidecarName(backend$location)
      FALSE
    },
    error = function(error) TRUE
  )
  if (invalid_location) {
    return(NULL)
  }
  list(type = backend$type, location = backend$location)
}

.publishCerebroExport <- function(
  export,
  final_file,
  stage_dir,
  expression_matrix_mode
) {
  final_dir <- dirname(final_file)
  if (!dir.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(final_dir)) {
    stop("Failed to create output directory: ", final_dir, call. = FALSE)
  }

  stage_crb <- file.path(stage_dir, basename(final_file))
  backend <- export$getExpressionBackend()
  if (
    !is.list(backend) ||
      !is.character(backend$type) ||
      length(backend$type) != 1L ||
      is.na(backend$type) ||
      !identical(backend$type, expression_matrix_mode)
  ) {
    stop(
      "The staged expression backend does not match expression_matrix_mode.",
      call. = FALSE
    )
  }
  previous_backend <- .readPublishedExportBackend(final_file)
  stage_sidecar <- NULL
  final_sidecar <- NULL
  if (!identical(backend$type, "embedded")) {
    .validateExportSidecarName(backend$location)
    expected_location <- .exportSidecarName(final_file, backend$type)
    if (!identical(backend$location, expected_location)) {
      stop(
        "The staged expression backend must use the fixed sidecar name `",
        expected_location,
        "`.",
        call. = FALSE
      )
    }
    stage_sidecar <- file.path(stage_dir, backend$location)
    final_sidecar <- file.path(final_dir, backend$location)
    if (
      .pathIsSymbolicLink(final_sidecar) ||
        .pathIsSymbolicLink(final_file)
    ) {
      stop(
        "Refusing to replace a symbolic-link export target.",
        call. = FALSE
      )
    }
  } else if (.pathIsSymbolicLink(final_file)) {
    stop(
      "Refusing to replace a symbolic-link export target.",
      call. = FALSE
    )
  }

  old_crb_backup <- NULL
  old_sidecar_backup <- NULL
  retired_sidecar <- NULL
  if (
    !is.null(previous_backend) &&
      previous_backend$type %in% c("h5", "bpcells") &&
      (identical(backend$type, "embedded") ||
        !identical(previous_backend$location, backend$location)) &&
      identical(
        previous_backend$location,
        .exportSidecarName(final_file, previous_backend$type)
      )
  ) {
    retired_sidecar <- file.path(final_dir, previous_backend$location)
  }
  old_crb_mode <- if (file.exists(final_file)) {
    as.octmode(bitwAnd(as.integer(file.info(final_file)$mode), 511L))
  } else {
    as.octmode("0600")
  }
  installed_crb <- FALSE
  installed_sidecar <- FALSE
  committed <- FALSE
  on.exit(
    {
      if (!committed) {
        if (installed_crb && file.exists(final_file)) {
          unlink(final_file, force = TRUE)
        }
        if (!is.null(old_crb_backup) && file.exists(old_crb_backup)) {
          if (!file.rename(old_crb_backup, final_file)) {
            warning(
              "Export rollback could not restore the previous .crb from: ",
              old_crb_backup,
              call. = FALSE
            )
          }
        }
        if (
          installed_sidecar &&
            (file.exists(final_sidecar) || dir.exists(final_sidecar))
        ) {
          unlink(final_sidecar, recursive = TRUE, force = TRUE)
        }
        if (
          !is.null(old_sidecar_backup) &&
            (file.exists(old_sidecar_backup) ||
              dir.exists(old_sidecar_backup))
        ) {
          if (!file.rename(old_sidecar_backup, final_sidecar)) {
            warning(
              "Export rollback could not restore the previous expression ",
              "sidecar from: ",
              old_sidecar_backup,
              call. = FALSE
            )
          }
        }
      }
    },
    add = TRUE
  )

  if (!is.null(stage_sidecar)) {
    if (.pathIsSymbolicLink(stage_sidecar)) {
      stop(
        "Refusing to publish a symbolic-link staged expression sidecar: ",
        stage_sidecar,
        call. = FALSE
      )
    }
    if (!(file.exists(stage_sidecar) || dir.exists(stage_sidecar))) {
      stop(
        "The staged ",
        backend$type,
        " expression matrix is missing.",
        call. = FALSE
      )
    }
    if (identical(backend$type, "h5") && dir.exists(stage_sidecar)) {
      stop("The staged h5 sidecar must be a regular file.", call. = FALSE)
    }
    if (identical(backend$type, "bpcells") && !dir.exists(stage_sidecar)) {
      stop("The staged bpcells sidecar must be a directory.", call. = FALSE)
    }
    if (.pathIsSymbolicLink(final_sidecar)) {
      stop(
        "Refusing to replace a symbolic-link expression sidecar: ",
        final_sidecar,
        call. = FALSE
      )
    }
    if (file.exists(final_sidecar) || dir.exists(final_sidecar)) {
      owns_target <-
        !is.null(previous_backend) &&
        identical(previous_backend$type, backend$type) &&
        identical(previous_backend$location, backend$location)
      if (!owns_target) {
        stop(
          "Refusing to replace an existing expression sidecar that is not ",
          "owned by the published CRB: ",
          final_sidecar,
          call. = FALSE
        )
      }
      old_sidecar_backup <- tempfile(
        pattern = paste0(".", basename(final_sidecar), "-backup-"),
        tmpdir = final_dir
      )
      if (!file.rename(final_sidecar, old_sidecar_backup)) {
        stop(
          "Failed to preserve the previous expression sidecar.",
          call. = FALSE
        )
      }
    }
    sidecar_mode <- if (dir.exists(stage_sidecar)) "0700" else "0600"
    .setExportArtifactMode(
      stage_sidecar,
      sidecar_mode,
      "the staged expression sidecar"
    )
    if (!file.rename(stage_sidecar, final_sidecar)) {
      stop("Failed to install the staged expression sidecar.", call. = FALSE)
    }
    installed_sidecar <- TRUE

    ## A BPCells handle serialises its absolute directory. Reopen it only after
    ## the staged directory has reached its published path, then serialise the
    ## CRB. H5 stores no live handle and needs no equivalent step.
    if (identical(expression_matrix_mode, "bpcells")) {
      export$setExpression(
        BPCells::open_matrix_dir(dir = final_sidecar),
        backend = "external"
      )
    }
  }

  export <- .stripCerebroSourceReferences(export)
  saveRDS(export, stage_crb)
  if (!file.exists(stage_crb)) {
    stop("Failed to serialise the staged Cerebro object.", call. = FALSE)
  }
  .setExportArtifactMode(stage_crb, old_crb_mode, "the staged Cerebro object")

  if (file.exists(final_file)) {
    old_crb_backup <- tempfile(
      pattern = paste0(".", basename(final_file), "-backup-"),
      tmpdir = final_dir
    )
    if (!file.rename(final_file, old_crb_backup)) {
      stop("Failed to preserve the previous .crb file.", call. = FALSE)
    }
  }
  if (!file.rename(stage_crb, final_file)) {
    stop("Failed to publish the staged .crb file.", call. = FALSE)
  }
  installed_crb <- TRUE
  .setExportArtifactMode(
    final_file,
    old_crb_mode,
    "the published Cerebro object"
  )
  committed <- TRUE

  for (backup in c(old_crb_backup, old_sidecar_backup)) {
    if (
      !is.null(backup) &&
        (file.exists(backup) || dir.exists(backup))
    ) {
      status <- unlink(backup, recursive = TRUE, force = TRUE)
      if (!identical(status, 0L)) {
        warning(
          "The new export was published, but an old backup remains at: ",
          backup,
          call. = FALSE
        )
      }
    }
  }
  if (
    !is.null(retired_sidecar) &&
      (file.exists(retired_sidecar) || dir.exists(retired_sidecar))
  ) {
    if (.pathIsSymbolicLink(retired_sidecar)) {
      warning(
        "The previous sidecar is a symbolic link and was not removed: ",
        retired_sidecar,
        call. = FALSE
      )
    } else {
      status <- unlink(retired_sidecar, recursive = TRUE, force = TRUE)
      if (!identical(status, 0L)) {
        warning(
          "The new export was published, but the previous sidecar remains at: ",
          retired_sidecar,
          call. = FALSE
        )
      }
    }
  }
  invisible(final_file)
}

.spx_export_projection_coordinates <- function(coordinates) {
  if (!is.data.frame(coordinates)) {
    return(NULL)
  }
  coordinate_columns <- .spx_find_coordinate_columns(coordinates)
  if (is.null(coordinate_columns)) {
    return(NULL)
  }
  projection <- coordinates[,
    c(coordinate_columns$x, coordinate_columns$y),
    drop = FALSE
  ]
  names(projection) <- c("x", "y")
  projection
}

#' @title
#' Export Seurat object to Cerebro.
#'
#' @description
#' This function allows to export a Seurat object to visualize in Cerebro.
#'
#' @param object Seurat object.
#' @param assay Assay to pull expression values from; defaults to \code{RNA}.
#' @param slot Slot to pull expression values from; defaults to \code{data}. It
#' is recommended to use sparse data (such as log-transformed or raw counts)
#' instead of dense data (such as the \code{scaled} slot) to avoid performance
#' bottlenecks in the Cerebro interface.
#' @param file Where to save the output. External backends require a
#'   \code{.crb} filename and store the matrix under a sibling name derived
#'   from the stem.
#' @param experiment_name Experiment name.
#' @param organism Organism, e.g. \code{hg} (human), \code{mm} (mouse), etc.
#' @param groups Names of grouping variables in meta data
#' (\code{object@meta.data}), e.g. \code{c("sample","cluster")}; at least one
#' must be provided; defaults to \code{NULL}.
#' @param main_group The primary grouping variable to use for display in Cerebro;
#' must be one of the grouping variables specified in \code{groups}; defaults to
#' \code{NULL}.
#' @param cell_cycle Names of columns in meta data
#' (\code{object@meta.data}) that contain cell cycle information, e.g.
#' \code{c("Phase")}; defaults to \code{NULL}.
#' @param nUMI Column in \code{object@meta.data} that contains information about
#' number of transcripts per cell; defaults to \code{nUMI}.
#' @param nGene Column in \code{object@meta.data} that contains information
#' about number of expressed genes per cell; defaults to \code{nGene}.
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
#' \code{"embedded"} (default), \code{"bpcells"}, or \code{"h5"}.
#' \itemize{
#'   \item \code{"embedded"} stores the matrix inside the \code{.crb} file, as
#'   before. Compatible with all existing \code{.crb} readers.
#'   \item \code{"bpcells"} writes the matrix to a BPCells on-disk directory
#'   next to the \code{.crb} and keeps only a lightweight handle in the
#'   serialised object. Recommended for large sparse matrices. The resulting
#'   \code{.crb} is portable as long as the sibling \code{.bpcells/} directory
#'   travels with it; the Shiny runtime re-resolves paths via
#'   \code{getExpressionBackend()$location} relative to the \code{.crb}'s
#'   parent directory (step 7.3 runtime attach).
#'   \item \code{"h5"} writes the matrix via \code{HDF5Array::writeTENxMatrix()}
#'   to a TENx-format sparse HDF5 file next to the \code{.crb} (sibling
#'   \code{<stem>.h5}) and tags the backend with that relative location. The
#'   on-disk layout matches \code{inst/extdata/examples/example.h5}: a single
#'   \code{/expression} group with \code{data}, \code{indices}, \code{indptr},
#'   \code{shape}, \code{genes}, and \code{barcodes} datasets. The matrix is
#'   stored cells x genes (TENx column-favoured, optimised for per-gene
#'   reads); the Shiny runtime attach reads it back as a lazy
#'   \code{HDF5Array::TENxMatrix} seed and transposes it lazily to Cerebro's
#'   internal genes x cells layout via \code{DelayedArray::t()} (free). The
#'   in-memory \code{dgCMatrix} is never materialised on attach, so RAM stays
#'   close to the \code{.crb} metadata size. Requires the \pkg{HDF5Array}
#'   package.
#' }
#' The CRB and any sidecar are built and validated in a private sibling stage.
#' On POSIX systems, new stages and external matrices are owner-only; replacing
#' a CRB preserves its existing mode. An existing sidecar is replaced only when
#' the current CRB identifies that exact path as its backend; backend changes
#' remove the previous owned sidecar after the new CRB is committed. Stop all
#' readers before replacing an existing export, because a reader can otherwise
#' observe the two-path replacement between steps.
#' Ordinary R errors trigger best-effort restoration. Process termination and
#' concurrent writers remain outside this multi-path transaction guarantee.
#' Only POSIX mode bits are set or preserved; ownership, ACLs, extended
#' attributes, and security labels remain the deployment system's
#' responsibility on every platform.
#' @param spatial_images Optional named list mapping Seurat image names to named
#'   image paths or descriptors of the form \code{list(path = ..., bounds = ...)}.
#'   Supported file extensions are png, jpg, jpeg, and svg. Missing bounds are
#'   derived from the exported x/y coordinate range.
#' @param verbose Set this to \code{TRUE} if you want additional log messages;
#' defaults to \code{FALSE}.
#' @param .expression_resolution Internal handoff used by
#' \code{convertSeuratToCerebro()} to reuse a matrix that has already been
#' resolved and validated. Users should leave this as \code{NULL}.
#' @param projections Optional ordered names of dimensional reductions to
#' export. When supplied, every named reduction is exported in that order,
#' including PCA beside other projections. The default \code{NULL} preserves
#' the legacy behavior that uses non-PCA reductions when they are available.
#' @param spatial_coordinate_transforms Optional named list of coordinate
#'   transforms keyed by exact Seurat spatial FOV/image name. Each entry may
#'   specify \code{rotation_degrees} and positive uniform \code{scale}; the
#'   pivot is the coordinate bounds center. The transform is applied once to
#'   the exported spatial coordinates and its normalized provenance is stored
#'   in the corresponding CRB spatial record. \code{NULL} preserves
#'   coordinates exactly as extracted from Seurat.
#'
#' @section Immune Repertoire:
#' If \code{object@misc$immune_repertoire} contains a named list of
#' data.frames (one per sample, with \code{barcode}, \code{CTgene},
#' \code{CTnt}, \code{CTaa}, and \code{CTstrict}), it will be automatically
#' exported into the Cerebro object via \code{addImmuneRepertoire()}. Legacy
#' \code{bcr_data} /
#' \code{tcr_data} slots are also supported as a fallback.
#'
#' @section HLA typing:
#' If \code{object@misc$hla_typing} holds an HLA genotype table -- a canonical
#' long \code{data.frame}, a wide \code{sample} + \code{HLA-*_1/_2}
#' \code{data.frame}, or a named list (sample -> allele vector) -- it is
#' exported via \code{addHLATyping()}, parallel to the immune repertoire. The
#' provenance in \code{object@misc$hla_typing_source_type} (one of
#' \code{"genotyped"}, \code{"imputed"}, \code{"synthetic"}, \code{"unknown"};
#' default \code{"unknown"}) is carried through, so a predicted or fabricated
#' genotype is never mistaken for a directly typed one.
#'
#' @return
#' No data returned.
#'
#' @examples
#' pbmc <- readRDS(system.file("extdata/examples/pbmc_seurat.rds",
#'   package = "CerebroNexus"))
#' exportFromSeurat(
#'   object = pbmc,
#'   file = file.path(tempdir(), 'pbmc_Seurat.crb'),
#'   experiment_name = 'PBMC',
#'   organism = 'hg',
#'   groups = c('sample','seurat_clusters'),
#'   nUMI = 'nCount_RNA',
#'   nGene = 'nFeature_RNA',
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
exportFromSeurat <- function(
  object,
  assay = 'RNA',
  slot = 'data',
  file,
  experiment_name,
  organism,
  groups,
  main_group = NULL,
  cell_cycle = NULL,
  nUMI = 'nUMI',
  nGene = 'nGene',
  add_all_meta_data = TRUE,
  use_delayed_array = FALSE,
  expression_matrix_mode = c("embedded", "bpcells", "h5"),
  spatial_images = NULL,
  verbose = FALSE,
  .expression_resolution = NULL,
  projections = NULL,
  spatial_coordinate_transforms = NULL
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  expression_matrix_mode <- match.arg(expression_matrix_mode)
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file)
  ) {
    stop("`file` must be one non-empty output path.", call. = FALSE)
  }
  if (dir.exists(file)) {
    stop("`file` points to a directory, not a .crb path: ", file, call. = FALSE)
  }
  if (!identical(expression_matrix_mode, "embedded")) {
    if (!identical(tolower(tools::file_ext(file)), "crb")) {
      stop(
        "External expression backends require `file` to end in .crb so the ",
        "Cerebro object and its sidecar have distinct portable names. Received: ",
        file,
        call. = FALSE
      )
    }
    .validatePortableExportBasename(file)
  }
  if (
    expression_matrix_mode == "h5" &&
      !requireNamespace("HDF5Array", quietly = TRUE)
  ) {
    stop(
      "expression_matrix_mode = \"h5\" requires the HDF5Array package. ",
      "Install it via BiocManager::install(\"HDF5Array\") and re-run, or ",
      "switch to expression_matrix_mode = \"bpcells\" / \"embedded\".",
      call. = FALSE
    )
  }
  if (
    expression_matrix_mode == "bpcells" &&
      !requireNamespace("BPCells", quietly = TRUE)
  ) {
    stop(
      "expression_matrix_mode = \"bpcells\" requires the BPCells package. ",
      "Install it and re-run, or switch to expression_matrix_mode = \"embedded\".",
      call. = FALSE
    )
  }
  if (expression_matrix_mode != "embedded" && use_delayed_array) {
    if (verbose) {
      message(
        "expression_matrix_mode = \"",
        expression_matrix_mode,
        "\" supersedes use_delayed_array; the RleArray conversion is skipped."
      )
    }
  }

  ## check if Seurat is installed
  if (!requireNamespace("Seurat", quietly = TRUE)) {
    stop(
      "The 'Seurat' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  ## Check Seurat package version using compareVersion
  seurat_version <- as.character(utils::packageVersion("Seurat"))
  if (utils::compareVersion(seurat_version, "3.0.0") < 0) {
    stop(
      paste0(
        "The installed Seurat package is of version `",
        seurat_version,
        "`, but at least v3.0 is required."
      ),
      call. = FALSE
    )
  }

  ## check if provided object is of class "Seurat"
  if (!inherits(object, "Seurat")) {
    stop(
      paste0(
        "Provided object is of class `",
        paste(class(object), collapse = ", "),
        "` but must be of class 'Seurat'."
      ),
      call. = FALSE
    )
  }

  ## check version of Seurat object and stop if it is lower than 3
  obj_version <- as.character(object@version)
  if (utils::compareVersion(obj_version, "3.0.0") < 0) {
    stop(
      paste0(
        "Provided Seurat object has version `",
        obj_version,
        "` but must be at least 3.0."
      ),
      call. = FALSE
    )
  }

  ## `groups`
  if (any(groups %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Some group columns could not be found in meta data: ',
        paste0(
          groups[which(groups %in% names(object@meta.data) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## `main_group`
  if (!is.null(main_group) && !(main_group %in% groups)) {
    stop(
      paste0(
        'Specified main_group `',
        main_group,
        '` is not in the list of groups. ',
        'Valid options are: ',
        paste(groups, collapse = ', ')
      ),
      call. = FALSE
    )
  }

  if (
    !is.null(projections) &&
      (!is.character(projections) ||
        !length(projections) ||
        anyNA(projections) ||
        any(!nzchar(projections)) ||
        anyDuplicated(projections) ||
        any(!projections %in% names(object@reductions)))
  ) {
    stop(
      "`projections` must name unique dimensional reductions in the object.",
      call. = FALSE
    )
  }

  ## `nUMI`
  if ((nUMI %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Column with number of transcripts per cell (`',
        nUMI,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `nGene`
  if ((nGene %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Column with number of expressed genes per cell (`',
        nGene,
        '`) not found in meta data.'
      ),
      call. = FALSE
    )
  }

  ## `cell_cycle`
  if (any(cell_cycle %in% names(object@meta.data) == FALSE)) {
    stop(
      paste0(
        'Some cell cycle columns could not be found in meta data: ',
        paste0(
          cell_cycle[which(cell_cycle %in% names(object@meta.data) == FALSE)],
          collapse = ', '
        )
      ),
      call. = FALSE
    )
  }

  ## check if provided assay exists
  if ((assay %in% names(object@assays) == FALSE)) {
    stop(
      paste0(
        'Specified assay `',
        assay,
        '` could not be found in provided Seurat ',
        'object.'
      ),
      call. = FALSE
    )
  }

  ## Stage every output artefact beside the final destination. External
  ## matrices can be expensive to write, but no published .crb or sidecar is
  ## touched until the complete object has passed every later validation.
  final_file <- file
  final_dir <- dirname(final_file)
  if (!dir.exists(final_dir)) {
    dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(final_dir)) {
    stop("Failed to create output directory: ", final_dir, call. = FALSE)
  }
  export_stage_dir <- .createPrivateExportStage(final_file)
  on.exit(
    unlink(export_stage_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## initialize Cerebro object
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Initializing Cerebro object...'
      )
    )
  } else {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Start collecting data...'
      )
    )
  }

  ## create new Cerebro object
  export <- Cerebro$new()

  ## add experiment name
  export$addExperiment('experiment_name', experiment_name)

  ## add organism
  export$addExperiment('organism', organism)

  ## add cerebroApp version
  export$setVersion(utils::packageVersion('CerebroNexus'))

  ##--------------------------------------------------------------------------##
  ## add transcript counts
  ##--------------------------------------------------------------------------##

  ## get expression data using shared utility function. This is a top-level,
  ## user-facing export entry point whose job is to get the object out to a
  ## .crb, so it opts into legacy cross-semantic layer fallback (e.g. a
  ## Seurat v5 RNA assay with only a `counts` layer, requested at the default
  ## slot = "data"). Without this the export would hard-stop here — before even
  ## reaching the spatial block — on objects that exported fine on master. The
  ## fallback itself warns, so the substitution is never silent.
  ## Split (layered) Seurat v5 objects are joined first, matching
  ## `convertSeuratToCerebro()`. Without it only the first sample's layer is
  ## read: the meta data still describes every cell while the matrix describes
  ## one sample, and the two are never compared again in the `h5` and `bpcells`
  ## modes. `JoinLayers()` materialises the merged matrix, so a very large split
  ## object pays one memory peak here -- the price of exporting all of it.
  if (is.null(.expression_resolution)) {
    expression_resolution <- .getExpressionMatrix(
      seurat = object,
      assay = assay,
      slot = slot,
      join_samples = TRUE,
      allow_cross_semantic_fallback = TRUE,
      verbose = verbose,
      return_resolution = TRUE
    )
  } else {
    required_resolution_fields <- c(
      "data",
      "assay",
      "requested",
      "resolved",
      "joined",
      "fallback"
    )
    if (
      !is.list(.expression_resolution) ||
        !all(required_resolution_fields %in% names(.expression_resolution)) ||
        !identical(.expression_resolution$assay, as.character(assay)) ||
        !identical(.expression_resolution$requested, as.character(slot))
    ) {
      stop(
        "`.expression_resolution` must be the validated resolution for the ",
        "requested `assay` and `slot`. It is an internal conversion handoff; ",
        "users should leave it as NULL.",
        call. = FALSE
      )
    }
    expression_resolution <- .expression_resolution
  }

  ## Backend-independent guard. Validate once before the storage modes diverge,
  ## and retain both the requested and physically resolved layer in diagnostics.
  object_cells <- Seurat::Cells(object)
  expression_data <- .validate_expression_cells(
    expression_data = expression_resolution$data,
    object_cells = object_cells,
    assay = assay,
    requested_layer = expression_resolution$requested,
    resolved_layer = expression_resolution$resolved
  )

  if (expression_matrix_mode == "embedded") {
    ## convert expression data to "RleArray" if requested, if it is "dgCMatrix" or
    ## "matrix" format, and if the "DelayedArray" package is available
    if (
      use_delayed_array == TRUE &&
        inherits(expression_data, c('matrix', 'dgCMatrix')) &&
        requireNamespace("DelayedArray", quietly = TRUE)
    ) {
      if (verbose) {
        message(
          paste0(
            '[',
            format(Sys.time(), '%H:%M:%S'),
            '] Storing expression data as ',
            'DelayedArray...'
          )
        )
      }
      requireNamespace("DelayedArray", quietly = TRUE)
      expression_data <- methods::as(expression_data, "RleArray")
    }

    ## add expression data
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Adding expression data (embedded)...'
      )
    )
    export$setExpression(expression_data)
  } else if (expression_matrix_mode == "bpcells") {
    ## Write the expression matrix to a BPCells on-disk directory sitting next
    ## to the target .crb. Keep a BPCells IterableMatrix handle on the object
    ## so that the in-place session (crb + sibling .bpcells dir on the same
    ## machine, same paths) can use it immediately. Step 7.3's runtime attach
    ## will additionally re-resolve the relative location when the crb has
    ## been moved to a different machine or layout.
    crb_dir <- export_stage_dir
    bpc_dirname <- .exportSidecarName(final_file, expression_matrix_mode)
    bpc_abs <- file.path(crb_dir, bpc_dirname)

    ## BPCells writes an error if the directory already exists; clean first
    ## so the exporter is idempotent.
    if (dir.exists(bpc_abs)) {
      unlink(bpc_abs, recursive = TRUE)
    }

    ## Sparse dgCMatrix is BPCells' native input; dense matrices have to be
    ## coerced once. Everything else (RleMatrix, DelayedMatrix) is rare enough
    ## here that we cover it defensively.
    if (!inherits(expression_data, "dgCMatrix")) {
      if (inherits(expression_data, "matrix")) {
        expression_data <- methods::as(expression_data, "CsparseMatrix")
      } else if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
        expression_data <- methods::as(
          as.matrix(expression_data),
          "CsparseMatrix"
        )
      }
    }

    ## BPCells's on-disk format only bit-packs when the matrix's storage
    ## type is integer. dgCMatrix always stores values as double, even when
    ## every nonzero is an integer count (the typical scRNA-seq case), so
    ## we explicitly convert to "uint32_t" when the values are losslessly
    ## representable as non-negative integers. This shrinks the BPCells
    ## sibling ~5x on integer counts (e.g. 50k cells x 20k genes: 440 MB
    ## raw double -> 78 MB bit-packed). Normalised data (slot = "data" or
    ## "scale.data") stays as double — bit-packing would silently truncate.
    nnz_int_ok <- length(expression_data@x) > 0L &&
      all(expression_data@x >= 0) &&
      all(expression_data@x == as.integer(expression_data@x)) &&
      all(expression_data@x <= .Machine$integer.max)
    bpc_iter <- methods::as(expression_data, "IterableMatrix")
    if (nnz_int_ok) {
      bpc_iter <- BPCells::convert_matrix_type(bpc_iter, type = "uint32_t")
      bpc_storage_msg <- "uint32_t (bit-packed)"
    } else {
      bpc_storage_msg <- "double (raw, non-integer values detected)"
    }

    if (verbose) {
      message(sprintf(
        "[%s] Writing expression matrix to BPCells directory: %s [%s]",
        format(Sys.time(), "%H:%M:%S"),
        bpc_abs,
        bpc_storage_msg
      ))
    }
    BPCells::write_matrix_dir(mat = bpc_iter, dir = bpc_abs)
    mat_handle <- BPCells::open_matrix_dir(dir = bpc_abs)

    ## Carry the live handle (absolute path inside @dir -- BPCells normalises
    ## it on open_matrix_dir()) AND the portable relative location tag. Step
    ## 7.3's attach reads the tag, not @dir, so the crb stays portable.
    export$setExpression(mat_handle, backend = "external")
    export$setExpressionBackend(type = "bpcells", location = bpc_dirname)
  } else if (expression_matrix_mode == "h5") {
    ## Write the expression matrix to a TENxMatrix-format sparse HDF5 file
    ## sitting next to the target .crb. The on-disk orientation is cells x
    ## genes — TENx CSC stores columns contiguously, so the per-gene reads
    ## that Cerebro does at runtime become single-column lookups. Cerebro's
    ## internal layout is genes x cells, so the runtime attach lazily
    ## transposes the TENxMatrix seed back via DelayedArray::t() (free).
    crb_dir <- export_stage_dir
    h5_filename <- .exportSidecarName(final_file, expression_matrix_mode)
    h5_abs <- file.path(crb_dir, h5_filename)

    if (!inherits(expression_data, "dgCMatrix")) {
      if (inherits(expression_data, "matrix")) {
        expression_data <- methods::as(expression_data, "CsparseMatrix")
      } else if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
        expression_data <- methods::as(
          as.matrix(expression_data),
          "CsparseMatrix"
        )
      }
    }

    ## transpose genes x cells -> cells x genes for storage
    m_disk <- methods::as(Matrix::t(expression_data), "CsparseMatrix")

    if (verbose) {
      message(sprintf(
        "[%s] Writing expression matrix to TENx HDF5 file: %s",
        format(Sys.time(), "%H:%M:%S"),
        h5_abs
      ))
    }

    if (file.exists(h5_abs)) {
      file.remove(h5_abs)
    }
    HDF5Array::writeTENxMatrix(m_disk, h5_abs, group = "expression")

    ## self$expression stays NULL — saveRDS therefore does not embed the
    ## matrix inside the .crb. The runtime attach reads the sibling back
    ## as a lazy TENxMatrix seed (no in-memory dgCMatrix materialisation).
    export$setExpressionBackend(type = "h5", location = h5_filename)
  }

  ##--------------------------------------------------------------------------##
  ## collect some more data if present
  ##--------------------------------------------------------------------------##

  ## date of analysis
  if (!is.null(object@misc$experiment$date_of_analysis)) {
    export$addExperiment(
      'date_of_analysis',
      object@misc$experiment$date_of_analysis
    )
  }

  ## date of export
  export$addExperiment('date_of_export', Sys.Date())

  ## `parameters`
  if (!is.null(object@misc$parameters)) {
    for (i in seq_along(object@misc$parameters)) {
      name <- names(object@misc$parameters)[i]
      export$addParameters(
        name,
        object@misc$parameters[[name]]
      )
    }
  }

  ## `technical_info`
  if (!is.null(object@misc$technical_info)) {
    for (i in seq_along(object@misc$technical_info)) {
      export$addTechnicalInfo(
        names(object@misc$technical_info)[i],
        object@misc$technical_info[[i]]
      )
    }
  }

  ## `gene_lists`
  if (!is.null(object@misc$gene_lists)) {
    for (i in seq_along(object@misc$gene_lists)) {
      export$addGeneList(
        names(object@misc$gene_lists)[i],
        object@misc$gene_lists[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## prepare meta data
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Collecting available meta data...'
      )
    )
  }

  ## cell barcodes
  temp_meta_data <- data.frame(
    "cell_barcode" = Seurat::Cells(object),
    stringsAsFactors = FALSE
  )

  ##--------------------------------------------------------------------------##
  ## add grouping variables, factorize if necessary
  ##--------------------------------------------------------------------------##

  ## go through grouping variables
  for (i in groups) {
    ## check content of column in meta data
    ## ... content not factorized
    if (
      !is.factor(object@meta.data[[i]]) &&
        is.character(object@meta.data[[i]])
    ) {
      ## get all values and unique values (sorted, which removes NA)
      values <- object@meta.data[[i]]
      levels <- sort(unique(values), na.last = NA)

      ## check if there are NA values; if so, change NA values to 'N/A' and add
      ## 'N/A' to levels
      if (any(is.na(values))) {
        values[is.na(values)] <- 'N/A'
        levels <- c(levels, 'N/A')
      }

      ## factorize values
      temp_meta_data[[i]] <- factor(values, levels = levels)

      ## ... content is factorized but there are NA values and NA is not among the
      ##     factor levels
    } else if (
      is.factor(object@meta.data[[i]]) &&
        any(is.na(object@meta.data[[i]])) &&
        'NA' %in% levels(object@meta.data[[i]]) == FALSE
    ) {
      ## print log message
      if (verbose) {
        message(
          glue::glue(
            '[{format(Sys.time(), "%H:%M:%S")}] Adding `NA` to factor levels ',
            'of group `{i}`...'
          )
        )
      }

      ## add 'N/A' to factor levels for NA values
      levels <- levels(object@meta.data[[i]])
      values <- as.character(object@meta.data[[i]])
      values[is.na(values)] <- 'N/A'
      values <- factor(values, levels = c(levels, 'N/A'))
      temp_meta_data[[i]] <- values

      ## ... none of the above
    } else {
      ## copy content to meta data
      temp_meta_data[[i]] <- object@meta.data[[i]]
    }
  }

  ## number of transcripts and expressed genes
  temp_meta_data[["nUMI"]] <- object@meta.data[[nUMI]]
  temp_meta_data[["nGene"]] <- object@meta.data[[nGene]]

  ## rest of meta data
  meta_data_columns <- names(object@meta.data)
  meta_data_columns <- meta_data_columns[-which(meta_data_columns %in% groups)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == nUMI)]
  meta_data_columns <- meta_data_columns[-which(meta_data_columns == nGene)]

  ##--------------------------------------------------------------------------##
  ## cell cycle
  ##--------------------------------------------------------------------------##
  if (
    !is.null(cell_cycle) &&
      length(cell_cycle) > 0
  ) {
    for (i in cell_cycle) {
      if (is.factor(object@meta.data[[i]])) {
        tmp_names <- levels(object@meta.data[[i]])
      } else {
        tmp_names <- unique(object@meta.data[[i]])
      }
      # colData(export$expression)[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
      temp_meta_data[[i]] <- factor(object@meta.data[[i]], levels = tmp_names)
    }
    meta_data_columns <- meta_data_columns[
      -which(meta_data_columns %in% cell_cycle)
    ]
  }

  ##--------------------------------------------------------------------------##
  ## add all other meta data if specified
  ##--------------------------------------------------------------------------##
  if (add_all_meta_data == TRUE) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting all meta data columns...'
        )
      )
    }
    for (i in meta_data_columns) {
      # colData(export$expression)[[i]] <- object@meta.data[[i]]
      temp_meta_data[[i]] <- object@meta.data[[i]]
    }
  }

  ## make column names in meta data unique (if necessary)
  # colnames(colData(export$expression)) <- make.unique(colnames(colData(export$expression)))
  colnames(temp_meta_data) <- make.unique(colnames(temp_meta_data))

  ##--------------------------------------------------------------------------##
  ## add meta data
  ##--------------------------------------------------------------------------##
  export$setMetaData(temp_meta_data)

  ##--------------------------------------------------------------------------##
  ## add grouping variables and cell cycle columns
  ##--------------------------------------------------------------------------##
  for (i in groups) {
    export$addGroup(i, levels(temp_meta_data[[i]]))
  }

  ## set main group if specified
  if (!is.null(main_group)) {
    export$addParameters('main_group', main_group)
  }

  if (
    !is.null(cell_cycle) &&
      length(cell_cycle) > 0
  ) {
    export$setCellCycle(cell_cycle)
  }

  ##--------------------------------------------------------------------------##
  ## projections
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Extracting dimensional reductions...'
      )
    )
  }
  explicit_projections <- !is.null(projections)
  projections_available <- if (explicit_projections) {
    projections
  } else {
    names(object@reductions)
  }
  projections_available_pca <- projections_available[grep(
    projections_available,
    pattern = 'pca',
    ignore.case = TRUE,
    invert = FALSE
  )]
  projections_available_non_pca <- projections_available[grep(
    projections_available,
    pattern = 'pca',
    ignore.case = TRUE,
    invert = TRUE
  )]
  if (length(projections_available) == 0) {
    stop('No dimensional reductions available.', call. = FALSE)
  } else if (explicit_projections) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Will export the following dimensional reductions: ',
          paste(projections_available, collapse = ', ')
        )
      )
    }
    for (projection in projections_available) {
      export$addProjection(
        projection,
        as.data.frame(object@reductions[[projection]]@cell.embeddings)
      )
    }
  } else if (
    length(projections_available) == 1 &&
      length(projections_available_pca) == 1
  ) {
    # SingleCellExperiment::reducedDims(export$expression)[[projections_available]] <- as.data.frame(
    #   object@reductions[[projections_available]]@cell.embeddings
    # )
    export$addProjection(
      projections_available,
      as.data.frame(object@reductions[[projections_available]]@cell.embeddings)
    )
    warning(
      paste0(
        'Warning: Only PCA as dimensional reduction found, will export ',
        'first and second principal components. Consider using tSNE and/or ',
        'UMAP instead.'
      )
    )
  } else if (length(projections_available_non_pca) >= 1) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] ',
          'Will export the following dimensional reductions: ',
          paste(projections_available_non_pca, collapse = ', ')
        )
      )
    }
    for (projection in projections_available_non_pca) {
      # SingleCellExperiment::reducedDims(export$expression)[[projection]] <- as.data.frame(
      #   object@reductions[[projection]]@cell.embeddings
      # )
      export$addProjection(
        projection,
        as.data.frame(object@reductions[[projection]]@cell.embeddings)
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## group trees
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$trees)) {
    ## check if it's a list
    if (!is.list(object@misc$trees)) {
      stop(
        '`object@misc$trees` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting trees...'
        )
      )
    }
    for (i in seq_along(object@misc$trees)) {
      export$addTree(
        names(object@misc$trees)[i],
        object@misc$trees[[i]]
      )
    }
  }

  ##--------------------------------------------------------------------------##
  ## most expressed genes
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$most_expressed_genes)) {
    ## check if it's a list
    if (!is.list(object@misc$most_expressed_genes)) {
      stop(
        '`object@misc$most_expressed_genes` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of most expressed genes...'
        )
      )
    }

    for (i in seq_along(object@misc$most_expressed_genes)) {
      group <- names(object@misc$most_expressed_genes)[i]
      if (group %in% groups) {
        export$addMostExpressedGenes(
          group,
          object@misc$most_expressed_genes[[i]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## mean expression
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$mean_expression)) {
    ## check if it's a list
    if (!is.list(object@misc$mean_expression)) {
      stop(
        '`object@misc$mean_expression` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of mean expression...'
        )
      )
    }

    for (i in seq_along(object@misc$mean_expression)) {
      group <- names(object@misc$mean_expression)[i]
      if (group %in% groups) {
        export$addMeanExpression(
          group,
          object@misc$mean_expression[[i]]
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## Immune repertoire data (unified)
  ##--------------------------------------------------------------------------##
  ## `is.list()` was the whole guard, and a data.frame passes it -- its columns
  ## became the sample names and the wrong shape reached the .crb intact.
  unified_repertoire <- .dropEmptyRepertoireSamples(
    object@misc$immune_repertoire
  )
  has_unified_repertoire <-
    !is.null(unified_repertoire) &&
    length(unified_repertoire) > 0
  if (has_unified_repertoire) {
    unified_repertoire <- .normalizeImmuneRepertoire(
      unified_repertoire,
      cell_barcodes = Seurat::Cells(object),
      source_label = "`@misc$immune_repertoire`"
    )
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting immune repertoire data (',
          length(unified_repertoire),
          ' samples)...'
        )
      )
    }
    export$addImmuneRepertoire(unified_repertoire)
  }

  ##--------------------------------------------------------------------------##
  ## HLA typing (optional; parallel to immune_repertoire)
  ##--------------------------------------------------------------------------##
  ## Accepts a canonical long data.frame, a wide sample x locus table, or a
  ## named list (sample -> allele vector). Provenance is read from an optional
  ## `object@misc$hla_typing_source_type` (default "unknown") so an uploaded or
  ## imputed genotype is never silently treated as directly typed.
  if (
    !is.null(object@misc$hla_typing) &&
      (is.data.frame(object@misc$hla_typing) ||
        (is.list(object@misc$hla_typing) &&
          length(object@misc$hla_typing) > 0))
  ) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting HLA typing...'
        )
      )
    }
    st <- object@misc$hla_typing_source_type
    if (is.null(st) || !nzchar(st)) {
      st <- 'unknown'
    }
    export$addHLATyping(object@misc$hla_typing, source_type = st)
  }

  ##--------------------------------------------------------------------------##
  ## BCR data (legacy)
  ##--------------------------------------------------------------------------##
  if (!has_unified_repertoire && !is.null(object@misc$bcr_data)) {
    bcr_data <- .dropEmptyRepertoireSamples(object@misc$bcr_data)
    ## Same check as the unified slot: `getImmuneRepertoire()` falls back to
    ## merging the legacy slots, so leaving them unchecked would just move the
    ## way around it rather than close it.
    bcr_data <- .normalizeImmuneRepertoire(
      bcr_data,
      cell_barcodes = Seurat::Cells(object),
      source_label = "`@misc$bcr_data`"
    )
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of BCR data...'
        )
      )
    }
    if (length(bcr_data) > 0L) {
      export$addBCRData(bcr_data)
    }
  }

  ##--------------------------------------------------------------------------##
  ## TCR data (legacy)
  ##--------------------------------------------------------------------------##
  if (!has_unified_repertoire && !is.null(object@misc$tcr_data)) {
    tcr_data <- .dropEmptyRepertoireSamples(object@misc$tcr_data)
    tcr_data <- .normalizeImmuneRepertoire(
      tcr_data,
      cell_barcodes = Seurat::Cells(object),
      source_label = "`@misc$tcr_data`"
    )
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting tables of TCR data...'
        )
      )
    }
    if (length(tcr_data) > 0L) {
      export$addTCRData(tcr_data)
    }
  }

  ## R6 methods are serialized into the .crb, so changing the class getter does
  ## not repair files that were already written. For new exports that only use
  ## the two legacy slots, populate the unified field that the serialized getter
  ## already prefers. Keep the legacy fields above for getBCR()/getTCR().
  if (!has_unified_repertoire) {
    legacy_repertoire <- .mergeRepertoiresBySample(
      if (exists("tcr_data", inherits = FALSE)) tcr_data else NULL,
      if (exists("bcr_data", inherits = FALSE)) bcr_data else NULL
    )
    legacy_repertoire <- .dropEmptyRepertoireSamples(legacy_repertoire)
    if (!is.null(legacy_repertoire) && length(legacy_repertoire) > 0) {
      legacy_repertoire <- .normalizeImmuneRepertoire(
        legacy_repertoire,
        cell_barcodes = Seurat::Cells(object),
        source_label = "the merged legacy immune repertoire"
      )
      export$addImmuneRepertoire(legacy_repertoire)
    }
  }

  ##--------------------------------------------------------------------------##
  ## marker genes
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$marker_genes)) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting marker genes table...'
        )
      )
    }
    ## marker_genes is a nested list: list(method = list(group = data.frame))
    ## (existing shiny consumers depend on the nested layout; the
    ## flat-data.frame simplification is deferred until H6 lands).
    if (!is.list(object@misc$marker_genes)) {
      stop('`object@misc$marker_genes` is not a list.', call. = FALSE)
    }
    for (i in seq_along(object@misc$marker_genes)) {
      method <- names(object@misc$marker_genes)[i]
      for (j in seq_along(object@misc$marker_genes[[method]])) {
        if (is.list(object@misc$marker_genes[[method]][j])) {
          group <- names(object@misc$marker_genes[[method]])[j]
          export$addMarkerGenes(
            method,
            group,
            object@misc$marker_genes[[method]][[group]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## enriched pathways
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$enriched_pathways)) {
    ## check if it's a list
    if (!is.list(object@misc$enriched_pathways)) {
      stop(
        '`object@misc$enriched_pathways` is not a list.',
        call. = FALSE
      )
    }
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] Extracting pathway enrichment results...'
        )
      )
    }
    ## for each method
    for (i in seq_along(object@misc$enriched_pathways)) {
      method <- names(object@misc$enriched_pathways)[i]
      ## for each group
      for (j in seq_along(object@misc$enriched_pathways[[method]])) {
        if (is.list(object@misc$enriched_pathways[[method]][j])) {
          group <- names(object@misc$enriched_pathways[[method]])[j]

          ## only add enriched pathways if group is present in `groups`
          if (group %in% groups) {
            export$addEnrichedPathways(
              method,
              group,
              object@misc$enriched_pathways[[method]][[group]]
            )
          }
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## trajectories
  ##--------------------------------------------------------------------------##
  if (length(object@misc$trajectories) == 0) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] No trajectories to extract...'
        )
      )
    }
  } else {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] ',
          # 'Extracting trajectories...'
          'Will export the following trajectories: ',
          paste(names(object@misc$trajectories$monocle2), collapse = ', ')
        )
      )
    }
    ## for each method
    for (i in seq_along(object@misc$trajectories)) {
      method <- names(object@misc$trajectories)[i]
      if (method == 'monocle2') {
        ## for each trajectory
        for (j in seq_along(object@misc$trajectories[[i]])) {
          export$addTrajectory(
            method,
            names(object@misc$trajectories[[i]])[j],
            object@misc$trajectories[[i]][[j]]
          )
        }
      } else {
        warning(
          paste0(
            'Warning: Skipping trajectories of method `',
            method,
            '`. At the ',
            'moment, only trajectories generated with Monocle 2 (`monocle2`) ',
            'are supported.'
          )
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## extra material
  ##
  ## currently, only tables can be exported
  ##--------------------------------------------------------------------------##

  ## define valid categories
  valid_categories <- c('tables')

  ## check of extra material exists, that it is in list format, and that the
  ## list is not empty
  if (
    !is.null(object@misc$extra_material) &&
      is.list(object@misc$extra_material) &&
      length(object@misc$extra_material) > 0
  ) {
    if (verbose) {
      message(
        glue::glue(
          '[{format(Sys.time(), "%H:%M:%S")}] Found extra material to export...'
        )
      )
    }

    ## go through categories in `extra_material` slot
    for (category in names(object@misc$extra_material)) {
      ## do this if category is `tables`
      if (category == 'tables') {
        ## go through tables
        for (i in seq_along(object@misc$extra_material$tables)) {
          ## export table
          export$addExtraMaterial(
            category = 'tables',
            name = names(object@misc$extra_material$tables)[i],
            content = object@misc$extra_material$tables[[i]]
          )
        }

        ## do this if category is `plots`
      } else if (category == 'plots') {
        ## go through tables
        for (i in seq_along(object@misc$extra_material$plots)) {
          ## export table
          export$addExtraMaterial(
            category = 'plots',
            name = names(object@misc$extra_material$plots)[i],
            content = object@misc$extra_material$plots[[i]]
          )
        }
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## Trekker single-cell spatial mapping
  ##--------------------------------------------------------------------------##
  if (!is.null(object@misc$trekker)) {
    export$addTrekker(object@misc$trekker)
  }

  ##--------------------------------------------------------------------------##
  ## spatial data
  ##--------------------------------------------------------------------------##
  if (verbose) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Checking for spatial data...'
      )
    )
  }

  ## Spatial images live in the `@images` slot on Seurat v3+ objects, so this
  ## path handles both Seurat v4 (VisiumV1, SlideSeq) and v5 (VisiumV2, FOV)
  ## objects. `.getSpatialData()` resolves coordinates version-agnostically via
  ## GetTissueCoordinates + S4 slot access, so no version branch is needed here.
  has_images <- .spx_has_slot(object, "images") &&
    !is.null(object@images) &&
    length(object@images) > 0
  spatial_names <- if (has_images) names(object@images) else character()
  misc_spatial_images <- .validateCerebroSpatialImages(
    object@misc$cerebro_spatial_images,
    spatial_names
  )
  path_spatial_images <- .normalizeSpatialImagePaths(
    spatial_images,
    spatial_names,
    "`spatial_images`"
  )
  .mergeSpatialImageDeclarations(misc_spatial_images, path_spatial_images)

  ## Validate every requested transform before the per-FOV extraction loop.
  ## That loop deliberately catches extraction failures to preserve a useful
  ## export with warnings; malformed caller input must instead fail fast and
  ## never be mistaken for a bad individual FOV.
  spatial_coordinate_transform_specs <- list()
  ## Builder's canonical no-op state is an empty list. Treat it exactly like
  ## NULL at this public boundary so direct callers can use the same value.
  if (
    is.list(spatial_coordinate_transforms) &&
      !is.object(spatial_coordinate_transforms) &&
      !length(spatial_coordinate_transforms)
  ) {
    spatial_coordinate_transforms <- NULL
  }
  if (!is.null(spatial_coordinate_transforms)) {
    transform_names <- names(spatial_coordinate_transforms)
    if (
      !is.list(spatial_coordinate_transforms) ||
        is.object(spatial_coordinate_transforms) ||
        is.null(transform_names) ||
        !is.character(transform_names) ||
        is.object(transform_names) ||
        anyNA(transform_names) ||
        any(!nzchar(transform_names)) ||
        anyDuplicated(transform_names)
    ) {
      stop(
        "`spatial_coordinate_transforms` must be an ordinary named list ",
        "with unique non-blank FOV names.",
        call. = FALSE
      )
    }
    unknown_transforms <- setdiff(transform_names, spatial_names)
    if (length(unknown_transforms)) {
      stop(
        "`spatial_coordinate_transforms` contains unknown FOV(s): ",
        paste(unknown_transforms, collapse = ", "),
        call. = FALSE
      )
    }
    spatial_coordinate_transform_specs <- lapply(
      transform_names,
      function(image_name) {
        .spx_coordinate_transform_spec_normalize(
          spatial_coordinate_transforms[[image_name]],
          context = paste0("spatial_coordinate_transforms$", image_name)
        )
      }
    )
    names(spatial_coordinate_transform_specs) <- transform_names
  }

  if (has_images) {
    if (verbose) {
      message(
        paste0(
          '[',
          format(Sys.time(), '%H:%M:%S'),
          '] ',
          'Spatial data found. Extracting spatial coordinates...'
        )
      )
    }

    for (image_name in names(object@images)) {
      spatial_data <- tryCatch(
        {
          # Extract spatial data (coordinates + expression)
          # Using .getSpatialData helper which handles Visium, FOV/Xenium, etc.
          spatial_data <- .getSpatialData(
            object,
            image = image_name,
            layer = slot,
            assay = assay,
            expression_data = expression_data,
            expression_layer = expression_resolution$resolved
          )

          # Also add coordinates as a projection for compatibility with existing visualization functions
          coords_df <- spatial_data$coordinates

          projection <- .spx_export_projection_coordinates(coords_df)
          if (!is.null(projection)) {
            coords_df <- projection
            if (verbose) {
              message(paste0(
                '[',
                format(Sys.time(), '%H:%M:%S'),
                '] ',
                'Added spatial projection: ',
                image_name
              ))
            }
          }
          transform_spec <- spatial_coordinate_transform_specs[[image_name]]
          if (!is.null(transform_spec)) {
            source_coordinate_fingerprint <-
              .spx_coordinate_transform_fingerprint(coords_df)
            coordinate_transform <- .spx_coordinate_transform_normalize(
              spatial_coordinate_transforms[[image_name]],
              coords_df,
              context = paste0("spatial_coordinate_transforms$", image_name)
            )
            coords_df <- .spx_apply_coordinate_transform(
              coords_df,
              spatial_coordinate_transforms[[image_name]]
            )
            coordinate_transform$transformed_coordinate_fingerprint <-
              .spx_coordinate_transform_fingerprint(coords_df)
            coordinate_transform$source_coordinate_fingerprint <-
              source_coordinate_fingerprint
            spatial_data$coordinate_transform <- coordinate_transform
          }
          spatial_data$coordinates <- coords_df
          spatial_data
        },
        error = function(e) {
          if (!is.null(spatial_coordinate_transform_specs[[image_name]])) {
            stop(
              "Could not apply spatial coordinate transform for FOV `",
              image_name,
              "`: ",
              conditionMessage(e),
              call. = FALSE
            )
          }
          ## Never drop a spatial image silently: an object that clearly has
          ## `@images` but whose extraction fails (e.g. requested layer=slot is
          ## absent) would otherwise export "successfully" with no Spatial tab
          ## and no clue why. Always warn, regardless of `verbose`.
          warning(
            'Could not extract spatial data for image `',
            image_name,
            '` (layer = "',
            slot,
            '"): ',
            conditionMessage(e),
            call. = FALSE
          )
          NULL
        }
      )
      if (is.null(spatial_data)) {
        next
      }
      merged_images <- .mergeSpatialImageDeclarations(
        misc_spatial_images[image_name],
        path_spatial_images[image_name],
        setNames(list(spatial_data$coordinates), image_name)
      )
      spatial_data$histology_images <- merged_images[[image_name]]

      export$addSpatialData(image_name, spatial_data)

      if (verbose) {
        message(
          paste0(
            '[',
            format(Sys.time(), '%H:%M:%S'),
            '] ',
            'Added spatial data: ',
            image_name,
            ' (',
            nrow(spatial_data$coordinates),
            ' cells)'
          )
        )
      }
    }
  }

  ##--------------------------------------------------------------------------##
  ## show overview of Cerebro object
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '[',
      format(Sys.time(), '%H:%M:%S'),
      '] ',
      'Overview of Cerebro object:\n'
    )
  )

  ## print object
  export$print()

  ##--------------------------------------------------------------------------##
  ## save Cerebro object to disk
  ##--------------------------------------------------------------------------##

  ## log message
  message(
    paste0(
      '[',
      format(Sys.time(), '%H:%M:%S'),
      '] Saving Cerebro object to: ',
      final_file
    )
  )

  ## Replace the sidecar and .crb with best-effort rollback on ordinary errors.
  .publishCerebroExport(
    export = export,
    final_file = final_file,
    stage_dir = export_stage_dir,
    expression_matrix_mode = expression_matrix_mode
  )

  ## log message
  ## ... writing to file was successful
  if (file.exists(final_file)) {
    message(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Done!'
      )
    )
    ## ... target file doesn't exist
  } else {
    stop(
      paste0(
        '[',
        format(Sys.time(), '%H:%M:%S'),
        '] Something went wrong while ',
        'saving the file.'
      ),
      .call = FALSE
    )
  }
}
