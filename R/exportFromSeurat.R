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

## Not tools::file_path_sans_ext(), which keeps ".crb" whole, so its stem is
## never empty. Only R >= 4.6 rejects ".crb" earlier, because its
## tools::file_ext() no longer reads a leading dot as an extension.
.exportStem <- function(name) {
  sub("[.][[:alnum:]]+$", "", name)
}

.exportSidecarName <- function(final_file, expression_matrix_mode) {
  if (!identical(expression_matrix_mode, "h5")) {
    stop("Only external backends have a sidecar name.", call. = FALSE)
  }
  paste0(.exportStem(basename(final_file)), ".h5")
}

.validatePortableExportBasename <- function(final_file) {
  name <- basename(final_file)
  stem <- .exportStem(name)
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

## The CRB that an export is about to replace, read once for the three
## ownership checks below; NULL when there is none or it is not a Cerebro file.
.readPublishedObject <- function(final_file) {
  if (!file.exists(final_file) || dir.exists(final_file)) {
    return(NULL)
  }
  object <- tryCatch(.readCerebroPayload(final_file), error = function(error) {
    NULL
  })
  if (!is.environment(object) || !any(grepl("^Cerebro", class(object)))) {
    return(NULL)
  }
  object
}

.readPublishedExportBackend <- function(
  final_file,
  object = .readPublishedObject(final_file)
) {
  if (
    is.null(object) ||
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
      !(backend$type %in% .crbContract()$.runtimeBackendTypes)
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

.readPublishedSpatialBackend <- function(
  final_file,
  object = .readPublishedObject(final_file)
) {
  if (
    is.null(object) ||
      !exists("spatial_molecule_backend", envir = object, inherits = FALSE) ||
      bindingIsActive("spatial_molecule_backend", object) ||
      isTRUE(rlang::env_binding_are_lazy(object, "spatial_molecule_backend"))
  ) {
    return(NULL)
  }
  backend <- object[["spatial_molecule_backend"]]
  valid <- is.list(backend) &&
    identical(backend$type, "directory") &&
    is.character(backend$location) &&
    length(backend$location) == 1L &&
    !is.na(backend$location) &&
    nzchar(backend$location) &&
    !backend$location %in% c(".", "..") &&
    !grepl("[/\\\\]", backend$location)
  if (!valid) NULL else backend[c("type", "location")]
}

.readPublishedImmuneBackend <- function(
  final_file,
  object = .readPublishedObject(final_file)
) {
  if (
    is.null(object) ||
      !exists("immune_repertoire_backend", envir = object, inherits = FALSE) ||
      bindingIsActive("immune_repertoire_backend", object) ||
      isTRUE(rlang::env_binding_are_lazy(object, "immune_repertoire_backend"))
  ) {
    return(NULL)
  }
  backend <- object[["immune_repertoire_backend"]]
  valid <- is.list(backend) &&
    identical(backend$type, "file") &&
    .cerebroSiblingName(backend$file)
  if (!valid) NULL else backend[c("type", "file")]
}

## The immune repertoire sidecar of the CRB being replaced that the new CRB no
## longer uses, or NULL. Only the canonical sidecar names of this CRB qualify,
## and a file that another CRB in the directory still points to stays.
.retiredImmuneSidecar <- function(previous, final_file, location) {
  canonical <- c(
    .immuneRepertoireSidecarName(final_file, "qs2"),
    .immuneRepertoireSidecarName(final_file, "rds")
  )
  if (
    is.null(previous) ||
      identical(previous$file, location) ||
      !previous$file %in% canonical
  ) {
    return(NULL)
  }
  others <- list.files(dirname(final_file), "[.]crb$", full.names = TRUE)
  others <- others[basename(others) != basename(final_file)]
  for (other in others) {
    shared <- .readPublishedImmuneBackend(other)
    if (!is.null(shared) && identical(shared$file, previous$file)) {
      return(NULL)
    }
  }
  file.path(dirname(final_file), previous$file)
}

.publishCerebroExport <- function(
  export,
  final_file,
  stage_dir,
  expression_matrix_mode,
  codec = "rds",
  immune_sidecar = "auto",
  immune_sidecar_threshold = 100000L
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
  previous <- .readPublishedObject(final_file)
  previous_backend <- .readPublishedExportBackend(final_file, previous)
  previous_spatial_backend <- .readPublishedSpatialBackend(final_file, previous)
  previous_immune_backend <- .readPublishedImmuneBackend(final_file, previous)
  rm(previous)
  stage_sidecar <- NULL
  final_sidecar <- NULL
  stage_spatial <- NULL
  final_spatial <- NULL
  stage_immune <- NULL
  final_immune <- NULL
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
  old_spatial_backup <- NULL
  old_immune_backup <- NULL
  retired_sidecar <- NULL
  retired_immune <- NULL
  if (
    !is.null(previous_backend) &&
      identical(previous_backend$type, "h5") &&
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
  installed_spatial <- FALSE
  installed_immune <- FALSE
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
        if (installed_spatial && dir.exists(final_spatial)) {
          unlink(final_spatial, recursive = TRUE, force = TRUE)
        }
        if (!is.null(old_spatial_backup) && dir.exists(old_spatial_backup)) {
          if (!file.rename(old_spatial_backup, final_spatial)) {
            warning(
              "Export rollback could not restore the previous spatial ",
              "molecule sidecar from: ",
              old_spatial_backup,
              call. = FALSE
            )
          }
        }
        if (installed_immune && file.exists(final_immune)) {
          unlink(final_immune, force = TRUE)
        }
        if (!is.null(old_immune_backup) && file.exists(old_immune_backup)) {
          if (!file.rename(old_immune_backup, final_immune)) {
            warning(
              "Export rollback could not restore the previous immune ",
              "repertoire sidecar from: ",
              old_immune_backup,
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
    if (dir.exists(stage_sidecar)) {
      stop("The staged h5 sidecar must be a regular file.", call. = FALSE)
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
    .setExportArtifactMode(
      stage_sidecar,
      "0600",
      "the staged expression sidecar"
    )
    if (!file.rename(stage_sidecar, final_sidecar)) {
      stop("Failed to install the staged expression sidecar.", call. = FALSE)
    }
    installed_sidecar <- TRUE
  }

  payload <- if (.recognizedCerebroObject(export)) {
    .thinCerebroPayload(export, final_file, sidecar = final_sidecar)
  } else {
    export
  }
  immune <- .stageImmuneRepertoire(
    payload,
    stage_crb,
    codec,
    immune_sidecar,
    immune_sidecar_threshold
  )
  payload <- immune$payload
  stage_immune <- immune$stage
  ## A sidecar this export no longer uses (the repertoire moved back into the
  ## CRB, or the codec changed) is removed after the new CRB is committed.
  retired_immune <- .retiredImmuneSidecar(
    previous_immune_backend,
    final_file,
    immune$location
  )
  if (!is.null(immune$location)) {
    final_immune <- file.path(final_dir, immune$location)
    if (.pathIsSymbolicLink(final_immune)) {
      stop(
        "Refusing to replace a symbolic-link immune repertoire sidecar.",
        call. = FALSE
      )
    }
    if (file.exists(final_immune)) {
      owns_target <- !is.null(previous_immune_backend) &&
        identical(previous_immune_backend$file, immune$location)
      if (!owns_target) {
        stop(
          "Refusing to replace an existing immune repertoire sidecar that is ",
          "not owned by the published CRB: ",
          final_immune,
          call. = FALSE
        )
      }
      old_immune_backup <- tempfile(
        pattern = paste0(".", basename(final_immune), "-backup-"),
        tmpdir = final_dir
      )
      if (!file.rename(final_immune, old_immune_backup)) {
        stop(
          "Failed to preserve the previous immune repertoire sidecar.",
          call. = FALSE
        )
      }
    }
    .setExportArtifactMode(
      stage_immune,
      "0600",
      "the immune repertoire sidecar"
    )
    if (!file.rename(stage_immune, final_immune)) {
      stop(
        "Failed to install the staged immune repertoire sidecar.",
        call. = FALSE
      )
    }
    installed_immune <- TRUE
  }
  spatial <- .stageSpatialMolecules(payload, stage_crb, codec)
  payload <- spatial$payload
  stage_spatial <- spatial$stage
  if (!is.null(spatial$location)) {
    final_spatial <- file.path(final_dir, spatial$location)
    if (.pathIsSymbolicLink(final_spatial)) {
      stop(
        "Refusing to replace a symbolic-link spatial sidecar.",
        call. = FALSE
      )
    }
    if (dir.exists(final_spatial)) {
      owns_target <- !is.null(previous_spatial_backend) &&
        identical(previous_spatial_backend$location, spatial$location)
      if (!owns_target) {
        stop(
          "Refusing to replace an existing spatial sidecar that is not owned ",
          "by the published CRB: ",
          final_spatial,
          call. = FALSE
        )
      }
      old_spatial_backup <- tempfile(
        pattern = paste0(".", basename(final_spatial), "-backup-"),
        tmpdir = final_dir
      )
      if (!file.rename(final_spatial, old_spatial_backup)) {
        stop("Failed to preserve the previous spatial sidecar.", call. = FALSE)
      }
    }
    .setExportArtifactMode(stage_spatial, "0700", "the spatial sidecar")
    if (!file.rename(stage_spatial, final_spatial)) {
      stop("Failed to install the staged spatial sidecar.", call. = FALSE)
    }
    installed_spatial <- TRUE
  }
  .writeCerebroPayload(payload, stage_crb, codec)
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

  for (backup in c(
    old_crb_backup,
    old_sidecar_backup,
    old_spatial_backup,
    old_immune_backup
  )) {
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
  for (retired in list(retired_sidecar, retired_immune)) {
    if (!is.null(retired)) {
      .removeRetiredSidecar(retired)
    }
  }
  invisible(final_file)
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
#' \code{"embedded"} (default) or \code{"h5"}.
#' \itemize{
#'   \item \code{"embedded"} stores the matrix inside the \code{.crb} file.
#'   \item \code{"h5"} writes the matrix via \code{HDF5Array::writeTENxMatrix()}
#'   to a TENx-format sparse HDF5 file next to the \code{.crb} (sibling
#'   \code{<stem>.h5}) and tags the backend with that file name. The
#'   file holds a single \code{/expression} group with \code{data},
#'   \code{indices}, \code{indptr}, \code{shape}, \code{genes}, and
#'   \code{barcodes} datasets. The matrix is stored cells x genes (TENx
#'   column-favoured, optimised for per-gene reads); the Viewer reads it back as
#'   a lazy \code{HDF5Array::TENxMatrix} seed and transposes it lazily to
#'   Cerebro's internal genes x cells layout via \code{DelayedArray::t()}
#'   (free). The in-memory \code{dgCMatrix} is never materialised on attach, so
#'   RAM stays close to the \code{.crb} metadata size. The \code{.crb} is thin:
#'   the cell names are kept once, in the H5 file, and the \code{.crb} records
#'   their order and a checksum. The gzip level is set by
#'   \code{h5_compression_level}; the default (no compression) is several
#'   times larger than a compressed file but builds and reads faster.
#'   Requires the \pkg{HDF5Array} package.
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
#' @param h5_compression_level Gzip compression level, from 0 to 9, of the
#' H5 file written when \code{expression_matrix_mode = "h5"}; ignored by the
#' other modes. The default 0 writes the data without compression: the file
#' is about 7 times larger than at HDF5Array's default level 6 (about 69 GiB
#' instead of 10 GiB for 1.5 million human prefrontal cortex cells), but it
#' builds about three times faster and single-gene reads are about a third
#' faster. Set 6 when disk space matters more than speed.
#' @param codec Serialization codec for the CRB payload. Defaults to
#' \code{"qs2"}; use \code{"rds"} when direct compatibility with
#' \code{readRDS()} is required.
#' @param viewer_binary Build the derived Viewer Pack \code{"auto"}matically
#' for large datasets, \code{"always"}, or \code{"never"}.
#' @param viewer_binary_threshold Dataset-level cell threshold used by
#' \code{viewer_binary = "auto"}.
#' @param immune_sidecar Store the immune repertoire in a sibling file
#' \code{<stem>.immune.qs2} (\code{.rds} with \code{codec = "rds"}) instead of
#' inside the \code{.crb}. \code{"auto"} does so for \code{"h5"} exports whose
#' repertoire has at least \code{immune_sidecar_threshold} rows; \code{"always"}
#' and \code{"never"} override that. The sidecar must travel with the
#' \code{.crb}.
#' @param immune_sidecar_threshold Number of repertoire rows (cells with
#' receptor data) used by \code{immune_sidecar = "auto"}.
#' @param spatial_images Optional named list mapping Seurat image names to named
#'   image paths or descriptors of the form \code{list(path = ..., bounds = ...)}.
#'   Supported file extensions are png, jpg, jpeg, and svg. Missing bounds are
#'   derived from the exported x/y coordinate range. An object whose
#'   \code{@images} hold a Seurat v4 \code{VisiumV1} image is rejected;
#'   rebuild it as \code{VisiumV2} with \code{Seurat::Load10X_Spatial()}.
#' @param verbose Set this to \code{TRUE} if you want additional log messages;
#' defaults to \code{FALSE}.
#' @param .expression_resolution Internal handoff used by
#' \code{convertSeuratToCerebro()} to reuse a matrix that has already been
#' resolved and validated. Users should leave this as \code{NULL}.
#'
#' @section Immune Repertoire:
#' If \code{object@misc$immune_repertoire} contains a named list of
#' data.frames (one per sample, with \code{barcode}, \code{CTgene},
#' \code{CTnt}, \code{CTaa}, and \code{CTstrict}), it will be automatically
#' exported into the Cerebro object via \code{addImmuneRepertoire()}. When that
#' slot is empty, receptors kept apart in \code{object@misc$tcr_data} and
#' \code{object@misc$bcr_data} are merged into it.
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
  expression_matrix_mode = c("embedded", "h5"),
  h5_compression_level = 0L,
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  spatial_images = NULL,
  verbose = FALSE,
  immune_sidecar = c("auto", "always", "never"),
  immune_sidecar_threshold = 100000L,
  .expression_resolution = NULL
) {
  ##--------------------------------------------------------------------------##
  ## safety checks before starting to do anything
  ##--------------------------------------------------------------------------##

  expression_matrix_mode <- match.arg(expression_matrix_mode)
  if (
    !is.numeric(h5_compression_level) ||
      length(h5_compression_level) != 1L ||
      is.na(h5_compression_level) ||
      !h5_compression_level %in% 0:9
  ) {
    stop(
      "`h5_compression_level` must be one integer from 0 to 9.",
      call. = FALSE
    )
  }
  h5_compression_level <- as.integer(h5_compression_level)
  codec <- match.arg(codec)
  viewer_options <- .viewerPackOptions(viewer_binary, viewer_binary_threshold)
  immune_options <- .immuneSidecarOptions(
    match.arg(immune_sidecar),
    immune_sidecar_threshold
  )
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
      "switch to expression_matrix_mode = \"embedded\".",
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

  .checkSeuratAvailable()
  .checkSeuratObject(object)

  ## spatial images must be Seurat v5 images
  .assertCurrentSpatialImages(object)

  .validateExportColumns(
    names(object@meta.data),
    groups,
    nUMI,
    nGene,
    cell_cycle,
    main_group
  )

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
  export_stage_dir <- .createPrivateExportStage(final_file)
  on.exit(
    unlink(export_stage_dir, recursive = TRUE, force = TRUE),
    add = TRUE
  )

  export <- .newCerebroExport(experiment_name, organism, verbose)

  ##--------------------------------------------------------------------------##
  ## add transcript counts
  ##--------------------------------------------------------------------------##

  ## get expression data using shared utility function. A requested layer that
  ## does not exist is an error naming the layers that do (no other layer is
  ## substituted).
  ## Split (layered) Seurat v5 objects are joined first, matching
  ## `convertSeuratToCerebro()`. Without it only the first sample's layer is
  ## read: the meta data still describes every cell while the matrix describes
  ## one sample, and the two are never compared again in the `h5` mode.
  ## `JoinLayers()` materialises the merged matrix, so a very large split
  ## object pays one memory peak here -- the price of exporting all of it.
  if (is.null(.expression_resolution)) {
    expression_resolution <- .getExpressionMatrix(
      seurat = object,
      assay = assay,
      slot = slot,
      join_samples = TRUE,
      layer_argument = "slot",
      verbose = verbose,
      return_resolution = TRUE
    )
  } else {
    required_resolution_fields <- c(
      "data",
      "assay",
      "requested",
      "resolved",
      "joined"
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
    expression_data <- .delayedExportExpression(
      expression_data,
      use_delayed_array,
      verbose
    )

    .logStep('Adding expression data (embedded)...')
    export$setExpression(expression_data)
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
      }
    }

    ## transpose genes x cells -> cells x genes for storage
    m_disk <- if (inherits(expression_data, c("RleMatrix", "DelayedMatrix"))) {
      DelayedArray::t(expression_data)
    } else {
      methods::as(Matrix::t(expression_data), "CsparseMatrix")
    }

    if (verbose) {
      .logStep("Writing expression matrix to TENx HDF5 file: ", h5_abs)
    }

    if (file.exists(h5_abs)) {
      file.remove(h5_abs)
    }
    ## The default gzip level 0 stores the data without compression (the
    ## shuffle and deflate filters are still applied). On 1.3-1.5M-cell
    ## sources it reads single genes ~30% faster than HDF5Array's default
    ## level 6, at roughly 7x the file size.
    HDF5Array::writeTENxMatrix(
      m_disk,
      h5_abs,
      group = "expression",
      level = h5_compression_level
    )

    ## self$expression stays NULL — saveRDS therefore does not embed the
    ## matrix inside the .crb. The runtime attach reads the sibling back
    ## as a lazy TENxMatrix seed (no in-memory dgCMatrix materialisation).
    export$setExpressionBackend(type = "h5", location = h5_filename)
  }

  extras <- object@misc
  .addExportExperiment(export, extras)
  .addExportMetaData(
    export,
    object@meta.data,
    Seurat::Cells(object),
    groups,
    nUMI,
    nGene,
    cell_cycle,
    add_all_meta_data,
    verbose
  )
  if (!is.null(main_group)) {
    export$addParameters('main_group', main_group)
  }
  .addExportProjections(
    export,
    names(object@reductions),
    function(name) object@reductions[[name]]@cell.embeddings,
    verbose
  )
  .addExportGroupTables(
    extras$trees,
    'object@misc$trees',
    'Extracting trees...',
    export$addTree,
    verbose
  )
  ## Unlike exportFromSCE(), tables of groups that are not exported are skipped.
  .addExportGroupTables(
    extras$most_expressed_genes,
    'object@misc$most_expressed_genes',
    'Extracting tables of most expressed genes...',
    export$addMostExpressedGenes,
    verbose,
    groups
  )
  .addExportGroupTables(
    extras$mean_expression,
    'object@misc$mean_expression',
    'Extracting tables of mean expression...',
    export$addMeanExpression,
    verbose,
    groups
  )

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
      .logStep(
        'Extracting immune repertoire data (',
        length(unified_repertoire),
        ' samples)...'
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
      .logStep('Extracting HLA typing...')
    }
    st <- object@misc$hla_typing_source_type
    if (is.null(st) || !nzchar(st)) {
      st <- 'unknown'
    }
    export$addHLATyping(object@misc$hla_typing, source_type = st)
  }

  ##--------------------------------------------------------------------------##
  ## Receptors kept apart in @misc$tcr_data / @misc$bcr_data
  ##--------------------------------------------------------------------------##
  ## Seurat objects prepared by older workflows hold TCR and BCR in two slots.
  ## They are checked like the unified slot and merged into the one repertoire
  ## the .crb stores.
  if (!has_unified_repertoire) {
    receptor_slots <- list(tcr_data = "TCR", bcr_data = "BCR")
    receptors <- lapply(names(receptor_slots), function(slot) {
      data <- .dropEmptyRepertoireSamples(object@misc[[slot]])
      if (is.null(data)) {
        return(NULL)
      }
      if (verbose) {
        .logStep('Extracting tables of ', receptor_slots[[slot]], ' data...')
      }
      .normalizeImmuneRepertoire(
        data,
        cell_barcodes = Seurat::Cells(object),
        source_label = paste0("`@misc$", slot, "`")
      )
    })
    merged_repertoire <- .dropEmptyRepertoireSamples(
      do.call(.mergeRepertoiresBySample, receptors)
    )
    if (!is.null(merged_repertoire) && length(merged_repertoire) > 0) {
      export$addImmuneRepertoire(.normalizeImmuneRepertoire(
        merged_repertoire,
        cell_barcodes = Seurat::Cells(object),
        source_label = "the merged TCR and BCR repertoire"
      ))
    }
  }

  ## marker_genes is a nested list: list(method = list(group = data.frame))
  .addExportMethodTables(
    extras$marker_genes,
    'object@misc$marker_genes',
    'Extracting marker genes table...',
    export$addMarkerGenes,
    verbose
  )
  .addExportMethodTables(
    extras$enriched_pathways,
    'object@misc$enriched_pathways',
    'Extracting pathway enrichment results...',
    export$addEnrichedPathways,
    verbose,
    groups
  )
  .addExportTrajectories(export, extras$trajectories, verbose)
  .addExportExtraMaterial(export, extras$extra_material, verbose)

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
    .logStep('Checking for spatial data...')
  }

  ## Spatial images live in the `@images` slot. A v4 `VisiumV1` image was
  ## rejected above; every other image class is exported.
  ## `.getSpatialData()` resolves their coordinates through GetTissueCoordinates
  ## + S4 slot access.
  has_images <- .spx_has_slot(object, "images") &&
    !is.null(object@images) &&
    length(object@images) > 0
  misc_spatial_images <- .validateCerebroSpatialImages(
    object@misc$cerebro_spatial_images,
    if (has_images) names(object@images) else character(0)
  )
  path_spatial_images <- .normalizeSpatialImagePaths(
    spatial_images,
    if (has_images) names(object@images) else character(0),
    "`spatial_images`"
  )
  .mergeSpatialImageDeclarations(misc_spatial_images, path_spatial_images)

  if (has_images) {
    if (verbose) {
      .logStep('Spatial data found. Extracting spatial coordinates...')
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

          # Identify coordinate columns to use for projection (2D)
          proj_cols <- character(0)

          if (all(c("x", "y") %in% colnames(coords_df))) {
            # Standard FOV/Xenium/Other
            proj_cols <- c("x", "y")
          } else if (ncol(coords_df) >= 2) {
            # Fallback: use first two columns
            proj_cols <- colnames(coords_df)[1:2]
          }

          if (length(proj_cols) == 2) {
            coords_df <- coords_df[, proj_cols, drop = FALSE]
            if (verbose) {
              .logStep('Added spatial projection: ', image_name)
            }
          }
          spatial_data$coordinates <- coords_df
          spatial_data
        },
        error = function(e) {
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
        .logStep(
          'Added spatial data: ',
          image_name,
          ' (',
          nrow(spatial_data$coordinates),
          ' cells)'
        )
      }
    }
  }

  .printExportOverview(export)
  .logStep('Saving Cerebro object to: ', final_file)

  ## Replace the sidecar and .crb with best-effort rollback on ordinary errors.
  .publishCerebroExport(
    export = export,
    final_file = final_file,
    stage_dir = export_stage_dir,
    expression_matrix_mode = expression_matrix_mode,
    codec = codec,
    immune_sidecar = immune_options$mode,
    immune_sidecar_threshold = immune_options$threshold
  )
  if (
    .viewerPackEnabled(
      viewer_options$mode,
      viewer_options$threshold,
      length(.viewerPackCells(export))
    )
  ) {
    buildViewerPack(
      final_file,
      viewer_binary = "always",
      viewer_binary_threshold = viewer_options$threshold,
      overwrite = TRUE
    )
  }

  ## .publishCerebroExport() stops when the file is not written
  .logStep('Done!')
}
