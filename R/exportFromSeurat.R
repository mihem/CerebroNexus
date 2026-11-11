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

.exportStageFailureMessage <- function(
  final_dir,
  stage,
  writable = file.access(final_dir, mode = 2L) == 0L,
  os_type = .Platform$OS.type
) {
  if (!isTRUE(writable)) {
    return(paste(
      "CRB preparation could not create a temporary folder because the output folder is not writable.",
      "Save the Project in a writable local folder and try again."
    ))
  }
  if (
    identical(os_type, "windows") &&
      nchar(stage, type = "bytes") >= 240L
  ) {
    return(paste(
      "CRB preparation could not create a temporary folder because the Project path is too long for this Windows setup.",
      "Move the Project to a shorter folder and try again."
    ))
  }
  paste(
    "CRB preparation could not create a temporary folder because the filesystem rejected it.",
    "Save the Project in a writable local folder instead of a synced or network folder, then try again."
  )
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
    pattern = ".crb-stage-",
    tmpdir = final_dir
  )
  creation_warning <- NULL
  created <- withCallingHandlers(
    dir.create(stage, mode = "0700", showWarnings = TRUE),
    warning = function(warning) {
      creation_warning <<- conditionMessage(warning)
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(created)) {
    diagnostic <- if (is.null(creation_warning)) {
      "dir.create() returned FALSE without a warning."
    } else {
      creation_warning
    }
    message(
      "CRB staging diagnostic: ",
      diagnostic,
      " Attempted path: ",
      stage
    )
    stop(.exportStageFailureMessage(final_dir, stage), call. = FALSE)
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
      identical(tolower(name), ".crb") ||
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
  immune_sidecar_threshold = 100000L,
  .open_gz = gzfile
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
        pattern = ".side-b-",
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
    .rememberExpressionSidecar(final_sidecar, backend$md5)
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
        pattern = ".immune-b-",
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
        pattern = ".spatial-b-",
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
  if (identical(codec, "rds")) {
    connection <- .open_gz(stage_crb, open = "wb", compression = 1L)
    tryCatch(saveRDS(payload, connection), finally = close(connection))
  } else {
    .writeCerebroPayload(payload, stage_crb, codec)
  }
  if (!file.exists(stage_crb)) {
    stop("Failed to serialise the staged Cerebro object.", call. = FALSE)
  }
  .setExportArtifactMode(stage_crb, old_crb_mode, "the staged Cerebro object")

  if (file.exists(final_file)) {
    old_crb_backup <- tempfile(
      pattern = ".crb-b-",
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
  ## The object that was serialized, for a caller that goes on to read the CRB.
  invisible(payload)
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
#' number of transcripts per cell; defaults to \code{nUMI}. \code{NULL} leaves
#' it out.
#' @param nGene Column in \code{object@meta.data} that contains information
#' about number of expressed genes per cell; defaults to \code{nGene}.
#' \code{NULL} leaves it out.
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
#'   \item \code{"h5"} writes the matrix, laid out as
#'   \code{HDF5Array::writeTENxMatrix()} writes it,
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
#' @param .finalize_export Internal hook used by the Builder: a function called
#' with the Cerebro object just before it is written, so the object can be
#' completed without reading the written file back. Users should leave this
#' as \code{NULL}.
#' @param .on_progress Internal hook used by the Builder: a function called
#' with the name of each part of the export as it starts, so a long export
#' can report how far it has got. Users should leave this as \code{NULL}.
#' @param projections Optional ordered names of dimensional reductions to
#'   export. When supplied, every named reduction is exported in that order,
#'   including PCA beside other projections. An empty vector exports none (an
#'   object without reductions, such as an unclustered spatial section). The
#'   default \code{NULL} preserves the legacy behavior that uses non-PCA
#'   reductions when they are available.
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
#' @examplesIf nzchar(system.file(package = "Seurat"))
#' \donttest{
#' ## Reading the Seurat object loads SeuratObject, which takes a few seconds.
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
#' }
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
  .expression_resolution = NULL,
  .finalize_export = NULL,
  .on_progress = NULL,
  projections = NULL,
  spatial_coordinate_transforms = NULL
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
  .exportCheckOutputArgs(
    file,
    expression_matrix_mode,
    use_delayed_array,
    verbose
  )

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

  if (
    !is.null(projections) &&
      (!is.character(projections) ||
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

  export <- .exportInit(experiment_name, organism, object@misc, verbose)

  ##--------------------------------------------------------------------------##
  ## add transcript counts
  ##--------------------------------------------------------------------------##

  .exportReportPart(.on_progress, "reading expression")
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

  .exportReportPart(.on_progress, "writing expression")
  expression_data <- .exportWriteExpression(
    export = export,
    expression_data = expression_data,
    expression_matrix_mode = expression_matrix_mode,
    use_delayed_array = use_delayed_array,
    export_stage_dir = export_stage_dir,
    final_file = final_file,
    verbose = verbose,
    h5_compression_level = h5_compression_level
  )

  .exportReportPart(.on_progress, "adding metadata and results")
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
    if (is.null(projections)) names(object@reductions) else projections,
    function(name) object@reductions[[name]]@cell.embeddings,
    verbose,
    explicit = !is.null(projections)
  )
  .exportAddMiscResults(
    export = export,
    misc = extras,
    groups = groups,
    cells = Seurat::Cells(object),
    verbose = verbose,
    where = "@misc",
    supported_note = paste0(
      'only trajectories generated with Monocle 2 (`monocle2`) are ',
      'supported.'
    )
  )

  ##--------------------------------------------------------------------------##
  ## spatial data
  ##--------------------------------------------------------------------------##
  .exportReportPart(.on_progress, "adding spatial data")
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
  image_names <- if (has_images) names(object@images) else character()
  reduction_names <- tryCatch(
    names(object@reductions),
    error = function(error) character()
  )
  reduction_names <- reduction_names[
    tolower(reduction_names) == "spatial" &
      !reduction_names %in% image_names
  ]
  reduction_names <- Filter(
    function(name) {
      dimensions <- tryCatch(
        dim(SeuratObject::Embeddings(object[[name]])),
        error = function(error) NULL
      )
      length(dimensions) == 2L && dimensions[[2L]] >= 2L
    },
    reduction_names
  )
  spatial_names <- c(reduction_names, image_names)
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

  spatial_coordinate_transform_specs <- list()
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

  if (length(spatial_names)) {
    if (verbose) {
      .logStep('Spatial data found. Extracting spatial coordinates...')
    }

    for (image_name in spatial_names) {
      spatial_data <- tryCatch(
        {
          # Extract spatial data (coordinates + expression)
          # Using .getSpatialData helper which handles Visium, FOV/Xenium, etc.
          spatial_data <- if (image_name %in% reduction_names) {
            .exportEmbeddingSpatialData(
              SeuratObject::Embeddings(object[[image_name]]),
              image_name,
              image_name,
              expression_data,
              assay,
              requested_layer = slot,
              layer = expression_resolution$resolved
            )
          } else {
            extracted <- .getSpatialData(
              object,
              image = image_name,
              layer = slot,
              assay = assay,
              expression_data = expression_data,
              expression_layer = expression_resolution$resolved
            )
            extracted$boundaries <- .getSpatialBoundaries(
              object,
              image_name,
              rownames(extracted$coordinates)
            )
            extracted$molecules <- .getSpatialMolecules(object, image_name)
            extracted
          }

          # Also add coordinates as a projection for compatibility with existing visualization functions
          coords_df <- spatial_data$coordinates

          projection <- .spx_export_projection_coordinates(coords_df)
          if (!is.null(projection)) {
            coords_df <- projection
            if (verbose) {
              .logStep('Added spatial projection: ', image_name)
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
            transform_overlay <- function(data) {
              if (is.null(data) || !nrow(data)) {
                return(data)
              }
              angle <- coordinate_transform$rotation_degrees * pi / 180
              centered_x <- data$x -
                coordinate_transform$pivot[["x"]]
              centered_y <- data$y -
                coordinate_transform$pivot[["y"]]
              data$x <-
                coordinate_transform$pivot[["x"]] +
                coordinate_transform$scale *
                  (centered_x * cos(angle) - centered_y * sin(angle))
              data$y <-
                coordinate_transform$pivot[["y"]] +
                coordinate_transform$scale *
                  (centered_x * sin(angle) + centered_y * cos(angle))
              data
            }
            spatial_data$boundaries <- transform_overlay(
              spatial_data$boundaries
            )
            if (!is.null(spatial_data$molecules)) {
              spatial_data$molecules$data <- transform_overlay(
                spatial_data$molecules$data
              )
            }
            coordinate_transform$transformed_coordinate_fingerprint <-
              .spx_coordinate_transform_fingerprint(coords_df)
            coordinate_transform$coordinate_fingerprint_version <- 2L
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

  if (is.function(.finalize_export)) {
    .finalize_export(export)
  }

  .exportFinalize(
    export = export,
    final_file = final_file,
    export_stage_dir = export_stage_dir,
    expression_matrix_mode = expression_matrix_mode,
    codec = codec,
    viewer_options = viewer_options,
    immune_options = immune_options,
    on_progress = .on_progress
  )
}
