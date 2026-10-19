.storedCerebroField <- function(object, field) {
  is.environment(object) &&
    exists(field, envir = object, inherits = FALSE) &&
    !bindingIsActive(field, object) &&
    !isTRUE(rlang::env_binding_are_lazy(object, field))
}

.recognizedCerebroObject <- function(object) {
  is.environment(object) &&
    any(startsWith(class(object), "Cerebro")) &&
    environmentIsLocked(object)
}

.currentCerebroCopy <- function(object) {
  if (!.recognizedCerebroObject(object)) {
    stop("`object` must be a serialized Cerebro object.", call. = FALSE)
  }

  copy <- Cerebro$new()
  for (field in names(Cerebro$public_fields)) {
    if (.storedCerebroField(object, field)) {
      copy[[field]] <- object[[field]]
    }
  }
  copy
}

.cerebroBackend <- function(object) {
  if (!.recognizedCerebroObject(object)) {
    stop("`object` must be a serialized Cerebro object.", call. = FALSE)
  }
  if (!exists("expression_backend", envir = object, inherits = FALSE)) {
    return(list(type = "embedded", location = NULL))
  }
  if (!.storedCerebroField(object, "expression_backend")) {
    stop("The Cerebro expression-backend descriptor is invalid.", call. = FALSE)
  }
  backend <- object[["expression_backend"]]
  if (is.null(backend)) {
    return(list(type = "embedded", location = NULL))
  }
  valid_type <- is.list(backend) &&
    is.character(backend$type) &&
    length(backend$type) == 1L &&
    !is.na(backend$type) &&
    backend$type %in% c("embedded", "bpcells", "h5")
  valid_location <- valid_type &&
    if (identical(backend$type, "embedded")) {
      is.null(backend$location)
    } else {
      is.character(backend$location) &&
        length(backend$location) == 1L &&
        !is.na(backend$location) &&
        nzchar(backend$location) &&
        !backend$location %in% c(".", "..") &&
        !grepl("[/\\\\]", backend$location)
    }
  if (!valid_type || !valid_location) {
    stop("The Cerebro expression-backend descriptor is invalid.", call. = FALSE)
  }
  list(type = backend$type, location = backend$location)
}

.cerebroSidecar <- function(file, backend) {
  sidecar <- file.path(
    dirname(normalizePath(file, mustWork = FALSE)),
    backend$location
  )
  exists <- if (identical(backend$type, "bpcells")) {
    dir.exists(sidecar)
  } else {
    file.exists(sidecar) && !dir.exists(sidecar)
  }
  if (!exists) {
    stop(
      "The ",
      backend$type,
      " expression sidecar is missing: ",
      sidecar,
      call. = FALSE
    )
  }
  sidecar
}

.bpcellsCellNamesChecksum <- function(sidecar) {
  names_file <- file.path(sidecar, "col_names")
  if (!file.exists(names_file) || dir.exists(names_file)) {
    stop("The BPCells sidecar has no cell-name index.", call. = FALSE)
  }
  checksum <- unname(tools::md5sum(names_file))
  if (length(checksum) != 1L || is.na(checksum) || !nzchar(checksum)) {
    stop("Could not checksum the BPCells cell-name index.", call. = FALSE)
  }
  checksum
}

.immuneRepertoireBackend <- function(object, file) {
  field <- "immune_repertoire_backend"
  if (!exists(field, envir = object, inherits = FALSE)) {
    return(NULL)
  }
  if (!.storedCerebroField(object, field)) {
    stop("The immune repertoire sidecar descriptor is invalid.", call. = FALSE)
  }
  descriptor <- object[[field]]
  if (is.null(descriptor)) {
    return(NULL)
  }
  valid <- is.list(descriptor) &&
    identical(descriptor$type, "bpcells-file") &&
    is.character(descriptor$file) &&
    length(descriptor$file) == 1L &&
    !is.na(descriptor$file) &&
    nzchar(descriptor$file) &&
    !descriptor$file %in% c(".", "..") &&
    !grepl("[/\\\\]", descriptor$file) &&
    is.character(descriptor$md5) &&
    length(descriptor$md5) == 1L &&
    !is.na(descriptor$md5) &&
    grepl("^[[:xdigit:]]{32}$", descriptor$md5)
  if (!valid) {
    stop("The immune repertoire sidecar descriptor is invalid.", call. = FALSE)
  }
  samples <- as.character(descriptor$samples %||% character())
  chains <- as.character(descriptor$chains %||% character())
  if (anyNA(samples) || anyNA(chains)) {
    stop("The immune repertoire sidecar descriptor is invalid.", call. = FALSE)
  }
  expression_backend <- .cerebroBackend(object)
  if (!identical(expression_backend$type, "bpcells")) {
    stop(
      "An immune repertoire sidecar requires a BPCells backend.",
      call. = FALSE
    )
  }
  root <- .cerebroSidecar(file, expression_backend)
  repertoire_file <- file.path(root, descriptor$file)
  if (!file.exists(repertoire_file) || dir.exists(repertoire_file)) {
    stop(
      "The immune repertoire sidecar is missing: ",
      repertoire_file,
      call. = FALSE
    )
  }
  list(
    type = "bpcells-file",
    file = descriptor$file,
    md5 = descriptor$md5,
    samples = unique(samples[nzchar(samples)]),
    chains = unique(chains[nzchar(chains)]),
    root = root
  )
}

.cerebroCellFingerprint <- function(cells) {
  cells <- sort(enc2utf8(cells), method = "radix")
  stream <- paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")
  path <- tempfile("cerebro-cell-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(stream), path)
  paste0("md5-cell-set-v1:", unname(tools::md5sum(path)))
}

.compactCountColumn <- function(values) {
  if (
    is.double(values) &&
      identical(class(values), "numeric") &&
      all(
        is.na(values) |
          (is.finite(values) &
            values >= 0 &
            values <= .Machine$integer.max &
            values == floor(values))
      )
  ) {
    return(as.integer(values))
  }
  values
}

.thinCerebroPayload <- function(object, file, sidecar = NULL) {
  backend <- .cerebroBackend(object)
  payload <- .currentCerebroCopy(object)
  payload$crb_schema <- NULL
  immune_backend <- .immuneRepertoireBackend(object, file)
  if (!is.null(immune_backend)) {
    payload$immune_repertoire <- list()
    payload$bcr_data <- list()
    payload$tcr_data <- list()
    payload$immune_repertoire_backend <- immune_backend[c(
      "type",
      "file",
      "md5",
      "samples",
      "chains"
    )]
  }
  if (identical(backend$type, "embedded")) {
    return(payload)
  }
  if (is.null(sidecar)) {
    sidecar <- .cerebroSidecar(file, backend)
  }
  if (identical(backend$type, "h5")) {
    payload$expression <- NULL
    return(payload)
  }

  cells <- colnames(payload$expression)
  metadata <- payload$meta_data
  if (
    !is.character(cells) ||
      anyNA(cells) ||
      any(!nzchar(cells)) ||
      anyDuplicated(cells) ||
      !is.data.frame(metadata) ||
      !"cell_barcode" %in% names(metadata) ||
      !identical(as.character(metadata$cell_barcode), cells)
  ) {
    stop(
      "The BPCells sidecar and CRB metadata must contain the same cells in ",
      "the same order.",
      call. = FALSE
    )
  }

  compact_projections <- character()
  for (name in names(payload$projections)) {
    projection <- payload$projections[[name]]
    if (identical(rownames(projection), cells)) {
      rownames(projection) <- NULL
      payload$projections[[name]] <- projection
      compact_projections <- c(compact_projections, name)
    }
  }

  rownames(metadata) <- NULL
  metadata$cell_barcode <- NULL
  for (name in intersect(c("nUMI", "nGene"), names(metadata))) {
    metadata[[name]] <- .compactCountColumn(metadata[[name]])
  }
  payload$meta_data <- metadata
  payload$expression <- NULL
  payload$cell_fingerprint <- .cerebroCellFingerprint(cells)
  payload$crb_schema <- list(
    version = 2L,
    cell_names = "expression",
    cell_names_md5 = .bpcellsCellNamesChecksum(sidecar),
    projection_rownames = compact_projections,
    n_cells = length(cells)
  )
  payload
}

.readCrbSchema <- function(object, file) {
  if (!exists("crb_schema", envir = object, inherits = FALSE)) {
    return(NULL)
  }
  if (!.storedCerebroField(object, "crb_schema")) {
    stop(
      "The Cerebro data file '",
      basename(file),
      "' has an unsupported CRB schema descriptor.",
      call. = FALSE
    )
  }
  schema <- object[["crb_schema"]]
  if (is.null(schema)) {
    return(NULL)
  }
  schema_names <- names(schema)
  expected_names <- if (identical(schema$version, 1L)) {
    c("version", "cell_names", "cell_names_md5", "projection_rownames")
  } else if (identical(schema$version, 2L)) {
    c(
      "version",
      "cell_names",
      "cell_names_md5",
      "projection_rownames",
      "n_cells"
    )
  } else {
    character()
  }
  valid <- is.list(schema) &&
    !is.data.frame(schema) &&
    length(expected_names) > 0L &&
    length(schema) == length(expected_names) &&
    !is.null(schema_names) &&
    !anyDuplicated(schema_names) &&
    setequal(schema_names, expected_names) &&
    identical(schema$cell_names, "expression") &&
    is.character(schema$cell_names_md5) &&
    length(schema$cell_names_md5) == 1L &&
    !is.na(schema$cell_names_md5) &&
    grepl("^[[:xdigit:]]{32}$", schema$cell_names_md5) &&
    is.character(schema$projection_rownames) &&
    !anyNA(schema$projection_rownames) &&
    !any(!nzchar(schema$projection_rownames)) &&
    !anyDuplicated(schema$projection_rownames) &&
    (identical(schema$version, 1L) ||
      (is.integer(schema$n_cells) &&
        length(schema$n_cells) == 1L &&
        !is.na(schema$n_cells) &&
        schema$n_cells >= 0L))
  if (!valid) {
    stop(
      "The Cerebro data file '",
      basename(file),
      "' has an unsupported CRB schema descriptor.",
      call. = FALSE
    )
  }
  schema
}

.attachCerebroExpression <- function(object, file, backend) {
  if (identical(backend$type, "embedded")) {
    return(object)
  }
  sidecar <- .cerebroSidecar(file, backend)
  if (identical(backend$type, "bpcells")) {
    if (!requireNamespace("BPCells", quietly = TRUE)) {
      stop("A BPCells-backed CRB requires the BPCells package.", call. = FALSE)
    }
    object$expression <- BPCells::open_matrix_dir(sidecar)
  } else {
    if (!requireNamespace("HDF5Array", quietly = TRUE)) {
      stop("An H5-backed CRB requires the HDF5Array package.", call. = FALSE)
    }
    object$expression <- t(HDF5Array::TENxMatrix(sidecar, group = "expression"))
  }
  object
}

.hydrateThinCerebro <- function(object, file, backend, schema) {
  if (is.null(schema)) {
    return(object)
  }
  if (!identical(backend$type, "bpcells")) {
    stop("A thin CRB requires a BPCells expression backend.", call. = FALSE)
  }
  sidecar <- .cerebroSidecar(file, backend)
  if (!identical(.bpcellsCellNamesChecksum(sidecar), schema$cell_names_md5)) {
    stop(
      "The BPCells cell-name index does not match CRB '",
      basename(file),
      "'.",
      call. = FALSE
    )
  }

  cells <- colnames(object$expression)
  if (
    identical(schema$version, 2L) &&
      !identical(length(cells), schema$n_cells)
  ) {
    stop(
      "The thin CRB cell count does not match its BPCells sidecar.",
      call. = FALSE
    )
  }
  metadata <- object$meta_data
  if (
    !is.character(cells) ||
      anyNA(cells) ||
      any(!nzchar(cells)) ||
      anyDuplicated(cells) ||
      !is.data.frame(metadata) ||
      nrow(metadata) != length(cells) ||
      "cell_barcode" %in% names(metadata)
  ) {
    stop(
      "The thin CRB and BPCells sidecar have incompatible cell metadata.",
      call. = FALSE
    )
  }

  missing_projections <- setdiff(
    schema$projection_rownames,
    names(object$projections)
  )
  if (length(missing_projections)) {
    stop(
      "The thin CRB is missing projection '",
      missing_projections[[1L]],
      "'.",
      call. = FALSE
    )
  }
  for (name in schema$projection_rownames) {
    projection <- object$projections[[name]]
    if (!is.data.frame(projection) || nrow(projection) != length(cells)) {
      stop(
        "Projection '",
        name,
        "' does not match the thin CRB cell index.",
        call. = FALSE
      )
    }
    rownames(projection) <- cells
    object$projections[[name]] <- projection
  }
  rownames(metadata) <- NULL
  object$meta_data <- data.frame(
    cell_barcode = cells,
    metadata,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  object
}

.writeCerebroPayload <- function(object, file, codec) {
  if (identical(codec, "qs2")) {
    qs2::qs_save(object, file)
  } else {
    saveRDS(object, file)
  }
}

.readCerebroPayload <- function(file) {
  connection <- file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  magic <- readBin(connection, "raw", n = 4L)
  if (identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))) {
    qs2::qs_read(file)
  } else {
    readRDS(file)
  }
}

.spatialMoleculeSidecarName <- function(file) {
  paste0(tools::file_path_sans_ext(basename(file)), ".spatial")
}

.spatialMoleculeBackend <- function(object, file) {
  if (!exists("spatial_molecule_backend", envir = object, inherits = FALSE)) {
    return(NULL)
  }
  if (!.storedCerebroField(object, "spatial_molecule_backend")) {
    stop("The spatial molecule backend descriptor is invalid.", call. = FALSE)
  }
  backend <- object[["spatial_molecule_backend"]]
  if (is.null(backend)) {
    return(NULL)
  }
  valid <- is.list(backend) &&
    identical(backend$type, "directory") &&
    is.character(backend$location) &&
    length(backend$location) == 1L &&
    !is.na(backend$location) &&
    nzchar(backend$location) &&
    !backend$location %in% c(".", "..") &&
    !grepl("[/\\\\]", backend$location)
  if (!valid) {
    stop("The spatial molecule backend descriptor is invalid.", call. = FALSE)
  }
  root <- backend$root
  if (
    !is.character(root) || length(root) != 1L || is.na(root) || !nzchar(root)
  ) {
    root <- file.path(
      dirname(normalizePath(file, mustWork = FALSE)),
      backend$location
    )
  }
  list(type = "directory", location = backend$location, root = root)
}

.stageSpatialMolecules <- function(object, file, codec) {
  if (!.recognizedCerebroObject(object)) {
    return(list(payload = object, stage = NULL, location = NULL))
  }
  payload <- .currentCerebroCopy(object)
  spatial <- payload$spatial
  if (!is.list(spatial) || !length(spatial)) {
    payload$spatial_molecule_backend <- NULL
    return(list(payload = payload, stage = NULL, location = NULL))
  }
  has_molecules <- vapply(
    spatial,
    function(entry) is.list(entry) && !is.null(entry[["molecules"]]),
    logical(1)
  )
  if (!any(has_molecules)) {
    payload$spatial_molecule_backend <- NULL
    return(list(payload = payload, stage = NULL, location = NULL))
  }

  source_backend <- .spatialMoleculeBackend(payload, file)
  location <- .spatialMoleculeSidecarName(file)
  stage <- tempfile(paste0(".", location, "-"), dirname(file))
  if (!dir.create(stage, recursive = FALSE, showWarnings = FALSE)) {
    stop(
      "Could not create the spatial molecule staging directory.",
      call. = FALSE
    )
  }
  committed <- FALSE
  on.exit(
    if (!committed) unlink(stage, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  extension <- if (identical(codec, "qs2")) "qs2" else "rds"
  indices <- which(has_molecules)
  for (position in seq_along(indices)) {
    index <- indices[[position]]
    molecules <- spatial[[index]][["molecules"]]
    sidecar_file <- sprintf("%04d.%s", position, extension)
    target <- file.path(stage, sidecar_file)
    if (inherits(molecules, "CerebroSpatialMoleculeRef")) {
      if (is.null(source_backend)) {
        stop(
          "Spatial molecule reference has no sidecar backend.",
          call. = FALSE
        )
      }
      source <- file.path(source_backend$root, molecules$file)
      if (
        !file.exists(source) || dir.exists(source) || !file.copy(source, target)
      ) {
        stop("Could not copy a spatial molecule sidecar entry.", call. = FALSE)
      }
    } else {
      .writeCerebroPayload(molecules, target, codec)
    }
    checksum <- unname(tools::md5sum(target))
    spatial[[index]][["molecules"]] <- structure(
      list(file = sidecar_file, md5 = checksum),
      class = "CerebroSpatialMoleculeRef"
    )
  }
  payload$spatial <- spatial
  payload$spatial_molecule_backend <- list(
    type = "directory",
    location = location
  )
  committed <- TRUE
  list(payload = payload, stage = stage, location = location)
}

.attachCerebroSpatialMolecules <- function(object, file) {
  backend <- .spatialMoleculeBackend(object, file)
  if (is.null(backend)) {
    return(object)
  }
  if (!dir.exists(backend$root)) {
    stop(
      "The spatial molecule sidecar is missing: ",
      backend$root,
      call. = FALSE
    )
  }
  object$spatial_molecule_backend <- backend
  object
}

.attachCerebroImmuneRepertoire <- function(object, file) {
  backend <- .immuneRepertoireBackend(object, file)
  if (is.null(backend)) {
    return(object)
  }
  object$immune_repertoire_backend <- backend
  object
}

.installCerebroPayload <- function(
  stage,
  file,
  spatial_stage = NULL,
  spatial = NULL
) {
  backup <- NULL
  spatial_backup <- NULL
  backed_up <- FALSE
  backed_up_spatial <- FALSE
  installed <- FALSE
  installed_spatial <- FALSE
  committed <- FALSE
  on.exit(
    {
      if (!committed) {
        if (installed && file.exists(file)) {
          unlink(file, force = TRUE)
        }
        if (backed_up && file.exists(backup) && !file.rename(backup, file)) {
          warning(
            "Could not restore the original CRB; preserved at: ",
            backup,
            call. = FALSE
          )
        }
        if (installed_spatial && dir.exists(spatial)) {
          unlink(spatial, recursive = TRUE, force = TRUE)
        }
        if (
          backed_up_spatial &&
            dir.exists(spatial_backup) &&
            !file.rename(spatial_backup, spatial)
        ) {
          warning(
            "Could not restore the original spatial sidecar; preserved at: ",
            spatial_backup,
            call. = FALSE
          )
        }
      }
    },
    add = TRUE
  )

  if (file.exists(file)) {
    backup <- tempfile(paste0(".", basename(file), "-backup-"), dirname(file))
    if (!file.rename(file, backup)) {
      stop("Could not preserve the existing CRB.", call. = FALSE)
    }
    backed_up <- TRUE
  }
  if (!is.null(spatial_stage)) {
    if (dir.exists(spatial)) {
      spatial_backup <- tempfile(
        paste0(".", basename(spatial), "-backup-"),
        dirname(spatial)
      )
      if (!file.rename(spatial, spatial_backup)) {
        stop("Could not preserve the existing spatial sidecar.", call. = FALSE)
      }
      backed_up_spatial <- TRUE
    }
    if (!file.rename(spatial_stage, spatial)) {
      stop("Could not install the spatial molecule sidecar.", call. = FALSE)
    }
    installed_spatial <- TRUE
  }
  if (!file.rename(stage, file)) {
    stop("Could not install the new CRB.", call. = FALSE)
  }
  installed <- TRUE
  committed <- TRUE
  if (!is.null(backup) && file.exists(backup)) {
    unlink(backup)
  }
  if (!is.null(spatial_backup) && dir.exists(spatial_backup)) {
    unlink(spatial_backup, recursive = TRUE)
  }
  invisible(file)
}

#' Save a Cerebro data file
#'
#' Saves a Cerebro object as a thin CRB when it uses a BPCells sidecar and
#' stores spatial molecule tables in a sibling per-FOV sidecar. The input
#' object and existing sidecars are not modified.
#'
#' @param object A Cerebro object.
#' @param file Output \code{.crb} path.
#' @param codec Serialization codec. Defaults to \code{"qs2"}; use
#' \code{"rds"} when direct compatibility with \code{readRDS()} is required.
#' @param viewer_binary Build the derived Viewer Pack \code{"auto"}matically
#' for large datasets, \code{"always"}, or \code{"never"}.
#' @param viewer_binary_threshold Dataset-level cell threshold used by
#' \code{viewer_binary = "auto"}.
#' @return The output path, invisibly.
#' @export
saveCerebro <- function(
  object,
  file,
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 500000L
) {
  codec <- match.arg(codec)
  viewer_options <- .viewerPackOptions(viewer_binary, viewer_binary_threshold)
  if (
    !is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)
  ) {
    stop("`file` must be one non-empty output path.", call. = FALSE)
  }
  if (dir.exists(file)) {
    stop("`file` must not be a directory.", call. = FALSE)
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(file))) {
    stop("Could not create the CRB output directory.", call. = FALSE)
  }

  payload <- .thinCerebroPayload(object, file)
  spatial <- .stageSpatialMolecules(payload, file, codec)
  payload <- spatial$payload
  stage <- tempfile(paste0(".", basename(file), "-"), dirname(file))
  on.exit(
    {
      unlink(stage, force = TRUE)
      if (!is.null(spatial$stage)) {
        unlink(spatial$stage, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  .writeCerebroPayload(payload, stage, codec)
  .installCerebroPayload(
    stage,
    file,
    spatial_stage = spatial$stage,
    spatial = if (is.null(spatial$location)) {
      NULL
    } else {
      file.path(dirname(file), spatial$location)
    }
  )
  n_cells <- tryCatch(
    length(.viewerPackCells(object)),
    error = function(error) {
      if (identical(viewer_options$mode, "always")) {
        stop(error)
      }
      0L
    }
  )
  if (
    .viewerPackEnabled(
      viewer_options$mode,
      viewer_options$threshold,
      n_cells
    )
  ) {
    buildViewerPack(
      file,
      viewer_binary = "always",
      viewer_binary_threshold = viewer_options$threshold,
      overwrite = TRUE
    )
  }
  invisible(file)
}

#' Read a Cerebro data file
#'
#' Auto-detects legacy RDS, thin RDS, and thin qs2 CRBs. Thin expression data
#' are hydrated from their sibling sidecar; spatial molecule tables stay lazy
#' until their FOV is requested.
#'
#' @param file Input \code{.crb} path.
#' @return A current Cerebro object.
#' @export
readCerebro <- function(file) {
  file <- normalizePath(file, mustWork = TRUE)
  serialized <- .readCerebroPayload(file)
  backend <- .cerebroBackend(serialized)
  schema <- .readCrbSchema(serialized, file)
  object <- .currentCerebroCopy(serialized)
  object <- .attachCerebroExpression(object, file, backend)
  object <- .hydrateThinCerebro(object, file, backend, schema)
  object <- .attachCerebroSpatialMolecules(object, file)
  .attachCerebroImmuneRepertoire(object, file)
}

#' Convert a Cerebro data file
#'
#' Re-serializes a CRB while reusing its existing expression sidecar.
#'
#' @param input Input \code{.crb} path.
#' @param output Output \code{.crb} path in the same directory as \code{input}.
#' Defaults to replacing \code{input}.
#' @param codec Serialization codec. Defaults to \code{"qs2"}; use
#' \code{"rds"} when direct compatibility with \code{readRDS()} is required.
#' @return The output path, invisibly.
#' @export
convertCerebro <- function(input, output = input, codec = c("qs2", "rds")) {
  codec <- match.arg(codec)
  input <- normalizePath(input, mustWork = TRUE)
  valid_output <-
    is.character(output) &&
    length(output) == 1L &&
    !is.na(output) &&
    nzchar(output)
  if (
    valid_output &&
      !identical(
        normalizePath(dirname(input), mustWork = FALSE),
        normalizePath(dirname(output), mustWork = FALSE)
      )
  ) {
    stop(
      "`input` and `output` must be in the same directory because ",
      "conversion reuses the existing expression sidecar.",
      call. = FALSE
    )
  }
  saveCerebro(readCerebro(input), output, codec = codec)
}
