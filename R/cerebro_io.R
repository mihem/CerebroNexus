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

  metadata$cell_barcode <- NULL
  for (name in intersect(c("nUMI", "nGene"), names(metadata))) {
    metadata[[name]] <- .compactCountColumn(metadata[[name]])
  }
  payload$meta_data <- metadata
  payload$expression <- NULL
  payload$cell_fingerprint <- .cerebroCellFingerprint(cells)
  payload$crb_schema <- list(
    version = 1L,
    cell_names = "expression",
    cell_names_md5 = .bpcellsCellNamesChecksum(sidecar),
    projection_rownames = compact_projections
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
  valid <- is.list(schema) &&
    !is.data.frame(schema) &&
    length(schema) == 4L &&
    !is.null(schema_names) &&
    !anyDuplicated(schema_names) &&
    setequal(
      schema_names,
      c("version", "cell_names", "cell_names_md5", "projection_rownames")
    ) &&
    identical(schema$version, 1L) &&
    identical(schema$cell_names, "expression") &&
    is.character(schema$cell_names_md5) &&
    length(schema$cell_names_md5) == 1L &&
    !is.na(schema$cell_names_md5) &&
    grepl("^[[:xdigit:]]{32}$", schema$cell_names_md5) &&
    is.character(schema$projection_rownames) &&
    !anyNA(schema$projection_rownames) &&
    !any(!nzchar(schema$projection_rownames)) &&
    !anyDuplicated(schema$projection_rownames)
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

.installCerebroPayload <- function(stage, file) {
  backup <- NULL
  committed <- FALSE
  on.exit(
    {
      if (!committed && !is.null(backup) && file.exists(backup)) {
        file.rename(backup, file)
      }
    },
    add = TRUE
  )

  if (file.exists(file)) {
    backup <- tempfile(paste0(".", basename(file), "-backup-"), dirname(file))
    if (!file.rename(file, backup)) {
      stop("Could not preserve the existing CRB.", call. = FALSE)
    }
  }
  if (!file.rename(stage, file)) {
    stop("Could not install the new CRB.", call. = FALSE)
  }
  committed <- TRUE
  if (!is.null(backup) && file.exists(backup)) {
    unlink(backup)
  }
  invisible(file)
}

#' Save a Cerebro data file
#'
#' Saves a Cerebro object as a thin CRB when it uses a BPCells sidecar. The
#' input object and expression sidecar are not modified.
#'
#' @param object A Cerebro object.
#' @param file Output \code{.crb} path.
#' @param codec Serialization codec. Defaults to \code{"qs2"}; use
#' \code{"rds"} when direct compatibility with \code{readRDS()} is required.
#' @return The output path, invisibly.
#' @export
saveCerebro <- function(object, file, codec = c("qs2", "rds")) {
  codec <- match.arg(codec)
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
  stage <- tempfile(paste0(".", basename(file), "-"), dirname(file))
  on.exit(unlink(stage, force = TRUE), add = TRUE)
  .writeCerebroPayload(payload, stage, codec)
  .installCerebroPayload(stage, file)
}

#' Read a Cerebro data file
#'
#' Auto-detects legacy RDS, thin RDS, and thin qs2 CRBs. Thin CRBs are
#' hydrated from their sibling expression sidecar.
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
  .hydrateThinCerebro(object, file, backend, schema)
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
        dirname(input),
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
