## The CRB reading contract shared with the Viewer runtime; load it once per
## R process.
.crbContract <- local({
  contract <- NULL
  function() {
    if (is.null(contract)) {
      ## Cache only after the file has been sourced: a failed load must not
      ## leave an empty environment behind for later calls.
      loaded <- new.env(parent = baseenv())
      sys.source(
        system.file(
          "viewer",
          "core",
          "crb_contract.R",
          package = "CerebroNexus"
        ),
        loaded
      )
      contract <<- loaded
    }
    contract
  }
})

.storedCerebroField <- function(object, field) {
  is.environment(object) &&
    exists(field, envir = object, inherits = FALSE) &&
    !bindingIsActive(field, object) &&
    !isTRUE(rlang::env_binding_are_lazy(object, field))
}

## The stored value of an optional descriptor field, or `default` when the
## field is absent or NULL. An active or lazy binding stops with `invalid`.
.storedCerebroDescriptor <- function(object, field, invalid, default = NULL) {
  if (!exists(field, envir = object, inherits = FALSE)) {
    return(default)
  }
  if (!.storedCerebroField(object, field)) {
    stop(invalid, call. = FALSE)
  }
  object[[field]] %||% default
}

.recognizedCerebroObject <- function(object) {
  is.environment(object) &&
    inherits(object, "R6") &&
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
  backend <- .storedCerebroDescriptor(
    object,
    "expression_backend",
    "The Cerebro expression-backend descriptor is invalid.",
    list(type = "embedded", location = NULL)
  )
  valid_type <- is.list(backend) &&
    is.character(backend$type) &&
    length(backend$type) == 1L &&
    !is.na(backend$type) &&
    backend$type %in% .crbContract()$.runtimeBackendTypes
  valid_location <- valid_type &&
    if (identical(backend$type, "embedded")) {
      is.null(backend$location)
    } else {
      .cerebroSiblingName(backend$location)
    }
  if (
    !valid_type ||
      !valid_location ||
      (!is.null(backend$md5) &&
        (identical(backend$type, "embedded") || !.validMd5(backend$md5)))
  ) {
    stop("The Cerebro expression-backend descriptor is invalid.", call. = FALSE)
  }
  out <- list(type = backend$type, location = backend$location)
  if (!is.null(backend$md5)) {
    out$md5 <- backend$md5
  }
  out
}

.validMd5 <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    grepl("^[[:xdigit:]]{32}$", x)
}

## Checksums of expression sidecars already verified in this process, keyed by
## path, size and modification time: a file is hashed once until it changes.
.cerebroH5Verified <- new.env(parent = emptyenv())

.verifyExpressionSidecar <- function(sidecar, md5, crb) {
  if (is.null(md5)) {
    return(invisible(TRUE))
  }
  info <- file.info(sidecar)
  key <- paste(
    normalizePath(sidecar, mustWork = TRUE),
    info$size,
    format(as.numeric(info$mtime), digits = 17),
    md5,
    sep = "\r"
  )
  if (isTRUE(get0(key, envir = .cerebroH5Verified, inherits = FALSE))) {
    return(invisible(TRUE))
  }
  if (!identical(tolower(unname(tools::md5sum(sidecar))), tolower(md5))) {
    stop(
      "The h5 expression sidecar checksum does not match CRB '",
      basename(crb),
      "'.",
      call. = FALSE
    )
  }
  assign(key, TRUE, envir = .cerebroH5Verified)
  invisible(TRUE)
}

## A sidecar is addressed by one file name that sits next to the CRB.
.cerebroSiblingName <- function(name) {
  .isString(name) &&
    !name %in% c(".", "..") &&
    !grepl("[/\\\\]", name)
}

.cerebroSidecar <- function(file, backend) {
  sidecar <- file.path(
    dirname(normalizePath(file, mustWork = FALSE)),
    backend$location
  )
  if (!file.exists(sidecar) || dir.exists(sidecar)) {
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

## ---- H5 expression sidecar ------------------------------------------------ ##
## exportFromSeurat() writes the matrix cells x genes through
## HDF5Array::writeTENxMatrix(), which names the two axes the other way round
## from the 10x convention: /expression/genes holds the CELL names and
## /expression/barcodes the GENE names. Reading them with rhdf5 avoids loading
## HDF5Array, whose namespace takes seconds to attach.

.readH5Names <- function(sidecar, dataset) {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("An H5-backed CRB requires the rhdf5 package.", call. = FALSE)
  }
  as.character(as.vector(rhdf5::h5read(
    sidecar,
    paste0("expression/", dataset)
  )))
}

.h5CellNames <- function(sidecar) .readH5Names(sidecar, "genes")

.h5GeneNames <- function(sidecar) .readH5Names(sidecar, "barcodes")

## c(n_cells, n_genes), as the matrix is stored (cells x genes).
.h5Dim <- function(sidecar) {
  if (!requireNamespace("rhdf5", quietly = TRUE)) {
    stop("An H5-backed CRB requires the rhdf5 package.", call. = FALSE)
  }
  as.integer(as.vector(rhdf5::h5read(sidecar, "expression/shape")))
}

.cerebroCellNamesHash <- function(cells) {
  rlang::hash(enc2utf8(as.character(cells)))
}

.validCellNames <- function(cells) {
  is.character(cells) &&
    !anyNA(cells) &&
    all(nzchar(cells)) &&
    !anyDuplicated(cells)
}

## ---- Barcodes stored by cell index (thin CRB schema version 4) ------------- ##
## A thin CRB keeps the cell names once, in the H5 file. Version 4 also stores
## the barcodes of repertoire samples whose barcodes are all cells as metadata
## rows (the column named by the attribute "cerebro_cell_index_column" holds
## integers). Reading restores the barcodes from the cell names in memory, which
## costs a vector of pointers instead of decoding every barcode again.

.validCellIndex <- function(index, n, n_cells) {
  is.integer(index) &&
    length(index) == n &&
    !anyNA(index) &&
    all(index >= 1L & index <= n_cells)
}

## Repertoire samples whose barcodes are all cells keep metadata rows instead.
.indexRepertoireByCell <- function(repertoire, cells) {
  lapply(repertoire, function(sample) {
    barcodes <- if (is.data.frame(sample)) sample[["barcode"]]
    if (!is.character(barcodes)) {
      return(sample)
    }
    index <- match(barcodes, cells)
    if (anyNA(index)) {
      return(sample)
    }
    sample[["barcode"]] <- index
    attr(sample, "cerebro_cell_index_column") <- "barcode"
    sample
  })
}

.restoreRepertoireByCell <- function(repertoire, cells) {
  lapply(repertoire, function(sample) {
    column <- attr(sample, "cerebro_cell_index_column", exact = TRUE)
    if (is.null(column)) {
      return(sample)
    }
    index <- sample[[column]]
    if (
      !identical(column, "barcode") ||
        !.validCellIndex(index, nrow(sample), length(cells))
    ) {
      stop(
        "The thin CRB stores an invalid repertoire cell index.",
        call. = FALSE
      )
    }
    attr(sample, "cerebro_cell_index_column") <- NULL
    sample[[column]] <- cells[index]
    sample
  })
}

## The repertoire samples of a thin payload stored by cell index where their
## barcodes allow it. Returns the names of the fields that changed.
.indexThinPayloadByCell <- function(payload, cells) {
  indexed <- character()
  repertoire <- payload$immune_repertoire
  if (is.list(repertoire) && length(repertoire)) {
    compact <- .indexRepertoireByCell(repertoire, cells)
    if (!identical(compact, repertoire)) {
      payload$immune_repertoire <- compact
      indexed <- union(indexed, "immune_repertoire")
    }
  }
  indexed
}

## The reverse, for a payload read with its cell names.
.restoreThinPayloadByCell <- function(object, cells) {
  repertoire <- object$immune_repertoire
  if (is.list(repertoire) && length(repertoire)) {
    object$immune_repertoire <- .restoreRepertoireByCell(repertoire, cells)
  }
  object
}

## ---- Immune repertoire sidecar -------------------------------------------- ##

.immuneRepertoireSidecarName <- function(file, codec) {
  paste0(
    tools::file_path_sans_ext(basename(file)),
    ".immune.",
    if (identical(codec, "qs2")) "qs2" else "rds"
  )
}

.immuneRepertoireChains <- function(repertoire) {
  genes <- unlist(
    lapply(repertoire, function(table) as.character(table[["CTgene"]])),
    use.names = FALSE
  )
  supported <- c("TRA", "TRB", "TRG", "TRD", "IGH", "IGK", "IGL")
  supported[vapply(
    supported,
    function(chain) any(grepl(chain, genes, fixed = TRUE), na.rm = TRUE),
    logical(1)
  )]
}

.immuneRepertoireRows <- function(repertoire) {
  sum(vapply(
    repertoire,
    function(table) if (is.data.frame(table)) nrow(table) else 0L,
    integer(1)
  ))
}

.immuneRepertoireBackend <- function(object, file) {
  descriptor <- .storedCerebroDescriptor(
    object,
    "immune_repertoire_backend",
    "The immune repertoire sidecar descriptor is invalid."
  )
  if (is.null(descriptor)) {
    return(NULL)
  }
  valid <- is.list(descriptor) &&
    identical(descriptor$type, "file") &&
    .cerebroSiblingName(descriptor$file) &&
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
  root <- dirname(normalizePath(file, mustWork = FALSE))
  repertoire_file <- file.path(root, descriptor$file)
  if (!file.exists(repertoire_file) || dir.exists(repertoire_file)) {
    stop(
      "The immune repertoire sidecar is missing: ",
      repertoire_file,
      call. = FALSE
    )
  }
  list(
    type = "file",
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

## Builds the object that is serialised. An H5-backed CRB is thin: the cell
## names live once, in the H5 file, and the metadata and projections keep only
## their row order. The schema records what readCerebro() must verify.
.thinCerebroPayload <- function(object, file, sidecar = NULL) {
  backend <- .cerebroBackend(object)
  payload <- .currentCerebroCopy(object)
  payload$crb_schema <- NULL
  payload$expression_backend <- backend
  immune_backend <- .immuneRepertoireBackend(object, file)
  if (!is.null(immune_backend)) {
    payload$immune_repertoire <- list()
    payload$immune_repertoire_backend <- immune_backend[c(
      "type",
      "file",
      "md5",
      "samples",
      "chains"
    )]
  }
  spatial_backend <- .spatialMoleculeBackend(object, file)
  if (!is.null(spatial_backend)) {
    payload$spatial_molecule_backend <- spatial_backend[c(
      "type",
      "location"
    )]
  }
  if (identical(backend$type, "embedded")) {
    return(payload)
  }
  if (is.null(sidecar)) {
    sidecar <- .cerebroSidecar(file, backend)
  }
  payload$expression <- NULL

  cells <- .h5CellNames(sidecar)
  metadata <- payload$meta_data
  if (
    !.validCellNames(cells) ||
      !is.data.frame(metadata) ||
      !"cell_barcode" %in% names(metadata) ||
      !identical(as.character(metadata$cell_barcode), cells)
  ) {
    stop(
      "The H5 sidecar and CRB metadata must contain the same cells in ",
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
  payload$cell_fingerprint <- .cerebroCellFingerprint(cells)
  indexed <- .indexThinPayloadByCell(payload, cells)
  payload$crb_schema <- list(
    version = 3L,
    cell_names_hash = .cerebroCellNamesHash(cells),
    projection_rownames = compact_projections,
    n_cells = length(cells)
  )
  ## Version 4 only when something is stored by cell index, so that a release
  ## that reads version 3 refuses it instead of showing rows without names.
  if (length(indexed)) {
    payload$crb_schema$version <- 4L
    payload$crb_schema$cell_indexed <- indexed
  }
  payload
}

.attachCerebroExpression <- function(object, file, backend) {
  if (identical(backend$type, "embedded")) {
    return(object)
  }
  sidecar <- .cerebroSidecar(file, backend)
  .verifyExpressionSidecar(sidecar, backend$md5, file)
  ## Check the installation without loading the namespace: loading HDF5Array
  ## and its S4 methods, then reading every cell name for the TENxMatrix, takes
  ## seconds. The matrix is attached on first use of `expression`.
  if (!nzchar(system.file(package = "HDF5Array"))) {
    stop("An H5-backed CRB requires the HDF5Array package.", call. = FALSE)
  }
  delayedAssign(
    "expression",
    t(HDF5Array::TENxMatrix(sidecar, group = "expression")),
    assign.env = object
  )
  object
}

.hydrateThinCerebro <- function(object, file, backend, schema) {
  if (is.null(schema)) {
    return(object)
  }
  if (!identical(backend$type, "h5")) {
    stop("A thin CRB requires an H5 expression backend.", call. = FALSE)
  }
  ## The H5 file's cell names, read with rhdf5 so that the expression matrix
  ## stays unattached.
  cells <- .h5CellNames(.cerebroSidecar(file, backend))
  if (!identical(.cerebroCellNamesHash(cells), schema$cell_names_hash)) {
    stop(
      "The H5 cell names do not match CRB '",
      basename(file),
      "'.",
      call. = FALSE
    )
  }
  if (!identical(length(cells), schema$n_cells)) {
    stop(
      "The thin CRB cell count does not match its H5 sidecar.",
      call. = FALSE
    )
  }
  metadata <- object$meta_data
  if (
    !.validCellNames(cells) ||
      !is.data.frame(metadata) ||
      nrow(metadata) != length(cells) ||
      "cell_barcode" %in% names(metadata)
  ) {
    stop(
      "The thin CRB and H5 sidecar have incompatible cell metadata.",
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
    ## `cells` passed .validCellNames() above; rownames<- would look for
    ## duplicates again for every projection.
    attr(projection, "row.names") <- cells
    object$projections[[name]] <- projection
  }
  rownames(metadata) <- NULL
  object$meta_data <- data.frame(
    cell_barcode = cells,
    metadata,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  ## A repertoire in its sidecar gets its barcodes back when it is read.
  if (length(schema$cell_indexed)) {
    object <- .restoreThinPayloadByCell(object, cells)
  }
  object
}

.writeCerebroPayload <- function(object, file, codec) {
  if (identical(codec, "qs2")) {
    qs2::qs_save(object, file)
  } else {
    saveRDS(object, file)
  }
}

.cerebroPayloadCodec <- function(file) {
  connection <- file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  magic <- readBin(connection, "raw", n = 4L)
  if (identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))) {
    return("qs2")
  }
  "rds"
}

.readCerebroPayload <- function(file) {
  if (identical(.cerebroPayloadCodec(file), "qs2")) {
    return(qs2::qs_read(file))
  }
  readRDS(file)
}

.spatialMoleculeSidecarName <- function(file) {
  paste0(tools::file_path_sans_ext(basename(file)), ".spatial")
}

.spatialMoleculeBackend <- function(object, file) {
  backend <- .storedCerebroDescriptor(
    object,
    "spatial_molecule_backend",
    "The spatial molecule backend descriptor is invalid."
  )
  if (is.null(backend)) {
    return(NULL)
  }
  valid <- is.list(backend) &&
    identical(backend$type, "directory") &&
    .isString(backend$location) &&
    !backend$location %in% c(".", "..") &&
    !grepl("[/\\\\]", backend$location)
  if (!valid) {
    stop("The spatial molecule backend descriptor is invalid.", call. = FALSE)
  }
  root <- backend$root
  if (!.isString(root)) {
    root <- file.path(
      dirname(normalizePath(file, mustWork = FALSE)),
      backend$location
    )
  }
  list(type = "directory", location = backend$location, root = root)
}

.stageSpatialMolecules <- function(
  object,
  file,
  codec,
  source_backend = .spatialMoleculeBackend(object, file)
) {
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

  location <- .spatialMoleculeSidecarName(file)
  stage <- tempfile(".spatial-s-", dirname(file))
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
  spatial = NULL,
  immune_stage = NULL,
  immune = NULL
) {
  backup <- NULL
  spatial_backup <- NULL
  immune_backup <- NULL
  backed_up <- FALSE
  backed_up_spatial <- FALSE
  backed_up_immune <- FALSE
  installed <- FALSE
  installed_spatial <- FALSE
  installed_immune <- FALSE
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
        if (installed_immune && file.exists(immune)) {
          unlink(immune, force = TRUE)
        }
        if (
          backed_up_immune &&
            file.exists(immune_backup) &&
            !file.rename(immune_backup, immune)
        ) {
          warning(
            "Could not restore the original immune repertoire sidecar; ",
            "preserved at: ",
            immune_backup,
            call. = FALSE
          )
        }
      }
    },
    add = TRUE
  )

  if (file.exists(file)) {
    backup <- tempfile(".crb-b-", dirname(file))
    if (!file.rename(file, backup)) {
      stop("Could not preserve the existing CRB.", call. = FALSE)
    }
    backed_up <- TRUE
  }
  if (!is.null(spatial_stage)) {
    if (dir.exists(spatial)) {
      spatial_backup <- tempfile(
        ".spatial-b-",
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
  if (!is.null(immune_stage)) {
    if (file.exists(immune)) {
      immune_backup <- tempfile(
        ".immune-b-",
        dirname(immune)
      )
      if (!file.rename(immune, immune_backup)) {
        stop(
          "Could not preserve the existing immune repertoire sidecar.",
          call. = FALSE
        )
      }
      backed_up_immune <- TRUE
    }
    if (!file.rename(immune_stage, immune)) {
      stop("Could not install the immune repertoire sidecar.", call. = FALSE)
    }
    installed_immune <- TRUE
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
  if (!is.null(immune_backup) && file.exists(immune_backup)) {
    unlink(immune_backup)
  }
  invisible(file)
}

## Moves a large repertoire out of the CRB into `<stem>.immune.<qs2|rds>` next
## to it. "auto" does so only for H5-backed CRBs, whose metadata and expression
## already live outside the serialised object; a repertoire that is already
## file-backed keeps its sidecar.
.stageImmuneRepertoire <- function(
  payload,
  file,
  codec,
  mode = "auto",
  threshold = 100000L
) {
  none <- list(payload = payload, stage = NULL, location = NULL)
  repertoire <- payload$immune_repertoire
  if (!is.list(repertoire) || !length(repertoire) || identical(mode, "never")) {
    return(none)
  }
  external <- identical(payload$expression_backend$type, "h5")
  large <- .immuneRepertoireRows(repertoire) >= threshold
  if (!(identical(mode, "always") || (external && large))) {
    return(none)
  }
  location <- .immuneRepertoireSidecarName(file, codec)
  stage <- tempfile(paste0(".", location, "-"), dirname(file))
  tryCatch(
    .writeCerebroPayload(repertoire, stage, codec),
    error = function(error) {
      unlink(stage, force = TRUE)
      stop(error)
    }
  )
  payload$immune_repertoire <- list()
  payload$immune_repertoire_backend <- list(
    type = "file",
    file = location,
    md5 = unname(tools::md5sum(stage)),
    samples = names(repertoire),
    chains = .immuneRepertoireChains(repertoire)
  )
  list(payload = payload, stage = stage, location = location)
}

.removeRetiredSidecar <- function(path) {
  if (!file.exists(path) && !dir.exists(path)) {
    return(invisible(NULL))
  }
  if (.pathIsSymbolicLink(path)) {
    warning(
      "The previous sidecar is a symbolic link and was not removed: ",
      path,
      call. = FALSE
    )
  } else if (!identical(unlink(path, recursive = TRUE, force = TRUE), 0L)) {
    warning(
      "The new file was published, but the previous sidecar remains at: ",
      path,
      call. = FALSE
    )
  }
  invisible(NULL)
}

.immuneSidecarOptions <- function(mode, threshold) {
  mode <- match.arg(mode, c("auto", "always", "never"))
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1L ||
      is.na(threshold) ||
      threshold < 0
  ) {
    stop(
      "`immune_sidecar_threshold` must be one non-negative number.",
      call. = FALSE
    )
  }
  list(mode = mode, threshold = threshold)
}

#' Save a Cerebro data file
#'
#' Saves a Cerebro object. An object whose expression matrix sits in an H5
#' sidecar is saved as a thin CRB: the cell names stay in the H5 file and the
#' CRB keeps only their order and a checksum. Spatial molecule tables go to a
#' sibling per-FOV sidecar, and a large immune repertoire to a sibling file (see
#' \code{immune_sidecar}). The input object and existing sidecars are not
#' modified.
#'
#' @param object A Cerebro object.
#' @param file Output \code{.crb} path.
#' @param codec Serialization codec. Defaults to \code{"qs2"}; use
#' \code{"rds"} when direct compatibility with \code{readRDS()} is required.
#' @param viewer_binary Build the derived Viewer Pack \code{"auto"}matically
#' for large datasets, \code{"always"}, or \code{"never"}.
#' @param viewer_binary_threshold Dataset-level cell threshold used by
#' \code{viewer_binary = "auto"}.
#' @param immune_sidecar Store the immune repertoire in a sibling file
#' \code{<stem>.immune.qs2} (\code{.rds} with \code{codec = "rds"}) instead of
#' inside the CRB. \code{"auto"} does so for H5-backed CRBs whose repertoire has
#' at least \code{immune_sidecar_threshold} rows; \code{"always"} and
#' \code{"never"} override that. The Viewer then lists samples and chains
#' without reading the repertoire, and loads it when the Immune repertoire page
#' needs it. The sidecar must travel with the CRB. An object whose repertoire
#' already sits in a sidecar keeps pointing to that file whatever the mode is (a
#' copy saved under another name shares it); to move it back into the CRB, call
#' \code{addImmuneRepertoire(object$getImmuneRepertoire())} first. A sidecar of
#' the CRB being replaced that the new CRB no longer uses is removed.
#' @param immune_sidecar_threshold Number of repertoire rows (cells with
#' receptor data) used by \code{immune_sidecar = "auto"}.
#' @return The output path, invisibly.
#' @export
saveCerebro <- function(
  object,
  file,
  codec = c("qs2", "rds"),
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  immune_sidecar = c("auto", "always", "never"),
  immune_sidecar_threshold = 100000L
) {
  codec <- match.arg(codec)
  viewer_options <- .viewerPackOptions(viewer_binary, viewer_binary_threshold)
  immune_options <- .immuneSidecarOptions(
    match.arg(immune_sidecar),
    immune_sidecar_threshold
  )
  if (!.isString(file)) {
    stop("`file` must be one non-empty output path.", call. = FALSE)
  }
  if (dir.exists(file)) {
    stop("`file` must not be a directory.", call. = FALSE)
  }
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dirname(file))) {
    stop("Could not create the CRB output directory.", call. = FALSE)
  }

  ## Only a CRB that sits next to one of its own sidecars needs to be read.
  previous_immune <- if (
    any(file.exists(file.path(
      dirname(file),
      c(
        .immuneRepertoireSidecarName(file, "qs2"),
        .immuneRepertoireSidecarName(file, "rds")
      )
    )))
  ) {
    .readPublishedImmuneBackend(file)
  }
  ## The thin payload keeps no absolute root, so where the sidecar sits now is
  ## taken from the object: saving to another directory still finds it.
  source_spatial_backend <- .spatialMoleculeBackend(object, file)
  payload <- .thinCerebroPayload(object, file)
  immune <- NULL
  spatial <- NULL
  stage <- tempfile(paste0(".", basename(file), "-"), dirname(file))
  ## Registered before the staging calls: a later step that fails must not
  ## leave the staged sidecars behind.
  on.exit(
    {
      unlink(stage, force = TRUE)
      unlink(immune$stage, force = TRUE)
      if (!is.null(spatial$stage)) {
        unlink(spatial$stage, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  immune <- .stageImmuneRepertoire(
    payload,
    file,
    codec,
    immune_options$mode,
    immune_options$threshold
  )
  payload <- immune$payload
  spatial <- .stageSpatialMolecules(
    payload,
    file,
    codec,
    source_backend = source_spatial_backend
  )
  payload <- spatial$payload
  .writeCerebroPayload(payload, stage, codec)
  retired_immune <- .retiredImmuneSidecar(
    previous_immune,
    file,
    immune$location %||% payload$immune_repertoire_backend$file
  )
  .installCerebroPayload(
    stage,
    file,
    spatial_stage = spatial$stage,
    spatial = if (is.null(spatial$location)) {
      NULL
    } else {
      file.path(dirname(file), spatial$location)
    },
    immune_stage = immune$stage,
    immune = if (is.null(immune$location)) {
      NULL
    } else {
      file.path(dirname(file), immune$location)
    }
  )
  ## The sidecar the saved CRB points to: a new one, or the one a file-backed
  ## repertoire keeps.
  if (!is.null(retired_immune)) {
    .removeRetiredSidecar(retired_immune)
  }
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

## What readCerebro() does after the checks; the Builder's read-back calls it on
## a payload it has read itself.
.hydrateCerebroPayload <- function(serialized, file) {
  file <- normalizePath(file, mustWork = TRUE)
  backend <- .cerebroBackend(serialized)
  schema <- .crbContract()$.readRuntimeCrbSchema(serialized, file)
  object <- .currentCerebroCopy(serialized)
  object <- .attachCerebroExpression(object, file, backend)
  object <- .hydrateThinCerebro(object, file, backend, schema)
  object <- .attachCerebroSpatialMolecules(object, file)
  .attachCerebroImmuneRepertoire(object, file)
}

#' Read a Cerebro data file
#'
#' Reads a CRB written by \code{saveCerebro()} or the export functions, as RDS or
#' qs2. An H5-backed expression matrix is attached lazily and a thin CRB gets
#' its cell names back from the H5 file; spatial molecule tables stay lazy until
#' their FOV is requested. A CRB from an older release that this version no
#' longer reads (for example one without an expression-backend descriptor,
#' or one with a singular spatial image or an embedded spatial molecule table)
#' is rejected with a pointer to \code{\link{convertCerebro}}.
#'
#' @param file Input \code{.crb} path.
#' @return A current Cerebro object.
#' @export
readCerebro <- function(file) {
  file <- normalizePath(file, mustWork = TRUE)
  serialized <- .readCerebroPayload(file)
  if (!.recognizedCerebroObject(serialized)) {
    stop("`object` must be a serialized Cerebro object.", call. = FALSE)
  }
  .crbContract()$.assertCurrentRuntimeCrb(serialized, file)
  .hydrateCerebroPayload(serialized, file)
}

## The one place that still knows the singular spatial image form: one
## `histology_image` (+ `histology_image_bounds`) becomes the image `Tissue
## background` of `histology_images`, which the current normalizer then checks.
## Bounds that were stored as a named list become the numeric vector it expects.
.upgradeLegacySpatialEntry <- function(entry) {
  singular <- c("histology_image", "histology_image_bounds")
  if (!is.list(entry) || !any(singular %in% names(entry))) {
    return(entry)
  }
  if (
    is.null(entry[["histology_images"]]) && !is.null(entry[["histology_image"]])
  ) {
    bounds <- entry[["histology_image_bounds"]]
    if (is.list(bounds)) {
      bounds <- unlist(bounds)
    }
    entry[["histology_images"]] <- list(
      `Tissue background` = list(
        histology_image = entry[["histology_image"]],
        histology_image_bounds = bounds
      )
    )
  }
  entry[singular] <- NULL
  entry
}

## Reads a CRB from an older release into a current object, repairing what can
## be repaired without the original data: a missing expression-backend
## descriptor means the matrix is embedded, receptors kept in `bcr_data` /
## `tcr_data` are merged into the unified repertoire, and a singular spatial
## image becomes `histology_images`. Molecule tables embedded in a spatial entry
## move into the sidecar when the object is saved again. A CRB whose expression
## storage is older than the formats this version reads cannot be repaired.
.readLegacyCerebro <- function(file) {
  serialized <- .readCerebroPayload(file)
  if (!.recognizedCerebroObject(serialized)) {
    stop("`object` must be a serialized Cerebro object.", call. = FALSE)
  }
  stored <- function(field) {
    if (.storedCerebroField(serialized, field)) serialized[[field]] else NULL
  }
  backend <- stored("expression_backend")
  schema <- stored("crb_schema")
  immune <- stored("immune_repertoire_backend")
  if (!is.null(backend) && !is.list(backend)) {
    stop(
      "The Cerebro data file '",
      basename(file),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  unrepairable <- (is.list(backend) &&
    !(is.character(backend$type) &&
      length(backend$type) == 1L &&
      backend$type %in% .crbContract()$.runtimeBackendTypes)) ||
    (is.list(schema) &&
      !(is.integer(schema$version) &&
        length(schema$version) == 1L &&
        schema$version %in% .crbContract()$.runtimeThinSchemaVersions)) ||
    (is.list(immune) && !identical(immune$type, "file"))
  if (unrepairable) {
    stop(
      "The Cerebro data file '",
      basename(file),
      "' stores its expression matrix in a layout this version cannot read ",
      "or repair. Re-export it from the source object with ",
      "exportFromSeurat(expression_matrix_mode = \"h5\").",
      call. = FALSE
    )
  }
  object <- .currentCerebroCopy(serialized)
  if (is.null(backend)) {
    object$setExpressionBackend("embedded")
  }
  if (!length(object$immune_repertoire)) {
    receptors <- Filter(
      function(part) is.list(part) && length(part) > 0L,
      list(stored("tcr_data"), stored("bcr_data"))
    )
    if (length(receptors)) {
      object$immune_repertoire <- do.call(.mergeRepertoiresBySample, receptors)
    }
  }
  for (name in names(object$spatial)) {
    entry <- object$spatial[[name]]
    upgraded <- .upgradeLegacySpatialEntry(entry)
    if (!identical(upgraded, entry)) {
      object$addSpatialData(name, upgraded)
    }
  }
  backend <- .cerebroBackend(object)
  object <- .attachCerebroExpression(object, file, backend)
  object <- .hydrateThinCerebro(
    object,
    file,
    backend,
    .crbContract()$.readRuntimeCrbSchema(serialized, file)
  )
  object <- .attachCerebroSpatialMolecules(object, file)
  .attachCerebroImmuneRepertoire(object, file)
}

#' Convert a Cerebro data file
#'
#' Re-serializes a CRB while reusing its existing expression sidecar. This is
#' also how a CRB from an older release is brought up to date: a missing
#' expression-backend descriptor is set to embedded, receptor data in the retired
#' \code{bcr_data} / \code{tcr_data} fields is merged into the unified immune
#' repertoire, and an H5-backed CRB written without a thin schema is thinned.
#' An object written before spatial, HLA typing, Trekker or enriched-pathways
#' support is rebuilt on the current class, so the accessors it lacked exist.
#' A spatial entry with the singular \code{histology_image} fields becomes
#' \code{histology_images}, and a molecule table embedded in a spatial entry
#' moves into the per-FOV sidecar next to the CRB.
#' A CRB whose expression storage is older than the formats this version reads
#' (an unknown expression backend, thin schema 1 or 2) cannot be converted;
#' re-export it with \code{\link{exportFromSeurat}}.
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
  input <- normalizePath(input, winslash = "/", mustWork = TRUE)
  valid_output <-
    .isString(output)
  if (
    valid_output &&
      !identical(
        normalizePath(dirname(input), winslash = "/", mustWork = FALSE),
        normalizePath(dirname(output), winslash = "/", mustWork = FALSE)
      )
  ) {
    stop(
      "`input` and `output` must be in the same directory because ",
      "conversion reuses the existing expression sidecar.",
      call. = FALSE
    )
  }
  saveCerebro(.readLegacyCerebro(input), output, codec = codec)
}
