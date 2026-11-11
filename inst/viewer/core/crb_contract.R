## The CRB reading contract shared by the package (readCerebro,
## createShinyApp) and the Viewer runtime. Pure functions only: the package
## loads this file into its own environment, the Viewer once per process.

## Expression backend types a CRB may declare; the one list for the package and
## the Viewer.
.runtimeBackendTypes <- c("embedded", "h5")

## TRUE for one string that is neither missing nor empty.
.runtimeIsString <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.runtimeWindowsPathSegmentInvalid <- function(parts) {
  grepl("[[:cntrl:]<>:\"|?*]", parts) |
    grepl("[. ]$", parts) |
    grepl(
      "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])($|\\.)",
      parts,
      ignore.case = TRUE
    )
}

.runtimePortableBackendPath <- function(
  path,
  context,
  requirement = "must be a portable relative path."
) {
  valid <- .runtimeIsString(path) &&
    !grepl("/$", path) &&
    !grepl("\\\\", path) &&
    !grepl("^(/|~|[A-Za-z]:)", path)
  parts <- if (valid) {
    strsplit(path, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  windows_invalid <- length(parts) > 0L &&
    any(.runtimeWindowsPathSegmentInvalid(parts))
  if (
    !valid ||
      length(parts) == 0L ||
      any(!nzchar(parts)) ||
      any(parts %in% c(".", "..")) ||
      windows_invalid
  ) {
    stop(context, " ", requirement, call. = FALSE)
  }
  paste(parts, collapse = "/")
}

## Thin-CRB schema versions the readers open: 3 keeps the cell names once, in
## the H5 file; 4 also stores repertoire barcodes and the row names of
## trajectories and spatial coordinates by cell index (`cell_indexed` names
## which fields). A release that reads only 3 refuses 4, and one that does not
## know a field named there refuses the file.
.runtimeThinSchemaVersions <- c(3L, 4L)
.runtimeCellIndexedFields <- c("immune_repertoire", "trajectories", "spatial")

.readRuntimeCrbSchema <- function(obj, crb_path) {
  field <- "crb_schema"
  if (!is.environment(obj) || !exists(field, envir = obj, inherits = FALSE)) {
    return(NULL)
  }
  unsupported <- function() {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported CRB schema descriptor.",
      call. = FALSE
    )
  }
  if (
    bindingIsActive(field, obj) ||
      isTRUE(rlang::env_binding_are_lazy(obj, field))
  ) {
    unsupported()
  }
  schema <- obj[[field]]
  if (is.null(schema)) {
    return(NULL)
  }
  schema_names <- names(schema)
  expected_names <- c(
    "version",
    "cell_names_hash",
    "projection_rownames",
    "n_cells",
    if (is.list(schema) && identical(schema$version, 4L)) "cell_indexed"
  )
  indexed <- if (is.list(schema)) schema$cell_indexed
  valid <- is.list(schema) &&
    !is.data.frame(schema) &&
    length(schema) == length(expected_names) &&
    !is.null(schema_names) &&
    !anyDuplicated(schema_names) &&
    setequal(schema_names, expected_names) &&
    is.integer(schema$version) &&
    length(schema$version) == 1L &&
    schema$version %in% .runtimeThinSchemaVersions &&
    (is.null(indexed) ||
      (is.character(indexed) &&
        length(indexed) > 0L &&
        !anyDuplicated(indexed) &&
        all(indexed %in% .runtimeCellIndexedFields))) &&
    is.character(schema$cell_names_hash) &&
    length(schema$cell_names_hash) == 1L &&
    !is.na(schema$cell_names_hash) &&
    grepl("^[[:xdigit:]]{32}$", schema$cell_names_hash) &&
    is.character(schema$projection_rownames) &&
    !anyNA(schema$projection_rownames) &&
    !any(!nzchar(schema$projection_rownames)) &&
    !anyDuplicated(schema$projection_rownames) &&
    is.integer(schema$n_cells) &&
    length(schema$n_cells) == 1L &&
    !is.na(schema$n_cells) &&
    schema$n_cells >= 0L
  if (!valid) {
    unsupported()
  }
  schema
}

## CRBs written by older releases that the readers no longer open, named once
## so readCerebro(), createShinyApp(), convertCerebro() and the Viewer reject
## the same forms with one message.
.runtimeLegacyReason <- function(obj) {
  is_stored <- function(field) {
    exists(field, envir = obj, inherits = FALSE) &&
      !bindingIsActive(field, obj) &&
      !isTRUE(rlang::env_binding_are_lazy(obj, field))
  }
  stored <- function(field) {
    if (is_stored(field)) obj[[field]] else NULL
  }
  no_backend <- "it has no expression-backend descriptor"
  if (!exists("expression_backend", envir = obj, inherits = FALSE)) {
    return(no_backend)
  }
  backend <- stored("expression_backend")
  if (is.null(backend)) {
    ## An active or lazy binding is malformed, not old; the readers report it.
    return(if (is_stored("expression_backend")) no_backend)
  }
  schema <- stored("crb_schema")
  if (
    is.list(schema) &&
      !(is.integer(schema$version) &&
        length(schema$version) == 1L &&
        schema$version %in% .runtimeThinSchemaVersions)
  ) {
    return(structure(
      paste0("it uses thin-CRB schema version ", schema$version[1L]),
      repairable = FALSE
    ))
  }
  if (length(stored("bcr_data")) > 0L || length(stored("tcr_data")) > 0L) {
    return("its receptors are stored in the retired bcr_data/tcr_data fields")
  }
  immune <- stored("immune_repertoire_backend")
  if (is.list(immune) && !identical(immune$type, "file")) {
    return(structure(
      "its immune repertoire sidecar uses a retired layout",
      repairable = FALSE
    ))
  }
  required <- .runtimeRequiredCerebroMethods()
  absent <- !vapply(
    names(required),
    exists,
    logical(1),
    envir = obj,
    inherits = FALSE
  )
  if (any(absent)) {
    return(paste0("it predates ", required[[which(absent)[[1L]]]]))
  }
  .runtimeLegacySpatialReason(obj)
}

## Accessor -> the feature that introduced it. A CRB whose object lacks one was
## written before that feature.
.runtimeRequiredCerebroMethods <- function() {
  c(
    availableSpatial = "spatial support",
    getHLATyping = "HLA typing support",
    getTrekker = "Trekker support",
    getMethodsForEnrichedPathways = "the enriched-pathways page"
  )
}

## Reason for a spatial entry in a retired form, or NULL: the singular
## `histology_image` / `histology_image_bounds` fields (now `histology_images`), a
## molecule table embedded in the CRB (now a per-FOV sidecar reference), or a
## `getSpatialData()` that cannot defer the molecules.
.runtimeLegacySpatialReason <- function(obj) {
  stored <- function(field) {
    if (
      exists(field, envir = obj, inherits = FALSE) &&
        !bindingIsActive(field, obj) &&
        !isTRUE(rlang::env_binding_are_lazy(obj, field))
    ) {
      obj[[field]]
    } else {
      NULL
    }
  }
  spatial <- stored("spatial")
  if (!is.list(spatial) || !length(spatial)) {
    return(NULL)
  }
  entry_names <- names(spatial)
  for (index in seq_along(spatial)) {
    entry <- spatial[[index]]
    if (!is.list(entry)) {
      next
    }
    label <- if (is.null(entry_names)) index else entry_names[[index]]
    if (any(c("histology_image", "histology_image_bounds") %in% names(entry))) {
      return(paste0(
        "its spatial entry '",
        label,
        "' stores a singular histology_image instead of histology_images"
      ))
    }
    molecules <- entry[["molecules"]]
    if (
      !is.null(molecules) && !inherits(molecules, "CerebroSpatialMoleculeRef")
    ) {
      return(paste0(
        "its spatial entry '",
        label,
        "' embeds its molecule table instead of using the spatial sidecar"
      ))
    }
  }
  getter <- stored("getSpatialData")
  if (
    !is.function(getter) || !"hydrate_molecules" %in% names(formals(getter))
  ) {
    return("it predates deferred spatial molecule loading")
  }
  NULL
}

.assertCurrentRuntimeCrb <- function(obj, crb_path) {
  reason <- .runtimeLegacyReason(obj)
  if (!is.null(reason)) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' uses a retired format: ",
      reason,
      ". ",
      if (isFALSE(attr(reason, "repairable"))) {
        paste0(
          "convertCerebro() cannot repair it; re-export it from the source ",
          "object with exportFromSeurat(expression_matrix_mode = \"h5\")."
        )
      } else {
        "Convert it once with CerebroNexus::convertCerebro()."
      },
      call. = FALSE
    )
  }
  invisible(obj)
}

## Treat the ordinary field as data. The getter binding is checked only as a
## format marker and is never invoked. `portable = FALSE` leaves the location
## check to the caller.
.readRuntimeBackendDescriptor <- function(obj, crb_path, portable = TRUE) {
  unsupported <- function() {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  if (!is.environment(obj)) {
    unsupported()
  }
  getter_name <- "getExpressionBackend"
  field_name <- "expression_backend"
  getter_exists <- exists(getter_name, envir = obj, inherits = FALSE)
  field_exists <- exists(field_name, envir = obj, inherits = FALSE)
  getter_active <- getter_exists && bindingIsActive(getter_name, obj)
  field_active <- field_exists && bindingIsActive(field_name, obj)
  getter_lazy <- getter_exists &&
    !getter_active &&
    isTRUE(rlang::env_binding_are_lazy(obj, getter_name))
  field_lazy <- field_exists &&
    !field_active &&
    isTRUE(rlang::env_binding_are_lazy(obj, field_name))
  if (
    getter_active ||
      field_active ||
      getter_lazy ||
      field_lazy ||
      xor(getter_exists, field_exists)
  ) {
    unsupported()
  }
  .assertCurrentRuntimeCrb(obj, crb_path)
  if (!is.function(get(getter_name, envir = obj, inherits = FALSE))) {
    unsupported()
  }
  backend <- get(field_name, envir = obj, inherits = FALSE)
  valid_type <- is.list(backend) &&
    is.character(backend$type) &&
    length(backend$type) == 1L &&
    !is.na(backend$type) &&
    backend$type %in% .runtimeBackendTypes
  valid_location <- valid_type &&
    if (identical(backend$type, "embedded")) {
      is.null(backend$location)
    } else {
      .runtimeIsString(backend$location)
    }
  if (!valid_type || !valid_location) {
    unsupported()
  }
  if (portable && !identical(backend$type, "embedded")) {
    backend$location <- .runtimePortableBackendPath(
      backend$location,
      paste0("The ", backend$type, " backend location")
    )
  }
  list(type = backend$type, location = backend$location)
}

## The backend plan of a descriptor. createShinyApp() serializes it so the
## runtime consumes the same decision that preflight validated.
.effectiveRuntimeBackendPlan <- function(backend) {
  if (identical(backend$type, "embedded")) {
    return(list(type = "embedded", mode = "embedded", location = NULL))
  }
  list(type = backend$type, mode = "bundled", location = backend$location)
}
