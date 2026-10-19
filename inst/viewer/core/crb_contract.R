## The CRB reading contract shared by the package (readCerebro,
## createShinyApp) and the Viewer runtime. Pure functions only: the package
## loads this file into its own environment, the Viewer once per process.

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
  valid <- is.character(path) &&
    length(path) == 1L &&
    !is.na(path) &&
    nzchar(path) &&
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
    unsupported()
  }
  schema
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
  if (!getter_exists && !field_exists) {
    return(list(type = "embedded", location = NULL, legacy = TRUE))
  }
  if (!is.function(get(getter_name, envir = obj, inherits = FALSE))) {
    unsupported()
  }
  backend <- get(field_name, envir = obj, inherits = FALSE)
  if (is.null(backend)) {
    return(list(type = "embedded", location = NULL, legacy = FALSE))
  }
  valid_type <- is.list(backend) &&
    is.character(backend$type) &&
    length(backend$type) == 1L &&
    !is.na(backend$type) &&
    backend$type %in% c("embedded", "h5", "bpcells")
  valid_location <- valid_type &&
    if (identical(backend$type, "embedded")) {
      is.null(backend$location)
    } else {
      is.character(backend$location) &&
        length(backend$location) == 1L &&
        !is.na(backend$location) &&
        nzchar(backend$location)
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
  list(type = backend$type, location = backend$location, legacy = FALSE)
}

## The host-override option a backend descriptor uses, or NULL. Legacy files
## (no descriptor) prefer an h5 override over a BPCells one.
.runtimeBackendOverrideKey <- function(backend, options) {
  if (isTRUE(backend$legacy)) {
    if (!is.null(options[["expression_matrix_h5"]])) {
      return("expression_matrix_h5")
    }
    if (!is.null(options[["expression_matrix_BPCells"]])) {
      return("expression_matrix_BPCells")
    }
    return(NULL)
  }
  if (identical(backend$type, "embedded")) {
    return(NULL)
  }
  key <- switch(
    backend$type,
    h5 = "expression_matrix_h5",
    bpcells = "expression_matrix_BPCells"
  )
  if (is.null(options[[key]])) {
    return(NULL)
  }
  key
}

## The post-override backend plan of a descriptor. `override_location(path,
## key)` turns the override option into the plan's location.
.effectiveRuntimeBackendPlan <- function(
  backend,
  options,
  override_location = function(path, key) path
) {
  override_key <- .runtimeBackendOverrideKey(backend, options)
  if (!is.null(override_key)) {
    type <- if (isTRUE(backend$legacy)) {
      switch(
        override_key,
        expression_matrix_h5 = "h5",
        expression_matrix_BPCells = "bpcells"
      )
    } else {
      backend$type
    }
    return(list(
      type = type,
      mode = "host_override",
      location = override_location(options[[override_key]], override_key)
    ))
  }
  if (isTRUE(backend$legacy) || identical(backend$type, "embedded")) {
    return(list(type = "embedded", mode = "embedded", location = NULL))
  }
  list(type = backend$type, mode = "bundled", location = backend$location)
}
