#' Remove Common Leading Whitespace from a String
#'
#' Eliminates the minimal common indentation shared by all non-empty lines of
#' the input, preserving relative indentation within blocks.
#'
#' @param string A character string containing text with indentation.
#' @return A dedented character string.
#' @keywords internal
#' @noRd
dedent <- function(string) {
  if (!is.character(string) || length(string) != 1) {
    stop("Input must be a single character string")
  }
  lines <- strsplit(string, "\n", fixed = TRUE)[[1]]
  while (length(lines) > 0 && grepl("^\\s*$", lines[1])) {
    lines <- lines[-1]
  }
  while (length(lines) > 0 && grepl("^\\s*$", lines[length(lines)])) {
    lines <- lines[-length(lines)]
  }
  if (length(lines) == 0) {
    return("")
  }
  non_empty_lines <- lines[!grepl("^\\s*$", lines)]
  if (length(non_empty_lines) == 0) {
    return("")
  }
  lead_spaces <- vapply(
    non_empty_lines,
    function(line) {
      m <- regmatches(line, regexpr("^\\s*", line))
      nchar(m)
    },
    integer(1)
  )
  min_indent <- min(lead_spaces)
  if (min_indent > 0) {
    pat <- paste0("^\\s{", min_indent, "}")
    lines <- vapply(
      lines,
      function(line) {
        if (grepl("^\\s*$", line)) line else sub(pat, "", line)
      },
      character(1)
    )
  }
  paste(lines, collapse = "\n")
}

.normalizeDatasetNumericOption <- function(
  value,
  data_labels,
  argument,
  minimum,
  maximum
) {
  supplied_as_list <- is.list(value)
  if (supplied_as_list) {
    scalar_numeric <- vapply(
      value,
      function(x) is.numeric(x) && length(x) == 1L,
      logical(1)
    )
    if (!all(scalar_numeric)) {
      stop("'", argument, "' values must be numeric scalars.", call. = FALSE)
    }
    value <- unlist(value, use.names = TRUE)
  }
  if (!is.numeric(value) || length(value) == 0L) {
    stop("'", argument, "' must be numeric.", call. = FALSE)
  }
  if (length(value) == 1L && !supplied_as_list) {
    value <- rep(unname(value), length(data_labels))
    names(value) <- data_labels
  } else {
    if (
      is.null(names(value)) || anyNA(names(value)) || any(names(value) == "")
    ) {
      stop(
        "'",
        argument,
        "' must be named when it has multiple values.",
        call. = FALSE
      )
    }
    if (
      anyDuplicated(names(value)) ||
        !setequal(names(value), data_labels) ||
        length(value) != length(data_labels)
    ) {
      stop(
        "'",
        argument,
        "' names must exactly match cerebro_data labels.",
        call. = FALSE
      )
    }
    value <- value[data_labels]
  }
  if (
    anyNA(value) ||
      any(!is.finite(value)) ||
      any(value < minimum | value > maximum)
  ) {
    stop(
      "'",
      argument,
      "' values must be between ",
      minimum,
      " and ",
      maximum,
      ".",
      call. = FALSE
    )
  }
  value
}

# Generated app run-option manifest ----------------------------------------

.bundleRunOptions <- function(
  max_request_size,
  port,
  host,
  launch_browser,
  quiet,
  display_mode,
  show_upload_ui
) {
  if (
    !is.numeric(max_request_size) ||
      length(max_request_size) != 1L ||
      is.na(max_request_size) ||
      !is.finite(max_request_size) ||
      max_request_size <= 0
  ) {
    stop(
      "'max_request_size' must be one finite numeric value greater than zero.",
      call. = FALSE
    )
  }
  max_request_size_bytes <- as.double(max_request_size * 1024^2)
  if (!is.finite(max_request_size_bytes)) {
    stop(
      "'max_request_size' must produce a finite number of bytes.",
      call. = FALSE
    )
  }
  if (
    !is.numeric(port) ||
      length(port) != 1L ||
      is.na(port) ||
      !is.finite(port) ||
      port != floor(port) ||
      port < 1 ||
      port > 65535
  ) {
    stop(
      "'port' must be one whole numeric value from 1 to 65535.",
      call. = FALSE
    )
  }
  if (
    !is.character(host) ||
      length(host) != 1L ||
      is.na(host) ||
      !nzchar(host)
  ) {
    stop(
      "'host' must be one non-empty, non-missing character value.",
      call. = FALSE
    )
  }
  validate_logical <- function(value, argument) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("'", argument, "' must be TRUE or FALSE.", call. = FALSE)
    }
  }
  validate_logical(launch_browser, "launch_browser")
  validate_logical(quiet, "quiet")
  validate_logical(show_upload_ui, "show_upload_ui")
  if (!show_upload_ui) {
    max_request_size_bytes <- min(
      max_request_size_bytes,
      as.double(6 * 1024^2)
    )
  }
  if (
    !is.character(display_mode) ||
      length(display_mode) != 1L ||
      is.na(display_mode) ||
      !display_mode %in% c("auto", "normal", "showcase")
  ) {
    stop(
      "'display_mode' must be one of 'auto', 'normal', or 'showcase'.",
      call. = FALSE
    )
  }

  list(
    schema_version = 1L,
    max_request_size_bytes = max_request_size_bytes,
    shiny_app_options = list(
      port = as.integer(port),
      host = host,
      launch.browser = launch_browser,
      quiet = quiet,
      display.mode = display_mode
    )
  )
}

.viewerInitialPageTabs <- function() {
  c(
    data_info = "loadData",
    projection = "overview",
    linked_views = "coordinated_views",
    groups = "groups",
    marker_genes = "markerGenes",
    most_expressed_genes = "mostExpressedGenes",
    enriched_pathways = "enrichedPathways",
    extra_material = "extra_material",
    immune_repertoire = "immune_repertoire",
    trajectory = "trajectory",
    spatial = "spatial",
    trekker = "trekker",
    hla_tcr_motifs = "hla_tcr_motifs",
    gene_expression = "geneExpression",
    gene_id_conversion = "geneIdConversion",
    color_management = "color_management",
    about = "about"
  )
}

# Portable bundle paths ----------------------------------------------------

.windowsPathSegmentInvalid <- function(parts) {
  grepl("[[:cntrl:]<>:\"|?*]", parts) |
    grepl("[. ]$", parts) |
    grepl(
      "^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])($|\\.)",
      parts,
      ignore.case = TRUE
    )
}

.portableBundlePath <- function(path, subject) {
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
    any(.windowsPathSegmentInvalid(parts))
  if (
    !valid ||
      length(parts) == 0L ||
      any(!nzchar(parts)) ||
      any(parts %in% c(".", "..")) ||
      windows_invalid
  ) {
    stop(
      subject,
      " must be one portable relative path using forward slashes, without ",
      "empty, '.', '..', or Windows-incompatible segments.",
      call. = FALSE
    )
  }
  paste(parts, collapse = "/")
}

# Native path resolution ---------------------------------------------------

.absolutePathStyle <- function(path, os_type = .Platform$OS.type) {
  slash_path <- gsub("\\", "/", path, fixed = TRUE)
  if (grepl("^//[?.]/", slash_path)) {
    return("windows-device")
  }
  if (grepl("^[A-Za-z]:/", slash_path)) {
    return("drive")
  }
  if (startsWith(slash_path, "//")) {
    if (grepl("^//[^/]+/[^/]+($|/)", slash_path)) {
      return("unc")
    }
    if (identical(os_type, "windows")) {
      return("windows-malformed")
    }
    return(NA_character_)
  }
  if (identical(os_type, "windows") && startsWith(slash_path, "/")) {
    return("windows-root")
  }
  if (startsWith(path, "/")) {
    return("posix")
  }
  NA_character_
}

.nativeAbsolutePath <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  if (is.na(style)) {
    return(FALSE)
  }
  if (identical(os_type, "windows")) {
    return(TRUE)
  }
  identical(style, "posix")
}

.nativeFilesystemStyle <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  if (
    !identical(os_type, "windows") &&
      startsWith(path, "/")
  ) {
    return("posix")
  }
  style
}

.lexicalAbsolutePath <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  slash_path <- if (
    identical(style, "posix") &&
      !identical(os_type, "windows")
  ) {
    path
  } else {
    gsub("\\", "/", path, fixed = TRUE)
  }
  if (identical(style, "drive")) {
    prefix <- paste0(substr(slash_path, 1L, 2L), "/")
    remainder <- sub("^[A-Za-z]:/+", "", slash_path)
  } else if (identical(style, "unc")) {
    unc_parts <- strsplit(sub("^/+", "", slash_path), "/", fixed = TRUE)[[1L]]
    prefix <- paste0("//", unc_parts[[1L]], "/", unc_parts[[2L]])
    remainder <- paste(unc_parts[-seq_len(2L)], collapse = "/")
  } else {
    prefix <- "/"
    remainder <- sub("^/+", "", slash_path)
  }

  parts <- strsplit(remainder, "/", fixed = TRUE)[[1L]]
  normalized <- character()
  for (part in parts) {
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      if (length(normalized) > 0L) {
        normalized <- normalized[-length(normalized)]
      }
      next
    }
    normalized <- c(normalized, part)
  }
  if (length(normalized) == 0L) {
    return(prefix)
  }
  paste0(
    prefix,
    if (endsWith(prefix, "/")) "" else "/",
    paste(
      normalized,
      collapse = "/"
    )
  )
}

.absolutePathComponents <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  slash_path <- if (
    identical(style, "posix") &&
      !identical(os_type, "windows")
  ) {
    path
  } else {
    gsub("\\", "/", path, fixed = TRUE)
  }
  if (identical(style, "drive")) {
    root <- paste0(substr(slash_path, 1L, 2L), "/")
    remainder <- sub("^[A-Za-z]:/+", "", slash_path)
  } else if (identical(style, "unc")) {
    unc_parts <- strsplit(
      sub("^/+", "", slash_path),
      "/",
      fixed = TRUE
    )[[1L]]
    root <- paste0("//", unc_parts[[1L]], "/", unc_parts[[2L]])
    remainder <- paste(unc_parts[-seq_len(2L)], collapse = "/")
  } else {
    root <- "/"
    remainder <- sub("^/+", "", slash_path)
  }
  parts <- if (nzchar(remainder)) {
    strsplit(remainder, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  list(root = root, parts = parts)
}

.composeAbsolutePath <- function(root, parts) {
  if (length(parts) == 0L) {
    return(root)
  }
  paste0(
    root,
    if (endsWith(root, "/")) "" else "/",
    paste(parts, collapse = "/")
  )
}

.relativeLinkComponents <- function(path, os_type = .Platform$OS.type) {
  if (identical(os_type, "windows")) {
    path <- gsub("\\", "/", path, fixed = TRUE)
  }
  strsplit(path, "/", fixed = TRUE)[[1L]]
}

.nativePathExists <- function(path) {
  file.exists(path) || dir.exists(path)
}

.readNativeLink <- function(
  path,
  os_type = .Platform$OS.type,
  is_link = function(candidate) fs::is_link(candidate),
  link_path = function(candidate) fs::link_path(candidate)
) {
  if (identical(os_type, "windows")) {
    linked <- is_link(path)
    if (length(linked) != 1L || is.na(linked) || !isTRUE(unname(linked))) {
      return("")
    }
    return(as.character(link_path(path))[[1L]])
  }
  Sys.readlink(path)
}

.normalizeExistingPath <- function(
  path,
  os_type = .Platform$OS.type,
  path_real = function(candidate) fs::path_real(candidate)
) {
  if (identical(os_type, "windows")) {
    return(as.character(path_real(path))[[1L]])
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.pathDirectoryEntryExists <- function(
  path,
  os_type = .Platform$OS.type,
  dir_exists = dir.exists,
  access_parent = function(parent) file.access(parent, mode = 5L),
  list_entries = function(parent) {
    list.files(parent, all.files = TRUE, no.. = TRUE)
  }
) {
  parent <- dirname(path)
  if (!isTRUE(dir_exists(parent))) {
    return(FALSE)
  }

  inspection_error <- NULL
  access_status <- tryCatch(
    access_parent(parent),
    error = function(error_condition) {
      inspection_error <<- conditionMessage(error_condition)
      NULL
    }
  )
  if (
    !is.null(inspection_error) ||
      length(access_status) != 1L ||
      is.na(access_status) ||
      access_status != 0L
  ) {
    stop(
      "The path cannot be resolved safely because the parent directory ",
      "could not be inspected.",
      call. = FALSE
    )
  }

  entries <- tryCatch(
    withCallingHandlers(
      list_entries(parent),
      warning = function(warning_condition) {
        inspection_error <<- conditionMessage(warning_condition)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) {
      inspection_error <<- conditionMessage(error_condition)
      NULL
    }
  )
  if (!is.null(inspection_error) || !is.character(entries)) {
    stop(
      "The path cannot be resolved safely because the parent directory ",
      "could not be inspected.",
      call. = FALSE
    )
  }

  needle <- basename(path)
  if (identical(os_type, "windows")) {
    needle <- tolower(needle)
    entries <- tolower(entries)
  }
  any(entries == needle)
}

## Resolve each component so intermediate symbolic links are reflected in the
## canonical path and callers can detect escape from an allowed root.
## Uninspectable filesystem entries fail closed.
.resolveNativeSymbolicLinks <- function(
  path,
  style = .absolutePathStyle(path),
  max_links = 64L,
  os_type = .Platform$OS.type,
  read_link = function(candidate) {
    .readNativeLink(candidate, os_type = os_type)
  },
  path_exists = .nativePathExists,
  normalize_existing = function(candidate) {
    .normalizeExistingPath(candidate, os_type = os_type)
  },
  entry_exists = function(candidate) {
    .pathDirectoryEntryExists(candidate, os_type = os_type)
  }
) {
  if (style %in% c("windows-device", "windows-malformed")) {
    detail <- if (identical(style, "windows-device")) {
      "device or extended-length"
    } else {
      "malformed Windows"
    }
    stop(
      "Windows ",
      detail,
      " path namespaces are not supported.",
      call. = FALSE
    )
  }
  parsed <- .absolutePathComponents(path, style, os_type = os_type)
  if (identical(os_type, "windows") && identical(style, "unc")) {
    unc_root_parts <- strsplit(
      sub("^/+", "", parsed$root),
      "/",
      fixed = TRUE
    )[[1L]]
    if (any(.windowsPathSegmentInvalid(unc_root_parts))) {
      stop(
        "Native Windows paths cannot contain Windows-incompatible segments.",
        call. = FALSE
      )
    }
  }
  root <- parsed$root
  resolved <- character()
  pending <- parsed$parts
  links_followed <- 0L

  if (isTRUE(path_exists(root))) {
    physical_root <- normalize_existing(root)
    physical_style <- .absolutePathStyle(physical_root, os_type = os_type)
    physical_style <- .nativeFilesystemStyle(
      physical_root,
      physical_style,
      os_type = os_type
    )
    physical_root <- .absolutePathComponents(
      physical_root,
      physical_style,
      os_type = os_type
    )
    root <- physical_root$root
    resolved <- physical_root$parts
  }

  while (length(pending) > 0L) {
    part <- pending[[1L]]
    pending <- pending[-1L]
    if (!nzchar(part) || identical(part, ".")) {
      next
    }
    if (identical(part, "..")) {
      if (length(resolved) > 0L) {
        resolved <- resolved[-length(resolved)]
      }
      next
    }
    if (
      identical(os_type, "windows") &&
        .windowsPathSegmentInvalid(part)
    ) {
      stop(
        "Native Windows paths cannot contain Windows-incompatible segments.",
        call. = FALSE
      )
    }

    candidate <- .composeAbsolutePath(root, c(resolved, part))
    link <- read_link(candidate)
    if (!is.na(link) && nzchar(link)) {
      links_followed <- links_followed + 1L
      if (links_followed > max_links) {
        stop(
          "Could not resolve the path because it contains too many ",
          "symbolic links.",
          call. = FALSE
        )
      }
      link_style <- .absolutePathStyle(link, os_type = os_type)
      native_absolute <- if (identical(os_type, "windows")) {
        !is.na(link_style)
      } else {
        startsWith(link, "/")
      }
      if (native_absolute) {
        if (!identical(os_type, "windows")) {
          link_style <- "posix"
        }
        link_parts <- .absolutePathComponents(
          link,
          link_style,
          os_type = os_type
        )
        root <- link_parts$root
        resolved <- character()
        pending <- c(link_parts$parts, pending)
      } else {
        pending <- c(
          .relativeLinkComponents(link, os_type = os_type),
          pending
        )
      }
      next
    }

    if (isTRUE(path_exists(candidate))) {
      physical <- normalize_existing(candidate)
      physical_style <- .absolutePathStyle(physical, os_type = os_type)
      physical_style <- .nativeFilesystemStyle(
        physical,
        physical_style,
        os_type = os_type
      )
      if (is.na(physical_style)) {
        stop(
          "The path cannot be resolved safely to an absolute native path.",
          call. = FALSE
        )
      }
      physical <- .absolutePathComponents(
        physical,
        physical_style,
        os_type = os_type
      )
      root <- physical$root
      resolved <- physical$parts
      next
    }
    if (isTRUE(entry_exists(candidate))) {
      stop(
        "The path cannot be resolved safely because it contains an ",
        "unresolved filesystem entry.",
        call. = FALSE
      )
    }
    resolved <- c(resolved, part)
  }

  .composeAbsolutePath(root, resolved)
}

.canonicalTargetPath <- function(path, os_type = .Platform$OS.type) {
  style <- .absolutePathStyle(path, os_type = os_type)
  if (style %in% c("windows-device", "windows-malformed")) {
    detail <- if (identical(style, "windows-device")) {
      "device or extended-length"
    } else {
      "malformed Windows"
    }
    stop(
      "Windows ",
      detail,
      " path namespaces are not supported.",
      call. = FALSE
    )
  }
  style <- .nativeFilesystemStyle(path, style, os_type = os_type)
  if (is.na(style)) {
    path <- file.path(getwd(), path)
    style <- .absolutePathStyle(path, os_type = os_type)
    style <- .nativeFilesystemStyle(path, style, os_type = os_type)
  }
  if (!.nativeAbsolutePath(path, style, os_type = os_type)) {
    return(.lexicalAbsolutePath(path, style, os_type = os_type))
  }
  path <- .resolveNativeSymbolicLinks(
    path,
    style,
    os_type = os_type
  )
  if (.nativePathExists(path)) {
    return(.normalizeExistingPath(path, os_type = os_type))
  }
  .lexicalAbsolutePath(
    path,
    .absolutePathStyle(path, os_type = os_type),
    os_type = os_type
  )
}

.normalizeOverridePath <- function(path, key) {
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !nzchar(path)
  ) {
    stop(
      "cerebro_options[['",
      key,
      "']] must be one non-empty absolute path.",
      call. = FALSE
    )
  }
  style <- .absolutePathStyle(path)
  if (style %in% c("windows-device", "windows-malformed")) {
    detail <- if (identical(style, "windows-device")) {
      "device or extended-length"
    } else {
      "malformed Windows"
    }
    stop(
      "Windows ",
      detail,
      " path namespaces are not supported.",
      call. = FALSE
    )
  }
  if (is.na(style)) {
    stop(
      "cerebro_options[['",
      key,
      "']] must be one absolute path.",
      call. = FALSE
    )
  }
  if (
    !identical(.Platform$OS.type, "windows") &&
      identical(style, "unc") &&
      startsWith(path, "/")
  ) {
    stop(
      "Forward-slash UNC paths are ambiguous outside Windows; use a ",
      "backslash UNC or drive path for a Windows host-managed override.",
      call. = FALSE
    )
  }
  if (!.nativeAbsolutePath(path, style)) {
    return(path)
  }
  .canonicalTargetPath(path)
}

.pathWithin <- function(
  candidate,
  parent,
  os_type = .Platform$OS.type,
  canonicalize = function(path) {
    .canonicalTargetPath(path, os_type = os_type)
  }
) {
  candidate_style <- .absolutePathStyle(candidate, os_type = os_type)
  candidate_style <- .nativeFilesystemStyle(
    candidate,
    candidate_style,
    os_type = os_type
  )
  if (!.nativeAbsolutePath(candidate, candidate_style, os_type = os_type)) {
    return(FALSE)
  }
  candidate <- canonicalize(candidate)
  parent <- canonicalize(parent)
  if (identical(os_type, "windows")) {
    candidate <- tolower(candidate)
    parent <- tolower(parent)
  }
  parent_prefix <- if (endsWith(parent, "/")) {
    parent
  } else {
    paste0(parent, "/")
  }
  identical(candidate, parent) || startsWith(candidate, parent_prefix)
}

# Backend descriptors and frozen plans ------------------------------------

.bundleRequiredCerebroMethods <- c(
  "print",
  "getVersion",
  "getExperiment",
  "getParameters",
  "getTechnicalInfo",
  "getMetaData",
  "getCellNames",
  "getCellCycle",
  "getGroups",
  "getGroupLevels",
  "getGeneLists",
  "getGeneNames",
  "availableProjections",
  "getProjection",
  "getExpressionMatrix",
  "getMethodsForMarkerGenes",
  "getGroupsWithMarkerGenes",
  "getMarkerGenes",
  "getGroupsWithMostExpressedGenes",
  "getMethodsForEnrichedPathways",
  "getExtraMaterialCategories",
  "getMethodsForTrajectories",
  "getNamesOfTrajectories",
  "getTrajectory"
)

.isRecognizedCerebroObject <- function(object) {
  is.environment(object) &&
    inherits(object, "R6") &&
    any(startsWith(class(object), "Cerebro")) &&
    environmentIsLocked(object)
}

.isPreSpatialCerebroV1_3 <- function(object) {
  ## b13fee58 added spatial storage and its accessors to Cerebro_v1.3. Its
  ## predecessor serializes with this exact R6 class identity, so only that
  ## historical format may omit the accessors. In particular, a damaged modern
  ## `Cerebro` object must never silently become a non-spatial dataset.
  identical(class(object), c("Cerebro_v1.3", "R6")) &&
    is.environment(object) &&
    environmentIsLocked(object)
}

## Treat the ordinary field as data. The getter binding is checked only as a
## format marker and is never invoked during preflight.
.readBundleBackend <- function(crb_path, object = readRDS(crb_path)) {
  recognized <- .isRecognizedCerebroObject(object)
  if (recognized) {
    for (method in .bundleRequiredCerebroMethods) {
      if (
        !exists(method, envir = object, inherits = FALSE) ||
          bindingIsActive(method, object) ||
          isTRUE(rlang::env_binding_are_lazy(object, method)) ||
          !is.function(object[[method]])
      ) {
        recognized <- FALSE
        break
      }
    }
  }
  if (!recognized) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' does not contain a recognized Cerebro object.",
      call. = FALSE
    )
  }
  getter_name <- "getExpressionBackend"
  backend_field <- "expression_backend"
  getter_exists <- exists(getter_name, envir = object, inherits = FALSE)
  field_exists <- exists(backend_field, envir = object, inherits = FALSE)
  getter_is_active <- getter_exists && bindingIsActive(getter_name, object)
  field_is_active <- field_exists && bindingIsActive(backend_field, object)
  getter_is_lazy <- getter_exists &&
    !getter_is_active &&
    isTRUE(rlang::env_binding_are_lazy(object, getter_name))
  field_is_lazy <- field_exists &&
    !field_is_active &&
    isTRUE(rlang::env_binding_are_lazy(object, backend_field))
  if (
    getter_is_active ||
      field_is_active ||
      getter_is_lazy ||
      field_is_lazy
  ) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  if (!getter_exists && !field_exists) {
    return(list(type = "embedded", location = NULL, legacy = TRUE))
  }
  if (
    !getter_exists ||
      !field_exists ||
      !is.function(object[[getter_name]])
  ) {
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  backend <- object[[backend_field]]
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
    stop(
      "The Cerebro data file '",
      basename(crb_path),
      "' has an unsupported expression-backend descriptor.",
      call. = FALSE
    )
  }
  backend$legacy <- FALSE
  backend
}

.readBundleSpatialCatalog <- function(object, dataset) {
  spatial_methods <- c("availableSpatial", "getSpatialData")
  method_exists <- vapply(
    spatial_methods,
    exists,
    logical(1),
    envir = object,
    inherits = FALSE
  )
  if (!any(method_exists)) {
    if (.isPreSpatialCerebroV1_3(object)) {
      ## Spatial accessors were added in b13fee58. The preceding
      ## Cerebro_v1.3 serialization is non-spatial, rather than malformed.
      return(list())
    }
    stop(
      "Dataset `",
      dataset,
      "` has no spatial accessor methods.",
      call. = FALSE
    )
  }
  valid_methods <- vapply(
    spatial_methods,
    function(method) {
      method_exists[[method]] &&
        !bindingIsActive(method, object) &&
        !isTRUE(rlang::env_binding_are_lazy(object, method)) &&
        is.function(object[[method]])
    },
    logical(1)
  )
  if (!all(valid_methods)) {
    stop(
      "Dataset `",
      dataset,
      "` has invalid spatial accessor methods.",
      call. = FALSE
    )
  }
  available <- tryCatch(
    object$availableSpatial(),
    error = function(error) {
      stop(
        "Could not read available spatial entries for dataset `",
        dataset,
        "`: ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )
  if (is.null(available)) {
    available <- character()
  }
  if (
    !is.character(available) ||
      anyNA(available) ||
      any(!nzchar(available)) ||
      anyDuplicated(available)
  ) {
    stop(
      "Dataset `",
      dataset,
      "` returned invalid available spatial entry names.",
      call. = FALSE
    )
  }
  catalog <- lapply(available, function(spatial_name) {
    data <- tryCatch(
      object$getSpatialData(spatial_name),
      error = function(error) {
        stop(
          "Could not read dataset `",
          dataset,
          "` spatial `",
          spatial_name,
          "`: ",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
    data <- .normalizeSpatialDataImages(data, spatial_name)
    images <- data[["histology_images"]]
    if (is.null(images)) character() else names(images)
  })
  names(catalog) <- available
  catalog
}

.preflightBundleData <- function(
  cerebro_data,
  read_object = readRDS,
  inspect_backend = .readBundleBackend,
  inspect_spatial = .readBundleSpatialCatalog,
  release_object = function(object) invisible(NULL)
) {
  backends <- vector("list", length(cerebro_data))
  spatial_catalogs <- vector("list", length(cerebro_data))
  names(backends) <- names(cerebro_data)
  names(spatial_catalogs) <- names(cerebro_data)
  for (index in seq_along(cerebro_data)) {
    object <- read_object(cerebro_data[[index]])
    inspection_error <- NULL
    release_error <- NULL
    tryCatch(
      {
        backends[[index]] <- inspect_backend(cerebro_data[[index]], object)
        spatial_catalogs[[index]] <- inspect_spatial(
          object,
          names(cerebro_data)[[index]]
        )
      },
      error = function(error) {
        inspection_error <<- error
      }
    )
    tryCatch(
      release_object(object),
      error = function(error) {
        release_error <<- error
      }
    )
    object <- NULL
    if (!is.null(inspection_error)) {
      stop(inspection_error)
    }
    if (!is.null(release_error)) {
      stop(release_error)
    }
  }
  list(backends = backends, spatial_catalogs = spatial_catalogs)
}

.spatialImageBundlePathDigest <- function(bytes) {
  path <- tempfile("cerebro-spatial-path-")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::md5sum(path))
}

.spatialImageBundlePathComponent <- function(value, maximum_bytes = 40L) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(
      "Spatial image bundle path components must be non-empty strings.",
      call. = FALSE
    )
  }
  bytes <- charToRaw(enc2utf8(value))
  encoded <- paste0(
    "u",
    paste(sprintf("%02x", as.integer(bytes)), collapse = "")
  )
  if (nchar(encoded, type = "bytes") <= maximum_bytes) {
    return(encoded)
  }
  digest <- .spatialImageBundlePathDigest(bytes)
  prefix_length <- maximum_bytes - nchar(digest, type = "bytes") - 1L
  paste0(substr(encoded, 1L, prefix_length), "-", digest)
}

.spatialImageBundleTarget <- function(
  dataset,
  spatial_name,
  image_label,
  filename
) {
  extension <- tools::file_ext(filename)
  extension_is_safe <- grepl("^[A-Za-z0-9]{1,16}$", extension)
  encoded_filename <- .spatialImageBundlePathComponent(
    filename,
    40L - if (extension_is_safe) nchar(extension, type = "bytes") + 1L else 0L
  )
  if (extension_is_safe) {
    encoded_filename <- paste0(encoded_filename, ".", extension)
  }
  target <- paste(
    "spatial-assets",
    .spatialImageBundlePathComponent(dataset),
    .spatialImageBundlePathComponent(spatial_name),
    .spatialImageBundlePathComponent(image_label),
    encoded_filename,
    sep = "/"
  )
  .portableBundlePath(
    target,
    paste0("The spatial image bundle target '", target, "'")
  )
}

.bundleBackendOverrideKey <- function(backend, cerebro_options) {
  if (isTRUE(backend$legacy)) {
    if (!is.null(cerebro_options[["expression_matrix_h5"]])) {
      return("expression_matrix_h5")
    }
    if (!is.null(cerebro_options[["expression_matrix_BPCells"]])) {
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
  if (is.null(cerebro_options[[key]])) {
    return(NULL)
  }
  key
}

## Compute the post-override decision once; the caller serializes it so runtime
## consumes the same decision that preflight validated.
.effectiveBundleBackendPlan <- function(backend, cerebro_options) {
  override_key <- .bundleBackendOverrideKey(backend, cerebro_options)
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
      location = cerebro_options[[override_key]]
    ))
  }
  if (isTRUE(backend$legacy) || identical(backend$type, "embedded")) {
    return(list(type = "embedded", mode = "embedded", location = NULL))
  }
  list(type = backend$type, mode = "bundled", location = backend$location)
}

# Native path identity and symbolic-link inspection -----------------------

.nativePathKey <- function(path, os_type = .Platform$OS.type) {
  path <- gsub("\\", "/", path, fixed = TRUE)
  if (nchar(path) > 1L) {
    path <- sub("/+$", "", path)
  }
  if (identical(os_type, "windows")) {
    path <- tolower(path)
  }
  path
}

.pathParentAndLeaf <- function(path, os_type = .Platform$OS.type) {
  if (identical(os_type, "windows")) {
    path <- gsub("\\", "/", path, fixed = TRUE)
  }
  style <- .absolutePathStyle(path, os_type = os_type)
  if (!is.na(style) && !identical(style, "windows-device")) {
    parsed <- .absolutePathComponents(path, style, os_type = os_type)
    if (length(parsed$parts) > 0L) {
      return(list(
        parent = .composeAbsolutePath(
          parsed$root,
          parsed$parts[-length(parsed$parts)]
        ),
        leaf = parsed$parts[[length(parsed$parts)]]
      ))
    }
  }
  list(parent = dirname(path), leaf = basename(path))
}

.pathIsSymbolicLink <- function(
  path,
  os_type = .Platform$OS.type,
  read_link = function(candidate) {
    .readNativeLink(candidate, os_type = os_type)
  },
  path_exists = .nativePathExists,
  normalize_existing = function(candidate) {
    .normalizeExistingPath(candidate, os_type = os_type)
  },
  entry_exists = .pathDirectoryEntryExists
) {
  link <- read_link(path)
  if (!is.na(link) && nzchar(link)) {
    return(TRUE)
  }
  if (!isTRUE(path_exists(path))) {
    return(isTRUE(entry_exists(path)))
  }
  if (!identical(os_type, "windows")) {
    return(FALSE)
  }

  path_parts <- .pathParentAndLeaf(path, os_type = os_type)
  physical_parent <- normalize_existing(path_parts$parent)
  expected <- .composeAbsolutePath(physical_parent, path_parts$leaf)
  physical <- normalize_existing(path)
  !identical(
    .nativePathKey(expected, os_type = os_type),
    .nativePathKey(physical, os_type = os_type)
  )
}

.backendPathContainsSymbolicLink <- function(root, parts, source) {
  cursor <- root
  for (part in parts) {
    cursor <- file.path(cursor, part)
    if (.pathIsSymbolicLink(cursor)) {
      return(TRUE)
    }
  }
  if (!dir.exists(source)) {
    return(FALSE)
  }

  pending <- source
  while (length(pending) > 0L) {
    current <- pending[[1L]]
    pending <- pending[-1L]
    children <- list.files(
      current,
      all.files = TRUE,
      full.names = TRUE,
      no.. = TRUE
    )
    if (length(children) == 0L) {
      next
    }
    linked <- vapply(children, .pathIsSymbolicLink, logical(1))
    if (any(linked)) {
      return(TRUE)
    }
    pending <- c(pending, children[!linked & dir.exists(children)])
  }
  FALSE
}

# Filesystem effect seams and operation helpers ---------------------------

.bundlePublicationOps <- function() {
  list(
    access = function(path, mode) file.access(path, mode = mode),
    list_dir = function(path) {
      list.files(path, all.files = TRUE, no.. = TRUE)
    },
    chmod = function(path, mode) Sys.chmod(path, mode = mode),
    rename = function(from, to) file.rename(from, to),
    unlink = function(path, recursive = TRUE, force = TRUE) {
      unlink(path, recursive = recursive, force = force)
    }
  )
}

.bundleBuildOps <- function() {
  list(
    access = function(path, mode) file.access(path, mode = mode),
    chmod = function(path, mode) Sys.chmod(path, mode = mode),
    copy = function(from, to, ...) file.copy(from, to, ...),
    mode = function(path) as.integer(file.info(path)$mode),
    save_rds = function(object, file) saveRDS(object, file),
    save_extra_rds = function(object, file) .saveExtraTableRDS(object, file),
    write_lines = function(text, connection) writeLines(text, connection)
  )
}

.attemptBundleOperation <- function(operation) {
  condition_message <- NULL
  value <- tryCatch(
    withCallingHandlers(
      operation(),
      warning = function(warning_condition) {
        condition_message <<- conditionMessage(warning_condition)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(error_condition) {
      condition_message <<- conditionMessage(error_condition)
      NULL
    }
  )
  list(value = value, condition = condition_message)
}

.bundleOperationSucceeded <- function(attempt) {
  is.null(attempt$condition) && isTRUE(attempt$value)
}

.bundleOperationDetail <- function(attempt) {
  if (is.null(attempt$condition)) {
    return("")
  }
  paste0(" Reason: ", attempt$condition)
}

.bundlePathExists <- function(path) {
  file.exists(path) || .pathIsSymbolicLink(path)
}

.removeBundleStage <- function(stage) {
  unlink(stage, recursive = TRUE, force = TRUE)
}

.isolateBundleLock <- function(from, to) {
  file.rename(from, to)
}

# Target preparation and build locks --------------------------------------

.bundleLockName <- function(name) {
  grepl(
    "^\\..*-build\\.lock($|-release-)",
    name,
    ignore.case = TRUE
  )
}

.assertOutsideBundleLockNamespace <- function(path) {
  slash_path <- gsub("\\", "/", path, fixed = TRUE)
  parts <- strsplit(slash_path, "/", fixed = TRUE)[[1L]]
  parts <- parts[nzchar(parts)]
  windows_aliases <- sub("[. ]+$", "", parts)
  if (any(vapply(windows_aliases, .bundleLockName, logical(1)))) {
    stop(
      "The 'result_dir' path uses a reserved bundle build-lock directory.",
      call. = FALSE
    )
  }
  invisible(path)
}

.bundleResultLeaf <- function(result_dir) {
  leaf <- basename(result_dir)
  .portableBundlePath(leaf, "The 'result_dir' basename")
  if (.bundleLockName(leaf)) {
    stop(
      "The 'result_dir' basename is reserved for bundle build locks.",
      call. = FALSE
    )
  }
  leaf
}

.stableBundleTarget <- function(result_dir) {
  result_dir <- path.expand(result_dir)
  .assertOutsideBundleLockNamespace(result_dir)
  leaf <- .bundleResultLeaf(result_dir)
  parent <- dirname(result_dir)
  parent <- if (dir.exists(parent)) {
    normalizePath(parent, winslash = "/", mustWork = TRUE)
  } else {
    .canonicalTargetPath(parent)
  }
  .assertOutsideBundleLockNamespace(parent)
  candidate <- file.path(parent, leaf)
  if (.pathIsSymbolicLink(candidate)) {
    stop("'result_dir' must not be a symbolic link.", call. = FALSE)
  }
  if (.nativePathExists(candidate)) {
    candidate <- .normalizeExistingPath(candidate)
    .assertOutsideBundleLockNamespace(candidate)
    return(candidate)
  }
  candidate
}

.prepareBundleResultTarget <- function(result_dir) {
  result_style <- .absolutePathStyle(result_dir)
  if (result_style %in% c("windows-device", "windows-malformed")) {
    detail <- if (identical(result_style, "windows-device")) {
      "device or extended-length"
    } else {
      "malformed Windows"
    }
    stop(
      "Windows ",
      detail,
      " path namespaces are not supported.",
      call. = FALSE
    )
  }
  result_dir <- path.expand(result_dir)
  .assertOutsideBundleLockNamespace(result_dir)
  .bundleResultLeaf(result_dir)

  parent <- dirname(result_dir)
  prospective_parent <- .canonicalTargetPath(parent)
  .assertOutsideBundleLockNamespace(prospective_parent)
  if (!dir.exists(prospective_parent)) {
    created <- dir.create(
      prospective_parent,
      recursive = TRUE,
      showWarnings = FALSE
    )
    if (!isTRUE(created) && !dir.exists(prospective_parent)) {
      stop("Failed to create the parent of 'result_dir'.", call. = FALSE)
    }
  }
  if (!dir.exists(prospective_parent)) {
    stop("The parent of 'result_dir' is not a directory.", call. = FALSE)
  }
  parent <- normalizePath(
    prospective_parent,
    winslash = "/",
    mustWork = TRUE
  )
  target <- .stableBundleTarget(file.path(parent, basename(result_dir)))
  if (identical(target, dirname(target))) {
    stop("'result_dir' must name an app directory.", call. = FALSE)
  }

  parent <- dirname(target)

  list(parent = parent, target = target)
}

.bundleLockPath <- function(result_dir) {
  target <- .stableBundleTarget(result_dir)
  file.path(
    dirname(target),
    paste0(".", basename(target), "-build.lock")
  )
}

## dir.create() is the atomic claim. Ownership metadata lets release
## distinguish this build's lock from a different-token replacement.
.acquireBundleLock <- function(result_dir) {
  target <- .stableBundleTarget(result_dir)
  lock_path <- .bundleLockPath(target)
  if (!dir.exists(dirname(lock_path))) {
    stop("The build-lock parent directory does not exist.", call. = FALSE)
  }
  if (!dir.create(lock_path, mode = "0700", showWarnings = FALSE)) {
    if (.bundlePathExists(lock_path)) {
      stop(
        "The app target '",
        target,
        "' is already being built. Build lock: '",
        lock_path,
        "'. If no build is running, inspect and remove the stale lock ",
        "manually.",
        call. = FALSE
      )
    }
    stop(
      "Failed to create the build lock at '",
      lock_path,
      "'. Check the parent-directory permissions.",
      call. = FALSE
    )
  }

  token <- basename(tempfile(pattern = paste0("owner-", Sys.getpid(), "-")))
  token_path <- file.path(lock_path, "owner-token")
  owner_path <- file.path(lock_path, "owner.txt")
  host <- unname(Sys.info()[["nodename"]])
  if (is.null(host) || is.na(host) || !nzchar(host)) {
    host <- "unknown"
  }
  owner <- c(
    "schema=1",
    paste0("pid=", Sys.getpid()),
    paste0("host=", encodeString(host, quote = '"')),
    paste0(
      "started_at=",
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
    ),
    paste0("result_dir=", encodeString(target, quote = '"'))
  )
  metadata_attempt <- .attemptBundleOperation(function() {
    writeLines(token, token_path, useBytes = TRUE)
    writeLines(owner, owner_path, useBytes = TRUE)
    TRUE
  })
  if (!.bundleOperationSucceeded(metadata_attempt)) {
    metadata_paths <- c(token_path, owner_path)
    regular_files <- utils::file_test("-f", metadata_paths) &
      !vapply(metadata_paths, .pathIsSymbolicLink, logical(1))
    if (any(regular_files)) {
      file.remove(metadata_paths[regular_files])
    }
    file.remove(lock_path)
    if (.bundlePathExists(lock_path)) {
      stop(
        "Failed to initialize the build lock. An incomplete lock remains at '",
        lock_path,
        "'.",
        .bundleOperationDetail(metadata_attempt),
        call. = FALSE
      )
    }
    stop(
      "Failed to initialize the build lock.",
      .bundleOperationDetail(metadata_attempt),
      call. = FALSE
    )
  }

  structure(
    list(path = lock_path, token = token, result_dir = target),
    class = "cerebro_bundle_lock"
  )
}

.inspectBundleLock <- function(path, token) {
  if (
    !.bundlePathExists(path) ||
      .pathIsSymbolicLink(path) ||
      !dir.exists(path)
  ) {
    return(list(valid = FALSE, kind = "ownership"))
  }

  listing_attempt <- .attemptBundleOperation(function() {
    list.files(path, all.files = TRUE, no.. = TRUE)
  })
  entries <- listing_attempt$value
  expected <- c("owner-token", "owner.txt")
  if (
    !is.null(listing_attempt$condition) ||
      !is.character(entries) ||
      !setequal(entries, expected)
  ) {
    return(list(valid = FALSE, kind = "contents"))
  }

  metadata_paths <- file.path(path, expected)
  regular_files <- utils::file_test("-f", metadata_paths)
  linked_files <- vapply(metadata_paths, .pathIsSymbolicLink, logical(1))
  if (!all(regular_files) || any(linked_files)) {
    return(list(valid = FALSE, kind = "contents"))
  }
  metadata_sizes <- file.info(metadata_paths)$size
  if (
    anyNA(metadata_sizes) ||
      any(metadata_sizes <= 0) ||
      metadata_sizes[[1L]] > 4096 ||
      metadata_sizes[[2L]] > 65536
  ) {
    return(list(valid = FALSE, kind = "contents"))
  }

  token_attempt <- .attemptBundleOperation(function() {
    readLines(file.path(path, "owner-token"), warn = FALSE)
  })
  stored_token <- token_attempt$value
  if (
    !is.null(token_attempt$condition) ||
      !is.character(stored_token) ||
      length(stored_token) != 1L ||
      !identical(stored_token, token)
  ) {
    return(list(valid = FALSE, kind = "ownership"))
  }

  list(valid = TRUE, kind = NULL)
}

## Revalidate ownership, rename the lock out of the shared namespace, then
## remove only the isolated copy. Preserve it if its identity changes.
.releaseBundleLock <- function(lock) {
  if (
    !inherits(lock, "cerebro_bundle_lock") ||
      !is.character(lock$path) ||
      length(lock$path) != 1L ||
      !is.character(lock$token) ||
      length(lock$token) != 1L
  ) {
    warning(
      "Cannot verify build-lock ownership; the lock was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  if (!.bundlePathExists(lock$path)) {
    return(invisible(FALSE))
  }
  inspection <- .inspectBundleLock(lock$path, lock$token)
  if (!isTRUE(inspection$valid) && identical(inspection$kind, "contents")) {
    warning(
      "The build lock contains unexpected or unreadable contents and was ",
      "preserved: ",
      lock$path,
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  if (!isTRUE(inspection$valid)) {
    warning(
      "Cannot verify build-lock ownership; the lock was preserved.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  release_path <- tempfile(
    pattern = paste0(".", basename(lock$path), "-release-"),
    tmpdir = dirname(lock$path)
  )
  rename_attempt <- .attemptBundleOperation(function() {
    .isolateBundleLock(lock$path, release_path)
  })
  moved <- dir.exists(release_path) && !.pathIsSymbolicLink(release_path)
  if (!moved) {
    warning(
      "Failed to isolate the owned build lock for removal: ",
      lock$path,
      .bundleOperationDetail(rename_attempt),
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  moved_inspection <- .inspectBundleLock(release_path, lock$token)
  if (!isTRUE(moved_inspection$valid)) {
    restored <- FALSE
    if (!.bundlePathExists(lock$path)) {
      restore_attempt <- .attemptBundleOperation(function() {
        file.rename(release_path, lock$path)
      })
      restored <- dir.exists(lock$path) &&
        !.bundlePathExists(release_path)
    }
    preserved_at <- if (restored) lock$path else release_path
    warning(
      "Build-lock ownership changed during release; it was preserved at: ",
      preserved_at,
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  metadata_paths <- file.path(release_path, c("owner-token", "owner.txt"))
  remove_files_attempt <- .attemptBundleOperation(function() {
    file.remove(metadata_paths)
  })
  if (any(vapply(metadata_paths, .bundlePathExists, logical(1)))) {
    warning(
      "Failed to remove owned build-lock metadata; the lock was preserved at: ",
      release_path,
      .bundleOperationDetail(remove_files_attempt),
      call. = FALSE
    )
    return(invisible(FALSE))
  }

  remove_dir_attempt <- .attemptBundleOperation(function() {
    file.remove(release_path)
  })
  if (.bundlePathExists(release_path)) {
    warning(
      "The owned build lock gained unexpected contents during release and was ",
      "preserved at: ",
      release_path,
      .bundleOperationDetail(remove_dir_attempt),
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

# Recoverable publication -------------------------------------------------

.bundleDestinationState <- function(
  result_dir,
  overwrite,
  ops = .bundlePublicationOps()
) {
  if (file.exists(result_dir) && !dir.exists(result_dir)) {
    stop("'result_dir' exists and is not a directory.", call. = FALSE)
  }
  if (.pathIsSymbolicLink(result_dir)) {
    stop("'result_dir' must not be a symbolic link.", call. = FALSE)
  }

  entries <- character()
  if (dir.exists(result_dir)) {
    inspection_error <- NULL
    access_status <- tryCatch(
      withCallingHandlers(
        ops$access(result_dir, mode = 5L),
        warning = function(warning_condition) {
          inspection_error <<- conditionMessage(warning_condition)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(error_condition) {
        inspection_error <<- conditionMessage(error_condition)
        NULL
      }
    )
    if (
      !is.null(inspection_error) ||
        length(access_status) != 1L ||
        is.na(access_status) ||
        access_status != 0L
    ) {
      detail <- if (is.null(inspection_error)) {
        "the directory is not readable and searchable"
      } else {
        inspection_error
      }
      stop(
        "Failed to inspect 'result_dir': ",
        detail,
        ".",
        call. = FALSE
      )
    }

    inspection_error <- NULL
    entries <- tryCatch(
      withCallingHandlers(
        ops$list_dir(result_dir),
        warning = function(warning_condition) {
          inspection_error <<- conditionMessage(warning_condition)
          invokeRestart("muffleWarning")
        }
      ),
      error = function(error_condition) {
        inspection_error <<- conditionMessage(error_condition)
        NULL
      }
    )
    if (!is.null(inspection_error) || !is.character(entries)) {
      detail <- if (is.null(inspection_error)) {
        "the directory listing was not a character vector"
      } else {
        inspection_error
      }
      stop(
        "Failed to inspect 'result_dir': ",
        detail,
        ".",
        call. = FALSE
      )
    }
  }
  if (!overwrite && length(entries) > 0L) {
    stop(
      "overwrite = FALSE rejects a non-empty result_dir; use an absent or ",
      "empty directory.",
      call. = FALSE
    )
  }

  list(exists = dir.exists(result_dir), entries = entries)
}

## For replacement, move the old bundle to backup, publish the stage, and
## restore on failure when the destination remains free. If an unexpected
## destination is detected, leave it untouched and retain the backup.
.publishBundleStage <- function(
  stage,
  result_dir,
  overwrite,
  publish_mode,
  ops = .bundlePublicationOps()
) {
  destination <- .bundleDestinationState(result_dir, overwrite, ops)
  chmod_attempt <- .attemptBundleOperation(function() {
    ops$chmod(stage, publish_mode)
  })
  if (!.bundleOperationSucceeded(chmod_attempt)) {
    stop(
      "Failed to apply deployment permissions to the staged app.",
      .bundleOperationDetail(chmod_attempt),
      call. = FALSE
    )
  }

  backup <- NULL
  if (isTRUE(destination$exists)) {
    backup <- tempfile(
      pattern = paste0(".", basename(result_dir), "-backup-"),
      tmpdir = dirname(result_dir)
    )
    backup <- .canonicalTargetPath(backup)
    backup_attempt <- .attemptBundleOperation(function() {
      ops$rename(result_dir, backup)
    })
    backup_ready <- dir.exists(backup) && !.bundlePathExists(result_dir)
    if (!.bundleOperationSucceeded(backup_attempt) || !backup_ready) {
      if (dir.exists(backup)) {
        if (.bundlePathExists(result_dir)) {
          stop(
            "Failed to stage the existing app for replacement because an ",
            "unexpected destination appeared at '",
            .canonicalTargetPath(result_dir),
            "'. The previous bundle remains recoverable at '",
            backup,
            "'.",
            .bundleOperationDetail(backup_attempt),
            call. = FALSE
          )
        }
        restore_attempt <- .attemptBundleOperation(function() {
          ops$rename(backup, result_dir)
        })
        restored <- dir.exists(result_dir) && !.bundlePathExists(backup)
        if (restored) {
          stop(
            "Failed to stage the existing app for replacement; the previous ",
            "bundle was restored.",
            .bundleOperationDetail(backup_attempt),
            call. = FALSE
          )
        }
        stop(
          "Failed to stage the existing app for replacement and could not ",
          "restore the previous bundle. It remains recoverable at '",
          backup,
          "'.",
          .bundleOperationDetail(restore_attempt),
          call. = FALSE
        )
      }
      stop(
        "Failed to stage the existing app for replacement.",
        .bundleOperationDetail(backup_attempt),
        call. = FALSE
      )
    }
  }

  publish_attempt <- .attemptBundleOperation(function() {
    ops$rename(stage, result_dir)
  })
  published <- .bundleOperationSucceeded(publish_attempt) &&
    dir.exists(result_dir) &&
    !.bundlePathExists(stage)
  if (!published) {
    if (!is.null(backup) && dir.exists(backup)) {
      unexpected_destination <- .bundlePathExists(result_dir)
      if (unexpected_destination) {
        stop(
          "Failed to publish the staged app bundle because an unexpected ",
          "destination appeared at '",
          .canonicalTargetPath(result_dir),
          "'. The previous bundle remains recoverable at '",
          backup,
          "'.",
          .bundleOperationDetail(publish_attempt),
          call. = FALSE
        )
      }
      restore_attempt <- .attemptBundleOperation(function() {
        ops$rename(backup, result_dir)
      })
      restored <- dir.exists(result_dir) && !.bundlePathExists(backup)
      if (restored) {
        stop(
          "Failed to publish the staged app bundle; the previous bundle was ",
          "restored.",
          .bundleOperationDetail(publish_attempt),
          call. = FALSE
        )
      }
      stop(
        "Failed to publish the staged app bundle and could not restore the ",
        "previous bundle. It remains recoverable at '",
        backup,
        "'.",
        .bundleOperationDetail(restore_attempt),
        call. = FALSE
      )
    }
    if (.bundlePathExists(result_dir)) {
      stop(
        "Failed to publish the staged app bundle because an unexpected ",
        "destination appeared at '",
        .canonicalTargetPath(result_dir),
        "'.",
        .bundleOperationDetail(publish_attempt),
        call. = FALSE
      )
    }
    stop(
      "Failed to publish the staged app bundle.",
      .bundleOperationDetail(publish_attempt),
      call. = FALSE
    )
  }

  if (!is.null(backup) && dir.exists(backup)) {
    cleanup_attempt <- .attemptBundleOperation(function() {
      ops$unlink(backup, recursive = TRUE, force = TRUE)
    })
    if (.bundlePathExists(backup)) {
      warning(
        "The new app was published, but an old backup remains at: ",
        backup,
        .bundleOperationDetail(cleanup_attempt),
        call. = FALSE
      )
    }
  }
  invisible(result_dir)
}

# Public API ---------------------------------------------------------------

#' Create a self-contained CerebroNexus Shiny app folder
#'
#' Bundles a CerebroNexus Shiny app into \code{result_dir}, copying the
#' \code{inst/viewer/} sources, the requested \code{.crb} data file(s),
#' and \code{extdata/}, and writes an \code{app.R} that sources the bundled
#' UI/server. The output directory can be served directly by shiny-server or
#' run with \code{shiny::runApp(result_dir)}.
#'
#' Supports external expression backends (\code{bpcells}, \code{h5}) in
#' addition to the embedded mode. Each \code{.crb} descriptor names a portable
#' relative file or directory, which is copied to the same private location
#' under \code{private-data/}. The historical \code{data/} name is deliberately
#' not reused: a still-running older generated app may retain its former
#' \code{/data} HTTP resource mapping during an in-place replacement. Raw
#' \code{.crb}, H5, and BPCells artifacts are not registered as HTTP resources.
#' Spatial background images are the deliberate exception: files explicitly
#' supplied through \code{spatial_images} are copied verbatim under
#' \code{spatial-assets/}. The server-side renderer reads these files and embeds
#' them as data URIs; the directory is not registered as an HTTP resource.
#' Callers must provide trusted image files. Preflight requires a minimum
#' stable runtime API and reads the ordinary \code{expression_backend} field
#' without invoking serialized methods or its getter. The generated
#' configuration stores the effective per-CRB attachment plan after applying
#' any deployment override, and configured CRBs consume that exact plan at
#' runtime. Direct launches and uploads also read and validate the ordinary
#' field without invoking the getter. A historical CRB may omit both field and
#' getter; an object containing only one is rejected as an unsupported mixed
#' format.
#' Dataset labels and canonical CRB sources are both unique: two labels cannot
#' select the same resolved input file.
#' Generated bundles follow the standard deployment model of one app per R
#' process; process-global \code{Cerebro.options} does not provide same-process
#' isolation between separately sourced apps.
#'
#' Launch settings are validated and frozen in a typed internal manifest before
#' target preparation. The generated \code{app.R} reads that manifest instead of
#' interpolating user values into source, and the staged source is parsed before
#' publication. The upload limit is installed as
#' \code{shiny.maxRequestSize} while the app is running and the previous process
#' option is restored when the app stops.
#'
#' A configured runtime matrix override keeps its existing precedence and skips
#' the descriptor sidecar copy. It must be an absolute path. Native paths are
#' resolved component by component and rejected if they resolve inside
#' \code{result_dir}. Unresolved filesystem entries, unsafe Windows aliases,
#' and Windows device namespaces fail closed; a truly absent ordinary target is
#' allowed for later provisioning. Non-native absolute paths intended for
#' another host are preserved lexically and cannot be compared with that host's
#' app tree. Such a bundle depends on that host-managed path and is therefore
#' not self-contained. Treat
#' input \code{.crb}/\code{.rds} files as trusted serialized R objects;
#' structural validation is not a sandbox for untrusted RDS input.
#'
#' One atomic sibling lock directory, \code{.<basename>-build.lock}, serializes
#' builds for each target. An existing lock stops the build and is never removed
#' automatically as "stale". Required inputs and targets are validated before
#' the app is assembled in a private sibling stage. Preflight and stage-build
#' failures leave an existing deployment in place. Publication first renames the
#' old app to a unique backup; if the final rename fails, restoration is
#' attempted. A failed restoration keeps the backup and reports its exact path.
#' Abrupt process death between the two publication renames is not recovered
#' automatically and can leave \code{result_dir} temporarily absent. After
#' confirming that the lock owner is no longer running, verify and restore the
#' correct backup before removing any leftover stage and lock.
#' On POSIX systems, the stage is mode \code{0700} while data is copied, and
#' replacement retains the existing deployment root's permission bits.
#' Platform-specific ACLs, ownership changes, and security labels remain the
#' deployment system's responsibility. All CRBs, descriptor sidecars, spatial
#' images and their source-path ancestors must remain trusted and unchanged
#' while the build is running; the target build lock does not protect inputs.
#'
#' @param cerebro_data Non-empty named character vector or list of \code{.crb}
#'   (or \code{.rds}) file paths. Names must be non-missing and unique and are
#'   used as dataset labels. Every path must resolve to a distinct canonical
#'   source file.
#' @param result_dir Output directory. Its basename must be portable, its path
#'   must not use the reserved build-lock namespace, and its final target must
#'   not be a symbolic link or unresolved filesystem entry.
#' @param max_request_size One finite positive numeric upload limit in MB;
#'   defaults to 8000. Closed Viewers cap the effective request size at 6 MB;
#'   open Viewers use the supplied value.
#' @param port One whole-number port from 1 through 65535; defaults to 8080.
#' @param host One non-empty character value that the generated app binds to;
#'   defaults to "127.0.0.1".
#' @param launch_browser One non-missing logical controlling whether to launch a
#'   browser; defaults to TRUE.
#' @param quiet One non-missing logical passed to \code{shiny::runApp}; defaults
#'   to FALSE.
#' @param display_mode Exactly one of \code{"auto"}, \code{"normal"}, or
#'   \code{"showcase"}; defaults to \code{"normal"}.
#' @param colors Optional named list of colour palettes per dataset.
#' @param cerebro_options Extra entries merged into \code{Cerebro.options} in
#'   the generated app. Matrix overrides must be absolute host paths and make
#'   the resulting app host-dependent. Native paths must resolve outside
#'   \code{result_dir}; non-native paths cannot be compared with the local app
#'   tree during the build. Duplicate internal entries are removed, and the
#'   keys \code{.bundle_backend_plan} and \code{.bundle_run_options} are written
#'   by the function.
#' @param overwrite If \code{TRUE} (default), replace \code{result_dir} only
#'   after a complete staged build succeeds. Publication failures attempt to
#'   restore the previous app and preserve its backup if restoration fails. If
#'   \code{FALSE}, \code{result_dir} must be absent or empty; a non-empty
#'   directory is rejected before any files are written.
#' @param verbose Print progress messages; defaults to TRUE.
#' @param crb_pick_smallest_file Forwarded to \code{Cerebro.options}.
#' @param show_upload_ui One non-missing logical controlling whether users may
#'   upload their own data; defaults to \code{FALSE}.
#' @param initial_page Optional initial Viewer page. Supported stable IDs are
#'   \code{"data_info"}, \code{"projection"}, \code{"linked_views"},
#'   \code{"groups"}, \code{"marker_genes"},
#'   \code{"most_expressed_genes"}, \code{"enriched_pathways"},
#'   \code{"extra_material"}, \code{"immune_repertoire"},
#'   \code{"trajectory"}, \code{"spatial"}, \code{"trekker"},
#'   \code{"hla_tcr_motifs"}, \code{"gene_expression"},
#'   \code{"gene_id_conversion"}, \code{"color_management"}, and
#'   \code{"about"}. A conditional page is selected only when it is available
#'   for the first loaded dataset. Initial routing is attempted only once, so
#'   switching datasets later never triggers a delayed redirect. Shared links
#'   take precedence after restoration.
#' @param welcome_message Welcome message shown in the Load Data tab.
#' @param point_size Point size from 1 through 20. Supply one number for every
#'   dataset, or a named numeric vector/list whose names exactly match
#'   \code{cerebro_data}. The value initializes every cell scatter view.
#' @param point_opacity Point opacity from 0.1 through 1. Supply one number for
#'   every dataset, or a named numeric vector/list whose names exactly match
#'   \code{cerebro_data}. The value initializes every cell scatter view.
#' @param percentage_cells_to_show Percentage of cells from 10 through 100.
#'   Supply one number for every dataset, or a named numeric vector/list whose
#'   names exactly match \code{cerebro_data}. The value initializes every cell
#'   scatter view; subsequent UI changes remain page-local.
#' @param variable_to_compare Forwarded to \code{Cerebro.options}.
#' @param spatial_images Optional nested external-image manifest in
#'   \code{dataset -> spatial entry -> image label -> path} form. Dataset names
#'   must match \code{cerebro_data}; spatial names must match the corresponding
#'   CRB's \code{availableSpatial()}, and image labels must be unique and
#'   non-empty within an entry. A leaf may instead be a descriptor containing
#'   \code{path} and optional coordinate \code{bounds}. Supplied files are
#'   copied to opaque, safe relative paths under \code{spatial-assets/}. The
#'   legacy \code{c(Dataset = path)} form is accepted only
#'   when that CRB has exactly one spatial entry.
#' @param spatial_image_settings Optional nested settings in
#'   \code{dataset -> spatial entry -> image label -> settings} form. Settings
#'   may contain only \code{flip_x}, \code{flip_y}, \code{scale_x},
#'   \code{scale_y}, \code{offset_x}, \code{offset_y}, and \code{rotation}.
#'   \code{image_opacity} may additionally set the initial image opacity from
#'   0 through 1.
#'   A leaf may target an embedded or external image available under that exact
#'   dataset and spatial entry. The image label must exist in the union of the
#'   CRB's embedded images and this call's \code{spatial_images}; unknown
#'   identities are rejected. Labels are user-facing names, not protocol names.
#' @param spatial_images_flip_x Legacy named per-dataset horizontal flip values.
#' @param spatial_images_flip_y Legacy named per-dataset vertical flip values.
#' @param spatial_images_scale_x Legacy named per-dataset X scale values.
#' @param spatial_images_scale_y Legacy named per-dataset Y scale values.
#' @param spatial_images_offset_x Legacy named per-dataset horizontal offsets.
#' @param spatial_images_offset_y Legacy named per-dataset vertical offsets.
#' @param spatial_plot_rotation Optional nested rotations in
#'   \code{dataset -> spatial entry -> degrees} form, applied only to spatial
#'   cell coordinates. The legacy named per-dataset vector form remains
#'   supported and applies one rotation to every spatial entry in that dataset.
#'   Dataset names must match \code{cerebro_data}; spatial names must match the
#'   corresponding CRB's \code{availableSpatial()}.
#' @param auth Optional authentication settings. \code{NULL}, the default,
#'   leaves the generated Viewer public. To require a login, provide a named
#'   list with \code{credentials}, the path to an encrypted SQLite database
#'   created by \code{shinymanager::create_db()}, and \code{passphrase_env},
#'   the name of the environment variable containing its passphrase. Optional
#'   \code{timeout_minutes} defaults to 15.
#' @param extra_tables Optional named collection of CSV, TSV, TXT, XLS, XLSX,
#'   or XLSM files to bundle as private Extra material tables. Workbook sheets
#'   are listed separately and loaded only when selected in the Viewer.
#' @param extra_tables_sheets Optional named list of Excel sheet renames keyed
#'   by \code{extra_tables} labels. Each entry maps displayed names to source
#'   sheet names; unmapped sheets remain available.
#'
#' @return Invisibly returns \code{result_dir}. If that path changes resolution
#'   during the build, warns and returns the frozen absolute publication path.
#'
#' @examples
#' \dontrun{
#' library(CerebroNexus)
#'
#' createShinyApp(
#'   cerebro_data = c(
#'     "PBMC example" = "output/cerebro_pbmc_seurat.crb"
#'   ),
#'   result_dir = "my_app",
#'   port = 8080,
#'   host = "127.0.0.1",
#'   max_request_size = 8000,
#'   overwrite = TRUE
#' )
#' # Run with shiny::runApp("my_app") or deploy my_app/ to Shiny Server.
#' }
#'
#' @importFrom later later
#' @importFrom stats setNames
#' @export
createShinyApp <- function(
  cerebro_data,
  result_dir = NULL,
  max_request_size = 8000,
  port = 8080,
  host = "127.0.0.1",
  launch_browser = TRUE,
  quiet = FALSE,
  display_mode = "normal",
  colors = NULL,
  cerebro_options = list(exclude_trivial_metadata = TRUE),
  overwrite = TRUE,
  verbose = TRUE,
  crb_pick_smallest_file = TRUE,
  show_upload_ui = FALSE,
  welcome_message = "Welcome to CerebroNexus!",
  point_size = 5,
  point_opacity = 1,
  percentage_cells_to_show = 100,
  variable_to_compare = NULL,
  spatial_images = NULL,
  spatial_image_settings = NULL,
  spatial_images_flip_x = NULL,
  spatial_images_flip_y = NULL,
  spatial_images_scale_x = NULL,
  spatial_images_scale_y = NULL,
  spatial_images_offset_x = NULL,
  spatial_images_offset_y = NULL,
  spatial_plot_rotation = NULL,
  auth = NULL,
  extra_tables = NULL,
  extra_tables_sheets = NULL,
  initial_page = NULL
) {
  # Validate inputs ----------------------------------------------------------##
  if (is.list(cerebro_data)) {
    valid_entries <- vapply(
      cerebro_data,
      function(path) {
        is.character(path) &&
          length(path) == 1L &&
          !is.na(path) &&
          nzchar(path)
      },
      logical(1)
    )
    if (!all(valid_entries)) {
      stop(
        "Every cerebro_data list entry must be one non-empty file path.",
        call. = FALSE
      )
    }
    data_names <- names(cerebro_data)
    cerebro_data <- vapply(cerebro_data, `[[`, character(1), 1L)
    names(cerebro_data) <- data_names
  }
  if (!is.character(cerebro_data)) {
    stop(
      "cerebro_data must be a named character vector or list of file paths.",
      call. = FALSE
    )
  }
  if (length(cerebro_data) == 0L) {
    stop("cerebro_data must contain at least one data set.", call. = FALSE)
  }
  if (!all(file.exists(cerebro_data))) {
    missing <- cerebro_data[!file.exists(cerebro_data)]
    stop(
      "Cerebro data file(s) not found: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (!all(grepl("\\.(crb|rds)$", cerebro_data, ignore.case = TRUE))) {
    warning(
      "Some input files do not have .crb or .rds extension. Make sure they are valid Cerebro files."
    )
  }

  data_labels <- names(cerebro_data)
  if (
    is.null(data_labels) ||
      anyNA(data_labels) ||
      any(data_labels == "")
  ) {
    stop(
      "cerebro_data labels must be non-empty and non-missing.",
      call. = FALSE
    )
  }
  if (anyDuplicated(data_labels)) {
    stop("cerebro_data labels must be unique.", call. = FALSE)
  }
  point_size <- .normalizeDatasetNumericOption(
    point_size,
    data_labels,
    "point_size",
    1,
    20
  )
  point_opacity <- .normalizeDatasetNumericOption(
    point_opacity,
    data_labels,
    "point_opacity",
    0.1,
    1
  )
  percentage_cells_to_show <- .normalizeDatasetNumericOption(
    percentage_cells_to_show,
    data_labels,
    "percentage_cells_to_show",
    10,
    100
  )
  builder_spatial_options <- c(
    "spatial_images",
    "spatial_image_settings",
    "spatial_images_flip_x",
    "spatial_images_flip_y",
    "spatial_images_scale_x",
    "spatial_images_scale_y",
    "spatial_images_offset_x",
    "spatial_images_offset_y"
  )
  supplied_option_names <- names(cerebro_options)
  duplicate_scatter_options <- intersect(
    supplied_option_names,
    c("point_size", "point_opacity", "percentage_cells_to_show")
  )
  if (length(duplicate_scatter_options)) {
    stop(
      "Supply ",
      paste(duplicate_scatter_options, collapse = " and "),
      " through the matching createShinyApp() parameter, not cerebro_options.",
      call. = FALSE
    )
  }
  forbidden_spatial_options <- unique(intersect(
    supplied_option_names,
    builder_spatial_options
  ))
  if (length(forbidden_spatial_options) > 0L) {
    stop(
      "`cerebro_options` contains builder-owned spatial key(s): ",
      paste(forbidden_spatial_options, collapse = ", "),
      ". Supply these through the corresponding createShinyApp() formal ",
      "parameters instead.",
      call. = FALSE
    )
  }
  if (
    !is.logical(overwrite) ||
      length(overwrite) != 1L ||
      is.na(overwrite)
  ) {
    stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
  }
  if (
    !is.logical(show_upload_ui) ||
      length(show_upload_ui) != 1L ||
      is.na(show_upload_ui)
  ) {
    stop("'show_upload_ui' must be TRUE or FALSE.", call. = FALSE)
  }
  initial_pages <- .viewerInitialPageTabs()
  if (
    !is.null(initial_page) &&
      (!is.character(initial_page) ||
        length(initial_page) != 1L ||
        is.na(initial_page) ||
        !initial_page %in% names(initial_pages))
  ) {
    stop(
      "'initial_page' must be NULL or one of: ",
      paste(names(initial_pages), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (
    is.null(result_dir) ||
      !is.character(result_dir) ||
      length(result_dir) != 1L ||
      is.na(result_dir) ||
      !nzchar(result_dir)
  ) {
    stop("'result_dir' must be provided.", call. = FALSE)
  }
  bundle_run_options <- .bundleRunOptions(
    max_request_size = max_request_size,
    port = port,
    host = host,
    launch_browser = launch_browser,
    quiet = quiet,
    display_mode = display_mode,
    show_upload_ui = show_upload_ui
  )
  resolved_crb_sources <- vapply(
    cerebro_data,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = TRUE
  )
  crb_source_keys <- vapply(
    resolved_crb_sources,
    .nativePathKey,
    character(1)
  )
  duplicate_source <- anyDuplicated(crb_source_keys)
  if (duplicate_source != 0L) {
    original_source <- match(
      crb_source_keys[[duplicate_source]],
      crb_source_keys
    )
    stop(
      "cerebro_data labels '",
      data_labels[[original_source]],
      "' and '",
      data_labels[[duplicate_source]],
      "' resolve to the same Cerebro data file.",
      call. = FALSE
    )
  }
  extra_table_plan <- .bundleExtraTables(
    extra_tables = extra_tables,
    extra_tables_sheets = extra_tables_sheets
  )
  viewer_auth <- .compileViewerAuth(auth)
  requested_result_dir <- result_dir
  prepared_result <- .prepareBundleResultTarget(result_dir)
  result_dir <- prepared_result$target
  result_parent <- prepared_result$parent
  bundle_lock <- .acquireBundleLock(result_dir)
  bundle_cleanup <- new.env(parent = emptyenv())
  bundle_cleanup$stage <- NULL
  on.exit(
    tryCatch(
      {
        stage <- bundle_cleanup$stage
        if (!is.null(stage) && .bundlePathExists(stage)) {
          .removeBundleStage(stage)
          if (.bundlePathExists(stage)) {
            warning(
              "Failed to remove the private app staging directory: ",
              stage,
              call. = FALSE
            )
          }
        }
      },
      finally = .releaseBundleLock(bundle_lock)
    ),
    add = TRUE
  )
  .bundleDestinationState(result_dir, overwrite)

  if (!is.null(colors)) {
    if (
      !is.list(colors) || is.null(names(colors)) || any(names(colors) == "")
    ) {
      stop("colors must be a named list by dataset.", call. = FALSE)
    }
    valid_palette <- vapply(
      colors,
      function(palette) {
        is.list(palette) &&
          length(palette) &&
          !is.null(names(palette)) &&
          all(nzchar(names(palette))) &&
          all(vapply(
            palette,
            function(values) {
              is.character(values) &&
                length(values) &&
                !is.null(names(values)) &&
                all(nzchar(names(values)))
            },
            logical(1)
          ))
      },
      logical(1)
    )
    if (!all(valid_palette)) {
      stop(
        paste(
          "Each colors dataset must be a named list of variables,",
          "each containing a named character palette."
        ),
        call. = FALSE
      )
    }
    valid_colours <- vapply(
      colors,
      function(palette) {
        all(vapply(
          palette,
          function(values) {
            tryCatch(
              {
                grDevices::col2rgb(values)
                TRUE
              },
              error = function(error) FALSE
            )
          },
          logical(1)
        ))
      },
      logical(1)
    )
    if (!all(valid_colours)) {
      stop("Every colors value must be a valid R colour.", call. = FALSE)
    }
    if (length(intersect(names(colors), names(cerebro_data))) == 0) {
      warning(
        "Colors and cerebro_data do not match, random colors will be used.",
        call. = FALSE
      )
      colors <- NULL
    }
  }

  if (!is.null(variable_to_compare) && !is.logical(variable_to_compare)) {
    if (
      (is.list(variable_to_compare) || is.vector(variable_to_compare)) &&
        !is.null(names(variable_to_compare))
    ) {
      if (
        length(intersect(names(variable_to_compare), names(cerebro_data))) == 0
      ) {
        warning(
          "No matching names found between variable_to_compare and cerebro_data. Ignoring.",
          call. = FALSE
        )
        variable_to_compare <- NULL
      }
    } else {
      warning(
        "variable_to_compare must be NULL, a single boolean, or a named list/vector. Ignoring.",
        call. = FALSE
      )
      variable_to_compare <- NULL
    }
  }

  ## Spatial background images (and their per-dataset transforms) must be named
  ## to match cerebro_data. Duplicate image data-set names are ambiguous at
  ## runtime and therefore rejected; other malformed entries are dropped with
  ## a warning.
  validate_named_against_data <- function(
    x,
    arg_name,
    reject_duplicates = FALSE
  ) {
    if (is.null(x)) {
      return(NULL)
    }
    if (is.null(names(x)) || anyNA(names(x)) || any(names(x) == "")) {
      warning(
        arg_name,
        " must be a named list or vector. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    if (reject_duplicates && anyDuplicated(names(x))) {
      stop(arg_name, " names must be unique.", call. = FALSE)
    }
    matching <- names(x) %in% names(cerebro_data)
    if (!any(matching)) {
      warning(
        "No matching names found between ",
        arg_name,
        " and cerebro_data. Ignoring.",
        call. = FALSE
      )
      return(NULL)
    }
    if (!all(matching)) {
      warning(
        "Some ",
        arg_name,
        " entries do not match cerebro_data and will be ignored: ",
        paste(unique(names(x)[!matching]), collapse = ", "),
        call. = FALSE
      )
    }
    x[matching]
  }
  if (!requireNamespace("CerebroNexus", quietly = TRUE)) {
    stop(
      "Package 'CerebroNexus' is required but not installed.",
      call. = FALSE
    )
  }
  shiny_source <- system.file("viewer", package = "CerebroNexus")
  if (!dir.exists(shiny_source)) {
    stop(
      "Shiny source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }
  extdata_source <- system.file("extdata", package = "CerebroNexus")
  if (!dir.exists(extdata_source)) {
    stop(
      "extdata source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }

  # Preflight data inputs ----------------------------------------------------##
  private_data_root <- "private-data"
  preflight_data <- .preflightBundleData(cerebro_data)
  backends <- preflight_data$backends
  spatial_catalogs <- preflight_data$spatial_catalogs
  spatial_plot_rotation <- .normalizeAppSpatialPlotRotation(
    spatial_plot_rotation,
    spatial_catalogs
  )
  spatial_images <- .normalizeAppSpatialImages(
    spatial_images,
    spatial_catalogs
  )
  spatial_image_settings <- .normalizeAppSpatialImageSettings(
    spatial_image_settings,
    spatial_catalogs,
    spatial_images
  )
  legacy_settings <- list(
    spatial_images_flip_x = "flip_x",
    spatial_images_flip_y = "flip_y",
    spatial_images_scale_x = "scale_x",
    spatial_images_scale_y = "scale_y",
    spatial_images_offset_x = "offset_x",
    spatial_images_offset_y = "offset_y"
  )
  for (argument in names(legacy_settings)) {
    spatial_image_settings <- .mergeAppSpatialImageSettings(
      spatial_image_settings,
      .normalizeLegacyAppSpatialSetting(
        get(argument, inherits = FALSE),
        argument,
        legacy_settings[[argument]],
        spatial_catalogs,
        spatial_images
      )
    )
  }
  crb_targets <- paste0(private_data_root, "/", basename(cerebro_data))
  copy_plan <- list()
  claimed_targets <- character()
  claimed_keys <- character()
  claimed_sources <- character()
  claimed_artifacts <- character()
  claimed_directories <- logical()
  claim_target <- function(target, source, artifact, directory = FALSE) {
    target <- .portableBundlePath(
      target,
      paste0("The ", artifact, " bundle target '", target, "'")
    )
    key <- tolower(target)
    for (claim_index in seq_along(claimed_keys)) {
      existing <- claimed_targets[[claim_index]]
      existing_key <- claimed_keys[[claim_index]]
      if (identical(key, existing_key)) {
        duplicate <- identical(target, existing) &&
          identical(source, claimed_sources[[claim_index]]) &&
          identical(artifact, claimed_artifacts[[claim_index]]) &&
          identical(isTRUE(directory), claimed_directories[[claim_index]])
        if (duplicate) {
          return(invisible(FALSE))
        }
        stop(
          "Different inputs resolve to the same bundle target '",
          target,
          "'. Rename one input before building the app.",
          call. = FALSE
        )
      }
      if (
        startsWith(key, paste0(existing_key, "/")) ||
          startsWith(existing_key, paste0(key, "/"))
      ) {
        stop(
          "Bundle target '",
          target,
          "' conflicts with parent or child target '",
          existing,
          "'. Rename one backend before building the app.",
          call. = FALSE
        )
      }
    }
    claimed_targets <<- c(claimed_targets, target)
    claimed_keys <<- c(claimed_keys, key)
    claimed_sources <<- c(claimed_sources, source)
    claimed_artifacts <<- c(claimed_artifacts, artifact)
    claimed_directories <<- c(claimed_directories, isTRUE(directory))
    copy_plan[[length(copy_plan) + 1L]] <<- list(
      target = target,
      source = source,
      artifact = artifact,
      directory = directory
    )
  }

  if (!is.null(viewer_auth)) {
    claim_target(
      viewer_auth$credentials_path,
      viewer_auth$source,
      "authentication database"
    )
  }

  for (index in seq_along(cerebro_data)) {
    claim_target(
      crb_targets[[index]],
      resolved_crb_sources[[index]],
      "Cerebro data file"
    )
  }

  override_keys <- c("expression_matrix_h5", "expression_matrix_BPCells")
  result_target <- .canonicalTargetPath(result_dir)
  for (key in override_keys) {
    override <- cerebro_options[[key]]
    if (is.null(override)) {
      next
    }
    override <- .normalizeOverridePath(override, key)
    if (.pathWithin(override, result_target)) {
      stop(
        "cerebro_options[['",
        key,
        "']] must point outside 'result_dir'.",
        call. = FALSE
      )
    }
    cerebro_options[[key]] <- override
  }

  override_users <- list(
    expression_matrix_h5 = character(),
    expression_matrix_BPCells = character()
  )
  for (index in seq_along(backends)) {
    backend <- backends[[index]]
    override_key <- .bundleBackendOverrideKey(backend, cerebro_options)
    if (!is.null(override_key)) {
      override_users[[override_key]] <- c(
        override_users[[override_key]],
        resolved_crb_sources[[index]]
      )
    }
  }
  distinct_override_users <- vapply(
    override_users,
    function(paths) length(unique(paths)),
    integer(1)
  )
  unsafe_override <- names(override_users)[distinct_override_users > 1L]
  if (length(unsafe_override) > 0L) {
    stop(
      "The global override cerebro_options[['",
      unsafe_override[[1L]],
      "']] would bind multiple Cerebro data files to the same expression ",
      "matrix. Use each .crb's own backend or build separate apps.",
      call. = FALSE
    )
  }

  effective_backend_entries <- list()
  for (index in seq_along(cerebro_data)) {
    file <- cerebro_data[[index]]
    backend <- backends[[index]]
    if (!identical(backend$type, "embedded")) {
      backend$location <- .portableBundlePath(
        backend$location,
        paste0(
          "The ",
          backend$type,
          " backend location in '",
          basename(file),
          "'"
        )
      )
    }
    effective <- .effectiveBundleBackendPlan(backend, cerebro_options)
    target <- crb_targets[[index]]
    existing <- effective_backend_entries[[target]]
    if (!is.null(existing) && !identical(existing, effective)) {
      stop(
        "The Cerebro data file '",
        basename(file),
        "' changed its expression-backend descriptor during preflight.",
        call. = FALSE
      )
    }
    effective_backend_entries[[target]] <- effective
    if (!identical(effective$mode, "bundled")) {
      next
    }

    parts <- strsplit(backend$location, "/", fixed = TRUE)[[1L]]
    source_root <- dirname(file)
    source <- file.path(source_root, backend$location)
    is_directory <- identical(backend$type, "bpcells")
    source_exists <- if (is_directory) {
      dir.exists(source)
    } else {
      file.exists(source) && !dir.exists(source)
    }
    if (!source_exists) {
      stop(
        "Expected the ",
        backend$type,
        " backend at '",
        source,
        "' recorded by '",
        basename(file),
        "', but it was not found.",
        call. = FALSE
      )
    }

    resolved_root <- normalizePath(
      source_root,
      winslash = "/",
      mustWork = TRUE
    )
    resolved_source <- normalizePath(
      source,
      winslash = "/",
      mustWork = TRUE
    )
    root_prefix <- if (identical(resolved_root, "/")) {
      "/"
    } else {
      paste0(sub("/+$", "", resolved_root), "/")
    }
    if (
      !startsWith(resolved_source, root_prefix) ||
        .backendPathContainsSymbolicLink(source_root, parts, source)
    ) {
      stop(
        "The ",
        backend$type,
        " backend location '",
        backend$location,
        "' in '",
        basename(file),
        "' resolves through a symbolic link. Copy the real backend beside ",
        "the .crb before building the app.",
        call. = FALSE
      )
    }
    claim_target(
      paste0(private_data_root, "/", backend$location),
      resolved_source,
      paste0(backend$type, " backend"),
      directory = is_directory
    )
  }

  ## Only spatial background images enter the public resource namespace. Raw
  ## Cerebro data and external backends remain under private-data/. Historical
  ## apps mapped /data over HTTP, so replacement bundles never reuse that path.
  if (!is.null(spatial_images) && length(spatial_images) > 0L) {
    bundled_spatial_images <- list()
    for (dataset in names(spatial_images)) {
      for (spatial_name in names(spatial_images[[dataset]])) {
        declarations <- spatial_images[[dataset]][[spatial_name]]
        for (image_label in names(declarations)) {
          descriptor <- declarations[[image_label]]
          image <- descriptor$path
          target <- .spatialImageBundleTarget(
            dataset,
            spatial_name,
            image_label,
            basename(image)
          )
          claim_target(
            target,
            normalizePath(image, winslash = "/", mustWork = TRUE),
            "spatial image"
          )
          bundled_descriptor <- descriptor
          bundled_descriptor$path <- target
          bundled_spatial_images[[dataset]][[spatial_name]][[image_label]] <-
            if (identical(names(bundled_descriptor), "path")) {
              target
            } else {
              bundled_descriptor
            }
        }
      }
    }
    spatial_images <- if (length(bundled_spatial_images) > 0L) {
      bundled_spatial_images
    } else {
      NULL
    }
  }

  # Assemble a private sibling stage ----------------------------------------##
  publish_mode <- if (dir.exists(result_dir)) {
    file.info(result_dir)$mode[[1L]]
  } else {
    current_umask <- strtoi(as.character(Sys.umask(NA)), base = 8L)
    as.octmode(bitwAnd(strtoi("777", base = 8L), bitwNot(current_umask)))
  }
  stage_result_dir <- tempfile(
    pattern = paste0(".", basename(result_dir), "-stage-"),
    tmpdir = result_parent
  )
  if (!dir.create(stage_result_dir, mode = "0700", showWarnings = FALSE)) {
    stop("Failed to create a private app staging directory.", call. = FALSE)
  }
  bundle_cleanup$stage <- stage_result_dir
  private_data_dir <- file.path(stage_result_dir, private_data_root)
  app_file <- file.path(stage_result_dir, "app.R")
  build_ops <- .bundleBuildOps()

  if (verbose) {
    cat("Creating staged directory structure...\n")
  }
  dir.create(private_data_dir, recursive = TRUE, showWarnings = FALSE)
  extra_table_bundle <- .materializeExtraTables(
    extra_table_plan,
    stage_result_dir,
    save_rds = build_ops$save_extra_rds
  )

  if (verbose) {
    cat("Copying Shiny source files...\n")
  }
  if (!build_ops$copy(shiny_source, stage_result_dir, recursive = TRUE)) {
    stop("Failed to copy Shiny source files.", call. = FALSE)
  }

  if (verbose) {
    cat("Copying data artifacts...\n")
  }
  for (entry in copy_plan) {
    target <- file.path(stage_result_dir, entry$target)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    copied <- if (isTRUE(entry$directory)) {
      build_ops$copy(entry$source, dirname(target), recursive = TRUE)
    } else {
      build_ops$copy(entry$source, target, overwrite = FALSE)
    }
    copied_target_exists <- if (isTRUE(entry$directory)) {
      dir.exists(target)
    } else {
      file.exists(target) && !dir.exists(target)
    }
    if (!isTRUE(copied) || !copied_target_exists) {
      stop(
        "Failed to copy ",
        entry$artifact,
        ": ",
        entry$target,
        call. = FALSE
      )
    }
    if (verbose) {
      cat("  -", entry$target, paste0("(", entry$artifact, ")\n"))
    }
  }

  if (!is.null(viewer_auth)) {
    auth_database <- file.path(
      stage_result_dir,
      viewer_auth$credentials_path
    )
    auth_directory <- dirname(auth_database)
    private_modes <- TRUE
    if (!identical(.Platform$OS.type, "windows")) {
      changed <- isTRUE(build_ops$chmod(auth_directory, mode = "0700")) &&
        isTRUE(build_ops$chmod(auth_database, mode = "0600"))
      private_modes <- changed &&
        identical(build_ops$mode(auth_directory), 448L) &&
        identical(build_ops$mode(auth_database), 384L)
    }
    accessible <- build_ops$access(auth_database, mode = 4L) == 0L &&
      build_ops$access(auth_directory, mode = 1L) == 0L
    if (!isTRUE(private_modes) || !isTRUE(accessible)) {
      stop(
        "Failed to prepare the authentication database for runtime.",
        call. = FALSE
      )
    }
  }

  # Copy extdata -------------------------------------------------------------##
  if (verbose) {
    cat("Copying extdata files...\n")
  }
  if (!build_ops$copy(extdata_source, stage_result_dir, recursive = TRUE)) {
    stop("Failed to copy extdata files.", call. = FALSE)
  }

  # Build Cerebro.options ----------------------------------------------------##
  if (verbose) {
    cat("Generating app.R file...\n")
  }

  crb_files <- setNames(
    crb_targets,
    names(cerebro_data)
  )

  cerebro_options[["mode"]] <- if (show_upload_ui) "open" else "closed"
  ## Resolve the version while the package is present, then serialize it into
  ## the generated app. The standalone bundle never needs CerebroNexus at
  ## runtime merely to render its About page.
  cerebro_options[["cerebro_version"]] <- as.character(
    utils::packageVersion("CerebroNexus")
  )
  cerebro_options[["crb_file_to_load"]] <- crb_files
  cerebro_options[["cerebro_root"]] <- "."
  internal_option_names <- c(
    ".bundle_backend_plan",
    ".bundle_run_options",
    ".viewer_auth",
    "initial_page",
    "extra_tables"
  )
  option_names <- names(cerebro_options)
  if (!is.null(option_names)) {
    remove_internal <- !is.na(option_names) &
      option_names %in% internal_option_names
    cerebro_options <- cerebro_options[!remove_internal]
  }
  cerebro_options[[".bundle_backend_plan"]] <- list(
    schema_version = 1L,
    entries = effective_backend_entries
  )
  cerebro_options[[".bundle_run_options"]] <- bundle_run_options
  if (!is.null(viewer_auth)) {
    cerebro_options[[".viewer_auth"]] <- viewer_auth[c(
      "credentials_path",
      "passphrase_env",
      "timeout_minutes"
    )]
  }
  if (!is.null(crb_pick_smallest_file)) {
    cerebro_options[["crb_pick_smallest_file"]] <- crb_pick_smallest_file
  }
  if (!is.null(show_upload_ui)) {
    cerebro_options[["show_upload_ui"]] <- show_upload_ui
  }
  if (!is.null(initial_page)) {
    cerebro_options[["initial_page"]] <- initial_page
  }
  cerebro_options[["point_size"]] <- point_size
  cerebro_options[["point_opacity"]] <- point_opacity
  cerebro_options[["percentage_cells_to_show"]] <- percentage_cells_to_show
  if (!is.null(colors)) {
    cerebro_options[["colors"]] <- colors
  }
  if (!is.null(welcome_message)) {
    cerebro_options[["welcome_message"]] <- welcome_message
  }
  if (!is.null(variable_to_compare)) {
    cerebro_options[["variable_to_compare"]] <- variable_to_compare
  }
  if (!is.null(spatial_images)) {
    cerebro_options[["spatial_images"]] <- spatial_images
  }
  if (!is.null(spatial_image_settings)) {
    cerebro_options[["spatial_image_settings"]] <- spatial_image_settings
  }
  if (!is.null(spatial_plot_rotation)) {
    cerebro_options[["spatial_plot_rotation"]] <- spatial_plot_rotation
  }
  if (!is.null(extra_table_bundle)) {
    cerebro_options[["extra_tables"]] <- extra_table_bundle
  }

  build_ops$save_rds(
    cerebro_options,
    file.path(stage_result_dir, "cerebro_config.rds")
  )

  # Generate app.R -----------------------------------------------------------##
  app_content <- dedent(
    '
    library(dplyr)
    library(DT)
    library(plotly)
    library(shiny)
    library(shinydashboard)
    library(shinyWidgets)

    cerebro_root <- "."

    if (file.exists("cerebro_config.rds")) {
      Cerebro.options <<- readRDS("cerebro_config.rds")
    } else {
      stop("cerebro_config.rds not found!")
    }

    if (!is.null(Cerebro.options$colors)) {
      colors <- Cerebro.options$colors
    }

    bundle_run_options <- Cerebro.options$.bundle_run_options
    shiny_options <- bundle_run_options$shiny_app_options

    source(file.path(cerebro_root, "viewer/shiny_UI.R"))
    source(file.path(cerebro_root, "viewer/shiny_server.R"))
    source(file.path(cerebro_root, "viewer/auth.R"), local = TRUE)

    viewer_app <- viewer_auth_apply(
      ui,
      server,
      Cerebro.options[[".viewer_auth"]],
      Cerebro.options[["cerebro_root"]]
    )

    shiny::shinyApp(
      ui = viewer_app$ui,
      server = viewer_app$server,
      onStart = function() {
        previous <- options(
          shiny.maxRequestSize = bundle_run_options$max_request_size_bytes
        )
        shiny::onStop(function() {
          options(previous)
        })
      },
      options = shiny_options
    )
  '
  )

  build_ops$write_lines(app_content, app_file)
  tryCatch(
    parse(file = app_file, keep.source = FALSE),
    error = function(error_condition) {
      stop(
        "Generated app.R is invalid: ",
        conditionMessage(error_condition),
        call. = FALSE
      )
    }
  )
  .publishBundleStage(
    stage_result_dir,
    result_dir,
    overwrite,
    publish_mode
  )

  # Summary ------------------------------------------------------------------##
  if (verbose) {
    cat("\n")
    cat("========================================\n")
    cat("Shiny app successfully created!\n")
    cat("========================================\n")
    cat("App directory:", result_dir, "\n")
    cat("Data file(s):\n")
    for (i in seq_along(cerebro_data)) {
      label <- names(cerebro_data)[i]
      if (!is.null(label) && nzchar(label)) {
        cat("  -", label, ":", basename(cerebro_data[i]), "\n")
      } else {
        cat("  -", basename(cerebro_data[i]), "\n")
      }
    }
    cat("Port:", port, "\n")
    cat("Host:", host, "\n")
    cat("Launch browser:", launch_browser, "\n")
    cat("\nTo launch the app, run:\n")
    cat("  setwd('", result_dir, "')\n", sep = "")
    cat("  shiny::runApp('app.R')\n")
    cat("========================================\n")
  }

  returned_result_dir <- requested_result_dir
  current_requested_target <- tryCatch(
    .stableBundleTarget(requested_result_dir),
    error = function(error_condition) NULL
  )
  if (!identical(current_requested_target, result_dir)) {
    warning(
      "The requested result_dir path changed while the app was being built; ",
      "returning the frozen published path instead: ",
      result_dir,
      call. = FALSE
    )
    returned_result_dir <- result_dir
  }
  invisible(returned_result_dir)
}
