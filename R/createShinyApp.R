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
  if (!.isString(host)) {
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

.portableBundlePath <- function(path, subject) {
  .crbContract()$.runtimePortableBackendPath(
    path,
    subject,
    paste0(
      "must be one portable relative path using forward slashes, without ",
      "empty, '.', '..', or Windows-incompatible segments."
    )
  )
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

## Windows device, extended-length and malformed paths fail closed.
.rejectWindowsNamespacePath <- function(style) {
  if (style %in% c("windows-device", "windows-malformed")) {
    stop(
      "Windows ",
      if (identical(style, "windows-device")) {
        "device or extended-length"
      } else {
        "malformed Windows"
      },
      " path namespaces are not supported.",
      call. = FALSE
    )
  }
}

.lexicalAbsolutePath <- function(
  path,
  style = .absolutePathStyle(path),
  os_type = .Platform$OS.type
) {
  parsed <- .absolutePathComponents(path, style, os_type = os_type)
  normalized <- character()
  for (part in parsed$parts) {
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
  .composeAbsolutePath(parsed$root, normalized)
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
  .rejectWindowsNamespacePath(style)
  parsed <- .absolutePathComponents(path, style, os_type = os_type)
  if (identical(os_type, "windows") && identical(style, "unc")) {
    unc_root_parts <- strsplit(
      sub("^/+", "", parsed$root),
      "/",
      fixed = TRUE
    )[[1L]]
    if (any(.crbContract()$.runtimeWindowsPathSegmentInvalid(unc_root_parts))) {
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
        .crbContract()$.runtimeWindowsPathSegmentInvalid(part)
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
  .rejectWindowsNamespacePath(style)
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

## Treat the ordinary field as data. The getter binding is checked only as a
## format marker and is never invoked during preflight. Callers check the
## location's portability with their own message.
.readBundleBackend <- function(
  crb_path,
  object = .readCerebroPayload(crb_path)
) {
  recognized <- .recognizedCerebroObject(object)
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
  .crbContract()$.readRuntimeBackendDescriptor(
    object,
    crb_path,
    portable = FALSE
  )
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
      object$getSpatialData(spatial_name, hydrate_molecules = FALSE),
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
  trekker <- if (
    exists("getTrekker", envir = object, inherits = FALSE) &&
      !bindingIsActive("getTrekker", object) &&
      !isTRUE(rlang::env_binding_are_lazy(object, "getTrekker")) &&
      is.function(object[["getTrekker"]])
  ) {
    tryCatch(
      object$getTrekker(),
      error = function(error) {
        stop(
          "Could not read dataset `",
          dataset,
          "` Trekker data: ",
          conditionMessage(error),
          call. = FALSE
        )
      }
    )
  } else {
    NULL
  }
  if (!is.null(trekker)) {
    if ("trekker" %in% available) {
      stop(
        "Dataset `",
        dataset,
        "` uses reserved spatial entry name `trekker` while also containing ",
        "Trekker data.",
        call. = FALSE
      )
    }
    catalog[["trekker"]] <- character()
  }
  catalog
}

.datasetInfoScalar <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    return(NA_character_)
  }
  as.character(value)
}

.readBundleDatasetInfo <- function(object, label, path) {
  if (!.recognizedCerebroObject(object)) {
    stop(
      "Dataset information requires a recognized Cerebro object.",
      call. = FALSE
    )
  }
  experiment <- object$getExperiment()
  metadata <- object$getMetaData()
  overview_helpers <- new.env(parent = baseenv())
  sys.source(
    .cerebroPackageResource("viewer", "core", "dataset_overview.R"),
    overview_helpers
  )
  overview <- overview_helpers$viewerDatasetOverview(object)
  # Thin CRBs omit expression. Read only the external matrix dimensions here.
  if (is.null(overview$genes)) {
    overview["genes"] <- list(tryCatch(
      {
        descriptor <- .readBundleBackend(path, object)
        if (identical(descriptor$type, "h5")) {
          .h5Dim(.cerebroSidecar(path, descriptor))[[2L]]
        } else {
          NULL
        }
      },
      error = function(error) NULL
    ))
  }
  backend <- tryCatch(object$immune_repertoire_backend, error = function(e) {
    NULL
  })
  if (is.list(backend) && identical(backend$type, "file")) {
    immune_repertoire <- TRUE
    tcr_repertoire <- any(as.character(backend$chains) %in% c("TRA", "TRB"))
  } else {
    repertoire <- tryCatch(object$getImmuneRepertoire(), error = function(e) {
      list()
    })
    immune_repertoire <- is.list(repertoire) && length(repertoire) > 0L
    tcr_repertoire <- immune_repertoire &&
      any(vapply(
        repertoire,
        function(sample) {
          !is.null(sample) &&
            "CTgene" %in% names(sample) &&
            any(grepl("TR[AB]", as.character(sample$CTgene)), na.rm = TRUE)
        },
        logical(1)
      ))
  }
  list(
    label = as.character(label),
    path = as.character(path),
    cells = as.integer(nrow(metadata)),
    organism = .datasetInfoScalar(experiment$organism),
    date = .datasetInfoScalar(experiment$date_of_export),
    immune_repertoire = immune_repertoire,
    tcr_repertoire = tcr_repertoire,
    projection_names = as.character(object$availableProjections()),
    overview = overview,
    capabilities = .viewerDatasetCapabilities(object)
  )
}

.datasetCatalogFromFiles <- function(
  files,
  retain_prototypes = FALSE,
  prototype_limit = 2L
) {
  if (is.null(files) || !is.character(files) || !length(files)) {
    return(list())
  }
  labels <- names(files)
  if (is.null(labels) || length(labels) != length(files)) {
    labels <- basename(files)
  }
  catalog <- list()
  prototypes <- list()
  for (index in seq_along(files)) {
    path <- files[[index]]
    if (!file.exists(path) || dir.exists(path)) {
      next
    }
    object <- .readCerebroPayload(path)
    if (.recognizedCerebroObject(object)) {
      .crbContract()$.assertCurrentRuntimeCrb(object, path)
    }
    catalog[[path]] <- .readBundleDatasetInfo(
      object,
      labels[[index]],
      path
    )
    if (isTRUE(retain_prototypes) && length(prototypes) < prototype_limit) {
      prototypes[[path]] <- object
    }
    object <- NULL
  }
  if (isTRUE(retain_prototypes)) {
    attr(catalog, "prototypes") <- prototypes
  }
  catalog
}

.preflightBundleData <- function(
  cerebro_data,
  read_object = .readCerebroPayload,
  inspect_backend = .readBundleBackend,
  inspect_spatial = .readBundleSpatialCatalog,
  release_object = function(object) invisible(NULL)
) {
  backends <- vector("list", length(cerebro_data))
  spatial_backends <- vector("list", length(cerebro_data))
  immune_backends <- vector("list", length(cerebro_data))
  spatial_catalogs <- vector("list", length(cerebro_data))
  dataset_catalog <- vector("list", length(cerebro_data))
  viewer_pack_valid <- logical(length(cerebro_data))
  names(backends) <- names(cerebro_data)
  names(spatial_backends) <- names(cerebro_data)
  names(immune_backends) <- names(cerebro_data)
  names(spatial_catalogs) <- names(cerebro_data)
  names(dataset_catalog) <- names(cerebro_data)
  for (index in seq_along(cerebro_data)) {
    cached <- .takeBundlePreflightData(
      cerebro_data[[index]],
      names(cerebro_data)[[index]]
    )
    ## Immune sidecars are only inspected for recognized Cerebro objects, and a
    ## file-backed repertoire always marks the dataset info as immune.
    immune_absent <- !is.null(cached) &&
      (is.null(cached$dataset_info) ||
        identical(cached$dataset_info$immune_repertoire, FALSE))
    if (!is.null(cached) && (isTRUE(cached$immune_known) || immune_absent)) {
      backends[[index]] <- cached$backend
      spatial_backends[index] <- list(cached$spatial_backend)
      immune_backends[index] <- list(cached$immune_backend)
      spatial_catalogs[[index]] <- cached$spatial_catalog
      dataset_catalog[index] <- list(cached$dataset_info)
      ## the CRB is not read again, so its Viewer Pack is checked the long way
      viewer_pack_valid[[index]] <- .bundleViewerPackValid(
        cerebro_data[[index]],
        NULL
      )
      next
    }
    object <- read_object(cerebro_data[[index]])
    inspection_error <- NULL
    release_error <- NULL
    tryCatch(
      {
        backends[[index]] <- inspect_backend(cerebro_data[[index]], object)
        if (.recognizedCerebroObject(object)) {
          dataset_catalog[index] <- list(.readBundleDatasetInfo(
            object,
            names(cerebro_data)[[index]],
            cerebro_data[[index]]
          ))
          ## `[<-` with list(): `[[<-` NULL would delete the slot.
          spatial_backends[index] <- list(.spatialMoleculeBackend(
            object,
            cerebro_data[[index]]
          ))
          immune_backends[index] <- list(.immuneRepertoireBackend(
            object,
            cerebro_data[[index]]
          ))
        }
        spatial_catalogs[[index]] <- inspect_spatial(
          object,
          names(cerebro_data)[[index]]
        )
        viewer_pack_valid[[index]] <- .bundleViewerPackValid(
          cerebro_data[[index]],
          object
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
  list(
    backends = backends,
    spatial_backends = spatial_backends,
    immune_backends = immune_backends,
    spatial_catalogs = spatial_catalogs,
    dataset_catalog = dataset_catalog,
    viewer_pack_valid = viewer_pack_valid
  )
}

## Whether the Viewer Pack beside a CRB is valid, checked while preflight has
## the CRB's object, so a thin CRB is not read a second time.
.bundleViewerPackValid <- function(path, payload) {
  file <- normalizePath(path, winslash = "/", mustWork = FALSE)
  pack <- .viewerPackPath(file)
  dir.exists(pack) &&
    isTRUE(tryCatch(
      .validateViewerPack(file, pack, full = TRUE, payload = payload)$valid,
      error = function(error) FALSE
    ))
}

.bundlePathDigest <- function(bytes) {
  path <- tempfile("cerebro-bundle-path-")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::md5sum(path))
}

.spatialImageBundlePathField <- function(value) {
  if (!.isString(value)) {
    stop(
      "Spatial image bundle path components must be non-empty strings.",
      call. = FALSE
    )
  }
  bytes <- charToRaw(enc2utf8(value))
  c(charToRaw(sprintf("%08x:", length(bytes))), bytes)
}

.spatialImageBundleTarget <- function(
  dataset,
  spatial_name,
  image_label,
  filename
) {
  fields <- lapply(
    list(dataset, spatial_name, image_label, filename),
    .spatialImageBundlePathField
  )
  digest <- .bundlePathDigest(c(
    charToRaw("cerebro-spatial-image-v1:"),
    do.call(c, fields)
  ))
  extension <- tools::file_ext(filename)
  extension_is_safe <- grepl("^[A-Za-z0-9]{1,16}$", extension)
  target <- paste0(
    "spatial-assets/u",
    digest,
    if (extension_is_safe) paste0(".", extension) else ""
  )
  .portableBundlePath(
    target,
    paste0("The spatial image bundle target '", target, "'")
  )
}

.datasetBundleTarget <- function(dataset_id) {
  if (!.isString(dataset_id)) {
    stop("A non-empty dataset identity is required.", call. = FALSE)
  }
  digest <- .bundlePathDigest(charToRaw(enc2utf8(dataset_id)))
  paste0("private-data/dataset-", digest, ".crb")
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

.bundleWindowsExtendedPath <- function(
  path,
  os_type = .Platform$OS.type
) {
  if (
    !identical(os_type, "windows") ||
      !is.character(path) ||
      length(path) != 1L ||
      is.na(path)
  ) {
    return(path)
  }
  path <- gsub("/", "\\", path, fixed = TRUE)
  if (startsWith(path, "\\\\?\\")) {
    return(path)
  }
  if (startsWith(path, "\\\\")) {
    return(paste0("\\\\?\\UNC\\", substring(path, 3L)))
  }
  if (grepl("^[A-Za-z]:\\\\", path)) {
    return(paste0("\\\\?\\", path))
  }
  path
}

.bundleCreateDirectory <- function(
  path,
  recursive = FALSE,
  mode = "0777",
  showWarnings = TRUE,
  os_type = .Platform$OS.type,
  .dir_create = dir.create
) {
  .dir_create(
    .bundleWindowsExtendedPath(path, os_type = os_type),
    recursive = recursive,
    mode = mode,
    showWarnings = showWarnings
  )
}

.bundleCopiedTargetExists <- function(
  path,
  directory = FALSE,
  os_type = .Platform$OS.type,
  .file_exists = file.exists,
  .dir_exists = dir.exists
) {
  path <- .bundleWindowsExtendedPath(path, os_type = os_type)
  if (isTRUE(directory)) {
    .dir_exists(path)
  } else {
    .file_exists(path) && !.dir_exists(path)
  }
}

.bundleCopyWindowsDirectory <- function(
  from,
  to,
  overwrite = TRUE,
  copy.mode = TRUE,
  copy.date = FALSE,
  .fallback = file.copy
) {
  source <- tryCatch(
    normalizePath(from, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (is.null(source) || !dir.exists(source)) {
    return(FALSE)
  }
  destination <- if (.bundleCopiedTargetExists(to, directory = TRUE)) {
    file.path(to, basename(source))
  } else {
    to
  }
  if (
    .bundleCopiedTargetExists(destination) ||
      (.bundleCopiedTargetExists(destination, directory = TRUE) &&
        !isTRUE(overwrite))
  ) {
    return(FALSE)
  }
  if (
    !.bundleCopiedTargetExists(destination, directory = TRUE) &&
      !.bundleCreateDirectory(destination, recursive = TRUE)
  ) {
    return(FALSE)
  }
  entries <- tryCatch(
    as.character(fs::dir_ls(
      source,
      all = TRUE,
      recurse = TRUE,
      type = "any",
      fail = TRUE
    )),
    error = function(error) NULL
  )
  if (is.null(entries)) {
    return(FALSE)
  }
  if (!length(entries)) {
    return(TRUE)
  }
  linked <- tryCatch(fs::is_link(entries), error = function(error) NULL)
  if (
    is.null(linked) ||
      length(linked) != length(entries) ||
      anyNA(linked) ||
      any(linked)
  ) {
    return(FALSE)
  }
  entries <- gsub("\\", "/", entries, fixed = TRUE)
  source_prefix <- paste0(source, "/")
  if (any(!startsWith(entries, source_prefix))) {
    return(FALSE)
  }
  relative <- substring(entries, nchar(source_prefix) + 1L)
  if (
    any(!nzchar(relative)) ||
      any(startsWith(relative, "/")) ||
      any(grepl("(^|/)\\.\\.?(/|$)", relative))
  ) {
    return(FALSE)
  }
  directories <- vapply(
    entries,
    function(path) {
      .bundleCopiedTargetExists(path, directory = TRUE)
    },
    logical(1)
  )
  regular_files <- vapply(
    entries,
    function(path) {
      .bundleCopiedTargetExists(path)
    },
    logical(1)
  )
  if (any(directories == regular_files)) {
    return(FALSE)
  }
  directory_targets <- file.path(destination, relative[directories])
  if (
    length(directory_targets) &&
      !all(vapply(
        directory_targets,
        function(path) {
          .bundleCopiedTargetExists(path, directory = TRUE) ||
            .bundleCreateDirectory(path, recursive = TRUE)
        },
        logical(1)
      ))
  ) {
    return(FALSE)
  }
  file_sources <- entries[regular_files]
  file_targets <- file.path(destination, relative[regular_files])
  if (!length(file_sources)) {
    return(TRUE)
  }
  copied <- mapply(
    function(source_file, target_file) {
      if (!.bundleCopiedTargetExists(dirname(target_file), directory = TRUE)) {
        if (!.bundleCreateDirectory(dirname(target_file), recursive = TRUE)) {
          return(FALSE)
        }
      }
      result <- .fallback(
        .bundleWindowsExtendedPath(source_file),
        .bundleWindowsExtendedPath(target_file),
        overwrite = overwrite,
        recursive = FALSE,
        copy.mode = copy.mode,
        copy.date = copy.date
      )
      isTRUE(result) && .bundleCopiedTargetExists(target_file)
    },
    file_sources,
    file_targets,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  isTRUE(all(copied))
}

.bundleCopyPath <- function(
  from,
  to,
  overwrite = recursive,
  recursive = FALSE,
  copy.mode = TRUE,
  copy.date = FALSE,
  .sysname = unname(Sys.info()[["sysname"]]),
  .command = system2,
  .fallback = file.copy
) {
  scalar <- is.character(from) &&
    length(from) == 1L &&
    !is.na(from) &&
    is.character(to) &&
    length(to) == 1L &&
    !is.na(to)
  command_args <- if (scalar && identical(.sysname, "Darwin")) {
    c(
      "-c",
      if (isTRUE(recursive)) c("-R", "-L"),
      "--",
      shQuote(from),
      shQuote(to)
    )
  } else if (scalar && identical(.sysname, "Linux")) {
    c(
      if (isTRUE(recursive)) c("-R", "-L"),
      "--reflink=auto",
      "--",
      shQuote(from),
      shQuote(to)
    )
  } else {
    NULL
  }
  copy_command <- unname(Sys.which("cp"))
  destination <- if (scalar && dir.exists(from) && dir.exists(to)) {
    file.path(to, basename(from))
  } else {
    to
  }
  ## Sys.readlink() is NA for a path that does not exist, and nzchar(NA) is TRUE.
  destination_existed <- scalar &&
    (file.exists(destination) ||
      isTRUE(nzchar(Sys.readlink(destination), keepNA = TRUE)))
  if (
    scalar &&
      identical(.sysname, "Windows") &&
      isTRUE(recursive) &&
      dir.exists(from)
  ) {
    return(.bundleCopyWindowsDirectory(
      from,
      to,
      overwrite = overwrite,
      copy.mode = copy.mode,
      copy.date = copy.date,
      .fallback = .fallback
    ))
  }
  if (scalar && identical(.sysname, "Windows")) {
    extended_from <- .bundleWindowsExtendedPath(from, os_type = "windows")
    extended_to <- .bundleWindowsExtendedPath(to, os_type = "windows")
    if (!identical(extended_from, from) || !identical(extended_to, to)) {
      return(.fallback(
        extended_from,
        extended_to,
        overwrite = overwrite,
        recursive = recursive,
        copy.mode = copy.mode,
        copy.date = copy.date
      ))
    }
  }
  ## file.copy() leaves an existing target alone unless overwrite is TRUE.
  if (scalar && !isTRUE(overwrite) && destination_existed) {
    return(FALSE)
  }
  if (length(command_args) && nzchar(copy_command)) {
    status <- suppressWarnings(tryCatch(
      .command(
        copy_command,
        command_args,
        stdout = FALSE,
        stderr = FALSE
      ),
      error = function(error) 1L
    ))
    if (isTRUE(status == 0L)) {
      return(TRUE)
    }
    if (!destination_existed) {
      unlink(destination, recursive = TRUE, force = TRUE)
    }
  }
  .fallback(
    from,
    to,
    overwrite = overwrite,
    recursive = recursive,
    copy.mode = copy.mode,
    copy.date = copy.date
  )
}

.bundleBuildOps <- function() {
  list(
    access = function(path, mode) file.access(path, mode = mode),
    chmod = function(path, mode) Sys.chmod(path, mode = mode),
    copy = .bundleCopyPath,
    mode = function(path) as.integer(file.info(path)$mode),
    save_rds = function(object, file) saveRDS(object, file),
    save_extra_rds = function(object, file) .saveExtraTableRDS(object, file),
    write_lines = function(text, connection) writeLines(text, connection)
  )
}

.removeBundleSystemMetadata <- function(root) {
  metadata <- list.files(
    file.path(root, c("viewer", "extdata")),
    pattern = "^(\\.DS_Store|\\._.*)$",
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  if (!length(metadata)) {
    return(invisible(TRUE))
  }
  unlink(metadata, recursive = FALSE, force = TRUE)
  if (any(vapply(metadata, .bundlePathExists, logical(1)))) {
    stop(
      "Failed to remove filesystem metadata from the staged App.",
      call. = FALSE
    )
  }
  invisible(TRUE)
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

## Remove `path` only while it is an empty directory, so a lock that gained
## contents is preserved rather than deleted recursively. POSIX remove()
## already refuses non-empty directories, but on Windows file.remove() cannot
## delete any directory and unlink() only deletes them recursively; cmd's
## rmdir without /S has the POSIX semantics.
.removeEmptyDirectory <- function(path) {
  if (.Platform$OS.type != "windows") {
    return(file.remove(path))
  }
  status <- system2(
    Sys.getenv("COMSPEC", "cmd.exe"),
    c(
      "/d",
      "/c",
      "rmdir",
      shQuote(normalizePath(path, winslash = "\\", mustWork = FALSE), "cmd")
    ),
    stdout = FALSE,
    stderr = FALSE
  )
  if (!identical(as.integer(status), 0L)) {
    warning(
      "cannot remove directory '",
      path,
      "': it is not empty or not accessible",
      call. = FALSE
    )
    return(FALSE)
  }
  TRUE
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
  .rejectWindowsNamespacePath(result_style)
  result_dir <- path.expand(result_dir)
  .assertOutsideBundleLockNamespace(result_dir)
  .bundleResultLeaf(result_dir)

  parent <- dirname(result_dir)
  prospective_parent <- .canonicalTargetPath(parent)
  .assertOutsideBundleLockNamespace(prospective_parent)
  if (!dir.exists(prospective_parent)) {
    created <- .bundleCreateDirectory(
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
  if (
    !.bundleCreateDirectory(
      lock_path,
      mode = "0700",
      showWarnings = FALSE
    )
  ) {
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
    .removeEmptyDirectory(lock_path)
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
    .removeEmptyDirectory(release_path)
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
    access <- .attemptBundleOperation(function() {
      ops$access(result_dir, mode = 5L)
    })
    status <- access$value
    if (
      !is.null(access$condition) ||
        length(status) != 1L ||
        is.na(status) ||
        status != 0L
    ) {
      stop(
        "Failed to inspect 'result_dir': ",
        access$condition %||% "the directory is not readable and searchable",
        ".",
        call. = FALSE
      )
    }
    listing <- .attemptBundleOperation(function() ops$list_dir(result_dir))
    entries <- listing$value
    if (!is.null(listing$condition) || !is.character(entries)) {
      stop(
        "Failed to inspect 'result_dir': ",
        listing$condition %||%
          "the directory listing was not a character vector",
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

  ## After a failed step, restore the previous bundle unless something else
  ## now occupies the destination, then stop with `failure`.
  recover_backup <- function(failure, attempt) {
    if (.bundlePathExists(result_dir)) {
      stop(
        failure,
        " because an unexpected destination appeared at '",
        .canonicalTargetPath(result_dir),
        "'. The previous bundle remains recoverable at '",
        backup,
        "'.",
        .bundleOperationDetail(attempt),
        call. = FALSE
      )
    }
    restore_attempt <- .attemptBundleOperation(function() {
      ops$rename(backup, result_dir)
    })
    if (dir.exists(result_dir) && !.bundlePathExists(backup)) {
      stop(
        failure,
        "; the previous bundle was restored.",
        .bundleOperationDetail(attempt),
        call. = FALSE
      )
    }
    stop(
      failure,
      " and could not restore the previous bundle. It remains recoverable ",
      "at '",
      backup,
      "'.",
      .bundleOperationDetail(restore_attempt),
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
        recover_backup(
          "Failed to stage the existing app for replacement",
          backup_attempt
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
      recover_backup("Failed to publish the staged app bundle", publish_attempt)
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

.cerebroSourceRoot <- function() {
  source_root <- Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = "")
  if (!nzchar(source_root)) {
    return(NULL)
  }
  description <- file.path(source_root, "DESCRIPTION")
  package <- if (file.exists(description)) {
    tryCatch(
      read.dcf(description, fields = "Package")[[1L]],
      error = function(error) NA_character_
    )
  } else {
    NA_character_
  }
  if (
    identical(unname(package), "CerebroNexus") &&
      dir.exists(file.path(source_root, "inst")) &&
      length(list.files(file.path(source_root, "R"), pattern = "[.][Rr]$")) > 0L
  ) {
    normalizePath(source_root, winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }
}

.cerebroPackageResource <- function(...) {
  relative <- file.path(...)
  source_root <- .cerebroSourceRoot()
  if (!is.null(source_root)) {
    source_resource <- file.path(source_root, "inst", relative)
    return(normalizePath(
      source_resource,
      winslash = "/",
      mustWork = FALSE
    ))
  }
  system.file(..., package = "CerebroNexus")
}

.cerebroRuntimeVersion <- function() {
  source_root <- .cerebroSourceRoot()
  if (!is.null(source_root)) {
    description <- tryCatch(
      read.dcf(file.path(source_root, "DESCRIPTION"), fields = "Version"),
      error = function(e) NULL
    )
    if (!is.null(description) && nzchar(description[[1L]])) {
      return(as.character(description[[1L]]))
    }
  }
  as.character(utils::packageVersion("CerebroNexus"))
}

#' Create a self-contained CerebroNexus Shiny app folder
#'
#' Bundles a CerebroNexus Shiny app into \code{result_dir}, copying the
#' \code{inst/viewer/} sources, the requested \code{.crb} data file(s),
#' and \code{extdata/}, and writes an \code{app.R} that sources the bundled
#' UI/server. The output directory can be served directly by shiny-server or
#' run with \code{shiny::runApp(result_dir)}.
#'
#' Supports the external H5 expression backend in addition to the embedded
#' mode. Each \code{.crb} descriptor names a portable relative file, and so
#' does an immune repertoire sidecar; both are copied to the same private
#' location under \code{private-data/}. The historical \code{data/} name is deliberately
#' not reused: a still-running older generated app may retain its former
#' \code{/data} HTTP resource mapping during an in-place replacement. Raw
#' \code{.crb}, H5, and sidecar files are not registered as HTTP resources.
#' Spatial background images are the deliberate exception: files explicitly
#' supplied through \code{spatial_images} are copied verbatim under
#' \code{spatial-assets/}. The server exposes each selected file through a
#' private, session-scoped URL; image bytes are not Base64-encoded and the
#' directory is not registered as a public HTTP resource.
#' Callers must provide trusted image files. Preflight requires a minimum
#' stable runtime API and reads the ordinary \code{expression_backend} field
#' without invoking serialized methods or its getter. The generated
#' configuration stores the effective per-CRB attachment plan, and configured
#' CRBs consume that exact plan at runtime. Direct launches and uploads also
#' read and validate the ordinary field without invoking the getter. A CRB
#' from an older release that is no longer read (see
#' \code{\link{convertCerebro}}) is rejected, as is an object containing only
#' one of field and getter.
#' Dataset labels are unique. Two labels may name the same input file; the
#' bundle then holds one copy of it per label.
#' While developing the package, the environment variable
#' \code{CEREBRO_PACKAGE_SOURCE} may name a source checkout (a directory with
#' \code{DESCRIPTION} for \code{Package: CerebroNexus}, \code{R/} and
#' \code{inst/}); the Viewer files and the version written into the bundle then
#' come from it instead of from the installed package. Any other directory is
#' ignored.
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
#' Treat
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
#'   used as stable dataset identities. Paths must resolve to distinct files.
#' @param dataset_labels Optional named character vector mapping every dataset
#'   identity in \code{cerebro_data} to its display label. Values must be
#'   non-empty but may repeat. When omitted, the dataset identities are also
#'   used as display labels.
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
#'   the generated app. Duplicate internal entries are removed, and the keys
#'   \code{.bundle_backend_plan} and \code{.bundle_run_options} are written by
#'   the function.
#' @param overwrite If \code{TRUE} (default), replace \code{result_dir} only
#'   after a complete staged build succeeds. Publication failures attempt to
#'   restore the previous app and preserve its backup if restoration fails. If
#'   \code{FALSE}, \code{result_dir} must be absent or empty; a non-empty
#'   directory is rejected before any files are written.
#' @param verbose Print progress messages; defaults to TRUE.
#' @param crb_pick_smallest_file Forwarded to \code{Cerebro.options}.
#' @param show_upload_ui One non-missing logical controlling whether users may
#'   upload their own data; defaults to \code{FALSE}.
#' @param initial_dataset Optional exact dataset identity to load initially. This
#'   does not change the order of \code{cerebro_data}. URL selection and a
#'   session's current selection take precedence.
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
#'   \code{cerebro_data}. Applies across ordinary projection views, including
#'   Gene expression and Linked views; Spatial is configured independently.
#' @param point_opacity Point opacity from 0.1 through 1. Supply one number for
#'   every dataset, or a named numeric vector/list whose names exactly match
#'   \code{cerebro_data}. Applies across ordinary projection views, including
#'   Gene expression and Linked views; Spatial is configured independently.
#' @param percentage_cells_to_show Percentage of cells from 10 through 100.
#'   Supply one number for every dataset, or a named numeric vector/list whose
#'   names exactly match \code{cerebro_data}. The value initializes every cell
#'   projection view. Viewer edits are shared by projection pages for the current
#'   dataset; Spatial uses its own coordinate settings.
#' @param spatial_point_appearance Optional named list in
#'   \code{dataset -> FOV -> list(point_size, point_opacity)} form. Point size
#'   is from 1 to 20 and opacity from 0 to 1. Used by Spatial and spatial panels
#'   in Linked views, independently of projection appearance.
#' @param spatial_roi_settings Optional named list in
#'   \code{dataset -> FOV -> ROI -> list(rotation_degrees, point_size, point_opacity)}
#'   form, matching Builder Coordinate settings. Rotation is a Viewer rotation
#'   after any coordinate transform baked into the exported CRB.
#' @param spatial_percentage_cells_to_show Independent spatial sampling percentage
#'   from 10 to 100; one number or a numeric vector named by dataset. Defaults to 100.
#' @param variable_to_compare Forwarded to \code{Cerebro.options}.
#' @param spatial_images Optional nested external-image manifest in
#'   \code{dataset -> spatial entry -> image label -> path} form. Dataset names
#'   must match \code{cerebro_data}; spatial names must match the corresponding
#'   CRB's \code{availableSpatial()}, and image labels must be unique and
#'   non-empty within an entry. A leaf may instead be a descriptor containing
#'   \code{path} and optional coordinate \code{bounds}. Supplied files are
#'   copied to opaque, safe relative paths under \code{spatial-assets/}. A
#'   plain \code{c(Dataset = path)} vector is rejected; write
#'   \code{list(Dataset = list(<spatial entry> = c(label = path)))}.
#' @param spatial_image_settings Optional nested settings in
#'   \code{dataset -> spatial entry -> image label -> settings} form. Settings
#'   may contain only \code{flip_x}, \code{flip_y}, \code{scale_x},
#'   \code{scale_y}, \code{offset_x}, \code{offset_y}, and \code{rotation}.
#'   \code{image_opacity} may additionally set the initial image opacity from
#'   0 through 1 (default 1).
#'   A leaf may target an embedded or external image available under that exact
#'   dataset and spatial entry. The image label must exist in the union of the
#'   CRB's embedded images and this call's \code{spatial_images}; unknown
#'   identities are rejected. Labels are user-facing names, not protocol names.
#' @param spatial_plot_rotation Optional nested rotations in
#'   \code{dataset -> spatial entry -> degrees} form, applied only to spatial
#'   cell coordinates. Dataset names must match \code{cerebro_data}; spatial
#'   names must match the corresponding CRB's \code{availableSpatial()}. A
#'   per-dataset numeric vector is rejected.
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
#' @param initial_projections Optional projection names to show when Linked
#'   views first opens. A character vector applies to every data set. A named
#'   list configures individual \code{cerebro_data} labels; omitted labels keep
#'   the Viewer's default single-projection behavior.
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
#' @importFrom mirai mirai
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
  spatial_plot_rotation = NULL,
  auth = NULL,
  extra_tables = NULL,
  extra_tables_sheets = NULL,
  dataset_labels = NULL,
  initial_dataset = NULL,
  initial_page = NULL,
  initial_projections = NULL,
  spatial_point_appearance = NULL,
  spatial_roi_settings = NULL,
  spatial_percentage_cells_to_show = 100
) {
  # Validate inputs ----------------------------------------------------------##
  if (is.list(cerebro_data)) {
    valid_entries <- vapply(
      cerebro_data,
      function(path) {
        .isString(path)
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
      "cerebro_data identities must be non-empty and non-missing.",
      call. = FALSE
    )
  }
  if (anyDuplicated(data_labels)) {
    stop("cerebro_data identities must be unique.", call. = FALSE)
  }
  if (is.null(dataset_labels)) {
    dataset_labels <- stats::setNames(data_labels, data_labels)
  } else {
    if (
      !is.character(dataset_labels) ||
        is.object(dataset_labels) ||
        length(dataset_labels) != length(data_labels) ||
        is.null(names(dataset_labels)) ||
        anyNA(names(dataset_labels)) ||
        any(!nzchar(names(dataset_labels))) ||
        anyDuplicated(names(dataset_labels)) ||
        !setequal(names(dataset_labels), data_labels) ||
        anyNA(dataset_labels) ||
        any(!nzchar(trimws(dataset_labels)))
    ) {
      stop(
        "dataset_labels must map every cerebro_data identity to one non-empty display label.",
        call. = FALSE
      )
    }
    dataset_labels <- dataset_labels[data_labels]
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
  validate_initial_projections <- function(value, context) {
    if (
      !is.character(value) ||
        is.object(value) ||
        !length(value) ||
        anyNA(value) ||
        any(!nzchar(trimws(value))) ||
        anyDuplicated(value)
    ) {
      stop(
        context,
        " must contain unique, non-empty projection names.",
        call. = FALSE
      )
    }
    unname(value)
  }
  initial_projections_by_dataset <- NULL
  if (is.character(initial_projections)) {
    shared_initial_projections <- validate_initial_projections(
      initial_projections,
      "'initial_projections'"
    )
    initial_projections_by_dataset <- setNames(
      rep(list(shared_initial_projections), length(data_labels)),
      data_labels
    )
  } else if (!is.null(initial_projections)) {
    projection_labels <- names(initial_projections)
    if (
      !is.list(initial_projections) ||
        is.object(initial_projections) ||
        !length(initial_projections) ||
        is.null(projection_labels) ||
        anyNA(projection_labels) ||
        any(!nzchar(projection_labels)) ||
        anyDuplicated(projection_labels) ||
        any(!projection_labels %in% data_labels)
    ) {
      stop(
        paste0(
          "'initial_projections' must be NULL, a character vector, or a ",
          "named list keyed by cerebro_data labels."
        ),
        call. = FALSE
      )
    }
    initial_projections_by_dataset <- lapply(
      projection_labels,
      function(label) {
        validate_initial_projections(
          initial_projections[[label]],
          paste0("initial_projections[[", sQuote(label), "]]")
        )
      }
    )
    names(initial_projections_by_dataset) <- projection_labels
  }
  if (!is.null(initial_projections_by_dataset)) {
    viewer_content <- cerebro_options[["viewer_content"]] %||% list()
    if (!is.list(viewer_content) || is.object(viewer_content)) {
      stop(
        "cerebro_options[['viewer_content']] must be an ordinary list.",
        call. = FALSE
      )
    }
    for (dataset in names(initial_projections_by_dataset)) {
      item <- viewer_content[[dataset]] %||% list()
      if (!is.list(item) || is.object(item)) {
        stop(
          "Each cerebro_options[['viewer_content']] entry must be an ordinary list.",
          call. = FALSE
        )
      }
      item[["initial_projections"]] <- initial_projections_by_dataset[[dataset]]
      viewer_content[[dataset]] <- item
    }
    cerebro_options[["viewer_content"]] <- viewer_content
  }
  ## The per-dataset `spatial_images_flip_x` ... `spatial_images_offset_y`
  ## options were removed; they stay reserved so they are not silently ignored.
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
  retired_options <- intersect(
    supplied_option_names,
    c("expression_matrix_h5", "expression_matrix_BPCells")
  )
  if (length(retired_options)) {
    stop(
      "`cerebro_options` contains retired key(s): ",
      paste(retired_options, collapse = ", "),
      ". A data file finds its expression matrix through its own ",
      "descriptor, next to the .crb; export it with ",
      "exportFromSeurat(expression_matrix_mode = \"h5\").",
      call. = FALSE
    )
  }
  retired_page_options <- intersect(
    supplied_option_names,
    paste0(
      "expression_",
      c("point_size", "point_opacity", "percentage_cells_to_show")
    )
  )
  if (length(retired_page_options)) {
    stop(
      "`cerebro_options` contains per-page scatter option(s) that no longer ",
      "exist: ",
      paste(retired_page_options, collapse = ", "),
      ". All projection views share the point_size, point_opacity and ",
      "percentage_cells_to_show parameters of createShinyApp().",
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
      ". Supply `spatial_images` and `spatial_image_settings` through the ",
      "createShinyApp() parameters instead; per-image flips, scales and ",
      "offsets go in `spatial_image_settings = list(Dataset = ",
      "list(<spatial entry> = list(<image label> = list(flip_x = TRUE))))`.",
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
  if (
    !is.null(initial_dataset) &&
      (!is.character(initial_dataset) ||
        length(initial_dataset) != 1L ||
        is.na(initial_dataset) ||
        !initial_dataset %in% data_labels)
  ) {
    stop(
      "'initial_dataset' must be NULL or exactly one cerebro_data identity.",
      call. = FALSE
    )
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
      !.isString(result_dir)
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

  if (!requireNamespace("CerebroNexus", quietly = TRUE)) {
    stop(
      "Package 'CerebroNexus' is required but not installed.",
      call. = FALSE
    )
  }
  shiny_source <- .cerebroPackageResource("viewer")
  if (!dir.exists(shiny_source)) {
    stop(
      "Shiny source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }
  extdata_source <- .cerebroPackageResource("extdata")
  if (!dir.exists(extdata_source)) {
    stop(
      "extdata source files not found in CerebroNexus package.",
      call. = FALSE
    )
  }
  app_template_source <- .cerebroPackageResource("viewer", "_bundle_app.R")
  if (!file.exists(app_template_source)) {
    stop(
      "The package-owned App entrypoint template is missing.",
      call. = FALSE
    )
  }

  # Preflight data inputs ----------------------------------------------------##
  private_data_root <- "private-data"
  preflight_data <- .preflightBundleData(cerebro_data)
  backends <- preflight_data$backends
  dataset_catalog <- preflight_data$dataset_catalog
  spatial_backends <- preflight_data$spatial_backends
  immune_backends <- preflight_data$immune_backends
  if (
    !is.list(immune_backends) || length(immune_backends) != length(cerebro_data)
  ) {
    immune_backends <- vector("list", length(cerebro_data))
  }
  spatial_catalogs <- preflight_data$spatial_catalogs
  spatial_point_appearance <- .normalizeAppSpatialPointSettings(
    spatial_point_appearance,
    spatial_catalogs,
    roi = FALSE
  )
  spatial_roi_settings <- .normalizeAppSpatialPointSettings(
    spatial_roi_settings,
    spatial_catalogs,
    roi = TRUE
  )
  spatial_percentage_cells_to_show <- .normalizeDatasetNumericOption(
    spatial_percentage_cells_to_show,
    names(cerebro_data),
    "spatial_percentage_cells_to_show",
    10,
    100
  )
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
  crb_targets <- paste0(private_data_root, "/", basename(cerebro_data))
  colliding_targets <- duplicated(tolower(crb_targets)) |
    duplicated(tolower(crb_targets), fromLast = TRUE)
  crb_targets[colliding_targets] <- vapply(
    data_labels[colliding_targets],
    .datasetBundleTarget,
    character(1)
  )
  if (anyDuplicated(tolower(crb_targets))) {
    stop(
      "Dataset identities generated colliding bundle filenames.",
      call. = FALSE
    )
  }
  for (index in seq_along(dataset_catalog)) {
    if (!is.null(dataset_catalog[[index]])) {
      dataset_catalog[[index]]$path <- crb_targets[[index]]
    }
  }
  names(dataset_catalog) <- crb_targets
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
    viewer_pack <- .viewerPackPath(resolved_crb_sources[[index]])
    if (isTRUE(preflight_data$viewer_pack_valid[[index]])) {
      claim_target(
        file.path(
          dirname(crb_targets[[index]]),
          paste0(
            tools::file_path_sans_ext(basename(crb_targets[[index]])),
            ".viewer"
          )
        ),
        viewer_pack,
        "Viewer Pack",
        directory = TRUE
      )
    }
  }

  for (index in seq_along(cerebro_data)) {
    backend <- spatial_backends[[index]]
    if (is.null(backend)) {
      next
    }
    source <- normalizePath(backend$root, winslash = "/", mustWork = TRUE)
    claim_target(
      paste0(private_data_root, "/", backend$location),
      source,
      "spatial molecule sidecar",
      directory = TRUE
    )
  }

  ## Sidecars are addressed by one file name next to the CRB. Bundle the real
  ## file, never a path that reaches it through a symbolic link.
  bundle_sibling <- function(file, location, artifact) {
    location <- .portableBundlePath(
      location,
      paste0("The ", artifact, " location in '", basename(file), "'")
    )
    parts <- strsplit(location, "/", fixed = TRUE)[[1L]]
    source_root <- dirname(file)
    source <- file.path(source_root, location)
    if (!file.exists(source) || dir.exists(source)) {
      stop(
        "Expected the ",
        artifact,
        " at '",
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
        artifact,
        " location '",
        location,
        "' in '",
        basename(file),
        "' resolves through a symbolic link. Copy the real file beside ",
        "the .crb before building the app.",
        call. = FALSE
      )
    }
    claim_target(
      paste0(private_data_root, "/", location),
      resolved_source,
      artifact
    )
    location
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
    effective <- .crbContract()$.effectiveRuntimeBackendPlan(backend)
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
    if (identical(effective$mode, "bundled")) {
      bundle_sibling(
        file,
        backend$location,
        paste0(backend$type, " backend")
      )
    }
    immune_backend <- immune_backends[[index]]
    if (!is.null(immune_backend)) {
      bundle_sibling(
        file,
        immune_backend$file,
        "immune repertoire sidecar"
      )
    }
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
  if (
    !.bundleCreateDirectory(
      stage_result_dir,
      mode = "0700",
      showWarnings = FALSE
    )
  ) {
    stop("Failed to create a private app staging directory.", call. = FALSE)
  }
  bundle_cleanup$stage <- stage_result_dir
  private_data_dir <- file.path(stage_result_dir, private_data_root)
  app_file <- file.path(stage_result_dir, "app.R")
  build_ops <- .bundleBuildOps()

  if (verbose) {
    cat("Creating staged directory structure...\n")
  }
  .bundleCreateDirectory(
    private_data_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
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
  hla_core_dir <- file.path(stage_result_dir, "viewer/hla_tcr_motifs/core")
  dir.create(hla_core_dir, showWarnings = FALSE)
  build_ops$write_lines(
    .viewerHLACoreSource(),
    file.path(hla_core_dir, "hla_core.R")
  )

  if (verbose) {
    cat("Copying data artifacts...\n")
  }
  for (entry in copy_plan) {
    target <- file.path(stage_result_dir, entry$target)
    .bundleCreateDirectory(
      dirname(target),
      recursive = TRUE,
      showWarnings = FALSE
    )
    copied <- if (isTRUE(entry$directory)) {
      if (identical(basename(entry$source), basename(target))) {
        build_ops$copy(entry$source, dirname(target), recursive = TRUE)
      } else {
        dir.create(target, showWarnings = FALSE)
        children <- list.files(
          entry$source,
          all.files = TRUE,
          no.. = TRUE,
          full.names = TRUE
        )
        all(build_ops$copy(children, target, recursive = TRUE))
      }
    } else {
      build_ops$copy(entry$source, target, overwrite = FALSE)
    }
    copied_target_exists <- .bundleCopiedTargetExists(
      target,
      directory = entry$directory
    )
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
  extdata_target <- file.path(stage_result_dir, "extdata")
  extdata_files <- list.files(extdata_source, full.names = TRUE)
  extdata_files <- extdata_files[file.info(extdata_files)$isdir %in% FALSE]
  if (
    !length(extdata_files) ||
      !isTRUE(.bundleCreateDirectory(extdata_target)) ||
      !isTRUE(all(build_ops$copy(extdata_files, extdata_target)))
  ) {
    stop("Failed to copy extdata files.", call. = FALSE)
  }
  .removeBundleSystemMetadata(stage_result_dir)

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
  cerebro_options[["cerebro_version"]] <- .cerebroRuntimeVersion()
  cerebro_options[["crb_file_to_load"]] <- crb_files
  cerebro_options[["cerebro_root"]] <- "."
  internal_option_names <- c(
    ".hla_core",
    ".bundle_backend_plan",
    ".bundle_run_options",
    ".dataset_catalog",
    ".viewer_auth",
    "dataset_labels",
    "initial_dataset",
    "initial_page",
    "extra_tables"
  )
  option_names <- names(cerebro_options)
  if (!is.null(option_names)) {
    remove_internal <- !is.na(option_names) &
      option_names %in% internal_option_names
    cerebro_options <- cerebro_options[!remove_internal]
  }
  cerebro_options[["initial_dataset"]] <- initial_dataset
  cerebro_options[["dataset_labels"]] <- dataset_labels
  cerebro_options[[".bundle_backend_plan"]] <- list(
    schema_version = 1L,
    entries = effective_backend_entries
  )
  cerebro_options[[".dataset_catalog"]] <- dataset_catalog
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
  if (!is.null(initial_dataset)) {
    cerebro_options[["initial_dataset"]] <- initial_dataset
  }
  if (!is.null(initial_page)) {
    cerebro_options[["initial_page"]] <- initial_page
  }
  for (field in c("spatial_point_appearance", "spatial_roi_settings")) {
    values <- get(field)
    for (dataset in names(values)) {
      cerebro_options$viewer_content[[dataset]][[field]] <- values[[dataset]]
    }
  }
  cerebro_options[[
    "spatial_percentage_cells_to_show"
  ]] <- spatial_percentage_cells_to_show
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
  if (!isTRUE(build_ops$copy(app_template_source, app_file))) {
    stop("Failed to copy the App entrypoint template.", call. = FALSE)
  }
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


.bundlePreflightCache <- new.env(parent = emptyenv())

.bundlePreflightFingerprint <- function(path) {
  info <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  canonical <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    is.null(info) ||
      nrow(info) != 1L ||
      !identical(as.character(info$type), "file") ||
      is.null(canonical)
  ) {
    return(NULL)
  }
  fingerprint <- list(
    path = canonical,
    size = as.double(info$size),
    permissions = as.character(info$permissions),
    device_id = as.double(info$device_id),
    inode = as.double(info$inode),
    hard_links = as.double(info$hard_links),
    modification_time = as.double(info$modification_time),
    change_time = as.double(info$change_time)
  )
  scalar <- vapply(fingerprint, length, integer(1)) == 1L
  missing <- vapply(fingerprint, function(value) anyNA(value), logical(1))
  numeric_fields <- setdiff(names(fingerprint), c("path", "permissions"))
  finite <- vapply(
    fingerprint[numeric_fields],
    function(value) is.finite(value),
    logical(1)
  )
  if (
    !all(scalar) ||
      any(missing) ||
      !all(finite) ||
      !nzchar(fingerprint$path) ||
      !nzchar(fingerprint$permissions) ||
      fingerprint$size < 0 ||
      fingerprint$device_id < 0 ||
      fingerprint$inode < 0 ||
      fingerprint$hard_links < 1
  ) {
    return(NULL)
  }
  fingerprint
}

.bundlePreflightCacheKey <- function(path, dataset) {
  paste(dataset, path, sep = "\r")
}

.clearBundlePreflightCache <- function() {
  keys <- ls(.bundlePreflightCache, all.names = TRUE)
  if (length(keys)) {
    rm(list = keys, envir = .bundlePreflightCache)
  }
  invisible(TRUE)
}

.cacheBundlePreflightData <- function(
  cerebro_data,
  preflight_data,
  fingerprints
) {
  labels <- names(cerebro_data)
  legacy_preflight <- is.list(preflight_data) &&
    identical(names(preflight_data), c("backends", "spatial_catalogs"))
  if (legacy_preflight) {
    preflight_data$spatial_backends <- stats::setNames(
      vector("list", length(cerebro_data)),
      labels
    )
    preflight_data <- preflight_data[c(
      "backends",
      "spatial_backends",
      "spatial_catalogs"
    )]
  }
  if (is.list(preflight_data) && is.null(preflight_data$dataset_catalog)) {
    preflight_data$dataset_catalog <- stats::setNames(
      vector("list", length(cerebro_data)),
      labels
    )
  }
  ## Immune sidecar descriptors are optional: callers that only verified the
  ## core preflight leave them unknown, and `.preflightBundleData()` then
  ## re-reads datasets that may carry an immune repertoire.
  immune_known <- is.list(preflight_data) &&
    !is.null(preflight_data$immune_backends)
  core_names <- setdiff(
    names(preflight_data),
    c("immune_backends", "viewer_pack_valid")
  )
  valid <- is.character(cerebro_data) &&
    length(cerebro_data) > 0L &&
    !is.null(labels) &&
    !anyNA(labels) &&
    all(nzchar(labels)) &&
    is.list(preflight_data) &&
    setequal(
      core_names,
      c("backends", "spatial_backends", "spatial_catalogs", "dataset_catalog")
    ) &&
    length(core_names) == 4L &&
    (!immune_known ||
      identical(names(preflight_data$immune_backends), labels)) &&
    identical(names(preflight_data$backends), labels) &&
    identical(names(preflight_data$spatial_backends), labels) &&
    identical(names(preflight_data$spatial_catalogs), labels) &&
    identical(names(preflight_data$dataset_catalog), labels) &&
    is.list(fingerprints) &&
    identical(names(fingerprints), labels)
  if (!valid) {
    return(FALSE)
  }

  entries <- vector("list", length(cerebro_data))
  for (index in seq_along(cerebro_data)) {
    observed <- .bundlePreflightFingerprint(cerebro_data[[index]])
    if (is.null(observed) || !identical(observed, fingerprints[[index]])) {
      return(FALSE)
    }
    entries[[index]] <- list(
      key = .bundlePreflightCacheKey(observed$path, labels[[index]]),
      fingerprint = observed,
      backend = preflight_data$backends[[index]],
      spatial_backend = preflight_data$spatial_backends[[index]],
      spatial_catalog = preflight_data$spatial_catalogs[[index]],
      dataset_info = preflight_data$dataset_catalog[[index]],
      immune_known = immune_known,
      immune_backend = if (immune_known) {
        preflight_data$immune_backends[[index]]
      } else {
        NULL
      }
    )
  }
  for (entry in entries) {
    assign(entry$key, entry, envir = .bundlePreflightCache)
  }
  TRUE
}

.takeBundlePreflightData <- function(path, dataset) {
  observed <- .bundlePreflightFingerprint(path)
  if (is.null(observed)) {
    return(NULL)
  }
  key <- .bundlePreflightCacheKey(observed$path, dataset)
  entry <- get0(key, envir = .bundlePreflightCache, inherits = FALSE)
  if (is.null(entry)) {
    return(NULL)
  }
  rm(list = key, envir = .bundlePreflightCache)
  if (!identical(entry$fingerprint, observed)) {
    return(NULL)
  }
  list(
    backend = entry$backend,
    spatial_backend = entry$spatial_backend,
    spatial_catalog = entry$spatial_catalog,
    dataset_info = entry$dataset_info,
    immune_known = isTRUE(entry$immune_known),
    immune_backend = entry$immune_backend
  )
}

# OS-backed entropy for generated application credentials. Never fall back to
# R's seeded statistical PRNG: these bytes can become authentication secrets.
.builderRandomBytes <- function(n) {
  if (
    length(n) != 1L ||
      !is.numeric(n) ||
      is.na(n) ||
      n < 1 ||
      n > 65536 ||
      n != as.integer(n)
  ) {
    stop("The requested random byte count is invalid.", call. = FALSE)
  }
  n <- as.integer(n)
  if (.Platform$OS.type == "windows") {
    command <- sprintf(
      paste0(
        "$ErrorActionPreference='Stop'; $bytes=New-Object byte[] %d; ",
        "$rng=[System.Security.Cryptography.RandomNumberGenerator]::Create(); ",
        "try {$rng.GetBytes($bytes); [BitConverter]::ToString($bytes).Replace('-', '')} ",
        "finally {$rng.Dispose()}"
      ),
      n
    )
    output <- system2(
      "powershell.exe",
      c(
        "-NoLogo",
        "-NoProfile",
        "-NonInteractive",
        "-Command",
        shQuote(command)
      ),
      stdout = TRUE,
      stderr = FALSE
    )
    if (!is.null(attr(output, "status")) || length(output) != 1L) {
      stop("Operating-system secure randomness is unavailable.", call. = FALSE)
    }
    if (
      nchar(output, type = "bytes") != 2L * n ||
        !grepl("^[0-9A-Fa-f]+$", output)
    ) {
      stop("Operating-system secure randomness is invalid.", call. = FALSE)
    }
    starts <- seq.int(1L, 2L * n, by = 2L)
    bytes <- as.raw(strtoi(substring(output, starts, starts + 1L), base = 16L))
  } else {
    connection <- file("/dev/urandom", open = "rb", raw = TRUE)
    on.exit(close(connection), add = TRUE)
    bytes <- readBin(connection, what = "raw", n = n)
  }
  if (!is.raw(bytes) || length(bytes) != n) {
    stop("Operating-system secure randomness is incomplete.", call. = FALSE)
  }
  bytes
}
