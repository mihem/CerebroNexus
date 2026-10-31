##----------------------------------------------------------------------------##
## Source adapters and immutable Seurat snapshots.
##
## File and example inputs differ only while loading. Once loaded, both pass
## through the same inspection and snapshot path. A snapshot owns every disk
## backing used by the object, so later builds never depend on mutable input.
##----------------------------------------------------------------------------##

.builder_adapter_abort <- function(message) {
  stop(message, call. = FALSE)
}

.builder_adapter_scalar_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_path_is_link <- function(path) {
  link <- tryCatch(Sys.readlink(path), error = function(error) "")
  length(link) == 1L && !is.na(link) && nzchar(link)
}

.builder_path_chain_has_link <- function(path, include_leaf = TRUE) {
  expanded <- path.expand(path)
  expanded <- gsub("\\\\", "/", expanded)
  if (.Platform$OS.type == "windows") {
    if (grepl("^[A-Za-z]:/", expanded)) {
      current <- substr(expanded, 1L, 3L)
      remainder <- substring(expanded, 4L)
    } else if (startsWith(expanded, "//")) {
      unc <- strsplit(sub("^//", "", expanded), "/", fixed = TRUE)[[1L]]
      unc <- unc[nzchar(unc)]
      if (length(unc) < 2L) {
        return(TRUE)
      }
      current <- paste0("//", unc[[1L]], "/", unc[[2L]])
      remainder <- paste(unc[-c(1L, 2L)], collapse = "/")
    } else {
      expanded <- gsub("\\\\", "/", file.path(getwd(), expanded))
      current <- substr(expanded, 1L, 3L)
      remainder <- substring(expanded, 4L)
    }
    components <- strsplit(remainder, "/", fixed = TRUE)[[1L]]
    components <- components[nzchar(components)]
  } else {
    if (!startsWith(expanded, "/")) {
      expanded <- file.path(getwd(), expanded)
    }
    components <- strsplit(expanded, "/", fixed = TRUE)[[1L]]
    components <- components[nzchar(components)]
    current <- "/"
  }
  if (!include_leaf && length(components)) {
    components <- head(components, -1L)
  }
  for (component in components) {
    current <- if (identical(current, "/")) {
      paste0("/", component)
    } else if (endsWith(current, "/")) {
      paste0(current, component)
    } else {
      file.path(current, component)
    }
    if (
      (file.exists(current) || dir.exists(current)) &&
        .builder_path_is_link(current)
    ) {
      target <- Sys.readlink(current)
      if (
        identical(Sys.info()[["sysname"]], "Darwin") &&
          identical(
            unname(c(
              "/etc" = "private/etc",
              "/tmp" = "private/tmp",
              "/var" = "private/var"
            )[current]),
            target
          )
      ) {
        next
      }
      return(TRUE)
    }
  }
  FALSE
}

.builder_path_tree_has_link <- function(path) {
  if (.builder_path_chain_has_link(path)) {
    return(TRUE)
  }
  if (!dir.exists(path)) {
    return(FALSE)
  }
  walk <- function(directory) {
    members <- list.files(
      directory,
      all.files = TRUE,
      no.. = TRUE,
      recursive = FALSE,
      full.names = TRUE
    )
    for (member in members) {
      if (.builder_path_is_link(member)) {
        return(TRUE)
      }
      if (dir.exists(member) && walk(member)) {
        return(TRUE)
      }
    }
    FALSE
  }
  walk(path)
}

.builder_canonical_path <- function(path, must_work = TRUE) {
  normalizePath(path, winslash = "/", mustWork = must_work)
}

.builder_path_within <- function(path, root) {
  path <- .builder_canonical_path(path, must_work = TRUE)
  root <- .builder_canonical_path(root, must_work = TRUE)
  identical(path, root) || startsWith(path, paste0(root, "/"))
}

.builder_saved_cache <- function(object) {
  if (!inherits(object, "Seurat") || !"tools" %in% methods::slotNames(object)) {
    return(NULL)
  }
  keys <- grep(
    "(^|::)SaveSeuratRds$",
    names(object@tools),
    value = TRUE
  )
  if (!length(keys)) {
    return(NULL)
  }
  object@tools[[keys[[1L]]]]
}

.builder_clear_saved_cache <- function(object) {
  if (inherits(object, "Seurat") && "tools" %in% methods::slotNames(object)) {
    keys <- grep(
      "(^|::)SaveSeuratRds$",
      names(object@tools),
      value = TRUE
    )
    object@tools[keys] <- NULL
  }
  object
}

.builder_file_source_state <- function(path) {
  if (
    !.builder_adapter_scalar_text(path) ||
      !file.exists(path) ||
      dir.exists(path) ||
      .builder_path_chain_has_link(path)
  ) {
    .builder_adapter_abort("The Seurat source changed since it was selected.")
  }
  info <- file.info(path)
  stat <- tryCatch(fs::file_info(path), error = function(error) NULL)
  stat_value <- function(name) {
    value <- if (is.data.frame(stat) && name %in% names(stat)) {
      suppressWarnings(as.double(stat[[name]][[1L]]))
    } else {
      NA_real_
    }
    if (length(value) == 1L && is.finite(value)) {
      sprintf("%.17g", value)
    } else {
      NULL
    }
  }
  if (
    nrow(info) != 1L ||
      is.na(info$size) ||
      !is.finite(info$size) ||
      is.na(info$mtime) ||
      is.na(info$ctime)
  ) {
    .builder_adapter_abort("The Seurat source file metadata is unavailable.")
  }
  stat_changed_at <- stat_value("change_time")
  changed_at <- stat_changed_at
  if (is.null(changed_at)) {
    changed_at <- sprintf("%.17g", as.double(info$ctime[[1L]]))
  }
  device_id <- stat_value("device_id")
  inode <- stat_value("inode")
  trusted_stat <- !is.null(stat_changed_at) &&
    !is.null(device_id) &&
    !is.null(inode)
  list(
    location = .builder_canonical_path(path),
    bytes = as.double(info$size[[1L]]),
    modified_at = format(
      info$mtime[[1L]],
      "%Y-%m-%dT%H:%M:%OS3Z",
      tz = "UTC"
    ),
    changed_at = changed_at,
    stat_version = if (trusted_stat) 1L else NULL,
    file_id = if (trusted_stat) {
      paste(device_id, inode, sep = ":")
    } else {
      NULL
    }
  )
}

.builder_adapter_after_read <- function(adapter) invisible(adapter)

.builder_cached_layers_materialized <- function(object, cache) {
  if (is.null(cache)) {
    return(TRUE)
  }
  if (
    !is.data.frame(cache) ||
      !all(c("assay", "layer") %in% names(cache)) ||
      !is.character(cache$assay) ||
      !is.character(cache$layer) ||
      anyNA(cache$assay) ||
      anyNA(cache$layer) ||
      any(!nzchar(cache$assay)) ||
      any(!nzchar(cache$layer))
  ) {
    return(FALSE)
  }
  all(vapply(
    seq_len(nrow(cache)),
    function(index) {
      assay <- cache$assay[[index]]
      layer <- cache$layer[[index]]
      if (!assay %in% names(object@assays)) {
        return(FALSE)
      }
      layer %in%
        tryCatch(
          SeuratObject::Layers(object[[assay]]),
          error = function(error) character()
        )
    },
    logical(1)
  ))
}

.builder_seurat_file_adapter <- function(
  path,
  expected_md5 = NULL,
  snapshot_reusable = FALSE
) {
  if (!.builder_adapter_scalar_text(path)) {
    .builder_adapter_abort("A Seurat file path must be one non-empty string.")
  }
  if (.builder_path_chain_has_link(path)) {
    .builder_adapter_abort("A Seurat source cannot be a symbolic link.")
  }
  if (!file.exists(path)) {
    .builder_adapter_abort("The Seurat source file does not exist.")
  }
  if (dir.exists(path)) {
    .builder_adapter_abort("The Seurat source must be a regular file.")
  }
  format <- builder_match_format(path)
  if (is.null(format)) {
    .builder_adapter_abort("The Seurat source has an unsupported file format.")
  }
  if (
    !is.null(expected_md5) &&
      (!.builder_adapter_scalar_text(expected_md5) ||
        !grepl("^[0-9a-fA-F]{32}$", expected_md5))
  ) {
    .builder_adapter_abort("The expected Seurat source MD5 is invalid.")
  }
  source_state <- .builder_file_source_state(path)
  expected_md5 <- if (is.null(expected_md5)) NULL else tolower(expected_md5)
  structure(
    list(
      type = "file",
      location = source_state$location,
      format = format$label,
      fingerprint = paste(
        "builder-snapshot-v2",
        basename(source_state$location),
        source_state$bytes,
        source_state$modified_at,
        source_state$changed_at,
        source_state$file_id %||% "no-file-id",
        expected_md5 %||% "stat",
        sep = ":"
      ),
      expected_md5 = expected_md5,
      source_state = source_state,
      snapshot_reusable = isTRUE(snapshot_reusable),
      reader = format
    ),
    class = c("builder_source_adapter", "list")
  )
}

#' Describe a Seurat file source without loading it.
builder_seurat_file_adapter <- function(path) {
  .builder_seurat_file_adapter(path, snapshot_reusable = TRUE)
}

#' Describe an in-memory example source.
builder_example_adapter <- function(id, object) {
  if (!.builder_adapter_scalar_text(id)) {
    .builder_adapter_abort("An example adapter requires a non-empty id.")
  }
  if (!inherits(object, "Seurat")) {
    .builder_adapter_abort("An example adapter requires a Seurat object.")
  }
  structure(
    list(
      type = "example",
      location = id,
      format = "Built-in example",
      fingerprint = paste0(
        "example:",
        id,
        ":",
        tryCatch(
          as.character(methods::slot(object, "version")),
          error = function(error) "unknown"
        )
      ),
      object = object
    ),
    class = c("builder_source_adapter", "list")
  )
}

## Records the package file an example object was read from: `source_md5` is
## its content hash, `source_file` its name.
.builder_example_source_adapter <- function(adapter, source_file, source_md5) {
  if (is.null(source_md5)) {
    return(adapter)
  }
  if (
    !identical(adapter$type, "example") ||
      !.builder_adapter_scalar_text(source_md5) ||
      !grepl("^[0-9a-fA-F]{32}$", source_md5) ||
      !.builder_adapter_scalar_text(source_file)
  ) {
    .builder_adapter_abort("The example source file identity is invalid.")
  }
  adapter$source_file <- basename(source_file)
  adapter$source_md5 <- tolower(source_md5)
  adapter
}

.builder_adapter_load <- function(adapter) {
  serialization <- NULL
  if (identical(adapter$type, "example")) {
    object <- adapter$object
  } else {
    if (!.builder_adapter_scalar_text(adapter$location)) {
      .builder_adapter_abort("The Seurat source format adapter changed.")
    }
    format <- builder_match_format(adapter$location)
    if (
      is.null(format) ||
        !is.list(adapter$reader) ||
        !identical(adapter$reader$id, format$id)
    ) {
      .builder_adapter_abort("The Seurat source format adapter changed.")
    }
    serialization <- format$id
    before <- .builder_file_source_state(adapter$location)
    if (!identical(before, adapter$source_state)) {
      .builder_adapter_abort("The Seurat source changed since it was selected.")
    }
    read <- builder_read_object(adapter$location)
    .builder_adapter_after_read(adapter)
    after <- .builder_file_source_state(adapter$location)
    if (!identical(before, after)) {
      .builder_adapter_abort(
        "The Seurat source changed while it was being read."
      )
    }
    if (!is.null(read$error)) {
      .builder_adapter_abort(read$error)
    }
    object <- read$object
  }
  cache <- .builder_saved_cache(object)
  ## Only a Seurat serialization can be copied as the snapshot; an h5ad, h5mu or
  ## SpatialData source is converted, so the file itself still needs its md5.
  snapshot_reusable <- identical(adapter$type, "file") &&
    isTRUE(adapter$snapshot_reusable) &&
    serialization %in% c("rds", "qs2", "qs") &&
    is.null(cache) &&
    !length(.builder_snapshot_layer_contracts(object))
  source_md5 <- NULL
  if (identical(adapter$type, "file") && !isTRUE(snapshot_reusable)) {
    source_md5 <- unname(as.character(tools::md5sum(adapter$location)))
    digest_after <- .builder_file_source_state(adapter$location)
    if (
      length(source_md5) != 1L ||
        is.na(source_md5) ||
        !grepl("^[[:xdigit:]]{32}$", source_md5) ||
        !identical(after, digest_after) ||
        (!is.null(adapter$expected_md5) &&
          !identical(source_md5, adapter$expected_md5))
    ) {
      .builder_adapter_abort(
        "The Seurat source changed while it was being read."
      )
    }
  }
  if (!is.null(cache) && !.builder_cached_layers_materialized(object, cache)) {
    .builder_adapter_abort(paste0(
      "This file is an incomplete SaveSeuratRds cache stub. Open it with ",
      "LoadSeuratRds in a trusted R session, verify every layer loaded, then ",
      "save the materialized Seurat object before adding it to Builder."
    ))
  }
  list(
    object = .builder_clear_saved_cache(object),
    snapshot_source = if (isTRUE(snapshot_reusable)) {
      list(
        location = adapter$location,
        source_state = adapter$source_state,
        expected_md5 = adapter$expected_md5,
        serialization = serialization
      )
    } else {
      NULL
    },
    source_md5 = source_md5
  )
}

#' The pictures a source object brought, as plain bytes, for the main process.
#'
#' convertSCEToSeurat() keeps them per section in `@misc$cerebro_source_images`
#' as data URIs. The import worker decodes them here so the main process can
#' write files and describe them; nothing else of the object crosses over.
#'
#' @return `list(section = list(label = list(bytes = <raw>, bounds = <list>)))`,
#'   empty when the object brought none.
builder_source_image_payload <- function(object) {
  if (!methods::is(object, "Seurat")) {
    return(list())
  }
  sections <- tryCatch(object@misc$cerebro_source_images, error = function(e) {
    NULL
  })
  if (!is.list(sections) || !length(sections) || is.null(names(sections))) {
    return(list())
  }
  result <- list()
  for (section in names(sections)) {
    for (label in names(sections[[section]])) {
      entry <- sections[[section]][[label]]
      uri <- entry$histology_image
      bounds <- as.list(entry$histology_image_bounds)
      if (
        !is.character(uri) ||
          length(uri) != 1L ||
          !startsWith(uri, "data:image/png;base64,") ||
          !all(c("xmin", "xmax", "ymin", "ymax") %in% names(bounds))
      ) {
        next
      }
      bytes <- tryCatch(
        base64enc::base64decode(sub("^data:image/png;base64,", "", uri)),
        error = function(e) NULL
      )
      if (is.null(bytes) || !length(bytes)) {
        next
      }
      result[[section]][[label]] <- list(
        bytes = bytes,
        bounds = bounds[c("xmin", "xmax", "ymin", "ymax")]
      )
    }
  }
  result
}

.builder_adapter_inspect <- function(adapter, progress = NULL) {
  if (!inherits(adapter, "builder_source_adapter")) {
    .builder_adapter_abort("Expected a Builder source adapter.")
  }
  if (is.function(progress)) {
    progress("reading")
  }
  loaded <- .builder_adapter_load(adapter)
  object <- loaded$object
  source <- list(
    type = adapter$type,
    location = adapter$location,
    fingerprint = adapter$fingerprint,
    format = adapter$format
  )
  if (is.function(progress)) {
    progress("inspecting")
  }
  profile <- builder_dataset_profile(object, source)
  if (is.function(progress)) {
    progress("validating")
  }
  legacy <- describe_seurat(object, metadata_groups = profile$groups)
  matrix_summary <- .builder_expression_matrix_summary(object, legacy)
  levels <- profile$groups$legacy$levels[unname(legacy$group_candidates)]
  profile <- builder_profile_workspace_contract(profile)
  list(
    object = object,
    profile = profile,
    legacy_profile = legacy,
    matrix_summary = matrix_summary,
    levels = levels,
    format = adapter$format,
    source = source,
    snapshot_source = loaded$snapshot_source,
    source_md5 = loaded$source_md5,
    source_images = builder_source_image_payload(object)
  )
}

#' Load and inspect either a file or example adapter through one path.
builder_adapter_inspect <- function(adapter) {
  .builder_adapter_inspect(adapter)
}

.builder_snapshot_now <- function() Sys.time()

.builder_snapshot_after_copy <- function() invisible(NULL)

.builder_snapshot_after_publish <- function(snapshot) invisible(snapshot)

.builder_snapshot_token <- function() {
  paste0(
    Sys.getpid(),
    "-",
    format(.builder_snapshot_now(), "%Y%m%d%H%M%OS6", tz = "UTC"),
    "-",
    basename(tempfile("owner-"))
  )
}

## -- Snapshot cache closure and safe loader parsing -------------------------

.builder_snapshot_cache_members <- function(path) {
  if (!.builder_adapter_scalar_text(path)) {
    .builder_adapter_abort("A layer cache contains a malformed path.")
  }
  if (
    grepl(",", path, fixed = TRUE) && (file.exists(path) || dir.exists(path))
  ) {
    .builder_adapter_abort(paste0(
      "An on-disk layer path contains a comma and is ambiguous. ",
      "Move that backing store to a path without commas and retry."
    ))
  }
  members <- strsplit(path, ",", fixed = TRUE)[[1L]]
  if (!length(members) || any(!nzchar(members))) {
    .builder_adapter_abort("A layer cache contains a malformed comma path.")
  }
  if (any(!file.exists(members) & !dir.exists(members))) {
    .builder_adapter_abort(paste0(
      "An on-disk layer dependency is missing. Restore every backing path ",
      "and retry."
    ))
  }
  members
}

.builder_snapshot_validate_cache <- function(cache) {
  required <- c("layer", "path", "class", "pkg", "fxn", "assay")
  if (is.null(cache)) {
    return(list(cache = NULL, members = list()))
  }
  if (!is.data.frame(cache) || !all(required %in% names(cache))) {
    .builder_adapter_abort("SaveSeuratRds produced a malformed layer cache.")
  }
  for (column in required) {
    if (!is.character(cache[[column]]) || anyNA(cache[[column]])) {
      .builder_adapter_abort("SaveSeuratRds produced a malformed layer cache.")
    }
  }
  if (any(cache$pkg != "HDF5Array")) {
    .builder_adapter_abort(
      "The snapshot layer loader is not allowed by Builder."
    )
  }
  missing_packages <- unique(cache$pkg[
    !vapply(
      cache$pkg,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ])
  if (length(missing_packages)) {
    .builder_adapter_abort(paste0(
      "Snapshot loading requires missing package(s): ",
      paste(missing_packages, collapse = ", "),
      ". Install them and retry."
    ))
  }
  members <- lapply(cache$path, .builder_snapshot_cache_members)
  for (path in unique(unlist(members, use.names = FALSE))) {
    if (.builder_path_tree_has_link(path)) {
      .builder_adapter_abort(paste0(
        "An on-disk layer backing tree contains a symbolic link: ",
        path,
        ". Replace links with real files before retrying."
      ))
    }
  }
  list(cache = cache, members = members)
}

.builder_loader_function_body <- function(loader) {
  parsed <- tryCatch(parse(text = loader), error = function(error) NULL)
  if (is.null(parsed) || length(parsed) != 1L) {
    return(NULL)
  }
  function_call <- parsed[[1L]]
  if (
    !is.call(function_call) ||
      !identical(function_call[[1L]], as.name("function")) ||
      !length(function_call) %in% c(3L, 4L) ||
      (length(function_call) == 4L && !is.null(function_call[[4L]]))
  ) {
    return(NULL)
  }
  arguments <- function_call[[2L]]
  if (
    !is.pairlist(arguments) ||
      !identical(arguments, formals(function(x) NULL))
  ) {
    return(NULL)
  }
  function_call[[3L]]
}

.builder_loader_namespace_call <- function(body, package, function_name) {
  if (!is.call(body)) {
    return(NULL)
  }
  call <- as.list(body)
  namespace_call <- call[[1L]]
  if (!is.call(namespace_call)) {
    return(NULL)
  }
  namespace <- as.list(namespace_call)
  if (
    length(namespace) != 3L ||
      !identical(namespace[[1L]], as.name("::")) ||
      !identical(namespace[[2L]], as.name(package)) ||
      !identical(namespace[[3L]], as.name(function_name))
  ) {
    return(NULL)
  }
  call
}

.builder_hdf5_loader_contract <- function(loader) {
  body <- .builder_loader_function_body(loader)
  call <- .builder_loader_namespace_call(body, "HDF5Array", "HDF5Array")
  if (
    is.null(call) ||
      !identical(names(call), c("", "filepath", "name", "as.sparse")) ||
      !identical(call[[2L]], as.name("x")) ||
      !is.character(call[[3L]]) ||
      length(call[[3L]]) != 1L ||
      is.na(call[[3L]]) ||
      !nzchar(call[[3L]]) ||
      !is.logical(call[[4L]]) ||
      length(call[[4L]]) != 1L ||
      is.na(call[[4L]])
  ) {
    return(NULL)
  }
  list(
    name = call[[3L]],
    sparse = call[[4L]]
  )
}

.builder_snapshot_load_layer <- function(cache, members, index) {
  package <- cache$pkg[[index]]
  loader <- cache$fxn[[index]]
  paths <- members[[index]]
  if (identical(package, "HDF5Array")) {
    contract <- .builder_hdf5_loader_contract(loader)
    if (
      is.null(contract) ||
        length(paths) != 1L ||
        !file.exists(paths[[1L]]) ||
        !cache$class[[index]] %in% c("HDF5Matrix", "DelayedMatrix")
    ) {
      .builder_adapter_abort(
        "The snapshot HDF5Array loader is not allowed by Builder."
      )
    }
    return(HDF5Array::HDF5Array(
      filepath = paths[[1L]],
      name = contract$name,
      as.sparse = contract$sparse
    ))
  }
  .builder_adapter_abort(
    "The snapshot layer loader is not allowed by Builder."
  )
}

.builder_snapshot_reopen_stub <- function(stub, discovered) {
  if (!inherits(stub, "Seurat")) {
    .builder_adapter_abort(
      "The Builder snapshot did not contain a Seurat object."
    )
  }
  cache <- discovered$cache
  if (is.null(cache)) {
    return(.builder_clear_saved_cache(stub))
  }
  object <- stub
  for (index in seq_len(nrow(cache))) {
    assay <- cache$assay[[index]]
    if (!assay %in% names(object@assays)) {
      .builder_adapter_abort("A snapshot cache names an assay that is absent.")
    }
    layer <- .builder_snapshot_load_layer(cache, discovered$members, index)
    tryCatch(
      suppressWarnings(
        SeuratObject::LayerData(
          object,
          assay = assay,
          layer = cache$layer[[index]]
        ) <- layer
      ),
      error = function(error) {
        .builder_adapter_abort(paste0(
          "The snapshot layer could not be restored: ",
          conditionMessage(error)
        ))
      }
    )
  }
  .builder_clear_saved_cache(object)
}

## -- Snapshot filesystem and disk budget -----------------------------------

.builder_snapshot_size <- function(path) {
  paths <- if (dir.exists(path)) {
    c(
      path,
      list.files(
        path,
        all.files = TRUE,
        no.. = TRUE,
        recursive = TRUE,
        full.names = TRUE,
        include.dirs = TRUE
      )
    )
  } else {
    path
  }
  info <- file.info(paths)
  sum(info$size[!info$isdir & !is.na(info$size)])
}

.builder_snapshot_tree_state <- function(path) {
  if (.builder_path_tree_has_link(path)) {
    .builder_adapter_abort("A snapshot source contains a symbolic link.")
  }
  root <- .builder_canonical_path(path)
  records <- list()
  visit <- function(current, relative) {
    info <- file.info(current)
    if (
      nrow(info) != 1L ||
        is.na(info$isdir) ||
        is.na(info$size) ||
        is.na(info$mtime)
    ) {
      .builder_adapter_abort(
        "A snapshot source changed while it was inspected."
      )
    }
    records[[length(records) + 1L]] <<- data.frame(
      path = relative,
      directory = isTRUE(info$isdir),
      size = unname(info$size),
      mtime = as.numeric(info$mtime),
      stringsAsFactors = FALSE
    )
    if (isTRUE(info$isdir)) {
      children <- sort(list.files(
        current,
        all.files = TRUE,
        no.. = TRUE,
        recursive = FALSE,
        full.names = TRUE
      ))
      for (child in children) {
        if (.builder_path_is_link(child)) {
          .builder_adapter_abort("A snapshot source contains a symbolic link.")
        }
        child_relative <- if (nzchar(relative)) {
          file.path(relative, basename(child))
        } else {
          basename(child)
        }
        visit(child, child_relative)
      }
    }
  }
  visit(root, "")
  do.call(rbind, records)
}

.builder_snapshot_space_query <- function(
  path,
  os_type = .Platform$OS.type
) {
  if (identical(os_type, "windows")) {
    if (grepl("^(//|\\\\\\\\)", path)) {
      .builder_adapter_abort(paste0(
        "Free-space checks for Windows UNC snapshot paths are not supported. ",
        "Choose a local drive path."
      ))
    }
    canonical <- if (grepl("^[A-Za-z]:[\\\\/]", path)) {
      path
    } else {
      normalizePath(path, winslash = "\\", mustWork = TRUE)
    }
    drive <- sub(":.*$", "", canonical)
    if (!grepl("^[A-Za-z]$", drive)) {
      .builder_adapter_abort("Builder could not identify the snapshot drive.")
    }
    expression <- paste0("(Get-PSDrive -Name '", drive, "').Free")
    powershell <- if (nzchar(Sys.which("powershell"))) {
      "powershell"
    } else if (nzchar(Sys.which("pwsh"))) {
      "pwsh"
    } else {
      "powershell"
    }
    return(list(
      command = powershell,
      args = c(
        "-NoProfile",
        "-NonInteractive",
        "-Command",
        shQuote(expression)
      ),
      multiplier = 1
    ))
  }
  list(
    command = "df",
    args = c("-Pk", shQuote(path)),
    multiplier = 1024
  )
}

.builder_snapshot_available_bytes <- function(
  path,
  os_type = .Platform$OS.type,
  runner = system2
) {
  query <- .builder_snapshot_space_query(path, os_type = os_type)
  output <- tryCatch(
    runner(
      query$command,
      query$args,
      stdout = TRUE,
      stderr = TRUE
    ),
    error = function(error) character()
  )
  status <- attr(output, "status")
  if (!length(output) || (!is.null(status) && status != 0L)) {
    .builder_adapter_abort(paste0(
      "Builder could not determine free disk space for the snapshot. ",
      "Check the destination and retry."
    ))
  }
  if (identical(os_type, "windows")) {
    value <- suppressWarnings(as.numeric(trimws(output[[length(output)]])))
  } else {
    fields <- strsplit(trimws(output[[length(output)]]), "[[:space:]]+")[[1L]]
    value <- if (length(fields) >= 4L) {
      suppressWarnings(as.numeric(fields[[4L]]))
    } else {
      NA_real_
    }
  }
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
    .builder_adapter_abort(paste0(
      "Builder could not interpret free disk space for the snapshot. ",
      "Check the destination with df and retry."
    ))
  }
  value * query$multiplier
}

.builder_snapshot_check_budget <- function(closure_bytes, available_bytes) {
  if (
    !is.numeric(available_bytes) ||
      length(available_bytes) != 1L ||
      is.na(available_bytes) ||
      !is.finite(available_bytes) ||
      available_bytes < 0
  ) {
    .builder_adapter_abort(
      "Available free space must be one finite byte count."
    )
  }
  .builder_snapshot_check_headroom(available_bytes)
  if (
    closure_bytes + 1024^3 > available_bytes ||
      closure_bytes > available_bytes * 0.8
  ) {
    .builder_adapter_abort(paste0(
      "There is not enough free space for an immutable snapshot plus 1 GiB ",
      "headroom. Free disk space or choose another temporary volume."
    ))
  }
  invisible(TRUE)
}

.builder_snapshot_check_headroom <- function(available_bytes) {
  if (
    !is.numeric(available_bytes) ||
      length(available_bytes) != 1L ||
      is.na(available_bytes) ||
      !is.finite(available_bytes) ||
      available_bytes < 0
  ) {
    .builder_adapter_abort(
      "Available free space must be one finite byte count."
    )
  }
  headroom <- 1024^3
  if (available_bytes < headroom) {
    .builder_adapter_abort(paste0(
      "There is not enough free space to preserve the required 1 GiB ",
      "headroom. Free disk space or choose another temporary volume."
    ))
  }
  invisible(TRUE)
}

.builder_snapshot_copy <- function(source, destination) {
  cloned <- FALSE
  if (
    identical(unname(Sys.info()[["sysname"]]), "Darwin") &&
      file.exists("/bin/cp")
  ) {
    if (dir.exists(source)) {
      dir.create(destination, recursive = TRUE, showWarnings = FALSE)
      status <- suppressWarnings(system2(
        "/bin/cp",
        c("-cR", "--", shQuote(file.path(source, ".")), shQuote(destination)),
        stdout = FALSE,
        stderr = FALSE
      ))
    } else {
      status <- suppressWarnings(system2(
        "/bin/cp",
        c("-c", "--", shQuote(source), shQuote(destination)),
        stdout = FALSE,
        stderr = FALSE
      ))
    }
    cloned <- isTRUE(status == 0L) &&
      if (dir.exists(source)) {
        dir.exists(destination)
      } else {
        file.exists(destination)
      }
    if (!cloned) {
      unlink(destination, recursive = TRUE, force = TRUE)
    }
  }
  if (cloned) {
    return(invisible(destination))
  }
  if (dir.exists(source)) {
    if (!dir.create(destination, recursive = TRUE, showWarnings = FALSE)) {
      .builder_adapter_abort("Could not create a snapshot backing directory.")
    }
    members <- list.files(
      source,
      all.files = TRUE,
      no.. = TRUE,
      full.names = TRUE
    )
    if (
      length(members) &&
        !all(file.copy(
          members,
          destination,
          recursive = TRUE,
          copy.mode = TRUE,
          copy.date = TRUE
        ))
    ) {
      .builder_adapter_abort("Could not copy an on-disk layer directory.")
    }
  } else if (
    !file.copy(
      source,
      destination,
      copy.mode = TRUE,
      copy.date = TRUE
    )
  ) {
    .builder_adapter_abort("Could not copy an on-disk layer file.")
  }
  invisible(destination)
}

.builder_snapshot_permissions <- function(root) {
  if (.Platform$OS.type == "windows") {
    ## This contract guarantees POSIX mode bits only. Windows ACL hardening is
    ## deployment-specific and intentionally remains outside this task.
    return(invisible(TRUE))
  }
  directories <- c(
    root,
    list.dirs(root, recursive = TRUE, full.names = TRUE)
  )
  files <- list.files(
    root,
    all.files = TRUE,
    no.. = TRUE,
    recursive = TRUE,
    full.names = TRUE
  )
  files <- files[!dir.exists(files)]
  if (length(directories)) {
    Sys.chmod(unique(directories), mode = "0700", use_umask = FALSE)
  }
  if (length(files)) {
    Sys.chmod(files, mode = "0600", use_umask = FALSE)
  }
  directory_modes <- as.integer(file.info(unique(directories))$mode)
  file_modes <- as.integer(file.info(files)$mode)
  if (
    anyNA(directory_modes) ||
      any(directory_modes != strtoi("700", base = 8L)) ||
      anyNA(file_modes) ||
      any(file_modes != strtoi("600", base = 8L))
  ) {
    .builder_adapter_abort(
      "Builder could not enforce private snapshot permissions."
    )
  }
  invisible(TRUE)
}

.builder_snapshot_save_stub <- function(object, path) {
  SaveSeuratRds <- SeuratObject::SaveSeuratRds
  tryCatch(
    withCallingHandlers(
      SaveSeuratRds(
        object,
        file = path,
        move = FALSE,
        compress = FALSE
      ),
      warning = function(warning) {
        message <- conditionMessage(warning)
        if (
          grepl(
            "No layers found matching search pattern provided",
            message,
            fixed = TRUE
          ) ||
            grepl(
              "The matrix provided does not exist on-disk",
              message,
              fixed = TRUE
            )
        ) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = function(error) {
      .builder_adapter_abort(paste0(
        "Could not inspect on-disk Seurat layers: ",
        conditionMessage(error)
      ))
    }
  )
}

## -- Snapshot ownership and destructive cleanup ----------------------------

.builder_snapshot_marker_path <- function(path) {
  file.path(path, ".builder-owner.rds")
}

.builder_stage_marker_path <- function(path) {
  file.path(path, ".builder-stage-owner.rds")
}

.builder_stage_owner <- function(path) {
  owner <- list(
    path = .builder_canonical_path(path),
    owner_token = .builder_snapshot_token(),
    created_at = .builder_snapshot_now()
  )
  saveRDS(owner, .builder_stage_marker_path(path))
  if (.Platform$OS.type != "windows") {
    Sys.chmod(path, mode = "0700", use_umask = FALSE)
    Sys.chmod(
      .builder_stage_marker_path(path),
      mode = "0600",
      use_umask = FALSE
    )
  }
  owner
}

.builder_stage_owned_at <- function(path, owner) {
  if (
    !dir.exists(path) ||
      .builder_path_is_link(path) ||
      !is.list(owner)
  ) {
    return(FALSE)
  }
  marker_path <- .builder_stage_marker_path(path)
  if (!file.exists(marker_path) || .builder_path_is_link(marker_path)) {
    return(FALSE)
  }
  marker <- tryCatch(readRDS(marker_path), error = function(error) NULL)
  is.list(marker) &&
    identical(marker$path, owner$path) &&
    identical(marker$owner_token, owner$owner_token) &&
    identical(marker$created_at, owner$created_at)
}

.builder_snapshot_marker <- function(snapshot) {
  path <- .builder_snapshot_marker_path(snapshot$path)
  if (!file.exists(path) || .builder_path_is_link(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(error) NULL)
}

.builder_snapshot_owned_at <- function(
  path,
  snapshot,
  verify_content = TRUE,
  .digest_file = tools::md5sum
) {
  if (
    !is.list(snapshot) ||
      !.builder_adapter_scalar_text(snapshot$path) ||
      !.builder_adapter_scalar_text(snapshot$owner_token) ||
      !dir.exists(path) ||
      .builder_path_is_link(path)
  ) {
    return(FALSE)
  }
  canonical <- .builder_canonical_path(path)
  snapshot_parent <- dirname(snapshot$path)
  if (!dir.exists(snapshot_parent)) {
    return(FALSE)
  }
  expected_snapshot_path <- file.path(
    .builder_canonical_path(snapshot_parent),
    basename(snapshot$path)
  )
  if (!identical(expected_snapshot_path, snapshot$path)) {
    return(FALSE)
  }
  object_file <- file.path(canonical, "object.rds")
  if (!file.exists(object_file) || .builder_path_is_link(object_file)) {
    return(FALSE)
  }
  marker_path <- .builder_snapshot_marker_path(canonical)
  if (!file.exists(marker_path) || .builder_path_is_link(marker_path)) {
    return(FALSE)
  }
  marker <- tryCatch(readRDS(marker_path), error = function(error) NULL)
  snapshot_serialization <- snapshot$serialization %||% "rds"
  marker_serialization <- if (is.list(marker)) {
    marker$serialization %||% "rds"
  } else {
    NULL
  }
  observed_state <- tryCatch(
    .builder_file_source_state(object_file),
    error = function(error) NULL
  )
  if (is.list(observed_state)) {
    observed_state$location <- NULL
  }
  state_matches <- is.list(marker) &&
    is.list(snapshot$object_state) &&
    identical(marker$object_state, snapshot$object_state) &&
    identical(observed_state, snapshot$object_state)
  content_matches <- state_matches &&
    if (
      !identical(verify_content, FALSE) &&
        !identical(snapshot_serialization, "qs")
    ) {
      identical(
        snapshot$object_md5,
        unname(.digest_file(object_file))
      )
    } else {
      TRUE
    }
  is.list(marker) &&
    .builder_adapter_scalar_text(snapshot_serialization) &&
    snapshot_serialization %in% c("rds", "qs2", "qs") &&
    .builder_adapter_scalar_text(marker_serialization) &&
    identical(marker$path, snapshot$path) &&
    identical(marker$owner_token, snapshot$owner_token) &&
    identical(marker$created_at, snapshot$created_at) &&
    identical(marker$object_md5, snapshot$object_md5) &&
    identical(marker_serialization, snapshot_serialization) &&
    content_matches
}

.builder_snapshot_owned <- function(
  snapshot,
  verify_content = TRUE,
  .digest_file = tools::md5sum
) {
  if (!is.list(snapshot)) {
    return(FALSE)
  }
  .builder_snapshot_owned_at(
    snapshot$path %||% "",
    snapshot,
    verify_content = verify_content,
    .digest_file = .digest_file
  )
}

.builder_cleanup_after_check <- function(path) invisible(path)

.builder_isolate_owned_path <- function(path, validator) {
  if (!isTRUE(validator(path))) {
    return(FALSE)
  }
  .builder_cleanup_after_check(path)
  quarantine <- tempfile(
    ".q-",
    tmpdir = dirname(path)
  )
  if (file.exists(quarantine) || dir.exists(quarantine)) {
    return(FALSE)
  }
  if (!file.rename(path, quarantine)) {
    return(FALSE)
  }
  if (!isTRUE(validator(quarantine))) {
    if (!file.exists(path) && !dir.exists(path)) {
      file.rename(quarantine, path)
    }
    return(FALSE)
  }
  unlink(quarantine, recursive = TRUE, force = TRUE)
  !file.exists(quarantine) && !dir.exists(quarantine)
}

.builder_stage_release <- function(path, owner) {
  .builder_isolate_owned_path(
    path,
    function(candidate) .builder_stage_owned_at(candidate, owner)
  )
}

## -- On-disk layer equivalence contracts -----------------------------------

.builder_hdf5_layer_safe <- function(layer) {
  if (
    identical(class(layer)[[1L]], "HDF5Matrix") &&
      isTRUE(DelayedArray::isPristine(layer)) &&
      identical(DelayedArray::nseed(layer), 1L) &&
      identical(class(DelayedArray::seed(layer))[[1L]], "HDF5ArraySeed")
  ) {
    return(TRUE)
  }
  if (
    !identical(class(layer)[[1L]], "DelayedMatrix") ||
      !"seed" %in% methods::slotNames(layer)
  ) {
    return(FALSE)
  }
  operation <- methods::slot(layer, "seed")
  identical(class(operation)[[1L]], "DelayedSetDimnames") &&
    "seed" %in% methods::slotNames(operation) &&
    identical(
      class(methods::slot(operation, "seed"))[[1L]],
      "HDF5ArraySeed"
    )
}

.builder_layer_sample <- function(layer) {
  dimensions <- dim(layer)
  rows <- unique(c(1L, max(1L, dimensions[[1L]] %/% 2L), dimensions[[1L]]))
  columns <- unique(c(1L, max(1L, dimensions[[2L]] %/% 2L), dimensions[[2L]]))
  as.matrix(layer[rows, columns, drop = FALSE])
}

.builder_snapshot_layer_contracts <- function(
  object,
  .layer_data = SeuratObject::LayerData
) {
  contracts <- list()
  for (assay in names(object@assays)) {
    assay_object <- object[[assay]]
    for (layer_name in SeuratObject::Layers(assay_object)) {
      layer <- if (
        inherits(assay_object, "Assay") &&
          layer_name %in% methods::slotNames(assay_object)
      ) {
        methods::slot(assay_object, layer_name)
      } else {
        suppressWarnings(.layer_data(assay_object, layer = layer_name))
      }
      signature <- NULL
      if (inherits(layer, "IterableMatrix")) {
        .builder_adapter_abort(paste0(
          "Layer `",
          assay,
          "/",
          layer_name,
          "` is a BPCells matrix, which Builder does not snapshot. ",
          "Convert it to a regular matrix before importing."
        ))
      } else if (inherits(layer, "DelayedMatrix")) {
        backing_paths <- tryCatch(
          DelayedArray::path(layer),
          error = function(error) NULL
        )
        backing_paths <- as.character(backing_paths %||% character())
        backing_paths <- backing_paths[nzchar(backing_paths)]
        if (length(backing_paths) && !.builder_hdf5_layer_safe(layer)) {
          .builder_adapter_abort(paste0(
            "Layer `",
            assay,
            "/",
            layer_name,
            "` contains an unsupported ",
            "HDF5Array delayed operation. Materialize it before snapshotting."
          ))
        }
        if (length(backing_paths)) {
          signature <- "HDF5Matrix"
        }
      }
      if (!is.null(signature)) {
        key <- paste(assay, layer_name, sep = "\r")
        contracts[[key]] <- list(
          assay = assay,
          layer = layer_name,
          signature = signature,
          dim = dim(layer),
          dimnames = dimnames(layer),
          sample = .builder_layer_sample(layer)
        )
      }
    }
  }
  contracts
}

.builder_snapshot_verify_layer_contracts <- function(object, contracts) {
  for (contract in contracts) {
    if (
      !contract$assay %in% names(object@assays) ||
        !contract$layer %in% SeuratObject::Layers(object[[contract$assay]])
    ) {
      .builder_adapter_abort("A reopened snapshot is missing an on-disk layer.")
    }
    layer <- suppressWarnings(SeuratObject::LayerData(
      object[[contract$assay]],
      layer = contract$layer
    ))
    signature <- if (.builder_hdf5_layer_safe(layer)) {
      "HDF5Matrix"
    } else {
      NULL
    }
    if (
      !identical(signature, contract$signature) ||
        !identical(dim(layer), contract$dim) ||
        !identical(dimnames(layer), contract$dimnames) ||
        !isTRUE(all.equal(
          .builder_layer_sample(layer),
          contract$sample,
          tolerance = 0,
          check.attributes = TRUE
        ))
    ) {
      .builder_adapter_abort(paste0(
        "A reopened snapshot changed the structure or values of layer `",
        contract$assay,
        "/",
        contract$layer,
        "`."
      ))
    }
  }
  invisible(TRUE)
}

.builder_snapshot_initial_estimate <- function(object) {
  estimate <- as.numeric(object.size(object))
  if (length(estimate) != 1L || is.na(estimate) || !is.finite(estimate)) {
    .builder_adapter_abort("Builder could not estimate the snapshot size.")
  }
  estimate
}

## -- Snapshot publication and public lifecycle -----------------------------

## `progress(copied_bytes, total_bytes)` reports a reusable source copy.
.builder_snapshot_seurat_impl <- function(
  object,
  snapshot_dir,
  available_bytes = NULL,
  source = NULL,
  progress = NULL
) {
  if (!inherits(object, "Seurat")) {
    .builder_adapter_abort("A snapshot requires a Seurat object.")
  }
  if (!.builder_adapter_scalar_text(snapshot_dir)) {
    .builder_adapter_abort("A snapshot path must be one non-empty string.")
  }
  if (file.exists(snapshot_dir) || dir.exists(snapshot_dir)) {
    .builder_adapter_abort("The snapshot target already exists.")
  }
  parent <- dirname(snapshot_dir)
  if (!dir.exists(parent) || .builder_path_chain_has_link(parent)) {
    .builder_adapter_abort(paste0(
      "The snapshot parent must exist and contain no symbolic links."
    ))
  }
  target <- file.path(.builder_canonical_path(parent), basename(snapshot_dir))
  object <- .builder_clear_saved_cache(object)
  layer_contracts <- .builder_snapshot_layer_contracts(object)
  has_on_disk_layers <- length(layer_contracts) > 0L
  reusable_source <- if (
    !has_on_disk_layers &&
      is.list(source) &&
      .builder_adapter_scalar_text(source$location) &&
      is.list(source$source_state) &&
      .builder_adapter_scalar_text(source$serialization) &&
      source$serialization %in% c("rds", "qs2", "qs") &&
      (is.null(source$expected_md5) ||
        (.builder_adapter_scalar_text(source$expected_md5) &&
          grepl("^[0-9a-fA-F]{32}$", source$expected_md5))) &&
      identical(
        .builder_file_source_state(source$location),
        source$source_state
      )
  ) {
    source
  } else {
    NULL
  }
  if (is.null(available_bytes)) {
    available_bytes <- .builder_snapshot_available_bytes(parent)
  }
  .builder_snapshot_check_budget(
    if (is.null(reusable_source)) {
      .builder_snapshot_initial_estimate(object)
    } else {
      reusable_source$source_state$bytes
    },
    available_bytes
  )
  stage <- tempfile(
    ".s-",
    tmpdir = parent
  )
  if (!dir.create(stage, mode = "0700")) {
    .builder_adapter_abort("Could not create the private snapshot stage.")
  }
  stage_owner <- NULL
  published <- FALSE
  success <- FALSE
  descriptor <- NULL
  on.exit(
    {
      if (dir.exists(stage) && !is.null(stage_owner)) {
        .builder_stage_release(stage, stage_owner)
      }
      if (published && !success && !is.null(descriptor)) {
        .builder_snapshot_release(descriptor)
      }
    },
    add = TRUE
  )
  stage_owner <- .builder_stage_owner(stage)

  # This is a stable, format-neutral slot; serialization selects its reader.
  stub_path <- file.path(stage, "object.rds")
  verified_source <- NULL
  if (is.null(reusable_source)) {
    .builder_snapshot_save_stub(object, stub_path)
  } else {
    verified_source <- tryCatch(
      builder_copy_file_verified(
        reusable_source$location,
        stub_path,
        expected_md5 = reusable_source$expected_md5,
        on_progress = progress
      ),
      error = function(error) {
        .builder_adapter_abort(conditionMessage(error))
      }
    )
    if (
      !identical(
        verified_source$bytes,
        as.double(reusable_source$source_state$bytes)
      )
    ) {
      .builder_adapter_abort("The Seurat source changed while it was copied.")
    }
  }
  if (has_on_disk_layers && is.null(reusable_source)) {
    stub <- readRDS(stub_path)
    discovered <- .builder_snapshot_validate_cache(.builder_saved_cache(stub))
  } else {
    stub <- NULL
    discovered <- list(cache = NULL, members = list())
  }
  sources <- unique(unlist(discovered$members, use.names = FALSE))
  source_states <- lapply(sources, .builder_snapshot_tree_state)
  names(source_states) <- sources
  closure_bytes <- .builder_snapshot_size(stub_path)
  if (length(sources)) {
    closure_bytes <- closure_bytes +
      sum(vapply(
        sources,
        .builder_snapshot_size,
        numeric(1)
      ))
  }
  .builder_snapshot_check_budget(closure_bytes, available_bytes)

  rewritten <- setNames(character(length(sources)), sources)
  if (length(sources)) {
    backing <- file.path(stage, "backing")
    dir.create(backing, mode = "0700")
    for (index in seq_along(sources)) {
      source <- sources[[index]]
      destination_name <- paste0(
        "b",
        sprintf("%04d", index)
      )
      stage_destination <- file.path(backing, destination_name)
      .builder_snapshot_copy(source, stage_destination)
      if (.builder_path_tree_has_link(stage_destination)) {
        .builder_adapter_abort(paste0(
          "An on-disk layer changed to a symbolic link while it was copied. ",
          "Keep inputs unchanged during snapshot creation and retry."
        ))
      }
      rewritten[[source]] <- file.path(target, "backing", destination_name)
    }
    .builder_snapshot_after_copy()
    for (source in sources) {
      if (
        !identical(
          source_states[[source]],
          .builder_snapshot_tree_state(source)
        )
      ) {
        .builder_adapter_abort(paste0(
          "An on-disk layer changed while it was copied. Keep inputs ",
          "unchanged during snapshot creation and retry."
        ))
      }
    }
    cache <- discovered$cache
    for (index in seq_len(nrow(cache))) {
      cache$path[[index]] <- paste(
        rewritten[discovered$members[[index]]],
        collapse = ","
      )
    }
    stub@tools[["SaveSeuratRds"]] <- cache
    saveRDS(stub, stub_path, compress = FALSE)
  } else {
    .builder_snapshot_after_copy()
    if (
      !is.null(reusable_source) &&
        !identical(
          .builder_file_source_state(reusable_source$location),
          reusable_source$source_state
        )
    ) {
      .builder_adapter_abort(paste0(
        "The Seurat source changed while its snapshot was copied. Keep the ",
        "input unchanged and retry."
      ))
    }
  }

  created_at <- .builder_snapshot_now()
  owner_token <- .builder_snapshot_token()
  object_md5 <- if (is.null(reusable_source)) {
    unname(tools::md5sum(stub_path))
  } else {
    verified_source$md5
  }
  serialization <- if (is.null(reusable_source)) {
    "rds"
  } else {
    reusable_source$serialization
  }
  descriptor <- list(
    path = target,
    object_file = file.path(target, "object.rds"),
    serialization = serialization,
    owner_token = owner_token,
    created_at = created_at,
    object_md5 = object_md5,
    closure_bytes = closure_bytes
  )
  marker <- list(
    path = target,
    owner_token = owner_token,
    created_at = created_at,
    object_md5 = object_md5,
    serialization = serialization
  )
  marker_path <- .builder_snapshot_marker_path(stage)
  saveRDS(marker, marker_path)
  .builder_snapshot_permissions(stage)
  # POSIX chmod updates ctime, so bind ownership only after permissions settle.
  object_state <- .builder_file_source_state(stub_path)
  object_state$location <- NULL
  descriptor$object_state <- object_state
  marker$object_state <- object_state
  saveRDS(marker, marker_path)
  if (file.exists(target) || dir.exists(target)) {
    .builder_adapter_abort("The snapshot target appeared during creation.")
  }
  if (!file.rename(stage, target)) {
    .builder_adapter_abort("Could not atomically publish the Seurat snapshot.")
  }
  published <- TRUE
  .builder_snapshot_after_publish(descriptor)

  runtime_object <- if (length(sources)) {
    rewritten_cache <- .builder_snapshot_validate_cache(cache)
    .builder_snapshot_reopen_stub(stub, rewritten_cache)
  } else {
    object
  }
  .builder_snapshot_verify_layer_contracts(runtime_object, layer_contracts)
  success <- TRUE
  list(snapshot = descriptor, object = runtime_object)
}

#' Freeze a Seurat object and every live on-disk layer into one snapshot.
builder_snapshot_seurat <- function(
  object,
  snapshot_dir,
  available_bytes = NULL
) {
  .builder_snapshot_seurat_impl(
    object,
    snapshot_dir,
    available_bytes = available_bytes
  )$snapshot
}

#' Reopen one verified Builder snapshot.
builder_open_snapshot <- function(snapshot) {
  .builder_open_snapshot(snapshot, verify_content = TRUE)
}

## `verify_content = FALSE` skips the full-content MD5 for a snapshot this
## worker wrote itself; the stat and owner-marker checks still apply.
.builder_open_snapshot <- function(snapshot, verify_content = TRUE) {
  if (!.builder_snapshot_owned(snapshot, verify_content = verify_content)) {
    .builder_adapter_abort(
      "The Builder snapshot owner record is missing or changed."
    )
  }
  object_file <- file.path(snapshot$path, "object.rds")
  if (!file.exists(object_file) || .builder_path_is_link(object_file)) {
    .builder_adapter_abort(
      "The Builder snapshot object file is missing or unsafe."
    )
  }
  marker <- .builder_snapshot_marker(snapshot)
  object_state <- .builder_file_source_state(object_file)
  serialization <- snapshot$serialization %||% "rds"
  marker_serialization <- if (is.list(marker)) {
    marker$serialization %||% "rds"
  } else {
    NULL
  }
  if (
    !is.list(marker) ||
      !.builder_adapter_scalar_text(marker$object_md5) ||
      !identical(marker$object_md5, snapshot$object_md5) ||
      !identical(marker$object_state, snapshot$object_state) ||
      !identical(marker_serialization, serialization)
  ) {
    .builder_adapter_abort("The Builder snapshot integrity check failed.")
  }
  readers <- stats::setNames(
    lapply(builder_formats, `[[`, "read"),
    vapply(builder_formats, `[[`, character(1), "id")
  )
  if (!serialization %in% names(readers)) {
    .builder_adapter_abort("The Builder snapshot loader is not allowed.")
  }
  stub <- tryCatch(
    readers[[serialization]](object_file),
    error = function(error) {
      .builder_adapter_abort("The Builder snapshot object file is unreadable.")
    }
  )
  if (!identical(object_state, .builder_file_source_state(object_file))) {
    .builder_adapter_abort("The Builder snapshot changed while it was opened.")
  }
  discovered <- .builder_snapshot_validate_cache(.builder_saved_cache(stub))
  members <- unlist(discovered$members, use.names = FALSE)
  if (
    length(members) &&
      !all(vapply(
        members,
        .builder_path_within,
        logical(1),
        root = snapshot$path
      ))
  ) {
    .builder_adapter_abort("A snapshot cache path escapes its private root.")
  }
  .builder_snapshot_reopen_stub(stub, discovered)
}

.builder_snapshot_release <- function(snapshot) {
  if (!is.list(snapshot)) {
    return(FALSE)
  }
  # Ownership (path, owner token, creation time and stat state in the marker)
  # authorizes deletion; rehashing the content first would only delay it.
  .builder_isolate_owned_path(
    snapshot$path %||% "",
    function(candidate) {
      .builder_snapshot_owned_at(candidate, snapshot, verify_content = FALSE)
    }
  )
}

## -- Worker registration ----------------------------------------------------

## Decoded working objects are kept in least-recently-used order within a
## memory budget (bytes; `.builder_object_budget`, set from the
## `CerebroNexus.builder.worker_object_budget` option when the worker starts).
## A budget of 0 keeps only the dataset in use. The object in use is always
## kept, even when it alone exceeds the budget.
.builder_worker_object_budget <- function() {
  budget <- get0(
    ".builder_object_budget",
    envir = globalenv(),
    inherits = FALSE
  )
  if (
    !is.numeric(budget) ||
      length(budget) != 1L ||
      is.na(budget) ||
      budget < 0
  ) {
    return(0)
  }
  as.double(budget)
}

.builder_worker_object_ledger <- function() {
  ledger <- get0(
    ".builder_object_ledger",
    envir = globalenv(),
    inherits = FALSE
  )
  if (!is.environment(ledger)) {
    ledger <- new.env(parent = emptyenv())
    ledger$bytes <- numeric()
    ledger$used <- numeric()
    ledger$clock <- 0
    assign(".builder_object_ledger", ledger, envir = globalenv())
  }
  ledger
}

.builder_worker_object_bytes <- function(object) {
  bytes <- tryCatch(
    as.double(utils::object.size(object)),
    error = function(error) NA_real_
  )
  if (length(bytes) != 1L || is.na(bytes) || !is.finite(bytes)) 0 else bytes
}

## Record a use of `id`; pass `object` when it was (re)opened to size it.
.builder_worker_touch_object <- function(id, object = NULL) {
  ledger <- .builder_worker_object_ledger()
  ledger$clock <- ledger$clock + 1
  ledger$used[[id]] <- ledger$clock
  if (!is.null(object) || is.na(ledger$bytes[id])) {
    ledger$bytes[[id]] <- if (is.null(object)) {
      0
    } else {
      .builder_worker_object_bytes(object)
    }
  }
  invisible(id)
}

## Evict least-recently-used objects until the kept ones plus `reserve_bytes`
## (an estimate of an object about to be decoded) fit the budget.
.builder_worker_evict_objects <- function(
  keep = character(),
  reserve_bytes = Inf
) {
  objects <- get(".builder_objects", envir = globalenv())
  present <- ls(objects, all.names = TRUE)
  ledger <- .builder_worker_object_ledger()
  ledger$bytes <- ledger$bytes[intersect(names(ledger$bytes), present)]
  ledger$used <- ledger$used[intersect(names(ledger$used), present)]
  keep <- intersect(as.character(keep), present)
  bytes <- ledger$bytes[present]
  bytes[is.na(bytes)] <- 0
  names(bytes) <- present
  used <- ledger$used[present]
  used[is.na(used)] <- -Inf
  names(used) <- present
  candidates <- setdiff(present, keep)
  candidates <- candidates[order(used[candidates])]
  budget <- .builder_worker_object_budget()
  reserve_bytes <- suppressWarnings(as.double(reserve_bytes)[1L])
  if (is.na(reserve_bytes) || reserve_bytes < 0) {
    reserve_bytes <- 0
  }
  total <- sum(bytes)
  evict <- character()
  for (candidate in candidates) {
    if (total + reserve_bytes <= budget) {
      break
    }
    evict <- c(evict, candidate)
    total <- total - bytes[[candidate]]
  }
  if (length(evict)) {
    rm(list = evict, envir = objects)
    ledger$bytes <- ledger$bytes[setdiff(names(ledger$bytes), evict)]
    ledger$used <- ledger$used[setdiff(names(ledger$used), evict)]
    gc(FALSE)
  }
  invisible(evict)
}

## Decoded size estimate for a file about to be read. Compressed formats
## usually expand several-fold; the estimate only decides how much of the
## cache to release before decoding.
.builder_worker_decode_estimate <- function(bytes, serialization = "rds") {
  bytes <- suppressWarnings(as.double(bytes)[1L])
  if (is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return(0)
  }
  bytes * if (identical(serialization, "rds")) 1 else 4
}

.builder_worker_object <- function(id, evict = TRUE) {
  if (!.builder_adapter_scalar_text(id)) {
    .builder_adapter_abort("A dataset id is required.")
  }
  objects <- get(".builder_objects", envir = globalenv())
  snapshots <- get(".builder_snapshots", envir = globalenv())
  if (exists(id, envir = objects, inherits = FALSE)) {
    .builder_worker_touch_object(id)
    return(get(id, envir = objects, inherits = FALSE))
  }
  if (!exists(id, envir = snapshots, inherits = FALSE)) {
    .builder_adapter_abort("The requested dataset was not found.")
  }
  snapshot <- get(id, envir = snapshots, inherits = FALSE)
  if (isTRUE(evict)) {
    .builder_worker_evict_objects(
      keep = id,
      reserve_bytes = .builder_worker_decode_estimate(
        snapshot$closure_bytes %||% 0,
        snapshot$serialization %||% "rds"
      )
    )
  }
  # The worker wrote this private snapshot and hashed it while writing; stat
  # and owner-marker checks reject a replaced file without rereading it.
  object <- .builder_open_snapshot(snapshot, verify_content = FALSE)
  assign(id, object, envir = objects)
  .builder_worker_touch_object(id, object)
  if (isTRUE(evict)) {
    .builder_worker_evict_objects(keep = id, reserve_bytes = 0)
  }
  object
}

## Import progress callbacks from the worker accept (stage, bytes_done,
## bytes_total); older single-argument callbacks only receive stages.
.builder_progress_reports_bytes <- function(progress) {
  if (!is.function(progress)) {
    return(FALSE)
  }
  arguments <- names(formals(progress))
  "..." %in% arguments || length(arguments) >= 3L
}

## The source identity a snapshot records. A file source, or an example read
## from a package file, ends in the source file's MD5, the form a saved Project
## compares with the copy of that file it keeps.
.builder_adapter_source_fingerprint <- function(adapter, content_md5 = NULL) {
  if (identical(adapter$type, "file")) {
    return(paste(
      "builder-snapshot-v2",
      basename(adapter$source_state$location),
      adapter$source_state$bytes,
      adapter$source_state$modified_at,
      content_md5,
      sep = ":"
    ))
  }
  if (.builder_adapter_scalar_text(adapter$source_md5 %||% NULL)) {
    return(paste(
      "builder-snapshot-v2",
      adapter$source_file,
      "example",
      adapter$location,
      adapter$source_md5,
      sep = ":"
    ))
  }
  adapter$fingerprint
}

.builder_register_adapter <- function(adapter, id, progress = NULL) {
  if (!.builder_adapter_scalar_text(id)) {
    .builder_adapter_abort("A dataset registration requires a non-empty id.")
  }
  objects <- get(".builder_objects", envir = globalenv())
  snapshots <- get(".builder_snapshots", envir = globalenv())
  ## Imported Seurat objects can be much larger than their QS/QS2 files.
  ## Release least-recently-used objects until the decoded source should fit
  ## the worker's object budget; immutable snapshots reopen evicted datasets
  ## on demand.
  .builder_worker_evict_objects(
    keep = character(),
    reserve_bytes = if (identical(adapter$type, "file")) {
      .builder_worker_decode_estimate(
        adapter$source_state$bytes,
        adapter$reader$id %||% "rds"
      )
    } else {
      .builder_worker_object_bytes(adapter$object)
    }
  )
  snapshot_cache <- get0(
    ".builder_snapshot_cache",
    envir = globalenv(),
    inherits = FALSE
  )
  snapshot_root <- get(".builder_snapshot_root", envir = globalenv())
  cached <- if (
    is.environment(snapshot_cache) &&
      exists(adapter$fingerprint, envir = snapshot_cache, inherits = FALSE)
  ) {
    get(adapter$fingerprint, envir = snapshot_cache, inherits = FALSE)
  } else {
    NULL
  }
  if (
    is.list(cached) &&
      identical(cached$snapshot$source_state, adapter$source_state) &&
      isTRUE(.builder_snapshot_owned(cached$snapshot, verify_content = FALSE))
  ) {
    # A cached snapshot is this worker's own private copy; one stat and
    # owner-marker check replaces the two full-content MD5 passes.
    reopened <- .builder_open_snapshot(cached$snapshot, verify_content = FALSE)
    assign(id, reopened, envir = objects)
    assign(id, cached$snapshot, envir = snapshots)
    .builder_worker_touch_object(id, reopened)
    .builder_worker_evict_objects(keep = id, reserve_bytes = 0)
    return(c(
      cached$result,
      list(
        snapshot = cached$snapshot,
        previous_snapshot = NULL,
        cache_hit = TRUE
      )
    ))
  }
  inspected <- .builder_adapter_inspect(adapter, progress = progress)
  if (is.function(progress)) {
    progress("preparing")
  }
  target <- tempfile(
    "d-",
    tmpdir = snapshot_root
  )
  frozen <- .builder_snapshot_seurat_impl(
    inspected$object,
    target,
    source = inspected$snapshot_source,
    progress = if (.builder_progress_reports_bytes(progress)) {
      function(copied, total) progress("preparing", copied, total)
    }
  )
  snapshot <- frozen$snapshot
  snapshot$source_fingerprint <- .builder_adapter_source_fingerprint(
    adapter,
    inspected$source_md5 %||% snapshot$object_md5
  )
  snapshot$source_state <- adapter$source_state
  reopened <- frozen$object
  registered <- FALSE
  on.exit(
    {
      if (!registered) {
        .builder_snapshot_release(snapshot)
      }
    },
    add = TRUE
  )
  previous_snapshot <- if (exists(id, envir = snapshots, inherits = FALSE)) {
    get(id, envir = snapshots, inherits = FALSE)
  } else {
    NULL
  }
  previous_object <- if (exists(id, envir = objects, inherits = FALSE)) {
    get(id, envir = objects, inherits = FALSE)
  } else {
    NULL
  }
  tryCatch(
    {
      assign(id, reopened, envir = objects)
      assign(id, snapshot, envir = snapshots)
    },
    error = function(error) {
      if (is.null(previous_object)) {
        if (exists(id, envir = objects, inherits = FALSE)) {
          rm(list = id, envir = objects)
        }
      } else {
        assign(id, previous_object, envir = objects)
      }
      if (is.null(previous_snapshot)) {
        if (exists(id, envir = snapshots, inherits = FALSE)) {
          rm(list = id, envir = snapshots)
        }
      } else {
        assign(id, previous_snapshot, envir = snapshots)
      }
      stop(error)
    }
  )
  registered <- TRUE
  .builder_worker_touch_object(id, reopened)
  .builder_worker_evict_objects(keep = id, reserve_bytes = 0)
  result <- list(
    profile = inspected$legacy_profile,
    dataset_profile = inspected$profile,
    format = inspected$format,
    levels = inspected$levels,
    source = inspected$source,
    matrix_summary = c(
      inspected$matrix_summary,
      list(estimated_bytes = snapshot$closure_bytes)
    ),
    snapshot = snapshot,
    previous_snapshot = previous_snapshot,
    source_images = inspected$source_images
  )
  if (is.environment(snapshot_cache)) {
    assign(
      adapter$fingerprint,
      list(
        snapshot = snapshot,
        result = result[c(
          "profile",
          "dataset_profile",
          "format",
          "levels",
          "source",
          "matrix_summary",
          "source_images"
        )]
      ),
      envir = snapshot_cache
    )
  }
  result$cache_hit <- FALSE
  result
}
