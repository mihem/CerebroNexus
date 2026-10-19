VIEWER_PACK_SCHEMA_VERSION <- 1L

viewerPackCellOrderFingerprint <- function(cells) {
  cells <- enc2utf8(as.character(cells))
  if (anyNA(cells) || any(!nzchar(cells)) || anyDuplicated(cells)) {
    return(NA_character_)
  }
  path <- tempfile("viewer-pack-cell-order-")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")),
    path
  )
  paste0("md5-cell-order-v1:", unname(tools::md5sum(path)))
}

viewerPackOpen <- function(file, object) {
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !file.exists(file) ||
      dir.exists(file)
  ) {
    return(NULL)
  }
  file <- normalizePath(file, mustWork = TRUE)
  pack <- file.path(
    dirname(file),
    paste0(tools::file_path_sans_ext(basename(file)), ".viewer")
  )
  manifest_file <- file.path(pack, "manifest.json")
  if (!dir.exists(pack) || !file.exists(manifest_file)) {
    return(NULL)
  }
  manifest <- tryCatch(
    jsonlite::read_json(manifest_file, simplifyVector = TRUE),
    error = function(error) NULL
  )
  if (
    !is.list(manifest) ||
      !identical(manifest$schema_version, VIEWER_PACK_SCHEMA_VERSION) ||
      !identical(
        manifest$dataset_fingerprint,
        paste0("md5-crb-v1:", unname(tools::md5sum(file)))
      )
  ) {
    return(NULL)
  }
  cell_count <- suppressWarnings(as.integer(manifest$n_cells))
  if (
    length(cell_count) != 1L ||
      is.na(cell_count) ||
      cell_count < 0L ||
      !identical(as.numeric(cell_count), as.numeric(manifest$n_cells))
  ) {
    return(NULL)
  }
  assets <- manifest$assets
  if (
    !is.data.frame(assets) ||
      !all(c("path", "bytes", "checksum") %in% colnames(assets)) ||
      any(!file.exists(file.path(pack, assets$path)))
  ) {
    return(NULL)
  }
  if (
    length(manifest$projection_names) &&
      !identical(manifest$projection_identity_version, 1L)
  ) {
    # Keep independent metadata/IR assets; never publish unchecked coordinates.
    manifest$projection_names <- character()
    frames <- manifest$trajectory_frames
    if (is.data.frame(frames) && "geometry_kind" %in% names(frames)) {
      manifest$trajectory_frames <- frames[
        frames$geometry_kind != "canonical_projection",
        ,
        drop = FALSE
      ]
    }
    message("Rebuild Viewer Pack to enable verified projection assets: ", pack)
  }
  list(
    path = pack,
    manifest = manifest,
    cells = NULL,
    cell_count = cell_count,
    canonical_order = TRUE,
    cache = new.env(parent = emptyenv())
  )
}

viewerPackSourceSignature <- function(file) {
  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !file.exists(file) ||
      dir.exists(file)
  ) {
    return(NULL)
  }
  file <- normalizePath(file, mustWork = TRUE)
  manifest_file <- file.path(
    dirname(file),
    paste0(tools::file_path_sans_ext(basename(file)), ".viewer"),
    "manifest.json"
  )
  if (!file.exists(manifest_file) || dir.exists(manifest_file)) {
    return(NULL)
  }
  info <- file.info(c(file, manifest_file))
  if (anyNA(info$size) || anyNA(info$mtime)) {
    return(NULL)
  }
  list(
    key = file,
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime)
  )
}

viewerPackOpenCached <- function(file, object, cache) {
  if (!is.environment(cache)) {
    stop("Viewer Pack descriptor cache must be an environment.", call. = FALSE)
  }
  signature <- viewerPackSourceSignature(file)
  if (is.null(signature)) {
    return(NULL)
  }
  cached <- cache[[signature$key]]
  if (!is.null(cached) && identical(cached$signature, signature)) {
    pack <- cached$descriptor
    pack$cache <- new.env(parent = emptyenv())
    return(pack)
  }
  pack <- viewerPackOpen(file, object)
  if (is.null(pack)) {
    if (!is.null(cached)) {
      rm(list = signature$key, envir = cache)
    }
    return(NULL)
  }
  descriptor <- pack
  descriptor$cache <- NULL
  cache[[signature$key]] <- list(
    signature = signature,
    descriptor = descriptor
  )
  pack
}

## The manifest entry of `path` when its file matches the recorded size and
## checksum, else NULL. `accept(asset)` vets the manifest fields (dtype,
## dimensions, bytes) before any file IO, so a mismatch never hashes the file.
viewerPackVerifiedAsset <- function(
  pack,
  path,
  md5 = tools::md5sum,
  accept = NULL
) {
  assets <- pack$manifest$assets
  row <- which(as.character(assets$path) == as.character(path))
  if (length(row) != 1L) {
    return(NULL)
  }
  asset <- list(
    file = file.path(pack$path, path),
    dtype = as.character(assets$dtype[[row]]),
    bytes = as.numeric(assets$bytes[[row]]),
    checksum = as.character(assets$checksum[[row]]),
    dimensions = if ("dimensions" %in% names(assets)) {
      suppressWarnings(as.integer(strsplit(
        as.character(assets$dimensions[[row]]),
        "x",
        fixed = TRUE
      )[[1L]]))
    }
  )
  if (!is.null(accept) && !isTRUE(accept(asset))) {
    return(NULL)
  }
  file <- asset$file
  valid <- file.exists(file) &&
    !dir.exists(file) &&
    identical(as.numeric(file.info(file)$size), asset$bytes) &&
    identical(unname(md5(file)), asset$checksum)
  if (valid) asset else NULL
}

viewerPackValidateCellOrder <- function(pack) {
  if (isTRUE(pack$canonical_order)) {
    return(TRUE)
  }
  key <- ".cell_order_valid"
  if (exists(key, envir = pack$cache, inherits = FALSE)) {
    return(isTRUE(get(key, envir = pack$cache, inherits = FALSE)))
  }
  path <- "common/cell_order.qs2"
  asset <- viewerPackVerifiedAsset(pack, path)
  stored <- if (is.null(asset)) {
    NULL
  } else {
    tryCatch(qs2::qs_read(asset$file), error = function(error) NULL)
  }
  valid <- is.character(stored) && identical(stored, pack$cells)
  if (valid) {
    assign(path, stored, envir = pack$cache)
  }
  assign(key, valid, envir = pack$cache)
  valid
}

viewerPackReadAsset <- function(pack, path, validate_cell_order = TRUE) {
  if (is.null(pack) || !is.list(pack) || !is.environment(pack$cache)) {
    return(NULL)
  }
  if (isTRUE(validate_cell_order) && !viewerPackValidateCellOrder(pack)) {
    return(NULL)
  }
  if (exists(path, envir = pack$cache, inherits = FALSE)) {
    return(get(path, envir = pack$cache, inherits = FALSE))
  }
  asset <- viewerPackVerifiedAsset(pack, path)
  if (is.null(asset)) {
    return(NULL)
  }
  value <- tryCatch(qs2::qs_read(asset$file), error = function(error) NULL)
  if (!is.null(value)) {
    assign(path, value, envir = pack$cache)
  }
  value
}

viewerPackCellBarcodes <- function(pack, index = NULL) {
  cells <- pack$cells
  if (is.null(cells)) {
    cells <- viewerPackReadAsset(
      pack,
      "common/cell_order.qs2",
      validate_cell_order = FALSE
    )
  }
  if (
    !is.character(cells) ||
      length(cells) != pack$cell_count ||
      anyNA(cells) ||
      any(!nzchar(cells))
  ) {
    return(NULL)
  }
  if (is.null(index)) {
    return(cells)
  }
  index <- suppressWarnings(as.integer(index))
  if (anyNA(index) || any(index < 1L | index > length(cells))) {
    return(NULL)
  }
  cells[index]
}

viewerPackTrajectoryIndex <- function(pack, method, name) {
  indexes <- viewerPackReadAsset(
    pack,
    file.path("trajectory", "cell_index.qs2"),
    validate_cell_order = FALSE
  )
  index <- indexes[[method]][[name]]
  if (is.null(index)) NULL else as.integer(index)
}

viewerPackTrajectoryFrame <- function(pack, method, name) {
  frames <- pack$manifest$trajectory_frames
  if (!is.data.frame(frames) || !nrow(frames)) {
    return(NULL)
  }
  row <- which(
    as.character(frames$method) == as.character(method) &
      as.character(frames$name) == as.character(name)
  )
  if (length(row) != 1L) {
    return(NULL)
  }
  frame <- as.list(frames[row, , drop = FALSE])
  frame <- lapply(frame, function(value) value[[1L]])
  frame$cells <- suppressWarnings(as.integer(frame$cells))
  frame$geometry_kind <- as.character(
    if (is.null(frame$geometry_kind)) {
      "trajectory_asset"
    } else {
      frame$geometry_kind
    }
  )
  frame$edges_path <- as.character(
    if (is.null(frame$edges_path)) {
      ""
    } else {
      frame$edges_path
    }
  )
  required_paths <- c("state_codes_path", "state_dictionary_path")
  geometry_valid <- if (
    identical(frame$geometry_kind, "canonical_projection")
  ) {
    is.character(frame$projection_name) &&
      length(frame$projection_name) == 1L &&
      !is.na(frame$projection_name) &&
      nzchar(frame$projection_name) &&
      frame$subset_kind %in%
        c(
          "identity",
          "include_uint32",
          "exclude_uint32"
        ) &&
      (identical(frame$subset_kind, "identity") ||
        (is.character(frame$subset_path) &&
          length(frame$subset_path) == 1L &&
          !is.na(frame$subset_path) &&
          nzchar(frame$subset_path) &&
          identical(frame$subset_dtype, "uint32")))
  } else {
    identical(frame$geometry_kind, "trajectory_asset") &&
      is.character(frame$geometry_path) &&
      length(frame$geometry_path) == 1L &&
      !is.na(frame$geometry_path) &&
      nzchar(frame$geometry_path)
  }
  valid <- length(frame$cells) == 1L &&
    !is.na(frame$cells) &&
    frame$cells >= 0L &&
    geometry_valid &&
    all(vapply(
      frame[required_paths],
      function(path) {
        is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
      },
      logical(1)
    )) &&
    frame$state_dtype %in% c("uint8", "uint16", "uint32")
  if (!isTRUE(valid)) NULL else frame
}

viewerPackTrajectoryEdges <- function(pack, method, name) {
  frame <- viewerPackTrajectoryFrame(pack, method, name)
  if (
    is.null(frame) ||
      length(frame$edges_path) != 1L ||
      is.na(frame$edges_path) ||
      !nzchar(frame$edges_path)
  ) {
    return(NULL)
  }
  edges <- viewerPackReadAsset(
    pack,
    frame$edges_path,
    validate_cell_order = FALSE
  )
  required <- c(
    "source_dim_1",
    "source_dim_2",
    "target_dim_1",
    "target_dim_2"
  )
  if (!is.data.frame(edges) || !all(required %in% colnames(edges))) {
    return(NULL)
  }
  edges
}

viewerPackSpatialIndex <- function(pack, name) {
  indexes <- viewerPackReadAsset(
    pack,
    file.path("spatial", "cell_index.qs2"),
    validate_cell_order = FALSE
  )
  index <- indexes[[name]]
  if (is.null(index)) NULL else as.integer(index)
}

viewerPackSpatialFrame <- function(pack, name) {
  frames <- pack$manifest$spatial_frames
  if (!is.data.frame(frames) || !nrow(frames)) {
    return(NULL)
  }
  row <- which(as.character(frames$name) == as.character(name))
  if (length(row) != 1L) {
    return(NULL)
  }
  frame <- as.list(frames[row, , drop = FALSE])
  frame <- lapply(frame, function(value) value[[1L]])
  frame$cells <- suppressWarnings(as.integer(frame$cells))
  frame$dimensions <- suppressWarnings(as.integer(frame$dimensions))
  valid <- length(frame$cells) == 1L &&
    !is.na(frame$cells) &&
    frame$cells >= 0L &&
    identical(frame$dimensions, 2L) &&
    is.character(frame$geometry_path) &&
    length(frame$geometry_path) == 1L &&
    !is.na(frame$geometry_path) &&
    nzchar(frame$geometry_path)
  if (!isTRUE(valid)) NULL else frame
}

viewerPackGroupMetric <- function(pack, group, metric) {
  if (
    is.null(pack) ||
      !is.list(pack) ||
      !identical(pack$manifest$violin_summary_version, 1L)
  ) {
    return(NULL)
  }
  frames <- pack$manifest$group_metric_frames
  if (!is.data.frame(frames) || !nrow(frames)) {
    return(NULL)
  }
  row <- which(
    as.character(frames$group) == as.character(group) &
      as.character(frames$metric) == as.character(metric)
  )
  if (length(row) != 1L) {
    return(NULL)
  }
  path <- as.character(frames$path[[row]])
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  value <- viewerPackReadAsset(
    pack,
    path,
    validate_cell_order = FALSE
  )
  if (
    !is.data.frame(value) ||
      !all(c(group, metric) %in% colnames(value))
  ) {
    return(NULL)
  }
  value
}

viewerPackHlaSegments <- function(pack, chain) {
  if (
    !is.character(chain) || length(chain) != 1L || !chain %in% c("TRA", "TRB")
  ) {
    return(NULL)
  }
  viewerPackReadAsset(pack, file.path("hla_tcr", paste0(chain, ".qs2")))
}

viewerPackHlaFirstFrame <- function(pack, chain) {
  if (
    !is.character(chain) || length(chain) != 1L || !chain %in% c("TRA", "TRB")
  ) {
    return(NULL)
  }
  viewerPackReadAsset(
    pack,
    file.path("hla_tcr", paste0(chain, ".first.qs2")),
    validate_cell_order = FALSE
  )
}

viewerPackImmuneIndex <- function(pack, receptor) {
  if (
    !is.character(receptor) ||
      length(receptor) != 1L ||
      !receptor %in% c("TCR", "BCR")
  ) {
    return(NULL)
  }
  viewerPackReadAsset(pack, file.path("immune", paste0(receptor, ".qs2")))
}

viewerPackImmuneAbundance <- function(pack, clone_col) {
  values <- viewerPackReadAsset(pack, "immune/abundance.qs2")
  if (is.null(values) || !(clone_col %in% names(values))) {
    return(NULL)
  }
  values[[clone_col]]
}
