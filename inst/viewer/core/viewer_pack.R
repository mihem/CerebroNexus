VIEWER_PACK_SCHEMA_VERSION <- 1L

## TRUE for an open Viewer Pack whose rows are the canonical cell order.
viewerPackCanonical <- function(pack) {
  is.list(pack) && isTRUE(pack$canonical_order)
}

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

viewerPackOpen <- function(file) {
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
  list(
    path = pack,
    manifest = manifest,
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
  info <- file.info(c(file, manifest_file), extra_cols = FALSE)
  if (anyNA(info$size) || anyNA(info$mtime)) {
    return(NULL)
  }
  list(
    key = file,
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime)
  )
}

viewerPackOpenCached <- function(file, cache) {
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
  pack <- viewerPackOpen(file)
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

## Process caches key one version of a file by its path as given, size and
## mtime.
.viewerFileVersionKey <- function(path, info) {
  paste(
    path,
    info$size,
    format(as.numeric(info$mtime), digits = 17),
    sep = "\r"
  )
}
## Viewer Pack assets are verified against their manifest checksum before a
## session exposes them. Hash each file version once per process, not once per
## session and asset.
.viewer_file_md5_cache <- new.env(parent = emptyenv())
.viewerFileMd5 <- function(paths) {
  vapply(
    as.character(paths),
    function(path) {
      info <- suppressWarnings(file.info(path, extra_cols = FALSE))
      if (is.na(info$size) || isTRUE(info$isdir)) {
        return(NA_character_)
      }
      key <- .viewerFileVersionKey(path, info)
      cached <- .viewer_file_md5_cache[[key]]
      if (!is.null(cached)) {
        return(cached)
      }
      value <- unname(tools::md5sum(path))
      if (!is.na(value)) {
        assign(key, value, envir = .viewer_file_md5_cache)
      }
      value
    },
    character(1),
    USE.NAMES = FALSE
  )
}

## The manifest entry of `path` when its file matches the recorded size and
## checksum, else NULL. `accept(asset)` vets the manifest fields (dtype,
## dimensions, bytes) before any file IO, so a mismatch never hashes the file.
viewerPackVerifiedAsset <- function(
  pack,
  path,
  md5 = .viewerFileMd5,
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
    identical(
      as.numeric(file.info(file, extra_cols = FALSE)$size),
      asset$bytes
    ) &&
    identical(unname(md5(file)), asset$checksum)
  if (valid) asset else NULL
}

viewerPackReadAsset <- function(pack, path) {
  if (is.null(pack) || !is.list(pack) || !is.environment(pack$cache)) {
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
  cells <- viewerPackReadAsset(pack, "common/cell_order.qs2")
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

## The names of some cells of the Pack. Where the Viewer has written the cell
## names file of the process (viewerCellNamesRead() in shiny_server.R) they
## are cut out of it, which creates only these strings; a session that has
## read every name of the Pack already, or a process without that file, takes
## them from the Pack's cell order.
viewerPackCellBarcodesAt <- function(pack, index) {
  read <- get0("viewerCellNamesRead", mode = "function")
  if (
    is.function(read) &&
      is.list(pack) &&
      is.environment(pack$cache) &&
      !exists("common/cell_order.qs2", envir = pack$cache, inherits = FALSE)
  ) {
    cells <- tryCatch(read(pack, index), error = function(error) NULL)
    if (!is.null(cells)) {
      return(cells)
    }
  }
  viewerPackCellBarcodes(pack, index)
}

viewerPackTrajectoryIndex <- function(pack, method, name) {
  indexes <- viewerPackReadAsset(
    pack,
    file.path("trajectory", "cell_index.qs2")
  )
  index <- indexes[[method]][[name]]
  if (is.null(index)) NULL else as.integer(index)
}

## Every reader finds a trajectory's cells through the Pack's cell index, so the
## row names of its `meta` (1.28 M strings, about 90 MB for mouse brain 1M)
## are dropped from `object` in place. A thin CRB that stores them as metadata
## rows (the attribute "cerebro_cell_index_rownames") has none to drop; its
## rows are then the Pack's too, and the object's getTrajectory() is not left
## to read every cell name for them.
viewerPackDropTrajectoryNames <- function(pack, object) {
  trajectories <- object$trajectories
  changed <- FALSE
  for (method in names(trajectories)) {
    for (name in names(trajectories[[method]])) {
      meta <- trajectories[[method]][[name]][["meta"]]
      if (!is.data.frame(meta)) {
        next
      }
      stored <- attr(meta, "row.names")
      indexed <- isTRUE(attr(meta, "cerebro_cell_index_rownames", exact = TRUE))
      if (!indexed && !is.character(stored)) {
        next
      }
      index <- viewerPackTrajectoryIndex(pack, method, name)
      if (
        length(index) != nrow(meta) ||
          anyNA(index) ||
          !all(index >= 1L & index <= pack$cell_count) ||
          (indexed && !identical(as.integer(stored), index))
      ) {
        next
      }
      attr(meta, "cerebro_cell_index_rownames") <- NULL
      rownames(meta) <- NULL
      trajectories[[method]][[name]][["meta"]] <- meta
      changed <- TRUE
    }
  }
  if (changed) {
    object$trajectories <- trajectories
  }
  invisible(object)
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
    frame$edges_path
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

## The Spatial page names the cells of a section by the row names of its
## coordinates. A thin CRB may store them as metadata rows (the attribute
## "cerebro_cell_index_rownames"): they are set in `object`, in place, from the
## Pack's cell names, so that the object's getSpatialData() does not hydrate
## the thin CRB for them. FALSE when there is nothing to name or no Pack to
## name it from; getSpatialData() then takes the names of the metadata.
viewerPackRestoreSpatialNames <- function(pack, object, name) {
  coordinates <- tryCatch(
    object$spatial[[name]][["coordinates"]],
    error = function(error) NULL
  )
  if (
    !is.data.frame(coordinates) ||
      !isTRUE(attr(coordinates, "cerebro_cell_index_rownames", exact = TRUE)) ||
      !viewerPackCanonical(pack)
  ) {
    return(invisible(FALSE))
  }
  index <- attr(coordinates, "row.names")
  if (!is.integer(index) || anyDuplicated(index)) {
    return(invisible(FALSE))
  }
  cells <- viewerPackCellBarcodesAt(pack, index)
  if (is.null(cells)) {
    return(invisible(FALSE))
  }
  attr(coordinates, "cerebro_cell_index_rownames") <- NULL
  attr(coordinates, "row.names") <- cells
  ## In the object the sessions of the process are cloned from too: reading
  ## the Pack's cell names takes a second for 800,000 cells, and only the
  ## first session that opens a section pays it then.
  for (target in list(object, viewerPackPrototype(object))) {
    stored <- if (is.environment(target)) {
      tryCatch(
        target$spatial[[name]][["coordinates"]],
        error = function(error) NULL
      )
    }
    if (
      is.data.frame(stored) &&
        isTRUE(attr(stored, "cerebro_cell_index_rownames", exact = TRUE)) &&
        identical(attr(stored, "row.names"), index)
    ) {
      spatial <- target$spatial
      spatial[[name]][["coordinates"]] <- coordinates
      target$spatial <- spatial
    }
  }
  invisible(TRUE)
}

## The object the sessions of this process clone the data set of `object` from
## (crb_cache.R), or NULL.
viewerPackPrototype <- function(object) {
  path <- attr(object, "cerebro_dataset_path", exact = TRUE)
  if (
    !is.character(path) ||
      length(path) != 1L ||
      is.na(path) ||
      !exists(".crb_process_cache", mode = "environment")
  ) {
    return(NULL)
  }
  prototype <- get(".crb_process_cache", mode = "environment")[[normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )]]$object
  if (is.environment(prototype) && !identical(prototype, object)) {
    prototype
  }
}

viewerPackSpatialIndex <- function(pack, name) {
  indexes <- viewerPackReadAsset(
    pack,
    file.path("spatial", "cell_index.qs2")
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
  if (is.null(pack) || !is.list(pack)) {
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
    path
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
    file.path("hla_tcr", paste0(chain, ".first.qs2"))
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

## The repertoire's integer columns (immune_repertoire/encode.R); NULL when the
## Pack holds another encoding version.
viewerPackImmuneEncoded <- function(pack, version) {
  if (!identical(as.integer(pack$manifest$immune_encoding), version)) {
    return(NULL)
  }
  value <- viewerPackReadAsset(pack, "immune/encoded.qs2")
  if (is.list(value) && identical(value$version, version)) value else NULL
}

## The clones `ids` of `column` on `chain`, read from the blocks that hold
## them (`blocks`: the encoding's block size and offsets). The file is
## verified once.
viewerPackImmuneCloneNames <- function(pack, column, chain, ids, blocks) {
  if (!column %in% c("CTgene", "CTnt", "CTaa", "CTstrict")) {
    return(NULL)
  }
  path <- file.path("immune", "names", paste0(column, ".bin"))
  key <- paste0(".verified:", path)
  if (!exists(key, envir = pack$cache, inherits = FALSE)) {
    assign(key, viewerPackVerifiedAsset(pack, path), envir = pack$cache)
  }
  asset <- get(key, envir = pack$cache, inherits = FALSE)
  offsets <- blocks$offsets[[column]][[chain]]
  if (is.null(asset) || is.null(offsets)) {
    return(NULL)
  }
  size <- blocks$size
  block <- (ids - 1L) %/% size + 1L
  out <- character(length(ids))
  con <- file(asset$file, "rb")
  on.exit(close(con))
  for (b in unique(block)) {
    seek(con, offsets[[b]])
    values <- qs2::qs_deserialize(
      readBin(con, "raw", offsets[[b + 1L]] - offsets[[b]])
    )
    hit <- block == b
    out[hit] <- values[(ids[hit] - 1L) %% size + 1L]
  }
  out
}
