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
  raw_bytes <- charToRaw(paste0(
    nchar(cells, type = "bytes"),
    ":",
    cells,
    collapse = ""
  ))
  digest <- if ("bytes" %in% names(formals(tools::md5sum))) {
    unname(tools::md5sum(bytes = raw_bytes))
  } else {
    path <- tempfile("viewer-pack-cell-order-")
    on.exit(unlink(path), add = TRUE)
    writeBin(raw_bytes, path)
    unname(tools::md5sum(path))
  }
  paste0("md5-cell-order-v1:", digest)
}

## TRUE when `domain` is eligible to be served from `pack`.
## Packs opened via exact CRB MD5 match have `eligible_domains = NULL` (all
## domains eligible); packs opened via content-digest decoupling carry a named
## logical list so changed domains seamlessly fall back to the CRB.
viewerPackDomainEligible <- function(pack, domain) {
  if (!is.list(pack)) {
    return(FALSE)
  }
  eligible <- pack$eligible_domains
  if (is.null(eligible)) {
    return(TRUE)
  }
  isTRUE(eligible[[as.character(domain)]])
}

.viewerPackAssetDomain <- function(path) {
  path <- as.character(path)
  if (length(path) != 1L || is.na(path)) {
    return(NA_character_)
  }
  slash <- regexpr("/", path, fixed = TRUE)
  if (slash <= 1L) {
    return(NA_character_)
  }
  substr(path, 1L, slash - 1L)
}

.viewerPackRuntimeContentDigests <- function(object) {
  hash_section <- function(value) paste0("xxh128-v1:", rlang::hash(value))
  projections <- tryCatch(object$availableProjections(), error = function(e) {
    character()
  })
  proj_data <- stats::setNames(
    lapply(projections, function(nm) {
      tryCatch(object$getProjection(nm), error = function(e) NULL)
    }),
    projections
  )
  metadata <- tryCatch(object$getMetaData(), error = function(e) NULL)
  spatial_names <- tryCatch(object$availableSpatial(), error = function(e) {
    character()
  })
  spatial_coords <- stats::setNames(
    lapply(spatial_names, function(nm) {
      tryCatch(
        object$getSpatialData(nm, hydrate_molecules = FALSE)[["coordinates"]],
        error = function(e) NULL
      )
    }),
    spatial_names
  )
  repertoire <- tryCatch(object$getImmuneRepertoire(), error = function(e) {
    list()
  })
  ir_version <- if (length(repertoire)) {
    if (exists("IR_ENCODING_VERSION", inherits = TRUE)) {
      get("IR_ENCODING_VERSION", inherits = TRUE)
    } else {
      tryCatch(
        {
          enc_env <- new.env(parent = baseenv())
          sys.source(
            system.file(
              "viewer",
              "immune_repertoire",
              "encode.R",
              package = "CerebroNexus"
            ),
            envir = enc_env
          )
          enc_env$IR_ENCODING_VERSION
        },
        error = function(e) NA_integer_
      )
    }
  } else {
    0L
  }
  has_hla_pkgs <- requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("stringdist", quietly = TRUE)
  list(
    projections = hash_section(proj_data),
    metadata = hash_section(list(
      groups = tryCatch(object$getGroups(), error = function(e) character()),
      cell_cycle = tryCatch(object$getCellCycle(), error = function(e) {
        character()
      }),
      metadata = metadata
    )),
    trajectories = hash_section(object$trajectories),
    spatial = hash_section(spatial_coords),
    immune = hash_section(list(
      version = ir_version,
      repertoire = repertoire
    )),
    hla_tcr = hash_section(list(
      enabled = has_hla_pkgs,
      groups = tryCatch(object$getGroups(), error = function(e) character()),
      lineage = tryCatch(
        object$getTechnicalInfo()$lineage_column,
        error = function(e) NULL
      ),
      repertoire = repertoire,
      metadata = if (length(repertoire)) metadata else NULL
    ))
  )
}

viewerPackOpen <- function(file, object = NULL) {
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
      !identical(manifest$schema_version, VIEWER_PACK_SCHEMA_VERSION)
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
  md5_matches <- identical(
    manifest$dataset_fingerprint,
    paste0("md5-crb-v1:", .viewerCrbMd5(file))
  )
  match_mode <- "crb_md5"
  eligible_domains <- NULL
  if (!md5_matches) {
    if (is.null(object) || !is.list(manifest$content_digests)) {
      return(NULL)
    }
    metadata <- tryCatch(object$getMetaData(), error = function(e) NULL)
    cells <- if (is.data.frame(metadata) && "cell_barcode" %in% colnames(metadata)) {
      as.character(metadata$cell_barcode)
    } else {
      NULL
    }
    if (is.null(cells) || !identical(length(cells), cell_count)) {
      return(NULL)
    }
    ## Check cell order against verified Pack cell_order.qs2 first (fast), else hash.
    probe_pack <- list(
      path = pack,
      manifest = manifest,
      cell_count = cell_count,
      canonical_order = TRUE,
      cache = new.env(parent = emptyenv())
    )
    stored_cells <- viewerPackReadAsset(probe_pack, "common/cell_order.qs2")
    order_ok <- is.character(stored_cells) && identical(stored_cells, cells)
    if (
      !order_ok &&
        !identical(
          as.character(manifest$cell_order_fingerprint),
          as.character(viewerPackCellOrderFingerprint(cells))
        )
    ) {
      return(NULL)
    }
    current_digests <- .viewerPackRuntimeContentDigests(object)
    domain_matches <- vapply(
      names(current_digests),
      function(nm) {
        identical(
          as.character(manifest$content_digests[[nm]]),
          as.character(current_digests[[nm]])
        )
      },
      logical(1)
    )
    if (!any(domain_matches)) {
      return(NULL)
    }
    eligible_domains <- list(
      common = TRUE,
      projections = isTRUE(domain_matches[["projections"]]),
      metadata = isTRUE(domain_matches[["metadata"]]),
      groups = isTRUE(domain_matches[["metadata"]]),
      trajectory = isTRUE(domain_matches[["trajectories"]]) &&
        isTRUE(domain_matches[["projections"]]),
      spatial = isTRUE(domain_matches[["spatial"]]),
      immune = isTRUE(domain_matches[["immune"]]),
      hla_tcr = isTRUE(domain_matches[["hla_tcr"]])
    )
    match_mode <- if (all(domain_matches)) "content_digest" else "partial_domain"
  }
  list(
    path = pack,
    manifest = manifest,
    cell_count = cell_count,
    canonical_order = TRUE,
    match_mode = match_mode,
    eligible_domains = eligible_domains,
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

viewerPackOpenCached <- function(file, cache, object = NULL) {
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
  pack <- viewerPackOpen(file, object = object)
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
## The md5 of a CRB, which names the data set its Viewer Pack was built from.
## Hashing a large CRB takes about 0.7 s per 380 MB, and every start of the
## Viewer did it before reading the data set: kept in the user cache by file
## version, so a later start of the same file reads it back. A file replaced by
## another of the same size and modification time (cp -p, rsync -a, an archive
## unpacked) must not keep the old md5, or the Pack of the old data would be
## taken for it: the version also has the status change time (which a copy
## cannot set on Unix), and a stored md5 is used only while the first and last
## MiB of the file still hash as they did (.viewerCrbSample()).
.viewerCrbMd5 <- function(file) {
  info <- suppressWarnings(file.info(file, extra_cols = FALSE))
  if (is.na(info$size) || isTRUE(info$isdir)) {
    return(unname(tools::md5sum(file)))
  }
  key <- .viewerFileVersionKey(normalizePath(file, winslash = "/"), info)
  version_line <- paste(
    normalizePath(file, winslash = "/"),
    info$size,
    format(as.numeric(info$mtime), digits = 17),
    format(as.numeric(info$ctime), digits = 17),
    sep = "\t"
  )
  cached <- .viewer_file_md5_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  store <- tryCatch(
    file.path(
      tools::R_user_dir("CerebroNexus", "cache"),
      "crb-md5",
      paste0(.viewerCrbMd5Key(key), ".txt")
    ),
    error = function(error) NULL
  )
  sample <- NULL
  value <- if (!is.null(store) && file.exists(store)) {
    stored <- tryCatch(
      readLines(store, n = 3L, warn = FALSE),
      error = function(error) character()
    )
    ## The file version, the hash of its first and last MiB, its md5.
    if (
      length(stored) == 3L &&
        identical(stored[[1L]], version_line) &&
        grepl("^[0-9a-f]{32}$", stored[[3L]])
    ) {
      sample <- .viewerCrbSample(file, info$size)
      if (identical(stored[[2L]], sample)) stored[[3L]]
    }
  }
  if (is.null(value)) {
    value <- unname(tools::md5sum(file))
    if (!is.na(value) && !is.null(store)) {
      tryCatch(
        {
          dir.create(dirname(store), recursive = TRUE, showWarnings = FALSE)
          temporary <- paste0(store, ".", Sys.getpid(), ".tmp")
          if (is.null(sample)) {
            sample <- .viewerCrbSample(file, info$size)
          }
          writeLines(
            c(version_line, sample, value),
            temporary,
            useBytes = TRUE
          )
          file.rename(temporary, store)
        },
        error = function(error) NULL
      )
    }
  }
  if (!is.na(value)) {
    assign(key, value, envir = .viewer_file_md5_cache)
  }
  value
}
## The hash of the first and last MiB of a file of `size` bytes (the whole of
## a smaller one): a replaced CRB differs there (its header, its payload's
## last block), and reading them takes milliseconds.
.viewerCrbSample <- function(file, size, bytes = 2^20) {
  connection <- file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  head <- readBin(connection, "raw", n = min(size, bytes))
  tail <- if (size > bytes) {
    seek(connection, max(bytes, size - bytes))
    readBin(connection, "raw", n = bytes)
  }
  rlang::hash(c(head, tail))
}
## A file name for a file version key (its md5, of the key's bytes).
.viewerCrbMd5Key <- function(key) {
  raw_bytes <- charToRaw(enc2utf8(key))
  if ("bytes" %in% names(formals(tools::md5sum))) {
    return(unname(tools::md5sum(bytes = raw_bytes)))
  }
  path <- tempfile("crb-md5-key-")
  on.exit(unlink(path), add = TRUE)
  writeBin(raw_bytes, path)
  unname(tools::md5sum(path))
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
  domain <- .viewerPackAssetDomain(path)
  if (!is.na(domain) && !viewerPackDomainEligible(pack, domain)) {
    return(NULL)
  }
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

viewerPackProjectionFrame <- function(pack, name) {
  if (!viewerPackDomainEligible(pack, "projections")) {
    return(NULL)
  }
  frames <- pack$manifest$projection_frames
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
  x_min <- suppressWarnings(as.numeric(frame$x_min))
  x_max <- suppressWarnings(as.numeric(frame$x_max))
  y_min <- suppressWarnings(as.numeric(frame$y_min))
  y_max <- suppressWarnings(as.numeric(frame$y_max))
  if (
    length(x_min) == 1L &&
      length(x_max) == 1L &&
      length(y_min) == 1L &&
      length(y_max) == 1L &&
      all(is.finite(c(x_min, x_max, y_min, y_max)))
  ) {
    frame$x_range <- c(x_min, x_max)
    frame$y_range <- c(y_min, y_max)
  }
  frame
}

viewerPackTrajectoryFrame <- function(pack, method, name) {
  if (!viewerPackDomainEligible(pack, "trajectory")) {
    return(NULL)
  }
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
  x_min <- suppressWarnings(as.numeric(frame$x_min))
  x_max <- suppressWarnings(as.numeric(frame$x_max))
  y_min <- suppressWarnings(as.numeric(frame$y_min))
  y_max <- suppressWarnings(as.numeric(frame$y_max))
  if (
    length(x_min) == 1L &&
      length(x_max) == 1L &&
      length(y_min) == 1L &&
      length(y_max) == 1L &&
      all(is.finite(c(x_min, x_max, y_min, y_max)))
  ) {
    frame$x_range <- c(x_min, x_max)
    frame$y_range <- c(y_min, y_max)
  }
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
  if (!viewerPackDomainEligible(pack, "spatial")) {
    return(NULL)
  }
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
  x_min <- suppressWarnings(as.numeric(frame$x_min))
  x_max <- suppressWarnings(as.numeric(frame$x_max))
  y_min <- suppressWarnings(as.numeric(frame$y_min))
  y_max <- suppressWarnings(as.numeric(frame$y_max))
  if (
    length(x_min) == 1L &&
      length(x_max) == 1L &&
      length(y_min) == 1L &&
      length(y_max) == 1L &&
      all(is.finite(c(x_min, x_max, y_min, y_max)))
  ) {
    frame$x_range <- c(x_min, x_max)
    frame$y_range <- c(y_min, y_max)
  }
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

## The first frame of a chain: version 3 (format revision 2, without its
## segments) under its own name, else version 2 of an older Pack (with them).
viewerPackHlaFirstFrame <- function(pack, chain) {
  if (
    !is.character(chain) || length(chain) != 1L || !chain %in% c("TRA", "TRB")
  ) {
    return(NULL)
  }
  first <- viewerPackReadAsset(
    pack,
    file.path("hla_tcr", paste0(chain, ".first.v3.qs2"))
  )
  if (is.list(first) && identical(first$version, 3L)) {
    return(first)
  }
  first <- viewerPackReadAsset(
    pack,
    file.path("hla_tcr", paste0(chain, ".first.qs2"))
  )
  if (is.list(first) && !identical(first$version, 3L)) first else NULL
}

## The Pack path of a chain's first frame, NULL without one.
viewerPackHlaFirstFramePath <- function(pack, chain) {
  paths <- file.path(
    "hla_tcr",
    paste0(chain, c(".first.v3.qs2", ".first.qs2"))
  )
  paths <- paths[paths %in% as.character(pack$manifest$assets$path)]
  if (length(paths)) paths[[1L]] else NULL
}

## The segments of a first frame (the initial samples' parsed rows). A Pack of
## format revision 2 keeps them out of the first frame (version 3), in a file
## read only when they are needed, not before the first view draws: NULL for a
## Pack without that file, whose first frame (version 2) holds them.
viewerPackHlaFirstSegments <- function(pack, chain) {
  if (
    !is.character(chain) || length(chain) != 1L || !chain %in% c("TRA", "TRB")
  ) {
    return(NULL)
  }
  value <- viewerPackReadAsset(
    pack,
    file.path("hla_tcr", paste0(chain, ".first.segments.qs2"))
  )
  if (is.data.frame(value)) value else NULL
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

## The default Clonal projection frame of `receptor` and clone column `column`
## (R/viewer_pack.R, .viewerPackImmuneClonalFrames()), from the Pack's
## `immune_clonal_frames`: its rows (`cells`), the verified `subset` (uint32
## canonical rows in paint order) and `codes` (uint8, each row's trace) assets
## and the traces' expansion codes (`trace_codes`, 0 the grey background).
## NULL for a Pack without one (built before them), or one made with another
## frame `version`, clone-size `bins` or `max_background` than the Viewer's.
viewerPackImmuneClonalFrame <- function(
  pack,
  receptor,
  column,
  version,
  bins,
  max_background
) {
  frames <- pack$manifest$immune_clonal_frames
  if (!is.data.frame(frames) || !nrow(frames)) {
    return(NULL)
  }
  row <- which(
    as.character(frames$receptor) == as.character(receptor) &
      as.character(frames$clone_column) == as.character(column)
  )
  if (length(row) != 1L) {
    return(NULL)
  }
  frame <- lapply(as.list(frames[row, , drop = FALSE]), function(value) {
    value[[1L]]
  })
  string <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  cells <- suppressWarnings(as.integer(frame$cells))
  stored_bins <- if (string(frame$bins)) {
    suppressWarnings(as.numeric(strsplit(frame$bins, ",", fixed = TRUE)[[1L]]))
  }
  if (
    !identical(
      suppressWarnings(as.integer(frame$version)),
      as.integer(version)
    ) ||
      !identical(stored_bins, as.numeric(bins)) ||
      !identical(
        suppressWarnings(as.integer(frame$max_background)),
        as.integer(max_background)
      ) ||
      length(cells) != 1L ||
      is.na(cells) ||
      cells < 1L ||
      cells > pack$cell_count ||
      !identical(frame$codes_dtype, "uint8") ||
      !string(frame$subset_path) ||
      !string(frame$codes_path) ||
      !string(frame$dictionary_path)
  ) {
    return(NULL)
  }
  subset <- viewerPackVerifiedAsset(
    pack,
    frame$subset_path,
    accept = function(asset) {
      identical(asset$dtype, "uint32") &&
        identical(asset$dimensions, cells) &&
        identical(asset$bytes, as.numeric(cells) * 4)
    }
  )
  codes <- if (!is.null(subset)) {
    viewerPackVerifiedAsset(
      pack,
      frame$codes_path,
      accept = function(asset) {
        identical(asset$dtype, "uint8") &&
          identical(asset$dimensions, cells) &&
          identical(asset$bytes, as.numeric(cells))
      }
    )
  }
  dictionary <- if (!is.null(codes)) {
    viewerPackVerifiedAsset(pack, frame$dictionary_path)
  }
  if (is.null(dictionary)) {
    return(NULL)
  }
  trace_codes <- tryCatch(
    as.integer(jsonlite::read_json(dictionary$file, simplifyVector = TRUE)),
    error = function(error) NULL
  )
  if (
    !length(trace_codes) ||
      !identical(length(trace_codes), dictionary$dimensions) ||
      anyNA(trace_codes) ||
      anyDuplicated(trace_codes) ||
      any(trace_codes < 0L | trace_codes >= length(bins))
  ) {
    return(NULL)
  }
  list(
    receptor = as.character(receptor),
    column = as.character(column),
    cells = cells,
    subset = subset,
    codes = codes,
    trace_codes = trace_codes
  )
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
