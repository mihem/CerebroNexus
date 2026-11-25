.viewerPackSchemaVersion <- 1L
## The assets a Pack of this schema holds beyond the first release's, each
## optional, so that the Viewer reads a Pack of any revision (manifest
## `format_revision`, absent before 2):
##   2: the default Clonal projection frames (`immune_clonal_frames`,
##      immune/clonal/), and the HLA first frames without their segments
##      (version 3, hla_tcr/<chain>.first.v3.qs2), which are in
##      hla_tcr/<chain>.first.segments.qs2.
## A Viewer uses each asset only when its manifest entry is present and valid,
## so an older Pack opens as before and a newer one is never rejected. An asset
## whose content changes gets a new path: a Viewer of revision 1 reads
## hla_tcr/<chain>.first.qs2 without checking its version and takes its
## segments from it, so a first frame without them must not be found there
## (that Viewer builds the first view from the repertoire instead).
.viewerPackFormatRevision <- 2L

.viewerDatasetCapabilities <- function(object) {
  ## Each capability is one stored field. An object without the accessor
  ## predates that content and has none; an accessor that fails is an error,
  ## never "no content", or the Viewer would hide a page the CRB has.
  stored <- function(accessor) {
    method <- tryCatch(object[[accessor]], error = function(error) NULL)
    if (!is.function(method)) {
      return(NULL)
    }
    method()
  }
  available <- function(accessor) length(stored(accessor)) > 0L
  trajectory_methods <- as.character(stored("getMethodsForTrajectories"))
  list(
    marker_genes = available("getMethodsForMarkerGenes"),
    most_expressed_genes = available("getGroupsWithMostExpressedGenes"),
    enriched_pathways = available("getMethodsForEnrichedPathways"),
    extra_material = available("getExtraMaterialCategories"),
    trajectory = any(
      trajectory_methods %in%
        c(
          "monocle2",
          "paga",
          "scvelo",
          "pseudotime",
          "marker_guided",
          "illustrative",
          "published_pseudotime"
        )
    ),
    spatial = available("availableSpatial"),
    trekker = available("getTrekker")
  )
}

.viewerPackOptions <- function(
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L
) {
  mode <- match.arg(viewer_binary)
  threshold <- suppressWarnings(as.integer(viewer_binary_threshold))
  if (length(threshold) != 1L || is.na(threshold) || threshold < 0L) {
    stop(
      "`viewer_binary_threshold` must be one non-negative integer.",
      call. = FALSE
    )
  }
  list(mode = mode, threshold = threshold)
}

.viewerPackEnabled <- function(mode, threshold, n_cells) {
  identical(mode, "always") ||
    (identical(mode, "auto") && n_cells >= threshold)
}

.viewerPackPath <- function(file) {
  file.path(
    dirname(normalizePath(file, mustWork = FALSE)),
    paste0(tools::file_path_sans_ext(basename(file)), ".viewer")
  )
}

.viewerPackQsThreads <- function() {
  configured <- suppressWarnings(as.integer(getOption(
    "cerebro.viewer_pack_threads",
    NA_integer_
  )))
  if (length(configured) == 1L && !is.na(configured) && configured >= 1L) {
    return(configured)
  }
  cores <- suppressWarnings(tryCatch(
    parallel::detectCores(logical = FALSE),
    error = function(e) 1L
  ))
  if (length(cores) != 1L || is.na(cores) || cores < 1L) 1L else min(4L, as.integer(cores))
}

.viewerPackCrbSample <- function(file, size, bytes = 2^20) {
  connection <- file(file, open = "rb")
  on.exit(close(connection), add = TRUE)
  head <- readBin(connection, "raw", n = min(size, bytes))
  tail <- if (size > bytes) {
    seek(connection, max(bytes, size - bytes))
    readBin(connection, "raw", n = bytes)
  }
  rlang::hash(c(head, tail))
}

.viewerPackCrbMd5CachePath <- function(file, info) {
  norm <- normalizePath(file, winslash = "/", mustWork = FALSE)
  key <- paste(
    norm,
    info$size,
    format(as.numeric(info$mtime), digits = 17),
    sep = "\r"
  )
  raw_bytes <- charToRaw(enc2utf8(key))
  key_hash <- if ("bytes" %in% names(formals(tools::md5sum))) {
    unname(tools::md5sum(bytes = raw_bytes))
  } else {
    tmp <- tempfile("vp-md5-key-")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(raw_bytes, tmp)
    unname(tools::md5sum(tmp))
  }
  tryCatch(
    file.path(
      tools::R_user_dir("CerebroNexus", "cache"),
      "crb-md5",
      paste0(key_hash, ".txt")
    ),
    error = function(error) NULL
  )
}

.viewerPackCrbVersionLine <- function(file, info) {
  paste(
    normalizePath(file, winslash = "/", mustWork = FALSE),
    info$size,
    format(as.numeric(info$mtime), digits = 17),
    format(as.numeric(info$ctime), digits = 17),
    sep = "\t"
  )
}

.viewerPackWriteCrbMd5Cache <- function(file, md5_value) {
  info <- suppressWarnings(file.info(file, extra_cols = FALSE))
  if (
    is.na(info$size) ||
      isTRUE(info$isdir) ||
      info$size < 1048576 ||
      is.na(md5_value) ||
      !nzchar(md5_value)
  ) {
    return(invisible(FALSE))
  }
  store <- .viewerPackCrbMd5CachePath(file, info)
  if (is.null(store)) {
    return(invisible(FALSE))
  }
  tryCatch(
    {
      dir.create(dirname(store), recursive = TRUE, showWarnings = FALSE)
      temporary <- paste0(store, ".", Sys.getpid(), ".tmp")
      writeLines(
        c(
          .viewerPackCrbVersionLine(file, info),
          .viewerPackCrbSample(file, info$size),
          as.character(md5_value)
        ),
        temporary,
        useBytes = TRUE
      )
      file.rename(temporary, store)
      invisible(TRUE)
    },
    error = function(error) invisible(FALSE)
  )
}

.viewerPackMd5 <- function(path, use_crb_cache = FALSE) {
  if (isTRUE(use_crb_cache)) {
    info <- suppressWarnings(file.info(path, extra_cols = FALSE))
    if (!is.na(info$size) && !isTRUE(info$isdir) && info$size >= 1048576) {
      store <- .viewerPackCrbMd5CachePath(path, info)
      if (!is.null(store) && file.exists(store)) {
        stored <- tryCatch(readLines(store, n = 3L, warn = FALSE), error = function(e) character())
        if (
          length(stored) == 3L &&
            identical(stored[[1L]], .viewerPackCrbVersionLine(path, info)) &&
            grepl("^[0-9a-f]{32}$", stored[[3L]]) &&
            identical(stored[[2L]], .viewerPackCrbSample(path, info$size))
        ) {
          return(stored[[3L]])
        }
      }
    }
  }
  value <- unname(tools::md5sum(path))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Could not checksum Viewer Pack input.", call. = FALSE)
  }
  if (isTRUE(use_crb_cache)) {
    .viewerPackWriteCrbMd5Cache(path, value)
  }
  value
}

.viewerPackCellOrderFingerprint <- function(cells) {
  cells <- enc2utf8(as.character(cells))
  if (anyNA(cells) || any(!nzchar(cells)) || anyDuplicated(cells)) {
    stop("Viewer Pack cells must be unique, non-empty strings.", call. = FALSE)
  }
  if ("bytes" %in% names(formals(tools::md5sum)) && length(cells) <= 2e5) {
    raw_bytes <- charToRaw(paste0(
      nchar(cells, type = "bytes"),
      ":",
      cells,
      collapse = ""
    ))
    return(paste0("md5-cell-order-v1:", unname(tools::md5sum(bytes = raw_bytes))))
  }
  path <- tempfile("viewer-pack-cell-order-")
  on.exit(unlink(path), add = TRUE)
  ## One string per cell, written as bytes: about half the time of collapsing
  ## 1.3 M names into a single string first, with the same digest.
  connection <- file(path, "wb")
  writeLines(
    paste0(nchar(cells, type = "bytes"), ":", cells),
    connection,
    sep = "",
    useBytes = TRUE
  )
  close(connection)
  paste0("md5-cell-order-v1:", .viewerPackMd5(path))
}

.viewerPackCells <- function(object) {
  metadata <- object$getMetaData()
  if (!is.data.frame(metadata) || !("cell_barcode" %in% colnames(metadata))) {
    stop(
      "Viewer Pack requires cell-aligned metadata with `cell_barcode`.",
      call. = FALSE
    )
  }
  as.character(metadata$cell_barcode)
}

.viewerPackWriteAsset <- function(root, path, value, dtype, dimensions = NULL) {
  target <- file.path(root, path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  extension <- tools::file_ext(path)
  if (identical(extension, "json")) {
    jsonlite::write_json(value, target, auto_unbox = TRUE)
  } else if (identical(extension, "bin")) {
    if (identical(dtype, "uint8")) {
      writeBin(as.raw(value), target)
    } else if (identical(dtype, "uint16")) {
      writeBin(as.integer(value), target, size = 2L, endian = "little")
    } else if (identical(dtype, "uint32")) {
      writeBin(as.integer(value), target, size = 4L, endian = "little")
    } else {
      writeBin(as.numeric(value), target, size = 4L, endian = "little")
    }
  } else {
    qs2::qs_save(value, target, nthreads = .viewerPackQsThreads())
  }
  data.frame(
    path = path,
    dtype = dtype,
    dimensions = if (is.null(dimensions)) {
      ""
    } else {
      paste(dimensions, collapse = "x")
    },
    bytes = as.numeric(file.info(target)$size),
    checksum = .viewerPackMd5(target),
    stringsAsFactors = FALSE
  )
}

.viewerPackCodeDtype <- function(n) {
  if (n <= 255L) {
    "uint8"
  } else if (n <= 65535L) {
    "uint16"
  } else {
    "uint32"
  }
}

.viewerPackRowsInReceptor <- function(frame, receptor, clone_col = "CTgene") {
  chains <- if (identical(receptor, "BCR")) {
    c("IGH", "IGK", "IGL")
  } else {
    c("TRA", "TRB", "TRG", "TRD")
  }
  reference <- if ("CTstrict" %in% colnames(frame)) {
    as.character(frame$CTstrict)
  } else {
    as.character(frame[[clone_col]])
  }
  Reduce(
    `|`,
    lapply(chains, function(chain) {
      match <- grepl(chain, reference, fixed = TRUE)
      match[is.na(match)] <- FALSE
      match
    })
  )
}

.viewerPackImmuneIndex <- function(repertoire, cells, receptor) {
  rows <- do.call(
    rbind,
    lapply(repertoire, function(frame) {
      if (is.null(frame) || !all(c("barcode", "CTgene") %in% colnames(frame))) {
        return(NULL)
      }
      keep <- .viewerPackRowsInReceptor(frame, receptor)
      if (!any(keep)) {
        return(NULL)
      }
      data.frame(
        barcode = as.character(frame$barcode[keep]),
        clone = as.character(frame$CTgene[keep]),
        ctaa = if ("CTaa" %in% colnames(frame)) {
          as.character(frame$CTaa[keep])
        } else {
          as.character(frame$CTgene[keep])
        },
        stringsAsFactors = FALSE
      )
    })
  )
  if (is.null(rows) || !nrow(rows)) {
    return(NULL)
  }
  rows <- rows[!is.na(rows$clone) & nzchar(rows$clone), , drop = FALSE]
  rows <- rows[!duplicated(rows$barcode), , drop = FALSE]
  cell_index <- match(rows$barcode, cells)
  keep <- !is.na(cell_index)
  rows <- rows[keep, , drop = FALSE]
  cell_index <- cell_index[keep]
  clone_ids <- match(rows$clone, unique(rows$clone))
  sizes <- tabulate(clone_ids)
  expansion <- as.integer(cut(
    sizes[clone_ids],
    breaks = c(0, 1, 5, 20, 100, Inf),
    labels = FALSE,
    right = TRUE,
    include.lowest = TRUE
  ))
  list(
    cell_index = as.integer(cell_index),
    clone = rows$clone,
    ctaa = rows$ctaa,
    expansion = expansion,
    receptor = receptor
  )
}

## TRUE for a Pack of a data set with a repertoire whose integer encoding is
## missing or of another version than this release's: its Immune repertoire
## page would encode the repertoire in memory at every start.
.viewerPackImmuneOutdated <- function(manifest) {
  length(manifest$immune_receptors) > 0L &&
    !identical(
      as.integer(manifest$immune_encoding %||% 0L)[[1L]],
      .viewerPackImmuneEncoder()$IR_ENCODING_VERSION
    )
}

## The repertoire's integer columns (inst/viewer/immune_repertoire/encode.R,
## which the Viewer also runs for a data set without a Pack).
.viewerPackImmuneEncoder <- local({
  scope <- NULL
  function() {
    if (is.null(scope)) {
      scope <<- new.env(parent = baseenv())
      sys.source(
        .cerebroPackageResource("viewer", "immune_repertoire", "encode.R"),
        envir = scope
      )
    }
    scope
  }
})

## The clone-size bins of the Clonal projection (inst/viewer/clone_contract.R,
## CEREBRO_CLONE_BINS); recorded with each frame, which the Viewer uses only
## while its bins are these.
.viewerPackImmuneClonalBins <- c(0, 1, 5, 20, 100, Inf)

## The default Clonal projection frame of each receptor (Immune repertoire
## page, clone call "gene", every cell shown, at most 100,000 grey background
## cells), cut by the Viewer's own function (encode.R, ir_clonal_frame_cells()),
## so that the Viewer draws it from these files instead of sending its
## coordinates and rows (Ren: about 4 MB per frame) from the encoding:
##   <receptor>.CTgene.subset.bin      the frame's canonical rows (0-based,
##                                     uint32) in paint order: the background,
##                                     then each expansion level, smallest first
##   <receptor>.CTgene.codes.bin       each row's trace (uint8, 0-based)
##   <receptor>.CTgene.dictionary.json each trace's expansion code (0: the
##                                     background, k: bin k)
## The frame holds the same cells on any projection, all of whose rows are the
## canonical cells. `encoded` NULL: none (the manifest's empty table).
.viewerPackImmuneClonalFrames <- function(encoded, n_cells, stage) {
  frames <- list()
  assets <- NULL
  for (receptor in intersect(c("TCR", "BCR"), encoded$receptors)) {
    encoder <- .viewerPackImmuneEncoder()
    frame <- encoder$ir_clonal_frame_cells(
      encoded,
      receptor,
      "CTgene",
      n_cells,
      .viewerPackImmuneClonalBins,
      max_background = 100000L
    )
    if (is.null(frame)) {
      next
    }
    ## Stable: the cells of a level keep their order, as in the Viewer's
    ## traces.
    coloured <- order(frame$code, method = "radix")
    rows <- c(frame$background, frame$coloured[coloured])
    trace_codes <- c(
      if (length(frame$background)) 0L,
      sort(unique(as.integer(frame$code)))
    )
    codes <- match(
      c(rep.int(0L, length(frame$background)), frame$code[coloured]),
      trace_codes
    ) -
      1L
    prefix <- file.path("immune", "clonal", paste0(receptor, ".CTgene"))
    paths <- paste0(prefix, c(".subset.bin", ".codes.bin", ".dictionary.json"))
    assets <- rbind(
      assets,
      .viewerPackWriteAsset(
        stage,
        paths[[1L]],
        rows - 1L,
        "uint32",
        length(rows)
      ),
      .viewerPackWriteAsset(stage, paths[[2L]], codes, "uint8", length(codes)),
      .viewerPackWriteAsset(
        stage,
        paths[[3L]],
        trace_codes,
        "int32",
        length(trace_codes)
      )
    )
    frames[[length(frames) + 1L]] <- data.frame(
      receptor = receptor,
      clone_column = "CTgene",
      version = encoder$IR_CLONAL_FRAME_VERSION,
      cells = length(rows),
      max_background = 100000L,
      bins = paste(.viewerPackImmuneClonalBins, collapse = ","),
      subset_path = paths[[1L]],
      codes_path = paths[[2L]],
      codes_dtype = "uint8",
      dictionary_path = paths[[3L]],
      stringsAsFactors = FALSE
    )
  }
  table <- if (length(frames)) {
    do.call(rbind, frames)
  } else {
    data.frame(
      receptor = character(),
      clone_column = character(),
      version = integer(),
      cells = integer(),
      max_background = integer(),
      bins = character(),
      subset_path = character(),
      codes_path = character(),
      codes_dtype = character(),
      dictionary_path = character(),
      stringsAsFactors = FALSE
    )
  }
  rownames(table) <- NULL
  list(frames = table, assets = assets)
}

## Each chain's clone names in blocks of `size`, each block serialized on its
## own, so the Viewer reads the few names it labels instead of every clone.
## `offsets` are each chain's block boundaries in the file.
.viewerPackWriteNameBlocks <- function(root, path, names, size) {
  target <- file.path(root, path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  con <- file(target, "wb")
  position <- 0
  offsets <- list()
  for (chain in names(names)) {
    values <- names[[chain]]
    bounds <- position
    for (start in seq_len(ceiling(length(values) / size)) * size - size + 1L) {
      block <- values[start:min(start + size - 1L, length(values))]
      block <- qs2::qs_serialize(block)
      writeBin(block, con)
      position <- position + length(block)
      bounds <- c(bounds, position)
    }
    offsets[[chain]] <- bounds
  }
  close(con)
  list(
    asset = data.frame(
      path = path,
      dtype = "immune-clone-name-blocks",
      dimensions = "",
      bytes = as.numeric(file.info(target)$size),
      checksum = .viewerPackMd5(target),
      stringsAsFactors = FALSE
    ),
    offsets = offsets
  )
}

.viewerPackTrajectoryFrames <- function(
  object,
  cells,
  stage,
  projections = list()
) {
  methods <- object$getMethodsForTrajectories()
  indexes <- stats::setNames(vector("list", length(methods)), methods)
  frames <- list()
  assets <- NULL
  frame_id <- 0L
  for (method in methods) {
    names <- object$getNamesOfTrajectories(method)
    method_indexes <- stats::setNames(vector("list", length(names)), names)
    for (name in names) {
      trajectory <- object$getTrajectory(method, name)
      meta <- trajectory[["meta"]]
      required <- c("DR_1", "DR_2", "pseudotime", "state")
      trajectory_cells <- rownames(meta)
      index <- match(trajectory_cells, cells)
      if (
        !is.data.frame(meta) ||
          is.null(trajectory_cells) ||
          length(index) != nrow(meta) ||
          anyNA(index) ||
          !all(required %in% colnames(meta))
      ) {
        stop("Viewer Pack trajectory is not cell-aligned.", call. = FALSE)
      }
      method_indexes[[name]] <- as.integer(index)

      ## Match the Viewer first-frame contract: cells with missing pseudotime
      ## are excluded before rendering, while trajectory row order is retained.
      keep <- !is.na(meta[["pseudotime"]])
      frame <- meta[keep, , drop = FALSE]
      frame_index <- as.integer(index[keep])
      state <- frame[["state"]]
      if (is.numeric(state)) {
        state <- factor(state)
      }
      state_values <- as.character(state)
      state_values[is.na(state_values)] <- "(missing)"
      state_levels <- if (is.factor(state)) {
        as.character(levels(state))
      } else {
        unique(state_values)
      }
      if ("(missing)" %in% state_values && !"(missing)" %in% state_levels) {
        state_levels <- c(state_levels, "(missing)")
      }
      state_codes <- match(state_values, state_levels) - 1L
      state_dtype <- .viewerPackCodeDtype(length(state_levels))
      frame_id <- frame_id + 1L
      prefix <- file.path("trajectory", sprintf("%03d", frame_id))
      geometry_path <- paste0(prefix, ".geometry.bin")
      edges_path <- paste0(prefix, ".edges.qs2")
      subset_path <- paste0(prefix, ".subset.bin")
      state_codes_path <- paste0(prefix, ".state.codes.bin")
      state_dictionary_path <- paste0(prefix, ".state.dictionary.json")
      geometry <- as.matrix(frame[, c("DR_1", "DR_2"), drop = FALSE])
      if (!is.numeric(geometry)) {
        stop(
          "Viewer Pack trajectory coordinates must be numeric.",
          call. = FALSE
        )
      }
      matching_projection <- if (
        length(frame_index) && !anyDuplicated(frame_index)
      ) {
        names(projections)[vapply(
          projections,
          function(projection) {
            is.matrix(projection) &&
              is.numeric(projection) &&
              nrow(projection) == length(cells) &&
              ncol(projection) == 2L &&
              identical(
                as.numeric(projection[frame_index, , drop = FALSE]),
                as.numeric(geometry)
              )
          },
          logical(1)
        )]
      } else {
        character()
      }
      geometry_kind <- if (length(matching_projection)) {
        "canonical_projection"
      } else {
        "trajectory_asset"
      }
      projection_name <- if (length(matching_projection)) {
        matching_projection[[1L]]
      } else {
        ""
      }
      subset_kind <- ""
      subset_values <- integer()
      if (identical(geometry_kind, "canonical_projection")) {
        if (identical(frame_index, seq_along(cells))) {
          subset_kind <- "identity"
          subset_path <- ""
        } else if (
          !is.unsorted(frame_index, strictly = TRUE) &&
            length(cells) - length(frame_index) < length(frame_index)
        ) {
          subset_kind <- "exclude_uint32"
          subset_values <- setdiff(seq_along(cells), frame_index) - 1L
        } else {
          subset_kind <- "include_uint32"
          subset_values <- frame_index - 1L
        }
      }
      assets <- rbind(
        assets,
        if (identical(geometry_kind, "trajectory_asset")) {
          .viewerPackWriteAsset(
            stage,
            geometry_path,
            as.numeric(t(geometry)),
            "float32",
            dim(geometry)
          )
        } else if (length(subset_values)) {
          .viewerPackWriteAsset(
            stage,
            subset_path,
            subset_values,
            "uint32",
            length(subset_values)
          )
        },
        .viewerPackWriteAsset(
          stage,
          state_codes_path,
          state_codes,
          state_dtype,
          length(state_codes)
        ),
        .viewerPackWriteAsset(
          stage,
          state_dictionary_path,
          state_levels,
          "utf8",
          length(state_levels)
        ),
        .viewerPackWriteAsset(
          stage,
          edges_path,
          trajectory[["edges"]],
          "trajectory-edges",
          nrow(trajectory[["edges"]])
        )
      )
      finite_x <- geometry[, 1L][is.finite(geometry[, 1L])]
      finite_y <- geometry[, 2L][is.finite(geometry[, 2L])]
      x_min <- if (length(finite_x)) min(finite_x) else NA_real_
      x_max <- if (length(finite_x)) max(finite_x) else NA_real_
      y_min <- if (length(finite_y)) min(finite_y) else NA_real_
      y_max <- if (length(finite_y)) max(finite_y) else NA_real_
      frames[[length(frames) + 1L]] <- data.frame(
        method = method,
        name = name,
        cells = nrow(frame),
        geometry_kind = geometry_kind,
        geometry_path = if (identical(geometry_kind, "trajectory_asset")) {
          geometry_path
        } else {
          ""
        },
        projection_name = projection_name,
        subset_kind = subset_kind,
        subset_path = subset_path,
        subset_dtype = if (nzchar(subset_path)) "uint32" else "",
        edges_path = edges_path,
        state_codes_path = state_codes_path,
        state_dictionary_path = state_dictionary_path,
        state_dtype = state_dtype,
        x_min = x_min,
        x_max = x_max,
        y_min = y_min,
        y_max = y_max,
        stringsAsFactors = FALSE
      )
    }
    indexes[[method]] <- method_indexes
  }
  frame_table <- if (length(frames)) {
    do.call(rbind, frames)
  } else {
    data.frame(
      method = character(),
      name = character(),
      cells = integer(),
      geometry_kind = character(),
      geometry_path = character(),
      projection_name = character(),
      subset_kind = character(),
      subset_path = character(),
      subset_dtype = character(),
      edges_path = character(),
      state_codes_path = character(),
      state_dictionary_path = character(),
      state_dtype = character(),
      x_min = numeric(),
      x_max = numeric(),
      y_min = numeric(),
      y_max = numeric(),
      stringsAsFactors = FALSE
    )
  }
  rownames(frame_table) <- NULL
  list(indexes = indexes, frames = frame_table, assets = assets)
}

.viewerPackSpatialAssets <- function(
  object,
  cells,
  stage,
  existing_pack = NULL,
  existing_manifest = NULL,
  item_digests = NULL
) {
  spatial_names <- object$availableSpatial()
  getter <- object$getSpatialData
  indexes <- stats::setNames(
    vector("list", length(spatial_names)),
    spatial_names
  )
  frames <- list()
  assets <- NULL
  old_frames <- if (
    !is.null(existing_manifest) && is.data.frame(existing_manifest$spatial_frames)
  ) {
    existing_manifest$spatial_frames
  } else {
    NULL
  }
  old_spatial_digests <- if (
    !is.null(existing_manifest) &&
      is.list(existing_manifest$item_digests) &&
      is.list(existing_manifest$item_digests$spatial)
  ) {
    existing_manifest$item_digests$spatial
  } else {
    NULL
  }
  new_spatial_digests <- if (
    is.list(item_digests) && is.list(item_digests$spatial)
  ) {
    item_digests$spatial
  } else {
    NULL
  }
  old_indexes <- NULL
  if (!is.null(existing_pack) && !is.null(existing_manifest)) {
    idx_row <- existing_manifest$assets[
      as.character(existing_manifest$assets$path) == "spatial/cell_index.qs2",
      ,
      drop = FALSE
    ]
    if (nrow(idx_row) == 1L) {
      idx_file <- file.path(existing_pack, "spatial", "cell_index.qs2")
      if (
        file.exists(idx_file) &&
          identical(
            as.numeric(file.info(idx_file, extra_cols = FALSE)$size),
            as.numeric(idx_row$bytes[[1L]])
          ) &&
          identical(.viewerPackMd5(idx_file), as.character(idx_row$checksum[[1L]]))
      ) {
        old_indexes <- tryCatch(qs2::qs_read(idx_file), error = function(e) NULL)
      }
    }
  }
  for (i in seq_along(spatial_names)) {
    name <- spatial_names[[i]]
    geometry_path <- file.path(
      "spatial",
      sprintf("%03d.geometry.bin", i)
    )
    reused_asset <- NULL
    old_frame_row <- NULL
    can_reuse_item <- !is.null(existing_pack) &&
      !is.null(old_frames) &&
      !is.null(old_spatial_digests) &&
      !is.null(new_spatial_digests) &&
      identical(
        as.character(old_spatial_digests[[name]]),
        as.character(new_spatial_digests[[name]])
      )
    if (isTRUE(can_reuse_item)) {
      match_row <- which(as.character(old_frames$name) == name)
      if (length(match_row) == 1L) {
        old_path <- as.character(old_frames$geometry_path[[match_row]])
        old_asset_row <- existing_manifest$assets[
          as.character(existing_manifest$assets$path) == old_path,
          ,
          drop = FALSE
        ]
        if (nrow(old_asset_row) == 1L) {
          reused_asset <- .viewerPackStageVerifiedRenamedAsset(
            existing_pack,
            old_asset_row,
            stage,
            geometry_path
          )
          if (!is.null(reused_asset)) {
            old_frame_row <- old_frames[match_row, , drop = FALSE]
          }
        }
      }
    }
    if (
      !is.null(reused_asset) &&
        !is.null(old_frame_row) &&
        is.list(old_indexes) &&
        !is.null(old_indexes[[name]])
    ) {
      indexes[[i]] <- as.integer(old_indexes[[name]])
      assets <- rbind(assets, reused_asset)
      x_min <- suppressWarnings(as.numeric(old_frame_row$x_min[[1L]]))
      x_max <- suppressWarnings(as.numeric(old_frame_row$x_max[[1L]]))
      y_min <- suppressWarnings(as.numeric(old_frame_row$y_min[[1L]]))
      y_max <- suppressWarnings(as.numeric(old_frame_row$y_max[[1L]]))
      frames[[length(frames) + 1L]] <- data.frame(
        name = name,
        cells = as.integer(old_frame_row$cells[[1L]]),
        dimensions = as.integer(old_frame_row$dimensions[[1L]]),
        geometry_path = geometry_path,
        x_min = if (length(x_min) == 1L) x_min else NA_real_,
        x_max = if (length(x_max) == 1L) x_max else NA_real_,
        y_min = if (length(y_min) == 1L) y_min else NA_real_,
        y_max = if (length(y_max) == 1L) y_max else NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }
    data <- getter(name, hydrate_molecules = FALSE)
    coordinates <- data[["coordinates"]]
    spatial_cells <- rownames(coordinates)
    index <- match(spatial_cells, cells)
    if (
      is.null(spatial_cells) ||
        length(index) != nrow(coordinates) ||
        anyNA(index)
    ) {
      stop("Viewer Pack spatial data is not cell-aligned.", call. = FALSE)
    }
    indexes[[i]] <- as.integer(index)
    coordinate_matrix <- tryCatch(
      as.matrix(coordinates),
      error = function(error) NULL
    )
    if (
      is.null(coordinate_matrix) ||
        !is.numeric(coordinate_matrix) ||
        ncol(coordinate_matrix) != 2L
    ) {
      next
    }
    if (!is.null(reused_asset)) {
      assets <- rbind(assets, reused_asset)
      x_min <- suppressWarnings(as.numeric(old_frame_row$x_min[[1L]]))
      x_max <- suppressWarnings(as.numeric(old_frame_row$x_max[[1L]]))
      y_min <- suppressWarnings(as.numeric(old_frame_row$y_min[[1L]]))
      y_max <- suppressWarnings(as.numeric(old_frame_row$y_max[[1L]]))
      if (
        length(x_min) != 1L ||
          length(x_max) != 1L ||
          length(y_min) != 1L ||
          length(y_max) != 1L ||
          !all(is.finite(c(x_min, x_max, y_min, y_max)))
      ) {
        finite_x <- coordinate_matrix[, 1L][is.finite(coordinate_matrix[, 1L])]
        finite_y <- coordinate_matrix[, 2L][is.finite(coordinate_matrix[, 2L])]
        x_min <- if (length(finite_x)) min(finite_x) else NA_real_
        x_max <- if (length(finite_x)) max(finite_x) else NA_real_
        y_min <- if (length(finite_y)) min(finite_y) else NA_real_
        y_max <- if (length(finite_y)) max(finite_y) else NA_real_
      }
    } else {
      assets <- rbind(
        assets,
        .viewerPackWriteAsset(
          stage,
          geometry_path,
          as.numeric(t(coordinate_matrix)),
          "float32",
          dim(coordinate_matrix)
        )
      )
      finite_x <- coordinate_matrix[, 1L][is.finite(coordinate_matrix[, 1L])]
      finite_y <- coordinate_matrix[, 2L][is.finite(coordinate_matrix[, 2L])]
      x_min <- if (length(finite_x)) min(finite_x) else NA_real_
      x_max <- if (length(finite_x)) max(finite_x) else NA_real_
      y_min <- if (length(finite_y)) min(finite_y) else NA_real_
      y_max <- if (length(finite_y)) max(finite_y) else NA_real_
    }
    frames[[length(frames) + 1L]] <- data.frame(
      name = name,
      cells = nrow(coordinate_matrix),
      dimensions = ncol(coordinate_matrix),
      geometry_path = geometry_path,
      x_min = x_min,
      x_max = x_max,
      y_min = y_min,
      y_max = y_max,
      stringsAsFactors = FALSE
    )
  }
  frame_table <- if (length(frames)) {
    do.call(rbind, frames)
  } else {
    data.frame(
      name = character(),
      cells = integer(),
      dimensions = integer(),
      geometry_path = character(),
      x_min = numeric(),
      x_max = numeric(),
      y_min = numeric(),
      y_max = numeric(),
      stringsAsFactors = FALSE
    )
  }
  rownames(frame_table) <- NULL
  list(indexes = indexes, frames = frame_table, assets = assets)
}

.viewerPackHlaFirstFrame <- function(segments, object) {
  all_segments <- segments
  available <- colnames(segments)
  declared <- object$getGroups()
  filter_groups <- unique(intersect(c("sample", declared), available))
  filter_levels <- stats::setNames(
    lapply(filter_groups, function(group) {
      values <- as.character(segments[[group]])
      sort(unique(values[!is.na(values) & nzchar(values)]))
    }),
    filter_groups
  )
  initial_samples <- filter_levels[["sample"]] %||% character()
  if (length(initial_samples)) {
    initial_samples <- hla_choose_initial_samples(segments, initial_samples)
    segments <- segments[
      as.character(segments$sample) %in% initial_samples,
      ,
      drop = FALSE
    ]
  }

  technical <- object$getTechnicalInfo()
  declared_lineage <- technical$lineage_column
  lineage_col <- if (
    is.character(declared_lineage) &&
      length(declared_lineage) >= 1L &&
      declared_lineage[[1L]] %in% available
  ) {
    declared_lineage[[1L]]
  } else {
    candidates <- intersect(declared, available)
    if (length(candidates)) {
      scores <- vapply(
        candidates,
        function(column) hla_lineage_column_score(all_segments[[column]]),
        numeric(1)
      )
      if (max(scores) >= HLA_LINEAGE_MIN_SHARE) {
        best <- candidates[scores == max(scores)]
        level_counts <- vapply(
          best,
          function(column) {
            values <- as.character(all_segments[[column]])
            length(unique(values[!is.na(values) & nzchar(values)]))
          },
          integer(1)
        )
        best[[which.max(level_counts)]]
      } else {
        NULL
      }
    } else {
      NULL
    }
  }
  if (!is.null(lineage_col)) {
    segments$mhc_context <- hla_lineage_context(segments[[lineage_col]])
  }
  color_meta_cols <- intersect(declared, available)
  color_level_counts <- stats::setNames(
    vapply(
      color_meta_cols,
      function(column) {
        values <- as.character(all_segments[[column]])
        length(unique(values[!is.na(values) & nzchar(values)]))
      },
      integer(1)
    ),
    color_meta_cols
  )
  node_meta_cols <- unique(intersect(
    c("sample", lineage_col, color_meta_cols),
    available
  ))
  by_v <- identical(technical$receptor_key, "v_gene+cdr3")
  graph_raw <- hla_build_motif_graph_raw(
    segments,
    by_v = by_v,
    meta_cols = node_meta_cols,
    context_col = if ("mhc_context" %in% colnames(segments)) {
      "mhc_context"
    } else {
      NULL
    },
    context_summary = hla_context_summary
  )
  default_min_nodes <- if (!hla_motif_graph_ok(graph_raw)) {
    2L
  } else if (igraph::vcount(graph_raw) > 2000L) {
    6L
  } else if (igraph::vcount(graph_raw) > 500L) {
    4L
  } else {
    2L
  }
  graph_default <- hla_finalize_motif_graph(
    graph_raw,
    min_nodes = default_min_nodes,
    show_isolated = FALSE
  )
  graph_snapshot <- if (hla_motif_graph_ok(graph_default)) {
    edge_matrix <- igraph::as_edgelist(graph_default, names = FALSE)
    list(
      version = 1L,
      vertex_attrs = igraph::vertex_attr(graph_default),
      total_cells = igraph::graph_attr(graph_default, "total_cells"),
      degree = as.integer(igraph::degree(graph_default)),
      edges = if (nrow(edge_matrix)) {
        unname(matrix(as.integer(edge_matrix), ncol = 2L))
      } else {
        matrix(integer(), ncol = 2L)
      },
      component_sizes = as.integer(igraph::components(graph_default)$csize)
    )
  } else {
    NULL
  }

  list(
    version = 2L,
    filter_groups = filter_groups,
    filter_levels = filter_levels,
    initial_samples = initial_samples,
    available_cols = available,
    color_level_counts = color_level_counts,
    lineage_col = lineage_col,
    node_meta_cols = node_meta_cols,
    by_v = by_v,
    segments = segments,
    graph_raw = graph_raw,
    default_min_nodes = default_min_nodes,
    graph_snapshot = graph_snapshot
  )
}

.viewerPackCompactViolinData <- local({
  compact <- NULL
  function(metadata, group, metric, max_points_per_group = 4096L) {
    if (is.null(compact)) {
      scope <- new.env(parent = baseenv())
      sys.source(
        .cerebroPackageResource("viewer", "plotting_functions.R"),
        envir = scope
      )
      compact <<- scope$compactViolinData
    }
    do.call(compact, list(metadata, metric, group, max_points_per_group))
  }
})

.viewerPackGroupMetricAssets <- function(
  object,
  metadata,
  stage,
  existing_pack = NULL,
  existing_manifest = NULL,
  item_digests = NULL
) {
  groups <- intersect(object$getGroups(), colnames(metadata))
  metrics <- intersect(
    c("nUMI", "nGene", "percent_mt", "percent_ribo"),
    colnames(metadata)
  )
  old_frames <- if (
    !is.null(existing_manifest) &&
      is.data.frame(existing_manifest$group_metric_frames)
  ) {
    existing_manifest$group_metric_frames
  } else {
    NULL
  }
  old_group_digests <- if (
    !is.null(existing_manifest) &&
      is.list(existing_manifest$item_digests) &&
      is.list(existing_manifest$item_digests$groups)
  ) {
    existing_manifest$item_digests$groups
  } else {
    NULL
  }
  new_group_digests <- if (
    is.list(item_digests) && is.list(item_digests$groups)
  ) {
    item_digests$groups
  } else {
    NULL
  }
  assets <- NULL
  frames <- list()
  if (length(groups) && length(metrics)) {
    frame_index <- 0L
    for (group in groups) {
      for (metric in metrics) {
        frame_index <- frame_index + 1L
        path <- file.path("groups", sprintf("%03d.qs2", frame_index))
        pair_key <- paste0(group, "::", metric)
        reused_asset <- NULL
        reused_rows <- NA_integer_
        if (
          !is.null(existing_pack) &&
            !is.null(old_frames) &&
            !is.null(old_group_digests) &&
            !is.null(new_group_digests) &&
            identical(
              as.character(old_group_digests[[pair_key]]),
              as.character(new_group_digests[[pair_key]])
            )
        ) {
          match_row <- which(
            as.character(old_frames$group) == group &
              as.character(old_frames$metric) == metric
          )
          if (length(match_row) == 1L) {
            old_path <- as.character(old_frames$path[[match_row]])
            old_asset_row <- existing_manifest$assets[
              as.character(existing_manifest$assets$path) == old_path,
              ,
              drop = FALSE
            ]
            if (nrow(old_asset_row) == 1L) {
              reused_asset <- .viewerPackStageVerifiedRenamedAsset(
                existing_pack,
                old_asset_row,
                stage,
                path
              )
              if (!is.null(reused_asset)) {
                reused_rows <- as.integer(old_frames$rows[[match_row]])
              }
            }
          }
        }
        if (!is.null(reused_asset)) {
          assets <- rbind(assets, reused_asset)
          n_rows <- reused_rows
        } else {
          value <- .viewerPackCompactViolinData(metadata, group, metric)
          assets <- rbind(
            assets,
            .viewerPackWriteAsset(
              stage,
              path,
              value,
              "group-metric-violin",
              nrow(value)
            )
          )
          n_rows <- nrow(value)
        }
        frames[[frame_index]] <- data.frame(
          group = group,
          metric = metric,
          rows = n_rows,
          path = path,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  frame_table <- if (length(frames)) {
    do.call(rbind, frames)
  } else {
    data.frame(
      group = character(),
      metric = character(),
      rows = integer(),
      path = character(),
      stringsAsFactors = FALSE
    )
  }
  rownames(frame_table) <- NULL
  list(frames = frame_table, assets = assets)
}

.viewerPackLinkOrCopy <- function(src, dst) {
  dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(suppressWarnings(file.link(src, dst)))) {
    return(TRUE)
  }
  isTRUE(file.copy(src, dst, overwrite = TRUE))
}

.viewerPackReadExistingManifest <- function(pack) {
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
      !identical(manifest$schema_version, .viewerPackSchemaVersion) ||
      !identical(
        suppressWarnings(as.integer(manifest$format_revision)),
        .viewerPackFormatRevision
      ) ||
      !is.data.frame(manifest$assets) ||
      !all(
        c("path", "dtype", "dimensions", "bytes", "checksum") %in%
          names(manifest$assets)
      )
  ) {
    return(NULL)
  }
  manifest
}

.viewerPackStageVerifiedAssets <- function(
  existing_pack,
  rows_df,
  stage,
  verify_md5 = FALSE
) {
  if (!is.data.frame(rows_df) || !nrow(rows_df)) {
    return(NULL)
  }
  rel_paths <- as.character(rows_df$path)
  src_paths <- file.path(existing_pack, rel_paths)
  if (any(!file.exists(src_paths)) || any(dir.exists(src_paths))) {
    return(NULL)
  }
  sizes <- unname(as.numeric(file.info(src_paths, extra_cols = FALSE)$size))
  if (!identical(sizes, as.numeric(rows_df$bytes))) {
    return(NULL)
  }
  if (
    isTRUE(verify_md5) &&
      !identical(unname(tools::md5sum(src_paths)), as.character(rows_df$checksum))
  ) {
    return(NULL)
  }
  dst_paths <- file.path(stage, rel_paths)
  ok <- vapply(
    seq_along(src_paths),
    function(i) .viewerPackLinkOrCopy(src_paths[[i]], dst_paths[[i]]),
    logical(1)
  )
  if (!all(ok)) {
    return(NULL)
  }
  rows_df
}

.viewerPackStageVerifiedRenamedAsset <- function(
  existing_pack,
  rows_df,
  stage,
  new_rel_paths,
  verify_md5 = FALSE
) {
  if (
    !is.data.frame(rows_df) ||
      !nrow(rows_df) ||
      length(new_rel_paths) != nrow(rows_df)
  ) {
    return(NULL)
  }
  old_rel_paths <- as.character(rows_df$path)
  src_paths <- file.path(existing_pack, old_rel_paths)
  if (any(!file.exists(src_paths)) || any(dir.exists(src_paths))) {
    return(NULL)
  }
  sizes <- unname(as.numeric(file.info(src_paths, extra_cols = FALSE)$size))
  if (!identical(sizes, as.numeric(rows_df$bytes))) {
    return(NULL)
  }
  if (
    isTRUE(verify_md5) &&
      !identical(unname(tools::md5sum(src_paths)), as.character(rows_df$checksum))
  ) {
    return(NULL)
  }
  dst_paths <- file.path(stage, as.character(new_rel_paths))
  ok <- vapply(
    seq_along(src_paths),
    function(i) .viewerPackLinkOrCopy(src_paths[[i]], dst_paths[[i]]),
    logical(1)
  )
  if (!all(ok)) {
    return(NULL)
  }
  out <- rows_df
  out$path <- as.character(new_rel_paths)
  out
}

.viewerPackSectionDigest <- function(value) {
  paste0("xxh128-v1:", rlang::hash(value))
}

.viewerPackDigests <- function(object, cells) {
  projections <- object$availableProjections()
  proj_digests <- stats::setNames(
    lapply(projections, function(nm) {
      .viewerPackSectionDigest(tryCatch(
        object$getProjection(nm),
        error = function(e) NULL
      ))
    }),
    projections
  )
  metadata <- object$getMetaData()
  metadata_names <- setdiff(colnames(metadata), "cell_barcode")
  meta_col_digests <- stats::setNames(
    lapply(metadata_names, function(col) {
      .viewerPackSectionDigest(metadata[[col]])
    }),
    metadata_names
  )
  groups <- intersect(object$getGroups(), colnames(metadata))
  metrics <- intersect(
    c("nUMI", "nGene", "percent_mt", "percent_ribo"),
    colnames(metadata)
  )
  group_digests <- list()
  if (length(groups) && length(metrics)) {
    for (g in groups) {
      for (m in metrics) {
        group_digests[[paste0(g, "::", m)]] <- .viewerPackSectionDigest(c(
          meta_col_digests[[g]],
          meta_col_digests[[m]]
        ))
      }
    }
  }
  spatial_names <- object$availableSpatial()
  spatial_digests <- stats::setNames(
    lapply(spatial_names, function(nm) {
      .viewerPackSectionDigest(tryCatch(
        object$getSpatialData(nm, hydrate_molecules = FALSE)[["coordinates"]],
        error = function(e) NULL
      ))
    }),
    spatial_names
  )
  repertoire <- tryCatch(object$getImmuneRepertoire(), error = function(e) {
    list()
  })
  has_hla_pkgs <- requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("stringdist", quietly = TRUE)
  content_digests <- list(
    projections = .viewerPackSectionDigest(proj_digests),
    metadata = .viewerPackSectionDigest(list(
      groups = object$getGroups(),
      cell_cycle = object$getCellCycle(),
      columns = meta_col_digests
    )),
    trajectories = .viewerPackSectionDigest(object$trajectories),
    spatial = .viewerPackSectionDigest(spatial_digests),
    immune = .viewerPackSectionDigest(list(
      version = if (length(repertoire)) {
        .viewerPackImmuneEncoder()$IR_ENCODING_VERSION
      } else {
        0L
      },
      repertoire = repertoire
    )),
    hla_tcr = .viewerPackSectionDigest(list(
      enabled = has_hla_pkgs,
      groups = object$getGroups(),
      lineage = object$getTechnicalInfo()$lineage_column,
      repertoire = repertoire,
      metadata = if (length(repertoire)) meta_col_digests else NULL
    ))
  )
  item_digests <- list(
    projections = proj_digests,
    metadata = meta_col_digests,
    groups = group_digests,
    spatial = spatial_digests
  )
  list(content_digests = content_digests, item_digests = item_digests)
}

.viewerPackContentDigests <- function(object, cells) {
  .viewerPackDigests(object, cells)$content_digests
}

.viewerPackTryReuseFull <- function(
  existing_pack,
  existing_manifest,
  stage,
  object,
  cells,
  cell_order_fingerprint,
  content_digests,
  item_digests = NULL
) {
  if (
    is.null(existing_manifest) ||
      !identical(as.integer(existing_manifest$n_cells), length(cells)) ||
      !identical(
        as.character(existing_manifest$cell_order_fingerprint),
        as.character(cell_order_fingerprint)
      ) ||
      !is.list(existing_manifest$content_digests) ||
      !is.list(existing_manifest$item_digests)
  ) {
    return(NULL)
  }
  if (
    length(existing_manifest$projection_names) &&
      (!is.data.frame(existing_manifest$projection_frames) ||
        nrow(existing_manifest$projection_frames) !=
          length(existing_manifest$projection_names))
  ) {
    return(NULL)
  }
  for (nm in names(content_digests)) {
    if (
      !identical(
        as.character(existing_manifest$content_digests[[nm]]),
        as.character(content_digests[[nm]])
      )
    ) {
      return(NULL)
    }
  }
  staged_assets <- .viewerPackStageVerifiedAssets(
    existing_pack,
    existing_manifest$assets,
    stage
  )
  if (is.null(staged_assets)) {
    return(NULL)
  }
  list(
    assets = staged_assets,
    modules = existing_manifest$modules,
    capabilities = .viewerDatasetCapabilities(object),
    projection_names = existing_manifest$projection_names,
    projection_frames = existing_manifest$projection_frames,
    metadata_names = existing_manifest$metadata_names,
    group_metric_frames = existing_manifest$group_metric_frames,
    trajectory_frames = existing_manifest$trajectory_frames,
    spatial_frames = existing_manifest$spatial_frames,
    immune_receptors = existing_manifest$immune_receptors,
    immune_encoding = existing_manifest$immune_encoding,
    immune_clonal_frames = existing_manifest$immune_clonal_frames,
    hla_chains = existing_manifest$hla_chains,
    content_digests = content_digests,
    item_digests = item_digests %||% existing_manifest$item_digests,
    reused_sections = names(content_digests)
  )
}

.viewerPackBuildAssets <- function(
  object,
  stage,
  existing_pack = NULL,
  existing_manifest = NULL,
  cell_order_fingerprint = NULL,
  content_digests = NULL,
  item_digests = NULL
) {
  cells <- .viewerPackCells(object)
  can_reuse <- !is.null(existing_pack) &&
    !is.null(existing_manifest) &&
    identical(as.integer(existing_manifest$n_cells), length(cells)) &&
    identical(
      as.character(existing_manifest$cell_order_fingerprint),
      as.character(cell_order_fingerprint)
    )
  reuse_prefix <- function(prefix) {
    if (!can_reuse) {
      return(NULL)
    }
    rows <- existing_manifest$assets[
      startsWith(as.character(existing_manifest$assets$path), prefix),
      ,
      drop = FALSE
    ]
    if (!nrow(rows)) {
      return(NULL)
    }
    .viewerPackStageVerifiedAssets(existing_pack, rows, stage)
  }
  section_unchanged <- function(section) {
    can_reuse &&
      is.list(existing_manifest$content_digests) &&
      is.list(content_digests) &&
      identical(
        as.character(existing_manifest$content_digests[[section]]),
        as.character(content_digests[[section]])
      )
  }
  reused_sections <- character()
  reused_common <- reuse_prefix("common/cell_order.qs2")
  if (!is.null(reused_common)) {
    assets <- reused_common
    reused_sections <- c(reused_sections, "common")
  } else {
    assets <- .viewerPackWriteAsset(
      stage,
      "common/cell_order.qs2",
      cells,
      "utf8",
      length(cells)
    )
  }
  modules <- "common"
  projections <- object$availableProjections()
  load_projection_values <- function() {
    values <- stats::setNames(
      vector("list", length(projections)),
      projections
    )
    for (i in seq_along(projections)) {
      value <- as.matrix(object$getProjection(projections[[i]]))
      if (!is.numeric(value) || nrow(value) != length(cells)) {
        stop("Viewer Pack projection is not cell-aligned.", call. = FALSE)
      }
      ids <- rownames(value)
      if (
        is.null(ids) ||
          anyNA(ids) ||
          any(!nzchar(ids)) ||
          anyDuplicated(ids) ||
          !setequal(ids, cells)
      ) {
        stop(
          "Viewer Pack projection requires unique, matching cell identities.",
          call. = FALSE
        )
      }
      if (!identical(ids, cells)) {
        value <- value[match(cells, ids), , drop = FALSE]
      }
      values[[i]] <- value
    }
    values
  }
  projection_values <- NULL
  empty_projection_frames <- function() {
    data.frame(
      name = character(),
      cells = integer(),
      dimensions = integer(),
      path = character(),
      x_min = numeric(),
      x_max = numeric(),
      y_min = numeric(),
      y_max = numeric(),
      stringsAsFactors = FALSE
    )
  }
  projection_frames <- empty_projection_frames()
  reused_projections <- if (
    length(projections) &&
      section_unchanged("projections") &&
      identical(
        as.character(existing_manifest$projection_names),
        as.character(projections)
      ) &&
      is.data.frame(existing_manifest$projection_frames) &&
      nrow(existing_manifest$projection_frames) == length(projections)
  ) {
    reuse_prefix("projections/")
  } else {
    NULL
  }
  if (!is.null(reused_projections)) {
    modules <- c(modules, "projections")
    assets <- rbind(assets, reused_projections)
    projection_frames <- existing_manifest$projection_frames
    reused_sections <- c(reused_sections, "projections")
  } else if (length(projections)) {
    modules <- c(modules, "projections")
    old_proj_names <- if (can_reuse) {
      as.character(existing_manifest$projection_names)
    } else {
      character()
    }
    old_proj_frames <- if (
      can_reuse && is.data.frame(existing_manifest$projection_frames)
    ) {
      existing_manifest$projection_frames
    } else {
      NULL
    }
    old_proj_digests <- if (
      can_reuse &&
        is.list(existing_manifest$item_digests) &&
        is.list(existing_manifest$item_digests$projections)
    ) {
      existing_manifest$item_digests$projections
    } else {
      NULL
    }
    new_proj_digests <- if (
      is.list(item_digests) && is.list(item_digests$projections)
    ) {
      item_digests$projections
    } else {
      NULL
    }
    proj_frame_rows <- list()
    load_one_projection <- function(nm) {
      value <- as.matrix(object$getProjection(nm))
      if (!is.numeric(value) || nrow(value) != length(cells)) {
        stop("Viewer Pack projection is not cell-aligned.", call. = FALSE)
      }
      ids <- rownames(value)
      if (
        is.null(ids) ||
          anyNA(ids) ||
          any(!nzchar(ids)) ||
          anyDuplicated(ids) ||
          !setequal(ids, cells)
      ) {
        stop(
          "Viewer Pack projection requires unique, matching cell identities.",
          call. = FALSE
        )
      }
      if (!identical(ids, cells)) {
        value <- value[match(cells, ids), , drop = FALSE]
      }
      value
    }
    for (i in seq_along(projections)) {
      nm <- projections[[i]]
      new_path <- file.path("projections", sprintf("%03d.bin", i))
      reused_one <- NULL
      reused_frame <- NULL
      if (
        can_reuse &&
          !is.null(old_proj_digests) &&
          !is.null(new_proj_digests) &&
          !is.null(old_proj_frames) &&
          identical(
            as.character(old_proj_digests[[nm]]),
            as.character(new_proj_digests[[nm]])
          )
      ) {
        old_i <- match(nm, old_proj_names)
        old_frame_idx <- which(as.character(old_proj_frames$name) == nm)
        if (!is.na(old_i) && length(old_frame_idx) == 1L) {
          old_path <- file.path("projections", sprintf("%03d.bin", old_i))
          old_row <- existing_manifest$assets[
            as.character(existing_manifest$assets$path) == old_path,
            ,
            drop = FALSE
          ]
          if (nrow(old_row) == 1L) {
            reused_one <- .viewerPackStageVerifiedRenamedAsset(
              existing_pack,
              old_row,
              stage,
              new_path
            )
            if (!is.null(reused_one)) {
              reused_frame <- old_proj_frames[old_frame_idx, , drop = FALSE]
              reused_frame$path <- new_path
            }
          }
        }
      }
      if (!is.null(reused_one) && !is.null(reused_frame)) {
        assets <- rbind(assets, reused_one)
        proj_frame_rows[[i]] <- reused_frame
      } else {
        value <- load_one_projection(nm)
        assets <- rbind(
          assets,
          .viewerPackWriteAsset(
            stage,
            new_path,
            as.numeric(t(value)),
            "float32",
            dim(value)
          )
        )
        finite_x <- value[, 1L][is.finite(value[, 1L])]
        finite_y <- if (ncol(value) >= 2L) {
          value[, 2L][is.finite(value[, 2L])]
        } else {
          numeric()
        }
        proj_frame_rows[[i]] <- data.frame(
          name = nm,
          cells = nrow(value),
          dimensions = ncol(value),
          path = new_path,
          x_min = if (length(finite_x)) min(finite_x) else NA_real_,
          x_max = if (length(finite_x)) max(finite_x) else NA_real_,
          y_min = if (length(finite_y)) min(finite_y) else NA_real_,
          y_max = if (length(finite_y)) max(finite_y) else NA_real_,
          stringsAsFactors = FALSE
        )
      }
    }
    projection_frames <- do.call(rbind, proj_frame_rows)
    rownames(projection_frames) <- NULL
  }
  metadata <- object$getMetaData()
  metadata_names <- setdiff(colnames(metadata), "cell_barcode")
  reused_meta <- NULL
  reused_groups <- NULL
  meta_reused <- FALSE
  if (
    section_unchanged("metadata") &&
      identical(
        as.character(existing_manifest$metadata_names),
        as.character(metadata_names)
      )
  ) {
    has_meta_files <- length(metadata_names) > 0L &&
      any(startsWith(as.character(existing_manifest$assets$path), "metadata/"))
    reused_meta <- if (has_meta_files) reuse_prefix("metadata/") else NULL
    has_group_frames <- is.data.frame(existing_manifest$group_metric_frames) &&
      nrow(existing_manifest$group_metric_frames) > 0L
    reused_groups <- if (has_group_frames) reuse_prefix("groups/") else NULL
    if (
      (!has_meta_files || !is.null(reused_meta)) &&
        (!has_group_frames || !is.null(reused_groups))
    ) {
      meta_reused <- TRUE
    }
  }
  if (isTRUE(meta_reused)) {
    if (length(metadata_names)) {
      modules <- c(modules, "metadata")
      if (!is.null(reused_meta)) {
        assets <- rbind(assets, reused_meta)
      }
    }
    group_metrics <- list(
      frames = existing_manifest$group_metric_frames,
      assets = reused_groups
    )
    if (!is.null(reused_groups)) {
      modules <- c(modules, "groups")
      assets <- rbind(assets, reused_groups)
    }
    reused_sections <- c(reused_sections, "metadata")
  } else {
    if (length(metadata_names)) {
      modules <- c(modules, "metadata")
      old_meta_names <- if (can_reuse) {
        as.character(existing_manifest$metadata_names)
      } else {
        character()
      }
      old_meta_digests <- if (
        can_reuse &&
          is.list(existing_manifest$item_digests) &&
          is.list(existing_manifest$item_digests$metadata)
      ) {
        existing_manifest$item_digests$metadata
      } else {
        NULL
      }
      new_meta_digests <- if (
        is.list(item_digests) && is.list(item_digests$metadata)
      ) {
        item_digests$metadata
      } else {
        NULL
      }
      for (i in seq_along(metadata_names)) {
        col_name <- metadata_names[[i]]
        value <- metadata[[col_name]]
        prefix <- file.path("metadata", sprintf("%03d", i))
        reused_col <- NULL
        if (
          can_reuse &&
            !is.null(old_meta_digests) &&
            !is.null(new_meta_digests) &&
            identical(
              as.character(old_meta_digests[[col_name]]),
              as.character(new_meta_digests[[col_name]])
            )
        ) {
          old_i <- match(col_name, old_meta_names)
          if (!is.na(old_i)) {
            old_prefix <- paste0(file.path("metadata", sprintf("%03d", old_i)), ".")
            old_rows <- existing_manifest$assets[
              startsWith(as.character(existing_manifest$assets$path), old_prefix),
              ,
              drop = FALSE
            ]
            if (nrow(old_rows) > 0L) {
              new_paths <- paste0(
                prefix,
                ".",
                substring(as.character(old_rows$path), nchar(old_prefix) + 1L)
              )
              reused_col <- .viewerPackStageVerifiedRenamedAsset(
                existing_pack,
                old_rows,
                stage,
                new_paths
              )
            }
          }
        }
        if (!is.null(reused_col)) {
          assets <- rbind(assets, reused_col)
          next
        }
        if (is.factor(value) || is.character(value) || is.logical(value)) {
          dictionary <- if (is.factor(value)) {
            levels(value)
          } else {
            unique(as.character(value[!is.na(value)]))
          }
          codes <- match(as.character(value), dictionary)
          codes[is.na(codes)] <- 0L
          dtype <- .viewerPackCodeDtype(length(dictionary) + 1L)
          assets <- rbind(
            assets,
            .viewerPackWriteAsset(
              stage,
              paste0(prefix, ".codes.bin"),
              codes,
              dtype,
              length(codes)
            ),
            .viewerPackWriteAsset(
              stage,
              paste0(prefix, ".dictionary.json"),
              as.character(dictionary),
              "utf8",
              length(dictionary)
            )
          )
        } else if (is.numeric(value) || is.integer(value)) {
          assets <- rbind(
            assets,
            .viewerPackWriteAsset(
              stage,
              paste0(prefix, ".bin"),
              value,
              "float32",
              length(value)
            )
          )
        }
      }
    }
    group_metrics <- .viewerPackGroupMetricAssets(
      object,
      metadata,
      stage,
      existing_pack = if (can_reuse) existing_pack else NULL,
      existing_manifest = if (can_reuse) existing_manifest else NULL,
      item_digests = item_digests
    )
    if (nrow(group_metrics$frames)) {
      modules <- c(modules, "groups")
      assets <- rbind(assets, group_metrics$assets)
    }
  }
  reused_traj <- if (
    section_unchanged("trajectories") && section_unchanged("projections")
  ) {
    reuse_prefix("trajectory/")
  } else {
    NULL
  }
  if (!is.null(reused_traj)) {
    modules <- c(modules, "trajectory")
    assets <- rbind(assets, reused_traj)
    trajectory <- list(
      indexes = NULL,
      frames = existing_manifest$trajectory_frames,
      assets = reused_traj
    )
    reused_sections <- c(reused_sections, "trajectories")
  } else {
    if (is.null(projection_values)) {
      projection_values <- if (length(projections)) {
        load_projection_values()
      } else {
        list()
      }
    }
    trajectory <- .viewerPackTrajectoryFrames(
      object,
      cells,
      stage,
      projection_values
    )
    if (length(trajectory$indexes)) {
      modules <- c(modules, "trajectory")
      assets <- rbind(
        assets,
        .viewerPackWriteAsset(
          stage,
          file.path("trajectory", "cell_index.qs2"),
          trajectory$indexes,
          "canonical-cell-index"
        ),
        trajectory$assets
      )
    }
  }
  reused_spatial <- if (section_unchanged("spatial")) {
    reuse_prefix("spatial/")
  } else {
    NULL
  }
  if (!is.null(reused_spatial)) {
    modules <- c(modules, "spatial")
    assets <- rbind(assets, reused_spatial)
    spatial <- list(
      indexes = NULL,
      frames = existing_manifest$spatial_frames,
      assets = reused_spatial
    )
    reused_sections <- c(reused_sections, "spatial")
  } else {
    spatial <- .viewerPackSpatialAssets(
      object,
      cells,
      stage,
      existing_pack = if (can_reuse) existing_pack else NULL,
      existing_manifest = if (can_reuse) existing_manifest else NULL,
      item_digests = item_digests
    )
    if (length(spatial$indexes)) {
      modules <- c(modules, "spatial")
      assets <- rbind(
        assets,
        .viewerPackWriteAsset(
          stage,
          file.path("spatial", "cell_index.qs2"),
          spatial$indexes,
          "canonical-cell-index"
        ),
        spatial$assets
      )
    }
  }
  repertoire <- tryCatch(object$getImmuneRepertoire(), error = function(error) {
    list()
  })
  receptors <- character()
  clonal_frames <- .viewerPackImmuneClonalFrames(NULL, 0L, stage)$frames
  reused_immune <- if (section_unchanged("immune")) {
    reuse_prefix("immune/")
  } else {
    NULL
  }
  if (!is.null(reused_immune)) {
    receptors <- as.character(existing_manifest$immune_receptors)
    clonal_frames <- existing_manifest$immune_clonal_frames
    modules <- c(modules, "immune")
    assets <- rbind(assets, reused_immune)
    reused_sections <- c(reused_sections, "immune")
  } else {
    if (length(repertoire)) {
      references <- unlist(
        lapply(repertoire, function(frame) {
          if ("CTstrict" %in% colnames(frame)) {
            as.character(frame$CTstrict)
          } else {
            as.character(frame$CTgene)
          }
        }),
        use.names = FALSE
      )
      if (any(grepl("TR[ABGD]", references))) {
        receptors <- c(receptors, "TCR")
      }
      if (any(grepl("IG[HKL]", references))) receptors <- c(receptors, "BCR")
    }
    if (length(receptors)) {
      modules <- c(modules, "immune")
      for (receptor in receptors) {
        index <- .viewerPackImmuneIndex(repertoire, cells, receptor)
        if (!is.null(index)) {
          assets <- rbind(
            assets,
            .viewerPackWriteAsset(
              stage,
              file.path("immune", paste0(receptor, ".qs2")),
              index,
              "sparse-cell-index",
              length(index$cell_index)
            )
          )
        }
      }
      ## Every Immune repertoire tab reads these instead of the repertoire. The
      ## clone names are only read to label clones (Compare), one clone column
      ## at a time.
      encoded <- .viewerPackImmuneEncoder()$ir_encode_repertoire(
        repertoire,
        cells
      )
      clonal <- .viewerPackImmuneClonalFrames(encoded, length(cells), stage)
      clonal_frames <- clonal$frames
      assets <- rbind(assets, clonal$assets)
      clone_names <- encoded$names
      encoded$names <- NULL
      encoded$name_blocks <- list(size = 4096L, offsets = list())
      for (column in names(clone_names)) {
        written <- .viewerPackWriteNameBlocks(
          stage,
          file.path("immune", "names", paste0(column, ".bin")),
          clone_names[[column]],
          encoded$name_blocks$size
        )
        assets <- rbind(assets, written$asset)
        encoded$name_blocks$offsets[[column]] <- written$offsets
      }
      assets <- rbind(
        assets,
        .viewerPackWriteAsset(
          stage,
          file.path("immune", "encoded.qs2"),
          encoded,
          "immune-encoded",
          length(encoded$sample)
        )
      )
    }
  }
  ## The HLA motif network needs igraph and stringdist (Suggests). Without
  ## them the pack has no HLA assets and the Viewer page asks to install them.
  reused_hla <- if (section_unchanged("hla_tcr")) {
    reuse_prefix("hla_tcr/")
  } else {
    NULL
  }
  if (!is.null(reused_hla)) {
    chains <- as.character(existing_manifest$hla_chains)
    modules <- c(modules, "hla_tcr")
    assets <- rbind(assets, reused_hla)
    reused_sections <- c(reused_sections, "hla_tcr")
  } else {
    chains <- if (
      requireNamespace("igraph", quietly = TRUE) &&
        requireNamespace("stringdist", quietly = TRUE)
    ) {
      intersect(hla_detect_chains(repertoire), c("TRA", "TRB"))
    } else {
      character()
    }
    if (length(chains)) {
      modules <- c(modules, "hla_tcr")
      annotated <- hla_annotate_ir_metadata(repertoire, metadata)
      for (chain in chains) {
        value <- hla_parse_ir_segments(annotated, chain)
        if (!is.null(value) && nrow(value)) {
          ## Canonical rows, so the Viewer reads a motif selection's cells by
          ## position rather than matching every cell name.
          value$cell_index <- match(as.character(value$barcode), cells)
          ## The scRepertoire strings the segments were parsed from: the HLA
          ## page never reads them, and they were most of the asset (Ren TRB
          ## 17.9 MB -> 3.9 MB).
          value <- value[setdiff(
            names(value),
            c("CTgene", "CTnt", "CTaa", "CTstrict", "clone_vjc")
          )]
          ## The first frame's segments in a file of their own (version 3): the
          ## first view draws from its graph, so the Viewer reads and verifies
          ## them only when they are needed (Ren TRB: about 18 MB of the file).
          first <- .viewerPackHlaFirstFrame(value, object)
          first_segments <- first$segments
          first$segments <- NULL
          first$version <- 3L
          assets <- rbind(
            assets,
            .viewerPackWriteAsset(
              stage,
              file.path("hla_tcr", paste0(chain, ".qs2")),
              value,
              "table",
              dim(value)
            ),
            .viewerPackWriteAsset(
              stage,
              file.path("hla_tcr", paste0(chain, ".first.v3.qs2")),
              first,
              "hla-first-frame"
            ),
            .viewerPackWriteAsset(
              stage,
              file.path("hla_tcr", paste0(chain, ".first.segments.qs2")),
              first_segments,
              "table",
              dim(first_segments)
            )
          )
        }
      }
    }
  }
  list(
    assets = assets,
    modules = unique(modules),
    capabilities = .viewerDatasetCapabilities(object),
    projection_names = projections,
    projection_frames = projection_frames,
    metadata_names = metadata_names,
    group_metric_frames = group_metrics$frames,
    trajectory_frames = trajectory$frames,
    spatial_frames = spatial$frames,
    immune_receptors = receptors,
    immune_encoding = if (length(receptors)) {
      .viewerPackImmuneEncoder()$IR_ENCODING_VERSION
    } else {
      0L
    },
    immune_clonal_frames = clonal_frames,
    hla_chains = chains,
    content_digests = content_digests,
    item_digests = item_digests,
    reused_sections = unique(reused_sections)
  )
}

.viewerPackInvalid <- function(reason) list(valid = FALSE, reason = reason)

.viewerPackReadCellOrder <- function(pack, manifest) {
  path <- "common/cell_order.qs2"
  if (!is.data.frame(manifest$assets) || !path %in% manifest$assets$path) {
    return(NULL)
  }
  tryCatch(
    qs2::qs_read(file.path(pack, path)),
    error = function(error) NULL
  )
}

.viewerPackSameCellOrder <- function(pack, manifest, cells) {
  stored <- .viewerPackReadCellOrder(pack, manifest)
  is.character(stored) && identical(stored, cells)
}

## A thin CRB records the hash of its H5 file's cell names, so a pack whose
## cell-order asset hashes to it was built for this CRB's cells, without
## reading the H5 file or hydrating the object a second time.
.viewerPackMatchesThinCrb <- function(file, pack, manifest, payload) {
  schema <- tryCatch(
    .crbContract()$.readRuntimeCrbSchema(payload, file),
    error = function(error) NULL
  )
  if (is.null(schema)) {
    return(FALSE)
  }
  stored <- .viewerPackReadCellOrder(pack, manifest)
  is.character(stored) &&
    identical(length(stored), as.integer(manifest$n_cells)) &&
    identical(length(stored), as.integer(schema$n_cells)) &&
    .crbContract()$.runtimeCellNamesMatch(stored, schema$cell_names_hash)
}

#' Validate a Viewer Pack
#' @param file Path to the canonical CRB.
#' @param pack Optional Viewer Pack directory.
#' @param full Verify every asset size and checksum.
#' @return A validation result.
#' @examples
#' dir <- tempfile("cerebro-")
#' dir.create(dir)
#' file.copy(system.file("extdata/examples/example.crb", package = "CerebroNexus"), dir)
#' crb <- file.path(dir, "example.crb")
#' buildViewerPack(crb, viewer_binary = "always")
#' validateViewerPack(crb)$valid
#' unlink(dir, recursive = TRUE)
#' @export
validateViewerPack <- function(
  file,
  pack = .viewerPackPath(file),
  full = TRUE,
  allow_content_match = FALSE
) {
  .validateViewerPack(
    file,
    pack,
    full,
    allow_content_match = allow_content_match
  )
}

## `payload` is the CRB's serialized object when the caller has read it
## already (or has just written it): the CRB is then not read a second time, and
## a thin CRB is matched through its recorded cell-name hash. `crb_md5` is the
## CRB's MD5 when the caller has just computed it from the file.
.validateViewerPack <- function(
  file,
  pack,
  full,
  payload = NULL,
  crb_md5 = NULL,
  allow_content_match = FALSE
) {
  if (!file.exists(file) || dir.exists(file)) {
    return(.viewerPackInvalid("CRB is missing"))
  }
  manifest_file <- file.path(pack, "manifest.json")
  if (!dir.exists(pack) || !file.exists(manifest_file)) {
    return(.viewerPackInvalid("Viewer Pack manifest is missing"))
  }
  manifest <- tryCatch(
    jsonlite::read_json(manifest_file, simplifyVector = TRUE),
    error = function(error) NULL
  )
  if (
    !is.list(manifest) ||
      !identical(manifest$schema_version, .viewerPackSchemaVersion)
  ) {
    return(.viewerPackInvalid("Viewer Pack schema is incompatible"))
  }
  md5_matches <- identical(
    manifest$dataset_fingerprint,
    paste0("md5-crb-v1:", crb_md5 %||% .viewerPackMd5(file))
  )
  if (
    !md5_matches &&
      (!isTRUE(allow_content_match) || !is.list(manifest$content_digests))
  ) {
    return(.viewerPackInvalid("Viewer Pack dataset fingerprint mismatch"))
  }
  match_mode <- if (md5_matches) "crb_md5" else "content_digest"
  eligible_domains <- NULL
  thin_match <- md5_matches &&
    isTRUE(full) &&
    !is.null(payload) &&
    .viewerPackMatchesThinCrb(file, pack, manifest, payload)
  object <- NULL
  cells <- NULL
  if (!thin_match) {
    object <- tryCatch(
      if (is.null(payload)) {
        readCerebro(file)
      } else {
        .hydrateCerebroPayload(
          .assertCurrentCerebroPayload(payload, file),
          file
        )
      },
      error = function(error) NULL
    )
    if (is.null(object)) {
      return(.viewerPackInvalid("CRB could not be read"))
    }
    cells <- tryCatch(.viewerPackCells(object), error = function(error) NULL)
    if (
      is.null(cells) || !identical(as.integer(manifest$n_cells), length(cells))
    ) {
      return(.viewerPackInvalid("Viewer Pack cell count mismatch"))
    }
    ## A full check compares the CRB's cells with the pack's own cell-order
    ## asset, whose checksum is verified below; hashing 1.3 M names again takes
    ## longer than reading and comparing them.
    same_order <- isTRUE(full) &&
      .viewerPackSameCellOrder(pack, manifest, cells)
    if (
      !same_order &&
        !identical(
          manifest$cell_order_fingerprint,
          .viewerPackCellOrderFingerprint(cells)
        )
    ) {
      return(.viewerPackInvalid("Viewer Pack cell-order fingerprint mismatch"))
    }
  }
  if (!md5_matches) {
    current_digests <- .viewerPackDigests(object, cells)$content_digests
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
    if (!any(domain_matches)) {
      return(.viewerPackInvalid("Viewer Pack content digests mismatch"))
    }
    match_mode <- if (all(domain_matches)) "content_digest" else "partial_domain"
  }
  assets <- manifest$assets
  if (
    !is.data.frame(assets) ||
      !all(c("path", "bytes", "checksum") %in% names(assets))
  ) {
    return(.viewerPackInvalid("Viewer Pack asset manifest is invalid"))
  }
  paths <- file.path(pack, assets$path)
  if (any(!file.exists(paths)) || any(dir.exists(paths))) {
    return(.viewerPackInvalid("Viewer Pack asset is missing"))
  }
  if (
    isTRUE(full) &&
      !identical(
        unname(as.numeric(file.info(paths)$size)),
        as.numeric(assets$bytes)
      )
  ) {
    return(.viewerPackInvalid("Viewer Pack asset size mismatch"))
  }
  if (
    isTRUE(full) &&
      !identical(unname(tools::md5sum(paths)), as.character(assets$checksum))
  ) {
    return(.viewerPackInvalid("Viewer Pack asset checksum mismatch"))
  }
  list(
    valid = TRUE,
    reason = NULL,
    manifest = manifest,
    path = pack,
    match_mode = match_mode,
    eligible_domains = eligible_domains
  )
}

#' Read a valid Viewer Pack descriptor
#' @inheritParams validateViewerPack
#' @return A descriptor or `NULL`.
#' @examples
#' dir <- tempfile("cerebro-")
#' dir.create(dir)
#' file.copy(system.file("extdata/examples/example.crb", package = "CerebroNexus"), dir)
#' crb <- file.path(dir, "example.crb")
#' buildViewerPack(crb, viewer_binary = "always")
#' pack <- readViewerPack(crb)
#' names(pack)
#' unlink(dir, recursive = TRUE)
#' @export
readViewerPack <- function(file, pack = .viewerPackPath(file)) {
  result <- validateViewerPack(file, pack, full = TRUE)
  if (isTRUE(result$valid)) result else NULL
}

#' Build a derived Viewer Pack for an existing CRB
#' @param file Existing canonical CRB path.
#' @param viewer_binary One of `"auto"`, `"always"`, or `"never"`.
#' @param viewer_binary_threshold Dataset-level threshold used by `"auto"`.
#' @param overwrite Replace an existing valid or invalid pack.
#' @return The pack path invisibly, or `NULL` when disabled.
#' @examples
#' dir <- tempfile("cerebro-")
#' dir.create(dir)
#' file.copy(system.file("extdata/examples/example.crb", package = "CerebroNexus"), dir)
#' crb <- file.path(dir, "example.crb")
#' buildViewerPack(crb, viewer_binary = "always")
#' validateViewerPack(crb)$valid
#' unlink(dir, recursive = TRUE)
#' @export
buildViewerPack <- function(
  file,
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 100000L,
  overwrite = FALSE
) {
  options <- .viewerPackOptions(viewer_binary, viewer_binary_threshold)
  .buildViewerPack(file, options, overwrite)
}

## `payload` is the serialized object of the CRB at `file` when the caller has
## just written it. Reading a CRB with an embedded matrix back takes about as
## long as writing it, and the pack needs the object twice (to build, and to
## check what it built), so a writer hands its payload over and the CRB is not
## read at all; otherwise it is read once.
.buildViewerPack <- function(file, options, overwrite = FALSE, payload = NULL) {
  file <- normalizePath(file, mustWork = TRUE)
  payload <- if (is.null(payload)) {
    .readCurrentCerebroPayload(file)
  } else {
    .assertCurrentCerebroPayload(payload, file)
  }
  object <- .hydrateCerebroPayload(payload, file)
  cells <- .viewerPackCells(object)
  enabled <- .viewerPackEnabled(options$mode, options$threshold, length(cells))
  if (!enabled) {
    return(NULL)
  }
  pack <- .viewerPackPath(file)
  if (dir.exists(pack) && !isTRUE(overwrite)) {
    stop(
      "Viewer Pack already exists; use `overwrite = TRUE` to rebuild it.",
      call. = FALSE
    )
  }
  stage <- tempfile(paste0(".", basename(pack), "-"), dirname(pack))
  if (!dir.create(stage, showWarnings = FALSE)) {
    stop("Could not create the Viewer Pack staging directory.", call. = FALSE)
  }
  published <- FALSE
  backup <- NULL
  on.exit(
    {
      if (dir.exists(stage)) {
        unlink(stage, recursive = TRUE, force = TRUE)
      }
      if (
        !published &&
          !is.null(backup) &&
          dir.exists(backup) &&
          !dir.exists(pack)
      ) {
        file.rename(backup, pack)
      }
    },
    add = TRUE
  )
  cell_order_fingerprint <- .viewerPackCellOrderFingerprint(cells)
  digests <- .viewerPackDigests(object, cells)
  content_digests <- digests$content_digests
  item_digests <- digests$item_digests
  existing_manifest <- if (dir.exists(pack) && isTRUE(overwrite)) {
    .viewerPackReadExistingManifest(pack)
  } else {
    NULL
  }
  built <- .viewerPackTryReuseFull(
    pack,
    existing_manifest,
    stage,
    object,
    cells,
    cell_order_fingerprint,
    content_digests,
    item_digests = item_digests
  )
  if (is.null(built)) {
    built <- .viewerPackBuildAssets(
      object,
      stage,
      existing_pack = if (!is.null(existing_manifest)) pack else NULL,
      existing_manifest = existing_manifest,
      cell_order_fingerprint = cell_order_fingerprint,
      content_digests = content_digests,
      item_digests = item_digests
    )
  }
  ## The CRB is hashed once (0.7 s for 380 MB): consult & seed the Viewer's
  ## sample-verified crb-md5 disk cache so the first Viewer open avoids a
  ## redundant cold MD5 pass.
  crb_md5 <- .viewerPackMd5(file, use_crb_cache = TRUE)
  manifest <- list(
    schema_version = .viewerPackSchemaVersion,
    format_revision = .viewerPackFormatRevision,
    dataset_fingerprint = paste0("md5-crb-v1:", crb_md5),
    cell_order_fingerprint = cell_order_fingerprint,
    n_cells = length(cells),
    modules = built$modules,
    capabilities = built$capabilities,
    projection_names = built$projection_names,
    projection_frames = built$projection_frames,
    metadata_names = built$metadata_names,
    group_metric_frames = built$group_metric_frames,
    trajectory_frames = built$trajectory_frames,
    spatial_frames = built$spatial_frames,
    immune_receptors = built$immune_receptors,
    immune_encoding = built$immune_encoding,
    immune_clonal_frames = built$immune_clonal_frames,
    hla_chains = built$hla_chains,
    content_digests = built$content_digests,
    item_digests = built$item_digests,
    assets = built$assets
  )
  jsonlite::write_json(
    manifest,
    file.path(stage, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    dataframe = "rows"
  )
  check <- .validateViewerPack(
    file,
    stage,
    full = TRUE,
    payload = payload,
    crb_md5 = crb_md5
  )
  if (!isTRUE(check$valid) && !is.null(existing_manifest)) {
    ## An existing pack file may have been corrupted with identical byte size:
    ## fall back to a clean full build before failing.
    unlink(stage, recursive = TRUE, force = TRUE)
    dir.create(stage, showWarnings = FALSE)
    built <- .viewerPackBuildAssets(
      object,
      stage,
      existing_pack = NULL,
      existing_manifest = NULL,
      cell_order_fingerprint = cell_order_fingerprint,
      content_digests = content_digests,
      item_digests = item_digests
    )
    manifest <- list(
      schema_version = .viewerPackSchemaVersion,
      format_revision = .viewerPackFormatRevision,
      dataset_fingerprint = paste0("md5-crb-v1:", crb_md5),
      cell_order_fingerprint = cell_order_fingerprint,
      n_cells = length(cells),
      modules = built$modules,
      capabilities = built$capabilities,
      projection_names = built$projection_names,
      projection_frames = built$projection_frames,
      metadata_names = built$metadata_names,
      group_metric_frames = built$group_metric_frames,
      trajectory_frames = built$trajectory_frames,
      spatial_frames = built$spatial_frames,
      immune_receptors = built$immune_receptors,
      immune_encoding = built$immune_encoding,
      immune_clonal_frames = built$immune_clonal_frames,
      hla_chains = built$hla_chains,
      content_digests = built$content_digests,
      item_digests = built$item_digests,
      assets = built$assets
    )
    jsonlite::write_json(
      manifest,
      file.path(stage, "manifest.json"),
      auto_unbox = TRUE,
      pretty = TRUE,
      dataframe = "rows"
    )
    check <- .validateViewerPack(
      file,
      stage,
      full = TRUE,
      payload = payload,
      crb_md5 = crb_md5
    )
  }
  if (!isTRUE(check$valid)) {
    stop("Built Viewer Pack failed validation: ", check$reason, call. = FALSE)
  }
  if (dir.exists(pack)) {
    backup <- tempfile(paste0(".", basename(pack), "-backup-"), dirname(pack))
    if (!file.rename(pack, backup)) {
      stop("Could not preserve the existing Viewer Pack.", call. = FALSE)
    }
  }
  if (!file.rename(stage, pack)) {
    stop("Could not publish the Viewer Pack.", call. = FALSE)
  }
  published <- TRUE
  if (!is.null(backup) && dir.exists(backup)) {
    unlink(backup, recursive = TRUE, force = TRUE)
  }
  invisible(pack)
}
