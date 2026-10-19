.viewerPackSchemaVersion <- 1L

.viewerDatasetCapabilities <- function(object) {
  available <- function(value) {
    isTRUE(tryCatch(length(force(value)) > 0L, error = function(error) FALSE))
  }
  trajectory_methods <- tryCatch(
    object$getMethodsForTrajectories(),
    error = function(error) character()
  )
  list(
    marker_genes = available(object$getMethodsForMarkerGenes()),
    most_expressed_genes = available(object$getGroupsWithMostExpressedGenes()),
    enriched_pathways = available(object$getMethodsForEnrichedPathways()),
    extra_material = available(object$getExtraMaterialCategories()),
    trajectory = any(
      trajectory_methods %in%
        c(
          "monocle2",
          "marker_guided",
          "illustrative"
        )
    ),
    spatial = available(object$availableSpatial()),
    trekker = available(object$getTrekker())
  )
}

.viewerPackOptions <- function(
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 500000L
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

.viewerPackMd5 <- function(path) {
  value <- unname(tools::md5sum(path))
  if (length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop("Could not checksum Viewer Pack input.", call. = FALSE)
  }
  value
}

.viewerPackCellOrderFingerprint <- function(cells) {
  cells <- enc2utf8(as.character(cells))
  if (anyNA(cells) || any(!nzchar(cells)) || anyDuplicated(cells)) {
    stop("Viewer Pack cells must be unique, non-empty strings.", call. = FALSE)
  }
  path <- tempfile("viewer-pack-cell-order-")
  on.exit(unlink(path), add = TRUE)
  writeBin(
    charToRaw(paste0(nchar(cells, type = "bytes"), ":", cells, collapse = "")),
    path
  )
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
    qs2::qs_save(value, target)
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

.viewerPackImmuneAbundance <- function(repertoire, drop_empty = TRUE) {
  columns <- intersect(
    c("CTgene", "CTnt", "CTaa", "CTstrict"),
    unique(unlist(lapply(repertoire, colnames)))
  )
  stats::setNames(
    lapply(columns, function(column) {
      samples <- names(repertoire)
      if (is.null(samples)) {
        samples <- as.character(seq_along(repertoire))
      }
      rows <- Map(
        function(frame, sample) {
          if (is.null(frame) || !(column %in% colnames(frame))) {
            return(NULL)
          }
          clones <- as.character(frame[[column]])
          clones <- clones[!is.na(clones) & (!drop_empty | nzchar(clones))]
          if (!length(clones)) {
            return(NULL)
          }
          abundance <- tabulate(match(clones, unique(clones)))
          bins <- table(abundance)
          data.frame(
            sample = sample,
            abundance = as.integer(names(bins)),
            n_clones = as.integer(bins),
            stringsAsFactors = FALSE
          )
        },
        repertoire,
        samples
      )
      rows <- rows[!vapply(rows, is.null, logical(1))]
      out <- if (length(rows)) {
        do.call(rbind, rows)
      } else {
        data.frame(
          sample = character(),
          abundance = integer(),
          n_clones = integer()
        )
      }
      rownames(out) <- NULL
      out
    }),
    columns
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
      stringsAsFactors = FALSE
    )
  }
  rownames(frame_table) <- NULL
  list(indexes = indexes, frames = frame_table, assets = assets)
}

.viewerPackSpatialAssets <- function(object, cells, stage) {
  spatial_names <- object$availableSpatial()
  getter <- object$getSpatialData
  indexes <- stats::setNames(
    vector("list", length(spatial_names)),
    spatial_names
  )
  frames <- list()
  assets <- NULL
  for (i in seq_along(spatial_names)) {
    name <- spatial_names[[i]]
    data <- if ("hydrate_molecules" %in% names(formals(getter))) {
      getter(name, hydrate_molecules = FALSE)
    } else {
      getter(name)
    }
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
    geometry_path <- file.path(
      "spatial",
      sprintf("%03d.geometry.bin", i)
    )
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
    frames[[length(frames) + 1L]] <- data.frame(
      name = name,
      cells = nrow(coordinate_matrix),
      dimensions = ncol(coordinate_matrix),
      geometry_path = geometry_path,
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
        system.file(
          "viewer",
          "plotting_functions.R",
          package = "CerebroNexus",
          mustWork = TRUE
        ),
        envir = scope
      )
      compact <<- scope$compactViolinData
    }
    do.call(compact, list(metadata, metric, group, max_points_per_group))
  }
})

.viewerPackGroupMetricAssets <- function(object, metadata, stage) {
  groups <- intersect(object$getGroups(), colnames(metadata))
  metrics <- intersect(
    c("nUMI", "nGene", "percent_mt", "percent_ribo"),
    colnames(metadata)
  )
  assets <- NULL
  frames <- list()
  if (length(groups) && length(metrics)) {
    frame_index <- 0L
    for (group in groups) {
      for (metric in metrics) {
        frame_index <- frame_index + 1L
        path <- file.path("groups", sprintf("%03d.qs2", frame_index))
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
        frames[[frame_index]] <- data.frame(
          group = group,
          metric = metric,
          rows = nrow(value),
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

.viewerPackBuildAssets <- function(object, stage) {
  cells <- .viewerPackCells(object)
  assets <- .viewerPackWriteAsset(
    stage,
    "common/cell_order.qs2",
    cells,
    "utf8",
    length(cells)
  )
  modules <- "common"
  projections <- object$availableProjections()
  projection_values <- stats::setNames(
    vector("list", length(projections)),
    projections
  )
  if (length(projections)) {
    modules <- c(modules, "projections")
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
      projection_values[[i]] <- value
      assets <- rbind(
        assets,
        .viewerPackWriteAsset(
          stage,
          file.path("projections", sprintf("%03d.bin", i)),
          as.numeric(t(value)),
          "float32",
          dim(value)
        )
      )
    }
  }
  metadata <- object$getMetaData()
  metadata_names <- setdiff(colnames(metadata), "cell_barcode")
  if (length(metadata_names)) {
    modules <- c(modules, "metadata")
    for (i in seq_along(metadata_names)) {
      value <- metadata[[metadata_names[[i]]]]
      prefix <- file.path("metadata", sprintf("%03d", i))
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
  group_metrics <- .viewerPackGroupMetricAssets(object, metadata, stage)
  if (nrow(group_metrics$frames)) {
    modules <- c(modules, "groups")
    assets <- rbind(assets, group_metrics$assets)
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
  spatial <- .viewerPackSpatialAssets(object, cells, stage)
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
  repertoire <- tryCatch(object$getImmuneRepertoire(), error = function(error) {
    list()
  })
  receptors <- character()
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
    abundance <- .viewerPackImmuneAbundance(repertoire)
    # Homeostasis counts empty strings as clone identities, unlike Abundance.
    # Store separate exact bins; older packs simply use the raw-data fallback.
    homeostasis <- .viewerPackImmuneAbundance(repertoire, drop_empty = FALSE)
    assets <- rbind(
      assets,
      .viewerPackWriteAsset(
        stage,
        file.path("immune", "abundance.qs2"),
        abundance,
        "table-list",
        sum(vapply(abundance, nrow, integer(1)))
      )
    )
    assets <- rbind(
      assets,
      .viewerPackWriteAsset(
        stage,
        file.path("immune", "homeostasis.qs2"),
        homeostasis,
        "table-list",
        sum(vapply(homeostasis, nrow, integer(1)))
      )
    )
  }
  chains <- intersect(hla_detect_chains(repertoire), c("TRA", "TRB"))
  if (length(chains)) {
    modules <- c(modules, "hla_tcr")
    annotated <- hla_annotate_ir_metadata(repertoire, metadata)
    for (chain in chains) {
      value <- hla_parse_ir_segments(annotated, chain)
      if (!is.null(value) && nrow(value)) {
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
            file.path("hla_tcr", paste0(chain, ".first.qs2")),
            .viewerPackHlaFirstFrame(value, object),
            "hla-first-frame"
          )
        )
      }
    }
  }
  list(
    assets = assets,
    modules = unique(modules),
    capabilities = .viewerDatasetCapabilities(object),
    projection_names = projections,
    metadata_names = metadata_names,
    group_metric_frames = group_metrics$frames,
    trajectory_frames = trajectory$frames,
    spatial_frames = spatial$frames,
    immune_receptors = receptors,
    hla_chains = chains
  )
}

.viewerPackInvalid <- function(reason) list(valid = FALSE, reason = reason)

#' Validate a Viewer Pack
#' @param file Path to the canonical CRB.
#' @param pack Optional Viewer Pack directory.
#' @param full Verify every asset size and checksum.
#' @return A validation result.
#' @export
validateViewerPack <- function(
  file,
  pack = .viewerPackPath(file),
  full = TRUE
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
  if (
    !identical(
      manifest$dataset_fingerprint,
      paste0("md5-crb-v1:", .viewerPackMd5(file))
    )
  ) {
    return(.viewerPackInvalid("Viewer Pack dataset fingerprint mismatch"))
  }
  if (
    length(manifest$projection_names) &&
      !identical(manifest$projection_identity_version, 1L)
  ) {
    return(.viewerPackInvalid(
      "Rebuild Viewer Pack: projection identity is unverified"
    ))
  }
  object <- tryCatch(readCerebro(file), error = function(error) NULL)
  if (is.null(object)) {
    return(.viewerPackInvalid("CRB could not be read"))
  }
  cells <- tryCatch(.viewerPackCells(object), error = function(error) NULL)
  if (
    is.null(cells) || !identical(as.integer(manifest$n_cells), length(cells))
  ) {
    return(.viewerPackInvalid("Viewer Pack cell count mismatch"))
  }
  if (
    !identical(
      manifest$cell_order_fingerprint,
      .viewerPackCellOrderFingerprint(cells)
    )
  ) {
    return(.viewerPackInvalid("Viewer Pack cell-order fingerprint mismatch"))
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
  list(valid = TRUE, reason = NULL, manifest = manifest, path = pack)
}

#' Read a valid Viewer Pack descriptor
#' @inheritParams validateViewerPack
#' @return A descriptor or `NULL`.
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
#' @export
buildViewerPack <- function(
  file,
  viewer_binary = c("auto", "always", "never"),
  viewer_binary_threshold = 500000L,
  overwrite = FALSE
) {
  options <- .viewerPackOptions(viewer_binary, viewer_binary_threshold)
  file <- normalizePath(file, mustWork = TRUE)
  object <- readCerebro(file)
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
  built <- .viewerPackBuildAssets(object, stage)
  manifest <- list(
    schema_version = .viewerPackSchemaVersion,
    projection_identity_version = 1L,
    violin_summary_version = 1L,
    dataset_fingerprint = paste0("md5-crb-v1:", .viewerPackMd5(file)),
    cell_order_fingerprint = .viewerPackCellOrderFingerprint(cells),
    n_cells = length(cells),
    modules = built$modules,
    capabilities = built$capabilities,
    projection_names = built$projection_names,
    metadata_names = built$metadata_names,
    group_metric_frames = built$group_metric_frames,
    trajectory_frames = built$trajectory_frames,
    spatial_frames = built$spatial_frames,
    immune_receptors = built$immune_receptors,
    hla_chains = built$hla_chains,
    assets = built$assets
  )
  jsonlite::write_json(
    manifest,
    file.path(stage, "manifest.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    dataframe = "rows"
  )
  check <- validateViewerPack(file, stage, full = TRUE)
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
