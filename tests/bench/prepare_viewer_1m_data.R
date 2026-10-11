.viewer1mCacheDir <- function(path = NULL) {
  if (!is.null(path)) {
    if (
      !is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)
    ) {
      stop("CEREBRO_LARGE_CACHE must be one non-empty path.", call. = FALSE)
    }
    return(path)
  }
  file.path(tools::R_user_dir("CerebroNexus", "cache"), "large-examples")
}

.viewer1mPaths <- function(cache_dir = NULL) {
  root <- file.path(.viewer1mCacheDir(cache_dir), "1m")
  list(
    source = file.path(
      root,
      "source",
      "1M_neurons_filtered_gene_bc_matrices_h5.h5"
    ),
    seurat = file.path(root, "seurat", "mouse_brain_1m.rds"),
    seurat_sidecar = file.path(root, "seurat", "mouse_brain_1m.bpcells"),
    crb = file.path(root, "cerebro", "cerebro_mouse_brain_1m.crb"),
    crb_sidecar = file.path(root, "cerebro", "cerebro_mouse_brain_1m.bpcells")
  )
}

.viewer1mComplete <- function(file, sidecar) {
  file.exists(file) &&
    !dir.exists(file) &&
    dir.exists(sidecar) &&
    length(list.files(sidecar, all.files = TRUE, no.. = TRUE)) > 0L
}

.viewer1mRequire <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "The 1M benchmark requires: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

.viewer1mDownload <- function(dest) {
  if (file.exists(dest) && !dir.exists(dest)) {
    return(dest)
  }
  url <- paste0(
    "https://cf.10xgenomics.com/samples/cell-exp/1.3.0/1M_neurons/",
    "1M_neurons_filtered_gene_bc_matrices_h5.h5"
  )
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  partial <- tempfile(paste0(basename(dest), ".part-"), dirname(dest))
  on.exit(unlink(partial, force = TRUE), add = TRUE)
  old_options <- options(timeout = max(getOption("timeout"), 86400))
  on.exit(options(old_options), add = TRUE)
  utils::download.file(url, partial, mode = "wb", method = "libcurl")
  if (!file.exists(partial) || !file.rename(partial, dest)) {
    stop("Could not publish the downloaded 1M source matrix.", call. = FALSE)
  }
  dest
}

.viewer1mUniqueGeneSymbols <- function(symbols, n_genes) {
  symbols <- as.character(symbols)
  if (
    length(symbols) != n_genes ||
      anyNA(symbols) ||
      any(!nzchar(symbols))
  ) {
    stop(
      "The 10x source must contain one non-empty gene symbol per row.",
      call. = FALSE
    )
  }
  make.unique(symbols)
}

.viewer1mScale01 <- function(values) {
  values <- as.numeric(values)
  finite <- is.finite(values)
  if (!any(finite)) {
    stop("The marker-guided trajectory has no finite scores.", call. = FALSE)
  }
  limits <- stats::quantile(
    values[finite],
    probs = c(0.01, 0.99),
    na.rm = TRUE,
    names = FALSE
  )
  if (!all(is.finite(limits)) || diff(limits) <= sqrt(.Machine$double.eps)) {
    stop(
      "The marker-guided trajectory needs a variable developmental score.",
      call. = FALSE
    )
  }
  scaled <- (pmin(pmax(values, limits[[1L]]), limits[[2L]]) -
    limits[[1L]]) /
    diff(limits)
  scaled[!finite] <- NA_real_
  scaled
}

.viewer1mMatchMarkers <- function(available, requested) {
  available <- as.character(available)
  index <- match(tolower(requested), tolower(available))
  unique(available[index[!is.na(index)]])
}

.viewer1mScaleProgram <- function(values) {
  values <- as.numeric(values)
  finite <- is.finite(values)
  if (!any(finite)) {
    return(rep(0, length(values)))
  }
  limits <- stats::quantile(
    values[finite],
    probs = c(0.01, 0.99),
    na.rm = TRUE,
    names = FALSE
  )
  if (!all(is.finite(limits)) || diff(limits) <= sqrt(.Machine$double.eps)) {
    return(rep(0, length(values)))
  }
  scaled <- (pmin(pmax(values, limits[[1L]]), limits[[2L]]) -
    limits[[1L]]) /
    diff(limits)
  scaled[!finite] <- 0
  scaled
}

.viewer1mMarkerGuidedEdges <- function(centers) {
  centers <- centers[
    order(centers$pseudotime, centers$DR_1, centers$DR_2, centers$state),
    ,
    drop = FALSE
  ]
  if (nrow(centers) < 2L) {
    return(data.frame(
      source_dim_1 = numeric(),
      source_dim_2 = numeric(),
      target_dim_1 = numeric(),
      target_dim_2 = numeric()
    ))
  }

  state <- as.character(centers$state)
  parent_by_state <- c(
    "Neuroblast" = "Neural progenitor",
    "Maturing neuron" = "Neuroblast",
    "Excitatory neuron" = if ("Maturing neuron" %in% state) {
      "Maturing neuron"
    } else {
      "Neuroblast"
    },
    "Inhibitory neuron" = if ("Maturing neuron" %in% state) {
      "Maturing neuron"
    } else {
      "Neuroblast"
    }
  )

  edges <- lapply(seq.int(2L, nrow(centers)), function(index) {
    current <- centers[index, , drop = FALSE]
    parent_state <- unname(parent_by_state[state[[index]]])
    parent <- match(parent_state, state)
    if (is.na(parent) || parent >= index) {
      parent <- index - 1L
    }
    data.frame(
      source_dim_1 = centers$DR_1[parent],
      source_dim_2 = centers$DR_2[parent],
      target_dim_1 = current$DR_1,
      target_dim_2 = current$DR_2
    )
  })
  do.call(rbind, edges)
}

.viewer1mMarkerGuidedTrajectory <- function(object, metadata, projection) {
  required_metadata <- c("cell_barcode", "seurat_clusters")
  if (
    !is.data.frame(metadata) ||
      !all(required_metadata %in% colnames(metadata)) ||
      is.null(dim(projection)) ||
      ncol(projection) < 2L
  ) {
    stop(
      "The 1M marker-guided trajectory requires cell barcodes, clusters, and a 2-D projection.",
      call. = FALSE
    )
  }

  cells <- as.character(metadata$cell_barcode)
  projection <- as.matrix(projection[, seq_len(2L), drop = FALSE])
  projection_cells <- rownames(projection)
  if (!is.null(projection_cells)) {
    projection <- projection[match(cells, projection_cells), , drop = FALSE]
  } else if (nrow(projection) != length(cells)) {
    stop("The 1M demo projection does not match its metadata.", call. = FALSE)
  }

  clusters <- as.character(metadata$seurat_clusters)
  coordinates <- data.frame(
    DR_1 = as.numeric(projection[, 1L]),
    DR_2 = as.numeric(projection[, 2L])
  )
  keep <- !is.na(cells) &
    nzchar(cells) &
    !is.na(clusters) &
    nzchar(clusters) &
    stats::complete.cases(coordinates)
  if (sum(keep) < 2L) {
    stop(
      "The 1M demo trajectory needs at least two valid cells.",
      call. = FALSE
    )
  }

  coordinates <- coordinates[keep, , drop = FALSE]
  cells <- cells[keep]
  clusters <- clusters[keep]
  if (length(unique(clusters)) < 2L) {
    stop(
      "The 1M marker-guided trajectory needs at least two clusters.",
      call. = FALSE
    )
  }

  available_genes <- object$getGeneNames()
  requested_markers <- list(
    early = c("Sox2", "Hes1", "Vim", "Fabp7", "Nes"),
    transition = c("Dcx", "Tubb3", "Sox11", "Neurod1"),
    late = c("Rbfox3", "Snap25", "Syp", "Map2", "Grin1"),
    excitatory = c("Slc17a7", "Slc17a6", "Neurod6", "Tbr1", "Satb2"),
    inhibitory = c("Gad1", "Gad2", "Dlx1", "Dlx2", "Dlx5"),
    endothelial = c("Kdr", "Cldn5", "Pecam1", "Flt1", "Esam"),
    microglia = c("C1qa", "C1qb", "C1qc", "Aif1", "Tyrobp"),
    oligodendrocyte = c("Olig1", "Olig2", "Sox10", "Mbp", "Mog")
  )
  markers <- lapply(
    requested_markers,
    function(genes) .viewer1mMatchMarkers(available_genes, genes)
  )
  if (!length(markers$early) || !length(markers$late)) {
    stop(
      paste0(
        "The 1M marker-guided trajectory requires at least one early and ",
        "one late neurogenesis marker in the expression matrix."
      ),
      call. = FALSE
    )
  }

  marker_expression <- lapply(markers, function(genes) {
    if (!length(genes)) {
      return(rep(0, length(cells)))
    }
    as.numeric(object$getMeanExpressionForCells(cells = cells, genes = genes))
  })
  marker_expression <- lapply(
    marker_expression,
    function(values) log1p(pmax(values, 0))
  )
  developmental_signal <-
    marker_expression$early +
    marker_expression$transition +
    marker_expression$late
  exclusion_signal <- marker_expression$endothelial +
    marker_expression$microglia +
    marker_expression$oligodendrocyte
  cluster_developmental <- tapply(
    developmental_signal,
    clusters,
    mean,
    na.rm = TRUE
  )
  cluster_exclusion <- tapply(
    exclusion_signal,
    clusters,
    mean,
    na.rm = TRUE
  )
  neural_clusters <- names(cluster_developmental)[
    is.finite(cluster_developmental) &
      cluster_developmental > 0 &
      cluster_developmental >= cluster_exclusion[names(cluster_developmental)]
  ]
  lineage_keep <- clusters %in% neural_clusters
  if (sum(lineage_keep) < 2L || length(neural_clusters) < 2L) {
    stop(
      "The 1M marker-guided trajectory found too few neural-lineage cells.",
      call. = FALSE
    )
  }
  excluded_clusters <- setdiff(unique(clusters), neural_clusters)
  cells <- cells[lineage_keep]
  clusters <- clusters[lineage_keep]
  coordinates <- coordinates[lineage_keep, , drop = FALSE]
  marker_expression <- lapply(
    marker_expression,
    function(values) values[lineage_keep]
  )

  developmental_scaled <- lapply(
    marker_expression[c("early", "transition", "late")],
    .viewer1mScaleProgram
  )
  total_signal <- Reduce(`+`, developmental_scaled)
  cell_score <- (0.5 *
    developmental_scaled$transition +
    developmental_scaled$late) /
    pmax(total_signal, .Machine$double.eps)
  cell_score[!is.finite(cell_score) | total_signal == 0] <- NA_real_

  cluster_score <- tapply(cell_score, clusters, stats::median, na.rm = TRUE)
  cluster_score[!is.finite(cluster_score)] <- NA_real_
  fallback_score <- stats::median(cell_score, na.rm = TRUE)
  if (!is.finite(fallback_score)) {
    stop(
      "The 1M marker genes have no expression in the selected cells.",
      call. = FALSE
    )
  }
  cell_cluster_score <- unname(cluster_score[clusters])
  cell_cluster_score[!is.finite(cell_cluster_score)] <- fallback_score
  cell_score[!is.finite(cell_score)] <- cell_cluster_score[
    !is.finite(cell_score)
  ]
  pseudotime <- .viewer1mScale01(
    0.75 * cell_cluster_score + 0.25 * cell_score
  )

  cuts <- stats::quantile(
    pseudotime,
    probs = c(0.34, 0.68),
    na.rm = TRUE,
    names = FALSE
  )
  state <- rep("Neuroblast", length(pseudotime))
  state[pseudotime <= cuts[[1L]]] <- "Neural progenitor"
  terminal <- pseudotime >= cuts[[2L]]
  branch_expression <- lapply(
    marker_expression[c("excitatory", "inhibitory")],
    .viewer1mScaleProgram
  )
  branch_total <- branch_expression$excitatory +
    branch_expression$inhibitory
  branch_cut <- stats::quantile(
    branch_total[terminal],
    probs = 0.25,
    na.rm = TRUE,
    names = FALSE
  )
  specified_branch <- terminal & branch_total > branch_cut
  state[terminal & !specified_branch] <- "Maturing neuron"
  state[
    specified_branch &
      branch_expression$excitatory >= branch_expression$inhibitory
  ] <-
    "Excitatory neuron"
  state[
    specified_branch &
      branch_expression$inhibitory > branch_expression$excitatory
  ] <-
    "Inhibitory neuron"
  desired_state_levels <- c(
    "Neural progenitor",
    "Neuroblast",
    "Maturing neuron",
    "Excitatory neuron",
    "Inhibitory neuron"
  )
  state_levels <- desired_state_levels[desired_state_levels %in% state]
  trajectory_meta <- data.frame(
    DR_1 = coordinates$DR_1,
    DR_2 = coordinates$DR_2,
    pseudotime = pseudotime,
    state = factor(state, levels = state_levels),
    row.names = cells
  )

  centers <- stats::aggregate(
    trajectory_meta[c("DR_1", "DR_2", "pseudotime")],
    list(state = trajectory_meta$state),
    mean
  )
  trajectory_edges <- .viewer1mMarkerGuidedEdges(centers)

  list(
    meta = trajectory_meta,
    edges = trajectory_edges,
    provenance = list(
      type = "marker_guided",
      version = 2L,
      description = paste0(
        "E18 neural-lineage cells selected against endothelial, microglial, ",
        "and oligodendrocyte programs, ",
        "then ordered with early, transitional, and late neural ",
        "markers and split by excitatory/inhibitory programs; this is a ",
        "marker-guided demo path, not a formal trajectory-inference result."
      ),
      markers = markers,
      excluded_clusters = sort(excluded_clusters, method = "radix")
    )
  )
}

.viewer1mDropLegacyTrajectory <- function(object) {
  methods <- tryCatch(
    object$getMethodsForTrajectories(),
    error = function(e) character()
  )
  if (!"illustrative" %in% methods) {
    return(FALSE)
  }
  names_for_method <- tryCatch(
    object$getNamesOfTrajectories("illustrative"),
    error = function(e) character()
  )
  if (!"UMAP_cluster_path" %in% names_for_method) {
    return(FALSE)
  }
  trajectories <- tryCatch(object$trajectories, error = function(e) NULL)
  if (!is.list(trajectories)) {
    return(FALSE)
  }
  trajectories[["illustrative"]][["UMAP_cluster_path"]] <- NULL
  if (!length(trajectories[["illustrative"]])) {
    trajectories[["illustrative"]] <- NULL
  }
  object$trajectories <- trajectories
  TRUE
}

.viewer1mEnrichDemoObject <- function(object) {
  changed <- .viewer1mDropLegacyTrajectory(object)
  parameters <- tryCatch(object$getParameters(), error = function(e) list())
  if (!identical(parameters[["main_group"]], "seurat_clusters")) {
    object$addParameters("main_group", "seurat_clusters")
    changed <- TRUE
  }

  method <- "marker_guided"
  trajectory_name <- "E18_neurogenesis"
  methods <- tryCatch(
    object$getMethodsForTrajectories(),
    error = function(e) character()
  )
  names_for_method <- if (method %in% methods) {
    tryCatch(
      object$getNamesOfTrajectories(method),
      error = function(e) character()
    )
  } else {
    character()
  }
  current_trajectory <- tryCatch(
    object$trajectories[[method]][[trajectory_name]],
    error = function(e) NULL
  )
  needs_trajectory <-
    !trajectory_name %in% names_for_method ||
    !identical(current_trajectory$provenance$version, 2L)
  if (needs_trajectory) {
    projection_names <- object$availableProjections()
    if (!length(projection_names)) {
      stop("The 1M demo needs a projection for Linked Views.", call. = FALSE)
    }
    object$addTrajectory(
      method,
      trajectory_name,
      .viewer1mMarkerGuidedTrajectory(
        object,
        object$getMetaData(),
        object$getProjection(projection_names[[1L]])
      )
    )
    changed <- TRUE
  }
  changed
}

.viewer1mEnrichDemoCrb <- function(file) {
  object <- readCerebro(file)
  if (.viewer1mEnrichDemoObject(object)) {
    message(
      paste0(
        "Adding the marker-guided E18 neurogenesis trajectory and ",
        "seurat_clusters default to the 1M demo."
      )
    )
    saveCerebro(object, file)
  }
  file
}

.viewer1mGeneSymbols <- function(source, n_genes) {
  listing <- rhdf5::h5ls(source, recursive = TRUE)
  hit <- which(listing$name == "gene_names" & listing$otype == "H5I_DATASET")
  if (length(hit) != 1L) {
    stop(
      "The 10x source must contain exactly one gene_names dataset.",
      call. = FALSE
    )
  }
  dataset <- file.path(listing$group[hit], listing$name[hit])
  .viewer1mUniqueGeneSymbols(rhdf5::h5read(source, dataset), n_genes)
}

.viewer1mAnalyze <- function(object) {
  previous_future_size <- getOption("future.globals.maxSize")
  options(future.globals.maxSize = max(previous_future_size, 4e9))
  on.exit(options(future.globals.maxSize = previous_future_size), add = TRUE)

  object <- Seurat::NormalizeData(object, verbose = TRUE)
  object <- Seurat::FindVariableFeatures(object, verbose = TRUE)
  object <- Seurat::SketchData(
    object,
    ncells = 50000L,
    method = "LeverageScore",
    sketched.assay = "sketch",
    seed = 123L,
    verbose = TRUE
  )
  SeuratObject::DefaultAssay(object) <- "sketch"
  object <- Seurat::FindVariableFeatures(object, verbose = TRUE)
  object <- Seurat::ScaleData(object, verbose = TRUE)
  object <- Seurat::RunPCA(object, npcs = 30L, seed.use = 123L, verbose = TRUE)
  object <- Seurat::FindNeighbors(object, dims = seq_len(30L), verbose = TRUE)
  object <- Seurat::FindClusters(
    object,
    resolution = 1,
    random.seed = 123L,
    verbose = TRUE
  )
  object <- Seurat::RunUMAP(
    object,
    dims = seq_len(30L),
    return.model = TRUE,
    seed.use = 123L,
    verbose = TRUE
  )
  Seurat::ProjectData(
    object,
    assay = "RNA",
    full.reduction = "pca.full",
    sketched.assay = "sketch",
    sketched.reduction = "pca",
    umap.model = "umap",
    dims = seq_len(30L),
    refdata = list(cluster_full = "seurat_clusters"),
    verbose = TRUE
  )
}

.viewer1mBuildSeurat <- function(paths) {
  matrix <- BPCells::open_matrix_10x_hdf5(paths$source)
  matrix_cells <- colnames(matrix)
  if (anyDuplicated(matrix_cells) || length(matrix_cells) < 1000000L) {
    stop("The 10x source must contain at least 1M unique cells.", call. = FALSE)
  }
  gene_symbols <- .viewer1mGeneSymbols(paths$source, nrow(matrix))
  selected_cells <- matrix_cells[
    floor(seq(0, length(matrix_cells) - 1, length.out = 1000000L)) + 1L
  ]
  selected_matrix <- matrix[, selected_cells]
  rownames(selected_matrix) <- gene_symbols
  if (!inherits(selected_matrix, "IterableMatrix")) {
    selected_matrix <- methods::as(selected_matrix, "IterableMatrix")
  }
  selected_matrix <- BPCells::convert_matrix_type(
    selected_matrix,
    type = "uint32_t"
  )

  dir.create(dirname(paths$seurat), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(paths$seurat_sidecar)) {
    unlink(paths$seurat_sidecar, recursive = TRUE, force = TRUE)
  }
  BPCells::write_matrix_dir(selected_matrix, dir = paths$seurat_sidecar)
  counts <- BPCells::open_matrix_dir(paths$seurat_sidecar)
  object <- SeuratObject::CreateSeuratObject(
    counts = counts,
    meta.data = data.frame(
      sample = rep("10x E18 mouse brain (1M subset)", 1000000L),
      row.names = selected_cells
    ),
    assay = "RNA",
    project = "mouse_brain_1m"
  )
  analyzed <- .viewer1mAnalyze(object)
  embedding <- SeuratObject::Embeddings(analyzed, "full.umap")
  metadata <- analyzed[[]]
  metadata <- data.frame(
    sample = metadata[selected_cells, "sample"],
    seurat_clusters = as.character(metadata[selected_cells, "cluster_full"]),
    row.names = selected_cells,
    check.names = FALSE
  )
  object <- SeuratObject::CreateSeuratObject(
    counts = BPCells::open_matrix_dir(paths$seurat_sidecar),
    meta.data = metadata,
    assay = "RNA",
    project = "mouse_brain_1m"
  )
  object[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding[selected_cells, , drop = FALSE],
    key = "UMAP_",
    assay = "RNA"
  )
  saveRDS(object, paths$seurat, version = 3)
}

prepareViewer1mBenchmarkData <- function(cache_dir = NULL) {
  paths <- .viewer1mPaths(cache_dir)
  .viewer1mRequire("BPCells")
  if (.viewer1mComplete(paths$crb, paths$crb_sidecar)) {
    return(.viewer1mEnrichDemoCrb(paths$crb))
  }

  .viewer1mRequire(c("Seurat", "SeuratObject", "rhdf5"))
  if (!.viewer1mComplete(paths$seurat, paths$seurat_sidecar)) {
    message("Preparing the 1M benchmark data from official 10x data.")
    .viewer1mDownload(paths$source)
    .viewer1mBuildSeurat(paths)
  }

  dir.create(dirname(paths$crb), recursive = TRUE, showWarnings = FALSE)
  convertSeuratToCerebro(
    seurat_file = paths$seurat,
    result_dir = dirname(paths$crb),
    assay = "RNA",
    slot = "counts",
    experiment_name = "10x E18 mouse brain (1M subset)",
    organism = "Mouse",
    groups = c("sample", "seurat_clusters"),
    nUMI = "nCount_RNA",
    nGene = "nFeature_RNA",
    expression_matrix_mode = "bpcells",
    add_most_expressed_genes = FALSE,
    verbose = TRUE
  )
  if (!.viewer1mComplete(paths$crb, paths$crb_sidecar)) {
    stop("The 1M benchmark CRB is incomplete.", call. = FALSE)
  }
  .viewer1mEnrichDemoCrb(paths$crb)
}
