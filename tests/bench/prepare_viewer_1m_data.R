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
    return(paths$crb)
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
  paths$crb
}
