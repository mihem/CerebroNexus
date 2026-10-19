##----------------------------------------------------------------------------##
## Helpers for the production smoke test: build fully-synthetic spatial Seurat
## objects and background images, so the convert -> createShinyApp -> run
## pipeline can be exercised end-to-end with no network and no data packages.
##----------------------------------------------------------------------------##

## Build a minimal spatial Seurat object with everything convertSeuratToCerebro
## requires: a counts assay, per-cell QC columns, grouping metadata, at least one
## dim reduction, and an @images FOV carrying tissue coordinates. `shift` offsets
## the coordinates so two datasets occupy visibly different coordinate spaces.
make_synthetic_spatial_seurat <- function(
  n_cells = 40,
  n_genes = 30,
  seed = 1,
  shift = 0
) {
  set.seed(seed)
  counts <- matrix(
    stats::rpois(n_genes * n_cells, lambda = 3),
    nrow = n_genes,
    dimnames = list(
      paste0("Gene", seq_len(n_genes)),
      paste0("Cell", seq_len(n_cells))
    )
  )
  ## Pass a sparse matrix so CreateSeuratObject does not warn about coercing a
  ## dense matrix (keeps the test output clean).
  counts <- methods::as(counts, "CsparseMatrix")
  obj <- SeuratObject::CreateSeuratObject(counts = counts, assay = "Spatial")
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)
  obj$seurat_clusters <- factor(
    sample(c("C1", "C2"), n_cells, replace = TRUE)
  )
  obj$cell_type_final <- obj$seurat_clusters

  ## synthetic tissue coordinates + FOV image (the @images slot export reads)
  coords <- data.frame(
    x = stats::runif(n_cells, 0, 100) + shift,
    y = stats::runif(n_cells, 0, 100) + shift,
    cell = colnames(obj)
  )
  cents <- SeuratObject::CreateCentroids(coords)
  fov <- SeuratObject::CreateFOV(
    coords = list(centroids = cents),
    type = "centroids",
    assay = "Spatial"
  )
  obj[["fov"]] <- fov

  ## a 2D embedding — convert requires at least one dim reduction
  emb <- matrix(
    stats::rnorm(n_cells * 2),
    nrow = n_cells,
    dimnames = list(colnames(obj), c("UMAP_1", "UMAP_2"))
  )
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb,
    key = "UMAP_",
    assay = "Spatial"
  )
  obj
}

## Convert one synthetic object into a .crb inside `result_dir`, returning the
## path to the produced file. Uses the Spatial-assay QC column names.
convert_synthetic_to_crb <- function(obj, result_dir, experiment_name) {
  dir.create(result_dir, showWarnings = FALSE, recursive = TRUE)
  convertSeuratToCerebro(
    seurat_file = obj,
    result_dir = result_dir,
    assay = "Spatial",
    slot = "data",
    experiment_name = experiment_name,
    organism = "mouse",
    groups = c("seurat_clusters", "cell_type_final"),
    nUMI = "nCount_Spatial",
    nGene = "nFeature_Spatial",
    add_most_expressed_genes = FALSE,
    verbose = FALSE
  )
  list.files(
    result_dir,
    pattern = "\\.crb$",
    full.names = TRUE,
    recursive = TRUE
  )[1]
}

## Write a tiny valid PNG of the given size to `path`. Content is irrelevant to
## the pipeline (createShinyApp copies the file without decoding it); the size
## only serves to make two backgrounds distinguishable.
write_dummy_png <- function(path, width = 4, height = 4) {
  if (!requireNamespace("png", quietly = TRUE)) {
    ## Fallback: a hard-coded 1x1 transparent PNG.
    writeBin(
      base64enc::base64decode(paste0(
        "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAACklEQVR4nGMAAQAABQAB",
        "DQottAAAAABJRU5ErkJggg=="
      )),
      path
    )
    return(invisible(path))
  }
  arr <- array(stats::runif(width * height * 3), dim = c(height, width, 3))
  png::writePNG(arr, path)
  invisible(path)
}
