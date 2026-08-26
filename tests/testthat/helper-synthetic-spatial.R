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
      as.raw(c(
        0x89,
        0x50,
        0x4e,
        0x47,
        0x0d,
        0x0a,
        0x1a,
        0x0a,
        0x00,
        0x00,
        0x00,
        0x0d,
        0x49,
        0x48,
        0x44,
        0x52,
        0x00,
        0x00,
        0x00,
        0x01,
        0x00,
        0x00,
        0x00,
        0x01,
        0x08,
        0x06,
        0x00,
        0x00,
        0x00,
        0x1f,
        0x15,
        0xc4,
        0x89,
        0x00,
        0x00,
        0x00,
        0x0a,
        0x49,
        0x44,
        0x41,
        0x54,
        0x78,
        0x9c,
        0x63,
        0x00,
        0x01,
        0x00,
        0x00,
        0x05,
        0x00,
        0x01,
        0x0d,
        0x0a,
        0x2d,
        0xb4,
        0x00,
        0x00,
        0x00,
        0x00,
        0x49,
        0x45,
        0x4e,
        0x44,
        0xae,
        0x42,
        0x60,
        0x82
      )),
      path
    )
    return(invisible(path))
  }
  arr <- array(stats::runif(width * height * 3), dim = c(height, width, 3))
  png::writePNG(arr, path)
  invisible(path)
}

## Build a synthetic object holding SEVERAL tissue sections, the way a real
## experiment does: sections cut from the same block share a sequencing sample,
## and each section occupies its own patch of coordinate space.
##
## Nothing else in the suite produces one. Every shipped demo `.crb` has exactly
## one spatial entry, which is how a builder that wrote one image into every
## section, and a viewer that could not reach any section but the first, both
## survived unnoticed.
##
## `sections` is a named list of cell counts; names become the @images entries.
## `samples` maps each section to its sequencing sample.
make_synthetic_multisection_seurat <- function(
  sections = c(sectionA1 = 30, sectionA2 = 30, sectionB1 = 40),
  samples = c(sectionA1 = "donorA", sectionA2 = "donorA", sectionB1 = "donorB"),
  n_genes = 30,
  seed = 11
) {
  set.seed(seed)
  n_cells <- sum(sections)
  cells <- paste0("Cell", seq_len(n_cells))
  counts <- matrix(
    stats::rpois(n_genes * n_cells, lambda = 3),
    nrow = n_genes,
    dimnames = list(paste0("Gene", seq_len(n_genes)), cells)
  )
  counts <- methods::as(counts, "CsparseMatrix")
  obj <- SeuratObject::CreateSeuratObject(counts = counts, assay = "Spatial")
  obj <- Seurat::NormalizeData(obj, verbose = FALSE)

  ## Which cells belong to which section, in order.
  ends <- cumsum(sections)
  starts <- c(1L, utils::head(ends, -1L) + 1L)
  names(starts) <- names(sections)

  obj$sample <- rep(unname(samples[names(sections)]), times = sections)
  obj$section <- rep(names(sections), times = sections)
  obj$seurat_clusters <- factor(
    sample(c("C1", "C2"), n_cells, replace = TRUE)
  )
  obj$cell_type_final <- obj$seurat_clusters

  ## Each section sits 500 units further along x, so a section drawn with the
  ## wrong coordinates is unmistakable rather than subtly off.
  for (i in seq_along(sections)) {
    nm <- names(sections)[i]
    idx <- starts[[nm]]:ends[[i]]
    coords <- data.frame(
      x = stats::runif(length(idx), 0, 100) + (i - 1L) * 500,
      y = stats::runif(length(idx), 0, 80),
      cell = cells[idx]
    )
    ## CreateCentroids first: handing CreateFOV a bare coordinate frame drops
    ## the cell names and the assignment fails.
    ## Each FOV needs its own key, or Seurat invents one and warns per section.
    obj[[nm]] <- SeuratObject::CreateFOV(
      coords = list(centroids = SeuratObject::CreateCentroids(coords)),
      type = "centroids",
      assay = "Spatial",
      key = paste0(tolower(nm), "_")
    )
  }

  emb <- matrix(
    stats::rnorm(n_cells * 2),
    nrow = n_cells,
    dimnames = list(cells, c("UMAP_1", "UMAP_2"))
  )
  obj[["umap"]] <- SeuratObject::CreateDimReducObject(
    embeddings = emb,
    key = "UMAP_",
    assay = "Spatial"
  )
  obj
}
