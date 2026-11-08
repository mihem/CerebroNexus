#!/usr/bin/env Rscript
# Builder "Spatial alignment demo": a small crop of a real 10x Visium section.
#
# Source: 10x Genomics, "Human Breast Cancer (Block A Section 1)", Visium
# Spatial Gene Expression, Space Ranger 1.1.0, licensed CC BY 4.0
# (https://creativecommons.org/licenses/by/4.0/). Download the two published
# files of that dataset from 10x Genomics
# (https://www.10xgenomics.com/datasets, "V1_Breast_Cancer_Block_A_Section_1")
# or the scverse mirror
# (https://exampledata.scverse.org/scanpy/visium/1.1.0/V1_Breast_Cancer_Block_A_Section_1/):
#   V1_Breast_Cancer_Block_A_Section_1_filtered_feature_bc_matrix.h5
#   V1_Breast_Cancer_Block_A_Section_1_spatial.tar.gz
# and pass their paths; nothing is downloaded and no local path is assumed.
#
# Rscript data-raw/build_builder_visium_fixture.R \
#   --matrix .../V1_Breast_Cancer_Block_A_Section_1_filtered_feature_bc_matrix.h5 \
#   --spatial .../V1_Breast_Cancer_Block_A_Section_1_spatial.tar.gz \
#   [--output DIR]
#
# --spatial also accepts an extracted directory that contains `spatial/` or
# the `spatial/` directory itself.

## The crop, in pixels of tissue_hires_image.png (2000 x 2000): the right
## edge of the section with the pink nodule, its stroma, fat and the glass
## beyond the tissue edge, so a wrong flip or offset is visible at once.
builder_visium_crop <- list(x0 = 1200L, y0 = 880L, width = 560L, height = 440L)
builder_visium_genes <- 300L
builder_visium_seed <- 20261104L
builder_visium_sample <- "V1_Breast_Cancer_Block_A_Section_1"

builder_visium_stabilize <- function(object) {
  for (name in names(object@commands)) {
    object@commands[[name]]@time.stamp <- as.POSIXct(
      "2026-11-04 00:00:00",
      tz = "UTC"
    )
  }
  object
}

builder_visium_read_matrix <- function(path) {
  read <- function(name) {
    as.vector(rhdf5::h5read(path, name, bit64conversion = "double"))
  }
  shape <- as.integer(read("matrix/shape"))
  counts <- Matrix::sparseMatrix(
    i = as.integer(read("matrix/indices")) + 1L,
    p = as.integer(read("matrix/indptr")),
    x = as.numeric(read("matrix/data")),
    dims = shape,
    dimnames = list(
      make.unique(as.character(read("matrix/features/name"))),
      as.character(read("matrix/barcodes"))
    ),
    repr = "C"
  )
  rhdf5::h5closeAll()
  methods::as(counts, "CsparseMatrix")
}

builder_visium_spatial_dir <- function(path) {
  if (!dir.exists(path)) {
    exdir <- tempfile("visium-spatial-")
    utils::untar(path, exdir = exdir)
    path <- exdir
  }
  if (dir.exists(file.path(path, "spatial"))) {
    path <- file.path(path, "spatial")
  }
  path
}

builder_visium_read_positions <- function(dir) {
  v2 <- file.path(dir, "tissue_positions.csv")
  positions <- if (file.exists(v2)) {
    utils::read.csv(v2, stringsAsFactors = FALSE)
  } else {
    utils::read.csv(
      file.path(dir, "tissue_positions_list.csv"),
      header = FALSE,
      stringsAsFactors = FALSE
    )
  }
  names(positions) <- c(
    "barcode",
    "in_tissue",
    "array_row",
    "array_col",
    "pxl_row_in_fullres",
    "pxl_col_in_fullres"
  )
  positions
}

builder_visium_fixture <- function(matrix_path, spatial_path, output) {
  previous_plan <- future::plan(future::sequential)
  on.exit(future::plan(previous_plan), add = TRUE)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  spatial <- builder_visium_spatial_dir(spatial_path)
  scales <- jsonlite::fromJSON(file.path(spatial, "scalefactors_json.json"))
  image <- png::readPNG(file.path(spatial, "tissue_hires_image.png"))
  positions <- builder_visium_read_positions(spatial)
  counts <- builder_visium_read_matrix(matrix_path)

  ## The whole section is analysed as 10x's own spots, so clusters and UMAP
  ## describe the real tissue; only the result is cropped.
  positions <- positions[
    positions$in_tissue == 1L & positions$barcode %in% colnames(counts),
  ]
  counts <- counts[, positions$barcode]
  set.seed(builder_visium_seed)
  object <- SeuratObject::CreateSeuratObject(
    counts,
    assay = "Spatial",
    project = "Visium_Breast_Cancer"
  )
  object <- Seurat::NormalizeData(object, verbose = FALSE)
  object <- Seurat::FindVariableFeatures(
    object,
    nfeatures = 2000,
    verbose = FALSE
  )
  object <- Seurat::ScaleData(object, verbose = FALSE)
  object <- Seurat::RunPCA(
    object,
    npcs = 30,
    seed.use = builder_visium_seed,
    verbose = FALSE
  )
  object <- Seurat::FindNeighbors(object, dims = 1:20, verbose = FALSE)
  object <- Seurat::FindClusters(
    object,
    resolution = 0.5,
    random.seed = builder_visium_seed,
    verbose = FALSE
  )
  object <- Seurat::RunUMAP(
    object,
    dims = 1:20,
    seed.use = builder_visium_seed,
    umap.method = "uwot",
    verbose = FALSE
  )

  ## Spot centres in the cropped image's pixel space: x = column, y = row.
  crop <- builder_visium_crop
  scale <- scales$tissue_hires_scalef
  radius <- scales$spot_diameter_fullres * scale / 2
  x <- positions$pxl_col_in_fullres * scale - crop$x0
  y <- positions$pxl_row_in_fullres * scale - crop$y0
  inside <- x >= radius &
    x <= crop$width - radius &
    y >= radius &
    y <= crop$height - radius
  cells <- positions$barcode[inside]
  genes <- utils::head(
    SeuratObject::VariableFeatures(object),
    builder_visium_genes
  )
  object <- Seurat::DietSeurat(
    object,
    layers = c("counts", "data"),
    features = genes,
    dimreducs = "umap",
    graphs = NULL
  )
  object <- object[, cells]
  ## nCount/nFeature stay the whole-transcriptome values of each spot.
  object$seurat_clusters <- droplevels(object$seurat_clusters)
  object$Spatial_snn_res.0.5 <- NULL
  object$sample <- builder_visium_sample
  SeuratObject::Idents(object) <- "seurat_clusters"
  SeuratObject::VariableFeatures(object) <- genes
  object[["visium_breast"]] <- SeuratObject::CreateFOV(
    coords = data.frame(x = x[inside], y = y[inside], cell = cells),
    type = "centroids",
    assay = "Spatial",
    key = "visium_"
  )
  saveRDS(
    builder_visium_stabilize(object),
    file.path(output, "visium_breast_cancer.rds"),
    version = 3,
    compress = "xz"
  )
  rows <- crop$y0 + seq_len(crop$height)
  columns <- crop$x0 + seq_len(crop$width)
  jpeg::writeJPEG(
    image[rows, columns, 1:3],
    file.path(output, "visium_breast_cancer_he.jpg"),
    quality = 0.82
  )
  invisible(object)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  script <- sub(
    "^--file=",
    "",
    grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
  )
  opts <- list(
    matrix = NULL,
    spatial = NULL,
    output = file.path(
      dirname(normalizePath(script)),
      "..",
      "inst",
      "builder",
      "fixtures"
    )
  )
  usage <- paste(
    "Usage: --matrix FILE_filtered_feature_bc_matrix.h5",
    "--spatial FILE_spatial.tar.gz|DIR [--output DIR]"
  )
  while (length(args)) {
    flag <- sub("^--", "", args[[1L]])
    if (length(args) < 2L || !flag %in% names(opts)) {
      stop(usage, call. = FALSE)
    }
    opts[[flag]] <- args[[2L]]
    args <- args[-c(1L, 2L)]
  }
  if (is.null(opts$matrix) || is.null(opts$spatial)) {
    stop(usage, call. = FALSE)
  }
  for (input in c(opts$matrix, opts$spatial)) {
    if (!file.exists(input)) stop("Not found: ", input, call. = FALSE)
  }
  packages <- c(
    "Seurat",
    "SeuratObject",
    "Matrix",
    "rhdf5",
    "png",
    "jpeg",
    "jsonlite",
    "future"
  )
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing)) {
    stop(
      "Install fixture build dependencies: ",
      paste(missing, collapse = ", ")
    )
  }
  options(Seurat.object.assay.version = "v5")
  builder_visium_fixture(
    opts$matrix,
    opts$spatial,
    file.path(opts$output, "spatial_alignment")
  )
  message(
    "Built the Visium fixture in ",
    normalizePath(file.path(opts$output, "spatial_alignment"))
  )
}
