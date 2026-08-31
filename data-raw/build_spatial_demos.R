##----------------------------------------------------------------------------##
## build_spatial_demos.R
##
## Reproducible build of the REAL, public spatial-transcriptomics demo `.crb`
## files shipped in inst/extdata/examples/ for the Spatial tab.
##
## Unlike demo_spatial.crb (which is fully SYNTHETIC), the datasets produced here
## are down-sampled subsets of genuinely measured, citeable public data. Each one
## exercises a different technology so the multi-crb dropdown demonstrates that
## the same platform-agnostic extraction pipeline (.getSpatialData) ingests them
## all:
##
##   | .crb                       | Technology   | Source                          | Image
##   |----------------------------|--------------|---------------------------------|------
##   | demo_spatial_visium.crb    | 10x Visium   | SeuratData::stxBrain anterior1  | H&E
##   | demo_spatial_slideseq.crb  | Slide-seq v2 | SeuratData::ssHippo             | none
##   | demo_spatial_merfish.crb   | MERFISH      | MerfishData Petukhov 2021 ileum | DAPI
##   | demo_spatial_xenium.crb    | 10x Xenium   | 10x mouse brain CTX+HP (public) | DAPI
##
## Visium and Xenium load their genuine histology images from standalone files;
## MERFISH embeds its DAPI mosaic in the .crb. This deliberately exercises both
## supported image paths without storing duplicate Xenium rasters in the .crb.
##
## Slide-seq v2 (ssHippo) is a Seurat *v4-era* `SlideSeq` object; it is the
## end-to-end proof that exportFromSeurat handles v4 spatial objects, not only
## Seurat v5 (Visium/MERFISH here are v5).
##
## One build pulls data directly over the network rather than from an R package.
## It now AUTO-DOWNLOADS the raw data on first run (via ensure_download /
## ensure_unzipped) so the whole link -> .crb pipeline runs from one command;
## the download is skipped if the file is already present, and the build no-ops
## with a message only if the download itself fails.
##   * Xenium  -- fetches the 10x mouse brain coronal CTX+HP outs bundle (~3.5 GB)
##                and unzips it under data-raw/xenium/brain/. Its DAPI morphology
##                image is a JPEG2000 OME-TIFF that R's `tiff`/EBImage cannot
##                decode, so it is read with the Bioconductor package
##                `RBioFormats` (a pure-R wrapper over the Java Bio-Formats
##                library). If `RBioFormats` is missing, the Xenium demo is built
##                WITHOUT the image (coordinates still ship).
##
## Run from the package root, with SeuratData + MerfishData installed:
##   Rscript data-raw/build_spatial_demos.R
##
## data-raw/ is excluded from the built package via .Rbuildignore; it lives in
## the repo for reproducibility only.
##----------------------------------------------------------------------------##

suppressPackageStartupMessages({
  library(Seurat)
  library(Matrix)
})

## Load the in-tree package so the local .getSpatialData / exportFromSeurat edits
## are exercised, not an installed copy.
pkgload::load_all(".", quiet = TRUE)

set.seed(42)
out_dir <- "inst/extdata/examples"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## Cap cells per demo so the shipped .crb stays small and the Spatial tab renders
## quickly. Sampling is stratified by cell type where available.
MAX_CELLS <- 5000

##----------------------------------------------------------------------------##
## helpers
##----------------------------------------------------------------------------##

## Download a file to `dest` if it is not already there, so the network-sourced
## demo (Xenium) builds end-to-end from one `Rscript` call instead of
## needing a manual curl step first. Uses a resumable, retrying download and
## writes to a temp file first so an interrupted transfer never leaves a
## half-written file that later looks complete. Returns TRUE on success.
ensure_download <- function(url, dest) {
  if (file.exists(dest)) {
    return(TRUE)
  }
  dir.create(dirname(dest), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(dest, ".part")
  message("  downloading ", basename(dest), " ...")
  ok <- tryCatch(
    {
      utils::download.file(
        url,
        destfile = tmp,
        mode = "wb",
        quiet = FALSE,
        method = "libcurl",
        extra = "--retry 3 --location"
      )
      TRUE
    },
    error = function(e) {
      message("  download failed: ", conditionMessage(e))
      FALSE
    }
  )
  if (!ok || !file.exists(tmp) || file.info(tmp)$size == 0) {
    unlink(tmp)
    return(FALSE)
  }
  file.rename(tmp, dest)
  TRUE
}

## Fetch + unzip a bundle if the expected sentinel file is not present. Returns
## TRUE when the sentinel exists afterwards.
ensure_unzipped <- function(url, zip_dest, unzip_dir, sentinel) {
  if (file.exists(sentinel)) {
    return(TRUE)
  }
  if (!ensure_download(url, zip_dest)) {
    return(FALSE)
  }
  dir.create(unzip_dir, showWarnings = FALSE, recursive = TRUE)
  message("  unzipping ", basename(zip_dest), " ...")
  utils::unzip(zip_dest, exdir = unzip_dir)
  file.exists(sentinel)
}

## Stratified down-sample of cell barcodes by a grouping vector.
sample_cells <- function(groups, max_cells) {
  cells <- names(groups)
  if (length(cells) <= max_cells) {
    return(cells)
  }
  frac <- max_cells / length(cells)
  keep <- unlist(
    lapply(split(cells, groups), function(cc) {
      n <- max(1L, round(length(cc) * frac))
      sample(cc, min(n, length(cc)))
    }),
    use.names = FALSE
  )
  ## trim any rounding overshoot
  if (length(keep) > max_cells) {
    keep <- sample(keep, max_cells)
  }
  keep
}

## Standard minimal Seurat processing so exportFromSeurat has a `data` layer, a
## non-PCA reduction and nUMI/nGene columns. Kept deliberately light: these are
## demo fixtures, not analysis outputs. nUMI/nGene are computed on the FULL gene
## set before trimming so they stay biologically meaningful.
##
## `max_genes` trims the embedded expression matrix (variable features first,
## then top-expressed) so the shipped .crb stays small; Visium's 31k genes would
## otherwise embed a ~70 MB matrix. Imaging panels (MERFISH ~135 genes) are kept
## whole.
process_seurat <- function(obj, assay, max_genes = 2000) {
  DefaultAssay(obj) <- assay
  obj$nUMI <- obj[[paste0("nCount_", assay)]][, 1]
  obj$nGene <- obj[[paste0("nFeature_", assay)]][, 1]
  obj <- NormalizeData(obj, assay = assay, verbose = FALSE)
  obj <- FindVariableFeatures(
    obj,
    assay = assay,
    nfeatures = min(2000L, nrow(obj)),
    verbose = FALSE
  )

  if (nrow(obj) > max_genes) {
    counts <- GetAssayData(obj, assay = assay, layer = "counts")
    top_expressed <- names(sort(Matrix::rowSums(counts), decreasing = TRUE))
    keep_genes <- unique(c(
      VariableFeatures(obj),
      head(top_expressed, max_genes)
    ))
    keep_genes <- head(keep_genes, max_genes)
    obj <- subset(obj, features = keep_genes)
    obj <- FindVariableFeatures(
      obj,
      assay = assay,
      nfeatures = min(2000L, nrow(obj)),
      verbose = FALSE
    )
  }

  obj <- ScaleData(obj, assay = assay, verbose = FALSE)
  npcs <- min(30L, ncol(obj) - 1L, nrow(obj) - 1L)
  obj <- RunPCA(obj, assay = assay, npcs = npcs, verbose = FALSE)
  obj <- RunUMAP(obj, dims = seq_len(min(20L, npcs)), verbose = FALSE)
  obj
}

## Encode an RGB/grey raster array (values in 0..1, or a `raster` object) to a
## base64 PNG data: URI, downscaling to at most `max_px` on the long edge so the
## embedded image does not bloat the .crb.
##
## `flip_y` mirrors the raster top-to-bottom before encoding. The demos do NOT
## use it: every embedded image is stored in its NATIVE orientation (row 0 =
## image top). It is kept only as an escape hatch for a source raster that is
## stored bottom-up. Display alignment (if a dataset needs a flip) is a user
## control in the Spatial tab, not a stored per-.crb flag.
## Normalise a raster/array to an H x W x 3 numeric array in [0, 1], optionally
## flipping it top-to-bottom and downscaling so the long edge is <= max_px.
.prepare_raster_array <- function(arr, max_px = 1200, flip_y = FALSE) {
  # normalise input to an H x W x 3 numeric array in 0..1
  if (inherits(arr, "raster")) {
    m <- col2rgb(as.matrix(arr)) / 255
    h <- nrow(arr)
    w <- ncol(arr)
    arr <- array(0, dim = c(h, w, 3))
    arr[,, 1] <- matrix(m[1, ], h, w, byrow = FALSE)
    arr[,, 2] <- matrix(m[2, ], h, w, byrow = FALSE)
    arr[,, 3] <- matrix(m[3, ], h, w, byrow = FALSE)
  }
  if (length(dim(arr)) == 2) {
    arr <- array(rep(arr, 3), dim = c(dim(arr), 3))
  }
  if (isTRUE(flip_y)) {
    arr <- arr[rev(seq_len(dim(arr)[1])), , , drop = FALSE]
  }
  arr[arr < 0] <- 0
  arr[arr > 1] <- 1

  # downscale (nearest-neighbour) if the long edge exceeds max_px
  h <- dim(arr)[1]
  w <- dim(arr)[2]
  scale <- min(1, max_px / max(h, w))
  if (scale < 1) {
    nh <- max(1L, round(h * scale))
    nw <- max(1L, round(w * scale))
    ri <- round(seq(1, h, length.out = nh))
    ci <- round(seq(1, w, length.out = nw))
    arr <- arr[ri, ci, , drop = FALSE]
  }
  arr
}

encode_raster_png <- function(arr, max_px = 1200, flip_y = FALSE) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("the 'png' package is required to embed histology images")
  }
  arr <- .prepare_raster_array(arr, max_px = max_px, flip_y = flip_y)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  png::writePNG(arr, tmp)
  paste0("data:image/png;base64,", base64enc::base64encode(tmp))
}

## Write a raster/array to a standalone PNG file (for the external-image demo,
## where the background lives on disk and is loaded via `spatial_images` instead
## of being embedded in the .crb).
save_raster_png <- function(arr, file, max_px = 1200, flip_y = FALSE) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("the 'png' package is required to write histology images")
  }
  arr <- .prepare_raster_array(arr, max_px = max_px, flip_y = flip_y)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  png::writePNG(arr, file)
  invisible(file)
}

## Preserve Xenium pixel geometry while creating a coloured 90-degree variant.
tint_and_rotate_raster <- function(arr, colour) {
  arr <- .prepare_raster_array(arr, max_px = Inf)
  grey <- 0.2126 * arr[,, 1] + 0.7152 * arr[,, 2] + 0.0722 * arr[,, 3]
  rgb <- grDevices::col2rgb(colour)[, 1] / 255
  tinted <- array(0, dim = dim(arr))
  for (channel in seq_len(3L)) {
    tinted[,, channel] <- grey * rgb[[channel]]
  }
  rotated <- aperm(tinted, c(2, 1, 3))
  rotated[, rev(seq_len(dim(rotated)[2])), , drop = FALSE]
}

## Extract one channel of a JPEG2000-compressed OME-TIFF (the format 10x Xenium
## uses for morphology_focus) to a contrast-stretched greyscale raster in [0, 1],
## and report its extent in coordinate (micron) space. R's `tiff`/`EBImage`
## cannot decode the JPEG2000 tiles, so this uses the Bioconductor `RBioFormats`
## package (a pure-R wrapper over the Java Bio-Formats library). Returns
## list(raster = <H x W matrix in 0..1>, bounds = list(xmin,xmax,ymin,ymax)) on
## success, or NULL if `RBioFormats` is unavailable (caller then ships the demo
## without an image). `um_per_px` converts the pixel raster to the micron
## coordinate space GetTissueCoordinates reports; bounds are computed from the
## FULL-resolution dimensions regardless of which pyramid level is read.
extract_ome_tiff_channel <- function(
  ome_tif,
  um_per_px,
  channel = 1L,
  max_px = 1400L
) {
  if (!requireNamespace("RBioFormats", quietly = TRUE)) {
    message(
      "  RBioFormats not installed; Xenium image skipped ",
      "(BiocManager::install('RBioFormats'))"
    )
    return(NULL)
  }
  res <- tryCatch(
    {
      ## The OME-TIFF stores a resolution pyramid. Read the full-res dimensions
      ## from metadata (they define the coordinate-space extent), then pick the
      ## smallest pyramid level whose long edge still covers `max_px` so the embed
      ## stays compact without an extra downscale.
      md <- RBioFormats::read.metadata(ome_tif)
      n_res <- length(md)
      full <- RBioFormats::coreMetadata(md, series = 1)
      full_w <- full$sizeX
      full_h <- full$sizeY

      level <- 1L
      for (r in seq_len(n_res)) {
        cm <- RBioFormats::coreMetadata(md, series = r)
        if (max(cm$sizeX, cm$sizeY) >= max_px) {
          level <- r
        }
      }

      img <- RBioFormats::read.image(
        ome_tif,
        resolution = level,
        normalize = FALSE
      )
      arr <- as.array(img)
      ## RBioFormats returns [X, Y, C]; take the requested channel and transpose
      ## to [Y (rows), X (cols)] so row 0 is the top of the image, matching the
      ## renderer's expectation.
      ch <- if (length(dim(arr)) == 3) arr[,, channel] else arr
      ch <- t(ch)

      ## 2–99th-percentile contrast stretch so the DAPI nuclei read against the
      ## dark background, then clamp to [0, 1].
      qs <- stats::quantile(ch, c(0.02, 0.99), na.rm = TRUE)
      lo <- qs[[1]]
      hi <- qs[[2]]
      ch <- (ch - lo) / (hi - lo + 1e-9)
      ch[ch < 0] <- 0
      ch[ch > 1] <- 1

      list(
        raster = ch,
        bounds = list(
          xmin = 0,
          xmax = full_w * um_per_px,
          ymin = 0,
          ymax = full_h * um_per_px
        )
      )
    },
    error = function(e) {
      message("  RBioFormats OME-TIFF read failed: ", conditionMessage(e))
      NULL
    }
  )
  res
}

## Export + verify the spatial slot; optionally embed a real histology image.
##
## `image` is a base64 data: URI (see encode_raster_png); `image_bounds` is a
## named list(xmin,xmax,ymin,ymax) giving the image's extent IN COORDINATE SPACE
## (not the spot bounding box) so the renderer overlays it with correct scale.
export_and_verify <- function(
  obj,
  assay,
  groups,
  file,
  experiment_name,
  organism,
  image = NULL,
  image_bounds = NULL
) {
  if (file.exists(file)) {
    file.remove(file)
  }
  exportFromSeurat(
    object = obj,
    assay = assay,
    slot = "data",
    file = file,
    experiment_name = experiment_name,
    organism = organism,
    groups = groups,
    nUMI = "nUMI",
    nGene = "nGene",
    verbose = TRUE
  )
  crb <- readRDS(file)
  spatial_names <- crb$availableSpatial()
  stopifnot(
    "no spatial data written to .crb" = length(spatial_names) > 0
  )

  ## Inject the real histology image into the spatial slot, if supplied. Use a
  ## dedicated `histology_image` key: `.getSpatialData` already stores the image
  ## *name* under `image`, so overloading that would clobber it.
  if (!is.null(image)) {
    for (nm in spatial_names) {
      sd <- crb$getSpatialData(nm)
      sd$histology_image <- image
      sd$histology_image_bounds <- image_bounds
      crb$addSpatialData(nm, sd)
    }
    saveRDS(crb, file)
    crb <- readRDS(file)
  }

  sd <- crb$getSpatialData(spatial_names[1])
  coords <- sd$coordinates
  stopifnot(
    "spatial coordinates missing x/y" = all(c("x", "y") %in% colnames(coords)),
    "spatial coordinates empty" = nrow(coords) > 0,
    "coords/expression cell mismatch" = length(intersect(
      rownames(coords),
      colnames(sd$expression)
    )) >
      0
  )
  message(sprintf(
    "  OK %s: image='%s', %d cells, coord cols=[%s], embedded_image=%s",
    basename(file),
    spatial_names[1],
    nrow(coords),
    paste(colnames(coords)[seq_len(min(6, ncol(coords)))], collapse = ","),
    if (is.null(image)) "no" else "YES"
  ))
  invisible(crb)
}

##----------------------------------------------------------------------------##
## 1. 10x Visium  (SeuratData::stxBrain, anterior1)  -- Seurat v5 VisiumV2
##----------------------------------------------------------------------------##
build_visium <- function() {
  message("== Visium (stxBrain / anterior1) ==")
  suppressPackageStartupMessages(library(SeuratData))
  obj <- SeuratData::LoadData("stxBrain", type = "anterior1")
  assay <- "Spatial"

  ## Extract the REAL low-res H&E tissue image and work out its extent in the
  ## coordinate space GetTissueCoordinates reports (full-resolution pixels).
  ## The stored raster is the full-res image scaled by the `lowres` factor, so
  ## it covers full-res px [0, W/lowres] x [0, H/lowres] — that, not the spot
  ## bounding box, is the image's extent in coordinate space.
  img_obj <- obj[[Images(obj)[1]]]
  he_raster <- slot(img_obj, "image") # H x W x 3, values 0..1

  ## Visium is the one demo that uses an EXTERNAL background image (loaded via
  ## `spatial_images` in inst/app.R) rather than embedding it in the .crb — a live
  ## example of that code path, and it keeps the Visium .crb small. Write the
  ## real H&E to a standalone PNG in the same extdata dir the .crb ships in. The
  ## raster is stored native (row 0 = image top); the app applies the vertical
  ## flip via `spatial_images_flip_y = TRUE` (ground-truth verified against
  ## Seurat's own SpatialPlot, same value as the embedded flag would be).
  save_raster_png(
    he_raster,
    file = file.path(out_dir, "demo_spatial_visium_he.png"),
    max_px = 1200
  )

  ## Visium counts are denser than the imaging panels, so trim harder (1200
  ## genes) to keep the shipped .crb ~6 MB.
  obj <- process_seurat(obj, assay, max_genes = 1200)
  ## Cluster to give the Spatial tab a categorical grouping to colour by.
  obj <- FindNeighbors(obj, dims = 1:20, verbose = FALSE)
  obj <- FindClusters(obj, resolution = 0.5, verbose = FALSE)
  obj$cluster <- factor(paste0("C", as.integer(obj$seurat_clusters)))
  keep <- sample_cells(setNames(obj$cluster, colnames(obj)), MAX_CELLS)
  obj <- subset(obj, cells = keep)
  ## No `image =` here: the background is the external PNG above, not embedded.
  export_and_verify(
    obj,
    assay,
    groups = c("cluster"),
    file = file.path(out_dir, "demo_spatial_visium.crb"),
    experiment_name = "Visium mouse brain (anterior)",
    organism = "mm"
  )
}

##----------------------------------------------------------------------------##
## 2. Slide-seq v2  (SeuratData::ssHippo)  -- Seurat v4 SlideSeq object
##----------------------------------------------------------------------------##
build_slideseq <- function() {
  message("== Slide-seq v2 (ssHippo) == [v4 SlideSeq object]")
  suppressPackageStartupMessages(library(SeuratData))
  data("ssHippo", package = "ssHippo.SeuratData")
  obj <- get("ssHippo")
  ## ssHippo ships as a Seurat v3.1.4 object whose `SlideSeq` image predates the
  ## current class definition (missing the `misc` slot); repair it before any
  ## subset/validObject call. The object stays a v4-era SlideSeq image, which is
  ## the whole point: it exercises the non-v5 extraction path.
  obj <- UpdateSeuratObject(obj)
  assay <- "Spatial"
  ## Down-sample first: 53k beads is large and unclustered here.
  keep <- sample(colnames(obj), min(MAX_CELLS, ncol(obj)))
  obj <- subset(obj, cells = keep)
  obj <- process_seurat(obj, assay)
  obj <- FindNeighbors(obj, dims = 1:20, verbose = FALSE)
  obj <- FindClusters(obj, resolution = 0.5, verbose = FALSE)
  obj$cluster <- factor(paste0("C", as.integer(obj$seurat_clusters)))
  export_and_verify(
    obj,
    assay,
    groups = c("cluster"),
    file = file.path(out_dir, "demo_spatial_slideseq.crb"),
    experiment_name = "Slide-seq v2 mouse hippocampus",
    organism = "mm"
  )
}

##----------------------------------------------------------------------------##
## 3. MERFISH  (MerfishData::MouseIleumPetukhov2021)  -- imaging + REAL DAPI
##
## This dataset ships a genuine DAPI tissue mosaic alongside the cells, and the
## cell coordinates are already in the DAPI pixel space, so the real image aligns
## to the points almost 1:1. That is why it replaces the earlier Moffitt
## hypothalamus set (which carried no image).
##----------------------------------------------------------------------------##
build_merfish <- function() {
  message("== MERFISH (Petukhov 2021 mouse ileum) == [real DAPI mosaic]")
  suppressPackageStartupMessages({
    library(MerfishData)
    library(SpatialExperiment)
    library(SummarizedExperiment)
  })
  spe <- MerfishData::MouseIleumPetukhov2021()
  ## This SPE ships without cell names; mint stable synthetic ids and apply them
  ## consistently to the matrix, metadata and coordinates.
  cell_ids <- sprintf("cell%05d", seq_len(ncol(spe)))
  colnames(spe) <- cell_ids
  cd <- as.data.frame(colData(spe))
  xy <- as.data.frame(spatialCoords(spe))[, c("x", "y")]
  rownames(cd) <- cell_ids
  rownames(xy) <- cell_ids

  ## Real DAPI mosaic, stored in its NATIVE orientation (row 0 = image top). The
  ## build never flips the raster; if a display flip is ever needed it is a user
  ## control in the Spatial tab (see the note in spatial.md). MERFISH is verified
  ## to need NO flip.
  dapi <- imgRaster(getImg(spe, image_id = "dapi"))
  img_h <- nrow(dapi)
  img_w <- ncol(dapi)
  dapi_uri <- encode_raster_png(dapi, max_px = 1400)
  dapi_bounds <- list(xmin = 0, xmax = img_w, ymin = 0, ymax = img_h)

  ## Keep only cells that were actually assigned a real type ("Removed" is the
  ## dataset's discard label).
  keep <- cell_ids[!is.na(cd$leiden_final) & cd$leiden_final != "Removed"]
  if (length(keep) > MAX_CELLS) {
    keep <- sample_cells(
      setNames(as.character(cd[keep, "leiden_final"]), keep),
      MAX_CELLS
    )
  }

  mat <- as(assay(spe, "counts")[, keep, drop = FALSE], "CsparseMatrix")
  obj <- CreateSeuratObject(counts = mat, assay = "Spatial")
  obj$cell_type <- factor(as.character(cd[keep, "leiden_final"]))
  obj$x <- xy[keep, "x"]
  obj$y <- xy[keep, "y"]

  ## Attach a proper FOV image so extraction runs via GetTissueCoordinates,
  ## exactly as a LoadVizgen/LoadXenium object would.
  cents <- CreateCentroids(as.matrix(xy[keep, c("x", "y")]))
  fov <- CreateFOV(
    list(centroids = cents),
    type = "centroids",
    assay = "Spatial"
  )
  obj[["fov"]] <- fov

  assay <- "Spatial"
  obj <- process_seurat(obj, assay)
  export_and_verify(
    obj,
    assay,
    groups = c("cell_type"),
    file = file.path(out_dir, "demo_spatial_merfish.crb"),
    experiment_name = "MERFISH mouse ileum",
    organism = "mm",
    image = dapi_uri,
    image_bounds = dapi_bounds
  )
}

##----------------------------------------------------------------------------##
## 4. 10x Xenium  (public mouse brain coronal CTX+HP section)  -- imaging + DAPI
##
## In-situ single-cell imaging, like MERFISH, so it flows through the same
## FOV/Centroids -> GetTissueCoordinates route. Built from the raw 10x output
## bundle (cell_feature_matrix.h5 + cells.csv.gz + a morphology OME-TIFF) rather
## than LoadXenium, because the bundle ships the .h5 matrix (not the mtx
## directory LoadXenium expects) and stores transcripts as parquet (needs
## `arrow`). This is a REAL tissue section (cortex + hippocampus) whose
## morphology shows recognisable brain structure, not a rectangular field-of-view
## crop. Cells sit in MICRON coordinates; the morphology raster is in pixels, so
## its extent in coordinate space is dims * pixel_size (um/px).
##----------------------------------------------------------------------------##
build_xenium <- function() {
  message(
    "== Xenium (10x mouse brain coronal CTX+HP) == [real DAPI morphology]"
  )
  dir <- "data-raw/xenium/brain"
  h5 <- file.path(dir, "cell_feature_matrix.h5")
  cells_csv <- file.path(dir, "cells.csv.gz")
  ## Auto-fetch the raw 10x outs bundle (~3.5 GB) if not already present, so the
  ## whole link -> .crb pipeline runs from one command. `unzip` extracts the outs
  ## contents directly into `dir`; cell_feature_matrix.h5 is the sentinel.
  xenium_url <- paste0(
    "https://cf.10xgenomics.com/samples/xenium/1.0.2/",
    "Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP/",
    "Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP_outs.zip"
  )
  ensure_unzipped(
    url = xenium_url,
    zip_dest = "data-raw/xenium/brain.zip",
    unzip_dir = dir,
    sentinel = h5
  )
  if (!file.exists(h5) || !file.exists(cells_csv)) {
    message(
      "  SKIP: Xenium bundle unavailable (download failed?) under ",
      dir,
      " (see DATASETS.md acquire)"
    )
    return(invisible(NULL))
  }
  assay <- "Spatial"

  mat <- Read10X_h5(h5)
  if (is.list(mat)) {
    mat <- mat[["Gene Expression"]]
  }
  cells <- read.csv(gzfile(cells_csv))
  rownames(cells) <- cells$cell_id
  common <- intersect(colnames(mat), cells$cell_id)
  mat <- mat[, common]
  cells <- cells[common, ]

  obj <- CreateSeuratObject(counts = mat, assay = assay)
  obj$nUMI <- obj[[paste0("nCount_", assay)]][, 1]
  obj$nGene <- obj[[paste0("nFeature_", assay)]][, 1]
  xy <- data.frame(
    x = cells$x_centroid,
    y = cells$y_centroid,
    row.names = common
  )
  fin <- is.finite(xy$x) & is.finite(xy$y)
  obj <- subset(obj, cells = rownames(xy)[fin])
  xy <- xy[colnames(obj), ]

  obj <- process_seurat(obj, assay)
  obj <- FindNeighbors(obj, dims = 1:20, verbose = FALSE)
  obj <- FindClusters(obj, resolution = 0.5, verbose = FALSE)
  obj$cluster <- factor(paste0("C", as.integer(obj$seurat_clusters)))
  keep <- sample_cells(setNames(obj$cluster, colnames(obj)), MAX_CELLS)
  obj <- subset(obj, cells = keep)
  xy <- xy[colnames(obj), ]

  cents <- CreateCentroids(as.matrix(xy[colnames(obj), c("x", "y")]))
  obj[["fov"]] <- CreateFOV(
    list(centroids = cents),
    type = "centroids",
    assay = assay
  )
  colour_xy <- data.frame(
    x = -xy$y,
    y = xy$x,
    row.names = rownames(xy)
  )
  obj[["fov_colour"]] <- CreateFOV(
    list(centroids = CreateCentroids(as.matrix(colour_xy))),
    type = "centroids",
    assay = assay
  )

  ## Extract the REAL DAPI morphology channel via RBioFormats (pure R).
  ## pixel_size (um/px) comes from experiment.xenium; fall back to the documented
  ## 0.2125 if that file is absent.
  exp_file <- file.path(dir, "experiment.xenium")
  um_per_px <- 0.2125
  if (file.exists(exp_file)) {
    exp <- tryCatch(jsonlite::fromJSON(exp_file), error = function(e) NULL)
    if (!is.null(exp$pixel_size)) {
      um_per_px <- exp$pixel_size
    }
  }
  ## Newer XOA bundles ship a single `morphology_focus.ome.tif`; older layouts
  ## use `morphology_focus/morphology_focus_0000.ome.tif`. Accept either.
  ome <- file.path(dir, "morphology_focus.ome.tif")
  if (!file.exists(ome)) {
    ome <- file.path(dir, "morphology_focus", "morphology_focus_0000.ome.tif")
  }
  if (file.exists(ome)) {
    res <- extract_ome_tiff_channel(
      ome,
      um_per_px,
      channel = 1L,
      max_px = 1400L
    )
    if (!is.null(res)) {
      image_dir <- file.path(out_dir, "spatial", "xenium")
      save_raster_png(
        res$raster,
        file.path(image_dir, "dapi.png"),
        max_px = 1400L
      )
      save_raster_png(
        tint_and_rotate_raster(res$raster, "#ff2d8d"),
        file.path(image_dir, "pink_stain_90.png"),
        max_px = 1400L
      )
      save_raster_png(
        tint_and_rotate_raster(res$raster, "#edff00"),
        file.path(image_dir, "fluorescent_yellow_90.png"),
        max_px = 1400L
      )
    }
  }

  built <- export_and_verify(
    obj,
    assay,
    groups = c("cluster"),
    file = file.path(out_dir, "demo_spatial_xenium.crb"),
    experiment_name = "Xenium mouse brain (CTX+HP)",
    organism = "mm"
  )
  stopifnot(
    "Xenium spatial variants missing" = setequal(
      built$availableSpatial(),
      c("fov", "fov_colour")
    )
  )
}

##----------------------------------------------------------------------------##
## run all
##----------------------------------------------------------------------------##
build_visium()
build_slideseq()
build_merfish()
build_xenium()

message("\nAll spatial demo .crb files rebuilt in ", out_dir)
