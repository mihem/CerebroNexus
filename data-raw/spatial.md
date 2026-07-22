# Spatial demos — design and rebuild notes

Provenance of record (citation, licence, sampling, output size) lives in [`DATASETS.md`](DATASETS.md).
This file is the working guide: what to install or download, what each step does to the data, and the code that does it.
Every command is meant to be copy-pasted and run from the package root.

## Contents

1. [What ships](#1-what-ships)
2. [Why these four](#2-why-these-four)
3. [Rebuild, end to end](#3-rebuild-end-to-end)
   - [3.1 Install the sources](#31-install-the-sources)
   - [3.2 Run the build](#32-run-the-build)
   - [3.3 The shared spine: four steps every platform goes through](#33-the-shared-spine-four-steps-every-platform-goes-through)
   - [3.4 Visium — external image, harder gene trim](#34-visium--external-image-harder-gene-trim)
   - [3.5 Slide-seq v2 — the Seurat v4 path](#35-slide-seq-v2--the-seurat-v4-path)
   - [3.6 MERFISH — a real DAPI mosaic, coordinates already in pixel space](#36-merfish--a-real-dapi-mosaic-coordinates-already-in-pixel-space)
   - [3.7 Xenium — raw outs, microns vs pixels, JPEG2000](#37-xenium--raw-outs-microns-vs-pixels-jpeg2000)
4. [Image ↔ point alignment](#4-image--point-alignment)
   - [4.1 The flip decision (and why the brightness score was abandoned)](#41-the-flip-decision-and-why-the-brightness-score-was-abandoned)
   - [4.2 Aspect lock at render time](#42-aspect-lock-at-render-time)
5. [Why Slide-seq has no background image](#5-why-slide-seq-has-no-background-image)
6. [Try it](#6-try-it)

---

# 1. What ships

Genuinely measured, public spatial-transcriptomics demos. Each is a down-sampled subset of a **different** technology, so the dropdown demonstrates that one platform-agnostic extraction pipeline (`.getSpatialData`, `R/seurat_utils.R`) ingests them all — there is no per-platform loader.

| File | Technology (dropdown label) | Public source | Cells | Real tissue image |
|---|---|---|---|---|
| `demo_spatial_visium.crb` | Mouse brain (Visium) | `SeuratData::stxBrain` (anterior1) | 2,696 | ✅ H&E, **external** PNG |
| `demo_spatial_slideseq.crb` | Mouse hippocampus (Slide-seq v2) | `SeuratData::ssHippo` | 5,000 | — (structural, see §5) |
| `demo_spatial_merfish.crb` | Mouse ileum (MERFISH) | `MerfishData::MouseIleumPetukhov2021` | 5,000 | ✅ DAPI mosaic, **embedded** |
| `demo_spatial_xenium.crb` | Mouse brain (Xenium) | 10x `Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP` | 5,000 | ✅ DAPI morphology, **embedded** |

They deliberately exercise **both** background-image paths the app supports:

- **MERFISH / Xenium** embed the image in the `.crb` under `histology_image`, with its extent in coordinate space under `histology_image_bounds`. The Spatial tab offers it as "Tissue background (H&E / DAPI)".
- **Visium** loads its H&E from an *external* PNG (`demo_spatial_visium_he.png`) via `spatial_images` in `inst/app.R` — a live example of that path, which also keeps the Visium `.crb` smaller. The tab offers it by filename.

`demo_spatial.crb` + `demo_spatial_histology.svg` are lightweight **synthetic** fixtures used only by `test-spatial.R`; they are intentionally not in the app's dropdown.

# 2. Why these four

They span the structural axes that decide whether the extraction pipeline copes with a platform.

**Spot vs bead vs in-situ imaging** — i.e. where the coordinates live:

| platform | unit | coordinates come from |
|---|---|---|
| Visium | 55 µm spots | the `VisiumV2` image (`imagerow` / `imagecol`) |
| Slide-seq v2 | 10 µm beads | the `SlideSeq` image slot (`x` / `y`) |
| MERFISH | single cells | `GetTissueCoordinates` on an `FOV` with `Centroids` |
| Xenium | single cells | same `FOV`/`Centroids` route, but built from raw outs; cells in **microns** while the raster is in **pixels** |

**Seurat v4 vs v5**: `ssHippo` ships as a Seurat v3.1.4-era `SlideSeq` object. It is the end-to-end proof that `exportFromSeurat` handles non-v5 spatial objects — an old blanket `is_seurat_v5 &&` gate used to drop these entirely.

**Package-shipped vs network-fetched**: Visium / Slide-seq / MERFISH come from R packages; Xenium fetches a 3.5 GB zip from the 10x CDN, auto-downloaded on first run.

---

# 3. Rebuild, end to end

## 3.1 Install the sources

Three of the four datasets are R packages, so "downloading" them is an install:

```bash
cd "$(git rev-parse --show-toplevel)"

Rscript -e '
  install.packages(c("Seurat", "jsonlite", "png"))
  remotes::install_github("satijalab/seurat-data")
  SeuratData::InstallData("stxBrain")     # Visium mouse brain
  SeuratData::InstallData("ssHippo")      # Slide-seq v2 hippocampus
  BiocManager::install(c("MerfishData", "SpatialExperiment", "RBioFormats"))
'
```

`RBioFormats` is needed only for Xenium: its DAPI morphology is a **JPEG2000-compressed OME-TIFF**, which R's `tiff` / `EBImage` cannot decode. `RBioFormats` is a pure-R wrapper over the Java Bio-Formats library (no Python). Without it the Xenium demo still builds — it just ships coordinates and no image.

Xenium's raw data is fetched by the build itself, but the equivalent by hand is:

```bash
mkdir -p data-raw/xenium
BASE=https://cf.10xgenomics.com/samples/xenium/1.0.2/Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP
curl -fL --retry 5 -C - -o data-raw/xenium/brain.zip \
  "$BASE/Xenium_V1_FF_Mouse_Brain_Coronal_Subset_CTX_HP_outs.zip"
unzip -oq data-raw/xenium/brain.zip -d data-raw/xenium/brain
```

## 3.2 Run the build

```bash
Rscript data-raw/build_spatial_demos.R
```

One command runs the whole link → `.crb` pipeline for all four. `set.seed(42)` makes it deterministic. The Xenium download is skipped when `cell_feature_matrix.h5` is already present (that file is the sentinel), and the whole Xenium build no-ops with a message if the download fails, rather than aborting the other three.

## 3.3 The shared spine: four steps every platform goes through

Each `build_*()` differs only in how it *loads* the data and whether it carries an image. The middle is shared.

**(a) Stratified down-sample to ~5,000 cells.** Proportional within each group, so rare clusters survive:

```r
sample_cells <- function(groups, max_cells) {
  cells <- names(groups)
  if (length(cells) <= max_cells) return(cells)
  frac <- max_cells / length(cells)
  keep <- unlist(lapply(split(cells, groups), function(cc) {
    n <- max(1L, round(length(cc) * frac))     # max(1L, ...): never drop a group entirely
    sample(cc, min(n, length(cc)))
  }), use.names = FALSE)
  if (length(keep) > max_cells) keep <- sample(keep, max_cells)   # trim rounding overshoot
  keep
}
```

**(b) Light Seurat processing**, so `exportFromSeurat` has a `data` layer, a non-PCA reduction and QC columns:

```r
process_seurat <- function(obj, assay, max_genes = 2000) {
  DefaultAssay(obj) <- assay
  # computed on the FULL gene set, BEFORE trimming, so they stay meaningful
  obj$nUMI  <- obj[[paste0("nCount_",   assay)]][, 1]
  obj$nGene <- obj[[paste0("nFeature_", assay)]][, 1]
  obj <- NormalizeData(obj, assay = assay, verbose = FALSE)
  obj <- FindVariableFeatures(obj, assay = assay, nfeatures = min(2000L, nrow(obj)), verbose = FALSE)

  if (nrow(obj) > max_genes) {          # trim the embedded matrix: Visium's 31k genes
    counts <- GetAssayData(obj, assay = assay, layer = "counts")   # would embed ~70 MB
    top_expressed <- names(sort(Matrix::rowSums(counts), decreasing = TRUE))
    keep_genes <- head(unique(c(VariableFeatures(obj), head(top_expressed, max_genes))), max_genes)
    obj <- subset(obj, features = keep_genes)
    obj <- FindVariableFeatures(obj, assay = assay, nfeatures = min(2000L, nrow(obj)), verbose = FALSE)
  }

  obj  <- ScaleData(obj, assay = assay, verbose = FALSE)
  npcs <- min(30L, ncol(obj) - 1L, nrow(obj) - 1L)
  obj  <- RunPCA(obj, assay = assay, npcs = npcs, verbose = FALSE)
  RunUMAP(obj, dims = seq_len(min(20L, npcs)), verbose = FALSE)
}
```

Small imaging panels (MERFISH 241 genes, Xenium 248) fall under `max_genes` and are kept whole.

**(c) Encode the image**, when there is one — downscaled and base64'd into a `data:` URI so it travels inside the `.crb`:

```r
encode_raster_png(arr, max_px = 1400)     # -> "data:image/png;base64,..."
```

Every embedded image is stored in its **native** orientation (row 0 = image top). The build never flips; display orientation is a user control (§4.1).

**(d) Export, inject the image, and verify.** The image cannot be passed to `exportFromSeurat` — it is added to the spatial slot afterwards, under a **dedicated key**, because `.getSpatialData` already stores the image *name* under `image` and overloading that would clobber it:

```r
exportFromSeurat(object = obj, assay = assay, slot = "data", file = file,
                 experiment_name = experiment_name, organism = organism,
                 groups = groups, nUMI = "nUMI", nGene = "nGene", verbose = TRUE)

crb <- readRDS(file)
stopifnot("no spatial data written to .crb" = length(crb$availableSpatial()) > 0)

if (!is.null(image)) {
  for (nm in crb$availableSpatial()) {
    sd <- crb$getSpatialData(nm)
    sd$histology_image        <- image           # dedicated key, NOT `image`
    sd$histology_image_bounds <- image_bounds    # extent in COORDINATE space
    crb$addSpatialData(nm, sd)
  }
  saveRDS(crb, file)
}

sd <- crb$getSpatialData(crb$availableSpatial()[1]); coords <- sd$coordinates
stopifnot(
  "spatial coordinates missing x/y"  = all(c("x", "y") %in% colnames(coords)),
  "spatial coordinates empty"        = nrow(coords) > 0,
  "coords/expression cell mismatch"  = length(intersect(rownames(coords),
                                                       colnames(sd$expression))) > 0
)
```

That last assertion is the one that matters: coordinates and expression are filtered along different paths, and a mismatch produces a `.crb` that opens fine and plots nothing.

## 3.4 Visium — external image, harder gene trim

```r
obj      <- SeuratData::LoadData("stxBrain", type = "anterior1")
img_obj  <- obj[[Images(obj)[1]]]
he_raster <- slot(img_obj, "image")            # H x W x 3, values 0..1

# the ONE demo that writes an external PNG instead of embedding
save_raster_png(he_raster, file = file.path(out_dir, "demo_spatial_visium_he.png"), max_px = 1200)

obj <- process_seurat(obj, "Spatial", max_genes = 1200)   # denser than imaging panels
obj <- FindNeighbors(obj, dims = 1:20, verbose = FALSE)
obj <- FindClusters(obj, resolution = 0.5, verbose = FALSE)   # source ships no cell types
obj$cluster <- factor(paste0("C", as.integer(obj$seurat_clusters)))
obj <- subset(obj, cells = sample_cells(setNames(obj$cluster, colnames(obj)), MAX_CELLS))
export_and_verify(obj, "Spatial", groups = c("cluster"), ...)   # note: no image = argument
```

The stored raster is the full-res image scaled by the `lowres` factor, so it covers full-res px `[0, W/lowres] × [0, H/lowres]` — **that**, not the spot bounding box, is its extent in coordinate space.

## 3.5 Slide-seq v2 — the Seurat v4 path

```r
data("ssHippo", package = "ssHippo.SeuratData")
obj <- get("ssHippo")
obj <- UpdateSeuratObject(obj)          # REQUIRED before any subset/validObject
obj <- subset(obj, cells = sample(colnames(obj), min(MAX_CELLS, ncol(obj))))
obj <- process_seurat(obj, "Spatial")
```

`ssHippo` is a Seurat v3.1.4 object whose `SlideSeq` image predates the current class definition (it is missing the `misc` slot), so any `subset()` or `validObject()` fails before the repair. After `UpdateSeuratObject()` it stays a v4-era `SlideSeq` image — which is the point: it exercises the non-v5 extraction path.

Also note the down-sample happens **first** here, not after clustering: 53k unclustered beads are expensive to process and there is no cluster structure to stratify on yet.

## 3.6 MERFISH — a real DAPI mosaic, coordinates already in pixel space

```r
spe <- MerfishData::MouseIleumPetukhov2021()
cell_ids <- sprintf("cell%05d", seq_len(ncol(spe)))   # the SPE ships WITHOUT cell names
colnames(spe) <- cell_ids                             # apply consistently to matrix,
cd <- as.data.frame(colData(spe));  rownames(cd) <- cell_ids       # metadata
xy <- as.data.frame(spatialCoords(spe))[, c("x","y")]; rownames(xy) <- cell_ids  # and coords

dapi        <- imgRaster(getImg(spe, image_id = "dapi"))
dapi_uri    <- encode_raster_png(dapi, max_px = 1400)
dapi_bounds <- list(xmin = 0, xmax = ncol(dapi), ymin = 0, ymax = nrow(dapi))

keep <- cell_ids[!is.na(cd$leiden_final) & cd$leiden_final != "Removed"]   # dataset's discard label
keep <- sample_cells(setNames(as.character(cd[keep, "leiden_final"]), keep), MAX_CELLS)

obj <- CreateSeuratObject(counts = as(assay(spe, "counts")[, keep], "CsparseMatrix"), assay = "Spatial")
obj$cell_type <- factor(as.character(cd[keep, "leiden_final"]))
# attach a real FOV so extraction runs via GetTissueCoordinates, exactly as a
# LoadVizgen/LoadXenium object would
obj[["fov"]] <- CreateFOV(list(centroids = CreateCentroids(as.matrix(xy[keep, c("x","y")]))),
                          type = "centroids", assay = "Spatial")
```

The cell coordinates are **already in the DAPI pixel space**, so image and points align almost 1:1 — the bounds are simply the raster dimensions. This is why this dataset replaced the earlier Moffitt hypothalamus set, which carried no image. It also ships real cell-type labels (`leiden_final`), so no clustering step is needed.

## 3.7 Xenium — raw outs, microns vs pixels, JPEG2000

Built from the raw bundle rather than `LoadXenium`, which expects an mtx directory (the bundle ships `.h5`) and reads transcripts from parquet (needs `arrow`):

```r
mat   <- Read10X_h5(file.path(dir, "cell_feature_matrix.h5"))
if (is.list(mat)) mat <- mat[["Gene Expression"]]
cells <- read.csv(gzfile(file.path(dir, "cells.csv.gz")));  rownames(cells) <- cells$cell_id
common <- intersect(colnames(mat), cells$cell_id)
mat <- mat[, common];  cells <- cells[common, ]

xy  <- data.frame(x = cells$x_centroid, y = cells$y_centroid, row.names = common)
obj <- subset(obj, cells = rownames(xy)[is.finite(xy$x) & is.finite(xy$y)])
```

The coordinate-space conversion is what makes this platform different — cells are in **microns**, the raster in **pixels**:

```r
um_per_px <- 0.2125                                   # documented default
exp_file  <- file.path(dir, "experiment.xenium")
if (file.exists(exp_file)) {
  exp <- tryCatch(jsonlite::fromJSON(exp_file), error = function(e) NULL)
  if (!is.null(exp$pixel_size)) um_per_px <- exp$pixel_size   # prefer the run's own value
}

# newer XOA bundles ship one morphology_focus.ome.tif; older ones a subdirectory
ome <- file.path(dir, "morphology_focus.ome.tif")
if (!file.exists(ome)) ome <- file.path(dir, "morphology_focus", "morphology_focus_0000.ome.tif")

res <- extract_ome_tiff_channel(ome, um_per_px, channel = 1L, max_px = 1400L)
img_uri    <- encode_raster_png(res$raster, max_px = 1400L)
img_bounds <- res$bounds                              # dims * pixel_size -> microns
```

So `histology_image_bounds` is `dims × pixel_size`, not the raw pixel dimensions — the conversion Visium and MERFISH do not need.

---

# 4. Image ↔ point alignment

Getting the real image to line up with the points needs two things, because the Spatial tab draws the background as a **DOM layer stretched to fill the plot's drawing area** — it is not a Plotly `layout.image`.

## 4.1 The flip decision (and why the brightness score was abandoned)

The image is always stored native (row 0 = image top); the renderer draws it top-down while Plotly's y-axis grows upward. Whether a display flip is needed is **not uniform** — it depends on how a dataset's point y relates to its image rows, which differs by platform (`GetTissueCoordinates` vs a raw `y_centroid` vs `MerfishData::imgRaster`).

There is **no stored per-`.crb` flip flag**. The user aligns with the Spatial tab's "Flip vertically/horizontally" checkboxes (`func_projection_update_plot.R` → `js_projection_update_plot.js`, via a background `scale(1, -1)`); external images can be pre-flipped with `spatial_images_flip_y` in `inst/app.R`.

Correct orientation is judged by **visual comparison against a native ground-truth reference**: overlay the real centroids on the source raster in its native frame, pick an unambiguous anatomical landmark, then flip in the app until it matches.

| demo | flip | landmark |
|---|---|---|
| Visium (mouse brain) | **vertical flip** | olfactory bulb lower-left, tissue body lower-middle — matches Seurat's own `SpatialPlot` |
| Xenium (brain CTX+HP) | none | hippocampus lower-right, pial surface along the top |
| MERFISH (mouse ileum) | none | villi orientation, confirmed against the native DAPI |

Automated point-on-tissue "brightness" scores were tried and proved **unreliable** — dense tissue like the Xenium brain defeats them. Landmark comparison is the standard. When adding a new image demo: build a native-frame centroid overlay, pick a landmark, flip in the app until it matches. Do not apply a blanket rule.

## 4.2 Aspect lock at render time

Stretch-to-fill would squash a non-square image (the MERFISH DAPI mosaic is ~0.6:1, tall). When an embedded image is active the renderer sets `yaxis.scaleanchor = 'x'` — `func_projection_update_plot.R` flags `is_embedded`, `js_projection_update_plot.js` applies the lock — so the drawing area keeps the image's width:height and the stretch stays proportional. Non-embedded projections (UMAP etc.) are untouched.

# 5. Why Slide-seq has no background image

Structural, not an oversight: the platform records positions, not a tissue photo. The `SlideSeq` S4 class in SeuratObject has only a `coordinates` slot — it carries no tissue raster, unlike Visium's `VisiumV2` (`image` slot) or the imaging `FOV`. Slide-seq *does* image beads, but only to recover their positions; the public `ssHippo` object stores those coordinates, not an H&E/DAPI photo.

The bead scatter therefore **is** the complete spatial view.

# 6. Try it

```r
library(cerebroAppLite)
createShinyApp(
  cerebro_data = c(
    "Mouse brain (Visium)"             = system.file("extdata/v1.4/demo_spatial_visium.crb",   package = "cerebroAppLite"),
    "Mouse hippocampus (Slide-seq v2)" = system.file("extdata/v1.4/demo_spatial_slideseq.crb", package = "cerebroAppLite"),
    "Mouse ileum (MERFISH)"            = system.file("extdata/v1.4/demo_spatial_merfish.crb",  package = "cerebroAppLite"),
    "Mouse brain (Xenium)"             = system.file("extdata/v1.4/demo_spatial_xenium.crb",   package = "cerebroAppLite")
  )
)
```

Selecting any of them reveals the conditional **Spatial** tab, which renders the real coordinates coloured by cluster / cell type, over the real tissue image where one exists.
