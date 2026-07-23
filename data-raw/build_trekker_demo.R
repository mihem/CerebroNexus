##----------------------------------------------------------------------------##
## build_trekker_demo.R
##
## Reproducible build of the Trekker single-cell spatial-mapping demo `.crb`
## shipped in inst/extdata/v1.4/ for the **Trekker** tab.
##
## WHAT TREKKER IS
##   Trekker (Curio Bioscience / Takara Bio, "Trekker Single-Cell Spatial
##   Mapping Kit") tags cell nuclei with location barcodes from known-position
##   beads, then recovers each nucleus's 2-D position from single-nucleus
##   sequencing. Unlike Visium (spots) or Xenium/MERFISH (gene panels) it gives
##   REAL single nuclei x WHOLE transcriptome, usually WITHOUT a matched
##   histology image. The vendor pipeline outputs a Seurat RDS + several CSV/MTX
##   companion files; CerebroNexus only INGESTS and DISPLAYS that output, it
##   never runs the vendor pipeline.
##
## DATA SOURCE (registration required, not redistributable here)
##   The raw bundles are NOT public downloads: you register for a Curio /
##   Takara account and request access to the Trekker example data, then
##   download the per-sample `.tar.gz`. This demo uses the SMALLEST bundle:
##       Mouse_Brain_TrekkerU_C_Sept2025.tar.gz   (~1.3 GB compressed)
##   See data-raw/trekker.md for the full download / extraction walk-through.
##   The bundle is gitignored; only the derived, down-sampled .crb ships.
##
## WHAT THIS SCRIPT PRODUCES
##   inst/extdata/v1.4/demo_trekker.crb  (target: <= 5 MB, self-contained)
##   A proper Cerebro_v1.3 object (whole-transcriptome expression + UMAP +
##   cell-type / cluster groups) PLUS a `trekker` slot carrying the Trekker
##   page's content: three measured coordinate orientations, positioning QC in
##   the vendor's own field names, the upstream (vendor) Moran's I table, and
##   base64-embedded per-nucleus positioning-evidence images. Everything lives
##   in the one .crb so the 5 MB budget covers crb + evidence together.
##
## SUB-SAMPLING (why not all 7,420 nuclei)
##   The ConfPositioned object is 21,374 genes x 7,420 nuclei. Embedding whole-
##   transcriptome expression for all of them blows the 5 MB budget (measured:
##   ~3.8 MB at 2,500 nuclei, before images). We keep ALL genes (the whole-
##   transcriptome property is the point) and DOWN-SAMPLE nuclei, stratified by
##   cluster, to N_CELLS. The 50 nuclei that carry official positioning-evidence
##   images are FORCE-INCLUDED so the evidence drill-down still works.
##
## Run from the package root, after extracting the bundle (see trekker.md):
##   TREKKER_OUTPUT_DIR=/path/to/Mouse_Brain_TrekkerU_C_Sept2025/output \
##     Rscript data-raw/build_trekker_demo.R
##
## data-raw/ is excluded from the built package via .Rbuildignore; it lives in
## the repo for reproducibility only. Build-only dependencies (not package
## Imports): `magick` (down-scale the evidence JPEGs), `FNN` (fast kNN for the
## cross-space metrics), `base64enc` (embed images). Seurat's AddModuleScore
## provides the demo's myelination signature meta column.
##----------------------------------------------------------------------------##

suppressPackageStartupMessages({
  library(Seurat)
  library(Matrix)
})

## Load the in-tree package so the local class edits (the new `trekker` slot and
## addTrekker/getTrekker) are exercised, not an installed copy.
pkgload::load_all(".", quiet = TRUE)

set.seed(42)

##----------------------------------------------------------------------------##
## config
##----------------------------------------------------------------------------##
SAMPLE <- "Mouse_Brain_TrekkerU_C"
BASE <- Sys.getenv(
  "TREKKER_OUTPUT_DIR",
  unset = file.path(
    "data-raw",
    "trekker",
    "Mouse_Brain_TrekkerU_C_Sept2025",
    "output"
  )
)
OUT_DIR <- "inst/extdata/v1.4"
CRB <- file.path(OUT_DIR, "demo_trekker.crb")

N_CELLS <- 2500L # down-sampled nucleus count (whole transcriptome kept)
EV_MAX_PX <- 620L # evidence JPEG long-edge after down-scaling
EV_QUALITY <- 68L # evidence JPEG quality

if (!dir.exists(BASE)) {
  stop(
    "Trekker bundle output dir not found: ",
    BASE,
    "\nDownload + extract the bundle first (see data-raw/trekker.md), then set ",
    "TREKKER_OUTPUT_DIR to <bundle>/output.",
    call. = FALSE
  )
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

f <- function(suffix) file.path(BASE, paste0(SAMPLE, suffix))

##----------------------------------------------------------------------------##
## cluster -> cell type
##
## The vendor object ships 18 unnamed Louvain clusters (SCT_snn_res.0.2). These
## labels were assigned once by marker z-score argmax over canonical mouse-brain
## markers (Snap25/Slc17a7 = ExN, Gad1/Gad2 = InN, Plp1/Mbp = Oligo, Aqp4/Gfap =
## Astro, Cx3cr1/C1qa/Csf1r = Micro, Pdgfra = OPC, Prox1 = DG); clusters without
## a clear winner are honestly left as the generic "Neuron" rather than
## over-called. Hard-coded here (indexed by 0-based cluster id) so the build is
## deterministic and needs no marker recomputation.
##----------------------------------------------------------------------------##
CELLTYPE_BY_CLUSTER <- c(
  "ExN",
  "InN",
  "Oligo",
  "ExN",
  "Astro",
  "InN",
  "DG",
  "ExN",
  "ExN",
  "Neuron",
  "OPC",
  "Micro",
  "Neuron",
  "DG",
  "ExN",
  "Neuron",
  "ExN",
  "Neuron"
)

##----------------------------------------------------------------------------##
## read vendor outputs
##----------------------------------------------------------------------------##
message("reading ConfPositioned Seurat RDS ...")
so <- readRDS(f("_ConfPositioned_seurat_spatial.rds"))
DefaultAssay(so) <- "SCT"
## The vendor object carries a legacy `SlideSeq` @images entry that predates the
## `misc` slot, so any subset()/validObject() on it errors (documented vendor
## quirk). We take coordinates from the Location CSV, never from @images, so the
## image is dead weight -- drop it up front to make the object subsettable.
so@images <- list()
bc_all <- colnames(so)
n_all <- length(bc_all)

## canonical coordinates: the vendor Location CSV is the coordinate authority
## (plain text, no Seurat-version coupling, and what the vendor Report plots).
loc <- read.csv(f("_Location_ConfPositionedNuclei.csv"))
names(loc)[1] <- "barcode"
loc <- loc[match(bc_all, loc$barcode), ]
stopifnot(!anyNA(loc$SPATIAL_1))
cx <- loc$SPATIAL_1
cy <- loc$SPATIAL_2

## UMAP
um <- Embeddings(so, "umap")

## 0-based cluster id per nucleus
clab <- as.integer(as.character(so@meta.data$seurat_clusters))
stopifnot(max(clab) < length(CELLTYPE_BY_CLUSTER))

##----------------------------------------------------------------------------##
## Cross-space metrics (Trekker differentiator: analysis, not a view).
##
## Computed on the FULL positioned set, because neighbourhood structure is
## DENSITY-DEPENDENT -- computing it on the down-sampled subset would distort it.
## Only the retained nuclei's values are shipped (subset in the slot below).
##
##   spatial_purity[i]  = of nucleus i's K nearest PHYSICAL neighbours, the
##                        fraction that share its cell type. Low = dispersed /
##                        tiled / infiltrating; high = tight anatomical domain.
##   xspace_concord[i]  = overlap between i's K nearest neighbours in EXPRESSION
##                        space (PCA) and in PHYSICAL space. High = transcriptomic
##                        neighbours are also physical neighbours (organized);
##                        low = identity is spatially mixed. Decoupled from the
##                        cell-type calling that spatial_purity depends on.
##
## Only Trekker can compute these: they need TRUE single-cell transcriptomic
## identity AND true physical position for the SAME nuclei.
##----------------------------------------------------------------------------##
K_XSPACE <- 15L
ct_full <- CELLTYPE_BY_CLUSTER[clab + 1L]
sp_nn <- FNN::get.knn(cbind(cx, cy), k = K_XSPACE)$nn.index
ex_nn <- FNN::get.knn(Embeddings(so, "pca"), k = K_XSPACE)$nn.index
spatial_purity <- vapply(
  seq_len(n_all),
  function(i) mean(ct_full[sp_nn[i, ]] == ct_full[i]),
  numeric(1)
)
xspace_concord <- vapply(
  seq_len(n_all),
  function(i) length(intersect(sp_nn[i, ], ex_nn[i, ])) / K_XSPACE,
  numeric(1)
)
## per-cell-type median purity + dispersed fraction (purity < 0.3) on the FULL
## set, for the page's at-a-glance "who forms domains, who infiltrates" readout.
purity_by_type <- lapply(sort(unique(ct_full)), function(t) {
  p <- spatial_purity[ct_full == t]
  list(
    type = t,
    n = length(p),
    median = round(stats::median(p), 3),
    dispersed = round(mean(p < 0.3), 3)
  )
})

##----------------------------------------------------------------------------##
## An EXISTING single-cell analysis, spatialized (differentiator 3): a per-cell
## signature score (Seurat::AddModuleScore) becomes a meta column the object
## carries, so the page can colour the physical map by it -- no page change, it
## just shows up under "Colour by" like any other per-cell value. We use a
## myelination signature: it reads as a clean white-matter gradient in tissue and
## is honestly interpretable across the section. Any per-cell analysis output
## (pseudotime, velocity magnitude, CellChat signaling score) rides the same
## mechanism once it is a meta column.
##----------------------------------------------------------------------------##
myelin_genes <- intersect(
  c("Plp1", "Mbp", "Mog", "Mag", "Mobp", "Cldn11"),
  rownames(so)
)
so <- Seurat::AddModuleScore(
  so,
  features = list(myelin_genes),
  name = "Myelination",
  assay = "SCT"
)

##----------------------------------------------------------------------------##
## Per-nucleus positioning confidence (differentiator 1: position uncertainty as
## an interactive axis). From the vendor bead statistics (coords_<sample>.txt),
## `proportion_SB_UMI_top_cluster` = the share of a nucleus's spatial-barcode UMI
## mass that falls in its ADOPTED position's cluster. Higher = the position is
## well supported; lower = it rests on a small sliver of a noisy bead cloud. No
## other spatial platform has a per-cell position confidence (Visium / Xenium
## positions are given, not inferred). Shipped as a colour field + the bead stats
## for the inspector; the page then dissolves the least-confident nuclei with a
## slider.
##
## The competing candidate positions of 2+-ambiguous nuclei are deliberately NOT
## shown: those nuclei are excluded from ConfPositioned (not in this .crb), and
## their candidate centroids are in no vendor table -- recovering them would mean
## re-running the bead clustering, which the app must not do.
##----------------------------------------------------------------------------##
coords <- read.table(
  file.path(BASE, paste0("coords_", SAMPLE, ".txt")),
  header = TRUE,
  stringsAsFactors = FALSE
)
ci <- match(bc_all, paste0(coords$cell_bc, "-1"))
conf_prop_top <- coords$proportion_SB_UMI_top_cluster[ci]
conf_sb_total <- coords$SB_total[ci]
conf_sb_umi_top <- coords$SB_UMI_top_cluster[ci]
conf_prop_noise <- coords$proportion_SB_UMI_noise[ci]

##----------------------------------------------------------------------------##
## positioning QC (keep the vendor's ORIGINAL field names; missing stays missing)
##----------------------------------------------------------------------------##
sm <- read.csv(f("_summary_metrics.csv"))
mv <- setNames(as.character(sm$Value), sm$Metrics)
num <- function(key) suppressWarnings(as.numeric(mv[[key]]))
qc <- list(
  sample_id = mv[["Sample_ID"]],
  assay = mv[["Single_cell_assay"]],
  tile_id = mv[["Tile_ID"]],
  eps = mv[["eps"]],
  min_sb = mv[["Min_spatial_barcodes_used_to_locate_a_nucleus_centroid"]],
  total_nuclei = num("Total_nuclei_from_single-nuclei_sequencing_library"),
  in_lib = num(
    "Nuclei_from_single-nuclei_sequencing_library_found_in_Trekker_library"
  ),
  pct_in_lib = num("Pct_nuclei_in_Trekker_library"),
  pct_valid_sb = num(
    "Pct_nuclei_in_Trekker_library_with_valid_spatial_barcodes"
  ),
  positioned = num("Total_nuclei_positioned"),
  pct_positioned = num("Pct_nuclei_positioned"),
  conf = num("Total_nuclei_positioned_with_1_spatial_location"),
  pct_conf = num("Pct_nuclei_positioned_with_1_spatial_location"),
  pct_2plus = num("Pct_nuclei_positioned_with_2+_spatial_locations"),
  o_1 = num("Nuclei_o_1"),
  salv_2 = num("Nuclei_salvaged_2"),
  salv_3 = num("Nuclei_salvaged_3"),
  pct_salv = num("Pct_nuclei_salvaged"),
  n_0 = num("Nuclei_0"),
  n_1 = num("Nuclei_1"),
  n_2 = num("Nuclei_2"),
  n_3 = num("Nuclei_3"),
  n_4p = num("Nuclei_>=4")
)

##----------------------------------------------------------------------------##
## upstream (vendor) Moran's I  -- NOT Cerebro's own; label it as such
##----------------------------------------------------------------------------##
mi <- read.table(
  f("_variable_features_spatial_moransi.txt"),
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE
)
mi <- mi[order(-mi$MoransI_observed), ]
moran <- lapply(seq_len(min(12L, nrow(mi))), function(i) {
  list(
    rank = i,
    gene = rownames(mi)[i],
    I = round(mi$MoransI_observed[i], 4)
  )
})
## rownames may be integer if the gene column is unnamed; recover gene names.
if (is.null(rownames(mi)) || all(grepl("^[0-9]+$", rownames(mi)))) {
  gcol <- names(mi)[
    !names(mi) %in%
      c(
        "MoransI_observed",
        "MoransI_p.value",
        "moransi.spatially.variable",
        "moransi.spatially.variable.rank"
      )
  ]
  gnm <- if (length(gcol)) mi[[gcol[1]]] else mi[[1]]
  moran <- lapply(seq_along(moran), function(i) {
    moran[[i]]$gene <- gnm[i]
    moran[[i]]
  })
}

##----------------------------------------------------------------------------##
## positioning-evidence images
##   cells_1_coordinates_assigned/<BC16>.jpeg  = a confidently-positioned
##     nucleus (the "why is this nucleus here" drill-down); file stem is the
##     16-char nucleus barcode, so <BC16>-1 is the object barcode.
##   cells_{0,2,3}_coordinates_assigned/       = excluded classes; one example
##     each for the "what the rejected nuclei look like" panel.
##----------------------------------------------------------------------------##
ev_dir <- file.path(BASE, "cell_bc_plots")
ev_files <- list.files(
  file.path(ev_dir, "cells_1_coordinates_assigned"),
  pattern = "\\.jpe?g$",
  full.names = TRUE
)
ev_bc <- paste0(sub("\\.jpe?g$", "", basename(ev_files)), "-1")
ev_keep <- ev_bc %in% bc_all
ev_files <- ev_files[ev_keep]
ev_bc <- ev_bc[ev_keep]
message("positioning-evidence nuclei found in object: ", length(ev_bc))

##----------------------------------------------------------------------------##
## sub-sample: stratified by cluster, plus force-include the evidence nuclei
##----------------------------------------------------------------------------##
strat <- integer(0)
for (lv in sort(unique(clab))) {
  w <- which(clab == lv)
  k <- max(1L, round(N_CELLS * length(w) / n_all))
  strat <- c(strat, sample(w, min(k, length(w))))
}
force_idx <- match(ev_bc, bc_all)
idx <- sort(unique(c(strat, force_idx)))
message("sub-sampled nuclei: ", length(idx), " (of ", n_all, ")")

sub_bc <- bc_all[idx]

##----------------------------------------------------------------------------##
## Cerebro object: whole-transcriptome expression + UMAP + groups via
## exportFromSeurat, then inject the `trekker` slot.
##----------------------------------------------------------------------------##
sub <- subset(so, cells = sub_bc)
sub$celltype <- CELLTYPE_BY_CLUSTER[
  as.integer(as.character(sub$seurat_clusters)) + 1L
]
sub$cluster <- factor(as.integer(as.character(sub$seurat_clusters)))
sub$nUMI <- sub$nCount_SCT
sub$nGene <- sub$nFeature_SCT
## AddModuleScore names the column <name>1; expose it plainly as a per-cell meta
## value the app can colour the physical map by (differentiator 3).
sub$Myelination <- sub$Myelination1
sub$Myelination1 <- NULL
## keep only the SCT assay + UMAP; drop the dense SCT scale.data and pca/SPATIAL
## reductions so the exported object is lean and Overview shows the UMAP.
sub <- DietSeurat(sub, assays = "SCT", dimreducs = "umap")

message("exportFromSeurat (whole transcriptome, ", nrow(sub), " genes) ...")
exportFromSeurat(
  sub,
  assay = "SCT",
  slot = "data",
  file = CRB,
  experiment_name = "Trekker mouse brain (TrekkerU_C, demo)",
  organism = "mm",
  groups = c("cluster", "celltype"),
  main_group = "celltype",
  nUMI = "nUMI",
  nGene = "nGene",
  verbose = FALSE
)

##----------------------------------------------------------------------------##
## build the trekker slot payload (arrays are in sub-sample order)
##----------------------------------------------------------------------------##
encode_jpeg <- function(path, max_px = EV_MAX_PX, quality = EV_QUALITY) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("the 'magick' package is required to embed evidence images")
  }
  img <- magick::image_read(path)
  img <- magick::image_resize(img, paste0(max_px, "x", max_px, ">"))
  raw <- magick::image_write(img, format = "jpeg", quality = quality)
  paste0("data:image/jpeg;base64,", base64enc::base64encode(raw))
}

## evidence: remap each nucleus to its 0-based position in the sub-sample
ev_pos <- match(ev_bc, sub_bc)
evidence <- lapply(seq_along(ev_bc), function(i) {
  list(
    cell = ev_pos[i] - 1L,
    bc = ev_bc[i],
    img = encode_jpeg(ev_files[i])
  )
})
message("embedded ", length(evidence), " evidence images")

## qc_examples: one embedded image per excluded class (0 / 2 / 3)
qc_examples <- lapply(c(0L, 2L, 3L), function(cl) {
  d <- file.path(ev_dir, paste0("cells_", cl, "_coordinates_assigned"))
  fs <- list.files(d, pattern = "\\.jpe?g$", full.names = TRUE)
  if (!length(fs)) {
    return(NULL)
  }
  list(class = cl, n = length(fs), img = encode_jpeg(fs[1]))
})
qc_examples <- Filter(Negate(is.null), qc_examples)

## A colour field = a per-nucleus continuous value quantised to 0-255 for the
## client. `scale = "unit"` keeps the natural 0-1 scale (fractions); `"minmax"`
## stretches [min, max] across the palette and records the real range for the
## colorbar. Subset to the retained nuclei; values were computed on the full set.
mk_field <- function(v, label, desc, by_type = NULL, scale = "unit") {
  vi <- v[idx]
  if (scale == "minmax") {
    mn <- min(vi, na.rm = TRUE)
    mx <- max(vi, na.rm = TRUE)
    rng <- if (mx > mn) mx - mn else 1
    out <- list(
      v = as.integer(round((vi - mn) / rng * 255)),
      min = round(mn, 3),
      max = round(mx, 3),
      label = label,
      desc = desc
    )
  } else {
    vv <- pmin(pmax(vi, 0), 1)
    out <- list(
      v = as.integer(round(vv * 255)),
      max = 1,
      label = label,
      desc = desc
    )
  }
  if (!is.null(by_type)) {
    out$by_type <- by_type
  }
  out
}

trekker <- list(
  meta = list(
    n_cells_full = n_all,
    n_cells = length(idx),
    n_genes_obj = nrow(so),
    unit = "um (per vendor manual; not declared in file)",
    coord_source = "Location_ConfPositionedNuclei.csv (vendor canonical)",
    r = R.version.string,
    seurat = as.character(utils::packageVersion("Seurat")),
    generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ),
  qc = qc,
  ## per-nucleus barcodes IN THE SAME ORDER as the coordinate arrays below, so
  ## the server can pull a gene's expression aligned to these points regardless
  ## of the expression matrix's internal column order (getExpressionMatrix honors
  ## the requested `cells` order).
  barcodes = unname(sub_bc),
  ## canonical coordinates (μm); the transposed / y-mirrored variants are
  ## derived in the browser (see the Trekker module JS), matching the three
  ## measured orientations of @images (transpose) and SPATIAL reduction (y-neg).
  ## unname() everything: a named vector serializes to a JSON *object* (barcode
  ## -> value), but the client expects arrays it can index by position.
  x = unname(round(cx[idx], 2)),
  y = unname(round(cy[idx], 2)),
  ux = unname(round(um[idx, 1], 3)),
  uy = unname(round(um[idx, 2], 3)),
  clusters = unname(clab[idx]),
  celltype = CELLTYPE_BY_CLUSTER,
  ## continuous colour fields (differentiator 2): cross-space discordance,
  ## computed on the full positioned set. The page's "Colour by" is data-driven
  ## off this list, so adding a field here (or, later, a per-cell meta value)
  ## makes it a physical-map colouring with no UI change.
  fields = list(
    spatial_purity = mk_field(
      spatial_purity,
      "Spatial purity",
      paste0(
        "Fraction of each nucleus's 15 nearest physical neighbours that share ",
        "its cell type (computed on the full positioned set). Low = dispersed / ",
        "tiled / infiltrating; high = tight anatomical domain. In this HEALTHY ",
        "brain, low microglial / astrocyte purity is baseline tiling, not ",
        "activation."
      ),
      by_type = purity_by_type
    ),
    xspace_concordance = mk_field(
      xspace_concord,
      "Cross-space concordance",
      paste0(
        "Overlap between a nucleus's 15 nearest neighbours in expression space ",
        "(PCA) and in physical space. High = its transcriptomic neighbours are ",
        "also its physical neighbours (organized domain); low = its identity is ",
        "spatially mixed."
      )
    ),
    position_confidence = mk_field(
      conf_prop_top,
      "Position confidence",
      paste0(
        "Share of this nucleus's spatial-barcode UMI mass that falls in its ",
        "adopted position's bead cluster (vendor statistic). Higher = the ",
        "position is well supported; lower = it rests on a small sliver of a ",
        "noisy bead cloud. A per-cell position confidence no other spatial ",
        "platform has -- Visium / Xenium positions are given, not inferred. Use ",
        "the confidence slider to dissolve the least-confident nuclei."
      ),
      scale = "minmax"
    )
  ),
  ## per-nucleus bead statistics behind the confidence (for the Cell inspector)
  conf = list(
    prop_top = unname(round(conf_prop_top[idx], 3)),
    prop_noise = unname(round(conf_prop_noise[idx], 3)),
    sb_total = unname(conf_sb_total[idx]),
    sb_umi_top = unname(conf_sb_umi_top[idx])
  ),
  moran = moran,
  evidence = evidence,
  qc_examples = qc_examples
)

##----------------------------------------------------------------------------##
## inject + re-save (xz for the smallest .crb)
##----------------------------------------------------------------------------##
crb <- readRDS(CRB)
crb$addTrekker(trekker)
saveRDS(crb, CRB, compress = "xz")

sz <- file.size(CRB) / 1e6
message(sprintf(
  "wrote %s  (%.2f MB, %d nuclei x %d genes, %d evidence imgs)",
  CRB,
  sz,
  length(idx),
  nrow(sub),
  length(evidence)
))
if (sz > 5) {
  warning(sprintf("demo_trekker.crb is %.2f MB (> 5 MB budget)", sz))
}
