# Trekker demo — from download to `.crb`

Design + build notes for the **Trekker** tab's demo dataset. This is the
end-to-end record the registry ([`DATASETS.md`](DATASETS.md)) points to:
where the raw data comes from, what is inside it, which files we read, how the
shipped subset is sampled, and how `demo_trekker.crb` is generated.

Everything here is produced by [`build_trekker_demo.R`](build_trekker_demo.R).
The raw bundle is gitignored and **not redistributable**; only the derived,
down-sampled `.crb` ships.

## Contents

1. [What Trekker is](#1-what-trekker-is)
2. [Download (registration required)](#2-download-registration-required)
3. [What is inside the bundle](#3-what-is-inside-the-bundle)
   - [The three coordinate orientations](#the-three-coordinate-orientations-measured-not-assumed)
4. [Extraction (which files, why)](#4-extraction-which-files-why)
5. [Sub-sampling (whole genes, fewer nuclei)](#5-sub-sampling-whole-genes-fewer-nuclei)
6. [How `demo_trekker.crb` is generated](#6-how-demo_trekkercrb-is-generated)
7. [Honesty the page enforces](#7-honesty-the-page-enforces)
8. [Registry](#8-registry)

---

## 1. What Trekker is

Trekker (Curio Bioscience / Takara Bio — the *Trekker Single-Cell Spatial
Mapping Kit*) tags cell **nuclei** with location barcodes carried by
known-position beads, then recovers each nucleus's 2-D position from ordinary
single-nucleus sequencing. It is built on Slide-tags foundational technology.

Why it is not "just another spatial platform" — it occupies a cell that the
others cannot:

| Platform | Spatial unit | Genes | True single cell | Whole transcriptome |
| --- | --- | --- | :---: | :---: |
| Visium | spot 55 µm | whole txome | ✗ needs deconvolution | ✓ |
| Xenium / MERFISH | single cell | panel 1–5k | ✓ | ✗ |
| Slide-seq v2 | bead 10 µm | whole txome | ~ bead ≠ cell | ✓ |
| **Trekker** | **nucleus** | **21,374 (measured)** | **✓** | **✓** |

Consequences that shape the demo:

- it is a **single-nucleus expression object with coordinates**, not a
  spot-deconvolution problem;
- it usually carries **no matched histology image**;
- positioning has explicit confidence classes and QC — not every coordinate is
  equally trustworthy;
- the expression matrix is whole-transcriptome and large, so it cannot be
  embedded the way a tiny synthetic fixture is.

CerebroNexus only **ingests and displays** the vendor pipeline's output. It
never runs the vendor Primary Analysis Pipeline (that needs an HPC/cloud run far
larger than a Shiny session).

---

## 2. Download (registration required)

The Trekker example bundles are **not public downloads**. Access is gated:

1. Register for / sign in to a **Curio Bioscience / Takara Bio** account and
   request access to the Trekker example data.
2. Download the per-sample bundle. This demo uses the **smallest** of the nine
   example bundles:

   ```
   Mouse_Brain_TrekkerU_C_Sept2025.tar.gz    (~1.3 GB compressed)
   ```

3. Extract it, and note the `output/` directory inside:

   ```bash
   tar -xzf Mouse_Brain_TrekkerU_C_Sept2025.tar.gz
   export TREKKER_OUTPUT_DIR=/path/to/Mouse_Brain_TrekkerU_C_Sept2025/output
   ```

`build_trekker_demo.R` reads only from `$TREKKER_OUTPUT_DIR`; it never phones
home and cannot auto-download (the source is access-gated).

---

## 3. What is inside the bundle

The single-reaction bundle's `output/` layout (companion `intermediates/` and
per-class QC directories elided):

```
output/
├── <S>_ConfPositioned_seurat_spatial.rds        272 MB   ← expression / UMAP / clusters   (WE READ)
├── <S>_ConfPositioned_anndata_matched.h5ad       31 MB   ← same, AnnData (unused)
├── <S>_Location_ConfPositionedNuclei.csv        328 KB   ← COORDINATE AUTHORITY            (WE READ)
├── <S>_MoleculesPer_ConfPositionedNuclei.mtx    135 MB   ← counts as MEX (unused; RDS has it)
├── <S>_{genes,barcodes}_ConfPositionedNuclei.tsv        ← MEX dims (unused)
├── <S>_variable_features_spatial_moransi.txt            ← UPSTREAM Moran's I               (WE READ)
├── <S>_variable_features_clusters.csv                   ← per-cluster variable features (unused)
├── <S>_summary_metrics.csv                              ← positioning QC (72 fields)       (WE READ)
├── <S>_Trekker_Report.html                       13 MB   ← vendor report (unused)
├── cell_bc_plots/
│   ├── cells_1_coordinates_assigned/   50 jpeg  ← confidently-positioned evidence          (WE READ)
│   └── cells_{0,2,3}_coordinates_assigned/  50 jpeg each ← excluded-class examples          (WE READ)
├── plots/{summary_metrics,summary_minPts}.csv
└── intermediates/   ← `all` / `Positioned` (2+-location, not excluded) objects
```

`<S>` = `Mouse_Brain_TrekkerU_C`. The `cell_bc_plots/cells_{0,1,2,3}` directory
names themselves confirm positioning is a **0/1/2/3+ four-way** classification,
not a binary one.

### The three coordinate orientations (measured, not assumed)

The same nucleus appears in three places, in three different orientations —
verified by reading the real object:

| source | value for `AAACCCAAGCCTCTGG-1` | relation to canonical |
| --- | --- | --- |
| **Location CSV** `SPATIAL_1,SPATIAL_2` | `(6647, -4916)` | **canonical** (authority) |
| `SPATIAL` reduction | `(6647, +4916)` | **Y mirrored** |
| `@images$slice1` `GetTissueCoordinates` | `(-4916, 6647)` | **axes transposed** |

The existing generic `.getSpatialData()` extractor reads `@images` and would
**silently draw the brain transposed 90°** — no error, just wrong. So the
builder takes coordinates from the **Location CSV only**, and the Trekker page's
"Coordinate source" switch offers all three so the hazard is visible, not hidden.

---

## 4. Extraction (which files, why)

`build_trekker_demo.R` reads exactly five things from `$TREKKER_OUTPUT_DIR`:

| file | used for |
| --- | --- |
| `..._ConfPositioned_seurat_spatial.rds` | whole-transcriptome expression, UMAP, `seurat_clusters` |
| `..._Location_ConfPositionedNuclei.csv` | canonical (x, y) coordinates in µm |
| `..._summary_metrics.csv` | positioning QC (kept under the vendor's original field names) |
| `..._variable_features_spatial_moransi.txt` | upstream (vendor) Moran's I table |
| `cell_bc_plots/cells_{0,1,2,3}_*/` | positioning-evidence JPEGs (50 class-1 + one each of 0/2/3) |

The RDS carries a legacy `SlideSeq` `@images` entry that predates the `misc`
slot, so `subset()`/`validObject()` errors on it. We never use `@images`
(coordinates come from the CSV), so the builder simply drops `so@images` up
front to make the object subsettable — the same vendor quirk documented for the
Slide-seq demo in [`spatial.md`](spatial.md).

---

## 5. Sub-sampling (whole genes, fewer nuclei)

The ConfPositioned object is **21,374 genes × 7,420 nuclei**. Embedding
whole-transcriptome expression for all of them blows the size budget:

| nuclei (all genes) | embedded expression, xz |
| ---: | ---: |
| 1,500 | 2.34 MB |
| **2,500** | **3.79 MB** |
| 3,500 | 5.25 MB |

"Real single cell × **whole transcriptome**" is the platform's whole point, so
we keep **all 21,374 genes** and down-sample **nuclei** instead:

- `set.seed(42)`; **2,532 nuclei** stratified by cluster (`SCT_snn_res.0.2`);
- **the 50 nuclei that carry official positioning-evidence images are
  force-included**, so the evidence drill-down still works after sampling;
- expression is the `SCT` `data` (normalised) layer; the dense `scale.data` and
  the `pca`/`SPATIAL` reductions are dropped (`DietSeurat`) so only UMAP + `data`
  are exported.

```r
strat <- integer(0)
for (lv in sort(unique(clab))) {                 # proportional within each cluster
  w <- which(clab == lv)
  k <- max(1L, round(N_CELLS * length(w) / n_all))   # max(1L,...): never drop a cluster
  strat <- c(strat, sample(w, min(k, length(w))))
}
force_idx <- match(ev_bc, bc_all)                # the evidence nuclei, unconditionally
idx <- sort(unique(c(strat, force_idx)))         # union -> 2,532, slightly over N_CELLS

sub <- subset(so, cells = bc_all[idx])
sub$celltype <- CELLTYPE_BY_CLUSTER[as.integer(as.character(sub$seurat_clusters)) + 1L]
sub$nUMI <- sub$nCount_SCT;  sub$nGene <- sub$nFeature_SCT
sub <- DietSeurat(sub, assays = "SCT", dimreducs = "umap")
```

The union with `force_idx` is why the shipped count is 2,532 rather than a round
`N_CELLS` — a stratified draw would not reliably include all 50 evidence nuclei,
and without them the page's drill-down would break on a rebuild.

### Cell types

The vendor object ships 18 unnamed Louvain clusters. They were labelled **once**
by marker z-score argmax and hard-coded (indexed by cluster id) so the build is
deterministic and needs no marker recomputation:

```
Snap25 / Slc17a7 → ExN     Gad1 / Gad2 → InN      Plp1 / Mbp → Oligo
Aqp4 / Gfap    → Astro     Cx3cr1/C1qa/Csf1r → Micro   Pdgfra → OPC   Prox1 → DG
```

Clusters without a clear marker winner are honestly left `Neuron` rather than
over-called.

---

## 6. How `demo_trekker.crb` is generated

Two stages in `build_trekker_demo.R`:

**(a) a proper Cerebro object** — the down-sampled Seurat (SCT `data` + UMAP +
`cluster`/`celltype` groups) goes through `exportFromSeurat()`, giving a normal
`Cerebro_v1.3` object with whole-transcriptome expression and gene names. This is
what powers gene colouring on the page: the gene picker lists all measured
genes, and on selection the server slices one gene, quantises it 0–255, and
sends it to the client aligned to the page's nuclei.

**(b) the `trekker` slot** — a new `Cerebro_v1.3` field (`addTrekker()` /
`getTrekker()`, backward-compatible so older `.crb` files simply lack it) holds
everything the bespoke page needs beyond expression:

```
trekker = list(
  meta       = list(n_cells, n_cells_full, n_genes_obj, unit, coord_source, r, seurat, generated),
  qc         = list(sample_id, assay, tile_id, eps, min_sb, total_nuclei, positioned, conf,
                    pct_conf, pct_2plus, o_1, salv_2, salv_3, n_0..n_4p, ...),  # vendor field names
  barcodes   = <chr N>,          # per-nucleus, SAME ORDER as the arrays below
  x, y       = <num N>,          # canonical µm (Location CSV); transpose / y-mirror derived client-side
  ux, uy     = <num N>,          # UMAP
  clusters   = <int N>,          # 0-based cluster id
  celltype   = <chr 18>,         # cluster id -> label
  moran      = list({rank, gene, I}, ...),          # upstream vendor Moran's I
  evidence   = list({cell, bc, img}, ...),          # 50 nuclei; img = base64 data: URI
  qc_examples= list({class, n, img}, ...)           # excluded-class examples; img = base64
)
```

All arrays are `unname()`d: a *named* R vector serialises to a JSON **object**
(barcode → value), but the client indexes them **positionally** as arrays. Miss
one and that field silently arrives as `{}` on the client.

```r
trekker <- list(
  barcodes = unname(sub_bc),                  # the alignment key
  x  = unname(round(cx[idx], 2)),             # canonical um, from the Location CSV
  y  = unname(round(cy[idx], 2)),
  ux = unname(round(um[idx, 1], 3)),          # UMAP
  uy = unname(round(um[idx, 2], 3)),
  clusters = unname(clab[idx]),
  ...
)
crb$addTrekker(trekker)
```

The `barcodes` field lets the server pull a gene's expression aligned to these
exact points regardless of the expression matrix's internal column order
(`getExpressionMatrix(cells = barcodes, genes = g)` honours the requested order).

Evidence JPEGs are down-scaled (`magick`, 620 px long edge, quality 68) and
base64-embedded, following the same "image as a `data:` URI in the slot"
convention the spatial demos use for histology. So the **whole** demo —
expression + evidence — is one self-contained `.crb`.

Build + size (measured):

```
TREKKER_OUTPUT_DIR=/path/to/.../output Rscript data-raw/build_trekker_demo.R
# → inst/extdata/v1.4/demo_trekker.crb  (4.70 MB, 2532 nuclei × 21374 genes, 50 evidence imgs)
```

`magick` and `base64enc` are **build-time only** (data-raw is `.Rbuildignore`d);
they are not package Imports.

---

## 7. Honesty the page enforces

- **`ConfPositioned` ≠ "exactly one location".** 264 / 7,420 (3.56%) are
  vendor-salvaged multi-location nuclei; the RDS's `number_clusters` is all `1`
  (salvage trace erased). Labelled `vendor_confidently_positioned`, not
  "one location".
- **Unpositioned nuclei are `(0,0)` sentinels**, not `NA` — they are class 0 and
  excluded, never plotted at the origin.
- **The vendor's own demo exceeds the vendor's own guideline**: 2+-location rate
  22.56% > the manual's `<20%`. The page shows "below vendor reference range" and
  does **not** adjudicate sample usability for the user.
- **Pipeline version is `missing`** — `summary_metrics.csv` carries no version
  field, so provenance says `missing`, never a guessed value.
- **Moran's I is `Upstream`** — the vendor value, computed differently from
  Cerebro's own; labelled and never mixed.

---

## 8. Registry

The one-line provenance entry lives in [`DATASETS.md`](DATASETS.md) under
**## Trekker**. The dropdown label is `Mouse brain (Trekker)` (`inst/app.R`); the
tab appears only when the loaded `.crb` carries a `trekker` slot.
