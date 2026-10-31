# data-raw — reproducible demo data

This directory reproducibly rebuilds every demo `.crb` shipped in `inst/extdata/examples/`. It is excluded from the built package via `.Rbuildignore`; it stays in the repository for reproducibility only. The built `.crb` files are what ships.

## Where to look

| I want to… | Read |
|------------|------|
| know the exact source / citation / download command / license of a dataset | [`DATASETS.md`](DATASETS.md) — the provenance registry |
| understand or rebuild the **spatial** demos | [`spatial.md`](spatial.md) |
| understand or rebuild the **immune repertoire** demos | [`immune_repertoire.md`](immune_repertoire.md) |
| understand or rebuild the **trajectory** demo | [`trajectory.md`](trajectory.md) |
| understand or rebuild the **HLA & TCR motif** demos | [`hla.md`](hla.md) |
| understand or rebuild the **Trekker** demo | [`trekker.md`](trekker.md) |
| prepare the **large demo data sets** (1M mouse brain, Ren COVID-19 atlas) for `run-demo.R` | [`large-demo/README.md`](large-demo/README.md) |
| rebuild the three synthetic **Builder inputs** | [`builder_fixtures.md`](builder_fixtures.md) |
| know what a sub-directory here is | [Directory layout](#directory-layout) below |
| add a **new** dataset | copy the template in [`DATASETS.md`](DATASETS.md), then add a build script + a per-type notes file |

`DATASETS.md` is the single source of truth for provenance across all data types. The per-type notes files (`spatial.md`, `immune_repertoire.md`, `trajectory.md`) carry only design and rebuild details and link back to it. This keeps citations in one place and avoids duplicating source info per file.

## Demo gene budget

The active builders call `save_demo_crb()` from `demo_gene_panel.R` before publishing their final artifacts. It caps embedded expression at **100 genes**; smaller panels are kept whole. Available neural, stromal and immune markers, the Trekker picker/myelination genes and its upstream Moran genes are kept first. The genes that the analysis tables name come next, the most variable first, but only until the 100-gene budget is used. Remaining places, if any, are filled by variance of the stored normalized expression, with deterministic input-order ties. The analysis tables themselves are not cut, so they name many genes that are not in the matrix: in `example.crb` only 89 of 1,911 marker genes, 32 of 182 most-expressed genes and 45 of 721 enriched-pathway genes are among the 100. `shrink_demos.R` applies the same cap to the shipped files in place. This is a demo policy, not a biological feature-selection method or a filter applied to users' data.

Filtering happens after analysis. Retained values, sparse representation, cell identity/order, metadata (including full-input nUMI/nGene), groups, coordinates, trajectories, receptor/HLA data and images are preserved. Spatial expression is subset by the same genes without changing its cell order. Panel provenance is stored in `parameters$demo_gene_panel`.

Gene search and any *new* gene-set analysis are limited to the exported panel; these artifacts are not whole-transcriptome datasets. Full-input precomputed scores retain their original interpretation. The active builders use the public CRB I/O functions, and the final writer uses qs2 level 19 with an exact decoded payload check. There is no separate release-compaction CLI or runtime change.

### Refreshing existing artifacts without recomputing their analyses

Run from the repository root in the project R environment, using a fresh output directory. After validation, replace only the CRBs, preserving companion images and HDF5 files at their original locations:

```r
pkgload::load_all(".", quiet = TRUE)
source("data-raw/demo_gene_panel.R")
output <- "/path/to/new-demo-staging"
stopifnot(!file.exists(output))
dir.create(output, recursive = TRUE)
for (input in list.files("inst/extdata/examples", "\\.crb$", full.names = TRUE)) {
  save_demo_crb(readCerebro(input), file.path(output, basename(input)))
}
```

History: the 2026-10-12 artifacts were derived this way from master `4d74a839`, without rerunning raw-data downloads, normalization, UMAP or cell sampling. At that point all nine kept exactly their previous cells and four had fewer genes. The current files were cut further afterwards (see below), so this paragraph does not describe them. The legacy synthetic Spatial fixture was transformed from its checked-in CRB, not regenerated from an original raw-data generator. Future active builder runs apply the same final export policy; full pipeline byte identity across dependency versions is not claimed.

### Measured artifact sizes (100-gene panel)

| CRB | Cells (kept) | Genes before → after | Before MiB | After MiB |
|---|---:|---:|---:|---:|
| demo_full_tcr_bcr.crb | 1000 | 967 → 100 | 1.04 | 0.46 |
| demo_hla_tcr_dextramer.crb | 2000 | 2000 → 100 | 9.03 | 0.64 |
| demo_spatial_merfish.crb | 1000 | 241 → 100 | 1.82 | 1.04 |
| demo_spatial_slideseq.crb | 3000 | 2000 → 100 | 1.48 | 0.45 |
| demo_spatial_visium.crb | 1000 | 1200 → 100 | 5.74 | 0.72 |
| demo_spatial_xenium.crb | 2000 | 248 → 100 | 2.72 | 0.70 |
| demo_spatial.crb | 1000 | 2500 → 100 | 0.62 | 0.16 |
| demo_trekker.crb | 1000 | 21374 → 100 | 6.79 | 0.91 |
| example.crb | 1000 | 967 → 100 | 1.03 | 0.44 |

Total: **30.26 → 5.51 MiB** (the "Before" column is the build before any gene cap; master, with the 1,500-gene cap, was 21.22 MiB). This includes both gene selection and build-time compression; it is not a load-time speedup claim. The three Xenium background images are recompressed without loss (3.5 → 1.8 MiB). Git history size is not reduced.

The shipped files were then cut in place to a random subset of their cells (1,000 to 3,000 each) with `subsample_demos.R` and to the gene panel above with `shrink_demos.R`, because their large sources are not rebuilt for this. Cells, expression, metadata, projections, spatial entries, the Trekker per-nucleus vectors and the repertoire were subset together, and group levels left without cells were dropped. The subset size, seed and input cell count of each file are stored in its `parameters$demo_cell_subsample`, and the panel in `parameters$demo_gene_panel`.

The builders in this directory produce the 100-gene panel but keep all their cells (Visium 2,696, Slide-seq 5,000, MERFISH 5,000, Xenium 5,000, HLA dextramer 12,000, Trekker 2,536); the shipped files are then cut with `subsample_demos.R` and `shrink_demos.R`. Every shipped example is reproduced by putting the examples of release 5.2.0 (tag `v5.2.0`) in `inst/extdata/examples/` and running `Rscript data-raw/subsample_demos.R` and then `Rscript data-raw/shrink_demos.R` from the package root. `subsample_demos.R` converts a file in a retired format first (`demo_spatial_merfish`), keeps the Trekker nuclei with a positioning-evidence image, and applies the 100-gene panel to the kept cells; `shrink_demos.R` caps `demo_spatial`, whose cells are not cut.

### Validation of the 2026-10-12 artifacts

This section is history: it describes the 2026-10-12 artifacts, before the 100-gene cap and the cell cut, not the current files. Validated with R 4.6.1 / qs2 0.3.1 in WSL on 2026-10-12:

- All nine input CRB hashes matched master `4d74a839`. Before/after checks compared every retained expression value and all non-expression data fields, including metadata, projections, receptor/HLA data, Trekker scores/images and spatial coordinates/images. Only expression rows and panel provenance changed.
- The source test suite passed **14,468 assertions**, with no failures or errors and two conditional skips for unavailable external artifacts. This includes 71 demo-panel assertions, covering the real Spatial post-export function.
- `R CMD check --no-tests` passed with **0 errors, 0 warnings, 1 NOTE** (the existing `.GlobalEnv` assignment in `launchCerebro`). Tests ran separately as above.
- Both updated HLA and Trekker vignettes rendered successfully. Air formatting checks passed for the changed builder/panel/Trekker R files and panel tests.

This is artifact/data-integrity and existing automated UI coverage, not a new Windows Brave acceptance run or a whole-application performance benchmark.

## Data families

| Family | Datasets | Build script | Runs | Notes |
|--------|----------|--------------|------|-------|
| Immune repertoire | 3 PBMC subsets (TCR/BCR by lineage) | `build_ir_demos.R` | download-then-run (needs the VDJ CSVs first — see acquire in `DATASETS.md`) | [`immune_repertoire.md`](immune_repertoire.md) |
| Spatial | Visium · Slide-seq v2 · MERFISH · Xenium | `build_spatial_demos.R` | self-contained (all sources fetched automatically — R packages for Visium/Slide-seq/MERFISH, auto-`download.file` for Xenium) | [`spatial.md`](spatial.md) |
| Trajectory | monocle2 pseudotime, carried inside `demo_full_tcr_bcr.crb` | `build_trajectory_demo.R` | self-contained (input is the already-built IR demo `.crb`; needs `monocle`) | [`trajectory.md`](trajectory.md) |
| HLA & TCR motifs | synthetic fixture · real bulk TCRβ + real genotypes · real antigen-selected single cells | `build_hla_tcr_demo.R` · `build_hla_tcr_bulk_demo.R` · `build_hla_tcr_dextramer_demo.R` | self-contained (the two real ones download their sources on first run: ~349 MB from Zenodo, ~1.6 GB from 10x) | [`hla.md`](hla.md) |
| Trekker | Trekker spatial mapping, `demo_trekker.crb` | `build_trekker_demo.R` | download-then-run (needs `magick` + `base64enc` at build time) | [`trekker.md`](trekker.md) |

**Verification (2026-07-07):** `build_spatial_demos.R` reproduces the four spatial objects at full size (`set.seed(42)` makes it deterministic). The shipped spatial `.crb` are additionally cut to a random subset of cells and to 100 genes (see [Demo gene budget](#demo-gene-budget)), which `subsample_demos.R` and `shrink_demos.R` reproduce. The builder is **fully self-contained** — the network-sourced demo (Xenium) auto-downloads its raw data on first run via `download.file`, so `Rscript data-raw/build_spatial_demos.R` runs the whole link → `.crb` pipeline from one command. The download is skipped when the file is already present (verified: Xenium skipped its 3.5 GB download when the outs bundle was already unzipped). The three package-sourced builds (Visium, Slide-seq, MERFISH) pull from SeuratData/Bioconductor. `build_ir_demos.R` is still a two-step build: its download URLs are live and its dependencies (`scRepertoire`, `example.crb`) are present, but the VDJ CSVs it consumes are intentionally not committed, so run the acquire `curl` step first.

## Directory layout

Only the scripts and the `.md` files are tracked. Every sub-directory here is a **download cache**: gitignored, safe to delete, and re-created by the build script that owns it. Deleting one costs only the re-download, so a missing directory below is normal.

| path | tracked? | owned by | what is in it |
|------|----------|----------|---------------|
| `*.R` | yes | — | the build scripts, one per data family |
| `DATASETS.md` | yes | — | the provenance registry: source, citation, licence, acquire command, sampling — one entry per shipped `.crb` |
| `*.md` (`spatial`, `immune_repertoire`, `trajectory`, `hla`, `trekker`) | yes | — | per-family design and rebuild notes: what to download, what each step does, and the code that does it. They link back to `DATASETS.md` rather than repeat it |
| `large-demo/` | yes | `run-demo.R` | the scripts that download, convert and register the two large demo data sets (not shipped in `inst/extdata/examples/`). Their downloads go to the user cache directory, not into this tree; see [`large-demo/README.md`](large-demo/README.md) |
| `vdj_10x/` | **no** (cache) | `build_ir_demos.R` | 10x PBMC VDJ contig CSVs (`pbmc3_t_contig.csv`, `pbmc3_b_contig.csv`) for the immune-repertoire demos |
| `vdj_10x_dextramer/` | **no** (cache, ~2.7 GB) | `build_hla_tcr_dextramer_demo.R` | the 10x dextramer cohort: per-donor contig annotations, binarized dextramer matrices and expression matrices, plus a local copy of the paper's supplementary PDF (table S1 = the donor genotypes; the publisher's file is never committed — the URL is in [`hla.md`](hla.md#32-download)) |
| `pubtcrs/` | **no** (cache, ~349 MB) | `build_hla_tcr_bulk_demo.R` | the Emerson/DeWitt cohort archive from Zenodo, plus a cached occurrence scan |
| `xenium/` | **no** (cache, ~3.5 GB) | `build_spatial_demos.R` | the Xenium outs bundle (auto-downloaded on first run) |

If a cache directory is missing, the owning script re-downloads into it — no manual step is needed except where `DATASETS.md` says so explicitly.

No raw source is committed. Everything the demos are built from is re-obtainable from the commands in the per-family notes.

## Environment of record

The shipped `.crb` files were last built with:

| Component | Version |
|-----------|---------|
| R | 4.5.2 |
| Bioconductor | 3.22 |
| Seurat | 5.4.0 |
| SeuratObject | 5.3.0 |
| SeuratData | 0.2.2.9002 |
| MerfishData | 1.12.0 |
| scRepertoire | ≥ 2.0 |
| monocle | for the trajectory demo (build-time only) |
| RBioFormats | for the Xenium DAPI OME-TIFF (JPEG2000) extraction (pure R, Bioconductor) |

- **One registry.** Every dataset is recorded in `DATASETS.md` with the same fields — no dataset ships without a registry entry.
- **Reproducible acquire.** Each entry gives the exact command to obtain the raw data (a package install + load, or a `curl` from a public URL); raw downloads are not committed, only the built `.crb`.
- **Public, citable sources only.** Every dataset is public reference data with a stated license.
- **Soft-wrapped Markdown.** Prose is one sentence (or clause) per line, no fixed-column hard wrapping — cleaner rendering and smaller diffs. Tables and code blocks are kept literal.
