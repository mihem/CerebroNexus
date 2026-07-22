# data-raw — reproducible demo data

This directory reproducibly rebuilds every demo `.crb` shipped in `inst/extdata/v1.4/`. It is excluded from the built package via `.Rbuildignore`; it stays in the repository for reproducibility only. The built `.crb` files are what ships.

## Where to look

| I want to… | Read |
|------------|------|
| know the exact source / citation / download command / license of a dataset | [`DATASETS.md`](DATASETS.md) — the provenance registry |
| understand or rebuild the **spatial** demos | [`spatial.md`](spatial.md) |
| understand or rebuild the **immune repertoire** demos | [`immune_repertoire.md`](immune_repertoire.md) |
| understand or rebuild the **trajectory** demo | [`trajectory.md`](trajectory.md) |
| understand or rebuild the **HLA & TCR motif** demos | [`hla.md`](hla.md) |
| understand or rebuild the **Trekker** demo | [`trekker.md`](trekker.md) |
| know what a sub-directory here is | [Directory layout](#directory-layout) below |
| add a **new** dataset | copy the template in [`DATASETS.md`](DATASETS.md), then add a build script + a per-type notes file |

`DATASETS.md` is the single source of truth for provenance across all data types. The per-type notes files (`spatial.md`, `immune_repertoire.md`, `trajectory.md`) carry only design and rebuild details and link back to it. This keeps citations in one place and avoids duplicating source info per file.

## Data families

| Family | Datasets | Build script | Runs | Notes |
|--------|----------|--------------|------|-------|
| Immune repertoire | 3 PBMC subsets (TCR/BCR by lineage) | `build_ir_demos.R` | download-then-run (needs the VDJ CSVs first — see acquire in `DATASETS.md`) | [`immune_repertoire.md`](immune_repertoire.md) |
| Spatial | Visium · Slide-seq v2 · MERFISH · Xenium | `build_spatial_demos.R` | self-contained (all sources fetched automatically — R packages for Visium/Slide-seq/MERFISH, auto-`download.file` for Xenium) | [`spatial.md`](spatial.md) |
| Trajectory | monocle2 pseudotime, carried inside `demo_full_tcr_bcr.crb` | `build_trajectory_demo.R` | self-contained (input is the already-built IR demo `.crb`; needs `monocle`) | [`trajectory.md`](trajectory.md) |
| HLA & TCR motifs | synthetic fixture · real bulk TCRβ + real genotypes · real antigen-selected single cells | `build_hla_tcr_demo.R` · `build_hla_tcr_bulk_demo.R` · `build_hla_tcr_dextramer_demo.R` | self-contained (the two real ones download their sources on first run: ~349 MB from Zenodo, ~1.6 GB from 10x) | [`hla.md`](hla.md) |
| Trekker | Trekker spatial mapping, `demo_trekker.crb` | `build_trekker_demo.R` | download-then-run (needs `magick` + `base64enc` at build time) | [`trekker.md`](trekker.md) |

**Verification (2026-07-07):** `build_spatial_demos.R` reproduces all four shipped spatial `.crb` (`set.seed(42)` makes it deterministic) and is now **fully self-contained** — the network-sourced demo (Xenium) auto-downloads its raw data on first run via `download.file`, so `Rscript data-raw/build_spatial_demos.R` runs the whole link → `.crb` pipeline from one command. The download is skipped when the file is already present (verified: Xenium skipped its 3.5 GB download when the outs bundle was already unzipped). The three package-sourced builds (Visium, Slide-seq, MERFISH) pull from SeuratData/Bioconductor. `build_ir_demos.R` is still a two-step build: its download URLs are live and its dependencies (`scRepertoire`, `example.crb`) are present, but the VDJ CSVs it consumes are intentionally not committed, so run the acquire `curl` step first.

## Directory layout

Only the scripts and the `.md` files are tracked. Every sub-directory here is a **download cache**: gitignored, safe to delete, and re-created by the build script that owns it. Deleting one costs only the re-download, so a missing directory below is normal.

| path | tracked? | owned by | what is in it |
|------|----------|----------|---------------|
| `*.R` | yes | — | the build scripts, one per data family |
| `DATASETS.md` | yes | — | the provenance registry: source, citation, licence, acquire command, sampling — one entry per shipped `.crb` |
| `*.md` (`spatial`, `immune_repertoire`, `trajectory`, `hla`, `trekker`) | yes | — | per-family design and rebuild notes: what to download, what each step does, and the code that does it. They link back to `DATASETS.md` rather than repeat it |
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
