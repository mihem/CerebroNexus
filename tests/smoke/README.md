# tests/smoke — local smoke + benchmark scripts

This directory holds **local-only** scripts that exercise cerebroAppLite end-to-end against full-size single-cell fixtures. It is **not** part of the package tarball and `R CMD check` does not see it:

- `src/` is committed (the scripts themselves)
- `data/` is gitignored (large `.qs` fixtures live here, not redistributable)
- `result/` is gitignored (per-script outputs)
- `profile/` is gitignored (profvis snapshots etc.)

## Synthetic fixtures

The committed smoke scripts no longer require private source data to be present under `data/`.

- `src/00_prepare_synthetic_data.R` generates a complete synthetic replacement corpus under `data/`.
- The direct data readers (`10_convert_embedded.R`, `11_convert_bpcells.R`, `12_convert_h5.R`, `90_export_bpcells.R`) call that helper automatically before reading fixtures.
- On the first run, if `data/` already contains non-synthetic local fixtures, the directory is preserved by renaming it to `data_private_backup_YYYYMMDD_HHMMSS/` and a fresh synthetic `data/` directory is created.
- Set `SMOKE_SYNTH_FORCE=1` to rebuild the synthetic corpus, and `SMOKE_SYNTH_SCALE=<number>` to scale the default synthetic gene/cell counts up or down.

The synthetic corpus mirrors the modalities and metadata columns expected by the smoke scripts:

- `data/39_shinyApp/39.0_samples_seurat_reclustered_myeloid_subset.qs`: myeloid snRNA-seq stand-in, `SCT` assay, marker table, groups `sample_id`, `condition`, `biobank`, `annotated_myeloid`
- `data/39_shinyApp/10.3.1_sc_Ctrl_duraFibro_both_integrated.qs`: Ctrl spatial fibro stand-in, `Xenium` assay, FOV coordinates, groups `annotated_manuscript`, `sample_id`
- `data/39_shinyApp/10.3.1_sc_MS_duraFibro_both_integrated.qs`: MS spatial fibro stand-in, same structure as above
- `data/tcr_bcr/seurat_PBMC_1002_Post_VDJ.qs`: PBMC repertoire stand-in with scRepertoire-style `CTgene`/`CTnt`/`CTaa`/`CTstrict` metadata
- `data/tcr_bcr/seurat_PBMC_1002_Post_VDJ.qs`: PBMC repertoire stand-in with simulated expression but original benchmark dimensions preserved (`2000 genes x 9287 cells`)
- `data/21_S04_seurat_integrated_STACAS_standard_pipeline.qs`: largest PBMC stand-in for backend/load benchmarks, with simulated expression and original benchmark dimensions preserved (`38606 genes x 147756 cells`)
- `data/39_shinyApp/36.1_myeloid_Cluster_Markers.xlsx`: synthetic marker sheet consumed by the myeloid conversion path
- `data/39_shinyApp/Xenium_Ctrl_ROI_HE.jpg` and `data/39_shinyApp/Xenium_MS_ROI_HE.jpg`: placeholder histology backgrounds for spatial smoke runs

The package's actual unit + shinytest2 suite lives under `tests/testthat/` (see [`tests/README.md`](../README.md)); `tests/smoke/` is a sandbox for hand-driven exploration with real-size data, plus the canonical reference bench cited in [`vignettes/expression_backend_benchmark.Rmd`](../../vignettes/expression_backend_benchmark.Rmd).

## Layout

```
tests/smoke/
├── README.md             # this file
├── run_all.sh            # interactive Bash runner (menu-driven)
├── data/                 # gitignored — drop your own .qs fixtures here
├── profile/              # gitignored — profvis snapshots
├── result/               # gitignored — per-script outputs (.crb, .h5, plots, csv)
└── src/                  # committed — see "Scripts" below
```

## Prerequisites

- R `>= 3.5.1` with `devtools` + the package's runtime/Suggests deps installed (`devtools::install_dev_deps()`).
- For `12_convert_h5.R` / `93_bench_backend_compare.R` / `94_bench_web_load.R`: **`HDF5Array`** from Bioconductor (`BiocManager::install("HDF5Array")`).
- For `11_convert_bpcells.R`: **`BPCells`** from GitHub.
- For `94_bench_web_load.R`: `callr`, `chromote`, `httr`, `jsonlite` + a working headless Chrome (verify via `chromote::find_chrome()`).
- Optional private fixtures: if you want to compare against your own real corpora, put them under `data/` only after moving the synthetic directory away; the helper will otherwise preserve them by renaming the original directory on first synthetic generation.

## Scripts

### Pipeline (10 → 12: convert; 20 → 22: bundle apps)

| ID  | Script                  | What it does                                                                                |
| --- | ----------------------- | ------------------------------------------------------------------------------------------- |
| 10  | `10_convert_embedded.R` | Convert N Seurat fixtures → `.crb` with `expression_matrix_mode = "embedded"`               |
| 11  | `11_convert_bpcells.R`  | Same, but `"bpcells"` (writes `<stem>.bpcells/` sibling)                                    |
| 12  | `12_convert_h5.R`       | Same, but `"h5"` (writes `<stem>.h5` sibling, TENx layout via `HDF5Array::writeTENxMatrix`) |
| 20  | `20_app_embedded.R`     | `createShinyApp()` bundles embedded crbs into a runnable Shiny app                          |
| 21  | `21_app_bpcells.R`      | Same for bpcells                                                                            |
| 22  | `22_app_h5.R`           | Same for h5                                                                                 |

### Diagnostic / compatibility

| ID  | Script                          | What it does                                           |
| --- | ------------------------------- | ------------------------------------------------------ |
| 30  | `30_verify_class_compat.R`      | Smoke-test `Cerebro_v1.3` R6 class API after refactors |
| 40  | `40_verify_module_load.R`       | Source shared Shiny modules outside a session          |
| 41  | `41_verify_spatial_roundtrip.R` | Spatial slot round-trip                                |

### Smoke + benchmark

| ID     | Script                             | What it does                                                                                                                                                                                                                                                                                                   | Output                                                          |
| ------ | ---------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------- |
| 90     | `90_export_bpcells.R`              | Bpcells exporter smoke test                                                                                                                                                                                                                                                                                    | log only                                                        |
| 91     | `91_attach_bpcells.R`              | Bpcells runtime attach smoke                                                                                                                                                                                                                                                                                   | log                                                             |
| 92     | `92_bench_expression_access.R`     | profvis-driven access pattern bench                                                                                                                                                                                                                                                                            | profile snapshot                                                |
| **93** | `93_bench_backend_compare.R`       | **Server-side** bench across 3 backends (callr-isolated subprocesses): disk / load / attach / RSS / cold + hot + bulk query latency                                                                                                                                                                            | `result/93_bench_backend_compare/{summary.csv, run.log}`        |
| **94** | `94_bench_web_load.R`              | **End-to-end browser** bench: `callr::r_bg` spawns Shiny per backend, `chromote::ChromoteSession` navigates and polls DOM for cell-count text. Records user-perceived "open URL → dataset visible" wall-clock + `performance.timing` TTFB / DOM-ready / `load`                                                 | `result/93_bench_backend_compare/{web_load.csv, web_load.log}`  |
| **95** | `95_bench_backend_plot.R`          | Composite **5-panel plot** consuming both `summary.csv` (server side, from 93) and `web_load.csv` (web side, from 94)                                                                                                                                                                                          | `result/93_bench_backend_compare/{summary.png, summary.pdf}`    |
| **96** | `96_bench_size_scaling.R`          | **Disk-size scaling** bench: synthetic sparse matrices across 3 fixture sizes × 4 backends (embedded / bpcells / bpcells_int / h5); validates when h5 beats embedded on disk and how much BPCells bit-packing saves. See [`96_bench_size_scaling.md`](96_bench_size_scaling.md) for results and interpretation | `result/96_bench_size_scaling/summary.csv`                      |
| **97** | `97_profile_coldpath.R`            | **Cold hot-path profile** (profvis): package load, `.crb` deserialise, metadata/projection accessors, `buildHoverInfoForProjections`, single-gene expression, plotly scattergl build. Writes profvis HTML artefacts + `summary.csv`                                                                            | `result/97_profile_coldpath/`                                   |
| **98** | `98_profile_bpcells_vs_embedded.R` | **BPCells vs embedded micro-benchmark**: crb load time, resident memory, single-gene extraction, 10-gene block, 200-gene panel aggregation, disk footprint. Depends on 90 output                                                                                                                               | `result/98_profile_bpcells_vs_embedded/bpcells_vs_embedded.rds` |
| **99** | `99_profile_deep.R`                | **Deep Rprof dive**: readRDS vs qs::qread, hover-info builder alternatives (current / sprintf / stringi), dgCMatrix row access strategies, plotly build scaling to 50k/200k cells                                                                                                                              | `result/99_profile_deep/rprof_hover_summary.rds`                |

## Running the benchmark

If you want to materialize the synthetic corpus explicitly before any converter runs:

```bash
cd tests/smoke
Rscript src/00_prepare_synthetic_data.R
```

Full bench (server side + web side + plot) on the PBMC All Samples fixture, after the converters have produced their crbs:

```bash
cd tests/smoke
Rscript src/93_bench_backend_compare.R   # ~5 min, 3 callr-isolated subprocesses
Rscript src/94_bench_web_load.R          # ~3 min, 3 callr+chromote sessions
Rscript src/95_bench_backend_plot.R      # ~5 s, generates summary.png
```

Or use the runner in either mode:

```bash
cd tests/smoke
bash run_all.sh         # interactive menu
bash run_all.sh --all   # run everything once, then exit with a status code
bash run_all.sh 94      # run a single script once, then exit
```

The PBMC All Samples crbs are produced by the relevant converters; for an h5-only re-bench the minimal sequence is:

```bash
cd tests/smoke
# regenerate just PBMC All Samples h5 (12 in full runs all 5 fixtures)
Rscript -e '
  suppressPackageStartupMessages({
    devtools::load_all("../..", quiet = TRUE)
    library(qs); library(Seurat)
  })
  seurat_obj <- qs::qread("data/21_S04_seurat_integrated_STACAS_standard_pipeline.qs")
  convertSeuratToCerebro(
    seurat_file = seurat_obj,
    result_dir  = "result/12_convert_h5",
    assay = "RNA", slot = "counts",
    experiment_name = "PBMC All Samples TCR/BCR",
    organism = "Human PBMC",
    groups = c("celltype_merged.l1", "timepoint", "sample"),
    expression_matrix_mode = "h5"
  )'
Rscript src/93_bench_backend_compare.R
Rscript src/94_bench_web_load.R
Rscript src/95_bench_backend_plot.R
```

## Latest results (1.7.0)

PBMC All Samples, 38,606 genes × 147,756 cells, macOS arm64. Numbers are taken verbatim from the most recent run of `93_bench_backend_compare.R` and `94_bench_web_load.R`:

| metric                                   |   embedded |   bpcells |    **h5** |
| ---------------------------------------- | ---------: | --------: | --------: |
| disk_total_mb                            |        681 |       592 |   **391** |
| load_secs                                |       9.18 |      3.57 |      3.43 |
| **attach_secs**                          |      0.015 |     0.201 | **0.087** |
| rss_mb (after attach)                    |      4,452 |     1,215 | **1,146** |
| cold_secs (single gene)                  |      0.510 |     0.737 | **0.012** |
| hot_p50_secs                             |      0.479 |     0.706 | **0.010** |
| hot_p95_secs                             |      0.506 |     0.759 | **0.031** |
| bulk_secs (50 genes × all cells)         |      0.505 |     0.834 | **0.143** |
| **open URL → dataset visible (browser)** | **14.3 s** | **9.2 s** | **8.7 s** |

h5 wins (or ties) on every dimension. bpcells is close on startup/RAM/disk (since 1.7.0 bit-packs integer counts via `convert_matrix_type("uint32_t")`), but ~70× slower than h5 on per-gene queries because its row-oriented packed format has to densify chunks.

## Before/after — h5 lazy refactor (1.6.0 → 1.7.0)

The h5 attach was rewritten in 1.7.0 to use `HDF5Array::TENxMatrix` (lazy `DelayedMatrix` seed) instead of `rhdf5::h5read` + `dgCMatrix` reconstruction. Same fixture, same scripts, same hardware — only the implementation changed:

| metric                     | h5 (1.6.0 eager) | h5 (1.7.0 lazy) |                   improvement |
| -------------------------- | ---------------: | --------------: | ----------------------------: |
| attach_secs                |             22.9 |       **0.087** |                  ~263× faster |
| rss_mb (after attach)      |           11,210 |       **1,133** |                  ~10× smaller |
| cold_secs (single gene)    |             0.45 |       **0.010** |                   ~45× faster |
| hot_p50_secs               |             0.46 |       **0.009** |                   ~51× faster |
| bulk_secs (50 × ncells)    |             0.51 |       **0.157** |                    ~3× faster |
| open URL → dataset visible |           33.0 s |       **8.7 s** |                    ~4× faster |
| disk_total_mb              |              354 |             391 | +10% (TENx metadata overhead) |

`.crb` size (~42 MB) and `load_secs` (~3.3 s) are unchanged — only the attach path and everything downstream of it improved.

The eager 1.6.0 numbers in this table came from the first run of `93_bench_backend_compare.R` against commit `b8cdadc`; to reproduce them, `git checkout b8cdadc -- R/exportFromSeurat.R inst/shiny/v1.4/utility_functions.R` and rerun the same scripts.

## Why a separate `94_bench_web_load.R` instead of shinytest2

`shinytest2::AppDriver$new()` was tried first, but its internal "wait for first idle" detector does not converge within `load_timeout = 300_000` ms on cerebroAppLite's multi-module dashboard against the PBMC All Samples app, so AppDriver init times out before the browser can navigate. `94_bench_web_load.R` bypasses shinytest2 entirely:

- `callr::r_bg()` spawns the Shiny server in its own R subprocess; the script polls the port via `httr::GET` to detect TCP readiness (server up, **not** part of the user-perceived metric).
- `chromote::ChromoteSession$new()` opens a fresh headless Chrome session per backend.
- `t0 = before chromote navigates`; `t1 = first poll iteration where the DOM contains the dataset's cell-count text`. `cell_count_visible_ms = t1 - t0`.
- TTFB / DOM-ready / `load_event` come from the browser's `performance.timing` JSON via `Runtime.evaluate`.

A CI-runnable shinytest2 perf regression test (using `tests/testthat/test-app-inst.R`'s small `example.crb` fixture, where AppDriver does converge) is a reasonable follow-up.
