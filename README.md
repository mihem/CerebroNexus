[![R-CMD-check (upstream)](https://github.com/mihem/cerebroAppLite/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/cerebroAppLite/actions/workflows/R-cmd-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)



# cerebroAppLite

Interactive visualization of single-cell RNA-seq data, built on top of [Shiny](https://shiny.posit.co/).

This is a fork of [cerebroAppLite](https://github.com/mihem/cerebroAppLite) by [mihem](https://github.com/mihem),
itself a slimmed-down fork of the original [cerebroApp](https://github.com/romanhaa/cerebroApp)
by [Roman Hillje](https://github.com/romanhaa). The R-CMD-check badge above tracks mihem's upstream branch.
For general usage, data preparation, and the original feature set, please refer to the official documentation:

> **<https://romanhaa.github.io/cerebroApp/>**

Everything described there (loading data, exploring projections, viewing marker genes, gene expression, etc.) works the same way in cerebroAppLite. The sections below only cover **what this fork adds or changes**.

## Installation

```r
remotes::install_github('duocang/cerebroAppLite')
```

## What's New in This Fork

### 1. `convertSeuratToCerebro()` — one-step data conversion

The original cerebroApp requires you to call `exportFromSeurat()` manually with many parameters. This fork adds a convenience wrapper that handles the entire process in a single call: reading the Seurat object (`.rds` on disk, or one already loaded in memory), renaming grouping variables, loading marker gene tables, calculating most-expressed genes, pulling scRepertoire columns out of `meta.data`, and saving a `.crb` file.

```r
library(cerebroAppLite)

convertSeuratToCerebro(
  seurat_file     = "my_seurat.rds",     # or an in-memory Seurat object
  result_dir      = "output/",
  assay           = "RNA",
  slot            = "data",
  experiment_name = "My Experiment",
  organism        = "Human",
  groups          = c("sample_id", "condition", "cell_type"),
  groups_naming   = list(
    "sample_id" = "sample",
    "cell_type" = "cluster"
  ),
  marker_file              = "markers.csv",   # optional: .csv/.tsv/.txt/.tab
  expression_matrix_mode   = "h5",            # "embedded" | "bpcells" | "h5", see §3
  bcr_file                 = NULL,            # optional: .rds with BCR data
  tcr_file                 = NULL             # optional: .rds with TCR data
)
# → saves output/cerebro_my_seurat.crb (+ sibling .h5 / .bpcells/ when applicable)
```

`.qs` input and `.xlsx` marker tables were dropped in 1.6.0 alongside the `qs` / `readxl` Suggests. If you have either, convert them once with `qs::qread() |> saveRDS()` or open the workbook and re-export as CSV / TSV.

### 2. `createShinyApp()` — generate a deployable Shiny app

Instead of running `launchCerebro()` interactively, you can generate a self-contained Shiny app directory with all data and source files bundled. This is useful for deploying to a Shiny server or sharing with collaborators.

```r
createShinyApp(
  cerebro_data = c(
    `snRNAseq` = "output/cerebro_snrnaseq.crb",
    `TCR-BCR`  = "output/cerebro_vdj.crb"
  ),
  result_dir       = "my_app/",
  welcome_message  = "<h2>My Single-Cell Atlas</h2>",   # rendered via HTML()
  port             = 8080,
  host             = "127.0.0.1",
  max_request_size = 8000,                              # MB
  overwrite        = TRUE
)
# → run with shiny::runApp("my_app/") or deploy to Shiny Server
```

`cerebro_data` is required and must be a *named* vector / list of `.crb` (or `.rds`) paths — names become the dataset labels users switch between in the app. `result_dir` is optional. Sibling `<stem>.bpcells/` and `<stem>.h5` artefacts produced by the external backends are detected and copied into the bundle automatically (see §3). Other knobs available: `colors`, `cerebro_options`, `crb_pick_smallest_file`, `show_upload_ui`, `point_size`, `variable_to_compare` — run `?createShinyApp` for the full list.

This is the slimmed-down variant in this fork — auth, spatial, and Docker-template handling were dropped because they depend on dev-only modules; they will reappear as the corresponding modules land.

### 3. Choosing an expression backend

`exportFromSeurat()` (and `convertSeuratToCerebro()`) accept `expression_matrix_mode = c("embedded", "bpcells", "h5")` for how the count matrix is persisted alongside the `.crb`:

| Backend     | Where the matrix lives                  | `.crb` size           | Load behaviour                                            | Extra packages | Portability                                  |
| ----------- | --------------------------------------- | --------------------- | --------------------------------------------------------- | -------------- | -------------------------------------------- |
| `embedded`  | inside the `.crb` itself                | full matrix included  | always in memory after `readRDS`                          | —              | single-file; works with any reader           |
| `bpcells`   | sibling `<stem>.bpcells/` directory     | tiny (handle only)    | **lazy** — `IterableMatrix` reads on slice access         | `BPCells`      | `.crb` + sibling dir must travel together    |
| `h5`        | sibling `<stem>.h5` file (TENx CSC)     | tiny (tag only)       | **lazy** — `HDF5Array::TENxMatrix` seed; queries stream from disk | `HDF5Array`    | `.crb` + sibling `.h5` must travel together  |

Measured trade-offs on a PBMC fixture (38,606 genes × 147,756 cells). Server-side metrics from `tests/smoke/src/93_bench_backend_compare.R` (callr-isolated, three backends each in a fresh R subprocess). End-to-end browser metric from `tests/smoke/src/94_bench_web_load.R` (callr-spawned Shiny + chromote-driven headless Chrome, fresh session per backend). Full methodology and a 5-panel plot in [`vignettes/expression_backend_benchmark.Rmd`](vignettes/expression_backend_benchmark.Rmd):

| metric                                   | embedded | bpcells   |  **h5** |
| ---------------------------------------- | -------: | --------: | ------: |
| total disk                               |   681 MB |  2,600 MB |  391 MB |
| **open URL → dataset visible (browser)** |  14.2 s  |    9.2 s  | **8.7 s** |
| RAM (RSS) on the server after attach     |   4.5 GB |    1.2 GB | **1.1 GB** |
| single-gene query, once loaded (cold)    |   0.50 s |    1.23 s | **0.01 s** |

Browser-side TTFB / DOM-ready / `load` are within ~10 ms of each other across backends — all the divergence in the second row is server-side R work (`readRDS` + `.attachExternalExpression()`) plus a constant ~5 s Shiny session handshake.

Picking one:

- **`h5`** *(recommended default)* — smallest disk, fastest startup, lowest RAM, fastest queries. The TENx CSC layout aligns with how Cerebro reads expression (per-gene = single column slice), and HDF5 page-caching makes repeated reads memory-fast without committing the whole matrix to RAM. Requires the `HDF5Array` Bioconductor package on the host.
- **`bpcells`** — RAM-constrained host with very large matrices, or workloads dominated by chunk-level batched operations rather than per-gene reads. Largest disk of the three but only paid once per dataset; single-gene query is ~1 s.
- **`embedded`** — single-file convenience (no sibling to manage), or compatibility with very old `.crb` readers. ~14 s end-to-end and pins the full matrix into RAM per loaded copy. Best for small datasets or one-shot scripts.

For reference, before the 1.7.0 lazy h5 refactor, h5 attach was eager (`rhdf5::h5read` + full `dgCMatrix` reconstruction), giving ~33 s open-URL time, ~11 GB RSS, and ~0.45 s queries — i.e. lazy-h5 is the same backend with attach **~263× faster, RAM ~10× smaller, queries ~45× faster, web load ~4× faster** (see [`expression_backend_benchmark.Rmd`](vignettes/expression_backend_benchmark.Rmd) for the comparison).

`createShinyApp()` already knows about both `<stem>.bpcells/` and `<stem>.h5` and copies them next to the bundled `.crb`. The Shiny runtime re-resolves the sibling location on load via `getExpressionBackend()$location` relative to the `.crb`'s parent directory, so the bundle stays portable.

### 4. Other Improvements

- **Seurat v5** support throughout (`GetAssayData()`-based slot access)
- Loading spinners on all plot outputs

## Testing

The package ships with a `testthat` + `shinytest2` suite under `tests/testthat/`. CI (`.github/workflows/R-cmd-check.yaml`, `.github/workflows/R-tests.yaml`) runs it on every PR; locally:

```r
# whole suite (loads dev source via pkgload::load_all)
devtools::test()

# one file at a time
devtools::test(filter = "app-inst")          # shinytest2 end-to-end smoke
devtools::test(filter = "exportFromSeurat")  # exporter-only
devtools::test(filter = "r-functions")       # plain unit tests
```

`test-app-inst.R` drives a real headless Chrome via `chromote` and relies on `NOT_CRAN=true` (already set in `tests/testthat/setup.R`). Install the extras once:

```r
install.packages(c("testthat", "shinytest2", "chromote"))
# or pull the whole Suggests block:
devtools::install_dev_deps()
```

From the shell (CI / scripting):

```bash
# what R CMD check effectively runs (uses installed package, not dev source)
Rscript -e 'testthat::test_dir("tests/testthat")'

# only the shinytest2 suite, with a verbose reporter
NOT_CRAN=true Rscript -e 'devtools::test(filter="app-inst", reporter="summary")'
```

Snapshot diffs from `expect_snapshot()` land under `tests/testthat/_snaps/`; review them with `testthat::snapshot_review()` and accept with `testthat::snapshot_accept()` only after confirming the new output is correct.

See [`tests/README.md`](tests/README.md) for the full layout, the `inst_dir` resolution rule, and the gotcha about regenerating `inst/extdata/v1.4/example.crb` after R6 method changes (a stale fixture surfaces as a misleading `Shiny app did not become stable in 15000ms` from shinytest2).

## License

MIT — see [LICENSE.md](LICENSE.md). Original cerebroApp © Roman Hillje; cerebroAppLite fork by [mihem](https://github.com/mihem).
