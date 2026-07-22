[![R-CMD-check (upstream)](https://github.com/mihem/cerebroAppLite/actions/workflows/R-cmd-check.yaml/badge.svg)](https://github.com/mihem/cerebroAppLite/actions/workflows/R-cmd-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
![Lifecycle: stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)



# cerebroAppLite

Interactive visualization of single-cell RNA-seq data, built on top of [Shiny](https://shiny.posit.co/).

cerebroAppLite supports loading pre-processed single-cell data, exploring projections and gene expression, browsing marker genes and enriched pathways, and inspecting group compositions — all through an interactive web interface. The sections below cover the key features.

For the original feature set and data preparation workflows, refer to the upstream cerebroApp documentation at <https://romanhaa.github.io/cerebroApp/> — everything described there works the same way here.

*A community fork of [cerebroApp](https://github.com/romanhaa/cerebroApp) by Roman Hillje, developed and maintained by [mihem](https://github.com/mihem).*

## Contents

- [1. Installation](#1-installation)
- [2. Features](#2-features)
  - [2.1 convertSeuratToCerebro()](#21-convertseurattocerebro)
  - [2.2 createShinyApp()](#22-createshinyapp)
  - [2.3 Choosing an expression backend](#23-choosing-an-expression-backend)
  - [2.4 Other improvements](#24-other-improvements)
- [3. Testing](#3-testing)
  - [3.1 Install the test tooling](#31-install-the-test-tooling)
  - [3.2 Run the tests](#32-run-the-tests)
  - [3.3 precheck: the one-shot local gate](#33-precheck-the-one-shot-local-gate)
  - [3.4 Self-containment of exported apps](#34-self-containment-of-exported-apps)
  - [3.5 Snapshots and further reading](#35-snapshots-and-further-reading)
- [4. License](#4-license)

## 1. Installation

```r
remotes::install_github('mihem/cerebroAppLite')
```

## 2. Features

### 2.1 convertSeuratToCerebro()

`convertSeuratToCerebro()` handles the entire export process in a single call: reading the Seurat object (`.rds` on disk, or one already loaded in memory), renaming grouping variables, loading marker gene tables, calculating most-expressed genes, and saving a `.crb` file.

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
  expression_matrix_mode   = "h5"             # "embedded" | "bpcells" | "h5", see §2.3
)
# → saves output/cerebro_my_seurat.crb (+ sibling .h5 / .bpcells/ when applicable)
```

### 2.2 createShinyApp()

Instead of running `launchCerebro()` interactively, you can generate a self-contained Shiny app directory with all data and source files bundled. This is useful for deploying to a Shiny server or sharing with collaborators.

```r
createShinyApp(
  cerebro_data = c(
    `snRNAseq` = "output/cerebro_snrnaseq.crb",
    `Sample2`  = "output/cerebro_sample2.crb"
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

`cerebro_data` is required and must be a *named* vector / list of `.crb` (or `.rds`) paths — names become the dataset labels users switch between in the app. `result_dir` is optional. Sibling `<stem>.bpcells/` and `<stem>.h5` artefacts produced by the external backends are detected and copied into the bundle automatically (see §2.3). Other knobs available: `colors`, `cerebro_options`, `crb_pick_smallest_file`, `show_upload_ui`, `point_size`, `variable_to_compare` — run `?createShinyApp` for the full list.

### 2.3 Choosing an expression backend

`exportFromSeurat()` (and `convertSeuratToCerebro()`) accept `expression_matrix_mode = c("embedded", "bpcells", "h5")` for how the count matrix is persisted alongside the `.crb`:

| Backend     | Where the matrix lives                  | `.crb` size           | Load behaviour                                            | Extra packages | Portability                                  |
| ----------- | --------------------------------------- | --------------------- | --------------------------------------------------------- | -------------- | -------------------------------------------- |
| `embedded`  | inside the `.crb` itself                | full matrix included  | always in memory after `readRDS`                          | —              | single-file; works with any reader           |
| `bpcells`   | sibling `<stem>.bpcells/` directory     | tiny (handle only)    | **lazy** — `IterableMatrix` reads on slice access         | `BPCells`      | `.crb` + sibling dir must travel together    |
| `h5`        | sibling `<stem>.h5` file (TENx CSC)     | tiny (tag only)       | **lazy** — `HDF5Array::TENxMatrix` seed; queries stream from disk | `HDF5Array`    | `.crb` + sibling `.h5` must travel together  |

Benchmark trade-offs on a PBMC fixture (38,606 genes × 147,756 cells):

| metric                                   | embedded | bpcells | **h5** |
| ---------------------------------------- | -------: | ------: | -----: |
| total disk                               |   681 MB |  592 MB |  391 MB |
| **open URL → dataset visible (browser)** |  14.3 s  |   9.2 s | **8.7 s** |
| RAM (RSS) on the server after attach     |   4.5 GB |  1.2 GB | **1.1 GB** |
| single-gene query, once loaded (cold)    |   0.51 s |  0.74 s | **0.01 s** |

Browser-side TTFB / DOM-ready / `load` are within ~10 ms of each other across backends — all the divergence in the second row is server-side R work (`readRDS` + `.attachExternalExpression()`) plus a constant ~5 s Shiny session handshake.

**Disk-size caveat — it depends on fixture size.** The 391 MB h5 total above is for a large fixture where HDF5's default gzip filter on the TENx CSC layout compresses sparse counts well (~6× vs uncompressed). On small/dense fixtures the trade-off can flip — for example on Roman Hillje's `inst/extdata/v1.4/example.h5` (1000 cells × 500 genes) the sibling `.h5` is actually *larger* than the equivalent embedded `.crb` would have been, because metadata + chunk overhead dominates over compressed payload. **The h5 win on RAM and load time is consistent across fixture sizes; the win on disk only emerges at scale.** Since 1.7.0 the `bpcells` exporter automatically calls `BPCells::convert_matrix_type("uint32_t")` whenever the input values are losslessly representable as non-negative integers (the typical scRNA-seq counts case), which triggers BPCells's bit-packed integer storage and shrinks the sibling by ~5× vs raw double; for normalised float values (`slot = "data"` / `"scale.data"`) the exporter falls back to raw storage to avoid silent precision loss.

Picking one:

- **`h5`** *(recommended default)* — fastest startup, lowest RAM, fastest queries; smallest disk on large fixtures (subject to the caveat above). The TENx CSC layout aligns with how Cerebro reads expression (per-gene = single column slice), and HDF5 page-caching makes repeated reads memory-fast without committing the whole matrix to RAM. Requires the `HDF5Array` Bioconductor package on the host.
- **`bpcells`** — RAM-constrained host with very large matrices, or workloads dominated by chunk-level batched operations rather than per-gene reads. Disk size is similar to h5 on integer counts (bit-packed since 1.7.0); per-gene query is ~0.7 s, so chunk-level batched ops benefit more than per-gene streaming.
- **`embedded`** — single-file convenience (no sibling to manage), or compatibility with very old `.crb` readers. ~14 s end-to-end and pins the full matrix into RAM per loaded copy. Best for small datasets or one-shot scripts.

For reference, before the 1.7.0 lazy h5 refactor, h5 attach was eager (`rhdf5::h5read` + full `dgCMatrix` reconstruction), giving ~33 s open-URL time, ~11 GB RSS, and ~0.45 s queries — i.e. lazy-h5 is the same backend with attach **~263× faster, RAM ~10× smaller, queries ~45× faster, web load ~4× faster**.

`createShinyApp()` already knows about both `<stem>.bpcells/` and `<stem>.h5` and copies them next to the bundled `.crb`. The Shiny runtime re-resolves the sibling location on load via `getExpressionBackend()$location` relative to the `.crb`'s parent directory, so the bundle stays portable.

### 2.4 Other improvements

- **Seurat v5** support throughout (`GetAssayData()`-based slot access)
- Loading spinners on all plot outputs

## 3. Testing

The package ships with a `testthat` + `shinytest2` suite under `tests/testthat/`. CI runs it on every PR (`.github/workflows/R-tests.yaml` and `R-cmd-check.yaml`); the sections below cover running it locally.

### 3.1 Install the test tooling

The shinytest2 suite drives a real headless Chrome via `chromote` and relies on `NOT_CRAN=true` (already set in `tests/testthat/setup.R`). Install the extras once:

```r
install.packages(c("testthat", "shinytest2", "chromote"))
# or pull the whole Suggests block:
devtools::install_dev_deps()
```

### 3.2 Run the tests

From R, loading the dev source via `pkgload::load_all()` (this is what CI's test job does too):

```r
# whole suite
devtools::test()

# one file at a time
devtools::test(filter = "app-inst")          # shinytest2 end-to-end smoke
devtools::test(filter = "exportFromSeurat")  # exporter-only
devtools::test(filter = "r-functions")       # plain unit tests
```

From the shell (CI / scripting):

```bash
# every test_*.R, dev source loaded
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'

# only the shinytest2 suite, with a verbose reporter
NOT_CRAN=true Rscript -e 'devtools::test(filter = "app-inst", reporter = "summary")'
```

### 3.3 precheck: the one-shot local gate

`scripts/precheck.sh` runs the same checks as CI, **on your machine, in the order CI runs them** (air-format → tests → `R CMD check` → pkgdown), so you catch failures before pushing:

```bash
scripts/precheck.sh        # full: air-format + tests + R CMD check + pkgdown
scripts/precheck.sh fast   # quick: air-format + tests only (day-to-day)
scripts/precheck.sh air    # air-format only
```

Run it before pushing. CI air-formats **before** testing, so running the steps out of order lets format-sensitive tests pass locally and fail on CI. This is a local convenience, not CI itself — the authoritative gate is GitHub Actions, which runs on every push regardless of your OS. It needs `air` on `PATH` plus an R with the Suggests packages (from a native R install, or the repo's `default.nix`).

### 3.4 Self-containment of exported apps

Apps built by `createShinyApp()` must stay **self-contained** — they run with no `cerebroAppLite` installed. `test-smoke-production.R` enforces this with a static source check, a hermetic `.crb` deserialize, and a hermetic bundle boot (each in a process whose library path lacks the package), so a bundle that reaches back into `cerebroAppLite` fails a test rather than a user. See [`CONTRIBUTING.md`](CONTRIBUTING.md) for the rule and where runtime code must live.

### 3.5 Snapshots and further reading

Snapshot diffs from `expect_snapshot()` land under `tests/testthat/_snaps/`; review them with `testthat::snapshot_review()` and accept with `testthat::snapshot_accept()` only after confirming the new output is correct.

See [`tests/README.md`](tests/README.md) for the full layout, the `inst_dir` resolution rule, and the gotcha about regenerating `inst/extdata/v1.4/example.crb` after R6 method changes (a stale fixture surfaces as a misleading `Shiny app did not become stable in 15000ms` from shinytest2).

## 4. License

MIT — see [LICENSE.md](LICENSE.md). Original cerebroApp © Roman Hillje; cerebroAppLite fork by [mihem](https://github.com/mihem).
