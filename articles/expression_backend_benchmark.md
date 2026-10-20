# Large-dataset benchmarks: expression, storage and Viewer

## Scope and evidence

This article separates expression access, CRB persistence, server
preparation, and browser rendering. Their timings have different
boundaries and cannot be added into an overall Viewer speedup. For
backend selection, sidecar portability and launching the demo, see
[Working with large
datasets](https://mihem.github.io/CerebroNexus/articles/million_cell_crb_artifacts.md).

The first section reports the current real-data expression-backend
benchmark. The sections after it are **historical experiments**, kept as
recorded; they were not remeasured under that protocol. A passed
regression suite does not establish a performance result.

All tables read the small, checked-in records under
`inst/extdata/benchmarks/`. These records are included in installed
packages; the figures are pre-drawn images. Rendering this article
requires neither a large dataset nor a developer cache. Do not overwrite
recorded experiments with a new run.

## Real-data expression backends (Scale run, 2026-10-18)

This study compares four ways to store the expression matrix: H5 with
gzip level 6 (HDF5Array’s default, used by the exporter before 5.1.3),
the same H5 file at gzip level 0, i.e. written without compression (the
[`exportFromSeurat()`](https://mihem.github.io/CerebroNexus/reference/exportFromSeurat.md)
default from 5.1.3; `h5_compression_level` changes it), BPCells, and
embedded storage. In the tables and figures, `h5_uncompressed` is the
gzip level 0 file. Two public datasets were used: the 10x mouse brain
E18 matrix (1,306,127 cells, 27,998 genes, 2.6 billion nonzeros) and the
PsychAD human prefrontal cortex HBCC matrix (1,486,324 cells, 34,176
genes, 6.1 billion nonzeros). Each source was measured at 1k, 10k, 50k,
100k, 300k and 500k cells and at its full size.

Every source, size and backend combination was built five times, and
every build was opened in two fresh R processes, one thread each. All
processes read the same cells and the same fixed 12-gene panel. Lazy
reads were fully materialized inside the timer, and the values were
checked against the source matrix afterwards; all 540 access processes
matched. Embedded storage was measured up to 500k cells. The full
matrices exceed its sparse-index limit, so there are no full-size
embedded results.

The run is `20261018T101740Z-1ed063d363a0-scale`, measured at commit
`1ed063d3` on one Linux host (Intel i7-14700, 128 GB RAM, R 4.6.1,
BPCells 0.3.1, HDF5Array 1.40.0). All 270 builds succeeded. The table
below keeps the median of each measurement; the raw files of the run are
not part of this repository.

``` r
scale <- read_tsv("expression-backends-scale-20261018.tsv")
full_size <- scale[scale$n_cells == max(scale$n_cells[scale$source == "mouse_brain_e18"]) |
  scale$n_cells == max(scale$n_cells[scale$source == "human_pfc_hbcc"]), ]
full_size$total_gib <- full_size$total_mib / 1024
knitr::kable(full_size[, c("source", "n_cells", "backend", "total_gib", "build_s",
  "startup_s", "rss_mib", "warmed_p50_s", "block_12gene_s")], digits = 3,
  row.names = FALSE)
```

| source | n_cells | backend | total_gib | build_s | startup_s | rss_mib | warmed_p50_s | block_12gene_s |
|:---|---:|:---|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1486324 | bpcells | 27.044 | 542.2 | 3.54 | 697 | 0.828 | 0.902 |
| human_pfc_hbcc | 1486324 | h5 | 9.838 | 2096.0 | 6.73 | 868 | 0.055 | 0.641 |
| human_pfc_hbcc | 1486324 | h5_uncompressed | 69.006 | 634.8 | 6.56 | 868 | 0.036 | 0.572 |
| mouse_brain_e18 | 1306127 | bpcells | 2.836 | 154.0 | 3.43 | 677 | 0.591 | 0.891 |
| mouse_brain_e18 | 1306127 | h5 | 3.614 | 526.7 | 5.72 | 838 | 0.034 | 0.338 |
| mouse_brain_e18 | 1306127 | h5_uncompressed | 29.738 | 194.7 | 6.04 | 837 | 0.025 | 0.296 |

At full size, H5 at gzip level 0 builds about three times faster than H5
at level 6 and reads single genes a quarter to a third faster, but it
needs about seven to eight times more disk. BPCells has the smallest
memory after load and the fastest 12-gene reads on a shuffled 100k-cell
subset, but a warmed full-cell single-gene read takes 0.6-0.8 s, against
0.02-0.06 s for H5. Embedded storage is competitive only for small
datasets: at 500k cells its single-gene read takes 1.7-3.2 s and its
memory after load is about 12-23 GiB.

![Scale study. Medians across five builds against cell count (log10
axis); whiskers show the observed minimum and maximum. The dotted line
and open symbols mark the full-size endpoint. a-b warmed single-gene
read, c-d 12-gene block read, e-f memory after load, g-h total on-disk
size.](img/expression_backend_benchmark_scale.png)

Scale study. Medians across five builds against cell count (log10 axis);
whiskers show the observed minimum and maximum. The dotted line and open
symbols mark the full-size endpoint. a-b warmed single-gene read, c-d
12-gene block read, e-f memory after load, g-h total on-disk size.

![Full-size endpoints of the same run. Small dots are single read
processes (ten per backend) or builds (five per backend, panels g-h);
the large symbol is the median across builds and the whisker the
observed minimum and
maximum.](img/expression_backend_benchmark_full.png)

Full-size endpoints of the same run. Small dots are single read
processes (ten per backend) or builds (five per backend, panels g-h);
the large symbol is the median across builds and the whisker the
observed minimum and maximum.

The benchmark harness, the run’s summary tables and the figure code are
in
[CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code)
(`benchmark/`). The full-size figure uses the Scale run’s own full-size
endpoints, not the separate Full run
(`20261017T174502Z-63d5337c3647-full`). Timings come from one host and
are not a guarantee for other hardware; the operating-system file cache
was not cleared between processes.

## Ren v2: expression backends

The recorded 2026-10-08 comparison used matching BPCells and HDF5 CRBs
with 1,462,702 cells and 27,943 genes. Each backend ran in three fresh R
processes, with alternating backend order. A separate planning process
froze a 12-gene panel and reference value fingerprints. Every
measurement verified identical single-row and block values outside the
timed expressions.

`startup_seconds` includes source-package loading and CRB loading.
First-gene latency is the first getter call in that process;
repeated-gene latency is a within-process median. The 12-gene block is
materialized. A fresh process does not imply a cold operating-system
disk cache. These are R-side measurements, not time until a browser page
becomes usable. The retained TSV does not contain a complete
host/package manifest, limiting portability and exact environment
reproduction; future reruns must retain it alongside their observations.

``` r
ren <- read_tsv("ren-backends-20261008.tsv")
stopifnot(all(ren$correctness == "OK"))
metrics <- c("startup_seconds", "first_gene_seconds", "hot_gene_p50_seconds", "twelve_gene_seconds")
ren_summary <- aggregate(ren[metrics], list(backend = ren$backend), stats::median)
knitr::kable(ren_summary, digits = 3)
```

| backend | startup_seconds | first_gene_seconds | hot_gene_p50_seconds | twelve_gene_seconds |
|:---|---:|---:|---:|---:|
| bpcells | 15.393 | 1.884 | 0.929 | 1.209 |
| h5 | 17.067 | 1.259 | 0.069 | 1.551 |

![Historical Ren v2 comparison; medians of three fresh-process rounds.
Backend computation, not browser page
load.](img/expression_backend_benchmark_ren.png)

Historical Ren v2 comparison; medians of three fresh-process rounds.
Backend computation, not browser page load.

On this workload, HDF5 has lower repeated single-gene latency, while
BPCells has lower materialized block latency. This does not establish a
universal backend ranking: storage orientation, query shape, data
density and caching all matter. In particular, results from older
BPCells layouts must not be used to describe the current gene-major
layout without a matched rerun.

## Historical mouse-million experiments

The input was the official 10x E18 mouse-brain matrix (1,306,127
barcodes, 27,998 features), reduced deterministically to one million
evenly spaced cells. The preparation used a 50,000-cell sketch (seed
123), normalization, variable features, 30-component PCA, neighbors,
clustering at resolution 1 and UMAP, then projected results to all
selected cells. Counts used BPCells uint32 storage; thin RDS and qs2
CRBs reused the same sidecar.

The 2026-09-12 backend records used an Apple M1 Pro with 32 GiB RAM,
macOS 27.0, R 4.6.1 arm64, BPCells 0.3.1 and Seurat 5.5.1. Exact source
revisions and repeat counts are retained in
`million_cell_backend_hot_paths_manifest.tsv`. The column named `latest`
in that historical file refers to its recorded candidate `3fb88b8a`,
**not the current checkout**. Baselines are `69893a2b` and `27303f21`.
The two baseline backend implementations were identical; their repeated
columns are not independent experiments.

### CRB lifecycle

Five alternating warm-cache rounds measured control-file size, writing,
physical decoding and hydrated reading. Hydration includes sidecar
attachment and canonical-cell checksum validation. It does not include
browser rendering.

``` r
lifecycle <- utils::read.csv(file.path(records, "million_cell_crb_pr165.csv"))
knitr::kable(lifecycle[, c("candidate", "size_mib", "write_median_ms",
  "decode_median_ms", "hydrated_median_ms")], digits = 2)
```

| candidate | size_mib | write_median_ms | decode_median_ms | hydrated_median_ms |
|:----------|---------:|----------------:|-----------------:|-------------------:|
| pr165     |    26.83 |            8001 |             1296 |               1479 |
| thin_rds  |    13.92 |            2947 |               93 |                356 |
| thin_qs2  |    13.21 |             231 |               41 |                297 |

### Server hot paths

Three warm-cache repetitions measured filtering, canonical indexing and
expression reads, with equality checks before timing. Allocation
describes R allocations, not browser heap or whole-process RSS. A
recorded zero-millisecond value is below the timer resolution, not
literally free computation.

``` r
hot <- read_tsv("million_cell_backend_hot_paths.tsv")
knitr::kable(hot[, c("metric", "scale", "pr165_ms", "latest_ms",
  "pr165_alloc_mib", "latest_alloc_mib", "check")], digits = 1)
```

| metric | scale | pr165_ms | latest_ms | pr165_alloc_mib | latest_alloc_mib | check |
|:---|:---|---:|---:|---:|---:|:---|
| full projection selection | 1,000,000 cells; all groups; 100% | 197 | 31 | 199.1 | 19.1 | equal |
| filtered projection selection | 1,000,000 cells; 17/33 clusters; 25% = 134557 | 329 | 28 | 166.5 | 39.5 | equal |
| cell index resolution | 1,000,000 shuffled canonical indices vs barcode match | 79 | 0 | 27.1 | 0.0 | equal |
| single-gene expression | 1 gene x 1,000,000 cells; canonical indices; BPCells | 4063 | 4022 | 154.1 | 157.1 | equal |
| shuffled single-gene expression | 1 gene x 100,000 shuffled cells; storage-order BPCells read | 7886 | 5881 | 33.4 | 22.3 | equal |
| RGB expression | 3 genes x 1,000,000 cells; canonical indices; 3 reads vs 1 | 12498 | 4021 | 461.4 | 226.1 | equal |
| multi-panel expression | 9 genes x 1,000,000 cells; canonical indices; transpose removed | 4309 | 4189 | 562.3 | 451.0 | equal |
| mean expression | 100 genes x 1,000,000 cells; canonical indices; backend-native | 5912 | 3931 | 1916.8 | 65.4 | equal |

The accompanying `million_cell_backend_bundle.tsv` records historical
Linked bundle construction and JSON costs; it predates later
geometry/resource changes. Static HTTP geometry and metadata must be
counted separately from WebSocket payloads. A tiny primary message is
not the complete network cost.

### Browser acceptance boundaries

Keep cold first entry, warm entry, revisit and repeated operations
separate. Record draw submission, GPU completion, painted
acknowledgement and progressive completion separately; global Shiny idle
is not a substitute for page readiness. The request must match the
current dataset generation, identity and selection. Use the same
dataset, viewport, source revision, browser and initial state on both
sides. Record timing and memory profiles in separate runs.

The historical five-round browser series and its immutable
candidate/fixture identities are kept in
[CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code)
under `data/viewer_performance/million_cell_pages_pr4_pr5_4_6_3*`. Those
records used macOS Chrome and an older completion boundary for Linked
Views. Later Windows Brave acceptance used **WSL R**, not Windows-native
R. Neither series can be pooled with the other. Durable results and
limitations are summarized in `PR184_ACCEPTANCE.md` in the same folder.
Stable Gene first usability below three seconds has not been
established, and there is no matched current-release, all-page speedup
claim.

## Earlier exploratory pilot

The 2026-07-30 embedded/BPCells/HDF5 sweep is retained in Git history
under `tests/bench/result/archive/pilot-2026-07-30/`. It used real
mouse-brain and human-PFC counts, synthetic grouping/embeddings, and
contiguous cell blocks. The macOS arm64 host had 32 GB RAM. The run
preceded balanced independent repeats and complete provenance; its
tables and figures are exploratory.

Its useful lesson is that dense realization and construction of a large
in-memory Seurat object can dominate memory before a backend is written.
Cell count alone is not a memory budget: density and nonzero count
matter. Do not generalize its approximately 53 bytes/nonzero peak or old
backend ranking to current streaming exports. Raw pilot CSVs remain
available for inspection; current figure scripts intentionally require
validated repeated runs instead of treating this pilot as release
evidence.

## Reproduce figures and rerun measurements

The figure code and every measurement script are in
[CerebroNexus_paper_code](https://github.com/duocang/CerebroNexus_paper_code):
`viewer/` holds the Viewer, CRB and Ren diagnostics and the Ren figure
(`viewer/scripts/plot_ren_backend_acceptance.R`), and `benchmark/` holds
the repeated H5 / BPCells / embedded experiment. They run against a
CerebroNexus checkout and need its declared dependencies and enough
storage; they do not work from an installed package alone. Record
`git rev-parse HEAD`,
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html),
host/browser versions and artifact checksums with every new output, and
save new runs outside the recorded-data directory.

The million-cell and Ren demo data used by `run-demo.R` are prepared by
the scripts in `tests/demo/` of a CerebroNexus checkout. For the mouse
preparation, allow at least 16 GiB free disk for the recorded artifact
layout, additional space for parallel backends/outputs, and tens of
minutes or longer depending on hardware and downloads. The source alone
is about 3.93 GiB. Full Ren preparation has separate large
download/storage costs; inspect the preparation script and available
disk before starting it. Do not use an arbitrary memory limit or a
historical completion time as a guarantee.

``` r
source("tests/demo/prepare_viewer_1m_data.R")
pkgload::load_all(".")
crb <- prepareViewer1mBenchmarkData()
```

Preserve raw rounds, source identities, failed observations and limits;
never replace them with a single favorable timing.
