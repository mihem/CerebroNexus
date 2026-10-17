# Real-data expression-backend benchmark

For the PR184 closeout, validation limits and archived investigation evidence,
see [PR184_ACCEPTANCE.md](PR184_ACCEPTANCE.md). Keep new one-off diagnostic
output outside the checkout; the reusable benchmark tools below remain here.

This directory compares the `embedded`, `bpcells`, and `h5` backends on public single-cell matrices. This page explains how to run it. Read [METHODOLOGY.md](METHODOLOGY.md) for the experimental design and [RESULTS.md](RESULTS.md) before interpreting any number.

## Million-cell page readiness

The page harness uses the official 1M CRB at 100% cells. Because that data set has no immune repertoire, HLA typing, or trajectory, first create a deterministic benchmark-only CRB in the same directory so it can share the expression sidecar:

```bash
Rscript tests/bench/prepare_viewer_1m_pages.R . /path/to/cerebro_mouse_brain_1m.crb /path/to/cerebro_mouse_brain_1m_pages.crb
Rscript tests/bench/benchmark_viewer_1m_pages.R current=. /path/to/cerebro_mouse_brain_1m_pages.crb /path/to/viewer_1m_pages.tsv 3
```

The unified harness measures Overview, Gene Expression, Groups, Coordinated Views, Immune Repertoire, HLA, Trajectory, and Spatial. The enforced fresh-page budget is strictly `<2,000 ms` for ordinary pages and `<3,000 ms` for Trajectory and HLA; every repeat visit must be strictly `<500 ms`. Spatial remains optional when unavailable. Synthetic fixture additions are performance fixtures only and must not be used for biological conclusions.

The measurement contract has four separate boundaries:

| Boundary | Definition | Formal use |
|---|---|---|
| `dataset_load_ms` | benchmark load request for the selected dataset to a fully materialized CRB, Viewer Pack, exact cell count, and dataset fingerprint | reported separately; never included in page timing |
| `primary_ready_ms` | browser tab click to the request-bound event proving the correct primary result has been drawn | official page KPI and budget field |
| `settled_ms` | browser tab click to the page's completion event, or `max(primary-ready, Shiny idle)` when no progressive completion exists | engineering diagnostic only |
| repeat `primary_ready_ms` | leave a warmed page for Data Info, then click back and wait for a new request-bound ready event | cache/hydration KPI |

`fresh` means the first visit to that page in a new R/browser process **after the full dataset load gate has passed**. It does not mean an empty Windows filesystem cache, the first R launch on the host, or application startup. Every candidate/page/visit row has its own Shiny R process and browser process, so page caches and GPU buffers cannot leak between fresh observations.

The click generation, `performance.now()` timestamp, event listener, and click are installed in one synchronous browser script. A ready event is accepted only when its benchmark generation, exact dataset fingerprint, page identity, rendered count, and page-specific state match the current request. Correctness is therefore part of the primary boundary rather than a later DOM-only check. Groups uses `plotly_afterplot` (or a validated cached activation) for its selected metric and does not wait for global Shiny idle. Specialist canvases use their post-draw `cerebro:specialist-state`. Linked Views uses `primaryReady` for primary and `ready` for its progressive supplement. Repeat observations must emit a new event; persisted DOM state alone cannot pass.

The dedicated Ren Linked transport benchmark reports three distinct endpoints. `primary_logical_ms` and `complete_logical_ms` use the browser's monotonic clock from the tab click to the corresponding page event; the `driver_*` fields retain the surrounding automation wait for diagnosing polling overhead. `complete_logical_ms` means the supplement has been merged; `visual_ready_ms` additionally requires every visible GPU renderer to become idle, a clonal canvas when receptor data exist, and two consecutive animation frames with identical panel geometry and opacity; `interactive_ready_ms` then waits for Shiny to become idle. Use the latter two when comparing against perceived page readiness. Coordinated Views transport reports `projectionBytes` and `metadataBytes` separately because validated Viewer Pack coordinates and initial categorical codes use HTTP resource paths rather than the Shiny websocket. Projection resource time is further split into download and decode, while renderer initialization and apply times distinguish prewarming from per-data-set construction.

The default `VIEWER_BENCH_MODE=timing` never starts recursive process-tree RSS polling. It retains only browser event timestamps, a lightweight WebSocket byte counter, and post-boundary correctness diagnostics. `VIEWER_BENCH_MODE=memory` starts the recursive R/browser RSS sampler and records peak R RSS, browser RSS, and JS heap; its elapsed values are marked `profiler_instrumented` and are excluded from page budgets and formal timing summaries. Run the two modes separately.

The browser benchmarks (`benchmark_viewer_1m_pages.R`, `benchmark_million_cell_viewer.R`, `benchmark_ren_linked_transport.R`) share `lib/viewer_launch.R`. By default (`VIEWER_BENCH_LOAD=installed`) each candidate is installed into a temporary library with `R CMD INSTALL` and launched with `library(CerebroNexus)`, as deployments run it. `VIEWER_BENCH_LOAD=load_all` uses `devtools::load_all()` instead, which also attaches every DESCRIPTION Imports package; the much larger R heap makes each full garbage collection take seconds and shifts pauses onto whichever page step allocates next, so use it only to profile an uninstalled source tree. The page benchmark manifest records the mode as `viewer_load_mode`.

The raw TSV, schedule, and manifest record benchmark mode, Git SHA, fixture and Viewer Pack hashes, browser, dataset load, primary, settled, correctness, renderer, transport, and (memory mode only) resource fields. Reports use P50, min–max, every raw observation, and correctness pass rate. Publication requires at least five rounds; 7–10 rounds are preferred for final evidence.

Historical reports are retained but are not pooled with this contract. Results that waited for global idle are `idle-inclusive`; results collected with recursive RSS polling are `profiler-instrumented`; older mixed-boundary reports are `legacy / diagnostic`. In particular, the historical Groups `2.640 s` remains an idle/profiler-inclusive diagnostic, while the earlier `1.1–1.2 s` is a reference that must be reconfirmed under unified primary-ready timing. Historical Gene, Spatial, HLA, Trajectory, and Overview values likewise cannot be combined into a publication table.

For a balanced comparison, pass additional `LABEL=REPO_ROOT` candidates after the round count. Candidate position rotates by round. Set `VIEWER_BENCH_PROFILE=publication` and use at least five rounds; shorter publication runs are rejected before launching an app.

```bash
VIEWER_BENCH_PROFILE=publication Rscript tests/bench/benchmark_viewer_1m_pages.R baseline=/path/to/baseline /path/to/cerebro_mouse_brain_1m_pages.crb /path/to/viewer_1m_pages.tsv 5 candidate=/path/to/candidate
```

On Windows, the reproducible wrapper resolves Git refs to immutable commits,
creates or reuses clean detached worktrees, runs a balanced three-candidate
comparison, and writes raw plus aggregated results outside the source tree:

```powershell
pwsh tests/bench/run_viewer_page_comparison.ps1 `
  -Artifact C:\data\cerebro_mouse_brain_1m_pages.crb `
  -Profile publication
```

The defaults compare the merge base before PR0 (`892097a1`), the fixed PR5
reference (`6c9718d7`), and the current checkout’s committed `HEAD`. Refs
are recorded together with their resolved SHA in `run-config.tsv`. The default
output root is the `CerebroNexus-benchmarks` directory beside the repository.
Each run writes `raw.tsv`, `raw_schedule.tsv`, `raw_manifest.tsv`, `run.log`,
`page-summary.tsv`, `comparison.tsv`, and `page-summary.md`. Use `-Profile quick`
for a one-round preflight, `-Pages overview,trajectory,spatial` to restrict a
diagnostic run, and explicit `-BaselineRef`, `-ReferenceRef`, or `-CandidateRef`
values to reproduce historical comparisons. Add `-Visits first` for a faster
first-open smoke test. Publication runs should keep the default of measuring
both `first` and `repeat` visits.

For a reproducible first-open comparison in which each revision builds and
uses its own Viewer Pack, run:

```powershell
pwsh tests/bench/run_viewer_release_comparison.ps1 `
  -Artifact C:\data\cerebro_mouse_brain_1m_pages.crb `
  -Rounds 3
```

This creates isolated hard-linked fixtures, builds one pack from the fixed PR5
revision and one from the latest candidate revision, runs every available page
three times with `VIEWER_VISITS_ONLY=first`, and stores raw data, manifests,
logs, resolved Git SHAs, and summaries below the sibling
`CerebroNexus-benchmarks` directory.

> **Historical result:** a complete five-round `publication` profile comparison between clean PR4 and PR5 revisions is recorded in `results/million_cell_pages_pr4_pr5_4_6_3.tsv`. All 140 observations passed status and correctness checks. The overall comparison does not pass the publication gate because both candidates contain budget failures. PR5 itself passes 11/14 page/visit gates; only its first-visit Gene Expression, Immune Repertoire, and Coordinated Views remain over budget. Use the run as a complete diagnostic comparison under its historical completion-ready Coordinated Views contract, not as a current primary-frame result. The archived pilot is retained for provenance only.

## Quick start

```bash
# Smallest correctness and harness check
BENCH_PROFILE=quick tests/bench/run_sweep.sh

# Repeated local review
BENCH_PROFILE=standard tests/bench/run_sweep.sh

# Repeated evidence plus staged figures
BENCH_PROFILE=publication tests/bench/run_sweep.sh

# Explicit memory-boundary experiment; normally rejected on a 32 GiB host
BENCH_PROFILE=stress tests/bench/run_sweep.sh
```

Limit a run to one source when developing the harness:

```bash
BENCH_SOURCES_ONLY=mouse_brain_e18 \
  BENCH_PROFILE=quick tests/bench/run_sweep.sh
```

`BENCH_ALLOW_UNSAFE=1` bypasses the resource gate. Use it only for an intentional stress run. Normal runs must not silently skip unsafe tiers.

## What happens before a download

The command first:

1. inspects source dimensions;
2. records the machine and Git revision;
3. creates the requested run plan; and
4. checks estimated memory, sparse-index, and free-disk limits.

If the plan is unsafe, it stops with the source, cell tier, estimated memory, safe budget, and reason. No complete source file or backend export has started at that point.

## Profiles

| profile | purpose | large boundary tiers |
|---|---|:---:|
| `quick` | verify the harness and correctness gate | no |
| `standard` | repeated local comparison | no |
| `publication` | repeated article evidence and figures | no |
| `stress` | opt-in host memory-boundary experiment | yes |

## Outputs

Validated runs are immutable under `result/runs/<run-id>/`. `result/CURRENT` contains the run used by report and plotting tools. A failed or interrupted run leaves the previous pointer unchanged.

The 2026-07-30 single-run pilot is retained under `result/archive/pilot-2026-07-30/`; it is superseded and cannot support current performance claims.

## Plain-language script map

| script | meaning |
|---|---|
| `01_inspect_data.R` | find out how large the sources are |
| `02_record_environment.R` | record the code and machine under test |
| `03_plan_runs.R` | list the requested backend runs |
| `04_check_resources.R` | stop before running a plan that will not fit |
| `10_export_backend.R` | export one backend in a fresh process |
| `20_measure_backend.R` | measure and correctness-check one backend |
| `30_check_measurements.R` | reject incomplete or incorrect measurements |
| `40_write_report.R` | generate the Markdown result report |
| `41_draw_figures.R` | generate publication figures |
| `50_check_outputs.R` | ensure the report package is complete |
| `60_publish_results.R` | publish immutably and update `CURRENT` last |

The two default public sources are 10x mouse brain E18 (4.2 GB) and the HBCC human prefrontal-cortex atlas (14.2 GB). The MSSM cohort is opt-in through `BENCH_SOURCES_EXTRA=human_pfc_mssm`.


## Recorded figures and installed documentation

The canonical small records used by the large-data benchmark vignette are in
`inst/extdata/benchmarks/`, including the Ren comparison, historical backend
hot paths, CRB lifecycle summaries and their provenance manifest. They were
moved from `tests/bench/acceptance/` and `tests/bench/results/`; do not maintain
second numerical copies. They are installed so documentation can render offline.

From the repository root, reproduce the Ren figure without measurements:

```bash
Rscript tests/bench/plot_ren_backend_acceptance.R inst/extdata/benchmarks/ren-backends-20261008.tsv ren-backends.png
```

See `vignettes/expression_backend_benchmark.Rmd` for boundaries and optional
reruns, and `vignettes/million_cell_crb_artifacts.Rmd` for user workflows.
`tests/bench/` is excluded from R source-package builds, so measurement reruns
require a checkout. The comparison wrappers resolve committed `HEAD`; uncommitted
changes are not included. Supply explicit revisions for historical experiments.
