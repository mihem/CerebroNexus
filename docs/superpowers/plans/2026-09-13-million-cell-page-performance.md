# Million-cell page performance implementation plan

> **For the implementer:** Execute the tasks in order. Local verification is intentionally deferred by explicit user request; do not report the branch as tested.

**Goal:** Rebuild PR4 from PR3 as one measured performance branch covering gene-major expression storage, shared page activation, targeted page optimizations and a reproducible one-million-cell browser benchmark.

**Architecture:** Keep exact data semantics in R, reduce transport to compact query results, and reuse the existing Canvas/WebGPU renderer for large scatter views. Generate missing HLA/TCR and trajectory benchmark payloads deterministically outside Git.

**Technical stack:** R, Shiny, BPCells, JavaScript, existing Canvas/WebGPU cell renderer, chromote.

---

### Task 1: Restore gene-major BPCells storage

**Files:**
- Modify: `R/exportFromSeurat.R`
- Modify: `tests/testthat/test-cerebro-io.R`
- Modify: `tests/testthat/test-seurat-v5-split-layers.R`
- Create: `tests/bench/benchmark_bpcells_storage_order.R`

- [ ] Apply the already reviewed rebased implementation commit from the backed-up PR4.
- [ ] Preserve PR3's five-field CRB schema v2 and infer storage order from the BPCells sidecar.
- [ ] Commit as `perf: store BPCells genes row-major`.

### Task 2: Defer inactive page work

**Files:**
- Create: `inst/viewer/source_cache.R`
- Modify: `inst/viewer/shiny_server.R`
- Modify: `inst/viewer/utility_functions.R`
- Modify: projection reactives under `inst/viewer/overview/`, `inst/viewer/gene_expression/` and `inst/viewer/spatial/` only where they currently debounce or build hover payloads eagerly.
- Modify: focused Viewer contract tests.

- [ ] Port parsed-source reuse without sharing session state.
- [ ] Gate `suspendWhenHidden = FALSE` outputs by owning sidebar page.
- [ ] Deliver the first complete render immediately and debounce only later interactions.
- [ ] Use columnar hover fields and build browser text only for the pointed cell.
- [ ] Prevent specialist pages from triggering the complete Linked Views bundle.
- [ ] Commit as `perf: defer inactive viewer pages`.

### Task 3: Optimize page-specific million-cell paths

**Files:**
- Modify: `inst/viewer/groups/` aggregation and metric rendering files.
- Modify: `inst/viewer/trajectory/` projection and data preparation files.
- Modify: `inst/viewer/hla_tcr_motifs/` aggregation and rendering files.
- Modify: `inst/viewer/spatial/` session cache paths when still missing from current code.
- Modify: existing page-focused tests only when behavior contracts change.

- [ ] Keep Groups counts exact and replace raw million-value violin transfer with compact density and quantile data.
- [ ] Send Trajectory scatter data through the existing million-cell renderer while preserving full pseudotime/state statistics.
- [ ] Cache HLA/TCR cell normalization and send aggregated network data rather than cell rows.
- [ ] Reuse Spatial image encodings and hull geometry within one dataset session.
- [ ] Commit as `perf: accelerate million-cell pages`.

### Task 4: Add page-ready evidence

**Files:**
- Create: `tests/bench/generate_viewer_1m_pages.R`
- Create: `tests/bench/benchmark_viewer_1m_pages.R`
- Create: `tests/bench/viewer_1m_page_ready.js`
- Modify: `tests/bench/VIEWER_1M_RESULTS.md`
- Modify: `vignettes/million_cell_viewer_benchmark.Rmd`

- [ ] Generate deterministic trajectory and HLA/TCR payloads using existing real fixture schemas and the official one-million-cell index.
- [ ] Add page-specific readiness markers and fresh-process alternating measurement.
- [ ] Encode gates: ordinary first visit under 2 seconds, HLA/Trajectory under 3 seconds, repeat visit under 500 milliseconds.
- [ ] Keep generated large files outside Git and refuse publication when correctness checks or browser logs fail.
- [ ] Record no new performance numbers until the benchmark has actually run.
- [ ] Commit as `bench: add million-cell page gates`.

### Deferred verification

The required later verification pass is `air format` on changed R files, focused testthat contexts, the full test suite, package check, fixture generation, five-round alternating browser benchmark, and comparison of published run manifests. None of these commands are executed in this implementation pass.
