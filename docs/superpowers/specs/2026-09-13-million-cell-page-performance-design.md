# Million-cell page performance design

## Goal

Make every Viewer page usable with a one-million-cell dataset while preserving the existing visual language and exact full-dataset statistics. The 100% cell view is the benchmark standard. Ordinary pages must become ready in under 2 seconds, HLA and Trajectory in under 3 seconds, and a repeated visit in the same session in under 500 milliseconds.

## Branch structure

The previous `perf/pr4-gene-major-bpcells-v2` is preserved as `backup/perf-pr4-gene-major-bpcells-v2-20260913`. The historical `perf/refactor-viewer-consolidation` and `perf/viewer-interactions` branches are preserved under matching dated backup names. The new `perf/pr4-million-cell-page-performance` starts directly from the CI-passing `perf/pr3-startup-optimization` branch.

The historical branches are source material only. No historical branch or large commit is merged wholesale.

## Architecture

The implementation uses a hybrid renderer. Scatter-style views reuse the existing Canvas/WebGPU cell renderer. Aggregate plots keep their current visual form but receive compact server-side summaries instead of one million raw observations. Counts, statistical summaries, selections and downloads continue to use the complete dataset.

The runtime activates expensive page work only after the owning sidebar page has been visited. Outputs explicitly marked `suspendWhenHidden = FALSE` retain that behavior after activation, but they no longer precompute for unvisited pages. Parsed immutable Viewer source files may be reused across sessions while each expression is still evaluated in the current session scope.

## Reused historical work

The useful concepts from the historical Viewer interaction branch are the parsed-source cache, owner-page gating for hidden outputs, immediate first render with debounce only on later interactions, compact columnar hover data, session reuse of Spatial images and hull geometry, specialist-page isolation from the full Linked Views bundle, and browser benchmark synchronization.

The consolidation branch contributes only the module-registry and shared-loader approach when it reduces measured page activation work. Its broad HLA refactor, dormant-module deletion, dependency cleanup and unrelated component consolidation remain excluded. Current code already contains the relevant centralized HLA core boundary.

## Page-specific behavior

- Gene Expression, Spatial gene colouring, Trekker and Linked Views read through the shared expression accessors and therefore use the gene-major BPCells sidecar.
- Overview, Gene Expression, Spatial and Trajectory scatter views share the million-cell renderer and compact hover payload.
- Groups composition retains exact full-data counts. Violin-style metrics use server-side density and quantile summaries rather than transferring one million raw values.
- Trajectory keeps exact pseudotime/state data for statistics and selection, while its scatter layer uses the shared million-cell renderer.
- HLA/TCR aggregates cell-level repertoire data before transferring network nodes, edges and summaries. Cell-level tables and downloads remain exact.

## Benchmark fixture

The generator starts from the official 10x E18 one-million-cell mouse-brain artifact. Because that artifact has no trajectory, immune-repertoire or HLA content, the generator adds deterministic performance payloads with the same schemas as the existing real examples. It expands existing valid trajectory and HLA/TCR structures across the one-million-cell index with a fixed seed. Generated large artifacts remain in the R user cache and are never committed.

## Measurement contract

Each page has an explicit browser-ready marker tied to its main visible result. PR3 and PR4 run in alternating fresh processes and browser sessions for at least five rounds. Results record process startup, first page activation, main result readiness and repeat activation. Correctness checks cover cell identity, selected cells, full-data counts, grouping summaries and browser/server errors.

## Compatibility and fallback

Canvas remains the fallback when WebGPU is unavailable. No public Cerebro API changes. Existing CRB schema v1 remains readable; current schema v2 retains the PR3 `n_cells` contract. The gene-major property is read from the BPCells sidecar rather than overloading the CRB schema version.

## Deferred verification

Local tests, browser runs and benchmark execution are intentionally deferred at the user's request. The branch must be treated as unverified until CI or a later local validation pass executes the documented checks.
