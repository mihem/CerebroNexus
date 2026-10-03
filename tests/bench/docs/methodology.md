# Real-data expression-backend benchmark methodology

## Contents

- [Research question](#research-question)
- [Sources and scope](#sources-and-scope)
- [Experimental units](#experimental-units)
- [Production artifact path](#production-artifact-path)
- [Frozen access workload](#frozen-access-workload)
- [Provenance and evidence gate](#provenance-and-evidence-gate)
- [Interpretation limits](#interpretation-limits)

## Research question

Across fixed scales and complete million-scale public scRNA-seq matrices, how do CerebroNexus's `embedded`, `bpcells`, and `h5` backends compare in backend construction, stored size, memory, hydrated startup, and expression access after the PR0-PR6 performance work?

This is descriptive component evidence. It does not compare biological methods, test statistical significance, control the operating-system page cache, or claim cross-machine generality.

Viewer behaviour is deliberately outside this research question. The focused
`tests/bench/harnesses/viewer/` checks provide component evidence; their
outcome cannot invalidate or publish expression-backend measurements.

## Sources and scope

The study uses two pinned public files: 1,306,127 cells from the 10x mouse brain E18 dataset (`GSE93421`, `SRP096558`) and 1,486,324 cells from the CELLxGENE PsychAD HBCC human prefrontal-cortex dataset (dataset `d27fb144-f105-46c2-b36f-f51421f74e4e`, collection `84ce6837-548d-4a1f-919f-0bc0d9a3952f`, DOI `10.1038/s41597-025-04687-5`). Downloads are reused only after byte-size and SHA-256 verification.

The scale study takes deterministic prefixes at its declared cell counts and attempts `embedded` through 500k cells; a failed `embedded` build is retained as an empty observation without invalidating successful out-of-core measurements. The full-source study uses every cell without sampling or cropping and excludes `embedded` because both complete matrices exceed the 32-bit non-zero index limit of `Matrix::dgCMatrix`.

## Experimental units

For each source/backend pair, five fresh R processes independently open the source, stream the backend, construct the Cerebro shell, and serialize the CRB. Backend order alternates across repeats. Each artifact is opened by two fresh access processes, yielding ten access observations per source/backend.

The independent scale study uses fixed tiers of 1k, 10k, 50k, 100k, 500k, and 1m cells from each source. The complete-source study uses all 1,306,127 mouse cells or all 1,486,324 human cells. Scale and full results have separate run IDs, result roots, validation, and current-result pointers and must not be pooled as one experiment.

Processes run sequentially with a fixed thread count on an exclusive node. Reports show medians, observed minima/maxima, and independent-process `n`; no significance test is performed.

## Production artifact path

BPCells output uses CerebroNexus's gene-major writer. H5 output is streamed as a cells-by-genes TENx matrix under the production `expression` group. The shell is saved with `saveCerebro()` and its default qs2 codec. Fresh-process access uses `readCerebro()`, so qs2 decoding, schema-v2 validation, thin-CRB hydration, and relative sidecar attachment are included rather than bypassed with `readRDS()`.

Build metrics separate lazy source opening, backend writing, shell construction, CRB serialization, stored size, R heap, and whole-process peak RSS.

## Frozen access workload

A separate untimed process selects 12 expressed genes across the source's observed density range and records reference fingerprints. Every timed process uses the same immutable plan.

The access workload measures hydrated startup, the first full-cell single-gene read, warmed full-cell single-gene latency, one 12-gene-by-all-cells block, one single-gene read over up to 100,000 deterministic reverse-ordered non-contiguous cell indices, and the corresponding 12-gene subset block. The subset specifically exercises sorting and order restoration for BPCells and DelayedArray without replacing the complete source with a sampled dataset.

The first getter call is fresh-process but not controlled cold disk; the operating-system page cache may be warm. Every successful result is fingerprinted outside the timed expression. Any full or subset mismatch invalidates the run. Failed `embedded` builds in the scale study retain missing metrics and do not receive access measurements; BPCells and H5 remain mandatory at every tier.

## Provenance and evidence gate

The wrapper requires a clean worktree, an explicit storage description, a fixed thread count, and a source cache outside Git. Each run records Git SHA, package and dependency versions, R, OS, CPU, storage, source URLs, file sizes, and hashes. Missing rows, failed mandatory backends, mismatched plans, incorrect values, missing figures, or dirty Git state reject the run before the immutable result is published. Scale-study `embedded` failures are the sole optional build outcome.

Validated evidence is published under either `results/benchmark/scale/runs/<run-id>/` or `results/benchmark/full/runs/<run-id>/`; each workflow updates only its own `CURRENT`, and only after all checks pass. The package also contains a deterministic file inventory with byte sizes and checksums, so the raw tables and generated report can be audited as one unit.

## Interpretation limits

- Synthetic metadata and sinusoidal projection coordinates make the serialized shell structurally complete; they are not biological results.
- The 100,000-cell access subset is a query workload, not a sampled study cohort.
- Historical one-million-cell Viewer fixtures remain reproducibility artifacts for earlier PR comparisons. They are distinct from the pinned-source 1m backend tiers and do not contribute to this benchmark.
