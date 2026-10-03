# Million-cell component results

These results explain the PR0-PR5 performance work on one exact 1,000,000-cell
10x mouse-brain fixture. They are component comparisons, not evidence for the
current real-data paper benchmark.

## Contents

- [Evidence status](#evidence-status)
- [Key results](#key-results)
- [Page readiness](#page-readiness)
- [Interpretation](#interpretation)
- [Reproduce](#reproduce)

## Evidence status

- CRB, backend, startup, and final page observations have retained raw files in
  [`results/component/`](../results/component/).
- The old PR4 quick run was exploratory and is intentionally omitted here.
- Rounded full-source observations from a discarded scratch directory were
  removed because the original raw files no longer exist.

## Key results

| Layer | Baseline | Optimized | Main result |
|---|---:|---:|---|
| Thin qs2 CRB write | 8,001 ms | 238 ms | 33.6x faster |
| Thin qs2 CRB decode | 1,296 ms | 40 ms | 32.4x faster |
| Hydrated `readCerebro()` | 1,479 ms | 298 ms | 5.0x faster |
| Viewer RGB expression | 12,498 ms | 4,021 ms | 67.8% faster |
| Linked Views bundle build | 3,473 ms | 253 ms | 92.7% faster |
| Process start to Data Info | 3,692 ms | 2,557 ms | 30.7% faster |

The CRB comparison isolates persistence changes. Backend comparisons reuse the
same hydrated thin-CRB fixture, so codec and Viewer gains are not conflated.

## Page readiness

The final balanced run compares PR4 `68945d0d` with PR5 `940001b0`: five rounds,
140 independent R/Chrome observations, and 140/140 correctness passes. PR5
passes 11 of 14 first/repeat page budgets; every repeat visit is below 500 ms.

| Page | PR4 first | PR5 first | PR4 repeat | PR5 repeat |
|---|---:|---:|---:|---:|
| Groups | 1,166 ms | 1,199 ms | 112 ms | 112 ms |
| Overview | 1,481 ms | 1,349 ms | 68 ms | 11 ms |
| Gene Expression | 3,061 ms | 3,025 ms | 614 ms | 9 ms |
| Immune Repertoire | 13,516 ms | 3,810 ms | 596 ms | 14 ms |
| Trajectory | 2,088 ms | 2,087 ms | 597 ms | 21 ms |
| HLA & TCR Motifs | 2,378 ms | 2,378 ms | 18 ms | 20 ms |
| Coordinated Views | 9,275 ms | 8,556 ms | 8 ms | 8 ms |

The remaining PR5 first-visit failures are Gene Expression, Immune Repertoire,
and Coordinated Views. Therefore this run is useful diagnostic evidence but does
not pass the full acceptance gate.

## Interpretation

- The strongest gains are CRB lifecycle, repeat-page readiness, Immune
  Repertoire, and Linked Views bundle construction.
- Ordinary full-cell single-gene reading remains I/O-bound.
- Do not generalize warm-cache component timings across machines or treat
  their sum as end-to-end Viewer latency.

## Reproduce

The supported entry points and their callers are listed in
[`harnesses/README.md`](../harnesses/README.md). The main backend comparison is:

```sh
tests/bench/harnesses/backend/compare.sh \
  27303f21 1ad8abc0 \
  tests/bench/results/component/backend/<run-id>
```
