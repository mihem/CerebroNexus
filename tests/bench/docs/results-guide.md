# Reading benchmark results

This guide defines how to interpret one completed full-source or scale run. It
contains no fixed performance conclusion.

## Contents

- [Select one immutable run](#select-one-immutable-run)
- [Interpret metrics](#interpret-metrics)
- [Correctness requirements](#correctness-requirements)
- [Unsupported claims](#unsupported-claims)

## Select one immutable run

Choose one profile, read `results/benchmark/<profile>/CURRENT`, then open the
matching directory under `results/benchmark/<profile>/runs/<run-id>/`, where
`<profile>` is `full` or `scale`. Do not combine files from different runs or
profiles.

The committed full run was produced before `panel_c2` was renamed to `full`,
so its internal manifest retains `panel_c2`. The validator treats that value as
the `full` compatibility alias; new runs record `full`.

Start with:

1. `summary.md` for the descriptive tables;
2. `run_manifest.csv` and `source_manifest.csv` for provenance;
3. `05_schedule.csv` and `resource_check.csv` for the exact complete-source grid;
4. `10_export.csv` for build observations;
5. `20_access.csv` for hydrated startup and expression access;
6. `query_plan_manifest.csv` and `query_panel.csv` for the frozen workload and correctness references;
7. `figures/expression_backend_benchmark_overview.png` for the benchmark overview;
8. scale only: `figures/expression_backend_benchmark_ceiling.png`;
9. `evidence_manifest.csv` to verify that the committed evidence package is complete and unchanged.

## Interpret metrics

A value such as `2.4 [2.2-2.9], n=6` is a median of 2.4, an observed process range of 2.2 to 2.9, and six independent processes. It is not a confidence interval.

| field | meaning |
|---|---|
| `read_secs` | lazy complete-source opening |
| `export_secs` | streaming backend construction |
| `shell_secs` | synthetic complete-cell Cerebro shell construction |
| `serialize_secs` | default qs2 CRB serialization |
| `total_mb` | CRB plus relative backend sibling |
| build `r_peak_mb`, `peak_rss_mb` | R heap and whole build-process high-water RSS |
| `startup_secs` | fresh-process public `readCerebro()` hydration |
| access `rss_mb`, `peak_rss_mb` | hydrated-process resident and peak memory |
| `first_query_secs` | first full-cell getter call; not controlled cold disk |
| `hot_p50_secs`, `hot_p95_secs` | warmed full-cell single-gene distribution |
| `block_secs` | deterministic 12-gene-by-all-cells read |
| `subset_row_secs`, `subset_block_secs` | reverse-ordered non-contiguous access over up to 100,000 cells |

## Correctness requirements

Every successful access row must have `status = OK`, `correctness = OK`, and matching full-row, full-block, subset-row, subset-block, and query-plan fingerprints.

`embedded` has no full-source observation because neither source is
representable as `dgCMatrix`. In the scale profile it is attempted through 500k
cells; a recorded embedded failure does not invalidate mandatory BPCells/H5
rows.

## Unsupported claims

- true cold-disk latency;
- universal or cross-machine performance;
- statistical significance from these descriptive repeats;
- biological-method quality;
- full-source `embedded` support.

The full profile supports the narrower claim that two complete public
million-scale matrices were processed through CerebroNexus's production
out-of-core paths on one fully recorded host. The scale profile supports
within-host comparisons across its declared fixed tiers.
