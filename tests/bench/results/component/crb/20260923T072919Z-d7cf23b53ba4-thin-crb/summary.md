# Thin CRB qs2 comparison

## Contents

- [Scope](#scope)
- [Headline results](#headline-results)
- [Findings](#findings)
- [Decision rule](#decision-rule)
- [Files](#files)

## Scope

One reconstructed legacy-compatible one-million-cell BPCells-backed payload; 5 alternating rounds; 32 qs2 parameter combinations. All candidates reuse the same sidecar.

## Headline results

| Role | Candidate | MiB | Write ms | Decode ms | Hydrated ms |
|---|---|---:|---:|---:|---:|
| Legacy baseline | `legacy_rds` | 44.96 | 9993 | 2152 | 2250 |
| Thin payload, RDS | `thin_rds` | 32.49 | 5406 | 576 | 2360 |
| Production default | `thin_qs2_l03_s1_t1` | 29.86 | 326 | 357 | 1873 |
| Recommended host setting | `thin_qs2_l03_s1_t4` | 29.86 | 104 | 268 | 1754 |
| Fastest write | `thin_qs2_l01_s0_t4` | 34.09 | 68 | 354 | 2097 |
| Fastest hydration | `thin_qs2_l15_s1_t1` | 28.44 | 9468 | 287 | 1618 |
| Smallest file | `thin_qs2_l22_s1_t1` | 26.29 | 42845 | 343 | 1748 |

## Findings

- Payload thinning alone reduces the control file by 27.73% and makes physical decoding 3.74x faster than the reconstructed legacy payload.
- Default qs2 adds a further 8.09% size reduction versus thin RDS and makes writing 16.58x faster.
- With the same level-3 shuffled file format, 4 threads write 3.13x faster and hydrate 1.07x faster than the one-thread production default on this host.

## Decision rule

Keep compression level 3 with shuffle enabled as the portable format default. Prefer the measured multi-thread setting when TBB is available; thread count changes runtime only, not the serialized format. Extreme compression levels save little additional space for disproportionate write cost.

## Files

- `raw.csv`: every timed observation and execution position.
- `summary.csv`: medians, baseline deltas, correctness, and Pareto status.
- `environment.csv`: hardware, software, source, and comparison parameters.
