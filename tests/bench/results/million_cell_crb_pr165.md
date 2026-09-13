# Million-cell CRB comparison against PR #165

Baseline: PR #165 merge `69893a2b`, whose CRB representation is the legacy RDS payload used by the cached 1M dataset. Measurements are warm-cache medians from five alternating rounds on the same BPCells sidecar.

| Candidate | CRB size | Write median | Decode median | Hydrated median | Size reduction | Write speedup | Decode speedup | Hydrated speedup |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| PR #165 | 26.83 MiB | 8001 ms | 1296 ms | 1479 ms | — | — | — | — |
| Thin CRB, RDS | 13.92 MiB | 2947 ms | 93 ms | 356 ms | 48.12% | 2.72x | 13.94x | 4.15x |
| Thin CRB, qs2 | 13.21 MiB | 231 ms | 41 ms | 297 ms | 50.75% | 34.64x | 31.61x | 4.98x |

`Write` includes payload compaction and serialization. `Decode` measures only physical deserialization. `Hydrated` uses `readCerebro()`, reopens the unchanged BPCells sidecar, validates its canonical cell index, and restores omitted metadata/projection row names.
