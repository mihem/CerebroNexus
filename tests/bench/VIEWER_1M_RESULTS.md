# 1M-cell backend hot-path benchmark

The benchmark uses the official 10x 1M neurons dataset and the existing `large-examples/1m` artifacts. It separates the CRB persistence gain from the Viewer backend gain so each release is credited only for the layer it changes.

## Compared revisions

| Stage | Revision | Version | Optimization layer |
| --- | --- | --- | --- |
| PR #165 | `69893a2b` | 4.4.3 | Legacy RDS CRB and original Viewer backend paths |
| Thin CRB/qs2 | `27303f21` | 4.5.0 | Thin CRB v1, qs2, hydration, and canonical BPCells cell index |
| Backend hot paths | `1ad8abc0` | 4.5.1 | Integer-index propagation, lower-copy filtering, batched and storage-order reads, backend-native aggregation, and compact Linked Views bundles |

The 4.4.3 and 4.5.0 Viewer hot-path implementations are identical: the intervening changes are confined to CRB serialization and hydration. Their Viewer measurements therefore share one run against the same hydrated Thin qs2 artifact; duplicating that baseline avoids claiming noise as a product difference. CRB lifecycle results are measured separately because that is where 4.5.0 changes behavior.

## Dataset preparation

| Property | Result |
| --- | ---: |
| Cells | 1,000,000 |
| Genes | 27,998 |
| Clusters | 33 |
| UMAP rows | 1,000,000 |
| End-to-end preparation time | 1,342.12 s (22 min 22 s) |
| Maximum resident memory | 8.47 GiB |
| Official source H5 | 4,216,018,749 bytes |
| Reusable Seurat output | 3,778,378,648 bytes |
| CRB + BPCells output | 3,765,141,671 bytes |
| Legacy CRB control payload | 28,132,278 bytes |
| Thin qs2 CRB control payload | 13.21 MiB |

All source and generated data are stored in the R user cache, outside Git.

## CRB lifecycle comparison

Warm-cache medians use five alternating rounds on the same BPCells sidecar. The 4.5.0 and 4.5.1 values come from separate runs of the same script; their small differences show run-to-run and serialized-method variation rather than a new persistence algorithm.

| Resource | PR #165 / 4.4.3 | Thin CRB / 4.5.0 | Backend / 4.5.1 | Latest vs PR #165 | Latest vs Thin CRB |
| --- | ---: | ---: | ---: | ---: | ---: |
| CRB control payload | 26.829 MiB | 13.213 MiB | 13.215 MiB | 50.7% smaller | +0.02% |
| Write median | 8,001 ms | 231 ms | 238 ms | 33.6x faster | 3.0% slower |
| Decode median | 1,296 ms | 41 ms | 40 ms | 32.4x faster | 2.4% faster |
| Hydrated `readCerebro()` median | 1,479 ms | 297 ms | 298 ms | 5.0x faster | 0.3% slower |

## Viewer backend hot paths

Measurements were collected on 2026-09-12 with three warm repetitions on the same 1M-cell Thin qs2 CRB. Times are wall-clock medians; allocations are cumulative R allocations from `Rprofmem`, not peak RSS. Every row checks equality of the selected indices or expression values before timing.

| Operation | Scale | PR #165 | Thin CRB | Backend 4.5.1 | Latest vs PR #165 | Latest vs Thin CRB | PR #165 alloc. | Thin CRB alloc. | Backend alloc. | Allocation change vs both |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Full projection selection | 1M cells, all groups, 100% | 197 ms | 197 ms | 31 ms | -84.3% | -84.3% | 199.1 MiB | 199.1 MiB | 19.1 MiB | -90.4% |
| Filtered projection selection | 1M cells, 17/33 clusters, 25% | 329 ms | 329 ms | 28 ms | -91.5% | -91.5% | 166.5 MiB | 166.5 MiB | 39.5 MiB | -76.3% |
| Cell-index resolution | 1M shuffled indices vs barcode match | 79 ms | 79 ms | <1 ms | >98.7% faster | >98.7% faster | 27.1 MiB | 27.1 MiB | 0 MiB | -100.0% |
| Single-gene expression | 1 gene x 1M cells, canonical order | 4,063 ms | 4,063 ms | 4,022 ms | -1.0% | -1.0% | 154.1 MiB | 154.1 MiB | 157.1 MiB | +2.0% |
| Shuffled single-gene expression | 1 gene x 100k cells, random Viewer order | 7,886 ms | 7,886 ms | 5,881 ms | -25.4% | -25.4% | 33.4 MiB | 33.4 MiB | 22.3 MiB | -33.1% |
| RGB expression | 3 genes x 1M cells | 12,498 ms | 12,498 ms | 4,021 ms | -67.8% | -67.8% | 461.4 MiB | 461.4 MiB | 226.1 MiB | -51.0% |
| Multi-panel expression | 9 genes x 1M cells | 4,309 ms | 4,309 ms | 4,189 ms | -2.8% | -2.8% | 562.3 MiB | 562.3 MiB | 451.0 MiB | -19.8% |
| Mean expression | 100 genes x 1M cells | 5,912 ms | 5,912 ms | 3,931 ms | -33.5% | -33.5% | 1,916.8 MiB | 1,916.8 MiB | 65.4 MiB | -96.6% |

The strongest user-visible improvement is RGB colouring: 12.50 seconds becomes 4.02 seconds, saving 8.48 seconds per update. A representative pass through the six full-scale operations, excluding the isolated index and 100k stress rows, falls from 27.308 to 16.222 seconds, a 40.6% reduction. Cumulative R allocation falls from 3,460.2 to 958.2 MiB, a 72.3% reduction. This sum is an explanatory workload, not an end-to-end Viewer latency claim.

The ordinary single-gene full scan remains bounded by reading one million BPCells values and is effectively unchanged. Random Viewer order is more expensive on an on-disk column backend; reading the same 100,000 requested columns in storage order and restoring the requested output order reduces that case by 25.4% without changing values or order.

## Linked Views bundle construction

The bundle benchmark uses three alternating warm rounds. Both candidates read the same hydrated Thin qs2 CRB. The optimized bundle reuses the session's saved-view fingerprint and keeps projection coordinates only in the canonical `projections` field; the existing client rebuilds the lightweight expression-space descriptor from that field.

| Resource | PR #165 and Thin CRB | Backend 4.5.1 | Change |
| --- | ---: | ---: | ---: |
| Bundle construction | 3,473 ms | 253 ms | 92.7% faster; 13.7x |
| JSON encoding | 3,625 ms | 1,961 ms | 45.9% faster; 1.85x |
| R bundle object | 135.88 MiB | 120.62 MiB | 11.2% smaller |
| JSON payload | 62.94 MiB | 48.78 MiB | 22.5% smaller |

The normalized before/after bundle contract is equal. The remaining 48.78 MiB is the intentional all-cell Linked Views workspace; sampling it or changing its transport format would alter Viewer behavior and belongs to the separate Viewer/WebGPU layer.

## PR3 installed Viewer startup

The PR3 startup benchmark uses the installed package, a fresh R process and browser session for every observation, five alternating rounds, the unchanged Data Info selector `load_data_number_of_cells == "1,000,000"`, and the same BPCells sidecar. Schema v1 eagerly restores one million cell names; schema v2 reads its stored cell count for Data Info and defers cell-name hydration until metadata or projections are used. Optional page servers start 100 ms after the first flush, while collapsed group filters resume after 500 ms; browser regression tests verify that the deferred pages and filters still initialize.

| Startup phase | Schema v1 eager | Schema v2 lazy | Change |
| --- | ---: | ---: | ---: |
| Package attach | 1.051 s | 1.048 s | 0.3% faster |
| App construction | 0.199 s | 0.201 s | 0.7% slower |
| Process start to server listening | 1.434 s | 1.433 s | 0.1% faster |
| Browser document load | 0.315 s | 0.296 s | 6.2% faster |
| Load event to Data Info | 1.770 s | 0.733 s | 58.6% faster |
| Browser navigation to Data Info | 2.105 s | 1.029 s | 51.1% faster |
| Process start to Data Info | 3.692 s | 2.557 s | 30.7% faster |

The current 2.557-second median passes the strict `<3,000 ms` gate with 443 ms of median headroom. Relative to the prior installed-runtime optimization point of 4.097 seconds, PR3 is 37.6% faster; relative to the original 9.045-second Legacy RDS path, it is 71.7% faster. Raw observations are in `tests/bench/results/million_cell_startup_sub3_4_6_0.csv`.

## PR4 quick comparison

These are exploratory single warm-cache observations, not publication medians. PR3 and PR4 used the same current-schema gene-major CRB, 1,000,000 trajectory rows, 100,000 synthetic receptor rows, and 32 synthetic HLA-typed samples. The page table isolates runtime changes because both candidates use the same gene-major expression artifact.

| Backend operation | BPCells column-major | Gene-major | Change |
| --- | ---: | ---: | ---: |
| Single gene × 1M cells | 3,982 ms | 194 ms | 95.1% faster |
| RGB, 3 genes × 1M cells | 3,984 ms | 202 ms | 94.9% faster |
| 9 genes × 1M cells | 3,970 ms | 231 ms | 94.2% faster |
| Mean, 100 genes × 1M cells | 3,748 ms | 55 ms | 98.5% faster |

All expression results passed equality checks.

| Page | PR3 first visit | PR4 first visit | Change | PR4 target |
| --- | ---: | ---: | ---: | ---: |
| Groups | 1,273 ms | 1,272 ms | 0.1% faster | pass `<2 s` |
| Overview | 10,198 ms | 10,375 ms | 1.7% slower | fail `<2 s` |
| Gene Expression | 3,620 ms | 2,135 ms | 41.0% faster | fail `<2 s` |
| Immune Repertoire | 7,504 ms | 6,973 ms | 7.1% faster | fail `<2 s` |
| Trajectory | 3,660 ms | 3,050 ms | 16.7% faster | fail `<3 s` by 50 ms |
| HLA & TCR Motifs | 5,913 ms | 5,714 ms | 3.4% faster | fail `<3 s` |
| Coordinated Views | 4,741 ms | 3,272 ms | 31.0% faster | fail `<2 s` |

| Page | PR3 repeat | PR4 repeat | PR4 target |
| --- | ---: | ---: | ---: |
| Groups | 117 ms | 200 ms | pass `<500 ms` |
| Overview | 256 ms | 231 ms | pass `<500 ms` |
| Gene Expression | 796 ms | 770 ms | fail `<500 ms` |
| Immune Repertoire | 811 ms | 815 ms | fail `<500 ms` |
| Trajectory | 1,460 ms | 1,436 ms | fail `<500 ms` |
| HLA & TCR Motifs | 248 ms | 128 ms | pass `<500 ms` |
| Coordinated Views | 130 ms | 113 ms | pass `<500 ms` |

The focused HLA/Trajectory pass then removed the HLA metadata/parsing bottlenecks, built motif edges without a dense adjacency matrix, rendered the full motif graph through the shared Canvas path, and deferred non-primary Trajectory panels until its million-point projection had flushed.

| Page | PR3 first visit | Focused PR4 first visit | Change | Target |
| --- | ---: | ---: | ---: | ---: |
| Trajectory | 3,660 ms | 2,434 ms | 33.5% faster | pass `<3 s` |
| HLA & TCR Motifs | 5,913 ms | 2,696 ms | 54.4% faster | pass `<3 s` |

| Page | Focused PR4 repeat | Repeat target |
| --- | ---: | ---: |
| Trajectory | 1,266 ms | fail `<500 ms` |
| HLA & TCR Motifs | 126 ms | pass `<500 ms` |

The HLA focused result waits for the shared Canvas renderer to finish drawing every node and edge instead of waiting for unrelated page-level Shiny work to become idle. It is the user-visible completed-first-frame metric; the old PR3 HLA number used the broader idle gate, so its percentage is directional rather than a publication-grade same-harness estimate. Raw exploratory observations are in `tests/bench/results/million_cell_bpcells_quick_pr4.tsv`, `tests/bench/results/million_cell_pages_quick_pr3_pr4.tsv`, and `tests/bench/results/million_cell_hla_trajectory_focused_pr4.tsv`.

## Environment

- Apple M1 Pro, 32 GiB RAM
- macOS 27.0 (26A5421a)
- R 4.6.1, aarch64
- BPCells 0.3.1, Seurat 5.5.1, Shiny 1.14.0

## Reproduce

The complete workflow reuses complete artifacts when present, otherwise prepares the missing official H5, BPCells-backed Seurat, and Thin CRB artifacts before measuring the backend hot paths. Its manifest records PR #165, Thin CRB, and latest implementation SHAs:

```sh
tests/bench/run_viewer_1m_benchmark.sh 27303f21 1ad8abc0 tests/bench/scratch/backend-4.5.1
```

The lower-level command accepts an already prepared CRB so measurements can be rerun without repeating conversion:

```sh
Rscript tests/bench/viewer_1m_hot_paths.R BEFORE_ROOT AFTER_ROOT CRB 3
```
