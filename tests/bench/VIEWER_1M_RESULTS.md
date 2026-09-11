# 1M-cell backend hot-path benchmark

Measured on 2026-09-11 with the official 10x 1M neurons dataset.

## Compared revisions

| Version | Revision | Notes |
| --- | --- | --- |
| Baseline | `69893a2b` | PR #165 merge / current upstream base |
| Candidate | Current PR #167 worktree | Backend-only candidate |

The candidate was measured directly from the cleaned worktree so the retained
numbers exclude the Viewer-interaction changes moved to the next PR.

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
| CRB metadata file alone | 28,132,278 bytes |

All source and generated data are stored in the R user cache, outside Git.

## Viewer hot paths

Medians use three repetitions on the same 1M-cell CRB. Allocations are R allocations reported by `Rprofmem`; correctness checks passed for every row.

| Operation | Scale | Before | After | Time | Before alloc. | After alloc. | Allocation |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Full projection selection | 1M cells, all groups, 100% | 226 ms | 31 ms | -86.3% | 206.7 MiB | 19.1 MiB | -90.8% |
| Filtered projection selection | 1M cells, 17/33 clusters, 25% (134,557 cells) | 345 ms | 28 ms | -91.9% | 170.6 MiB | 39.5 MiB | -76.9% |
| Single-gene expression | 1 gene x 1M cells, BPCells | 4,167 ms | 4,182 ms | +0.4% | 154.1 MiB | 169.3 MiB | +9.9% |
| RGB expression | 3 genes x 1M cells, 3 reads vs 1 | 12,310 ms | 4,106 ms | -66.6% | 461.4 MiB | 238.3 MiB | -48.4% |
| Multi-panel expression | 9 genes x 1M cells, transpose removed | 5,327 ms | 4,702 ms | -11.7% | 562.3 MiB | 463.2 MiB | -17.6% |
| Mean expression | 100 genes x 1M cells, dense vs backend-native | 5,861 ms | 4,093 ms | -30.2% | 1,916.8 MiB | 112.8 MiB | -94.1% |

The single-gene path is effectively time-neutral in this run and allocates
15.3 MiB more R memory. The main expression gains come from batching RGB
reads and keeping aggregate computation backend-native.

## Environment

- Apple M1 Pro, 32 GiB RAM
- macOS 27.0 (26A5421a)
- R 4.6.1, aarch64
- BPCells 0.3.1, Seurat 5.5.1, Shiny 1.14.0

## Reproduce

The complete workflow downloads the official H5, creates the BPCells-backed
Seurat and CRB artifacts, and measures the backend hot paths:

```sh
tests/bench/run_viewer_1m_benchmark.sh
```

The lower-level command accepts an already prepared CRB so measurements can be
rerun without repeating conversion:

```sh
Rscript tests/bench/viewer_1m_hot_paths.R BEFORE_ROOT AFTER_ROOT CRB 3
```
