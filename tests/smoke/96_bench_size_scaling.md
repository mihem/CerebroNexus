# Backend size-scaling experiment

Companion document to [`src/96_bench_size_scaling.R`](src/96_bench_size_scaling.R) and a deep-dive on **why the bpcells exporter changed in 1.7.0** to auto-call `BPCells::convert_matrix_type("uint32_t")` before `write_matrix_dir()`.

The PBMC All Samples bench in [`vignettes/expression_backend_benchmark.Rmd`](../../vignettes/expression_backend_benchmark.Rmd) only measures one fixture size. This document records the controlled size-scaling experiment that motivated the bpcells fix.

## Three hypotheses

1. **"h5 disk wins only at scale"** — the gzip-compressed TENx CSC sibling is *larger* than the equivalent embedded `.crb` on small/dense fixtures (HDF5 chunk + dimnames overhead dominates), but smaller on large sparse fixtures. Roman Hillje's [`create_expression_matrix_in_h5_format.Rmd`](../../vignettes/create_expression_matrix_in_h5_format.Rmd) reports this on a 1000-cell example; our previous PBMC All Samples (147,756 cells) bench reported the opposite. **Where exactly does the trade-off flip?**

2. **"BPCells stays huge unless integer bit-packing is triggered"** — `BPCells::write_matrix_dir()` only bit-packs when the matrix's storage type is integer. `dgCMatrix@x` is always `double`, even when every value is an integer count, so the bpcells exporter wrote raw doubles by default. The fix is one line — `BPCells::convert_matrix_type("uint32_t")` before write — but only safe for losslessly-integer values. **How much does that line actually save?**

3. **"bpcells query speed depends on payload size, not just decompression cost"** — if hypothesis 2 is true, bit-packed integer storage means less disk to read per query and the chunk-level decompression is cheap compared to IO. **Does bpcells get faster after bit-packing, or just smaller?**

## Method

Run [`src/96_bench_size_scaling.R`](src/96_bench_size_scaling.R). Synthetic Seurat objects with:

- `Matrix::rsparsematrix(ngenes, ncells, density, rand.x = function(n) rpois(n, 1.5) + 1L)` (Poisson + 1, mimics scRNA-seq small-count distribution)
- Dummy UMAP (random 2D coords, skips real `RunPCA`/`RunUMAP`)
- Two random grouping vars (`sample`, `cluster`)

Three sizes:

| | cells | genes | density | nnz |
|---|---:|---:|---:|---:|
| small  | 500     | 2,000  | 10% | 100K |
| medium | 50,000  | 20,000 | 5%  | 50M |
| large (optional) | 500,000 | 20,000 | 5% | 500M |

Four backends:

- `embedded` — default exporter
- `bpcells` — default exporter (since 1.7.0 auto-detects integer values)
- `bpcells_int` — **probe**: explicit `BPCells::convert_matrix_type("uint32_t")` before write. This was the *manual* path for confirming hypothesis 2 before the 1.7.0 exporter change made it the default. Now equivalent to `bpcells` whenever values are integer; kept in the script to make the comparison auditable when running against pre-1.7.0 code (`git checkout`).
- `h5` — default exporter (lazy `TENxMatrix` since 1.7.0)

For each (size × backend) the script records: `.crb` size, sibling size, total disk, export wall-clock. Output: `result/96_bench_size_scaling/summary.csv`.

Run:

```bash
cd tests/smoke
Rscript src/96_bench_size_scaling.R                     # small + medium only (~2-3 min)
BENCH_INCLUDE_LARGE=1 Rscript src/96_bench_size_scaling.R  # add large (~20-40 min, ~8 GB peak RAM)
```

## Results — small + medium (default run)

Disk in MB, export time in seconds:

| size | backend | crb | sibling | **total** | export |
|---|---|---:|---:|---:|---:|
| **small** (500 × 2,000, 10% nnz) | embedded    | 0.35 |  —    | **0.35** | 0.10 |
|                                  | bpcells     | 0.12 | 0.91  | **1.03** | 0.22 |
|                                  | bpcells_int | 0.12 | 0.19  | **0.31** | 0.05 |
|                                  | h5          | 0.11 | 0.20  | **0.31** | 0.13 |
| **medium** (50,000 × 20,000, 5% nnz) | embedded    | 138  |  —    | **138** | 32.4 |
|                                       | bpcells     | 1.44 | 440   | **441** | 1.06 |
|                                       | bpcells_int | 1.44 | 78    | **80**  | 1.97 |
|                                       | h5          | 1.27 | 82    | **84**  | 20.4 |

(`bpcells` row uses 1.6.0-style raw double; `bpcells_int` row simulates 1.7.0's auto-integer-pack. After the 1.7.0 fix, `bpcells` matches `bpcells_int` on integer counts.)

## What the data says

### Hypothesis 1: h5 disk-vs-embedded — partially confirmed

On `small` (500 cells × 2,000 genes, 10% density): h5 total 0.31 MB vs embedded 0.35 MB — **basically tied**, slight edge to h5. Roman's reverse observation (h5 > embedded) was on 1000 cells × 500 genes — *fewer* genes amplifies the HDF5 dimnames + chunk metadata overhead. With more genes (2,000) and more nnz, the gzip win already breaks even.

On `medium` (50,000 cells × 20,000 genes, 5%): h5 total 84 MB vs embedded 138 MB — **h5 clearly smaller** (~1.6×). Scale effect kicked in.

PBMC All Samples (147,756 × 38,606, ~5%): h5 391 MB vs embedded 681 MB — same direction as medium, ~1.7×.

**Conclusion:** the "h5 wins at scale" intuition is correct but the flip point is fixture-shape-dependent, not just cell-count-dependent. Fewer genes / lower density / lower nnz → less compression payload → metadata overhead wins. Roman's example.h5 hit that regime; anything beyond a few thousand cells × a few thousand genes is past the flip.

### Hypothesis 2: BPCells bit-packing — strongly confirmed

On every size, `bpcells_int` (= 1.7.0 auto-integer-pack) shrinks the sibling **3-6×** vs `bpcells` (= 1.6.0 raw double):

- small: 0.91 → 0.19 MB (~4.8×)
- medium: 440 → 78 MB (~5.6×)
- PBMC All Samples (re-measured): 2,557 → 549 MB (~4.7×)

The first time `96_bench_size_scaling.R` was run, BPCells itself emitted:

> `Warning: Matrix compression performs poorly with non-integers. Consider calling convert_matrix_type if a compressed integer matrix is intended.`

— the library was already telling us. The 1.7.0 exporter change just listens to it.

**Why this is safe by default**: the check is `length(@x) > 0 && all(@x >= 0) && all(@x == as.integer(@x)) && all(@x <= .Machine$integer.max)`. Normalised data (`slot = "data"`) fails the integer check and falls back to raw double — no precision loss, no surprises.

### Hypothesis 3: bpcells query speed — confirmed

Server-side bench (`93_bench_backend_compare.R`) re-run after the 1.7.0 fix, same PBMC All Samples fixture:

| metric | bpcells 1.6.0 (raw double) | bpcells 1.7.0 (bit-packed) | improvement |
|---|---:|---:|---:|
| cold_secs (single gene)     | 1.234 | **0.737** | ~1.7× |
| hot_p50_secs                | 1.099 | **0.706** | ~1.6× |
| bulk_secs (50 × all cells)  | 1.154 | **0.834** | ~1.4× |
| sibling disk                | 2,557 MB | **549 MB** | ~4.7× |
| RSS after attach            | 1,170 MB | 1,215 MB | unchanged |
| attach_secs                 | 0.305 | 0.201 | ~1.5× |

bpcells queries are 1.4-1.7× faster on the same data because the on-disk payload is 5× smaller and bit-unpacking is cheaper than the IO it saves. RSS doesn't change because the matrix is still streamed lazily either way.

## What the bpcells change does NOT solve

bpcells per-gene query is still ~70× slower than h5 (0.74 s vs 0.01 s on the PBMC fixture). The packed format is row-oriented (cells × genes with cells as the chunked dimension); "fetch one gene" means scanning chunks and densifying. h5 TENx is column-oriented (cells × genes with genes as columns) and per-gene = one contiguous CSC column slice, which is the cheapest possible HDF5 access pattern and on top of that benefits from the OS page cache for repeated reads.

**bpcells is now the right choice when** the workload is dominated by chunk-level batched operations (e.g. all-genes-by-all-cells PCA / NMF), where chunked streaming wins; **h5 stays the right default for per-gene Shiny queries** (Cerebro's typical access pattern).

## Reproduce

The PBMC All Samples bpcells re-conversion (only one PBMC dataset is needed for the bench, not all five in `12_convert_h5.R`):

```bash
cd tests/smoke
Rscript -e '
  suppressPackageStartupMessages({
    devtools::load_all("../..", quiet = TRUE)
    library(qs); library(Seurat)
  })
  seurat_obj <- qs::qread("data/21_S04_seurat_integrated_STACAS_standard_pipeline.qs")
  convertSeuratToCerebro(
    seurat_file = seurat_obj,
    result_dir  = "result/11_convert_bpcells",
    assay = "RNA", slot = "counts",
    experiment_name = "PBMC All Samples TCR/BCR",
    organism = "Human PBMC",
    groups = c("celltype_merged.l1", "timepoint", "sample"),
    expression_matrix_mode = "bpcells"
  )'
Rscript src/93_bench_backend_compare.R
Rscript src/94_bench_web_load.R
Rscript src/95_bench_backend_plot.R
```

Size-scaling synthetic bench (no external fixture needed):

```bash
cd tests/smoke
Rscript src/96_bench_size_scaling.R
# add BENCH_INCLUDE_LARGE=1 for the 500k-cell row (slow)
```

## See also

- [`vignettes/expression_backend_benchmark.Rmd`](../../vignettes/expression_backend_benchmark.Rmd) — published bench writeup (PBMC All Samples fixture, the canonical reference)
- [`vignettes/create_expression_matrix_in_h5_format.Rmd`](../../vignettes/create_expression_matrix_in_h5_format.Rmd) — Roman Hillje's original h5 design note (where the "h5 can be larger" observation comes from)
- `R/exportFromSeurat.R` `expression_matrix_mode = "bpcells"` branch — the integer-detection block that triggers BPCells bit-packing
