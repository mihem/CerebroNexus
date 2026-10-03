# Expression-backend benchmark on real public datasets

**Run:** `20260922T162843Z-8eaeadbb6ccb-full`  
**Profile:** `full`  
**Git:** `8eaeadbb6ccb440687178970da048cea178f2d3c`

> **Evidence status.** Benchmark evidence: backend comparisons use independent process repeats and correctness fingerprints.

## Sources

| source | cells | genes | nnz | nnz/cell | full dgCMatrix | representable |
|---|---:|---:|---:|---:|---:|:--:|
| 10x mouse brain E18 | 1,306,127 | 27,998 | 2.625e+09 | 2010 | 29.3 GB | **no** |
| human PFC cross-disorder (HBCC) | 1,486,324 | 34,176 | 6.112e+09 | 4112 | 68.3 GB | **no** |

## Export

Values are median [minimum-maximum], followed by the number of independent export processes.

| source | cells | backend | total MB | backend build s | shell s | CRB serialization s | peak R heap MB | peak process RSS MB |
|---|---:|---|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 27701.6 [27701.6-27701.6], n=5 | 466.3 [436.1-547.4], n=5 | 3.34 [2.96-3.53], n=5 | 5.74 [5.46-7.01], n=5 | 826 [826-826], n=5 | 1930 [1930-1931], n=5 |
| human_pfc_hbcc | 1,486,324 | h5 | 70662.6 [70662.6-70662.6], n=5 | 589.0 [580.7-629.2], n=5 | 1.82 [1.69-2.43], n=5 | 0.63 [0.61-0.70], n=5 | 877 [877-877], n=5 | 2036 [2036-2037], n=5 |
| mouse_brain_e18 | 1,306,127 | bpcells | 2909.0 [2909.0-2909.0], n=5 | 134.7 [130.6-149.4], n=5 | 2.50 [2.45-3.36], n=5 | 5.41 [5.18-5.79], n=5 | 745 [745-745], n=5 | 1835 [1835-1836], n=5 |
| mouse_brain_e18 | 1,306,127 | h5 | 30451.4 [30451.4-30451.4], n=5 | 181.4 [178.4-212.9], n=5 | 2.24 [1.90-2.72], n=5 | 0.54 [0.46-0.58], n=5 | 868 [868-868], n=5 | 1976 [1975-1976], n=5 |

## Runtime access

The first-query metric is the first backend getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MB | peak process RSS MB | first query s | warmed p50 s | warmed p95 s | 12-gene full block s | shuffled 100k row s | shuffled 100k block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 11.78 [10.81-15.83], n=10 | 1056 [1056-1056], n=10 | 1965 [1965-1965], n=10 | 1.4270 [1.3210-1.8900], n=10 | 1.3000 [1.1750-1.4030], n=10 | 1.6752 [1.5232-2.0092], n=10 | 1.237 [1.119-1.354], n=10 | 0.2350 [0.2040-0.2760], n=10 | 0.251 [0.232-0.332], n=10 |
| human_pfc_hbcc | 1,486,324 | h5 | 9.80 [7.51-14.08], n=10 | 1076 [1076-1076], n=10 | 1687 [1687-1688], n=10 | 1.1890 [0.9670-1.5320], n=10 | 0.3430 [0.2790-0.4110], n=10 | 0.6519 [0.5422-0.9816], n=10 | 0.389 [0.314-0.465], n=10 | 0.1290 [0.1130-0.2060], n=10 | 0.149 [0.125-0.240], n=10 |
| mouse_brain_e18 | 1,306,127 | bpcells | 11.30 [9.88-12.22], n=10 | 935 [935-936], n=10 | 1746 [1746-1746], n=10 | 1.2545 [1.1530-1.5950], n=10 | 1.1360 [0.9900-1.2390], n=10 | 1.4817 [1.3982-2.3960], n=10 | 0.698 [0.562-1.322], n=10 | 0.2295 [0.1670-0.3440], n=10 | 0.284 [0.214-0.419], n=10 |
| mouse_brain_e18 | 1,306,127 | h5 | 9.11 [7.80-10.06], n=10 | 999 [999-999], n=10 | 1612 [1612-1612], n=10 | 1.2080 [1.0050-1.6970], n=10 | 0.3330 [0.2630-0.3770], n=10 | 0.4232 [0.3424-0.4928], n=10 | 0.372 [0.336-0.702], n=10 | 0.1290 [0.1170-0.2040], n=10 | 0.148 [0.122-0.180], n=10 |

Correctness: 40/40 access processes matched both source-matrix fingerprints.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-09-22T18:28:55+0200 |
| git_branch | codex/integrate-performance-benchmark |
| git_dirty | false |
| package_version | 4.5.0 |
| r_version | R version 4.6.1 (2026-06-24) |
| os | Linux 6.18.33.2-microsoft-standard-WSL2 #1 SMP PREEMPT_DYNAMIC Thu Jun 18 21:54:43 UTC 2026 |
| cpu | 12th Gen Intel(R) Core(TM) i9-12900K |
| logical_cores | 24 |
| benchmark_threads | 1 |
| slurm_job_id |  |
| slurm_node_list |  |
| slurm_cpus_per_task |  |
| slurm_memory_per_node |  |
| memory_mb | 112683.6 |
| r_vector_limit_mb | Inf |
| source `mouse_brain_e18` | 4,216,018,749 bytes; SHA-256 `255a36ee92de25cb3568faa2c27d31fe6d0db30f285c5c977be8d6245de14044` |
| source `human_pfc_hbcc` | 14,150,526,668 bytes; SHA-256 `aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f` |

