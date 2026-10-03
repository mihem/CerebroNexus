# Expression-backend benchmark on real public datasets

**Run:** `20260922T005414Z-c00cecb5ba0c-full`
**Profile:** `panel_c2`
**Git:** `c00cecb5ba0c662a58da9ad2e30a40424245ee71`

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
| human_pfc_hbcc | 1,486,324 | bpcells | 27701.6 [27701.6-27701.6], n=5 | 436.1 [434.9-437.0], n=5 | 1.79 [1.78-1.80], n=5 | 4.08 [4.05-4.11], n=5 | 818 [818-818], n=5 | 1923 [1922-1923], n=5 |
| human_pfc_hbcc | 1,486,324 | h5 | 70662.6 [70662.6-70662.6], n=5 | 529.2 [527.1-531.9], n=5 | 1.26 [1.25-1.27], n=5 | 0.65 [0.65-0.66], n=5 | 907 [907-907], n=5 | 2023 [2022-2023], n=5 |
| mouse_brain_e18 | 1,306,127 | bpcells | 2909.1 [2909.1-2909.1], n=5 | 125.0 [124.9-125.3], n=5 | 1.47 [1.44-1.52], n=5 | 2.35 [2.33-2.45], n=5 | 757 [757-757], n=5 | 1828 [1827-1829], n=5 |
| mouse_brain_e18 | 1,306,127 | h5 | 30451.4 [30451.4-30451.4], n=5 | 155.7 [154.7-156.2], n=5 | 1.24 [1.22-1.24], n=5 | 0.45 [0.45-0.46], n=5 | 917 [917-917], n=5 | 2018 [2018-2019], n=5 |

## Runtime access

The first-query metric is the first backend getter call in a fresh R process. The operating-system file cache is uncontrolled, so it is not a cold-disk measurement.

| source | cells | backend | startup s | RSS MB | peak process RSS MB | first query s | warmed p50 s | warmed p95 s | 12-gene full block s | shuffled 100k row s | shuffled 100k block s |
|---|---:|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| human_pfc_hbcc | 1,486,324 | bpcells | 4.14 [4.09-4.19], n=10 | 924 [924-924], n=10 | 1864 [1824-1865], n=10 | 0.7895 [0.7650-0.8160], n=10 | 0.6320 [0.6230-0.6440], n=10 | 0.7020 [0.6844-0.7134], n=10 | 0.001 [0.001-0.002], n=10 | 0.0945 [0.0870-0.0970], n=10 | 0.038 [0.037-0.039], n=10 |
| human_pfc_hbcc | 1,486,324 | h5 | 5.51 [5.47-5.72], n=10 | 997 [997-997], n=10 | 2005 [1900-2037], n=10 | 0.4925 [0.4760-0.5710], n=10 | 0.0670 [0.0670-0.0690], n=10 | 0.0907 [0.0894-0.0932], n=10 | 0.065 [0.064-0.066], n=10 | 0.0100 [0.0100-0.0100], n=10 | 0.056 [0.056-0.059], n=10 |
| mouse_brain_e18 | 1,306,127 | bpcells | 3.85 [3.82-3.87], n=10 | 898 [898-898], n=10 | 1615 [1614-1616], n=10 | 0.5725 [0.5610-0.6100], n=10 | 0.4695 [0.4620-0.4760], n=10 | 0.5386 [0.5238-0.5506], n=10 | 0.001 [0.001-0.002], n=10 | 0.0710 [0.0650-0.0730], n=10 | 0.036 [0.035-0.038], n=10 |
| mouse_brain_e18 | 1,306,127 | h5 | 4.98 [4.95-5.04], n=10 | 969 [969-970], n=10 | 1541 [1472-1595], n=10 | 0.4855 [0.4780-0.5380], n=10 | 0.0220 [0.0210-0.0220], n=10 | 0.0716 [0.0706-0.0722], n=10 | 0.014 [0.013-0.014], n=10 | 0.0090 [0.0080-0.0090], n=10 | 0.052 [0.051-0.052], n=10 |

Correctness: 40/40 access processes matched both source-matrix fingerprints.

## Provenance

| key | value |
|---|---|
| generated_at | 2026-09-22T02:54:24+0200 |
| git_branch | paper/real-data-benchmark |
| git_dirty | false |
| package_version | 4.6.4 |
| r_version | R version 4.6.1 (2026-06-24) |
| os | Linux 6.8.0-136-generic #136-Ubuntu SMP PREEMPT_DYNAMIC Wed Jul  1 21:53:05 UTC 2026 |
| cpu | Intel(R) Core(TM) i7-14700 |
| logical_cores | 28 |
| benchmark_threads | 1 |
| slurm_job_id |  |
| slurm_node_list |  |
| slurm_cpus_per_task |  |
| slurm_memory_per_node |  |
| memory_mb | 128434.2 |
| r_vector_limit_mb | Inf |
| source `mouse_brain_e18` | 4,216,018,749 bytes; SHA-256 `255a36ee92de25cb3568faa2c27d31fe6d0db30f285c5c977be8d6245de14044` |
| source `human_pfc_hbcc` | 14,150,526,668 bytes; SHA-256 `aeca0480ab8941a7e4cf6b0ff6dc8c5f9d0de376466d65ca8198dc873f1cb16f` |
