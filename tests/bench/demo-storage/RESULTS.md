# Lossless demo CRB storage

## Scope and baseline

- Baseline: master `4d74a8394e880a3df2ac5220eeae121472254f46` (also the current data in #192/#193).
- Date: 2026-10-12. Windows host, R execution in WSL; R 4.6.1, qs2 0.3.1.
- Repack the existing nine qs2 payloads with compression level 19, one thread. No raw-data regeneration, downsampling, numeric conversion, image re-encoding, or application reader/writer changes.
- Complete payload verification: for every file, the version-3 R serialization of the source qs2 object is byte-identical to that of the decoded candidate. This includes R6 fields, expressions, cell order, coordinates, metadata, analytical results and embedded images.

## Measurements

| File | Before MiB | After MiB | Saved | Baseline read ms | Candidate read ms |
|---|---:|---:|---:|---:|---:|
| `demo_full_tcr_bcr.crb` | 1.037 | 0.853 | 17.8% | 19.06 | 17.98 |
| `demo_hla_tcr_dextramer.crb` | 9.032 | 7.196 | 20.3% | 95.19 | 85.71 |
| `demo_spatial_merfish.crb` | 1.822 | 1.664 | 8.7% | 18.96 | 19.10 |
| `demo_spatial_slideseq.crb` | 1.476 | 1.199 | 18.8% | 22.76 | 20.20 |
| `demo_spatial_visium.crb` | 5.740 | 4.532 | 21.1% | 55.67 | 46.33 |
| `demo_spatial_xenium.crb` | 2.717 | 2.094 | 22.9% | 31.53 | 27.05 |
| `demo_spatial.crb` | 0.617 | 0.490 | 20.6% | 11.41 | 10.88 |
| `demo_trekker.crb` | 6.791 | 5.401 | 20.5% | 82.97 | 56.35 |
| `example.crb` | 1.027 | 0.844 | 17.8% | 19.44 | 18.88 |

Total: 31,729,141 → 25,451,032 bytes; **5.987 MiB / 19.79% smaller**. This reduces the checked-out/released artifacts; older binaries remain in Git history.

Read timings are medians of five interleaved baseline/candidate pairs per file after a warm-up read. Alternating order; `qs2::qs_read()` only, with GC outside timing and a C++ steady (monotonic) clock. These are warm local-file decode measurements, not cold OS reads, Viewer page-ready times or UI performance claims. The tiny MERFISH difference (about 0.14 ms) is within the observed run variation.

Three fresh R processes per condition each loaded the package and then read the nine files one at a time. Peak RSS in KiB:
- baseline: [1187740, 1187220, 1188612]; median 1,187,740 KiB.
- candidate: [1187864, 1188676, 1188344]; median 1,188,344 KiB.

These RSS values include package initialization and the R process, not just decoder allocations; they show no meaningful whole-process increase in this small comparison. They do not establish a long-running Viewer memory bound.

Level 19 performs more offline compression work than the normal fast writer; write-time screening is not used for quantitative conclusions. It is used only by the release compaction tool; the application keeps its existing default. An initial `system.time()` screening sample showed a negative elapsed time and was excluded from performance conclusions; the reported read samples use the monotonic clock.

## Reproduction and evidence

- `data-raw/compact_demo_crbs.R`: source-preserving staging, complete payload comparison, keep-original fallback if no size saving.
- `compression.csv`: exact sizes and round-trip results for all nine files.
- `read-samples.csv`: all 90 monotonic read samples.
- `benchmark-read.R`: rerun from the repository root, passing a directory with `baseline/` and `candidate/` CRBs. Requires Rcpp at benchmark time only.
- Obtain baseline files from the baseline commit, run the compactor into `candidate/`, then run the benchmark. Keep HDF5/image companions in place when installing CRBs back into the app.

## Validation and limitations

- Tool tests cover complete sparse/R6 payload preservation (including NA, non-integer expression, numeric-looking cell IDs and reordered coordinates), source immutability, no-overwrite behavior and corrupt input rejection.
- All data-raw R scripts parse. The active IR, Trajectory, Spatial, HLA dextramer and Trekker builders now use the public CRB reader/writer; original upstream RDS files and caches keep their native format.
- The actual Spatial post-export function was exercised with a synthetic qs2 exporter, both without an image and with an image. Its normalized named image collection now retains the injected image and bounds; this is a build-time compatibility fix, not a change to the existing demo images.
- Full raw-data regeneration was not run: some source downloads are large, and the base example preparation still has legacy environment/path dependencies. This change does not claim byte-identical regeneration across R/Seurat versions.
- Full source regression: 14,416 passed, 0 failed/errors, 2 conditional skips. This includes the latest Spatial builder regression.
- Installed-package tests: 13,844 passed, 0 failed/warnings, 76 conditional/environment skips (the build tools are excluded from the installed package).
- R CMD check with vignettes: 0 errors, 0 warnings, 2 notes (global-environment assignment and Chrome temporary directories).
- New R tool/test/benchmark files pass Air 0.12.0 formatting checks; git diff --check passes. GitHub Nix CI remains the upstream acceptance check.
