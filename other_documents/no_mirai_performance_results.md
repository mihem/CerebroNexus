# No-mirai Viewer performance results

## Scope

- Historical comparison: PR #153 baseline `2378aa2ffe80` versus
  `08f96280ff97`.
- Current microbenchmarks: the candidate working tree above `649508cf` versus
  the replaced algorithm on the same runtime.
- Runtime: Apple M1 Pro, 32 GiB RAM, macOS 27.0, R 4.6.1, Node 26.5.1.
- No asynchronous runtime, cross-session data/R6 cache, or new dependency is
  used.

## End-to-end and server-path measurements

| Path | Scale | Before | After | Time change | Allocation change |
| --- | ---: | ---: | ---: | ---: | ---: |
| Filter and sample | 500,000 cells | 35 ms | 6 ms | -82.9% | -79.0% |
| Full-display path | 500,000 cells | 34 ms | 5 ms | -85.3% | -82.0% |
| Metadata reuse | 500,000 cells | 93 ms | 78 ms | -16.1% | -48.5% |
| Hover construction | 50,000 cells | 1,898 ms | 1,526 ms | -19.6% | -52.8% |
| Gene multi-panel preparation | multiple panels | unchanged | unchanged | ~0% | 218.1 to 187.6 MiB (-14.0%) |

These measurements cover the branch through `08f96280ff97`. The hover row
predates the columnar payload result below.

## Current hot-path microbenchmarks

| Path | Fixture | Before | After | Time change | Allocated memory change |
| --- | --- | ---: | ---: | ---: | ---: |
| Configuration fingerprint | 500,000 cells | 159 ms | 148 ms | -6.9% | 36.43 to 29.75 MiB (-18.3%) |
| Hover build and JSON | 50,000 cells, 3 groups | 1,381 ms | 39 ms | -97.2% | 56.15 to 12.79 MiB (-77.2%) |
| External Spatial image reuse | 8 MiB image, 5 repeat renders | 332 ms | <1 ms | ~-100% | 105.23 to 0 MiB |
| Spatial hull reuse | 500,000 cells, 20 groups, 5 style changes | 475 ms | 79 ms | -83.4% | 945.44 to 189.09 MiB (-80.0%) |
| Browser hover hit test | 500,000 cells, 200 events | 1.359 ms/event | 0.005 ms/event | -99.6% | not measured |

The structured hover wire payload fell from 8.655 MiB to 1.442 MiB (-83.3%).
The hit-test grid costs 9.18 ms once after a projection change and breaks even
after 6.8 pointer events. It is built lazily, so pan, zoom, and orbit frames do
not pay that cost.

Moving interaction marks to the overlay removes 6,000,000 cell-loop iterations
and 2,000,000 bytes (1.91 MiB) of visibility-mask allocation per hover event for
four 500,000-cell panels, before counting the avoided Canvas paint operations.

## Logic and test simplification after `d0691418`

These are static source counts for the files touched by the cleanup, not new
runtime benchmark results. Existing behavior-oriented tests were retained;
dead-helper coverage and duplicate source-shape assertions were removed.

| Area | Before | After | Change |
| --- | ---: | ---: | ---: |
| Modified production source | 11,999 lines | 11,961 lines | -38 (-0.3%) |
| Targeted Viewer tests | 3,403 lines | 3,271 lines | -132 (-3.9%) |
| Targeted test cases | 95 | 89 | -6 (-6.3%) |
| Node process launches in coordinated-view tests | 12 | 10 | -2 (-16.7%) |
| Completed implementation plan | 93 lines | 0 | -93 (-100%) |
| Cleanup targets excluding report/design edits | 15,495 lines | 15,232 lines | -263 (-1.7%) |
| All modified files including report/design edits | 15,627 lines | 15,394 lines | -233 (-1.5%) |

| Control/data path | Before | After | Structural change |
| --- | ---: | ---: | ---: |
| Linked-view config expression reads | up to N genes | 1 batch | N to 1 |
| Gene render: coordinate reactive reads | 6 call sites | 1 | -83.3% |
| Gene render: expression reactive reads | 3 call sites | 2 | -33.3% |
| Gene render: hover reactive reads | 4 call sites | 1 | -75.0% |
| Gene render: trajectory reactive reads | 3 call sites | 1 | -66.7% |
| Spatial render: metadata reactive reads | 3 call sites | 1 | -66.7% |
| Spatial render: hover reactive reads | 4 call sites | 1 | -75.0% |
| Spatial render: coordinate reactive reads | 2 call sites | 1 | -50.0% |

The Shiny reactive values were already memoized within an invalidation cycle, so
the call-site reductions primarily flatten dependency flow and remove repeated
lookups. No additional runtime speedup is claimed without a fresh benchmark.

## Method

Run from the repository root:

```sh
Rscript tests/bench/viewer_hot_paths.R .
node tests/bench/viewer_browser_hot_paths.js .
```

Reported times are medians. R allocation figures come from `Rprofmem`; cached
image reuse records no new R allocation at its timer resolution. The benchmark
checks fingerprint equality, image data equality, hull equality, and indexed
versus linear hit-test equality before reporting results.

An initial 100,000-cell chunked fingerprint implementation was rejected after
measurement: it changed 144 ms to 174 ms and increased cumulative allocation.
Writing the identical byte stream directly avoids the raw copy and improves
both time and allocation without changing the stored fingerprint.

The test suite was intentionally not run for this measurement pass.
