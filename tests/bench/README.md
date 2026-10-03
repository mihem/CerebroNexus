# Real-data expression-backend benchmark

## Contents

- [Directory layout](#directory-layout)
- [Dataset scope](#dataset-scope)
- [Run the benchmark](#run-the-benchmark)
- [Scale profile](#scale-profile)
- [Script map](#script-map)

The backend benchmark runs two profiles in sequence. The full-source profile measures every cell in each source, then the scale profile measures the same sources at 1k, 10k, 50k, 100k, 500k, and 1m cells. Results and `CURRENT` pointers are never shared between them.

This workflow measures expression backends only. Focused Viewer checks live
under [`harnesses/viewer/`](harnesses/viewer/) and cannot pass, fail, publish,
or replace backend evidence.

> **Current status:** validated full-source and scale runs are selected by their
> separate `CURRENT` pointers under `results/benchmark/`.

Start with the [documentation index](docs/README.md). Read the
[methodology](docs/methodology.md) and [results guide](docs/results-guide.md)
before interpreting generated values.

## Directory layout

| path | purpose |
|---|---|
| `benchmark/` | current real-data benchmark implementation and launcher |
| `acceptance/` | acceptance policy, evaluator, CLI, documentation, and fixtures |
| `docs/` | methodology, result interpretation, and concise component findings |
| `harnesses/` | PR0-PR5 component harnesses, with callers documented in its README |
| `results/` | the only in-repository result root |

Results are separated by purpose: paper runs go to `results/benchmark/`,
acceptance records to `results/acceptance/`, and focused component comparisons
to `results/component/<component>/`. Temporary data stays outside the checkout. See
[`results/README.md`](results/README.md) for naming and ordering rules.

## Dataset scope

The default benchmark has two public million-scale sources: the complete
1,306,127-cell 10x mouse-brain matrix and the complete 1,486,324-cell PsychAD
HBCC human-PFC matrix. The scale profile also takes a 1,000,000-cell prefix
from each of these same two sources; those are not additional datasets.

The component Viewer/CRB harness uses one exact 1,000,000-cell subset of the
same 10x mouse-brain source. An optional 4,140,453-cell MSSM source is registered
for opt-in experiments, but it is not part of the default or committed results.

## Run the benchmark

Use a clean checkout on an exclusive high-memory Linux host with `nix-shell` available. One command starts the complete benchmark in the background; the full-source profile runs first and the six-tier scale profile runs second:

```bash
bash tests/bench/benchmark/run.sh
```

The launcher uses `$HOME/.cache/cerebronexus-benchmark/` for its source cache, scratch space, PID, exit status, and log. It preserves checksum-verified downloads and failed-run scratch directories. Optional `BENCH_THREADS`, `BENCH_SOURCE_CACHE`, `BENCH_SCRATCH_PARENT`, and `BENCH_STORAGE_DESCRIPTION` overrides remain available for unusual hosts.

```bash
bash tests/bench/benchmark/run.sh status
tail -f "$HOME/.cache/cerebronexus-benchmark/runner/benchmark.log"
```

If measurement completed but final validation or reporting failed, publish the retained measurements without rerunning them:

```bash
bash tests/bench/benchmark/run.sh resume /path/to/cerebro-bench.XXXXXX
```

The full-source profile rejects source overrides and runs exactly this grid:

| sources | cells | backends | builds | access processes |
|---|---:|---|---:|---:|
| 10x mouse brain E18 | 1,306,127 | bpcells, h5 | 10 | 20 |
| PsychAD HBCC human PFC | 1,486,324 | bpcells, h5 | 10 | 20 |

`embedded` is recorded as not representable because each complete matrix exceeds the 32-bit non-zero index limit of `Matrix::dgCMatrix`; it is not attempted on a smaller substitute.

Each source has one frozen 12-gene query plan. Runtime measurements cover full-cell single-gene and 12-gene reads plus deterministic reverse-ordered, non-contiguous reads of up to 100,000 cells. CRBs are written with the default qs2 codec, BPCells uses CerebroNexus's production gene-major writer, and fresh-process startup uses `readCerebro()`.

Every source/backend pair has five independent builds. Each built artifact is
opened by two independent access processes. Backend order alternates by repeat.

## Scale profile

The scale profile follows the complete-source profile automatically. It uses BPCells and H5 at all six fixed cell-count tiers and attempts embedded storage through 500k cells, with five independent builds and two access processes per successful build. An embedded failure is retained with empty metrics and does not invalidate the mandatory BPCells/H5 results.

Its immutable results are written under
`tests/bench/results/benchmark/scale/`; it neither reads nor replaces
`tests/bench/results/benchmark/full/`.

Validated runs are published under the selected workflow's
`results/benchmark/{scale,full}/runs/<run-id>/` directory; its own `CURRENT`
changes last. Every published run includes `evidence_manifest.csv`, which
records the byte size and MD5 checksum of every raw table, report, and figure
in the committed evidence package. Transient execution logs stay in the
external scratch directory and are not inventoried.

## Script map

| script | purpose |
|---|---|
| `benchmark/run.sh` | public background launcher for the complete benchmark |
| `benchmark/core.R` | load the benchmark modules below |
| `benchmark/sources.R`, `protocol.R` | source registry, profiles, and schedules |
| `benchmark/metrics.R`, `storage_backends.R` | measurements and backend I/O |
| `benchmark/reporting.R`, `resources.R` | summaries, figures, and capacity checks |
| `benchmark/cli.R`, `cli_*.R` | dispatch isolated benchmark stages |
| `acceptance/check.R` | evaluate normalized PR acceptance evidence |

The reproducibility harnesses are grouped by responsibility:

| path | purpose |
|---|---|
| `harnesses/crb/` | shared 1M fixture, CRB codec benchmark, and verification |
| `harnesses/backend/` | expression hot paths, BPCells layout, and bundle comparison |
| `harnesses/viewer/` | Viewer startup, page readiness, and interaction diagnostics |
| `harnesses/renderer/` | isolated WebGPU renderer benchmark and test app |

They remain because the acceptance policy and million-cell vignettes execute
them; they are not copies kept only for history. Their imported evidence lives
under `results/component/{crb,backend,viewer}/`.
