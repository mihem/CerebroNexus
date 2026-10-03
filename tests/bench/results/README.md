# Benchmark result contract

## Contents

- [Repository layout](#repository-layout)
- [External data](#external-data)

## Repository layout

`tests/bench/results/` is the only result root inside the repository.

- `benchmark/{full,scale}/runs/<UTC>-<sha>-<profile>/` contains immutable
  real-data benchmark runs; each profile's `CURRENT` pointer is updated last.
- `acceptance/<branch>-<candidate8>-<platform>/` contains one acceptance record
  for a pinned candidate and platform.
- `component/<component>/` contains focused CRB, backend, Viewer, or renderer
  measurements. A Thin CRB comparison writes `raw.csv`, `summary.csv`,
  `environment.csv`, and `summary.md` into one timestamped CRB directory.
- Historical PR0-PR5 evidence stays beside the component it measures instead
  of being mixed in one archive directory.

## External data

Raw datasets, caches, temporary artifacts, and failed-run scratch directories
stay outside the Git checkout. Low-level commands may accept explicit temporary
paths, but repository evidence must be published into one of the locations
above.
