# Acceptance records

## Contents

- [Record layout](#record-layout)
- [Manual records](#manual-records)
- [Fixtures](#fixtures)

## Record layout

One directory per judgement under `tests/bench/results/acceptance/`:

`<branch>-<candidate8>-<platform>/`

Each record contains the small files that are safe to commit:

- `run-config.tsv`, `files.tsv` - provenance as consumed by `check.R`
- `acceptance.tsv`, `acceptance.md` - the verdicts

Raw benchmark output stays outside git (cache, benchmark output directory, or
`CerebroNexus-benchmarks`); `files.tsv` pins the hash of every raw file used.
Absolute paths are allowed in `files.tsv`.

The CLI rejects acceptance record directories outside this root. A custom
`--results-root` is available only for isolated tests or external evidence
workspaces; repository records still belong under `tests/bench/results/`.

## Manual records

Manual records for pr6/pr7 use `pr6-<sha8>-manual.md` / `pr7-<sha8>-manual.md`
and list the checklist from `STANDARD.md` section 10 with a date
and the confirming person.

## Fixtures

Fixtures under `tests/bench/acceptance/fixtures/` are format contracts for the
adapters. They are synthetic and never used for performance conclusions.
