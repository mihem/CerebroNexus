# Performance Acceptance Standard

- Status: implemented v1.2.0
- Date: 2026-09-22
- Applies to: `perf/pr0`-`perf/pr7` (pr0-pr5 machine-judged; pr6/pr7 manual checklists)
- Judge implementation: `tests/bench/acceptance/policy.R`, `tests/bench/acceptance/evaluator.R`, `tests/bench/acceptance/check.R`, `tests/testthat/test-bench-acceptance.R`

## Contents

- [1. Background](#1-background)
- [2. Goals](#2-goals)
- [3. Non-goals](#3-non-goals)
- [4. Tiers](#4-tiers)
- [5. Acceptance unit, baselines, and branch topology](#5-acceptance-unit-baselines-and-branch-topology)
- [6. Layer-to-harness mapping](#6-layer-to-harness-mapping)
- [7. Verdict rules](#7-verdict-rules-effective-from-l1)
- [8. Judge design](#8-judge-design)
- [9. Per-branch acceptance matrix](#9-per-branch-acceptance-matrix-pr0-pr5)
- [10. Manual acceptance checklists](#10-pr6--pr7-manual-acceptance-checklists)
- [11. Run process](#11-run-process)
- [12. Change process](#12-change-process)
- [13. Risks](#13-risks)
- [14. Decision record](#14-decision-record)
- [15. Landing requirements](#15-landing-requirements-for-the-judge)

## 1. Background

Since `69893a2b` (PR #165, 2026-09-11), `perf/pr0`-`perf/pr7` have been stacked one on top of another: pr0 3 commits, pr1 3, pr2 5, pr3 4, pr4 6, pr5 174, pr6 9, pr7 24. The optimizations span IO/CRB, backend hot paths, WebGPU rendering, startup, page readiness, UI, and builder layers.

`tests/bench` already contains high-quality assets: the scientific expression benchmark, CRB lifecycle, hot paths, startup, the million-cell page harness, and release comparison scripts. What is missing is a single executable way to decide, for each branch, whether it is acceptable:

- metrics, rounds, budgets, fixtures, and baselines are spread across scripts and documents;
- the same metric has different contracts in different historical documents (idle-inclusive, profiler-instrumented legacy);
- there is no configured registry of branch -> layer -> metric -> tolerance;
- decisions require reading Markdown by hand; there is no one-command pass/fail.

## 2. Goals

1. A three-tier system (L0 self-check / L1 branch acceptance / L2 release evidence) over the same raw data with different gates.
2. A machine-judged acceptance standard for pr0-pr5; a manual checklist for pr6/pr7.
3. The judge reads existing harness output only: no harness changes, no duplicated measurement code.
4. Reproducible, traceable decisions: SHAs, fixture hashes, platform, profile, and sample counts travel with every record.
5. Policy and logic separated: policy in `policy.R`, verdict logic in pure, unit-tested functions.

## 3. Non-goals

- Keep the component harnesses under `harnesses/`; acceptance evaluates their evidence without rewriting them.
- The judge never runs heavy benchmarks (no R app, no browser); `plan` only prints commands.
- Do not build new performance benchmarks for pr6/pr7 (manual checklist only).
- No CI million-cell performance gate; CI runs only the judge contract tests.
- No cross-platform, cross-fixture, or cross-contract pooling; Mac and Windows conclusions are reported separately.
- Do not redefine existing readiness boundaries; this standard references existing contracts (`primary_ready_ms` and friends).

## 4. Tiers

| Tier | Name | Gate | Use | When |
|---|---|---|---|---|
| L0 | self-check | correctness + harness health + obvious regression | fast iteration during development | any platform, any time, not recorded |
| L1 | branch acceptance | **hard gate** (section 7) | acceptance of pr0-pr5 before merge/restack | each branch tip, once per platform |
| L2 | release evidence | L1 + clean worktree + `evidence` + >=5 balanced rounds + memory gate + public report | upstream review, release or paper citation | before stack merge, before release |

L1 hard gate = no regression + 100% correctness + sample counts + at least one headline metric improved by >=10% (otherwise a recorded justification). Absolute budgets (<2s/<3s/<500ms) are tracking targets at L1, not blocking conditions; the L2 report must present budget status in full.

## 5. Acceptance unit, baselines, and branch topology

- Acceptance unit = **branch x platform x layer**; verdicts use **paired medians** from the same platform, fixture, and harness.
- Baseline = the branch's **declared parent tip**, pinned by SHA in the config.
- Topology: pr0->pr1->pr2->pr3->pr4->pr5 is a linear stack; pr6 forks from mid-pr5; pr7 is based on mid-pr6. pr6/pr7 are not machine-judged; only their topology and manual checklist are recorded.

| Branch | Declared parent SHA | Candidate tip SHA (2026-09-22) |
|---|---|---|
| pr0 | `69893a2b` | `99d305c0` |
| pr1 | `99d305c0` | `35128c51` |
| pr2 | `35128c51` | `5ad9ed40` |
| pr3 | `5ad9ed40` | `8f38ee56` |
| pr4 | `8f38ee56` | `a78b5454` |
| pr5 | `a78b5454` | `258cee14` |
| pr6 (manual) | fork point `33145024` | `88930fb5` |
| pr7 (manual) | fork point `cc973713` | `17b04c92` |

- **No rebase is required for acceptance.** When a branch is rewritten (restack/amend), update the SHAs in the config and re-run only the affected layers of the rewritten branch and its descendants; records are replaced per SHA, older records are retained.
- L2 uses the same pinned parent baseline as L1, with stricter provenance,
  sample-count, memory, and reporting gates.

## 6. Layer-to-harness mapping

| Layer | Branch | Harness | Raw output | Main metrics |
|---|---|---|---|---|
| IO/CRB | pr0 | `harnesses/crb/benchmark.R`, `harnesses/crb/verify.R` | CSV (wide: `pr165/thin_rds/latest`) | `write_median_ms`, `decode_median_ms`, `hydrated_median_ms`, `size_mib` |
| Backend hot paths | pr1 | `harnesses/backend/hot_paths.R` | TSV (wide: `pr165/thin_crb/latest`) | 8 operations in ms + alloc MiB + `check` |
| Linked bundle | pr1 | same run (bundle output) | TSV (`candidate` rows) | `build_ms`, `encode_ms`, `object_mib`, `json_mib` |
| Renderer | pr2 | `harnesses/renderer/benchmark.R` | one CSV per revision | first frame / frame times, WebGPU path, no-WebGPU fallback |
| Startup | pr3 | `harnesses/viewer/startup.R` | one CSV per revision | `library_ms`, `app_construct_ms`, `server_listen_ms`, `browser_load_ms`, `load_to_data_ms`, `browser_to_data_ms`, `process_to_data_ms` |
| Page readiness | pr4 (framework) + pr5 (per page) | `harnesses/viewer/page_readiness.R` | `raw.tsv` (single file with both candidates and provenance columns) | `primary_ready_ms` (fresh/repeat), `correctness_pass`, `pass`, resource columns |

## 7. Verdict rules (effective from L1)

### 7.1 Tolerances

Formula: `cand_median <= base_median * (1 + r) + floor`. Improvements are not capped.

| Class | r | floor | Applies to |
|---|---:|---:|---|
| latency | 5% | 50 ms | page `primary_ready_ms`, startup phases, hot-path operations, CRB write/decode/read |
| allocation | 15% | 16 MiB | Rprofmem cumulative allocations |
| bytes | 2% | 1 MiB | JSON payload, bundle object, CRB payload |
| memory | 15% | 64 MiB | R RSS, Chrome RSS, JS heap; **reported at L1, gated at L2** |

Units are declared when each metric is registered (ms / MiB / bytes).

### 7.2 Budgets and crossings

Page budgets keep the existing contract: fresh ordinary pages `<2000 ms`, Trajectory/HLA `<3000 ms`, repeat `<500 ms` (strict `<`).

- Budget state is decided from the **candidate/baseline median versus the page budget**, not from individual `pass` rows;
- baseline pass -> candidate fail = **hard failure**;
- baseline fail -> candidate pass = report (continuing improvement);
- both fail = convergence tracking table, not blocking at L1;
- current tracked items (pr5 first visits): Gene Expression ~3.0 s, Immune Repertoire ~3.8 s, Coordinated Views ~8.6 s.

### 7.3 Correctness and provenance

- Correctness 100%: pages require every `correctness_pass = TRUE` and `status = ok`; backend/bundle require `check = equal`; CRB requires `harnesses/crb/verify.R <legacy-rds> all` to print `1M CRB all contract: PASS` (exit 0); other layers follow their own contracts (fingerprints, bound events).
- Provenance: candidate/baseline SHAs match the config pins; fixture/artifact `sha256` matches the config; platform matches; profile and rounds meet the layer requirement; L2 requires a clean worktree.
- L2 excludes `profiler_instrumented` rows; timing and memory are collected in separate runs.

### 7.4 Minimum sample counts

| Layer | L1 | L2 |
|---|---:|---:|
| CRB | >=5 alternating rounds | >=5 |
| hot paths / bundle | >=3 alternating rounds | >=3 |
| startup | >=3 (`CEREBRO_STARTUP_REPEATS`) | >=5 |
| renderer | >=15 repeats | >=15 |
| pages | `quick` + 3 balanced rounds | `evidence` + >=5 rounds |

For pages, n means samples per page x visit (first/repeat) for the median, for
both candidate and baseline. CRB, hot-path, bundle, and renderer summaries must
record their observed round count; it must match `run-config.tsv`. Below the
minimum the evidence is invalid (exit 2): neither pass nor fail.

### 7.5 Performance credit (headline)

Every machine-judged branch declares 1-3 headline metrics in the config. L1 requires at least one headline metric to improve by >=10% over the baseline; otherwise a recorded justification ("why this branch exists", e.g. its main gain is size or memory) is required. Headline is judged per platform: each platform's L1 record must satisfy or document it. This prevents a zero-gain branch from passing on the no-regression gate alone.

### 7.6 Waivers

Zero waivers by default. A waiver must be registered explicitly in the config
(metric, reason, date); the judge prints `PASS (WAIVED)` and surfaces it at the
top of the summary. Correctness failures cannot be waived. Other waivers apply
to both L1 and L2, but an L2 report must list every effective waiver.

### 7.7 Outcomes and exit codes

- exit 0 = accepted (may include `REPORT` / `WAIVED`);
- exit 1 = rejected (hard regression, correctness failure, budget crossing, headline miss);
- exit 2 = invalid evidence (missing files, SHA/hash/profile/rounds/platform mismatch, dirty L2 worktree).

The judge writes `acceptance.tsv` (one row per metric: metric, scope, visit, baseline, candidate, delta, rule, verdict) and `acceptance.md` (human summary).

## 8. Judge design

### 8.1 File layout

```
tests/bench/acceptance/STANDARD.md          # this document
tests/bench/acceptance/policy.R             # policy: tolerances/budgets/samples/layers/branches/waivers/change log
tests/bench/acceptance/evaluator.R          # pure functions: adapters, tolerance math, provenance, verdicts
tests/bench/acceptance/check.R              # CLI: plan / judge
tests/testthat/test-bench-acceptance.R      # judge contract tests
tests/bench/acceptance/README.md            # how records are named and stored
tests/bench/acceptance/fixtures/            # frozen raw-format fixtures for the adapters
tests/bench/results/acceptance/<branch>-<sha8>-<platform>/  # acceptance records (small MD/TSV committed; raw data stays out of git, hashes recorded)
```

### 8.2 CLI

```
Rscript tests/bench/acceptance/check.R plan  pr3 --platform windows
Rscript tests/bench/acceptance/check.R judge pr3 --platform windows --run-dir <dir> [--json]
```

- `plan`: prints the branch's pinned SHAs, the harness command templates, required rounds, expected raw file names, and budget targets; `--write-run-dir` scaffolds empty `run-config.tsv` / `files.tsv`.
- `judge`: reads `--run-dir`, validates evidence, then decides; writes `acceptance.tsv` / `acceptance.md`. The judge is read-only: it never starts a benchmark.

### 8.3 run-dir contract

Harness outputs differ in shape (CRB single wide file, hot paths single two-root file, startup one CSV per revision, pages single file with both candidates). Two small files express the role mapping:

- `run-config.tsv`: `branch`, `layer`, `platform`, `level` (`L1`/`L2`), `profile`, `mode`, `rounds`, `candidate_label`, `candidate_sha`, `baseline_label`, `baseline_sha`, `baseline_column`, `candidate_column`, `fixture_path`, `fixture_sha256`, `git_dirty`, `created_utc`.
- `files.tsv`: `role` (candidate/baseline/both), `file_role`, `relative_path` (run-dir relative or absolute), `sha256` (required for L2, optional for L1 local development).

### 8.4 Config schema (illustrative)

```r
acceptance_config <- list(
  version = "1.1.0",
  tolerances = list(
    latency    = list(rel = 0.05, floor = 50),
    allocation = list(rel = 0.15, floor = 16),
    bytes      = list(rel = 0.02, floor = 1),
    memory     = list(rel = 0.15, floor = 64, l1_verdict = "REPORT")
  ),
  budgets = list(fresh_ms = 2000, fresh_special_ms = 3000,
                 special_pages = c("trajectory", "hla"), repeat_ms = 500),
  sample_min = list(crb = 5L, hot_paths = 3L, startup = 3L,
                    renderer = 15L, pages_l1 = 3L, pages_l2 = 5L),
  layers = list(
    crb = list(adapter = "acceptance_extract_crb", files = "*.csv",
               file_roles = c("crb", "correctness"),
               commands = list(windows = "...", mac = "...")),
    hot_paths = list(adapter = "acceptance_extract_hot_paths", files = "*.tsv",
                     file_roles = c("hot_paths", "bundle"),
                     commands = list(windows = "...", mac = "...")),
    startup = list(adapter = "acceptance_extract_startup", files = "*.csv",
                   file_roles = c("startup_baseline", "startup_candidate"),
                   commands = list(windows = "...", mac = "...")),
    renderer = list(adapter = "acceptance_extract_renderer", files = "*.csv",
                    file_roles = c("renderer_baseline", "renderer_candidate"),
                    commands = list(windows = "...", mac = "...")),
    pages = list(adapter = "acceptance_extract_pages", files = "raw.tsv",
                 file_roles = c("pages"),
                 commands = list(windows = "...", mac = "..."))
  ),
  branches = list(
    pr0 = list(layer = "crb", parent = "69893a2b", candidate = "99d305c0",
               headline = c("write_median_ms", "size_mib")),
    # pr1 .. pr5 are structurally identical; pr6/pr7 machine = FALSE + manual checklist
    pr6 = list(machine = FALSE, checklist = "pr6-ui-accessibility",
               fork_point = "33145024", candidate = "88930fb5"),
    pr7 = list(machine = FALSE, checklist = "pr7-builder-workspace",
               fork_point = "cc973713", candidate = "17b04c92")
  ),
  waivers = list(),
  change_log = list()
)
```

### 8.5 Adapters

One `extract_*` pure function per layer turns that layer's raw file into a normalized long table: `metric_id`, `scope`, `visit`, `role`, `value`, `unit`, `correctness`. Adapters read and normalize only; they do not decide. Format changes are caught by contract tests over frozen fixture files.

### 8.6 Judge contract tests

- tolerance boundaries: floor active/inactive, exactly at the limit, exact improvement;
- budget crossings: pass->fail hard failure, fail->pass report, both-fail tracking;
- correctness veto: any `correctness_pass = FALSE` -> exit 1;
- insufficient samples -> exit 2; SHA drift -> exit 2; fixture hash mismatch -> exit 2;
- L2 dirty worktree, wrong profile/rounds -> exit 2;
- memory metrics `REPORT` at L1 and gated at L2;
- waiver shows `PASS (WAIVED)` and surfaces at the top of the summary;
- missing headline or <10% improvement -> exit 1 unless a justification is registered.

## 9. Per-branch acceptance matrix (pr0-pr5)

`plan` prints every layer command; a human runs it on each target platform. Common requirements: one L1 run per platform; 100% correctness; headline satisfied.

| Branch | Layer | Key metrics / headline | Notes |
|---|---|---|---|
| pr0 | CRB/IO | write/decode/hydrated medians, payload MiB; headline = write speedup + payload shrink | the before input is the legacy 1M RDS; lazy IR/spatial loading must prove content equality with eager |
| pr1 | backend hot paths + bundle | projection selection, index resolution, single gene/RGB/multi-panel/mean, alloc, bundle build/JSON; headline = RGB, mean, bundle build | storage-order reads must not change values or order (`check = equal`) |
| pr2 | renderer | observed backend first frame; headline = first-frame time | checks context loss and GPU errors for the measured backend; fallback remains a package regression contract |
| pr3 | startup | each phase; headline = `load_to_data_ms` / `browser_to_data_ms` / `process_to_data_ms` | deferred pages and collapsed filters must still initialize correctly (existing regression tests) |
| pr4 | pages (framework) | all-page `primary_ready_ms` fresh/repeat, budget crossings, deferral/reuse correctness; headline = budget pass count + page improvement | full page sweep; paired against the pr3 baseline |
| pr5 | pages (per page) + hot-path regression | grouped by touched page (groups/overview/gene_expression/immune_repertoire/trajectory/hla/coordinated_views); headline = IR first visit, all repeats | Spatial is skipped when the fixture has no data, and the record says so |

## 10. pr6 / pr7 manual acceptance checklists

Method: after the full `testthat` suite and precheck pass, confirm each item by
hand and write the result into a `manual` record under
`tests/bench/results/acceptance/` (not machine-judged).

**pr6 (UI / accessibility / progressive loading)**
1. Per-page loading feedback appears and disappears at the right time; no stuck or fake progress.
2. Large atlases default to static / progressive loading without losing data or interaction.
3. Keyboard focus order and visible focus are correct; legend wrapping and selector width show no regression at 390/800/1200 viewports.
4. Compared with pr5, page interactions show no visible stutter or flicker.

**pr7 (builder workspace)**
1. Local dataset import / cancel (native picker) behaves correctly.
2. Large asset import; spatial sidecars preserved.
3. Interrupted release recovers safely; the app bundle digest reuse produces byte-identical output.
4. Windows path preflight matches the actual viewer paths.
5. Review actions are correctly disabled while alignment saves; cancel leaves no half-written state.

## 11. Run process

- **L0 (during development)**: `quick`, 1 round, any platform, not recorded.
- **L1 (per branch, once per platform)**:
  1. `plan` scaffolds the run-dir and prints commands;
  2. run the harness on Mac / Windows in an external raw-data workspace and
     reference those files from the acceptance record's `files.tsv`;
  3. decide using section 7 and write `acceptance.tsv` / `acceptance.md` (the `judge` command does this once the judge lands);
  4. failure -> fix or register a waiver -> re-run only the affected layers.
- **L2 (before stack merge / release)**: `evidence` + >=5 balanced rounds + clean worktree + both platforms + memory gate; produce the public report (tables + figures) and an `acceptance/` record.
- Raw data stays in the cache / `CerebroNexus-benchmarks`-style directories and out of git; only summaries, hashes, and run-configs are committed.

## 12. Change process

- **New branch**: edit only `branches` in `policy.R` (SHA, layer, headline); no judge change.
- **New layer/metric**: register it in the config first, then add the adapter and contract tests.
- **Budget/tolerance change**: record the date and reason in this document and in the config `change_log`.
- Every judgement records the config version and hash so conclusions stay traceable.

## 13. Risks

- **Windows budgets not yet collected**: start with the same numeric targets as Mac; L1 only enforces no-regression. Update the budget table once both platforms have their own baselines.
- **pr5 is large and currently has 3 over-budget first visits**: L1 judges no-regression; over-budget items go to the tracking table and must be flagged in the L2 report.
- **Harness outputs are wide tables**: format changes break adapters; contract tests use frozen fixture files, and a format change must update those tests in the same commit.
- **Mac and Windows conclusions are never pooled**: report separately so machine differences are not averaged away.
- **The pr0 harness needs the legacy 1M RDS input**: if the cache is missing, prepare it first; preparation time is not part of the judgement.

## 14. Decision record

1. Purpose: self-check / branch acceptance / release evidence, all three, as one layered standard.
2. Delivery: document + harness-driven automated judging (full benchmarks still run manually on Mac / Windows).
3. Pass rule: no-regression is the hard gate; budgets are tracking targets.
4. pr6/pr7: machine judgement covers pr0-pr5 only; pr6/pr7 use manual checklists.
5. Reference environments: dual platform (Mac + Windows conclusions reported separately).
6. Technical route: an acceptance judge layer (existing harnesses unchanged).
7. Memory metrics: reported at L1, gated at L2 (+15% / 64 MiB).

## 15. Landing requirements for the judge

Status: implemented (judge v1.1.0). The sections above describe the shipped behavior; config and code live in the files listed in section 8.1.

- Adapters: one per layer for pr0-pr5, all emitting the normalized long table.
- `check.R`: `plan` / `judge` subcommands with exit semantics 0/1/2 as in section 7.7.
- Contract tests: every item in section 8.6 green.
- The standard and the config agree; historical raw data can be re-judged as judge self-validation.
- Remaining manual step: at least one branch (pr3 recommended) completes the dual-platform L1 rehearsal on the Mac.
