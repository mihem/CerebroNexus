# Machine-readable policy for the performance acceptance standard.
# Human-readable rules live in tests/bench/acceptance/STANDARD.md.

ACCEPTANCE_CONFIG <- list(
  version = "1.2.0",
  tolerances = list(
    latency = list(rel = 0.05, floor = 50),
    allocation = list(rel = 0.15, floor = 16),
    bytes = list(rel = 0.02, floor = 1),
    memory = list(rel = 0.15, floor = 64, l1_verdict = "REPORT")
  ),
  budgets = list(
    fresh_ms = 2000,
    fresh_special_ms = 3000,
    special_pages = c("trajectory", "hla"),
    repeat_ms = 500
  ),
  sample_min = list(
    crb = 5L,
    hot_paths = 3L,
    startup = 3L,
    startup_l2 = 5L,
    renderer = 15L,
    pages_l1 = 3L,
    pages_l2 = 5L
  ),
  layers = list(
    crb = list(
      adapter = "acceptance_extract_crb",
      files = "*.csv",
      file_roles = c("crb", "correctness"),
      correctness_marker = "1M CRB all contract: PASS",
      commands = list(
        windows = c(
          "Rscript tests/bench/harnesses/crb/benchmark.R <legacy-rds> 5 <crb.csv>",
          "Rscript tests/bench/harnesses/crb/verify.R <legacy-rds> all *> <correctness.txt>"
        ),
        mac = c(
          "Rscript tests/bench/harnesses/crb/benchmark.R <legacy-rds> 5 <crb.csv>",
          "Rscript tests/bench/harnesses/crb/verify.R <legacy-rds> all > <correctness.txt> 2>&1"
        )
      )
    ),
    hot_paths = list(
      adapter = "acceptance_extract_hot_paths",
      files = "*.tsv",
      file_roles = c("hot_paths", "bundle"),
      commands = list(
        windows = c(
          "Rscript tests/bench/harnesses/backend/hot_paths.R <baseline-root> <candidate-root> <crb> 3 > <hot-paths.tsv>",
          "Rscript tests/bench/harnesses/backend/bundle.R <baseline-root> <candidate-root> <crb> 3 > <bundle.tsv>"
        ),
        mac = c(
          "Rscript tests/bench/harnesses/backend/hot_paths.R <baseline-root> <candidate-root> <crb> 3 > <hot-paths.tsv>",
          "Rscript tests/bench/harnesses/backend/bundle.R <baseline-root> <candidate-root> <crb> 3 > <bundle.tsv>"
        )
      )
    ),
    renderer = list(
      adapter = "acceptance_extract_renderer",
      files = "*.csv",
      file_roles = c("renderer_baseline", "renderer_candidate"),
      commands = list(
        windows = c(
          "Push-Location <baseline-root>; Rscript tests/bench/harnesses/renderer/benchmark.R 1000000 15 <baseline.csv>; Pop-Location",
          "Push-Location <candidate-root>; Rscript tests/bench/harnesses/renderer/benchmark.R 1000000 15 <candidate.csv>; Pop-Location"
        ),
        mac = c(
          "(cd <baseline-root> && Rscript tests/bench/harnesses/renderer/benchmark.R 1000000 15 <baseline.csv>)",
          "(cd <candidate-root> && Rscript tests/bench/harnesses/renderer/benchmark.R 1000000 15 <candidate.csv>)"
        )
      )
    ),
    startup = list(
      adapter = "acceptance_extract_startup",
      files = "*.csv",
      file_roles = c("startup_baseline", "startup_candidate"),
      commands = list(
        windows = c(
          "$env:CEREBRO_STARTUP_REPEATS='3'; $env:CEREBRO_STARTUP_OUTPUT='<baseline.csv>'; Rscript tests/bench/harnesses/viewer/startup.R baseline=<baseline.crb>",
          "$env:CEREBRO_STARTUP_OUTPUT='<candidate.csv>'; Rscript tests/bench/harnesses/viewer/startup.R candidate=<candidate.crb>"
        ),
        mac = c(
          "CEREBRO_STARTUP_REPEATS=3 CEREBRO_STARTUP_OUTPUT=<baseline.csv> Rscript tests/bench/harnesses/viewer/startup.R baseline=<baseline.crb>",
          "CEREBRO_STARTUP_REPEATS=3 CEREBRO_STARTUP_OUTPUT=<candidate.csv> Rscript tests/bench/harnesses/viewer/startup.R candidate=<candidate.crb>"
        )
      )
    ),
    pages = list(
      adapter = "acceptance_extract_pages",
      files = "raw.tsv",
      file_roles = c("pages"),
      commands = list(
        windows = "Rscript tests/bench/harnesses/viewer/page_readiness.R baseline=<root> <crb> <out.tsv> 3 candidate=<root>",
        mac = "Rscript tests/bench/harnesses/viewer/page_readiness.R baseline=<root> <crb> <out.tsv> 3 candidate=<root>"
      )
    )
  ),
  branches = list(
    pr0 = list(machine = TRUE, layer = "crb", parent = "69893a2b",
               candidate = "99d305c0",
               headline = c("write_median_ms", "size_mib")),
    pr1 = list(machine = TRUE, layer = "hot_paths", parent = "99d305c0",
               candidate = "35128c51",
               headline = c("RGB expression_ms", "Mean expression_ms",
                            "build_ms")),
    pr2 = list(machine = TRUE, layer = "renderer", parent = "35128c51",
               candidate = "5ad9ed40",
               headline = c("firstFrameMs", "rgbMedianMs")),
    pr3 = list(machine = TRUE, layer = "startup", parent = "5ad9ed40",
               candidate = "8f38ee56",
               headline = c("load_to_data_ms", "process_to_data_ms")),
    pr4 = list(machine = TRUE, layer = "pages", parent = "8f38ee56",
               candidate = "a78b5454", headline = c("primary_ready_ms")),
    pr5 = list(machine = TRUE, layer = "pages", parent = "a78b5454",
               candidate = "258cee14", headline = c("primary_ready_ms")),
    pr6 = list(machine = FALSE, checklist = "pr6-ui-accessibility",
               fork_point = "33145024", candidate = "88930fb5"),
    pr7 = list(machine = FALSE, checklist = "pr7-builder-workspace",
               fork_point = "cc973713", candidate = "17b04c92")
  ),
  waivers = list(),
  change_log = list(
    list(date = "2026-09-22", change = "initial standard v1.0.0"),
    list(
      date = "2026-09-22",
      change = "complete run commands and rename L2 profile to evidence"
    ),
    list(
      date = "2026-09-22",
      change = "validate observed rounds and make correctness non-waivable"
    )
  )
)
