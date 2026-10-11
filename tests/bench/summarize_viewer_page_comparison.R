#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) {
  stop(
    "usage: summarize_viewer_page_comparison.R RAW_TSV OUTPUT_DIRECTORY",
    call. = FALSE
  )
}

raw_path <- normalizePath(args[[1L]], mustWork = TRUE)
output_dir <- normalizePath(args[[2L]], mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

raw <- utils::read.delim(
  raw_path,
  check.names = FALSE,
  na.strings = c("NA", ""),
  stringsAsFactors = FALSE
)

required <- c(
  "candidate",
  "round",
  "page",
  "visit",
  "budget_ms",
  "status",
  "correctness_pass",
  "primary_ready_ms",
  "settled_ms",
  "benchmark_mode",
  "performance_applicable",
  "pass"
)
missing <- setdiff(required, names(raw))
if (length(missing)) {
  stop("raw result is missing columns: ", paste(missing, collapse = ", "))
}

candidate_order <- unique(raw$candidate)
page_order <- unique(raw$page)
visit_order <- c("first", "repeat")

finite_values <- function(value, positive = FALSE) {
  value <- suppressWarnings(as.numeric(value))
  value <- value[is.finite(value)]
  if (positive) {
    value <- value[value > 0]
  }
  value
}

stat_value <- function(value, stat, positive = FALSE) {
  value <- finite_values(value, positive = positive)
  if (!length(value)) {
    return(NA_real_)
  }
  switch(
    stat,
    median = stats::median(value),
    p25 = unname(stats::quantile(value, 0.25, names = FALSE)),
    p75 = unname(stats::quantile(value, 0.75, names = FALSE)),
    min = min(value),
    max = max(value),
    stop("unknown statistic: ", stat)
  )
}

count_true <- function(value) sum(value %in% TRUE, na.rm = TRUE)
count_false <- function(value) sum(value %in% FALSE, na.rm = TRUE)

metric_specs <- list(
  dataset_load_ms = FALSE,
  primary_ready_ms = FALSE,
  settled_ms = FALSE,
  performance_ms = FALSE,
  complete_elapsed_ms = FALSE,
  click_to_request_ms = FALSE,
  server_prepare_ms = FALSE,
  server_resource_ms = FALSE,
  server_bundle_ms = FALSE,
  serialize_transfer_ms = FALSE,
  binary_decode_ms = FALSE,
  projection_fetch_ms = FALSE,
  projection_download_ms = FALSE,
  projection_decode_ms = FALSE,
  projection_subset_fetch_ms = FALSE,
  projection_subset_download_ms = FALSE,
  projection_subset_decode_ms = FALSE,
  projection_subset_materialize_ms = FALSE,
  metadata_fetch_ms = FALSE,
  renderer_initialization_ms = FALSE,
  renderer_apply_ms = FALSE,
  build_spaces_ms = FALSE,
  pre_draw_ms = FALSE,
  first_draw_ms = FALSE,
  activation_ms = FALSE,
  request_to_ready_ms = FALSE,
  click_to_ready_ms = FALSE,
  r_peak_rss_kib = TRUE,
  chrome_peak_rss_kib = TRUE,
  js_heap_used_bytes = TRUE,
  websocket_sent_payload_bytes = FALSE,
  websocket_received_at_ready_bytes = FALSE,
  websocket_post_ready_received_bytes = FALSE,
  primary_payload_count_at_ready = FALSE,
  primary_payload_bytes_at_ready = FALSE,
  aux_payload_count_at_ready = FALSE,
  aux_payload_bytes_at_ready = FALSE,
  primary_payload_count = FALSE,
  primary_payload_meter_bytes = FALSE,
  aux_payload_count = FALSE,
  aux_payload_bytes = FALSE,
  primary_payload_bytes = FALSE,
  projection_asset_bytes = FALSE,
  projection_subset_asset_bytes = FALSE,
  metadata_asset_bytes = FALSE
)
metric_specs <- metric_specs[names(metric_specs) %in% names(raw)]

groups <- unique(raw[c(
  "candidate",
  "page",
  "visit",
  "budget_ms",
  "benchmark_mode"
)])
summary_rows <- lapply(seq_len(nrow(groups)), function(index) {
  key <- groups[index, , drop = FALSE]
  rows <- raw[
    raw$candidate == key$candidate &
      raw$page == key$page &
      raw$visit == key$visit &
      raw$benchmark_mode == key$benchmark_mode,
    ,
    drop = FALSE
  ]
  ok <- rows$status == "ok"
  applicable <- ok & rows$performance_applicable %in% TRUE
  result <- data.frame(
    candidate = key$candidate,
    page = key$page,
    visit = key$visit,
    benchmark_mode = key$benchmark_mode,
    budget_ms = key$budget_ms,
    observations = nrow(rows),
    ok = sum(ok),
    skipped = sum(rows$status == "skipped"),
    errors = sum(!rows$status %in% c("ok", "skipped")),
    correctness_pass = count_true(rows$correctness_pass[ok]),
    applicable = sum(applicable),
    budget_pass = count_true(rows$pass[applicable]),
    budget_fail = count_false(rows$pass[applicable]),
    stringsAsFactors = FALSE
  )
  for (metric in names(metric_specs)) {
    positive <- isTRUE(metric_specs[[metric]])
    values <- rows[[metric]][ok]
    for (stat in c("median", "p25", "p75", "min", "max")) {
      result[[paste(metric, stat, sep = "_")]] <- stat_value(
        values,
        stat,
        positive = positive
      )
    }
  }
  primary_values <- finite_values(rows$primary_ready_ms[ok])
  result$primary_ready_ms_raw <- if (length(primary_values)) {
    paste(round(primary_values, 1L), collapse = " / ")
  } else {
    ""
  }
  result
})
page_summary <- do.call(rbind, summary_rows)
page_summary$candidate <- factor(
  page_summary$candidate,
  levels = candidate_order
)
page_summary$page <- factor(page_summary$page, levels = page_order)
page_summary$visit <- factor(page_summary$visit, levels = visit_order)
page_summary <- page_summary[
  order(
    page_summary$page,
    page_summary$visit,
    page_summary$candidate
  ),
]
page_summary[] <- lapply(page_summary, function(value) {
  if (is.factor(value)) as.character(value) else value
})

write_tsv <- function(data, path) {
  utils::write.table(
    data,
    path,
    row.names = FALSE,
    sep = "\t",
    quote = FALSE,
    na = "NA"
  )
}

summary_path <- file.path(output_dir, "page-summary.tsv")
write_tsv(page_summary, summary_path)

pair_specs <- if (length(candidate_order) >= 2L) {
  utils::combn(candidate_order, 2L, simplify = FALSE)
} else {
  list()
}

comparison_rows <- list()
comparison_index <- 0L
for (pair in pair_specs) {
  baseline <- pair[[1L]]
  candidate <- pair[[2L]]
  for (page in page_order) {
    for (visit in visit_order) {
      left <- raw[
        raw$candidate == baseline &
          raw$page == page &
          raw$visit == visit &
          raw$status == "ok" &
          raw$benchmark_mode == "timing",
        c("round", "primary_ready_ms"),
        drop = FALSE
      ]
      right <- raw[
        raw$candidate == candidate &
          raw$page == page &
          raw$visit == visit &
          raw$status == "ok" &
          raw$benchmark_mode == "timing",
        c("round", "primary_ready_ms"),
        drop = FALSE
      ]
      paired <- merge(
        left,
        right,
        by = "round",
        suffixes = c("_base", "_candidate")
      )
      if (!nrow(paired)) {
        next
      }
      base_values <- finite_values(paired$primary_ready_ms_base)
      candidate_values <- finite_values(paired$primary_ready_ms_candidate)
      delta <- paired$primary_ready_ms_candidate - paired$primary_ready_ms_base
      delta <- finite_values(delta)
      comparison_index <- comparison_index + 1L
      base_median <- if (length(base_values)) {
        stats::median(base_values)
      } else {
        NA_real_
      }
      candidate_median <- if (length(candidate_values)) {
        stats::median(candidate_values)
      } else {
        NA_real_
      }
      comparison_rows[[comparison_index]] <- data.frame(
        baseline = baseline,
        candidate = candidate,
        page = page,
        visit = visit,
        paired_rounds = nrow(paired),
        baseline_median_ms = base_median,
        candidate_median_ms = candidate_median,
        median_delta_ms = candidate_median - base_median,
        median_change_pct = if (is.finite(base_median) && base_median != 0) {
          100 * (candidate_median / base_median - 1)
        } else {
          NA_real_
        },
        paired_median_delta_ms = if (length(delta)) {
          stats::median(delta)
        } else {
          NA_real_
        },
        candidate_faster_rounds = sum(delta < 0),
        candidate_slower_rounds = sum(delta > 0),
        stringsAsFactors = FALSE
      )
    }
  }
}
comparison <- if (length(comparison_rows)) {
  do.call(rbind, comparison_rows)
} else {
  data.frame()
}
comparison_path <- file.path(output_dir, "comparison.tsv")
write_tsv(comparison, comparison_path)

format_number <- function(value, digits = 1L) {
  if (!is.finite(value)) "NA" else format(round(value, digits), nsmall = digits)
}

markdown <- c(
  "# Viewer page comparison",
  "",
  sprintf("Raw observations: `%s`", raw_path),
  "",
  sprintf(
    "Candidates: %s",
    paste(sprintf("`%s`", candidate_order), collapse = ", ")
  ),
  "",
  "Official KPI: browser click to request-bound, correctness-bound primary-ready.",
  "Timing and profiler-instrumented memory observations are never pooled.",
  "",
  "## Unified primary-ready timing",
  "",
  "| Candidate | Page | Fresh primary P50 ms | Fresh min–max ms | Repeat primary P50 ms | Settled P50 ms | Correctness | Fresh raw ms |",
  "|---|---|---:|---:|---:|---:|---:|---|"
)
timing_summary <- page_summary[
  page_summary$benchmark_mode == "timing",
  ,
  drop = FALSE
]
timing_keys <- unique(timing_summary[c("candidate", "page")])
for (index in seq_len(nrow(timing_keys))) {
  key <- timing_keys[index, ]
  first <- timing_summary[
    timing_summary$candidate == key$candidate &
      timing_summary$page == key$page &
      timing_summary$visit == "first",
    ,
    drop = FALSE
  ]
  repeat_row <- timing_summary[
    timing_summary$candidate == key$candidate &
      timing_summary$page == key$page &
      timing_summary$visit == "repeat",
    ,
    drop = FALSE
  ]
  if (!nrow(first)) {
    next
  }
  markdown <- c(
    markdown,
    sprintf(
      "| %s | %s | %s | %s–%s | %s | %s | %d/%d | %s |",
      key$candidate,
      key$page,
      format_number(first$primary_ready_ms_median),
      format_number(first$primary_ready_ms_min),
      format_number(first$primary_ready_ms_max),
      if (nrow(repeat_row)) {
        format_number(repeat_row$primary_ready_ms_median)
      } else {
        "NA"
      },
      format_number(first$settled_ms_median),
      first$correctness_pass,
      first$ok,
      first$primary_ready_ms_raw
    )
  )
}

memory_summary <- page_summary[
  page_summary$benchmark_mode == "memory" & page_summary$visit == "first",
  ,
  drop = FALSE
]
markdown <- c(
  markdown,
  "",
  "## Profiler-instrumented memory",
  "",
  "These elapsed values are diagnostic only and are excluded from timing KPIs.",
  "",
  "| Candidate | Page | Peak R RSS MiB | Peak Browser RSS MiB | JS heap MiB | Memory mode |",
  "|---|---|---:|---:|---:|---|"
)
if (nrow(memory_summary)) {
  for (index in seq_len(nrow(memory_summary))) {
    row <- memory_summary[index, ]
    markdown <- c(
      markdown,
      sprintf(
        "| %s | %s | %s | %s | %s | profiler-instrumented |",
        row$candidate,
        row$page,
        format_number(row$r_peak_rss_kib_median / 1024),
        format_number(row$chrome_peak_rss_kib_median / 1024),
        format_number(row$js_heap_used_bytes_median / 1024^2)
      )
    )
  }
} else {
  markdown <- c(
    markdown,
    "| — | — | NA | NA | NA | run memory mode separately |"
  )
}

if (nrow(comparison)) {
  markdown <- c(
    markdown,
    "",
    "## Median comparisons",
    "",
    "Positive change means the candidate is slower.",
    "",
    "| Baseline | Candidate | Page | Visit | Baseline P50 ms | Candidate P50 ms | Delta ms | Change | Faster rounds | Slower rounds |",
    "|---|---|---|---:|---:|---:|---:|---:|---:|---:|"
  )
  for (index in seq_len(nrow(comparison))) {
    row <- comparison[index, ]
    markdown <- c(
      markdown,
      sprintf(
        "| %s | %s | %s | %s | %s | %s | %s | %s%% | %d | %d |",
        row$baseline,
        row$candidate,
        row$page,
        row$visit,
        format_number(row$baseline_median_ms),
        format_number(row$candidate_median_ms),
        format_number(row$median_delta_ms),
        format_number(row$median_change_pct),
        row$candidate_faster_rounds,
        row$candidate_slower_rounds
      )
    )
  }
}

markdown_path <- file.path(output_dir, "page-summary.md")
writeLines(markdown, markdown_path, useBytes = TRUE)

cat("wrote\n")
cat(normalizePath(summary_path), "\n")
cat(normalizePath(comparison_path), "\n")
cat(normalizePath(markdown_path), "\n")
