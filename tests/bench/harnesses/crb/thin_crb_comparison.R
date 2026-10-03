#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "usage: thin_crb_comparison.R SOURCE_CRB ROUNDS OUTPUT_DIR",
    call. = FALSE
  )
}

baseline <- normalizePath(args[[1L]], mustWork = TRUE)
rounds <- suppressWarnings(as.integer(args[[2L]]))
output_dir <- normalizePath(args[[3L]], mustWork = FALSE)
if (is.na(rounds) || rounds < 1L) {
  stop("ROUNDS must be a positive integer.", call. = FALSE)
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
script_path <- normalizePath(sub("^--file=", "", script_arg), mustWork = TRUE)
repo_root <- normalizePath(
  file.path(dirname(script_path), "..", "..", "..", ".."),
  mustWork = TRUE
)
suppressPackageStartupMessages(pkgload::load_all(repo_root, quiet = TRUE))
if (!requireNamespace("qs2", quietly = TRUE)) {
  stop("The qs2 package is required.", call. = FALSE)
}

baseline_payload <- readCerebro(baseline)
baseline_payload$crb_schema <- NULL
if (nrow(baseline_payload$meta_data) != 1000000L) {
  stop("SOURCE_CRB must contain exactly one million cells.", call. = FALSE)
}
backend <- baseline_payload$getExpressionBackend()
if (!identical(backend$type, "bpcells")) {
  stop("SOURCE_CRB must use a BPCells sidecar.", call. = FALSE)
}
sidecar <- normalizePath(
  file.path(dirname(baseline), backend$location),
  mustWork = TRUE
)
expected_cells <- colnames(baseline_payload$expression)

artifact_root <- tempfile("cerebro-thin-crb-comparison-")
dir.create(artifact_root)
on.exit(unlink(artifact_root, recursive = TRUE, force = TRUE), add = TRUE)
if (!file.symlink(sidecar, file.path(artifact_root, backend$location))) {
  stop("Could not link the shared BPCells sidecar.", call. = FALSE)
}

thin_file <- file.path(artifact_root, "thin.crb")
thin_payload <- CerebroNexus:::.thinCerebroPayload(
  baseline_payload,
  thin_file
)

thread_probe <- tempfile(fileext = ".qs2")
thread_warning <- character()
withCallingHandlers(
  qs2::qs_save(1:10, thread_probe, nthreads = 2L),
  warning = function(condition) {
    thread_warning <<- c(thread_warning, conditionMessage(condition))
    invokeRestart("muffleWarning")
  }
)
unlink(thread_probe)
multithread_supported <- !any(grepl(
  "TBB|fall back|falling back",
  thread_warning,
  ignore.case = TRUE
))

parse_integers <- function(name, default) {
  value <- Sys.getenv(name)
  if (!nzchar(value)) return(default)
  parsed <- suppressWarnings(as.integer(strsplit(value, ",", fixed = TRUE)[[1L]]))
  if (!length(parsed) || anyNA(parsed)) {
    stop(name, " must be a comma-separated integer list.", call. = FALSE)
  }
  unique(parsed)
}

# Representative Zstd levels cover fast, balanced, compact, and maximum modes.
# Set CEREBRO_QS2_LEVELS=1,2,...,22 for every positive compression level.
levels <- parse_integers(
  "CEREBRO_QS2_LEVELS",
  c(1L, 3L, 6L, 9L, 12L, 15L, 19L, 22L)
)
if (any(levels < 1L | levels > 22L)) {
  stop("CEREBRO_QS2_LEVELS must stay within 1..22.", call. = FALSE)
}
default_threads <- 1L
if (multithread_supported) {
  default_threads <- unique(c(
    1L,
    min(4L, max(1L, parallel::detectCores(logical = TRUE)))
  ))
}
threads <- parse_integers("CEREBRO_QS2_THREADS", default_threads)
if (any(threads < 1L)) {
  stop("CEREBRO_QS2_THREADS must contain positive integers.", call. = FALSE)
}
if (!multithread_supported && any(threads > 1L)) {
  stop("This qs2 build has no multithreaded TBB support.", call. = FALSE)
}

grid <- expand.grid(
  compress_level = levels,
  shuffle = c(FALSE, TRUE),
  nthreads = threads,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
grid$candidate <- sprintf(
  "thin_qs2_l%02d_s%d_t%d",
  grid$compress_level,
  as.integer(grid$shuffle),
  grid$nthreads
)
grid$codec <- "qs2"
grid$production_default <- with(
  grid,
  compress_level == 3L & shuffle & nthreads == 1L
)
fixed <- data.frame(
  compress_level = NA_integer_,
  shuffle = NA,
  nthreads = 1L,
  candidate = c("legacy_rds", "thin_rds"),
  codec = "rds",
  production_default = FALSE,
  stringsAsFactors = FALSE
)
candidates <- rbind(fixed, grid)

files <- stats::setNames(
  file.path(artifact_root, paste0(candidates$candidate, ".crb")),
  candidates$candidate
)
initial_qs2 <- c(
  compress_level = qs2::qopt("compress_level"),
  shuffle = qs2::qopt("shuffle"),
  nthreads = qs2::qopt("nthreads")
)
on.exit({
  qs2::qopt("compress_level", as.integer(initial_qs2[["compress_level"]]))
  qs2::qopt("shuffle", as.logical(initial_qs2[["shuffle"]]))
  qs2::qopt("nthreads", as.integer(initial_qs2[["nthreads"]]))
}, add = TRUE)

configure_qs2 <- function(row) {
  if (!identical(row$codec[[1L]], "qs2")) return(invisible(NULL))
  qs2::qopt("compress_level", row$compress_level[[1L]])
  qs2::qopt("shuffle", row$shuffle[[1L]])
  qs2::qopt("nthreads", row$nthreads[[1L]])
  invisible(NULL)
}

write_candidate <- function(row) {
  name <- row$candidate[[1L]]
  if (identical(name, "legacy_rds")) {
    saveRDS(baseline_payload, files[[name]])
  } else if (identical(name, "thin_rds")) {
    saveRDS(thin_payload, files[[name]])
  } else {
    configure_qs2(row)
    qs2::qs_save(
      thin_payload,
      files[[name]],
      compress_level = row$compress_level[[1L]],
      shuffle = row$shuffle[[1L]],
      nthreads = row$nthreads[[1L]]
    )
  }
}

decode_candidate <- function(row) {
  name <- row$candidate[[1L]]
  if (identical(row$codec[[1L]], "rds")) {
    readRDS(files[[name]])
  } else {
    qs2::qs_read(files[[name]], nthreads = row$nthreads[[1L]])
  }
}

check_hydrated <- function(value) {
  identical(value$getCellNames(), expected_cells) &&
    isTRUE(all.equal(value$meta_data, baseline_payload$meta_data)) &&
    isTRUE(all.equal(value$projections, baseline_payload$projections))
}

message("Preparing ", nrow(candidates), " candidates...")
for (index in seq_len(nrow(candidates))) {
  write_candidate(candidates[index, , drop = FALSE])
}
for (index in seq_len(nrow(candidates))) {
  row <- candidates[index, , drop = FALSE]
  configure_qs2(row)
  invisible(decode_candidate(row))
  value <- readCerebro(files[[row$candidate]])
  if (!check_hydrated(value)) {
    stop("Correctness warm-up failed for ", row$candidate, call. = FALSE)
  }
}

rows <- list()
candidate_count <- nrow(candidates)
for (round in seq_len(rounds)) {
  offset <- floor((round - 1L) * candidate_count / rounds)
  order <- ((seq_len(candidate_count) + offset - 1L) %% candidate_count) + 1L
  if (round %% 2L == 0L) order <- rev(order)
  for (position in seq_along(order)) {
    index <- order[[position]]
    row <- candidates[index, , drop = FALSE]
    name <- row$candidate[[1L]]
    configure_qs2(row)
    gc()
    write_ms <- unname(system.time(write_candidate(row))[["elapsed"]] * 1000)
    gc()
    decode_ms <- unname(system.time({
      decoded <- decode_candidate(row)
    })[["elapsed"]] * 1000)
    rm(decoded)
    gc()
    hydrated_ms <- unname(system.time({
      hydrated <- readCerebro(files[[name]])
    })[["elapsed"]] * 1000)
    correctness <- check_hydrated(hydrated)
    rm(hydrated)
    rows[[length(rows) + 1L]] <- data.frame(
      candidate = name,
      round = round,
      position = position,
      write_ms = write_ms,
      decode_ms = decode_ms,
      hydrated_ms = hydrated_ms,
      size_mib = unname(file.info(files[[name]])$size / 1024^2),
      correctness = correctness,
      stringsAsFactors = FALSE
    )
    message(
      sprintf(
        "round %d/%d, %d/%d: %s",
        round, rounds, position, candidate_count, name
      )
    )
  }
}
raw <- do.call(rbind, rows)

median_for <- function(values) stats::median(values, na.rm = TRUE)
summary <- do.call(rbind, lapply(seq_len(nrow(candidates)), function(index) {
  definition <- candidates[index, , drop = FALSE]
  selected <- raw[raw$candidate == definition$candidate, , drop = FALSE]
  data.frame(
    definition,
    rounds = nrow(selected),
    size_mib = median_for(selected$size_mib),
    write_median_ms = median_for(selected$write_ms),
    decode_median_ms = median_for(selected$decode_ms),
    hydrated_median_ms = median_for(selected$hydrated_ms),
    correctness = all(selected$correctness),
    stringsAsFactors = FALSE
  )
}))

baseline_row <- summary[summary$candidate == "legacy_rds", , drop = FALSE]
summary$size_reduction_pct <- 100 *
  (1 - summary$size_mib / baseline_row$size_mib)
summary$write_speedup <- baseline_row$write_median_ms / summary$write_median_ms
summary$decode_speedup <- baseline_row$decode_median_ms / summary$decode_median_ms
summary$hydrated_speedup <- baseline_row$hydrated_median_ms /
  summary$hydrated_median_ms

qs_rows <- which(summary$codec == "qs2")
summary$pareto <- FALSE
for (index in qs_rows) {
  values <- summary[index, c(
    "size_mib", "write_median_ms", "hydrated_median_ms"
  )]
  dominated <- vapply(qs_rows, function(other) {
    if (other == index) return(FALSE)
    other_values <- summary[other, names(values)]
    all(other_values <= values) && any(other_values < values)
  }, logical(1))
  summary$pareto[[index]] <- !any(dominated)
}

cpu <- Sys.info()[["machine"]]
if (file.exists("/proc/cpuinfo")) {
  cpu_lines <- grep(
    "^model name",
    readLines("/proc/cpuinfo", warn = FALSE),
    value = TRUE
  )
  if (length(cpu_lines)) cpu <- sub("^[^:]+:[[:space:]]*", "", cpu_lines[[1L]])
}
memory_mib <- NA_real_
if (file.exists("/proc/meminfo")) {
  memory_line <- grep(
    "^MemTotal:",
    readLines("/proc/meminfo", warn = FALSE),
    value = TRUE
  )
  memory_mib <- as.numeric(gsub("[^0-9]", "", memory_line[[1L]])) / 1024
}
environment <- data.frame(
  key = c(
    "generated_at", "git_sha", "r_version", "platform", "os", "cpu",
    "logical_cores", "memory_mib", "qs2_version", "multithread_supported",
    "levels", "threads", "rounds", "source_crb"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    Sys.getenv("CEREBRO_BENCH_GIT_SHA"),
    R.version.string,
    R.version$platform,
    paste(Sys.info()[c("sysname", "release", "version")], collapse = " "),
    cpu,
    parallel::detectCores(logical = TRUE),
    round(memory_mib),
    as.character(utils::packageVersion("qs2")),
    multithread_supported,
    paste(levels, collapse = "|"),
    paste(threads, collapse = "|"),
    rounds,
    baseline
  ),
  stringsAsFactors = FALSE
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
utils::write.csv(raw, file.path(output_dir, "raw.csv"), row.names = FALSE)
utils::write.csv(summary, file.path(output_dir, "summary.csv"), row.names = FALSE)
utils::write.csv(
  environment,
  file.path(output_dir, "environment.csv"),
  row.names = FALSE
)

default_row <- summary[summary$production_default, , drop = FALSE]
thin_rds_row <- summary[summary$candidate == "thin_rds", , drop = FALSE]
threaded_default <- summary[
  summary$codec == "qs2" &
    summary$compress_level == 3L &
    summary$shuffle,
  ,
  drop = FALSE
]
threaded_default <- threaded_default[
  which.max(threaded_default$nthreads),
  ,
  drop = FALSE
]
best_write <- summary[qs_rows[which.min(summary$write_median_ms[qs_rows])], , drop = FALSE]
best_read <- summary[qs_rows[which.min(summary$hydrated_median_ms[qs_rows])], , drop = FALSE]
smallest <- summary[qs_rows[which.min(summary$size_mib[qs_rows])], , drop = FALSE]
format_row <- function(label, row) {
  sprintf(
    "| %s | `%s` | %.2f | %.0f | %.0f | %.0f |",
    label,
    row$candidate,
    row$size_mib,
    row$write_median_ms,
    row$decode_median_ms,
    row$hydrated_median_ms
  )
}
report <- c(
  "# Thin CRB qs2 comparison",
  "",
  "## Contents",
  "",
  "- [Scope](#scope)",
  "- [Headline results](#headline-results)",
  "- [Findings](#findings)",
  "- [Decision rule](#decision-rule)",
  "- [Files](#files)",
  "",
  "## Scope",
  "",
  sprintf(
    paste0(
      "One reconstructed legacy-compatible one-million-cell BPCells-backed ",
      "payload; %d alternating rounds; ",
      "%d qs2 parameter combinations. All candidates reuse the same sidecar."
    ),
    rounds,
    nrow(grid)
  ),
  "",
  "## Headline results",
  "",
  "| Role | Candidate | MiB | Write ms | Decode ms | Hydrated ms |",
  "|---|---|---:|---:|---:|---:|",
  format_row("Legacy baseline", baseline_row),
  format_row("Thin payload, RDS", thin_rds_row),
  format_row("Production default", default_row),
  format_row("Recommended host setting", threaded_default),
  format_row("Fastest write", best_write),
  format_row("Fastest hydration", best_read),
  format_row("Smallest file", smallest),
  "",
  "## Findings",
  "",
  sprintf(
    paste0(
      "- Payload thinning alone reduces the control file by %.2f%% and makes ",
      "physical decoding %.2fx faster than the reconstructed legacy payload."
    ),
    thin_rds_row$size_reduction_pct,
    thin_rds_row$decode_speedup
  ),
  sprintf(
    paste0(
      "- Default qs2 adds a further %.2f%% size reduction versus thin RDS and ",
      "makes writing %.2fx faster."
    ),
    100 * (1 - default_row$size_mib / thin_rds_row$size_mib),
    thin_rds_row$write_median_ms / default_row$write_median_ms
  ),
  sprintf(
    paste0(
      "- With the same level-3 shuffled file format, %d threads write %.2fx ",
      "faster and hydrate %.2fx faster than the one-thread production default ",
      "on this host."
    ),
    threaded_default$nthreads,
    default_row$write_median_ms / threaded_default$write_median_ms,
    default_row$hydrated_median_ms / threaded_default$hydrated_median_ms
  ),
  "",
  "## Decision rule",
  "",
  paste0(
    "Keep compression level 3 with shuffle enabled as the portable format ",
    "default. Prefer the measured multi-thread setting when TBB is available; ",
    "thread count changes runtime only, not the serialized format. Extreme ",
    "compression levels save little additional space for disproportionate ",
    "write cost."
  ),
  "",
  "## Files",
  "",
  "- `raw.csv`: every timed observation and execution position.",
  "- `summary.csv`: medians, baseline deltas, correctness, and Pareto status.",
  "- `environment.csv`: hardware, software, source, and comparison parameters."
)
writeLines(report, file.path(output_dir, "summary.md"))

print(summary, row.names = FALSE, digits = 4)
cat("Results:", normalizePath(output_dir, mustWork = TRUE), "\n")
