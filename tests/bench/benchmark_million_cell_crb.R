#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("tests/bench/prepare_viewer_1m_data.R")

baseline <- if (length(args)) {
  normalizePath(args[[1L]], mustWork = TRUE)
} else {
  prepareViewer1mBenchmarkData()
}
rounds <- if (length(args) >= 2L) as.integer(args[[2L]]) else 5L
output <- if (length(args) >= 3L) args[[3L]] else tempfile(fileext = ".csv")
if (is.na(rounds) || rounds < 1L) {
  stop("ROUNDS must be a positive integer.", call. = FALSE)
}

suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
baseline_payload <- readRDS(baseline)
if (
  nrow(baseline_payload$meta_data) != 1000000L ||
    !is.null(baseline_payload$crb_schema)
) {
  stop(
    "The baseline must be the legacy 1M RDS CRB produced at PR #165.",
    call. = FALSE
  )
}
backend <- baseline_payload$getExpressionBackend()
if (!identical(backend$type, "bpcells")) {
  stop("The PR #165 baseline must use BPCells.", call. = FALSE)
}
sidecar <- normalizePath(
  file.path(dirname(baseline), backend$location),
  mustWork = TRUE
)
expected_cells <- colnames(baseline_payload$expression)

root <- tempfile("cerebro-1m-crb-benchmark-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
if (!file.symlink(sidecar, file.path(root, backend$location))) {
  stop("Could not link the shared 1M BPCells sidecar.", call. = FALSE)
}
files <- c(
  pr165 = file.path(root, "pr165.crb"),
  thin_rds = file.path(root, "thin-rds.crb"),
  thin_qs2 = file.path(root, "thin-qs2.crb")
)
writers <- list(
  pr165 = function() saveRDS(baseline_payload, files[["pr165"]]),
  thin_rds = function() {
    saveCerebro(baseline_payload, files[["thin_rds"]], codec = "rds")
  },
  thin_qs2 = function() {
    saveCerebro(baseline_payload, files[["thin_qs2"]], codec = "qs2")
  }
)
writers[["pr165"]]()
saveCerebro(baseline_payload, files[["thin_rds"]], codec = "rds")
saveCerebro(baseline_payload, files[["thin_qs2"]], codec = "qs2")

decode <- list(
  pr165 = function() readRDS(files[["pr165"]]),
  thin_rds = function() readRDS(files[["thin_rds"]]),
  thin_qs2 = function() qs2::qs_read(files[["thin_qs2"]])
)
invisible(lapply(names(files), function(name) decode[[name]]()))
invisible(lapply(files, readCerebro))

rows <- list()
for (round in seq_len(rounds)) {
  order <- if (round %% 2L) names(files) else rev(names(files))
  for (candidate in order) {
    gc()
    write_ms <- unname(
      system.time(writers[[candidate]]())[["elapsed"]] *
        1000
    )
    gc()
    decode_ms <- unname(
      system.time({
        value <- decode[[candidate]]()
      })[["elapsed"]] *
        1000
    )
    rm(value)
    gc()
    hydrated_ms <- unname(
      system.time({
        value <- readCerebro(files[[candidate]])
      })[["elapsed"]] *
        1000
    )
    stopifnot(identical(value$getCellNames(), expected_cells))
    rm(value)
    rows[[length(rows) + 1L]] <- data.frame(
      candidate = candidate,
      round = round,
      write_ms = write_ms,
      decode_ms = decode_ms,
      hydrated_ms = hydrated_ms
    )
  }
}

timings <- do.call(rbind, rows)
summary <- do.call(
  rbind,
  lapply(names(files), function(candidate) {
    selected <- timings[timings$candidate == candidate, , drop = FALSE]
    data.frame(
      candidate = candidate,
      size_mib = unname(file.info(files[[candidate]])$size / 1024^2),
      write_median_ms = median(selected$write_ms),
      decode_median_ms = median(selected$decode_ms),
      hydrated_median_ms = median(selected$hydrated_ms)
    )
  })
)
baseline_summary <- summary[summary$candidate == "pr165", , drop = FALSE]
summary$size_reduction_pct <- 100 *
  (1 - summary$size_mib / baseline_summary$size_mib)
summary$write_speedup <- baseline_summary$write_median_ms /
  summary$write_median_ms
summary$decode_speedup <- baseline_summary$decode_median_ms /
  summary$decode_median_ms
summary$hydrated_speedup <- baseline_summary$hydrated_median_ms /
  summary$hydrated_median_ms
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(summary, output, row.names = FALSE)
print(summary, row.names = FALSE, digits = 4)
cat("Results:", normalizePath(output, mustWork = TRUE), "\n")
