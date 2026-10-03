#!/usr/bin/env Rscript

command_args <- commandArgs(trailingOnly = TRUE)
if (!length(command_args)) {
  stop("need a benchmark command", call. = FALSE)
}
command <- command_args[[1L]]
command_args <- command_args[-1L]
here <- Sys.getenv("BENCH_ROOT", "")
if (!nzchar(here)) {
  here <- normalizePath("tests/bench")
}
source(file.path(here, "benchmark", "core.R"))
.bench_cli_handled <- FALSE
for (cli_module in c(
  "cli_prepare.R",
  "cli_measure.R",
  "cli_report.R",
  "cli_evidence.R"
)) {
  source(file.path(here, "benchmark", cli_module), local = TRUE)
}
if (!.bench_cli_handled) {
  stop("unknown benchmark command: ", command, call. = FALSE)
}
rm(.bench_cli_handled, cli_module)
