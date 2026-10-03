# Loader for the real-data benchmark modules.
bench_root <- Sys.getenv("BENCH_ROOT", "")
core_candidates <- c(
  if (nzchar(bench_root)) file.path(bench_root, "benchmark", "core.R") else "",
  file.path("tests", "bench", "benchmark", "core.R"),
  file.path("..", "bench", "benchmark", "core.R"),
  file.path("benchmark", "core.R")
)
core_candidates <- core_candidates[
  nzchar(core_candidates) & file.exists(core_candidates)
]
if (!length(core_candidates)) {
  stop("cannot locate benchmark/core.R", call. = FALSE)
}
core_file <- normalizePath(core_candidates[[1L]], mustWork = TRUE)
core_dir <- dirname(core_file)
for (module in c(
  "sources.R",
  "protocol.R",
  "metrics.R",
  "storage_backends.R",
  "reporting.R",
  "resources.R"
)) {
  source(file.path(core_dir, module), local = TRUE)
}
rm(bench_root, core_candidates, core_file, core_dir, module)
