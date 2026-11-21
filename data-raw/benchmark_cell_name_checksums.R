## Run from the repository root (reads names only; never modifies the dataset):
## nix-shell --run 'Rscript data-raw/benchmark_cell_name_checksums.R file.h5 7'
## A Viewer Pack's common/cell_order.qs2 is also accepted.
## For an isolated server check, pipe core/crb_contract.R before this script
## into Rscript - <file.h5> <repeats>, without changing the deployed app.

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop(
    "Usage: benchmark_cell_name_checksums.R <file.h5|cell_order.qs2> [repeats]"
  )
}
repeats <- if (length(args) >= 2L) as.integer(args[[2L]]) else 7L
stopifnot(length(repeats) == 1L, !is.na(repeats), repeats >= 3L)
if (!exists(".runtimeCellNamesHash", mode = "function")) {
  sys.source("inst/viewer/core/crb_contract.R", envir = environment())
}
path <- normalizePath(args[[1L]], mustWork = TRUE)
cells <- if (grepl("[.]qs2$", path)) {
  qs2::qs_read(path, nthreads = 1L)
} else {
  as.character(as.vector(rhdf5::h5read(path, "expression/genes")))
}
expected_master <- rlang::hash(enc2utf8(as.character(cells)))
expected_portable <- .runtimeCellNamesHash(cells)
schema <- list(cell_names_hash = expected_portable)
checks <- list(
  master = function() {
    identical(rlang::hash(enc2utf8(as.character(cells))), expected_master)
  },
  portable = function() {
    .runtimeCheckCellNames(cells, schema, "benchmark.crb")
    TRUE
  }
)
cat(
  R.version.string,
  "\nrlang:",
  as.character(utils::packageVersion("rlang")),
  "; digest:",
  if (requireNamespace("digest", quietly = TRUE)) {
    as.character(utils::packageVersion("digest"))
  } else {
    "not installed (file-hash fallback)"
  },
  "\nCells:",
  length(cells),
  "; encoded-name size (MiB):",
  round(sum(nchar(enc2utf8(cells), type = "bytes") + 1) / 1024^2, 2),
  "\nChecksum:",
  expected_portable,
  "\nName reads and expected-checksum preparation excluded from timings.\n"
)
results <- data.frame()
for (iteration in seq_len(repeats)) {
  order <- if (iteration %% 2L) names(checks) else rev(names(checks))
  for (method in order) {
    gc(verbose = FALSE)
    elapsed <- system.time(valid <- checks[[method]]())[["elapsed"]]
    stopifnot(isTRUE(valid))
    results <- rbind(
      results,
      data.frame(iteration = iteration, method = method, seconds = elapsed)
    )
  }
}
print(results, row.names = FALSE)
medians <- tapply(results$seconds, results$method, stats::median)
cat(
  "Median master:",
  medians[["master"]],
  "s; portable:",
  medians[["portable"]],
  "s; portable/master:",
  round(medians[["portable"]] / medians[["master"]], 3),
  "\n"
)
