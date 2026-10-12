# Run from the repository root; requires Rcpp for a monotonic clock.
# Rscript tests/bench/demo-storage/benchmark-read.R /path/to/comparison
pkgload::load_all('.', quiet = TRUE)
Rcpp::cppFunction(
  'double monotonic_seconds() { return std::chrono::duration<double>(std::chrono::steady_clock::now().time_since_epoch()).count(); }',
  includes = '#include <chrono>'
)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop('Pass a directory containing baseline/ and candidate/ CRBs.')
}
root <- normalizePath(args[[1L]], mustWork = TRUE)
files <- list.files(file.path(root, 'baseline'), pattern = '[.]crb$')
rows <- list()
for (file in files) {
  for (kind in c('baseline', 'candidate')) {
    invisible(qs2::qs_read(file.path(root, kind, file)))
  }
  for (pair in 1:5) {
    kinds <- if (pair %% 2) {
      c('baseline', 'candidate')
    } else {
      c('candidate', 'baseline')
    }
    for (kind in kinds) {
      gc()
      start <- monotonic_seconds()
      x <- qs2::qs_read(file.path(root, kind, file))
      elapsed <- monotonic_seconds() - start
      rows[[length(rows) + 1L]] <- data.frame(
        file,
        pair,
        kind,
        read_ms = elapsed * 1000
      )
      rm(x)
    }
  }
}
r <- do.call(rbind, rows)
write.csv(r, file.path(root, 'read-samples.csv'), row.names = FALSE)
a <- aggregate(read_ms ~ file + kind, r, median)
write.csv(a, file.path(root, 'read-medians.csv'), row.names = FALSE)
print(a, row.names = FALSE)
