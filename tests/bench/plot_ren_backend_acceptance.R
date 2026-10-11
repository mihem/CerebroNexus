args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args) >= 1L) {
  args[[1L]]
} else {
  "inst/extdata/benchmarks/ren-backends-20261008.tsv"
}
output <- if (length(args) >= 2L) args[[2L]] else "ren-backends.png"
source("inst/extdata/benchmarks/plot-ren.R")
data <- utils::read.delim(input, check.names = FALSE)
grDevices::png(output, width = 1800, height = 600, res = 150)
tryCatch(plotRenBackendBenchmark(data), finally = grDevices::dev.off())
