#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "usage: benchmark_bpcells_storage_order.R V1_CRB V2_CRB OUTPUT [REPEATS]",
    call. = FALSE
  )
}

v1_path <- normalizePath(args[[1L]], mustWork = TRUE)
v2_path <- normalizePath(args[[2L]], mustWork = TRUE)
output_path <- args[[3L]]
repeats <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
if (is.na(repeats) || repeats < 1L) {
  stop("REPEATS must be a positive integer.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(BPCells)
  library(Matrix)
})
devtools::load_all(".", quiet = TRUE)

v1 <- readCerebro(v1_path)
v2 <- readCerebro(v2_path)
stopifnot(
  identical(BPCells::storage_order(v1$expression), "col"),
  identical(BPCells::storage_order(v2$expression), "row"),
  identical(dim(v1$expression), c(27998L, 1000000L)),
  identical(dim(v1$expression), dim(v2$expression)),
  identical(dimnames(v1$expression), dimnames(v2$expression))
)

cells <- seq_len(ncol(v1$expression))
genes <- rownames(v1$expression)
queries <- list(
  "single-gene expression" = list(
    scale = "1 gene x 1,000,000 cells",
    run = function(object) {
      unname(object$getExpressionRow(genes[[1L]], cells))
    }
  ),
  "RGB expression" = list(
    scale = "3 genes x 1,000,000 cells",
    run = function(object) {
      unname(object$getExpressionMatrix(cells, genes[seq_len(3L)]))
    }
  ),
  "multi-panel expression" = list(
    scale = "9 genes x 1,000,000 cells",
    run = function(object) {
      unname(object$getExpressionMatrix(cells, genes[seq_len(9L)]))
    }
  ),
  "mean expression" = list(
    scale = "100 genes x 1,000,000 cells",
    run = function(object) {
      unname(BPCells::colMeans(object$getExpressionBlock(
        genes[seq_len(100L)],
        cells
      )))
    }
  )
)

elapsed_pair <- function(v1_work, v2_work) {
  samples <- matrix(NA_real_, nrow = repeats, ncol = 2L)
  for (index in seq_len(repeats)) {
    order <- if (index %% 2L) c(1L, 2L) else c(2L, 1L)
    for (candidate in order) {
      gc()
      work <- if (candidate == 1L) v1_work else v2_work
      samples[index, candidate] <- unname(system.time(work())[["elapsed"]]) *
        1000
    }
  }
  apply(samples, 2L, median)
}

allocated_mib <- function(work) {
  profile <- tempfile("bpcells-storage-order-alloc-")
  on.exit(unlink(profile), add = TRUE)
  gc()
  Rprofmem(profile)
  on.exit(Rprofmem(NULL), add = TRUE)
  work()
  Rprofmem(NULL)
  records <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
  sum(records[is.finite(records)]) / 1024^2
}

rows <- lapply(names(queries), function(metric) {
  query <- queries[[metric]]
  v1_work <- function() query$run(v1)
  v2_work <- function() query$run(v2)
  v1_value <- v1_work()
  v2_value <- v2_work()
  if (!isTRUE(all.equal(v1_value, v2_value, tolerance = 1e-12))) {
    stop("correctness check failed for: ", metric, call. = FALSE)
  }
  elapsed <- elapsed_pair(v1_work, v2_work)
  allocation <- c(allocated_mib(v1_work), allocated_mib(v2_work))
  message("measured ", metric)
  data.frame(
    metric = metric,
    scale = query$scale,
    v1_ms = elapsed[[1L]],
    v2_ms = elapsed[[2L]],
    time_change_pct = (elapsed[[2L]] / elapsed[[1L]] - 1) * 100,
    v1_alloc_mib = allocation[[1L]],
    v2_alloc_mib = allocation[[2L]],
    alloc_change_pct = (allocation[[2L]] / allocation[[1L]] - 1) * 100,
    check = "equal",
    check.names = FALSE
  )
})

write.table(
  do.call(rbind, rows),
  output_path,
  row.names = FALSE,
  sep = "\t",
  quote = FALSE
)
