#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]]
script_path <- normalizePath(
  sub("^--file=", "", script_arg),
  mustWork = TRUE
)
repo_root <- normalizePath(
  file.path(dirname(script_path), "..", "..", "..", ".."),
  mustWork = TRUE
)
if (length(args) < 3L) {
  stop(
    "usage: bpcells_order.R CELL_MAJOR_CRB GENE_MAJOR_CRB OUTPUT [REPEATS]",
    call. = FALSE
  )
}

cell_major_path <- normalizePath(args[[1L]], mustWork = TRUE)
gene_major_path <- normalizePath(args[[2L]], mustWork = TRUE)
output_path <- args[[3L]]
repeats <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
if (is.na(repeats) || repeats < 1L) {
  stop("REPEATS must be a positive integer.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(BPCells)
  library(Matrix)
})
devtools::load_all(repo_root, quiet = TRUE)

cell_major <- readCerebro(cell_major_path)
gene_major <- readCerebro(gene_major_path)
stopifnot(
  identical(BPCells::storage_order(cell_major$expression), "col"),
  identical(BPCells::storage_order(gene_major$expression), "row"),
  identical(dim(cell_major$expression), c(27998L, 1000000L)),
  identical(dim(cell_major$expression), dim(gene_major$expression)),
  identical(dimnames(cell_major$expression), dimnames(gene_major$expression))
)

cells <- seq_len(ncol(cell_major$expression))
genes <- rownames(cell_major$expression)
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

elapsed_pair <- function(cell_major_work, gene_major_work) {
  samples <- matrix(NA_real_, nrow = repeats, ncol = 2L)
  for (index in seq_len(repeats)) {
    order <- if (index %% 2L) c(1L, 2L) else c(2L, 1L)
    for (candidate in order) {
      gc()
      work <- if (candidate == 1L) cell_major_work else gene_major_work
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
  cell_major_work <- function() query$run(cell_major)
  gene_major_work <- function() query$run(gene_major)
  cell_major_value <- cell_major_work()
  gene_major_value <- gene_major_work()
  if (!isTRUE(all.equal(
    cell_major_value,
    gene_major_value,
    tolerance = 1e-12
  ))) {
    stop("correctness check failed for: ", metric, call. = FALSE)
  }
  elapsed <- elapsed_pair(cell_major_work, gene_major_work)
  allocation <- c(
    allocated_mib(cell_major_work),
    allocated_mib(gene_major_work)
  )
  message("measured ", metric)
  data.frame(
    metric = metric,
    scale = query$scale,
    cell_major_ms = elapsed[[1L]],
    gene_major_ms = elapsed[[2L]],
    time_change_pct = (elapsed[[2L]] / elapsed[[1L]] - 1) * 100,
    cell_major_alloc_mib = allocation[[1L]],
    gene_major_alloc_mib = allocation[[2L]],
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
