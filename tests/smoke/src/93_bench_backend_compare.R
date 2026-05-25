#!/usr/bin/env Rscript
# 93 — Expression-backend performance comparison on the largest dataset.
#
# Compares the three expression-matrix backends (embedded / bpcells / h5) on
# the PBMC All Samples fixture — the largest .crb produced by 10/11/12.
# Each backend runs in a fresh R subprocess via callr so RSS readings are
# not contaminated by earlier loads.
#
# Metrics per backend:
#   disk_*_mb     on-disk footprint (crb + sibling)
#   load_secs     time to readRDS the .crb
#   attach_secs   time spent in .attachExternalExpression (NA for embedded)
#   rss_mb        process RSS after load + attach
#   cold_secs     first single-gene single-cell read (cold cache)
#   hot_p50_secs  median of n_reps single-gene reads, rotating a 50-gene pool
#   hot_p95_secs  95th percentile of the same
#   bulk_secs     slice 50 genes × all cells (marker_genes tab pattern)
#
# Depends on: 10_convert_embedded.R, 11_convert_bpcells.R, 12_convert_h5.R
# having produced their PBMC All Samples outputs.
#
# Output: result/93_bench_backend_compare/{summary.csv, run.log}

rm(list = ls())

suppressPackageStartupMessages({
  library(callr)
})

pkg_root <- normalizePath(file.path(dirname(getwd()), ".."))
result_dir <- "result/93_bench_backend_compare"
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(result_dir, "run.log")
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)

message(sprintf("[%s] pkg root: %s", Sys.time(), pkg_root))

backends <- list(
  embedded = list(
    crb = "result/10_convert_embedded/cerebro_PBMC_All_Samples_TCR_BCR.crb",
    sibling = NULL
  ),
  bpcells = list(
    crb = "result/11_convert_bpcells/cerebro_PBMC_All_Samples_TCR_BCR.crb",
    sibling = "result/11_convert_bpcells/cerebro_PBMC_All_Samples_TCR_BCR.bpcells"
  ),
  h5 = list(
    crb = "result/12_convert_h5/cerebro_PBMC_All_Samples_TCR_BCR.crb",
    sibling = "result/12_convert_h5/cerebro_PBMC_All_Samples_TCR_BCR.h5"
  )
)

# Returns a single-row data.frame of metrics for the requested backend.
# Executed inside callr::r() so we have a clean R process per call.
bench_one <- function(name, crb, sibling, pkg_root,
                      n_reps = 30, gene_pool_size = 50, bulk_n_genes = 50) {
  suppressPackageStartupMessages({
    devtools::load_all(pkg_root, quiet = TRUE)
  })

  # The runtime attach helper lives in the Shiny utility file, not in R/.
  source(system.file("shiny/v1.4/utility_functions.R",
                     package = "cerebroAppLite"))

  # Modern crbs (produced by 10/11/12) carry their own expression_backend
  # tag; .attachExternalExpression resolves the sibling via dirname(crb) +
  # be$location, so we don't supply any override here. Setting
  # Cerebro.options[["expression_matrix_h5"]] would force a literal path
  # that fails to resolve when given a basename.
  assign("Cerebro.options", list(), envir = .GlobalEnv)

  # Disk footprint
  disk_crb_mb <- file.info(crb)$size / 1024^2
  disk_sibling_mb <- NA_real_
  if (!is.null(sibling) && file.exists(sibling)) {
    disk_sibling_mb <- if (dir.exists(sibling)) {
      sum(
        file.info(
          list.files(sibling, recursive = TRUE, full.names = TRUE)
        )$size,
        na.rm = TRUE
      ) / 1024^2
    } else {
      file.info(sibling)$size / 1024^2
    }
  }

  # Load
  message(sprintf("[%s] [%s] loading crb (%.0f MB)...",
                  Sys.time(), name, disk_crb_mb))
  t0 <- Sys.time()
  con <- file(crb, "rb"); hdr <- readBin(con, "raw", n = 2); close(con)
  is_gzip <- length(hdr) == 2 && hdr[1] == 0x1f && hdr[2] == 0x8b
  obj <- if (is_gzip) {
    readRDS(crb)
  } else {
    if (!requireNamespace("qs", quietly = TRUE)) {
      stop("crb is not gzip-RDS and qs is not available", call. = FALSE)
    }
    qs::qread(crb, nthreads = 2)
  }
  load_secs <- as.numeric(Sys.time() - t0, units = "secs")

  # Attach external backend (no-op for embedded)
  message(sprintf("[%s] [%s] attaching external backend...", Sys.time(), name))
  t0 <- Sys.time()
  obj <- .attachExternalExpression(obj, crb)
  attach_secs <- as.numeric(Sys.time() - t0, units = "secs")

  # Process RSS (post load + attach)
  rss_kb <- tryCatch(
    as.numeric(system2(
      "ps", c("-o", "rss=", "-p", Sys.getpid()),
      stdout = TRUE
    )),
    error = function(e) NA_real_
  )
  rss_mb <- rss_kb / 1024

  all_genes <- rownames(obj$expression)
  all_cells <- colnames(obj$expression)
  n_genes <- length(all_genes)
  n_cells <- length(all_cells)

  set.seed(42)
  gene_pool <- sample(all_genes, min(gene_pool_size, n_genes))

  # Use the class's backend-aware accessors (getExpressionRow,
  # getExpressionBlock) rather than raw `obj$expression[g, cells]`, so the
  # comparison reflects what the Shiny app actually runs and works uniformly
  # across dgCMatrix, IterableMatrix, and DelayedArray.

  # Cold read: one single-gene full-row access (first touch of expression)
  message(sprintf("[%s] [%s] cold single-gene read...", Sys.time(), name))
  t0 <- Sys.time()
  v <- obj$getExpressionRow(gene_pool[1], cells = all_cells)
  cold_secs <- as.numeric(Sys.time() - t0, units = "secs")

  # Hot reads: rotate through gene pool to defeat per-row cache effects
  message(sprintf("[%s] [%s] hot single-gene reads (%d reps)...",
                  Sys.time(), name, n_reps))
  t_hot <- numeric(n_reps)
  for (i in seq_len(n_reps)) {
    g <- gene_pool[((i - 1) %% length(gene_pool)) + 1]
    t_start <- Sys.time()
    v <- obj$getExpressionRow(g, cells = all_cells)
    t_hot[i] <- as.numeric(Sys.time() - t_start, units = "secs")
  }
  hot_p50_secs <- median(t_hot)
  hot_p95_secs <- as.numeric(quantile(t_hot, 0.95, names = FALSE))

  # Bulk read: 50 genes × all cells densified (marker_genes tab pattern)
  message(sprintf("[%s] [%s] bulk slice (%d genes × %d cells)...",
                  Sys.time(), name, bulk_n_genes, n_cells))
  bulk_genes <- gene_pool[seq_len(min(bulk_n_genes, length(gene_pool)))]
  t0 <- Sys.time()
  m <- as.matrix(obj$getExpressionBlock(bulk_genes, cells = all_cells))
  bulk_secs <- as.numeric(Sys.time() - t0, units = "secs")

  data.frame(
    backend         = name,
    n_genes         = n_genes,
    n_cells         = n_cells,
    disk_crb_mb     = round(disk_crb_mb, 1),
    disk_sibling_mb = round(disk_sibling_mb, 1),
    disk_total_mb   = round(disk_crb_mb + ifelse(is.na(disk_sibling_mb), 0, disk_sibling_mb), 1),
    load_secs       = round(load_secs, 3),
    attach_secs     = round(attach_secs, 3),
    rss_mb          = round(rss_mb, 0),
    cold_secs       = round(cold_secs, 4),
    hot_p50_secs    = round(hot_p50_secs, 4),
    hot_p95_secs    = round(hot_p95_secs, 4),
    bulk_secs       = round(bulk_secs, 3),
    stringsAsFactors = FALSE
  )
}

results <- list()
for (b in names(backends)) {
  cfg <- backends[[b]]
  if (!file.exists(cfg$crb)) {
    message(sprintf("[%s] [%s] crb missing, skipping: %s",
                    Sys.time(), b, cfg$crb))
    next
  }
  if (!is.null(cfg$sibling) && !file.exists(cfg$sibling)) {
    message(sprintf("[%s] [%s] sibling missing, skipping: %s",
                    Sys.time(), b, cfg$sibling))
    next
  }
  message(sprintf("[%s] [%s] spawning fresh R subprocess...", Sys.time(), b))
  t0 <- Sys.time()
  results[[b]] <- callr::r(
    bench_one,
    args = list(
      name = b,
      crb = normalizePath(cfg$crb),
      sibling = if (is.null(cfg$sibling)) NULL else normalizePath(cfg$sibling),
      pkg_root = pkg_root
    ),
    spinner = FALSE,
    show = TRUE
  )
  wall <- as.numeric(Sys.time() - t0, units = "secs")
  message(sprintf("[%s] [%s] subprocess wall: %.1fs", Sys.time(), b, wall))
}

df <- do.call(rbind, results)
rownames(df) <- NULL
csv_path <- file.path(result_dir, "summary.csv")
write.csv(df, csv_path, row.names = FALSE)

message(sprintf("[%s] wrote %s", Sys.time(), csv_path))
message("\n=== summary ===")
print(df, row.names = FALSE)

sink()
close(log_con)
