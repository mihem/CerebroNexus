#!/usr/bin/env Rscript
# 92 — Expression matrix access benchmark
#
# Compares four access patterns on a real .crb file (20 reps each):
#   A) zero-arg getExpressionMatrix() + [gene, cells] — original baseline
#   B) getExpressionMatrix(cells=, genes=) — sliced but still densifies
#   C) getExpressionRow(gene, cells=) — sparse, no densify
#   D) getExpressionBlock(genes, cells=) + Matrix::rowMeans — sparse block
#
# Depends on: 10_convert_embedded.R must have produced the PBMC crb at
#             result/10_convert_embedded/.

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(qs)
})

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop("None of the expected files exist:\n", paste(paths, collapse = "\n"), call. = FALSE)
  }
  hit
}

crb_path <- first_existing(c(
  "result/10_convert_embedded/cerebro_PBMC_1002_Post_TCR_BCR.crb",
  "result/10_convert_embedded/cerebro_seurat_PBMC_1002_Post_VDJ.crb"
))
n_reps <- 20
n_gene_pool <- 50  # rotate through 50 genes to avoid cache effects

message(sprintf("[%s] Loading crb: %s", Sys.time(), crb_path))
t_load0 <- Sys.time()
# crb 文件首 2 字节 0x1f 0x8b 是 gzip (RDS)，否则尝试 qs
con <- file(crb_path, "rb"); hdr <- readBin(con, "raw", n = 2); close(con)
is_gzip <- length(hdr) == 2 && hdr[1] == 0x1f && hdr[2] == 0x8b
ds <- if (is_gzip) readRDS(crb_path) else qs::qread(crb_path, nthreads = 4)
t_load1 <- Sys.time()
message(sprintf("[%s] crb load time: %.3fs", Sys.time(), as.numeric(t_load1 - t_load0, units = "secs")))

message(sprintf("[%s] Class of expression: %s", Sys.time(), paste(class(ds$expression), collapse = ",")))
message(sprintf("[%s] Dim: %d genes x %d cells", Sys.time(), nrow(ds$expression), ncol(ds$expression)))

all_genes <- rownames(ds$expression)
all_cells <- colnames(ds$expression)
gene_pool <- sample(all_genes, min(n_gene_pool, length(all_genes)))

# Pattern A: mimic projection_server.R:452
# expression_data <- getExpressionMatrix()           # zero arg -> full dense
# expr_values    <- as.vector(expression_data[gene, cells_to_extract])
t_A <- numeric(n_reps)
for (i in seq_len(n_reps)) {
  gene <- gene_pool[((i - 1) %% length(gene_pool)) + 1]
  t0 <- Sys.time()
  expression_data <- ds$getExpressionMatrix()
  expr_values <- as.vector(expression_data[gene, all_cells])
  t_A[i] <- as.numeric(Sys.time() - t0, units = "secs")
  rm(expression_data); invisible(gc(verbose = FALSE))
}

# Pattern B: TODO step 0 -- sliced call but still densifies via as.matrix
t_B <- numeric(n_reps)
for (i in seq_len(n_reps)) {
  gene <- gene_pool[((i - 1) %% length(gene_pool)) + 1]
  t0 <- Sys.time()
  expr_row <- ds$getExpressionMatrix(cells = all_cells, genes = gene)
  expr_values <- as.vector(expr_row)
  t_B[i] <- as.numeric(Sys.time() - t0, units = "secs")
}

# Pattern C: TODO step 4.1 -- new getExpressionRow, no densify
t_C <- numeric(n_reps)
for (i in seq_len(n_reps)) {
  gene <- gene_pool[((i - 1) %% length(gene_pool)) + 1]
  t0 <- Sys.time()
  expr_values <- ds$getExpressionRow(gene, cells = all_cells)
  t_C[i] <- as.numeric(Sys.time() - t0, units = "secs")
}

# Pattern D: TODO step 4.1 -- getExpressionBlock keeps sparse; Matrix::rowMeans
# stays on the sparse block. Compare against dense rowMeans that would happen
# under pattern B for a 50-gene block.
n_block <- min(50L, length(all_genes))
gene_block_pool <- replicate(
  n_reps,
  sample(all_genes, n_block),
  simplify = FALSE
)

t_B_block <- numeric(n_reps)  # dense block via getExpressionMatrix then rowMeans
for (i in seq_len(n_reps)) {
  gene_block <- gene_block_pool[[i]]
  t0 <- Sys.time()
  mat <- ds$getExpressionMatrix(cells = all_cells, genes = gene_block)  # dense
  rm_dense <- rowMeans(mat)
  t_B_block[i] <- as.numeric(Sys.time() - t0, units = "secs")
}

t_D_block <- numeric(n_reps)  # sparse block via getExpressionBlock + Matrix::rowMeans
for (i in seq_len(n_reps)) {
  gene_block <- gene_block_pool[[i]]
  t0 <- Sys.time()
  blk <- ds$getExpressionBlock(gene_block, cells = all_cells)  # sparse / lazy
  rm_sparse <- Matrix::rowMeans(blk)
  t_D_block[i] <- as.numeric(Sys.time() - t0, units = "secs")
}

summarise <- function(label, v) {
  cat(sprintf("%s:\n  mean = %.4fs  median = %.4fs  min = %.4fs  max = %.4fs\n",
              label, mean(v), median(v), min(v), max(v)))
}

cat("\n========== Benchmark Result ==========\n")
cat(sprintf("CRB: %s\n", basename(crb_path)))
cat(sprintf("Dim: %d genes x %d cells\n", nrow(ds$expression), ncol(ds$expression)))
cat(sprintf("Backend class: %s\n", paste(class(ds$expression), collapse = ",")))
cat(sprintf("Reps per pattern: %d\n\n", n_reps))

cat("--- Single-gene row extraction ---\n")
summarise("Pattern A (zero-arg getExpressionMatrix + subset)", t_A)
summarise("Pattern B (getExpressionMatrix(cells=, genes=gene))", t_B)
summarise("Pattern C (getExpressionRow(gene, cells=))", t_C)
cat(sprintf("Speedup A/B = %.1fx   A/C = %.1fx   B/C = %.2fx\n\n",
            mean(t_A) / mean(t_B), mean(t_A) / mean(t_C), mean(t_B) / mean(t_C)))

cat(sprintf("--- %d-gene block rowMeans ---\n", n_block))
summarise("Pattern B-block (getExpressionMatrix + rowMeans, dense)", t_B_block)
summarise("Pattern D-block (getExpressionBlock + Matrix::rowMeans, sparse)", t_D_block)
cat(sprintf("Speedup B-block / D-block = %.2fx\n", mean(t_B_block) / mean(t_D_block)))
cat("======================================\n")
