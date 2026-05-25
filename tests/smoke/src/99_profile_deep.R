## ----------------------------------------------------------------------------
## Deep-dive profile: what specifically is slow inside the hot paths?
##
## Uses Rprof + summaryRprof for parseable call-stack reports, plus targeted
## micro-benchmarks of plausible alternatives:
##   - readRDS  vs  qs::qread (with multiple threads)
##   - per-column for-loop in buildHoverInfoForProjections
##     vs  paste()-once-per-col vs sprintf vs data.table::fwrite-style
##   - dgCMatrix row extraction: object$getExpressionRow
##     vs  Matrix::Matrix-native row indexing
##   - scaled-up cell counts (50k, 200k) using replicated meta-data
## ----------------------------------------------------------------------------

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(microbenchmark)
  library(qs)
  library(plotly)
})

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop("None of the expected files exist:\n", paste(paths, collapse = "\n"), call. = FALSE)
  }
  hit
}

CRB_MEDIUM <- first_existing(c(
  "result/10_convert_embedded/cerebro_PBMC_1002_Post_TCR_BCR.crb",
  "result/10_convert_embedded/cerebro_seurat_PBMC_1002_Post_VDJ.crb"
))
OUT_DIR    <- "result/99_profile_deep"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

d <- readRDS(CRB_MEDIUM)
meta <- d$getMetaData()
if (!"cell_barcode" %in% colnames(meta)) {
  meta$cell_barcode <- if ("barcode" %in% colnames(meta)) meta$barcode else seq_len(nrow(meta))
}

## ----- A. readRDS vs qs ------------------------------------------------------
log("A. readRDS vs qs::qread on the medium .crb")
qs_path <- file.path(tempdir(), "medium.qs")
qs::qsave(d, qs_path, nthreads = 4)
log(sprintf("   qs file size on disk: %.1f MB",
            file.size(qs_path)/1024/1024))
bm_io <- microbenchmark(
  readRDS = readRDS(CRB_MEDIUM),
  qs_1t   = qs::qread(qs_path, nthreads = 1),
  qs_4t   = qs::qread(qs_path, nthreads = 4),
  times = 3
)
print(bm_io, unit = "ms")

## ----- B. hover-info: alternative implementations ---------------------------
log("B. hover-info alternative implementations")
groups <- intersect(c("sample", "cluster", "seurat_clusters", "cell_type"), colnames(meta))
if (length(groups) == 0) groups <- colnames(meta)[sapply(meta, function(x) is.factor(x) || is.character(x))][1:2]

build_current <- function(table, groups) {
  pieces <- list(
    "<b>Cell</b>: ", table[["cell_barcode"]],
    "<br><b>Transcripts</b>: ",
    formatC(table[["nUMI"]], format = "f", big.mark = ",", digits = 0),
    "<br><b>Expressed genes</b>: ",
    formatC(table[["nGene"]], format = "f", big.mark = ",", digits = 0)
  )
  for (group in groups) {
    pieces <- c(pieces, list("<br><b>", group, "</b>: ", table[[group]]))
  }
  do.call(paste0, pieces)
}

build_sprintf <- function(table, groups) {
  ## one big sprintf -- avoid ~100 paste calls inside formatC and per-group concat
  group_block <- ""
  for (g in groups) {
    group_block <- paste0(group_block, "<br><b>", g, "</b>: ", table[[g]])
  }
  sprintf("<b>Cell</b>: %s<br><b>Transcripts</b>: %s<br><b>Expressed genes</b>: %s%s",
          table[["cell_barcode"]],
          formatC(table[["nUMI"]],  format="f", big.mark=",", digits=0),
          formatC(table[["nGene"]], format="f", big.mark=",", digits=0),
          group_block)
}

## stringi is far faster than base paste for big vectors. Try if installed.
have_stringi <- requireNamespace("stringi", quietly = TRUE)
build_stringi <- if (have_stringi) function(table, groups) {
  pieces <- list(
    "<b>Cell</b>: ", table[["cell_barcode"]],
    "<br><b>Transcripts</b>: ",
    formatC(table[["nUMI"]], format = "f", big.mark = ",", digits = 0),
    "<br><b>Expressed genes</b>: ",
    formatC(table[["nGene"]], format = "f", big.mark = ",", digits = 0)
  )
  for (group in groups) {
    pieces <- c(pieces, list("<br><b>", group, "</b>: ", table[[group]]))
  }
  do.call(stringi::stri_c, pieces)
} else NULL

log(sprintf("   ncells = %d, group cols used = %s", nrow(meta), paste(groups, collapse=",")))
bm_hover <- microbenchmark(
  current = build_current(meta, groups),
  sprintf = build_sprintf(meta, groups),
  stringi = if (have_stringi) build_stringi(meta, groups) else NULL,
  times = 5
)
print(bm_hover, unit = "ms")

## ----- C. dgCMatrix per-gene access ----------------------------------------
log("C. dgCMatrix per-gene row extraction alternatives")
gene_names <- d$getGeneNames()
gene <- gene_names[ which(toupper(gene_names) %in% c("CD3D","MS4A1","GAPDH","ACTB"))[1] ]
if (is.na(gene)) gene <- gene_names[1]
expr_mat <- d$expression          # dgCMatrix, genes x cells
log(sprintf("   gene='%s' index=%d, matrix is %s %d x %d",
            gene, which(rownames(expr_mat) == gene),
            class(expr_mat)[1], nrow(expr_mat), ncol(expr_mat)))

idx <- which(rownames(expr_mat) == gene)
bm_expr <- microbenchmark(
  cerebro_method   = d$getExpressionRow(gene),
  Matrix_row_full  = as.numeric(expr_mat[idx, ]),
  Matrix_row_keep  = expr_mat[idx, , drop = FALSE],
  times = 5
)
print(bm_expr, unit = "ms")

## How fast is access if we transpose once (cells x genes)?
log("   pre-transposed (cells x genes) column access (one-off transpose cost first):")
t_tx <- system.time({ tx <- Matrix::t(expr_mat) })
log(sprintf("   one-off Matrix::t() = %.3f s", t_tx["elapsed"]))
bm_expr_tx <- microbenchmark(
  col_dense  = as.numeric(tx[, idx]),
  col_sparse = tx[, idx, drop = FALSE],
  times = 5
)
print(bm_expr_tx, unit = "ms")

## ----- D. scaled hover-info: 50k and 200k cells -----------------------------
log("D. scaling hover-info to 50k / 200k cells (replicated meta)")
for (k in c(5, 22)) {
  big <- meta[rep(seq_len(nrow(meta)), k), ]
  bm <- microbenchmark(
    current = build_current(big, groups),
    sprintf = build_sprintf(big, groups),
    times = 3
  )
  log(sprintf("   ncells = %d", nrow(big)))
  print(bm, unit = "ms")
}

## ----- E. plot_ly + toWebGL scaling -----------------------------------------
log("E. plotly build at 50k / 200k cells")
coords <- d$getProjection(names(d$projections)[1])
expr   <- as.numeric(d$getExpressionRow(gene))
for (k in c(1, 5, 22)) {
  n <- nrow(coords) * k
  df_plot <- data.frame(
    x = rep(coords[,1], k),
    y = rep(coords[,2], k),
    color = rep(expr, k),
    hover = paste0("cell ", seq_len(n))
  )
  bm <- microbenchmark(
    build = {
      p <- plotly::plot_ly(
        df_plot, x = ~x, y = ~y, color = ~color,
        type = "scattergl", mode = "markers",
        hoverinfo = "text", text = ~hover,
        marker = list(size = 4)
      ) %>% plotly::toWebGL()
      plotly::plotly_build(p)
    },
    times = 3)
  log(sprintf("   n_points = %d, plot_ly+toWebGL+build median = %.1f ms",
              n, median(bm$time)/1e6))
}

## ----- F. Rprof flame summary on the hover-info path ------------------------
log("F. Rprof summary on hover-info (5 reps, large input)")
prof_path <- file.path(OUT_DIR, "rprof_hover.out")
Rprof(prof_path, interval = 0.005, line.profiling = TRUE)
big <- meta[rep(seq_len(nrow(meta)), 22), ]   # ~200k rows
for (i in 1:5) build_current(big, groups)
Rprof(NULL)
hover_summary <- summaryRprof(prof_path, lines = "show")
top_self <- head(hover_summary$by.self[order(-hover_summary$by.self$self.time), ], 12)
print(top_self)
saveRDS(hover_summary, file.path(OUT_DIR, "rprof_hover_summary.rds"))

cat("\n[done]\n")
