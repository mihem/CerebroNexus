## ----------------------------------------------------------------------------
## Headless performance profile of cerebroAppLite hot paths.
##
## Targets the exact code that runs on app start and on first plot render:
##   - package load (library(cerebroAppLite))
##   - .crb deserialise (readRDS / read_cerebro_file)
##   - meta-data accessor (data_set()$getMetaData())
##   - hover-string build (buildHoverInfoForProjections)
##   - projection coordinates extraction
##   - expression vector extraction for a single gene (the hottest UI path)
##   - plotly::plot_ly + toWebGL (the rendering path on the R side)
##
## Two crb files are profiled:
##   - small  : tests/result/.../cerebro_10.3.1_sc_Ctrl_duraFibro_both_integrated_renamed.crb (~340K)
##   - medium : tests/result/.../cerebro_seurat_PBMC_1002_Post_VDJ.crb (~41M, variable cells/genes)
## ----------------------------------------------------------------------------

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(profvis)
  library(microbenchmark)
  library(plotly)
})

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop("None of the expected files exist:\n", paste(paths, collapse = "\n"), call. = FALSE)
  }
  hit
}

CRB_SMALL  <- first_existing(c(
  "result/10_convert_embedded/cerebro_Ctrl_Dura_Mater_-_Fibroblasts_spatialseq.crb",
  "result/10_convert_embedded/cerebro_10.3.1_sc_Ctrl_duraFibro_both_integrated_renamed.crb"
))
CRB_MEDIUM <- first_existing(c(
  "result/10_convert_embedded/cerebro_PBMC_1002_Post_TCR_BCR.crb",
  "result/10_convert_embedded/cerebro_seurat_PBMC_1002_Post_VDJ.crb"
))
OUT_DIR    <- "result/97_profile_coldpath"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

results <- list()
log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

## ----- 1. package load -------------------------------------------------------
log("1. devtools::load_all (dev build)")
t_pkg <- system.time(devtools::load_all(pkg_root, quiet = TRUE))
results$package_load <- t_pkg
log(sprintf("   elapsed = %.3f s", t_pkg["elapsed"]))

## ----- 2. .crb deserialise ---------------------------------------------------
profile_crb <- function(label, path) {
  log(sprintf("2. deserialise crb (%s): %s (%.1f MB on disk)",
              label, basename(path), file.size(path)/1024/1024))
  pv <- profvis(d <- readRDS(path), interval = 0.005)
  htmlwidgets::saveWidget(pv,
    file = file.path(OUT_DIR, sprintf("profvis_load_%s.html", label)),
    selfcontained = TRUE)
  t_read <- system.time(d <- readRDS(path))
  size_mb <- round(as.numeric(utils::object.size(d)) / 1024 / 1024, 1)
  log(sprintf("   readRDS elapsed = %.3f s, in-memory size approx = %.1f MB",
              t_read["elapsed"], size_mb))
  list(time = t_read, data = d, size_mb = size_mb)
}
res_small  <- profile_crb("small",  CRB_SMALL)
res_medium <- profile_crb("medium", CRB_MEDIUM)
results$crb_small  <- res_small$time
results$crb_medium <- res_medium$time

## ----- 3. accessor + hover-info path on the medium crb ----------------------
d <- res_medium$data
log(sprintf("3. medium dataset: %d cells x %d genes, projections=%s",
            ncol(d$expression), nrow(d$expression),
            paste(names(d$projections), collapse=",")))

## getMetaData
t_meta <- microbenchmark(meta = d$getMetaData(), times = 5)
results$getMetaData <- t_meta
log(sprintf("   getMetaData() median = %.3f ms",
            median(t_meta$time)/1e6))

## projection coords - replicate what projection_server does
proj_name <- names(d$projections)[1]
t_proj <- microbenchmark(coords = d$getProjection(proj_name), times = 10)
results$getProjection <- t_proj
log(sprintf("   getProjection('%s') median = %.3f ms",
            proj_name, median(t_proj$time)/1e6))

## hover-info builder - replicate buildHoverInfoForProjections.
## The shiny utility_functions.R wraps R6 methods around a server-scoped
## `data_set()` reactive, so to profile them we need to stub `data_set`
## to a thunk that returns the loaded R6 object.
util_path <- system.file("shiny/v1.4/utility_functions.R", package = "cerebroAppLite")
local_env <- new.env(parent = globalenv())
sys.source(util_path, envir = local_env)
local_env$data_set <- function() d                         # stub the reactive
if (exists("buildHoverInfoForProjections", envir = local_env)) {
  meta <- local_env$getMetaData()
  if (!"cell_barcode" %in% colnames(meta)) {
    meta$cell_barcode <- if ("barcode" %in% colnames(meta)) meta$barcode else seq_len(nrow(meta))
  }
  pv <- profvis(
    hover <- local_env$buildHoverInfoForProjections(meta),
    interval = 0.005)
  htmlwidgets::saveWidget(pv,
    file = file.path(OUT_DIR, "profvis_hover.html"),
    selfcontained = TRUE)
  t_hover <- microbenchmark(
    hover = local_env$buildHoverInfoForProjections(meta), times = 3)
  results$hover <- t_hover
  log(sprintf("   buildHoverInfoForProjections (%d cells, %d cols) median = %.3f ms",
              nrow(meta), ncol(meta), median(t_hover$time)/1e6))
} else {
  log("   buildHoverInfoForProjections not found in utility_functions.R")
}

## ----- 4. expression-row extraction (per-gene UI path) ----------------------
gene_names <- d$getGeneNames()
gene <- gene_names[ which(toupper(gene_names) %in% c("CD3D","MS4A1","GAPDH","ACTB"))[1] ]
if (is.na(gene)) gene <- gene_names[1]
log(sprintf("4. gene-expression vector for '%s'", gene))
t_gene <- microbenchmark(
  vec = d$getExpressionRow(gene), times = 10)
results$getExpressionRow <- t_gene
log(sprintf("   getExpressionRow('%s') median = %.3f ms",
            gene, median(t_gene$time)/1e6))

## ----- 5. plotly scattergl + toWebGL path -----------------------------------
log("5. plotly::plot_ly(scattergl) + toWebGL build for the medium dataset")
coords <- d$getProjection(proj_name)
expr   <- as.numeric(d$getExpressionRow(gene))
df_plot <- data.frame(
  x = coords[,1],
  y = coords[,2],
  color = expr,
  hover = paste0("cell ", seq_along(expr))
)
pv <- profvis({
  p <- plotly::plot_ly(
    df_plot,
    x = ~x, y = ~y, color = ~color,
    type = "scattergl", mode = "markers",
    hoverinfo = "text", text = ~hover,
    marker = list(size = 4)
  ) %>% plotly::toWebGL()
  ## simulate widget JSON build (Shiny does this on render)
  json <- plotly::plotly_build(p)
}, interval = 0.005)
htmlwidgets::saveWidget(pv,
  file = file.path(OUT_DIR, "profvis_plot.html"),
  selfcontained = TRUE)

t_plot <- microbenchmark(
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
results$plotly_build <- t_plot
log(sprintf("   plot_ly + toWebGL + plotly_build median = %.3f ms",
            median(t_plot$time)/1e6))

## ----- 6. summary table ------------------------------------------------------
summary_rows <- list()
add <- function(label, ms, n = NA, note = "") {
  summary_rows[[length(summary_rows) + 1]] <<- data.frame(
    step = label,
    median_ms = round(ms, 1),
    note = note,
    stringsAsFactors = FALSE)
}
n_medium_cells <- ncol(d$expression)
n_medium_genes <- nrow(d$expression)
md_safe <- d$getMetaData()
n_medium_meta  <- nrow(md_safe)

add("devtools::load_all()",           results$package_load["elapsed"]*1000, note="dev build cold load")
add("readRDS small (.crb 0.3MB)",    results$crb_small["elapsed"]*1000)
add("readRDS medium (.crb 41MB)",    results$crb_medium["elapsed"]*1000,
    note = sprintf("dgCMatrix %dk x %dk",
                   round(n_medium_cells / 1000), round(n_medium_genes / 1000)))
add("getMetaData()",                 median(results$getMetaData$time)/1e6,
    note = sprintf("%d x %d data.frame", n_medium_meta, ncol(md_safe)))
add("getProjection()",               median(results$getProjection$time)/1e6, note="UMAP coords")
if (!is.null(results$hover))
  add("buildHoverInfoForProjections", median(results$hover$time)/1e6,
      note = sprintf("%d cells", n_medium_meta))
add("getExpressionRow(gene)",        median(results$getExpressionRow$time)/1e6, note="dgCMatrix row")
add("plotly build (scattergl + toWebGL + plotly_build)",
    median(results$plotly_build$time)/1e6,
    note = sprintf("%d points", n_medium_meta))

summary_df <- do.call(rbind, summary_rows)
write.csv(summary_df, file.path(OUT_DIR, "summary.csv"), row.names = FALSE)
saveRDS(results, file.path(OUT_DIR, "raw_results.rds"))

cat("\n================ summary ================\n")
print(summary_df, row.names = FALSE)
cat("\nprofvis html artifacts in:", OUT_DIR, "\n")
