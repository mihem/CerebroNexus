#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "usage: viewer_1m_hot_paths.R BEFORE_ROOT AFTER_ROOT CRB [REPEATS]",
    call. = FALSE
  )
}

before_root <- normalizePath(args[[1L]], mustWork = TRUE)
after_root <- normalizePath(args[[2L]], mustWork = TRUE)
crb_path <- normalizePath(args[[3L]], mustWork = TRUE)
repeats <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
if (is.na(repeats) || repeats < 1L) {
  stop("REPEATS must be a positive integer.", call. = FALSE)
}

suppressPackageStartupMessages({
  library(BPCells)
  library(dplyr)
  library(glue)
  library(magrittr)
  library(Matrix)
  library(shiny)
})

sidecar <- sub("[.]crb$", ".bpcells", crb_path)
if (identical(sidecar, crb_path) || !dir.exists(sidecar)) {
  stop("CRB must have a sibling .bpcells directory.", call. = FALSE)
}

data_set <- readRDS(crb_path)
data_set$expression <- BPCells::open_matrix_dir(sidecar)
metadata <- data_set$getMetaData()
cells <- as.character(metadata$cell_barcode)
genes <- rownames(data_set$expression)
if (
  nrow(metadata) != 1000000L ||
    ncol(data_set$expression) != 1000000L ||
    length(genes) < 100L
) {
  stop("The benchmark requires the prepared 1M example CRB.", call. = FALSE)
}

source_helpers <- function(root) {
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "inst/viewer/utility_functions.R"), envir = env)
  env$getGroups <- function() c("sample", "seurat_clusters", "orig.ident")
  env
}

before <- source_helpers(before_root)
after <- source_helpers(after_root)
group_levels <- lapply(
  c("sample", "seurat_clusters", "orig.ident"),
  function(group) levels(metadata[[group]])
)
names(group_levels) <- c("sample", "seurat_clusters", "orig.ident")
after$getGroupLevels <- function(group) group_levels[[group]]
after$getMetaData <- function() metadata

elapsed_ms <- function(work) {
  work()
  median(vapply(
    seq_len(repeats),
    function(index) {
      gc()
      unname(system.time(work())[["elapsed"]]) * 1000
    },
    numeric(1)
  ))
}

allocated_mib <- function(work) {
  profile <- tempfile("viewer-large-alloc-")
  on.exit(unlink(profile), add = TRUE)
  gc()
  Rprofmem(profile)
  on.exit(Rprofmem(NULL), add = TRUE)
  work()
  Rprofmem(NULL)
  records <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
  sum(records[is.finite(records)]) / 1024^2
}

rows <- list()
record <- function(metric, scale, before_work, after_work, check) {
  before_value <- before_work()
  after_value <- after_work()
  if (!isTRUE(check(before_value, after_value))) {
    stop("correctness check failed for: ", metric, call. = FALSE)
  }
  before_ms <- elapsed_ms(before_work)
  after_ms <- elapsed_ms(after_work)
  before_alloc <- allocated_mib(before_work)
  after_alloc <- allocated_mib(after_work)
  rows[[length(rows) + 1L]] <<- data.frame(
    metric = metric,
    scale = scale,
    before_ms = before_ms,
    after_ms = after_ms,
    time_change_pct = (after_ms / before_ms - 1) * 100,
    before_alloc_mib = before_alloc,
    after_alloc_mib = after_alloc,
    alloc_change_pct = (after_alloc / before_alloc - 1) * 100,
    check = "equal",
    check.names = FALSE
  )
  message("measured ", metric)
}

legacy_projection_indices <- function(filters, percentage) {
  cells_df <- dplyr::mutate(metadata, row_id = dplyr::row_number())
  cells_df <- cells_df[
    before$cerebroGroupFilterMask(cells_df, filters),
    ,
    drop = FALSE
  ]
  cells_df <- dplyr::select(cells_df, cell_barcode, row_id)
  cells_df <- before$randomlySubsetCells(cells_df, percentage)
  cells_df <- cells_df[sample(seq_len(nrow(cells_df))), , drop = FALSE]
  cells_df$row_id
}

optimized_projection_indices <- function(filters, percentage) {
  after$input <- c(
    list(overview_projection_percentage_cells_to_show = percentage),
    stats::setNames(
      filters,
      paste0("overview_projection_group_filter_", names(filters))
    )
  )
  after$viewerProjectionCellIndices("overview_projection", metadata)
}

all_filters <- lapply(group_levels, identity)
full_before <- function() {
  set.seed(20260910L)
  legacy_projection_indices(all_filters, 100)
}
full_after <- function() {
  set.seed(20260910L)
  optimized_projection_indices(all_filters, 100)
}
record(
  "full projection selection",
  "1,000,000 cells; all groups; 100%",
  full_before,
  full_after,
  function(x, y) identical(x, y)
)

filtered_levels <- group_levels
filtered_levels$seurat_clusters <- head(
  filtered_levels$seurat_clusters,
  ceiling(length(filtered_levels$seurat_clusters) / 2)
)
filtered_before <- function() {
  set.seed(20260910L)
  legacy_projection_indices(filtered_levels, 25)
}
filtered_after <- function() {
  set.seed(20260910L)
  optimized_projection_indices(filtered_levels, 25)
}
eligible <- which(metadata$seurat_clusters %in% filtered_levels$seurat_clusters)
expected_filtered <- ceiling(length(eligible) * 0.25)
record(
  "filtered projection selection",
  paste0("1,000,000 cells; 17/33 clusters; 25% = ", expected_filtered),
  filtered_before,
  filtered_after,
  function(x, y) {
    length(x) == expected_filtered &&
      length(y) == expected_filtered &&
      all(x %in% eligible) &&
      all(y %in% eligible) &&
      !anyDuplicated(x) &&
      !anyDuplicated(y)
  }
)

single_gene <- genes[[1L]]
before_single <- function() {
  unname(as.numeric(data_set$getExpressionMatrix(
    cells = cells,
    genes = single_gene
  )))
}
after_single <- function() {
  unname(after$viewerExpressionRow(data_set, cells, single_gene))
}
record(
  "single-gene expression",
  "1 gene x 1,000,000 cells; BPCells",
  before_single,
  after_single,
  function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
)

rgb_genes <- genes[seq_len(3L)]
before_rgb <- function() {
  stats::setNames(
    lapply(rgb_genes, function(gene) {
      unname(as.numeric(data_set$getExpressionMatrix(
        cells = cells,
        genes = gene
      )))
    }),
    rgb_genes
  )
}
after_rgb <- function() after$viewerExpressionValues(data_set, cells, rgb_genes)
record(
  "RGB expression",
  "3 genes x 1,000,000 cells; 3 reads vs 1",
  before_rgb,
  after_rgb,
  function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
)

panel_genes <- genes[seq_len(9L)]
before_panels <- function() {
  expression_matrix <- data_set$getExpressionMatrix(
    cells = cells,
    genes = panel_genes
  )
  expression_matrix <- Matrix::t(expression_matrix)
  stats::setNames(
    lapply(seq_len(ncol(expression_matrix)), function(index) {
      as.vector(expression_matrix[, index])
    }),
    colnames(expression_matrix)
  )
}
after_panels <- function() {
  after$viewerExpressionValues(data_set, cells, panel_genes)
}
record(
  "multi-panel expression",
  "9 genes x 1,000,000 cells; transpose removed",
  before_panels,
  after_panels,
  function(x, y) isTRUE(all.equal(x, y, check.attributes = FALSE))
)

mean_genes <- genes[seq_len(100L)]
before_mean <- function() {
  unname(Matrix::colMeans(data_set$getExpressionMatrix(
    cells = cells,
    genes = mean_genes
  )))
}
after_mean <- function() {
  unname(BPCells::colMeans(data_set$getExpressionBlock(
    genes = mean_genes,
    cells = cells
  )))
}
record(
  "mean expression",
  "100 genes x 1,000,000 cells; dense vs backend-native",
  before_mean,
  after_mean,
  function(x, y) isTRUE(all.equal(x, y, tolerance = 1e-12))
)

output <- do.call(rbind, rows)
write.table(output, row.names = FALSE, sep = "\t", quote = FALSE)
