#!/usr/bin/env Rscript
# 90 — BPCells export smoke test
#
# Validates that exportFromSeurat(..., expression_matrix_mode = "bpcells")
# correctly produces a lightweight .crb + sibling .bpcells/ directory, and
# that the exported data round-trips without numeric loss.
#
# Checks:
#   1. convertSeuratToCerebro with bpcells mode runs without error
#   2. Reloaded crb carries correct expression_backend tag
#   3. Basic API (dim, gene/cell names) works on the BPCells handle
#   4. Per-gene values match a reference dgCMatrix to floating-point tolerance
#
# Does NOT test: Shiny runtime attach or cross-directory portability (see 91).
# Depends on: data/tcr_bcr/seurat_PBMC_1002_Post_VDJ.qs

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(qs)
  library(Matrix)
  library(BPCells)
  library(Seurat)   # .getExpressionMatrix uses Seurat v5's Layers()
})
options(width = 100)

source("src/smoke_fixture_utils.R")
ensure_smoke_fixtures()

## Pick the smallest real seurat object in the test corpus so the test is fast.
seurat_file <- "data/tcr_bcr/seurat_PBMC_1002_Post_VDJ.qs"
stopifnot(file.exists(seurat_file))

result_dir <- "result/90_export_bpcells"
if (dir.exists(result_dir)) {
  unlink(file.path(result_dir, "*"), recursive = TRUE, force = TRUE)
} else {
  dir.create(result_dir, recursive = TRUE)
}

cat("=== step 7.2 bpcells export smoke test ===\n")
cat("seurat fixture:", seurat_file, "\n")
cat("result dir:    ", result_dir, "\n\n")

cat("[1/4] convertSeuratToCerebro(..., expression_matrix_mode = \"bpcells\")\n")
cat("  loading fixture with qs::qread ...\n")
seurat_loaded <- qs::qread(seurat_file)
experiment_nm <- "PBMC 7.2 bpcells smoke"
convertSeuratToCerebro(
  seurat_file         = seurat_loaded,
  result_dir          = result_dir,
  assay               = "RNA",
  slot                = "counts",
  experiment_name     = experiment_nm,
  organism            = "Human PBMC",
  groups              = c("celltype_merged.l1", "treatment"),
  expression_matrix_mode = "bpcells",
  verbose             = FALSE
)

## Locate the produced artefacts
## When seurat_file is an in-memory object, convertSeuratToCerebro derives the
## basename from experiment_name via gsub("[^A-Za-z0-9._-]", "_", ...).
stem <- gsub("[^A-Za-z0-9._-]", "_", experiment_nm)
crb_path <- file.path(result_dir, paste0("cerebro_", stem, ".crb"))
bpc_dir_relative <- paste0("cerebro_", stem, ".bpcells")
bpc_dir_abs      <- file.path(result_dir, bpc_dir_relative)
stopifnot(file.exists(crb_path), dir.exists(bpc_dir_abs))

cat("  crb:       ", crb_path, "(", format(file.info(crb_path)$size, big.mark = ","), "bytes )\n")
cat("  bpcells/:  ", bpc_dir_abs, "\n")
cat("  bpcells files:", length(list.files(bpc_dir_abs)), "\n\n")

cat("[2/4] reload crb and inspect expression_backend tag\n")
con <- file(crb_path, "rb"); hdr <- readBin(con, "raw", 2); close(con)
is_gzip <- length(hdr) == 2 && hdr[1] == 0x1f && hdr[2] == 0x8b
ds <- if (is_gzip) readRDS(crb_path) else qs::qread(crb_path, nthreads = 2)

be <- ds$getExpressionBackend()
cat("  type    :", be$type,     "\n")
cat("  location:", be$location, "\n")
cat("  class(expression):", paste(class(ds$expression), collapse = ","), "\n\n")
stopifnot(be$type == "bpcells",
          be$location == bpc_dir_relative,
          inherits(ds$expression, "IterableMatrix"))

cat("[3/4] crb-side API spot checks\n")
cat("  ncol :", ncol(ds$expression), "\n")
cat("  nrow :", nrow(ds$expression), "\n")
cat("  head getGeneNames():", paste(head(ds$getGeneNames(), 5), collapse = ", "), "\n")
cat("  head getCellNames():", paste(head(ds$getCellNames(), 5), collapse = ", "), "\n\n")

cat("[4/4] numeric equivalence against a reference dgCMatrix\n")
## Build the reference via the shared helper (same code path used inside the
## embedded mode). If this matches, bpcells round-trip preserved the values.
seurat_ref <- qs::qread(seurat_file)
ref <- cerebroAppLite:::.getExpressionMatrix(
  seurat = seurat_ref, assay = "RNA", slot = "counts",
  join_samples = FALSE, verbose = FALSE
)
test_genes <- head(intersect(rownames(ref), ds$getGeneNames()), 3)
stopifnot(length(test_genes) >= 1)

for (g in test_genes) {
  via_bpc <- ds$getExpressionRow(g)  # numeric vector, named by cell
  via_ref <- as.numeric(ref[g, colnames(ds$expression)])
  ok <- isTRUE(all.equal(unname(via_bpc), via_ref, tolerance = 1e-12))
  cat(sprintf("  gene = %-10s  bpc sum = %.3f  ref sum = %.3f  match = %s\n",
              g, sum(via_bpc), sum(via_ref), ok))
  stopifnot(ok)
}

cat("\nAll checks passed. bpcells export works at the exporter level.\n")
cat("NOTE: Shiny runtime attach (crb portability across machines / moved\n")
cat("      directories) lives in step 7.3 and is not exercised here.\n")
