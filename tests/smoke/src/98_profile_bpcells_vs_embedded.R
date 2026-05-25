## ----------------------------------------------------------------------------
## Compare embedded dgCMatrix vs BPCells on-disk backend.
##
## Both crbs were exported from the SAME source object
## (seurat_PBMC_1002_Post_VDJ, 9 287 cells x 38 606 genes), differing only in
## expression_matrix_mode = "embedded" | "bpcells".
##
## Targets the four operations that show up in the Shiny hot path:
##   1. crb load (cold, fresh R process implied per run)
##   2. resident memory after load
##   3. single-gene vector extraction (typed gene name → numeric vector)
##   4. multi-gene block extraction (gene-set / panel of 10)
##   5. memory-efficient row aggregation (Matrix::rowMeans on a 200-gene block)
## ----------------------------------------------------------------------------

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(microbenchmark)
  library(BPCells)
  library(Matrix)
})

OUT_DIR <- "result/98_profile_bpcells_vs_embedded"
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit)) {
    stop("None of the expected files exist:\n", paste(paths, collapse = "\n"), call. = FALSE)
  }
  hit
}

CRB_EMBEDDED <- first_existing(c(
  "result/10_convert_embedded/cerebro_PBMC_1002_Post_TCR_BCR.crb",
  "result/10_convert_embedded/cerebro_seurat_PBMC_1002_Post_VDJ.crb"
))
CRB_BPCELLS  <- "result/90_export_bpcells/cerebro_PBMC_7.2_bpcells_smoke.crb"
DIR_BPCELLS  <- "result/90_export_bpcells/cerebro_PBMC_7.2_bpcells_smoke.bpcells"

log <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n", sep = "")

## ---------- helper: attach BPCells backend like the Shiny app does -----------
## The crb stores an `expression_backend$location` tag pointing at a sibling
## .bpcells/ directory; .attachExternalExpression (defined inside
## inst/shiny/v1.4/utility_functions.R, not exported) resolves it to a real
## handle. We source that file into a private env so we get the exact
## production path.
.shiny_env <- new.env(parent = globalenv())
sys.source(system.file("shiny/v1.4/utility_functions.R", package = "cerebroAppLite"),
           envir = .shiny_env)
attach_bpcells <- function(d, crb_path) {
  .shiny_env$.attachExternalExpression(d, crb_path)
}

mem_now <- function() {
  invisible(gc(reset = TRUE))
  m <- gc()
  ## 'used' rows: 1 = Ncells, 2 = Vcells (vector mem); column 'Mb' for Vcells.
  vmb <- sum(m[, "(Mb)"])
  list(total_Mb = vmb, gc_table = m)
}

## ---------- 1. crb load ------------------------------------------------------
log("1. crb load (cold)")
mem0 <- mem_now()$total_Mb
log(sprintf("   baseline R Vmem after package load: %.1f Mb", mem0))

bm_load <- microbenchmark(
  embedded = {
    rm(d_emb); invisible(gc())
    d_emb <<- readRDS(CRB_EMBEDDED)
  },
  bpcells = {
    rm(d_bpc); invisible(gc())
    d_bpc <<- readRDS(CRB_BPCELLS)
    d_bpc <<- attach_bpcells(d_bpc, CRB_BPCELLS)
  },
  times = 3, setup = { d_emb <- NULL; d_bpc <- NULL }
)
print(bm_load, unit = "ms")

## final attach for downstream tests
d_emb <- readRDS(CRB_EMBEDDED)
d_bpc <- attach_bpcells(readRDS(CRB_BPCELLS), CRB_BPCELLS)

## ---------- 2. resident memory ----------------------------------------------
log("2. resident R memory after each backend")
m_emb <- mem_now()$total_Mb
log(sprintf("   embedded loaded     : %.1f Mb (full dgCMatrix in RAM)", m_emb))

## drop embedded, measure bpcells alone
rm(d_emb); invisible(gc())
m_bpc <- mem_now()$total_Mb
log(sprintf("   bpcells loaded only : %.1f Mb (handle is just a metadata stub)", m_bpc))

## reload embedded for downstream comparisons
d_emb <- readRDS(CRB_EMBEDDED)

## report the matrix-only object size
cat(sprintf("   in-RAM matrix  embedded dgCMatrix : %.1f Mb (object.size)\n",
            as.numeric(object.size(d_emb$expression))/1024/1024))
cat(sprintf("   in-RAM matrix  BPCells handle     : %.3f Mb (object.size)\n",
            as.numeric(object.size(d_bpc$expression))/1024/1024))

## ---------- 3. single-gene vector extraction --------------------------------
log("3. per-gene extraction")
gene_names <- d_emb$getGeneNames()
test_genes <- intersect(c("CD3D", "MS4A1", "GAPDH", "ACTB", "CD8A", "FCGR3A",
                          "GNLY", "PPBP", "FCER1A", "MS4A7"), gene_names)
log(sprintf("   testing %d genes: %s", length(test_genes),
            paste(test_genes, collapse = ", ")))

## warm-up (BPCells will hit the OS page cache after the first call)
invisible(d_emb$getExpressionRow(test_genes[1]))
invisible(d_bpc$getExpressionRow(test_genes[1]))

## benchmark on a rotating set so the same gene is not always hot
i <- 0
bm_gene <- microbenchmark(
  embedded = { i <<- i + 1; d_emb$getExpressionRow(test_genes[(i %% length(test_genes)) + 1]) },
  bpcells  = { i <<- i + 1; d_bpc$getExpressionRow(test_genes[(i %% length(test_genes)) + 1]) },
  times = 30
)
print(bm_gene, unit = "ms")

## verify numeric equivalence to be safe
ref <- as.numeric(d_emb$expression[test_genes[1], ])
chk <- as.numeric(d_bpc$getExpressionRow(test_genes[1]))
log(sprintf("   numeric match on '%s': %s",
            test_genes[1], isTRUE(all.equal(ref, chk, tolerance = 1e-12))))

## ---------- 4. multi-gene block extraction ----------------------------------
log("4. block extraction (10 genes)")
bm_block <- microbenchmark(
  embedded = d_emb$getExpressionBlock(genes = test_genes),
  bpcells  = as.matrix(d_bpc$getExpressionBlock(genes = test_genes)),  # force read
  times = 10
)
print(bm_block, unit = "ms")

## ---------- 5. row aggregation (a "gene-set / signature score" workload) ----
log("5. mean-expression over a 200-gene panel")
panel <- sample(gene_names, 200)
bm_panel <- microbenchmark(
  embedded_block_then_mean = {
    blk <- d_emb$getExpressionBlock(genes = panel)
    Matrix::colMeans(blk)
  },
  bpcells_block_then_mean = {
    blk <- d_bpc$getExpressionBlock(genes = panel)
    ## IterableMatrix has its own colMeans avoiding densify
    BPCells::colMeans(blk)
  },
  times = 5
)
print(bm_panel, unit = "ms")

## ---------- 6. disk-footprint summary ---------------------------------------
log("6. disk footprint")
emb_size <- file.size(CRB_EMBEDDED) / 1024 / 1024
bpc_crb  <- file.size(CRB_BPCELLS)  / 1024 / 1024
bpc_dir  <- sum(file.info(list.files(DIR_BPCELLS, full.names = TRUE))$size) / 1024 / 1024
cat(sprintf("   embedded .crb        : %.1f MB (everything inside)\n", emb_size))
cat(sprintf("   bpcells  .crb stub   : %.1f MB\n", bpc_crb))
cat(sprintf("   bpcells  .bpcells/   : %.1f MB\n", bpc_dir))
cat(sprintf("   bpcells  total       : %.1f MB\n", bpc_crb + bpc_dir))

## ---------- save raw results ------------------------------------------------
saveRDS(list(load = bm_load, gene = bm_gene, block = bm_block, panel = bm_panel,
             mem_after_embedded = m_emb, mem_after_bpcells_only = m_bpc,
             matrix_size_embedded_mb = as.numeric(object.size(d_emb$expression))/1024/1024,
             matrix_size_bpcells_mb  = as.numeric(object.size(d_bpc$expression))/1024/1024,
             disk = list(embedded_mb = emb_size, bpcells_crb_mb = bpc_crb,
                         bpcells_dir_mb = bpc_dir)),
        file.path(OUT_DIR, "bpcells_vs_embedded.rds"))

cat("\n[done] saved to", file.path(OUT_DIR, "bpcells_vs_embedded.rds"), "\n")
