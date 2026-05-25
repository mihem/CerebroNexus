#!/usr/bin/env Rscript
# 91 — BPCells runtime attach smoke test (Shiny-side helper)
#
# Validates that .attachExternalExpression() correctly re-resolves the
# BPCells on-disk handle when a .crb is loaded from a different directory
# than where it was originally exported.
#
# Scenarios:
#   1. crb loaded from original export location
#   2. crb + .bpcells/ copied to a new directory (simulates cross-machine deploy)
#   3. Missing .bpcells/ directory -> actionable error message
#   4. Cerebro.options override for explicit path pinning
#
# Does NOT spin up Shiny — exercises the helper function directly.
# Depends on: 90_export_bpcells.R must have run first (uses its output
#             crb at result/90_export_bpcells/).

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(qs)
  library(BPCells)
})

## .attachExternalExpression lives in inst/shiny/utility_functions.R, which is
## sourced by the Shiny server at runtime rather than exported by the package.
## Source it manually for the test.
source(system.file("shiny/v1.4", "utility_functions.R", package = "cerebroAppLite"),
       local = FALSE)

cat("=== step 7.3 bpcells runtime attach smoke test ===\n\n")

orig_crb  <- "result/90_export_bpcells/cerebro_PBMC_7.2_bpcells_smoke.crb"
orig_dir  <- dirname(orig_crb)
orig_bpc  <- file.path(orig_dir, "cerebro_PBMC_7.2_bpcells_smoke.bpcells")
stopifnot(file.exists(orig_crb), dir.exists(orig_bpc))

## -----------------------------------------------------------------------------
## Scenario 1: crb loaded from its original place.
## -----------------------------------------------------------------------------
cat("[1/4] original location (crb is where the exporter wrote it)\n")
obj <- read_cerebro_file(orig_crb)
obj <- .attachExternalExpression(obj, orig_crb)
test_gene <- intersect(c("GAPDH", "ACTB", "CD3D", "MS4A1"), obj$getGeneNames())[1]
if (is.na(test_gene)) {
  test_gene <- obj$getGeneNames()[1]
}
row1 <- obj$getExpressionRow(test_gene)
stopifnot(is.numeric(row1), length(row1) == ncol(obj$expression))
cat(sprintf("  attach ok; sum(%s) = %.3f over %d cells\n\n", test_gene, sum(row1), length(row1)))

## -----------------------------------------------------------------------------
## Scenario 2: simulate "move the whole export dir to a new machine".
## Copy both crb and .bpcells/ into a fresh scratch dir, discard the original
## absolute @dir baked into the serialised handle, and resolve via the helper.
## -----------------------------------------------------------------------------
cat("[2/4] moved-together (both crb and .bpcells copied to a fresh dir)\n")
scratch <- tempfile("step73_moved_")
dir.create(scratch)
moved_crb <- file.path(scratch, basename(orig_crb))
file.copy(orig_crb, moved_crb)
file.copy(orig_bpc, scratch, recursive = TRUE)
stopifnot(file.exists(moved_crb),
          dir.exists(file.path(scratch, basename(orig_bpc))))

obj2 <- read_cerebro_file(moved_crb)
## Prove that the baked-in @dir is the original writer path (the exact scenario
## 7.3 was invented to handle):
baked_dir <- obj2$expression@dir
cat(sprintf("  handle.@dir  = %s\n", baked_dir))
cat(sprintf("  crb is at    = %s\n", moved_crb))
stopifnot(baked_dir != file.path(scratch, basename(orig_bpc)))

obj2 <- .attachExternalExpression(obj2, moved_crb)
row2 <- obj2$getExpressionRow(test_gene)
stopifnot(all.equal(unname(row1), unname(row2), tolerance = 1e-12))
cat("  attach ok; values identical to scenario 1 => portable across moves\n\n")

## -----------------------------------------------------------------------------
## Scenario 3: missing .bpcells dir -> actionable error.
## -----------------------------------------------------------------------------
cat("[3/4] missing .bpcells sibling -> actionable error\n")
broken <- tempfile("step73_broken_"); dir.create(broken)
broken_crb <- file.path(broken, basename(orig_crb))
file.copy(orig_crb, broken_crb)
obj3 <- read_cerebro_file(broken_crb)
msg <- tryCatch(.attachExternalExpression(obj3, broken_crb),
                error = function(e) conditionMessage(e))
stopifnot(grepl("directory does not exist", msg, fixed = TRUE),
          grepl(".bpcells", msg, fixed = TRUE))
cat("  error message:\n  ", msg, "\n\n", sep = "")

## -----------------------------------------------------------------------------
## Scenario 4: Cerebro.options override wins even when the sibling is present.
## Lets deployers pin the matrix to a shared / mounted location independent of
## where the crb lives.
## -----------------------------------------------------------------------------
cat("[4/4] Cerebro.options[['expression_matrix_BPCells']] override\n")
Cerebro.options <- list(
  expression_matrix_BPCells = normalizePath(orig_bpc)
)
## put it on .GlobalEnv as helper expects
assign("Cerebro.options", Cerebro.options, envir = .GlobalEnv)

obj4 <- read_cerebro_file(moved_crb)
obj4 <- .attachExternalExpression(obj4, moved_crb)
row4 <- obj4$getExpressionRow(test_gene)
stopifnot(all.equal(unname(row1), unname(row4), tolerance = 1e-12))
cat("  attach ok using override path:", get("Cerebro.options", envir = .GlobalEnv)$expression_matrix_BPCells, "\n\n")

rm("Cerebro.options", envir = .GlobalEnv)

cat("All scenarios passed. bpcells runtime attach is portable.\n")
