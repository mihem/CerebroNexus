#!/usr/bin/env Rscript
# 11 — Seurat -> Cerebro conversion (bpcells mode)
#
# Same datasets as 10_convert_embedded.R, but exports with
# expression_matrix_mode = "bpcells". Each .crb is paired with a sibling
# .bpcells/ directory holding the on-disk compressed expression matrix.
#
# Output: result/11_convert_bpcells/
# Depends on: same Seurat fixtures as 10; requires BPCells package.

rm(list = ls())
source("src/smoke_fixture_utils.R")
ensure_smoke_fixtures()
smoke_convert_fixtures("bpcells")
