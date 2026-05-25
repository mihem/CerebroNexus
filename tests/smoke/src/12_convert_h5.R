#!/usr/bin/env Rscript
# 12 — Seurat -> Cerebro conversion (h5 mode)
#
# Same datasets as 10_convert_embedded.R, but exports with
# expression_matrix_mode = "h5". Each .crb is paired with a sibling
# <stem>.h5 file storing the expression matrix in TENx-style CSC sparse
# layout written by HDF5Array::writeTENxMatrix().
#
# Output: result/12_convert_h5/
# Depends on: same Seurat fixtures as 10; requires HDF5Array (Bioconductor).

rm(list = ls())
source("src/smoke_fixture_utils.R")
ensure_smoke_fixtures()
smoke_convert_fixtures("h5")
