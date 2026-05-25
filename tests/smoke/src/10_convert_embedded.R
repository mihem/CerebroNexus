#!/usr/bin/env Rscript
# 10 — Seurat -> Cerebro conversion (embedded mode)
#
# Converts multiple Seurat objects into .crb files using the default embedded
# expression matrix storage. Covers snRNA-seq (Myeloid), spatial (Ctrl/MS
# Fibroblasts), and TCR/BCR (PBMC) datasets.
#
# Output: result/10_convert_embedded/

rm(list = ls())
source("src/smoke_fixture_utils.R")
ensure_smoke_fixtures()
smoke_convert_fixtures("embedded")
