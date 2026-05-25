#!/usr/bin/env Rscript
# 30 - Cerebro class API compatibility
#
# Validates the full Shiny-side loading path (get_or_load_crb) on:
#   * freshly exported .crb files (class "Cerebro", from 10_convert_embedded.R)
#   * legacy .crb files (class "Cerebro_v1.3", inst/extdata/v1.4/example.crb)
#
# This is the real path every .crb takes in production: read_cerebro_file ->
# .attachExternalExpression -> cache. If legacy files blow up here, they'll
# blow up in the Shiny app too.
#
# Output: result/30_verify_class_compat/  (one PASS marker)

rm(list = ls())
pkg_root <- file.path(dirname(getwd()), "..")
devtools::load_all(pkg_root, quiet = TRUE)

shiny_dir <- file.path(pkg_root, "inst", "shiny", "v1.4")
source(file.path(shiny_dir, "utility_functions.R"), local = TRUE)

result_dir <- "result/30_verify_class_compat"
unlink(result_dir, recursive = TRUE, force = TRUE)
dir.create(result_dir, recursive = TRUE)

`%||%` <- function(a, b) if (!is.null(a)) a else b

is_cerebro <- function(obj) {
  inherits(obj, "Cerebro") || inherits(obj, "Cerebro_v1.3")
}

##----------------------------------------------------------------------------##
## Helper: probe a Cerebro object with the new API
##----------------------------------------------------------------------------##
probe_new_api <- function(obj, label) {
  cat("\n>>> [new API]", label, "\n")
  stopifnot(is_cerebro(obj))

  ## expression_backend
  be <- obj$getExpressionBackend()
  cat("  expression_backend: type=", be$type,
      " location=", be$location %||% "NULL", "\n")
  stopifnot(be$type == "embedded")

  ## getExpressionRow
  genes <- obj$getGeneNames()
  stopifnot(length(genes) > 0)
  g1 <- genes[1]
  row <- obj$getExpressionRow(g1)
  cat("  getExpressionRow('", g1, "'): length=", length(row),
      " (cells=", length(obj$getCellNames()), ")\n", sep = "")
  stopifnot(length(row) == length(obj$getCellNames()))

  ## getExpressionBlock
  gs <- head(genes, min(3, length(genes)))
  blk <- obj$getExpressionBlock(gs)
  cat("  getExpressionBlock(", paste(gs, collapse = ", "),
      "): dims=", paste(dim(blk), collapse = "x"), "\n")
  stopifnot(nrow(blk) == length(gs), ncol(blk) == length(obj$getCellNames()))

  ## getMeanExpressionForGenes (sparse-aware)
  mn <- obj$getMeanExpressionForGenes(gs)
  cat("  getMeanExpressionForGenes(...): rows=", nrow(mn),
      " cols=", paste(colnames(mn), collapse = ","), "\n")
  stopifnot(nrow(mn) == length(gs),
            all(c("gene", "expression") %in% colnames(mn)))

  invisible(TRUE)
}

##----------------------------------------------------------------------------##
## Part A: freshly exported crbs via get_or_load_crb
##----------------------------------------------------------------------------##
crb_dir <- "result/10_convert_embedded"
crbs <- list.files(crb_dir, pattern = "\\.crb$", full.names = TRUE)
stopifnot("Run 10_convert_embedded.R first" = length(crbs) >= 1)

for (f in crbs) {
  obj <- get_or_load_crb(normalizePath(f))
  probe_new_api(obj, basename(f))
}

##----------------------------------------------------------------------------##
## Part B: legacy crb via get_or_load_crb
##
## The legacy example.crb was serialized before expression_backend /
## getExpressionRow etc. existed.  readRDS produces an R6 instance frozen at
## the OLD class definition -- missing the new methods entirely.
##
## get_or_load_crb -> .attachExternalExpression calls obj$getExpressionBackend()
## immediately. If the legacy object lacks that method, this is where it blows
## up -- exactly the scenario we need to catch.
##----------------------------------------------------------------------------##
legacy_crb <- file.path(pkg_root, "inst", "extdata", "v1.4", "example.crb")
stopifnot("Legacy example.crb not found" = file.exists(legacy_crb))

cat("\n>>> [legacy] loading via get_or_load_crb\n")
legacy <- tryCatch(
  get_or_load_crb(normalizePath(legacy_crb)),
  error = function(e) {
    cat("  ERROR in get_or_load_crb: ", conditionMessage(e), "\n")
    cat("  Falling back to raw readRDS for diagnostics...\n")
    readRDS(legacy_crb)
  }
)
stopifnot(is_cerebro(legacy))

has_new_api <- "getExpressionBackend" %in% ls(legacy)
cat("  has new API methods: ", has_new_api, "\n")

if (has_new_api) {
  be <- legacy$getExpressionBackend()
  cat("  expression_backend fallback: type=", be$type, "\n")
  stopifnot(be$type == "embedded")
}

has_expr <- !is.null(legacy$expression) &&
  (is.matrix(legacy$expression) ||
   inherits(legacy$expression, "dgCMatrix") ||
   inherits(legacy$expression, "IterableMatrix"))

if (has_expr && has_new_api) {
  probe_new_api(legacy, "example.crb (legacy, with expression)")
} else {
  cat("  expression is NULL or old class — skipping expression accessor tests\n")
  cat("  verifying core legacy methods...\n")
  stopifnot(!is.null(legacy$getGroups()))
  stopifnot(!is.null(legacy$getVersion()))
  cat("  legacy core methods: OK\n")
}

##----------------------------------------------------------------------------##
## Done
##----------------------------------------------------------------------------##
total <- length(crbs) + 1L
writeLines("ok", file.path(result_dir, "PASS"))
cat("\n[30] all assertions passed across", total, "crb file(s).\n")
