#!/usr/bin/env Rscript
#' Recompute a monocle2 pseudotime trajectory on the B cells of the existing
#' `demo_full_tcr_bcr.crb` and inject it, so a SINGLE demo carries TCR + BCR +
#' trajectory. This replaces the former standalone `demo_trajectory.crb`
#' (an opaque binary with no build script).
#'
#' Data source: the trajectory is derived entirely from `demo_full_tcr_bcr.crb`,
#' itself built by `build_ir_demos.R` from the public 10x Genomics dataset
#' `vdj_v1_hs_pbmc3` (Human PBMC, 5' V(D)J) + `example.crb`. No new data is
#' downloaded.
#'
#' NOTE (honest scope): these are peripheral-blood B cells, not a bone-marrow
#' developmental lineage, so the trajectory is ILLUSTRATIVE of the pseudotime
#' feature, not a biological claim about B-cell ontogeny.
#'
#' Pipeline: subset B cells -> monocle2 newCellDataSet -> ordering filter on
#' high-variance genes -> DDRTree -> orderCells -> addTrajectory("monocle2",
#' "B_cell_maturation", coords+edges).

set.seed(42)

suppressMessages({
  library(CerebroNexus)
  library(monocle)
  library(Matrix)
})

crb_path <- Sys.getenv("FULL_CRB", "inst/extdata/v1.4/demo_full_tcr_bcr.crb")

message("Reading ", crb_path)
crb <- readRDS(crb_path)

md <- crb$getMetaData()
stopifnot("cell_type" %in% colnames(md))
b_idx <- which(md$cell_type == "B cells")
message("B cells: ", length(b_idx))
stopifnot(length(b_idx) > 50)

expr <- crb$getExpressionMatrix()
b_barcodes <- md$cell_barcode[b_idx]
expr_b <- expr[, b_idx, drop = FALSE]

pd <- new(
  "AnnotatedDataFrame",
  data = data.frame(
    cell_barcode = b_barcodes,
    row.names = colnames(expr_b),
    stringsAsFactors = FALSE
  )
)
fd <- new(
  "AnnotatedDataFrame",
  data = data.frame(
    gene_short_name = rownames(expr_b),
    row.names = rownames(expr_b),
    stringsAsFactors = FALSE
  )
)

# Data is already log-normalized (range ~0..8.35), so we model it with a
# Gaussian (uninormal) expression family instead of monocle2's default
# negbinomial (which expects raw counts).
cds <- newCellDataSet(
  as(as.matrix(expr_b), "sparseMatrix"),
  phenoData = pd,
  featureData = fd,
  expressionFamily = VGAM::uninormal()
)

# estimateSizeFactors()/estimateDispersions() are only meaningful for
# negbinomial/count-based families; they error out (or are no-ops) for
# uninormal, so they are intentionally skipped here.

gene_var <- apply(as.matrix(expr_b), 1, var)
ordering_genes <- names(sort(gene_var, decreasing = TRUE))[seq_len(min(
  500,
  length(gene_var)
))]
cds <- setOrderingFilter(cds, ordering_genes)

# norm_method = "none": the expression matrix is already log-normalized, so
# monocle2 must not re-log/re-normalize it before DDRTree.
cds <- reduceDimension(
  cds,
  max_components = 2,
  reduction_method = "DDRTree",
  norm_method = "none",
  pseudo_expr = 0
)

# --- Compatibility shims: monocle2 2.38.0 vs. installed igraph 2.2.1 -------
# monocle2's internal orderCells() -> extract_ddrtree_ordering() /
# project2MST() still call two igraph APIs that were later removed:
#   * graph.dfs(..., neimode = "all") -> defunct; replaced by dfs(..., mode=)
#   * nei(v, mode = "all")            -> defunct; replaced by .nei(v, mode=)
# Both replacements are exact drop-ins for the removed calls (same call-site
# semantics, igraph's own deprecation migration), so this restores monocle2's
# original algorithm rather than reimplementing any ordering logic. Patched
# via assignInNamespace() on the two affected internal functions only.
patch_extract_ddrtree_ordering <- function() {
  f <- monocle:::extract_ddrtree_ordering
  orig <- deparse(body(f))
  src <- gsub(
    "graph.dfs(dp_mst, root = root_cell, neimode = \"all\", ",
    "igraph::dfs(dp_mst, root = root_cell, mode = \"all\", ",
    orig,
    fixed = TRUE
  )
  src <- gsub("father = TRUE)", "parent = TRUE)", src, fixed = TRUE)
  # Fail loudly if the target source ever stops matching (different monocle
  # build, whitespace change) so the patch can never become a silent no-op.
  stopifnot(!identical(orig, src))
  stopifnot(!any(grepl("neimode", src, fixed = TRUE)))
  body(f) <- parse(text = src)[[1]]
  assignInNamespace("extract_ddrtree_ordering", f, ns = "monocle")
}

patch_project2MST <- function() {
  f <- monocle:::project2MST
  orig <- deparse(body(f))
  src <- gsub(
    "nei(closest_vertex_names[i], ",
    ".nei(closest_vertex_names[i], ",
    orig,
    fixed = TRUE
  )
  # Fail loudly if the substitution missed, and assert no bare `nei(` remains
  # (the perl lookbehind allows the intended `.nei(` but rejects `nei(`).
  stopifnot(!identical(orig, src))
  stopifnot(!any(grepl("(?<![.])nei\\(", src, perl = TRUE)))
  body(f) <- parse(text = src)[[1]]
  assignInNamespace("project2MST", f, ns = "monocle")
}

patch_extract_ddrtree_ordering()
patch_project2MST()

cds <- orderCells(cds)

reduced <- t(reducedDimS(cds))
meta <- data.frame(
  DR_1 = reduced[, 1],
  DR_2 = reduced[, 2],
  pseudotime = pData(cds)$Pseudotime,
  # factor (not character): the by-state color code uses levels(meta$state) to
  # build its palette; a character column makes levels() return NULL and the
  # per-state colors silently vanish.
  state = factor(as.character(pData(cds)$State)),
  row.names = colnames(expr_b),
  stringsAsFactors = FALSE
)

dp_mst <- minSpanningTree(cds)
Y <- t(reducedDimK(cds))
edges_list <- igraph::as_edgelist(dp_mst)
edges <- data.frame(
  source = edges_list[, 1],
  target = edges_list[, 2],
  weight = 1,
  source_dim_1 = Y[edges_list[, 1], 1],
  source_dim_2 = Y[edges_list[, 1], 2],
  target_dim_1 = Y[edges_list[, 2], 1],
  target_dim_2 = Y[edges_list[, 2], 2],
  stringsAsFactors = FALSE
)

## Fail loudly if a future re-run silently produces a degenerate trajectory.
stopifnot(
  nrow(meta) == length(b_idx),
  !anyNA(meta$pseudotime),
  nrow(edges) > 0,
  all(c("DR_1", "DR_2", "pseudotime", "state") %in% colnames(meta))
)

crb$addTrajectory(
  "monocle2",
  "B_cell_maturation",
  list(meta = meta, edges = edges)
)

message(
  "Trajectory methods now: ",
  paste(crb$getMethodsForTrajectories(), collapse = ", ")
)
message(
  "Trajectory names: ",
  paste(crb$getNamesOfTrajectories("monocle2"), collapse = ", ")
)

saveRDS(crb, crb_path)
message("Wrote ", crb_path)
