# Trajectory demo — design and rebuild notes

Provenance of record (citation, licence, sampling, output) lives in [`DATASETS.md`](DATASETS.md).
This file is the working guide: what to run, what each step does to the data, and the code that does it.
Every command is meant to be copy-pasted and run from the package root.

## Contents

1. [What ships, and where it lives](#1-what-ships-and-where-it-lives)
2. [Rebuild, end to end](#2-rebuild-end-to-end)
   - [2.1 Prerequisites and run](#21-prerequisites-and-run)
   - [2.2 Step 1 — subset the B cells](#22-step-1--subset-the-b-cells)
   - [2.3 Step 2 — the monocle2 CellDataSet, and two settings that matter](#23-step-2--the-monocle2-celldataset-and-two-settings-that-matter)
   - [2.4 Step 3 — ordering, and the igraph shims](#24-step-3--ordering-and-the-igraph-shims)
   - [2.5 Step 4 — extract coordinates and tree edges](#25-step-4--extract-coordinates-and-tree-edges)
   - [2.6 Step 5 — inject and verify](#26-step-5--inject-and-verify)
3. [Honest scope](#3-honest-scope)
4. [Try it](#4-try-it)

---

# 1. What ships, and where it lives

The trajectory demo is **not a separate `.crb`**. A monocle2 pseudotime trajectory is carried **inside** the immune-repertoire demo `demo_full_tcr_bcr.crb`, computed on its B-cell subset, so a single dataset demonstrates TCR **and** BCR **and** trajectory — rather than shipping a standalone trajectory-only file (which the former opaque `demo_trajectory.crb` was, with no build script).

| Carried in | Method | Trajectory name | Cells | Stored fields |
|---|---|---|---|---|
| `demo_full_tcr_bcr.crb` | monocle2 `DDRTree` | `B_cell_maturation` | 915 B cells | `DR_1`, `DR_2`, `pseudotime`, `state` + tree edges |

Consequence worth noting: this script **overwrites its own input**. Rebuilding the IR demo ([`immune_repertoire.md`](immune_repertoire.md)) drops the trajectory, so the two builds run in order — IR first, trajectory second.

---

# 2. Rebuild, end to end

## 2.1 Prerequisites and run

Self-contained: the input is the already-built IR demo, so nothing is downloaded.

```bash
cd "$(git rev-parse --show-toplevel)"

# monocle2 is a BUILD-TIME dependency only, deliberately not a runtime one
Rscript -e 'BiocManager::install("monocle")'

Rscript data-raw/build_ir_demos.R          # must run first: it rewrites the .crb
Rscript data-raw/build_trajectory_demo.R   # then injects the trajectory slot
```

The path is overridable for a dry run:

```bash
FULL_CRB=/tmp/full.crb Rscript data-raw/build_trajectory_demo.R
```

## 2.2 Step 1 — subset the B cells

```r
crb <- readRDS(crb_path)
md  <- crb$getMetaData()
b_idx      <- which(md$cell_type == "B cells")
stopifnot(length(b_idx) > 50)              # a trajectory on a handful of cells is noise

expr   <- crb$getExpressionMatrix()
expr_b <- expr[, b_idx, drop = FALSE]
```

## 2.3 Step 2 — the monocle2 CellDataSet, and two settings that matter

```r
cds <- newCellDataSet(
  as(as.matrix(expr_b), "sparseMatrix"),
  phenoData        = pd,                   # AnnotatedDataFrame: cell_barcode
  featureData      = fd,                   # AnnotatedDataFrame: gene_short_name
  expressionFamily = VGAM::uninormal()     # <-- (1)
)

gene_var       <- apply(as.matrix(expr_b), 1, var)
ordering_genes <- names(sort(gene_var, decreasing = TRUE))[seq_len(min(500, length(gene_var)))]
cds <- setOrderingFilter(cds, ordering_genes)

cds <- reduceDimension(cds, max_components = 2, reduction_method = "DDRTree",
                       norm_method = "none", pseudo_expr = 0)   # <-- (2)
```

Both marked lines exist for the same reason — **the matrix in a `.crb` is already log-normalised** (range ~0–8.35), not raw counts:

1. `uninormal()` models it as Gaussian. monocle2's default is `negbinomial()`, which expects counts and would be modelling the wrong distribution. As a follow-on, `estimateSizeFactors()` / `estimateDispersions()` are intentionally **skipped** — they are only meaningful for count families and error out or no-op here.
2. `norm_method = "none"`, `pseudo_expr = 0` stop monocle2 re-logging data that is already logged.

## 2.4 Step 3 — ordering, and the igraph shims

monocle2 (v2.x) is unmaintained and calls two igraph functions that modern igraph has removed. `orderCells()` would simply fail. The script patches the two affected internal functions in place:

| monocle2 calls | removed | drop-in replacement |
|---|---|---|
| `graph.dfs(..., neimode = "all", father = TRUE)` | defunct | `igraph::dfs(..., mode = "all", parent = TRUE)` |
| `nei(v, mode = "all")` | defunct | `.nei(v, mode = "all")` |

```r
patch_extract_ddrtree_ordering <- function() {
  f    <- monocle:::extract_ddrtree_ordering
  orig <- deparse(body(f))
  src  <- gsub('graph.dfs(dp_mst, root = root_cell, neimode = "all", ',
               'igraph::dfs(dp_mst, root = root_cell, mode = "all", ', orig, fixed = TRUE)
  src  <- gsub("father = TRUE)", "parent = TRUE)", src, fixed = TRUE)
  # fail loudly if the target source ever stops matching, so the patch can
  # never become a silent no-op
  stopifnot(!identical(orig, src))
  stopifnot(!any(grepl("neimode", src, fixed = TRUE)))
  body(f) <- parse(text = src)[[1]]
  assignInNamespace("extract_ddrtree_ordering", f, ns = "monocle")
}
```

These are igraph's own documented migrations — same call-site semantics — so this restores monocle2's original algorithm rather than reimplementing any ordering logic. The `stopifnot()` guards are the important part: a substitution that stops matching on a different monocle build fails the run instead of quietly leaving the old broken call in place.

```r
patch_extract_ddrtree_ordering()
patch_project2MST()
cds <- orderCells(cds)
```

## 2.5 Step 4 — extract coordinates and tree edges

Two things get stored: per-cell coordinates, and the backbone tree drawn over them.

```r
reduced <- t(reducedDimS(cds))              # per-cell DDRTree embedding
meta <- data.frame(
  DR_1       = reduced[, 1],
  DR_2       = reduced[, 2],
  pseudotime = pData(cds)$Pseudotime,
  # factor, NOT character: the by-state colour code uses levels(meta$state) to
  # build its palette; a character column makes levels() return NULL and the
  # per-state colours silently vanish
  state      = factor(as.character(pData(cds)$State)),
  row.names  = colnames(expr_b)
)

dp_mst     <- minSpanningTree(cds)
Y          <- t(reducedDimK(cds))           # the tree's own vertex coordinates
edges_list <- igraph::as_edgelist(dp_mst)
edges <- data.frame(
  source = edges_list[, 1],           target = edges_list[, 2],  weight = 1,
  source_dim_1 = Y[edges_list[, 1], 1], source_dim_2 = Y[edges_list[, 1], 2],
  target_dim_1 = Y[edges_list[, 2], 1], target_dim_2 = Y[edges_list[, 2], 2]
)
```

The edge coordinates come from `reducedDimK` (the tree vertices), not `reducedDimS` (the cells) — they live in the same 2-D space but are different point sets.

## 2.6 Step 5 — inject and verify

```r
stopifnot(
  nrow(meta) == length(b_idx),
  !anyNA(meta$pseudotime),
  nrow(edges) > 0,
  all(c("DR_1", "DR_2", "pseudotime", "state") %in% colnames(meta))
)

crb$addTrajectory("monocle2", "B_cell_maturation", list(meta = meta, edges = edges))
saveRDS(crb, crb_path)
```

The assertions catch a degenerate re-run: an all-`NA` pseudotime or an empty edge table would still save and still open in the app, just showing nothing.

---

# 3. Honest scope

These are peripheral-blood B cells, not a bone-marrow developmental lineage. The trajectory is **illustrative** of the pseudotime feature, not a biological claim about B-cell ontogeny.

# 4. Try it

```r
library(CerebroNexus)
createShinyApp(
  cerebro_data = c(
    "PBMC - Full (T+B)" = system.file("extdata/v1.4/demo_full_tcr_bcr.crb",
                                      package = "CerebroNexus")
  )
)
```

The conditional **Trajectory** tab appears because the `.crb` carries trajectory data. It colours the projection by monocle2 `state` or continuous `pseudotime`, and shows states-by-group and expression-along-pseudotime views.
