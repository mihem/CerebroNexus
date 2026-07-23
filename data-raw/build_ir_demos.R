#!/usr/bin/env Rscript
#' Build the immune-repertoire demo .crb.
#'
#' The SHIPPED demo is `demo_full_tcr_bcr.crb` ("PBMC - Full (T+B)") — the full
#' cell subset carrying both TCR and BCR (and, via build_trajectory_demo.R, a
#' monocle2 trajectory), so one dataset covers everything. Clonotypes are
#' **cell-type-constrained** (TCR -> T cells, BCR -> B cells) so the immune
#' repertoire is biologically plausible rather than random noise.
#'
#' This script ALSO builds two narrower subsets, kept for a multi-sample
#' switcher demo but NOT shipped by default (the Full set is their superset):
#'
#'   demo_full_tcr_bcr.crb "PBMC - Full (T+B)"      all cells, TCR->T + BCR->B  [SHIPPED]
#'   demo_healthy_t.crb   "PBMC - Healthy (T/NK)"   T + Mono subset,  TCR only  [optional]
#'   demo_bcell_rich.crb  "PBMC - B-cell rich"      B + some T subset, BCR only [optional]
#'
#' Clonotype source: 10x Genomics public dataset vdj_v1_hs_pbmc3 (Human PBMC,
#' 5' VDJ), Cell Ranger 3.1.0. Purely public data; the only identity handling
#' is neutral, descriptive sample names.
#'
#' Pipeline: 10x filtered_contig_annotations.csv -> scRepertoire loadContigs +
#' combineTCR/combineBCR -> CT* clonotype pool -> assigned only to cells of the
#' matching lineage -> injected into a fresh Cerebro_v1.3 subset of example.crb.

suppressMessages({
  library(CerebroNexus)
  library(scRepertoire)
})

## ---- Paths (override via env for a tmp dry-run) ---------------------------
src_crb <- Sys.getenv("SRC_CRB", "inst/extdata/v1.4/example.crb")
t_csv <- Sys.getenv("T_CSV", "data-raw/vdj_10x/pbmc3_t_contig.csv")
b_csv <- Sys.getenv("B_CSV", "data-raw/vdj_10x/pbmc3_b_contig.csv")
out_healthy <- Sys.getenv("OUT_HEALTHY", "inst/extdata/v1.4/demo_healthy_t.crb")
out_bcell <- Sys.getenv("OUT_BCELL", "inst/extdata/v1.4/demo_bcell_rich.crb")
out_full <- Sys.getenv("OUT_FULL", "inst/extdata/v1.4/demo_full_tcr_bcr.crb")

ct_cols <- c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict")

## Fields copied when reconstructing a Cerebro_v1.3 object.
data_fields <- c(
  "expression",
  "meta_data",
  "projections",
  "groups",
  "gene_lists",
  "trees",
  "trajectories",
  "enriched_pathways",
  "marker_genes",
  "most_expressed_genes",
  "extra_material",
  "cell_cycle",
  "parameters",
  "technical_info",
  "experiment",
  "version",
  "expression_backend"
)

## ---- 1. clonotype pools from 10x contigs ----------------------------------
message("[1/5] Building clonotype pools from 10x contigs ...")

pool_from <- function(csv, kind) {
  raw <- read.csv(csv, stringsAsFactors = FALSE)
  contigs <- loadContigs(raw, format = "10X")
  combined <- if (kind == "TCR") {
    combineTCR(contigs)
  } else {
    combineBCR(contigs, threshold = 0.85)
  }
  df <- do.call(rbind, combined)
  df <- df[, ct_cols, drop = FALSE]
  df <- df[
    !is.na(df$CTgene) &
      nzchar(df$CTgene) &
      !is.na(df$CTstrict) &
      nzchar(df$CTstrict),
    ,
    drop = FALSE
  ]
  unique(df)
}

pool_tcr <- pool_from(t_csv, "TCR")
pool_bcr <- pool_from(b_csv, "BCR")
message(sprintf(
  "  TCR pool: %d clonotypes | BCR pool: %d clonotypes",
  nrow(pool_tcr),
  nrow(pool_bcr)
))

## ---- 2. helpers: subset + cell-type-constrained IR injection --------------
old <- readRDS(src_crb)
full_meta <- old$getMetaData()
stopifnot("cell_type" %in% colnames(full_meta))

## Filter the pre-computed group-level analyses (marker genes, most-expressed
## genes, enriched pathways) so they only reference groups that still exist in
## the subset. These slots are nested lists whose leaves are data.frames keyed
## by a grouping column (`cell_type`, `seurat_clusters`, `sample`, ...) held in
## the first column. `allowed` maps each grouping column to the values kept in
## the subset's meta_data; any leaf row whose group is absent is dropped, and
## emptied leaves/branches are pruned. Without this, a T/Mono subset would still
## advertise B-cell marker/expression rows it no longer contains.
filter_group_slot <- function(x, allowed) {
  if (is.data.frame(x)) {
    grp <- names(x)[1]
    if (!is.null(grp) && grp %in% names(allowed)) {
      x <- x[as.character(x[[grp]]) %in% allowed[[grp]], , drop = FALSE]
      ## drop unused factor levels so stale groups don't linger in dropdowns
      for (col in names(x)) {
        if (is.factor(x[[col]])) x[[col]] <- droplevels(x[[col]])
      }
    }
    if (nrow(x) == 0) {
      return(NULL)
    }
    return(x)
  }
  if (is.list(x)) {
    x <- lapply(x, filter_group_slot, allowed = allowed)
    x <- x[!vapply(x, is.null, logical(1))]
    if (length(x) == 0) {
      return(NULL)
    }
    return(x)
  }
  x
}

## Rebuild a fresh Cerebro_v1.3 restricted to the given cell barcodes. Per-cell
## slots (expression columns, meta rows, projection rows) are filtered by
## barcode; group-level analyses are filtered by the groups that survive in the
## subset (see filter_group_slot) so the demo is internally consistent.
group_slots <- c("marker_genes", "most_expressed_genes", "enriched_pathways")

subset_cerebro <- function(keep_barcodes, experiment_name) {
  new <- Cerebro_v1.3$new()
  for (f in data_fields) {
    val <- old[[f]]
    if (!is.null(val)) new[[f]] <- val
  }
  bc <- old$getMetaData()$cell_barcode
  keep <- bc %in% keep_barcodes

  ## expression: genes x cells -> keep matching columns
  if (!is.null(new$expression)) {
    new$expression <- old$expression[, keep, drop = FALSE]
  }
  ## meta_data: one row per cell; drop now-empty factor levels so stale groups
  ## (e.g. a cell type no longer present) don't linger in the app's dropdowns
  new$meta_data <- old$meta_data[keep, , drop = FALSE]
  for (col in colnames(new$meta_data)) {
    if (is.factor(new$meta_data[[col]])) {
      new$meta_data[[col]] <- droplevels(new$meta_data[[col]])
    }
  }
  ## projections: one row per cell, keyed by barcode
  if (!is.null(old$projections)) {
    new$projections <- lapply(old$projections, function(p) {
      p[keep, , drop = FALSE]
    })
  }

  ## group-level slots: keep only groups present in the subset's meta_data
  meta_sub <- new$meta_data
  allowed <- list()
  for (grp in c("cell_type", "seurat_clusters", "sample")) {
    if (grp %in% colnames(meta_sub)) {
      allowed[[grp]] <- unique(as.character(meta_sub[[grp]]))
    }
  }
  for (f in group_slots) {
    if (!is.null(new[[f]])) {
      new[[f]] <- filter_group_slot(new[[f]], allowed)
    }
  }

  ## groups slot: list(dim -> character vector of group values). This drives the
  ## app's group dropdowns, so prune values absent from the subset.
  if (!is.null(new$groups)) {
    for (grp in names(new$groups)) {
      if (grp %in% names(allowed)) {
        new$groups[[grp]] <- intersect(new$groups[[grp]], allowed[[grp]])
      }
    }
  }

  ## stamp a descriptive experiment name if the slot supports it
  if (!is.null(new$experiment) && is.list(new$experiment)) {
    new$experiment$experiment_name <- experiment_name
  }
  new
}

## Assign clonotypes from `pool` only to cells whose cell_type matches
## `lineage_regex`, sampling with replacement. Returns a data.frame with the
## five IR columns the Shiny app expects.
assign_ir <- function(meta, pool, lineage_regex, seed = 42) {
  set.seed(seed)
  lineage_cells <- meta$cell_barcode[
    grepl(lineage_regex, meta$cell_type, ignore.case = TRUE)
  ]
  if (length(lineage_cells) == 0 || nrow(pool) == 0) {
    return(NULL)
  }
  picks <- pool[
    sample(nrow(pool), length(lineage_cells), replace = TRUE),
    ct_cols,
    drop = FALSE
  ]
  data.frame(
    barcode = lineage_cells,
    CTgene = picks$CTgene,
    CTnt = picks$CTnt,
    CTaa = picks$CTaa,
    CTstrict = picks$CTstrict,
    stringsAsFactors = FALSE
  )
}

## Split a clonotype data.frame into a per-sample list keyed by the data set's
## `sample` column, matched by barcode. The immune_repertoire slot is a
## list(sample -> data.frame); giving it >1 sample is what enables the
## cross-sample / Paired Scatter analyses in the app.
split_ir_by_sample <- function(ir_df, meta) {
  if (is.null(ir_df) || nrow(ir_df) == 0) {
    return(list())
  }
  samp <- meta$sample[match(ir_df$barcode, meta$cell_barcode)]
  samp[is.na(samp)] <- "unknown"
  split(ir_df, factor(samp))
}

## ---- 3. demo_healthy_t: T + Monocytes, TCR on T cells ---------------------
message("[2/5] Building demo_healthy_t (T/NK, TCR) ...")
keep_healthy <- full_meta$cell_barcode[
  grepl("T cell|Mono", full_meta$cell_type, ignore.case = TRUE)
]
healthy <- subset_cerebro(keep_healthy, "PBMC - Healthy (T/NK)")
healthy_ir <- assign_ir(healthy$getMetaData(), pool_tcr, "T cell")
healthy$immune_repertoire <- split_ir_by_sample(
  healthy_ir,
  healthy$getMetaData()
)

## ---- 4. demo_bcell_rich: B + subset of T, BCR on B cells ------------------
message("[3/5] Building demo_bcell_rich (B-cell rich, BCR) ...")
set.seed(7)
b_cells <- full_meta$cell_barcode[grepl("B cell", full_meta$cell_type)]
t_cells_all <- full_meta$cell_barcode[grepl("T cell", full_meta$cell_type)]
t_subset <- sample(t_cells_all, floor(length(t_cells_all) * 0.25))
keep_bcell <- c(b_cells, t_subset)
bcell <- subset_cerebro(keep_bcell, "PBMC - B-cell rich")
bcell_ir <- assign_ir(bcell$getMetaData(), pool_bcr, "B cell")
bcell$immune_repertoire <- split_ir_by_sample(bcell_ir, bcell$getMetaData())

## ---- 5. demo_full_tcr_bcr: all cells, TCR->T and BCR->B -------------------
message("[4/5] Building demo_full_tcr_bcr (Full, T+B) ...")
full <- subset_cerebro(full_meta$cell_barcode, "PBMC - Full (T+B)")
full_ir <- rbind(
  assign_ir(full_meta, pool_tcr, "T cell"),
  assign_ir(full_meta, pool_bcr, "B cell")
)
full$immune_repertoire <- split_ir_by_sample(full_ir, full$getMetaData())

## ---- 6. save + verify ------------------------------------------------------
message("[5/5] Saving and verifying ...")
dir.create(dirname(out_full), recursive = TRUE, showWarnings = FALSE)
saveRDS(healthy, out_healthy)
saveRDS(bcell, out_bcell)
saveRDS(full, out_full)

verify <- function(path) {
  o <- readRDS(path)
  m <- o$getMetaData()
  ir <- o$immune_repertoire
  ct <- unlist(lapply(ir, function(d) d$CTgene))
  chains <- c(
    if (any(grepl("TRA|TRB|TRG|TRD", ct))) "TCR",
    if (any(grepl("IGH|IGK|IGL", ct))) "BCR"
  )
  ## sanity: are TCR barcodes actually on T cells (and BCR on B)?
  ir_df <- do.call(rbind, ir)
  ct_of <- m$cell_type[match(ir_df$barcode, m$cell_barcode)]
  tcr_on_t <- all(
    grepl("T cell", ct_of[grepl("TRA|TRB", ir_df$CTgene)])
  )
  bcr_on_b <- all(
    grepl("B cell", ct_of[grepl("IGH|IGK|IGL", ir_df$CTgene)])
  )
  message(sprintf(
    paste0(
      "  %-22s | %.0f KB | cells=%d | types=%s | IR=%s | rows=%d | ",
      "TCR-on-T=%s BCR-on-B=%s"
    ),
    basename(path),
    file.size(path) / 1024,
    nrow(m),
    paste(sort(unique(as.character(m$cell_type))), collapse = "/"),
    paste(chains, collapse = "+"),
    nrow(ir_df),
    tcr_on_t,
    bcr_on_b
  ))
}
verify(out_healthy)
verify(out_bcell)
verify(out_full)
cat("\nDone.\n")
