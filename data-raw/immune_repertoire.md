# Immune repertoire demos — design and rebuild notes

Provenance of record (citation, licence, sampling, output size) lives in [`DATASETS.md`](DATASETS.md).
This file is the working guide: what to download, what each step does to the data, and the code that does it.
Every command is meant to be copy-pasted and run from the package root.

## Contents

1. [What ships](#1-what-ships)
2. [What is real and what is assigned](#2-what-is-real-and-what-is-assigned)
3. [Rebuild, end to end](#3-rebuild-end-to-end)
   - [3.1 Download the contigs](#31-download-the-contigs)
   - [3.2 Run the build](#32-run-the-build)
   - [3.3 Step 1 — contigs to clonotype pools](#33-step-1--contigs-to-clonotype-pools)
   - [3.4 Step 2 — subsetting `example.crb`](#34-step-2--subsetting-examplecrb)
   - [3.5 Step 3 — lineage-constrained assignment](#35-step-3--lineage-constrained-assignment)
   - [3.6 Step 4 — the per-sample list](#36-step-4--the-per-sample-list)
   - [3.7 Step 5 — verification](#37-step-5--verification)
4. [Why these stay three separate files](#4-why-these-stay-three-separate-files)
5. [Try it](#5-try-it)

---

# 1. What ships

| File | Dropdown label | Cells | Repertoire | shipped? |
|---|---|---|---|---|
| `demo_full_tcr_bcr.crb` | PBMC - Full (T+B) | all (T + B + Mono), 1,476 | TCR **and** BCR | **yes** |
| `demo_healthy_t.crb` | PBMC - Healthy (T/NK) | T + Mono | TCR only | optional |
| `demo_bcell_rich.crb` | PBMC - B-cell rich | B + 25 % of T | BCR only | optional |

The shipped file also carries the monocle2 pseudotime trajectory (see [`trajectory.md`](trajectory.md)), so one dataset surfaces the Immune Repertoire **and** Trajectory tabs.
The two narrower subsets are built by the same script but not shipped by default; they exist for a multi-sample switcher demo.

# 2. What is real and what is assigned

Worth being precise about, because this demo mixes two sources:

| | |
|---|---|
| **real, measured** | the cells, their expression and UMAP (from `example.crb`); the clonotype sequences — genuine 10x PBMC VDJ contigs, `CTgene` / `CTnt` / `CTaa` / `CTstrict` as scRepertoire derives them |
| **assigned** | *which* cell gets *which* clonotype. The cells and the receptors come from different experiments, so the pairing is drawn with replacement from the pool |

The assignment is **lineage-constrained**, not random: TCR clonotypes go only to T cells and BCR only to B cells, so the repertoire is biologically plausible rather than noise, and clone-size distributions behave the way the app's plots expect. It is still an assignment — do not read biology off a clone's position in the UMAP.

This is why the demo cannot answer questions about receptor–phenotype association. For a dataset where the receptor and the transcriptome were measured **in the same cell**, use `demo_hla_tcr_dextramer.crb` ([`hla.md`](hla.md)).

---

# 3. Rebuild, end to end

## 3.1 Download the contigs

A two-step build: the raw VDJ contig CSVs are not committed, only the built `.crb`. This block is complete:

```bash
cd "$(git rev-parse --show-toplevel)"
mkdir -p data-raw/vdj_10x
BASE=https://cf.10xgenomics.com/samples/cell-vdj/3.1.0/vdj_v1_hs_pbmc3

curl -fL -o data-raw/vdj_10x/pbmc3_t_contig.csv \
  "$BASE/vdj_v1_hs_pbmc3_t_filtered_contig_annotations.csv"
curl -fL -o data-raw/vdj_10x/pbmc3_b_contig.csv \
  "$BASE/vdj_v1_hs_pbmc3_b_filtered_contig_annotations.csv"
```

Source: 10x Genomics public dataset `vdj_v1_hs_pbmc3` (human PBMC, 5′ VDJ), Cell Ranger 3.1.0. A few MB each — no resume logic needed, unlike the dextramer matrices.

The other input, `inst/extdata/v1.4/example.crb`, is already in the repository.

## 3.2 Run the build

```bash
Rscript data-raw/build_ir_demos.R
```

Needs `cerebroAppLite` and `scRepertoire` (≥ 2.0) installed. Paths are overridable for a dry run into a scratch directory:

```bash
OUT_FULL=/tmp/full.crb OUT_HEALTHY=/tmp/h.crb OUT_BCELL=/tmp/b.crb \
  Rscript data-raw/build_ir_demos.R
```

(`SRC_CRB`, `T_CSV`, `B_CSV` are overridable the same way.)

The script prints five stages; steps 3.3–3.7 below are what each one does.

## 3.3 Step 1 — contigs to clonotype pools

```r
pool_from <- function(csv, kind) {
  raw      <- read.csv(csv, stringsAsFactors = FALSE)
  contigs  <- loadContigs(raw, format = "10X")
  combined <- if (kind == "TCR") combineTCR(contigs) else combineBCR(contigs, threshold = 0.85)
  df <- do.call(rbind, combined)[, c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict")]
  # a clonotype with no gene call or no strict definition is not usable
  df <- df[!is.na(df$CTgene) & nzchar(df$CTgene) &
           !is.na(df$CTstrict) & nzchar(df$CTstrict), , drop = FALSE]
  unique(df)                       # a POOL, so identical clonotypes collapse
}

pool_tcr <- pool_from(t_csv, "TCR")
pool_bcr <- pool_from(b_csv, "BCR")
```

`combineBCR(threshold = 0.85)` clusters BCR sequences by nucleotide similarity, since B-cell receptors somatically hypermutate and exact matching would split one clone into many. TCRs do not mutate, so `combineTCR` matches exactly.

The `unique()` is what makes this a *pool* rather than a per-cell table: the original barcodes are discarded here, because the cells they refer to are not the cells in `example.crb`.

## 3.4 Step 2 — subsetting `example.crb`

`subset_cerebro()` rebuilds a fresh `Cerebro_v1.3` restricted to a set of barcodes. Three kinds of slot need different treatment:

```r
bc   <- old$getMetaData()$cell_barcode
keep <- bc %in% keep_barcodes

# per-cell slots: filter by barcode
new$expression  <- old$expression[, keep, drop = FALSE]      # genes x cells
new$meta_data   <- old$meta_data[keep, , drop = FALSE]
new$projections <- lapply(old$projections, function(p) p[keep, , drop = FALSE])

# drop now-empty factor levels, or stale groups linger in the app's dropdowns
for (col in colnames(new$meta_data)) {
  if (is.factor(new$meta_data[[col]])) new$meta_data[[col]] <- droplevels(new$meta_data[[col]])
}

# group-level slots: filter by the groups that SURVIVE in the subset
for (f in c("marker_genes", "most_expressed_genes", "enriched_pathways")) {
  if (!is.null(new[[f]])) new[[f]] <- filter_group_slot(new[[f]], allowed)
}
```

The group-level pass matters and is easy to forget: `marker_genes`, `most_expressed_genes` and `enriched_pathways` are nested lists whose leaves are data frames keyed by a grouping column in their **first** column. Without pruning them, a T + Mono subset would still advertise B-cell marker rows for cells it no longer contains. `filter_group_slot()` recurses, drops rows whose group is gone, and prunes leaves and branches that empty out.

## 3.5 Step 3 — lineage-constrained assignment

```r
assign_ir <- function(meta, pool, lineage_regex, seed = 42) {
  set.seed(seed)
  lineage_cells <- meta$cell_barcode[
    grepl(lineage_regex, meta$cell_type, ignore.case = TRUE)
  ]
  if (length(lineage_cells) == 0 || nrow(pool) == 0) return(NULL)
  picks <- pool[sample(nrow(pool), length(lineage_cells), replace = TRUE), ct_cols, drop = FALSE]
  data.frame(barcode = lineage_cells, CTgene = picks$CTgene, CTnt = picks$CTnt,
             CTaa = picks$CTaa, CTstrict = picks$CTstrict, stringsAsFactors = FALSE)
}

full_ir <- rbind(
  assign_ir(full_meta, pool_tcr, "T cell"),
  assign_ir(full_meta, pool_bcr, "B cell")
)
```

`replace = TRUE` is deliberate: sampling with replacement is what produces expanded clones, which is the thing the clonal-expansion plots are there to show. `set.seed(42)` pins it.

## 3.6 Step 4 — the per-sample list

The `immune_repertoire` slot is a **named list, one data frame per sample** — not a single flat table. Giving it more than one sample is what enables the app's cross-sample and Paired Scatter analyses:

```r
split_ir_by_sample <- function(ir_df, meta) {
  if (is.null(ir_df) || nrow(ir_df) == 0) return(list())
  samp <- meta$sample[match(ir_df$barcode, meta$cell_barcode)]
  samp[is.na(samp)] <- "unknown"
  split(ir_df, factor(samp))
}
full$immune_repertoire <- split_ir_by_sample(full_ir, full$getMetaData())
```

Each data frame carries exactly the five columns the app's `immune_repertoire/data.R` expects — `barcode`, `CTgene`, `CTnt`, `CTaa`, `CTstrict`. Chain type is **inferred from `CTgene`** (`TRA|TRB|TRG|TRD` → TCR, `IGH|IGK|IGL` → BCR), not stored separately.

## 3.7 Step 5 — verification

The script re-reads each written file and asserts the lineage constraint actually held, so a broken assignment fails loudly instead of shipping:

```r
ir_df <- do.call(rbind, o$immune_repertoire)
ct_of <- m$cell_type[match(ir_df$barcode, m$cell_barcode)]
tcr_on_t <- all(grepl("T cell", ct_of[grepl("TRA|TRB",     ir_df$CTgene)]))
bcr_on_b <- all(grepl("B cell", ct_of[grepl("IGH|IGK|IGL", ir_df$CTgene)]))
```

Output:

```
  demo_healthy_t.crb     | ... | cells=... | types=Monocytes/T cells        | IR=TCR     | TCR-on-T=TRUE BCR-on-B=TRUE
  demo_bcell_rich.crb    | ... | cells=... | types=B cells/T cells          | IR=BCR     | TCR-on-T=TRUE BCR-on-B=TRUE
  demo_full_tcr_bcr.crb  | ... | cells=1476| types=B cells/Monocytes/T cells| IR=TCR+BCR | TCR-on-T=TRUE BCR-on-B=TRUE
```

---

# 4. Why these stay three separate files

`demo_full_tcr_bcr.crb` / `demo_healthy_t.crb` / `demo_bcell_rich.crb` are deliberately not merged.
Their whole point is to *differ* — cell composition, UMAP and TCR/BCR content all change when you switch — so the multi-`.crb` switching feature has something to demonstrate.
A single merged file would remove the only demonstration of that feature.

Worth stating because "too many datasets" comes up periodically: the objection is about isolated one-feature datasets, not about this intra-family variation, which exists to exercise dataset switching itself.

(Recorded from the 2026-07-07 trajectory-demo consolidation, whose other outcome — folding the pseudotime trajectory into `demo_full_tcr_bcr.crb` and deleting the standalone `demo_trajectory.crb` — is in [`trajectory.md`](trajectory.md).)

# 5. Try it

```r
library(cerebroAppLite)
createShinyApp(
  cerebro_data = c(
    "PBMC - Full (T+B)" = system.file("extdata/v1.4/demo_full_tcr_bcr.crb",
                                      package = "cerebroAppLite")
  )
)
```

The Immune Repertoire tab appears because the `.crb` carries clonotypes; the Trajectory tab appears because the same file carries the monocle2 trajectory.
