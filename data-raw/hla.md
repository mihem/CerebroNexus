# HLA & TCR motif demos — design and rebuild notes

Provenance of record (citation, licence, sampling, output size) lives in [`DATASETS.md`](DATASETS.md).
This file is the working guide: what to download, what each step does to the data, and the code that does it.
Every command is meant to be copy-pasted and run from the package root; nothing here is pseudocode.

## Contents

1. [What ships, and what no longer does](#1-what-ships-and-what-no-longer-does)
2. [Why a real single-cell demo was needed](#2-why-a-real-single-cell-demo-was-needed)
3. [`demo_hla_tcr_dextramer.crb` — real antigen-selected single cells](#3-demo_hla_tcr_dextramercrb--real-antigen-selected-single-cells)
   - [3.1 Source and licence](#31-source-and-licence)
   - [3.2 Download](#32-download)
   - [3.3 What each file contains](#33-what-each-file-contains)
   - [3.4 Step 1 — contigs to one clonotype per cell](#34-step-1--contigs-to-one-clonotype-per-cell)
   - [3.5 Step 2 — dextramer binder calls and the reagent's HLA restriction](#35-step-2--antigen-specificity-and-its-hla-restriction)
   - [3.6 Step 3 — donor genotypes (table S1)](#36-step-3--donor-genotypes-table-s1)
   - [3.7 Step 4 — cell selection](#37-step-4--cell-selection)
   - [3.8 Step 5 — expression and UMAP, and whether Seurat is needed](#38-step-5--expression-and-umap-and-whether-seurat-is-needed)
   - [3.9 Step 6 — assembling the `.crb`](#39-step-6--assembling-the-crb)
   - [3.10 Step 7 — verification](#310-step-7--verification)
4. [Known problems](#4-known-problems)
   - [4.1 Inferring the genotypes from binding was wrong](#41-inferring-the-genotypes-from-binding-was-wrong)
   - [4.2 What is still declared: the selection](#42-what-is-still-declared-the-selection)
   - [4.3 The paper's curated table cannot be joined](#43-the-papers-curated-table-cannot-be-joined)
   - [4.4 Smaller limitations](#44-smaller-limitations)
5. [`demo_hla_tcr_bulk.crb` — removed, pipeline kept](#5-demo_hla_tcr_bulkcrb--removed-pipeline-kept)
6. [`demo_hla_tcr_synthetic.crb` — removed, pipeline kept](#6-demo_hla_tcr_syntheticcrb--removed-pipeline-kept)
7. [Rebuilding everything](#7-rebuilding-everything)

---

# 1. What ships, and what no longer does

**One demo ships**, and everything in it is measured:

| demo | cells | TCR | HLA genotype | build script |
|---|---|---|---|---|
| `demo_hla_tcr_dextramer.crb` | **real** | **real** | **real** | `build_hla_tcr_dextramer_demo.R` |

Two earlier demos were **removed from the package** (2026-07-21). CerebroNexus
is a single-cell application, and a demo that is neither real nor single-cell
earns its place only while nothing better exists:

| removed demo | what it was | why it went |
|---|---|---|
| `demo_hla_tcr_synthetic.crb` | fabricated fixture, 30 donors x 167 cells | it existed only because real repertoires were thought too sparse to draw. Section 2 shows they are not, once selected — so it was answering a question the real demo now answers better |
| `demo_hla_tcr_bulk.crb` | real bulk TCRb + real genotypes, 100 donors | real, but bulk: no cells, no transcriptome. Its workflow moved to the *bring your own bulk cohort* vignette |

Both **build scripts are kept and still run** (sections 5 and 6) — they are the
reproducibility record, and `data-raw/` is `.Rbuildignore`d, so they add nothing
to the installed package. What changed is only what ships in
`inst/extdata/v1.4/`.

What the surviving demo does not cover, stated plainly: it is sorted CD8+ T
cells, so **Class I only**. The Class I x Class II pair scope is gated on
`hla_pair_available()` and therefore stays hidden here; it appears when a data
set carries Class II typing plus a lineage column. `observation_unit =
"analysis unit"` likewise no longer has a shipped example, though the bulk build
script still produces one.

---

# 2. Why a real single-cell demo was needed

The fair objection to the motif page is: *if the network is only legible on synthetic data, what is the feature for?*

A CDR3 Hamming-1 network needs an **antigen-selected** repertoire.
An unselected polyclonal repertoire is sparse in CDR3 space: neighbours at distance 1 are rare, and adding cells does not fix it — pair count grows roughly with n², so a few thousand cells still extrapolates to almost nothing.
A selected repertoire converges instead: different donors independently arrive at near-identical CDR3s against the same epitope (public / convergent recombination), which is what the network draws.

That is a claim, so it was measured — same source, same code, three subsets:

| subset | unique CDR3β | result |
|---|---|---|
| all cells, unselected | 26,449 | trips the size guard; nothing to draw |
| cells binding any dextramer | 2,910 | **308 nodes in 75 motifs** |
| one epitope, Flu-MP `GILGFVFTL` | 267 | **121 nodes in 7 motifs** |

The last row is the argument: against one immunodominant influenza epitope, 45 % of the observed CDR3s collapse into **seven** families.
The control is the predecessor — a real-sequence demo built from an *unselected* repertoire rendered a **4-node** graph (456 unique CDR3β → 2 Hamming-1 pairs). Same code, different kind of repertoire.

---

# 3. `demo_hla_tcr_dextramer.crb` — real antigen-selected single cells

## 3.1 Source and licence

10x Genomics, *CD8+ T cells of Healthy Donor 1–4* (2019) — the dextramer / Immune Map experiment published as:

> Zhang W, Hawkins PG, He J, Gupta NT, Liu J, Choonoo G, Jeong SW, Chen CR, Dhanik A, Dillon M, Deering R, Macdonald LE, Thurston G, Atwal GS.
> *A framework for highly multiplexed dextramer mapping and prediction of T cell receptor sequences to antigen specificity.*
> **Science Advances** 7(20):eabf5835 (2021). <https://doi.org/10.1126/sciadv.abf5835>

Licence **CC BY 4.0**; the attribution ships beside the data as `inst/extdata/v1.4/demo_hla_tcr_dextramer.ATTRIBUTION.md`.

CD8+ T cells from four HLA-typed healthy donors were stained with a pool of dCODE dextramers — 98 reagents over 8 HLA alleles, the same panel for every donor — **sorted for dextramer binding**, then run on 10x 5′ Single Cell Immune Profiling.
Each cell therefore carries a paired αβ TCR, a transcriptome, a TotalSeq-C surface-protein panel, and the identity of the dextramer it bound. ~190,000 cells before filtering.

Per-donor landing pages: [donor 1](https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-1-1-standard-3-0-2) · [donor 2](https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-2-1-standard-3-0-2) · [donor 3](https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-3-1-standard-3-0-2) · [donor 4](https://www.10xgenomics.com/datasets/cd-8-plus-t-cells-of-healthy-donor-4-1-standard-3-0-2)

## 3.2 Download

`Rscript data-raw/build_hla_tcr_dextramer_demo.R` does this itself on first run and skips anything already present, so a manual download is only needed if you want the raw files without building.
This block is complete — copy it whole:

```bash
cd "$(git rev-parse --show-toplevel)"
base=https://cf.10xgenomics.com/samples/cell-vdj/3.0.2
cache=data-raw/vdj_10x_dextramer
mkdir -p "$cache"

for d in 1 2 3 4; do
  stem="vdj_v1_hs_aggregated_donor${d}"
  for f in _all_contig_annotations.csv _binarized_matrix.csv _filtered_feature_bc_matrix.tar.gz; do
    curl -fL --retry 5 --retry-delay 3 -C - -o "${cache}/${stem}${f}" "${base}/${stem}/${stem}${f}"
  done
  mkdir -p "${cache}/${stem}_gex"
  tar xzf "${cache}/${stem}_filtered_feature_bc_matrix.tar.gz" -C "${cache}/${stem}_gex"
done

du -sh "$cache"     # ~2.7 GB once unpacked
```

`-C -` resumes a partial transfer and `--retry 5` survives a dropped connection: the expression matrices are ~283 MB each and the server is not always fast.
The R script needs the same care for a different reason — R's default `download.file` timeout is 60 s, which truncates these files and leaves a partial one that the *next* run mistakes for a finished download. It writes to a `.part` file and only renames on success:

```r
options(timeout = max(getOption("timeout"), 3600))

fetch <- function(url, dest) {
  if (file.exists(dest)) return(invisible(dest))
  part <- paste0(dest, ".part")
  status <- system2("curl", c("-fL", "--retry", "5", "--retry-delay", "3",
                              "-C", "-", "-o", shQuote(part), shQuote(url)),
                    stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L) || !file.exists(part) || file.info(part)$size == 0) {
    unlink(part); stop("download failed: ", url, call. = FALSE)
  }
  file.rename(part, dest)                 # only now is it a completed download
}
```

The **donor HLA genotypes** are not in these files. They come from table S1 of the paper's supplementary PDF:

```
https://www.science.org/doi/suppl/10.1126/sciadv.abf5835/suppl_file/abf5835_sm.pdf
```

**Open that in a browser — `curl` will not work.** science.org sits behind Cloudflare and returns 403 to any command-line client, with or without a browser user-agent (verified). It is the only download in this repository that cannot be scripted.

Two further notes on that link: the one printed *inside* the paper (`advances.sciencemag.org/.../DC1`) is dead — that domain was retired when Science migrated to science.org — and the PDF is not committed, since this repository never commits third-party raw sources.

None of this blocks a rebuild: the 14 genotype rows are transcribed inline in the build script, so `build_hla_tcr_dextramer_demo.R` needs nothing from the PDF. Fetch it only to check the transcription.

## 3.3 What each file contains

| file | size | one row is | fields used |
|---|---|---|---|
| `*_all_contig_annotations.csv` | 32–51 MB | one **contig** (a cell appears ≥ 2×, once per chain) | `barcode`, `chain`, `v_gene`, `j_gene`, `cdr3`, `productive` |
| `*_binarized_matrix.csv` | 19–53 MB | one **cell** | `barcode`, `donor`, one `True`/`False` column per dextramer |
| `*_filtered_feature_bc_matrix.tar.gz` | ~283 MB | matrix | 33,538 genes × cells, `Gene Expression` assay |

A dextramer column is named `<allele>_<peptide>_<antigen>_binder`:

```
A0201_GILGFVFTL_Flu-MP_Influenza_binder
A0301_KLGGALQAK_IE-1_CMV_binder
A1101_AVFDRKSDAK_EBNA-3B_EBV_binder
```

The allele prefix is the **reagent's** HLA restriction — a published property of the dextramer, independent of these cells. Columns containing `NR(` are 10x's negative controls and are never treated as evidence.

## 3.4 Step 1 — contigs to one clonotype per cell

The app reads receptors through `hla_parse_ir_segments()`, whose contract is `barcode` + `CTgene` + `CTaa`: for the requested chain it takes the matching underscore slot, pulls the `TRBV…`/`TRBJ…` tokens out of `CTgene` and the CDR3 out of the same slot of `CTaa`.
**A row without a V gene is dropped** — which is why the pipeline starts from the contigs and not from the much smaller binarized matrix: that file has CDR3 amino acids but no V/J gene at all, and this source identifies a receptor by *(V gene, CDR3)*.

```r
contigs <- read.csv(donor_files(d)$contigs, stringsAsFactors = FALSE)
# a non-productive contig carries a CDR3 the cell never displayed
contigs <- contigs[
  contigs$productive %in% c("True", "TRUE", TRUE) &
    contigs$chain %in% c("TRA", "TRB"), , drop = FALSE
]

out <- scRepertoire::combineTCR(
  list(contigs),
  samples     = sprintf("donor%d", d),
  filterMulti = TRUE          # keep the dominant chain when a barcode has several,
)[[1]]                        # rather than letting an ambiguous cell contribute a
                              # CDR3 it may not carry
out$barcode_raw <- sub("^donor[0-9]+_", "", out$barcode)   # to rejoin the matrix
out$donor       <- sprintf("donor%d", d)
```

What that does to the shape:

```
before — one row per contig
  AAACCTGAGAAACCTA-1  TRA  TRAV12-2  TRAJ33   CAVNVAGKSTF
  AAACCTGAGAAACCTA-1  TRB  TRBV19    TRBJ2-7  CASSIRSSYEQYF

after — one row per cell
  barcode                    CTgene                                      CTaa
  donor1_AAACCTGAGAAACCTA-1  TRAV12-2.TRAJ33.TRAC_TRBV19.TRBJ2-7.TRBC2   CAVNVAGKSTF_CASSIRSSYEQYF
```

## 3.5 Step 2 — dextramer binder calls and the reagent's HLA restriction

```r
b   <- read.csv(donor_files(d)$binarized, stringsAsFactors = FALSE, check.names = FALSE)
dex <- grep("^[ABC][0-9]{4}_", colnames(b), value = TRUE)
dex <- grep("NR\\(", dex, value = TRUE, invert = TRUE)   # drop negative controls

hits <- as.matrix(b[, dex, drop = FALSE]) == "True"
hits[is.na(hits)] <- FALSE
keep <- rowSums(hits) == 1L                              # exactly one, or none
idx  <- max.col(hits, ties.method = "first")

data.frame(
  barcode_raw = b$barcode,
  donor       = sprintf("donor%d", d),
  single_binder = keep,
  dextramer   = ifelse(keep, dex[idx], NA_character_),
  stringsAsFactors = FALSE
)
```

Cells binding several dextramers are **dropped, not guessed at**: an ambiguous call would put a cell in the wrong HLA context, the one error this page must not make.
Of 189,512 cells, **87,490** bind exactly one.

Note what this removes and what it does not: it removes *ambiguity*, not *cross-reactivity*. A cell binding exactly one reagent is unambiguous, which is not the same as being specific for it — see [§4.1](#41-inferring-the-genotypes-from-binding-was-wrong), whose numbers apply per cell just as much as per donor. That is why the fields below ship as `dextramer_*` rather than `antigen` / `restricting_allele`, with a `restriction_in_genotype` (`yes`/`no`) column beside them.

The winning column name then parses into three fields — all three properties of the **reagent**:

```r
allele_of  <- function(x) paste0("HLA-", sub("^([ABC])([0-9]{2})([0-9]{2})_.*", "\\1*\\2:\\3", x))
peptide_of <- function(x) sub("^[ABC][0-9]{4}_([A-Z]+)_.*", "\\1", x)
antigen_of <- function(x) sub("_binder$", "", sub("^[ABC][0-9]{4}_[A-Z]+_", "", x))

# A0201_GILGFVFTL_Flu-MP_Influenza_binder
#   -> "HLA-A*02:01"   "GILGFVFTL"   "Flu-MP_Influenza"
```

## 3.6 Step 3 — donor genotypes (table S1)

Transcribed from table S1 (“HLA haplotypes of the healthy donors”) and kept **inline** in the build script as `DONOR_HLA`, so the script is self-contained — it is 14 rows, and a separate file would only be one more thing to keep in step:

| Donor | HLA-A1 | HLA-A2 | HLA-B1 | HLA-B2 |
|---|---|---|---|---|
| Donor 1 | 02:01 | 11:01 | 35:01 | na |
| Donor 2 | 02:01 | 01:01 | 08:01 | na |
| Donor 3 | 24:02 | 29:02 | 35:02 | 44:03 |
| Donor 4 | 03:01 | 03:01 | 07:02 | 57:01 |
| Donor V | 02:01 | 29:02 | 35:01 | 57:01 |

Donor V appears in the paper but not in the four aggregated 10x data sets, so the demo ships donors 1–4.
“na” is why donors 1 and 2 contribute a single B allele; donor 4 is homozygous A\*03:01.

```r
DONOR_HLA <- read.csv(text = "donor,copy,allele
donor1,1,HLA-A*02:01
donor1,2,HLA-A*11:01
donor1,1,HLA-B*35:01
...
donor4,2,HLA-B*57:01", stringsAsFactors = FALSE)

hla_donor_typing <- function(donors) {
  tab <- DONOR_HLA[DONOR_HLA$donor %in% donors, , drop = FALSE]
  data.frame(
    sample   = tab$donor,
    donor_id = tab$donor,               # donor-level counting in the app
    allele   = tab$allele,
    copy     = as.integer(tab$copy),    # 1 or 2 within a locus
    stringsAsFactors = FALSE
  )
}
```

Because these were measured independently of these cells, a carrier / non-carrier contrast on this demo is a real comparison. [§4.1](#41-inferring-the-genotypes-from-binding-was-wrong) is what happens if you try to infer them instead.

## 3.7 Step 4 — cell selection

Keep cells with a **fully paired** clonotype **and** exactly one binder call. The dextramer sort *is* the data set's defining property, not a convenience — it is what makes the network legible.

The paired test is stricter than "has a `CTaa`" on purpose: `combineTCR()` writes `<alpha>_<beta>` and puts the literal string `NA` on a side it could not resolve, so an earlier build shipped 1,493 single-chain cells while the docs called the demo paired αβ.

```r
sel <- merge(
  tcr_all,
  dex_all[dex_all$single_binder,
          c("barcode_raw", "donor", "dextramer",
            "dextramer_antigen", "dextramer_peptide", "dextramer_allele")],
  by = c("barcode_raw", "donor")
)

is_paired <- function(ctaa) {
  parts <- strsplit(ifelse(is.na(ctaa), "", ctaa), "_", fixed = TRUE)
  vapply(parts, function(p) {
    length(p) == 2L && all(nzchar(p)) && !any(p %in% c("NA", "None"))
  }, logical(1))
}
sel <- sel[is_paired(sel$CTaa), , drop = FALSE]

# The deterministic per-donor subsample (set.seed(20260721) at the top of the
# script) runs LATER, in step 5, once the expression join is done -- so the
# shipped object is exactly 3,000 x 4 = 12,000 cells rather than however many
# survived the join.
```

## 3.8 Step 5 — expression and UMAP, and whether Seurat is needed

**Is a Seurat object needed?** Only as a *tool*, never as a format. The `.crb` stores a plain numeric matrix and a plain two-column coordinate table; nothing Seurat-specific survives into it, and the app never loads Seurat at runtime. Seurat is used here because normalisation, variable-gene selection, PCA and UMAP are what produce those two things, and re-implementing them would be pointless. Any pipeline that yields a normalised matrix plus 2-D coordinates would do.

The matrix is subset to the kept cells **first**, so nothing is computed on cells that are then thrown away:

```r
mats <- lapply(DONORS, function(d) {
  f   <- donor_files(d)
  sub <- list.dirs(f$gex_dir, recursive = TRUE)
  hit <- sub[file.exists(file.path(sub, "matrix.mtx.gz")) |
             file.exists(file.path(sub, "matrix.mtx"))]
  m <- Seurat::Read10X(hit[1])
  if (is.list(m)) m <- m[["Gene Expression"]]      # a list when the run has several assays
  want <- sel$barcode_raw[sel$donor == sprintf("donor%d", d)]
  m <- m[, intersect(colnames(m), want), drop = FALSE]
  colnames(m) <- paste0(sprintf("donor%d_", d), colnames(m))
  m
})
genes <- Reduce(intersect, lapply(mats, rownames))
expr  <- do.call(cbind, lapply(mats, function(m) m[genes, , drop = FALSE]))
sel   <- sel[sel$barcode %in% colnames(expr), , drop = FALSE]
expr  <- expr[, sel$barcode, drop = FALSE]        # same order as the metadata

so <- Seurat::CreateSeuratObject(counts = expr)
so <- Seurat::NormalizeData(so, verbose = FALSE)
so <- Seurat::FindVariableFeatures(so, nfeatures = N_GENES, verbose = FALSE)   # 2000
so <- Seurat::ScaleData(so, verbose = FALSE)
so <- Seurat::RunPCA(so, npcs = 30, verbose = FALSE)
so <- Seurat::RunUMAP(so, dims = 1:30, verbose = FALSE)

hv         <- Seurat::VariableFeatures(so)
# SPARSE, like every other demo here: normalized single-cell expression is ~90%
# zeros, and densifying this block cost 184 MiB of memory and 4.5 MiB of
# installed package. The class reads it through Matrix::rowMeans/colMeans.
expression <- Seurat::GetAssayData(so, layer = "data")[hv, , drop = FALSE]
expression <- methods::as(expression, "CsparseMatrix")
umap       <- as.data.frame(Seurat::Embeddings(so, "umap"))
colnames(umap) <- c("UMAP_1", "UMAP_2")
```

Only the 2,000 variable genes ship — 33,538 × 12,000 would be a large file for a demo whose point is the receptors.

## 3.9 Step 6 — assembling the `.crb`

A `.crb` is an R6 `Cerebro_v1.3` object written with `saveRDS()`. Building one from scratch means assigning its fields directly; there is no converter to go through.

```r
# Three states, not two: absence from the published table is only evidence of
# absence when the LOCUS was called completely. Table S1 gives donors 1 and 2 a
# single HLA-B allele, so a B-restricted binder call there is undecidable. The
# rule is the package's own (hla_locus_call_state: complete at two copies).
donor_typing_canonical <- CerebroNexus:::hla_normalize_typing(donor_typing)
genotype_key <- paste(donor_typing_canonical$sample, donor_typing_canonical$allele)
# ... locus_complete built per (sample, locus) from hla_locus_call_state ...
restriction_in_genotype <- ifelse(
  paste(sel$donor, sel$dextramer_allele) %in% genotype_key, "yes",
  ifelse(locus_fully_called, "no", "unknown")
)

meta <- data.frame(
  cell_barcode            = sel$barcode,
  sample                  = sel$donor,
  cell_type               = "CD8 T",    # sorted CD8+; declared, never inferred
  # `dextramer_*`, never `antigen` / `restricting_allele`: these are 10x's raw
  # binder calls for a reagent, not validated peptide specificity (S3.5).
  dextramer_antigen       = sel$dextramer_antigen,
  dextramer_peptide       = sel$dextramer_peptide,
  dextramer_allele        = sel$dextramer_allele,
  restriction_in_genotype = restriction_in_genotype,
  stringsAsFactors = FALSE
)
rownames(umap) <- meta$cell_barcode

# the repertoire is a NAMED LIST, one data frame per sample
immune_repertoire <- lapply(split(sel, sel$donor), function(x) {
  data.frame(barcode = x$barcode, CTgene = x$CTgene, CTnt = x$CTnt,
             CTaa = x$CTaa, CTstrict = x$CTstrict, stringsAsFactors = FALSE)
})

crb <- Cerebro_v1.3$new()
crb$expression  <- expression
crb$setMetaData(meta)
crb$projections <- list(umap = umap)
crb$groups <- list(
  sample                  = sort(unique(meta$sample)),
  cell_type               = sort(unique(meta$cell_type)),
  dextramer_antigen       = sort(unique(meta$dextramer_antigen)),
  dextramer_allele        = sort(unique(meta$dextramer_allele)),
  # declared as a group so the cross-reactivity is colourable in the app
  restriction_in_genotype = sort(unique(meta$restriction_in_genotype))
)
crb$immune_repertoire <- immune_repertoire
crb$experiment <- list(
  experiment_name = "Antigen-selected CD8 T cells - real 10x dextramer cohort",
  organism        = "hg",
  date_of_export  = Sys.Date()
)
crb$technical_info <- list(
  observation_unit     = "cell",
  receptor_key         = "v_gene+cdr3",
  tcr_selection        = "antigen-selected",
  tcr_selection_detail = "Cells were sorted for binding to a pooled dCODE dextramer panel ...",
  lineage_column       = "cell_type"
)
crb$addHLATyping(
  donor_typing,
  source_type      = "genotyped",
  typing_method    = "HLA typing published in table S1 of Zhang et al., Sci Adv 2021",
  source_reference = "10x Genomics CD8+ T cells of Healthy Donor 1-4; Zhang et al., Sci Adv 2021, eabf5835"
)

# STAGING, not the shipped path -- S3.10 decides whether this ever becomes OUT.
staged <- paste0(OUT, ".staged")
on.exit(unlink(staged), add = TRUE)
saveRDS(crb, staged, compress = "xz")
```

Note also that `expression` was kept **sparse** in S3.8: a `dgCMatrix`, like
every other demo this package ships.

The four declared contracts and why each one:

| contract | value | why |
|---|---|---|
| `observation_unit` | `cell` | these really are sequenced cells, unlike the bulk demo |
| `receptor_key` | `v_gene+cdr3` | the source identifies a receptor by V **and** CDR3, so split-by-V is the app's default here and a node means what the source means |
| `tcr_selection` | `antigen-selected` | the reagent panel decided which receptors are present; the page prints this above the Associations tables |
| `lineage_column` | `cell_type` | declared, so the app never has to guess which column holds the CD4/CD8 label |

## 3.10 Step 7 — the verification gate

The object is written to a **staging** path. The script then re-reads that file, re-derives the network with the package's own motif core, and **asserts** every number: donor balance, all-paired observations, a sparse expression block aligned to the metadata and the projection, motif thresholds, genotypes equal to table S1, provenance, and the honesty columns. Only then does `file.rename()` publish it.

This is a gate, not a report. An earlier version printed the same numbers *after* saving, so a drifted input still replaced a good demo and still exited 0.

```r
check <- readRDS(staged)          # the STAGED bytes, not the shipped file

stopifnot(
  "donors are not balanced"            = all(table(m$sample) == CELLS_PER_DONOR),
  "expression block is not sparse"     = methods::is(check$expression, "CsparseMatrix"),
  "not every observation is paired"    = all(is_paired(ctaa)),
  "TRB network has collapsed"          = n_nodes > 100 && n_motifs >= 20,
  # sorted canonical ROWS incl. `copy`, not a set of sample+allele pairs: donor4
  # is homozygous A*03:01, so a set comparison cannot see one row go missing
  "HLA typing drifted from table S1"   = identical(hla_canonical_rows(ht),
                                                   hla_canonical_rows(donor_typing_canonical)),
  "HLA typing lost or gained rows"     = nrow(ht) == nrow(donor_typing_canonical),
  "restriction_in_genotype must be yes/no/unknown" =
    all(m$restriction_in_genotype %in% c("yes", "no", "unknown")),
  # both of these must still EXIST: no off-genotype calls would mean the binder
  # calls stopped being raw 10x calls; no unknowns would mean the three-state
  # logic collapsed back into calling missing data a negative
  "off-genotype binding vanished"      = any(m$restriction_in_genotype == "no"),
  "the undecidable calls vanished"     = any(m$restriction_in_genotype == "unknown")
)

# file.rename() RETURNS failure rather than signalling it, and printing
# PUBLISHED over a rename that did nothing is exactly the bug this gate exists
# to stop. Then re-read what actually landed.
if (!file.rename(staged, OUT)) {
  stop("could not publish the staged object -- the shipped file is untouched")
}
published <- readRDS(OUT)
```

Current output, and what the shipped object contains:

```
   cells: 12000 in 4 donors, balanced
   expression: dgCMatrix 2000x12000, 40.2 MiB in memory
   repertoire: 12000 observations, all paired; chains TRA, TRB
   TRB: 3270 unique CDR3 -> 169 nodes in 39 motifs
   TRA: 3189 unique CDR3 -> 396 nodes in 141 motifs
   HLA: 4 donors, 12 alleles, source_type=genotyped
   binder calls vs genotype: 6654 off-genotype, 75 undecidable, of 12000 cells
   PUBLISHED inst/extdata/v1.4/demo_hla_tcr_dextramer.crb (5.2 MB)

   groups:   sample, cell_type, dextramer_antigen, dextramer_allele,
             restriction_in_genotype
   metadata: cell_barcode, sample, cell_type, dextramer_antigen,
             dextramer_peptide, dextramer_allele, restriction_in_genotype
   restriction_in_genotype: yes 5271 / no 6654 / unknown 75
             (the 75 are donor1 HLA-B*08:01 calls: table S1 publishes only one
              HLA-B allele for that donor, so the second copy could be it)
   23 antigens; 6 reagent restrictions present on cells
```

A narrated walkthrough of the same pipeline, showing the data before and after each transformation, is `vignettes/hla_tcr_antigen_selected.Rmd`.

---

# 4. Known problems

## 4.1 Inferring the genotypes from binding was wrong

Worth recording, because the mistake is seductive: a cell can only bind a dextramer restricted by an allele its donor carries, so the binding profile *ought* to reveal the haplotype.
An earlier build did exactly that, requiring an allele to account for ≥ 200 cells **and** ≥ 10 % of a donor's antigen-specific cells — a cut that looked careful and reproduced the per-donor profile of the paper's own quality-controlled call set.

Against table S1 it still fails, for three of the four donors:

| donor | inferred from binding | published (table S1) |
|---|---|---|
| 1 | A\*02:01, A\*03:01, A\*11:01 | A\*02:01, A\*11:01, B\*35:01 — **no A\*03:01** |
| 2 | A\*02:01, A\*03:01, B\*08:01 | A\*02:01, **A\*01:01**, B\*08:01 — **no A\*03:01** |
| 3 | A\*03:01 | A\*24:02, A\*29:02, B\*35:02, B\*44:03 — **no A\*03:01 at all** |
| 4 | A\*03:01, A\*11:01 | A\*03:01 (homozygous), B\*07:02, B\*57:01 — **no A\*11:01** |

Donor 3 settles it: **25,674 cells — 92.8 % of its antigen-specific cells — bound A\*03:01-restricted dextramers, and it carries no A\*03:01.**
No threshold separates cross-reactivity at that scale.
The general lesson outlives the specific fix: an inference calibrated against a *derived* data set can look well-behaved and still be wrong about the thing it claims to measure.

## 4.2 What is still declared: the selection

With published genotypes the earlier circularity is gone — a donor's alleles were measured independently of these cells, so a carrier / non-carrier contrast here is a real comparison.

What remains true, and is declared, is that the **repertoire is antigen-selected**: the reagent panel decided which receptors are present, so this is not an unbiased sample of the donors' repertoires. That lives in `technical_info$tcr_selection` with the detail in `tcr_selection_detail`, and the app prints it above the Associations tables. It is a statement about how the cells were chosen, not about the genotypes.

## 4.3 The paper's curated table cannot be joined

`abf5835_data_file_s1.csv` (a separate supplementary download) is in ways a better input: it is the authors' ICON-filtered call set, one row per cell with paired αβ and V/J already parsed, plus real phenotype labels (Tem / Tcm / Tpm / Temra / Naïve) and a fifth donor.

It is not used because its barcodes carry the paper's own cross-library aggregation index — `D1.AAACCTGAGATTACCC-20` — while the per-donor matrices use `-1`.
Of donor 1's 7,573 curated cells, **11** match. No mapping key is published.
Using it would mean giving up the transcriptome, and "real **single-cell**" is exactly what this demo exists to show. It served instead as the independent check that exposed §4.1.

## 4.4 Smaller limitations

- **CD8 only.** Every cell is a sorted CD8+ T cell, so `cell_type` has one level, the typing is Class I only, and the Class I × Class II **pair scope is hidden** here — `hla_pair_available()` gates it, so the control simply does not appear rather than appearing broken.
- **No phenotype detail.** The Tem / Tcm / … labels live in the un-joinable curated table (§4.3), so `cell_type` is the flat `CD8 T`.
- **7.8 MB**, the largest demo in the package. Lower `CELLS_PER_DONOR` in the build script to trade network richness for size.
- **One panel for all donors**, so panel composition carries no donor-specific HLA information — only binding does, and §4.1 is why that is not enough.

---

# 5. `demo_hla_tcr_bulk.crb` — removed, pipeline kept

**No longer shipped** (see §1). The build script still runs and is the reference for bringing a bulk cohort in; the user-facing version of this workflow is the *HLA Associations on bulk TCRβ* vignette.

Real public TCRβ chains with each donor's **real** HLA genotype: the Emerson 2017 / DeWitt 2018 cohort, Zenodo record 1248193 (~349 MB, downloaded on first run into `data-raw/pubtcrs/`).

Bulk, so each row is a *(donor, clonotype)* analysis unit rather than a cell: no expression, no projection, and the lineage MHC context is Unknown by design. Its receptors were selected *using* the published HLA association, so it declares `tcr_selection = "association-conditioned"` — a positive control for the Associations tables, not independent evidence.

```bash
Rscript data-raw/build_hla_tcr_bulk_demo.R
```

Worth knowing if you are testing carrier logic: this cohort carries **single-copy** calls, so the loose and strict carrier definitions genuinely differ on it. The shipped dextramer demo cannot exercise that distinction (all its calls are double-copy or 4-digit), so build this one when you need to.

---

# 6. `demo_hla_tcr_synthetic.crb` — removed, pipeline kept

**No longer shipped** (see §1). Kept because it is still the fastest way to get a dense, fully-controlled network in front of the page when developing.

Fully simulated: expression, projection, cell types, CDR3s and donor genotypes, 30 donors × 167 cells. Self-contained, no download.

```bash
Rscript data-raw/build_hla_tcr_demo.R
```

The motif families and their HLA associations are **designed in**, because an unselected real repertoire renders a near-empty network (§2). It declares `tcr_selection = "synthetic"`, the page's hardest disclosure — use it to see the page work, never to read biology off it. The build asserts that the recovered motif sizes match the design and fails rather than shipping a drifted fixture.

---

# 7. Rebuilding everything

Only the first line rebuilds something the package ships. The other two write
`.crb` files into `inst/extdata/v1.4/` that are **not** tracked or installed — if
you run them, `git status` will show untracked files you probably want to delete
again.

```bash
cd "$(git rev-parse --show-toplevel)"

Rscript data-raw/build_hla_tcr_dextramer_demo.R   # SHIPPED; ~1.6 GB on first run
Rscript data-raw/build_hla_tcr_demo.R             # not shipped: synthetic fixture, self-contained
Rscript data-raw/build_hla_tcr_bulk_demo.R        # not shipped: real bulk, ~349 MB on first run
```

Build-time packages: `Seurat`, `scRepertoire`, `Matrix`, `igraph` — `Seurat` and `scRepertoire` are **not** runtime dependencies of the package.
Every script re-derives the motif network from the `.crb` it just wrote and prints the measured result, so a drifted build is loud rather than silent.
