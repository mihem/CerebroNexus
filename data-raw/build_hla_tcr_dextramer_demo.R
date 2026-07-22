#!/usr/bin/env Rscript
# ============================================================================
# Build the REAL single-cell antigen-selected TCR demo (.crb)
#   -> inst/extdata/v1.4/demo_hla_tcr_dextramer.crb
# ============================================================================
# This is the ONLY HLA demo the package ships. It replaced two earlier ones
# (2026-07-21): a fully fabricated fixture and a real BULK TCRb cohort. Neither
# was both real and single-cell, and cerebroAppLite is a single-cell app. Their
# build scripts are kept in data-raw/ as the reproducibility record --
# build_hla_tcr_demo.R and build_hla_tcr_bulk_demo.R -- but what they write into
# inst/extdata/v1.4/ is no longer tracked or installed. See data-raw/hla.md S1.
#
# Because this demo is sorted CD8+ T cells, its typing is CLASS I ONLY, so the
# Class I x Class II pair scope stays hidden on it (hla_pair_available() gates
# the control). That is a real gap, stated rather than papered over.
#
# WHY THIS DATA SET EXISTS
# ------------------------
# The honest objection to the motif page is: "if the network is only legible on
# synthetic data, what is the feature for?" The answer is that a CDR3 Hamming-1
# network needs a repertoire that has been ANTIGEN-SELECTED. An unselected
# polyclonal repertoire is sparse in CDR3 space -- pairs at distance 1 are rare,
# and more cells do not fix it. A selected repertoire converges: many donors
# arrive at near-identical CDR3s for the same epitope (public / convergent
# recombination), which is exactly what the network draws.
#
# Measured on this source with THIS PACKAGE's own motif core (not a claim, a
# number -- the verification block at the bottom re-measures it on the shipped
# object):
#
#   all cells, unselected         26,449 unique CDR3  -> trips the size guard
#   dextramer-binding cells        2,910 unique CDR3  ->  308 nodes / 75 motifs
#   one epitope: Flu-MP GILGFVFTL    267 unique CDR3  ->  121 nodes /  7 motifs
#
# The Flu M1 line is the point: 45% of the CDR3s seen against one immunodominant
# epitope collapse into SEVEN families. That is real measured convergence.
#
# SOURCE
# ------
# 10x Genomics, "CD8+ T cells of Healthy Donor 1-4" (2019), the dextramer /
# Immune Map experiment published as Zhang et al., Sci Adv 7:eabf5835 (2021).
# CD8+ T cells from four HLA-haplotyped healthy donors were stained with a pool
# of dCODE dextramers, sorted, and run on 10x 5' immune profiling: paired ab TCR
# + transcriptome + surface protein + per-cell dextramer counts.
# License: CC BY 4.0.
#
# WHAT IS REAL AND WHAT IS DERIVED  (read this before trusting any number)
# -----------------------------------------------------------------------
#   REAL, measured:
#     * the cells, their transcriptomes, their surface protein counts
#     * the paired TCR alpha/beta contigs -- V/J gene and CDR3 amino acids
#     * which dextramer each cell bound (10x's own binarized calls)
#     * the HLA restriction of each dextramer: it is a property of the reagent,
#       stated in the reagent's name (A0201_GILGFVFTL_Flu-MP -> HLA-A*02:01)
#
#     * the donors' HLA GENOTYPES, transcribed from table S1 of the paper's
#       supplementary PDF (inline as DONOR_HLA below, with citation and URL).
#       They were measured independently of these cells, so they can carry an
#       association claim.
#
#   NOT established -- and this is the important one:
#     * that a cell which bound a reagent is genuinely SPECIFIC for that
#       reagent's peptide, or that the reagent's allele is the one presenting
#       it in that donor. 10x's binarized flags are raw binder calls, and
#       dextramer staining is famously cross-reactive at this scale. Measured
#       on the shipped cells: only 5,271 of 12,000 bound a reagent restricted
#       by an allele their donor actually carries. 6,654 bound one the donor
#       demonstrably lacks, and 75 cannot be decided because table S1 leaves
#       that donor's HLA-B half-called (the per-donor counts are printed and
#       asserted by the verification block).
#       donor3 is the extreme case -- nearly all of its cells bind reagents
#       restricted by alleles it does not carry.
#       Therefore the per-cell columns are named `dextramer_*`, never
#       "antigen-specific", and a `restriction_in_genotype` column ships beside
#       them so the noise is visible in the app rather than described in a
#       footnote. These calls must NOT be read as peptide-level specificity,
#       and no HLA-association claim rests on them: the associations use the
#       published genotypes, which are independent of binding.
#
# WHY THE GENOTYPES ARE NOT INFERRED FROM BINDING
# -----------------------------------------------
# An earlier version of this script derived each donor's alleles from which
# dextramers their cells bound. That was circular AND wrong. Circular, because a
# donor would be a carrier of HLA-X precisely because their cells bound an
# X-restricted reagent, and the motifs come from those same cells. Wrong,
# because binding is simply not genotype: donor3 has 25,674 cells -- 92.8% of
# its antigen-specific cells -- binding A*03:01-restricted dextramers, and
# table S1 shows it carries no A*03:01 at all (it is A*24:02 / A*29:02). No
# threshold separates cross-reactivity at that scale. The published table is
# used instead, and the demo can therefore ship real genotypes.
#
# WHAT REMAINS DECLARED
# ---------------------
# The repertoire is still ANTIGEN-SELECTED: cells were sorted for dextramer
# binding, so this is not an unbiased repertoire and the page says so through
# `technical_info$tcr_selection`. That is a statement about how the cells were
# chosen, not about the genotypes, which are independent.
#
# USAGE
#   Rscript data-raw/build_hla_tcr_dextramer_demo.R
# Raw downloads are cached in data-raw/vdj_10x_dextramer/ (gitignored, ~1.6 GB);
# only the built .crb ships.
# ============================================================================

suppressPackageStartupMessages({
  library(Matrix)
  library(Seurat)
  library(scRepertoire)
})
devtools::load_all(".", quiet = TRUE)

set.seed(20260721)

## ---- Configuration ------------------------------------------------------ ##
DONORS <- 1:4
CACHE <- "data-raw/vdj_10x_dextramer"
OUT <- "inst/extdata/v1.4/demo_hla_tcr_dextramer.crb"
BASE <- "https://cf.10xgenomics.com/samples/cell-vdj/3.0.2"

## The donors' REAL genotypes: table S1 ("HLA haplotypes of the healthy donors")
## of the paper's supplementary PDF, transcribed by hand. Kept inline so this
## script is self-contained -- it is 14 rows, and a separate file would only be
## one more thing to keep in step.
##
##   Supplement (Figs. S1-S11, Tables S1-S5), ~6 MB:
##   https://www.science.org/doi/suppl/10.1126/sciadv.abf5835/suppl_file/abf5835_sm.pdf
##   (the link printed inside the paper, advances.sciencemag.org/.../DC1, is dead:
##    that domain was retired when Science migrated to science.org)
##
## Table S1 gives two HLA-A and two HLA-B alleles per donor, "na" where the paper
## reports none -- which is why donors 1 and 2 contribute a single B allele.
## Donor 4 is homozygous A*03:01. `copy` is 1 or 2 within a locus, matching the
## canonical HLA table this package stores.
##
## This matters more than it looks. An earlier version of this script inferred
## the genotypes from which dextramers each donor's cells bound, and that
## inference was WRONG: donor3 has 25,674 cells (92.8% of its specific cells)
## binding A*03:01-restricted dextramers and is not an A*03:01 carrier at all --
## it is A*24:02 / A*29:02. Binding is not genotype, and cross-reactivity is far
## larger than a threshold can separate. The published table is used instead.
DONOR_HLA <- read.csv(
  text = "donor,copy,allele
donor1,1,HLA-A*02:01
donor1,2,HLA-A*11:01
donor1,1,HLA-B*35:01
donor2,1,HLA-A*02:01
donor2,2,HLA-A*01:01
donor2,1,HLA-B*08:01
donor3,1,HLA-A*24:02
donor3,2,HLA-A*29:02
donor3,1,HLA-B*35:02
donor3,2,HLA-B*44:03
donor4,1,HLA-A*03:01
donor4,2,HLA-A*03:01
donor4,1,HLA-B*07:02
donor4,2,HLA-B*57:01",
  stringsAsFactors = FALSE
)

## Shipped size. The demo must stay a few MB, so the antigen-selected cells are
## subsampled per donor and the matrix is cut to informative genes.
CELLS_PER_DONOR <- 3000L
N_GENES <- 2000L

## ---- 1. Download (on demand) -------------------------------------------- ##
dir.create(CACHE, showWarnings = FALSE, recursive = TRUE)

## These are big files (the expression matrix is ~300 MB) on a server that is
## not always fast. R's default 60 s timeout truncates them silently-ish, and a
## truncated file left at `dest` would be treated as a completed download on the
## next run, so: download to a .part file, resume if interrupted, and only move
## it into place once curl reports success.
options(timeout = max(getOption("timeout"), 3600))

fetch <- function(url, dest) {
  if (file.exists(dest)) {
    return(invisible(dest))
  }
  part <- paste0(dest, ".part")
  cat("  downloading", basename(dest), "...\n")
  ok <- FALSE
  if (nzchar(Sys.which("curl"))) {
    status <- system2(
      "curl",
      c(
        "-fL",
        "--retry",
        "5",
        "--retry-delay",
        "3",
        "-C",
        "-",
        "-o",
        shQuote(part),
        shQuote(url)
      ),
      stdout = FALSE,
      stderr = FALSE
    )
    ok <- identical(status, 0L)
  } else {
    ok <- tryCatch(
      {
        utils::download.file(url, part, mode = "wb", quiet = TRUE)
        TRUE
      },
      error = function(e) FALSE
    )
  }
  if (!ok || !file.exists(part) || file.info(part)$size == 0) {
    unlink(part)
    stop("download failed: ", url, call. = FALSE)
  }
  file.rename(part, dest)
  invisible(dest)
}

donor_files <- function(d) {
  stem <- sprintf("vdj_v1_hs_aggregated_donor%d", d)
  list(
    contigs = file.path(CACHE, sprintf("%s_all_contig_annotations.csv", stem)),
    binarized = file.path(CACHE, sprintf("%s_binarized_matrix.csv", stem)),
    gex_tar = file.path(
      CACHE,
      sprintf("%s_filtered_feature_bc_matrix.tar.gz", stem)
    ),
    gex_dir = file.path(CACHE, sprintf("%s_gex", stem)),
    url_stem = sprintf("%s/%s/%s", BASE, stem, stem)
  )
}

cat("== 1. raw data ==\n")
for (d in DONORS) {
  f <- donor_files(d)
  fetch(paste0(f$url_stem, "_all_contig_annotations.csv"), f$contigs)
  fetch(paste0(f$url_stem, "_binarized_matrix.csv"), f$binarized)
  fetch(paste0(f$url_stem, "_filtered_feature_bc_matrix.tar.gz"), f$gex_tar)
  if (!dir.exists(f$gex_dir)) {
    dir.create(f$gex_dir, showWarnings = FALSE, recursive = TRUE)
    utils::untar(f$gex_tar, exdir = f$gex_dir)
  }
}

## ---- 2. TCR: contigs -> scRepertoire CT* columns ------------------------- ##
## The app reads receptors through hla_parse_ir_segments(), whose contract is
## the scRepertoire shape: `barcode`, `CTgene`, `CTaa`. The binarized matrix
## carries CDR3s but NO V/J gene, and V is required (it is half of this source's
## receptor key), so the contigs are the input and combineTCR() does the join.
cat("== 2. TCR contigs -> clonotypes ==\n")
tcr_by_donor <- lapply(DONORS, function(d) {
  contigs <- read.csv(donor_files(d)$contigs, stringsAsFactors = FALSE)
  # combineTCR wants one data frame per sample and productive contigs only.
  contigs <- contigs[
    contigs$productive %in%
      c("True", "TRUE", TRUE) &
      contigs$chain %in% c("TRA", "TRB"),
    ,
    drop = FALSE
  ]
  out <- scRepertoire::combineTCR(
    list(contigs),
    samples = sprintf("donor%d", d),
    filterMulti = TRUE
  )[[1]]
  # combineTCR prefixes the barcode with the sample id; keep the raw 10x barcode
  # too so the dextramer table and the expression matrix can be joined on it.
  out$barcode_raw <- sub("^donor[0-9]+_", "", out$barcode)
  out$donor <- sprintf("donor%d", d)
  out
})
names(tcr_by_donor) <- sprintf("donor%d", DONORS)
cat(sprintf(
  "   cells with a clonotype: %s\n",
  paste(
    sprintf("d%d=%d", DONORS, vapply(tcr_by_donor, nrow, integer(1))),
    collapse = " "
  )
))

## ---- 3. Dextramer binder calls (NOT validated specificity) --------------- ##
## Every dextramer column is named <allele>_<peptide>_<antigen>_binder, e.g.
## A0201_GILGFVFTL_Flu-MP_Influenza_binder. The allele prefix is the reagent's
## own HLA restriction -- a real, published property OF THE REAGENT. What is
## real here is therefore "this cell was called a binder of this reagent", not
## "this cell is specific for this peptide, presented by this allele in this
## donor". The distinction is not pedantic: see the cross-reactivity numbers in
## the header. Everything below is named `dextramer_*` for that reason.
## Columns containing "NR(" are 10x's negative controls and are never evidence.
cat("== 3. dextramer binder calls ==\n")

allele_of <- function(col) {
  a <- sub("^([ABC])([0-9]{2})([0-9]{2})_.*", "\\1*\\2:\\3", col)
  paste0("HLA-", a)
}
peptide_of <- function(col) sub("^[ABC][0-9]{4}_([A-Z]+)_.*", "\\1", col)
antigen_of <- function(col) {
  x <- sub("^[ABC][0-9]{4}_[A-Z]+_", "", col)
  sub("_binder$", "", x)
}

dex_by_donor <- lapply(DONORS, function(d) {
  b <- read.csv(
    donor_files(d)$binarized,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dex <- grep("^[ABC][0-9]{4}_", colnames(b), value = TRUE)
  dex <- grep("NR\\(", dex, value = TRUE, invert = TRUE)
  hits <- as.matrix(b[, dex, drop = FALSE]) == "True"
  hits[is.na(hits)] <- FALSE
  n_hit <- rowSums(hits)
  # A cell is assigned to ONE specificity: the dextramer it bound. Cells binding
  # several are dropped rather than guessed at -- an ambiguous specificity would
  # put a cell in the wrong HLA context, which is the one thing this page must
  # not do.
  keep <- n_hit == 1L
  idx <- max.col(hits, ties.method = "first")
  data.frame(
    barcode_raw = b$barcode,
    donor = sprintf("donor%d", d),
    single_binder = keep,
    dextramer = ifelse(keep, dex[idx], NA_character_),
    stringsAsFactors = FALSE
  )
})
dex_all <- do.call(rbind, dex_by_donor)
dex_all$dextramer_antigen <- ifelse(
  is.na(dex_all$dextramer),
  NA_character_,
  antigen_of(dex_all$dextramer)
)
dex_all$dextramer_peptide <- ifelse(
  is.na(dex_all$dextramer),
  NA_character_,
  peptide_of(dex_all$dextramer)
)
dex_all$dextramer_allele <- ifelse(
  is.na(dex_all$dextramer),
  NA_character_,
  allele_of(dex_all$dextramer)
)
cat(sprintf(
  "   cells binding exactly one dextramer: %d of %d\n",
  sum(dex_all$single_binder),
  nrow(dex_all)
))

## Donor genotype, read off the published table (see DONOR_HLA above).
hla_donor_typing <- function(donors) {
  tab <- DONOR_HLA[DONOR_HLA$donor %in% donors, , drop = FALSE]
  data.frame(
    sample = tab$donor,
    donor_id = tab$donor,
    allele = tab$allele,
    copy = as.integer(tab$copy),
    stringsAsFactors = FALSE
  )
}
donor_typing <- hla_donor_typing(sprintf("donor%d", DONORS))
cat("   published genotypes (table S1):\n")
for (dn in unique(donor_typing$sample)) {
  cat(sprintf(
    "     %s: %s\n",
    dn,
    paste(donor_typing$allele[donor_typing$sample == dn], collapse = ", ")
  ))
}

## ---- 4. Cell selection: the dextramer-selected repertoire ---------------- ##
## Only cells that (a) carry a clonotype with BOTH chains, and (b) bound exactly
## one dextramer. The dextramer sort IS the selection that makes the motif
## network legible, and it is the data set's defining property, not a
## convenience. The paired requirement is stricter than "has a CTaa": the
## documentation calls this demo paired alpha/beta, so the data has to be.
cat("== 4. cell selection ==\n")
tcr_all <- do.call(
  rbind,
  lapply(tcr_by_donor, function(x) {
    x[, c(
      "barcode",
      "barcode_raw",
      "donor",
      "CTgene",
      "CTnt",
      "CTaa",
      "CTstrict"
    )]
  })
)
sel <- merge(
  tcr_all,
  dex_all[
    dex_all$single_binder,
    c(
      "barcode_raw",
      "donor",
      "dextramer",
      "dextramer_antigen",
      "dextramer_peptide",
      "dextramer_allele"
    )
  ],
  by = c("barcode_raw", "donor")
)

## scRepertoire writes CTaa as "<alpha>_<beta>" and puts the literal string NA
## on a side it could not resolve, so "has a CTaa" is NOT "is paired". Require
## two non-empty, non-NA sides. This runs BEFORE the per-donor subsample (which
## is deferred to S5, after the expression join), so dropping the half-chain
## cells costs no donor balance while enough paired cells remain.
is_paired <- function(ctaa) {
  parts <- strsplit(ifelse(is.na(ctaa), "", ctaa), "_", fixed = TRUE)
  vapply(
    parts,
    function(p) {
      length(p) == 2L && all(nzchar(p)) && !any(p %in% c("NA", "None"))
    },
    logical(1)
  )
}
n_before <- nrow(sel)
sel <- sel[is_paired(sel$CTaa), , drop = FALSE]
cat(sprintf(
  "   paired alpha/beta: %d of %d clonotypes (%d dropped for a missing chain)\n",
  nrow(sel),
  n_before,
  n_before - nrow(sel)
))

cat(sprintf(
  "   eligible dextramer-selected cells: %d (%s)\n",
  nrow(sel),
  paste(
    sprintf(
      "d%s=%d",
      sub("donor", "", names(table(sel$donor))),
      table(sel$donor)
    ),
    collapse = " "
  )
))

## ---- 5. Expression + projection ----------------------------------------- ##
## Real measured transcriptomes for exactly the kept cells. Subset FIRST, then
## run the standard Seurat pipeline, so nothing is computed on cells that are
## thrown away. The per-donor subsample happens HERE, after the expression join,
## so the shipped object is exactly CELLS_PER_DONOR per donor -- balance the
## verification gate asserts. Subsampling before the join left it to chance.
cat("== 5. expression + UMAP ==\n")
mats <- lapply(DONORS, function(d) {
  f <- donor_files(d)
  sub <- list.dirs(f$gex_dir, recursive = TRUE)
  hit <- sub[
    file.exists(file.path(sub, "matrix.mtx.gz")) |
      file.exists(file.path(sub, "matrix.mtx"))
  ]
  m <- Seurat::Read10X(hit[1])
  if (is.list(m)) {
    m <- m[["Gene Expression"]]
  }
  want <- sel$barcode_raw[sel$donor == sprintf("donor%d", d)]
  m <- m[, intersect(colnames(m), want), drop = FALSE]
  colnames(m) <- paste0(sprintf("donor%d_", d), colnames(m))
  m
})
genes <- Reduce(intersect, lapply(mats, rownames))
expr <- do.call(cbind, lapply(mats, function(m) m[genes, , drop = FALSE]))
sel <- sel[sel$barcode %in% colnames(expr), , drop = FALSE]

## Subsample per donor, deterministically, now that every remaining cell is
## known to have both a paired clonotype and a transcriptome.
per_donor <- table(sel$donor)
stopifnot(
  "a donor has too few eligible cells to ship a balanced demo" = all(
    per_donor >= CELLS_PER_DONOR
  )
)
keep_rows <- unlist(lapply(split(seq_len(nrow(sel)), sel$donor), function(ix) {
  sort(sample(ix, CELLS_PER_DONOR))
}))
sel <- sel[sort(keep_rows), , drop = FALSE]
expr <- expr[, sel$barcode, drop = FALSE]
cat(sprintf(
  "   matrix: %d genes x %d cells (%d per donor)\n",
  nrow(expr),
  ncol(expr),
  CELLS_PER_DONOR
))

so <- Seurat::CreateSeuratObject(counts = expr)
so <- Seurat::NormalizeData(so, verbose = FALSE)
so <- Seurat::FindVariableFeatures(so, nfeatures = N_GENES, verbose = FALSE)
so <- Seurat::ScaleData(so, verbose = FALSE)
so <- Seurat::RunPCA(so, npcs = 30, verbose = FALSE)
so <- Seurat::RunUMAP(so, dims = 1:30, verbose = FALSE)

## Keep the block SPARSE. Normalized single-cell expression is ~90% zeros, and
## every other demo this package ships is a dgCMatrix; densifying this one cost
## 184 MiB of session memory and 4.5 MiB of installed package for nothing. The
## class reads the block through Matrix::rowMeans/colMeans, which are sparse-
## aware, so no downstream code needs to change.
hv <- Seurat::VariableFeatures(so)
expression <- Seurat::GetAssayData(so, layer = "data")[hv, , drop = FALSE]
expression <- methods::as(expression, "CsparseMatrix")
umap <- as.data.frame(Seurat::Embeddings(so, "umap"))
colnames(umap) <- c("UMAP_1", "UMAP_2")

## ---- 6. Assemble the Cerebro object ------------------------------------- ##
cat("== 6. assemble .crb ==\n")
## The evidence column that keeps the binder calls honest inside the app: is the
## bound reagent's restriction actually one of the alleles this donor carries,
## per the published table? A "no" is not a data error -- it is dextramer
## cross-reactivity, and it is common here. Shipping it as a colourable group
## means a user meets the caveat by looking at the UMAP, not by reading a
## footnote they will skip.
##
## THREE states, not two. Absence from the published table is only evidence of
## absence when the LOCUS was called completely: table S1 reports a single
## HLA-B allele for donors 1 and 2, so their second B copy is unknown and a
## B-restricted reagent they bind cannot be called off-genotype. Calling it "no"
## would manufacture a confirmed negative out of missing data -- exactly the
## false-negative bias hla_allele_status_by_unit() refuses to take. The rule
## used here is the package's own (hla_locus_call_state: a locus is complete at
## two copies), applied to the canonical form of the same table the object ships.
donor_typing_canonical <- cerebroAppLite:::hla_normalize_typing(donor_typing)
genotype_key <- paste(
  donor_typing_canonical$sample,
  donor_typing_canonical$allele
)
donor_samples <- unique(donor_typing_canonical$sample)
locus_complete <- do.call(
  rbind,
  lapply(unique(donor_typing_canonical$locus), function(lc) {
    st <- cerebroAppLite:::hla_locus_call_state(
      donor_typing_canonical,
      donor_samples,
      lc
    )
    data.frame(
      sample = st$sample,
      locus = lc,
      complete = st$call_state == "complete",
      stringsAsFactors = FALSE
    )
  })
)
# hla_allele_locus() is scalar (it guards with a bare `if (is.na(allele))`), so
# it is mapped rather than handed the column.
sel_locus <- vapply(
  sel$dextramer_allele,
  cerebroAppLite:::hla_allele_locus,
  character(1),
  USE.NAMES = FALSE
)
sel_complete <- locus_complete$complete[
  match(
    paste(sel$donor, sel_locus),
    paste(locus_complete$sample, locus_complete$locus)
  )
]
# NA from the match counts as not-complete, for the same reason a half-called
# locus does: no information is not evidence of absence.
locus_fully_called <- !is.na(sel_complete) & sel_complete
restriction_in_genotype <- ifelse(
  paste(sel$donor, sel$dextramer_allele) %in% genotype_key,
  "yes",
  # Not carried and the locus was fully called -> a real off-genotype call.
  # Not carried but the locus is half-called -> unknown, not "no".
  ifelse(locus_fully_called, "no", "unknown")
)

meta <- data.frame(
  cell_barcode = sel$barcode,
  sample = sel$donor,
  # Every cell here is a sorted CD8+ T cell. Declared, not inferred: see
  # technical_info$lineage_column below.
  cell_type = "CD8 T",
  # `dextramer_*`, never `antigen`/`restricting_allele`: these are 10x's raw
  # binder calls for a reagent, not validated peptide specificity. See S3.
  dextramer_antigen = sel$dextramer_antigen,
  dextramer_peptide = sel$dextramer_peptide,
  dextramer_allele = sel$dextramer_allele,
  restriction_in_genotype = restriction_in_genotype,
  stringsAsFactors = FALSE
)
rownames(umap) <- meta$cell_barcode

cat(sprintf(
  "   reagent restriction vs the donor's published genotype: yes %d / no %d / unknown %d of %d cells\n",
  sum(restriction_in_genotype == "yes"),
  sum(restriction_in_genotype == "no"),
  sum(restriction_in_genotype == "unknown"),
  nrow(meta)
))
for (dn in sort(unique(meta$sample))) {
  ix <- meta$sample == dn
  st <- meta$restriction_in_genotype[ix]
  cat(sprintf(
    "     %s: yes %d / no %d / unknown %d of %d\n",
    dn,
    sum(st == "yes"),
    sum(st == "no"),
    sum(st == "unknown"),
    sum(ix)
  ))
}

immune_repertoire <- lapply(split(sel, sel$donor), function(x) {
  data.frame(
    barcode = x$barcode,
    CTgene = x$CTgene,
    CTnt = x$CTnt,
    CTaa = x$CTaa,
    CTstrict = x$CTstrict,
    stringsAsFactors = FALSE
  )
})

crb <- Cerebro_v1.3$new()
crb$expression <- expression
crb$setMetaData(meta)
crb$projections <- list(umap = umap)
crb$groups <- list(
  sample = sort(unique(meta$sample)),
  cell_type = sort(unique(meta$cell_type)),
  dextramer_antigen = sort(unique(meta$dextramer_antigen)),
  dextramer_allele = sort(unique(meta$dextramer_allele)),
  restriction_in_genotype = sort(unique(meta$restriction_in_genotype))
)
crb$immune_repertoire <- immune_repertoire
crb$experiment <- list(
  experiment_name = "Antigen-selected CD8 T cells - real 10x dextramer cohort",
  organism = "hg",
  date_of_export = Sys.Date()
)
crb$technical_info <- list(
  observation_unit = "cell",
  # This source identifies a receptor by V gene + CDR3, like the bulk demo.
  receptor_key = "v_gene+cdr3",
  tcr_selection = "antigen-selected",
  tcr_selection_detail = paste(
    "Cells were sorted for binding to a pooled dCODE dextramer panel, so this",
    "repertoire is antigen-SELECTED -- which is exactly why its motif network",
    "is legible where an unselected repertoire's is not. It is therefore not an",
    "unbiased sample of the donors' repertoires: which receptors are present",
    "was decided by the panel. The donor HLA genotypes are the published ones",
    "(table S1 of the source paper), measured independently of these cells, so",
    "they are not circular with the selection -- but that is a narrower claim",
    "than it sounds. Independent genotypes remove circularity; they do not",
    "remove ASCERTAINMENT: the repertoire being compared was itself captured by",
    "the panel, the panel's reagents are restricted by particular alleles, and",
    "there are four donors, so donor and panel remain confounded with genotype.",
    "Read a carrier contrast here as suggestive, not as a test.",
    "IMPORTANT: the per-cell dextramer_* columns are 10x's RAW BINDER CALLS for",
    "a reagent, not validated peptide specificity. Dextramer staining is",
    "strongly cross-reactive here -- for most cells the bound reagent's HLA",
    "restriction is not even among the alleles that donor carries (see the",
    "restriction_in_genotype column, and colour the projection by it). Treat",
    "them as a reagent label, not as biology, and do not read the HLA",
    "associations as being driven by them."
  ),
  # Declared, so the app never has to guess which column holds the lineage.
  lineage_column = "cell_type"
)
crb$addHLATyping(
  donor_typing,
  source_type = "genotyped",
  typing_method = "HLA typing published in table S1 of Zhang et al., Sci Adv 2021",
  source_reference = "10x Genomics CD8+ T cells of Healthy Donor 1-4; Zhang et al., Sci Adv 2021, eabf5835"
)

## Write to a STAGING path first. Section 7 below is a gate, not a report: if
## anything it asserts fails, the script stops and the shipped .crb is left
## exactly as it was. Publishing before verifying would mean a drifted source or
## a broken input silently replaces a good demo and still exits 0.
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
staged <- paste0(OUT, ".staged")
on.exit(unlink(staged), add = TRUE)
saveRDS(crb, staged, compress = "xz")
cat(sprintf("   staged %.1f MB\n", file.info(staged)$size / 1024^2))

## ---- 7. Verification gate (measured on the object about to ship) --------- ##
## Everything above is intent. What matters is what the object actually
## produces, so these numbers are re-measured on the staged file with the
## package's OWN core, and every one of them is an assertion. Thresholds are set
## meaningfully below the measured values: they are drift detectors, not a
## transcript of today's run.
cat("== 7. verification gate (measured on the staged object) ==\n")
check <- readRDS(staged)

## Canonical rows of a typing table, ordered, for comparison by VALUE. Includes
## `copy` and keeps duplicates, which is the whole point: donor4 is homozygous
## A*03:01, so its two rows are identical apart from `copy` and a set-based
## comparison cannot see one of them go missing.
HLA_ROW_COLS <- c("sample", "donor_id", "locus", "allele", "copy")
hla_canonical_rows <- function(typing) {
  ## Fixed column set, and it must be complete on both sides: intersecting with
  ## whatever each table happens to carry would let a missing `copy` column make
  ## the comparison pass by comparing less.
  stopifnot(
    "typing table is missing a canonical column" = all(
      HLA_ROW_COLS %in% colnames(typing)
    )
  )
  df <- typing[, HLA_ROW_COLS, drop = FALSE]
  df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  df <- df[do.call(order, df), , drop = FALSE]
  rownames(df) <- NULL
  df
}

## -- shape and donor balance
m <- check$getMetaData()
stopifnot(
  "meta data lost rows" = nrow(m) == nrow(meta),
  "expected 4 donors" = length(unique(m$sample)) == length(DONORS),
  "donors are not balanced" = all(table(m$sample) == CELLS_PER_DONOR),
  "projection is not aligned to the cells" = identical(
    rownames(check$projections$umap),
    m$cell_barcode
  )
)
cat(sprintf("   cells: %d in %d donors, balanced\n", nrow(m), length(DONORS)))

## -- the expression block must stay sparse (a dense one is a 25x size regression)
stopifnot(
  # methods::is(), not inherits(): the block is an S4 dgCMatrix.
  "expression block is not sparse" = methods::is(
    check$expression,
    "CsparseMatrix"
  ),
  "expression is not aligned to the cells" = identical(
    colnames(check$expression),
    m$cell_barcode
  )
)
cat(sprintf(
  "   expression: %s %dx%d, %.1f MiB in memory\n",
  class(check$expression)[1],
  nrow(check$expression),
  ncol(check$expression),
  as.numeric(utils::object.size(check$expression)) / 1024^2
))

## -- every observation is paired alpha/beta, because the docs say so
ir <- check$getImmuneRepertoire()
ctaa <- unlist(lapply(ir, function(x) x$CTaa), use.names = FALSE)
stopifnot(
  "repertoire lost or gained observations" = length(ctaa) == nrow(m),
  "not every observation is paired alpha/beta" = all(is_paired(ctaa))
)
chains <- cerebroAppLite:::hla_detect_chains(ir)
stopifnot(
  "both TRA and TRB must be detectable" = all(c("TRA", "TRB") %in% chains)
)
cat(sprintf(
  "   repertoire: %d observations, all paired; chains %s\n",
  length(ctaa),
  paste(chains, collapse = ", ")
))

## -- the motif network must actually be worth drawing
motif_counts <- list()
for (ch in c("TRB", "TRA")) {
  seg <- cerebroAppLite:::hla_parse_ir_segments(ir, ch)
  stopifnot("no segments parsed" = !is.null(seg) && nrow(seg) > 0)
  g <- cerebroAppLite:::hla_build_motif_graph(seg, by_v = TRUE, min_nodes = 2L)
  stopifnot("no usable motif graph" = cerebroAppLite:::hla_motif_graph_ok(g))
  motif_counts[[ch]] <- c(
    nodes = igraph::vcount(g),
    motifs = length(unique(igraph::V(g)$cluster))
  )
  cat(sprintf(
    "   %s: %d unique CDR3 -> %d nodes in %d motifs\n",
    ch,
    length(unique(seg$cdr3)),
    motif_counts[[ch]][["nodes"]],
    motif_counts[[ch]][["motifs"]]
  ))
}
## TRB is the chain the page defaults to and the one the tests assert on.
stopifnot(
  "TRB network has collapsed below a useful size" = motif_counts$TRB[[
    "nodes"
  ]] >
    100 &&
    motif_counts$TRB[["motifs"]] >= 20
)

## -- HLA: the published genotypes, unmodified, for every donor
ht <- check$getHLATyping()
stopifnot(
  "HLA typing must cover every donor" = setequal(
    unique(ht$sample),
    unique(m$sample)
  ),
  "HLA typing must be the published genotypes" = identical(
    unique(ht$source_type),
    "genotyped"
  ),
  ## Sorted canonical ROWS, not a set of sample+allele pairs: a set comparison
  ## silently tolerates a lost duplicate (donor4 is homozygous A*03:01, so one
  ## of its two rows can vanish and the set is unchanged) and ignores `copy`
  ## entirely. Compare what the object actually stores, and count the rows.
  "HLA typing drifted from the published table" = identical(
    hla_canonical_rows(ht),
    hla_canonical_rows(donor_typing_canonical)
  ),
  "HLA typing lost or gained rows" = nrow(ht) == nrow(donor_typing_canonical),
  "provenance is missing" = nzchar(unique(ht$typing_method)) &&
    nzchar(unique(ht$source_reference))
)
cat(sprintf(
  "   HLA: %d donors, %d alleles, source_type=%s\n",
  length(unique(ht$sample)),
  length(unique(ht$allele)),
  paste(unique(ht$source_type), collapse = ",")
))

## -- the honesty columns must be present and must still show the cross-reactivity
stopifnot(
  "the dextramer_* / restriction_in_genotype columns are missing" = all(
    c(
      "dextramer_antigen",
      "dextramer_peptide",
      "dextramer_allele",
      "restriction_in_genotype"
    ) %in%
      colnames(m)
  ),
  "restriction_in_genotype must be yes/no/unknown" = all(
    m$restriction_in_genotype %in% c("yes", "no", "unknown")
  ),
  ## If this ever came out clean, the binder calls would have stopped being raw
  ## 10x calls and the documentation would need rewriting -- so assert the
  ## caveat is still true rather than assuming it.
  "off-genotype binding vanished; re-check the specificity claims" = any(
    m$restriction_in_genotype == "no"
  ),
  ## And "unknown" must exist too: table S1 publishes one HLA-B allele for two
  ## donors, so a binder call at their second B copy is undecidable. If this
  ## became empty, either the typing gained copies or the three-state logic
  ## collapsed back into a two-state one that calls missing data a negative.
  "the undecidable calls vanished; check locus completeness" = any(
    m$restriction_in_genotype == "unknown"
  ),
  "the caveat must be recorded in the object" = grepl(
    "RAW BINDER CALLS",
    check$technical_info$tcr_selection_detail
  )
)
cat(sprintf(
  "   binder calls vs genotype: %d off-genotype, %d undecidable, of %d cells\n",
  sum(m$restriction_in_genotype == "no"),
  sum(m$restriction_in_genotype == "unknown"),
  nrow(m)
))

## -- gate passed: publish
## file.rename() RETURNS failure, it does not signal it -- and it is the failure
## most likely to happen in practice (a replacement across devices, or an
## existing OUT on Windows). Printing PUBLISHED over a rename that silently did
## nothing would leave the old file in place while claiming the new one shipped,
## which is the whole class of bug this gate exists to stop.
if (!file.rename(staged, OUT)) {
  stop(
    "could not publish the staged object to ",
    OUT,
    " -- the previously shipped file is untouched",
    call. = FALSE
  )
}
## Re-read what actually landed: the gate measured the staged bytes, so this is
## the last step that can catch a truncated or partially written destination.
published <- readRDS(OUT)
stopifnot(
  "the published file does not match what was verified" = identical(
    dim(published$expression),
    dim(check$expression)
  ) &&
    nrow(published$getMetaData()) == nrow(m)
)
cat(sprintf(
  "   PUBLISHED %s (%.1f MB)\n",
  OUT,
  file.info(OUT)$size / 1024^2
))
cat("done.\n")
