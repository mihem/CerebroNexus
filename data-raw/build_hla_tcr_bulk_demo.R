#!/usr/bin/env Rscript
# ============================================================================
# NOT SHIPPED (since 2026-07-21). This script still runs and is maintained, but
# the .crb it writes is no longer tracked or installed: CerebroNexus is a
# single-cell application and this cohort is bulk. Kept because it is the
# reference pipeline for bringing a bulk cohort in (see the "HLA Associations on
# bulk TCRb" vignette), and because its single-copy HLA calls are the only thing
# that exercises the loose-vs-strict carrier distinction. data-raw/ is
# .Rbuildignore'd, so this costs the installed package nothing.
# ============================================================================
# Build the real-HLA TCR demo (.crb)  ->  inst/extdata/v1.4/demo_hla_tcr_bulk.crb
# ============================================================================
# EVERYTHING in this demo is real measured data. Nothing is synthesised.
#
#   * TCRs   : real public TCR-beta chains (V family + CDR3 amino acids) from
#              the Emerson et al. 2017 cohort, as cleaned and published by
#              DeWitt et al. 2018.
#   * HLA    : the real HLA genotype of each of those donors (same cohort).
#   * Linkage: which donor carries which TCR is the real observed occurrence
#              pattern, not a simulation.
#
# The shipped demo is now `demo_hla_tcr_dextramer.crb` (real single cells, real
# paired TCR, real published genotypes). What this cohort still adds, and why the
# script is kept: 100 donors against that demo's 4, 139 alleles against 12, both
# MHC classes rather than Class I only, and single-copy calls -- the only input
# that makes the loose-vs-strict carrier distinction differ. Build it when you
# need any of those; it is not installed.
#
# WHAT BULK MEANS HERE. The source is bulk TCR-beta immunosequencing: one
# repertoire per donor, no single cells, no transcriptome, no CD4/CD8 label.
# To fit the .crb contract each (donor, TCR clonotype) pair becomes one row —
# an analysis unit, NOT a real sequenced cell. Consequences, all intended:
#   - there is no expression matrix and no projection (the Projection / Gene
#     expression tabs have nothing to show for this data set);
#   - `cell_type` is "T cell (bulk TCRb)" for every row, so the lineage MHC
#     context is Unknown by design — this data cannot distinguish CD4 from CD8
#     and the page must not pretend otherwise;
#   - `sample` is the donor, which is the correct unit for HLA carrier counts.
#
# SCOPE. The TCR pool is restricted to the paper's HLA-ASSOCIATED TCRs for a
# few single alleles (below). Two reasons: it keeps the graph inside the size
# guards, and those TCRs are the ones with a published donor-level HLA
# association, so the descriptive overlap the page shows can be checked against
# the paper's own table.
#
# Restricting to single alleles is a schema constraint: the canonical HLA table
# stores one allele per locus x copy, so the source's DR/DQ haplotype triples
# (HLA-DRDQ*15:01_01:02_06:02) and DQ/DP alpha-beta pairs cannot be represented
# and are excluded.
#
# Source (public, CC-BY 4.0):
#   Zenodo   https://zenodo.org/records/1248193  (pubtcrs_data_v1.tgz, 349 MB)
#   Paper    DeWitt et al., eLife 2018; data from Emerson et al., Nat Genet 2017
#   Tools    https://github.com/phbradley/pubtcrs
#
# The 349 MB archive is NOT tracked (see data-raw/DATASETS.md); this script
# downloads it on demand. Only the built .crb ships.
#
# Run from the package root:
#   Rscript data-raw/build_hla_tcr_bulk_demo.R
# ============================================================================

suppressMessages(library(CerebroNexus))

set.seed(42)

raw_dir <- Sys.getenv("PUBTCRS_DIR", unset = "data-raw/pubtcrs")
out <- Sys.getenv("OUT_CRB", unset = "inst/extdata/v1.4/demo_hla_tcr_bulk.crb")

## Alleles to build the demo around: a Class I + Class II mix, each with enough
## associated TCRs AND enough carriers/non-carriers among the donors for the
## carrier table to be meaningful.
demo_alleles <- c(
  "HLA-A*02:01",
  "HLA-A*01:01",
  "HLA-B*07:02",
  "HLA-B*08:01",
  "HLA-DRB1*04:01",
  "HLA-DRB1*07:01"
)
n_donors <- 100L

## ---- 0. Acquire ---------------------------------------------------------- ##
tgz <- file.path(raw_dir, "pubtcrs_data_v1.tgz")
data_dir <- file.path(raw_dir, "pubtcrs_data")
if (!dir.exists(data_dir)) {
  dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
  if (!file.exists(tgz)) {
    message("Downloading pubtcrs_data_v1.tgz (349 MB) ...")
    utils::download.file(
      "https://zenodo.org/api/records/1248193/files/pubtcrs_data_v1.tgz/content",
      tgz,
      mode = "wb",
      quiet = FALSE
    )
  }
  message("Extracting ...")
  utils::untar(tgz, exdir = raw_dir)
}
stopifnot(dir.exists(data_dir))

## ---- 1. Real per-donor HLA genotype -------------------------------------- ##
## HLA_features.txt lists, per allele, the indices of the donors carrying it:
##   feature: HLA-A*33:01 num_positives: 11 positives: 25 64 ... num_negatives: ...
parse_hla_features <- function(path) {
  lines <- readLines(path)
  out <- list()
  for (ln in lines) {
    feat <- sub("^feature:\\s*(\\S+).*$", "\\1", ln)
    pos_part <- sub("^.*\\spositives:\\s*(.*?)\\s+num_negatives:.*$", "\\1", ln)
    if (identical(pos_part, ln)) {
      next
    }
    pos <- suppressWarnings(as.integer(strsplit(trimws(pos_part), "\\s+")[[1]]))
    out[[feat]] <- pos[!is.na(pos)]
  }
  out
}
hla_by_allele <- parse_hla_features(file.path(data_dir, "HLA_features.txt"))
message(sprintf("HLA alleles in cohort: %d", length(hla_by_allele)))

# Invert to donor -> alleles.
hla_by_donor <- list()
for (allele in names(hla_by_allele)) {
  for (s in hla_by_allele[[allele]]) {
    key <- as.character(s)
    hla_by_donor[[key]] <- c(hla_by_donor[[key]], allele)
  }
}
message(sprintf("donors with HLA typing: %d", length(hla_by_donor)))

## ---- 2. Real HLA-associated TCRs, restricted to the demo alleles ---------- ##
assoc <- utils::read.delim(
  file.path(data_dir, "HLA_associated_TCRs.tsv"),
  stringsAsFactors = FALSE
)
assoc <- assoc[assoc$hla_allele %in% demo_alleles, , drop = FALSE]
wanted_tcrs <- unique(assoc$tcr)
message(sprintf(
  "HLA-associated TCRs for the %d demo alleles: %d",
  length(demo_alleles),
  length(wanted_tcrs)
))

## ---- 3. Real occurrence: which donor carries which TCR -------------------- ##
## pubtcrs_matrix.txt is ~11M lines; read it in chunks and keep only the TCRs
## we want, so this never loads 850 MB into memory.
##   tcr: V02,CAGGLAGTDTQYF num_subjects: 12 of: 666 subjects: 0 26 75 ...
matrix_path <- file.path(data_dir, "pubtcrs_matrix.txt")
## The scan is the expensive step (~850 MB, 11M lines), so cache its result:
## re-running the build to tweak donor counts should not rescan the matrix.
cache_path <- file.path(raw_dir, "tcr_donors_cache.rds")
cache_key <- sort(wanted_tcrs)

tcr_donors <- NULL
if (file.exists(cache_path)) {
  cached <- readRDS(cache_path)
  if (identical(cached$key, cache_key)) {
    tcr_donors <- cached$tcr_donors
    message("Reusing cached occurrence scan.")
  }
}

if (is.null(tcr_donors)) {
  want <- new.env(hash = TRUE, parent = emptyenv())
  for (t in wanted_tcrs) {
    assign(t, TRUE, envir = want)
  }
  tcr_donors <- list()
  con <- file(matrix_path, "r")
  on.exit(close(con), add = TRUE)
  n_read <- 0L
  repeat {
    chunk <- readLines(con, n = 200000L)
    if (length(chunk) == 0) {
      break
    }
    n_read <- n_read + length(chunk)
    tcr <- sub("^tcr:\\s*(\\S+)\\s+num_subjects:.*$", "\\1", chunk)
    hit <- vapply(tcr, exists, logical(1), envir = want, inherits = FALSE)
    if (any(hit)) {
      sub_part <- sub("^.*\\ssubjects:\\s*", "", chunk[hit])
      donors <- strsplit(trimws(sub_part), "\\s+")
      names(donors) <- tcr[hit]
      tcr_donors <- c(tcr_donors, donors)
    }
    message(sprintf(
      "  scanned %s lines, matched %d TCRs",
      format(n_read, big.mark = ","),
      length(tcr_donors)
    ))
  }
  close(con)
  on.exit(NULL)
  saveRDS(list(key = cache_key, tcr_donors = tcr_donors), cache_path)
}
message(sprintf(
  "TCRs located in the occurrence matrix: %d",
  length(tcr_donors)
))

## ---- 4. Choose donors ---------------------------------------------------- ##
## Keep donors that (a) have HLA typing and (b) carry at least one demo TCR.
## Then take the first `n_donors` by donor index so the subset is deterministic
## and not chosen to flatter any particular association.
donor_hits <- table(unlist(tcr_donors))
eligible <- intersect(names(donor_hits), names(hla_by_donor))
eligible <- eligible[order(as.integer(eligible))]
donors <- head(eligible, n_donors)
message(sprintf(
  "donors: %d eligible -> %d kept (deterministic: lowest donor index first)",
  length(eligible),
  length(donors)
))

## ---- 5. Build one IR table per donor ------------------------------------- ##
## Each row = one (donor, TCR clonotype) analysis unit. The source gives a V
## family and the CDR3 amino-acid sequence; there is no J gene, so CTgene
## carries the V family only and the parser leaves j_gene NA.
donor_set <- new.env(hash = TRUE, parent = emptyenv())
for (d in donors) {
  assign(d, character(0), envir = donor_set)
}
for (t in names(tcr_donors)) {
  for (d in intersect(tcr_donors[[t]], donors)) {
    assign(d, c(get(d, envir = donor_set), t), envir = donor_set)
  }
}

donor_label <- function(d) sprintf("donor_%03d", as.integer(d))

ir <- list()
meta_rows <- list()
for (d in donors) {
  tcrs <- get(d, envir = donor_set)
  if (length(tcrs) == 0) {
    next
  }
  parts <- strsplit(tcrs, ",", fixed = TRUE)
  vfam <- vapply(parts, `[`, character(1), 1) # "V02"
  cdr3 <- vapply(parts, `[`, character(1), 2) # "CAGGLAGTDTQYF"
  # "V02" is this source's shorthand for the TRB V family 02; render it in the
  # TRBV form the CT* parser expects. No J gene exists in the source.
  ctgene <- paste0("TRB", vfam)
  bc <- sprintf("%s_%04d", donor_label(d), seq_along(tcrs))
  lab <- donor_label(d)
  ir[[lab]] <- data.frame(
    barcode = bc,
    CTgene = ctgene,
    CTnt = NA_character_,
    CTaa = cdr3,
    CTstrict = NA_character_,
    stringsAsFactors = FALSE
  )
  meta_rows[[lab]] <- data.frame(
    cell_barcode = bc,
    sample = lab,
    donor_id = lab,
    # Bulk TCR-beta: the source cannot distinguish CD4 from CD8, so the lineage
    # MHC context is Unknown by design. Do not invent a lineage here.
    cell_type = "T cell (bulk TCRb)",
    stringsAsFactors = FALSE
  )
}
meta <- do.call(rbind, meta_rows)
rownames(meta) <- NULL
message(sprintf(
  "built %d donor repertoires, %s analysis units, %s unique CDR3",
  length(ir),
  format(nrow(meta), big.mark = ","),
  format(length(unique(unlist(lapply(ir, function(x) x$CTaa)))), big.mark = ",")
))

## ---- 6. Real HLA typing table for the kept donors ------------------------- ##
## Write the CANONICAL LONG TABLE directly rather than going through the
## named-list adapter: the adapter has no donor column, so it would leave
## donor_id NA and silently demote the whole data set to sample-level counting.
## Here one donor is one sample, so donor_id is stated explicitly and the app
## can honour its donor-level contract.
typing_rows <- lapply(donors, function(d) {
  alleles <- hla_by_donor[[d]]
  if (length(alleles) == 0) {
    return(NULL)
  }
  lab <- donor_label(d)
  loci <- sub("\\*.*$", "", alleles)
  # copy 1/2 within each locus, in the source's order
  copy <- unlist(lapply(split(seq_along(alleles), loci), seq_along))
  copy <- copy[order(unlist(split(seq_along(alleles), loci)))]
  data.frame(
    sample = lab,
    donor_id = lab,
    locus = loci,
    copy = as.integer(copy),
    allele = alleles,
    stringsAsFactors = FALSE
  )
})
typing_long <- do.call(rbind, typing_rows)

## ---- 7. Assemble the .crb ------------------------------------------------ ##
crb <- Cerebro_v1.3$new()
crb$experiment <- list(
  experiment_name = "PBMC bulk TCRb cohort (Emerson 2017) - real HLA",
  organism = "hg",
  date_of_analysis = "2018-08-21",
  date_of_export = format(Sys.Date())
)
crb$parameters <- list()
crb$technical_info <- list(
  note = paste(
    "Bulk TCR-beta immunosequencing; no transcriptome, no single cells.",
    "Each row is a (donor, TCR clonotype) analysis unit, not a sequenced cell."
  ),
  # Declared contract, read app-wide (see getObservationUnit): a row here is an
  # analysis unit, not a sequenced cell. Without this the app would call these
  # rows "Cells" and state a measurement that was never made.
  observation_unit = "analysis unit",
  # Declared contract, read by the app (see hla_selection_caveat): the receptor
  # set was chosen USING the published HLA association, and the donors were then
  # chosen for carrying those receptors. Any carrier/non-carrier difference the
  # page shows is therefore built in by that selection. This is a positive
  # control for the workflow, NOT independent evidence of an association, and
  # the app must say so wherever the contrast is displayed.
  # This source identifies a receptor by (V family, CDR3), not by CDR3 alone:
  # 22 of its CDR3s occur on more than one V family. Declaring the key makes
  # split-by-V the app's default, so nodes match the source's own receptor
  # identity instead of fusing two receptors (and double-counting a donor).
  receptor_key = "v_gene+cdr3",
  tcr_selection = "association-conditioned",
  tcr_selection_detail = paste(
    "Receptors are the published HLA-associated TCRs for six alleles",
    "(DeWitt et al. 2018), and donors were kept only if they carry at least",
    "one of them. A carrier/non-carrier contrast here is a consequence of that",
    "selection, not a new finding, and re-computing overlap on the same cohort",
    "the association was derived from is not independent replication."
  )
)
# Bulk TCR-seq measures no transcriptome and produces no embedding, so this data
# set genuinely has zero genes and no projection. We store a real 0-row (0 gene x
# N unit) matrix rather than NULL: it states "no genes were measured" honestly
# while keeping every ncol()/nrow() code path in the app well defined.
crb$expression <- Matrix::Matrix(
  0,
  nrow = 0,
  ncol = nrow(meta),
  sparse = TRUE
)
colnames(crb$expression) <- meta$cell_barcode
crb$meta_data <- meta
crb$addImmuneRepertoire(ir)
crb$addHLATyping(
  typing_long,
  source_type = "genotyped",
  typing_method = "Emerson et al. 2017 cohort HLA typing",
  source_reference = "Zenodo 1248193 (pubtcrs_data_v1); DeWitt et al. eLife 2018"
)
crb$addGroup("sample", unique(meta$sample))
crb$addGroup("cell_type", unique(meta$cell_type))

dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
saveRDS(crb, out)
message(sprintf("\nWrote %s (%.1f MB)", out, file.info(out)$size / 1024^2))

## ---- 8. Verify round-trip ------------------------------------------------ ##
check <- readRDS(out)
ht <- check$getHLATyping()
stopifnot(
  length(check$getImmuneRepertoire()) == length(ir),
  nrow(ht) > 0,
  all(ht$source_type == "genotyped"),
  all(demo_alleles %in% ht$allele)
)
message(sprintf(
  "Round-trip OK: %d donors, %d HLA rows, %d distinct alleles (all real).",
  length(unique(ht$sample)),
  nrow(ht),
  length(unique(ht$allele))
))
for (a in demo_alleles) {
  message(sprintf(
    "  %-16s carriers among demo donors: %d / %d",
    a,
    length(unique(ht$sample[ht$allele == a])),
    length(ir)
  ))
}
