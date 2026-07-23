#!/usr/bin/env Rscript
# ============================================================================
# NOT SHIPPED (since 2026-07-21). This script still runs and is maintained, but
# the .crb it writes is no longer tracked or installed: it is fabricated end to
# end, and demo_hla_tcr_dextramer.crb now shows the same network on measured
# sequences. Kept because a fully-controlled dense network is still the fastest
# fixture to develop the page against. data-raw/ is .Rbuildignore'd, so this
# costs the installed package nothing.
# ============================================================================
# Build the HLA & TCR Motifs demo (.crb) — A FULLY SYNTHETIC FIXTURE
# ============================================================================
# Produces `inst/extdata/v1.4/demo_hla_tcr_synthetic.crb`.
#
# WHAT THIS IS
# ------------
# Every value in this data set is fabricated: expression, projection, cell
# types, CDR3 sequences, donor HLA genotypes, and the association between them.
# Its ONLY purpose is to exercise the "HLA & TCR Motifs" page end to end with a
# repertoire that actually forms a readable motif network. It supports NO
# biological claim whatsoever.
#
# WHY IT HAD TO BE FABRICATED
# ---------------------------
# Its predecessor carried REAL CDR3 sequences (real expression, real receptors,
# synthetic receptor-to-cell linkage) and rendered a 4-node network. Measured on
# that object:
#
#   chain   unique CDR3   Hamming-1 pairs   nodes with >=1 neighbour
#   TRB     456           2                 4
#   TRA     395           32                20
#
# That is not a bug and not a sample-size accident: an unselected polyclonal
# repertoire is sparse in CDR3 space (20^14 possible 14-mers), so randomly drawn
# receptors have almost no 1-mismatch neighbours. Hamming-1 pair count grows
# ~n^2, so even 5,000 cells (~4,000 unique CDR3) only extrapolates to ~150 pairs
# — still a scatter of isolated dots. Dense motif networks in real data come
# from SELECTION (public / antigen-conditioned receptors converge), not from
# scale. To show the page working, the motif families must be designed in.
#
# HONESTY CONTRACT
# ----------------
# `technical_info$tcr_selection = "synthetic"` — a value the page treats as its
# hardest disclosure: receptors AND their HLA association were both constructed,
# so the carrier/non-carrier contrast this data set displays was put there on
# purpose. It is circular by construction, more so than an
# "association-conditioned" positive control (where at least the sequences and
# genotypes are real). The shipped demo, demo_hla_tcr_dextramer.crb, has real
# cells, real TCR and real published genotypes -- prefer it for anything factual.
#
# WHAT IS REUSED RATHER THAN INVENTED
# -----------------------------------
# Gene SYMBOLS (a real vocabulary, so the Gene expression tab is searchable),
# V/J gene names, and European HLA allele frequency ranges. These are naming
# conventions and public frequency tables, not measurements. Expression VALUES
# against those symbols are simulated.
#
# Run from the package root:
#   Rscript data-raw/build_hla_tcr_demo.R
# ============================================================================

suppressMessages(library(CerebroNexus))
suppressMessages(library(Matrix))
suppressMessages(library(stringdist))

set.seed(20260715)

out <- Sys.getenv(
  "OUT_CRB",
  unset = "inst/extdata/v1.4/demo_hla_tcr_synthetic.crb"
)
## Gene-symbol vocabulary is borrowed from the base PBMC object rather than this
## script's own output, so re-running the build is never self-referential.
symbol_src <- Sys.getenv(
  "SYMBOL_CRB",
  unset = "inst/extdata/v1.4/demo_full_tcr_bcr.crb"
)

## ---- Cohort shape ------------------------------------------------------ ##
N_DONORS <- 30L
DONORS <- sprintf("donor_%02d", seq_len(N_DONORS))

## 30 donors x 167 cells = 5010. Donor count is a design driver, not a detail:
## node carrier status rests on how many typed donors share a CDR3, and the
## allele picker ranks alleles by pmin(n_carrier, n_noncarrier). Few donors =>
## no contrast to show.
CELLS_PER_DONOR <- 167L

CELL_TYPES <- c(
  "CD8 T" = 2000L,
  "CD4 T" = 1750L,
  "Treg" = 500L,
  "B cells" = 500L,
  "Monocytes" = 260L
)
T_LINEAGES <- c("CD8 T", "CD4 T", "Treg")

## Fraction of T cells with a detected chain. B cells / monocytes never get a
## receptor, which also exercises the "cell without a receptor" path.
TRB_DETECTION <- 0.95
TRA_DETECTION <- 0.90

## Share of motif members that are PUBLIC (the identical CDR3 in 2-4 donors);
## the rest are private to one donor. Drives the "Shared" colouring, which only
## reads as a signal while it stays a minority.
PUBLIC_NODE_RATE <- 0.15

## ---- Amino-acid junction composition ----------------------------------- ##
AA <- c(
  "A",
  "C",
  "D",
  "E",
  "F",
  "G",
  "H",
  "I",
  "K",
  "L",
  "M",
  "N",
  "P",
  "Q",
  "R",
  "S",
  "T",
  "V",
  "W",
  "Y"
)
## Rough junctional (N/D/N region) composition: glycine/serine rich, cysteine
## and tryptophan nearly absent.
AA_P <- c(
  0.070,
  0.003,
  0.050,
  0.062,
  0.030,
  0.110,
  0.020,
  0.032,
  0.022,
  0.070,
  0.012,
  0.040,
  0.050,
  0.052,
  0.070,
  0.130,
  0.070,
  0.052,
  0.008,
  0.047
)
AA_P <- AA_P / sum(AA_P)

## One codon per residue; enough to emit a plausible CTnt/CTstrict.
CODON <- c(
  A = "GCC",
  C = "TGC",
  D = "GAC",
  E = "GAG",
  F = "TTC",
  G = "GGC",
  H = "CAC",
  I = "ATC",
  K = "AAG",
  L = "CTG",
  M = "ATG",
  N = "AAC",
  P = "CCC",
  Q = "CAG",
  R = "AGG",
  S = "AGC",
  T = "ACC",
  V = "GTG",
  W = "TGG",
  Y = "TAC"
)

back_translate <- function(aa_seq) {
  vapply(
    strsplit(aa_seq, "", fixed = TRUE),
    function(chars) paste0("TGT", paste(CODON[chars[-1]], collapse = "")),
    character(1)
  )
}

## ---- V / J germline vocabulary ----------------------------------------- ##
## The V gene fixes the CDR3 prefix and the J gene fixes the suffix; only the
## junction between them is diversified. Family members therefore share a V and
## a J, which matters: `receptor_key = "v_gene+cdr3"` makes the page default to
## "Split motifs by V gene", and a family whose members disagreed on V would
## shatter the moment that box is ticked.
TRBV <- c(
  "TRBV5-1" = "CASS",
  "TRBV6-5" = "CASS",
  "TRBV7-9" = "CASS",
  "TRBV9" = "CASS",
  "TRBV12-3" = "CASS",
  "TRBV19" = "CASS",
  "TRBV27" = "CASS",
  "TRBV28" = "CASS",
  "TRBV4-1" = "CASS",
  "TRBV20-1" = "CSA",
  "TRBV2" = "CASS",
  "TRBV11-2" = "CASS"
)
TRBJ <- c(
  "TRBJ1-1" = "TEAFF",
  "TRBJ1-2" = "NYGYTF",
  "TRBJ1-5" = "NQPQHF",
  "TRBJ2-1" = "NEQFF",
  "TRBJ2-3" = "TDTQYF",
  "TRBJ2-5" = "ETQYF",
  "TRBJ2-7" = "YEQYF"
)
TRAV <- c(
  "TRAV1-2" = "CAV",
  "TRAV8-4" = "CAV",
  "TRAV12-1" = "CAV",
  "TRAV21" = "CAV",
  "TRAV29DV5" = "CAA",
  "TRAV13-1" = "CAA",
  "TRAV38-2DV8" = "CAY",
  "TRAV26-1" = "CIV"
)
TRAJ <- c(
  "TRAJ33" = "DSNYQLIW",
  "TRAJ42" = "GGSQGNLIF",
  "TRAJ39" = "NNAGNMLTF",
  "TRAJ49" = "NTGNQFYF",
  "TRAJ20" = "DYKLSF",
  "TRAJ52" = "GANSKLTF",
  "TRAJ57" = "GSEKLVF"
)

rand_middle <- function(len) {
  paste(sample(AA, len, replace = TRUE, prob = AA_P), collapse = "")
}

make_cdr3 <- function(prefix, jsuffix, middle_len) {
  paste0(prefix, rand_middle(middle_len), jsuffix)
}

## ---- Family growth in sequence space ----------------------------------- ##
## A family grows as a branching walk: repeatedly pick an existing member and
## substitute one junction residue. `p_hub` steers the shape by choosing how
## often the seed itself is the parent:
##   high  -> hub-dominant star (a dominant clonotype's variant cloud)
##   low   -> chain-like path (large diameter; the transitive-membership case
##            the page's diameter readout exists to disclose)
## Topology is NOT asserted from this parameter — it is measured afterwards
## with the package's own hla_process_length_group().
grow_family <- function(seed, size, p_hub, mut_range, taken) {
  members <- seed
  guard <- 0L
  max_guard <- size * 400L
  while (length(members) < size && guard < max_guard) {
    guard <- guard + 1L
    parent <- if (stats::runif(1) < p_hub) {
      seed
    } else {
      members[sample.int(length(members), 1L)]
    }
    chars <- strsplit(parent, "", fixed = TRUE)[[1]]
    pos <- mut_range[sample.int(length(mut_range), 1L)]
    alt <- setdiff(AA, chars[pos])
    chars[pos] <- alt[sample.int(length(alt), 1L)]
    cand <- paste(chars, collapse = "")
    if (cand %in% members || cand %in% taken) {
      next
    }
    members <- c(members, cand)
  }
  if (length(members) < size) {
    stop(sprintf(
      "grow_family exhausted: got %d of %d requested members",
      length(members),
      size
    ))
  }
  members
}

## Reject a candidate set that would fuse with anything already generated.
## Hamming only ever compares equal-length strings, so only the matching length
## bin can collide.
collides <- function(cands, pool) {
  same_len <- pool[nchar(pool) == nchar(cands[1])]
  if (length(same_len) == 0L) {
    return(FALSE)
  }
  dm <- stringdist::stringdistmatrix(cands, same_len, method = "hamming")
  any(dm <= 1L)
}

## ---- Family design ------------------------------------------------------ ##
## Sizes are deliberately unequal (a few large hubs, a long tail of small ones)
## so the network reads as islands rather than 20 identical blobs. HLA tier:
##   strong -> members appear ONLY in carriers of the anchor allele
##             => every node scores "Carrier" for that allele
##   weak   -> carrier-enriched but leaks into non-carriers => Carrier + Mixed
##   none   -> donors drawn at random => almost all nodes score "Mixed"
## Lineage follows MHC class so the page's lineage context stays coherent:
## class I anchors (A/B/C) live in CD8 T, class II (DRB1) in CD4 T / Treg.
##
## ANCHOR CONCENTRATION. Colouring is per-allele, so a family only responds to
## its own anchor: spreading the six strong families over six different alleles
## lights exactly one island per allele and leaves the other nineteen "Mixed" —
## measured at 103 Carrier / 303 Mixed / 33 Non-carrier, i.e. the same
## Mixed-dominated wash this fixture exists to avoid. Mixed is the SEMANTICALLY
## CORRECT label for a CDR3 seen in both carriers and non-carriers, so it must
## not be suppressed; instead the biggest families cluster on HLA-A*02:01 (the
## top-contrast allele) so that picking it lights several islands at once.
ANCHOR_PRIMARY <- "HLA-A*02:01"

trb_design <- data.frame(
  id = sprintf("TRB_F%02d", 1:20),
  size = c(
    64L,
    56L,
    50L,
    38L,
    34L,
    30L,
    26L,
    22L,
    17L,
    16L,
    14L,
    12L,
    11L,
    10L,
    9L,
    8L,
    7L,
    6L,
    5L,
    5L
  ),
  p_hub = c(
    0.60,
    0.55,
    0.55,
    0.35,
    0.30,
    0.30,
    0.25,
    0.30,
    0.08,
    0.05,
    0.10,
    0.05,
    0.08,
    0.05,
    0.10,
    0.30,
    0.30,
    0.25,
    0.30,
    0.30
  ),
  tier = c(
    "strong",
    "weak",
    "strong",
    "strong",
    "weak",
    "none",
    "strong",
    "weak",
    "none",
    "strong",
    "weak",
    "none",
    "strong",
    "weak",
    "none",
    "weak",
    "weak",
    "none",
    "weak",
    "none"
  ),
  anchor = c(
    "HLA-A*02:01",
    "HLA-A*02:01",
    "HLA-A*02:01",
    "HLA-B*07:02",
    "HLA-A*02:01",
    NA,
    "HLA-DRB1*15:01",
    "HLA-B*07:02",
    NA,
    "HLA-A*01:01",
    "HLA-DRB1*15:01",
    NA,
    "HLA-C*07:01",
    "HLA-A*03:01",
    NA,
    "HLA-C*07:01",
    "HLA-B*08:01",
    NA,
    "HLA-DRB1*03:01",
    NA
  ),
  middle_len = c(
    9L,
    9L,
    8L,
    8L,
    8L,
    7L,
    8L,
    7L,
    7L,
    7L,
    6L,
    6L,
    6L,
    6L,
    6L,
    5L,
    5L,
    5L,
    5L,
    5L
  ),
  stringsAsFactors = FALSE
)

tra_design <- data.frame(
  id = sprintf("TRA_F%02d", 1:10),
  size = c(40L, 32L, 26L, 20L, 16L, 14L, 12L, 10L, 8L, 6L),
  p_hub = c(0.55, 0.50, 0.30, 0.30, 0.08, 0.05, 0.30, 0.10, 0.30, 0.25),
  tier = c(
    "strong",
    "weak",
    "none",
    "strong",
    "none",
    "weak",
    "none",
    "strong",
    "weak",
    "none"
  ),
  anchor = c(
    "HLA-A*02:01",
    "HLA-A*02:01",
    NA,
    "HLA-B*07:02",
    NA,
    "HLA-A*02:01",
    NA,
    "HLA-DRB1*15:01",
    "HLA-A*03:01",
    NA
  ),
  middle_len = c(8L, 8L, 7L, 7L, 6L, 6L, 6L, 5L, 5L, 5L),
  stringsAsFactors = FALSE
)

## ---- Donor HLA genotypes ------------------------------------------------ ##
## Allele frequencies are illustrative, in the range of published European
## reference panels. Only the four loci the page enforces (HLA_MVP_LOCI:
## A / B / C / DRB1) are emitted.
ALLELE_POOL <- list(
  "HLA-A" = c(
    "02:01" = 0.28,
    "01:01" = 0.16,
    "03:01" = 0.14,
    "24:02" = 0.09,
    "11:01" = 0.06,
    "32:01" = 0.04,
    "26:01" = 0.04,
    "68:01" = 0.03,
    "29:02" = 0.03,
    "31:01" = 0.03
  ),
  "HLA-B" = c(
    "07:02" = 0.13,
    "08:01" = 0.12,
    "44:02" = 0.09,
    "15:01" = 0.06,
    "40:01" = 0.06,
    "51:01" = 0.06,
    "35:01" = 0.06,
    "44:03" = 0.04,
    "18:01" = 0.04,
    "27:05" = 0.04
  ),
  "HLA-C" = c(
    "07:01" = 0.16,
    "07:02" = 0.14,
    "04:01" = 0.11,
    "05:01" = 0.09,
    "06:02" = 0.08,
    "03:04" = 0.07,
    "12:03" = 0.05,
    "02:02" = 0.04,
    "16:01" = 0.03,
    "03:03" = 0.03
  ),
  "HLA-DRB1" = c(
    "15:01" = 0.14,
    "03:01" = 0.12,
    "07:01" = 0.11,
    "04:01" = 0.08,
    "01:01" = 0.08,
    "13:01" = 0.06,
    "11:01" = 0.06,
    "11:04" = 0.03,
    "04:04" = 0.03,
    "08:01" = 0.03
  )
)

## Carrier counts for the anchor alleles are FIXED rather than drawn, so the
## allele picker's leading contrast is a design target and not a lottery. The
## picker ranks by pmin(n_carrier, n_noncarrier), so ANCHOR_PRIMARY is given the
## only perfect 15/15 split and therefore sorts first — which is what puts its
## four concentrated families in front of the user on the first HLA render.
ANCHOR_CARRIERS <- c(
  "HLA-A*02:01" = 15L,
  "HLA-C*07:01" = 14L,
  "HLA-B*07:02" = 13L,
  "HLA-DRB1*15:01" = 12L,
  "HLA-A*01:01" = 11L,
  "HLA-B*08:01" = 10L,
  "HLA-DRB1*03:01" = 11L,
  "HLA-A*03:01" = 12L
)

anchor_carrier_sets <- lapply(ANCHOR_CARRIERS, function(n) {
  sample(DONORS, n)
})
names(anchor_carrier_sets) <- names(ANCHOR_CARRIERS)

split_allele <- function(x) {
  list(locus = sub("\\*.*$", "", x), allele = sub("^.*\\*", "", x))
}

## Build each donor's two alleles per locus: anchors this donor was chosen to
## carry go in first, the remaining slots are drawn from the pool (excluding
## anchors the donor must NOT carry, so the fixed carrier counts hold exactly).
donor_typing <- lapply(DONORS, function(d) {
  unlist(lapply(names(ALLELE_POOL), function(locus) {
    pool <- ALLELE_POOL[[locus]]
    locus_anchors <- names(ANCHOR_CARRIERS)[
      vapply(
        names(ANCHOR_CARRIERS),
        function(a) {
          split_allele(a)$locus == locus
        },
        logical(1)
      )
    ]
    carried <- locus_anchors[vapply(
      locus_anchors,
      function(a) d %in% anchor_carrier_sets[[a]],
      logical(1)
    )]
    forbidden <- vapply(
      setdiff(locus_anchors, carried),
      function(a) split_allele(a)$allele,
      character(1)
    )
    slots <- vapply(carried, function(a) split_allele(a)$allele, character(1))
    slots <- utils::head(slots, 2L)
    if (length(slots) < 2L) {
      free <- pool[!(names(pool) %in% forbidden)]
      extra <- sample(
        names(free),
        2L - length(slots),
        replace = TRUE,
        prob = free / sum(free)
      )
      slots <- c(slots, extra)
    }
    paste0(locus, "*", unname(slots))
  }))
})
names(donor_typing) <- DONORS

is_carrier <- function(donor, allele) {
  allele %in% donor_typing[[donor]]
}

## ---- Cells --------------------------------------------------------------- ##
## Barcodes are unique per donor so the IR list can key on them.
cells <- do.call(
  rbind,
  lapply(DONORS, function(d) {
    data.frame(
      cell_barcode = sprintf("%s_%03d", d, seq_len(CELLS_PER_DONOR)),
      sample = d,
      stringsAsFactors = FALSE
    )
  })
)
stopifnot(nrow(cells) == N_DONORS * CELLS_PER_DONOR)

## Spread the cell-type budget across donors, then shuffle within donor.
type_vec <- rep(names(CELL_TYPES), times = unname(CELL_TYPES))
stopifnot(length(type_vec) == nrow(cells))
cells$cell_type_fine <- ave(
  sample(type_vec),
  cells$sample,
  FUN = function(x) x
)
cells$cell_type <- ifelse(
  cells$cell_type_fine %in% T_LINEAGES,
  "T cells",
  cells$cell_type_fine
)

t_cells <- cells$cell_barcode[cells$cell_type_fine %in% T_LINEAGES]
cat(sprintf(
  "Cells: %d across %d donors (%d T cells)\n",
  nrow(cells),
  N_DONORS,
  length(t_cells)
))

## ---- Receptor assignment ------------------------------------------------- ##
## Assigns one CDR3 per receptor-bearing cell, honouring each family's donor
## eligibility (HLA tier) and lineage. Returns a per-cell table.
##
## A designed member that fails to land in any cell is NOT a cosmetic loss: the
## family grew as a connected walk in sequence space, so dropping members
## disconnects it and one designed family silently arrives as several small
## components. Concentrating four families on one allele makes this easy to hit
## (they all compete for CD8 T cells belonging to the same 15 carriers, ~950
## cells). Every node therefore takes cells through a pool that is popped, falls
## back to any eligible donor, and raises rather than skipping when exhausted.
assign_chain <- function(
  design,
  v_pool,
  j_pool,
  detection,
  background_len_probs
) {
  eligible_cells <- sample(t_cells, floor(length(t_cells) * detection))
  cell_meta <- cells[match(eligible_cells, cells$cell_barcode), ]
  taken <- character(0)
  rows <- list()

  ## Mutable free-cell pools keyed by donor + lineage; take_cell() pops.
  pool_env <- new.env(parent = emptyenv())
  free <- split(
    cell_meta$cell_barcode,
    list(cell_meta$sample, cell_meta$cell_type_fine),
    drop = TRUE
  )
  for (k in names(free)) {
    assign(k, free[[k]], envir = pool_env)
  }
  take_cell <- function(donor, lineages) {
    for (ln in sample(lineages)) {
      key <- paste(donor, ln, sep = ".")
      if (!exists(key, envir = pool_env, inherits = FALSE)) {
        next
      }
      v <- get(key, envir = pool_env, inherits = FALSE)
      if (length(v) == 0L) {
        next
      }
      assign(key, v[-1L], envir = pool_env)
      return(v[1L])
    }
    NA_character_
  }
  used_cells <- character(0)

  ## Give every family a distinct (V, J) pair so members never mix V genes and
  ## families land in different (V, length) bins under split-by-V.
  vj <- expand.grid(
    v = names(v_pool),
    j = names(j_pool),
    stringsAsFactors = FALSE
  )
  vj <- vj[sample.int(nrow(vj)), ]
  stopifnot(nrow(vj) >= nrow(design))

  for (i in seq_len(nrow(design))) {
    fam <- design[i, ]
    v_gene <- vj$v[i]
    j_gene <- vj$j[i]
    prefix <- v_pool[[v_gene]]
    jsuffix <- j_pool[[j_gene]]
    mut_range <- seq(nchar(prefix) + 1L, nchar(prefix) + fam$middle_len)

    ## Seed + grow, retrying until the family is isolated from everything
    ## generated so far.
    members <- NULL
    for (attempt in seq_len(60L)) {
      seed <- make_cdr3(prefix, jsuffix, fam$middle_len)
      if (collides(seed, taken)) {
        next
      }
      cand <- grow_family(seed, fam$size, fam$p_hub, mut_range, taken)
      if (!collides(cand, taken)) {
        members <- cand
        break
      }
    }
    if (is.null(members)) {
      stop(sprintf("could not place isolated family %s", fam$id))
    }
    taken <- c(taken, members)

    ## Donors this family may appear in. Strong and weak families both draw
    ## their base donors from the anchor's carriers; only weak ones additionally
    ## leak into non-carriers, which is what turns some of their nodes "Mixed".
    donor_pool <- if (fam$tier == "none" || is.na(fam$anchor)) {
      DONORS
    } else {
      anchor_carrier_sets[[fam$anchor]]
    }
    leak_pool <- if (identical(fam$tier, "weak")) {
      setdiff(DONORS, anchor_carrier_sets[[fam$anchor]])
    } else {
      character(0)
    }

    ## Lineage: class I anchors -> CD8 T, class II -> CD4 T / Treg.
    lineage <- if (is.na(fam$anchor)) {
      T_LINEAGES
    } else if (grepl("^HLA-DRB1", fam$anchor)) {
      c("CD4 T", "Treg")
    } else {
      "CD8 T"
    }

    ## One cell per (node, donor); the node's donor spread is its size in the
    ## network and also decides its carrier status and its "Shared" label.
    ##
    ## Most members are PRIVATE to one donor. A motif family is a cloud of
    ## DIFFERENT sequences contributed by different donors; the same exact CDR3
    ## recurring across donors is a public clonotype and is the exception. An
    ## earlier version put every member in 2-5 donors, which made 419 of 430
    ## rendered nodes "Shared" — the whole network black, so the level that is
    ## supposed to be the signal became the background.
    for (node in members) {
      n_donors <- if (stats::runif(1) < PUBLIC_NODE_RATE) {
        sample(2:4, 1L)
      } else {
        1L
      }
      donors_here <- sample(donor_pool, min(n_donors, length(donor_pool)))
      if (length(leak_pool) > 0L && stats::runif(1) < 0.55) {
        donors_here <- c(donors_here, sample(leak_pool, sample(1:2, 1L)))
      }
      chosen <- character(0)
      chosen_donors <- character(0)
      for (d in donors_here) {
        cb <- take_cell(d, lineage)
        if (!is.na(cb)) {
          chosen <- c(chosen, cb)
          chosen_donors <- c(chosen_donors, d)
        }
      }
      ## A member with no cell would vanish and split its family, so fall back
      ## to any donor the tier allows before giving up.
      if (length(chosen) == 0L) {
        for (d in sample(donor_pool)) {
          cb <- take_cell(d, lineage)
          if (!is.na(cb)) {
            chosen <- cb
            break
          }
        }
      }
      if (length(chosen) == 0L) {
        stop(sprintf(
          "cell pool exhausted placing family %s (%s, lineage %s): %d donors offer no free cell",
          fam$id,
          fam$anchor,
          paste(lineage, collapse = "/"),
          length(donor_pool)
        ))
      }
      used_cells <- c(used_cells, chosen)
      rows[[length(rows) + 1L]] <- data.frame(
        cell_barcode = chosen,
        v_gene = v_gene,
        j_gene = j_gene,
        cdr3 = node,
        motif_design = fam$id,
        stringsAsFactors = FALSE
      )
    }
  }

  ## Background: every remaining eligible cell gets its own unique CDR3, kept
  ## at Hamming distance >= 2 from everything, so "background = singletons" is
  ## exact and the Data & QC filter counts are real.
  rest <- setdiff(eligible_cells, used_cells)
  bg <- character(0)
  bg_v <- character(0)
  bg_j <- character(0)
  guard <- 0L
  while (length(bg) < length(rest) && guard < length(rest) * 200L) {
    guard <- guard + 1L
    v_gene <- sample(names(v_pool), 1L)
    j_gene <- sample(names(j_pool), 1L)
    mlen <- sample(
      as.integer(names(background_len_probs)),
      1L,
      prob = background_len_probs
    )
    cand <- make_cdr3(v_pool[[v_gene]], j_pool[[j_gene]], mlen)
    if (cand %in% taken || cand %in% bg) {
      next
    }
    if (collides(cand, c(taken, bg))) {
      next
    }
    bg <- c(bg, cand)
    bg_v <- c(bg_v, v_gene)
    bg_j <- c(bg_j, j_gene)
  }
  if (length(bg) < length(rest)) {
    stop(sprintf(
      "background generator stalled: %d of %d sequences placed",
      length(bg),
      length(rest)
    ))
  }
  rows[[length(rows) + 1L]] <- data.frame(
    cell_barcode = rest,
    v_gene = bg_v,
    j_gene = bg_j,
    cdr3 = bg,
    motif_design = "background",
    stringsAsFactors = FALSE
  )

  out <- do.call(rbind, rows)
  out[!duplicated(out$cell_barcode), ]
}

## Junction lengths for background, chosen so total CDR3 length lands in the
## same 12-20 band the design families occupy.
BG_LEN_P <- c(
  "4" = 0.06,
  "5" = 0.16,
  "6" = 0.24,
  "7" = 0.24,
  "8" = 0.18,
  "9" = 0.09,
  "10" = 0.03
)

cat("Assigning TRB ...\n")
trb <- assign_chain(trb_design, TRBV, TRBJ, TRB_DETECTION, BG_LEN_P)
cat("Assigning TRA ...\n")
tra <- assign_chain(tra_design, TRAV, TRAJ, TRA_DETECTION, BG_LEN_P)

cat(sprintf(
  "  TRB: %d cells, %d unique CDR3 | TRA: %d cells, %d unique CDR3\n",
  nrow(trb),
  length(unique(trb$cdr3)),
  nrow(tra),
  length(unique(tra$cdr3))
))

## Every designed member must have survived into the object. This is the guard
## that catches a silently fragmented family: it fires long before the network
## is looked at.
for (nm in c("trb", "tra")) {
  tab <- get(nm)
  design <- get(paste0(nm, "_design"))
  got <- tapply(tab$cdr3, tab$motif_design, function(x) length(unique(x)))
  got <- got[names(got) != "background"]
  want <- stats::setNames(design$size, design$id)
  missing <- want[names(want)] - got[names(want)]
  if (any(missing != 0L)) {
    stop(sprintf(
      "%s: designed families lost members (%s) — they would arrive as split components",
      toupper(nm),
      paste(
        sprintf(
          "%s -%d",
          names(missing)[missing != 0],
          missing[missing != 0]
        ),
        collapse = ", "
      )
    ))
  }
}

## ---- scRepertoire-style CT* strings ------------------------------------- ##
## Slot 1 = TRA, slot 2 = TRB, joined by "_"; absent chain = literal "NA".
ir_cells <- union(trb$cell_barcode, tra$cell_barcode)
a <- tra[match(ir_cells, tra$cell_barcode), ]
b <- trb[match(ir_cells, trb$cell_barcode), ]

a_gene <- ifelse(
  is.na(a$cdr3),
  "NA",
  paste0(a$v_gene, ".", a$j_gene, ".TRAC")
)
b_gene <- ifelse(
  is.na(b$cdr3),
  "NA",
  paste0(b$v_gene, ".None.", b$j_gene, ".TRBC2")
)
a_aa <- ifelse(is.na(a$cdr3), "NA", a$cdr3)
b_aa <- ifelse(is.na(b$cdr3), "NA", b$cdr3)
a_nt <- ifelse(
  is.na(a$cdr3),
  "NA",
  back_translate(ifelse(is.na(a$cdr3), "C", a$cdr3))
)
b_nt <- ifelse(
  is.na(b$cdr3),
  "NA",
  back_translate(ifelse(is.na(b$cdr3), "C", b$cdr3))
)

ir_df <- data.frame(
  barcode = ir_cells,
  CTgene = paste0(a_gene, "_", b_gene),
  CTnt = paste0(a_nt, "_", b_nt),
  CTaa = paste0(a_aa, "_", b_aa),
  CTstrict = paste0(
    a_gene,
    ";",
    a_nt,
    "_",
    b_gene,
    ";",
    b_nt
  ),
  stringsAsFactors = FALSE
)
ir_df$sample <- cells$sample[match(ir_df$barcode, cells$cell_barcode)]
immune_repertoire <- split(
  ir_df[, setdiff(colnames(ir_df), "sample")],
  ir_df$sample
)

## ---- Expression --------------------------------------------------------- ##
## Real gene SYMBOLS (a searchable vocabulary) carrying simulated values.
stopifnot(nzchar(symbol_src), file.exists(symbol_src))
symbols <- rownames(readRDS(symbol_src)$expression)
symbols <- union(symbols, c("IL2RA", "SELL", "CTLA4", "IKZF2", "B2M"))

MARKERS <- list(
  "CD8 T" = c("CD8A", "CD8B", "GZMK", "NKG7", "CD3D", "CD3E", "TRAC", "TRBC2"),
  "CD4 T" = c("CD4", "IL7R", "CCR7", "SELL", "CD3D", "CD3E", "TRAC", "TRBC2"),
  "Treg" = c(
    "FOXP3",
    "IL2RA",
    "CTLA4",
    "TIGIT",
    "IKZF2",
    "CD3D",
    "CD3E",
    "TRAC"
  ),
  "B cells" = c("MS4A1", "CD79A", "CD79B"),
  "Monocytes" = c("LYZ", "CD14", "S100A8", "FCGR3A")
)
HOUSEKEEPING <- c("ACTB", "GAPDH", "B2M", "TMSB4X")
MARKERS <- lapply(MARKERS, function(x) intersect(x, symbols))
stopifnot(all(lengths(MARKERS) > 0))

n_genes <- length(symbols)
n_cells <- nrow(cells)
gene_idx <- setNames(seq_along(symbols), symbols)

## Sparse assembly: a low background of detected genes per cell, plus the
## lineage's markers boosted well above it.
set.seed(20260715)
i_list <- vector("list", n_cells)
j_list <- vector("list", n_cells)
x_list <- vector("list", n_cells)
for (k in seq_len(n_cells)) {
  ct <- cells$cell_type_fine[k]
  n_bg <- rpois(1, 70)
  bg_genes <- sample.int(n_genes, min(n_bg, n_genes))
  bg_val <- rgamma(length(bg_genes), shape = 1.1, rate = 1.7)
  mk <- unique(c(MARKERS[[ct]], HOUSEKEEPING))
  mk_idx <- unname(gene_idx[mk])
  mk_val <- rgamma(length(mk_idx), shape = 9, rate = 2.2)
  ii <- c(bg_genes, mk_idx)
  xx <- c(bg_val, mk_val)
  keep <- !duplicated(ii, fromLast = TRUE)
  i_list[[k]] <- ii[keep]
  j_list[[k]] <- rep.int(k, sum(keep))
  x_list[[k]] <- pmin(xx[keep], 8.4)
}
expression <- sparseMatrix(
  i = unlist(i_list),
  j = unlist(j_list),
  x = unlist(x_list),
  dims = c(n_genes, n_cells),
  dimnames = list(symbols, cells$cell_barcode)
)

## ---- Projection --------------------------------------------------------- ##
## Per-lineage gaussian blobs with a curved T-cell arm, so the shared
## Projection / Gene expression tabs render something that reads as a UMAP.
CENTRES <- list(
  "CD8 T" = c(-4.5, 2.0),
  "CD4 T" = c(-1.0, 5.0),
  "Treg" = c(1.5, 7.2),
  "B cells" = c(6.5, -3.0),
  "Monocytes" = c(-6.0, -6.5)
)
SPREAD <- c(
  "CD8 T" = 1.5,
  "CD4 T" = 1.4,
  "Treg" = 0.7,
  "B cells" = 1.1,
  "Monocytes" = 0.8
)
umap <- t(vapply(
  seq_len(n_cells),
  function(k) {
    ct <- cells$cell_type_fine[k]
    ctr <- CENTRES[[ct]]
    s <- SPREAD[[ct]]
    p <- c(rnorm(1, ctr[1], s), rnorm(1, ctr[2], s))
    ## Bend the T-cell arm so the island is not a plain circle.
    if (ct %in% T_LINEAGES) {
      p[2] <- p[2] + 0.10 * p[1]^2 * 0.25
    }
    p
  },
  numeric(2)
))
projection <- data.frame(umap_1 = umap[, 1], umap_2 = umap[, 2])
rownames(projection) <- cells$cell_barcode

## ---- Metadata ----------------------------------------------------------- ##
meta <- data.frame(
  cell_barcode = cells$cell_barcode,
  sample = factor(cells$sample, levels = DONORS),
  cell_type = factor(cells$cell_type),
  cell_type_fine = factor(cells$cell_type_fine, levels = names(CELL_TYPES)),
  nUMI = round(Matrix::colSums(expression) * 340 + rnorm(n_cells, 0, 90)),
  nGene = Matrix::colSums(expression > 0),
  percent.mt = round(pmax(0, rnorm(n_cells, 4.2, 1.4)), 2),
  stringsAsFactors = FALSE
)
meta$nUMI <- pmax(meta$nUMI, 500)

## ---- Assemble ----------------------------------------------------------- ##
crb <- Cerebro_v1.3$new()
crb$expression <- expression
crb$setMetaData(meta)
crb$projections <- list(umap = projection)
crb$groups <- list(
  sample = DONORS,
  cell_type = levels(meta$cell_type),
  cell_type_fine = levels(meta$cell_type_fine)
)
crb$immune_repertoire <- immune_repertoire
crb$experiment <- list(
  experiment_name = "Synthetic cohort - HLA & TCR motifs (fixture)",
  organism = "hg",
  date_of_export = Sys.Date(),
  hla_tcr_demo_scope = paste(
    "FULLY SYNTHETIC software fixture: simulated expression, projection,",
    "cell types, CDR3 sequences and HLA genotypes. The HLA-motif association",
    "shown by this data set was constructed on purpose and is not evidence."
  )
)

## The three declared contracts the HLA page reads. `tcr_selection = "synthetic"`
## is the strongest disclosure the page has: both the receptors and their HLA
## association are fabricated.
crb$technical_info <- list(
  observation_unit = "cell",
  receptor_key = "v_gene+cdr3",
  tcr_selection = "synthetic",
  tcr_selection_detail = paste(
    "Every sequence, genotype and association in this data set is fabricated.",
    "Motif families were designed to sit at Hamming distance 1 and were then",
    "assigned to carriers of a chosen allele, so any carrier/non-carrier",
    "contrast shown here was put there by construction. It demonstrates the",
    "page's mechanics and is not evidence of anything."
  )
)

crb$addHLATyping(
  donor_typing,
  source_type = "synthetic",
  typing_method = "synthetic (European allele frequency ranges)",
  source_reference = "data-raw/build_hla_tcr_demo.R"
)

dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
saveRDS(crb, out)
cat(sprintf("\nWrote %s (%.1f MB)\n", out, file.info(out)$size / 1024^2))

## ---- Verify against the package's OWN motif core ------------------------ ##
## The design parameters above are intent. What matters is what
## hla_process_length_group() actually builds from the shipped object, so the
## numbers reported here are measured, never assumed.
check <- readRDS(out)
ir_check <- check$getImmuneRepertoire()
cat("\nMeasured with the package motif core (NOT design parameters):\n")
cat(
  "  chains detected:",
  paste(
    CerebroNexus:::hla_detect_chains(ir_check),
    collapse = ", "
  ),
  "\n"
)

for (ch in c("TRB", "TRA")) {
  seg <- CerebroNexus:::hla_parse_ir_segments(ir_check, ch)
  nodes <- CerebroNexus:::hla_aggregate_cdr3_nodes(seg, by_v = TRUE)
  built <- CerebroNexus:::hla_build_motif_groups(nodes, by_v = TRUE)
  m <- built$motif_df
  sizes <- tapply(m$motif_size, m$motif_group, function(x) x[1])
  in_motif <- m[m$motif_size >= 2L, ]
  fams <- unique(in_motif$motif_group)
  diam <- tapply(in_motif$motif_diameter, in_motif$motif_group, function(x) {
    x[1]
  })
  cat(sprintf(
    "  %s: %d unique nodes | %d nodes in motifs (>=2) | %d motifs | sizes %d-%d | diameter %d-%d\n",
    ch,
    nrow(m),
    nrow(in_motif),
    length(fams),
    min(sizes[sizes >= 2]),
    max(sizes),
    min(diam),
    max(diam)
  ))

  ## Designed family count and the sizes the core actually recovers must match
  ## exactly. A fragmented family shows up here as extra motifs and as sizes the
  ## design never asked for, which is precisely how the split above was found.
  design <- if (ch == "TRB") trb_design else tra_design
  observed <- sort(unname(sizes[sizes >= 2L]))
  expected <- sort(design$size)
  if (!identical(as.integer(observed), as.integer(expected))) {
    stop(sprintf(
      "%s: recovered motif sizes do not match the design.\n  designed: %s\n  observed: %s",
      ch,
      paste(expected, collapse = ", "),
      paste(observed, collapse = ", ")
    ))
  }
}

typing <- check$getHLATyping()
stopifnot(
  nrow(typing) > 0,
  all(typing$source_type == "synthetic"),
  identical(check$technical_info$tcr_selection, "synthetic"),
  identical(check$technical_info$observation_unit, "cell")
)

## Only the four loci the page enforces may be present.
stopifnot(all(typing$locus %in% c("HLA-A", "HLA-B", "HLA-C", "HLA-DRB1")))

summ <- CerebroNexus:::hla_allele_carrier_summary(typing, samples = DONORS)
summ$contrast <- pmin(summ$n_carrier, summ$n_noncarrier)
ranked <- summ[order(-summ$contrast, -summ$n_carrier, summ$allele), ]
cat("\nAllele picker will lead with:\n")
for (r in seq_len(min(5L, nrow(ranked)))) {
  cat(sprintf(
    "  %s - %d carrier / %d non-carrier\n",
    ranked$allele[r],
    ranked$n_carrier[r],
    ranked$n_noncarrier[r]
  ))
}
## The concentrated families hang off ANCHOR_PRIMARY, so the fixture only tells
## its story if the picker actually opens on that allele. A pool-drawn allele
## can tie on contrast, so this is checked rather than assumed.
if (!identical(ranked$allele[1], ANCHOR_PRIMARY)) {
  stop(sprintf(
    "allele picker would open on %s, not the anchor %s: the concentrated families would not be the first thing shown",
    ranked$allele[1],
    ANCHOR_PRIMARY
  ))
}

## The payload claim of this fixture: colouring by the primary anchor must light
## whole islands solid "Carrier", not wash everything to "Mixed". Measured
## through the same core the page calls.
md <- check$getMetaData()
ir_annot <- lapply(ir_check, function(df) {
  idx <- match(df$barcode, md$cell_barcode)
  for (col in setdiff(colnames(md), "cell_barcode")) {
    df[[col]] <- md[[col]][idx]
  }
  df
})
seg <- CerebroNexus:::hla_parse_ir_segments(ir_annot, "TRB")
nodes <- CerebroNexus:::hla_aggregate_cdr3_nodes(
  seg,
  meta_cols = c("sample", "cell_type_fine"),
  by_v = TRUE
)
m <- CerebroNexus:::hla_build_motif_groups(nodes, by_v = TRUE)$motif_df
m <- m[m$motif_size >= 2L, ]
st <- CerebroNexus:::hla_node_carrier_status(
  m$samples_all,
  typing,
  names(ir_annot),
  ANCHOR_PRIMARY
)
pure <- tapply(st, m$motif_group, function(x) all(x == "Carrier"))
cat(sprintf("\nColouring TRB by %s:\n", ANCHOR_PRIMARY))
print(table(st))
cat(sprintf(
  "  motifs entirely Carrier: %d / %d | overall Carrier rate: %.0f%%\n",
  sum(pure),
  length(pure),
  100 * mean(st == "Carrier")
))
## Most members are private to one donor, so an UNASSOCIATED node just inherits
## that donor's status: with the anchor at 15/30 carriers the background sits
## near 50% Carrier by construction. The readable contrast is therefore "the
## anchored family entirely red against a red/blue background", not "red against
## Mixed". Both halves must hold — the strong families solid, the rest NOT — or
## there is no contrast to see.
stopifnot(sum(pure) >= 2L, mean(st == "Carrier") < 0.75)

origin <- CerebroNexus:::hla_node_sample_origin(m$samples_all)
shared_rate <- mean(origin == CerebroNexus:::HLA_SHARED_LABEL, na.rm = TRUE)
cat(sprintf(
  "Sample origin: %d of %d motif nodes Shared (%.0f%%)\n",
  sum(origin == CerebroNexus:::HLA_SHARED_LABEL, na.rm = TRUE),
  length(origin),
  100 * shared_rate
))
## "Shared" is the level the eye is meant to hunt for, so it has to stay a
## visible minority: all-shared paints the whole network black (the first build
## did exactly that at 419/430), none-shared makes the level pointless.
stopifnot(shared_rate > 0.03, shared_rate < 0.45)

cat("\nRound-trip verification passed.\n")
