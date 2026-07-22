# Tests for descriptive feature x HLA overlap. These functions intentionally
# perform no inferential statistics.

# Every locus meant to support a NON-carrier call is written with both copies:
# one copy leaves the second unknown, which is untyped, not negative (see
# hla_locus_call_state). s3 is the fixture's non-carrier, so it gets a full
# diploid call; s4 stays HLA-A-untyped on purpose.
make_overlap_typing <- function() {
  hla_normalize_typing(
    data.frame(
      sample = c("s1", "s1", "s2", "s2", "s3", "s3", "s4", "s4"),
      donor_id = c("d1", "d1", "d1", "d1", "d2", "d2", "d3", "d3"),
      locus = c(
        "HLA-A",
        "HLA-A",
        "HLA-A",
        "HLA-A",
        "HLA-A",
        "HLA-A",
        "HLA-B",
        "HLA-B"
      ),
      allele = c(
        "HLA-A*02:01",
        "HLA-A*11:01",
        "HLA-A*02:01",
        "HLA-A*11:01",
        "HLA-A*01:01",
        "HLA-A*11:01",
        "HLA-B*08:01",
        "HLA-B*07:02"
      ),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )
}

make_overlap_segments <- function() {
  data.frame(
    sample = c("s1", "s1", "s2", "s2", "s3", "s3", "s4"),
    cdr3 = c("CASSL", "CASSL", "CASSF", "OTHER", "CASSL", "OTHER", "OTHER"),
    stringsAsFactors = FALSE
  )
}

test_that("analysis unit collapses samples only with complete donor mapping", {
  typing <- make_overlap_typing()

  donor_map <- hla_analysis_unit_map(typing, c("s1", "s2", "s3"))
  expect_true(all(donor_map$unit_type == "donor"))
  expect_equal(donor_map$analysis_unit, c("d1", "d1", "d2"))

  typing$donor_id[typing$sample == "s3"] <- NA_character_
  sample_map <- hla_analysis_unit_map(typing, c("s1", "s2", "s3"))
  expect_true(all(sample_map$unit_type == "sample"))
  expect_equal(sample_map$analysis_unit, c("s1", "s2", "s3"))
})

test_that("allele status is locus-specific at the analysis-unit level", {
  typing <- make_overlap_typing()

  status <- hla_allele_status_by_unit(
    typing,
    samples = c("s1", "s2", "s3", "s4"),
    allele = "HLA-A*02:01"
  )

  expect_equal(status$hla_status[status$analysis_unit == "d1"], "carrier")
  expect_equal(status$hla_status[status$analysis_unit == "d2"], "non-carrier")
  expect_equal(status$hla_status[status$analysis_unit == "d3"], "untyped")
})

test_that("locus call completeness needs two copies at the locus", {
  typing <- hla_normalize_typing(
    data.frame(
      sample = c("s1", "s1", "s2", "s3"),
      locus = c("HLA-A", "HLA-A", "HLA-A", "HLA-B"),
      allele = c("HLA-A*01:01", "HLA-A*03:01", "HLA-A*01:01", "HLA-B*08:01"),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )

  state <- hla_locus_call_state(typing, c("s1", "s2", "s3"), "HLA-A")
  expect_equal(state$call_state[state$sample == "s1"], "complete")
  expect_equal(state$call_state[state$sample == "s2"], "partial")
  expect_equal(state$call_state[state$sample == "s3"], "absent")
})

test_that("a homozygous call written twice still reads as complete", {
  typing <- hla_normalize_typing(
    data.frame(
      sample = c("s1", "s1"),
      locus = c("HLA-A", "HLA-A"),
      allele = c("HLA-A*01:01", "HLA-A*01:01"),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )

  state <- hla_locus_call_state(typing, "s1", "HLA-A")
  expect_equal(state$call_state, "complete")
})

test_that("a half-typed locus cannot rule an allele out", {
  # s2's second HLA-A copy is unknown, so it may yet be A*02:01. Calling it a
  # non-carrier would put a possible carrier in the comparison group.
  typing <- hla_normalize_typing(
    data.frame(
      sample = c("s1", "s1", "s2"),
      locus = c("HLA-A", "HLA-A", "HLA-A"),
      allele = c("HLA-A*01:01", "HLA-A*03:01", "HLA-A*01:01"),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )

  status <- hla_allele_status_by_unit(typing, c("s1", "s2"), "HLA-A*02:01")
  expect_equal(status$hla_status[status$analysis_unit == "s1"], "non-carrier")
  expect_equal(status$hla_status[status$analysis_unit == "s2"], "untyped")
})

test_that("a half-typed locus can still rule an allele IN", {
  # Knowing one copy IS the query settles carriage; the unknown copy cannot
  # un-carry it. Incomplete typing blocks negative calls only.
  typing <- hla_normalize_typing(
    data.frame(
      sample = "s1",
      locus = "HLA-A",
      allele = "HLA-A*02:01",
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )

  status <- hla_allele_status_by_unit(typing, "s1", "HLA-A*02:01")
  expect_equal(status$hla_status, "carrier")
})

test_that("donor completeness comes from one sample, not pooled samples", {
  # d1 has two samples with one HLA-A copy each. Pooling would count two rows
  # and read as a complete diploid call; neither sample actually typed copy 2.
  typing <- hla_normalize_typing(
    data.frame(
      sample = c("s1", "s2"),
      donor_id = c("d1", "d1"),
      locus = c("HLA-A", "HLA-A"),
      allele = c("HLA-A*01:01", "HLA-A*01:01"),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )

  status <- hla_allele_status_by_unit(typing, c("s1", "s2"), "HLA-A*02:01")
  expect_equal(status$hla_status[status$analysis_unit == "d1"], "untyped")
})

test_that("feature overlap reports donor-level presence breadth and cell fraction", {
  overlap <- hla_descriptive_feature_overlap(
    typing = make_overlap_typing(),
    segments = make_overlap_segments(),
    samples = c("s1", "s2", "s3", "s4"),
    allele = "HLA-A*02:01",
    feature_cdr3 = c("CASSL", "CASSF")
  )

  d1 <- overlap[overlap$analysis_unit == "d1", ]
  expect_true(d1$feature_present)
  expect_equal(d1$n_cells, 4L)
  expect_equal(d1$n_feature_cells, 3L)
  expect_equal(d1$n_unique_clonotypes, 3L)
  expect_equal(d1$n_feature_clonotypes, 2L)
  expect_equal(d1$unique_clonotype_fraction, 2 / 3)
  expect_equal(d1$cell_fraction, 3 / 4)
  expect_equal(d1$hla_status, "carrier")

  d3 <- overlap[overlap$analysis_unit == "d3", ]
  expect_false(d3$feature_present)
  expect_equal(d3$hla_status, "untyped")
})

test_that("feature overlap can freeze V-specific members", {
  typing <- hla_normalize_typing(
    list(s1 = "HLA-A*02:01"),
    source_type = "genotyped"
  )
  segments <- data.frame(
    sample = c("s1", "s1"),
    v_gene = c("TRBV1", "TRBV9"),
    cdr3 = c("CASSL", "CASSL"),
    stringsAsFactors = FALSE
  )

  overlap <- hla_descriptive_feature_overlap(
    typing = typing,
    segments = segments,
    samples = "s1",
    allele = "HLA-A*02:01",
    feature_cdr3 = "CASSL",
    feature_v_gene = "TRBV1"
  )

  expect_equal(overlap$n_feature_cells, 1L)
  expect_equal(overlap$n_unique_clonotypes, 2L)
  expect_equal(overlap$n_feature_clonotypes, 1L)
  expect_equal(overlap$unique_clonotype_fraction, 0.5)
  expect_equal(overlap$cell_fraction, 0.5)
})

## ---- per-node carrier status (render-time colouring) ------------------- ##

test_that("node carrier status summarises the node's samples for one allele", {
  typing <- make_overlap_typing()
  samples <- c("s1", "s2", "s3", "s4")
  # d1 (s1,s2) carries A*02:01; d2 (s3) is locus-typed non-carrier;
  # d3 (s4) has only HLA-B typing -> untyped at the HLA-A locus.
  status <- hla_node_carrier_status(
    samples_all = c("s1", "s1,s2", "s3", "s1,s3", "s4", "s3,s4"),
    typing = typing,
    samples = samples,
    allele = "HLA-A*02:01"
  )
  expect_equal(status[1], "Carrier") # s1 only
  expect_equal(status[2], "Carrier") # s1+s2, both the same donor d1
  expect_equal(status[3], "Non-carrier") # s3 typed at HLA-A, lacks the allele
  expect_equal(status[4], "Mixed") # carrier + non-carrier
  expect_equal(status[5], "Untyped") # s4 has no HLA-A typing
  # untyped alongside a non-carrier must not invent a carrier class
  expect_equal(status[6], "Non-carrier")
})

test_that("carrier counts expose untyped units a label alone would hide", {
  # s1 carries A*02:01; s2 is typed at HLA-B only -> untyped at the HLA-A locus.
  typing <- hla_normalize_typing(
    data.frame(
      sample = c("s1", "s2"),
      locus = c("HLA-A", "HLA-B"),
      allele = c("HLA-A*02:01", "HLA-B*08:01"),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )
  samples <- c("s1", "s2")
  st <- hla_node_carrier_status("s1,s2", typing, samples, "HLA-A*02:01")
  cnt <- hla_node_carrier_counts("s1,s2", typing, samples, "HLA-A*02:01")
  # The label says Carrier ("no evidence against"), and the counts must reveal
  # that half the node's units were never typed at this locus.
  expect_equal(st, "Carrier")
  expect_equal(cnt$n_carrier, 1L)
  expect_equal(cnt$n_noncarrier, 0L)
  expect_equal(cnt$n_untyped, 1L)
})

test_that("carrier label is always consistent with its counts", {
  typing <- make_overlap_typing()
  samples <- c("s1", "s2", "s3", "s4")
  nodes <- c("s1", "s3", "s1,s3", "s4", "s3,s4", NA_character_)
  st <- hla_node_carrier_status(nodes, typing, samples, "HLA-A*02:01")
  cnt <- hla_node_carrier_counts(nodes, typing, samples, "HLA-A*02:01")
  derived <- ifelse(
    cnt$n_carrier > 0 & cnt$n_noncarrier > 0,
    "Mixed",
    ifelse(
      cnt$n_carrier > 0,
      "Carrier",
      ifelse(cnt$n_noncarrier > 0, "Non-carrier", "Untyped")
    )
  )
  expect_equal(st, derived)
})

test_that("carrier counts count donors once, not each of their samples", {
  # s1 and s2 are the same donor d1: a node in both is ONE carrier unit.
  typing <- make_overlap_typing()
  cnt <- hla_node_carrier_counts(
    "s1,s2",
    typing,
    c("s1", "s2", "s3", "s4"),
    "HLA-A*02:01"
  )
  expect_equal(cnt$n_carrier, 1L)
})

test_that("node carrier status is Untyped without usable typing", {
  empty <- hla_normalize_typing(list(), source_type = "unknown")
  status <- hla_node_carrier_status(
    samples_all = c("s1", "s2"),
    typing = empty,
    samples = c("s1", "s2"),
    allele = "HLA-A*02:01"
  )
  expect_equal(status, c("Untyped", "Untyped"))
})

test_that("node carrier status handles empty and NA sample sets", {
  typing <- make_overlap_typing()
  expect_equal(
    hla_node_carrier_status(character(0), typing, "s1", "HLA-A*02:01"),
    character(0)
  )
  expect_equal(
    hla_node_carrier_status(NA_character_, typing, "s1", "HLA-A*02:01"),
    "Untyped"
  )
})

test_that("unit by allele matrix distinguishes non-carrier from locus-untyped", {
  mat <- hla_unit_allele_matrix(
    make_overlap_typing(),
    samples = c("s1", "s2", "s3", "s4")
  )

  expect_equal(mat[mat$analysis_unit == "d1", "HLA-A*02:01"], 1L)
  expect_equal(mat[mat$analysis_unit == "d2", "HLA-A*02:01"], 0L)
  expect_true(is.na(mat[mat$analysis_unit == "d3", "HLA-A*02:01"]))
  expect_equal(mat[mat$analysis_unit == "d3", "HLA-B*08:01"], 1L)
})

## ---- per-allele evidence scope ----------------------------------------- ##

scope_seg <- function() {
  # s1 carries A*02:01, s2 does not. Each has a CD8 and a CD4 cell.
  df <- data.frame(
    barcode = c("a", "b", "c", "d"),
    CTgene = "TRBV1.TRBJ2",
    CTaa = c("CASSL", "CASSF", "CASSW", "CASSY"),
    sample = c("s1", "s1", "s2", "s2"),
    mhc_context = c("Class I", "Class II", "Class I", "Class II"),
    stringsAsFactors = FALSE
  )
  hla_parse_ir_segments(list(s1 = df), "TRB")
}

scope_typing <- function() {
  hla_normalize_typing(
    list(
      s1 = c("HLA-A*02:01", "HLA-A*01:01", "HLA-DRB1*15:01", "HLA-DRB1*03:01"),
      s2 = c("HLA-A*01:01", "HLA-A*03:01", "HLA-DRB1*03:01", "HLA-DRB1*04:01")
    ),
    source_type = "genotyped"
  )
}

test_that("a class I allele scopes to carriers' class I cells only", {
  out <- hla_scope_segments_by_allele(
    scope_seg(),
    scope_typing(),
    "HLA-A*02:01"
  )
  # s1 carries it; only s1's Class I cell survives. s2 is a non-carrier, and
  # s1's CD4 cell cannot be restricted by a class I allele.
  expect_equal(nrow(out), 1L)
  expect_equal(out$cdr3, "CASSL")
})

test_that("a class II allele scopes to carriers' class II cells only", {
  out <- hla_scope_segments_by_allele(
    scope_seg(),
    scope_typing(),
    "HLA-DRB1*15:01"
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$cdr3, "CASSF")
})

test_that("an allele both samples carry still splits by class", {
  out <- hla_scope_segments_by_allele(
    scope_seg(),
    scope_typing(),
    "HLA-A*01:01"
  )
  expect_setequal(out$cdr3, c("CASSL", "CASSW"))
  expect_setequal(out$sample, c("s1", "s2"))
})

test_that("scoping drops Unknown context rather than assuming a class", {
  seg <- scope_seg()
  seg$mhc_context <- "Unknown"
  out <- hla_scope_segments_by_allele(seg, scope_typing(), "HLA-A*02:01")
  expect_equal(nrow(out), 0L)
})

test_that("without a context column the scope is carrier-only", {
  # A bulk repertoire has no lineage, so class matching is impossible. The
  # carrier filter must still apply rather than the whole scope silently
  # passing everything through.
  seg <- scope_seg()
  seg$mhc_context <- NULL
  out <- hla_scope_segments_by_allele(seg, scope_typing(), "HLA-A*02:01")
  expect_equal(nrow(out), 2L)
  expect_true(all(out$sample == "s1"))
})

test_that("an allele nobody carries scopes to nothing, not to everything", {
  out <- hla_scope_segments_by_allele(
    scope_seg(),
    scope_typing(),
    "HLA-B*07:02"
  )
  expect_equal(nrow(out), 0L)
})

test_that("scoping refuses rather than guesses without usable inputs", {
  expect_null(hla_scope_segments_by_allele(scope_seg(), NULL, "HLA-A*02:01"))
  expect_null(hla_scope_segments_by_allele(scope_seg(), scope_typing(), ""))
  expect_null(hla_scope_segments_by_allele(scope_seg(), scope_typing(), NA))
})

test_that("scoping a carrier's cells keeps every class I allele's receptors", {
  # The honesty limit worth pinning: s1 carries A*02:01 AND A*01:01, so scoping
  # to A*02:01 keeps ALL of s1's class I receptors. The scope is candidate
  # co-occurrence; it cannot attribute a receptor to one of the donor's alleles.
  a2 <- hla_scope_segments_by_allele(scope_seg(), scope_typing(), "HLA-A*02:01")
  a1 <- hla_scope_segments_by_allele(scope_seg(), scope_typing(), "HLA-A*01:01")
  expect_true(all(a2$cdr3 %in% a1$cdr3))
})

## ---- typing resolution must not manufacture non-carriers --------------- ##
## HLA typing arrives at whatever resolution the lab reported and is never
## zero-padded, so "HLA-A*02" and "HLA-A*02:01" are different strings for the
## same molecule family. Exact string matching mis-called BOTH directions, and
## both errors push people into the comparison group that must not hold them.

res_typing <- function() {
  hla_normalize_typing(
    list(
      donor_lo = c("HLA-A*02", "HLA-A*01:01"), # 1-field only
      donor_hi = c("HLA-A*02:01", "HLA-A*01:01"), # 2-field
      donor_no = c("HLA-A*03:01", "HLA-A*01:01") # definitely not A*02
    ),
    source_type = "genotyped"
  )
}
res_samples <- c("donor_lo", "donor_hi", "donor_no")

status_of <- function(allele, unit) {
  st <- hla_allele_status_by_unit(res_typing(), res_samples, allele)
  st$hla_status[st$analysis_unit == unit]
}

test_that("coarser typing than the query is untyped, never non-carrier", {
  # donor_lo was typed A*02 and A*02:01 IS an A*02, so this donor may well
  # carry it. Calling them a non-carrier puts a possible carrier into the
  # "definitely lacks it" group and biases the very contrast this page rests on.
  expect_equal(status_of("HLA-A*02:01", "donor_lo"), "untyped")
})

test_that("finer typing than the query is a carrier", {
  # donor_hi is A*02:01, which is an A*02. Anything else is a false negative.
  expect_equal(status_of("HLA-A*02", "donor_hi"), "carrier")
})

test_that("a real mismatch is still a non-carrier", {
  expect_equal(status_of("HLA-A*02:01", "donor_no"), "non-carrier")
  expect_equal(status_of("HLA-A*02", "donor_no"), "non-carrier")
})

test_that("an exact match is still a carrier", {
  expect_equal(status_of("HLA-A*02:01", "donor_hi"), "carrier")
  expect_equal(status_of("HLA-A*01:01", "donor_lo"), "carrier")
})

test_that("a definite carrier at one copy beats an ambiguous other copy", {
  # Both copies are inspected: A*02:01 settles it regardless of the A*02.
  typing <- hla_normalize_typing(
    list(d1 = c("HLA-A*02", "HLA-A*02:01")),
    source_type = "genotyped"
  )
  st <- hla_allele_status_by_unit(typing, "d1", "HLA-A*02:01")
  expect_equal(st$hla_status, "carrier")
})

test_that("ambiguity at one copy and a mismatch at the other is untyped", {
  typing <- hla_normalize_typing(
    list(d1 = c("HLA-A*02", "HLA-A*03:01")),
    source_type = "genotyped"
  )
  st <- hla_allele_status_by_unit(typing, "d1", "HLA-A*02:01")
  expect_equal(st$hla_status, "untyped")
})

test_that("an untyped locus stays untyped", {
  typing <- hla_normalize_typing(
    list(d1 = c("HLA-B*07:02")),
    source_type = "genotyped"
  )
  st <- hla_allele_status_by_unit(typing, "d1", "HLA-A*02:01")
  expect_equal(st$hla_status, "untyped")
})

test_that("field prefixes do not match across loci or on partial digits", {
  # A*02 must not match B*02, and A*2 must not match A*24 (fields compare whole,
  # never as string prefixes).
  # d1's HLA-A is called at both copies, so "non-carrier" here is the compare
  # logic talking, not an incomplete call.
  typing <- hla_normalize_typing(
    list(d1 = c("HLA-A*24:02", "HLA-A*11:01"), d2 = c("HLA-B*02:01")),
    source_type = "genotyped"
  )
  st <- hla_allele_status_by_unit(typing, c("d1", "d2"), "HLA-A*02")
  expect_equal(st$hla_status[st$analysis_unit == "d1"], "non-carrier")
  expect_equal(st$hla_status[st$analysis_unit == "d2"], "untyped")
})

test_that("the per-allele scope follows the same resolution rule", {
  # The scope must not drop a donor whose typing REFINES the queried allele.
  seg <- hla_parse_ir_segments(
    list(
      s = data.frame(
        barcode = c("a", "b"),
        CTgene = "TRBV1.TRBJ2",
        CTaa = c("CASSL", "CASSF"),
        sample = c("donor_hi", "donor_no"),
        mhc_context = "Class I",
        stringsAsFactors = FALSE
      )
    ),
    "TRB"
  )
  out <- hla_scope_segments_by_allele(seg, res_typing(), "HLA-A*02")
  expect_equal(nrow(out), 1L)
  expect_equal(out$sample, "donor_hi")
})

test_that("an all-Unknown context column empties a class-matched scope", {
  # Why the lineage column must be found by what its labels RESOLVE to, not by
  # its name: a coarse annotation ("T cells") produces a context column that is
  # entirely Unknown, and class-matching against it keeps nothing at all. The
  # page must report no lineage instead of drawing an empty network.
  seg <- data.frame(
    sample = c("s1", "s1", "s2"),
    cdr3 = c("CASSL", "CASSF", "CASSL"),
    mhc_context = "Unknown",
    stringsAsFactors = FALSE
  )
  typing <- hla_normalize_typing(
    list(s1 = c("HLA-A*02:01", "HLA-A*11:01"), s2 = c("HLA-A*02:01")),
    source_type = "genotyped"
  )

  matched <- hla_scope_segments_by_allele(
    seg,
    typing,
    "HLA-A*02:01",
    context_col = "mhc_context"
  )
  expect_equal(nrow(matched), 0L)

  # With no lineage claimed, the same scope keeps the carriers.
  unmatched <- hla_scope_segments_by_allele(
    seg,
    typing,
    "HLA-A*02:01",
    context_col = NULL
  )
  expect_equal(nrow(unmatched), 3L)
})

test_that("coarse annotation resolves to no lineage at all", {
  expect_true(all(
    hla_lineage_context(c("T cells", "B cells", "Monocytes")) == "Unknown"
  ))
  expect_equal(hla_lineage_context("CD8 TEM"), "Class I")
  expect_equal(hla_lineage_context("Treg"), "Class II")
})

## ---- Class I x Class II pair scope ------------------------------------ ##

make_pair_typing <- function() {
  hla_normalize_typing(
    data.frame(
      sample = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
      locus = c(
        "HLA-A",
        "HLA-A",
        "HLA-DRB1",
        "HLA-DRB1",
        "HLA-A",
        "HLA-A",
        "HLA-DRB1",
        "HLA-DRB1"
      ),
      allele = c(
        "HLA-A*02:01",
        "HLA-A*11:01",
        "HLA-DRB1*15:01",
        "HLA-DRB1*04:01",
        "HLA-A*01:01",
        "HLA-A*11:01",
        "HLA-DRB1*15:01",
        "HLA-DRB1*04:01"
      ),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )
}

make_pair_segments <- function() {
  data.frame(
    sample = c("s1", "s1", "s2", "s2", "s1"),
    cdr3 = c("CASSA", "CASSB", "CASSC", "CASSD", "CASSE"),
    mhc_context = c(
      "Class I",
      "Class II",
      "Class I",
      "Class II",
      "Unknown"
    ),
    stringsAsFactors = FALSE
  )
}

test_that("the pair scope assigns each cell the allele its lineage would use", {
  # s1 carries A*02:01 AND DRB1*15:01; s2 carries only DRB1*15:01.
  out <- hla_scope_segments_by_allele_pair(
    make_pair_segments(),
    make_pair_typing(),
    allele_i = "HLA-A*02:01",
    allele_ii = "HLA-DRB1*15:01",
    context_col = "mhc_context"
  )

  # s1 Class I cell -> the Class I allele it carries
  expect_equal(out$pair_allele[out$cdr3 == "CASSA"], "HLA-A*02:01")
  # s1 Class II cell -> the Class II allele
  expect_equal(out$pair_allele[out$cdr3 == "CASSB"], "HLA-DRB1*15:01")
  # s2 does NOT carry A*02:01, so its Class I cell has no candidate: dropped
  expect_false("CASSC" %in% out$cdr3)
  # s2 carries DRB1*15:01, so its Class II cell stays
  expect_equal(out$pair_allele[out$cdr3 == "CASSD"], "HLA-DRB1*15:01")
  # Unknown lineage cannot claim either allele
  expect_false("CASSE" %in% out$cdr3)
})

test_that("the pair scope needs a lineage to assign anything", {
  # Without lineage every cell is Unknown, so no cell can be said to use one
  # class's allele rather than the other's. The scope is undefined, not empty.
  expect_null(
    hla_scope_segments_by_allele_pair(
      make_pair_segments(),
      make_pair_typing(),
      allele_i = "HLA-A*02:01",
      allele_ii = "HLA-DRB1*15:01",
      context_col = NULL
    )
  )
})

test_that("the pair scope refuses two alleles of the same class", {
  expect_null(
    hla_scope_segments_by_allele_pair(
      make_pair_segments(),
      make_pair_typing(),
      allele_i = "HLA-A*02:01",
      allele_ii = "HLA-A*11:01",
      context_col = "mhc_context"
    )
  )
})

test_that("a CDR3 in both compartments summarises as Mixed", {
  expect_equal(
    hla_pair_class_summary(c("HLA-A*02:01", "HLA-A*02:01")),
    "HLA-A*02:01"
  )
  expect_equal(
    hla_pair_class_summary(c("HLA-A*02:01", "HLA-DRB1*15:01")),
    HLA_PAIR_MIXED_LABEL
  )
  expect_true(is.na(hla_pair_class_summary(c(NA_character_, NA_character_))))
})
