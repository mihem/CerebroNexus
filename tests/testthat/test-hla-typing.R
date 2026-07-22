# Tests for HLA typing normalization (R/hla_typing.R). Pure functions.

## ---- allele normalization --------------------------------------------- ##

test_that("allele normalization accepts common input forms", {
  expect_equal(hla_normalize_allele("HLA-A*02:01"), "HLA-A*02:01")
  expect_equal(hla_normalize_allele("A*02:01"), "HLA-A*02:01")
  expect_equal(hla_normalize_allele("02:01", locus = "HLA-A"), "HLA-A*02:01")
  expect_equal(hla_normalize_allele("a*02:01"), "HLA-A*02:01") # case-fold
})

test_that("allele normalization treats NNNN / empty / NA as missing", {
  expect_true(is.na(hla_normalize_allele("NNNN")))
  expect_true(is.na(hla_normalize_allele("")))
  expect_true(is.na(hla_normalize_allele(NA)))
  expect_true(is.na(hla_normalize_allele("NA")))
})

test_that("bare fields without a locus are unrecognisable", {
  expect_true(is.na(hla_normalize_allele("02:01")))
})

test_that("more than one '*' is rejected, not silently truncated", {
  # A*02:01*03 used to become HLA-A*02:01 (the trailing *03 dropped); a token
  # with two locus/field separators is malformed and must be NA.
  expect_true(is.na(hla_normalize_allele("A*02:01*03")))
  expect_true(is.na(hla_normalize_allele("HLA-A*02:01*03")))
  expect_true(is.na(hla_normalize_allele("A**02:01")))
  # a well-formed single-'*' token still normalizes
  expect_equal(hla_normalize_allele("A*02:01"), "HLA-A*02:01")
})

test_that("resolution is preserved, not padded", {
  expect_equal(hla_allele_resolution("HLA-A*02"), "1-field")
  expect_equal(hla_allele_resolution("HLA-A*02:01"), "2-field")
  expect_equal(hla_allele_resolution("HLA-A*02:01:01"), "3-field")
})

test_that("expression suffix is accepted", {
  expect_equal(hla_normalize_allele("HLA-A*02:01N"), "HLA-A*02:01N")
})

test_that("official G and P group suffixes are preserved", {
  expect_equal(
    hla_normalize_allele("HLA-A*02:01:01G"),
    "HLA-A*02:01:01G"
  )
  expect_equal(hla_normalize_allele("HLA-A*02:01P"), "HLA-A*02:01P")
  expect_equal(hla_allele_resolution("HLA-A*02:01:01G"), "3-field")
  expect_equal(hla_allele_resolution("HLA-A*02:01P"), "2-field")
})

test_that("garbage alleles are rejected", {
  expect_true(is.na(hla_normalize_allele("banana")))
  expect_true(is.na(hla_normalize_allele("A*")))
})

## ---- locus class ------------------------------------------------------ ##

test_that("locus class maps I / II / other", {
  expect_equal(hla_locus_class("HLA-A"), "Class I")
  expect_equal(hla_locus_class("HLA-DRB1"), "Class II")
  expect_equal(hla_locus_class("HLA-E"), "Other")
})

## ---- named-list input ------------------------------------------------- ##

test_that("named list normalizes to canonical long table", {
  x <- list(
    sample_1 = c("HLA-A*02:01", "HLA-A*01:01", "HLA-B*08:01"),
    sample_2 = c("HLA-A*03:01")
  )
  t <- hla_normalize_typing(x, source_type = "genotyped")
  expect_true(hla_is_typing_table(t))
  expect_equal(nrow(t), 4L)
  # copy 1/2 assigned within (sample, locus)
  a <- t[t$sample == "sample_1" & t$locus == "HLA-A", ]
  expect_setequal(a$copy, c(1L, 2L))
  expect_true(all(t$source_type == "genotyped"))
})

## ---- wide input (57.R style) ------------------------------------------ ##

test_that("wide table normalizes to canonical long table", {
  wide <- data.frame(
    sample = c("s1", "s2"),
    `HLA-A_1` = c("02:01", "01:01"),
    `HLA-A_2` = c("33:01", "NNNN"),
    `HLA-B_1` = c("08:01", "40:01"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  t <- hla_normalize_typing(wide, source_type = "genotyped")
  expect_true(hla_is_typing_table(t))
  # s1 has A*02:01, A*33:01, B*08:01; s2 has A*01:01 (A_2 is NNNN -> dropped), B*40:01
  expect_equal(sum(t$sample == "s1"), 3L)
  expect_equal(sum(t$sample == "s2"), 2L)
  expect_true("HLA-A*02:01" %in% t$allele)
  expect_false(any(grepl("NNNN", t$allele)))
})

test_that("wide table preserves donor mapping", {
  wide <- data.frame(
    sample = c("s1", "s2"),
    donor_id = c("d1", "d2"),
    `HLA-A_1` = c("02:01", "01:01"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  t <- hla_normalize_typing(wide, source_type = "genotyped")

  expect_equal(t$donor_id[match(c("s1", "s2"), t$sample)], c("d1", "d2"))
})

test_that("donor_id survives normalization of a long table", {
  # Losing donor_id here silently demotes the whole app to sample-level
  # counting while the UI still says donor-level, so it is contract-critical.
  t <- hla_normalize_typing(
    data.frame(
      sample = c("s1", "s2"),
      donor_id = c("d1", "d1"),
      locus = "HLA-A",
      allele = c("HLA-A*02:01", "HLA-A*01:01"),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )
  expect_false(any(is.na(t$donor_id)))
  expect_equal(unique(t$donor_id), "d1")
})

test_that("a named list has no donor column, so donor_id is NA", {
  # The named-list adapter cannot express donors; callers who need donor-level
  # counting must supply a long table (see the test above).
  t <- hla_normalize_typing(
    list(s1 = "HLA-A*02:01"),
    source_type = "genotyped"
  )
  expect_true(all(is.na(t$donor_id)))
})

## ---- provenance safety ------------------------------------------------ ##

test_that("missing source_type defaults to unknown with a QC warning", {
  t <- hla_normalize_typing(list(s1 = "HLA-A*02:01"))
  expect_true(all(t$source_type == "unknown"))
  qc <- attr(t, "qc")
  expect_true(any(grepl("unknown", qc$issue)))
})

test_that("unrecognisable alleles are reported in QC, not silently dropped", {
  t <- hla_normalize_typing(
    list(s1 = c("HLA-A*02:01", "banana")),
    source_type = "genotyped"
  )
  expect_equal(nrow(t), 1L) # only the valid one kept
  qc <- attr(t, "qc")
  expect_true(any(grepl("unrecognisable", qc$issue)))
})

## ---- carrier index + coverage ----------------------------------------- ##

test_that("carrier index maps allele -> samples", {
  x <- list(
    s1 = c("HLA-A*02:01", "HLA-B*08:01"),
    s2 = c("HLA-A*02:01"),
    s3 = c("HLA-A*01:01")
  )
  t <- hla_normalize_typing(x, source_type = "genotyped")
  ci <- hla_carrier_index(t)
  expect_setequal(ci[["HLA-A*02:01"]], c("s1", "s2"))
  expect_setequal(ci[["HLA-A*01:01"]], "s3")
})

test_that("coverage-by-sample summarises loci and allele counts", {
  x <- list(s1 = c("HLA-A*02:01", "HLA-A*01:01", "HLA-B*08:01"))
  t <- hla_normalize_typing(x, source_type = "genotyped")
  cov <- hla_coverage_by_sample(t)
  expect_equal(cov$n_alleles, 3L)
  expect_true(grepl("HLA-A", cov$loci))
  expect_true(grepl("HLA-B", cov$loci))
})

## ---- lineage-derived MHC context -------------------------------------- ##

test_that("lineage context maps CD8 -> Class I, CD4/Treg -> Class II", {
  expect_equal(hla_lineage_context("CD8 T"), "Class I")
  expect_equal(hla_lineage_context("CD8+ T cells"), "Class I")
  expect_equal(hla_lineage_context("CD4 T"), "Class II")
  expect_equal(hla_lineage_context("Treg"), "Class II")
  expect_equal(hla_lineage_context("regulatory (Treg)"), "Class II")
})

test_that("coarse or non-T labels map to Unknown, never guessed", {
  expect_equal(hla_lineage_context("T cells"), "Unknown")
  expect_equal(hla_lineage_context("B cells"), "Unknown")
  expect_equal(hla_lineage_context("Monocytes"), "Unknown")
  expect_equal(hla_lineage_context("T (unassigned)"), "Unknown")
})

test_that("lineage context is vectorised", {
  expect_equal(
    hla_lineage_context(c("CD8 T", "CD4 T", "T cells")),
    c("Class I", "Class II", "Unknown")
  )
})

test_that("context summary collapses per-cell contexts to a node label", {
  expect_equal(hla_context_summary(c("Class I", "Class I")), "Class I")
  expect_equal(hla_context_summary(c("Class II", "Unknown")), "Class II")
  expect_equal(hla_context_summary(c("Class I", "Class II")), "Mixed")
  expect_equal(hla_context_summary(c("Unknown", "Unknown")), "Unknown")
})

## ---- descriptive carrier summary -------------------------------------- ##

test_that("carrier summary counts carriers / non-carriers / untyped", {
  x <- list(
    s1 = c("HLA-A*02:01", "HLA-B*08:01"),
    s2 = c("HLA-A*02:01"),
    s3 = c("HLA-A*01:01", "HLA-A*03:01") # complete HLA-A call, genuinely lacks A*02:01
  )
  t <- hla_normalize_typing(x, source_type = "genotyped")
  # scope includes a 4th sample (s4) with no typing -> untyped
  summ <- hla_allele_carrier_summary(t, samples = c("s1", "s2", "s3", "s4"))
  a2 <- summ[summ$allele == "HLA-A*02:01", ]
  expect_equal(a2$n_carrier, 2L) # s1, s2
  expect_equal(a2$n_noncarrier, 1L) # s3 (completely typed, lacks it)
  expect_equal(a2$n_untyped, 1L) # s4
  expect_equal(a2$mhc_class, "Class I")
  expect_true(grepl("s1", a2$carriers) && grepl("s2", a2$carriers))
})

test_that("carrier summary uses locus-specific typing denominators", {
  t <- hla_normalize_typing(
    list(
      s1 = c("HLA-A*02:01", "HLA-B*08:01"),
      s2 = "HLA-A*01:01"
    ),
    source_type = "genotyped"
  )

  summ <- hla_allele_carrier_summary(t, samples = c("s1", "s2"))
  b8 <- summ[summ$allele == "HLA-B*08:01", ]

  expect_equal(b8$n_carrier, 1L)
  expect_equal(b8$n_noncarrier, 0L)
  expect_equal(b8$n_untyped, 1L)
})

test_that("a one-copy locus call is untyped, not a non-carrier", {
  # The #2 fix: the picker summary must agree with hla_allele_status_by_unit().
  # A unit typed at a single HLA-A copy that is not the queried allele cannot be
  # ruled out -- the unknown second copy may yet be it -- so it is untyped, never
  # a non-carrier. The old summary counted it as a non-carrier, seeding that
  # group with possible carriers (a bias pointing the way the effect does).
  t <- hla_normalize_typing(
    list(
      s1 = c("HLA-A*02:01", "HLA-A*11:01"), # complete; carries A*11:01
      s2 = "HLA-A*01:01" # ONE copy, not the query allele
    ),
    source_type = "genotyped"
  )
  summ <- hla_allele_carrier_summary(t, samples = c("s1", "s2"))
  a11 <- summ[summ$allele == "HLA-A*11:01", ]

  expect_equal(a11$n_carrier, 1L) # s1
  expect_equal(a11$n_noncarrier, 0L) # s2 is one-copy -> NOT ruled out
  expect_equal(a11$n_untyped, 1L) # s2 is untyped, not a non-carrier
})

test_that("carrier summary collapses repeated samples to donor when complete", {
  t <- hla_normalize_typing(
    data.frame(
      # d2 is typed at BOTH HLA-A copies (complete) so it is a genuine
      # non-carrier of A*02:01, not an unknown one-copy call.
      sample = c("s1", "s2", "s3", "s3"),
      donor_id = c("d1", "d1", "d2", "d2"),
      locus = "HLA-A",
      allele = c(
        "HLA-A*02:01",
        "HLA-A*02:01",
        "HLA-A*01:01",
        "HLA-A*03:01"
      ),
      stringsAsFactors = FALSE
    ),
    source_type = "genotyped"
  )

  summ <- hla_allele_carrier_summary(t, samples = c("s1", "s2", "s3"))
  a2 <- summ[summ$allele == "HLA-A*02:01", ]

  expect_equal(a2$analysis_unit, "donor")
  expect_equal(a2$n_carrier, 1L)
  expect_equal(a2$n_noncarrier, 1L)
  expect_equal(a2$n_untyped, 0L)
  expect_equal(a2$carriers, "d1")
})

test_that("carrier summary is ordered by descending carrier count", {
  x <- list(s1 = "HLA-A*02:01", s2 = "HLA-A*02:01", s3 = "HLA-A*01:01")
  t <- hla_normalize_typing(x, source_type = "genotyped")
  summ <- hla_allele_carrier_summary(t, samples = c("s1", "s2", "s3"))
  expect_equal(summ$allele[1], "HLA-A*02:01") # 2 carriers first
})

test_that("carrier summary is empty on empty typing", {
  t <- hla_normalize_typing(list(), source_type = "genotyped")
  summ <- hla_allele_carrier_summary(t, samples = c("s1"))
  expect_equal(nrow(summ), 0L)
})

## ---- empty input ------------------------------------------------------ ##

test_that("empty input yields an empty canonical table", {
  t <- hla_normalize_typing(list(), source_type = "genotyped")
  expect_true(hla_is_typing_table(t))
  expect_equal(nrow(t), 0L)
})

## ---- uploaded files: the read must not rewrite the format -------------- ##

test_that("a wide CSV survives the read the upload path uses", {
  # The regression this guards: read.csv's default check.names rewrites
  # `HLA-A_1` to `HLA.A_1`, and the wide adapter matches on `^HLA-`. That made
  # every real wide upload -- a format the Data & QC tab advertises -- fail as
  # "no valid HLA alleles found", blaming the user's file.
  wide <- data.frame(
    sample = c("p1", "p2"),
    `HLA-A_1` = c("A*02:01", "A*01:01"),
    `HLA-A_2` = c("A*11:01", "A*03:01"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(wide, path, row.names = FALSE)

  raw <- hla_read_typing_file(path)
  expect_true("HLA-A_1" %in% colnames(raw))

  t <- hla_normalize_typing(raw, source_type = "genotyped")
  expect_true(hla_is_typing_table(t))
  expect_setequal(unique(t$sample), c("p1", "p2"))
  expect_setequal(
    unique(t$allele),
    c("HLA-A*02:01", "HLA-A*11:01", "HLA-A*01:01", "HLA-A*03:01")
  )
  expect_equal(hla_carriers_of(t, "HLA-A*02:01"), "p1")
})

test_that("a wide TSV is read on its name, not its temp path", {
  # Shiny hands the handler a temp path with no useful extension, so the
  # delimiter has to come from the uploaded file's own name.
  wide <- data.frame(
    sample = "p1",
    `HLA-A_1` = "A*02:01",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".bin")
  on.exit(unlink(path), add = TRUE)
  utils::write.table(wide, path, sep = "\t", row.names = FALSE, quote = FALSE)

  raw <- hla_read_typing_file(path, name = "typing.tsv")
  expect_true("HLA-A_1" %in% colnames(raw))

  t <- hla_normalize_typing(raw, source_type = "genotyped")
  expect_equal(unique(t$allele), "HLA-A*02:01")
})

test_that("a long CSV still reads unchanged", {
  long <- data.frame(
    sample = c("p1", "p1"),
    locus = c("HLA-A", "HLA-A"),
    allele = c("HLA-A*02:01", "HLA-A*11:01"),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path), add = TRUE)
  utils::write.csv(long, path, row.names = FALSE)

  t <- hla_normalize_typing(
    hla_read_typing_file(path),
    source_type = "genotyped"
  )
  expect_setequal(unique(t$allele), c("HLA-A*02:01", "HLA-A*11:01"))
})

test_that("carrier summary is empty, not an error, when no sample matches", {
  # A typing file for another cohort is format-valid and non-empty, so the page
  # accepts it; every sample then falls outside the repertoire. That used to
  # reach `order(-out$n_carrier)` with out = NULL and die on the unary minus.
  t <- hla_normalize_typing(
    list(ghost = c("HLA-A*02:01", "HLA-A*11:01")),
    source_type = "genotyped"
  )
  summ <- hla_allele_carrier_summary(t, samples = c("real1", "real2"))
  expect_true(is.data.frame(summ))
  expect_equal(nrow(summ), 0L)
})

test_that("validation survives factor-valued canonical columns", {
  # A factor's integer code is its LEVEL INDEX, not its value: with levels
  # c("2", "1") the value "2" has code 1, so as.integer() on the factor would
  # silently turn copy 2 into 1. And "unknown" cannot be assigned into a factor
  # that has no such level -- it becomes NA, so an invalid provenance would
  # vanish instead of being downgraded. Both must go through character first.
  tbl <- data.frame(
    sample = factor(c("s1", "s2")),
    donor_id = factor(c("d1", "d2")),
    locus = c("A", "A"),
    copy = factor(c("2", "1"), levels = c("2", "1")),
    allele = c("HLA-A*02:01", "HLA-A*01:01"),
    resolution = c("two-field", "two-field"),
    source_type = factor(c("genotyped", "not-a-source-type")),
    typing_method = factor(c("m", "m")),
    source_reference = factor(c("r", "r")),
    confidence = c(1, 1),
    stringsAsFactors = FALSE
  )
  out <- hla_validate_typing(tbl)
  expect_true(hla_is_typing_table(out))
  # the VALUES, not the level codes
  expect_equal(out$copy, c(2L, 1L))
  # downgraded to "unknown", not silently NA
  expect_equal(out$source_type, c("genotyped", "unknown"))
  # canonical table is character-typed, whatever came in
  expect_true(is.character(out$sample))
  expect_true(is.character(out$donor_id))
  expect_true(is.character(out$typing_method))
  expect_true(is.character(out$source_reference))
})

test_that("lineage inference ignores treatment and study-arm labels", {
  # These carry a lineage token but name an EXPERIMENT, not a cell. Counting
  # them would let a treatment or study-arm column take the lineage role and
  # silently change which cells the Class I / Class II scope keeps.
  expect_true(all(hla_is_condition_label(c(
    "anti-CD4",
    "anti CD8",
    "antiCD4",
    "α-CD4",
    "CD8_case",
    "CD4-control",
    "vehicle",
    "mock",
    "untreated",
    "anti-CD4 treated",
    "CD8 depleted"
  ))))
  expect_false(any(hla_is_condition_label(c(
    "CD8 TEM",
    "CD4 naive",
    "Treg",
    "B cell",
    "NK",
    "Monocyte"
  ))))
  # a column that is entirely treatments scores zero, so it can never win
  expect_equal(
    hla_lineage_column_score(c("anti-CD4", "anti-CD8", "vehicle")),
    0
  )
  # a real lineage column scores on the share it actually resolves
  expect_gt(
    hla_lineage_column_score(c("CD8 TEM", "CD4 naive", "B cell", "NK")),
    0
  )
  expect_equal(hla_lineage_column_score(c("CD8 T", "CD4 T")), 1)
  expect_equal(hla_lineage_column_score(character(0)), 0)
})
