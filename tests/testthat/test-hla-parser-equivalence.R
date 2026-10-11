test_that("vectorized segments preserve the complete bd47f107 parser contract", {
  reference <- new.env(parent = baseenv())
  sys.source(testthat::test_path("fixtures/hla-parser-bd47f107.R"), reference)
  old <- reference$legacy_hla_parse_ir_segments
  compare <- function(data, chain) {
    outcome <- function(f) {
      tryCatch(f(data, chain), error = function(e) {
        list(error_class = class(e), message = conditionMessage(e))
      })
    }
    expect_identical(outcome(hla_parse_ir_segments), outcome(old))
  }
  df <- data.frame(
    barcode = c("same", "b", "same", NA, "", "c", "d", "e", "f", "g", "h", "i"),
    CTgene = c(
      "TRAV1.TRAJ1_TRBV2.TRBJ2",
      "TRBV1;TRBV9.TRBJ9.TRBJ1",
      "TRBV2.TRBJ2_TRBV3.TRBJ3",
      "TRBV1",
      "NA",
      NA,
      "",
      "TRBV1.TRBJ1",
      "TRAV1_TRAJ1_TRBV2.TRBJ2",
      "TRBV1.TRBJ1",
      "TRBV1.TRBJ1",
      "TRBV1.TRBJ1"
    ),
    CTaa = c(
      "CAAA_CBBB",
      "CASSL;CASST",
      "CBBB_CDDD",
      "CASSF",
      "NA",
      NA,
      "",
      "NA;ALT",
      "CAAA",
      ";ALT",
      "_CBBB",
      "CBBB_"
    ),
    sample = "s1",
    count = c(0, NA_real_, seq_len(10)),
    category = factor(rep(c("x", "y"), 6)),
    stringsAsFactors = FALSE
  )
  for (chain in list(
    "TRB",
    "TRA",
    "TRB",
    "TRG",
    "UNKNOWN",
    "",
    NULL,
    NA_character_
  )) {
    compare(list(s1 = df), chain)
    compare(list(s1 = df[0, ]), chain)
    compare(list(NULL, data.frame(barcode = "missing columns")), chain)
  }
  compare(NULL, "TRB")
  compare(list(), "TRB")
  compare(
    list(s1 = df[c(3, 1, 2, 1), ], s2 = transform(df, extra = "metadata")),
    "TRB"
  )
  compare(
    list(s1 = transform(df, CTgene = factor(CTgene), CTaa = factor(CTaa))),
    "TRB"
  )
  # The legacy API uses literal chain matching, but regex V/J prefixes. Preserve
  # that distinction for unusual chains instead of silently narrowing the API.
  special <- data.frame(barcode = "x", CTgene = "TRB|V1.TRB|J1", CTaa = "CASSL")
  compare(list(s1 = special), "TRB|")
  expect_identical(hla_parse_ir_segments(list(s1 = df), "TRB")$cdr3[5], "NA")

  set.seed(301)
  genes <- c(
    df$CTgene,
    "TRAV1;TRBV1_TRBV4.TRBJ2",
    "_TRBV1.TRBJ1",
    "TRBV1..TRBJ1"
  )
  aa <- c(df$CTaa, "CASSL_CASSF", "NA_CASSL", "NA;CASSL", "CASSL__CASSF")
  for (i in seq_len(30)) {
    random <- data.frame(
      barcode = sample(c("a", "a", "b", NA), 80, TRUE),
      CTgene = sample(genes, 80, TRUE),
      CTaa = sample(aa, 80, TRUE),
      ordinal = seq_len(80)
    )
    for (chain in c("TRB", "TRA", "IGH")) {
      compare(list(s1 = random), chain)
    }
  }
})

test_that("parser equivalence preserves downstream graph inputs and identities", {
  reference <- new.env(parent = baseenv())
  sys.source(testthat::test_path("fixtures/hla-parser-bd47f107.R"), reference)
  data <- list(
    s1 = data.frame(
      barcode = c("b", "a", "b", "c"),
      CTgene = c("TRBV1.TRBJ1", "TRBV1.TRBJ1", "TRBV2", "TRBV2"),
      CTaa = c("CASSL", "CASSF", "CASSL", "CASTL"),
      sample = "s1"
    )
  )
  a <- reference$legacy_hla_parse_ir_segments(data, "TRB")
  b <- hla_parse_ir_segments(data, "TRB")
  expect_identical(a, b)
  for (by_v in c(FALSE, TRUE)) {
    set.seed(301)
    ga <- hla_build_motif_graph_raw(a, by_v = by_v)
    set.seed(301)
    gb <- hla_build_motif_graph_raw(b, by_v = by_v)
    expect_identical(
      igraph::as_data_frame(ga, what = "both"),
      igraph::as_data_frame(gb, what = "both")
    )
  }
})

test_that("segment binding preserves metadata types and attributes", {
  reference <- new.env(parent = baseenv())
  sys.source(testthat::test_path("fixtures/hla-parser-bd47f107.R"), reference)
  frame <- data.frame(
    barcode = c("b", "a"),
    CTgene = "TRBV1.TRBJ1",
    CTaa = c("CASSL", "CASSF"),
    row.names = c("custom-b", "custom-a")
  )
  cases <- list(
    c(TRUE, NA),
    c(0L, NA_integer_),
    c(NaN, Inf),
    c(0 + 1i, NA_complex_),
    c("", NA_character_),
    as.raw(c(0, 255)),
    factor(c("b", NA), levels = c("a", "b", "unused")),
    ordered(c("b", "a"), levels = c("a", "b", "unused")),
    factor(c("a", NA), exclude = NULL),
    as.Date(c("2020-01-01", NA)),
    structure(c(1, 2), names = c("x", "y")),
    I(list(1, NULL)),
    I(matrix(1:4, 2)),
    structure(factor(c("a", "b")), class = c("custom", "factor"))
  )
  for (x in cases) {
    a <- b <- frame
    a$metadata <- b$metadata <- x
    data <- list(first = a, second = b)
    saved <- unserialize(serialize(data, NULL))
    expect_identical(
      hla_parse_ir_segments(data, "TRB"),
      reference$legacy_hla_parse_ir_segments(data, "TRB")
    )
    expect_identical(data, saved)
  }
  # Different factor levels, storage types and missing columns must retain
  # base data.frame coercion rather than use the uniform-column shortcut.
  variants <- list(
    list(factor(c("a", "b")), factor(c("c", "b"), levels = c("c", "b"))),
    list(ordered(c("a", "b")), factor(c("a", "b"))),
    list(c(1L, NA_integer_), c(1.5, 0)),
    list(c(1L, NA_integer_), c("x", "")),
    list(factor(c("a", "b")), c("c", NA_character_))
  )
  for (pair in variants) {
    a <- b <- frame
    a$metadata <- pair[[1]]
    b$metadata <- pair[[2]]
    data <- list(a, b)
    expect_identical(
      hla_parse_ir_segments(data, "TRB"),
      reference$legacy_hla_parse_ir_segments(data, "TRB")
    )
  }
  expect_identical(
    hla_parse_ir_segments(list(frame, transform(frame, extra = "x")), "TRB"),
    reference$legacy_hla_parse_ir_segments(
      list(frame, transform(frame, extra = "x")),
      "TRB"
    )
  )
})
