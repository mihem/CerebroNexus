## The segment parser must keep the complete contract of the bd47f107 parser.
## fixtures/hla-parser-bd47f107.rds holds what that parser returned for the
## inputs built below. It was generated once from the frozen bd47f107 code with
## the inputs built as test_that() builds them (C collation, English messages);
## they are fixed literals and seeded draws, so they are rebuilt the same here.

hla_parser_expected <- function() {
  readRDS(testthat::test_path("fixtures", "hla-parser-bd47f107.rds"))
}

## The parser's value, or the class and message of the error it raises.
hla_parser_outcome <- function(f, data, chain) {
  tryCatch(f(data, chain), error = function(e) {
    list(error_class = class(e), message = conditionMessage(e))
  })
}

hla_parser_frame <- function() {
  data.frame(
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
}

## (data, chain) pairs covering chains, empty and malformed inputs, ordering,
## factors, literal chain matching with regex V/J prefixes, and random rows.
hla_parser_cases <- function() {
  df <- hla_parser_frame()
  cases <- list()
  add <- function(data, chain) {
    cases[[length(cases) + 1L]] <<- list(data = data, chain = chain)
  }
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
    add(list(s1 = df), chain)
    add(list(s1 = df[0, ]), chain)
    add(list(NULL, data.frame(barcode = "missing columns")), chain)
  }
  add(NULL, "TRB")
  add(list(), "TRB")
  add(
    list(s1 = df[c(3, 1, 2, 1), ], s2 = transform(df, extra = "metadata")),
    "TRB"
  )
  add(
    list(s1 = transform(df, CTgene = factor(CTgene), CTaa = factor(CTaa))),
    "TRB"
  )
  add(
    list(
      s1 = data.frame(barcode = "x", CTgene = "TRB|V1.TRB|J1", CTaa = "CASSL")
    ),
    "TRB|"
  )
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
      add(list(s1 = random), chain)
    }
  }
  cases
}

hla_parser_graph_input <- function() {
  list(
    s1 = data.frame(
      barcode = c("b", "a", "b", "c"),
      CTgene = c("TRBV1.TRBJ1", "TRBV1.TRBJ1", "TRBV2", "TRBV2"),
      CTaa = c("CASSL", "CASSF", "CASSL", "CASTL"),
      sample = "s1"
    )
  )
}

## Inputs whose extra metadata column takes every storage type and attribute,
## then pairs of frames whose metadata columns cannot be bound uniformly.
hla_parser_metadata_cases <- function() {
  frame <- data.frame(
    barcode = c("b", "a"),
    CTgene = "TRBV1.TRBJ1",
    CTaa = c("CASSL", "CASSF"),
    row.names = c("custom-b", "custom-a")
  )
  values <- list(
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
  same <- lapply(values, function(x) {
    frame$metadata <- x
    list(first = frame, second = frame)
  })
  variants <- list(
    list(factor(c("a", "b")), factor(c("c", "b"), levels = c("c", "b"))),
    list(ordered(c("a", "b")), factor(c("a", "b"))),
    list(c(1L, NA_integer_), c(1.5, 0)),
    list(c(1L, NA_integer_), c("x", "")),
    list(factor(c("a", "b")), c("c", NA_character_))
  )
  mixed <- lapply(variants, function(pair) {
    a <- b <- frame
    a$metadata <- pair[[1]]
    b$metadata <- pair[[2]]
    list(a, b)
  })
  c(same, mixed, list(list(frame, transform(frame, extra = "x"))))
}

test_that("vectorized segments preserve the complete bd47f107 parser contract", {
  expected <- hla_parser_expected()$segments
  cases <- hla_parser_cases()
  expect_length(expected, length(cases))
  for (i in seq_along(cases)) {
    expect_identical(
      hla_parser_outcome(
        hla_parse_ir_segments,
        cases[[i]]$data,
        cases[[i]]$chain
      ),
      expected[[i]],
      info = paste("case", i)
    )
  }
  expect_identical(
    hla_parse_ir_segments(list(s1 = hla_parser_frame()), "TRB")$cdr3[5],
    "NA"
  )
})

test_that("parser equivalence preserves downstream graph inputs and identities", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("stringdist")
  a <- hla_parser_expected()$graph_input
  b <- hla_parse_ir_segments(hla_parser_graph_input(), "TRB")
  expect_identical(b, a)
  for (by_v in c(FALSE, TRUE)) {
    set.seed(301)
    ga <- hla_build_motif_graph_raw(a, by_v = by_v)
    set.seed(301)
    gb <- hla_build_motif_graph_raw(b, by_v = by_v)
    expect_identical(
      igraph::as_data_frame(gb, what = "both"),
      igraph::as_data_frame(ga, what = "both")
    )
  }
})

test_that("segment binding preserves metadata types and attributes", {
  expected <- hla_parser_expected()$metadata
  cases <- hla_parser_metadata_cases()
  expect_length(expected, length(cases))
  for (i in seq_along(cases)) {
    data <- cases[[i]]
    saved <- unserialize(serialize(data, NULL))
    expect_identical(
      hla_parse_ir_segments(data, "TRB"),
      expected[[i]],
      info = paste("case", i)
    )
    expect_identical(data, saved, info = paste("case", i))
  }
})
