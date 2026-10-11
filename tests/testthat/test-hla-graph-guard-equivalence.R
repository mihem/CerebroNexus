guard_test_cores <- function(limit = HLA_MOTIF_MAX_TOTAL) {
  parent <- environment(hla_build_motif_graph_raw)
  before <- new.env(parent = parent)
  sys.source(testthat::test_path("fixtures/hla-graph-guard-860e07c5.R"), before)
  after <- new.env(parent = parent)
  for (name in c(
    "hla_aggregate_cdr3_nodes",
    "hla_build_motif_graph_raw",
    ".hla_cdr3_node_keys"
  )) {
    f <- get(name, parent)
    environment(f) <- after
    assign(name, f, after)
  }
  before$HLA_MOTIF_MAX_TOTAL <- after$HLA_MOTIF_MAX_TOTAL <- limit
  list(before = before, after = after)
}

test_that("early total guard preserves counts, boundaries and normal graph payloads", {
  cores <- guard_test_cores(4L)
  canonical <- function(g) {
    if (!inherits(g, "igraph")) {
      return(g)
    }
    list(
      data = igraph::as_data_frame(g, what = "both"),
      attributes = igraph::graph_attr(g)
    )
  }
  compare <- function(seg, by_v) {
    args <- list(
      seg = seg,
      by_v = by_v,
      meta_cols = c("sample", "label"),
      context_col = "mhc_context"
    )
    saved <- unserialize(serialize(seg, NULL))
    a <- do.call(cores$before$hla_build_motif_graph_raw, args)
    b <- do.call(cores$after$hla_build_motif_graph_raw, args)
    expect_identical(canonical(b), canonical(a))
    expect_identical(seg, saved)
    for (minimum in c(2L, 4L)) {
      for (isolated in c(FALSE, TRUE)) {
        expect_identical(
          canonical(hla_finalize_motif_graph(b, minimum, isolated)),
          canonical(hla_finalize_motif_graph(a, minimum, isolated))
        )
      }
    }
  }
  base <- data.frame(
    cdr3 = c("CASSL", "CASSF", "CASST", "CWWWW", "CWWWL", "CASSL"),
    v_gene = c("V1", "V1", "V2", "V1", "V2", "V2"),
    j_gene = c(NA, "J1", "J1", "J2", "J2", "J1"),
    barcode = c("a", "a", "b", "c", "d", "e"),
    sample = c("S1", "S2", "S1", "S2", "S3", "S1"),
    label = c(NA, "", "B", "A", "B", "A"),
    mhc_context = c(
      "Class I",
      "Class II",
      "Unknown",
      "Class I",
      "Class II",
      "Unknown"
    )
  )
  for (n in 0:6) {
    for (by_v in c(FALSE, TRUE)) {
      compare(base[seq_len(n), ], by_v)
    }
  }
  compare(NULL, FALSE)
  for (by_v in c(FALSE, TRUE)) {
    s <- base
    s$cdr3 <- ordered(s$cdr3, levels = c("unused", rev(unique(s$cdr3))))
    s$v_gene <- factor(s$v_gene, levels = c("unused", "V2", "V1"))
    compare(s, by_v)
    # Keep interaction's existing separator-collision semantics exactly.
    s <- base
    s$v_gene <- c("V.a", "V", "V.a", "V", "V.a", "V")
    s$cdr3 <- c("b", "a.b", "b", "a.b", "c", "a.c")
    compare(s, by_v)
  }
})

test_that("over-limit graphs skip aggregation and normal graphs group only once", {
  cores <- guard_test_cores(4L)
  e <- cores$after
  calls <- new.env()
  calls$keys <- calls$aggregation <- 0L
  key_fun <- e$.hla_cdr3_node_keys
  agg_fun <- e$hla_aggregate_cdr3_nodes
  e$.hla_cdr3_node_keys <- function(...) {
    calls$keys <- calls$keys + 1L
    key_fun(...)
  }
  e$hla_aggregate_cdr3_nodes <- function(...) {
    calls$aggregation <- calls$aggregation + 1L
    agg_fun(...)
  }
  seg <- data.frame(
    cdr3 = c("CASSL", "CASSF", "CASST", "CWWWW", "CWWWL"),
    v_gene = "V1",
    j_gene = "J1",
    sample = "S1"
  )
  g <- e$hla_build_motif_graph_raw(seg)
  expect_identical(calls$keys, 1L)
  expect_identical(calls$aggregation, 0L)
  expect_identical(g, cores$before$hla_build_motif_graph_raw(seg))
  calls$keys <- calls$aggregation <- 0L
  e$hla_build_motif_graph_raw(seg[1:4, ])
  expect_identical(calls$keys, 1L)
  expect_identical(calls$aggregation, 1L)
})

test_that("node keys preserve existing missing, factor and by-V semantics", {
  seg <- data.frame(
    cdr3 = c("CASSL", NA, "", "CASSL"),
    v_gene = c("V1", "V2", "V1", NA)
  )
  for (by_v in c(FALSE, TRUE)) {
    old <- if (by_v) {
      interaction(seg$v_gene, seg$cdr3, drop = TRUE, lex.order = TRUE)
    } else {
      factor(seg$cdr3)
    }
    expect_identical(.hla_cdr3_node_keys(seg, by_v), old)
  }
})
