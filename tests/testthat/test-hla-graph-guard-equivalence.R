## The early total guard must keep what the 860e07c5 graph builder returned.
## fixtures/hla-graph-guard-860e07c5.rds holds its graphs for the segments built
## below, generated once from the frozen 860e07c5 code with a limit of 4 and the
## inputs built as test_that() builds them (C collation).

## The current graph core with its node limit set to `limit`.
guard_test_core <- function(limit = HLA_MOTIF_MAX_TOTAL) {
  parent <- environment(hla_build_motif_graph_raw)
  core <- new.env(parent = parent)
  for (name in c(
    "hla_aggregate_cdr3_nodes",
    "hla_build_motif_graph_raw",
    ".hla_cdr3_node_keys"
  )) {
    f <- get(name, parent)
    environment(f) <- core
    assign(name, f, core)
  }
  core$HLA_MOTIF_MAX_TOTAL <- limit
  core
}

guard_test_expected <- function() {
  readRDS(testthat::test_path("fixtures", "hla-graph-guard-860e07c5.rds"))
}

## (seg, by_v) pairs: every prefix of a small cohort, NULL, unused factor
## levels, and interaction's separator collisions.
guard_test_cases <- function() {
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
  cases <- list()
  add <- function(seg, by_v) {
    cases[[length(cases) + 1L]] <<- list(seg = seg, by_v = by_v)
  }
  for (n in 0:6) {
    for (by_v in c(FALSE, TRUE)) {
      add(base[seq_len(n), ], by_v)
    }
  }
  add(NULL, FALSE)
  for (by_v in c(FALSE, TRUE)) {
    s <- base
    s$cdr3 <- ordered(s$cdr3, levels = c("unused", rev(unique(s$cdr3))))
    s$v_gene <- factor(s$v_gene, levels = c("unused", "V2", "V1"))
    add(s, by_v)
    # Keep interaction's existing separator-collision semantics exactly.
    s <- base
    s$v_gene <- c("V.a", "V", "V.a", "V", "V.a", "V")
    s$cdr3 <- c("b", "a.b", "b", "a.b", "c", "a.c")
    add(s, by_v)
  }
  cases
}

guard_test_build <- function(core, case) {
  core$hla_build_motif_graph_raw(
    seg = case$seg,
    by_v = case$by_v,
    meta_cols = c("sample", "label"),
    context_col = "mhc_context"
  )
}

## Five segments of one V/J: over a limit of 4 unique CDR3s.
guard_test_over_limit <- function() {
  data.frame(
    cdr3 = c("CASSL", "CASSF", "CASST", "CWWWW", "CWWWL"),
    v_gene = "V1",
    j_gene = "J1",
    sample = "S1"
  )
}

test_that("early total guard preserves counts, boundaries and normal graph payloads", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("stringdist")
  core <- guard_test_core(4L)
  expected <- guard_test_expected()$graphs
  canonical <- function(g) {
    if (!inherits(g, "igraph")) {
      return(g)
    }
    data <- igraph::as_data_frame(g, what = "both")
    ## The layout is floating point: macOS gives the stored coordinates to
    ## about 1e-10, so compare it to 6 digits; everything else is exact.
    for (column in intersect(c("layout_x", "layout_y"), names(data$vertices))) {
      data$vertices[[column]] <- round(data$vertices[[column]], 6L)
    }
    list(data = data, attributes = igraph::graph_attr(g))
  }
  cases <- guard_test_cases()
  expect_length(expected, length(cases))
  for (i in seq_along(cases)) {
    seg <- cases[[i]]$seg
    saved <- unserialize(serialize(seg, NULL))
    a <- expected[[i]]
    b <- guard_test_build(core, cases[[i]])
    expect_identical(canonical(b), canonical(a), info = paste("case", i))
    expect_identical(seg, saved, info = paste("case", i))
    for (minimum in c(2L, 4L)) {
      for (isolated in c(FALSE, TRUE)) {
        expect_identical(
          canonical(hla_finalize_motif_graph(b, minimum, isolated)),
          canonical(hla_finalize_motif_graph(a, minimum, isolated)),
          info = paste("case", i)
        )
      }
    }
  }
})

test_that("over-limit graphs skip aggregation and normal graphs group only once", {
  skip_if_not_installed("igraph")
  skip_if_not_installed("stringdist")
  e <- guard_test_core(4L)
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
  seg <- guard_test_over_limit()
  g <- e$hla_build_motif_graph_raw(seg)
  expect_identical(calls$keys, 1L)
  expect_identical(calls$aggregation, 0L)
  expect_identical(g, guard_test_expected()$over_limit)
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
