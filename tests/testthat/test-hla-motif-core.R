# Tests for the shared CDR3 motif-network core (R/hla_motif_core.R).
# Pure functions, no Shiny app required.

## ---- helpers ---------------------------------------------------------- ##

# Minimal scRepertoire-style IR list with metadata already joined by barcode.
# CTgene / CTaa pack one TRB chain per cell. `cdr3s` supplies the TRB CDR3 aa.
make_ir_list <- function(cdr3s, samples = NULL, cell_types = NULL) {
  n <- length(cdr3s)
  if (is.null(samples)) {
    samples <- rep("sample_1", n)
  }
  df <- data.frame(
    barcode = paste0("bc", seq_len(n)),
    CTgene = "TRBV1.TRBJ2.TRBC2",
    CTnt = NA_character_,
    CTaa = cdr3s,
    CTstrict = NA_character_,
    sample = samples,
    stringsAsFactors = FALSE
  )
  if (!is.null(cell_types)) {
    df$cell_type <- cell_types
  }
  split(df, df$sample)
}

## ---- J gene is optional (bulk sources give V family + CDR3 only) ------- ##

test_that("rows with a V gene but no J gene are kept, with J as NA", {
  # A bulk repertoire source (e.g. Adaptive/pubtcrs) reports only a V family
  # and the CDR3. Those rows must survive: V + CDR3 define the node.
  df <- data.frame(
    barcode = c("a", "b"),
    CTgene = c("TRBV02", "TRBV02"),
    CTaa = c("CASSL", "CASSF"),
    sample = "s1",
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  expect_false(is.null(seg))
  expect_equal(nrow(seg), 2L)
  expect_true(all(is.na(seg$j_gene)))
  expect_equal(seg$v_gene, c("TRBV02", "TRBV02"))
  # The graph still builds from such rows.
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_true(hla_motif_graph_ok(g))
  expect_equal(igraph::vcount(g), 2L)
})

test_that("rows without a V gene are still dropped", {
  df <- data.frame(
    barcode = "a",
    CTgene = "NA",
    CTaa = "CASSL",
    sample = "s1",
    stringsAsFactors = FALSE
  )
  expect_null(hla_parse_ir_segments(list(s1 = df), "TRB"))
})

## ---- hla_detect_chains ------------------------------------------------ ##

test_that("hla_detect_chains reports chains present in CTgene", {
  data <- list(
    s1 = data.frame(
      CTgene = c("TRBV1.TRBJ2", "TRAV1.TRAJ2_TRBV3.TRBJ1"),
      stringsAsFactors = FALSE
    )
  )
  chains <- hla_detect_chains(data)
  expect_true("TRB" %in% chains)
  expect_true("TRA" %in% chains)
  expect_false("IGH" %in% chains)
})

test_that("hla_detect_chains handles empty / NULL input", {
  expect_equal(hla_detect_chains(NULL), character(0))
  expect_equal(hla_detect_chains(list()), character(0))
})

test_that("hla_detect_chains scans beyond the first three samples", {
  make_chain <- function(gene) {
    data.frame(CTgene = gene, stringsAsFactors = FALSE)
  }
  data <- list(
    s1 = make_chain("IGHV1.IGHJ1"),
    s2 = make_chain("IGHV1.IGHJ1"),
    s3 = make_chain("IGHV1.IGHJ1"),
    s4 = make_chain("TRBV1.TRBJ1")
  )

  expect_true("TRB" %in% hla_detect_chains(data))
})

## ---- hla_make_consensus / variable_aa --------------------------------- ##

test_that("consensus marks differing positions with x", {
  expect_equal(hla_make_consensus(c("CASSL", "CASSF")), "CASSx")
  expect_equal(hla_make_consensus("CASSL"), "CASSL")
  expect_true(is.na(hla_make_consensus(character(0))))
})

test_that("variable_aa returns residues only at consensus x positions", {
  expect_equal(hla_motif_variable_aa("CASSL", "CASSx"), "L")
  expect_equal(hla_motif_variable_aa("CASSL", "CASSL"), "")
  # length mismatch is a display no-op, not an error
  expect_equal(hla_motif_variable_aa("CASS", "CASSx"), "")
})

## ---- Hamming edges: only equal-length, distance == 1 ------------------ ##

test_that("edges join only equal-length CDR3 at Hamming distance 1", {
  # CASSL and CASSF differ by 1 (edge); CASSLL differs in length (no edge).
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSL", "CASSF", "CASSLL")),
    "TRB"
  )
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_false(is.null(g))
  # Only the two length-5 sequences connect.
  expect_equal(igraph::vcount(g), 2L)
  expect_equal(igraph::ecount(g), 1L)
  expect_setequal(igraph::V(g)$name, c("CASSL", "CASSF"))
})

test_that("distance-2 pairs get no direct edge", {
  # CASSL vs CATTL = 2 substitutions -> no edge, no component of size 2.
  seg <- hla_parse_ir_segments(make_ir_list(c("CASSL", "CATTL")), "TRB")
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_null(g)
})

## ---- transitive components + diameter --------------------------------- ##

test_that("transitive component keeps A,C together and reports max mismatch 2", {
  # A=XAAAA, B=XBAAA, C=XBBAA :
  #   A-B = 1 (pos2), B-C = 1 (pos3), A-C = 2 (pos2,pos3).
  # So A and C are only linked transitively via B; the component is {A,B,C}
  # with max mismatch 2. This proves membership is transitive AND that we surface
  # the true max pairwise distance rather than implying all pairs are <= 1.
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CAAAA", "CABAA", "CABBA")),
    "TRB"
  )
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_false(is.null(g))
  expect_equal(igraph::components(g)$no, 1L)
  expect_equal(igraph::vcount(g), 3L)
  expect_equal(igraph::ecount(g), 2L) # A-B and B-C only, NOT A-C
  expect_true(all(igraph::V(g)$motif_max_mismatch == 2L))
})

## ---- min_nodes uses >= (not >) ---------------------------------------- ##

test_that("min_nodes keeps components with size >= N", {
  seg <- hla_parse_ir_segments(make_ir_list(c("CASSL", "CASSF")), "TRB")
  # A 2-node component survives min_nodes = 2 (>=), which the old > would drop.
  g2 <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_false(is.null(g2))
  expect_equal(igraph::vcount(g2), 2L)
  # min_nodes = 3 drops the 2-node component.
  g3 <- hla_build_motif_graph(seg, min_nodes = 3L)
  expect_null(g3)
})

## ---- show_isolated ---------------------------------------------------- ##

test_that("show_isolated keeps degree-0 CDR3 as points", {
  # CASSL~CASSF connect; CWWWW is isolated (unique length + no neighbour).
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSL", "CASSF", "CWWWW")),
    "TRB"
  )
  g_hidden <- hla_build_motif_graph(seg, min_nodes = 2L, show_isolated = FALSE)
  expect_equal(igraph::vcount(g_hidden), 2L)
  g_shown <- hla_build_motif_graph(seg, min_nodes = 2L, show_isolated = TRUE)
  expect_true("CWWWW" %in% igraph::V(g_shown)$name)
})

## ---- split by V ------------------------------------------------------- ##

test_that("split-by-V does not connect same CDR3 across different V genes", {
  # Same CDR3 length + 1-diff sequences, but different V genes -> no edge.
  df1 <- data.frame(
    barcode = c("a", "b"),
    CTgene = c("TRBV1.TRBJ2", "TRBV9.TRBJ2"),
    CTaa = c("CASSL", "CASSF"),
    sample = "s1",
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df1), "TRB")
  g <- hla_build_motif_graph(seg, by_v = TRUE, min_nodes = 2L)
  # Different V genes, so the two are in separate bins and never connect.
  expect_null(g)
})

test_that("split-by-V preserves the same CDR3 in multiple V bins", {
  df <- data.frame(
    barcode = letters[1:4],
    CTgene = c(
      "TRBV1.TRBJ2",
      "TRBV9.TRBJ2",
      "TRBV1.TRBJ2",
      "TRBV9.TRBJ2"
    ),
    CTaa = c("CASSL", "CASSL", "CASSF", "CASST"),
    sample = "s1",
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")

  g <- hla_build_motif_graph(seg, by_v = TRUE, min_nodes = 2L)

  expect_equal(igraph::vcount(g), 4L)
  expect_equal(igraph::ecount(g), 2L)
  expect_setequal(
    paste(igraph::V(g)$v_gene, igraph::V(g)$cdr3, sep = "::"),
    c("TRBV1::CASSL", "TRBV1::CASSF", "TRBV9::CASSL", "TRBV9::CASST")
  )
})

## ---- clone_count aggregation ------------------------------------------ ##

test_that("duplicate CDR3 across cells aggregates clone_count", {
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSL", "CASSL", "CASSF")),
    "TRB"
  )
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  cc <- igraph::V(g)$clone_count[igraph::V(g)$name == "CASSL"]
  expect_equal(cc, 2L)
})

## ---- metadata distributions carried on nodes -------------------------- ##

test_that("metadata distribution is carried per node without collapsing", {
  seg <- hla_parse_ir_segments(
    make_ir_list(
      c("CASSL", "CASSL", "CASSF"),
      cell_types = c("CD8 T", "CD4 T", "CD8 T")
    ),
    "TRB"
  )
  g <- hla_build_motif_graph(seg, min_nodes = 2L, meta_cols = "cell_type")
  dist <- igraph::V(g)$cell_type_dist[igraph::V(g)$name == "CASSL"]
  expect_true(grepl("2 types", dist))
  expect_true(grepl("CD8 T", dist))
  expect_true(grepl("CD4 T", dist))
})

## ---- size guard ------------------------------------------------------- ##

test_that("total size guard trips with a guard message (not a usable graph)", {
  many <- sprintf("CASS%05d", seq_len(HLA_MOTIF_MAX_TOTAL + 1))
  # give them all the same length so parsing keeps them
  seg <- hla_parse_ir_segments(make_ir_list(many), "TRB")
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_false(hla_motif_graph_ok(g))
  expect_true(grepl("unique CDR3", attr(g, "guard")))
})

## ---- draw layout ------------------------------------------------------- ##

test_that("the graph carries igraph-computed draw coordinates", {
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSL", "CASSF", "CASST", "CWWWL", "CWWWF")),
    "TRB"
  )
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_true(hla_motif_graph_ok(g))
  x <- igraph::V(g)$layout_x
  y <- igraph::V(g)$layout_y
  expect_equal(length(x), igraph::vcount(g))
  expect_equal(length(y), igraph::vcount(g))
  expect_false(any(is.na(c(x, y))))
  # Not all at the origin: a degenerate layout would draw every node on one spot.
  expect_true(length(unique(paste(x, y))) > 1)
})

test_that("the layout is deterministic for the same graph", {
  # The layout must not move when nothing about the graph moved. It is computed
  # once and cached WITH the graph, but the seed is what makes two sessions (and
  # two screenshots of one analysis) agree.
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSL", "CASSF", "CASST", "CWWWL", "CWWWF")),
    "TRB"
  )
  a <- hla_build_motif_graph(seg, min_nodes = 2L)
  b <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_equal(igraph::V(a)$layout_x, igraph::V(b)$layout_x)
  expect_equal(igraph::V(a)$layout_y, igraph::V(b)$layout_y)
})

## ---- split build: raw (cached) + finalize (cheap) ---------------------- ##

test_that("raw build keeps every node; finalize applies the min-size filter", {
  # CQQQQ shares no Hamming-1 neighbour with the others, so it is isolated.
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSL", "CASSF", "CASST", "CWWWL", "CWWWF", "CQQQQ")),
    "TRB"
  )
  raw <- hla_build_motif_graph_raw(seg)
  expect_true(hla_motif_graph_ok(raw))
  expect_equal(igraph::vcount(raw), 6L)
  # Default finalize drops the isolated singleton (min size 2, no isolated).
  g <- hla_finalize_motif_graph(raw, min_nodes = 2L, show_isolated = FALSE)
  expect_true(hla_motif_graph_ok(g))
  expect_equal(igraph::vcount(g), 5L)
})

test_that("split build matches the wrapper across thresholds", {
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSL", "CASSF", "CASST", "CWWWL", "CWWWF")),
    "TRB"
  )
  raw <- hla_build_motif_graph_raw(seg)
  for (mn in 2:3) {
    a <- hla_finalize_motif_graph(raw, min_nodes = mn)
    b <- hla_build_motif_graph(seg, min_nodes = mn)
    expect_setequal(igraph::V(a)$name, igraph::V(b)$name)
    expect_equal(igraph::ecount(a), igraph::ecount(b))
  }
})

test_that("raising the threshold keeps surviving clusters in place", {
  # A size-3 cluster (length 6) and a size-2 cluster (length 5, a different
  # bin). Raising min_nodes past 2 drops the small one; the big one's nodes must
  # keep the exact coordinates they had -- that is the whole point of laying the
  # core out once and only filtering afterwards.
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSLL", "CASSFL", "CASSTL", "CWWWL", "CWWWF")),
    "TRB"
  )
  raw <- hla_build_motif_graph_raw(seg)
  g2 <- hla_finalize_motif_graph(raw, min_nodes = 2L)
  g3 <- hla_finalize_motif_graph(raw, min_nodes = 3L)
  common <- intersect(igraph::V(g2)$name, igraph::V(g3)$name)
  expect_true(length(common) >= 3)
  i2 <- match(common, igraph::V(g2)$name)
  i3 <- match(common, igraph::V(g3)$name)
  expect_equal(igraph::V(g2)$layout_x[i2], igraph::V(g3)$layout_x[i3])
  expect_equal(igraph::V(g2)$layout_y[i2], igraph::V(g3)$layout_y[i3])
})

test_that("positions also hold when isolated CDR3s are shown", {
  # Same graph plus an isolated CDR3 (CQQQQQ shares no Hamming-1 neighbour).
  # Keeping it on screen must not re-run the layout: filling in the isolate has
  # to leave every connected survivor on the exact coordinates it already had.
  seg <- hla_parse_ir_segments(
    make_ir_list(c("CASSLL", "CASSFL", "CASSTL", "CWWWL", "CWWWF", "CQQQQQ")),
    "TRB"
  )
  raw <- hla_build_motif_graph_raw(seg)
  g2 <- hla_finalize_motif_graph(raw, min_nodes = 2L, show_isolated = TRUE)
  g3 <- hla_finalize_motif_graph(raw, min_nodes = 3L, show_isolated = TRUE)
  # size-3 cluster + size-2 cluster + the isolate, then the size-2 one drops out
  expect_equal(igraph::vcount(g2), 6L)
  expect_equal(igraph::vcount(g3), 4L)
  # every shown node has a coordinate, including the isolate
  expect_false(anyNA(igraph::V(g2)$layout_x))
  expect_false(anyNA(igraph::V(g3)$layout_x))
  common <- intersect(igraph::V(g2)$name, igraph::V(g3)$name)
  expect_equal(length(common), 4L)
  i2 <- match(common, igraph::V(g2)$name)
  i3 <- match(common, igraph::V(g3)$name)
  expect_equal(igraph::V(g2)$layout_x[i2], igraph::V(g3)$layout_x[i3])
  expect_equal(igraph::V(g2)$layout_y[i2], igraph::V(g3)$layout_y[i3])
})

test_that("computing the layout leaves the caller's RNG stream alone", {
  # hla_motif_layout seeds itself so the picture is reproducible. Doing that
  # without restoring would re-seed the whole Shiny session from a render call:
  # every later random draw, in any tab, would follow from HLA_LAYOUT_SEED.
  seg <- hla_parse_ir_segments(make_ir_list(c("CASSL", "CASSF")), "TRB")
  g <- hla_build_motif_graph(seg, min_nodes = 2L)

  set.seed(123)
  expected <- runif(3)
  set.seed(123)
  invisible(hla_motif_layout(g))
  expect_equal(runif(3), expected)
})

test_that("hla_motif_layout declines a graph that is not drawable", {
  expect_null(hla_motif_layout(NULL))
  expect_null(hla_motif_layout(NA))
})

## ---- per-node summaries: mode + distribution --------------------------- ##
## These pin the CONTRACT of the per-node summaries rather than any one
## implementation of them. The aggregation was rewritten from a per-node
## table()/sort() loop to one grouped tally per column; that is a ~40x speedup
## and must be a pure speedup, so every tie-break and format below is the
## behaviour of the version that shipped before it.

test_that("a tied mode resolves to the alphabetically first value", {
  # NOT the first-seen value. `table()` names its counts by factor level, i.e.
  # alphabetically, and a stable descending sort keeps that order among equal
  # counts — so "A" wins a 2-2 tie against "B" even though "B" was seen first.
  # A rewrite that tallies in first-appearance order silently flips ~20% of
  # ties, which would move node colours and tooltips with no error anywhere.
  df <- data.frame(
    barcode = paste0("bc", 1:4),
    CTgene = c("TRBV2.TRBJ1", "TRBV2.TRBJ1", "TRBV1.TRBJ1", "TRBV1.TRBJ1"),
    CTaa = "CASSL",
    sample = "s1",
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  agg <- hla_aggregate_cdr3_nodes(seg)
  expect_equal(nrow(agg), 1L)
  expect_equal(agg$v_gene, "TRBV1")
})

test_that("the distribution is ordered by count, ties alphabetically", {
  # The string is read left to right as "what this node mostly is": the order
  # is the information. Equal counts fall back to alphabetical so the same node
  # never renders two different strings across sessions.
  df <- data.frame(
    barcode = paste0("bc", 1:5),
    CTgene = "TRBV1.TRBJ1",
    CTaa = "CASSL",
    sample = "s1",
    cell_type = c("CD8 T", "CD8 T", "CD8 T", "B cell", "A cell"),
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  agg <- hla_aggregate_cdr3_nodes(seg, meta_cols = "cell_type")
  expect_equal(agg$cell_type_dist, "3 types: CD8 T (3), A cell (1), B cell (1)")
  expect_equal(agg$cell_type, "CD8 T")
})

test_that("a single-level distribution says 'type', not 'types'", {
  df <- data.frame(
    barcode = c("bc1", "bc2"),
    CTgene = "TRBV1.TRBJ1",
    CTaa = "CASSL",
    sample = "s1",
    cell_type = c("CD8 T", "CD8 T"),
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  agg <- hla_aggregate_cdr3_nodes(seg, meta_cols = "cell_type")
  expect_equal(agg$cell_type_dist, "1 type: CD8 T (2)")
})

test_that("NA and empty values are dropped from mode and distribution", {
  # An unlabelled cell is absent from the summary, never a level called "NA":
  # the tooltip would otherwise report missingness as if it were a cell type.
  df <- data.frame(
    barcode = paste0("bc", 1:4),
    CTgene = "TRBV1.TRBJ1",
    CTaa = "CASSL",
    sample = "s1",
    cell_type = c("CD8 T", NA, "", "CD8 T"),
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  agg <- hla_aggregate_cdr3_nodes(seg, meta_cols = "cell_type")
  expect_equal(agg$cell_type_dist, "1 type: CD8 T (2)")
  expect_equal(agg$cell_type, "CD8 T")
  expect_equal(agg$clone_count, 4L) # the cells still count toward node size
})

test_that("a node whose column is entirely NA summarises to NA", {
  df <- data.frame(
    barcode = c("bc1", "bc2"),
    CTgene = "TRBV1.TRBJ1",
    CTaa = "CASSL",
    sample = "s1",
    cell_type = NA_character_,
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  agg <- hla_aggregate_cdr3_nodes(seg, meta_cols = "cell_type")
  expect_true(is.na(agg$cell_type))
  expect_true(is.na(agg$cell_type_dist))
})

test_that("samples_all is the sorted unique sample set, not the modal sample", {
  seg <- hla_parse_ir_segments(
    make_ir_list(rep("CASSL", 3), samples = c("s2", "s1", "s2")),
    "TRB"
  )
  agg <- hla_aggregate_cdr3_nodes(seg, meta_cols = "sample")
  expect_equal(agg$samples_all, "s1,s2")
})

test_that("the context column is summarised by context_summary, not by mode", {
  # A node spanning both lineages is the finding; the modal value would report
  # it as whichever compartment happened to contribute more cells. The plain
  # `_dist` string alongside it stays an ordinary tally.
  seg <- hla_parse_ir_segments(
    make_ir_list(
      rep("CASSL", 3),
      cell_types = c("CD8 T", "CD8 T", "CD4 T")
    ),
    "TRB"
  )
  seg$mhc_context <- hla_lineage_context(seg$cell_type)
  agg <- hla_aggregate_cdr3_nodes(seg, context_col = "mhc_context")
  expect_equal(agg$mhc_context, "Mixed") # NOT "Class I", which is the mode
  expect_equal(agg$mhc_context_dist, "2 types: Class I (2), Class II (1)")
})

test_that("by_v keys nodes on (V gene, CDR3) and keeps both", {
  df <- data.frame(
    barcode = paste0("bc", 1:3),
    CTgene = c("TRBV1.TRBJ1", "TRBV2.TRBJ1", "TRBV1.TRBJ1"),
    CTaa = "CASSL",
    sample = "s1",
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  agg <- hla_aggregate_cdr3_nodes(seg, by_v = TRUE)
  expect_equal(nrow(agg), 2L)
  expect_setequal(agg$node_id, c("TRBV1::CASSL", "TRBV2::CASSL"))
  expect_setequal(agg$clone_count, c(2L, 1L))
  expect_true(all(agg$cdr3 == "CASSL"))
})

## ---- sample of origin -------------------------------------------------- ##

test_that("sample origin names a single sample and collapses the rest", {
  expect_equal(hla_node_sample_origin("s1"), "s1")
  expect_equal(hla_node_sample_origin("s1,s2"), HLA_SHARED_LABEL)
  expect_equal(hla_node_sample_origin("s1,s2,s3"), HLA_SHARED_LABEL)
  expect_equal(
    hla_node_sample_origin(c("s1", "s1,s2", "s3")),
    c("s1", HLA_SHARED_LABEL, "s3")
  )
})

test_that("sample origin is NA when the node tracks no sample", {
  expect_true(is.na(hla_node_sample_origin("")))
  expect_true(is.na(hla_node_sample_origin(NA_character_)))
  expect_equal(hla_node_sample_origin(character(0)), character(0))
})

test_that("sample origin is not the modal sample", {
  # The point of the column: a CDR3 seen once in s1 and three times in s2 has
  # mode "s2", which would paint it as an s2-private clone and hide that it
  # recurs across samples. Origin must say "Shared" instead.
  df <- data.frame(
    barcode = c("a", "b", "c", "d"),
    CTgene = "TRBV1.TRBJ2",
    CTaa = "CASSL",
    sample = c("s1", "s2", "s2", "s2"),
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  agg <- hla_aggregate_cdr3_nodes(seg, meta_cols = "sample")
  expect_equal(nrow(agg), 1L)
  expect_equal(agg$sample, "s2") # the mode
  expect_equal(agg$sample_origin, HLA_SHARED_LABEL) # what we must show
  expect_equal(agg$samples_all, "s1,s2")
})

test_that("sample origin reaches the graph as a vertex attribute", {
  # It has to survive aggregation -> motif grouping -> igraph, or the renderer
  # silently falls back to the "cluster" colouring.
  df <- data.frame(
    barcode = c("a", "b", "c"),
    CTgene = "TRBV1.TRBJ2",
    CTaa = c("CASSL", "CASSF", "CASSL"),
    sample = c("s1", "s2", "s2"),
    stringsAsFactors = FALSE
  )
  seg <- hla_parse_ir_segments(list(s1 = df), "TRB")
  g <- hla_build_motif_graph(seg, min_nodes = 2L, meta_cols = "sample")
  expect_true(hla_motif_graph_ok(g))
  origin <- igraph::vertex_attr(g, "sample_origin")
  expect_setequal(origin, c(HLA_SHARED_LABEL, "s2"))
})

test_that("max mismatch is the Hamming spread, NOT the graph diameter", {
  # These are different numbers and the reported one must stay the Hamming
  # spread. A chain A-B-C-D-E where every hop touches a fresh position has a
  # graph diameter of 4 hops, and here that happens to equal the Hamming spread.
  # The distinguishing case is below.
  seqs <- c("CAAAAA", "CBAAAA", "CBBAAA", "CBBBAA", "CBBBBA")
  seg <- hla_parse_ir_segments(make_ir_list(seqs), "TRB")
  g <- hla_build_motif_graph(seg, min_nodes = 2L)
  expect_equal(igraph::diameter(g), 4)
  expect_true(all(igraph::V(g)$motif_max_mismatch == 4L))

  # Now a walk that REVISITS a position: A->B changes pos2, B->C changes pos3,
  # C->D changes pos2 again. D differs from A at pos3 only, so A-D is an edge
  # and the graph is a cycle: graph diameter 2 hops, but the Hamming spread is
  # still 2. Reporting hops would answer a question nobody asked - how many
  # substitution steps the walk took - rather than how different the sequences
  # are, which is what a reader of a CDR3 motif needs.
  seqs2 <- c("CAAAA", "CABAA", "CABBA", "CAABA")
  seg2 <- hla_parse_ir_segments(make_ir_list(seqs2), "TRB")
  g2 <- hla_build_motif_graph(seg2, min_nodes = 2L)
  expect_equal(igraph::components(g2)$no, 1L)
  expect_equal(igraph::ecount(g2), 4L) # a 4-cycle: A-B, B-C, C-D, D-A
  expect_true(all(igraph::V(g2)$motif_max_mismatch == 2L))
  # The reported value must equal the max pairwise Hamming distance, computed
  # independently of the implementation.
  dm <- stringdist::stringdistmatrix(seqs2, seqs2, method = "hamming")
  expect_equal(max(dm), 2)
})

test_that("no user-facing string calls the max mismatch a diameter", {
  # `diameter` on a network reads as the longest shortest-path in hops, which is
  # a different and larger number: measured on the shipped demo, 16 of 20 motifs
  # disagree (e.g. max mismatch 6 against graph diameter 8). The word may appear
  # only where the two are being contrasted.
  viz <- paste(
    readLines(
      system.file(
        "shiny/v1.4/hla_tcr_motifs/visualizations.R",
        package = "CerebroNexus"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_no_match(viz, "consensus %s &middot; diameter", fixed = TRUE)
  expect_match(viz, "max mismatch", fixed = TRUE)
  expect_no_match(names(hla_motif_summary(NULL)), "^diameter$", perl = TRUE)
  expect_true("max_mismatch" %in% names(hla_motif_summary(NULL)))
})
