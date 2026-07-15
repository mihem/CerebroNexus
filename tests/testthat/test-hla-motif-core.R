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

test_that("transitive component keeps A,C together and reports diameter 2", {
  # A=XAAAA, B=XBAAA, C=XBBAA :
  #   A-B = 1 (pos2), B-C = 1 (pos3), A-C = 2 (pos2,pos3).
  # So A and C are only linked transitively via B; the component is {A,B,C}
  # with diameter 2. This proves membership is transitive AND that we surface
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
  expect_true(all(igraph::V(g)$motif_diameter == 2L))
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
