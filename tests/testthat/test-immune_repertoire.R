# test-immune_repertoire.R — Tests for immune repertoire module
#
# The example dataset (example.crb) carries real 10x immune repertoire data
# (sc5p_v2_hs_PBMC_10k, 5' GEX + TCR + BCR from the same experiment). The
# single donor is partitioned into three demo samples; sample labels do not
# represent distinct biological donors.

shiny_root <- viewer_test_path()
example_crb <- inst_test_path("extdata", "examples", "example.crb")

test_that("immune_repertoire UI defines correct tabName", {
  scope <- viewer_ui_test_env()
  sys.source(viewer_test_path("immune_repertoire", "UI.R"), scope)
  page <- htmltools::tagQuery(scope$tab_immune_repertoire)
  expect_equal(page$filter("#shiny-tab-immune_repertoire")$length(), 1L)
})

test_that("example.crb contains real immune repertoire data", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  ir <- crb$getImmuneRepertoire()
  expect_true(is.list(ir))
  expect_true(length(ir) > 0)
  for (nm in names(ir)) {
    df <- ir[[nm]]
    expect_s3_class(df, "data.frame")
    expect_true(all(
      c("barcode", "CTgene", "CTnt", "CTaa", "CTstrict") %in%
        colnames(df)
    ))
    expect_true(nrow(df) > 0)
  }
})

test_that("example.crb IR barcodes align with cell metadata", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  ir <- crb$getImmuneRepertoire()
  md <- crb$getMetaData()
  ir_bc <- unlist(lapply(ir, function(df) df$barcode), use.names = FALSE)
  overlap <- length(intersect(ir_bc, md$cell_barcode))
  expect_true(overlap > 0)
  # every IR barcode should correspond to a real cell in the dataset
  expect_equal(overlap, length(unique(ir_bc)))
})

test_that("IR grouping variables are recoverable from cell metadata by barcode", {
  # The IR data.frames carry only standard scRepertoire columns. The module
  # joins ANY grouping variable (getGroups(): sample, condition, cell type, ...)
  # onto the IR rows by barcode at runtime. This verifies that join is possible
  # for the example data set, so the Group by dropdown is populated regardless
  # of which columns a producer embedded in the IR table.
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  ir <- crb$getImmuneRepertoire()
  md <- crb$getMetaData()
  groups <- crb$getGroups()
  expect_true(length(groups) >= 1)

  # replicate the module's join: map each IR barcode to a metadata row
  ir_bc <- unlist(lapply(ir, function(df) df$barcode), use.names = FALSE)
  idx <- match(ir_bc, md$cell_barcode)
  expect_true(all(!is.na(idx)))

  # at least one grouping variable yields >= 2 levels over the IR cells
  multilevel <- vapply(
    groups,
    function(g) {
      length(unique(md[[g]][idx])) >= 2
    },
    logical(1)
  )
  expect_true(any(multilevel))
})

test_that("example.crb IR contains both TCR and BCR clonotypes", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  ir <- crb$getImmuneRepertoire()
  all_ct <- paste(unlist(lapply(ir, function(df) df$CTgene)), collapse = ";")
  expect_true(grepl("TR[AB]", all_ct)) # TCR present
  expect_true(grepl("IG[HKL]", all_ct)) # BCR present
})

test_that("example.crb IR has TCR chains detectable from CTgene", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  ir <- crb$getImmuneRepertoire()
  all_ct <- paste(unlist(lapply(ir, function(df) df$CTgene)), collapse = ";")
  has_tcr <- grepl("TRA", all_ct) || grepl("TRB", all_ct)
  expect_true(has_tcr)
})

test_that("paired scatter falls back to manual sample selection", {
  helper <- file.path(
    shiny_root,
    "immune_repertoire",
    "paired_scatter_helpers.R"
  )
  expect_true(file.exists(helper))
  env <- new.env(parent = globalenv())
  sys.source(helper, envir = env)

  meta <- data.frame(
    .sample_name = c("sample_1", "sample_2", "sample_3"),
    sample = c("sample_1", "sample_2", "sample_3"),
    orig.ident = c("sample_1", "sample_2", "sample_3"),
    stringsAsFactors = FALSE
  )
  choices <- env$ir_paired_scatter_choices(meta)

  expect_equal(choices$compare_candidates, character(0))
  expect_equal(choices$mode, "manual")
  expect_equal(choices$sample_choices, meta$.sample_name)
})

test_that("example.crb preserves core data fields", {
  skip_if_not(file.exists(example_crb))
  crb <- readCerebro(example_crb)
  expect_true(!is.null(crb$getMetaData()))
  expect_true(nrow(crb$getMetaData()) > 0)
  expect_true(!is.null(crb$experiment))
})

test_that("Clonal UMAP split layout avoids empty facet slots on wide canvases", {
  viz <- file.path(shiny_root, "immune_repertoire", "visualizations.R")
  skip_if_not(file.exists(viz))
  env <- new.env(parent = baseenv())
  env$`%||%` <- function(x, y) if (is.null(x)) y else x
  eval(viewer_r_assignment(viz, "ir_umap_split_layout"), env)

  layout <- env$ir_umap_split_layout(3, width = 1200, height = 450)

  expect_equal(layout$ncol, 3L)
  expect_equal(layout$nrow, 1L)
  expect_gte(layout$panel_px, 300)
  expect_lte(layout$height, 500)

  tall_layout <- env$ir_umap_split_layout(6, width = 900, height = 500)

  expect_equal(tall_layout$ncol, 3L)
  expect_equal(tall_layout$nrow, 2L)
  expect_gte(tall_layout$panel_px, 300)
  expect_gte(tall_layout$height, 600)
})

# ---- Compare (interactive alluvial) ------------------------------------- #

# Source the pure helpers into a throwaway env so the geometry can be tested
# without the Shiny app or scRepertoire (the prep + drawing functions take a
# plain data.frame shaped like clonalCompare(exportTable = TRUE)).
compare_env <- local({
  helper <- file.path(shiny_root, "immune_repertoire", "compare_helpers.R")
  if (!file.exists(helper)) {
    return(NULL)
  }
  e <- new.env(parent = globalenv())
  sys.source(file.path(shiny_root, "plotting_functions.R"), envir = e)
  sys.source(helper, envir = e)
  e
})

# A minimal two-group table: clone A is shared (different sizes), clone B is
# private to sample_1, clone C is private to sample_2.
compare_tab_2grp <- data.frame(
  clones = c("A", "B", "A", "C"),
  Proportion = c(0.5, 0.3, 0.2, 0.4),
  Sample = c("sample_1", "sample_1", "sample_2", "sample_2"),
  stringsAsFactors = FALSE
)

test_that("compare prep stacks two groups and links only shared clones", {
  skip_if(is.null(compare_env))
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  expect_true(prep$ok)
  expect_equal(prep$value_col, "Proportion")
  expect_setequal(prep$clones, c("A", "B", "C"))
  # 4 non-zero clone/group cells -> 4 rectangles.
  expect_equal(nrow(prep$rects), 4L)
  # Only clone A appears in both groups -> exactly one ribbon.
  expect_equal(nrow(prep$ribbons), 1L)
  expect_equal(prep$ribbons$clone, "A")
  expect_equal(prep$ribbons$from_index, 1L)
  expect_equal(prep$ribbons$to_index, 2L)
})

test_that("compare ribbon ends have different widths when the clone changes size", {
  skip_if(is.null(compare_env))
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  rb <- prep$ribbons[prep$ribbons$clone == "A", ]
  src_h <- rb$from_ymax - rb$from_ymin
  tgt_h <- rb$to_ymax - rb$to_ymin
  # A is 0.5 in sample_1 and 0.2 in sample_2, so the two ends differ.
  expect_equal(src_h, 0.5)
  expect_equal(tgt_h, 0.2)
  expect_false(isTRUE(all.equal(src_h, tgt_h)))
})

test_that("compare prep detects the Count column in count mode", {
  skip_if(is.null(compare_env))
  tab <- data.frame(
    clones = c("A", "A"),
    Count = c(10, 4),
    Sample = c("sample_1", "sample_2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("sample_1", "sample_2"),
    proportion = FALSE
  )
  expect_true(prep$ok)
  expect_equal(prep$value_col, "Count")
  expect_false(prep$proportion)
})

test_that("compare prep stacks every column in one shared clone order", {
  skip_if(is.null(compare_env))
  # A has the largest total, then B, then C. All columns must stack in that
  # SHARED order (A bottom, C top) so a clone keeps a consistent band and its
  # ribbons run parallel without crossing — even though within s2 the sizes
  # would rank differently if each column were sorted on its own.
  tab <- data.frame(
    clones = c("A", "B", "C", "A", "B", "C"),
    Proportion = c(0.6, 0.3, 0.1, 0.1, 0.3, 0.6),
    Sample = c("s1", "s1", "s1", "s2", "s2", "s2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2"),
    proportion = TRUE
  )
  # A(0.7 total) > B(0.6) > C(0.7)? totals: A=0.7, B=0.6, C=0.7 -> A,C tie by
  # name so order is A, C, B. Assert the SAME order in both columns.
  seg_order <- function(gi) {
    r <- prep$rects[prep$rects$group_index == gi, , drop = FALSE]
    r <- r[order(r$ymin), , drop = FALSE] # bottom -> top
    r$clone
  }
  expect_equal(seg_order(1), prep$clones)
  expect_equal(seg_order(2), prep$clones)
  expect_equal(seg_order(1), seg_order(2))
})

test_that("compare prep reports per-clone totals in clone order", {
  skip_if(is.null(compare_env))
  tab <- data.frame(
    clones = c("A", "B", "A", "C"),
    Proportion = c(0.5, 0.3, 0.2, 0.4),
    Sample = c("s1", "s1", "s2", "s2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2"),
    proportion = TRUE
  )
  # totals: A = 0.7, C = 0.4, B = 0.3 -> clone order A, C, B.
  expect_equal(prep$clones, c("A", "C", "B"))
  expect_equal(prep$totals, c(0.7, 0.4, 0.3))
  # The legend label appends the total in the current mode.
  expect_equal(
    compare_env$ir_compare_legend_label("A", 0.7, TRUE),
    "A (0.7)"
  )
  expect_equal(
    compare_env$ir_compare_legend_label("A", 21, FALSE),
    "A (21)"
  )
  # No total -> bare (possibly truncated) name.
  expect_equal(compare_env$ir_compare_legend_label("A"), "A")
})

test_that("compare prep links only adjacent groups across three groups", {
  skip_if(is.null(compare_env))
  tab <- data.frame(
    clones = rep("A", 3),
    Proportion = c(0.4, 0.3, 0.5),
    Sample = c("s1", "s2", "s3"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2", "s3"),
    proportion = TRUE
  )
  pairs <- paste0(prep$ribbons$from_index, "->", prep$ribbons$to_index)
  # A spans all three groups: link 1->2 and 2->3, never 1->3.
  expect_setequal(pairs, c("1->2", "2->3"))
})

test_that("compare prep preserves the requested group order", {
  skip_if(is.null(compare_env))
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_2", "sample_1"), # reversed
    proportion = TRUE
  )
  expect_equal(prep$groups, c("sample_2", "sample_1"))
})

test_that("compare prep returns friendly empty states", {
  skip_if(is.null(compare_env))
  # Empty table.
  e1 <- compare_env$ir_prepare_compare_alluvial(
    data.frame(),
    c("a", "b")
  )
  expect_false(e1$ok)
  expect_true(nzchar(e1$message))
  # Only one group present -> nothing to compare.
  one <- data.frame(
    clones = c("A", "B"),
    Proportion = c(0.5, 0.5),
    Sample = c("s1", "s1"),
    stringsAsFactors = FALSE
  )
  e2 <- compare_env$ir_prepare_compare_alluvial(one, c("s1", "s2"))
  expect_false(e2$ok)
  # All-zero values.
  z <- data.frame(
    clones = c("A", "A"),
    Proportion = c(0, 0),
    Sample = c("s1", "s2"),
    stringsAsFactors = FALSE
  )
  e3 <- compare_env$ir_prepare_compare_alluvial(z, c("s1", "s2"))
  expect_false(e3$ok)
})

test_that("compare plotly shows exactly one legend entry per clone", {
  skip_if(is.null(compare_env))
  skip_if_not_installed("plotly")
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  fig <- compare_env$ir_compare_alluvial_plotly(prep, palette = "Harmonic")
  expect_s3_class(fig, "plotly")
  built <- plotly::plotly_build(fig)
  # Rectangles and ribbons are separate traces (so only the rects get a dark
  # border), but exactly one trace per clone carries the legend entry.
  legend_traces <- Filter(
    function(d) isTRUE(d$showlegend) || is.null(d$showlegend),
    built$x$data
  )
  legend_names <- vapply(
    legend_traces,
    function(d) if (is.null(d$name)) "" else d$name,
    character(1)
  )
  legend_names <- legend_names[nzchar(legend_names)]
  expect_equal(length(legend_names), length(prep$clones))
  expect_equal(length(unique(legend_names)), length(prep$clones))
  # The legend-bearing (rectangle) traces use the dark hairline border; the
  # borderless ribbon traces (showlegend = FALSE) use width 0.
  rect_traces <- Filter(
    function(d) !isFALSE(d$showlegend),
    built$x$data
  )
  borders <- vapply(
    rect_traces,
    function(d) if (is.null(d$line$color)) "" else as.character(d$line$color),
    character(1)
  )
  expect_true(all(borders == "#333333"))
  ribbon_traces <- Filter(
    function(d) {
      isFALSE(d$showlegend) && identical(d$mode, "lines")
    },
    built$x$data
  )
  ribbon_widths <- vapply(
    ribbon_traces,
    function(d) {
      if (is.null(d$line$width)) NA_real_ else as.numeric(d$line$width)
    },
    numeric(1)
  )
  expect_true(all(ribbon_widths == 0))
})

test_that("compare hover comes from single-tooltip anchor markers", {
  skip_if(is.null(compare_env))
  skip_if_not_installed("plotly")
  prep <- compare_env$ir_prepare_compare_alluvial(
    compare_tab_2grp,
    c("sample_1", "sample_2"),
    proportion = TRUE
  )
  fig <- compare_env$ir_compare_alluvial_plotly(prep, palette = "Harmonic")
  built <- plotly::plotly_build(fig)
  # The fill traces (lines mode) must not carry hover text — that is what
  # produced the repeated per-vertex tooltip; their hoverinfo is "skip".
  line_traces <- Filter(
    function(d) identical(d$mode, "lines"),
    built$x$data
  )
  # plotly_build recycles hoverinfo to one value per point, so check every
  # element rather than identity against a scalar.
  expect_true(all(vapply(
    line_traces,
    function(d) all(d$hoverinfo == "skip", na.rm = TRUE),
    logical(1)
  )))
  # Hover lives on invisible marker anchors (one text per rectangle, not
  # per vertex), with the marker made transparent.
  anchor_traces <- Filter(
    function(d) identical(d$mode, "markers"),
    built$x$data
  )
  expect_true(length(anchor_traces) >= 1)
  # Each anchor point has exactly one clean hover string; no NA fragments.
  for (d in anchor_traces) {
    txt <- as.character(d$text)
    expect_false(any(is.na(txt)))
    expect_true(all(grepl("Group:", txt, fixed = TRUE)))
    expect_true(all(grepl("Proportion:", txt, fixed = TRUE)))
  }
  # Markers are visually invisible (opacity 0) so only the tooltip shows.
  expect_true(all(vapply(
    anchor_traces,
    function(d) isTRUE(d$marker$opacity == 0),
    logical(1)
  )))
})

test_that("compare plotly hover keeps the full clone and honours the value mode", {
  skip_if(is.null(compare_env))
  skip_if_not_installed("plotly")
  long_clone <- paste(rep("IGHV3-23.IGHJ4", 6), collapse = "_")
  tab <- data.frame(
    clones = c(long_clone, long_clone),
    Count = c(12, 7),
    Sample = c("s1", "s2"),
    stringsAsFactors = FALSE
  )
  prep <- compare_env$ir_prepare_compare_alluvial(
    tab,
    c("s1", "s2"),
    proportion = FALSE
  )
  fig <- compare_env$ir_compare_alluvial_plotly(prep)
  built <- plotly::plotly_build(fig)
  # Hover text carries the full (untruncated) clone name plus the group and the
  # value in the current mode (count here).
  hover_txt <- vapply(
    built$x$data,
    function(d) paste(as.character(d$text), collapse = " "),
    character(1)
  )
  expect_true(any(grepl(long_clone, hover_txt, fixed = TRUE)))
  expect_true(any(grepl("Count: 12", hover_txt, fixed = TRUE)))
  expect_true(any(grepl("Group: s1", hover_txt, fixed = TRUE)))
  # Legend label is truncated for display but annotated with the clone total
  # (Count 12 + 7 = 19 here), so size is readable straight from the legend.
  legend_names <- vapply(
    built$x$data,
    function(d) if (is.null(d$name)) "" else d$name,
    character(1)
  )
  expect_true(any(nchar(legend_names) < nchar(long_clone)))
  expect_true(any(grepl("(19)", legend_names, fixed = TRUE)))
  # Count mode -> y axis title names the count AND says the height is the value.
  ytitle <- built$x$layout$yaxis$title
  if (is.list(ytitle)) {
    ytitle <- ytitle$text
  }
  expect_match(ytitle, "Clone count", fixed = TRUE)
  expect_match(ytitle, "block height", fixed = TRUE)
})
