# test-ir-definition-sharing.R — pure-function tests for the Definition /
# Sharing tabs. These test the data-layer helpers directly (no Shiny reactive
# context needed), so they source data.R's function definitions.
#
# data.R is a Shiny module fragment: its reactives reference `input`/`session`,
# but the three pure helpers below (ir_parse_segments / ir_definition_counts /
# ir_sharing_classify) are plain functions. We source data.R inside a throwaway
# environment that stubs the reactive machinery, then lift just the helpers out.

data_r <- viewer_test_path("immune_repertoire", "data.R")
testthat::skip_if_not(
  nzchar(data_r) && file.exists(data_r),
  "immune_repertoire/data.R not found (source tree or installed package)"
)

# Stub environment: reactive()/req()/etc. are no-ops that just capture the
# function bodies. We only need the three pure helpers, which don't call these.
ir_env <- new.env()
ir_env$reactive <- function(x) function() eval(substitute(x))
ir_env$reactiveVal <- function(...) function(...) NULL
ir_env$req <- function(...) invisible(NULL)
ir_env$observeEvent <- function(...) invisible(NULL)
ir_env$observe <- function(...) invisible(NULL)
ir_env$`%||%` <- function(a, b) if (is.null(a)) b else a
# Stub out CerebroNexus package functions referenced at top-level in data.R
ir_env$getImmuneRepertoire <- function(...) NULL
ir_env$getMetaData <- function(...) NULL
ir_env$availableProjections <- function(...) character(0)
ir_env$getProjection <- function(...) NULL
ir_env$detect_chains <- function(...) character(0)
ir_env$viewerMemoOne <- function() function(key, compute) compute()
# Shared palette helper (defined in color_setup.R inside the running app); the
# plot builders call it for fill colours, so provide a lightweight stand-in.
ir_env$cerebro_group_colors <- function(n) {
  grDevices::colorRampPalette(c("#4c72a6", "#dd8452", "#6e9e6b"))(max(1L, n))
}
ir_env$input <- list()
ir_env$session <- list()
sys.source(
  file.path(dirname(dirname(data_r)), "clone_contract.R"),
  envir = ir_env,
  keep.source = FALSE
)
sys.source(data_r, envir = ir_env, keep.source = FALSE)

test_that("immune metadata joins follow only active analysis controls", {
  fields <- ir_env$ir_requested_metadata_fields
  expect_length(fields("SizeDist", "", "cell_type", "patient"), 0L)
  expect_identical(
    fields("Clone Sharing", "", "cell_type", "patient"),
    "patient"
  )
  expect_identical(
    fields("Clonal UMAP", "", "cell_type", "patient"),
    "cell_type"
  )
  expect_identical(fields("SizeDist", "sample"), "sample")
  expect_identical(fields("Clone Sharing", "", NULL, "sample"), character())
})

ir_parse_segments <- ir_env$ir_parse_segments
ir_definition_counts <- ir_env$ir_definition_counts
ir_sharing_classify <- ir_env$ir_sharing_classify
ir_build_definition_plot <- ir_env$ir_build_definition_plot
ir_is_bcr_chain <- ir_env$ir_is_bcr_chain
ir_build_sharing_plot <- ir_env$ir_build_sharing_plot
ir_chain_clone_values <- ir_env$ir_chain_clone_values
ir_clone_size_profiles <- ir_env$ir_clone_size_profiles
ir_size_profile_plot <- ir_env$ir_size_profile_plot

test_that("immune metadata joins only requested columns in one aligned pass", {
  data <- list(
    a = data.frame(barcode = c("c3", "c1"), CTgene = c("x", "y")),
    b = data.frame(barcode = "c2", CTgene = "z")
  )
  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3"),
    group = c("g1", "g2", "g3"),
    unused = c("u1", "u2", "u3"),
    stringsAsFactors = FALSE
  )

  out <- ir_env$ir_annotate_metadata(data, metadata, "group")

  expect_identical(out$a$group, c("g3", "g1"))
  expect_identical(out$b$group, "g2")
  expect_false("unused" %in% names(out$a))
})

test_that("segment parsing keeps only requested columns and supplies sample", {
  data <- list(
    donor_a = data.frame(
      barcode = c("c1", "c2"),
      CTgene = rep("TRBV1..TRBJ1.TRBC1", 2),
      CTaa = rep("CASSA", 2),
      unused = c("large-a", "large-b"),
      stringsAsFactors = FALSE
    )
  )
  out <- ir_parse_segments(data, "TRB", columns = "sample")
  expect_named(
    out,
    c("barcode", "sample", "v_gene", "j_gene", "cdr3", "clone_vjc")
  )
  expect_identical(out$sample, rep("donor_a", 2))
  expect_false("unused" %in% names(out))
})

test_that("empirical clone-size profiles preserve exact observed counts", {
  frame <- data.frame(
    CTgene = rep("TRBV1..TRBJ1.TRBC1", 8),
    CTstrict = c(rep("clone-a", 4), rep("clone-b", 2), "clone-c", "clone-d"),
    cohort = c(rep("A", 6), "B", "B"),
    stringsAsFactors = FALSE
  )
  profiles <- ir_clone_size_profiles(
    list(sample_1 = frame),
    chain = "TRB",
    group_by = "cohort",
    threshold = 1
  )
  expect_named(profiles$clone_counts, c("A", "B"))
  expect_equal(sort(unname(profiles$clone_counts$A)), c(2L, 4L))
  expect_equal(sort(unname(profiles$clone_counts$B)), c(1L, 1L))
  expect_equal(rowSums(profiles$profile), c(A = 1, B = 1))

  skip_if_not_installed("ggdendro")
  plot <- ir_size_profile_plot(profiles)
  expect_s3_class(plot, "ggplot")
})

test_that("clonal projection bounds the contextual background before rendering", {
  cells <- paste0("c", seq_len(1000L))
  coords <- matrix(
    seq_len(2000L),
    ncol = 2L,
    dimnames = list(cells, c("x", "y"))
  )
  ir_env$availableProjections <- function() "tsne"
  ir_env$getProjection <- function(...) coords
  ir_env$ir_data_annotated <- function() {
    list(
      sample = data.frame(
        barcode = cells[1:3],
        CTgene = c("TRB-a", "TRB-a", "TRB-b"),
        stringsAsFactors = FALSE
      )
    )
  }
  ir_env$ir_clonecall_col <- function(...) "CTgene"
  ir_env$ir_umap_chains <- function(...) "TRB"

  out <- ir_env$ir_clonal_umap_data(
    "tsne",
    "TCR",
    show_all = TRUE,
    max_background = 10L
  )

  expect_lte(nrow(out), 13L)
  expect_setequal(out$barcode[!is.na(out$expansion)], cells[1:3])

  sampled <- ir_env$ir_clonal_umap_data(
    "tsne",
    "TCR",
    show_all = TRUE,
    percentage = 34,
    max_background = 10L
  )
  expect_false(any(
    cells[1:3] %in% sampled$barcode[is.na(sampled$expansion)]
  ))

  grouped <- ir_env$ir_clonal_umap_data(
    "tsne",
    "TCR",
    show_all = TRUE,
    max_background = 10L,
    split_output = TRUE
  )
  expect_identical(grouped$trace_codes, c(NA_integer_, 1L, 2L))
  expect_identical(lengths(grouped$x), c(10L, 1L, 2L))
  expect_identical(lengths(grouped$y), c(10L, 1L, 2L))
  expect_identical(grouped$key_type, "barcode")
  grouped_coloured_keys <- unlist(lapply(
    grouped$coloured_rows,
    function(rows) grouped$coloured_key[rows]
  ))
  expect_setequal(grouped_coloured_keys, cells[1:3])
  expect_false(any(grouped$background_key %in% cells[1:3]))
})

test_that("clonal expansion sizes avoid sorting clone labels", {
  expansion <- ir_env$ir_clone_expansion(c("clone-b", "clone-a", "clone-b"))

  expect_identical(as.integer(expansion), c(2L, 1L, 2L))
  expect_identical(levels(expansion), ir_env$IR_CLONE_LABELS)
})

test_that("default abundance reduces cells to clone-count bins", {
  data <- list(
    sample_a = data.frame(CTgene = c("a", "a", "b", "c", NA)),
    sample_b = data.frame(CTgene = c("x", "x", "x", "y"))
  )

  counts <- ir_env$ir_clonal_abundance_counts(data, "CTgene")

  expect_identical(
    counts$sample,
    c("sample_a", "sample_a", "sample_b", "sample_b")
  )
  expect_identical(counts$abundance, c(1L, 2L, 1L, 3L))
  expect_identical(counts$n_clones, c(2L, 1L, 1L, 1L))
})

# --- ir_parse_segments -----------------------------------------------------

test_that("ir_parse_segments extracts TRB V/J/CDR3 from CT* columns", {
  data <- list(
    s1 = data.frame(
      barcode = c("bc1", "bc2", "bc3"),
      CTgene = c(
        "TRAV8-6.TRAJ8.TRAC_TRBV6-2..TRBJ2-6.TRBC2",
        "NA_TRBV18..TRBJ2-5.TRBC2",
        "TRAV3.TRAJ26.TRAC;TRAV16.TRAJ5.TRAC_TRBV14..TRBJ2-3.TRBC2"
      ),
      CTaa = c(
        "CAVSAFFQKLVF_CASSYLPRRQDRESSGANVLTF",
        "NA_CASSPMEPIGTQYF",
        "CAVTHYGQNFVF;CASYTGRRALTF_CASSPGGQNTQYF"
      ),
      stringsAsFactors = FALSE
    )
  )
  out <- ir_parse_segments(data, chain = "TRB")
  expect_equal(nrow(out), 3)
  expect_equal(out$v_gene, c("TRBV6-2", "TRBV18", "TRBV14"))
  expect_equal(out$j_gene, c("TRBJ2-6", "TRBJ2-5", "TRBJ2-3"))
  expect_equal(
    out$cdr3,
    c("CASSYLPRRQDRESSGANVLTF", "CASSPMEPIGTQYF", "CASSPGGQNTQYF")
  )
  expect_equal(
    out$clone_vjc,
    paste(out$v_gene, out$j_gene, out$cdr3, sep = ";")
  )
})

test_that("ir_parse_segments drops rows lacking the requested chain", {
  data <- list(
    s1 = data.frame(
      barcode = c("bc1", "bc2"),
      CTgene = c(
        "TRAV8-6.TRAJ8.TRAC_NA",
        "TRAV3.TRAJ26.TRAC_TRBV14..TRBJ2-3.TRBC2"
      ),
      CTaa = c("CAVSAFFQKLVF_NA", "CAVTHYGQNFVF_CASSPGGQNTQYF"),
      stringsAsFactors = FALSE
    )
  )
  out <- ir_parse_segments(data, chain = "TRB")
  expect_equal(nrow(out), 1)
  expect_equal(out$barcode, "bc2")
})

test_that("ir_parse_segments carries metadata columns through", {
  data <- list(
    s1 = data.frame(
      barcode = "bc1",
      CTgene = "TRBV6-2..TRBJ2-6.TRBC2",
      CTaa = "CASSYLPRRQDRESSGANVLTF",
      condition = "A",
      sample = "s1",
      stringsAsFactors = FALSE
    )
  )
  out <- ir_parse_segments(data, chain = "TRB")
  expect_true(all(c("condition", "sample") %in% colnames(out)))
  expect_equal(out$condition, "A")
})

test_that("ir_parse_segments returns NULL on empty or null input", {
  expect_null(ir_parse_segments(NULL, "TRB"))
  expect_null(ir_parse_segments(list(), "TRB"))
})

test_that("ir_parse_segments returns NULL when no row has the chain", {
  data <- list(
    s1 = data.frame(
      barcode = c("bc1", "bc2"),
      CTgene = c("TRAV8-6.TRAJ8.TRAC_NA", "TRAV3.TRAJ26.TRAC_NA"),
      CTaa = c("CAVSAFFQKLVF_NA", "CAVTHYGQNFVF_NA"),
      stringsAsFactors = FALSE
    )
  )
  expect_null(ir_parse_segments(data, chain = "TRB"))
})

test_that("ir_parse_segments preserves per-sample metadata via column union", {
  data <- list(
    s1 = data.frame(
      barcode = "bc1",
      CTgene = "TRBV6-2..TRBJ2-6.TRBC2",
      CTaa = "CASSYLPRRQDRESSGANVLTF",
      condition = "A",
      stringsAsFactors = FALSE
    ),
    s2 = data.frame(
      barcode = "bc2",
      CTgene = "TRBV14..TRBJ2-3.TRBC2",
      CTaa = "CASSPGGQNTQYF",
      treatment = "X",
      stringsAsFactors = FALSE
    )
  )
  out <- ir_parse_segments(data, chain = "TRB")
  expect_equal(nrow(out), 2)
  expect_true(all(c("condition", "treatment") %in% colnames(out)))
  expect_equal(out$condition, c("A", NA))
  expect_equal(out$treatment, c(NA, "X"))
})

# --- ir_definition_counts --------------------------------------------------

test_that("ir_definition_counts returns the 7 resolution levels in order", {
  seg <- data.frame(
    barcode = paste0("bc", 1:4),
    v_gene = c("TRBV1", "TRBV1", "TRBV2", "TRBV2"),
    j_gene = c("TRBJ1", "TRBJ1", "TRBJ1", "TRBJ2"),
    cdr3 = c("CAAA", "CAAA", "CBBB", "CBBB"),
    stringsAsFactors = FALSE
  )
  seg$clone_vjc <- paste(seg$v_gene, seg$j_gene, seg$cdr3, sep = ";")
  out <- ir_definition_counts(seg, group = NULL)
  expect_equal(
    as.character(out$definition),
    c("cells", "V", "J", "V+J", "CDR3", "V+CDR3", "V+J+CDR3")
  )
  # 4 cells; V: {TRBV1,TRBV2}=2; J: {TRBJ1,TRBJ2}=2; V+J: {V1J1,V2J1,V2J2}=3;
  # CDR3: {CAAA,CBBB}=2; V+CDR3: {V1CAAA,V2CBBB}=2; V+J+CDR3: 3 distinct
  expect_equal(out$n[out$definition == "cells"], 4)
  expect_equal(out$n[out$definition == "V"], 2)
  expect_equal(out$n[out$definition == "J"], 2)
  expect_equal(out$n[out$definition == "V+J"], 3)
  expect_equal(out$n[out$definition == "CDR3"], 2)
  expect_equal(out$n[out$definition == "V+CDR3"], 2)
  expect_equal(out$n[out$definition == "V+J+CDR3"], 3)
  expect_true(is.ordered(out$definition))
})

test_that("ir_definition_counts splits by group when given", {
  seg <- data.frame(
    barcode = paste0("bc", 1:4),
    v_gene = c("TRBV1", "TRBV1", "TRBV2", "TRBV2"),
    j_gene = c("TRBJ1", "TRBJ1", "TRBJ1", "TRBJ1"),
    cdr3 = c("CAAA", "CAAA", "CBBB", "CBBB"),
    grp = c("A", "A", "B", "B"),
    stringsAsFactors = FALSE
  )
  seg$clone_vjc <- paste(seg$v_gene, seg$j_gene, seg$cdr3, sep = ";")
  out <- ir_definition_counts(seg, group = "grp")
  expect_true("grp" %in% colnames(out))
  expect_setequal(unique(out$grp), c("A", "B"))
  expect_equal(out$n[out$grp == "A" & out$definition == "cells"], 2)
})

test_that("ir_definition_counts returns NULL on empty input", {
  expect_null(ir_definition_counts(NULL, group = NULL))
  expect_null(ir_definition_counts(
    data.frame(
      v_gene = character(0),
      j_gene = character(0),
      cdr3 = character(0),
      clone_vjc = character(0)
    ),
    group = NULL
  ))
})

# --- ir_sharing_classify ---------------------------------------------------

test_that("ir_sharing_classify assigns Private / within / cross correctly", {
  # clone P: 1 unit only            -> Private
  # clone W: 2 units, same group    -> Public (within-group)
  # clone X: 2 units, 2 groups      -> Public (cross-group)
  seg <- data.frame(
    clone_vjc = c("P", "W", "W", "X", "X"),
    unit = c("s1", "s1", "s2", "s1", "s3"),
    grp = c("A", "A", "A", "A", "B"),
    stringsAsFactors = FALSE
  )
  out <- ir_sharing_classify(seg, unit_col = "unit", group_col = "grp")
  cls <- setNames(out$sharing, out$clone_vjc)
  expect_equal(as.character(cls[["P"]]), "Private")
  expect_equal(as.character(cls[["W"]]), "Public (within-group)")
  expect_equal(as.character(cls[["X"]]), "Public (cross-group)")
})

test_that("ir_sharing_classify degrades to two classes without a group", {
  seg <- data.frame(
    clone_vjc = c("P", "S", "S"),
    unit = c("s1", "s1", "s2"),
    stringsAsFactors = FALSE
  )
  out <- ir_sharing_classify(seg, unit_col = "unit", group_col = NULL)
  cls <- setNames(as.character(out$sharing), out$clone_vjc)
  expect_equal(cls[["P"]], "Private")
  expect_equal(cls[["S"]], "Public")
  expect_false(any(grepl("group", out$sharing)))
})

test_that("ir_sharing_classify returns NULL on empty input or missing unit col", {
  expect_null(ir_sharing_classify(NULL, unit_col = "unit", group_col = NULL))
  expect_null(ir_sharing_classify(
    data.frame(clone_vjc = character(0), unit = character(0)),
    unit_col = "unit",
    group_col = NULL
  ))
  expect_null(ir_sharing_classify(
    data.frame(clone_vjc = "P", other = "x"),
    unit_col = "unit",
    group_col = NULL
  ))
})

test_that("ir_sharing_classify sharing factor levels match the mode", {
  seg3 <- data.frame(
    clone_vjc = c("A", "A"),
    unit = c("s1", "s2"),
    grp = c("g1", "g2"),
    stringsAsFactors = FALSE
  )
  out3 <- ir_sharing_classify(seg3, unit_col = "unit", group_col = "grp")
  expect_equal(
    levels(out3$sharing),
    c("Private", "Public (within-group)", "Public (cross-group)")
  )
  seg2 <- data.frame(
    clone_vjc = c("A", "A"),
    unit = c("s1", "s2"),
    stringsAsFactors = FALSE
  )
  out2 <- ir_sharing_classify(seg2, unit_col = "unit", group_col = NULL)
  expect_equal(levels(out2$sharing), c("Private", "Public"))
})

test_that("ir_sharing_classify ignores NA unit/group values when counting", {
  # clone P: 1 real unit + 1 NA-unit row -> still Private (NA must not count)
  # clone Q: 2 units in 1 real group + 1 NA-group row -> within-group, not cross
  seg <- data.frame(
    clone_vjc = c("P", "P", "Q", "Q", "Q"),
    unit = c("s1", NA, "s1", "s2", "s2"),
    grp = c("A", "A", "A", "A", NA),
    stringsAsFactors = FALSE
  )
  out <- ir_sharing_classify(seg, unit_col = "unit", group_col = "grp")
  cls <- setNames(as.character(out$sharing), out$clone_vjc)
  expect_equal(cls[["P"]], "Private")
  expect_equal(cls[["Q"]], "Public (within-group)")
})

# --- ir_build_definition_plot ----------------------------------------------

test_that("ir_build_definition_plot returns a ggplot for valid TCR data", {
  data <- list(
    s1 = data.frame(
      barcode = c("b1", "b2", "b3"),
      CTgene = c(
        "TRBV6-2..TRBJ2-6.TRBC2",
        "TRBV6-2..TRBJ2-6.TRBC2",
        "TRBV14..TRBJ2-3.TRBC2"
      ),
      CTaa = c("CASSA", "CASSA", "CASSB"),
      sample = c("s1", "s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  p <- ir_build_definition_plot(data, chain = "TRB", group_by = NULL)
  expect_s3_class(p, "ggplot")
})

test_that("ir_build_definition_plot returns NULL when no cells for the chain", {
  data <- list(
    s1 = data.frame(
      barcode = "b1",
      CTgene = "TRAV8-6.TRAJ8.TRAC_NA",
      CTaa = "CAVSA_NA",
      stringsAsFactors = FALSE
    )
  )
  expect_null(ir_build_definition_plot(data, chain = "TRB", group_by = NULL))
})

test_that("ir_build_definition_plot adds a BCR caveat to the subtitle for IGH", {
  data <- list(
    s1 = data.frame(
      barcode = "b1",
      CTgene = "IGHV4-34..IGHJ6.IGHG1",
      CTaa = "CARDA",
      stringsAsFactors = FALSE
    )
  )
  p <- ir_build_definition_plot(data, chain = "IGH", group_by = NULL)
  expect_s3_class(p, "ggplot")
  expect_true(grepl("SHM", p$labels$subtitle %||% ""))
})

test_that("ir_is_bcr_chain is TRUE for BCR chains, FALSE otherwise and safe on NA/NULL", {
  expect_true(ir_is_bcr_chain("IGH"))
  expect_true(ir_is_bcr_chain("IGK"))
  expect_false(ir_is_bcr_chain("TRB"))
  expect_false(ir_is_bcr_chain(NA_character_))
  expect_false(ir_is_bcr_chain(NULL))
  expect_false(ir_is_bcr_chain(""))
})

# --- ir_build_sharing_plot -------------------------------------------------

test_that("ir_build_sharing_plot returns a ggplot and maps display labels", {
  data <- list(
    s1 = data.frame(
      barcode = c("b1", "b2"),
      CTgene = c("TRBV6-2..TRBJ2-6.TRBC2", "TRBV14..TRBJ2-3.TRBC2"),
      CTaa = c("CASSA", "CASSB"),
      sample = c("s1", "s1"),
      stringsAsFactors = FALSE
    )
  )
  p <- ir_build_sharing_plot(
    data,
    chain = "TRB",
    unit_col = "sample",
    group_by = NULL
  )
  expect_s3_class(p, "ggplot")
  # Friendly display labels appear on the x scale, not the raw factor labels.
  built <- ggplot2::ggplot_build(p)
  x_labels <- built$layout$panel_params[[1]]$x$get_labels()
  expect_true(any(grepl("1 sample", x_labels)))
  expect_false(any(grepl("Public \\(within", x_labels)))
})

test_that("ir_build_sharing_plot returns NULL when the unit column is absent", {
  data <- list(
    s1 = data.frame(
      barcode = "b1",
      CTgene = "TRBV6-2..TRBJ2-6.TRBC2",
      CTaa = "CASSA",
      sample = "s1",
      stringsAsFactors = FALSE
    )
  )
  expect_null(ir_build_sharing_plot(
    data,
    chain = "TRB",
    unit_col = "nonexistent_col",
    group_by = NULL
  ))
})

test_that("ir_build_sharing_plot adds a BCR caveat to the subtitle for IGH", {
  data <- list(
    s1 = data.frame(
      barcode = "b1",
      CTgene = "IGHV4-34..IGHJ6.IGHG1",
      CTaa = "CARDA",
      sample = "s1",
      stringsAsFactors = FALSE
    )
  )
  p <- ir_build_sharing_plot(
    data,
    chain = "IGH",
    unit_col = "sample",
    group_by = NULL
  )
  expect_s3_class(p, "ggplot")
  expect_true(grepl("SHM", p$labels$subtitle %||% ""))
})

test_that("ir_build_sharing_plot keeps 3-class x order Private -> within -> across", {
  # clone P: 1 sample (s1/A)              -> Private
  # clone W: s1 + s2, both group A        -> Shared within group
  # clone X: s1 (A) + s3 (B)              -> Shared across groups
  mk <- function(barcode, v, sample, grp) {
    data.frame(
      barcode = barcode,
      CTgene = sprintf("%s..TRBJ2-6.TRBC2", v),
      CTaa = "CASSX",
      sample = sample,
      grp = grp,
      stringsAsFactors = FALSE
    )
  }
  data <- list(
    all = do.call(
      rbind,
      list(
        mk("b1", "TRBV1", "s1", "A"), # P
        mk("b2", "TRBV2", "s1", "A"), # W in s1
        mk("b3", "TRBV2", "s2", "A"), # W in s2 -> within
        mk("b4", "TRBV3", "s1", "A"), # X in s1
        mk("b5", "TRBV3", "s3", "B") # X in s3 -> across
      )
    )
  )
  p <- ir_build_sharing_plot(
    data,
    chain = "TRB",
    unit_col = "sample",
    group_by = "grp"
  )
  built <- ggplot2::ggplot_build(p)
  x_labels <- built$layout$panel_params[[1]]$x$get_labels()
  expect_equal(
    x_labels,
    c("Private (1 sample)", "Shared within group", "Shared across groups")
  )
})


test_that("clonal expansion is independent of display filtering and pack presence", {
  scope <- new.env(parent = ir_env)
  # Rebind the actual helper to an isolated fixture, without changing other tests.
  f <- ir_env$ir_clonal_umap_data
  environment(f) <- scope
  cells <- paste0("c", 1:6)
  coords <- data.frame(x = 1:6, y = 6:1, row.names = cells)
  scope$availableProjections <- function() "umap"
  scope$getProjection <- function(...) coords
  scope$ir_data_annotated <- function() {
    list(
      sample = data.frame(
        barcode = cells,
        CTgene = rep("TRB-clone", 6)
      )
    )
  }
  scope$ir_clonecall_col <- function(...) "CTgene"
  scope$ir_umap_chains <- function(...) "TRB"
  packed <- NULL
  scope$viewerPackCurrent <- function() list(cells = cells)
  scope$viewerPackImmuneIndex <- function(...) packed
  plain <- f("umap", "TCR", cells = "c1", show_all = FALSE)
  expect_identical(as.character(plain$expansion), "Medium (5 < X <= 20)")
  packed <- list(
    cell_index = 1:6,
    clone = rep("TRB-clone", 6),
    expansion = as.integer(ir_env$ir_clone_expansion(rep("TRB-clone", 6)))
  )
  cached <- f("umap", "TCR", cells = "c1", show_all = FALSE)
  expect_identical(cached, plain)
  scope$viewerPackCurrent <- function() {
    list(cells = cells, canonical_order = TRUE)
  }
  scope$viewerProjectionFirstFrameCoordinates <- function(...) coords
  scope$viewerPackCellBarcodes <- function(...) cells
  indexed <- f("umap", "TCR", cells = "c1", show_all = FALSE)
  expect_identical(indexed$expansion, plain$expansion)
  expect_equal(indexed$x, plain$x)
})


test_that("sharing choices discover metadata fields before any field is selected", {
  data <- list(
    a = data.frame(barcode = c("c3", "c1"), CTgene = "TRB-a"),
    b = data.frame(barcode = "c2", CTgene = "TRB-a")
  )
  metadata <- data.frame(
    cell_barcode = c("c1", "c2", "c3", "unmatched"),
    patient = c("p1", "p2", "p1", "p3"),
    condition = c("case", "control", "case", "other"),
    constant = c("x", "x", "x", "different"),
    score = 1:4
  )
  choices <- ir_env$ir_discover_sharing_units(data, metadata)
  expect_identical(choices, c("sample", "patient", "condition"))
  expect_named(data$a, c("barcode", "CTgene"))
  joined <- ir_env$ir_annotate_metadata(data, metadata, "patient")
  expect_identical(joined$a$patient, c("p1", "p1"))
  expect_identical(joined$b$patient, "p2")
  expect_false("condition" %in% names(joined$a))
  expect_identical(ir_env$ir_discover_sharing_units(joined, metadata), choices)
  data$a$native <- c("n1", "n2")
  data$b$native <- "n3"
  expect_true("native" %in% ir_env$ir_discover_sharing_units(data, metadata))
})


test_that("packed and fallback clone sizes use the same unique dataset cells", {
  scope <- new.env(parent = ir_env)
  f <- ir_env$ir_clonal_umap_data
  environment(f) <- scope
  cells <- paste0("c", 1:5)
  data <- list(
    sample = data.frame(
      barcode = c(cells, "c1", "outside"),
      CTgene = rep("TRB-clone", 7)
    )
  )
  scope$availableProjections <- function() "umap"
  scope$getProjection <- function(...) {
    data.frame(x = 1:5, y = 5:1, row.names = cells)
  }
  scope$ir_data_annotated <- function() data
  scope$ir_clonecall_col <- function(...) "CTgene"
  scope$ir_umap_chains <- function(...) "TRB"
  packed <- NULL
  scope$viewerPackCurrent <- function() list(cells = cells)
  scope$viewerPackImmuneIndex <- function(...) packed
  plain <- f("umap", "TCR", cells = "c1", show_all = FALSE)
  packed <- CerebroNexus:::.viewerPackImmuneIndex(data, cells, "TCR")
  cached <- f("umap", "TCR", cells = "c1", show_all = FALSE)
  expect_identical(cached, plain)
  expect_identical(as.character(plain$expansion), "Small (1 < X <= 5)")
})
