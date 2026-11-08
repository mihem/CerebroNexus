# test-ir-definition-sharing.R — pure-function tests for the Clone Sharing tab
# and other IR data-layer helpers. These test the helpers directly (no Shiny
# reactive context needed), so they source data.R's function definitions.
#
# data.R is a Shiny module fragment: its reactives reference `input`/`session`,
# but the pure helpers below (ir_sharing_classes / ir_build_sharing_plot / ...)
# are plain functions. We source data.R inside a throwaway
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
sys.source(
  file.path(dirname(data_r), "encode.R"),
  envir = ir_env,
  keep.source = FALSE
)
sys.source(data_r, envir = ir_env, keep.source = FALSE)
ir_encode <- ir_env$ir_encode_repertoire

ir_is_bcr_chain <- ir_env$ir_is_bcr_chain
ir_build_sharing_plot <- ir_env$ir_build_sharing_plot
ir_clone_size_profiles <- ir_env$ir_clone_size_profiles
ir_size_profile_plot <- ir_env$ir_size_profile_plot

test_that("empirical clone-size profiles preserve exact observed counts", {
  frame <- data.frame(
    CTgene = rep("TRBV1..TRBJ1.TRBC1", 8),
    CTstrict = c(rep("clone-a", 4), rep("clone-b", 2), "clone-c", "clone-d"),
    cohort = c(rep("A", 6), "B", "B"),
    stringsAsFactors = FALSE
  )
  encoded <- ir_encode(list(sample_1 = frame))
  profiles <- ir_clone_size_profiles(
    encoded,
    chain = "both",
    groups = ir_env$ir_row_values(encoded, "cohort", NULL),
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
  scope <- new.env(parent = ir_env)
  f <- ir_env$ir_clonal_umap_data
  environment(f) <- scope
  cells <- paste0("c", seq_len(1000L))
  coords <- matrix(
    seq_len(2000L),
    ncol = 2L,
    dimnames = list(cells, c("x", "y"))
  )
  scope$availableProjections <- function() "tsne"
  scope$viewerProjectionFirstFrameCoordinates <- function(...) coords
  encoded <- ir_encode(
    list(
      sample = data.frame(
        barcode = cells[1:3],
        CTgene = c("TRB-a", "TRB-a", "TRB-b"),
        stringsAsFactors = FALSE
      )
    ),
    cells
  )
  scope$ir_encoded <- function() encoded

  out <- f("tsne", "TCR", show_all = TRUE, max_background = 10L)

  expect_lte(nrow(out), 13L)
  expect_setequal(out$cell_index[!is.na(out$expansion)], 1:3)

  sampled <- f(
    "tsne",
    "TCR",
    show_all = TRUE,
    percentage = 34,
    max_background = 10L
  )
  expect_false(any(1:3 %in% sampled$cell_index[is.na(sampled$expansion)]))

  grouped <- f(
    "tsne",
    "TCR",
    show_all = TRUE,
    max_background = 10L,
    split_output = TRUE
  )
  expect_identical(grouped$trace_codes, c(NA_integer_, 1L, 2L))
  expect_identical(lengths(grouped$x), c(10L, 1L, 2L))
  expect_identical(lengths(grouped$y), c(10L, 1L, 2L))
  expect_identical(grouped$key_type, "cell_index")
  grouped_coloured_keys <- unlist(lapply(
    grouped$coloured_rows,
    function(rows) grouped$coloured_key[rows]
  ))
  expect_setequal(grouped_coloured_keys, 1:3)
  expect_false(any(grouped$background_key %in% 1:3))
  ## The BCR has no cell here: only the background is drawn.
  expect_null(f("tsne", "BCR", show_all = FALSE))
})

test_that("default abundance reduces cells to clone-count bins", {
  data <- list(
    sample_a = data.frame(CTgene = c("a", "a", "b", "c", NA)),
    sample_b = data.frame(CTgene = c("x", "x", "x", "y"))
  )

  counts <- ir_env$ir_abundance_histogram(ir_encode(data), "CTgene")

  expect_identical(
    counts$sample,
    c("sample_a", "sample_a", "sample_b", "sample_b")
  )
  expect_identical(counts$abundance, c(1L, 2L, 1L, 3L))
  expect_identical(counts$n_clones, c(2L, 1L, 1L, 1L))
})

## One row per (clone, unit[, group]); the classes of the clones in order.
classes <- function(seg, group = TRUE) {
  clone <- match(seg$clone_vjc, unique(seg$clone_vjc))
  out <- ir_env$ir_sharing_classes(
    clone,
    max(clone),
    seg$unit,
    if (group) seg$grp
  )
  setNames(as.character(out), unique(seg$clone_vjc))
}

test_that("ir_sharing_classes assigns Private / within / cross correctly", {
  # clone P: 1 unit only            -> Private
  # clone W: 2 units, same group    -> Public (within-group)
  # clone X: 2 units, 2 groups      -> Public (cross-group)
  seg <- data.frame(
    clone_vjc = c("P", "W", "W", "X", "X"),
    unit = c("s1", "s1", "s2", "s1", "s3"),
    grp = c("A", "A", "A", "A", "B"),
    stringsAsFactors = FALSE
  )
  cls <- classes(seg)
  expect_equal(cls[["P"]], "Private")
  expect_equal(cls[["W"]], "Public (within-group)")
  expect_equal(cls[["X"]], "Public (cross-group)")
})

test_that("ir_sharing_classes degrades to two classes without a group", {
  seg <- data.frame(
    clone_vjc = c("P", "S", "S"),
    unit = c("s1", "s1", "s2"),
    stringsAsFactors = FALSE
  )
  cls <- classes(seg, group = FALSE)
  expect_equal(cls[["P"]], "Private")
  expect_equal(cls[["S"]], "Public")
  expect_false(any(grepl("group", cls)))
})

test_that("ir_sharing_classes sharing factor levels match the mode", {
  seg3 <- data.frame(
    clone_vjc = c("A", "A"),
    unit = c("s1", "s2"),
    grp = c("g1", "g2"),
    stringsAsFactors = FALSE
  )
  out3 <- ir_env$ir_sharing_classes(c(1L, 1L), 1L, seg3$unit, seg3$grp)
  expect_equal(
    levels(out3),
    c("Private", "Public (within-group)", "Public (cross-group)")
  )
  seg2 <- data.frame(
    clone_vjc = c("A", "A"),
    unit = c("s1", "s2"),
    stringsAsFactors = FALSE
  )
  out2 <- ir_env$ir_sharing_classes(c(1L, 1L), 1L, seg2$unit)
  expect_equal(levels(out2), c("Private", "Public"))
})

test_that("ir_sharing_classes ignores NA unit/group values when counting", {
  # clone P: 1 real unit + 1 NA-unit row -> still Private (NA must not count)
  # clone Q: 2 units in 1 real group + 1 NA-group row -> within-group, not cross
  seg <- data.frame(
    clone_vjc = c("P", "P", "Q", "Q", "Q"),
    unit = c("s1", NA, "s1", "s2", "s2"),
    grp = c("A", "A", "A", "A", NA),
    stringsAsFactors = FALSE
  )
  cls <- classes(seg)
  expect_equal(cls[["P"]], "Private")
  expect_equal(cls[["Q"]], "Public (within-group)")
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
    ir_encode(data),
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
    ir_encode(data),
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
    ir_encode(data),
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
    ir_encode(data),
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


test_that("the Clone Sharing bar labels name the chosen sharing unit", {
  expect_identical(
    unname(ir_env$ir_sharing_display_labels("sample")[c("Private", "Public")]),
    c("Private (1 sample)", "Shared (≥ 2 samples)")
  )
  expect_identical(
    unname(
      ir_env$ir_sharing_display_labels("tissue_site")[c("Private", "Public")]
    ),
    c("Private (1 tissue site)", "Shared (≥ 2 tissue sites)")
  )
  ## A missing unit falls back to the sample wording.
  expect_identical(
    unname(ir_env$ir_sharing_display_labels(NULL)["Private"]),
    "Private (1 sample)"
  )
  ## A plural column name is singular for one value; a singular one ending
  ## in "s" still gets its plural.
  expect_identical(
    unname(ir_env$ir_sharing_display_labels("donors")[c("Private", "Public")]),
    c("Private (1 donor)", "Shared (≥ 2 donors)")
  )
  expect_identical(
    unname(ir_env$ir_sharing_display_labels("status")[c("Private", "Public")]),
    c("Private (1 status)", "Shared (≥ 2 statuses)")
  )
  expect_identical(
    unname(ir_env$ir_sharing_display_labels("batches")[c("Private", "Public")]),
    c("Private (1 batch)", "Shared (≥ 2 batches)")
  )

  mk <- function(barcode, v, sample, site) {
    data.frame(
      barcode = barcode,
      CTgene = sprintf("%s..TRBJ2-6.TRBC2", v),
      CTaa = "CASSX",
      sample = sample,
      tissue_site = site,
      stringsAsFactors = FALSE
    )
  }
  data <- list(
    all = do.call(
      rbind,
      list(
        mk("b1", "TRBV1", "s1", "tumour"), # private
        mk("b2", "TRBV2", "s1", "tumour"), # in two tissue sites
        mk("b3", "TRBV2", "s2", "blood")
      )
    )
  )
  p <- ir_env$ir_build_sharing_plot(
    ir_encode(data),
    chain = "TRB",
    unit_col = "tissue_site"
  )
  x_labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
  expect_equal(
    x_labels,
    c("Private (1 tissue site)", "Shared (≥ 2 tissue sites)")
  )
})


test_that("clonal expansion is independent of display filtering", {
  scope <- new.env(parent = ir_env)
  f <- ir_env$ir_clonal_umap_data
  environment(f) <- scope
  cells <- paste0("c", 1:6)
  coords <- data.frame(x = 1:6, y = 6:1, row.names = cells)
  scope$availableProjections <- function() "umap"
  scope$viewerProjectionFirstFrameCoordinates <- function(...) coords
  encoded <- ir_encode(
    list(sample = data.frame(barcode = cells, CTgene = rep("TRB-clone", 6))),
    cells
  )
  scope$ir_encoded <- function() encoded
  ## Display filters name cells by canonical (metadata) row.
  plain <- f("umap", "TCR", cells = 1L, show_all = FALSE)
  expect_identical(as.character(plain$expansion), "Medium (5 < X <= 20)")
  expect_identical(plain$cell_index, 1L)
  expect_equal(plain$x, 1)
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
  encoded <- ir_encode(data, metadata$cell_barcode)
  choices <- ir_env$ir_discover_sharing_units(encoded, metadata)
  expect_identical(choices, c("sample", "patient", "condition"))
  expect_identical(
    ir_env$ir_row_values(encoded, "patient", metadata),
    c("p1", "p1", "p2")
  )
  data$a$native <- c("n1", "n2")
  data$b$native <- "n3"
  encoded <- ir_encode(data, metadata$cell_barcode)
  expect_true("native" %in% ir_env$ir_discover_sharing_units(encoded, metadata))
})

test_that("clone sizes count each data set cell once", {
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
  scope$viewerProjectionFirstFrameCoordinates <- function(...) {
    data.frame(x = 1:5, y = 5:1, row.names = cells)
  }
  encoded <- ir_encode(data, cells)
  scope$ir_encoded <- function() encoded
  plain <- f("umap", "TCR", cells = 1L, show_all = FALSE)
  expect_identical(as.character(plain$expansion), "Small (1 < X <= 5)")
})
