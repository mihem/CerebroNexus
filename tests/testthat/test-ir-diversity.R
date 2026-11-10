ir_diversity_scope <- function() {
  ir_scope(c(
    "ir_row_values",
    "ir_samples_with_clones",
    "ir_diversity_metrics",
    "ir_diversity_table"
  ))
}

## Skewed clones per sample, with cells on another chain (TRG first), cells
## without a clone on one chain, NA and blank clones, a sample whose clones are
## all NA, an empty sample, a BCR sample, and metadata with NA and nested
## levels.
ir_diversity_fixture <- function() {
  set.seed(7)
  sample_frame <- function(n, prefix, patient, condition) {
    alpha <- paste0(prefix, "A", sample(1:12, n, TRUE, prob = 12:1))
    beta <- paste0("B", sample(1:8, n, TRUE))
    gene_a <- sample(c("TRAV1", "TRAV2", "TRGV9", "NA"), n, TRUE, c(5, 4, 1, 1))
    gene_b <- sample(c("TRBV1", "TRBV2", "NA"), n, TRUE, c(5, 4, 1))
    aa <- paste0(
      ifelse(gene_a == "NA", "NA", alpha),
      "_",
      ifelse(gene_b == "NA", "NA", beta)
    )
    aa[sample(n, 2)] <- NA
    aa[sample(n, 1)] <- ""
    data.frame(
      barcode = paste0(prefix, seq_len(n)),
      CTgene = paste0(gene_a, "_", gene_b),
      CTaa = aa,
      CTstrict = aa,
      patient = patient,
      condition = sample(c(condition, NA), n, TRUE, c(20, 1)),
      cluster = sample(c("c1", "c2", "c3"), n, TRUE),
      stringsAsFactors = FALSE
    )
  }
  data <- list(
    s1 = sample_frame(60, "a", "p1", "mild"),
    s2 = sample_frame(45, "b", "p1", "mild"),
    s3 = sample_frame(80, "c", "p2", "severe"),
    s4 = sample_frame(30, "d", "p3", "severe"),
    allna = sample_frame(5, "e", "p3", "severe"),
    empty = sample_frame(5, "f", "p4", "mild")[0, ],
    bcr = sample_frame(20, "g", "p3", "severe")
  )
  data$allna$CTaa <- NA_character_
  data$bcr$CTgene <- "IGHV1_IGKV1"
  data
}

test_that("Diversity bootstraps equal clonalDiversity for the same seed", {
  ## Slow.
  skip_on_cran()
  skip_if_not_installed("scRepertoire")
  scope <- ir_diversity_scope()
  data <- ir_diversity_fixture()
  ## clonalDiversity() with `group.by` and `x.axis` keeps their empty
  ## combinations, so every value is computed from no cells. The app leaves
  ## them out: for a group nested in the x levels (and the x-axis grouping
  ## itself) its values are clonalDiversity()'s by the pasted combination,
  ## whose sorted levels are in the same order here. clonalDiversity() stops
  ## at a sample without a cell on the chain; the app leaves it out.
  data <- lapply(data, function(frame) {
    frame$patient_condition <- ifelse(
      is.na(frame$condition),
      NA,
      paste0(frame$patient, ".", frame$condition)
    )
    frame$cluster_cluster <- paste(frame$cluster, frame$cluster, sep = ".")
    frame
  })
  cases <- list(
    list(group_by = NULL, x_axis = NULL),
    list(group_by = "cluster", x_axis = NULL),
    list(group_by = NULL, x_axis = "condition"),
    list(group_by = "cluster", x_axis = "condition"),
    list(group_by = "patient", x_axis = "condition", by = "patient_condition"),
    list(group_by = "cluster", x_axis = "cluster", by = "cluster_cluster")
  )
  metrics <- names(scope$ir_diversity_metrics)
  encoded <- scope$ir_encode_repertoire(data)
  values <- function(name) scope$ir_row_values(encoded, name, NULL)
  rows <- tabulate(encoded$sample, length(data))
  for (chain in c("both", "TRA", "TRB")) {
    ## Samples with a row on the chain.
    other <- tabulate(encoded$sample[encoded$other[[chain]]], length(data))
    for (case in cases) {
      for (metric in metrics) {
        args <- list(
          data[rows > other],
          cloneCall = "aa",
          chain = chain,
          group.by = case$by %||% case$group_by,
          metric = metric,
          n.boots = 7,
          return.boots = TRUE,
          exportTable = TRUE
        )
        if (is.null(case$by)) {
          args$x.axis <- case$x_axis
        }
        set.seed(42)
        expected <- suppressMessages(suppressWarnings(
          do.call(scRepertoire::clonalDiversity, args)
        ))
        next_draw <- runif(1)
        set.seed(42)
        actual <- scope$ir_diversity_table(
          encoded,
          "CTaa",
          chain,
          case$group_by,
          case$x_axis,
          metric,
          7,
          values(case$group_by),
          values(case$x_axis)
        )
        label <- paste(chain, metric, case$group_by, case$x_axis)
        expect_equal(
          as.numeric(actual$value),
          as.numeric(expected$value),
          tolerance = 1e-12,
          label = paste(label, "value")
        )
        # The same draws leave the generator in the same state.
        expect_identical(runif(1), next_draw, label = label)
        if (is.null(case$by)) {
          expect_identical(names(actual), names(expected), label = label)
          for (column in setdiff(names(expected), "value")) {
            expect_identical(
              as.character(actual[[column]]),
              as.character(expected[[column]]),
              label = paste(label, column)
            )
          }
        } else {
          unit <- strsplit(expected[[case$by]], ".", fixed = TRUE)
          expect_identical(
            actual[[case$group_by]],
            vapply(unit, `[`, "", 1L),
            label = label
          )
          expect_identical(
            actual[[case$x_axis]],
            vapply(unit, `[`, "", 2L),
            label = label
          )
        }
      }
    }
  }
})

test_that("Diversity rejects an unknown metric", {
  scope <- ir_diversity_scope()
  expect_error(
    scope$ir_diversity_table(
      scope$ir_encode_repertoire(ir_diversity_fixture()),
      "CTaa",
      "both",
      metric = "x"
    ),
    "Invalid"
  )
})

test_that("Diversity follows the Order groups control", {
  scope <- ir_diversity_scope()
  scope$ir_clonecall_col <- function(clone_call) "CTaa"
  scope$IR_PALETTE <- "Harmonic"
  ir_scope(
    c("ir_plot_clonal_diversity", "ir_abundance_units"),
    "visualizations.R",
    scope
  )
  scope$ir_diversity_plotly <- function(df, x, ...) levels(df[[x]])
  data <- ir_diversity_fixture()[c("s3", "s1", "s4", "s2")]
  names(data) <- c("s10", "s2", "s1", "s9")
  levels_for <- function(order_by) {
    set.seed(1)
    scope$ir_plot_clonal_diversity(
      scope$ir_encode_repertoire(data),
      "aa",
      "both",
      group_by = NULL,
      metric = "shannon",
      x_axis = NULL,
      n_boots = 2,
      plotly = TRUE,
      order_by = order_by
    )
  }
  expect_equal(levels_for("alphanumeric"), c("s1", "s2", "s9", "s10"))
  expect_false(identical(levels_for(NULL), c("s1", "s2", "s9", "s10")))
})
