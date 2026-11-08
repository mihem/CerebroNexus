ir_fast_scope <- function() {
  scope <- ir_scope(
    c(
      "ir_row_values",
      "ir_samples_with_clones",
      "ir_compare_table",
      "ir_clone_size_profiles",
      "ir_sharing_classes",
      "IR_SHARING_LEVELS_3",
      "IR_SHARING_LEVELS_2",
      "ir_sharing_unit_noun",
      "ir_sharing_display_labels",
      "IR_BCR_SHM_CAVEAT",
      "IR_BCR_CHAINS",
      "ir_is_bcr_chain",
      "ir_build_sharing_plot"
    ),
    sources = "clone_contract.R"
  )
  scope$cerebro_group_colors <- function(n) rep("#000000", n)
  scope
}

## The values of column `name` of the encoded rows, without cell metadata.
ir_fast_values <- function(scope, encoded, name) {
  scope$ir_row_values(encoded, name, NULL)
}

ir_fast_fixture <- function() {
  list(
    s1 = data.frame(
      barcode = paste0("a", 1:8),
      CTgene = c(
        "TRAV1.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1",
        "TRAV1.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1",
        "TRAV3.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1",
        "TRAV3.TRAJ1.TRAC_NA",
        "NA_TRBV9.TRBJ1.TRBC1",
        "IGHV1.IGHJ1.IGHM_IGKV2.IGKJ1.IGKC",
        "TRAV5.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1;TRBV4.TRBJ1.TRBC1",
        "TRAV1.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1"
      ),
      CTaa = c(
        "CAA_CBB",
        "CAA_CBB",
        "CAC_CBB",
        "CAC_NA",
        "NA_CDD",
        "CHH_CKK",
        "CAE_CBB;CBF",
        "CAA_CBB"
      ),
      CTstrict = c(
        "TRAV1;CAA_TRBV2;CBB",
        "TRAV1;CAA_TRBV2;CBB",
        "TRAV3;CAC_TRBV2;CBB",
        "TRAV3;CAC_NA",
        "NA_TRBV9;CDD",
        "IGHV1;CHH_IGKV2;CKK",
        "TRAV5;CAE_TRBV2;CBB",
        "TRAV1;CAA_TRBV2;CBB"
      ),
      group = c("x", "y", "x", "y", "x", "x", NA, "y"),
      patient = "p1",
      stringsAsFactors = FALSE
    ),
    s2 = data.frame(
      barcode = paste0("b", 1:5),
      CTgene = c(
        "TRAV1.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1",
        "TRAV1.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1",
        "TRAV1.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1",
        "TRAV7.TRAJ1.TRAC_TRBV8.TRBJ1.TRBC1",
        "TRAV3.TRAJ1.TRAC_TRBV2.TRBJ1.TRBC1"
      ),
      CTaa = c("CAA_CBB", "CAA_CBB", "CAA_CBB", "CAG_CBH", "CAC_CBB"),
      CTstrict = c(
        "TRAV1;CAA_TRBV2;CBB",
        "TRAV1;CAA_TRBV2;CBB",
        "TRAV1;CAA_TRBV2;CBB",
        "TRAV7;CAG_TRBV8;CBH",
        "TRAV3;CAC_TRBV2;CBB"
      ),
      group = c("x", "x", "y", "y", "z"),
      patient = c("p1", "p1", "p2", "p2", "p2"),
      stringsAsFactors = FALSE
    ),
    s3 = data.frame(
      barcode = "c1",
      CTgene = "TRAV9.TRAJ1.TRAC_TRBV9.TRBJ1.TRBC1",
      CTaa = "CAZ_CBZ",
      CTstrict = "TRAV9;CAZ_TRBV9;CBZ",
      group = "z",
      stringsAsFactors = FALSE
    )
  )
}

test_that("the Compare table matches clonalCompare's exported table", {
  skip_if_not_installed("scRepertoire")
  scope <- ir_fast_scope()
  ## scRepertoire binds the samples, so they share their columns here.
  data <- lapply(ir_fast_fixture(), function(d) d[names(d) != "patient"])
  normalise <- function(table, value) {
    table <- data.frame(
      clones = as.character(table$clones),
      value = as.numeric(table[[value]]),
      Sample = as.character(table$Sample)
    )
    table[order(table$Sample, table$clones), , drop = FALSE]
  }
  for (chain in c("both", "TRA", "TRB")) {
    for (group_by in list(NULL, "group")) {
      units <- if (is.null(group_by)) c("s1", "s2") else c("x", "y")
      for (proportion in c(TRUE, FALSE)) {
        for (top in list(NULL, 1)) {
          value <- if (proportion) "Proportion" else "Count"
          expected <- suppressWarnings(scRepertoire::clonalCompare(
            data,
            cloneCall = "aa",
            chain = chain,
            group.by = group_by,
            samples = units,
            top.clones = top,
            proportion = proportion,
            exportTable = TRUE
          ))
          encoded <- scope$ir_encode_repertoire(data)
          actual <- scope$ir_compare_table(
            encoded,
            "CTaa",
            chain,
            ir_fast_values(scope, encoded, group_by),
            units,
            top_clones = if (is.null(top)) NULL else top,
            proportion = proportion,
            clone_rank = encoded$rank$CTaa[[chain]],
            clone_names = function(ids) encoded$names$CTaa[[chain]][ids]
          )
          expect_named(actual, c("clones", value, "Sample"))
          expect_equal(
            normalise(actual, value),
            normalise(expected, value),
            ignore_attr = TRUE,
            info = paste(chain, group_by, proportion, top)
          )
        }
      }
    }
  }
})

test_that("Clone Sharing counts each clonotype's class", {
  scope <- ir_fast_scope()
  encoded <- scope$ir_encode_repertoire(ir_fast_fixture())
  counts <- function(chain, unit, group = NULL) {
    data <- scope$ir_build_sharing_plot(encoded, chain, unit, group)$data
    setNames(data$Freq, as.character(data$sharing))
  }
  expect_identical(counts("TRB", "sample"), c(Private = 3L, Public = 1L))
  expect_identical(
    counts("TRB", "sample", "group"),
    c(
      Private = 3L,
      "Public (within-group)" = 0L,
      "Public (cross-group)" = 1L
    )
  )
  expect_identical(counts("TRA", "patient"), c(Private = 3L, Public = 2L))
  expect_identical(
    counts("TRA", "group", "patient"),
    c(
      Private = 3L,
      "Public (within-group)" = 0L,
      "Public (cross-group)" = 2L
    )
  )
})

test_that("SizeDist profiles count the strict clones of the chain", {
  scope <- ir_fast_scope()
  encoded <- scope$ir_encode_repertoire(ir_fast_fixture())
  profiles <- scope$ir_clone_size_profiles(
    encoded,
    "TRB",
    ir_fast_values(scope, encoded, "group")
  )
  expect_identical(rownames(profiles$profile), c("x", "y", "z"))
  expect_identical(
    lapply(profiles$clone_counts, sort),
    list(x = c(1L, 4L), y = c(1L, 3L), z = c(1L, 1L))
  )
})

ir_scatter_scope <- function() {
  scope <- ir_scope(
    c(
      "IR_PALETTE",
      "ir_scatter_table",
      "ir_scatter_plotly",
      "ir_diversity_plotly"
    ),
    file = "visualizations.R",
    scope = ir_fast_scope()
  )
  scope$`%>%` <- dplyr::`%>%`
  scope$cerebro_plotly_axis <- function(title = "", mirror = TRUE, ...) {
    c(list(title = title), list(...))
  }
  scope$cerebro_plotly_hoverlabel <- function() list()
  scope$cerebro_plotly_theme <- function() list(transparent = "rgba(0,0,0,0)")
  scope
}

test_that("the paired scatter table matches clonalScatter's exported table", {
  skip_if_not_installed("scRepertoire")
  scope <- ir_scatter_scope()
  data <- lapply(ir_fast_fixture(), function(d) d[names(d) != "patient"])
  cases <- list(
    list(group_by = NULL, x = "s1", y = "s2"),
    list(group_by = "group", x = "x", y = "y")
  )
  for (chain in c("both", "TRB")) {
    for (case in cases) {
      expected <- suppressWarnings(scRepertoire::clonalScatter(
        data,
        cloneCall = "aa",
        chain = chain,
        x.axis = case$x,
        y.axis = case$y,
        group.by = case$group_by,
        exportTable = TRUE
      ))
      encoded <- scope$ir_encode_repertoire(data)
      actual <- scope$ir_scatter_table(
        encoded,
        "CTaa",
        chain,
        ir_fast_values(scope, encoded, case$group_by),
        case$x,
        case$y,
        clone_names = function(ids) encoded$names$CTaa[[chain]][ids]
      )
      expected <- expected[order(as.character(expected$Var1)), ]
      actual <- actual[order(actual$clone), ]
      info <- paste(chain, case$x, case$y)
      expect_identical(actual$clone, as.character(expected$Var1), info = info)
      expect_equal(actual$x_n, expected[[case$x]], info = info)
      expect_equal(actual$y_n, expected[[case$y]], info = info)
      expect_identical(actual$class, expected$class, info = info)
      expect_equal(actual$size, expected$size, info = info)
      expect_equal(
        actual$x_fraction,
        expected[[paste0(case$x, ".fraction")]],
        info = info
      )
    }
  }
})

test_that("the paired scatter draws each spot once with its clone count", {
  scope <- ir_scatter_scope()
  table <- data.frame(
    clone = c("a", "b", "c", "d"),
    x_n = c(1, 1, 0, 3),
    y_n = c(0, 0, 2, 1),
    x_fraction = c(0.2, 0.2, 0, 0.6),
    y_fraction = c(0, 0, 2 / 3, 1 / 3),
    class = c("x.singlet", "x.singlet", "y.expanded", "dual.expanded"),
    size = c(1, 1, 2, 4)
  )
  traces <- scope$ir_scatter_plotly(table, "x", "y")$x$data
  expect_identical(
    vapply(traces, `[[`, "", "name"),
    c("dual.expanded", "x.singlet", "y.expanded")
  )
  singlet <- traces[[2L]]
  expect_equal(singlet$x, sqrt(0.2))
  expect_match(singlet$text, "Clones: 2", fixed = TRUE)
  counts <- scope$ir_scatter_plotly(table, "x", "y", "count")$x$data
  expect_equal(counts[[1L]]$x, 3)
})

test_that("the diversity boxes use geom_boxplot's quartiles and whiskers", {
  scope <- ir_scatter_scope()
  table <- data.frame(
    value = c(1, 2, 3, 4, 100, 5, 6, 7),
    x = factor(c("a", "a", "a", "a", "a", "b", "b", "b"), levels = c("a", "b")),
    Group = factor(c("g1", "g1", "g2", "g2", "g2", "g1", "g1", "g2"))
  )
  fills <- c(g1 = "#111111", g2 = "#222222")
  plot <- scope$ir_diversity_plotly(table, "x", "Group", "x", "Score", fills)
  box <- plot$x$data[[1L]]
  built <- ggplot2::layer_data(
    ggplot2::ggplot(table, ggplot2::aes(x, value)) + ggplot2::geom_boxplot()
  )
  expect_equal(unname(box$q1), built$lower)
  expect_equal(unname(box$median), built$middle)
  expect_equal(unname(box$q3), built$upper)
  expect_equal(unname(box$lowerfence), built$ymin)
  expect_equal(unname(box$upperfence), built$ymax)
  points <- plot$x$data[-1L]
  expect_identical(vapply(points, `[[`, "", "name"), c("g1", "g2"))
  expect_equal(sum(lengths(lapply(points, `[[`, "y"))), nrow(table))
})

test_that("Homeostasis on one chain matches clonalHomeostasis", {
  skip_if_not_installed("scRepertoire")
  scope <- ir_scope(
    c(
      "ir_bind_clone_histograms",
      "ir_homeostasis_counts",
      "ir_homeostasis_matrix"
    ),
    scope = ir_fast_scope()
  )
  data <- lapply(ir_fast_fixture(), function(d) d[names(d) != "patient"])
  size <- c(Rare = 1e-04, Small = 0.001, Medium = 0.01, Large = 0.1, Hyper = 1)
  for (chain in c("TRA", "TRB")) {
    for (group_by in list(NULL, "group")) {
      expected <- suppressWarnings(scRepertoire::clonalHomeostasis(
        data,
        cloneCall = "aa",
        chain = chain,
        cloneSize = size,
        group.by = group_by,
        exportTable = TRUE
      ))
      encoded <- scope$ir_encode_repertoire(data)
      counts <- scope$ir_homeostasis_counts(
        encoded,
        "CTaa",
        chain,
        ir_fast_values(scope, encoded, group_by)
      )
      actual <- scope$ir_homeostasis_matrix(counts, size)
      actual <- actual[rownames(expected), , drop = FALSE]
      expect_equal(
        unname(actual),
        unname(expected),
        info = paste(chain, if (is.null(group_by)) "samples" else group_by)
      )
    }
  }
})
