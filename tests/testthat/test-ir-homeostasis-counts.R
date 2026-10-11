ir_home_env <- new.env(parent = globalenv())
ir_home_env$`%||%` <- function(a, b) if (is.null(a)) b else a
ir_home_env$viewerPackCurrent <- function() NULL
ir_home_env$viewerPackReadAsset <- function(...) NULL
ir_home_file <- testthat::test_path(
  "../../inst/viewer/immune_repertoire/data.R"
)
if (!file.exists(ir_home_file)) {
  ir_home_file <- system.file(
    "viewer/immune_repertoire/data.R",
    package = "CerebroNexus"
  )
}
for (expr in parse(ir_home_file)) {
  if (
    is.call(expr) &&
      identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) &&
      identical(expr[[3]][[1]], as.name("function"))
  ) {
    eval(expr, ir_home_env)
  }
}

test_that("shared clone histograms preserve distinct Abundance and Homeostasis policies", {
  values <- c("", "", NA, "NA", "01", "1", "克隆", "克隆", "a\034b")
  frame <- data.frame(
    CTgene = values,
    CTnt = values,
    CTaa = values,
    CTstrict = values
  )
  for (named in c(FALSE, TRUE)) {
    raw <- list(frame, frame[FALSE, ], NULL, data.frame(unrelated = "x"))
    if (named) {
      names(raw) <- c("z10", "z2", "absent", "missing_column")
    }
    abundance <- CerebroNexus:::.viewerPackImmuneAbundance(raw)
    homeostasis <- CerebroNexus:::.viewerPackImmuneAbundance(
      raw,
      drop_empty = FALSE
    )
    for (column in names(abundance)) {
      # The unchanged Pack builder is an independent reference for each policy.
      expect_identical(
        as.list(ir_home_env$ir_clonal_abundance_counts(raw, column)),
        as.list(abundance[[column]])
      )
      expect_identical(
        as.list(ir_home_env$ir_homeostasis_counts(raw, column)),
        as.list(homeostasis[[column]])
      )
    }
  }
  empty <- data.frame(
    sample = character(),
    abundance = integer(),
    n_clones = integer()
  )
  expect_identical(
    ir_home_env$ir_clonal_abundance_counts(list(), "CTgene"),
    empty
  )
  expect_identical(ir_home_env$ir_homeostasis_counts(list(), "CTgene"), empty)
})

test_that("Homeostasis exact bins match scRepertoire, including blanks and thresholds", {
  skip_if_not_installed("scRepertoire")
  frames <- list(
    z10 = c(rep("a", 4), rep("b", 2), "", NA, "NA"),
    z2 = c("", "", "c", "d"),
    empty = character(),
    missing = c(NA, NA),
    unicode = c("克隆", "克隆", "other")
  )
  repertoire <- lapply(frames, function(x) {
    data.frame(CTgene = x, CTnt = x, CTaa = x, CTstrict = x)
  })
  for (call in c("gene", "nt", "aa", "strict")) {
    for (breaks in list(
      c(
        Rare = .0001,
        Small = .001,
        Medium = .01,
        Large = .1,
        Hyperexpanded = 1
      ),
      c(A = .25, B = .5, C = 1),
      c(A = .25, B = .5)
    )) {
      column <- paste0("CT", call)
      counts <- ir_home_env$ir_homeostasis_counts(repertoire, column)
      actual <- ir_home_env$ir_homeostasis_matrix(counts, breaks)
      expected <- scRepertoire::clonalHomeostasis(
        repertoire,
        cloneCall = call,
        chain = "both",
        cloneSize = breaks,
        exportTable = TRUE
      )
      expect_equal(actual, expected, tolerance = 1e-12)
      # The optional pack retains exactly these bins; no approximation to values.
      packed <- CerebroNexus:::.viewerPackImmuneAbundance(
        repertoire,
        drop_empty = FALSE
      )
      expect_equal(
        ir_home_env$ir_homeostasis_matrix(packed[[column]], breaks),
        expected,
        tolerance = 1e-12
      )
    }
  }
  abundance <- CerebroNexus:::.viewerPackImmuneAbundance(repertoire)
  homeostasis <- CerebroNexus:::.viewerPackImmuneAbundance(
    repertoire,
    drop_empty = FALSE
  )
  expect_false(identical(abundance$CTgene, homeostasis$CTgene))
  expect_equal(
    sum(homeostasis$CTgene$abundance * homeostasis$CTgene$n_clones),
    15
  )
})

test_that("Homeostasis reads only the current dataset's optional asset", {
  env <- new.env(parent = ir_home_env)
  counter <- 0L
  packed <- list(
    CTgene = data.frame(sample = "A", abundance = 2L, n_clones = 3L)
  )
  env$viewerPackCurrent <- function() packed
  env$viewerPackReadAsset <- function(pack, path) {
    expect_identical(path, "immune/homeostasis.qs2")
    pack
  }
  fn <- ir_home_env$ir_homeostasis_counts
  environment(fn) <- env
  expect_identical(fn(stop("must not hydrate"), "CTgene"), packed$CTgene)
  packed$CTgene$sample <- "B"
  expect_identical(fn(stop("must not hydrate"), "CTgene")$sample, "B")
  packed <- NULL
  raw <- list(legacy = data.frame(CTgene = c("x", "x", "")))
  expect_identical(fn(raw, "CTgene")$abundance, c(1L, 2L))
})

test_that("Homeostasis plot preserves stacked cell proportions", {
  skip_if_not_installed("scRepertoire")
  data <- list(
    B = data.frame(CTgene = c("a", "a", "b", "c")),
    A = data.frame(CTgene = c("x", "y"))
  )
  breaks <- c(Small = .25, Large = 1)
  p <- ir_home_env$ir_homeostasis_plot(
    ir_home_env$ir_homeostasis_counts(data, "CTgene"),
    breaks,
    "Harmonic"
  )
  reference <- scRepertoire::clonalHomeostasis(
    data,
    cloneCall = "gene",
    cloneSize = breaks,
    palette = "Harmonic"
  )
  actual <- ggplot2::ggplot_build(p)$data[[1]]
  expected <- ggplot2::ggplot_build(reference)$data[[1]]
  expect_equal(
    actual[c("x", "y", "ymin", "ymax", "fill")],
    expected[c("x", "y", "ymin", "ymax", "fill")],
    ignore_attr = TRUE
  )
})

test_that("grouped Homeostasis pools exact clone identities and preserves empty levels", {
  skip_if_not_installed("scRepertoire")
  make <- function(clones, groups) {
    data.frame(
      CTgene = clones,
      CTnt = clones,
      CTaa = clones,
      CTstrict = clones,
      severity = groups,
      stringsAsFactors = FALSE
    )
  }
  raw <- list(
    B = make(
      c("same", "same", "", "NA", NA, "克隆", "a\034b"),
      c("mild", "severe", "mild", "", "zero", NA, "g\034x")
    ),
    A = make(
      c("same", "same", "", "a\034b"),
      c("mild", "mild", "severe", "g\034x")
    ),
    empty = make(character(), character()),
    missing = make(NA_character_, "all_missing_sample")
  )
  for (factor_groups in c(FALSE, TRUE)) {
    data <- raw
    if (factor_groups) {
      data <- lapply(data, function(x) {
        x$severity <- factor(
          x$severity,
          levels = c(
            "severe",
            "mild",
            "",
            "zero",
            "g\034x",
            "unused",
            "all_missing_sample"
          )
        )
        x
      })
    }
    for (call in c("gene", "nt", "aa", "strict")) {
      counts <- ir_home_env$ir_grouped_homeostasis_counts(
        data,
        paste0("CT", call),
        "severity"
      )
      for (breaks in list(c(A = .25, B = .5, C = 1), c(A = .25, B = .5))) {
        expected <- scRepertoire::clonalHomeostasis(
          data,
          cloneCall = call,
          chain = "both",
          group.by = "severity",
          cloneSize = breaks,
          exportTable = TRUE
        )
        expect_equal(
          ir_home_env$ir_homeostasis_matrix(counts, breaks),
          expected,
          tolerance = 1e-12
        )
      }
    }
    # Shared clone "same" has three cells in mild, even across source samples.
    mild <- counts[counts$sample == "mild", ]
    expect_equal(mild$abundance, c(1L, 3L))
    expect_equal(mild$n_clones, c(1L, 1L))
  }
  data <- list(
    B = make(c("x", "x", "y"), c("severe", "mild", "mild")),
    A = make(c("x", "z"), c("mild", "severe"))
  )
  breaks <- c(Small = .5, Large = 1)
  actual <- ir_home_env$ir_homeostasis_plot(
    ir_home_env$ir_grouped_homeostasis_counts(data, "CTgene", "severity"),
    breaks,
    "inferno"
  )
  expected <- scRepertoire::clonalHomeostasis(
    data,
    cloneCall = "gene",
    group.by = "severity",
    cloneSize = breaks,
    palette = "inferno"
  )
  cols <- c("x", "y", "ymin", "ymax", "fill")
  expect_equal(
    ggplot2::ggplot_build(actual)$data[[1]][cols],
    ggplot2::ggplot_build(expected)$data[[1]][cols],
    ignore_attr = TRUE
  )
  expect_error(
    ir_home_env$ir_grouped_homeostasis_counts(data, "CTgene", "absent"),
    "missing"
  )
  # A new dataset must produce its own labels, not reuse a grouped result.
  other <- list(C = make(c("q", "q"), c("new", "new")))
  expect_identical(
    ir_home_env$ir_grouped_homeostasis_counts(
      other,
      "CTgene",
      "severity"
    )$sample,
    "new"
  )
})
