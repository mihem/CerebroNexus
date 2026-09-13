plotting_file <- viewer_test_path("plotting_functions.R")
plotting_env <- new.env(parent = globalenv())
sys.source(plotting_file, envir = plotting_env)

test_that("large violins retain distribution coverage with bounded rows", {
  table <- data.frame(
    group = factor(rep(c("A", "B"), each = 1000L)),
    value = c(rep(0, 999L), 1000, seq_len(1000L) * 2)
  )

  compact <- plotting_env$compactViolinData(
    table,
    metric = "value",
    coloring_variable = "group",
    max_points_per_group = 100L
  )

  expect_equal(as.integer(table(compact$group)), c(100L, 100L))
  expect_equal(levels(compact$group), levels(table$group))
  expect_equal(
    vapply(split(compact$value, compact$group), mean, numeric(1)),
    vapply(split(table$value, table$group), mean, numeric(1)),
    tolerance = 1e-12
  )

  signed <- data.frame(
    group = "signed",
    value = c(rep(-1, 999L), 999)
  )
  signed_compact <- plotting_env$compactViolinData(
    signed,
    metric = "value",
    coloring_variable = "group",
    max_points_per_group = 100L
  )
  expect_equal(mean(signed_compact$value), 0, tolerance = 1e-12)
  expect_gt(diff(range(signed_compact$value)), 0)
  expect_match(
    paste(deparse(body(plotting_env$plotlyViolin)), collapse = "\n"),
    "compactViolinData",
    fixed = TRUE
  )
})

test_that("million-cell violin callers share the compact path", {
  callers <- c(
    "gene_expression/func_expression_summary.R",
    "overview/out_details_selected_cells_plot.R",
    "spatial/out_details_selected_cells_plot.R",
    "coordinated_views/server.R"
  )
  for (caller in callers) {
    source <- paste(
      readLines(viewer_test_path(caller), warn = FALSE),
      collapse = "\n"
    )
    expect_match(source, "compactViolinData(", fixed = TRUE, info = caller)
  }
})
