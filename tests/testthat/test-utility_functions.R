## Unit tests for helper functions defined in
## inst/shiny/v1.4/utility_functions.R.
##
## These functions live in the Shiny app tree (not the package R/ namespace),
## so they are loaded by sourcing the file into a throw-away environment. They
## are pure R and require neither a running app nor Seurat.
##
## Coverage focuses on the edge cases that previously crashed the app at
## runtime (NA-only percentage columns, NULL/NA toggle inputs, a missing
## grouping column) plus the caching contract of the cachePlot() wrapper. See
## the git history of utility_functions.R for context.

## Prefer the installed copy (mirrors how test-app-inst.R locates the app),
## falling back to the source tree when running against an uninstalled
## checkout (e.g. devtools::load_all()).
utils_file <- system.file(
  "shiny",
  "v1.4",
  "utility_functions.R",
  package = "CerebroNexus"
)
if (!nzchar(utils_file) || !file.exists(utils_file)) {
  utils_file <- testthat::test_path(
    "..",
    "..",
    "inst",
    "shiny",
    "v1.4",
    "utility_functions.R"
  )
}
skip_if_not(file.exists(utils_file), "utility_functions.R not found")

utils_env <- new.env()
source(utils_file, local = utils_env)
prettifyTable <- utils_env$prettifyTable
centerOfGroups <- utils_env$centerOfGroups
cachePlot <- utils_env$cachePlot
dynamicPointSize <- utils_env$dynamicPointSize

test_that("spatial offset ranges require finite coordinates", {
  path <- file.path(
    dirname(utils_file),
    "spatial/UI_projection_additional_parameters.R"
  )
  source_text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source_text,
    "x <- co\\[\\[1\\]\\]\\[is.finite\\(co\\[\\[1\\]\\]\\)\\]"
  )
  expect_match(source_text, "length\\(x\\) > 0 && length\\(y\\) > 0")
})

## ---------------------------------------------------------------------------
## centerOfGroups
## ---------------------------------------------------------------------------

test_that("centerOfGroups computes 2D medians per group", {
  result <- centerOfGroups(
    coordinates = list(c(0, 10, 2), c(0, 10, 12)),
    df = data.frame(grp = c("A", "A", "B")),
    n_dimensions = 2,
    group = "grp"
  )
  result <- as.data.frame(result)
  expect_setequal(result$group, c("A", "B"))
  expect_equal(result$x_median[result$group == "A"], 5)
  expect_equal(result$y_median[result$group == "A"], 5)
  expect_equal(result$x_median[result$group == "B"], 2)
  expect_equal(result$y_median[result$group == "B"], 12)
})

test_that("centerOfGroups returns a typed empty tibble for a missing group column", {
  result <- centerOfGroups(
    coordinates = matrix(c(1, 2, 3, 4), ncol = 2),
    df = data.frame(cluster = c("a", "b")),
    n_dimensions = 2,
    group = "does_not_exist"
  )
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("group", "x_median", "y_median", "z_median") %in% colnames(result)
  ))
})

test_that("centerOfGroups returns a typed empty tibble for a NULL group", {
  result <- centerOfGroups(
    coordinates = matrix(c(1, 2, 3, 4), ncol = 2),
    df = data.frame(cluster = c("a", "b")),
    n_dimensions = 2,
    group = NULL
  )
  expect_equal(nrow(result), 0)
})

## ---------------------------------------------------------------------------
## prettifyTable edge cases
## ---------------------------------------------------------------------------

test_that("prettifyTable does not crash on an all-NA percentage column", {
  ## Old code did `if (max(col > 1))`, which returned NA for an all-NA column
  ## and threw "missing value where TRUE/FALSE needed".
  table <- data.frame(
    gene = c("g1", "g2"),
    percent_mt = c(NA_real_, NA_real_)
  )
  expect_no_error(
    prettifyTable(
      table,
      filter = "none",
      dom = "t",
      number_formatting = TRUE,
      columns_percentage = 2
    )
  )
})

test_that("prettifyTable still rescales a 0-100 percentage column to 0-1", {
  table <- data.frame(
    gene = c("g1", "g2", "g3"),
    percent_mt = c(50, NA_real_, 20)
  )
  widget <- prettifyTable(
    table,
    filter = "none",
    dom = "t",
    number_formatting = TRUE,
    columns_percentage = 2
  )
  ## The rescaled values live in the widget's data payload.
  rescaled <- widget$x$data$percent_mt
  expect_equal(rescaled[!is.na(rescaled)], c(0.5, 0.2))
})

test_that("prettifyTable tolerates NA / NULL toggle inputs", {
  table <- data.frame(
    gene = c("g1", "g2"),
    percent_mt = c(10, 20)
  )
  ## materialSwitch can transiently pass NA / NULL during UI re-render.
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", number_formatting = NA)
  )
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", show_buttons = NULL)
  )
  expect_no_error(
    prettifyTable(table, filter = "none", dom = "t", hide_long_columns = NA)
  )
})

## ---------------------------------------------------------------------------
## cachePlot: the shared bindCache wrapper used by the plot renderers.
##
## Drives a minimal server that caches a counting reactive through cachePlot,
## then asserts the caching contract: the reactive evaluates, an unchanged key
## does not recompute, a changed plot-specific key invalidates the cache, and a
## changed dataset key invalidates the cache. The last case would regress if
## the dataset key were forwarded as an already-evaluated value instead of an
## unevaluated expression, so this also guards the wrapper's cache-key scoping.
## ---------------------------------------------------------------------------

test_that("cachePlot caches by key and invalidates on key or dataset change", {
  skip_if_not_installed("shiny", "1.6.0")

  compute_count <- 0

  server <- function(input, output, session) {
    available_crb_files <- shiny::reactiveValues(selected = "datasetA")
    cached <- shiny::reactive({
      compute_count <<- compute_count + 1
      paste(input$metric, available_crb_files$selected)
    }) %>%
      cachePlot(input$metric, available_crb_files$selected)
    output$val <- shiny::renderText(cached())
  }

  shiny::testServer(server, {
    ## 1. evaluates successfully
    session$setInputs(metric = "nUMI")
    expect_equal(cached(), "nUMI datasetA")
    first <- compute_count
    expect_equal(first, 1)

    ## 2. unchanged keys do not recompute
    cached()
    expect_equal(compute_count, first)

    ## 3. changing a plot-specific key invalidates the cache
    session$setInputs(metric = "nGene")
    expect_equal(cached(), "nGene datasetA")
    expect_equal(compute_count, first + 1)

    ## returning to a previously cached key hits the cache
    session$setInputs(metric = "nUMI")
    cached()
    expect_equal(compute_count, first + 1)

    ## 4. changing the dataset key invalidates the cache
    available_crb_files$selected <- "datasetB"
    session$flushReact()
    expect_equal(cached(), "nUMI datasetB")
    expect_equal(compute_count, first + 2)
  })
})

## ---------------------------------------------------------------------------
## dynamicPointSize: default marker size from point count (+ optional canvas)
## ---------------------------------------------------------------------------

test_that("dynamicPointSize shrinks as the point count grows", {
  ## More points -> smaller default, monotonically non-increasing.
  sizes <- vapply(
    c(100, 1000, 10000, 100000, 1e6),
    function(n) dynamicPointSize(n),
    numeric(1)
  )
  expect_true(all(diff(sizes) <= 0))
  ## A small dataset should be clearly larger than a huge one.
  expect_gt(dynamicPointSize(100), dynamicPointSize(200000))
})

test_that("dynamicPointSize stays within [min, max] and snaps to step", {
  vals <- vapply(
    c(1, 10, 500, 5000, 5e5, 1e7),
    function(n) dynamicPointSize(n, min = 1, max = 20, step = 1),
    numeric(1)
  )
  expect_true(all(vals >= 1 & vals <= 20))
  expect_true(all(vals == round(vals))) # step = 1 -> integers
})

test_that("dynamicPointSize returns the fallback for missing/invalid counts", {
  expect_equal(dynamicPointSize(NULL, fallback = 3), 3)
  expect_equal(dynamicPointSize(NA, fallback = 3), 3)
  expect_equal(dynamicPointSize(0, fallback = 3), 3)
  expect_equal(dynamicPointSize(-5, fallback = 3), 3)
})

test_that("dynamicPointSize lets a larger canvas carry larger points", {
  small <- dynamicPointSize(5000, plot_width_px = 500, plot_height_px = 400)
  big <- dynamicPointSize(5000, plot_width_px = 1600, plot_height_px = 1100)
  expect_gte(big, small)
  ## The canvas correction only nudges — it never flips the point-count order.
  expect_gt(
    dynamicPointSize(200, plot_width_px = 500, plot_height_px = 400),
    dynamicPointSize(100000, plot_width_px = 1600, plot_height_px = 1100)
  )
})
