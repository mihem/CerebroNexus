## Filtering the projection selection by legend-hidden groups (Main tab).
##
## When the user hides a group via the custom legend, the selected-cells count
## and downstream panels should count only cells in still-visible groups. The
## pure filter that implements this lives in a func_ helper sourced by the
## overview module; source it directly here so it can be unit-tested without a
## Shiny session.

# The shared helper lives in utility_functions.R (sourced once at server start).
# Source it into a throwaway environment and return the function. The file only
# DEFINES functions at top level (Shiny / later calls happen inside them), so
# sourcing it standalone is safe.
load_filter <- function() {
  env <- new.env()
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = env
  )
  env[["filterSelectionByHiddenGroups"]]
}

# A tiny selection + metadata fixture. identifier keys a cell the same way the
# app does (paste0(X1, '-', X2)); the metadata carries the grouping column.
make_fixture <- function() {
  selection <- data.frame(
    x = c(1, 2, 3, 4),
    y = c(1, 2, 3, 4),
    identifier = c("1-1", "2-2", "3-3", "4-4"),
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    identifier = c("1-1", "2-2", "3-3", "4-4"),
    sample = c("sample_1", "sample_2", "sample_2", "sample_3"),
    stringsAsFactors = FALSE
  )
  list(selection = selection, metadata = metadata)
}

test_that("no hidden groups returns the selection unchanged", {
  f <- load_filter()
  fx <- make_fixture()
  out <- f(fx$selection, fx$metadata, "sample", character(0))
  expect_equal(out, fx$selection)
  out_null <- f(fx$selection, fx$metadata, "sample", NULL)
  expect_equal(out_null, fx$selection)
})

test_that("hiding a group drops its cells from the selection", {
  f <- load_filter()
  fx <- make_fixture()
  out <- f(fx$selection, fx$metadata, "sample", "sample_2")
  # sample_2 owns the two middle cells (2-2, 3-3); both must be gone.
  expect_equal(out$identifier, c("1-1", "4-4"))
  expect_equal(nrow(out), 2L)
})

test_that("hiding every represented group yields an empty selection", {
  f <- load_filter()
  fx <- make_fixture()
  out <- f(
    fx$selection,
    fx$metadata,
    "sample",
    c("sample_1", "sample_2", "sample_3")
  )
  expect_equal(nrow(out), 0L)
})

test_that("a NULL selection stays NULL regardless of hidden groups", {
  f <- load_filter()
  fx <- make_fixture()
  expect_null(f(NULL, fx$metadata, "sample", "sample_2"))
})

test_that("hidden groups not present in the data are ignored", {
  f <- load_filter()
  fx <- make_fixture()
  out <- f(fx$selection, fx$metadata, "sample", "sample_ZZZ")
  expect_equal(out, fx$selection)
})

test_that("viewerVisibleSelection reads cells only when a group is hidden", {
  env <- new.env()
  sys.source(viewer_test_path("utility_functions.R"), envir = env)
  context <- list(epoch = "s", dataset_key = "A", generation = 1)
  reads <- 0L
  cells <- function() {
    reads <<- reads + 1L
    data.frame(
      group = c("a", "b", "a"),
      DR_1 = c(1, 2, 3),
      DR_2 = c(4, 5, 6)
    )
  }
  selection <- data.frame(identifier = c("1-4", "2-5", "3-6"))
  visible <- function(request) {
    env$viewerVisibleSelection(
      selection,
      request,
      context,
      "group",
      cells,
      coordinates = c("DR_1", "DR_2")
    )
  }
  expect_identical(visible(NULL), selection)
  expect_identical(
    visible(list(
      groups = "a",
      dataset_context = modifyList(
        context,
        list(generation = 2)
      )
    )),
    selection
  )
  expect_identical(reads, 0L)
  expect_identical(
    visible(list(groups = "a", dataset_context = context))$identifier,
    "2-5"
  )
  expect_null(visible(list(groups = c("a", "b"), dataset_context = context)))
  expect_identical(reads, 2L)
})
