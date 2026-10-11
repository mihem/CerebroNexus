selection_env <- new.env(parent = globalenv())
sys.source(viewer_test_path("plotting_functions.R"), selection_env)
sys.source(viewer_test_path("utility_functions.R"), selection_env)

test_that("persistent selection preserves cell identity at coincident coordinates", {
  context <- selection_env$viewerDatasetContext("session", "spatial", 1)
  result <- selection_env$viewerPersistentSelection(
    list(
      x = c("1", "1", "2"),
      y = c("3", "3", "4"),
      ids = c("ROI1/甲", "ROI2/乙", "cell-3"),
      dataset_context = context
    ),
    context
  )
  expect_identical(result$x, c(1, 1, 2))
  expect_identical(result$identifier, c("1-3", "1-3", "2-4"))
  expect_identical(result$selection_key, c("ROI1/甲", "ROI2/乙", "cell-3"))
  expect_identical(
    selection_env$selectedCellMask(
      c("ROI2/乙", "unselected"),
      c("1-3", "1-3"),
      result
    ),
    c(TRUE, FALSE)
  )
})

test_that("empty and stale selection messages cannot acquire another dataset", {
  context <- selection_env$viewerDatasetContext("session", "first", 1)
  input <- list(x = 1, y = 2, ids = "cell", dataset_context = context)
  f <- selection_env$viewerPersistentSelection
  expect_null(f(NULL, stop("An empty message must not force context")))
  expect_null(f("invalid", context))
  expect_null(f(list(x = 1, y = 2), context))
  expect_null(f(input, modifyList(context, list(dataset_key = "second"))))
  expect_null(f(input, modifyList(context, list(generation = 2))))
  expect_null(f(input, modifyList(context, list(epoch = "new session"))))
  expect_null(f(modifyList(input, list(x = numeric(), y = numeric())), context))
})

test_that("coordinate-only selection keeps its legacy fallback", {
  context <- selection_env$viewerDatasetContext("session", "legacy", 1)
  result <- selection_env$viewerPersistentSelection(
    list(
      x = c(1, 2),
      y = c(3, 4),
      ids = "partial",
      dataset_context = context
    ),
    context
  )
  expect_identical(names(result), c("x", "y", "identifier"))
  expect_identical(
    selection_env$selectedCellMask(
      c("a", "b"),
      c("1-3", "5-6"),
      result
    ),
    c(TRUE, FALSE)
  )
})

test_that("cached module loading resolves selection context in each session", {
  cache <- new.env(parent = globalenv())
  sys.source(viewer_test_path("source_cache.R"), cache)
  sessions <- list(new.env(parent = globalenv()), new.env(parent = globalenv()))
  for (session in sessions) {
    cache$viewerSource(viewer_test_path("utility_functions.R"), session)
    context <- session$viewerDatasetContext("session", "dataset", 1)
    message <- list(x = 1, y = 2, ids = "cell", dataset_context = context)
    expect_identical(
      session$viewerPersistentSelection(message, context)$selection_key,
      "cell"
    )
    expect_null(session$viewerPersistentSelection(
      message,
      modifyList(context, list(generation = 2))
    ))
  }
})

test_that("shared distribution keeps category counts and registered empty levels", {
  env <- new.env(parent = selection_env)
  env$`%>%` <- dplyr::`%>%`
  env$getGroups <- function() "sample"
  env$getGroupLevels <- function(variable) c("A", "B")
  env$assignColorsToGroups <- function(table, variable) {
    c(A = "#ff8000", B = "#777777")
  }
  plotter <- selection_env$cerebroSelectedCellsPlot
  environment(plotter) <- env
  renderer <- function(data, variable) {
    keys <- as.character(seq_len(nrow(data)))
    selection <- data.frame(selection_key = keys[data$group == "selected"])
    plotter(
      data.frame(
        x = seq_len(nrow(data)),
        y = seq_len(nrow(data)),
        cell_barcode = keys,
        sample = data$sample
      ),
      selection,
      variable
    )
  }
  data <- data.frame(
    sample = c("A", "A", "B"),
    group = c("selected", "selected", "not selected")
  )
  plot <- suppressWarnings(plotly::plotly_build(renderer(data, "sample")))
  expect_equal(sum(unlist(lapply(plot$x$data, `[[`, "y"))), 2)
  data$group <- "not selected"
  empty <- suppressWarnings(plotly::plotly_build(renderer(data, "sample")))
  expect_equal(sum(unlist(lapply(empty$x$data, `[[`, "y"))), 0)
  expect_setequal(unlist(lapply(empty$x$data, `[[`, "x")), c("A", "B"))
})

test_that("coordinate frames preserve identity checks and coordinate-only fallback", {
  xy <- data.frame(x = c(1, 1, 2), y = c(3, 3, 4))
  stable <- data.frame(selection_key = "b", identifier = "1-3")
  mask <- selection_env$selectedCellMask
  expect_identical(mask(c("a", "b", "c"), xy, stable), c(FALSE, TRUE, FALSE))
  expect_identical(
    mask(c("a", "b", "c"), xy, stable["identifier"]),
    c(TRUE, TRUE, FALSE)
  )
  expect_identical(mask(c("a", "b", "c"), xy, NULL), rep(FALSE, 3))
  expect_error(mask("a", xy, stable), "equal length")
  expect_error(mask(c("a", "b", "c"), xy["x"], stable), "two columns")
})

test_that("distribution preparation keeps selected groups and legacy key modes", {
  prepare <- selection_env$prepareSelectedCellDistribution
  xy <- data.frame(x = c(1, 1, 2), y = c(3, 3, 4))
  metadata <- data.frame(
    cell_barcode = c("a", "b", "c"),
    value = c(0, NA_real_, 7),
    category = factor(c("A", "B", "C"))
  )
  stable <- data.frame(selection_key = "b", identifier = "1-3")
  result <- prepare(xy, metadata, stable, "value")
  expect_named(result, c("group", "value"))
  expect_identical(result$value, metadata$value)
  expect_identical(
    as.character(result$group),
    c("not selected", "selected", "not selected")
  )
  expect_identical(levels(result$group), c("selected", "not selected"))
  expect_identical(
    prepare(xy, metadata, NULL, "category")$category,
    metadata$category
  )
  expect_identical(
    prepare(xy, metadata, NULL, "value")$group,
    rep("not selected", 3)
  )
  row_selection <- data.frame(selection_key = "2")
  expect_identical(
    prepare(xy, metadata[-1], row_selection, "value")$group,
    result$group
  )
  coordinate_selection <- data.frame(selection_key = "1-3")
  spatial <- prepare(
    xy,
    metadata[-1],
    coordinate_selection,
    "value",
    coordinate_keys = TRUE
  )
  expect_identical(
    as.character(spatial$group),
    c("selected", "selected", "not selected")
  )
  expect_identical(
    prepare(xy, metadata, stable, "identifier")$identifier,
    c("1-3", "1-3", "2-4")
  )
  expect_identical(
    prepare(xy, metadata, stable, "selection_key")$selection_key,
    metadata$cell_barcode
  )
  expect_equal(nrow(prepare(xy[FALSE, ], metadata[FALSE, ], NULL, "value")), 0)
})
