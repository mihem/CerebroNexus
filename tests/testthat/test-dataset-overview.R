overview_helpers <- new.env(parent = globalenv())
sys.source(viewer_test_path("core", "dataset_overview.R"), overview_helpers)
sys.source(viewer_test_path("load_data", "overview_ui.R"), overview_helpers)

overview_fixture <- function() {
  list(getExperiment = function() list(description = "  ", source = NA_character_),
       getMetaData = function() data.frame(sample = c("a", "a", "b", NA),
         cluster = c("T", "T", "B", "B")),
       getGroups = function() c("sample", "cluster"),
       getGeneNames = function() c("A", "B", "C"),
       availableProjections = function() "umap",
       availableSpatial = function() character(),
       getExpressionMatrix = function(...) stop("Must never read expression values"))
}

test_that("overview uses known metadata and omits blank information", {
  result <- overview_helpers$viewerDatasetOverview(overview_fixture())
  expect_null(result$description)
  expect_null(result$source)
  expect_identical(result$genes, 3L)
  expect_identical(result$samples, 2L)
  expect_equal(result$sample$counts, c(a = 2, b = 1))
  expect_equal(result$sample$missing, 1)
  expect_named(result$annotations, "cluster")
  html <- as.character(overview_helpers$viewerOverviewUI(list(label = "<script>dataset</script>", cells = 4, overview = result)))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("Data source|Exported|not available|Spatial views", html))
})

test_that("bundle creation embeds the same compact overview", {
  object <- Cerebro$new()
  object$setMetaData(data.frame(sample = c("a", "b")))
  object$addGroup("sample", c("a", "b"))
  info <- CerebroNexus:::.readBundleDatasetInfo(object, "Dataset", "data.crb")
  expect_identical(info$overview$samples, 2L)
  expect_equal(info$overview$sample$counts, c(a = 1, b = 1))
})

test_that("missing metadata produces no invented sample or gene counts", {
  object <- overview_fixture()
  object$getMetaData <- function() data.frame(condition = c("a", "b"))
  object$getGroups <- function() "condition"
  object$getGeneNames <- function() NULL
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_null(result$samples)
  expect_null(result$sample)
  expect_null(result$genes)
  expect_named(result$annotations, "condition")
})

test_that("all categories remain separately visible in large summaries", {
  object <- overview_fixture()
  object$getMetaData <- function() data.frame(sample = rep(seq_len(200), 5))
  object$getGroups <- function() "sample"
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_length(result$sample$counts, 200)
  expect_equal(result$sample$total, 1000)
  expect_equal(sum(result$sample$counts) + result$sample$remaining, 1000)
  expect_equal(result$samples, 200)
  expect_false(overview_helpers$viewerOverviewTruncated(result))
  html <- as.character(overview_helpers$viewerOverviewBars(result$sample))
  expect_equal(lengths(regmatches(html, gregexpr('class="dataset-bar-row"', html, fixed = TRUE))), 200)
  expect_false(grepl("Top 10|Other categories", html))
  legacy <- result
  legacy$sample$counts <- head(result$sample$counts, 10)
  legacy$sample$remaining <- 950
  expect_true(overview_helpers$viewerOverviewTruncated(legacy))
  expect_false(overview_helpers$viewerOverviewTruncated(NULL))
})

test_that("explicit sample column works without name heuristics", {
  object <- overview_fixture()
  object$getExperiment <- function() list(sample_column = "donor", description = "Study description")
  object$getMetaData <- function() data.frame(donor = c("a", "b", "a"))
  object$getGroups <- function() "donor"
  result <- overview_helpers$viewerDatasetOverview(object)
  expect_equal(result$samples, 2)
  expect_length(result$annotations, 0)
  expect_identical(result$description, "Study description")
})

test_that("existing app summaries are rejected after data file changes", {
  path <- tempfile()
  on.exit(unlink(path))
  writeLines("first", path)
  stamp <- file.info(path)
  entries <- setNames(list(list(size = as.numeric(stamp$size), mtime = as.numeric(stamp$mtime), overview = list(genes = 3L))), path)
  expect_equal(overview_helpers$viewerSavedOverview(entries, path)$genes, 3L)
  writeLines("different data", path)
  expect_null(overview_helpers$viewerSavedOverview(entries, path))
})

test_that("thin CRB summaries obtain gene counts from matrix headers", {
  skip_if_not_installed("BPCells")
  root <- withr::local_tempdir()
  sidecar <- file.path(root, "matrix.bpcells")
  mat <- matrix(0, 3, 2, dimnames = list(c("A", "B", "C"), c("x", "y")))
  BPCells::write_matrix_dir(mat = methods::as(Matrix::Matrix(mat, sparse = TRUE), "IterableMatrix"), dir = sidecar)
  object <- Cerebro$new()
  object$setMetaData(data.frame(sample = c("one", "two"), row.names = c("x", "y")))
  object$setExpressionBackend(type = "bpcells", location = basename(sidecar))
  info <- CerebroNexus:::.readBundleDatasetInfo(object, "Thin", file.path(root, "thin.crb"))
  expect_identical(info$overview$genes, 3L)
  expect_null(object$expression)
})
