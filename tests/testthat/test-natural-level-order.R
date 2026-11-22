## Issue #284: group levels such as "Cluster 1", "Cluster 2", ..., "Cluster 10"
## are shown in natural order, not "Cluster 1", "Cluster 10", "Cluster 2".

natural_contract <- function() {
  env <- new.env(parent = baseenv())
  sys.source(viewer_test_path("core", "crb_contract.R"), env)
  env
}

test_that("natural order compares runs of digits as numbers", {
  contract <- natural_contract()
  expect_identical(
    contract$.runtimeNaturalSort(c(
      "Cluster 10", "Cluster 2", "Unassigned", "Cluster 1", "Cluster 21"
    )),
    c("Cluster 1", "Cluster 2", "Cluster 10", "Cluster 21", "Unassigned")
  )
  expect_identical(
    contract$.runtimeNaturalSort(c("s2_roi10", "s10_roi1", "s2_roi9")),
    c("s2_roi9", "s2_roi10", "s10_roi1")
  )
  expect_identical(
    contract$.runtimeNaturalSort(c("b", NA, "a")),
    c("a", "b", NA)
  )
  expect_identical(contract$.runtimeNaturalSort(character()), character())
})

test_that("only plainly sorted levels are put in natural order", {
  contract <- natural_contract()
  plain <- sort(paste("Cluster", 1:12))
  expect_identical(
    contract$.runtimeNaturalLevels(plain),
    paste("Cluster", 1:12)
  )
  expect_identical(
    contract$.runtimeNaturalLevels(c(plain, "N/A")),
    c(paste("Cluster", 1:12), "N/A")
  )
  chosen <- c("Cluster 3", "Cluster 1", "Cluster 10", "Cluster 2")
  expect_identical(contract$.runtimeNaturalLevels(chosen), chosen)
  expect_identical(contract$.runtimeNaturalLevels("one"), "one")
})

test_that("a cached CRB prototype gets natural group levels and codes", {
  scope <- viewer_runtime_env()
  clusters <- c("Cluster 2", "Cluster 10", "Cluster 1", "Cluster 2", NA)
  prototype <- new.env()
  prototype$groups <- list(
    cluster = sort(unique(clusters)),
    chosen = c("b", "a"),
    stage = c("S1", "S10", "S2")
  )
  prototype$meta_data <- data.frame(
    cluster = factor(clusters, levels = sort(unique(clusters))),
    chosen = factor(c("a", "b", "a", "b", "a"), levels = c("b", "a")),
    stage = factor(paste0("S", c(1, 10, 2, 1, 2)), ordered = TRUE),
    other = factor(paste0("C", c(1, 10, 2, 1, 2)))
  )
  scope$.naturalCrbLevels(prototype)

  expect_identical(
    prototype$groups$cluster,
    c("Cluster 1", "Cluster 2", "Cluster 10")
  )
  expect_identical(prototype$groups$chosen, c("b", "a"))
  expect_identical(
    levels(prototype$meta_data$cluster),
    c("Cluster 1", "Cluster 2", "Cluster 10")
  )
  expect_identical(as.character(prototype$meta_data$cluster), clusters)
  expect_identical(levels(prototype$meta_data$chosen), c("b", "a"))
  ## An ordered factor keeps its order; a column that is not a group is left.
  expect_identical(prototype$groups$stage, c("S1", "S10", "S2"))
  expect_true(is.ordered(prototype$meta_data$stage))
  expect_identical(levels(prototype$meta_data$stage), c("S1", "S10", "S2"))
  expect_identical(levels(prototype$meta_data$other), c("C1", "C10", "C2"))
})

test_that("Linked views colour character columns in natural order", {
  scope <- viewer_runtime_env()
  sys.source(viewer_test_path("coordinated_views", "bundle.R"), scope)
  expect_identical(
    scope$cv_group_levels(c("Cluster 10", NA, "Cluster 9", "Cluster 1")),
    c("Cluster 1", "Cluster 9", "Cluster 10")
  )
})

test_that("exported character groups get natural levels", {
  values <- c("Cluster 10", "Cluster 2", NA, "Cluster 1")
  exported <- .exportGroupColumn(values, "cluster", verbose = FALSE)
  expect_identical(
    levels(exported),
    c("Cluster 1", "Cluster 2", "Cluster 10", "N/A")
  )
  expect_identical(
    as.character(exported),
    c("Cluster 10", "Cluster 2", "N/A", "Cluster 1")
  )
})
