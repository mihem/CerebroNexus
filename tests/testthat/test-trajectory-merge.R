# test-trajectory-merge.R — unit tests for mergeTrajectoryWithMetaData()
#
# Regression guard for the "arguments imply differing number of rows: 915, 1476"
# crash: when a trajectory covers only a SUBSET of cells (e.g. the monocle2
# B-cell trajectory in demo_full_tcr_bcr.crb — 915 B cells out of 1476), the old
# `cbind(trajectory_data[["meta"]], getMetaData())` blew up because the two
# frames have different row counts. The helper joins by cell barcode instead,
# so non-trajectory cells get NA pseudotime (and are filtered downstream).

# mergeTrajectoryWithMetaData() lives in the Shiny app's utility_functions.R
# (sourced at app runtime, not exported). Source that file into a scratch env
# that stubs the app globals the helper needs.
util_file <- testthat::test_path("../../inst/shiny/v1.4/utility_functions.R")
if (!file.exists(util_file)) {
  util_file <- file.path(
    system.file(package = "CerebroNexus"),
    "shiny",
    "v1.4",
    "utility_functions.R"
  )
}

make_env <- function(meta_data) {
  env <- new.env(parent = globalenv())
  suppressWarnings(source(util_file, local = env, echo = FALSE))
  # utility_functions.R defines its own getMetaData() (an app wrapper around the
  # data_set() reactive). Override it AFTER sourcing so the helper reads our
  # fixture instead of app state.
  env$getMetaData <- function() meta_data
  env
}

# Fixtures mirroring the real shapes: metadata has cell_barcode (superset),
# trajectory meta has rownames = barcodes (subset) + DR_1/DR_2/pseudotime/state.
make_metadata <- function(n = 6) {
  data.frame(
    cell_barcode = paste0("cell", seq_len(n)),
    sample = rep(c("s1", "s2"), length.out = n),
    cell_type = rep(c("B cells", "T cells", "Monocytes"), length.out = n),
    nGene = seq_len(n) * 10L,
    stringsAsFactors = FALSE
  )
}

make_traj_meta <- function(barcodes) {
  data.frame(
    DR_1 = seq_along(barcodes) + 0.1,
    DR_2 = seq_along(barcodes) + 0.2,
    pseudotime = as.numeric(seq_along(barcodes)),
    state = as.character(seq_along(barcodes)),
    row.names = barcodes,
    stringsAsFactors = FALSE
  )
}

test_that("mergeTrajectoryWithMetaData is defined in utility_functions.R", {
  env <- make_env(make_metadata())
  expect_true(is.function(env$mergeTrajectoryWithMetaData))
})

test_that("subset trajectory (915-vs-1476 shape) does not crash and aligns by barcode", {
  md <- make_metadata(6) # 6 "cells"
  # trajectory covers only cells 2 and 4 (a strict subset, like B cells)
  traj <- list(meta = make_traj_meta(c("cell2", "cell4")))
  env <- make_env(md)

  merged <- env$mergeTrajectoryWithMetaData(traj)

  # one row per metadata cell (the full set), never the smaller trajectory count
  expect_equal(nrow(merged), nrow(md))
  # trajectory columns present
  expect_true(all(
    c("DR_1", "DR_2", "pseudotime", "state") %in% colnames(merged)
  ))
  # all original metadata columns present
  expect_true(all(colnames(md) %in% colnames(merged)))
  # row order follows metadata (cell1..cell6)
  expect_equal(merged$cell_barcode, md$cell_barcode)
  # cells IN the trajectory carry their pseudotime; cells OUT get NA
  expect_equal(
    merged$pseudotime[merged$cell_barcode == "cell2"],
    1
  )
  expect_true(is.na(merged$pseudotime[merged$cell_barcode == "cell1"]))
  expect_true(is.na(merged$pseudotime[merged$cell_barcode == "cell4"]) == FALSE)
  # DR coords land on the right cells (cell4 was 2nd trajectory row -> DR_1 2.1)
  expect_equal(merged$DR_1[merged$cell_barcode == "cell4"], 2.1)
})

test_that("full-coverage trajectory (every cell) still works and preserves order", {
  md <- make_metadata(4)
  traj <- list(meta = make_traj_meta(c("cell1", "cell2", "cell3", "cell4")))
  env <- make_env(md)

  merged <- env$mergeTrajectoryWithMetaData(traj)

  expect_equal(nrow(merged), 4)
  expect_false(anyNA(merged$pseudotime))
  expect_equal(merged$cell_barcode, md$cell_barcode)
  # pseudotime aligned per barcode, not by position
  expect_equal(merged$pseudotime, c(1, 2, 3, 4))
})

test_that("column types from trajectory meta are preserved (state stays character)", {
  md <- make_metadata(4)
  traj <- list(meta = make_traj_meta(c("cell1", "cell2", "cell3", "cell4")))
  env <- make_env(md)

  merged <- env$mergeTrajectoryWithMetaData(traj)
  expect_type(merged$state, "character")
  expect_type(merged$pseudotime, "double")
})
