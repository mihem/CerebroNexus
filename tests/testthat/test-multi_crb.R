# test-multi_crb.R — unit tests for the multi-crb dataset switcher helper.
#
# match_dataset_id_by_url() lives in the Shiny app's utility_functions.R
# (sourced at app runtime, not exported from the package). We load its parsed
# declaration so the helper can be tested without launching the full app.

ir_utils <- viewer_test_path("utility_functions.R")

# Load a parsed declaration, independent of layout and braces in comments.
match_dataset_id_by_url <- eval(
  viewer_r_assignment(ir_utils, "match_dataset_id_by_url")[[3]],
  new.env(parent = globalenv())
)

test_that("matches a dataset by its id", {
  files <- c(alpha = "/data/a.crb", beta = "/data/b.crb")
  expect_identical(match_dataset_id_by_url("beta", files), "beta")
})

test_that("matches by basename", {
  files <- c(first = "/data/a.crb", second = "/data/b.crb")
  expect_identical(match_dataset_id_by_url("b.crb", files), "second")
})

test_that("matches by basename without extension", {
  files <- c(first = "/data/a.crb", second = "/data/b.crb")
  expect_identical(match_dataset_id_by_url("a", files), "first")
})

test_that("returns empty string when nothing matches", {
  files <- c(alpha = "/data/a.crb")
  expect_identical(match_dataset_id_by_url("nope", files), "")
  expect_identical(match_dataset_id_by_url("a", unname(files)), "")
  for (token in list(NA_character_, "", c("a", "b"), 1L)) {
    expect_identical(match_dataset_id_by_url(token, files), "")
  }
})

test_that("id match takes precedence over basename", {
  # a token that is both an id and a different file's basename resolves by id
  files <- c(b = "/data/x.crb", other = "/data/b.crb")
  expect_identical(match_dataset_id_by_url("b", files), "b")
})
