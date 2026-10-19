# test-multi_crb.R — unit tests for the multi-crb dataset switcher helper.
#
# match_dataset_by_url() lives in the Shiny app's utility_functions.R (sourced
# at app runtime, not exported from the package). We source that file directly
# so the helper can be tested in isolation, without launching the full app.

ir_utils <- viewer_test_path("utility_functions.R")

# Load a parsed declaration, independent of layout and braces in comments.
match_dataset_by_url <- eval(
  viewer_r_assignment(ir_utils, "match_dataset_by_url")[[3]],
  new.env(parent = globalenv())
)

test_that("matches a named dataset by its name", {
  files <- c(alpha = "/data/a.crb", beta = "/data/b.crb")
  expect_equal(
    match_dataset_by_url("beta", files, names(files)),
    "/data/b.crb"
  )
})

test_that("matches by basename when names are absent", {
  files <- c("/data/a.crb", "/data/b.crb")
  expect_equal(
    match_dataset_by_url("b.crb", files, NULL),
    "/data/b.crb"
  )
})

test_that("matches by basename without extension", {
  files <- c("/data/a.crb", "/data/b.crb")
  expect_equal(
    match_dataset_by_url("a", files, NULL),
    "/data/a.crb"
  )
})

test_that("returns empty string when nothing matches", {
  files <- c(alpha = "/data/a.crb")
  expect_identical(
    match_dataset_by_url("nope", files, names(files)),
    ""
  )
})

test_that("name match takes precedence over basename", {
  # a token that is both a name and a different file's basename resolves by name
  files <- c(b = "/data/x.crb", other = "/data/b.crb")
  expect_equal(
    match_dataset_by_url("b", files, names(files)),
    "/data/x.crb"
  )
})
