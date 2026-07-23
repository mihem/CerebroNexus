# test-multi_crb.R — unit tests for the multi-crb dataset switcher helper.
#
# match_dataset_by_url() lives in the Shiny app's utility_functions.R (sourced
# at app runtime, not exported from the package). We source that file directly
# so the helper can be tested in isolation, without launching the full app.

# Prefer the source tree copy (always current); fall back to the installed
# package location when tests run against an installed build.
ir_utils_candidates <- c(
  testthat::test_path("../../inst/shiny/v1.4/utility_functions.R"),
  file.path(
    system.file(package = "CerebroNexus"),
    "shiny",
    "v1.4",
    "utility_functions.R"
  )
)
ir_utils <- ir_utils_candidates[file.exists(ir_utils_candidates)][1]
testthat::skip_if(
  is.na(ir_utils),
  "utility_functions.R not found in source tree or installed package"
)

# The file references helpers/objects only defined inside the running app; we
# only need match_dataset_by_url(), so extract just that function's source and
# evaluate it in a throwaway environment.
extract_fn <- function(path, fn) {
  lines <- readLines(path, warn = FALSE)
  start <- grep(paste0("^", fn, " <- function"), lines)
  testthat::skip_if(
    length(start) == 0,
    "helper not found in utility_functions.R"
  )
  # find the closing brace at column 1 after the definition start
  end <- start
  depth <- 0L
  for (i in seq(start, length(lines))) {
    depth <- depth + lengths(regmatches(lines[i], gregexpr("\\{", lines[i])))
    depth <- depth - lengths(regmatches(lines[i], gregexpr("\\}", lines[i])))
    if (i > start && depth == 0L) {
      end <- i
      break
    }
  }
  eval(
    parse(text = paste(lines[start:end], collapse = "\n")),
    envir = globalenv()
  )
}

extract_fn(ir_utils, "match_dataset_by_url")

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
