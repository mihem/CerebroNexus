## buildGpuData() (www/cell_views.js) reuses the GPU positions of a projection
## that another view of the same data set has uploaded. It must read the shared
## dataset state when it builds: the state is made after the script loads, so
## a reference taken at load time is always undefined and nothing is shared.

test_that("buildGpuData reads the shared dataset state when it runs", {
  js <- paste(
    readLines(viewer_test_path("www", "cell_views.js"), warn = FALSE),
    collapse = "\n"
  )
  start <- regexpr("\n  function buildGpuData(", js, fixed = TRUE)
  expect_gt(start, 0L)
  rest <- substring(js, start + 1L)
  end <- regexpr("\n  }\n", rest, fixed = TRUE)
  body <- substring(rest, 1L, end)
  expect_match(
    body,
    "var shared = window.CerebroSharedDatasetState;",
    fixed = TRUE
  )
  expect_match(body, "var sharedName = null;", fixed = TRUE)
  ## Not also at the top level of the script, before the function.
  before <- substring(js, 1L, start)
  expect_false(grepl(
    "\n    var shared = window.CerebroSharedDatasetState;",
    before,
    fixed = TRUE
  ))
})
