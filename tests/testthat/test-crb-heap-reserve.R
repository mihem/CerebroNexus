crb_cache <- new.env()
sys.source(viewer_test_path("crb_cache.R"), crb_cache)

test_that("only a large configured CRB reserves heap", {
  small <- withr::local_tempfile(fileext = ".crb")
  writeBin(raw(1024), small)
  expect_false(crb_cache$.reserveCrbHeap(NULL))
  expect_false(crb_cache$.reserveCrbHeap(c(
    small,
    file.path(tempdir(), "missing.crb"),
    "variable_name"
  )))
  large <- withr::local_tempfile(fileext = ".crb")
  writeBin(raw(17 * 1024^2), large)
  expect_true(crb_cache$.reserveCrbHeap(c(small, large)))
})

test_that("launchCerebro reserves heap for a large CRB before reading it", {
  ## with the Viewer's helper, not a copy of it
  expect_false(exists(".reserveHeapForCrb", mode = "function"))

  ## The data sets are read after the UI is built, the server functions are
  ## loaded (which starts the background work on the configured files) and the
  ## heap is raised, and the Viewer does not raise the heap again.
  launch <- deparse(body(launchCerebro))
  position <- function(pattern) grep(pattern, launch, fixed = TRUE)[[1L]]
  expect_lt(position("viewer/shiny_UI.R"), position("viewer/shiny_server.R"))
  expect_lt(position("viewer/shiny_server.R"), position(".reserveCrbHeap("))
  expect_lt(
    position(".reserveCrbHeap("),
    position(".datasetCatalogFromFiles(")
  )
  expect_lt(
    position(".datasetCatalogFromFiles("),
    position("viewerLaunchPrototypes(")
  )
  ## and collected once after the first session has its page
  expect_lt(
    position("viewerLaunchPrototypes("),
    position(".collectBeforeSessions(")
  )
  expect_lt(position(".collectBeforeSessions("), position("shiny::shinyApp"))
  ## it reports whether a second collection had to enlarge the heap
  expect_type(.collectBeforeSessions(), "logical")
  server_file <- readLines(viewer_test_path("shiny_server.R"))
  reserve <- grep(".reserveCrbHeap(", server_file, fixed = TRUE)
  expect_length(reserve, 1L)
  expect_match(
    server_file[[reserve - 1L]],
    "if (!.viewer_launch_reads_datasets)",
    fixed = TRUE
  )
  expect_true(any(grepl(".launch_reads_datasets", launch, fixed = TRUE)))
})
