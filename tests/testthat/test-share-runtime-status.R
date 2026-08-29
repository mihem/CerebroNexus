viewer_text <- function(path) {
  paste(
    readLines(viewer_test_path(path), warn = FALSE),
    collapse = "\n"
  )
}

test_that("an unavailable share store reports a stable deployment error", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("coordinated_views", "share_store.R"),
    envir = runtime
  )

  expect_error(
    runtime$cv_share_store_open(""),
    class = "cv_share_error"
  )
  expect_error(
    runtime$cv_share_store_open(""),
    "not configured"
  )
})

test_that("the server publishes actual Share availability and retention", {
  server <- viewer_text("coordinated_views/server.R")

  expect_match(server, "coordviews_share_status", fixed = TRUE)
  expect_match(server, "conditionMessage(error)", fixed = TRUE)
  expect_match(server, "CEREBRONEXUS_LINKED_VIEW_SHARE_DB", fixed = TRUE)
  expect_match(server, "warning", fixed = TRUE)
  expect_match(server, "ttl_days", fixed = TRUE)
})

test_that("the Share dialog reflects server status instead of promising 90 days", {
  ui <- viewer_text("coordinated_views/UI.R")
  client <- viewer_text("www/coordviews-config.js")

  expect_match(ui, 'id = "cv-share-retention"', fixed = TRUE)
  expect_false(grepl("expires after 90 days", ui, fixed = TRUE))
  expect_match(client, "coordviews_share_status", fixed = TRUE)
  expect_match(client, "shareAvailable", fixed = TRUE)
  expect_match(
    client,
    "Set CEREBRONEXUS_LINKED_VIEW_SHARE_DB",
    fixed = TRUE
  )
})
