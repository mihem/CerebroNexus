viewer_hardening_text <- function(path) {
  paste(
    readLines(viewer_test_path(path), warn = FALSE),
    collapse = "\n"
  )
}

viewer_share_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("coordinated_views", "share_store.R"),
    envir = runtime
  )
  runtime
}

test_that("Shared Link records are isolated by Viewer namespace", {
  runtime <- viewer_share_runtime()
  path <- withr::local_tempfile()
  first <- runtime$cv_share_store_open(path, "viewer-a")
  second <- runtime$cv_share_store_open(path, "viewer-b")
  withr::defer(DBI::dbDisconnect(first$con))
  withr::defer(DBI::dbDisconnect(second$con))
  created <- runtime$cv_share_store_create(
    first,
    json = "{}",
    fingerprint = "dataset-a"
  )
  token <- created$token

  expect_identical(runtime$cv_share_store_list(second)$token, character())
  expect_error(
    runtime$cv_share_store_fetch(second, token, "dataset-a"),
    "unavailable"
  )
  expect_error(
    runtime$cv_share_store_revoke_admin(second, token),
    "unavailable"
  )
  expect_identical(
    runtime$cv_share_store_fetch(first, token, "dataset-a")$json,
    "{}"
  )
})

test_that("Shared Link creation has a persistent per-creator rate limit", {
  runtime <- viewer_share_runtime()
  runtime$CV_SHARE_RATE_LIMIT <- 2L
  store <- runtime$cv_share_store_open(withr::local_tempfile(), "viewer-a")
  withr::defer(DBI::dbDisconnect(store$con))
  now <- as.POSIXct("2026-08-28 12:00:00", tz = "UTC")

  for (attempt in seq_len(2L)) {
    runtime$cv_share_store_create(
      store,
      json = "{}",
      fingerprint = "dataset-a",
      creator = "anonymous",
      now = now
    )
  }

  error <- tryCatch(
    runtime$cv_share_store_create(
      store,
      json = "{}",
      fingerprint = "dataset-a",
      creator = "anonymous",
      now = now
    ),
    error = identity
  )
  expect_s3_class(error, "cv_share_error")
  expect_identical(error$code, "share_limit")
})

test_that("anonymous share quotas are scoped by client identity", {
  runtime <- viewer_share_runtime()

  expect_identical(
    runtime$cv_share_creator_id(user = "owner"),
    "user:owner"
  )
  expect_false(identical(
    runtime$cv_share_creator_id(remote_addr = "192.0.2.1"),
    runtime$cv_share_creator_id(remote_addr = "192.0.2.2")
  ))
  expect_false(identical(
    runtime$cv_share_creator_id(session_token = "session-a"),
    runtime$cv_share_creator_id(session_token = "session-b")
  ))
})

test_that("Shared Link creation has an active per-creator quota", {
  runtime <- viewer_share_runtime()
  runtime$CV_SHARE_RATE_LIMIT <- 100L
  runtime$CV_SHARE_MAX_ACTIVE_PER_CREATOR <- 1L
  store <- runtime$cv_share_store_open(withr::local_tempfile(), "viewer-a")
  withr::defer(DBI::dbDisconnect(store$con))

  runtime$cv_share_store_create(
    store,
    json = "{}",
    fingerprint = "dataset-a",
    creator = "client-a"
  )
  error <- tryCatch(
    runtime$cv_share_store_create(
      store,
      json = "{}",
      fingerprint = "dataset-a",
      creator = "client-a"
    ),
    error = identity
  )

  expect_s3_class(error, "cv_share_error")
  expect_identical(error$code, "share_limit")
})

test_that("Shared Link storage enforces a per-Viewer record quota", {
  runtime <- viewer_share_runtime()
  runtime$CV_SHARE_MAX_RECORDS <- 1L
  store <- runtime$cv_share_store_open(withr::local_tempfile(), "viewer-a")
  withr::defer(DBI::dbDisconnect(store$con))

  runtime$cv_share_store_create(
    store,
    json = "{}",
    fingerprint = "dataset-a",
    creator = "first"
  )
  error <- tryCatch(
    runtime$cv_share_store_create(
      store,
      json = "{}",
      fingerprint = "dataset-a",
      creator = "second"
    ),
    error = identity
  )

  expect_s3_class(error, "cv_share_error")
  expect_identical(error$code, "share_limit")
})

test_that("revoked and expired shares stop consuming active capacity", {
  runtime <- viewer_share_runtime()
  runtime$CV_SHARE_RATE_LIMIT <- 100L
  runtime$CV_SHARE_MAX_RECORDS <- 1L
  store <- runtime$cv_share_store_open(withr::local_tempfile(), "viewer-a")
  withr::defer(DBI::dbDisconnect(store$con))
  now <- as.POSIXct("2026-08-28 12:00:00", tz = "UTC")

  revoked <- runtime$cv_share_store_create(
    store,
    json = "{}",
    fingerprint = "dataset-a",
    creator = "client-a",
    now = now
  )
  runtime$cv_share_store_revoke_admin(store, revoked$token, now = now)
  expect_no_error(runtime$cv_share_store_create(
    store,
    json = "{}",
    fingerprint = "dataset-a",
    creator = "client-b",
    now = now
  ))

  other <- runtime$cv_share_store_open(withr::local_tempfile(), "viewer-b")
  withr::defer(DBI::dbDisconnect(other$con))
  runtime$cv_share_store_create(
    other,
    json = "{}",
    fingerprint = "dataset-a",
    creator = "client-a",
    now = now,
    ttl_seconds = 60L
  )
  expect_no_error(runtime$cv_share_store_create(
    other,
    json = "{}",
    fingerprint = "dataset-a",
    creator = "client-b",
    now = now + 61L
  ))
})

test_that("Shared Link payload quota recovers after expired records are purged", {
  runtime <- viewer_share_runtime()
  runtime$CV_SHARE_MAX_STORE_BYTES <- 300L
  store <- runtime$cv_share_store_open(withr::local_tempfile(), "viewer-a")
  withr::defer(DBI::dbDisconnect(store$con))
  now <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  runtime$cv_share_store_create(
    store,
    json = paste0('{"value":"', paste(rep("x", 240L), collapse = ""), '"}'),
    fingerprint = "dataset-a",
    now = now
  )
  error <- tryCatch(
    runtime$cv_share_store_create(
      store,
      json = paste0('{"value":"', paste(rep("y", 100L), collapse = ""), '"}'),
      fingerprint = "dataset-a",
      now = now
    ),
    error = identity
  )

  expect_s3_class(error, "cv_share_error")
  expect_identical(error$code, "share_limit")
  expect_no_error(runtime$cv_share_store_create(
    store,
    json = "{}",
    fingerprint = "dataset-a",
    now = now + 40L * 24L * 60L * 60L
  ))
})

test_that("Shared Link SQLite files are private on POSIX", {
  skip_on_os("windows")
  runtime <- viewer_share_runtime()
  path <- withr::local_tempfile()
  store <- runtime$cv_share_store_open(path, "viewer-a")
  withr::defer(DBI::dbDisconnect(store$con))

  expect_identical(as.integer(file.info(path)$mode), 384L)
})

test_that("Share URLs keep dataset identity and hide bearer tokens in fragments", {
  client <- viewer_hardening_text("www/coordviews-config.js")
  admin <- viewer_hardening_text("www/admin.js")
  server <- viewer_hardening_text("coordinated_views/server.R")

  expect_match(
    client,
    "url.searchParams.set('dataset', datasetLabel)",
    fixed = TRUE
  )
  expect_match(client, "url.hash = 'linked_view='", fixed = TRUE)
  expect_false(grepl(
    "url.searchParams.set('linked_view'",
    client,
    fixed = TRUE
  ))
  expect_match(client, "window.location.pathname", fixed = TRUE)
  expect_false(grepl("!linked || !linked.ready()", client, fixed = TRUE))
  expect_match(admin, "url.hash = 'linked_view='", fixed = TRUE)
  expect_match(
    server,
    "dataset_label = cv_selected_dataset_name()",
    fixed = TRUE
  )
  expect_false(grepl(
    '"private-data", "linked-view-shares.sqlite"',
    server,
    fixed = TRUE
  ))
})
