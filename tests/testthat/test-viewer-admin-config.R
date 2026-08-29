test_that("Shared Link expiry accepts explicit Viewer retention", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("coordinated_views", "share_store.R"),
    envir = runtime
  )
  store <- runtime$cv_share_store_open(withr::local_tempfile(), "test-viewer")
  withr::defer(DBI::dbDisconnect(store$con))
  now <- as.POSIXct("2026-08-28 12:00:00", tz = "UTC")

  short <- runtime$cv_share_store_create(
    store,
    json = "{}",
    fingerprint = "dataset-a",
    now = now,
    ttl_seconds = 7L * 24L * 60L * 60L
  )
  long <- runtime$cv_share_store_create(
    store,
    json = "{}",
    fingerprint = "dataset-a",
    now = now,
    ttl_seconds = 90L * 24L * 60L * 60L
  )

  expect_identical(short$expires_at, "2026-09-04T12:00:00Z")
  expect_identical(long$expires_at, "2026-11-26T12:00:00Z")
})

test_that("Shared Links use long retention only when Admin is usable", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("admin", "core.R"),
    envir = runtime
  )
  options <- list(
    .share_admin = list(
      account = "owner",
      password_env = "CEREBRO_ADMIN_PASSWORD"
    )
  )
  missing <- function(name, unset) unset
  configured <- function(name, unset) "a-runtime-password"

  expect_identical(runtime$viewer_admin_share_ttl(list(), missing), 604800L)
  expect_identical(runtime$viewer_admin_share_ttl(options, missing), 604800L)
  expect_identical(
    runtime$viewer_admin_share_ttl(options, configured),
    7776000L
  )
})
