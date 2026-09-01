viewer_admin_test_core <- function() {
  path <- viewer_test_path("admin", "core.R")
  expect_true(file.exists(path), info = "Viewer Admin core must be bundled")
  runtime <- new.env(parent = globalenv())
  sys.source(path, envir = runtime)
  runtime
}

test_that("Viewer Admin reads its password only from the runtime environment", {
  runtime <- viewer_admin_test_core()
  options <- list(
    .share_admin = list(
      account = "owner",
      password_env = "CEREBRO_ADMIN_PASSWORD"
    )
  )
  env <- function(name, unset) {
    if (identical(name, "CEREBRO_ADMIN_PASSWORD")) {
      "runtime-password"
    } else {
      unset
    }
  }

  config <- runtime$viewer_admin_config(options, env = env)

  expect_identical(
    config,
    list(account = "owner", password = "runtime-password")
  )
  expect_true(runtime$viewer_admin_login(
    "owner",
    "runtime-password",
    config
  ))
  expect_false(runtime$viewer_admin_login("owner", "wrong", config))
  expect_null(runtime$viewer_admin_config(list(), env = env))
})

test_that("Viewer Admin fails closed when the runtime secret is unavailable", {
  runtime <- viewer_admin_test_core()
  options <- list(
    .share_admin = list(
      account = "owner",
      password_env = "CEREBRO_ADMIN_PASSWORD"
    )
  )

  expect_null(runtime$viewer_admin_config(
    options,
    env = function(name, unset) unset
  ))
})

test_that("Viewer Admin login throttling survives individual sessions", {
  runtime <- viewer_admin_test_core()
  key <- "owner@127.0.0.1"
  now <- as.POSIXct("2026-08-28 12:00:00", tz = "UTC")

  expect_true(runtime$viewer_admin_login_allowed(key, now))
  for (attempt in seq_len(5L)) {
    runtime$viewer_admin_login_failed(key, now + attempt)
  }
  expect_false(runtime$viewer_admin_login_allowed(key, now + 10))
  expect_true(runtime$viewer_admin_login_allowed(key, now + 16 * 60))
  runtime$viewer_admin_login_failed(key, now + 16 * 60)
  runtime$viewer_admin_login_succeeded(key)
  expect_true(runtime$viewer_admin_login_allowed(key, now + 16 * 60 + 1))
})

test_that("Viewer Admin route and session authorization are server-owned", {
  runtime <- viewer_admin_test_core()

  expect_true(runtime$viewer_admin_route("/admin"))
  expect_true(runtime$viewer_admin_route("/study/admin/"))
  expect_false(runtime$viewer_admin_route("/administrator"))
  expect_false(runtime$viewer_admin_route("/admin/links"))

  session <- list(userData = new.env(parent = emptyenv()))
  expect_false(runtime$viewer_is_admin(session))
  session$userData$viewer_auth <- list(
    authenticated = TRUE,
    user = "owner",
    is_admin = TRUE
  )
  expect_true(runtime$viewer_is_admin(session))
  session$userData$viewer_auth$is_admin <- FALSE
  expect_false(runtime$viewer_is_admin(session))
})

test_that("Viewer Admin HTTP wrapper maps only the page route to Shiny root", {
  runtime <- viewer_admin_test_core()
  seen <- character()
  app <- list(httpHandler = function(request) {
    seen <<- request$PATH_INFO
    request$PATH_INFO
  })
  wrapped <- runtime$viewer_admin_http_app(app)

  expect_identical(
    wrapped$httpHandler(list(REQUEST_METHOD = "GET", PATH_INFO = "/admin")),
    "/"
  )
  expect_identical(
    wrapped$httpHandler(list(REQUEST_METHOD = "POST", PATH_INFO = "/admin")),
    "/admin"
  )
  expect_identical(seen, "/admin")
})

test_that("Viewer Admin inventory exposes display fields without JSON", {
  runtime <- viewer_admin_test_core()
  rows <- data.frame(
    token = c("active", "revoked", "expired"),
    fingerprint = rep("fingerprint", 3L),
    dataset_label = rep("PBMC", 3L),
    creator = rep("alice", 3L),
    created_at = rep("2026-08-01T12:00:00Z", 3L),
    expires_at = c(
      "2026-11-26T12:00:00Z",
      "2026-11-26T12:00:00Z",
      "2026-08-27T12:00:00Z"
    ),
    revoked_at = c(NA_character_, "2026-08-26T12:00:00Z", NA_character_),
    stringsAsFactors = FALSE
  )

  records <- runtime$viewer_admin_records(
    rows,
    now = as.POSIXct("2026-08-28 12:00:00", tz = "UTC")
  )

  expect_length(records, 3L)
  expect_identical(records[[1L]]$dataset_label, "PBMC")
  expect_identical(
    vapply(records, `[[`, character(1), "status"),
    c("active", "revoked", "expired")
  )
  expect_identical(records[[2L]]$revoked_at, "2026-08-26T12:00:00Z")
  expect_false("json" %in% names(records[[1L]]))
})

test_that("Viewer Admin keeps recent inactive history and purges it after 30 days", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("coordinated_views", "share_store.R"),
    envir = runtime
  )
  store <- runtime$cv_share_store_open(withr::local_tempfile(), "test-viewer")
  withr::defer(DBI::dbDisconnect(store$con))
  now <- as.POSIXct("2026-08-28 12:00:00", tz = "UTC")
  add <- function(created, ttl) {
    runtime$cv_share_store_create(
      store,
      json = "{}",
      fingerprint = "dataset-a",
      now = now - created * 24L * 60L * 60L,
      ttl_seconds = ttl * 24L * 60L * 60L
    )$token
  }
  tokens <- c(
    active = add(1L, 90L),
    expired = add(10L, 1L),
    old_expired = add(40L, 1L),
    revoked = add(5L, 90L),
    old_revoked = add(40L, 90L)
  )
  runtime$cv_share_store_revoke_admin(
    store,
    tokens[["revoked"]],
    now - 2L * 86400L
  )
  runtime$cv_share_store_revoke_admin(
    store,
    tokens[["old_revoked"]],
    now - 35L * 86400L
  )

  rows <- runtime$cv_share_store_list(store, now = now)

  expect_setequal(
    rows$token,
    tokens[c("active", "expired", "revoked")]
  )
})

test_that("Viewer bundles the Admin surface without hard-coded credentials", {
  root <- viewer_test_path()
  expected <- c(
    "admin/UI.R",
    "admin/server.R",
    "www/admin.css",
    "www/admin.js"
  )
  expect_true(all(file.exists(file.path(root, expected))))

  ui <- paste(readLines(file.path(root, "admin/UI.R")), collapse = "\n")
  server <- paste(
    readLines(file.path(root, "admin/server.R")),
    collapse = "\n"
  )
  js <- paste(readLines(file.path(root, "www/admin.js")), collapse = "\n")
  app_ui <- paste(readLines(file.path(root, "shiny_UI.R")), collapse = "\n")
  app_server <- paste(
    readLines(file.path(root, "shiny_server.R")),
    collapse = "\n"
  )
  auth <- paste(readLines(file.path(root, "auth.R")), collapse = "\n")
  share_server <- paste(
    readLines(file.path(root, "coordinated_views/server.R")),
    collapse = "\n"
  )

  expect_match(ui, 'id = "viewer-admin-login"', fixed = TRUE)
  expect_match(ui, 'id = "viewer-admin-search"', fixed = TRUE)
  expect_match(ui, 'id = "viewer-admin-logout"', fixed = TRUE)
  expect_match(server, "viewer_admin_login_allowed", fixed = TRUE)
  expect_false(grepl("admin_login_failures", server, fixed = TRUE))
  expect_match(server, "cv_share_store_revoke_admin", fixed = TRUE)
  expect_match(server, 'input[["viewer_admin_logout"]]', fixed = TRUE)
  expect_match(js, "viewer_admin_login", fixed = TRUE)
  expect_match(js, "window.confirm", fixed = TRUE)
  expect_match(js, "viewer_admin_logout", fixed = TRUE)
  expect_match(app_ui, 'cerebro_css("admin.css")', fixed = TRUE)
  expect_match(
    app_ui,
    'div(id = "sidebar_item_admin_placeholder")',
    fixed = TRUE
  )
  expect_match(app_server, '"/viewer/admin/core.R"', fixed = TRUE)
  expect_match(app_server, '"/viewer/admin/server.R"', fixed = TRUE)
  expect_match(auth, "viewer_admin_config", fixed = TRUE)
  expect_match(
    share_server,
    "creator = cv_share_creator(session)",
    fixed = TRUE
  )
  expect_false(any(grepl("admin123", c(ui, server, js, auth), fixed = TRUE)))
})
