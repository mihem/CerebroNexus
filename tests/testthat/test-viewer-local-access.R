test_that("automatic Viewer access protection is limited to local open mode", {
  for (host in c("127.0.0.1", "127.0.0.2", "localhost", "::1", "[::1]")) {
    expect_true(.viewerAccessRequired("open", NULL, host))
    expect_false(.viewerAccessRequired("closed", NULL, host))
    expect_false(.viewerAccessRequired("open", FALSE, host))
  }
  for (host in c("0.0.0.0", "::", "192.168.1.2", "127.0.0.1.attacker.test")) {
    expect_false(.viewerAccessRequired("open", NULL, host))
    expect_true(.viewerAccessRequired("open", TRUE, host))
  }
  for (invalid in list(NA, "auto", 1, logical(), c(TRUE, FALSE))) {
    expect_error(launchCerebro(require_token = invalid), "require_token")
  }
})

test_that("Viewer query tokens must match exactly", {
  token <- strrep("ab", 32)
  expect_true(.viewerAccessAllowed(paste0("?token=", token), token))
  expect_true(.viewerAccessAllowed(paste0("?dataset=a&token=", token), token))
  for (query in list(
    NULL,
    "",
    NA_character_,
    character(),
    c("a", "b"),
    "?token=",
    paste0("?token=", strrep("ac", 32)),
    paste0("?token=", token, "0"),
    "?token=%00",
    paste0("?token2=", token)
  )) {
    expect_false(.viewerAccessAllowed(query, token))
  }
  expect_true(.viewerAccessAllowed("", NULL))
  expect_true(.viewerAccessAllowed(
    "",
    token,
    paste0("/cerebro-", token, ".html")
  ))
  expect_true(.viewerAccessAllowed(
    "",
    token,
    paste0("/proxy/cerebro-", token, ".html")
  ))
  expect_false(.viewerAccessAllowed("", token, "/cerebro-wrong.html"))
})

test_that("authorized Viewer URLs retain dataset queries and fragments", {
  expect_identical(
    .viewerAccessUrl("http://localhost:3838/sample?view=umap#panel", "abc"),
    "http://localhost:3838/sample?view=umap&token=abc#panel"
  )
  expect_identical(
    .viewerAccessUrl("http://localhost:3838/?", "abc"),
    "http://localhost:3838/?token=abc"
  )
  expect_identical(
    .viewerAccessUrl("http://localhost/", NULL),
    "http://localhost/"
  )
  expect_identical(
    .viewerAccessUrl("http://localhost/?token=old&dataset=a#view", "new"),
    "http://localhost/?dataset=a&token=new#view"
  )
})

test_that("page and session guards run before Viewer initialization", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- viewer_local_access_fixture()
  env <- environment(app$serverFuncSource())
  expect_false(env$viewer_prepared)
  response <- app$httpHandler(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    QUERY_STRING = "",
    SCRIPT_NAME = ""
  ))
  expect_identical(response$status, 403L)
  expect_match(response$content, "Access token required", fixed = TRUE)
  expect_false(env$viewer_prepared)
  closed <- FALSE
  session <- list(
    clientData = list(url_search = ""),
    close = function() closed <<- TRUE
  )
  app$serverFuncSource()(NULL, NULL, session)
  expect_true(closed)
  expect_identical(env$prepared_count, 0L)
})

test_that("transport compatibility does not wrap an access-denied page", {
  withr::local_options(cerebro.sockjs_compat = TRUE)
  app <- launchCerebro()
  response <- app$httpHandler(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    QUERY_STRING = "",
    SCRIPT_NAME = ""
  ))
  expect_identical(response$status, 403L)
  expect_false(grepl("transport_compat", response$content, fixed = TRUE))
})

test_that("protected Viewers print their URL and preserve browser callbacks", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  opened <- NULL
  app <- launchCerebro(
    options = list(
      launch.browser = function(url) opened <<- url
    )
  )
  expect_message(
    authorized <- app$options$launch.browser("http://127.0.0.1:3838"),
    "Open the Viewer at"
  )
  expect_identical(opened, authorized)
  expect_match(authorized, "^http://127[.]0[.]0[.]1:3838[?]token=[0-9a-f]{64}$")
  other <- launchCerebro()
  expect_false(identical(
    environment(app$serverFuncSource())$viewer_access_token(),
    environment(other$serverFuncSource())$viewer_access_token()
  ))
  expect_null(launchCerebro(mode = "closed")$options$launch.browser)
  expect_null(launchCerebro(require_token = FALSE)$options$launch.browser)
})

test_that("protected startup retains authorized cached pages and previews", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- launchCerebro()
  env <- environment(app$serverFuncSource())
  calls <- character()
  served_token <- NULL
  fetched_address <- NULL
  env$viewer_document <- tempdir()
  env$viewer_document_capture <- TRUE
  env$viewerDocumentServe <- function(server, directory, access_token) {
    calls <<- c(calls, "serve")
    served_token <<- access_token
  }
  env$viewerDocumentCapture <- function(server, key, fetch) {
    calls <<- c(calls, "capture")
    fetch("http://127.0.0.1:3838/")
  }
  env$viewerDocumentFetch <- function(address) fetched_address <<- address
  env$viewerFirstViewPublishKept <- function(...) calls <<- c(calls, "preview")
  env$viewerSharedResourceHeaders <- function(...) invisible(NULL)
  env$viewer_prepare_ui <- function() invisible(NULL)
  env$viewer_prepare <- function() TRUE
  env$.viewerWhenQuiet <- function(fn) fn()
  original <- shiny::getShinyOption("server", default = NULL)
  withr::defer(shiny::shinyOptions(server = original))
  shiny::shinyOptions(
    server = list(
      isRunning = function() TRUE,
      getHost = function() "127.0.0.1",
      getPort = function() 3838
    )
  )
  expect_message(env$viewer_started(), "Open the Viewer at")
  deadline <- Sys.time() + 2
  while (!"capture" %in% calls && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  expect_setequal(calls, c("serve", "preview", "capture"))
  expect_identical(served_token, env$viewer_access_token())
  expect_identical(
    fetched_address,
    paste0("http://127.0.0.1:3838/?token=", served_token)
  )
})

test_that("restoring a cached page cannot bypass the page check", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- launchCerebro()
  env <- environment(app$serverFuncSource())
  document <- withr::local_tempdir()
  writeLines(
    "<!DOCTYPE html><html><body>cached-viewer-page</body></html>",
    file.path(document, "index.html")
  )
  env$viewer_document <- document
  request <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    QUERY_STRING = "",
    SCRIPT_NAME = ""
  )
  expect_identical(app$httpHandler(request)$status, 403L)
  request$QUERY_STRING <- paste0("?token=", env$viewer_access_token())
  response <- app$httpHandler(request)
  expect_identical(response$status, 200L)
  expect_match(rawToChar(response$content), "cached-viewer-page", fixed = TRUE)
})

test_that("auto mode uses the running server's address, not just launch options", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- launchCerebro(options = list(host = "127.0.0.1"))
  env <- environment(app$serverFuncSource())
  original <- shiny::getShinyOption("server", default = NULL)
  withr::defer(shiny::shinyOptions(server = original))
  shiny::shinyOptions(
    server = list(
      isRunning = function() TRUE,
      getHost = function() "0.0.0.0"
    )
  )
  expect_null(env$viewer_access_token())
  forced <- launchCerebro(require_token = TRUE)
  expect_match(
    environment(forced$serverFuncSource())$viewer_access_token(),
    "^[0-9a-f]{64}$"
  )
})

test_that("rerunning an app refreshes the token without changing global token options", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- launchCerebro()
  env <- environment(app$serverFuncSource())
  before <- options()
  token <- env$viewer_access_token()
  cleanup <- list()
  local_mocked_bindings(
    onStop = function(fn, ...) cleanup[[length(cleanup) + 1L]] <<- fn,
    .package = "shiny"
  )
  local_mocked_bindings(
    later = function(...) invisible(NULL),
    .package = "later"
  )
  env$.viewerJitSuspend <- function() function() invisible(NULL)
  app$onStart()
  withr::defer(
    for (fn in cleanup) {
      fn()
    }
  )
  expect_false(identical(env$viewer_access_token(), token))
  expect_identical(
    getOption("CerebroNexus.builder.access_token"),
    before[["CerebroNexus.builder.access_token"]]
  )
  expect_false("CerebroNexus.viewer.access_token" %in% names(options()))
  prefix <- paste0("cerebro-first-view-", env$viewer_access_token())
  unrelated <- "viewerlocalaccessunrelated"
  shiny::addResourcePath(prefix, tempdir())
  shiny::addResourcePath(unrelated, tempdir())
  withr::defer(try(shiny::removeResourcePath(unrelated), silent = TRUE))
  for (fn in cleanup) {
    fn()
  }
  expect_false(prefix %in% names(shiny::resourcePaths()))
  expect_true(unrelated %in% names(shiny::resourcePaths()))
})

test_that("cerebroStart never starts an unchecked early server for protected launches", {
  env <- new.env(parent = globalenv())
  sys.source(inst_test_path("start.R"), env)
  early_attempts <- 0L
  received <- NULL
  env$sys.source <- function(...) {
    early_attempts <<- early_attempts + 1L
    stop("test: early server not needed")
  }
  env$loadNamespace <- function(...) {
    list(
      launchCerebro = function(...) {
        received <<- list(...)
        list()
      }
    )
  }
  local_mocked_bindings(
    runApp = function(...) invisible(NULL),
    .package = "shiny"
  )
  root <- withr::local_tempdir()
  writeLines(
    c("Package: CerebroNexus", "Version: 0.0.1"),
    file.path(root, "DESCRIPTION")
  )
  libraries <- .libPaths()
  withr::defer(.libPaths(libraries))
  for (settings in list(
    list(mode = "open"),
    list(mode = "open", require_token = TRUE),
    list(mode = "closed", require_token = TRUE)
  )) {
    do.call(env$cerebroStart, c(settings, list(root = root)))
    expect_identical(early_attempts, 0L)
    expect_identical(received$options$host, "127.0.0.1")
  }
  expect_error(
    env$cerebroStart(root = root, require_token = "invalid"),
    "require_token"
  )
  expect_identical(early_attempts, 0L)
  env$cerebroStart(root = root, mode = "open", require_token = FALSE)
  expect_identical(early_attempts, 1L)
})
