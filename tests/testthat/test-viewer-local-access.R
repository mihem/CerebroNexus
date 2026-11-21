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

test_that("the public shell does not bypass the session guard", {
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
  expect_equal(response$status, 200L)
  expect_match(as.character(response$content), "ping", fixed = TRUE)
  expect_false(env$viewer_prepared)
  closed <- FALSE
  session <- list(
    clientData = list(url_search = ""),
    sendCustomMessage = function(...) invisible(NULL),
    close = function() closed <<- TRUE
  )
  app$serverFuncSource()(NULL, NULL, session)
  expect_true(closed)
  expect_identical(env$prepared_count, 0L)
})

test_that("the public shell retains transport compatibility", {
  withr::local_options(cerebro.sockjs_compat = TRUE)
  app <- viewer_local_access_fixture()
  response <- app$httpHandler(list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET",
    QUERY_STRING = "",
    SCRIPT_NAME = ""
  ))
  expect_equal(response$status, 200L)
  expect_match(
    as.character(response$content),
    "cerebro_sockjs_binary",
    fixed = TRUE
  )
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
  env$viewer_document <- tempdir()
  env$viewer_document_capture <- TRUE
  env$viewerDocumentServe <- function(server, directory) {
    calls <<- c(calls, "serve")
  }
  env$viewerDocumentCapture <- function(...) {
    calls <<- c(calls, "capture")
  }
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
  expect_identical(
    env$.viewer_shared_resources$access_token,
    env$viewer_access_token()
  )
})

test_that("cached HTML stays public without generating a credential or reading data", {
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
  response <- app$httpHandler(request)
  expect_equal(response$status, 200L)
  expect_match(rawToChar(response$content), "cached-viewer-page", fixed = TRUE)
  expect_null(env$viewer_access$token)
  expect_false(env$viewer_prepared)
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

test_that("the first credential preserves adopted shared prefixes and counters", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- launchCerebro()
  env <- environment(app$serverFuncSource())
  ## Match the helper's canonical-root contract: Windows separators and
  ## symlinked temporary directories (as on macOS) must already be resolved.
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  directory <- file.path(root, "projections")
  dir.create(directory)
  writeLines("{}", file.path(root, "manifest.json"))
  env$.viewer_shared_resources$token <- strrep("c", 24)
  env$.viewer_shared_resources$adopted_marker <- "keep"
  env$.viewer_shared_resources$published <- TRUE
  registrations <- character()
  register <- function(prefix, directory) {
    registrations <<- c(registrations, prefix)
  }
  before <- env$viewerSharedResourcePrefix(
    "projection",
    directory,
    root,
    register
  )
  expect_match(before, "^cerebro-projection-")
  expect_length(registrations, 1L)
  env$viewer_access_token()
  after <- env$viewerSharedResourcePrefix(
    "projection",
    directory,
    root,
    register
  )
  expect_identical(after, before)
  expect_identical(registrations, before)
  expect_identical(env$.viewer_shared_resources$adopted_marker, "keep")
  expect_identical(env$.viewer_shared_resources$token, strrep("c", 24))
  expect_null(env$.viewer_shared_resources$published)
})

test_that("console addresses are built without optional cache code", {
  for (host in c("127.0.0.1", "0.0.0.0", "::", "::1", "[::1]")) {
    server <- list(getHost = function() host, getPort = function() 3838L)
    expected <- if (grepl(":", host, fixed = TRUE)) {
      "http://[::1]:3838/"
    } else {
      "http://127.0.0.1:3838/"
    }
    expect_identical(.viewerAccessAddress(server), expected)
  }
})

test_that("startup does not depend on an optional cached-document address helper", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- launchCerebro()
  env <- environment(app$serverFuncSource())
  env$viewer_document <- NULL
  env$viewerDocumentAddress <- function(...) {
    stop("optional cache helper unavailable")
  }
  env$viewerFirstViewPublishKept <- function(...) invisible(NULL)
  env$viewerSharedResourceHeaders <- function(...) invisible(NULL)
  prepared <- FALSE
  env$viewer_prepare_ui <- function() prepared <<- TRUE
  env$viewer_prepare <- function() TRUE
  original <- shiny::getShinyOption("server", default = NULL)
  withr::defer(shiny::shinyOptions(server = original))
  shiny::shinyOptions(
    server = list(
      isRunning = function() TRUE,
      getHost = function() "127.0.0.1",
      getPort = function() 3838
    )
  )
  expect_message(
    env$viewer_started(),
    "Open the Viewer at http://127.0.0.1:3838/"
  )
  deadline <- Sys.time() + 2
  while (!prepared && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  expect_true(prepared)
})

test_that("rerunning an app refreshes the token without changing global token options", {
  withr::local_options(cerebro.sockjs_compat = FALSE)
  app <- launchCerebro()
  env <- environment(app$serverFuncSource())
  before <- options()
  token <- env$viewer_access_token()
  env$.viewer_shared_resources$rotation_marker <- "remove"
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
  expect_null(env$.viewer_shared_resources$rotation_marker)
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

test_that("cerebroStart preserves the early shell but defers protected previews", {
  withr::local_options(CerebroNexus.ui_cache = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(inst_test_path("start.R"), env)
  early_attempts <- 0L
  early_options <- NULL
  received <- NULL
  env$sys.source <- function(file, envir) {
    if (identical(basename(file), "access_policy.R")) {
      return(base::sys.source(file, envir))
    }
    early_attempts <<- early_attempts + 1L
    envir$viewerEarlyServerStart <- function(root, host, port, options) {
      early_options <<- options
      NULL
    }
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
  dir.create(file.path(root, "viewer", "core"), recursive = TRUE)
  file.copy(
    viewer_test_path("core", "access_policy.R"),
    file.path(root, "viewer", "core", "access_policy.R")
  )
  libraries <- .libPaths()
  withr::defer(.libPaths(libraries))
  for (settings in list(
    list(mode = "open"),
    list(mode = "open", require_token = TRUE),
    list(mode = "closed", require_token = TRUE),
    list(mode = "open", require_token = TRUE, host = "0.0.0.0")
  )) {
    do.call(env$cerebroStart, c(settings, list(root = root)))
    expect_true(early_options$.defer_first_views)
    expect_identical(
      received$options$host,
      if (is.null(settings$host)) "127.0.0.1" else settings$host
    )
  }
  expect_error(
    env$cerebroStart(root = root, require_token = "invalid"),
    "require_token"
  )
  expect_identical(early_attempts, 4L)
  for (settings in list(
    list(mode = "closed"),
    list(mode = "open", require_token = FALSE),
    list(mode = "open", host = "0.0.0.0")
  )) {
    do.call(env$cerebroStart, c(settings, list(root = root)))
    expect_false(early_options$.defer_first_views)
  }
  expect_identical(early_attempts, 7L)
})
