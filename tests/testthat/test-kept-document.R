## The page as a file (inst/viewer/core/kept_document.R), and launchCerebro()
## starting the web server before it prepares the Viewer.

kept_document_env <- function() {
  env <- new.env()
  sys.source(viewer_test_path("ui_cache.R"), env)
  sys.source(viewer_test_path("core", "kept_document.R"), env)
  env
}

kept_document_page <- function() {
  charToRaw(paste0(
    "<!DOCTYPE html>\n<html><head>",
    "<script src=\"shiny-javascript-1.0/shiny.min.js\"></script>",
    "</head><body>page</body></html>"
  ))
}

test_that("a kept document comes back as a file, with its resource paths", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- kept_document_env()
  ## the files of an installed package, as the scripts of a page are
  scripts <- normalizePath(system.file(package = "withr"), winslash = "/")
  prefix <- "keptdocumenttestpath"
  shiny::addResourcePath(prefix, scripts)
  withr::defer(try(shiny::removeResourcePath(prefix), silent = TRUE))
  ## paths of this process only are not kept: the Viewer's own, and what is
  ## in the temporary directory
  process_prefix <- "cerebro-first-view-test"
  shiny::addResourcePath(process_prefix, scripts)
  withr::defer(try(shiny::removeResourcePath(process_prefix), silent = TRUE))
  temporary_prefix <- "keptdocumenttesttemporary"
  shiny::addResourcePath(temporary_prefix, withr::local_tempdir())
  withr::defer(try(shiny::removeResourcePath(temporary_prefix), silent = TRUE))

  html <- kept_document_page()
  file <- env$viewerDocumentKeep("key", html)
  expect_identical(basename(file), "key.document.rds")
  kept <- readRDS(file)
  expect_identical(kept$html, html)
  expect_identical(kept$resource_paths[[prefix]], scripts)
  expect_false(process_prefix %in% names(kept$resource_paths))
  expect_false(temporary_prefix %in% names(kept$resource_paths))
  expect_true(all(c("shiny", "htmltools") %in% kept$packages$package))

  ## A process that has not rendered the page: the paths are registered and
  ## the document is a file to serve.
  shiny::removeResourcePath(prefix)
  target <- withr::local_tempdir()
  expect_identical(env$viewerDocumentRestore("key", target = target), target)
  expect_identical(
    readBin(file.path(target, "index.html"), "raw", n = 1e6),
    html
  )
  expect_identical(
    normalizePath(shiny::resourcePaths()[[prefix]], winslash = "/"),
    scripts
  )
  expect_null(env$viewerDocumentRestore("nothing-kept", target = target))

  ## A name another App of the process serves from elsewhere stays that
  ## App's, and the document is left for a process without it.
  elsewhere <- normalizePath(system.file(package = "testthat"), winslash = "/")
  shiny::addResourcePath(prefix, elsewhere)
  expect_null(env$viewerDocumentRestore("key", target = target))
  expect_identical(
    normalizePath(shiny::resourcePaths()[[prefix]], winslash = "/"),
    elsewhere
  )
  expect_true(file.exists(file))

  ## What is not a Shiny page is not kept.
  expect_null(env$viewerDocumentKeep("other", charToRaw("<html>404</html>")))
  expect_null(env$viewerDocumentKeep("other", "not raw"))
  expect_false(file.exists(env$viewerDocumentFile("other")))
})

test_that("a kept document is used only while its packages and paths are there", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- kept_document_env()
  target <- withr::local_tempdir()
  file <- env$viewerDocumentKeep("key", kept_document_page())
  kept <- readRDS(file)
  expect_false(is.null(env$viewerDocumentRestore("key", target = target)))

  changed <- kept
  changed$packages$version[[1L]] <- "0.0.0"
  saveRDS(changed, file)
  expect_null(env$viewerDocumentRestore("key", target = target))
  expect_false(file.exists(file))

  moved <- kept
  moved$resource_paths[[1L]] <- file.path(tempdir(), "no-such-directory")
  saveRDS(moved, file)
  expect_null(env$viewerDocumentRestore("key", target = target))

  writeLines("not an rds file", file)
  expect_null(env$viewerDocumentRestore("key", target = target))
})

test_that("the web server answers the page from the file, and Shiny the rest", {
  env <- kept_document_env()
  ## no bundle written by another test of this process
  unlink(env$viewerDocumentBundleDirectory(), recursive = TRUE)
  directory <- withr::local_tempdir()
  writeBin(kept_document_page(), file.path(directory, "index.html"))
  paths <- NULL
  server <- list(
    setStaticPath = function(.list) paths <<- .list,
    getHost = function() "0.0.0.0",
    getPort = function() 4321L
  )
  expect_true(env$viewerDocumentServe(server, directory))
  expect_named(paths, "/")
  options <- paths[["/"]]$options
  expect_true(options$indexhtml)
  ## every other path still reaches Shiny: the WebSocket, a session's files
  expect_true(options$fallthrough)
  expect_identical(options$headers[["Cache-Control"]], "no-cache")

  ## the address this machine reaches the server at
  expect_identical(env$viewerDocumentAddress(server), "http://127.0.0.1:4321/")
  server$getHost <- function() "::1"
  expect_identical(env$viewerDocumentAddress(server), "http://[::1]:4321/")
  server$getHost <- function() "192.168.1.5"
  expect_identical(
    env$viewerDocumentAddress(server),
    "http://192.168.1.5:4321/"
  )
})

test_that("the document a process sends is kept once it has been fetched", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- kept_document_env()
  server <- list(getHost = function() "127.0.0.1", getPort = function() 4321L)
  asked <- NULL
  html <- kept_document_page()
  ## a fetch that has answered, as a resolved mirai does
  env$viewerDocumentCapture(server, "key", fetch = function(address) {
    asked <<- address
    list(data = html)
  })
  expect_identical(asked, "http://127.0.0.1:4321/")
  expect_identical(readRDS(env$viewerDocumentFile("key"))$html, html)
  ## a fetch that failed keeps nothing
  env$viewerDocumentCapture(server, "failed", fetch = function(address) {
    list(data = structure("connection refused", class = "errorValue"))
  })
  expect_false(file.exists(env$viewerDocumentFile("failed")))
})

test_that("launchCerebro returns before the Viewer is prepared", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  object <- Cerebro$new()
  cells <- c("cell-1", "cell-2", "cell-3")
  object$setMetaData(data.frame(cell_barcode = cells, row.names = cells))
  object$setExpressionBackend("embedded")
  crb <- withr::local_tempfile(fileext = ".crb")
  saveRDS(object, crb)

  app <- launchCerebro(mode = "closed", crb_file_to_load = crb)
  expect_s3_class(app, "shiny.appobj")
  env <- environment(app$serverFuncSource())
  ## nothing read yet: the data sets, the page and the server come with
  ## viewer_prepare()
  expect_false(env$viewer_prepared)
  expect_null(env$Cerebro.options[[".dataset_catalog"]])
  expect_false(exists("ui", envir = env, inherits = FALSE))

  ## A request for the page before that gets the kept document as it is.
  document <- withr::local_tempdir()
  writeBin(kept_document_page(), file.path(document, "index.html"))
  env$viewer_document <- document
  response <- env$viewer_ui(NULL)
  expect_s3_class(response, "httpResponse")
  expect_identical(response$status, 200L)
  expect_identical(response$content, kept_document_page())
  expect_false(env$viewer_prepared)

  ## Prepared once; then the page is the Viewer's own.
  expect_true(env$viewer_prepare())
  expect_true(env$viewer_prepared)
  expect_named(env$Cerebro.options[[".dataset_catalog"]], crb)
  expect_false(env$viewer_prepare())
  page <- env$viewer_ui(NULL)
  expect_false(inherits(page, "httpResponse"))
  expect_identical(page, env$ui)
  expect_true(is.function(env$server))
})

test_that("what preparing the Viewer reports is reported again, not retried", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  app <- launchCerebro(mode = "closed")
  env <- environment(app$serverFuncSource())
  calls <- 0L
  ## as when a CRB file cannot be read
  env$.datasetCatalogFromFiles <- function(files, ...) {
    calls <<- calls + 1L
    stop("this CRB file cannot be read")
  }
  expect_error(env$viewer_prepare(), "cannot be read")
  expect_error(env$viewer_prepare(), "cannot be read")
  expect_error(env$viewer_server(NULL, NULL, NULL), "cannot be read")
  expect_identical(calls, 1L)
  expect_false(env$viewer_prepared)
})

test_that("a started App stops with what preparing the Viewer reports", {
  skip_on_cran()
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  crb <- withr::local_tempfile(fileext = ".crb")
  saveRDS(list(not = "a Cerebro object"), crb)
  app <- launchCerebro(mode = "closed", crb_file_to_load = crb)
  ## launchCerebro() has not read the file; the App does once it runs
  expect_s3_class(app, "shiny.appobj")
  expect_error(
    shiny::runApp(
      app,
      port = httpuv::randomPort(),
      launch.browser = FALSE,
      quiet = TRUE
    ),
    "recognized Cerebro object"
  )
})

test_that("byte-code compiling is switched off while Apps run, and back on after the last", {
  skip_if(interactive())
  level <- compiler::enableJIT(3L)
  withr::defer(compiler::enableJIT(level))
  withr::defer(options(CerebroNexus.jit_level = NULL))
  jit <- function() compiler::enableJIT(-1L)

  ## two Apps that overlap: the level to return to is the one before the first
  first <- .viewerJitSuspend()
  expect_identical(jit(), 0L)
  second <- .viewerJitSuspend()
  expect_identical(jit(), 0L)
  first()
  first()
  expect_identical(jit(), 0L)
  second()
  expect_identical(jit(), 3L)

  ## an App that was created and never run does nothing: the switch happens
  ## when it starts
  app <- launchCerebro(mode = "closed")
  expect_identical(jit(), 3L)

  ## start.R has switched it off already, and says what to put back
  options(CerebroNexus.jit_level = 2L)
  compiler::enableJIT(0L)
  release <- .viewerJitSuspend()
  release()
  expect_identical(jit(), 2L)
  expect_null(getOption("CerebroNexus.jit_level"))
})

test_that("the key of a document says how the page is sent", {
  env <- kept_document_env()
  withr::local_options(shiny.http.response.filter = NULL)
  plain <- env$viewerDocumentTransport(FALSE)
  expect_identical(env$viewerDocumentTransport(FALSE), plain)
  expect_false(identical(env$viewerDocumentTransport(TRUE), plain))

  ## Shiny Server inserts its scripts with a filter that holds their text.
  filter <- local({
    inject <- "<script src=\"__assets__/sockjs.min.js\"></script></head>"
    function(...) list(...)[[...length()]]
  })
  withr::local_options(shiny.http.response.filter = filter)
  served <- env$viewerDocumentTransport(TRUE)
  expect_false(identical(served, env$viewerDocumentTransport(FALSE)))
  expect_false(identical(served, plain))
  ## another configuration of the server, another page
  environment(
    filter
  )$inject <- "<script>preShinyInit({reconnect:false});</script></head>"
  expect_false(identical(env$viewerDocumentTransport(TRUE), served))
  ## a filter of another kind counts by what it does
  withr::local_options(shiny.http.response.filter = function(
    request,
    response
  ) {
    response
  })
  other <- env$viewerDocumentTransport(TRUE)
  expect_false(identical(other, served))
  withr::local_options(shiny.http.response.filter = function(
    request,
    response
  ) {
    NULL
  })
  expect_false(identical(env$viewerDocumentTransport(TRUE), other))
})

test_that("the fetch carries the secret Shiny Server gave the process", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  env <- kept_document_env()
  env$.viewerMirai <- function(.expr, ...) {
    expr <- substitute(.expr)
    mirai::mirai(expr, ...)
  }
  seen <- list()
  server <- httpuv::startServer(
    "127.0.0.1",
    httpuv::randomPort(),
    list(call = function(request) {
      seen[[length(seen) + 1L]] <<- request$HTTP_SHINY_SHARED_SECRET
      list(
        status = 200L,
        headers = list("Content-Type" = "text/html"),
        body = "page"
      )
    })
  )
  withr::defer(server$stop())
  fetch <- function() {
    task <- env$viewerDocumentFetch(env$viewerDocumentAddress(server))
    waited <- 0
    while (mirai::unresolved(task) && waited < 30) {
      later::run_now(0.05)
      waited <- waited + 0.05
    }
    task$data
  }
  withr::local_options(shiny.sharedSecret = "a-secret")
  expect_identical(fetch(), charToRaw("page"))
  expect_identical(seen[[1L]], "a-secret")
  ## no header where there is no secret
  withr::local_options(shiny.sharedSecret = NULL)
  expect_identical(fetch(), charToRaw("page"))
  expect_null(seen[2L][[1L]])
})
