## The web server start.R starts before CerebroNexus is loaded
## (inst/viewer/core/early_server.R), and launchCerebro() continuing with it.

early_server_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "early_server.R"), env)
  env
}

## The package directory the early server reads the Viewer files from.
early_server_root <- function() {
  dirname(viewer_test_path())
}

early_server_options <- function(crb = NULL) {
  list(
    cerebro_version = "0.0.1",
    crb_file_to_load = crb,
    point_size = 5,
    point_opacity = 1,
    percentage_cells_to_show = 100
  )
}

early_server_page <- function() {
  charToRaw(paste0(
    "<!DOCTYPE html>\n<html><head>",
    "<script src=\"shiny-javascript-1.0/shiny.min.js\"></script>",
    "</head><body>early page</body></html>"
  ))
}

## Keeps a document for the Viewer files of this source tree, as a process
## that has sent its page does. Returns the directory its one resource path
## names.
early_server_keep <- function(html = early_server_page()) {
  kept <- new.env(parent = globalenv())
  sys.source(viewer_test_path("ui_cache.R"), kept)
  sys.source(viewer_test_path("core", "kept_document.R"), kept)
  files <- normalizePath(system.file(package = "withr"), winslash = "/")
  shiny::addResourcePath("earlyservertestfiles", files)
  withr::defer(
    try(shiny::removeResourcePath("earlyservertestfiles"), silent = TRUE),
    envir = parent.frame()
  )
  key <- kept$viewerUiCacheKey(
    file.path(early_server_root(), "viewer"),
    c(version = "0.0.1", www_prefix = "cerebro_www")
  )
  kept$viewerDocumentKeep(key, html)
  files
}

early_server_get <- function(port, path) {
  connection <- url(sprintf("http://127.0.0.1:%d%s", port, path), open = "rb")
  on.exit(close(connection))
  readBin(connection, "raw", n = 1e6)
}

test_that("without a kept document there is no early server", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- early_server_env()
  expect_null(env$viewerEarlyServerStart(
    early_server_root(),
    "127.0.0.1",
    httpuv::randomPort(),
    early_server_options()
  ))
  expect_false(env$viewerEarlyServerRunning(NULL))
})

test_that("the early server sends the kept page and its files by itself", {
  skip_on_cran()
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  files <- early_server_keep()
  env <- early_server_env()
  port <- httpuv::randomPort()
  early <- env$viewerEarlyServerStart(
    early_server_root(),
    "127.0.0.1",
    port,
    early_server_options()
  )
  withr::defer(try(early$server$stop(), silent = TRUE))
  expect_s3_class(early, "cerebro_early_server")
  expect_true(env$viewerEarlyServerRunning(early))
  expect_identical(early$port, port)
  expect_match(early$key, "^[0-9a-f]{32}$")
  ## The App it was given can be read back: Shiny's will be read the same way.
  expect_true(is.function(env$viewerEarlyServerShinyApp(early$server)$call))
  expect_null(env$viewerEarlyServerShinyApp(list()))

  ## Files are answered by the web server's own thread: R is busy here, as it
  ## is while it loads the Viewer.
  expect_identical(early_server_get(port, "/"), early_server_page())
  expect_identical(
    early_server_get(port, "/earlyservertestfiles/DESCRIPTION"),
    readBin(file.path(files, "DESCRIPTION"), "raw", n = 1e6)
  )
  early$server$stop()
  expect_false(env$viewerEarlyServerRunning(early))
})

test_that("requests reach the App of Shiny's server once that runs", {
  skip_on_cran()
  skip_if_not_installed("mirai")
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  early_server_keep()
  env <- early_server_env()
  port <- httpuv::randomPort()
  early <- env$viewerEarlyServerStart(
    early_server_root(),
    "127.0.0.1",
    port,
    early_server_options()
  )
  withr::defer(try(early$server$stop(), silent = TRUE))

  ## Shiny's server, as runApp() starts it: another port, its own App, and a
  ## directory it serves as files.
  theirs <- normalizePath(system.file(package = "testthat"), winslash = "/")
  shiny_server <- httpuv::startServer(
    "127.0.0.1",
    httpuv::randomPort(),
    list(
      call = function(req) {
        list(
          status = 200L,
          headers = list("Content-Type" = "text/plain"),
          body = paste("from the App:", req$PATH_INFO)
        )
      },
      onWSOpen = function(ws) NULL,
      staticPaths = list(theirs = theirs)
    )
  )
  withr::defer(try(shiny_server$stop(), silent = TRUE))
  previous <- shiny::getShinyOption("server", default = NULL)
  withr::defer(shiny::shinyOptions(server = previous))

  both <- env$viewerEarlyServerAdopt(early, shiny_server)
  expect_identical(shiny::getShinyOption("server"), both)
  ## the address the Viewer names is the early server's
  expect_identical(both$getPort(), port)
  expect_identical(both$getHost(), "127.0.0.1")
  ## what Shiny serves as files, the early server serves too
  expect_identical(
    early_server_get(port, "/theirs/DESCRIPTION"),
    readBin(file.path(theirs, "DESCRIPTION"), "raw", n = 1e6)
  )
  ## and what is set from now on is set on both
  added <- normalizePath(system.file(package = "R6"), winslash = "/")
  both$setStaticPath(.list = list(added = httpuv::staticPath(added)))
  expect_true("/added" %in% names(shiny_server$getStaticPaths()))
  expect_identical(
    early_server_get(port, "/added/DESCRIPTION"),
    readBin(file.path(added, "DESCRIPTION"), "raw", n = 1e6)
  )
  both$removeStaticPath("added")
  expect_false("/added" %in% names(early$server$getStaticPaths()))

  ## A request that is not a file is answered by the App, in this R process:
  ## another one asks, this one serves.
  task <- mirai::mirai(
    {
      connection <- url(address, open = "rb")
      on.exit(close(connection))
      rawToChar(readBin(connection, "raw", n = 1e5))
    },
    address = sprintf("http://127.0.0.1:%d/session/anything", port)
  )
  waited <- 0
  while (mirai::unresolved(task) && waited < 30) {
    later::run_now(0.05)
    waited <- waited + 0.05
  }
  expect_identical(task$data, "from the App: /session/anything")
})

test_that("first views are published through the functions they are given", {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("core", "shared_resources.R"), env)
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  crb <- withr::local_tempfile(fileext = ".crb")
  writeBin(raw(8), crb)
  served <- list()
  expect_true(env$viewerFirstViewPublishKept(
    list(crb_file_to_load = crb, cerebro_version = "1"),
    register = function(prefix, directory) stop("no Viewer Pack was kept"),
    serve = function(prefix, directory) {
      served[[prefix]] <<- directory
    }
  ))
  ## not through Shiny: the directory of the first views went to `serve`
  expect_named(served, "cerebro-first-view")
  expect_identical(served[[1L]], env$viewerFirstViewDirectory())
  ## once per process
  expect_false(env$viewerFirstViewPublishKept(
    list(crb_file_to_load = crb),
    serve = function(prefix, directory) stop("published already")
  ))
})

test_that("launchCerebro continues with a running early server", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  state <- new.env(parent = emptyenv())
  state$token <- "tokenoftheearlyserver000"
  state$published <- TRUE
  running <- TRUE
  early <- structure(
    list(
      server = list(isRunning = function() running),
      gate = new.env(parent = emptyenv()),
      host = "127.0.0.1",
      port = 4321L,
      key = "keyoftheearlyserver",
      state = state,
      registered = character(),
      ## published for the files and options of this App: none
      basis = NULL,
      sources = character()
    ),
    class = "cerebro_early_server"
  )
  withr::local_options(CerebroNexus.early_server = early)
  app <- launchCerebro(mode = "closed")
  env <- environment(app$serverFuncSource())
  expect_identical(env$viewer_early, early)
  ## the key of the page is the one the early server read its document with
  expect_identical(env$viewer_document_key(), "keyoftheearlyserver")
  ## and the URL prefixes it gave out stay the ones of this process
  expect_identical(
    env$.viewer_shared_resources$token,
    "tokenoftheearlyserver000"
  )
  ## the first views it published were for other options: published again
  expect_null(env$.viewer_shared_resources$published)

  ## A server that has stopped is not continued with.
  running <- FALSE
  app <- launchCerebro(mode = "closed")
  expect_null(environment(app$serverFuncSource())$viewer_early)
})
