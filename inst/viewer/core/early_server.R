## Answering before Shiny is loaded.
##
## launchCerebro() answers its first request 0.66 s after the R process starts
## on an idle machine: 0.24 s for R itself, 0.28 s to load CerebroNexus with
## shiny, 0.14 s to build and start the App. A static site is answered after
## 0.3 s, and that difference is all that is left between the two when a page
## shows its first view.
##
## Nothing a page needs first is made by Shiny: the page is the HTML document
## kept from an earlier run (kept_document.R), its scripts and styles are
## files, and so are the first view of its Projection and the Viewer Pack
## files it reads (shared_resources.R). start.R (in the package directory)
## therefore starts a web server with httpuv alone, on the address the App is
## asked for, before it loads CerebroNexus: viewerEarlyServerStart() below,
## 0.2 s of work after R has started.
##
## Shiny still starts a web server of its own, on another port of this
## machine that nobody is given. The early server hands every request that is
## not a file to the App that server was given (viewerEarlyServerApp()): its
## pages, its WebSockets, a session's downloads. Both servers run on the one R
## thread, and R is busy loading the Viewer until Shiny's server exists, so no
## request is handed over before there is an App to take it.
##
## What Shiny registers as static files while it runs has to reach the early
## server too. viewerEarlyServerAdopt() gives Shiny, in place of its own
## server object, one that sets a static path on both.
##
## httpuv does not export the App a running server was given; it is read from
## the server object (viewerEarlyServerShinyApp()). The early server is only
## kept when its own App can be read back that way, so with another httpuv
## the Viewer starts as launchCerebro() does.
##
## Everything here runs without shiny, and without the CerebroNexus namespace.

## The App a running httpuv server was given, or NULL.
viewerEarlyServerShinyApp <- function(server) {
  app <- tryCatch(
    {
      wrapper <- server$.__enclos_env__$private$appWrapper
      wrapper$.__enclos_env__$private$app
    },
    error = function(error) NULL
  )
  if (is.function(app$call) && is.function(app$onWSOpen)) app else NULL
}

## The App the early server hands its requests to: Shiny's, once Shiny runs.
viewerEarlyServerApp <- function(gate) {
  if (is.null(gate$app) && isNamespaceLoaded("shiny")) {
    gate$app <- viewerEarlyServerShinyApp(
      shiny::getShinyOption("server", default = NULL)
    )
  }
  gate$app
}

## What the early server answers itself: `/` with the kept document, the files
## the document names, and the first views with their Viewer Pack files.
## Returns the server with what launchCerebro() takes over from it, or NULL
## when there is no kept document for this installation (the first process),
## or the App it serves cannot be read back from the server.
viewerEarlyServerStart <- function(root, host, port, options) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "viewer", "ui_cache.R"), env)
  sys.source(file.path(root, "viewer", "core", "shared_resources.R"), env)
  key <- env$viewerUiCacheKey(
    file.path(root, "viewer"),
    c(
      version = as.character(options[["cerebro_version"]]),
      www_prefix = "cerebro_www"
    )
  )
  file <- file.path(
    env$viewerUiCacheDirectory(),
    paste0(key, ".document.rds")
  )
  if (!file.exists(file)) {
    return(NULL)
  }
  kept <- tryCatch(readRDS(file), error = function(error) NULL)
  resource_paths <- kept$resource_paths
  valid <- is.list(kept) &&
    is.raw(kept$html) &&
    length(kept$html) > 0L &&
    env$viewerUiCachePackagesValid(kept$packages) &&
    is.character(resource_paths) &&
    length(resource_paths) > 0L &&
    !is.null(names(resource_paths)) &&
    all(dir.exists(resource_paths))
  if (!isTRUE(valid)) {
    return(NULL)
  }
  document <- file.path(tempdir(), "cerebro-early-document")
  dir.create(document, showWarnings = FALSE, recursive = TRUE)
  writeBin(kept$html, file.path(document, "index.html"))

  files <- function(directory) {
    httpuv::staticPath(
      normalizePath(directory, mustWork = TRUE),
      indexhtml = FALSE,
      fallthrough = TRUE
    )
  }
  static <- list(
    "/" = httpuv::staticPath(
      document,
      indexhtml = TRUE,
      fallthrough = TRUE,
      headers = list("Cache-Control" = "no-cache")
    )
  )
  for (prefix in names(resource_paths)) {
    static[[paste0("/", prefix)]] <- files(resource_paths[[prefix]])
  }
  gate <- new.env(parent = emptyenv())
  unavailable <- function() {
    list(
      status = 503L,
      headers = list("Content-Type" = "text/plain", "Retry-After" = "1"),
      body = "The Viewer is starting.\n"
    )
  }
  app <- list(
    call = function(req) {
      target <- viewerEarlyServerApp(gate)
      if (is.null(target)) unavailable() else target$call(req)
    },
    onHeaders = function(req) {
      target <- viewerEarlyServerApp(gate)
      if (is.function(target$onHeaders)) target$onHeaders(req)
    },
    onWSOpen = function(ws) {
      target <- viewerEarlyServerApp(gate)
      if (is.null(target)) ws$close() else target$onWSOpen(ws)
    },
    staticPaths = static
  )
  server <- httpuv::startServer(host, port, app)
  if (!identical(viewerEarlyServerShinyApp(server)$call, app$call)) {
    ## This httpuv keeps the App somewhere else: Shiny's could not be read
    ## either, so the Viewer starts without an early server.
    server$stop()
    return(NULL)
  }
  ## The first views kept from earlier runs, as viewerFirstViewPublishKept()
  ## publishes them for Shiny: here as static paths of this server. They are
  ## published once it answers (the page asks for them 0.1 s after it has
  ## arrived at the earliest, and it opens without a preview when they are
  ## not there): the server does not wait for the 50 ms it takes.
  registered <- character()
  publish <- function(prefix, directory, path) {
    server$setStaticPath(
      .list = stats::setNames(
        list(path),
        paste0("/", prefix)
      )
    )
    registered[[prefix]] <<- directory
  }
  try(
    env$viewerFirstViewPublishKept(
      options,
      register = function(prefix, directory) {
        publish(
          prefix,
          directory,
          env$viewerSharedResourceStaticPath(directory)
        )
      },
      serve = function(prefix, directory) {
        publish(prefix, directory, files(directory))
      }
    ),
    silent = TRUE
  )
  structure(
    list(
      server = server,
      gate = gate,
      host = host,
      port = port,
      key = key,
      document = document,
      ## What launchCerebro() continues with: the URL prefixes given out, and
      ## what they were given for.
      state = env$.viewer_shared_resources,
      registered = registered,
      basis = env$viewerFirstViewBasis(options),
      sources = env$viewerSharedResourceSources(options)
    ),
    class = "cerebro_early_server"
  )
}

## A static path as a server gives it back, as one a server takes: what
## getStaticPaths() returns is not what setStaticPath() accepts.
viewerEarlyServerStaticPath <- function(path) {
  options <- unclass(path$options)
  if (isTRUE(options$exclude)) {
    return(httpuv::excludeStaticPath())
  }
  arguments <- options[intersect(
    names(options),
    c("indexhtml", "fallthrough", "html_charset", "headers", "validation")
  )]
  arguments <- arguments[!vapply(arguments, is.null, logical(1))]
  if (is.character(arguments$headers)) {
    arguments$headers <- as.list(arguments$headers)
  }
  do.call(httpuv::staticPath, c(list(path$path), arguments))
}

## TRUE for an early server that still runs.
viewerEarlyServerRunning <- function(early) {
  inherits(early, "cerebro_early_server") &&
    isTRUE(tryCatch(early$server$isRunning(), error = function(error) FALSE))
}

## Makes the early server and Shiny's `server` one for the Viewer: requests go
## to Shiny's App, the static paths Shiny has are served by the early server
## too, and so is every one set from now on. Returns the object that stands
## for both, which Shiny is given as its server.
viewerEarlyServerAdopt <- function(early, server) {
  early$gate$app <- viewerEarlyServerShinyApp(server)
  bare <- function(names) sub("^/+", "", names)
  theirs <- tryCatch(server$getStaticPaths(), error = function(error) list())
  ours <- bare(names(early$server$getStaticPaths()))
  ## Not the ones this server has: its Viewer Pack files keep their headers.
  missing <- theirs[!bare(names(theirs)) %in% ours]
  if (length(missing)) {
    early$server$setStaticPath(
      .list = lapply(missing, viewerEarlyServerStaticPath)
    )
  }
  both <- new.env(parent = emptyenv())
  both$setStaticPath <- function(..., .list = NULL) {
    server$setStaticPath(..., .list = .list)
    early$server$setStaticPath(..., .list = .list)
  }
  both$removeStaticPath <- function(path) {
    ## A server knows its static paths by their URL path.
    path <- as.character(path)
    path <- ifelse(startsWith(path, "/"), path, paste0("/", path))
    server$removeStaticPath(path)
    early$server$removeStaticPath(path)
  }
  both$getStaticPaths <- function() early$server$getStaticPaths()
  both$getHost <- function() early$host
  both$getPort <- function() early$port
  both$isRunning <- function() early$server$isRunning()
  shiny::shinyOptions(server = both)
  both
}
