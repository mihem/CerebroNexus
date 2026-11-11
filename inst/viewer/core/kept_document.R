## The page as a file.
##
## launchCerebro() needs 1.3 s (10,000 cells) to 4.5 s (1.3 million) before the
## Viewer can serve a session: it reads the data sets, loads the server code and
## collects. None of that is needed to send the page. Shiny renders the page in
## R, though, so a browser that asks for it while R is busy waits for R.
##
## The page is therefore also kept as the HTML document Shiny sends, and a
## process that has one serves it as a static file under "/": httpuv answers
## static files from its own thread, while R prepares the Viewer. The page's
## scripts, styles and Viewer Pack files are static already, so a browser gets
## the page and the preview of its first view (www/first-view.js) at once; its
## WebSocket waits until R is free, and the session starts then.
##
## The document is the one this installation's Shiny renders: the first process
## of a Viewer installation fetches "/" from itself through a background R
## process once it runs (nothing else gives the exact bytes; it is never taken
## from a browser), and keeps it with the page (ui_cache.R) under the same key
## and the same validity. With it are kept the resource paths the document
## names (the scripts and styles of shiny, bootstrap, ...), which Shiny only
## registers when it renders the page itself: a process that serves the kept
## document registers them before it starts.
##
## Not under Shiny Server, where the page carries one more script
## (transport_compat.R), and not with options(CerebroNexus.ui_cache = FALSE).

viewerDocumentDirectory <- function() {
  directory <- file.path(tempdir(), "cerebro-document")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  directory
}

viewerDocumentFile <- function(key, directory = viewerUiCacheDirectory()) {
  file.path(directory, paste0(key, ".document.rds"))
}

## The resource paths a kept document may name: those registered now, without
## the Viewer's per-process ones (first views, Viewer Pack files, cell names)
## and anything in the session's temporary directory.
viewerDocumentResourcePaths <- function() {
  paths <- shiny::resourcePaths()
  paths <- paths[!grepl("^cerebro-", names(paths))]
  directories <- normalizePath(unname(paths), winslash = "/", mustWork = FALSE)
  temporary <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  keep <- dir.exists(directories) &
    !startsWith(paste0(directories, "/"), paste0(temporary, "/"))
  stats::setNames(directories[keep], names(paths)[keep])
}

## Keeps `html` (the bytes of the page as Shiny sent it) for the key of the
## page. Returns the file, or NULL when `html` is not a Shiny page.
viewerDocumentKeep <- function(
  key,
  html,
  directory = viewerUiCacheDirectory()
) {
  text <- if (is.raw(html)) rawToChar(html) else ""
  if (
    !grepl("^\\s*<!DOCTYPE html>", text, ignore.case = TRUE, useBytes = TRUE) ||
      !grepl("shiny", text, fixed = TRUE, useBytes = TRUE)
  ) {
    return(invisible(NULL))
  }
  resource_paths <- viewerDocumentResourcePaths()
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  file <- viewerDocumentFile(key, directory)
  staging <- paste0(file, ".", Sys.getpid(), ".tmp")
  on.exit(unlink(staging), add = TRUE)
  saveRDS(
    list(
      html = html,
      resource_paths = resource_paths,
      packages = viewerUiCachePackageTable(unname(resource_paths))
    ),
    staging,
    compress = FALSE
  )
  file.rename(staging, file)
  invisible(file)
}

## Writes the kept document of `key` as `target`/index.html and registers the
## resource paths it names. Returns `target`, or NULL: nothing kept, or a
## package that is no longer where or what it was.
viewerDocumentRestore <- function(
  key,
  directory = viewerUiCacheDirectory(),
  target = viewerDocumentDirectory()
) {
  file <- viewerDocumentFile(key, directory)
  if (!file.exists(file)) {
    return(NULL)
  }
  kept <- tryCatch(readRDS(file), error = function(error) NULL)
  resource_paths <- kept$resource_paths
  valid <- is.list(kept) &&
    is.raw(kept$html) &&
    length(kept$html) > 0L &&
    viewerUiCachePackagesValid(kept$packages) &&
    is.character(resource_paths) &&
    length(resource_paths) > 0L &&
    !is.null(names(resource_paths)) &&
    all(dir.exists(resource_paths))
  if (!isTRUE(valid)) {
    unlink(file)
    return(NULL)
  }
  registered <- shiny::resourcePaths()
  registered <- stats::setNames(
    normalizePath(unname(registered), winslash = "/", mustWork = FALSE),
    names(registered)
  )
  current <- unname(registered[names(resource_paths)])
  wanted <- normalizePath(
    unname(resource_paths),
    winslash = "/",
    mustWork = FALSE
  )
  ## A name that another App of this process serves from elsewhere (its own
  ## www/ under "cerebro_www") stays that App's. The document is not used
  ## then, and shiny_UI.R gives the files of this page other names.
  if (any(!is.na(current) & current != wanted)) {
    return(NULL)
  }
  for (prefix in names(resource_paths)[is.na(current)]) {
    shiny::addResourcePath(prefix, resource_paths[[prefix]])
  }
  writeBin(kept$html, file.path(target, "index.html"))
  ## Renewed by use, as the kept page is.
  Sys.setFileTime(file, Sys.time())
  target
}

## Makes the running web server answer "/" with `directory`/index.html. Every
## other path still reaches Shiny.
viewerDocumentServe <- function(server, directory) {
  server$setStaticPath(
    .list = list(
      "/" = httpuv::staticPath(
        normalizePath(directory, mustWork = TRUE),
        indexhtml = TRUE,
        fallthrough = TRUE,
        ## The file is this process's; a browser asks for it every time.
        headers = list("Cache-Control" = "no-cache")
      )
    )
  )
  invisible(TRUE)
}

## The address of the running web server as this machine reaches it.
viewerDocumentAddress <- function(server) {
  host <- server$getHost()
  if (host %in% c("0.0.0.0", "::", "")) {
    host <- "127.0.0.1"
  }
  if (grepl(":", host, fixed = TRUE)) {
    host <- paste0("[", host, "]")
  }
  sprintf("http://%s:%d/", host, as.integer(server$getPort()))
}

## Fetches "/" from the running web server in a background R process (this one
## has to answer the request) and keeps the document for `key`. `fetch(address)`
## returns a mirai whose value is the raw response.
viewerDocumentCapture <- function(
  server,
  key,
  fetch = viewerDocumentFetch,
  directory = viewerUiCacheDirectory()
) {
  task <- fetch(viewerDocumentAddress(server))
  waited <- 0
  poll <- function() {
    if (mirai::unresolved(task)) {
      waited <<- waited + 0.05
      if (waited < 60) {
        later::later(poll, 0.05)
      }
      return(invisible(NULL))
    }
    try(viewerDocumentKeep(key, task$data, directory), silent = TRUE)
    invisible(NULL)
  }
  poll()
  invisible(task)
}

## Started like every background task of the Viewer, through .viewerMirai()
## (crb_cache.R), which the prepared Viewer has loaded: the child R process
## finds its packages where this one did.
viewerDocumentFetch <- function(address) {
  .viewerMirai(
    {
      connection <- url(address, open = "rb")
      on.exit(close(connection))
      readBin(connection, "raw", n = 5e7)
    },
    address = address
  )
}
