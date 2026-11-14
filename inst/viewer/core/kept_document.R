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
## The page names 60 and more files (the scripts and styles of shiny,
## bootstrap, ... and the Viewer's own), which a browser fetches six at a time
## over HTTP/1.1: on a network with a round trip of 40 ms that is 0.2 s of the
## first view, and 0.7 s at 100 ms. The kept document therefore comes with a
## second form of itself, in which each run of scripts and each run of
## styles that follow one another is one file (viewerDocumentBundle()),
## served from "cerebro-bundle/" with a header that lets a browser keep it.
## Both forms are kept; options(CerebroNexus.bundle = FALSE) uses the first.
##
## An app.R keeps and serves its page the same way (viewerStagedDocument() in
## staged_start.R), also under Shiny Server. The page is another one there: it
## carries one more script of the Viewer (transport_compat.R) and what Shiny
## Server adds to every page it sends, so its key says how the page is sent
## (viewerDocumentTransport()). launchCerebro() does not keep the page under
## Shiny Server. Nothing is kept with options(CerebroNexus.ui_cache = FALSE).

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

## ---- one file for each run of scripts and of styles ------------------------

## The file under the resource paths that the URL `url` of the document names,
## or NULL (another site, an absolute path, a file that is not there).
viewerDocumentResourceFile <- function(url, resource_paths) {
  url <- sub("[?#].*$", "", url)
  if (grepl("^([a-zA-Z][a-zA-Z0-9+.-]*:|/)", url)) {
    return(NULL)
  }
  parts <- strsplit(sub("^\\./", "", url), "/", fixed = TRUE)[[1L]]
  if (length(parts) < 2L || !parts[[1L]] %in% names(resource_paths)) {
    return(NULL)
  }
  if (any(parts == "..")) {
    return(NULL)
  }
  file <- do.call(
    file.path,
    c(list(resource_paths[[parts[[1L]]]]), as.list(parts[-1L]))
  )
  if (file.exists(file) && !dir.exists(file)) file else NULL
}

## `path` (relative, with ".." and ".") without them, or NULL when it leaves
## the root.
viewerDocumentNormalizePath <- function(path) {
  out <- character()
  for (part in strsplit(path, "/", fixed = TRUE)[[1L]]) {
    if (part == "..") {
      if (!length(out)) {
        return(NULL)
      }
      out <- out[-length(out)]
    } else if (!part %in% c("", ".")) {
      out <- c(out, part)
    }
  }
  paste(out, collapse = "/")
}

## The bytes `css` (raw) of the stylesheet at `path` (relative to the page),
## for a file one directory down ("cerebro-bundle/"): its relative URLs name
## the same files from there. NULL when it cannot be moved (@import). Bytes
## stay bytes: nothing is converted.
viewerDocumentMoveStyles <- function(css, path) {
  text <- rawToChar(css)
  if (grepl("@import", text, fixed = TRUE, useBytes = TRUE)) {
    return(NULL)
  }
  directory <- dirname(path)
  pattern <- "url\\(\\s*([\"']?)([^)\"']+)\\1\\s*\\)"
  found <- gregexpr(pattern, text, perl = TRUE, useBytes = TRUE)[[1L]]
  if (found[[1L]] == -1L) {
    return(css)
  }
  starts <- as.integer(found)
  ends <- starts + attr(found, "match.length") - 1L
  chunks <- vector("list", 2L * length(starts) + 1L)
  cursor <- 1L
  for (i in seq_along(starts)) {
    chunks[[2L * i - 1L]] <- css[seq_len(starts[[i]] - cursor) + cursor - 1L]
    reference <- css[starts[[i]]:ends[[i]]]
    url <- sub(
      pattern,
      "\\2",
      rawToChar(reference),
      perl = TRUE,
      useBytes = TRUE
    )
    target <- NULL
    if (
      !grepl("^([a-zA-Z][a-zA-Z0-9+.-]*:|/|#)", url, useBytes = TRUE) &&
        !any(as.integer(charToRaw(url)) > 127L)
    ) {
      suffix <- regmatches(url, regexpr("[?#].*$", url))
      suffix <- if (length(suffix)) suffix else ""
      target <- viewerDocumentNormalizePath(paste0(
        directory,
        "/",
        sub("[?#].*$", "", url)
      ))
      if (!is.null(target)) {
        target <- charToRaw(paste0("url(\"../", target, suffix, "\")"))
      }
    }
    chunks[[2L * i]] <- if (is.null(target)) reference else target
    cursor <- ends[[i]] + 1L
  }
  chunks[[length(chunks)]] <- css[
    seq_len(length(css) - cursor + 1L) + cursor - 1L
  ]
  unlist(chunks, use.names = FALSE)
}

## TRUE for a script that begins with a string (after comments): a directive
## such as "use strict", which would apply to every file joined after it. Such
## a file stays a script of its own.
viewerDocumentStartsWithDirective <- function(bytes) {
  text <- tryCatch(
    rawToChar(bytes[seq_len(min(length(bytes), 65536L))]),
    error = function(error) NULL
  )
  if (is.null(text)) {
    return(TRUE)
  }
  grepl(
    "(?s)^(\\xef\\xbb\\xbf)?\\s*(?:(?://[^\\n]*|/\\*.*?\\*/)\\s*)*[\"']",
    text,
    perl = TRUE,
    useBytes = TRUE
  )
}

## The page `html` (raw) with each run of scripts, and each run of styles, that
## follow one another as one file. Returns list(html = raw, files = named list
## of raw), or NULL when nothing is joined. Only what the resource paths name
## is joined. An inline script (or style) ends a run, so everything still runs
## in the order of the page, and what is not plain (async, module, media,
## @import, another site) stays where it is and ends a run too. Scripts with
## `defer` run in the order of the page after it is parsed, whatever lies
## between them: they are joined among themselves, and only another script
## with `defer` that stays ends their run. The bytes of the page are never
## converted. Joined scripts share one fate: a file that begins with a directive
## is left alone (above), but one that throws at its top level stops the files
## after it, which separate tags would not.
viewerDocumentBundle <- function(html, resource_paths) {
  text <- rawToChar(html)
  pattern <- paste0(
    "(?is)<script\\b([^>]*)>(.*?)</script>",
    "|<link\\b([^>]*)>",
    "|<style\\b[^>]*>.*?</style>"
  )
  found <- gregexpr(pattern, text, perl = TRUE, useBytes = TRUE)[[1L]]
  if (found[[1L]] == -1L) {
    return(NULL)
  }
  starts <- as.integer(found)
  ends <- starts + attr(found, "match.length") - 1L
  ## the tags as strings of bytes, to read attributes from
  tags <- vapply(
    seq_along(starts),
    function(i) rawToChar(html[starts[[i]]:ends[[i]]]),
    character(1)
  )
  attribute <- function(tag, name) {
    value <- regmatches(
      tag,
      regexec(
        paste0("(?i)\\s", name, "\\s*=\\s*\"([^\"]*)\""),
        tag,
        perl = TRUE,
        useBytes = TRUE
      )
    )[[1L]]
    if (length(value)) value[[2L]] else NA_character_
  }
  ## no attribute but the ones wanted
  plain <- function(tag, wanted) {
    names <- regmatches(
      tag,
      gregexpr(
        "(?<=\\s)[a-zA-Z-]+(?=\\s*(=|/?>|\\s))",
        tag,
        perl = TRUE,
        useBytes = TRUE
      )
    )[[1L]]
    all(tolower(names) %in% wanted)
  }
  read <- function(file) readBin(file, "raw", n = file.size(file))
  replacement <- rep(NA_character_, length(tags))
  files <- list()
  runs <- list(script = integer(), defer = integer(), style = integer())
  members <- list(script = list(), defer = list(), style = list())
  flush <- function(kind) {
    index <- runs[[kind]]
    if (length(index) >= 2L) {
      script <- !identical(kind, "style")
      bytes <- unlist(
        lapply(members[[kind]], function(item) {
          if (script) {
            c(read(item), charToRaw("\n;\n"))
          } else {
            c(
              viewerDocumentMoveStyles(read(item$file), item$path),
              charToRaw("\n")
            )
          }
        }),
        use.names = FALSE
      )
      name <- sprintf(
        "bundle-%s.%s",
        substr(rlang::hash(bytes), 1L, 16L),
        if (script) "js" else "css"
      )
      files[[name]] <<- bytes
      replacement[index] <<- ""
      replacement[[index[[1L]]]] <<- if (script) {
        sprintf(
          "<script%s src=\"cerebro-bundle/%s\"></script>",
          if (identical(kind, "defer")) " defer" else "",
          name
        )
      } else {
        sprintf("<link href=\"cerebro-bundle/%s\" rel=\"stylesheet\" />", name)
      }
    }
    runs[[kind]] <<- integer()
    members[[kind]] <<- list()
  }
  for (i in seq_along(tags)) {
    tag <- tags[[i]]
    if (grepl("^<script", tag, ignore.case = TRUE, useBytes = TRUE)) {
      source <- attribute(tag, "src")
      type <- attribute(tag, "type")
      runs_js <- is.na(type) ||
        grepl("javascript", type, ignore.case = TRUE, useBytes = TRUE)
      opening <- sub(
        "^(<script[^>]*>).*$",
        "\\1",
        tag,
        perl = TRUE,
        useBytes = TRUE
      )
      kind <- if (
        grepl("\\sdefer(\\s|=|>)", opening, ignore.case = TRUE, useBytes = TRUE)
      ) {
        "defer"
      } else {
        "script"
      }
      if (is.na(source)) {
        ## a data block does not run; an inline script does, and ends the run
        if (runs_js) {
          flush("script")
        }
        next
      }
      file <- viewerDocumentResourceFile(source, resource_paths)
      if (
        is.null(file) ||
          !runs_js ||
          !plain(opening, c("script", "src", "type", "defer")) ||
          viewerDocumentStartsWithDirective(read(file))
      ) {
        flush(kind)
        next
      }
      runs[[kind]] <- c(runs[[kind]], i)
      members[[kind]][[length(members[[kind]]) + 1L]] <- file
    } else if (grepl("^<link", tag, ignore.case = TRUE, useBytes = TRUE)) {
      if (!identical(tolower(attribute(tag, "rel")), "stylesheet")) {
        next
      }
      href <- attribute(tag, "href")
      file <- if (!is.na(href)) viewerDocumentResourceFile(href, resource_paths)
      path <- NULL
      moved <- NULL
      if (!is.null(file) && plain(tag, c("link", "href", "rel", "type"))) {
        path <- viewerDocumentNormalizePath(sub("[?#].*$", "", href))
        moved <- if (!is.null(path)) {
          viewerDocumentMoveStyles(read(file), path)
        }
      }
      if (is.null(moved)) {
        flush("style")
        next
      }
      runs$style <- c(runs$style, i)
      members$style[[length(members$style) + 1L]] <- list(
        file = file,
        path = path
      )
    } else {
      flush("style")
    }
  }
  flush("script")
  flush("defer")
  flush("style")
  if (!length(files)) {
    return(NULL)
  }
  chunks <- vector("list", 2L * length(tags) + 1L)
  cursor <- 1L
  for (i in seq_along(tags)) {
    chunks[[2L * i - 1L]] <- html[seq_len(starts[[i]] - cursor) + cursor - 1L]
    chunks[[2L * i]] <- if (is.na(replacement[[i]])) {
      html[starts[[i]]:ends[[i]]]
    } else {
      charToRaw(replacement[[i]])
    }
    cursor <- ends[[i]] + 1L
  }
  chunks[[length(chunks)]] <- html[
    seq_len(length(html) - cursor + 1L) + cursor - 1L
  ]
  list(html = unlist(chunks, use.names = FALSE), files = files)
}

## Where the process that serves a bundled page writes its files.
viewerDocumentBundleDirectory <- function() {
  file.path(tempdir(), "cerebro-bundle")
}

## The page of a kept document as list(html, files): the bundled form when it
## has one, the page as it was kept otherwise (or with
## options(CerebroNexus.bundle = FALSE)).
viewerDocumentPage <- function(kept) {
  bundle <- kept$bundle
  if (
    isTRUE(getOption("CerebroNexus.bundle", TRUE)) &&
      is.list(bundle) &&
      is.raw(bundle$html) &&
      length(bundle$html) > 0L &&
      is.list(bundle$files) &&
      length(bundle$files) > 0L &&
      !is.null(names(bundle$files)) &&
      all(grepl("^bundle-[0-9a-f]+[.](js|css)$", names(bundle$files))) &&
      all(vapply(bundle$files, is.raw, logical(1)))
  ) {
    return(list(html = bundle$html, files = bundle$files))
  }
  list(html = kept$html, files = list())
}

## Writes the files of a page; returns their directory, or NULL without files.
viewerDocumentWriteFiles <- function(
  files,
  directory = viewerDocumentBundleDirectory()
) {
  if (!length(files)) {
    return(NULL)
  }
  unlink(directory, recursive = TRUE)
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  for (name in names(files)) {
    writeBin(files[[name]], file.path(directory, name))
  }
  directory
}

## The static path of the bundle: the name of a file holds the hash of its
## content, so a browser keeps it for good.
viewerDocumentBundleStaticPath <- function(directory) {
  httpuv::staticPath(
    normalizePath(directory, mustWork = TRUE),
    indexhtml = FALSE,
    fallthrough = TRUE,
    headers = list("Cache-Control" = "public, max-age=31536000, immutable")
  )
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
      bundle = tryCatch(
        viewerDocumentBundle(html, resource_paths),
        error = function(error) NULL
      ),
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
  page <- viewerDocumentPage(kept)
  writeBin(page$html, file.path(target, "index.html"))
  viewerDocumentWriteFiles(page$files)
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
  bundle <- viewerDocumentBundleDirectory()
  if (length(list.files(bundle))) {
    server$setStaticPath(
      .list = list("/cerebro-bundle" = viewerDocumentBundleStaticPath(bundle))
    )
  }
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

## What the page as it is sent depends on besides the page: whether the
## Viewer adds its script for Shiny Server (`sockjs`), and what the server
## that runs the App adds to every page. Shiny Server does that with
## options(shiny.http.response.filter), a function that holds the text it
## inserts (its scripts, and the settings of its configuration file). A value
## for the key of the document.
viewerDocumentTransport <- function(sockjs = FALSE) {
  filter <- getOption("shiny.http.response.filter")
  added <- NULL
  if (is.function(filter)) {
    added <- tryCatch(
      get0("inject", envir = environment(filter), inherits = FALSE),
      error = function(error) NULL
    )
    if (!is.character(added)) {
      added <- deparse(body(filter))
    }
  }
  rlang::hash(list(sockjs = isTRUE(sockjs), added = added))
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
## finds its packages where this one did. Shiny Server lets the App answer
## only requests that carry the secret it gave the process.
viewerDocumentFetch <- function(address) {
  secret <- getOption("shiny.sharedSecret")
  .viewerMirai(
    {
      connection <- if (is.character(secret) && nzchar(secret)) {
        url(
          address,
          open = "rb",
          headers = c("Shiny-Shared-Secret" = secret)
        )
      } else {
        url(address, open = "rb")
      }
      on.exit(close(connection))
      readBin(connection, "raw", n = 5e7)
    },
    address = address,
    secret = secret
  )
}
