## URL prefixes for static files that every session of the process may read:
## the Viewer Pack directories of the configured data sets.
##
## A session used to register each directory under a prefix of its own, so the
## URL of a Pack file was unknown until the session had started, and every
## session downloaded the same files again (10 MB of coordinates for 1.3 million
## cells). One prefix per directory for the whole process gives a file one URL:
## the browser keeps it across sessions, and a page can name it before its
## session exists. The prefix carries a token drawn when the process starts, so
## the URLs cannot be guessed, as before, and it changes when the Pack is built
## again: a URL always names the same bytes, and the browser may keep them
## without asking.
##
## Only without a login: there the files of a configured data set are meant for
## everyone who can open the App. With a login nothing is shared and a file is
## reachable only through the prefix of a session that signed in; an uploaded
## data set also stays with its session.

.viewer_shared_resources <- new.env(parent = emptyenv())

## The configured CRB files, whose content every visitor may see: none with a
## login.
viewerSharedResourceSources <- function(options) {
  if (!is.null(options[[".viewer_auth"]])) {
    return(character())
  }
  files <- as.character(options[["crb_file_to_load"]])
  files <- files[!is.na(files) & file.exists(files) & !dir.exists(files)]
  if (!length(files)) {
    return(character())
  }
  unique(normalizePath(files, winslash = "/", mustWork = TRUE))
}

## Directories whose files may be shared: `<name>.viewer` beside each CRB.
viewerSharedResourceRoots <- function(options) {
  files <- viewerSharedResourceSources(options)
  if (!length(files)) {
    return(character())
  }
  unique(file.path(
    dirname(files),
    paste0(tools::file_path_sans_ext(basename(files)), ".viewer")
  ))
}

## The process-wide prefix of `directory`, registered on first use, or NULL
## when the directory is not below one of `roots` and stays with its session.
## `register(prefix, directory)` makes the files reachable.
viewerSharedResourcePrefix <- function(kind, directory, roots, register) {
  if (!length(roots)) {
    return(NULL)
  }
  directory <- normalizePath(directory, winslash = "/", mustWork = FALSE)
  root <- roots[startsWith(paste0(directory, "/"), paste0(roots, "/"))]
  if (!length(root)) {
    return(NULL)
  }
  ## The manifest is written last when a Pack is built.
  built <- file.mtime(file.path(root[[1L]], "manifest.json"))
  if (is.null(.viewer_shared_resources$token)) {
    .viewer_shared_resources$token <- paste0(
      sample(c(letters, 0:9), 24L, replace = TRUE),
      collapse = ""
    )
  }
  key <- paste(kind, directory, format(as.numeric(built), digits = 17L))
  prefix <- .viewer_shared_resources[[key]]
  if (is.null(prefix)) {
    count <- (.viewer_shared_resources[[paste("count", kind)]] %||% 0L) + 1L
    prefix <- paste0(
      "cerebro-",
      kind,
      "-",
      .viewer_shared_resources$token,
      "-",
      count
    )
    register(prefix, directory)
    assign(paste("count", kind), count, envir = .viewer_shared_resources)
    assign(key, prefix, envir = .viewer_shared_resources)
    assign(
      paste("directory", prefix),
      directory,
      envir = .viewer_shared_resources
    )
  }
  prefix
}

## Registers a shared prefix before the App runs: the files are reachable at
## once, and viewerSharedResourceHeaders() gives them their headers as soon as
## the web server exists.
viewerSharedResourceRegisterEarly <- function(prefix, directory) {
  shiny::addResourcePath(prefix, directory)
  .viewer_shared_resources$pending <- c(
    .viewer_shared_resources$pending,
    stats::setNames(directory, prefix)
  )
}

## These files are binary arrays, which httpuv would gzip for every request
## (10 MB of coordinates by 7 %, in half a second), and a shared prefix names
## the same bytes for as long as it exists, so a browser may keep them.
viewerSharedResourceStaticPath <- function(directory) {
  httpuv::staticPath(
    normalizePath(directory, mustWork = TRUE),
    headers = list(
      `Content-Encoding` = "identity",
      `Cache-Control` = "private, max-age=86400, immutable"
    )
  )
}

## Gives the prefixes registered early their headers. Needs the running web
## server: called when the App has started and again by every session.
viewerSharedResourceHeaders <- function() {
  pending <- .viewer_shared_resources$pending
  if (!length(pending)) {
    return(invisible(FALSE))
  }
  server <- shiny::getShinyOption("server", default = NULL)
  if (is.null(server)) {
    return(invisible(FALSE))
  }
  server$setStaticPath(
    .list = lapply(pending, viewerSharedResourceStaticPath)
  )
  .viewer_shared_resources$pending <- NULL
  invisible(TRUE)
}

## ---- the first view of the Projection page ----------------------------------
## What a page needs to draw the default Projection before its session exists:
## the two Pack files (coordinates and the group codes) and how the first frame
## shows them. A browser gets it from the fixed prefix `cerebro-first-view/`
## and draws a preview while the session starts (www/first-view.js); the
## session's own first frame replaces the preview.
##
## It is taken from a first frame a session really sent, so the colours, the
## group and the point size are the ones that frame will have, whatever decided
## them (the data set, the author's configuration). One file per address a
## page can be opened at, named by viewerFirstViewKey().

viewerFirstViewDirectory <- function() {
  directory <- file.path(tempdir(), "cerebro-first-view")
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  directory
}

## The file name for a page address: the path and the `dataset` query value,
## which is all that selects a data set, as hexadecimal UTF-8. The browser
## computes the same from its location (firstViewKey() in first-view.js).
viewerFirstViewKey <- function(pathname, search) {
  dataset <- shiny::parseQueryString(as.character(search %||% ""))[["dataset"]]
  if (!is.character(dataset) || length(dataset) != 1L) {
    dataset <- ""
  }
  address <- paste0(
    as.character(pathname %||% "/"),
    "\n",
    dataset
  )
  paste(as.character(charToRaw(enc2utf8(address))), collapse = "")
}

## The first-view description of a Projection frame, or NULL for a frame a
## preview cannot stand in for: anything but all cells of a 2-D projection,
## coloured by one group, read from shared Pack files.
viewerFirstViewPayload <- function(message) {
  meta <- message$meta
  data <- message$data
  projection <- data$projection_resource
  categories <- data$categorical_resource
  shared_url <- function(resource, kind) {
    is.list(resource) &&
      is.character(resource$url) &&
      length(resource$url) == 1L &&
      !is.null(.viewer_shared_resources$token) &&
      startsWith(
        resource$url,
        paste0("cerebro-", kind, "-", .viewer_shared_resources$token, "-")
      )
  }
  colors <- unlist(lapply(data$color, as.character), use.names = FALSE)
  levels <- as.character(unlist(categories$levels, use.names = FALSE))
  eligible <- identical(message$id, "overview_projection") &&
    identical(meta$color_type, "categorical") &&
    is.null(data$panels) &&
    shared_url(projection, "projection") &&
    shared_url(categories, "metadata") &&
    identical(as.integer(projection$dimensions), 2L) &&
    identical(as.character(categories$dtype), "uint8") &&
    isTRUE(data$client_percentage) &&
    isTRUE(all.equal(as.numeric(data$percentage_cells_to_show), 100)) &&
    !length(data$point_line) &&
    !length(data$x_range) &&
    !length(data$y_range) &&
    length(levels) > 0L &&
    length(colors) == length(levels) &&
    all(grepl("^#[0-9A-Fa-f]{6}$", colors))
  if (!isTRUE(eligible)) {
    return(NULL)
  }
  list(
    version = 1L,
    id = "overview_projection",
    cells = as.integer(projection$cells),
    point_size = as.numeric(data$point_size),
    point_opacity = as.numeric(data$point_opacity),
    colors = I(colors),
    ## the names of the groups, in the order of the colours: the preview lays
    ## out the legend with them, to know where the plot begins below it
    labels = I(levels),
    code_map = I(as.integer(unlist(categories$code_map, use.names = FALSE))),
    projection = list(
      url = projection$url,
      bytes = as.numeric(projection$bytes)
    ),
    categories = list(
      url = categories$url,
      bytes = as.numeric(categories$bytes)
    )
  )
}

## A data set without a Viewer Pack (fewer than 100,000 cells by default) has
## no files to name: its first frame carries the cells. The same description
## is made for it from that frame, with the coordinates and the group of each
## cell to be written as two files (viewerFirstViewWriteInline()). Returns
## list(view, xy, codes), or NULL for a frame a preview cannot stand in for.
viewerFirstViewInline <- function(message) {
  meta <- message$meta
  data <- message$data
  x <- data$x
  y <- data$y
  ## Fewer than 4,096 cells arrive as one vector per group, more as one vector
  ## with the group of each cell.
  if (is.list(x) && !inherits(x, "AsIs")) {
    groups <- rep(seq_along(x) - 1L, lengths(x))
    x <- unlist(x, use.names = FALSE)
    y <- unlist(y, use.names = FALSE)
  } else {
    groups <- as.integer(data$canonical_group)
  }
  colors <- unlist(lapply(data$color, as.character), use.names = FALSE)
  n <- length(x)
  range <- function(value) {
    value <- suppressWarnings(as.numeric(unlist(value, use.names = FALSE)))
    if (!length(value)) {
      return(NULL)
    }
    if (length(value) != 2L || anyNA(value) || value[[1L]] == value[[2L]]) {
      return(NA)
    }
    value
  }
  x_range <- range(data$x_range)
  y_range <- range(data$y_range)
  eligible <- identical(message$id, "overview_projection") &&
    identical(meta$color_type, "categorical") &&
    is.null(data$panels) &&
    is.null(data$z) &&
    is.null(data$projection_resource) &&
    is.null(message$shared_projection) &&
    is.numeric(x) &&
    is.numeric(y) &&
    n > 0L &&
    length(y) == n &&
    length(groups) == n &&
    ## every cell of the data set, as the page shows by default
    identical(as.integer(message$dataset_identity$cell_count), n) &&
    !length(data$point_line) &&
    !identical(x_range, NA) &&
    !identical(y_range, NA) &&
    length(colors) > 0L &&
    length(colors) <= 255L &&
    all(grepl("^#[0-9A-Fa-f]{6}$", colors))
  if (!isTRUE(eligible)) {
    return(NULL)
  }
  ## One byte per cell: its group, 255 for a cell without one.
  codes <- groups
  codes[is.na(codes) | codes < 0L | codes >= length(colors)] <- 255L
  view <- list(
    version = 1L,
    id = "overview_projection",
    cells = n,
    point_size = as.numeric(data$point_size),
    point_opacity = as.numeric(data$point_opacity),
    colors = I(colors),
    code_map = I(seq_along(colors) - 1L)
  )
  ## The names of the groups, as in viewerFirstViewPayload(); a frame that
  ## does not name one for each colour is described without them.
  labels <- as.character(unlist(meta$traces, use.names = FALSE))
  if (length(labels) == length(colors) && !anyNA(labels)) {
    view$labels <- I(labels)
  }
  if (!is.null(x_range) && !is.null(y_range)) {
    view$x_range <- I(x_range)
    view$y_range <- I(y_range)
  }
  list(
    view = view,
    xy = as.numeric(rbind(as.numeric(x), as.numeric(y))),
    codes = as.raw(codes)
  )
}

## Writes the two files of an inline first view into `directory` and returns
## its description. The files are named by their content: a name always means
## the same bytes, so a browser may keep them.
viewerFirstViewWriteInline <- function(inline, key, directory) {
  staging <- file.path(
    directory,
    paste0(key, ".", Sys.getpid(), c(".xy.tmp", ".codes.tmp"))
  )
  on.exit(unlink(staging), add = TRUE)
  connection <- file(staging[[1L]], "wb")
  writeBin(inline$xy, connection, size = 4L, endian = "little")
  close(connection)
  writeBin(inline$codes, staging[[2L]])
  digest <- paste(substr(unname(tools::md5sum(staging)), 1L, 8L), collapse = "")
  names <- paste0(key, ".", digest, c(".xy.bin", ".codes.bin"))
  files <- file.path(directory, names)
  ## files of an earlier content of this page
  old <- list.files(
    directory,
    paste0("^", key, "[.][0-9a-f]+[.](xy|codes)[.]bin$"),
    full.names = TRUE
  )
  unlink(old[!basename(old) %in% names])
  for (index in 1:2) {
    if (!file.exists(files[[index]])) {
      file.rename(staging[[index]], files[[index]])
    }
  }
  view <- inline$view
  view$projection <- list(
    url = paste0("cerebro-first-view/", names[[1L]]),
    bytes = as.numeric(file.size(files[[1L]]))
  )
  view$categories <- list(
    url = paste0("cerebro-first-view/", names[[2L]]),
    bytes = as.numeric(file.size(files[[2L]]))
  )
  view
}

## ---- kept between runs -------------------------------------------------------
## A process that has just started has sent no frame yet, so its first visitor
## would see no preview: every first visit after a start, which for one person
## looking at their own data is every visit. The description is therefore also
## kept in the user's cache directory, with the Pack files named by their place
## in the Pack instead of by this process's URLs. viewerFirstViewRestore()
## publishes the kept descriptions of the configured data sets when a process
## starts, for the same Pack, Viewer version and launch options only.

viewerFirstViewCache <- function() {
  file.path(tools::R_user_dir("CerebroNexus", "cache"), "first-view")
}

## What decides a first frame besides the data set: the Viewer's version and
## the author's options. A description is used only under the basis it was
## taken with.
viewerFirstViewBasis <- function(options) {
  basis <- options[c(
    "cerebro_version",
    "point_size",
    "point_opacity",
    "percentage_cells_to_show",
    "colors",
    "initial_page"
  )]
  file <- tempfile("first-view-basis-")
  on.exit(unlink(file), add = TRUE)
  writeBin(serialize(basis, NULL, version = 2L), file)
  unname(tools::md5sum(file))
}

## When the Pack in `root` was built: its manifest is written last.
viewerFirstViewBuilt <- function(root) {
  format(
    as.numeric(file.mtime(file.path(root, "manifest.json"))),
    digits = 17L
  )
}

## The Pack a shared file URL belongs to, and the file's place in it.
viewerFirstViewPackFile <- function(url, roots) {
  parts <- strsplit(as.character(url), "/", fixed = TRUE)[[1L]]
  directory <- .viewer_shared_resources[[paste("directory", parts[[1L]])]]
  if (length(parts) != 2L || is.null(directory) || !length(roots)) {
    return(NULL)
  }
  root <- roots[startsWith(paste0(directory, "/"), paste0(roots, "/"))]
  if (!length(root)) {
    return(NULL)
  }
  list(
    root = root[[1L]],
    path = paste0(
      substring(directory, nchar(root[[1L]]) + 2L),
      "/",
      parts[[2L]]
    )
  )
}

## Keeps `payload` (the description of the page `key`) for later processes.
viewerFirstViewKeep <- function(payload, key, roots, basis) {
  projection <- viewerFirstViewPackFile(payload$projection$url, roots)
  categories <- viewerFirstViewPackFile(payload$categories$url, roots)
  if (
    is.null(projection) ||
      is.null(categories) ||
      !identical(projection$root, categories$root)
  ) {
    return(invisible(NULL))
  }
  payload$projection$url <- NULL
  payload$projection$path <- projection$path
  payload$categories$url <- NULL
  payload$categories$path <- categories$path
  entry <- list(
    basis = basis,
    pack = projection$root,
    built = viewerFirstViewBuilt(projection$root),
    key = key,
    view = payload
  )
  directory <- viewerFirstViewCache()
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  ## Descriptions nothing has renewed for two months belong to data sets or
  ## versions no longer in use.
  old <- list.files(directory, full.names = TRUE)
  unlink(old[difftime(Sys.time(), file.mtime(old), units = "days") > 60])
  file <- file.path(
    directory,
    paste0(viewerFirstViewSourceTag(projection$root), "-", key, ".json")
  )
  staging <- paste0(file, ".", Sys.getpid(), ".tmp")
  writeLines(
    jsonlite::toJSON(entry, auto_unbox = TRUE, digits = NA),
    staging,
    useBytes = TRUE
  )
  file.rename(staging, file)
  invisible(file)
}

## The name a kept file of the data set `source` starts with.
viewerFirstViewSourceTag <- function(source) {
  file <- tempfile("first-view-source-")
  on.exit(unlink(file), add = TRUE)
  writeLines(enc2utf8(source), file, useBytes = TRUE)
  unname(tools::md5sum(file))
}

## Keeps an inline description (`view`, published in `directory`) of the CRB
## file `source` for later processes, with its two files.
viewerFirstViewKeepInline <- function(view, key, source, basis, directory) {
  names <- basename(c(view$projection$url, view$categories$url))
  cache <- viewerFirstViewCache()
  dir.create(cache, showWarnings = FALSE, recursive = TRUE)
  old <- list.files(cache, full.names = TRUE)
  unlink(old[difftime(Sys.time(), file.mtime(old), units = "days") > 60])
  tag <- viewerFirstViewSourceTag(source)
  kept <- file.path(cache, paste0(tag, "-", names))
  if (!all(file.copy(file.path(directory, names), kept, overwrite = TRUE))) {
    return(invisible(NULL))
  }
  view$projection$url <- NULL
  view$projection$file <- names[[1L]]
  view$categories$url <- NULL
  view$categories$file <- names[[2L]]
  entry <- list(
    basis = basis,
    source = source,
    built = format(as.numeric(file.mtime(source)), digits = 17L),
    key = key,
    view = view
  )
  file <- file.path(cache, paste0(tag, "-", key, ".json"))
  staging <- paste0(file, ".", Sys.getpid(), ".tmp")
  writeLines(
    jsonlite::toJSON(entry, auto_unbox = TRUE, digits = NA),
    staging,
    useBytes = TRUE
  )
  file.rename(staging, file)
  invisible(file)
}

## Publishes a kept inline description again, or returns NULL when it is not
## for one of `sources` as it is now.
viewerFirstViewRestoreInline <- function(entry, basis, sources, directory) {
  view <- entry$view
  names <- c(view$projection$file, view$categories$file)
  usable <- is.list(view) &&
    identical(entry$basis, basis) &&
    is.character(entry$source) &&
    length(entry$source) == 1L &&
    entry$source %in% sources &&
    identical(
      entry$built,
      format(as.numeric(file.mtime(entry$source)), digits = 17L)
    ) &&
    grepl("^[0-9a-f]+$", as.character(entry$key)) &&
    is.character(names) &&
    length(names) == 2L &&
    all(grepl(
      paste0("^", entry$key, "[.][0-9a-f]+[.](xy|codes)[.]bin$"),
      names
    ))
  if (!isTRUE(usable)) {
    return(NULL)
  }
  kept <- file.path(
    viewerFirstViewCache(),
    paste0(viewerFirstViewSourceTag(entry$source), "-", names)
  )
  if (
    !all(file.exists(kept)) ||
      !all(file.copy(kept, file.path(directory, names), overwrite = TRUE))
  ) {
    return(NULL)
  }
  Sys.setFileTime(kept, Sys.time())
  view$projection <- list(
    url = paste0("cerebro-first-view/", names[[1L]]),
    bytes = view$projection$bytes
  )
  view$categories <- list(
    url = paste0("cerebro-first-view/", names[[2L]]),
    bytes = view$categories$bytes
  )
  view
}

## Publishes the kept descriptions of the Packs in `roots` and of the CRB files
## in `sources` that have no Pack. `register` makes a Pack directory reachable
## (viewerSharedResourceRegisterEarly()). Returns the files written.
viewerFirstViewRestore <- function(
  roots,
  basis,
  register,
  sources = character()
) {
  written <- character()
  kept <- list.files(viewerFirstViewCache(), "[.]json$", full.names = TRUE)
  publish <- function(file, entry, view) {
    for (field in c("colors", "code_map", "x_range", "y_range")) {
      if (!is.null(view[[field]])) {
        view[[field]] <- I(unlist(view[[field]], use.names = FALSE))
      }
    }
    json <- jsonlite::toJSON(view, auto_unbox = TRUE, digits = NA)
    target <- file.path(viewerFirstViewDirectory(), paste0(entry$key, ".json"))
    writeLines(json, target, useBytes = TRUE)
    assign(
      paste("first view", entry$key),
      json,
      envir = .viewer_shared_resources
    )
    ## Renewed by use: the two-month limit counts from the last start.
    Sys.setFileTime(file, Sys.time())
    written <<- c(written, target)
  }
  for (file in kept) {
    entry <- tryCatch(
      jsonlite::fromJSON(file, simplifyVector = FALSE),
      error = function(error) NULL
    )
    view <- entry$view
    if (!is.null(entry$source)) {
      view <- viewerFirstViewRestoreInline(
        entry,
        basis,
        sources,
        viewerFirstViewDirectory()
      )
      if (!is.null(view)) {
        publish(file, entry, view)
      }
      next
    }
    pack_file <- function(path) {
      is.character(path) &&
        length(path) == 1L &&
        grepl("^[A-Za-z0-9_-]+/[A-Za-z0-9._-]+$", path) &&
        file.exists(file.path(entry$pack, path))
    }
    usable <- is.list(view) &&
      identical(entry$basis, basis) &&
      is.character(entry$pack) &&
      length(entry$pack) == 1L &&
      entry$pack %in% roots &&
      identical(entry$built, viewerFirstViewBuilt(entry$pack)) &&
      grepl("^[0-9a-f]+$", as.character(entry$key)) &&
      pack_file(view$projection$path) &&
      pack_file(view$categories$path)
    if (!isTRUE(usable)) {
      next
    }
    url <- function(kind, path) {
      prefix <- viewerSharedResourcePrefix(
        kind,
        file.path(entry$pack, dirname(path)),
        roots,
        register
      )
      paste0(prefix, "/", basename(path))
    }
    ## The fields in the order viewerFirstViewPayload() gives them: the frame
    ## of this page's session then describes itself in the same words.
    view$projection <- list(
      url = url("projection", view$projection$path),
      bytes = view$projection$bytes
    )
    view$categories <- list(
      url = url("metadata", view$categories$path),
      bytes = view$categories$bytes
    )
    publish(file, entry, view)
  }
  written
}

## Writes the first view of the page at (`pathname`, `search`) when `message`
## is a frame a preview can stand in for. Returns the file, or NULL.
## With `basis` it is also kept for the processes started later. `source` is
## the CRB file of the frame when that is a configured data set: a frame that
## carries its cells is described only then.
viewerFirstViewCapture <- function(
  message,
  pathname,
  search,
  roots = character(),
  basis = NULL,
  source = NULL
) {
  key <- viewerFirstViewKey(pathname, search)
  payload <- viewerFirstViewPayload(message)
  inline <- is.null(payload) && !is.null(source)
  if (inline) {
    cells <- viewerFirstViewInline(message)
    if (!is.null(cells)) {
      payload <- viewerFirstViewWriteInline(
        cells,
        key,
        viewerFirstViewDirectory()
      )
    }
  }
  if (is.null(payload)) {
    return(NULL)
  }
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA)
  file <- file.path(viewerFirstViewDirectory(), paste0(key, ".json"))
  ## Written once per content: every later session sends the same frame.
  if (!identical(.viewer_shared_resources[[paste("first view", key)]], json)) {
    staging <- paste0(file, ".", Sys.getpid(), ".tmp")
    writeLines(json, staging, useBytes = TRUE)
    file.rename(staging, file)
    assign(paste("first view", key), json, envir = .viewer_shared_resources)
    if (!is.null(basis)) {
      try(
        if (inline) {
          viewerFirstViewKeepInline(
            payload,
            key,
            source,
            basis,
            viewerFirstViewDirectory()
          )
        } else {
          viewerFirstViewKeep(payload, key, roots, basis)
        },
        silent = TRUE
      )
    }
  }
  file
}

## Makes `cerebro-first-view/` reachable and publishes the first views kept
## from earlier runs, so that the first visitor of this process has one too.
## Only where Pack files are shared: not with a login. Once per process:
## launchCerebro() calls it as soon as its web server runs, shiny_server.R
## when the Viewer is prepared.
## `register(prefix, directory)` makes a Viewer Pack directory reachable and
## `serve(prefix, directory)` the directory of the first views: through Shiny,
## or as static paths of the early server, which runs before Shiny is loaded
## (early_server.R).
viewerFirstViewPublishKept <- function(
  options,
  register = viewerSharedResourceRegisterEarly,
  serve = function(prefix, directory) shiny::addResourcePath(prefix, directory)
) {
  if (isTRUE(.viewer_shared_resources$published)) {
    return(invisible(FALSE))
  }
  .viewer_shared_resources$published <- TRUE
  sources <- viewerSharedResourceSources(options)
  if (!length(sources)) {
    return(invisible(FALSE))
  }
  serve("cerebro-first-view", viewerFirstViewDirectory())
  try(
    viewerFirstViewRestore(
      viewerSharedResourceRoots(options),
      viewerFirstViewBasis(options),
      register,
      sources
    ),
    silent = TRUE
  )
  invisible(TRUE)
}

## Continues with what the early server of this process published
## (viewerEarlyServerStart() in early_server.R): the URL prefixes it gave out
## stay the ones of the process, and Shiny learns their directories. First
## views it published for other files or other options are published again.
viewerSharedResourceAdopt <- function(early, options) {
  list2env(
    as.list(early$state, all.names = TRUE),
    envir = .viewer_shared_resources
  )
  for (prefix in names(early$registered)) {
    shiny::addResourcePath(prefix, early$registered[[prefix]])
  }
  if (
    !identical(early$basis, viewerFirstViewBasis(options)) ||
      !identical(early$sources, viewerSharedResourceSources(options))
  ) {
    .viewer_shared_resources$published <- NULL
  }
  invisible(TRUE)
}
