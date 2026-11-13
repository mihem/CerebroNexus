## The finished page, kept between runs.
##
## viewerUiCached(key, build) returns the page `build()` makes, from the user's
## cache directory when a page with this key was kept. The key names everything
## the page is made from, so a kept page is never used for anything else:
##
## - every file of the Viewer (its R files, and the scripts and styles the page
##   links by content hash or carries inline), by size and modification time;
## - the options the UI files read: the Viewer's version, and the prefix under
##   which this process serves www/ (the directory is in the file list);
## - R's version, and the format of the kept file.
##
## The page also names files of other packages (their scripts and styles, as
## htmltools dependencies). The kept file records the version and location of
## each of those packages and of the packages that make the tags; a page is
## used only while all of them are as they were.
##
## Building the page also registers resource paths (shinyjs::useShinyjs() makes
## its scripts reachable under "shinyjs" when it is called). A page read back
## was not built in this process, so the kept file records the paths of other
## packages that are registered once the page is built, and reading it
## registers them again.
##
## Anything else a page file would do to the R process besides making the page
## is lost with a page read back: a page file must not do it. The one case there
## was, shinyjs::extendShinyjs() defining the functions the server calls, is in
## shiny_UI.R, which every process evaluates; a test keeps the page files free
## of such calls.
##
## options(CerebroNexus.ui_cache = FALSE) turns it off.

viewerUiCacheDirectory <- function() {
  file.path(tools::R_user_dir("CerebroNexus", "cache"), "ui")
}

viewerUiCacheKey <- function(viewer, inputs) {
  viewer <- normalizePath(viewer, winslash = "/", mustWork = FALSE)
  files <- sort(list.files(viewer, recursive = TRUE))
  info <- file.info(file.path(viewer, files), extra_cols = FALSE)
  basis <- list(
    format = 1L,
    r = R.version.string,
    viewer = viewer,
    inputs = inputs[order(names(inputs))],
    files = files,
    size = info$size,
    modified = format(as.numeric(info$mtime), digits = 17L)
  )
  file <- tempfile("ui-key-")
  on.exit(unlink(file), add = TRUE)
  writeBin(serialize(basis, NULL, version = 2L), file)
  unname(tools::md5sum(file))
}

## The packages a kept page depends on, with their version and location: the
## ones whose files the page names, and the ones its tags are made by.
viewerUiCachePackages <- function(ui, resource_paths = character()) {
  sources <- unlist(lapply(
    htmltools::renderTags(ui)$dependencies,
    function(dependency) {
      source <- dependency$src$file
      if (is.null(source)) {
        return(NULL)
      }
      if (!is.null(dependency$package)) {
        source <- system.file(source, package = dependency$package)
      }
      normalizePath(source, winslash = "/", mustWork = FALSE)
    }
  ))
  viewerUiCachePackageTable(c(sources, unname(resource_paths)))
}

## The loaded packages that own one of the directories `sources`, and the ones
## that make the tags, with their version and location.
viewerUiCachePackageTable <- function(sources) {
  loaded <- setdiff(loadedNamespaces(), "CerebroNexus")
  paths <- vapply(
    loaded,
    function(package) {
      path <- tryCatch(find.package(package), error = function(error) "")
      if (length(path)) {
        normalizePath(path[[1L]], winslash = "/", mustWork = FALSE)
      } else {
        ""
      }
    },
    character(1)
  )
  owns <- vapply(
    paths,
    function(path) {
      nzchar(path) && any(startsWith(paste0(sources, "/"), paste0(path, "/")))
    },
    logical(1)
  )
  keep <- owns | loaded %in% c("shiny", "htmltools", "shinydashboard")
  data.frame(
    package = loaded[keep],
    path = unname(paths[keep]),
    version = vapply(
      which(keep),
      function(index) {
        viewerUiCachePackageVersion(loaded[[index]], paths[[index]])
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

## TRUE while every package of a kept table is where and what it was.
viewerUiCachePackagesValid <- function(packages) {
  is.data.frame(packages) &&
    nrow(packages) > 0L &&
    all(vapply(
      seq_len(nrow(packages)),
      function(index) {
        identical(
          viewerUiCachePackageVersion(
            packages$package[[index]],
            packages$path[[index]]
          ),
          packages$version[[index]]
        )
      },
      logical(1)
    ))
}

viewerUiCachePackageVersion <- function(package, path) {
  description <- file.path(path, "DESCRIPTION")
  if (!file.exists(description)) {
    return("")
  }
  version <- tryCatch(
    read.dcf(description, fields = "Version")[[1L]],
    error = function(error) NA_character_
  )
  if (is.na(version)) "" else as.character(version)
}

## The resource paths of other packages that are registered now: not the
## Viewer's own, which every process registers for itself.
viewerUiCacheResourcePaths <- function() {
  paths <- shiny::resourcePaths()
  paths <- paths[!grepl("^cerebro[_-]", names(paths))]
  stats::setNames(
    normalizePath(unname(paths), winslash = "/", mustWork = FALSE),
    names(paths)
  )
}

## The kept page for `key`, or NULL: nothing kept, an unreadable file, or a
## package that is no longer where or what it was. Registers the resource
## paths kept with the page.
viewerUiCacheRead <- function(key, directory = viewerUiCacheDirectory()) {
  file <- file.path(directory, paste0(key, ".rds"))
  if (!file.exists(file)) {
    return(NULL)
  }
  kept <- tryCatch(readRDS(file), error = function(error) NULL)
  valid <- is.list(kept) &&
    !is.null(kept$ui) &&
    viewerUiCachePackagesValid(kept$packages)
  resource_paths <- kept$resource_paths
  valid <- isTRUE(valid) &&
    is.character(resource_paths) &&
    (!length(resource_paths) ||
      (!is.null(names(resource_paths)) && all(dir.exists(resource_paths))))
  if (!isTRUE(valid)) {
    unlink(file)
    return(NULL)
  }
  registered <- viewerUiCacheResourcePaths()
  for (prefix in names(resource_paths)) {
    if (
      !identical(unname(registered[prefix]), unname(resource_paths[prefix]))
    ) {
      shiny::addResourcePath(prefix, resource_paths[[prefix]])
    }
  }
  ## Renewed by use: a page nothing has read for two months is removed.
  Sys.setFileTime(file, Sys.time())
  kept$ui
}

viewerUiCacheWrite <- function(key, ui, directory = viewerUiCacheDirectory()) {
  dir.create(directory, showWarnings = FALSE, recursive = TRUE)
  old <- list.files(directory, "[.]rds$", full.names = TRUE)
  unlink(old[difftime(Sys.time(), file.mtime(old), units = "days") > 60])
  file <- file.path(directory, paste0(key, ".rds"))
  staging <- paste0(file, ".", Sys.getpid(), ".tmp")
  on.exit(unlink(staging), add = TRUE)
  resource_paths <- viewerUiCacheResourcePaths()
  saveRDS(
    list(
      ui = ui,
      packages = viewerUiCachePackages(ui, resource_paths),
      resource_paths = resource_paths
    ),
    staging,
    compress = FALSE
  )
  file.rename(staging, file)
  invisible(file)
}

viewerUiCached <- function(key, build) {
  if (!isTRUE(getOption("CerebroNexus.ui_cache", TRUE))) {
    return(build())
  }
  ui <- tryCatch(viewerUiCacheRead(key), error = function(error) NULL)
  if (!is.null(ui)) {
    return(ui)
  }
  ui <- build()
  ## A page that cannot be kept is built again next time. It is written when
  ## the Viewer is quiet (R/quiet_queue.R), not in the middle of the start.
  keep <- function() try(viewerUiCacheWrite(key, ui), silent = TRUE)
  if (exists(".viewerWhenQuiet", mode = "function")) {
    .viewerWhenQuiet(keep)
  } else {
    keep()
  }
  ui
}
