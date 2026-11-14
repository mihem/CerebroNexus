## The finished page kept between runs (inst/viewer/ui_cache.R).

ui_cache_env <- function() {
  env <- new.env()
  sys.source(viewer_test_path("ui_cache.R"), env)
  ## the page is kept when the Viewer is quiet (R/quiet_queue.R): at once here
  env$.viewerWhenQuiet <- function(task, ...) task()
  env
}

## A directory that stands for the Viewer's files.
ui_cache_viewer <- function(root) {
  viewer <- file.path(root, "viewer")
  dir.create(file.path(viewer, "www"), recursive = TRUE)
  writeLines("ui <- 1", file.path(viewer, "shiny_UI.R"))
  writeLines("console.log(1)", file.path(viewer, "www", "page.js"))
  viewer
}

test_that("the key names the Viewer's files and the options the page reads", {
  env <- ui_cache_env()
  viewer <- ui_cache_viewer(withr::local_tempdir())
  inputs <- c(version = "8.0.1", www_prefix = "cerebro_www")
  key <- env$viewerUiCacheKey(viewer, inputs)
  expect_match(key, "^[0-9a-f]{32}$")
  expect_identical(env$viewerUiCacheKey(viewer, inputs), key)
  ## the order the inputs are given in does not matter
  expect_identical(env$viewerUiCacheKey(viewer, rev(inputs)), key)

  ## another version, another prefix for www/
  expect_false(identical(
    env$viewerUiCacheKey(
      viewer,
      c(version = "8.0.2", www_prefix = "cerebro_www")
    ),
    key
  ))
  expect_false(identical(
    env$viewerUiCacheKey(
      viewer,
      c(version = "8.0.1", www_prefix = "cerebro_www_1")
    ),
    key
  ))
  ## a file edited (a script the page links by content, an R file), added
  script <- file.path(viewer, "www", "page.js")
  writeLines("console.log(2)  ", script)
  edited <- env$viewerUiCacheKey(viewer, inputs)
  expect_false(identical(edited, key))
  Sys.setFileTime(script, Sys.time() + 60)
  touched <- env$viewerUiCacheKey(viewer, inputs)
  expect_false(identical(touched, edited))
  writeLines("x <- 1", file.path(viewer, "extra.R"))
  expect_false(identical(env$viewerUiCacheKey(viewer, inputs), touched))
  ## the same files somewhere else are another Viewer
  other <- ui_cache_viewer(withr::local_tempdir())
  expect_false(identical(env$viewerUiCacheKey(other, inputs), key))
})

test_that("a page is built once and read back afterwards", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- ui_cache_env()
  built <- 0L
  build <- function() {
    built <<- built + 1L
    shiny::tags$div(
      id = "page",
      shiny::selectInput("choice", "Choice", c("a", "b")),
      shiny::tags$p("text")
    )
  }
  first <- env$viewerUiCached("key-one", build)
  expect_identical(built, 1L)
  expect_true(file.exists(file.path(
    env$viewerUiCacheDirectory(),
    "key-one.rds"
  )))
  second <- env$viewerUiCached("key-one", build)
  expect_identical(built, 1L)
  ## the same page, with the files of other packages it names
  rendered <- htmltools::renderTags(first)
  kept <- htmltools::renderTags(second)
  expect_identical(kept$html, rendered$html)
  expect_identical(
    lapply(kept$dependencies, unclass),
    lapply(rendered$dependencies, unclass)
  )
  expect_true(length(rendered$dependencies) > 0L)
  ## another key is another page
  env$viewerUiCached("key-two", build)
  expect_identical(built, 2L)

  ## turned off: built every time, nothing read or written
  withr::local_options(CerebroNexus.ui_cache = FALSE)
  env$viewerUiCached("key-one", build)
  env$viewerUiCached("key-three", build)
  expect_identical(built, 4L)
  expect_false(file.exists(file.path(
    env$viewerUiCacheDirectory(),
    "key-three.rds"
  )))
})

test_that("a kept page is used only while its packages are as they were", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- ui_cache_env()
  ui <- shiny::tags$div(shiny::selectInput("choice", "Choice", c("a", "b")))
  file <- env$viewerUiCacheWrite("key", ui)
  kept <- readRDS(file)
  ## the packages whose files the page names, and the ones that make its tags
  expect_true(all(c("shiny", "htmltools") %in% kept$packages$package))
  expect_true(all(nzchar(kept$packages$version)))
  expect_true(all(dir.exists(kept$packages$path)))
  expect_false("CerebroNexus" %in% kept$packages$package)
  expect_false(is.null(env$viewerUiCacheRead("key")))

  ## another version of one of them, as after an update: built again
  changed <- kept
  changed$packages$version[[1L]] <- "0.0.0"
  saveRDS(changed, file)
  expect_null(env$viewerUiCacheRead("key"))
  expect_false(file.exists(file))
  ## a package that is no longer there
  moved <- kept
  moved$packages$path[[1L]] <- file.path(tempdir(), "no-such-library", "shiny")
  saveRDS(moved, file)
  expect_null(env$viewerUiCacheRead("key"))
  ## a file that is not a kept page
  writeLines("not an rds file", file)
  expect_null(env$viewerUiCacheRead("key"))
  expect_null(env$viewerUiCacheRead("nothing-kept"))
})

test_that("reading a kept page registers the resource paths building it did", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- ui_cache_env()
  scripts <- normalizePath(withr::local_tempdir(), winslash = "/")
  prefix <- "uicachetestpath"
  withr::defer(try(shiny::removeResourcePath(prefix), silent = TRUE))

  ## Building the page registers a path, as shinyjs::useShinyjs() does.
  build <- function() {
    shiny::addResourcePath(prefix, scripts)
    shiny::tags$div("page")
  }
  env$viewerUiCached("key", build)
  kept <- readRDS(file.path(env$viewerUiCacheDirectory(), "key.rds"))
  expect_identical(kept$resource_paths[[prefix]], scripts)
  ## the Viewer's own paths belong to each process
  expect_length(grep("^cerebro[_-]", names(kept$resource_paths)), 0L)

  ## A process that reads the page has not built it: the path is there again.
  shiny::removeResourcePath(prefix)
  expect_false(prefix %in% names(shiny::resourcePaths()))
  built <- FALSE
  env$viewerUiCached("key", function() {
    built <<- TRUE
    shiny::tags$div("page")
  })
  expect_false(built)
  expect_identical(
    normalizePath(shiny::resourcePaths()[[prefix]], winslash = "/"),
    scripts
  )

  ## A path whose directory is gone: the page is built again.
  kept$resource_paths[[prefix]] <- file.path(scripts, "gone")
  saveRDS(kept, file.path(env$viewerUiCacheDirectory(), "key.rds"))
  env$viewerUiCached("key", function() {
    built <<- TRUE
    shiny::tags$div("page")
  })
  expect_true(built)
})

test_that("pages nothing has read for two months are removed", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- ui_cache_env()
  directory <- env$viewerUiCacheDirectory()
  dir.create(directory, recursive = TRUE)
  stale <- file.path(directory, "stale.rds")
  saveRDS(list(), stale)
  Sys.setFileTime(stale, Sys.time() - 61 * 24 * 3600)
  env$viewerUiCacheWrite("fresh", shiny::tags$div("page"))
  expect_false(file.exists(stale))
  expect_true(file.exists(file.path(directory, "fresh.rds")))
})

test_that("the page files define nothing the server uses", {
  ## A page read back from the cache evaluates neither ui_page.R nor the UI.R
  ## files. What they assign must be used by those files only.
  viewer <- viewer_test_path()
  files <- list.files(viewer, "[.]R$", recursive = TRUE, full.names = TRUE)
  relative <- substring(files, nchar(viewer) + 2L)
  ## ui_page.R, the UI.R of each page, and the two files those source. (The
  ## other UI_*.R files hold server-side renderUI code.)
  page_file <- grepl("^[a-z_]+/UI[.]R$", relative) |
    relative %in% c("ui_page.R", "static_ui.R", "overview/UI_projection.R")
  expect_identical(sum(page_file), 20L)
  ## every file a page file sources is one of them
  sourced <- unique(unlist(lapply(files[page_file], function(file) {
    text <- readLines(file, warn = FALSE)
    found <- regmatches(text, regexpr("viewer/[A-Za-z_/]+[.]R", text))
    sub("^viewer/", "", found)
  })))
  expect_true(all(sourced %in% relative[page_file]))
  expect_true("overview/UI_projection.R" %in% sourced)
  assigned <- unique(unlist(lapply(files[page_file], function(file) {
    unlist(lapply(parse(file, keep.source = FALSE), function(expression) {
      if (
        is.call(expression) &&
          as.character(expression[[1L]])[1L] %in% c("<-", "=") &&
          is.symbol(expression[[2L]])
      ) {
        as.character(expression[[2L]])
      }
    }))
  })))
  expect_true(all(c("tab_overview", "viewerStaticUI") %in% assigned))
  used <- unique(unlist(lapply(files[!page_file], function(file) {
    all.names(parse(file, keep.source = FALSE), unique = TRUE)
  })))
  ## `ui` is the page itself, which shiny_UI.R takes from viewerUiCached().
  expect_identical(setdiff(intersect(assigned, used), "ui"), character())

  ## Nor may a page file change the R process: a page read back skips it. The
  ## paths useShinyjs() registers are kept with the page; extendShinyjs(),
  ## which defines the functions the server calls, is in shiny_UI.R.
  calls <- unique(unlist(lapply(files[page_file], function(file) {
    all.names(parse(file, keep.source = FALSE), unique = TRUE)
  })))
  expect_identical(
    intersect(
      calls,
      c(
        "extendShinyjs",
        "registerInputHandler",
        "addResourcePath",
        "shinyOptions",
        "options",
        "Sys.setenv",
        "<<-",
        "assign",
        "library",
        "require",
        "setMethod",
        "registerS3method"
      )
    ),
    character()
  )
  expect_true("viewer_spatial_page_script" %in% calls)
  ui_file <- parse(file.path(viewer, "shiny_UI.R"), keep.source = FALSE)
  expect_true("extendShinyjs" %in% all.names(ui_file))

  ## and the options the page is made from are the ones in its key
  options_read <- unique(unlist(lapply(
    c(files[page_file], file.path(viewer, "shiny_UI.R")),
    function(file) {
      text <- paste(readLines(file, warn = FALSE), collapse = "\n")
      found <- regmatches(
        text,
        gregexpr('Cerebro[.]options\\[\\["[^"]+"\\]\\]', text)
      )[[1L]]
      sub('"\\]\\]$', "", sub('^Cerebro[.]options\\[\\["', "", found))
    }
  )))
  expect_setequal(options_read, c("cerebro_root", "cerebro_version"))
})

test_that("the page is written through the quiet queue when there is one", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- new.env()
  sys.source(viewer_test_path("ui_cache.R"), env)
  queued <- list()
  env$.viewerWhenQuiet <- function(task, ...) {
    queued[[length(queued) + 1L]] <<- task
  }
  ui <- env$viewerUiCached("queued-key", function() "page")
  expect_identical(ui, "page")
  ## built and returned, not yet written
  expect_false(file.exists(file.path(
    env$viewerUiCacheDirectory(),
    "queued-key.rds"
  )))
  expect_length(queued, 1L)
  queued[[1L]]()
  expect_true(file.exists(file.path(
    env$viewerUiCacheDirectory(),
    "queued-key.rds"
  )))
})
