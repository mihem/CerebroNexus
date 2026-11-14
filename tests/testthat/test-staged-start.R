## The start of an app.R in two stages (inst/viewer/staged_start.R).

staged_start_env <- function() {
  env <- new.env(parent = globalenv())
  sys.source(viewer_test_path("staged_start.R"), env)
  env
}

test_that("nothing a session uses is loaded until the page is sent", {
  env <- staged_start_env()
  loaded <- 0L
  start <- env$viewerStagedStart(
    packages = "stats",
    load_server = function() {
      loaded <<- loaded + 1L
      function(input, output, session) "session"
    }
  )
  page <- shiny::tags$div("page")
  ui <- start$ui(page)
  expect_identical(loaded, 0L)

  ## The page is answered first; the Viewer is prepared after it.
  expect_identical(ui(list()), page)
  expect_identical(loaded, 0L)
  later::run_now(0.5)
  expect_identical(loaded, 1L)

  ## Once for the process, whoever asks.
  expect_identical(ui(list()), page)
  later::run_now(0.2)
  expect_false(start$prepare())
  expect_identical(start$server(NULL, NULL, NULL), "session")
  expect_identical(loaded, 1L)
})

test_that("a page that is a function of the request stays one", {
  env <- staged_start_env()
  start <- env$viewerStagedStart(character(), function() function(...) NULL)
  ui <- start$ui(function(request) request$PATH_INFO)
  expect_identical(ui(list(PATH_INFO = "/a")), "/a")
  later::run_now(0.2)
})

test_that("a session that comes before the page was asked for prepares itself", {
  env <- staged_start_env()
  loaded <- 0L
  start <- env$viewerStagedStart(
    packages = character(),
    load_server = function() {
      loaded <<- loaded + 1L
      function(input, output, session) session
    }
  )
  expect_identical(start$server(NULL, NULL, "first"), "first")
  expect_identical(start$server(NULL, NULL, "second"), "second")
  expect_identical(loaded, 1L)
})

test_that("the packages are attached in the order given, once", {
  env <- staged_start_env()
  attached <- character()
  testthat::local_mocked_bindings(
    library = function(package, character.only = FALSE, ...) {
      attached <<- c(attached, package)
      invisible()
    },
    .package = "base"
  )
  start <- env$viewerStagedStart(c("b", "a"), function() function(...) NULL)
  expect_true(start$attach())
  expect_false(start$attach())
  start$prepare()
  expect_identical(attached, c("b", "a"))
})

test_that("what stops the preparation stops every session with its message", {
  env <- staged_start_env()
  tried <- 0L
  start <- env$viewerStagedStart(
    packages = character(),
    load_server = function() {
      tried <<- tried + 1L
      stop("a CRB file that cannot be read")
    }
  )
  ui <- start$ui("page")
  ## The page is still sent; nobody listens to the preparation behind it.
  expect_identical(ui(list()), "page")
  expect_no_error(later::run_now(0.5))
  expect_error(start$server(NULL, NULL, NULL), "cannot be read")
  expect_error(start$server(NULL, NULL, NULL), "cannot be read")
  expect_identical(tried, 1L)
})

test_that("the server function is compiled after the first session", {
  env <- staged_start_env()
  start <- env$viewerStagedStart(
    packages = character(),
    load_server = function() {
      function(input, output, session) paste("ses", "sion", sep = "")
    }
  )
  start$server(NULL, NULL, NULL)
  state <- environment(start$server)$state
  expect_identical(typeof(.Internal(bodyCode(state$server))), "language")
  environment(start$server)$after_first_session()
  expect_identical(typeof(.Internal(bodyCode(state$server))), "bytecode")
  expect_identical(start$server(NULL, NULL, NULL), "session")
})

test_that("inst/app.R attaches no session package before it starts", {
  code <- parse(
    inst_test_path("app.R"),
    keep.source = FALSE
  )
  attached <- unlist(lapply(code, function(expression) {
    if (
      is.call(expression) && identical(expression[[1L]], as.name("library"))
    ) {
      as.character(expression[[2L]])
    }
  }))
  expect_identical(attached, "shiny")
  text <- paste(
    vapply(code, function(e) paste(deparse(e), collapse = " "), ""),
    collapse = "\n"
  )
  expect_match(text, "viewerStagedStart", fixed = TRUE)
  expect_no_match(text, "source\\(\"viewer/shiny_server.R\", local = TRUE\\)")
})

staged_document_env <- function() {
  env <- staged_start_env()
  sys.source(viewer_test_path("ui_cache.R"), env)
  sys.source(viewer_test_path("core", "kept_document.R"), env)
  env
}

test_that("the first process of an app.R keeps its page after the first session", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- staged_document_env()
  captured <- NULL
  env$viewerDocumentCapture <- function(server, key) captured <<- key
  web_server <- list(setStaticPath = function(.list) stop("nothing to serve"))
  shiny::shinyOptions(server = web_server)
  withr::defer(shiny::shinyOptions(server = NULL))

  start <- env$viewerStagedStart(character(), function() function(...) NULL)
  document <- env$viewerStagedDocument(start, "a-key")
  expect_null(document$directory)
  document$on_start()
  later::run_now(0.2)
  ## not before a session has its page
  expect_null(captured)
  start$server(NULL, NULL, NULL)
  environment(start$server)$after_first_session()
  waited <- 0
  while (is.null(captured) && waited < 5) {
    later::run_now(0.1)
    waited <- waited + 0.1
  }
  expect_identical(captured, "a-key")
})

test_that("a later process of an app.R serves the kept page as a file", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  env <- staged_document_env()
  unlink(env$viewerDocumentBundleDirectory(), recursive = TRUE)
  html <- charToRaw(paste0(
    "<!DOCTYPE html>\n<html><head>",
    "<script src=\"shiny-javascript-1.0/shiny.min.js\"></script>",
    "</head><body>page</body></html>"
  ))
  expect_false(is.null(env$viewerDocumentKeep("a-key", html)))
  env$viewerDocumentCapture <- function(...) stop("the page is kept already")
  paths <- NULL
  shiny::shinyOptions(
    server = list(setStaticPath = function(.list) paths <<- .list)
  )
  withr::defer(shiny::shinyOptions(server = NULL))

  start <- env$viewerStagedStart(character(), function() function(...) NULL)
  document <- env$viewerStagedDocument(start, "a-key")
  expect_identical(
    readBin(file.path(document$directory, "index.html"), "raw", n = 1e6),
    html
  )
  document$on_start()
  later::run_now(0.2)
  expect_named(paths, "/")
  expect_true(paths[["/"]]$options$indexhtml)

  ## another way of sending the page is another document
  expect_null(env$viewerStagedDocument(start, "another-key")$directory)
  ## and nothing is kept or served when the page is not kept either
  withr::local_options(CerebroNexus.ui_cache = FALSE)
  expect_null(env$viewerStagedDocument(start, "a-key")$directory)
})
