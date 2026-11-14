## The start of a Viewer that Shiny runs from an app.R (Shiny Server,
## shiny::runApp() on the directory), in two stages.
##
## Such a process is usually started by the first visitor: Shiny Server stops
## an App nobody uses, and the next page request starts R again. That visitor
## waited for everything app.R did before Shiny could answer: 2.4 s, of which
## the page itself needs 0.5 s. The rest is what only a session uses: plotly,
## DT, dplyr and the other attached packages (0.9 s) and the server code (1 s,
## most of it compiling the server function to byte code).
##
## app.R therefore makes the page and starts; what a session needs is prepared
## once the first page is sent, while the browser fetches the scripts and
## styles of the page, which the web server sends from its own thread. A
## session that arrives sooner prepares it itself and waits as before.
##
## launchCerebro() does the same for an installed package (R/launchCerebro.R),
## with the package namespace and its lazy imports. An app.R has neither: its
## Viewer files find plotly's, DT's and dplyr's functions on the search path,
## so the packages are attached, in the order app.R attached them.
##
## `packages`: attached before the server code is loaded, in this order.
## `load_server()`: loads the server code (it defines `server`) and returns
## the server function.
##
## Returns a list of class "cerebro_staged_start", which app.R keeps as
## `viewer_start` (shiny_UI.R and shiny_server.R look for it):
## - `ui(page)`: the page as Shiny is given it;
## - `server`: the server function Shiny is given;
## - `attach()`: attaches the packages now (a page that has to be built needs
##   them, see viewerUiBuild() in shiny_UI.R);
## - `prepare()`: prepares the Viewer now;
## - `after_first_session(task)`: runs `task()` once the first session has its
##   page (what only keeps something for the next start);
## - `on_start(delay)`: for `onStart` of shiny::shinyApp(). The Viewer is
##   prepared `delay` seconds later when no page was asked for until then.
viewerStagedStart <- function(packages, load_server) {
  state <- new.env(parent = emptyenv())
  state$attached <- FALSE
  state$server <- NULL
  state$error <- NULL
  state$scheduled <- FALSE
  state$jit_release <- function() invisible()
  state$first_session <- FALSE
  state$tasks <- list()

  attach <- function() {
    if (state$attached) {
      return(invisible(FALSE))
    }
    for (package in packages) {
      library(package, character.only = TRUE)
    }
    state$attached <- TRUE
    invisible(TRUE)
  }

  prepare <- function() {
    if (!is.null(state$server)) {
      return(invisible(FALSE))
    }
    ## What stopped it once stops every session with the same message, as an
    ## error while app.R ran stopped the App.
    if (!is.null(state$error)) {
      stop(state$error)
    }
    tryCatch(
      {
        attach()
        state$server <- load_server()
      },
      error = function(error) {
        state$error <- error
        stop(error)
      }
    )
    invisible(TRUE)
  }

  ## Once, after the page that asked for it is sent.
  schedule <- function(delay = 0) {
    if (state$scheduled || !is.null(state$server)) {
      return(invisible(FALSE))
    }
    state$scheduled <- TRUE
    later::later(
      function() {
        ## A session reports what stopped it; nothing listens here.
        try(prepare(), silent = TRUE)
      },
      delay
    )
    invisible(TRUE)
  }

  ## The first session has its page: later ones run the server function
  ## compiled (0.1 s of each), which was left out while that visitor waited
  ## (0.9 s, see shiny_server.R).
  after_first_session <- function() {
    state$jit_release()
    state$server <- tryCatch(
      compiler::cmpfun(state$server),
      error = function(error) state$server
    )
    for (task in state$tasks) {
      later::later(function() try(task(), silent = TRUE), 2)
    }
    state$tasks <- list()
    invisible()
  }
  add_task <- function(task) {
    force(task)
    state$tasks[[length(state$tasks) + 1L]] <- task
    invisible()
  }

  ui <- function(page) {
    function(request) {
      schedule()
      if (is.function(page)) page(request) else page
    }
  }

  server <- function(input, output, session) {
    prepare()
    if (!state$first_session) {
      state$first_session <- TRUE
      later::later(after_first_session, 8)
    }
    state$server(input, output, session)
  }

  on_start <- function(delay = 1) {
    ## The functions a session creates are new for every session and run once
    ## or twice while its first page is made: compiling them to byte code
    ## costs the first session more than it saves. A process that only runs
    ## the App does without until that session has its page; an R session
    ## someone works in keeps its setting.
    if (!interactive()) {
      level <- compiler::enableJIT(0L)
      released <- FALSE
      state$jit_release <- function() {
        if (!released) {
          released <<- TRUE
          compiler::enableJIT(level)
        }
        invisible()
      }
      shiny::onStop(function() state$jit_release())
      later::later(function() state$jit_release(), 60)
    }
    ## A process is started for a visitor, whose page is on its way: the
    ## Viewer is prepared after it. Without a page request, soon.
    schedule(delay)
    invisible()
  }

  structure(
    list(
      ui = ui,
      server = server,
      attach = attach,
      prepare = prepare,
      after_first_session = add_task,
      on_start = on_start
    ),
    class = "cerebro_staged_start"
  )
}

## The page as a file for such an App (core/kept_document.R, which says why,
## and ui_cache.R are loaded where this is called): an earlier process of the
## same Viewer kept the page it sent, with the scripts and the styles of the
## page joined into a few files. This process then serves both as static
## files, which costs no R time and 14 requests instead of 64. The first
## process has nothing kept, sends the page as Shiny makes it and keeps it
## once its first session has its page.
##
## `start`: what viewerStagedStart() returned; `key`: the key of the document
## (the key of the page, and how it is sent). Returns a list: `directory`, of
## the kept document or NULL, and `on_start()` for `onStart` of
## shiny::shinyApp().
viewerStagedDocument <- function(start, key) {
  directory <- NULL
  if (isTRUE(getOption("CerebroNexus.ui_cache", TRUE))) {
    directory <- tryCatch(
      viewerDocumentRestore(key),
      error = function(error) NULL
    )
  } else {
    return(list(directory = NULL, on_start = function() invisible()))
  }
  started <- function() {
    server <- shiny::getShinyOption("server", default = NULL)
    if (is.null(server)) {
      return(invisible(FALSE))
    }
    if (!is.null(directory)) {
      try(viewerDocumentServe(server, directory), silent = TRUE)
    } else {
      start$after_first_session(function() {
        viewerDocumentCapture(server, key)
      })
    }
    invisible(TRUE)
  }
  list(
    directory = directory,
    on_start = function() {
      later::later(started, 0)
      invisible()
    }
  )
}
