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
## Returns a list:
## - `ui(page)`: the page as Shiny is given it;
## - `server`: the server function Shiny is given;
## - `attach()`: attaches the packages now (a page that has to be built needs
##   them, see viewerUiBuild() in shiny_UI.R);
## - `prepare()`: prepares the Viewer now;
## - `on_start()`: for `onStart` of shiny::shinyApp().
viewerStagedStart <- function(packages, load_server) {
  state <- new.env(parent = emptyenv())
  state$attached <- FALSE
  state$server <- NULL
  state$error <- NULL
  state$scheduled <- FALSE
  state$jit_release <- function() invisible()
  state$first_session <- FALSE

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

  on_start <- function() {
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
    schedule(1)
    invisible()
  }

  list(
    ui = ui,
    server = server,
    attach = attach,
    prepare = prepare,
    on_start = on_start
  )
}
