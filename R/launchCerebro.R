#' @title
#' Launch CerebroNexus
#'
#' @description
#' Launch the CerebroNexus Shiny application.
#'
#' @param mode Cerebro can be ran in \code{open} or \code{closed} mode, allowing
#' the user to load their own data set (\code{open}) or only show a pre-loaded
#' data set (\code{closed}, removes the "Load data" element); defaults to
#' \code{open}.
#' @param maxFileSize Maximum size of input file; defaults to \code{800}
#' (800 MB).
#' @param crb_file_to_load Path to \code{.crb} file to load on launch of
#' Cerebro. Useful when using/hosting Cerebro in \code{closed} mode. To offer
#' several data sets, pass a \emph{named} character vector, e.g.
#' \code{c(sampleA = "a.crb", sampleB = "b.crb")}: every name is the stable,
#' unique data set identity used in links and saved state. The files are read
#' when the App starts, so a file that cannot be read stops the running App
#' with its error. Defaults to \code{NULL}.
#' @param welcome_message \code{string} with custom welcome message to display
#' in the "Load data" tab. Can contain HTML formatting, e.g.
#' \code{'<h3>Hi!</h3>'}. Defaults to \code{NULL}.
#' @param point_size Default point size for cell scatter views. This value can
#'   be changed in the UI; defaults to 5.
#' @param point_opacity Default point opacity for cell scatter views. This value
#'   can be changed in the UI; defaults to 1.
#' @param percentage_cells_to_show Default percentage of cells shown in every
#'   cell scatter view. This value can be changed independently in each page;
#'   defaults to 100.
#' @param projections_show_hover_info Show hover information in projections. This
#' setting can be changed in the UI; defaults to TRUE.
#' @param ... Further parameters that are used by \code{shiny::runApp}, e.g.
#' \code{host} or \code{port}.
#'
#' @details
#' To answer the first request sooner, start the App with the file
#' \code{start.R} of the installed package instead of calling this function:
#' \code{Rscript <package directory>/start.R --port=3838 data.crb}, with
#' \code{system.file(package = "CerebroNexus")} as the package directory, or
#' \code{source(system.file("start.R", package = "CerebroNexus"));
#' cerebroStart("data.crb", port = 3838)} in R. It starts a small web server
#' before CerebroNexus is loaded, which sends the page of the previous run,
#' its files and the first view of the Projection page, and then runs
#' \code{launchCerebro()} behind it; the App keeps the address it was given.
#' The first start of an installation has no page to send yet and starts as
#' \code{launchCerebro()} does.
#'
#' @return
#' Shiny application.
#'
#' @examples
#' if ( interactive() ) {
#'   launchCerebro(
#'     mode = "open",
#'     maxFileSize = 800
#'   )
#' }
#'
#' @importFrom grDevices col2rgb rgb
#' @import shiny
#' @importFrom htmltools save_html
#' @import shinydashboard
#' @importFrom shinyjs inlineCSS
#'
#' @export
#'
launchCerebro <- function(
  mode = "open",
  maxFileSize = 800,
  crb_file_to_load = NULL,
  welcome_message = NULL,
  point_size = 5,
  point_opacity = 1,
  percentage_cells_to_show = 100,
  projections_show_hover_info = TRUE,
  ...
) {
  ##--------------------------------------------------------------------------##
  ## Check validity of input parameters.
  ##--------------------------------------------------------------------------##
  if (mode %in% c('open', 'closed') == FALSE) {
    stop(
      "'mode' parameter must be set to either 'open' or 'closed'.",
      call. = FALSE
    )
  }
  limits <- list(
    point_size = c(1, 20),
    point_opacity = c(0.1, 1),
    percentage_cells_to_show = c(10, 100)
  )
  for (name in names(limits)) {
    value <- get(name, inherits = FALSE)
    limit <- limits[[name]]
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        !is.finite(value) ||
        value < limit[[1]] ||
        value > limit[[2]]
    ) {
      stop(
        "'",
        name,
        "' parameter must be between ",
        limit[[1]],
        " and ",
        limit[[2]],
        call. = FALSE
      )
    }
  }
  if (projections_show_hover_info %in% c(TRUE, FALSE) == FALSE) {
    stop(
      "'projections_show_hover_info' parameter must be set to either TRUE or FALSE.",
      call. = FALSE
    )
  }
  if (length(crb_file_to_load) > 1L) {
    file_names <- names(crb_file_to_load)
    if (
      is.null(file_names) ||
        anyNA(file_names) ||
        any(!nzchar(file_names)) ||
        anyDuplicated(file_names)
    ) {
      stop(
        "'crb_file_to_load' needs unique, non-empty names when more than one ",
        "file is given, e.g. crb_file_to_load = c(sampleA = \"a.crb\", ",
        "sampleB = \"b.crb\"). Each name is the data set identity used in ",
        "links and saved state.",
        call. = FALSE
      )
    }
  }

  ## --------------------------------------------------------------------------##
  ## Options the Viewer reads. The Viewer files are sourced into this function's
  ## environment below, so they find `Cerebro.options` here without touching
  ## the global environment.
  ## --------------------------------------------------------------------------##
  Cerebro.options <- list(
    "mode" = mode,
    "cerebro_version" = as.character(
      utils::packageVersion("CerebroNexus")
    ),
    "crb_file_to_load" = crb_file_to_load,
    "welcome_message" = welcome_message,
    "cerebro_root" = system.file(package = "CerebroNexus"),
    "point_size" = point_size,
    "point_opacity" = point_opacity,
    "percentage_cells_to_show" = percentage_cells_to_show,
    "projections_show_hover_info" = projections_show_hover_info,
    ## Imports left unloaded so far; the Viewer loads them while it is idle.
    ".idle_packages" = .lazy_imports_idle,
    ## The Viewer does not read the configured CRBs at its first session:
    ## viewer_prepare() below reads them and hands them over.
    ".launch_reads_datasets" = TRUE
  )
  viewer_env <- environment()

  ##--------------------------------------------------------------------------##
  ## Prepare the Viewer: everything a session needs, done once.
  ##
  ## Not while launchCerebro() runs. It takes 1.3 s with 10,000 cells and 4.5 s
  ## with 1.3 million, and none of it is needed to start the web server or to
  ## send the page (see below). It runs as soon as the App has started, or when
  ## the App is first asked for its page or a session, whichever comes first;
  ## what it reports (a CRB file that cannot be read) then stops the App.
  ##--------------------------------------------------------------------------##
  viewer_jit_release <- function() invisible()
  viewer_jit_scheduled <- FALSE
  viewer_prepared <- FALSE
  viewer_prepare_error <- NULL
  viewer_prepare <- function() {
    if (viewer_prepared) {
      return(invisible(FALSE))
    }
    if (!is.null(viewer_prepare_error)) {
      stop(viewer_prepare_error)
    }
    tryCatch(
      {
        ## The installed Viewer has no HLA core source; core_shim.R binds
        ## these.
        Cerebro.options[[".hla_core"]] <<- .viewerHLACore()
        ## The UI functions before the data sets are read: the UI needs none
        ## of their content, and with a large CRB in memory every garbage
        ## collection while it is built costs more (0.5 s for a 1.3 million
        ## cell data set).
        source(
          system.file(
            paste0("viewer/shiny_UI.R"),
            package = "CerebroNexus"
          ),
          local = viewer_env
        )
        ## What every session needs is loaded before the data sets are in
        ## memory too (see R/lazy_imports.R), and so are the server
        ## functions: loading them starts the background work on the
        ## configured files (the cell names of a Viewer Pack), which then
        ## runs while the data sets are read.
        for (package in .lazy_imports_session) {
          loadNamespace(package)
        }
        source(
          system.file(
            paste0("viewer/shiny_server.R"),
            package = "CerebroNexus"
          ),
          local = viewer_env
        )
        ## The Viewer's own helper (viewer/crb_cache.R, which a bundled App
        ## has no other way to reach): raised before the data sets are read.
        .reserveCrbHeap(crb_file_to_load)
        dataset_catalog <- .datasetCatalogFromFiles(
          crb_file_to_load,
          retain_prototypes = TRUE
        )
        ## Dataset cataloguing already validated and deserialized these
        ## immutable CRB prototypes. Hand them to the Viewer process cache so
        ## the first analysis page does not read the same multi-gigabyte file
        ## a second time. The server's one-time handoff then owns these
        ## references: a copy kept here would defeat later prototype
        ## eviction.
        viewerLaunchPrototypes(attr(
          dataset_catalog,
          "prototypes",
          exact = TRUE
        ))
        attr(dataset_catalog, "prototypes") <- NULL
        Cerebro.options[[".dataset_catalog"]] <<- dataset_catalog
        rm(dataset_catalog)
        viewer_prepared <<- TRUE
      },
      error = function(error) {
        viewer_prepare_error <<- error
        stop(error)
      }
    )
    invisible(TRUE)
  }
  ## The page and the server function Shiny is given: the Viewer's own, once
  ## it is prepared.
  viewer_ui <- function(request) {
    ## A request for the page that reaches R before the Viewer is prepared
    ## (the web server answers "/" from the file only once viewer_started()
    ## has run) gets the kept page as it is, without waiting for the Viewer.
    if (!viewer_prepared && !is.null(viewer_document)) {
      document <- file.path(viewer_document, "index.html")
      return(shiny::httpResponse(
        200L,
        "text/html; charset=UTF-8",
        readBin(document, "raw", n = file.size(document)),
        headers = list("Cache-Control" = "no-cache")
      ))
    }
    viewer_prepare()
    ui
  }
  viewer_server <- function(input, output, session) {
    viewer_prepare()
    if (!viewer_jit_scheduled) {
      ## The first session has its page long before this, and the collection
      ## of everything read and built so far (.collectBeforeSessions()) no
      ## longer holds it back: before the session it took 0.4 s of the 1.4 s
      ## to the plot with 10,000 cells, and the session's own collections
      ## cost as much with 1.3 million.
      viewer_jit_scheduled <<- TRUE
      later::later(
        function() {
          viewer_jit_release()
          try(.collectBeforeSessions(), silent = TRUE)
        },
        8
      )
    }
    server(input, output, session)
  }

  ##--------------------------------------------------------------------------##
  ## Launch Cerebro.
  ##--------------------------------------------------------------------------##
  message(
    paste0(
      '##---------------------------------------------------------------------------##\n',
      '## Launching CerebroNexus\n',
      '##---------------------------------------------------------------------------##'
    )
  )
  ## Shiny Server cannot carry binary WebSocket frames; wrap them there only.
  ## Its helpers read `Cerebro.options` from this function's environment.
  transport_env <- new.env(parent = environment())
  sys.source(
    system.file("viewer/transport_compat.R", package = "CerebroNexus"),
    envir = transport_env
  )
  transport <- transport_env$viewerApplyTransportCompat(
    viewer_ui,
    viewer_server,
    system.file(package = "CerebroNexus")
  )

  ##--------------------------------------------------------------------------##
  ## What a page can be given before the Viewer is prepared: the page as a
  ## file (viewer/core/kept_document.R) and the first views kept from earlier
  ## runs (viewer/core/shared_resources.R, published in viewer_started()). Both
  ## only read small files. Without them the App starts as before and the first
  ## page waits for the Viewer.
  ##--------------------------------------------------------------------------##
  viewer_document <- NULL
  viewer_document_capture <- FALSE
  ## start.R (in the package directory) answers on the App's address before
  ## this package is loaded: a web server that already sends the kept page,
  ## its files and the first views (viewer/core/early_server.R). The App then
  ## continues with what that server published, and takes its requests once
  ## Shiny's own server runs.
  viewer_early <- getOption("CerebroNexus.early_server")
  if (
    !inherits(viewer_early, "cerebro_early_server") ||
      !isTRUE(tryCatch(
        viewer_early$server$isRunning(),
        error = function(error) FALSE
      ))
  ) {
    viewer_early <- NULL
  }
  ## The key of the page (viewerUiCacheKey() in viewer/ui_cache.R), as
  ## shiny_UI.R computes it. www/ is served under "cerebro_www" unless another
  ## App of this process holds that name, which shiny_UI.R finds out.
  viewer_document_key <- function(www_prefix = "cerebro_www") {
    if (!is.null(viewer_early) && identical(www_prefix, "cerebro_www")) {
      return(viewer_early$key)
    }
    viewerUiCacheKey(
      file.path(system.file(package = "CerebroNexus"), "viewer"),
      c(
        version = Cerebro.options[["cerebro_version"]],
        www_prefix = as.character(www_prefix)
      )
    )
  }
  try(
    {
      for (file in c(
        "viewer/core/shared_resources.R",
        "viewer/ui_cache.R",
        "viewer/core/kept_document.R"
      )) {
        source(system.file(file, package = "CerebroNexus"), local = viewer_env)
      }
      if (!is.null(viewer_early)) {
        source(
          system.file("viewer/core/early_server.R", package = "CerebroNexus"),
          local = viewer_env
        )
        viewerSharedResourceAdopt(viewer_early, Cerebro.options)
        viewer_document <- viewerDocumentRestore(viewer_document_key())
      } else if (
        isTRUE(getOption("CerebroNexus.ui_cache", TRUE)) &&
          !isTRUE(transport_env$viewerSockjsCompatEnabled())
      ) {
        viewer_document <- viewerDocumentRestore(viewer_document_key())
        viewer_document_capture <- is.null(viewer_document)
      }
    },
    silent = TRUE
  )
  ## Once the web server runs: the page as a file, the first views kept from
  ## earlier runs with the headers of their Viewer Pack files, and then the
  ## Viewer itself.
  viewer_started <- function() {
    web_server <- shiny::getShinyOption("server", default = NULL)
    if (!is.null(viewer_early) && !is.null(web_server)) {
      ## From here on "the server" is the early one and Shiny's together.
      web_server <- tryCatch(
        viewerEarlyServerAdopt(viewer_early, web_server),
        error = function(error) web_server
      )
    }
    if (!is.null(web_server) && !is.null(viewer_document)) {
      try(viewerDocumentServe(web_server, viewer_document), silent = TRUE)
    }
    try(viewerFirstViewPublishKept(Cerebro.options), silent = TRUE)
    try(viewerSharedResourceHeaders(), silent = TRUE)
    later::later(
      function() {
        prepared <- tryCatch(
          {
            viewer_prepare()
            TRUE
          },
          error = function(error) {
            ## runApp() reports it, as launchCerebro() used to.
            shiny::stopApp(stop(error))
            FALSE
          }
        )
        ## The first process of an installation keeps the page it sends.
        if (prepared && viewer_document_capture && !is.null(web_server)) {
          viewer_document_capture <<- FALSE
          later::later(
            function() {
              try(
                viewerDocumentCapture(
                  web_server,
                  viewer_document_key(cerebro_www_prefix)
                ),
                silent = TRUE
              )
            },
            1
          )
        }
      },
      ## Not at once: a request that arrived before "/" was answered from the
      ## file is on its way to R, and is answered first (viewer_ui()).
      0.05
    )
  }

  ## Allow uploads up to `maxFileSize` MB while the app runs; the previous
  ## value is restored when it stops. A user `onStart` still runs afterwards.
  app_args <- list(...)
  if (is.null(app_args[["uiPattern"]])) {
    app_args[["uiPattern"]] <- transport_env$viewerDatasetUiPattern(
      names(Cerebro.options[["crb_file_to_load"]])
    )
  }
  user_on_start <- app_args[["onStart"]]
  max_request_size <- maxFileSize * 1024^2
  app_args[["onStart"]] <- function() {
    previous <- options(shiny.maxRequestSize = max_request_size)
    ## From here the App runs, and its first session has no use for compiling
    ## (see .viewerJitSuspend()). Put back after that session has its page
    ## (viewer_server()), when the App stops, and after a minute at the latest.
    viewer_jit_release <<- .viewerJitSuspend()
    shiny::onStop(function() {
      options(previous)
      viewer_jit_release()
    })
    later::later(viewer_jit_release, 60)
    later::later(viewer_started, 0)
    if (is.function(user_on_start)) {
      user_on_start()
    }
  }
  do.call(
    shiny::shinyApp,
    c(list(ui = transport$ui, server = transport$server), app_args)
  )
}

## R compiles a function to byte code the second time it is called. The
## functions a session creates (its server function and everything it defines)
## are new for every session and run once or twice while the first page is
## made, so compiling them costs more than it saves: 0.3 s of the 1.4 s from
## the start of the session to the plot with 10,000 cells, 0.6 s of 2.7 s with
## 1.3 million. A process that only runs the App (`Rscript`, or start.R, which
## has switched compiling off already and says what to put back in
## `options(CerebroNexus.jit_level)`) does without until the first session has
## its page. An R session someone is working in keeps its setting.
##
## The setting to return to is remembered once for the process, by the first
## App that switches compiling off; every App that does counts, and compiling
## is switched on again when the last of them has let go. Returns the function
## that lets go (once).
.viewer_jit <- new.env(parent = emptyenv())
.viewerJitSuspend <- function() {
  started <- getOption("CerebroNexus.jit_level")
  if (is.null(.viewer_jit$original)) {
    if (!is.null(started)) {
      .viewer_jit$original <- as.integer(started)
    } else if (interactive()) {
      return(function() invisible())
    } else {
      .viewer_jit$original <- compiler::enableJIT(0L)
    }
    .viewer_jit$holders <- 0L
  }
  .viewer_jit$holders <- .viewer_jit$holders + 1L
  released <- FALSE
  function() {
    if (released) {
      return(invisible())
    }
    released <<- TRUE
    .viewer_jit$holders <- .viewer_jit$holders - 1L
    if (.viewer_jit$holders <= 0L) {
      compiler::enableJIT(.viewer_jit$original)
      .viewer_jit$original <- NULL
      options(CerebroNexus.jit_level = NULL)
    }
    invisible()
  }
}

## Collects once the first session has its page, while nobody waits. Everything
## read and built before is still young to R's collector: the first collection
## of it takes 0.4 s with 10,000 cells and more with 1.3 million, which is why
## it does not run before that session (it did, and delayed the plot by as
## much). After this one the collections of later sessions only look at what
## they allocate themselves.
##
## How often a session collects depends on the room left above what is in use.
## R enlarges its heap only after a full collection that leaves it more than
## 70 % full, and a process can end up just below that: a session then
## collects 11 to 15 times instead of 4. A block held through a second
## collection makes R take that step now.
.collectBeforeSessions <- function() {
  usage <- gc(verbose = FALSE, full = TRUE)
  cells <- ceiling(0.72 * usage["Ncells", 3L] - usage["Ncells", 1L])
  if (cells <= 0) {
    return(invisible(FALSE))
  }
  block <- as.list(seq_len(cells))
  gc(verbose = FALSE, full = TRUE)
  rm(block)
  invisible(TRUE)
}
