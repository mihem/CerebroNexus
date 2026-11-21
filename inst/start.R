## Starts the Viewer and answers before CerebroNexus is loaded.
##
##   Rscript <CerebroNexus>/start.R --port=3838 data.crb
##   Rscript <CerebroNexus>/start.R --port=3838 --host=0.0.0.0 a=first.crb b=second.crb
##
## where <CerebroNexus> is system.file(package = "CerebroNexus"). Or, from R
## code that has not loaded the package:
##
##   source(system.file("start.R", package = "CerebroNexus"))
##   cerebroStart("data.crb", port = 3838)
##
## launchCerebro() can only start its web server once CerebroNexus and shiny
## are loaded. cerebroStart() first starts one with httpuv alone
## (viewer/core/early_server.R), which sends the page, its files and the first
## view of the Projection page, and then loads the package and runs the App
## behind it: a browser has the page 0.2 s sooner. The first process of an
## installation has no page kept yet and starts as launchCerebro() does; so
## does one with options(CerebroNexus.ui_cache = FALSE).
##
## The arguments are those of launchCerebro(); `host` and `port` are the
## address of the App, and `launch.browser` opens it. `root` is the directory
## of the installed package: the one this file is in when it is run as a
## script.
## `require_token = NULL` protects local open-mode launches automatically;
## TRUE forces protection and FALSE opts out. The CLI equivalent is
## --require-token=auto|true|false.
cerebroStart <- function(
  crb_file_to_load = NULL,
  port = 3838L,
  host = "127.0.0.1",
  mode = "closed",
  maxFileSize = 800,
  welcome_message = NULL,
  point_size = 5,
  point_opacity = 1,
  percentage_cells_to_show = 100,
  projections_show_hover_info = TRUE,
  launch.browser = FALSE,
  root = find.package("CerebroNexus"),
  require_token = NULL
) {
  if (
    !is.null(require_token) &&
      (!is.logical(require_token) ||
        length(require_token) != 1L ||
        is.na(require_token))
  ) {
    stop("'require_token' must be NULL, TRUE or FALSE.", call. = FALSE)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  ## The package, and the background processes of the Viewer, are found where
  ## this installation is.
  library <- dirname(root)
  if (!library %in% normalizePath(.libPaths(), winslash = "/")) {
    .libPaths(c(library, .libPaths()))
  }
  port <- as.integer(port)
  ## launchCerebro() explains: no byte-code compiling while the first session
  ## makes its page. It puts the setting back after that.
  jit <- compiler::enableJIT(0L)
  previous_jit <- options(CerebroNexus.jit_level = jit)
  on.exit(
    {
      compiler::enableJIT(jit)
      options(previous_jit)
    },
    add = TRUE
  )
  early <- NULL
  ## The interface may be sent immediately, before loading the package. Data
  ## previews wait for the private token prefix when protection may be needed.
  token_requested <- !identical(require_token, FALSE) &&
    (identical(mode, "open") || isTRUE(require_token))
  if (isTRUE(getOption("CerebroNexus.ui_cache", TRUE))) {
    early <- tryCatch(
      {
        version <- read.dcf(file.path(root, "DESCRIPTION"), "Version")[[1L]]
        env <- new.env(parent = globalenv())
        sys.source(file.path(root, "viewer", "core", "early_server.R"), env)
        env$viewerEarlyServerStart(
          root,
          host,
          port,
          ## What launchCerebro() publishes first views for.
          list(
            cerebro_version = as.character(package_version(version)),
            crb_file_to_load = crb_file_to_load,
            point_size = point_size,
            point_opacity = point_opacity,
            percentage_cells_to_show = percentage_cells_to_show,
            .defer_first_views = token_requested
          )
        )
      },
      error = function(error) NULL
    )
  }
  if (!is.null(early)) {
    on.exit(try(early$server$stop(), silent = TRUE), add = TRUE)
  }
  previous <- options(CerebroNexus.early_server = early)
  on.exit(options(previous), add = TRUE)

  namespace <- loadNamespace("CerebroNexus", lib.loc = library)
  app <- namespace$launchCerebro(
    mode = mode,
    maxFileSize = maxFileSize,
    crb_file_to_load = crb_file_to_load,
    welcome_message = welcome_message,
    point_size = point_size,
    point_opacity = point_opacity,
    percentage_cells_to_show = percentage_cells_to_show,
    projections_show_hover_info = projections_show_hover_info,
    require_token = require_token,
    options = list(host = host, port = port, launch.browser = launch.browser)
  )
  if (is.null(early)) {
    return(shiny::runApp(
      app,
      host = host,
      port = port
    ))
  }
  address <- sprintf(
    "http://%s:%d",
    if (host %in% c("0.0.0.0", "::")) "127.0.0.1" else host,
    port
  )
  message("\nListening on ", address)
  open <- app$options$launch.browser
  on_start <- app$onStart
  app$onStart <- function() {
    if (is.function(on_start)) {
      on_start()
    }
    ## onStart refreshes the per-run credential. Only open/print its URL
    ## afterwards, when Shiny is ready to accept the early server's sockets.
    later::later(
      function() {
        if (is.function(open)) {
          open(address)
        } else if (isTRUE(launch.browser)) {
          utils::browseURL(address)
        }
      },
      0
    )
  }
  ## Shiny's own server: on this machine only, and on a port nobody is given.
  ## The early server takes its requests to the same App.
  shiny::runApp(
    app,
    host = "127.0.0.1",
    port = httpuv::randomPort(),
    launch.browser = FALSE,
    quiet = TRUE
  )
}

## Run as a script (`Rscript start.R ...`, not sourced from other code): the
## arguments of the command line.
local({
  arguments <- commandArgs(trailingOnly = FALSE)
  script <- sub("^--file=", "", arguments[startsWith(arguments, "--file=")])
  if (length(script) != 1L || !identical(basename(script), "start.R")) {
    return(invisible())
  }
  arguments <- commandArgs(trailingOnly = TRUE)
  flags <- arguments[startsWith(arguments, "--")]
  files <- arguments[!startsWith(arguments, "--")]
  flag <- function(name, default) {
    value <- sub(
      paste0("^--", name, "="),
      "",
      flags[startsWith(flags, paste0("--", name, "="))]
    )
    if (length(value)) value[[length(value)]] else default
  }
  token_flag <- flag("require-token", "auto")
  require_token <- switch(
    token_flag,
    auto = NULL,
    true = TRUE,
    false = FALSE,
    stop("--require-token must be auto, true or false.", call. = FALSE)
  )
  ## `name=file.crb` names a data set, as a named vector does.
  named <- grepl("^[^=/\\\\]+=", files)
  crb <- sub("^[^=/\\\\]+=", "", files)
  if (any(named)) {
    names(crb) <- ifelse(named, sub("=.*$", "", files), "")
  }
  cerebroStart(
    crb_file_to_load = if (length(crb)) crb,
    port = as.integer(flag("port", "3838")),
    host = flag("host", "127.0.0.1"),
    mode = flag("mode", "closed"),
    launch.browser = "--browser" %in% flags,
    require_token = require_token,
    root = dirname(normalizePath(script, winslash = "/", mustWork = TRUE))
  )
})
