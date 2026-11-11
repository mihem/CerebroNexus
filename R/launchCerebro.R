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
#' unique data set identity used in links and saved state. Defaults to
#' \code{NULL}.
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
#' @import dplyr
#' @importFrom DT datatable formatPercentage formatRound formatSignif formatStyle styleColorBar styleEqual styleInterval
#' @importFrom grDevices col2rgb rgb
#' @importFrom plotly add_lines add_trace event_data layout plot_ly plotlyOutput renderPlotly toWebGL
#' @import shiny
#' @importFrom htmltools save_html
#' @importFrom colourpicker colourInput
#' @importFrom shinycssloaders withSpinner
#' @import shinydashboard
#' @importFrom shinyjs inlineCSS
#' @importFrom shinyWidgets awesomeCheckbox dropdownButton materialSwitch radioGroupButtons sendSweetAlert
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
    ## The installed Viewer has no HLA core source; core_shim.R binds these.
    ".hla_core" = .viewerHLACore(),
    "point_size" = point_size,
    "point_opacity" = point_opacity,
    "percentage_cells_to_show" = percentage_cells_to_show,
    "projections_show_hover_info" = projections_show_hover_info
  )

  ##--------------------------------------------------------------------------##
  ## Load the UI functions before the data sets are read: the UI needs none of
  ## their content, and with a large CRB in memory every garbage collection
  ## while it is built costs more (0.5 s for a 1.3 million cell data set).
  ##--------------------------------------------------------------------------##
  source(
    system.file(
      paste0("viewer/shiny_UI.R"),
      package = "CerebroNexus"
    ),
    local = TRUE
  )

  ##--------------------------------------------------------------------------##
  ## Read the data sets, then load the server functions that take them over.
  ##--------------------------------------------------------------------------##
  .reserveHeapForCrb(crb_file_to_load)
  dataset_catalog <- .datasetCatalogFromFiles(
    crb_file_to_load,
    retain_prototypes = TRUE
  )
  ## Dataset cataloguing already validated and deserialized these immutable
  ## CRB prototypes. Hand them to the Viewer process cache so the first
  ## analysis page does not read the same multi-gigabyte file a second time.
  Cerebro.options[[".dataset_prototypes"]] <- attr(
    dataset_catalog,
    "prototypes",
    exact = TRUE
  )
  attr(dataset_catalog, "prototypes") <- NULL
  Cerebro.options[[".dataset_catalog"]] <- dataset_catalog
  rm(dataset_catalog)
  source(
    system.file(
      paste0("viewer/shiny_server.R"),
      package = "CerebroNexus"
    ),
    local = TRUE
  )
  ## The server's one-time handoff now owns these references. Keeping copies
  ## in launch options/closure would defeat later prototype eviction.
  Cerebro.options[[".dataset_prototypes"]] <- NULL
  .collectBeforeSessions()

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
    ui,
    server,
    system.file(package = "CerebroNexus")
  )
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
    shiny::onStop(function() options(previous))
    if (is.function(user_on_start)) {
      user_on_start()
    }
  }
  do.call(
    shiny::shinyApp,
    c(list(ui = transport$ui, server = transport$server), app_args)
  )
}

## Raises R's heap trigger before large CRBs are read, as .reserveCrbHeap() in
## inst/viewer/crb_cache.R does for a Viewer that reads them itself: a request
## larger than the free heap makes R collect once and grow its trigger, instead
## of collecting several times while a CRB is deserialized. The unused block
## is released by the next collection.
.reserveHeapForCrb <- function(paths) {
  sizes <- suppressWarnings(file.size(as.character(paths)))
  largest <- max(c(0, sizes), na.rm = TRUE)
  bytes <- min(16 * largest, 4 * 1024^3)
  if (bytes < 256 * 1024^2) {
    return(invisible(FALSE))
  }
  invisible(tryCatch(
    {
      block <- raw(bytes)
      rm(block)
      TRUE
    },
    error = function(error) FALSE
  ))
}

## Collects at the end of launchCerebro(), while nobody waits. Everything read
## and built there is still young to R's collector, so the first browser
## session would start with a full collection of it (0.5-1 s with a 1.3 million
## cell data set in memory, counted against the first view); after this one its
## collections only look at what the session itself allocates.
##
## How often a session collects depends on the room left above what is in use.
## R enlarges its heap only after a full collection that leaves it more than
## 70 % full, and a process can end up just below that: the first session then
## collected 11 to 15 times before its first view instead of 4, which took
## 0.25 s with 10,000 cells and up to 0.7 s with 1.3 million. A block held
## through a second collection makes R take that step now.
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
