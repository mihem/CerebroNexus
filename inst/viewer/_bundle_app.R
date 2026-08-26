library(dplyr)
library(DT)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

cerebro_root <- "."

if (file.exists("cerebro_config.rds")) {
  Cerebro.options <<- readRDS("cerebro_config.rds")
} else {
  stop("cerebro_config.rds not found!")
}

if (!is.null(Cerebro.options$colors)) {
  colors <- Cerebro.options$colors
}

bundle_run_options <- Cerebro.options$.bundle_run_options
shiny_options <- bundle_run_options$shiny_app_options
embedded_options <- shiny_options
embedded_options[c(
  "port",
  "host",
  "launch.browser",
  "quiet",
  "display.mode"
)] <- NULL

source(file.path(cerebro_root, "viewer/shiny_UI.R"))
source(file.path(cerebro_root, "viewer/shiny_server.R"))
source(file.path(cerebro_root, "viewer/auth.R"), local = TRUE)

viewer_app <- viewer_auth_apply(
  ui,
  server,
  Cerebro.options[[".viewer_auth"]],
  Cerebro.options[["cerebro_root"]]
)

app <- shiny::shinyApp(
  ui = viewer_app$ui,
  server = viewer_app$server,
  onStart = function() {
    previous <- options(
      shiny.maxRequestSize = bundle_run_options$max_request_size_bytes
    )
    shiny::onStop(function() {
      options(previous)
    })
  },
  options = embedded_options
)

if (sys.nframe() == 0L) {
  direct_options <- shiny_options
  direct_options$quiet <- FALSE
  do.call(shiny::runApp, c(list(appDir = app), direct_options))
} else {
  app
}
