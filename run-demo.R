#!/usr/bin/env Rscript

## Prepares the large demo data sets and opens them in this checkout's Viewer.
##
##   Rscript run-demo.R [--data=1m,ren,cosmx,tcell] [--prepare-only]
##
## --data names the data sets to prepare (default: 1m, the mouse brain).
## --prepare-only downloads and converts without starting the Viewer.

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  script <- normalizePath(
    sub("^--file=", "", script_arg[[1L]]),
    mustWork = TRUE
  )
  ## dirname() turns Windows separators into "/"; normalise again so both
  ## branches return the same form.
  normalizePath(dirname(script), mustWork = TRUE)
} else {
  normalizePath(".", mustWork = TRUE)
}

## Each demo: its preparation function, the variable the Viewer reads and the
## version stamp (rebuilt when it changes).
demos <- list(
  "1m" = list(
    prepare = function() prepareViewer1mBenchmarkData(),
    variable = "CEREBRO_1M_DEMO_CRB",
    version = function() .viewer1mVersion
  ),
  ren = list(
    prepare = function() prepareViewerRenDemoData(),
    variable = "CEREBRO_REN_DEMO_CRB",
    version = function() .viewerRenVersion
  ),
  cosmx = list(
    prepare = function() prepareViewerCosMxDemoData(),
    variable = "CEREBRO_COSMX_DEMO_CRB",
    version = function() .viewerCosMxVersion
  ),
  tcell = list(
    prepare = function() prepareViewerTcellDemoData(),
    variable = "CEREBRO_TCELL_DEMO_CRB",
    version = function() .viewerTcellVersion
  )
)

args <- commandArgs(trailingOnly = TRUE)
data_arg <- grep("^--data=", args, value = TRUE)
selected <- if (length(data_arg)) {
  strsplit(sub("^--data=", "", data_arg[[1L]]), ",", fixed = TRUE)[[1L]]
} else {
  "1m"
}
if (
  length(setdiff(args, c(data_arg, "--prepare-only"))) ||
    length(data_arg) > 1L ||
    !length(selected) ||
    !all(selected %in% names(demos))
) {
  stop(
    "Usage: Rscript run-demo.R [--data=1m,ren,cosmx,tcell] [--prepare-only]",
    call. = FALSE
  )
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first: install.packages('pkgload')", call. = FALSE)
}
## Always run this checkout, including after switching branches. An installed
## package from an earlier PR must not supply the data/backend implementation.
pkgload::load_all(repo_root, export_all = FALSE, quiet = TRUE, helpers = FALSE)
message("Viewer source: ", repo_root)
for (script in c(
  "prepare_viewer_1m_data.R",
  "download_helpers.R",
  "prepare_viewer_ren_data.R",
  "prepare_viewer_cosmx_data.R",
  "prepare_viewer_tcell_data.R",
  "prepare_viewer_pack.R"
)) {
  source(file.path(repo_root, "data-raw", "large-demo", script), local = TRUE)
}

for (name in names(demos)) {
  demo <- demos[[name]]
  if (!name %in% selected) {
    Sys.unsetenv(demo$variable)
    next
  }
  crb <- demo$prepare()
  prepareDemoViewerPack(crb)
  do.call(
    Sys.setenv,
    stats::setNames(list(normalizePath(crb, mustWork = TRUE)), demo$variable)
  )
  message(name, " data version ", demo$version(), ": ", crb)
}
if ("--prepare-only" %in% args) {
  quit(save = "no", status = 0)
}

port <- tryCatch(
  httpuv::randomPort(min = 7451L, max = 7451L, n = 1L),
  error = function(error) httpuv::randomPort()
)
message(sprintf("Opening CerebroNexus at http://127.0.0.1:%d", port))

shiny::runApp(
  file.path(repo_root, "inst"),
  host = "127.0.0.1",
  port = port,
  launch.browser = TRUE
)
