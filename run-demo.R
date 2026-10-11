#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
repo_root <- if (length(script_arg)) {
  dirname(normalizePath(sub("^--file=", "", script_arg[[1L]]), mustWork = TRUE))
} else {
  normalizePath(".", mustWork = TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
supported <- c("--ren-only", "--prepare-only", "--no-browser")
if (any(!args %in% supported)) {
  stop("Supported options: ", paste(supported, collapse = ", "), call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first: install.packages('pkgload')", call. = FALSE)
}
## Always run this checkout, including after switching branches. An installed
## package from an earlier PR must not supply the data/backend implementation.
pkgload::load_all(repo_root, export_all = FALSE, quiet = TRUE, helpers = FALSE)
message("Viewer source: ", repo_root)
source(
  file.path(repo_root, "tests", "bench", "prepare_viewer_1m_data.R"),
  local = TRUE
)
source(
  file.path(repo_root, "tests", "bench", "prepare_viewer_ren_data.R"),
  local = TRUE
)

if (!"--ren-only" %in% args) {
  crb <- prepareViewer1mBenchmarkData()
  Sys.setenv(CEREBRO_1M_DEMO_CRB = normalizePath(crb, mustWork = TRUE))
} else {
  Sys.unsetenv("CEREBRO_1M_DEMO_CRB")
}
ren_crb <- prepareViewerRenDemoData()
Sys.setenv(CEREBRO_REN_DEMO_CRB = normalizePath(ren_crb, mustWork = TRUE))
message("Ren data version: ", .viewerRenVersion, "; file: ", ren_crb)
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
  launch.browser = !"--no-browser" %in% args
)
