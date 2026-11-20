#!/usr/bin/env Rscript

## Prepares the large demo data sets and opens them in this checkout's Viewer,
## or, for the multi-section Visium data sets, in its Builder.
##
##   Rscript run-demo.R [--data=1m,ren,cosmx,tcell] [--prepare-only]
##   Rscript run-demo.R --data=dlpfc,mouse_brain,breast,prostate [--prepare-only]
##
## --data names the data sets to prepare (default: 1m, the mouse brain). Each
## prepared Viewer data set also gets a compressed twin of its H5 file, listed
## in the menu as "<name> [compressed]". --prepare-only downloads and converts
## without starting the Viewer or the Builder.
##
## The Visium data sets are real tissue with their H&E photographs, several
## sections each (data-raw/large-demo/prepare_builder_visium_data.R):
##   dlpfc        human prefrontal cortex, 12 sections, cortical layers
##   mouse_brain  mouse brain, 4 sagittal sections
##   breast       human breast cancer, 2 sections
##   prostate     human prostate, cancer and normal, 2 sections
## Each becomes a SingleCellExperiment file for the Builder. The Builder opens
## with the files' paths printed here: import them, and Build makes the Viewer
## App, every section with its own photograph.

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
## Builder input, prepared by prepareBuilderVisiumDemoData().
builder_demos <- c("dlpfc", "mouse_brain", "breast", "prostate")
usage <- paste0(
  "Usage: Rscript run-demo.R [--data=1m,ren,cosmx,tcell] [--prepare-only]\n",
  "       Rscript run-demo.R --data=dlpfc,mouse_brain,breast,prostate ",
  "[--prepare-only]"
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
    !all(selected %in% c(names(demos), builder_demos))
) {
  stop(usage, call. = FALSE)
}
for_builder <- selected %in% builder_demos
if (any(for_builder) && !all(for_builder)) {
  stop(
    "The Visium data sets open in the Builder and the others in the Viewer; ",
    "prepare them in separate runs.\n",
    usage,
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
  "h5_transposed_write.R",
  "prepare_viewer_1m_data.R",
  "download_helpers.R",
  "prepare_viewer_ren_data.R",
  "prepare_viewer_cosmx_data.R",
  "prepare_viewer_tcell_data.R",
  "prepare_viewer_pack.R",
  "h5_compressed_variant.R",
  "prepare_builder_visium_data.R"
)) {
  source(file.path(repo_root, "data-raw", "large-demo", script), local = TRUE)
}

if (all(for_builder)) {
  inputs <- vapply(selected, prepareBuilderVisiumDemoData, character(1))
  message("Builder input:")
  for (name in selected) {
    message("  ", name, ": ", normalizePath(inputs[[name]], winslash = "/"))
  }
  if (!"--prepare-only" %in% args) {
    message(
      "In the Builder, import each file; Build makes the Viewer App, ",
      "every section with its own photograph."
    )
    launchCerebroBuilder(launch_browser = TRUE)
  }
} else {
  for (name in names(demos)) {
    demo <- demos[[name]]
    if (!name %in% selected) {
      Sys.unsetenv(demo$variable)
      next
    }
    crb <- demo$prepare()
    prepareDemoViewerPack(crb)
    prepareDemoViewerPack(prepareCompressedDemoVariant(crb))
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
}
