#!/usr/bin/env Rscript

## Prepares the large demo data sets and opens them together in this
## checkout's Viewer.
##
##   Rscript run-demo.R [--data=1m,ren,cosmx,tcell,allen,eae,ms,xenium-prime,dlpfc,...]
##                      [--prepare-only]
##
## --data names the data sets to prepare (default: 1m, the mouse brain), any
## mix of the two kinds below. --prepare-only downloads and converts without
## starting the Viewer.
##
## 1m, ren, cosmx, tcell, allen, eae and ms are written as CRBs directly. The
## spatial ones have many sections each:
##   allen  Allen whole mouse brain MERFISH atlas, 59 sections of one brain
##   eae    mouse spinal cord in EAE by in situ sequencing, 52 sections
##   ms     human multiple sclerosis spinal cord by Xenium, 6 sections
## (eae and ms: Kukanja et al. 2024).
##
## The Visium data sets are real tissue with their H&E photographs, several
## sections each (data-raw/large-demo/prepare_builder_visium_data.R):
##   dlpfc        human prefrontal cortex, 12 sections, cortical layers
##   mouse_brain  mouse brain, 4 sagittal sections
##   breast       human breast cancer, 2 sections
##   prostate     human prostate, cancer and normal, 2 sections
## Each becomes a SingleCellExperiment file, which the Builder's own build
## steps turn into a CRB, every section with its own photograph and Spatial
## opening as the grid. The file's path is printed: import it in the Builder
## (launchCerebroBuilder()) to build it there yourself.

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

## Each demo: its preparation function and the variable the Viewer reads. A
## demo is prepared again when its stamp no longer matches this checkout
## (.demoStampKey() in data-raw/large-demo/download_helpers.R).
demos <- list(
  "1m" = list(
    prepare = function() prepareViewer1mBenchmarkData(),
    variable = "CEREBRO_1M_DEMO_CRB"
  ),
  ren = list(
    prepare = function() prepareViewerRenDemoData(),
    variable = "CEREBRO_REN_DEMO_CRB"
  ),
  cosmx = list(
    prepare = function() prepareViewerCosMxDemoData(),
    variable = "CEREBRO_COSMX_DEMO_CRB"
  ),
  tcell = list(
    prepare = function() prepareViewerTcellDemoData(),
    variable = "CEREBRO_TCELL_DEMO_CRB"
  ),
  allen = list(
    prepare = function() prepareViewerAllenDemoData(),
    variable = "CEREBRO_ALLEN_DEMO_CRB"
  ),
  eae = list(
    prepare = function() prepareViewerKukanjaDemoData("eae"),
    variable = "CEREBRO_EAE_DEMO_CRB"
  ),
  ms = list(
    prepare = function() prepareViewerKukanjaDemoData("ms"),
    variable = "CEREBRO_MS_DEMO_CRB"
  ),
  "xenium-prime" = list(
    prepare = function() prepareViewerXeniumPrimeDemoData(),
    variable = "CEREBRO_XENIUM_PRIME_DEMO_CRB"
  )
)
## Built by the Builder from the file prepareBuilderVisiumDemoData() writes.
visium_demos <- c("dlpfc", "mouse_brain", "breast", "prostate")
usage <- paste0(
  "Usage: Rscript run-demo.R [--data=1m,ren,cosmx,tcell,allen,eae,ms,xenium-prime,",
  "dlpfc,mouse_brain,breast,prostate] [--prepare-only]"
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
    !all(selected %in% c(names(demos), visium_demos))
) {
  stop(usage, call. = FALSE)
}
if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("Install pkgload first: install.packages('pkgload')", call. = FALSE)
}
## Always run this checkout, including after switching branches. An installed
## package from an earlier PR must not supply the data/backend implementation.
pkgload::load_all(repo_root, export_all = FALSE, quiet = TRUE, helpers = FALSE)
message("Viewer source: ", repo_root)
for (script in c(
  "download_helpers.R",
  "prepare_viewer_1m_data.R",
  "prepare_viewer_ren_data.R",
  "prepare_viewer_cosmx_data.R",
  "prepare_viewer_tcell_data.R",
  "prepare_viewer_allen_data.R",
  "prepare_viewer_kukanja_data.R",
  "prepare_viewer_xenium_prime_data.R",
  "prepare_viewer_pack.R",
  "prepare_builder_visium_data.R"
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
  message(name, " data: ", crb)
}
visium <- intersect(visium_demos, selected)
built <- vapply(
  visium,
  function(id) {
    demo <- prepareViewerVisiumDemoData(id, repo_root)
    message(
      id,
      ": built by the Builder from ",
      normalizePath(prepareBuilderVisiumDemoData(id), winslash = "/")
    )
    normalizePath(demo, winslash = "/", mustWork = TRUE)
  },
  character(1)
)
Sys.setenv(CEREBRO_VISIUM_DEMOS = paste(built, collapse = .Platform$path.sep))

if (!"--prepare-only" %in% args) {
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
