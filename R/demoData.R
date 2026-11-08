## Demo files that are too large for the package tarball. They stay in the
## GitHub repository and are downloaded on request from a fixed release tag
## into the user cache. Paths are relative to the installed package root.
.demo_data_tag <- "v7.0.0"
.demo_data_url <- paste0(
  "https://raw.githubusercontent.com/mihem/CerebroNexus/",
  .demo_data_tag,
  "/inst/"
)
.demo_data_files <- data.frame(
  path = c(
    "extdata/examples/demo_spatial_visium.crb",
    "extdata/examples/demo_spatial_slideseq.crb",
    "extdata/examples/demo_spatial_merfish.crb",
    "extdata/examples/demo_spatial_xenium.crb",
    "extdata/examples/demo_hla_tcr_dextramer.crb",
    "builder/fixtures/trekker_spatial/trekker_4_tissues.qs2",
    "builder/fixtures/trekker_spatial/tissue_01_he.jpg",
    "builder/fixtures/trekker_spatial/tissue_02_he.jpg",
    "builder/fixtures/trekker_spatial/tissue_03_he.jpg",
    "builder/fixtures/trekker_spatial/tissue_04_he.jpg",
    "builder/fixtures/spatial_alignment/visium_breast_cancer.rds",
    "builder/fixtures/spatial_alignment/visium_breast_cancer_he.jpg"
  ),
  size = c(
    759430,
    471639,
    1091169,
    743245,
    666436,
    1157977,
    178655,
    176908,
    176905,
    176367,
    178744,
    60318
  ),
  md5 = c(
    "d90af44866611f1ed0a8b6bee69fecaf",
    "c93472444c76588e6a68c3d082f79a02",
    "b0c01e44b6d91e3681819ec1535d1585",
    "48400923c2ae408858f32c65bef13e19",
    "e5e962959b1e0349483aafa88ca48aca",
    "b98ccd6a7941f38c771a523dc6ef1227",
    "77d1c60474eb05c19dd30c8d704d96b8",
    "71f87d64ca80801d1ff7857c5e7b5d81",
    "1a41735961966a8bb004ba4097f328bc",
    "fa778ec553dcda52a4e3ba0b501ed727",
    "15a538b8c70f1192efaaff77e7c359a3",
    "106b73c0eb251101e424ea4f585e37b5"
  ),
  stringsAsFactors = FALSE
)

## The demo data sets of the bundled demo app that downloadDemoData() offers.
## The Trekker demo is derived from vendor data that may not be redistributed,
## so it is only part of the source repository.
.demo_datasets <- c(
  "Mouse brain (Visium)" = "extdata/examples/demo_spatial_visium.crb",
  "Mouse hippocampus (Slide-seq v2)" = "extdata/examples/demo_spatial_slideseq.crb",
  "Mouse ileum (MERFISH)" = "extdata/examples/demo_spatial_merfish.crb",
  "Mouse brain (Xenium)" = "extdata/examples/demo_spatial_xenium.crb",
  "HLA & TCR" = "extdata/examples/demo_hla_tcr_dextramer.crb"
)

.demoDataCacheRoot <- function() {
  file.path(
    tools::R_user_dir("CerebroNexus", "cache"),
    "demo-data",
    .demo_data_tag
  )
}

## Where a demo file is: the installed copy when the package has it (a source
## checkout does), else its place in the user cache, which may not exist yet.
.demoDataPath <- function(path) {
  installed <- system.file(path, package = "CerebroNexus")
  if (nzchar(installed)) {
    return(installed)
  }
  file.path(.demoDataCacheRoot(), path)
}

## The registered demo files in one directory, e.g. a Builder example.
.demoDataFiles <- function(directory) {
  .demo_data_files$path[dirname(.demo_data_files$path) == directory]
}

## Downloads the registered demo files in `paths` that are neither installed
## nor cached. A file is written to a temporary name first and moved into place
## only when its size and MD5 sum match. Returns the local paths.
.demoDataFetch <- function(paths, quiet = FALSE) {
  unknown <- setdiff(paths, .demo_data_files$path)
  if (length(unknown)) {
    stop("Unknown demo file: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  old <- options(timeout = max(300, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  for (path in paths) {
    target <- .demoDataPath(path)
    if (file.exists(target)) {
      next
    }
    file <- .demo_data_files[.demo_data_files$path == path, ]
    url <- paste0(.demo_data_url, path)
    if (!quiet) {
      message(
        "Downloading ",
        basename(path),
        sprintf(" (%.1f MB) from ", file$size / 1024^2),
        url
      )
    }
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(basename(path), tmpdir = dirname(target))
    ok <- tryCatch(
      utils::download.file(url, temporary, mode = "wb", quiet = TRUE) == 0L,
      error = function(error) FALSE,
      warning = function(warning) FALSE
    )
    ok <- isTRUE(ok) &&
      identical(unname(file.size(temporary)), file$size) &&
      identical(unname(tools::md5sum(temporary)), file$md5) &&
      file.rename(temporary, target)
    if (!ok) {
      unlink(temporary)
      stop(
        "Could not download the demo file ",
        basename(path),
        " from ",
        url,
        ". Check the internet connection and try again.",
        call. = FALSE
      )
    }
  }
  vapply(paths, .demoDataPath, character(1), USE.NAMES = FALSE)
}

#' @title
#' Download the larger demo data sets
#'
#' @description
#' The spatial (Visium, Slide-seq, MERFISH, Xenium) and HLA & TCR demo data
#' sets are not included in the package to keep it small.
#' \code{downloadDemoData()} downloads their \code{.crb} files from the
#' CerebroNexus GitHub repository (release tag \code{v7.0.0}) into the user
#' cache, \code{tools::R_user_dir("CerebroNexus", "cache")}, and checks the
#' size and MD5 sum of each file. Files that are already there are not
#' downloaded again.
#'
#' The demo app shipped with the package,
#' \code{shiny::runApp(system.file(package = "CerebroNexus"))}, lists the
#' downloaded data sets next to the bundled one. The tissue images of the
#' Visium and Xenium demos are only part of the demo app in the source
#' repository.
#'
#' @param datasets Names of the demo data sets to download; defaults to all of
#' them: "Mouse brain (Visium)", "Mouse hippocampus (Slide-seq v2)",
#' "Mouse ileum (MERFISH)", "Mouse brain (Xenium)" and "HLA & TCR".
#'
#' @return
#' A named character vector with the paths of the downloaded \code{.crb} files,
#' invisibly. It can be passed to \code{launchCerebro(crb_file_to_load = ...)}.
#'
#' @examples
#' if (interactive()) {
#'   files <- downloadDemoData("Mouse brain (Visium)")
#'   launchCerebro(mode = "closed", crb_file_to_load = files)
#' }
#'
#' @export
downloadDemoData <- function(datasets = names(.demo_datasets)) {
  unknown <- setdiff(datasets, names(.demo_datasets))
  if (!is.character(datasets) || !length(datasets) || length(unknown)) {
    stop(
      "'datasets' must name demo data sets from: ",
      paste0('"', names(.demo_datasets), '"', collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  paths <- .demoDataFetch(unname(.demo_datasets[datasets]))
  invisible(stats::setNames(paths, datasets))
}
