#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
source("tests/bench/prepare_viewer_1m_data.R")

crb <- if (length(args)) {
  normalizePath(args[[1L]], mustWork = TRUE)
} else {
  prepareViewer1mBenchmarkData()
}
phase <- if (length(args) >= 2L) args[[2L]] else "all"
if (!phase %in% c("rds", "qs2", "all")) {
  stop("PHASE must be rds, qs2, or all.", call. = FALSE)
}

suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
legacy <- readRDS(crb)
if (nrow(legacy$meta_data) != 1000000L) {
  stop("The CRB must contain exactly 1,000,000 cells.", call. = FALSE)
}
backend <- legacy$getExpressionBackend()
if (!identical(backend$type, "bpcells")) {
  stop("The 1M CRB must use a BPCells backend.", call. = FALSE)
}
sidecar <- normalizePath(
  file.path(dirname(crb), backend$location),
  mustWork = TRUE
)
expected_cells <- colnames(BPCells::open_matrix_dir(sidecar))
if (length(expected_cells) != 1000000L || anyDuplicated(expected_cells)) {
  stop(
    "The BPCells sidecar must contain 1,000,000 unique cells.",
    call. = FALSE
  )
}

root <- tempfile("cerebro-1m-codec-")
dir.create(root)
on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
linked_sidecar <- file.path(root, backend$location)
if (!file.symlink(sidecar, linked_sidecar)) {
  stop("Could not link the shared 1M BPCells sidecar.", call. = FALSE)
}

verify_hydrated <- function(object) {
  stopifnot(
    inherits(object, "Cerebro"),
    identical(object$getCellNames(), expected_cells),
    identical(object$meta_data$cell_barcode, expected_cells),
    identical(rownames(object$getProjection("umap")), expected_cells),
    identical(colnames(object$expression), expected_cells)
  )
  invisible(object)
}

verify_thin_payload <- function(object) {
  stopifnot(
    is.null(object$expression),
    !"cell_barcode" %in% names(object$meta_data),
    .row_names_info(object$projections$umap, type = 1L) ==
      -nrow(object$projections$umap),
    identical(object$crb_schema$version, 1L),
    identical(object$crb_schema$cell_names, "expression"),
    identical(object$crb_schema$projection_rownames, "umap")
  )
  invisible(object)
}

verify_standalone_runtime <- function(path) {
  runtime <- new.env(parent = globalenv())
  sys.source("inst/viewer/utility_functions.R", envir = runtime)
  object <- runtime$read_cerebro_file(path)
  object <- runtime$.attachExternalExpression(object, path)
  verify_hydrated(object)
}

if (phase %in% c("rds", "all")) {
  rds <- file.path(root, "thin-rds.crb")
  saveCerebro(legacy, rds, codec = "rds")
  stopifnot(
    !is.null(legacy$expression),
    "cell_barcode" %in% names(legacy$meta_data),
    identical(rownames(legacy$projections$umap), expected_cells)
  )
  verify_thin_payload(readRDS(rds))
  verify_hydrated(readCerebro(rds))
  verify_standalone_runtime(rds)

  invalid <- readRDS(rds)
  invalid$crb_schema$cell_names_md5 <- paste(rep("0", 32L), collapse = "")
  invalid_path <- file.path(root, "invalid-checksum.crb")
  saveRDS(invalid, invalid_path)
  stopifnot(inherits(
    try(readCerebro(invalid_path), silent = TRUE),
    "try-error"
  ))

  converted <- file.path(root, "converted-rds.crb")
  convertCerebro(crb, converted, codec = "rds")
  verify_thin_payload(readRDS(converted))
  verify_hydrated(readCerebro(converted))
}

if (phase %in% c("qs2", "all")) {
  qs <- file.path(root, "thin-qs2.crb")
  saveCerebro(legacy, qs, codec = "qs2")
  magic <- readBin(qs, "raw", n = 4L)
  stopifnot(identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))))
  verify_hydrated(readCerebro(qs))
  verify_standalone_runtime(qs)
  preflight <- .preflightBundleData(c(dataset = qs))
  stopifnot(identical(preflight$backends$dataset$type, "bpcells"))

  back_to_rds <- file.path(root, "qs2-to-rds.crb")
  convertCerebro(qs, back_to_rds, codec = "rds")
  verify_thin_payload(readRDS(back_to_rds))
  verify_hydrated(readCerebro(back_to_rds))

  default <- file.path(root, "thin-default.crb")
  convertCerebro(crb, default)
  stopifnot(identical(
    readBin(default, "raw", n = 4L),
    as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))
  ))
  verify_hydrated(readCerebro(default))

  in_place <- file.path(root, "converted-in-place.crb")
  stopifnot(file.copy(crb, in_place))
  convertCerebro(in_place)
  stopifnot(identical(
    readBin(in_place, "raw", n = 4L),
    as.raw(c(0x0b, 0x0e, 0x0a, 0xc1))
  ))
  verify_hydrated(readCerebro(in_place))
}

cat("1M CRB", phase, "contract: PASS\n")
