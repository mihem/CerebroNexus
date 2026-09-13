#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "usage: viewer_1m_bundle.R BEFORE_ROOT AFTER_ROOT CRB [REPEATS]",
    call. = FALSE
  )
}

before_root <- normalizePath(args[[1L]], mustWork = TRUE)
after_root <- normalizePath(args[[2L]], mustWork = TRUE)
crb_path <- normalizePath(args[[3L]], mustWork = TRUE)
repeats <- if (length(args) >= 4L) as.integer(args[[4L]]) else 3L
if (is.na(repeats) || repeats < 1L) {
  stop("REPEATS must be a positive integer.", call. = FALSE)
}

suppressPackageStartupMessages(devtools::load_all(after_root, quiet = TRUE))
data_set <- readCerebro(crb_path)
if (ncol(data_set$expression) != 1000000L) {
  stop("The benchmark requires the prepared 1M example CRB.", call. = FALSE)
}

bundle_environment <- function(root) {
  env <- new.env(parent = globalenv())
  sys.source(file.path(root, "inst/viewer/utility_functions.R"), envir = env)
  sys.source(
    file.path(root, "inst/viewer/coordinated_views/bundle.R"),
    envir = env
  )
  env
}

canonical_bundle <- function(bundle) {
  bundle$cell_fingerprint <- NULL
  bundle$spaces <- lapply(bundle$spaces, function(space) {
    if (identical(space$id, "umap")) {
      space[c("x", "y", "z", "axes")] <- NULL
    }
    space
  })
  bundle
}

environments <- list(
  before = bundle_environment(before_root),
  after = bundle_environment(after_root)
)
rows <- list()
reference <- list()
for (round in seq_len(repeats)) {
  candidates <- if (round %% 2L) {
    names(environments)
  } else {
    rev(names(environments))
  }
  for (candidate in candidates) {
    gc()
    build_ms <- unname(system.time({
      bundle <- environments[[candidate]]$cv_build_bundle(data_set)
    })[["elapsed"]]) *
      1000
    gc()
    encode_ms <- unname(system.time({
      json <- jsonlite::toJSON(
        bundle,
        auto_unbox = TRUE,
        null = "null",
        na = "null",
        digits = NA
      )
    })[["elapsed"]]) *
      1000
    reference[[candidate]] <- canonical_bundle(bundle)
    rows[[length(rows) + 1L]] <- data.frame(
      candidate = candidate,
      round = round,
      build_ms = build_ms,
      encode_ms = encode_ms,
      object_mib = as.numeric(object.size(bundle)) / 1024^2,
      json_mib = nchar(json, type = "bytes") / 1024^2,
      check.names = FALSE
    )
    rm(bundle, json)
  }
}

if (
  !isTRUE(all.equal(
    reference$before,
    reference$after,
    check.attributes = FALSE
  ))
) {
  stop("The compact bundle changed the Viewer data contract.", call. = FALSE)
}

raw <- do.call(rbind, rows)
summary <- stats::aggregate(
  raw[c("build_ms", "encode_ms", "object_mib", "json_mib")],
  raw["candidate"],
  stats::median
)
summary$check <- "equal"
write.table(summary, row.names = FALSE, sep = "\t", quote = FALSE)
