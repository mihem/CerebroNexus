#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(if (length(args)) args[[1L]] else ".", mustWork = TRUE)

helpers <- new.env(parent = globalenv())
sys.source(file.path(root, "inst/viewer/utility_functions.R"), envir = helpers)
sys.source(
  file.path(root, "inst/viewer/coordinated_views/config.R"),
  envir = helpers
)
sys.source(
  file.path(root, "inst/viewer/spatial/func_spatial_helpers.R"),
  envir = helpers
)
sys.source(
  file.path(root, "inst/viewer/spatial/func_projection_update_plot.R"),
  envir = helpers
)

elapsed_ms <- function(work, repeats = 5L) {
  median(vapply(
    seq_len(repeats),
    function(index) {
      gc()
      unname(system.time(work())[["elapsed"]]) * 1000
    },
    numeric(1)
  ))
}

allocation_stats <- function(work) {
  profile <- tempfile("viewer-alloc-")
  on.exit(unlink(profile), add = TRUE)
  gc()
  Rprofmem(profile)
  on.exit(Rprofmem(NULL), add = TRUE)
  work()
  Rprofmem(NULL)
  records <- suppressWarnings(as.numeric(sub(" .*", "", readLines(profile))))
  records <- records[is.finite(records)]
  c(total = sum(records), largest = max(records, 0)) / 1024^2
}

result <- list()
record <- function(metric, scale, before, after, repeats = 5L) {
  before_time <- elapsed_ms(before, repeats)
  after_time <- elapsed_ms(after, repeats)
  before_alloc <- allocation_stats(before)
  after_alloc <- allocation_stats(after)
  result[[length(result) + 1L]] <<- data.frame(
    metric = metric,
    scale = scale,
    before_ms = before_time,
    after_ms = after_time,
    time_delta_pct = (after_time / before_time - 1) * 100,
    before_alloc_mib = before_alloc[["total"]],
    after_alloc_mib = after_alloc[["total"]],
    alloc_delta_pct = (after_alloc[["total"]] / before_alloc[["total"]] - 1) *
      100,
    before_largest_alloc_mib = before_alloc[["largest"]],
    after_largest_alloc_mib = after_alloc[["largest"]],
    check.names = FALSE
  )
}

cells <- sprintf("cell-%06d", seq_len(500000L))
legacy_fingerprint <- function() {
  if (
    !is.character(cells) ||
      anyNA(cells) ||
      any(!nzchar(cells)) ||
      anyDuplicated(cells)
  ) {
    stop("invalid cells")
  }
  sorted <- sort(enc2utf8(cells), method = "radix")
  stream <- paste0(
    nchar(sorted, type = "bytes"),
    ":",
    sorted,
    collapse = ""
  )
  path <- tempfile("legacy-fingerprint-")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw(stream), path)
  paste0("md5-cell-set-v1:", unname(tools::md5sum(path)))
}
streamed_fingerprint <- function() helpers$cv_config_cell_fingerprint(cells)
stopifnot(identical(legacy_fingerprint(), streamed_fingerprint()))
record(
  "configuration fingerprint",
  "500,000 cells",
  legacy_fingerprint,
  streamed_fingerprint,
  repeats = 3L
)

set.seed(20260908)
hover_table <- data.frame(
  cell_barcode = sprintf("cell-%05d", seq_len(50000L)),
  nUMI = sample.int(50000L, 50000L, replace = TRUE),
  nGene = sample.int(8000L, 50000L, replace = TRUE),
  cluster = sprintf("cluster-%02d", sample.int(24L, 50000L, replace = TRUE)),
  sample = sprintf("sample-%02d", sample.int(8L, 50000L, replace = TRUE)),
  condition = c("control", "treated")[sample.int(2L, 50000L, replace = TRUE)],
  stringsAsFactors = FALSE
)
helpers$getGroups <- function() c("cluster", "sample", "condition")
hover_json <- function(value) {
  jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}
legacy_hover <- function() {
  hover_json(list(
    hoverinfo = "text",
    text = I(helpers$buildHoverInfoForProjections(hover_table))
  ))
}
structured_hover <- function() {
  hover_json(
    helpers$buildHoverDataForProjections(hover_table)
  )
}
legacy_hover_value <- legacy_hover()
structured_hover_value <- structured_hover()
record(
  "hover payload build and JSON",
  "50,000 cells; 3 groups",
  legacy_hover,
  structured_hover,
  repeats = 3L
)

image <- tempfile("spatial-background-", fileext = ".png")
connection <- file(image, open = "wb")
writeBin(raw(8L * 1024L * 1024L), connection)
close(connection)
legacy_image <- function() {
  value <- NULL
  for (index in seq_len(5L)) {
    value <- paste0(
      "data:image/png;base64,",
      base64enc::base64encode(image)
    )
  }
  value
}
invisible(helpers$spatialBackgroundDataUri(image))
cached_image <- function() {
  value <- NULL
  for (index in seq_len(5L)) {
    value <- helpers$spatialBackgroundDataUri(image)
  }
  value
}
stopifnot(identical(legacy_image(), cached_image()))
record(
  "external spatial image reuse",
  "8 MiB image; 5 repeat renders",
  legacy_image,
  cached_image,
  repeats = 3L
)

hull_x <- runif(500000L)
hull_y <- runif(500000L)
hull_group <- sprintf("group-%02d", (seq_len(500000L) - 1L) %% 20L)
legacy_hulls <- function() {
  value <- NULL
  for (index in seq_len(5L)) {
    value <- helpers$compute_group_hulls(hull_x, hull_y, hull_group)
  }
  value
}
reactive_hulls <- function() {
  helpers$compute_group_hulls(hull_x, hull_y, hull_group)
}
stopifnot(identical(legacy_hulls(), reactive_hulls()))
record(
  "spatial hull geometry reuse",
  "500,000 cells; 20 groups; 5 style changes",
  legacy_hulls,
  reactive_hulls,
  repeats = 3L
)

output <- do.call(rbind, result)
output$before_output_mib <- c(
  NA_real_,
  nchar(legacy_hover_value, type = "bytes") / 1024^2,
  NA_real_,
  NA_real_
)
output$after_output_mib <- c(
  NA_real_,
  nchar(structured_hover_value, type = "bytes") / 1024^2,
  NA_real_,
  NA_real_
)
write.table(output, row.names = FALSE, sep = "\t", quote = FALSE)
unlink(image)
