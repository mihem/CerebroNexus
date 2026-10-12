# Offline, lossless repacking of demo CRBs. Run from the repository root:
# Rscript data-raw/compact_demo_crbs.R inst/extdata/examples /path/to/staging
# The source files are never replaced. Review and test the staged files before
# publishing them. This does not change saveCerebro() or its default settings.
compact_demo_crbs <- function(input_dir, output_dir, compress_level = 19L) {
  input_dir <- normalizePath(input_dir, mustWork = TRUE)
  if (file.exists(output_dir)) {
    stop("Use a new output directory; existing data will not be overwritten.")
  }
  files <- list.files(input_dir, pattern = "\\.crb$", full.names = TRUE)
  if (!length(files)) {
    stop("No demo CRBs found.")
  }
  if (!dir.create(output_dir, recursive = TRUE)) {
    stop("Cannot create staging directory.")
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)

  rows <- lapply(files, function(input) {
    # Read the serialized payload without attaching backends or modifying it.
    # Only qs2 CRBs are accepted; migration of other formats is a separate task.
    object <- qs2::qs_read(input)
    original <- serialize(object, NULL, version = 3L)
    output <- file.path(output_dir, basename(input))
    stage <- tempfile(".compact-", output_dir)
    on.exit(unlink(stage), add = TRUE)
    qs2::qs_save(object, stage, compress_level = compress_level, nthreads = 1L)
    restored <- qs2::qs_read(stage)
    if (!identical(original, serialize(restored, NULL, version = 3L))) {
      stop("Lossless round-trip verification failed: ", basename(input))
    }
    # Keep the exact original bytes if repacking offers no saving.
    smaller <- file.size(stage) < file.size(input)
    if (smaller) {
      if (!file.rename(stage, output)) {
        stop("Cannot publish staged file: ", output)
      }
    } else {
      if (!file.copy(input, output)) stop("Cannot copy original file: ", output)
    }
    data.frame(
      file = basename(input),
      before_bytes = file.size(input),
      after_bytes = file.size(output),
      repacked = smaller,
      identical_payload = TRUE
    )
  })
  report <- do.call(rbind, rows)
  write.csv(report, file.path(output_dir, "compression.csv"), row.names = FALSE)
  report
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 2L) {
    stop("Usage: Rscript data-raw/compact_demo_crbs.R INPUT_DIR NEW_OUTPUT_DIR")
  }
  if (!requireNamespace("CerebroNexus", quietly = TRUE)) {
    stop("Install CerebroNexus and its dependencies before compacting demos.")
  }
  print(compact_demo_crbs(args[[1L]], args[[2L]]), row.names = FALSE)
}
