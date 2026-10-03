# Commands: evidence, check, publish.
if (command %in% c("evidence", "check", "publish")) {
  .bench_cli_handled <- TRUE
if (identical(command, "evidence")) {
  # Write a deterministic checksum inventory for a completed evidence package.

  args <- command_args
  if (length(args) != 1L) {
    stop("usage: cli.R evidence <stage_dir>", call. = FALSE)
  }
  stage <- normalizePath(args[[1]], mustWork = TRUE)
  output <- file.path(stage, "evidence_manifest.csv")
  files <- list.files(
    stage,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  files <- files[
    !dir.exists(files) &
      normalizePath(files) != normalizePath(output, mustWork = FALSE)
  ]
  relative <- substring(files, nchar(stage) + 2L)
  relative <- gsub("\\\\", "/", relative)
  keep <- !startsWith(relative, "logs/")
  files <- files[keep]
  relative <- relative[keep]
  order <- order(relative, method = "radix")
  files <- files[order]
  relative <- relative[order]
  if (!length(files)) {
    stop("cannot inventory an empty evidence package", call. = FALSE)
  }
  info <- file.info(files)
  manifest <- data.frame(
    path = relative,
    bytes = as.numeric(info$size),
    md5 = unname(tools::md5sum(files)),
    stringsAsFactors = FALSE
  )
  utils::write.csv(manifest, output, row.names = FALSE, na = "")
  message("wrote evidence inventory for ", nrow(manifest), " files")
} else if (identical(command, "check")) {
  # Check the complete staged evidence package before publishing it.
  #
  # Usage: Rscript cli.R check <stage_dir>

  args <- command_args
  if (length(args) < 1L) {
    stop("need <stage_dir>", call. = FALSE)
  }
  stage <- normalizePath(args[1], mustWork = TRUE)
  manifest_path <- file.path(stage, "run_manifest.csv")
  if (!file.exists(manifest_path)) {
    stop("missing staged output: run_manifest.csv", call. = FALSE)
  }
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  values <- stats::setNames(as.character(manifest$value), manifest$key)
  profile <- values[["profile"]]
  required <- c(
    "00_probe.csv",
    "05_schedule.csv",
    "10_export.csv",
    "20_access.csv",
    "crashes.csv",
    "query_panel.csv",
    "query_plan_manifest.csv",
    "resource_check.csv",
    "run_manifest.csv",
    "source_manifest.csv",
    "summary.md",
    "evidence_manifest.csv"
  )
  if (identical(profile, "scale")) {
    required <- c(
      required,
      file.path("figures", "expression_backend_benchmark_overview.png"),
      file.path("figures", "expression_backend_benchmark_ceiling.png")
    )
  }
  if (profile %in% c("full", "panel_c2")) {
    required <- c(
      required,
      file.path("figures", "expression_backend_benchmark_overview.png")
    )
  }
  paths <- file.path(stage, required)
  missing <- !file.exists(paths) |
    is.na(file.info(paths)$size) |
    file.info(paths)$size <= 0
  if (any(missing)) {
    stop("missing staged output: ", required[missing][1], call. = FALSE)
  }

  inventory <- utils::read.csv(
    file.path(stage, "evidence_manifest.csv"),
    stringsAsFactors = FALSE
  )
  if (
    !identical(names(inventory), c("path", "bytes", "md5")) ||
      !nrow(inventory) ||
      anyDuplicated(inventory$path) ||
      any(!nzchar(inventory$path)) ||
      any(grepl("(^|/)\\.\\.(/|$)|^/|^[A-Za-z]:|\\\\", inventory$path)) ||
      any(!grepl("^[0-9a-f]{32}$", inventory$md5)) ||
      any(!is.finite(inventory$bytes) | inventory$bytes < 0)
  ) {
    stop("evidence_manifest.csv is invalid", call. = FALSE)
  }
  inventoried_paths <- file.path(stage, inventory$path)
  if (any(!file.exists(inventoried_paths))) {
    stop("evidence inventory names a missing file", call. = FALSE)
  }
  actual_md5 <- unname(tools::md5sum(inventoried_paths))
  actual_bytes <- as.numeric(file.info(inventoried_paths)$size)
  if (
    any(actual_md5 != inventory$md5) || any(actual_bytes != inventory$bytes)
  ) {
    stop("evidence inventory does not match staged files", call. = FALSE)
  }
  required_inventory <- setdiff(
    gsub("\\\\", "/", required),
    "evidence_manifest.csv"
  )
  if (!all(required_inventory %in% inventory$path)) {
    stop(
      "evidence inventory does not cover all required outputs",
      call. = FALSE
    )
  }
  actual_files <- list.files(
    stage,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  actual_files <- actual_files[!dir.exists(actual_files)]
  actual_relative <- substring(actual_files, nchar(stage) + 2L)
  actual_relative <- gsub("\\\\", "/", actual_relative)
  actual_relative <- actual_relative[
    actual_relative != "evidence_manifest.csv" &
      !startsWith(actual_relative, "logs/")
  ]
  if (!setequal(actual_relative, inventory$path)) {
    stop(
      "evidence inventory does not exactly cover staged files",
      call. = FALSE
    )
  }
  message("validated complete staged benchmark outputs")
} else if (identical(command, "publish")) {
  # Publish a validated result directory without replacing prior runs.

  args <- command_args
  if (length(args) < 3L) {
    stop("need <stage_dir> <result_root> <run_id>", call. = FALSE)
  }
  stage <- normalizePath(args[1], mustWork = TRUE)
  result_root <- args[2]
  run_id <- args[3]

  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", run_id)) {
    stop("unsafe run id: ", run_id, call. = FALSE)
  }

  tree_fingerprint <- function(path) {
    files <- list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    files <- files[!dir.exists(files)]
    relative <- substring(files, nchar(path) + 2L)
    order <- order(relative)
    data.frame(
      path = relative[order],
      md5 = unname(tools::md5sum(files[order])),
      stringsAsFactors = FALSE
    )
  }

  source_fingerprint <- tree_fingerprint(stage)
  if (!nrow(source_fingerprint)) {
    stop("staged result directory is empty", call. = FALSE)
  }

  runs <- file.path(result_root, "runs")
  dir.create(runs, recursive = TRUE, showWarnings = FALSE)
  destination <- file.path(runs, run_id)

  if (dir.exists(destination)) {
    if (!identical(source_fingerprint, tree_fingerprint(destination))) {
      stop("conflicting existing run: ", run_id, call. = FALSE)
    }
  } else {
    staged_destination <- tempfile(
      paste0(".stage-", run_id, "-"),
      tmpdir = runs
    )
    dir.create(staged_destination)
    on.exit(
      {
        if (dir.exists(staged_destination)) {
          unlink(staged_destination, recursive = TRUE)
        }
      },
      add = TRUE
    )
    entries <- list.files(
      stage,
      full.names = TRUE,
      all.files = TRUE,
      no.. = TRUE
    )
    copied <- file.copy(
      entries,
      staged_destination,
      recursive = TRUE,
      copy.mode = TRUE,
      copy.date = TRUE
    )
    if (
      !all(copied) ||
        !identical(
          source_fingerprint,
          tree_fingerprint(staged_destination)
        )
    ) {
      stop("failed to stage an exact result copy", call. = FALSE)
    }
    if (!file.rename(staged_destination, destination)) {
      stop("failed to publish immutable result directory", call. = FALSE)
    }
  }

  if (identical(Sys.getenv("BENCH_PUBLISH_FAIL_AT"), "before-pointer")) {
    stop("injected failure before CURRENT update", call. = FALSE)
  }

  pointer <- file.path(result_root, "CURRENT")
  pointer_stage <- tempfile(".CURRENT-", tmpdir = result_root)
  writeLines(run_id, pointer_stage, useBytes = TRUE)
  if (!file.rename(pointer_stage, pointer)) {
    copied <- file.copy(pointer_stage, pointer, overwrite = TRUE)
    unlink(pointer_stage)
    if (!copied) {
      stop("failed to update CURRENT result pointer", call. = FALSE)
    }
  }
  message("published immutable benchmark run ", run_id)
} else {
  stop("unknown benchmark command: ", command, call. = FALSE)
}
}
