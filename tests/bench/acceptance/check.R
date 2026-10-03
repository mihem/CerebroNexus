#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
bench_root <- dirname(dirname(normalizePath(
  sub("^--file=", "", script_arg[[1L]])
)))
source(file.path(bench_root, "acceptance", "evaluator.R"))
source(file.path(bench_root, "acceptance", "policy.R"))

acceptance_require_result_path <- function(path, result_root) {
  resolved_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  resolved_root <- normalizePath(result_root, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") {
    resolved_path <- tolower(resolved_path)
    resolved_root <- tolower(resolved_root)
  }
  inside <- identical(resolved_path, resolved_root) ||
    startsWith(resolved_path, paste0(resolved_root, "/"))
  if (!inside) {
    stop("acceptance records must be under ", result_root, call. = FALSE)
  }
  invisible(path)
}

acceptance_parse_args <- function(args) {
  if (length(args) < 3L) {
    stop("usage: check.R <plan|judge> <branch> --platform <mac|windows>",
         call. = FALSE)
  }
  command <- args[[1L]]
  branch <- args[[2L]]
  flags <- list()
  i <- 3L
  while (i <= length(args)) {
    flag <- args[[i]]
    if (!grepl("^--", flag)) {
      stop("unexpected argument: ", flag, call. = FALSE)
    }
    name <- sub("^--", "", flag)
    if (name == "json") {
      flags[[name]] <- TRUE
      i <- i + 1L
    } else {
      if (i + 1L > length(args)) {
        stop("missing value for --", name, call. = FALSE)
      }
      flags[[name]] <- args[[i + 1L]]
      i <- i + 2L
    }
  }
  if (!command %in% c("plan", "judge")) {
    stop("unknown command: ", command, call. = FALSE)
  }
  platform <- flags[["platform"]]
  if (is.null(platform) || !platform %in% c("mac", "windows")) {
    stop("--platform must be mac or windows", call. = FALSE)
  }
  list(command = command, branch = branch, platform = platform, flags = flags)
}

acceptance_write_report <- function(judged, run_dir, branch, platform,
                                    context = NULL) {
  verdicts <- judged$verdicts
  if (nrow(verdicts)) {
    utils::write.table(verdicts, file.path(run_dir, "acceptance.tsv"),
                       sep = "\t", quote = FALSE, row.names = FALSE)
  }
  writeLines(acceptance_markdown(judged, branch, platform, context),
             file.path(run_dir, "acceptance.md"))
}

acceptance_plan_lines <- function(parsed, config) {
  entry <- config$branches[[parsed$branch]]
  if (is.null(entry)) {
    stop("unknown branch: ", parsed$branch, call. = FALSE)
  }
  if (!isTRUE(entry$machine)) {
    stop("branch is not machine-judged: ", parsed$branch, call. = FALSE)
  }
  layer <- config$layers[[entry$layer]]
  minimum <- config$sample_min[[acceptance_sample_key(entry$layer, "L1",
                                                      config)]]
  minimum_l2 <- config$sample_min[[acceptance_sample_key(entry$layer, "L2",
                                                         config)]]
  c(
    paste0("branch: ", parsed$branch),
    paste0("layer: ", entry$layer),
    paste0("platform: ", parsed$platform),
    paste0("candidate: ", entry$candidate),
    paste0("baseline: ", entry$parent),
    paste0("L1 rounds minimum: ", minimum),
    paste0("L2 rounds minimum: ", minimum_l2),
    paste0("headline: ", paste(entry$headline, collapse = ", ")),
    paste0("expected raw files: ", layer$files),
    paste0("budget targets: fresh ", config$budgets$fresh_ms, " ms, special ",
           config$budgets$fresh_special_ms, " ms (",
           paste(config$budgets$special_pages, collapse = ", "),
           "), repeat ", config$budgets$repeat_ms, " ms"),
    "commands:",
    paste0("  ", layer$commands[[parsed$platform]]),
    paste0("required file roles: ", paste(layer$file_roles, collapse = ", ")),
    "run-dir layout: run-config.tsv + files.tsv (see acceptance/STANDARD.md section 8.3)"
  )
}

acceptance_write_run_dir_skeleton <- function(path, parsed) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  config <- data.frame(
    branch = parsed$branch, layer = "", platform = parsed$platform,
    level = "L1", profile = "quick", mode = "timing", rounds = "",
    candidate_label = "", candidate_sha = "", baseline_label = "",
    baseline_sha = "", baseline_column = "", candidate_column = "",
    fixture_path = "", fixture_sha256 = "", git_dirty = "",
    created_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  utils::write.table(config, file.path(path, "run-config.tsv"), sep = "\t",
                     quote = FALSE, row.names = FALSE)
  utils::write.table(
    data.frame(role = "", file_role = "", relative_path = "", sha256 = "",
               stringsAsFactors = FALSE),
    file.path(path, "files.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
  invisible(path)
}

main <- function() {
  parsed <- acceptance_parse_args(commandArgs(trailingOnly = TRUE))
  result_root <- parsed$flags[["results-root"]]
  if (is.null(result_root)) {
    result_root <- file.path(bench_root, "results", "acceptance")
  }
  if (identical(parsed$command, "plan")) {
    if (!is.null(parsed$flags[["write-run-dir"]])) {
      run_dir <- parsed$flags[["write-run-dir"]]
      acceptance_require_result_path(run_dir, result_root)
      acceptance_write_run_dir_skeleton(run_dir, parsed)
    }
    writeLines(acceptance_plan_lines(parsed, ACCEPTANCE_CONFIG))
    return(0L)
  }
  run_dir <- parsed$flags[["run-dir"]]
  if (is.null(run_dir) || !dir.exists(run_dir)) {
    stop("--run-dir must point to an existing directory", call. = FALSE)
  }
  acceptance_require_result_path(run_dir, result_root)
  judged <- acceptance_judge(run_dir, parsed$branch, parsed$platform,
                             ACCEPTANCE_CONFIG)
  context <- list(
    version = ACCEPTANCE_CONFIG$version,
    config_sha256 = acceptance_sha256(
      file.path(bench_root, "acceptance", "policy.R"))
  )
  acceptance_write_report(judged, run_dir, parsed$branch, parsed$platform,
                          context)
  if (isTRUE(parsed$flags$json)) {
    cat(acceptance_json(judged, parsed$branch, parsed$platform), "\n")
    if (length(judged$problems)) {
      message("INVALID: ", paste(judged$problems, collapse = "; "))
    }
    message("exit_code: ", judged$exit_code)
  } else {
    if (length(judged$problems)) {
      writeLines(paste0("INVALID: ", judged$problems))
    }
    writeLines(paste0("exit_code: ", judged$exit_code))
  }
  judged$exit_code
}

if (sys.nframe() == 0L) {
  status <- tryCatch(main(), error = function(error) {
    message(conditionMessage(error))
    2L
  })
  quit(status = status, save = "no")
}
