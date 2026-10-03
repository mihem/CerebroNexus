# Pure acceptance helpers for the perf acceptance standard.

acceptance_tolerance_limit <- function(baseline_median, tolerance) {
  baseline_median * (1 + tolerance$rel) + tolerance$floor
}

acceptance_metric_verdict <- function(baseline_median, candidate_median,
                                      tolerance, level = "L1") {
  if (!is.finite(baseline_median) || !is.finite(candidate_median)) {
    return(list(verdict = "INVALID", limit = NA_real_, delta = NA_real_,
                delta_pct = NA_real_))
  }
  limit <- acceptance_tolerance_limit(baseline_median, tolerance)
  delta <- candidate_median - baseline_median
  delta_pct <- if (baseline_median == 0) NA_real_ else delta / baseline_median
  if (identical(level, "L1") && identical(tolerance$l1_verdict, "REPORT")) {
    return(list(verdict = "REPORT", limit = limit, delta = delta,
                delta_pct = delta_pct))
  }
  verdict <- if (candidate_median <= limit) "PASS" else "FAIL"
  list(verdict = verdict, limit = limit, delta = delta, delta_pct = delta_pct)
}

acceptance_budget_verdict <- function(baseline_median, candidate_median, budget) {
  if (!is.finite(baseline_median) || !is.finite(candidate_median) ||
      !is.finite(budget)) {
    return(list(verdict = "INVALID", baseline_state = NA,
                candidate_state = NA))
  }
  baseline_state <- baseline_median < budget
  candidate_state <- candidate_median < budget
  verdict <- if (baseline_state && candidate_state) {
    "PASS"
  } else if (baseline_state && !candidate_state) {
    "FAIL"
  } else if (!baseline_state && candidate_state) {
    "REPORT"
  } else {
    "TRACK"
  }
  list(verdict = verdict, baseline_state = baseline_state,
       candidate_state = candidate_state)
}

acceptance_headline_improvement <- function(baseline_median, candidate_median) {
  if (!is.finite(baseline_median) || !is.finite(candidate_median) ||
      baseline_median <= 0) {
    return(NA_real_)
  }
  (baseline_median - candidate_median) / baseline_median
}

acceptance_headline_verdict <- function(improvements, threshold = 0.10,
                                        justified = FALSE) {
  improvements <- improvements[is.finite(improvements)]
  if (!length(improvements)) {
    return("FAIL")
  }
  if (any(improvements >= threshold)) {
    return("PASS")
  }
  if (isTRUE(justified)) "REPORT" else "FAIL"
}

acceptance_exit_code <- function(verdicts) {
  values <- verdicts$verdict
  if (any(values %in% "INVALID")) {
    return(2L)
  }
  if (any(values %in% "FAIL")) {
    return(1L)
  }
  0L
}

acceptance_as_bool <- function(values) {
  toupper(trimws(as.character(values))) %in% c("TRUE", "T", "1", "YES")
}

acceptance_read_table <- function(path) {
  if (grepl("[.]tsv$", path, ignore.case = TRUE)) {
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

acceptance_long_row <- function(metric_id, scope, visit, role, value, unit,
                                tolerance_class, correctness, budget = NA_real_) {
  data.frame(
    metric_id = metric_id,
    scope = if (is.na(scope)) "" else as.character(scope),
    visit = if (is.na(visit)) "" else as.character(visit),
    role = role,
    value = as.numeric(value),
    unit = unit,
    tolerance_class = tolerance_class,
    correctness = as.logical(correctness),
    budget = as.numeric(budget),
    stringsAsFactors = FALSE
  )
}

acceptance_extract_crb <- function(rows, baseline_label, candidate_label,
                                   correctness = TRUE) {
  units <- c(size_mib = "bytes", write_median_ms = "latency",
             decode_median_ms = "latency", hydrated_median_ms = "latency")
  out <- lapply(names(units), function(metric) {
    do.call(rbind, lapply(c("baseline", "candidate"), function(role) {
      label <- if (role == "baseline") baseline_label else candidate_label
      row <- rows[rows$candidate == label, , drop = FALSE]
      if (nrow(row) != 1L) {
        stop("expected exactly one CRB row for label ", label, call. = FALSE)
      }
      tolerance_class <- units[[metric]]
      unit <- if (identical(tolerance_class, "bytes")) "MiB" else "ms"
      acceptance_long_row(metric, "crb", NA_character_, role, row[[metric]][[1L]],
                          unit, tolerance_class, correctness)
    }))
  })
  do.call(rbind, out)
}

acceptance_extract_hot_paths <- function(rows, baseline_column, candidate_column) {
  if (!baseline_column %in% names(rows) || !candidate_column %in% names(rows)) {
    stop("hot-path columns not found: ", baseline_column, ", ", candidate_column,
         call. = FALSE)
  }
  baseline_stem <- sub("_ms$", "", baseline_column)
  candidate_stem <- sub("_ms$", "", candidate_column)
  baseline_alloc <- paste0(baseline_stem, "_alloc_mib")
  candidate_alloc <- paste0(candidate_stem, "_alloc_mib")
  if (!baseline_alloc %in% names(rows) || !candidate_alloc %in% names(rows)) {
    stop("hot-path allocation columns not found.", call. = FALSE)
  }
  correctness <- all(rows$check == "equal")
  out <- list()
  for (i in seq_len(nrow(rows))) {
    for (metric in c("_ms", "_alloc_mib")) {
      baseline_value <- rows[[paste0(baseline_stem, metric)]][[i]]
      candidate_value <- rows[[paste0(candidate_stem, metric)]][[i]]
      metric_id <- paste0(rows$metric[[i]], metric)
      unit <- if (identical(metric, "_ms")) "ms" else "MiB"
      tolerance_class <- if (identical(metric, "_ms")) "latency" else "allocation"
      out[[length(out) + 1L]] <- acceptance_long_row(
        metric_id, rows$scale[[i]], NA_character_, "baseline",
        baseline_value, unit, tolerance_class, correctness)
      out[[length(out) + 1L]] <- acceptance_long_row(
        metric_id, rows$scale[[i]], NA_character_, "candidate",
        candidate_value, unit, tolerance_class, correctness)
    }
  }
  do.call(rbind, out)
}

acceptance_extract_bundle <- function(rows, baseline_label, candidate_label) {
  units <- c(build_ms = "latency", encode_ms = "latency",
             object_mib = "bytes", json_mib = "bytes")
  out <- lapply(names(units), function(metric) {
    do.call(rbind, lapply(c("baseline", "candidate"), function(role) {
      label <- if (role == "baseline") baseline_label else candidate_label
      row <- rows[rows$candidate == label, , drop = FALSE]
      if (nrow(row) != 1L) {
        stop("expected exactly one bundle row for label ", label, call. = FALSE)
      }
      tolerance_class <- units[[metric]]
      unit <- if (identical(tolerance_class, "bytes")) "MiB" else "ms"
      acceptance_long_row(metric, "bundle", NA_character_, role,
                          row[[metric]][[1L]], unit, tolerance_class,
                          all(rows$check == "equal"))
    }))
  })
  do.call(rbind, out)
}

acceptance_extract_startup <- function(rows, role) {
  metrics <- c("library_ms", "app_construct_ms", "server_listen_ms",
               "browser_load_ms", "load_to_data_ms", "browser_to_data_ms",
               "process_to_data_ms")
  do.call(rbind, lapply(metrics, function(metric) {
    acceptance_long_row(metric, "startup", NA_character_, role,
                        rows[[metric]], "ms", "latency", TRUE)
  }))
}

acceptance_extract_renderer <- function(rows, role) {
  metrics <- c(initializeMs = "latency", categoricalBuildMs = "latency",
               uploadMs = "latency", firstFrameMs = "latency",
               panZoomMedianMs = "latency", panZoomP95Ms = "latency",
               rgbBuildMs = "latency", rgbUploadMs = "latency",
               rgbMedianMs = "latency", rgbP95Ms = "latency",
               categoricalImageBytes = "bytes", imageBytes = "bytes")
  correctness <- !acceptance_as_bool(rows$contextLost) &
    acceptance_as_bool(rows$gpuError == 0)
  out <- list()
  for (i in seq_len(nrow(rows))) {
    for (metric in names(metrics)) {
      tolerance_class <- metrics[[metric]]
      unit <- if (identical(tolerance_class, "bytes")) "bytes" else "ms"
      out[[length(out) + 1L]] <- acceptance_long_row(
        metric, rows$backend[[i]], NA_character_, role, rows[[metric]][[i]],
        unit, tolerance_class, correctness[[i]])
    }
  }
  do.call(rbind, out)
}

acceptance_extract_pages <- function(rows, baseline_label, candidate_label) {
  correctness <- all(rows$status == "ok") &
    all(acceptance_as_bool(rows$correctness_pass))
  out <- list()
  for (i in seq_len(nrow(rows))) {
    role <- if (rows$candidate[[i]] == baseline_label) {
      "baseline"
    } else if (rows$candidate[[i]] == candidate_label) {
      "candidate"
    } else {
      stop("unknown pages candidate label: ", rows$candidate[[i]], call. = FALSE)
    }
    scope <- rows$page[[i]]
    visit <- rows$visit[[i]]
    out[[length(out) + 1L]] <- acceptance_long_row(
      "primary_ready_ms", scope, visit, role, rows$elapsed_ms[[i]], "ms",
      "latency", correctness, budget = rows$budget_ms[[i]])
    out[[length(out) + 1L]] <- acceptance_long_row(
      "r_peak_rss_mib", scope, visit, role, rows$r_peak_rss_kib[[i]] / 1024,
      "MiB", "memory", correctness)
    out[[length(out) + 1L]] <- acceptance_long_row(
      "chrome_peak_rss_mib", scope, visit, role,
      rows$chrome_peak_rss_kib[[i]] / 1024, "MiB", "memory", correctness)
    out[[length(out) + 1L]] <- acceptance_long_row(
      "js_heap_used_mib", scope, visit, role,
      rows$js_heap_used_bytes[[i]] / 1048576, "MiB", "memory", correctness)
    out[[length(out) + 1L]] <- acceptance_long_row(
      "websocket_received_bytes", scope, visit, role,
      rows$websocket_received_payload_bytes[[i]], "bytes", "bytes", correctness)
  }
  do.call(rbind, out)
}

acceptance_marker_pass <- function(path, pattern) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  any(grepl(pattern, readLines(path, warn = FALSE), fixed = TRUE))
}

acceptance_sha256 <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  if (exists("sha256sum", where = asNamespace("tools"), inherits = FALSE)) {
    return(unname(as.character(tools::sha256sum(path))))
  }
  tool <- Sys.which("sha256sum")
  if (nzchar(tool)) {
    out <- suppressWarnings(system2(tool, shQuote(path), stdout = TRUE, stderr = FALSE))
    if (length(out)) {
      return(sub(" .*$", "", out[[1L]]))
    }
  }
  tool <- Sys.which("shasum")
  if (nzchar(tool)) {
    out <- suppressWarnings(system2(tool, c("-a", "256", shQuote(path)),
                                    stdout = TRUE, stderr = FALSE))
    if (length(out)) {
      return(sub(" .*$", "", out[[1L]]))
    }
  }
  NA_character_
}

acceptance_resolve_path <- function(path, base) {
  if (grepl("^(/|\\\\|[A-Za-z]:[/\\\\])", path)) {
    return(path)
  }
  file.path(base, path)
}

acceptance_load_run_dir <- function(path) {
  config_path <- file.path(path, "run-config.tsv")
  files_path <- file.path(path, "files.tsv")
  if (!file.exists(config_path) || !file.exists(files_path)) {
    stop("run-dir must contain run-config.tsv and files.tsv", call. = FALSE)
  }
  config <- utils::read.delim(config_path, stringsAsFactors = FALSE,
                              check.names = FALSE)
  required <- c("branch", "layer", "platform", "level", "profile", "mode",
                "rounds", "candidate_label", "candidate_sha", "baseline_label",
                "baseline_sha", "baseline_column", "candidate_column",
                "fixture_path", "fixture_sha256", "git_dirty", "created_utc")
  missing <- setdiff(required, names(config))
  if (length(missing)) {
    stop("run-config.tsv is missing fields: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  if (nrow(config) != 1L) {
    stop("run-config.tsv must contain exactly one data row", call. = FALSE)
  }
  config <- as.list(config[1L, , drop = FALSE])
  config <- lapply(config, function(value) as.character(value[[1L]]))
  files <- utils::read.delim(files_path, stringsAsFactors = FALSE,
                             check.names = FALSE)
  file_fields <- c("role", "file_role", "relative_path", "sha256")
  missing_file_fields <- setdiff(file_fields, names(files))
  if (length(missing_file_fields)) {
    stop("files.tsv is missing fields: ",
         paste(missing_file_fields, collapse = ", "), call. = FALSE)
  }
  if (!nrow(files)) {
    stop("files.tsv must contain at least one data row", call. = FALSE)
  }
  files$path <- vapply(files$relative_path, acceptance_resolve_path,
                       character(1), base = path)
  list(path = path, config = config, files = files)
}

acceptance_observed_rounds <- function(path, file_role) {
  field <- switch(
    file_role,
    crb = "rounds",
    hot_paths = "repeats",
    bundle = "repeats",
    renderer_baseline = "repeats",
    renderer_candidate = "repeats",
    NULL
  )
  if (is.null(field)) {
    return(NULL)
  }
  rows <- acceptance_read_table(path)
  if (!field %in% names(rows) || !nrow(rows)) {
    return(NA_integer_)
  }
  values <- unique(suppressWarnings(as.numeric(rows[[field]])))
  if (
    length(values) != 1L ||
      !is.finite(values) ||
      values < 1 ||
      values != floor(values)
  ) {
    return(NA_integer_)
  }
  as.integer(values)
}

acceptance_sample_key <- function(layer, level, config) {
  base_key <- if (identical(layer, "pages")) "pages" else layer
  key <- if (identical(level, "L2")) {
    paste0(base_key, "_l2")
  } else {
    paste0(base_key, "_l1")
  }
  if (!key %in% names(config$sample_min)) {
    key <- base_key
  }
  key
}

acceptance_validate_run <- function(run, branch, platform,
                                    config = ACCEPTANCE_CONFIG) {
  problems <- character()
  entry <- config$branches[[branch]]
  if (is.null(entry) || !isTRUE(entry$machine)) {
    return(list(problems = paste0("unknown or non-machine branch: ", branch),
                correctness_fail = character()))
  }
  if (!identical(run$config$branch, branch)) {
    problems <- c(problems, paste0("branch mismatch: ", run$config$branch))
  }
  if (!identical(run$config$platform, platform)) {
    problems <- c(problems, paste0("platform mismatch: ", run$config$platform))
  }
  if (!identical(run$config$layer, entry$layer)) {
    problems <- c(problems, paste0("layer mismatch: ", run$config$layer))
  }
  if (!identical(run$config$candidate_sha, entry$candidate)) {
    problems <- c(problems, paste0("candidate_sha mismatch: ",
                                   run$config$candidate_sha))
  }
  if (!identical(run$config$baseline_sha, entry$parent)) {
    problems <- c(problems, paste0("baseline_sha mismatch: ",
                                   run$config$baseline_sha))
  }
  level <- run$config$level
  if (!level %in% c("L1", "L2")) {
    problems <- c(problems, paste0("invalid level: ", level))
    level <- "L1"
  }
  required <- config$sample_min[[acceptance_sample_key(entry$layer, level,
                                                        config)]]
  rounds <- suppressWarnings(as.integer(run$config$rounds))
  if (is.na(rounds) || rounds < required) {
    problems <- c(problems, paste0("rounds below minimum: ",
                                   run$config$rounds, " < ", required))
  }
  if (identical(level, "L2")) {
    if (!identical(run$config$profile, "evidence")) {
      problems <- c(problems, "L2 requires the evidence profile")
    }
    if (!identical(toupper(run$config$git_dirty), "FALSE")) {
      problems <- c(problems, "L2 requires a clean worktree")
    }
  }
  missing_roles <- setdiff(config$layers[[entry$layer]]$file_roles,
                           run$files$file_role)
  if (length(missing_roles)) {
    problems <- c(problems, paste0("missing file roles: ",
                                   paste(missing_roles, collapse = ", ")))
  }
  for (i in seq_len(nrow(run$files))) {
    if (!file.exists(run$files$path[[i]])) {
      problems <- c(problems, paste0("missing file: ",
                                     run$files$relative_path[[i]]))
      next
    }
    observed_rounds <- acceptance_observed_rounds(
      run$files$path[[i]],
      run$files$file_role[[i]]
    )
    if (length(observed_rounds)) {
      if (is.na(observed_rounds)) {
        problems <- c(
          problems,
          paste0(
            "missing or invalid observed rounds: ",
            run$files$relative_path[[i]]
          )
        )
      } else if (is.na(rounds) || observed_rounds != rounds) {
        problems <- c(
          problems,
          paste0(
            "observed rounds mismatch for ",
            run$files$relative_path[[i]],
            ": ", observed_rounds, " != ", run$config$rounds
          )
        )
      }
    }
    declared <- run$files$sha256[[i]]
    if (is.na(declared) || !nzchar(declared)) {
      if (identical(level, "L2")) {
        problems <- c(problems, paste0("missing sha256 for ",
                                       run$files$relative_path[[i]]))
      }
      next
    }
    observed <- acceptance_sha256(run$files$path[[i]])
    if (is.na(observed) ||
        !identical(tolower(observed), tolower(declared))) {
      problems <- c(problems, paste0("sha256 mismatch: ",
                                     run$files$relative_path[[i]]))
    }
  }
  fixture_path <- run$config$fixture_path
  if (!is.na(fixture_path) && nzchar(fixture_path)) {
    resolved <- acceptance_resolve_path(fixture_path, run$path)
    fixture_sha <- run$config$fixture_sha256
    if (!file.exists(resolved)) {
      problems <- c(problems, paste0("missing fixture: ", fixture_path))
    } else if (!is.na(fixture_sha) && nzchar(fixture_sha) &&
               !identical(tolower(acceptance_sha256(resolved)),
                          tolower(fixture_sha))) {
      problems <- c(problems, paste0("fixture sha256 mismatch: ",
                                     fixture_path))
    }
  }
  correctness_files <- run$files$path[run$files$file_role == "correctness"]
  correctness_fail <- character()
  marker <- config$layers[[entry$layer]]$correctness_marker
  if (is.null(marker)) {
    marker <- "PASS"
  }
  for (path in correctness_files) {
    if (file.exists(path) && !acceptance_marker_pass(path, marker)) {
      correctness_fail <- c(correctness_fail, basename(path))
    }
  }
  list(problems = problems, correctness_fail = correctness_fail)
}

acceptance_pair_medians <- function(rows) {
  rows$scope[is.na(rows$scope)] <- ""
  rows$visit[is.na(rows$visit)] <- ""
  keys <- unique(rows[c("metric_id", "scope", "visit")])
  parts <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    key <- keys[i, , drop = FALSE]
    part <- rows[
      rows$metric_id == key$metric_id & rows$scope == key$scope &
        rows$visit == key$visit, , drop = FALSE]
    baseline <- part$value[part$role == "baseline"]
    candidate <- part$value[part$role == "candidate"]
    parts[[i]] <- data.frame(
      metric_id = key$metric_id,
      scope = key$scope,
      visit = key$visit,
      unit = part$unit[[1L]],
      tolerance_class = part$tolerance_class[[1L]],
      baseline_n = sum(is.finite(baseline)),
      candidate_n = sum(is.finite(candidate)),
      baseline_median = if (any(is.finite(baseline))) {
        stats::median(baseline, na.rm = TRUE)
      } else {
        NA_real_
      },
      candidate_median = if (any(is.finite(candidate))) {
        stats::median(candidate, na.rm = TRUE)
      } else {
        NA_real_
      },
      budget = if (any(is.finite(part$budget))) part$budget[[1L]] else NA_real_,
      correctness = isTRUE(all(part$correctness)),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, parts)
}

acceptance_extract_run_files <- function(run) {
  config <- run$config
  out <- list()
  for (i in seq_len(nrow(run$files))) {
    file_role <- run$files$file_role[[i]]
    path <- run$files$path[[i]]
    rows <- acceptance_read_table(path)
    long <- if (identical(file_role, "crb")) {
      acceptance_extract_crb(rows, config$baseline_label, config$candidate_label,
                             correctness = TRUE)
    } else if (identical(file_role, "hot_paths")) {
      acceptance_extract_hot_paths(rows, config$baseline_column,
                                   config$candidate_column)
    } else if (identical(file_role, "bundle")) {
      acceptance_extract_bundle(rows, config$baseline_label,
                                config$candidate_label)
    } else if (identical(file_role, "startup_baseline")) {
      acceptance_extract_startup(rows, "baseline")
    } else if (identical(file_role, "startup_candidate")) {
      acceptance_extract_startup(rows, "candidate")
    } else if (identical(file_role, "renderer_baseline")) {
      acceptance_extract_renderer(rows, "baseline")
    } else if (identical(file_role, "renderer_candidate")) {
      acceptance_extract_renderer(rows, "candidate")
    } else if (identical(file_role, "pages")) {
      acceptance_extract_pages(rows, config$baseline_label,
                               config$candidate_label)
    } else if (identical(file_role, "correctness")) {
      next
    } else {
      stop("unknown file role: ", file_role, call. = FALSE)
    }
    out[[length(out) + 1L]] <- long
  }
  if (!length(out)) {
    return(data.frame())
  }
  do.call(rbind, out)
}

acceptance_verdicts <- function(pairs, branch, level,
                                config = ACCEPTANCE_CONFIG) {
  entry <- config$branches[[branch]]
  improvement <- vapply(seq_len(nrow(pairs)), function(i) {
    pair <- pairs[i, , drop = FALSE]
    if (!pair$metric_id %in% entry$headline) {
      return(NA_real_)
    }
    acceptance_headline_improvement(pair$baseline_median[[1L]],
                                    pair$candidate_median[[1L]])
  }, numeric(1))
  improvement <- improvement[is.finite(improvement)]
  verdicts <- lapply(seq_len(nrow(pairs)), function(i) {
    pair <- pairs[i, , drop = FALSE]
    base <- pair$baseline_median[[1L]]
    cand <- pair$candidate_median[[1L]]
    if (!isTRUE(pair$correctness)) {
      verdict <- "FAIL"
      rule <- "correctness"
      limit <- NA_real_
    } else if (is.finite(pair$budget[[1L]])) {
      budget <- acceptance_budget_verdict(base, cand, pair$budget[[1L]])
      verdict <- budget$verdict
      rule <- "budget"
      limit <- pair$budget[[1L]]
    } else {
      tolerance <- config$tolerances[[pair$tolerance_class[[1L]]]]
      metric <- acceptance_metric_verdict(base, cand, tolerance, level)
      verdict <- metric$verdict
      rule <- "tolerance"
      limit <- metric$limit
    }
    data.frame(
      metric_id = pair$metric_id,
      scope = pair$scope,
      visit = pair$visit,
      unit = pair$unit,
      baseline_median = base,
      candidate_median = cand,
      delta = cand - base,
      delta_pct = if (is.finite(base) && base != 0) (cand - base) / base else NA_real_,
      limit = limit,
      rule = rule,
      verdict = verdict,
      stringsAsFactors = FALSE
    )
  })
  verdicts <- do.call(rbind, verdicts)
  headline <- acceptance_headline_verdict(
    improvement, justified = !is.null(entry$justification))
  verdicts <- rbind(verdicts, data.frame(
    metric_id = paste0("headline:", paste(entry$headline, collapse = ",")),
    scope = "", visit = "", unit = "",
    baseline_median = NA_real_, candidate_median = NA_real_,
    delta = NA_real_, delta_pct = NA_real_, limit = NA_real_,
    rule = "headline", verdict = headline, stringsAsFactors = FALSE))
  verdicts
}

acceptance_apply_waivers <- function(verdicts, branch,
                                     config = ACCEPTANCE_CONFIG) {
  if (!length(config$waivers)) {
    return(verdicts)
  }
  for (waiver in config$waivers) {
    if (!identical(waiver$branch, branch)) next
    match <- verdicts$metric_id == waiver$metric_id &
      verdicts$scope == waiver$scope
    if (!is.null(waiver$visit)) {
      match <- match & verdicts$visit == waiver$visit
    }
    match <- match & verdicts$verdict == "FAIL"
    if ("rule" %in% names(verdicts)) {
      match <- match & verdicts$rule != "correctness"
    }
    verdicts$verdict[match] <- "WAIVED"
  }
  verdicts
}

acceptance_sample_problems <- function(pairs, layer, level, config) {
  if (!layer %in% c("pages", "startup")) {
    return(character())
  }
  required <- config$sample_min[[acceptance_sample_key(layer, level, config)]]
  bad <- pairs$baseline_n < required | pairs$candidate_n < required
  if (!any(bad)) {
    return(character())
  }
  labels <- unique(paste0(pairs$metric_id[bad], "[", pairs$scope[bad], "/",
                          pairs$visit[bad], "]"))
  paste0("insufficient paired samples (<", required, "): ",
         paste(labels, collapse = ", "))
}

acceptance_judge <- function(run_dir, branch, platform,
                            config = ACCEPTANCE_CONFIG) {
  run <- acceptance_load_run_dir(run_dir)
  validation <- acceptance_validate_run(run, branch, platform, config)
  if (length(validation$problems)) {
    return(list(exit_code = 2L, problems = validation$problems,
                verdicts = data.frame(), pairs = data.frame()))
  }
  rows <- acceptance_extract_run_files(run)
  pairs <- acceptance_pair_medians(rows)
  sample_problems <- acceptance_sample_problems(pairs, run$config$layer,
                                                run$config$level, config)
  if (length(sample_problems)) {
    return(list(exit_code = 2L, problems = sample_problems,
                verdicts = data.frame(), pairs = pairs))
  }
  verdicts <- acceptance_verdicts(pairs, branch, run$config$level, config)
  if (length(validation$correctness_fail)) {
    verdicts <- rbind(verdicts, data.frame(
      metric_id = paste0("correctness:", validation$correctness_fail),
      scope = "", visit = "", unit = "",
      baseline_median = NA_real_, candidate_median = NA_real_,
      delta = NA_real_, delta_pct = NA_real_, limit = NA_real_,
      rule = "correctness", verdict = "FAIL", stringsAsFactors = FALSE))
  }
  verdicts <- acceptance_apply_waivers(verdicts, branch, config)
  list(exit_code = acceptance_exit_code(verdicts), problems = character(),
       correctness_fail = validation$correctness_fail,
       verdicts = verdicts, pairs = pairs, rows = rows, run = run)
}

acceptance_markdown <- function(judged, branch, platform, context = NULL) {
  lines <- c(
    paste0("# Acceptance report: ", branch, " (", platform, ")"),
    "",
    paste0("- exit_code: ", judged$exit_code),
    paste0("- problems: ", if (length(judged$problems)) {
      paste(judged$problems, collapse = "; ")
    } else {
      "none"
    })
  )
  if (!is.null(context)) {
    lines <- c(
      lines,
      paste0("- config_version: ", context$version),
      paste0("- config_sha256: ", context$config_sha256)
    )
  }
  lines <- c(lines, "")
  if (length(judged$problems)) {
    return(c(lines, "Evidence is invalid; no verdicts produced."))
  }
  verdicts <- judged$verdicts
  waived <- verdicts[verdicts$verdict == "WAIVED", , drop = FALSE]
  if (nrow(waived)) {
    lines <- c(lines, "## Waivers (PASS (WAIVED))", "")
    for (i in seq_len(nrow(waived))) {
      lines <- c(lines, paste0("- ", waived$metric_id[[i]], " [",
                               waived$scope[[i]], "/", waived$visit[[i]], "]"))
    }
    lines <- c(lines, "")
  }
  table <- c(
    "| metric | scope | visit | baseline | candidate | delta% | rule | verdict |",
    "|---|---|---|---:|---:|---:|---|---|"
  )
  for (i in seq_len(nrow(verdicts))) {
    row <- verdicts[i, , drop = FALSE]
    display <- if (identical(row$verdict[[1L]], "WAIVED")) {
      "PASS (WAIVED)"
    } else {
      row$verdict[[1L]]
    }
    table <- c(table, sprintf(
      "| %s | %s | %s | %.2f | %.2f | %s | %s | %s |",
      row$metric_id, row$scope, row$visit,
      row$baseline_median, row$candidate_median,
      if (is.finite(row$delta_pct)) sprintf("%+.2f%%", 100 * row$delta_pct) else "--",
      row$rule, display
    ))
  }
  c(lines, table)
}

acceptance_json_escape <- function(value) {
  value <- gsub("\\\\", "\\\\\\\\", value)
  value <- gsub('"', '\\\\"', value)
  value
}

acceptance_json <- function(judged, branch, platform) {
  verdicts <- judged$verdicts
  rows <- character(0)
  if (nrow(verdicts)) {
    for (i in seq_len(nrow(verdicts))) {
      row <- verdicts[i, , drop = FALSE]
      rows <- c(rows, sprintf(
        '{"metric_id":"%s","scope":"%s","visit":"%s","verdict":"%s"}',
        acceptance_json_escape(row$metric_id), acceptance_json_escape(row$scope),
        acceptance_json_escape(row$visit), row$verdict
      ))
    }
  }
  sprintf(
    '{"branch":"%s","platform":"%s","exit_code":%d,"problems":%d,"verdicts":[%s]}',
    acceptance_json_escape(branch), platform, judged$exit_code,
    length(judged$problems), paste(rows, collapse = ",")
  )
}
