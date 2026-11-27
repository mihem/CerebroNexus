## Parsed Viewer code is immutable in installed/generated apps. Cache only the
## parsed expressions; every session still evaluates them in its own scope.
.viewer_source_cache <- new.env(parent = emptyenv())

## These helpers have no session-reactive state. Evaluate them once in a
## process-owned environment, then bind the same immutable function objects
## into each session. The remaining expressions are still evaluated locally.
.viewer_process_function_names <- c(
  "viewerUploadsEnabled",
  "viewerUploadPath",
  "viewerInitialPageDecision",
  "viewerDatasetName",
  "viewerDatasetPath",
  "viewerScatterDefaults",
  "viewerPublishChanged",
  "spatialImagePreset",
  "spatialImageSettingPreset",
  "spatialEmbeddedImagePreset",
  "spatialPlotRotation",
  "rotateSpatialCoordinates",
  "viewerOutputTab",
  "viewerExpressionCells",
  "viewerExpressionCellCount",
  "viewerH5ExpressionRows",
  "viewerH5ExpressionMean",
  "viewerExpressionRow",
  "viewerExpressionValues",
  "findColumnsInteger",
  "findColumnsMatching",
  "replaceInfiniteValues",
  "neutralizeSpreadsheetFormulas",
  "prepareEmptyTable",
  "cerebroGroupFilterMask",
  "randomlySubsetCells",
  "getXYranges",
  "match_dataset_id_by_url",
  "is_cerebro_dataset",
  "read_cerebro_file",
  ".cloneCachedCrb",
  ".runtimeBackendPlanError",
  ".validateRuntimeBackendEntry",
  ".configuredRuntimeBackendPlan",
  ".runtimeBackendCacheIdentity",
  ".crbBackendDescriptor",
  ".attachSpatialMoleculeBackend",
  ".validateThinCrbShape",
  ".runtimeH5Names",
  ".runtimeH5CellCount",
  ".checkThinCrbCells",
  ".thinCrbCellNames",
  ".hydrateThinCrbFields",
  ".deferThinCrbHydration",
  ".runtimeRepertoireIndexed",
  ".restoreRuntimeRepertoireCells",
  ".fallbackRuntimeBackendPlan",
  ".runtimeBackendRecoveryAdvice",
  ".attachExternalExpression",
  ".runtimeH5ChecksumRecord",
  ".verifyRuntimeH5Checksum",
  ".h5ExpressionRows",
  "extra_material_table_filter",
  "viewerHasTcrRepertoire",
  "filterSelectionByHiddenGroups",
  "viewerSelectionRows",
  "viewerRowMask",
  "selectedCellMask",
  "trajectorySelectionValid",
  "compactViolinData",
  "cerebroViolinData",
  "cerebroBarChartData",
  "cerebroSankeyData",
  "morans_i",
  "spatialRoiPivots",
  "rotateSpatialCoordinatesByRoi",
  "compute_group_hulls"
)

.viewerSourceAssignment <- function(expression) {
  if (
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      is.symbol(expression[[2L]]) &&
      is.call(expression[[3L]]) &&
      (identical(expression[[3L]][[1L]], as.name("function")) ||
        identical(expression[[3L]][[1L]], as.name("c")))
  ) {
    return(as.character(expression[[2L]]))
  }
  NA_character_
}

## The installed package's byte code for a Viewer file's helper functions
## (written by R CMD INSTALL, see R/viewer_bytecode.R), or NULL when there is
## none for this exact file and R version. Bundles have none.
.viewerBytecode <- function(path) {
  root <- tryCatch(Cerebro.options[["cerebro_root"]], error = function(e) NULL)
  if (!is.character(root) || length(root) != 1L || !nzchar(root)) {
    return(NULL)
  }
  viewer <- normalizePath(
    file.path(root, "viewer"),
    winslash = "/",
    mustWork = FALSE
  )
  source_path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!startsWith(source_path, paste0(viewer, "/"))) {
    return(NULL)
  }
  file <- file.path(
    root,
    "viewer-bytecode",
    paste0(substring(source_path, nchar(viewer) + 2L), ".rds")
  )
  if (!file.exists(file)) {
    return(NULL)
  }
  bytecode <- tryCatch(readRDS(file), error = function(e) NULL)
  if (
    !is.list(bytecode) ||
      !identical(bytecode$r_version, R.version.string) ||
      !identical(bytecode$md5, unname(tools::md5sum(path)))
  ) {
    return(NULL)
  }
  bytecode
}

## The cache is keyed by the path as given: normalizePath() costs 1-3 ms per
## call on Windows. For immutable installed package files (under .../CerebroNexus/viewer/
## when cerebro.dev_reload is not enabled), a cached entry is reused without
## repeating file.info(). Editable/temp files still stat on every call.
viewerSource <- function(path, envir = parent.frame()) {
  cached <- .viewer_source_cache[[path]]
  check_stat <- is.null(cached) ||
    !isTRUE(cached$immutable) ||
    isTRUE(getOption("cerebro.dev_reload", FALSE))
  fingerprint <- NULL
  if (check_stat) {
    info <- file.info(path, extra_cols = FALSE)
    fingerprint <- c(size = info$size, mtime = as.numeric(info$mtime))
  }
  if (
    is.null(cached) ||
      (check_stat && !identical(cached$fingerprint, fingerprint))
  ) {
    expressions <- parse(file = path, keep.source = FALSE)
    shared_names <- if (
      basename(path) %in%
        c(
          "utility_functions.R",
          "plotting_functions.R",
          "func_spatial_helpers.R"
        )
    ) {
      vapply(expressions, .viewerSourceAssignment, character(1))
    } else {
      rep(NA_character_, length(expressions))
    }
    ## Helpers then run compiled from their first call in this process.
    bytecode <- .viewerBytecode(path)
    if (!is.null(bytecode) && max(bytecode$index) <= length(expressions)) {
      expressions[bytecode$index] <- bytecode$code
    }
    shared <- !is.na(shared_names) &
      shared_names %in% .viewer_process_function_names
    shared_environment <- new.env(parent = environment(viewerSource))
    if (any(shared)) {
      invisible(eval(expressions[shared], envir = shared_environment))
    }
    norm_path <- gsub("\\", "/", path, fixed = TRUE)
    cached <- list(
      fingerprint = fingerprint,
      immutable = grepl("/CerebroNexus/viewer/", norm_path, fixed = TRUE),
      expressions = expressions[!shared],
      shared_names = shared_names[shared],
      shared_environment = shared_environment
    )
    .viewer_source_cache[[path]] <- cached
  }
  for (name in cached$shared_names) {
    assign(name, cached$shared_environment[[name]], envir = envir)
  }
  invisible(eval(cached$expressions, envir = envir))
}

## Runs `tasks` (functions named by the work they do) one per event-loop turn
## while `active()` is TRUE, so a click waits for one task at most. Each name is
## done once per process.
.viewer_idle_done <- new.env(parent = emptyenv())
## The packages each package's NAMESPACE imports (viewer_package_imports() in
## shiny_server.R), read once per process: parsing them on every quiet moment
## cost 0.13 s for the 26 packages the Viewer loads while idle.
.viewer_package_imports_cache <- new.env(parent = emptyenv())
viewerIdleRun <- function(tasks, active, delay = 0.01, done = NULL) {
  tasks <- tasks[!names(tasks) %in% ls(.viewer_idle_done, all.names = TRUE)]
  step <- function(i) {
    if (i > length(tasks) || !isTRUE(active())) {
      ## `done` when every task ran, not when the run was stopped
      if (i > length(tasks) && is.function(done)) {
        done()
      }
      return(invisible())
    }
    assign(names(tasks)[[i]], TRUE, envir = .viewer_idle_done)
    try(tasks[[i]](), silent = TRUE)
    later::later(function() step(i + 1L), delay)
  }
  later::later(function() step(1L), delay)
}
