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
  "spatialPlotRotation",
  "rotateSpatialCoordinates",
  "viewerOutputTab",
  "viewerExpressionCells",
  "viewerBpcellsValues",
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
  ".runtimeAbsoluteBackendPath",
  ".validateRuntimeBackendEntry",
  ".configuredRuntimeBackendPlan",
  ".runtimeBackendCacheIdentity",
  ".attachSpatialMoleculeBackend",
  ".validateThinCrbShape",
  ".hydrateThinCrbFields",
  ".hydrateThinCrb",
  ".runtimeCerebroOptions",
  ".normalizeRuntimeOverride",
  ".fallbackRuntimeBackendPlan",
  ".runtimeBackendPlan",
  ".runtimeBackendRecoveryAdvice",
  ".attachExternalExpression",
  "extra_material_table_filter",
  "viewerHasTcrRepertoire",
  "filterSelectionByHiddenGroups",
  "selectedCellMask",
  "trajectorySelectionValid"
)

.viewerSourceAssignment <- function(expression) {
  if (
    is.call(expression) &&
      identical(expression[[1L]], as.name("<-")) &&
      is.symbol(expression[[2L]]) &&
      is.call(expression[[3L]]) &&
      identical(expression[[3L]][[1L]], as.name("function"))
  ) {
    return(as.character(expression[[2L]]))
  }
  NA_character_
}

viewerSource <- function(path, envir = parent.frame()) {
  path <- normalizePath(path, mustWork = TRUE)
  info <- file.info(path)
  fingerprint <- c(size = info$size, mtime = as.numeric(info$mtime))
  cached <- .viewer_source_cache[[path]]
  if (is.null(cached) || !identical(cached$fingerprint, fingerprint)) {
    expressions <- parse(file = path, keep.source = FALSE)
    shared_names <- if (identical(basename(path), "utility_functions.R")) {
      vapply(expressions, .viewerSourceAssignment, character(1))
    } else {
      rep(NA_character_, length(expressions))
    }
    shared <- !is.na(shared_names) &
      shared_names %in% .viewer_process_function_names
    shared_environment <- new.env(parent = environment(viewerSource))
    if (any(shared)) {
      invisible(eval(expressions[shared], envir = shared_environment))
    }
    cached <- list(
      fingerprint = fingerprint,
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
