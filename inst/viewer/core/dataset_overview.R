# Compact, expression-value-free summaries shared by bundle creation and Viewer.
viewerOverviewText <- function(value) {
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))) return(NULL)
  trimws(as.character(value))
}

viewerDatasetOverview <- function(object) {
  safely <- function(expr, fallback = NULL) tryCatch(expr, error = function(e) fallback)
  experiment <- safely(object$getExperiment(), list())
  metadata <- safely(object$getMetaData(), data.frame())
  groups <- intersect(safely(object$getGroups(), character()), names(metadata))
  # Only explicit sample metadata or the canonical sample column is used.
  sample_column <- viewerOverviewText(experiment$sample_column)
  if (is.null(sample_column) && "sample" %in% names(metadata)) sample_column <- "sample"
  if (!is.null(sample_column) && !sample_column %in% names(metadata)) sample_column <- NULL
  summarize <- function(column) {
    values <- as.character(metadata[[column]])
    values[is.na(values) | !nzchar(trimws(values))] <- NA_character_
    counts <- sort(table(values, useNA = "no"), decreasing = TRUE)
    if (!length(counts)) return(NULL)
    list(column = column, levels = length(counts), total = sum(counts),
         missing = sum(is.na(values)),
         counts = stats::setNames(as.numeric(counts), names(counts)),
         remaining = 0)
  }
  sample <- if (!is.null(sample_column)) summarize(sample_column) else NULL
  annotations <- lapply(setdiff(groups, sample_column), summarize)
  names(annotations) <- setdiff(groups, sample_column)
  annotations <- Filter(Negate(is.null), annotations)
  genes <- safely(length(object$getGeneNames()))
  if (is.null(genes) || genes == 0L) genes <- NULL
  spatial <- safely(as.character(object$availableSpatial()), character())
  projections <- safely(as.character(object$availableProjections()), character())
  list(version = 2L, description = viewerOverviewText(experiment$description),
       source = viewerOverviewText(experiment$source),
       experiment = viewerOverviewText(experiment$experiment_name),
       genes = genes, samples = if (!is.null(sample)) sample$levels else NULL,
       spatial = spatial, projections = projections, sample = sample,
       annotations = annotations)
}

viewerOverviewTruncated <- function(overview) {
  summaries <- c(list(overview$sample), overview$annotations)
  any(vapply(summaries, function(summary) {
    !is.null(summary) && (isTRUE(summary$remaining > 0) ||
      isTRUE(length(summary$counts) < summary$levels))
  }, logical(1)))
}

viewerSavedOverview <- function(entries, path) {
  entry <- entries[[path]]
  if (!is.list(entry)) return(NULL)
  stamp <- file.info(path)
  if (!nrow(stamp) || is.na(stamp$size) ||
      !identical(as.numeric(stamp$size), entry$size) ||
      !identical(as.numeric(stamp$mtime), entry$mtime)) return(NULL)
  entry$overview
}
