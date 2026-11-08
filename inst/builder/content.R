##----------------------------------------------------------------------------##
## Assemble stable optional-content facts for one DatasetProfile.
##
## These records describe what the source contains. They deliberately do not
## choose a winning source, a final disposition, or Viewer page visibility;
## those decisions belong to the frozen BuildPlan.
##----------------------------------------------------------------------------##

.builder_profile_content_ids <- function() {
  c(
    "marker_genes",
    "most_expressed_genes",
    "mean_expression",
    "enriched_pathways",
    "trajectory",
    "gene_lists",
    "extra_material",
    "immune_repertoire",
    "hla",
    "spatial",
    "trekker"
  )
}

.builder_profile_content_text_vector <- function(value) {
  is.character(value) && !anyNA(value)
}

.builder_profile_validate_content_fact <- function(fact, id) {
  required <- c(
    "detected",
    "valid",
    "normalized",
    "diagnostics",
    "requirements",
    "page_candidates"
  )
  if (!is.list(fact) || !all(required %in% names(fact))) {
    .builder_profile_abort(
      "invalid_content_fact",
      paste("Optional content fact", id, "is missing required fields.")
    )
  }
  for (field in c("detected", "valid")) {
    value <- fact[[field]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      .builder_profile_abort(
        "invalid_content_fact",
        paste("Optional content fact", id, "has an invalid", field, "flag.")
      )
    }
  }
  for (field in c("diagnostics", "requirements", "page_candidates")) {
    if (!.builder_profile_content_text_vector(fact[[field]])) {
      .builder_profile_abort(
        "invalid_content_fact",
        paste("Optional content fact", id, "has invalid", field, "values.")
      )
    }
  }
  builder_viewer_validate_pages(fact$page_candidates)
  invisible(fact)
}

builder_profile_content_context <- function(
  source,
  cells,
  features,
  metadata,
  assays,
  default_assay,
  groups,
  reductions
) {
  list(
    source = source,
    cells = cells,
    features = features,
    metadata = metadata,
    assays = assays,
    default_assay = default_assay,
    groups = groups,
    reductions = reductions
  )
}

builder_profile_optional_content <- function(object, context) {
  if (exists("builder_worker_require_capability", mode = "function")) {
    builder_worker_require_capability("immune")
    builder_worker_require_capability("spatial")
  }
  parts <- list(
    builder_profile_table_content(object, context),
    builder_profile_immune_content(object, context),
    builder_profile_spatial_content(object, context)
  )
  facts <- unlist(parts, recursive = FALSE, use.names = TRUE)
  expected <- .builder_profile_content_ids()
  if (!identical(names(facts), expected)) {
    .builder_profile_abort(
      "invalid_content_profile",
      "Optional content collectors returned an unexpected capability set."
    )
  }
  for (id in expected) {
    .builder_profile_validate_content_fact(facts[[id]], id)
  }
  structure(facts, class = c("builder_content_profile", "list"))
}
