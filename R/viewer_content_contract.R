## Viewer page identity and visibility rules shared by the package, Builder,
## and generated app bundles. Keep this file pure base R: the runtime copy is
## sourced from a self-contained bundle with no package namespace available.

.builder_manifest_abort <- function(code, message) {
  condition <- structure(
    list(message = message, call = NULL, code = code),
    class = c("builder_manifest_error", "error", "condition")
  )
  stop(condition)
}

.builder_manifest_text <- function(value, allow_empty = FALSE) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    (isTRUE(allow_empty) || nzchar(value))
}

.builder_page_table <- function(id, tab_name, label, icon) {
  data.frame(
    id = id,
    tab_name = tab_name,
    label = label,
    icon = icon,
    stringsAsFactors = FALSE
  )
}

builder_viewer_page_catalog <- function() {
  list(
    always = .builder_page_table(
      id = c(
        "data_info",
        "projection",
        "groups",
        "gene_expression",
        "gene_id_conversion",
        "color_management",
        "about"
      ),
      tab_name = c(
        "loadData",
        "coordinated_views",
        "groups",
        "geneExpression",
        "geneIdConversion",
        "color_management",
        "about"
      ),
      label = c(
        "Data info",
        "Linked views",
        "Groups",
        "Gene expression",
        "Gene ID conversion",
        "Color management",
        "About"
      ),
      icon = c(
        "info",
        "home",
        "layer-group",
        "signal",
        "barcode",
        "palette",
        "at"
      )
    ),
    conditional = .builder_page_table(
      id = c(
        "marker_genes",
        "most_expressed_genes",
        "enriched_pathways",
        "extra_material",
        "immune_repertoire",
        "trajectory",
        "spatial",
        "trekker",
        "hla_tcr_motifs"
      ),
      tab_name = c(
        "markerGenes",
        "mostExpressedGenes",
        "enrichedPathways",
        "extra_material",
        "immune_repertoire",
        "trajectory",
        "coordinated_views",
        "coordinated_views",
        "hla_tcr_motifs"
      ),
      label = c(
        "Marker genes",
        "Most expressed genes",
        "Enriched pathways",
        "Extra material",
        "Immune repertoire",
        "Trajectory",
        "Spatial",
        "Trekker",
        "HLA & TCR Motifs"
      ),
      icon = c(
        "list-alt",
        "bullhorn",
        "project-diagram",
        "gift",
        "dna",
        "route",
        "map-pin",
        "map-marked-alt",
        "project-diagram"
      )
    )
  )
}

builder_viewer_known_page_ids <- function() {
  catalog <- builder_viewer_page_catalog()
  c(catalog$always$id, catalog$conditional$id)
}

builder_viewer_validate_pages <- function(pages) {
  if (is.null(pages)) {
    return(character())
  }
  if (
    !is.character(pages) ||
      anyNA(pages) ||
      any(!nzchar(pages))
  ) {
    .builder_manifest_abort(
      "invalid_pages",
      "Viewer pages must be non-empty character identifiers."
    )
  }
  if (anyDuplicated(pages)) {
    .builder_manifest_abort(
      "duplicate_page",
      "Viewer page identifiers must be unique within an entry."
    )
  }
  unknown <- setdiff(pages, builder_viewer_known_page_ids())
  if (length(unknown)) {
    .builder_manifest_abort(
      "unknown_page",
      paste("Unknown Viewer page:", unknown[[1L]])
    )
  }
  pages
}

.builder_manifest_validate_action <- function(required_action, status) {
  if (is.null(required_action)) {
    if (identical(status, "attention")) {
      .builder_manifest_abort(
        "missing_required_action",
        "Attention entries must explain the required user action."
      )
    }
    return(NULL)
  }
  if (
    !is.list(required_action) ||
      !.builder_manifest_text(required_action[["type"]])
  ) {
    .builder_manifest_abort(
      "invalid_required_action",
      "Required actions must be typed records."
    )
  }
  if (!required_action[["type"]] %in% c("acknowledge", "choose", "provide")) {
    .builder_manifest_abort(
      "invalid_action_type",
      "Required action type is not supported."
    )
  }
  if (
    identical(required_action[["type"]], "acknowledge") &&
      !.builder_manifest_text(required_action[["token"]])
  ) {
    .builder_manifest_abort(
      "invalid_action_token",
      "Acknowledgement actions require a stable token."
    )
  }
  required_action
}

.builder_manifest_validate_values <- function(
  id,
  source,
  status,
  disposition,
  artifact_scope,
  summary,
  diagnostics,
  compatibility,
  pages,
  required_action,
  verifier
) {
  if (!.builder_manifest_text(id)) {
    .builder_manifest_abort(
      "invalid_id",
      "Manifest entry ids must be non-empty strings."
    )
  }
  if (
    !is.list(source) ||
      !.builder_manifest_text(source[["type"]]) ||
      !.builder_manifest_text(source[["location"]])
  ) {
    .builder_manifest_abort(
      "invalid_source",
      "Manifest sources require type and location strings."
    )
  }

  statuses <- c(
    "checking",
    "valid",
    "attention",
    "blocking",
    "not_applicable"
  )
  if (!.builder_manifest_text(status) || !status %in% statuses) {
    .builder_manifest_abort(
      "invalid_status",
      "Manifest status is not supported."
    )
  }

  dispositions <- c(
    "preserved",
    "generated",
    "converted",
    "attached",
    "filtered",
    "stored_only",
    "rejected"
  )
  disposition_is_na <- is.character(disposition) &&
    length(disposition) == 1L &&
    is.na(disposition)
  if (disposition_is_na) {
    if (!status %in% c("checking", "not_applicable")) {
      .builder_manifest_abort(
        "missing_disposition",
        "This manifest status requires a disposition."
      )
    }
  } else if (
    !.builder_manifest_text(disposition) ||
      !disposition %in% dispositions
  ) {
    .builder_manifest_abort(
      "invalid_disposition",
      "Manifest disposition is not supported."
    )
  }

  if (
    !.builder_manifest_text(artifact_scope) ||
      !artifact_scope %in% c("crb", "app", "both")
  ) {
    .builder_manifest_abort(
      "invalid_artifact_scope",
      "Manifest artifact scope is not supported."
    )
  }
  if (!.builder_manifest_text(summary, allow_empty = TRUE)) {
    .builder_manifest_abort(
      "invalid_summary",
      "Manifest summaries must be scalar character values."
    )
  }
  if (!is.list(diagnostics)) {
    .builder_manifest_abort(
      "invalid_diagnostics",
      "Manifest diagnostics must be a list."
    )
  }
  if (!is.list(compatibility)) {
    .builder_manifest_abort(
      "invalid_compatibility",
      "Manifest compatibility must be a list."
    )
  }
  pages <- builder_viewer_validate_pages(pages)
  required_action <- .builder_manifest_validate_action(required_action, status)
  if (!is.null(verifier) && !.builder_manifest_text(verifier)) {
    .builder_manifest_abort(
      "invalid_verifier",
      "Post-build verifier ids must be non-empty strings."
    )
  }

  list(
    id = id,
    source = source,
    status = status,
    disposition = disposition,
    artifact_scope = artifact_scope,
    summary = summary,
    diagnostics = diagnostics,
    compatibility = compatibility,
    pages = pages,
    required_action = required_action,
    verifier = verifier
  )
}

.builder_manifest_required_fields <- function() {
  c(
    "id",
    "source",
    "status",
    "disposition",
    "artifact_scope",
    "summary",
    "diagnostics",
    "compatibility",
    "pages",
    "required_action",
    "verifier"
  )
}

.builder_manifest_validate_entry <- function(entry) {
  if (!inherits(entry, "builder_manifest_entry") || !is.list(entry)) {
    .builder_manifest_abort(
      "invalid_entry",
      "Content manifests accept only typed manifest entries."
    )
  }
  required <- .builder_manifest_required_fields()
  if (!all(required %in% names(entry))) {
    .builder_manifest_abort(
      "invalid_entry",
      "A manifest entry is missing required fields."
    )
  }
  do.call(
    .builder_manifest_validate_values,
    unname(entry[required])
  )
  invisible(entry)
}

.builder_manifest_validate <- function(manifest) {
  if (!inherits(manifest, "builder_content_manifest") || !is.list(manifest)) {
    .builder_manifest_abort(
      "invalid_manifest",
      "Expected a typed content manifest."
    )
  }
  for (entry in unname(manifest)) {
    .builder_manifest_validate_entry(entry)
  }
  ids <- vapply(unname(manifest), function(entry) entry$id, character(1))
  if (anyDuplicated(ids) || !identical(names(manifest), ids)) {
    .builder_manifest_abort(
      "invalid_manifest",
      "Manifest names must match unique entry ids."
    )
  }
  invisible(manifest)
}

.builder_page_entry_opens <- function(entry, page_id) {
  is.list(entry) &&
    identical(entry$status, "valid") &&
    is.character(entry$disposition) &&
    length(entry$disposition) == 1L &&
    !is.na(entry$disposition) &&
    entry$disposition %in%
      c(
        "preserved",
        "generated",
        "converted",
        "attached"
      ) &&
    is.character(entry$artifact_scope) &&
    length(entry$artifact_scope) == 1L &&
    entry$artifact_scope %in% c("app", "both") &&
    is.character(entry$pages) &&
    page_id %in% entry$pages
}

builder_viewer_page_contract <- function(manifest) {
  .builder_manifest_validate(manifest)

  entries <- unname(manifest)

  catalog <- builder_viewer_page_catalog()
  visible <- catalog$conditional$id[vapply(
    catalog$conditional$id,
    function(page_id) {
      any(vapply(
        entries,
        .builder_page_entry_opens,
        logical(1),
        page_id = page_id
      ))
    },
    logical(1)
  )]

  list(
    always = catalog$always,
    conditional = catalog$conditional,
    visible_conditional = visible,
    hidden_conditional = setdiff(catalog$conditional$id, visible)
  )
}
