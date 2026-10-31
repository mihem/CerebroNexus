## Viewer page identity and visibility rules shared by the package, Builder,
## and generated app bundles. Keep this file pure base R: the runtime copy is
## sourced from a self-contained bundle with no package namespace available.

## Coordinate appearance has one typed contract for authoring and rendering.
viewerNamedSettings <- function(value) {
  is.list(value) &&
    !is.object(value) &&
    (!length(value) ||
      (!is.null(names(value)) &&
        !anyNA(names(value)) &&
        all(nzchar(names(value))) &&
        !anyDuplicated(names(value))))
}

viewerSpatialPointSetting <- function(value, roi = FALSE) {
  fields <- c(if (roi) "rotation_degrees", "point_opacity", "point_size")
  if (
    !viewerNamedSettings(value) ||
      !setequal(names(value), fields) ||
      !all(vapply(
        value,
        function(x) is.numeric(x) && length(x) == 1L && is.finite(x),
        logical(1)
      )) ||
      value$point_size < 1 ||
      value$point_size > 20 ||
      value$point_opacity < 0 ||
      value$point_opacity > 1
  ) {
    return(NULL)
  }
  lapply(value[fields], as.numeric)
}

## Extra-material labels are presentation metadata keyed by immutable table IDs.
## Discard unavailable labels and unrelated source-file metadata.
viewerExtraMaterialTableIndex <- function(value = NULL, table_names = NULL) {
  if (!is.list(value) || is.object(value) || is.null(names(value))) {
    return(list())
  }
  keys <- names(value)
  keep <- !is.na(keys) & nzchar(trimws(keys)) & !duplicated(keys)
  if (!is.null(table_names)) {
    keep <- keep & keys %in% table_names
  }
  result <- list()
  for (key in keys[keep]) {
    record <- value[[key]]
    if (!is.list(record) || is.object(record)) {
      next
    }
    labels <- list()
    for (field in c("workbook_name", "display_name")) {
      text <- record[[field]]
      if (
        is.character(text) &&
          length(text) == 1L &&
          !is.na(text) &&
          nzchar(trimws(text))
      ) {
        labels[[field]] <- trimws(text)
      }
    }
    if (length(labels)) {
      result[[key]] <- labels
    }
  }
  result
}

## Author defaults, independent of transient browsing selections. Keep the
## schema small and typed; arbitrary CSS and executable font URLs are excluded.
builderViewerDisplayDefaults <- function(value = NULL) {
  defaults <- list(
    group_labels = FALSE,
    draw_border = FALSE,
    keep_square = FALSE,
    label_size = 11,
    font_family = "system-ui",
    color_clip = 0.01,
    gene_palette = "cerebro_orange",
    gene_panel_mode = "shared",
    gene_panel_palette = "default",
    projection_group_labels = FALSE,
    projection_draw_border = FALSE,
    projection_keep_square = FALSE,
    gene_draw_border = FALSE,
    gene_keep_square = FALSE,
    gene_plotting_order = "Input order",
    gene_show_trend_line = TRUE,
    gene_trend_line_width = 2,
    spatial_group_labels = FALSE,
    spatial_draw_border = FALSE,
    spatial_keep_square = FALSE,
    spatial_region_outlines = FALSE,
    trajectory_group_labels = FALSE,
    trajectory_draw_border = TRUE,
    trajectory_keep_square = FALSE,
    trajectory_density_opacity = .6,
    trajectory_show_trend_line = TRUE,
    trajectory_trend_line_width = 2,
    ir_group_labels = FALSE,
    ir_draw_border = FALSE,
    ir_keep_square = FALSE,
    ir_scatter_point_size = 1,
    ir_scatter_opacity = .8,
    hla_legend_mode = "auto",
    hla_node_scale = 1
  )
  if (is.null(value)) {
    return(defaults)
  }
  if (
    !is.list(value) ||
      is.object(value) ||
      any(!names(value) %in% names(defaults)) ||
      (length(value) && (is.null(names(value)) || anyDuplicated(names(value))))
  ) {
    stop(
      "Viewer display settings must be a named settings list.",
      call. = FALSE
    )
  }
  for (key in names(value)) {
    defaults[[key]] <- value[[key]]
  }
  scalar <- function(x, type) {
    typeof(x) %in% type && !is.object(x) && length(x) == 1L && !is.na(x)
  }
  flags <- grep(
    "(^|_)(group_labels|draw_border|keep_square|region_outlines|show_trend_line)$",
    names(defaults),
    value = TRUE
  )
  valid <- all(vapply(defaults[flags], scalar, logical(1), type = "logical")) &&
    scalar(defaults$label_size, c("double", "integer")) &&
    is.finite(defaults$label_size) &&
    defaults$label_size >= 8 &&
    defaults$label_size <= 24 &&
    scalar(defaults$color_clip, c("double", "integer")) &&
    defaults$color_clip %in% c(0, .01, .02, .05)
  choices <- list(
    font_family = c("system-ui", "Arial", "Georgia"),
    gene_palette = c("cerebro_orange", "blues", "greens", "viridis"),
    gene_panel_mode = c("shared", "distinct"),
    gene_panel_palette = c("default", "soft"),
    gene_plotting_order = c("Input order", "Highest expression on top"),
    hla_legend_mode = c("auto", "always", "never")
  )
  ranges <- list(
    gene_trend_line_width = c(1, 10),
    trajectory_density_opacity = c(0, 1),
    trajectory_trend_line_width = c(1, 10),
    ir_scatter_point_size = c(.1, 6),
    ir_scatter_opacity = c(.1, 1),
    hla_node_scale = c(.3, 2.5)
  )
  valid <- valid &&
    all(vapply(
      names(ranges),
      function(key) {
        x <- defaults[[key]]
        scalar(x, c("double", "integer")) &&
          is.finite(x) &&
          x >= ranges[[key]][[1L]] &&
          x <= ranges[[key]][[2L]]
      },
      logical(1)
    ))
  valid <- valid &&
    all(vapply(
      names(choices),
      function(key) {
        scalar(defaults[[key]], "character") &&
          defaults[[key]] %in% choices[[key]]
      },
      logical(1)
    ))
  if (!valid) {
    stop("Viewer display settings are invalid.", call. = FALSE)
  }
  defaults$label_size <- as.double(defaults$label_size)
  defaults$color_clip <- as.double(defaults$color_clip)
  for (key in names(ranges)) {
    defaults[[key]] <- as.double(defaults[[key]])
  }
  defaults
}

viewerDisplayDefaults <- function(options, dataset) {
  value <- options[["viewer_content"]][[dataset]][["display"]]
  tryCatch(builderViewerDisplayDefaults(value), error = function(e) {
    builderViewerDisplayDefaults()
  })
}

viewerPageDisplayDefaults <- function(options, dataset, page) {
  value <- viewerDisplayDefaults(options, dataset)
  prefix <- paste0(page, "_")
  keys <- names(value)[startsWith(names(value), prefix)]
  out <- value[keys]
  names(out) <- substring(keys, nchar(prefix) + 1L)
  out
}

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

builder_viewer_spatial_scene <- function(
  id,
  label = id,
  kind,
  source_id = id,
  unit = "Spatial coordinate units",
  observations,
  annotations = list(),
  layers = "points"
) {
  structure(
    list(
      id = id,
      label = label,
      kind = kind,
      source_id = source_id,
      unit = unit,
      observations = observations,
      annotations = annotations,
      layers = layers
    ),
    class = c("builder_viewer_spatial_scene", "list")
  )
}

builder_viewer_spatial_annotation <- function(
  metadata,
  cells,
  candidates
) {
  empty <- list(
    field = NULL,
    count = 0L,
    values = character(),
    truncated = FALSE
  )
  if (!is.data.frame(metadata) || !length(cells) || !ncol(metadata)) {
    return(empty)
  }
  matches <- match(
    tolower(candidates),
    tolower(colnames(metadata)),
    nomatch = 0L
  )
  matches <- matches[matches > 0L]
  if (!length(matches)) {
    return(empty)
  }
  field <- colnames(metadata)[[matches[[1L]]]]
  keys <- if ("cell_barcode" %in% colnames(metadata)) {
    as.character(metadata[["cell_barcode"]])
  } else {
    rownames(metadata)
  }
  row_index <- match(cells, keys)
  values <- metadata[[field]][row_index[!is.na(row_index)]]
  if (!is.atomic(values) || is.list(values)) {
    return(empty)
  }
  values <- unique(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]
  preview_count <- min(length(values), 5L)
  preview <- values[seq_len(preview_count)]
  bound <- function(value) {
    value <- enc2utf8(value)
    if (nchar(value, type = "bytes") <= 80L) {
      return(value)
    }
    keep <- min(nchar(value), 77L)
    while (
      keep > 1L &&
        nchar(substr(value, 1L, keep), type = "bytes") > 77L
    ) {
      keep <- keep - 1L
    }
    paste0(substr(value, 1L, keep), "...")
  }
  bounded <- vapply(preview, bound, character(1))
  list(
    field = field,
    count = as.integer(length(values)),
    values = unname(bounded),
    truncated = length(values) > 5L ||
      !identical(unname(preview), unname(bounded))
  )
}

builder_viewer_spatial_scene_label <- function(id, annotations) {
  describe <- function(annotation, singular, plural) {
    if (!length(annotation$count) || annotation$count < 1L) {
      return(character())
    }
    if (annotation$count == 1L) {
      return(paste0(singular, ": ", annotation$values[[1L]]))
    }
    paste(annotation$count, plural)
  }
  paste(
    c(
      id,
      describe(annotations$sample, "sample", "samples"),
      describe(annotations$roi, "ROI", "ROIs")
    ),
    collapse = " \u00b7 "
  )
}

builder_viewer_page_catalog <- function() {
  list(
    always = .builder_page_table(
      id = c(
        "data_info",
        "projection",
        "linked_views",
        "groups",
        "gene_expression",
        "gene_id_conversion",
        "color_management",
        "about"
      ),
      tab_name = c(
        "loadData",
        "overview",
        "coordinated_views",
        "groups",
        "geneExpression",
        "geneIdConversion",
        "color_management",
        "about"
      ),
      label = c(
        "Data info",
        "Projection",
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
        "link",
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
        "spatial",
        "trekker",
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
