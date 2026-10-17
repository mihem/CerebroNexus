## Viewer display settings shared by the package and generated app bundles. Keep this file pure base R: the runtime copy is
## sourced from a self-contained bundle with no package namespace available.

## Coordinate appearance has one typed contract for authoring and rendering.
viewerNamedSettings <- function(value) {
  is.list(value) && !is.object(value) &&
    (!length(value) || (!is.null(names(value)) && !anyNA(names(value)) &&
      all(nzchar(names(value))) && !anyDuplicated(names(value))))
}

viewerSpatialPointSetting <- function(value, roi = FALSE) {
  fields <- c(if (roi) "rotation_degrees", "point_opacity", "point_size")
  if (!viewerNamedSettings(value) || !setequal(names(value), fields) ||
      !all(vapply(value, function(x) is.numeric(x) && length(x) == 1L &&
        is.finite(x), logical(1))) ||
      value$point_size < 1 || value$point_size > 20 ||
      value$point_opacity < 0 || value$point_opacity > 1) return(NULL)
  lapply(value[fields], as.numeric)
}

## Extra-material labels are presentation metadata keyed by immutable table IDs.
## Discard unavailable labels and unrelated source-file metadata.
viewerExtraMaterialTableIndex <- function(value = NULL, table_names = NULL) {
  if (!is.list(value) || is.object(value) || is.null(names(value))) return(list())
  keys <- names(value)
  keep <- !is.na(keys) & nzchar(trimws(keys)) & !duplicated(keys)
  if (!is.null(table_names)) keep <- keep & keys %in% table_names
  result <- list()
  for (key in keys[keep]) {
    record <- value[[key]]
    if (!is.list(record) || is.object(record)) next
    labels <- list()
    for (field in c("workbook_name", "display_name")) {
      text <- record[[field]]
      if (is.character(text) && length(text) == 1L && !is.na(text) && nzchar(trimws(text))) {
        labels[[field]] <- trimws(text)
      }
    }
    if (length(labels)) result[[key]] <- labels
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
    gene_plotting_order = c("Input order", "Random", "Highest expression on top"),
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
  # Older bundles called input order "Random" without actually shuffling cells.
  if (identical(defaults$gene_plotting_order, "Random")) {
    defaults$gene_plotting_order <- "Input order"
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
