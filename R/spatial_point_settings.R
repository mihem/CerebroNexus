#' Normalize Viewer-only coordinate appearance for createShinyApp
#' @keywords internal
#' @noRd
.normalizeAppSpatialPointSettings <- function(settings, catalogs, roi = FALSE) {
  if (is.null(settings)) {
    return(NULL)
  }
  argument <- if (roi) "spatial_roi_settings" else "spatial_point_appearance"
  named <- function(value, context) {
    if (!is.list(value) || is.object(value)) {
      stop(context, " must be a named list.", call. = FALSE)
    }
    if (!length(value)) {
      return(character())
    }
    .spatialManifestNames(value, context)
  }
  leaf <- function(value, context) {
    named(value, context)
    normalized <- viewerSpatialPointSetting(value, roi)
    if (is.null(normalized)) {
      stop(
        context,
        ": requires finite numeric ",
        if (roi) "rotation_degrees, ",
        "point_size (1-20) and point_opacity (0-1).",
        call. = FALSE
      )
    }
    # Preserve the caller's field order; the shared contract owns validation.
    normalized[names(value)]
  }
  result <- list()
  for (dataset in named(settings, argument)) {
    if (!dataset %in% names(catalogs)) {
      stop(argument, ": unknown dataset ", dataset, ".", call. = FALSE)
    }
    sections <- settings[[dataset]]
    result[[dataset]] <- list()
    for (section in named(sections, paste(argument, dataset))) {
      if (!section %in% names(catalogs[[dataset]])) {
        stop(argument, ": unknown FOV ", section, ".", call. = FALSE)
      }
      context <- paste(argument, dataset, section, sep = "/")
      value <- sections[[section]]
      if (roi) {
        result[[dataset]][[section]] <- stats::setNames(
          lapply(named(value, context), function(key) {
            leaf(value[[key]], paste(context, key, sep = "/"))
          }),
          names(value)
        )
      } else {
        result[[dataset]][[section]] <- leaf(value, context)
      }
    }
  }
  result
}
