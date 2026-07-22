# ============================================================================
# Display helpers shared by the HLA motif Shiny module
# ============================================================================

#' Generate a stable distinct colour for every categorical level
#'
#' The first ten levels use the page's established palette. Larger sets switch
#' to an HCL palette rather than cycling colours and making different categories
#' visually indistinguishable.
#'
#' @param levels Character vector of categorical levels in display order.
#' @return Named character vector `level -> colour`.
#' @importFrom htmlwidgets JS
#' @keywords internal
hla_distinct_colors <- function(levels) {
  levels <- unique(as.character(levels))
  if (length(levels) == 0) {
    return(stats::setNames(character(0), character(0)))
  }
  base_palette <- c(
    "#636EFA",
    "#EF553B",
    "#00CC96",
    "#AB63FA",
    "#FFA15A",
    "#19D3F3",
    "#FF6692",
    "#B6E880",
    "#FF97FF",
    "#FECB52"
  )
  colours <- if (length(levels) <= length(base_palette)) {
    base_palette[seq_along(levels)]
  } else {
    grDevices::hcl.colors(length(levels), palette = "Dynamic")
  }
  stats::setNames(colours, levels)
}

## ---- Node radius: area encodes the clone count ------------------------- ##
## vis-network maps a node's `value` LINEARLY onto a dot's RADIUS between
## scaling$min and scaling$max, so passing a clone count as `value` makes the
## AREA — which is what the eye compares — grow with the SQUARE of the count.
## Measured on the shipped demo before this changed: counts 1..6 rendered at
## radii 8, 14.4, 20.8, 27.2, 33.6, 40, i.e. a 6-cell clone drawn at 25x the
## area of a 1-cell one. A 4x exaggeration.
##
## No scaling curve fixes that while the radius range is pinned: radii of 8 and
## 40 force a 25x area ratio between the smallest and largest node whatever
## function maps values onto them. The radius has to be set directly.

#' Smallest node radius, in px: the radius of a single-unit clone.
#' @keywords internal
HLA_NODE_R_MIN <- 8

#' Largest node radius, in px. One enormous clone must not swallow the layout.
#' @keywords internal
HLA_NODE_R_MAX <- 40

#' Clone size above which node area stops being proportional (the cap bites).
#' @keywords internal
HLA_NODE_MAX_EXACT <- (HLA_NODE_R_MAX / HLA_NODE_R_MIN)^2

#' Bounds of the display-only node size multiplier.
#'
#' A dense network can read as one blob at the default radii, and a sparse one
#' as specks. The multiplier is presentation only: it scales every radius and
#' the cap by the same factor, so it never changes what a node's area means.
#' @keywords internal
HLA_NODE_SCALE_MIN <- 0.3

#' @rdname HLA_NODE_SCALE_MIN
#' @keywords internal
HLA_NODE_SCALE_MAX <- 2.5

#' Node radius whose AREA is proportional to the clone count
#'
#' `r = R_MIN * sqrt(count)` makes area exactly proportional to `count`, so
#' twice the area means twice the units. Radius is capped at [HLA_NODE_R_MAX],
#' i.e. proportionality holds up to [HLA_NODE_MAX_EXACT] units and above that
#' every node draws the same. Callers must state that cap rather than let it
#' read as data; the tooltip carries the exact count either way.
#'
#' @param clone_count Numeric vector of per-node clone sizes. NA and values
#'   below 1 are floored to 1 (a drawn node stands for at least one unit).
#' @param scale Display multiplier applied to every radius, clamped to
#'   `[HLA_NODE_SCALE_MIN, HLA_NODE_SCALE_MAX]`. It scales the cap by the same
#'   factor, so the area-proportional reading is unchanged -- only how much of
#'   the canvas the network occupies. Invalid or non-positive values fall back
#'   to 1.
#' @return Numeric vector of radii in px.
#' @keywords internal
hla_node_radius <- function(clone_count, scale = 1) {
  n <- suppressWarnings(as.numeric(clone_count))
  if (length(n) == 0) {
    return(numeric(0))
  }
  n[is.na(n) | n < 1] <- 1
  s <- suppressWarnings(as.numeric(scale))[1]
  if (is.na(s) || s <= 0) {
    s <- 1
  }
  s <- max(HLA_NODE_SCALE_MIN, min(HLA_NODE_SCALE_MAX, s))
  pmin(HLA_NODE_R_MIN * sqrt(n), HLA_NODE_R_MAX) * s
}
