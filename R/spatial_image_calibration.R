## Placement of a tissue picture from its calibration (pixel size, origin and
## orientation), shared by every import that knows it.
##
## An embedded picture is placed by two fields the Viewer reads:
## `histology_image_bounds`, the data coordinates the picture covers, and
## `histology_alignment`, the settings it opens with (the same live settings
## the Builder writes for its pictures). The Viewer draws a picture's first
## row at `ymax` and its first column at `xmin`. Spot and cell coordinates
## taken from a picture count rows downwards, so the picture opens flipped
## vertically; the pixels themselves are not changed.

#' Bounds and opening orientation of a picture from two of its corners
#'
#' @param first Data coordinates `c(x, y)` of the picture's first pixel corner
#'   (row 0, column 0).
#' @param last Data coordinates `c(x, y)` of the opposite corner (after the
#'   last row and the last column).
#' @return `list(histology_image_bounds, histology_alignment)`.
#' @keywords internal
#' @noRd
.spatialImagePlacement <- function(first, last) {
  first <- as.numeric(first)
  last <- as.numeric(last)
  if (
    length(first) != 2L ||
      length(last) != 2L ||
      any(!is.finite(c(first, last))) ||
      first[[1L]] == last[[1L]] ||
      first[[2L]] == last[[2L]]
  ) {
    stop(
      "A tissue picture needs two distinct, finite corners to be placed.",
      call. = FALSE
    )
  }
  list(
    histology_image_bounds = c(
      xmin = min(first[[1L]], last[[1L]]),
      xmax = max(first[[1L]], last[[1L]]),
      ymin = min(first[[2L]], last[[2L]]),
      ymax = max(first[[2L]], last[[2L]])
    ),
    histology_alignment = list(
      geometry = "live",
      use_as_defaults = TRUE,
      viewer_setting = list(
        flip_x = first[[1L]] > last[[1L]],
        flip_y = first[[2L]] < last[[2L]]
      )
    )
  )
}

#' Placement of a picture that is the full-resolution image scaled by `scale`
#'
#' Spots are in full-resolution pixels, so the picture covers
#' `[0, width / scale]` by `[0, height / scale]`, row 0 at y = 0.
#' @keywords internal
#' @noRd
.spatialImageScaled <- function(width, height, scale) {
  if (
    !is.numeric(scale) ||
      length(scale) != 1L ||
      !is.finite(scale) ||
      scale <= 0
  ) {
    stop("A tissue picture's scale factor must be positive.", call. = FALSE)
  }
  .spatialImagePlacement(c(0, 0), c(width / scale, height / scale))
}

#' The tissue picture Seurat keeps in a Visium image, placed by its scale factor
#'
#' Seurat stores full-resolution spot coordinates and the scale factors for
#' both Space Ranger pictures. A `cerebro_image_label` attribute on the stored
#' pixels identifies which factor to use when the extent is ambiguous.
#'
#' @param image A Seurat spatial image.
#' @param coordinates Exported coordinates of the image's spots (x, y).
#' @return A one-picture `histology_images` manifest, or NULL.
#' @keywords internal
#' @noRd
.seuratVisiumImage <- function(image, coordinates) {
  if (!methods::is(image, "VisiumV2")) {
    return(NULL)
  }
  pixels <- tryCatch(image@image, error = function(e) NULL)
  factors <- tryCatch(unclass(image@scale.factors), error = function(e) NULL)
  if (
    !is.numeric(pixels) ||
      !(is.matrix(pixels) ||
        (length(dim(pixels)) == 3L && dim(pixels)[[3L]] %in% 2:4)) ||
      !is.list(factors) ||
      !is.data.frame(coordinates) ||
      !nrow(coordinates)
  ) {
    return(NULL)
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    message(
      "[INFO] The 'png' package is needed to embed the Visium tissue ",
      "picture; it was skipped."
    )
    return(NULL)
  }
  label <- attr(pixels, "cerebro_image_label", exact = TRUE)
  orientation <- tryCatch(image@coords_x_orientation, error = function(e) NULL)
  # Seurat before 5.4 stored the pixel row as x and had no orientation slot.
  if (!identical(orientation, "horizontal")) {
    axes <- seq_along(dim(pixels))
    axes[1:2] <- c(2L, 1L)
    pixels <- aperm(pixels, axes)
  }
  height <- dim(pixels)[[1L]]
  width <- dim(pixels)[[2L]]
  labels <- c("lowres", "hires")
  if (!is.null(label)) {
    if (length(label) != 1L || !label %in% labels) {
      stop("'cerebro_image_label' must be 'lowres' or 'hires'.", call. = FALSE)
    }
    labels <- label
  } else if (max(height, width) > 600L) {
    # A standard Space Ranger lowres image has at most 600 pixels per side.
    labels <- "hires"
  }
  scales <- vapply(
    labels,
    function(label) {
      scale <- factors[[label]]
      if (is.numeric(scale) && length(scale) == 1L) scale else NA_real_
    },
    numeric(1L),
    USE.NAMES = TRUE
  )
  scales <- scales[is.finite(scales) & scales > 0]
  fits <- vapply(
    scales,
    function(scale) {
      isTRUE(all(
        coordinates$x >= 0 &
          coordinates$x <= width / scale &
          coordinates$y >= 0 &
          coordinates$y <= height / scale
      ))
    },
    logical(1L)
  )
  scales <- scales[fits]
  if (!length(scales)) {
    message(
      "[INFO] The Visium tissue picture does not cover the spots at any ",
      "applicable scale factor; it was left out."
    )
    return(NULL)
  }
  if (length(unique(scales)) > 1L) {
    message(
      "[INFO] The Visium tissue picture has an ambiguous scale and was left ",
      "out. Set the 'cerebro_image_label' attribute on the stored image ",
      "pixels to 'lowres' or 'hires'."
    )
    return(NULL)
  }
  label <- names(scales)[[1L]]
  placement <- .spatialImageScaled(width, height, scales[[1L]])
  pixels[pixels < 0] <- 0
  pixels[pixels > 1] <- 1
  stats::setNames(
    list(c(
      list(histology_image = .h5adImageDataUri(pixels)),
      placement,
      list(image_label = label)
    )),
    label
  )
}
