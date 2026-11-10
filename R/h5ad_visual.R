## Visual information that travels with an AnnData file ---------------------------
##
## scanpy keeps one colour per category in uns[<column>_colors]; scanpy, squidpy
## and CELLxGENE keep the Visium tissue images and their scale factors in
## uns["spatial"][<library_id>]. Both are read here and handed to the export
## core as plain structures (metadata(sce)$group_colors and
## metadata(sce)$spatial_images), so every input format can supply them the
## same way.

#' Colours as "#RRGGBB" (alpha dropped), or NULL when any entry is not a colour.
#' @keywords internal
#' @noRd
.h5adNormalizeColors <- function(colors) {
  colors <- as.character(colors)
  if (!length(colors) || anyNA(colors)) {
    return(NULL)
  }
  rgb <- tryCatch(
    grDevices::col2rgb(colors),
    error = function(e) NULL
  )
  if (is.null(rgb)) {
    return(NULL)
  }
  toupper(grDevices::rgb(rgb[1L, ], rgb[2L, ], rgb[3L, ], maxColorValue = 255))
}

#' scanpy's uns[<column>_colors] as a named vector (level -> colour) per
#' categorical column. A column is skipped unless it has exactly one valid
#' colour for every one of its levels.
#' @keywords internal
#' @noRd
.h5adReadColors <- function(ctx, columns) {
  result <- list()
  for (name in names(columns)) {
    values <- columns[[name]]
    path <- paste0("uns/", name, "_colors")
    if (!is.factor(values) || !.h5adHas(ctx, path)) {
      next
    }
    colors <- tryCatch(
      .h5adNormalizeColors(.h5adVector(.h5adRead(ctx, path))),
      error = function(e) NULL
    )
    if (is.null(colors) || length(colors) != nlevels(values)) {
      next
    }
    result[[name]] <- stats::setNames(colors, levels(values))
  }
  result
}

#' A tissue image as an H x W x C array in [0, 1].
#'
#' HDF5 stores the image C-order as (height, width, channels); rhdf5 returns
#' the reversed dimensions. Images are float in [0, 1] (scanpy) or uint8 in
#' [0, 255] (CELLxGENE, squidpy), told apart by their range.
#' @keywords internal
#' @noRd
.h5adReadImageArray <- function(ctx, path) {
  raw <- .h5adRead(ctx, path)
  if (length(dim(raw)) == 3L) {
    raw <- aperm(raw, c(3L, 2L, 1L))
  } else if (length(dim(raw)) == 2L) {
    raw <- t(raw)
  } else {
    return(NULL)
  }
  ## rhdf5 returns the integer types as integer and the floats as double. An
  ## integer image is 0-255 (or 0-65535); a float one is 0-1 unless it holds
  ## values well above 1, so a float image whose maximum is 1.000001 is not
  ## divided by 255
  integer_pixels <- is.integer(raw)
  storage.mode(raw) <- "double"
  if (length(dim(raw)) == 3L && !dim(raw)[[3L]] %in% c(1L, 3L, 4L)) {
    return(NULL)
  }
  if (length(dim(raw)) == 3L && dim(raw)[[3L]] == 1L) {
    raw <- raw[,, 1L]
  }
  top <- suppressWarnings(max(raw, na.rm = TRUE))
  if (is.finite(top) && (integer_pixels || top > 1.5)) {
    raw <- raw / (if (top > 255) 65535 else 255)
  }
  raw[raw < 0] <- 0
  raw[raw > 1] <- 1
  raw[is.na(raw)] <- 0
  raw
}

#' @keywords internal
#' @noRd
.h5adImageDataUri <- function(array) {
  bytes <- png::writePNG(array, target = raw())
  paste0(
    "data:image/png;base64,",
    gsub("\n", "", jsonlite::base64_enc(bytes), fixed = TRUE)
  )
}

#' Visium tissue images from uns["spatial"][<library_id>]["images"], ready to
#' embed as histology images.
#'
#' Spot coordinates in obsm["spatial"] are full-resolution pixels, and an image
#' is the full-resolution picture scaled by its scale factor. In coordinate
#' space it therefore covers [0, width / scale] x [0, height / scale], and that
#' is what is recorded as its bounds. The picture is stored in its own
#' orientation (row 0 on top); flipping it for display is the Spatial tab's
#' setting, as for Seurat objects.
#'
#' @param which "auto" (hires when present, otherwise lowres), a character
#'   vector of image labels (hires, lowres, fullres), or FALSE for none.
#' @return list(library_id -> list(label -> list(histology_image,
#'   histology_image_bounds, image_label))).
#' @keywords internal
#' @noRd
.h5adReadSpatialImages <- function(ctx, which = "auto", verbose = FALSE) {
  if (isFALSE(which) || !.h5adHas(ctx, "uns/spatial")) {
    return(list())
  }
  if (!requireNamespace("png", quietly = TRUE)) {
    message(
      "[INFO] The 'png' package is needed to embed Visium tissue images; ",
      "they were skipped."
    )
    return(list())
  }
  scale_names <- c(
    hires = "tissue_hires_scalef",
    lowres = "tissue_lowres_scalef",
    fullres = NA_character_
  )
  result <- list()
  for (library in .h5adChildren(ctx, "uns/spatial")) {
    base <- paste0("uns/spatial/", library)
    images_path <- paste0(base, "/images")
    if (!.h5adHas(ctx, images_path)) {
      next
    }
    present <- intersect(names(scale_names), .h5adChildren(ctx, images_path))
    labels <- if (identical(which, "auto")) {
      present[seq_len(min(1L, length(present)))]
    } else {
      intersect(as.character(which), present)
    }
    for (label in labels) {
      scale <- if (is.na(scale_names[[label]])) {
        1
      } else {
        scale_path <- paste0(base, "/scalefactors/", scale_names[[label]])
        if (.h5adHas(ctx, scale_path)) {
          as.numeric(.h5adVector(.h5adRead(ctx, scale_path)))[[1L]]
        } else {
          NA_real_
        }
      }
      if (!is.finite(scale) || scale <= 0) {
        if (verbose) {
          message(
            "[INFO] Skipping image `",
            label,
            "` of `",
            library,
            "`: no usable scale factor."
          )
        }
        next
      }
      array <- tryCatch(
        .h5adReadImageArray(ctx, paste0(images_path, "/", label)),
        error = function(e) NULL
      )
      if (is.null(array)) {
        next
      }
      height <- dim(array)[[1L]]
      width <- dim(array)[[2L]]
      result[[library]][[label]] <- list(
        histology_image = .h5adImageDataUri(array),
        histology_image_bounds = c(
          xmin = 0,
          xmax = width / scale,
          ymin = 0,
          ymax = height / scale
        ),
        image_label = label
      )
    }
  }
  result
}
