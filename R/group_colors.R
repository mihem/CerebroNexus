## Categorical colours shared by the Viewer and the Builder.
##
## A palette that runs out of colours is extended, not stretched: its own
## colours stay where they are, and every further level gets the candidate
## colour farthest (in CIELAB) from all the colours chosen so far. Stretching
## with colorRampPalette() squeezed in-between shades that neighbouring levels
## could not be told apart by, and moved every level's colour whenever a level
## was added.

#' The usable part of a data set's colours for one Group, or NULL.
#'
#' One rule for the exporter (which writes the colours into the CRB) and the
#' Builder (which offers them as "From the data set"): a named character vector
#' level -> "#RRGGBB". Colours that are not all valid hex colours make the whole
#' vector unusable; an entry without a level name is dropped, and when a level
#' name repeats the first colour is kept.
#' @noRd
.cerebroValidGroupColors <- function(colors) {
  if (
    !is.character(colors) ||
      is.null(names(colors)) ||
      !length(colors) ||
      anyNA(colors) ||
      !all(grepl("^#[0-9A-Fa-f]{6}$", colors))
  ) {
    return(NULL)
  }
  keep <- !is.na(names(colors)) &
    nzchar(names(colors)) &
    !duplicated(names(colors))
  colors <- colors[keep]
  if (length(colors)) colors else NULL
}

#' sRGB (a matrix of red, green, blue in 0-255) to CIELAB (D65).
#' @noRd
.cerebroSrgbToLab <- function(rgb) {
  linear <- rgb / 255
  linear <- ifelse(
    linear <= 0.04045,
    linear / 12.92,
    ((linear + 0.055) / 1.055)^2.4
  )
  xyz <- linear %*%
    t(matrix(
      c(
        0.4124564,
        0.3575761,
        0.1804375,
        0.2126729,
        0.7151522,
        0.0721750,
        0.0193339,
        0.1191920,
        0.9503041
      ),
      nrow = 3L,
      byrow = TRUE
    ))
  scaled <- sweep(xyz, 2L, c(0.95047, 1, 1.08883), "/")
  f <- ifelse(
    scaled > 216 / 24389,
    scaled^(1 / 3),
    (24389 / 27 * scaled + 16) / 116
  )
  cbind(
    L = 116 * f[, 2L] - 16,
    a = 500 * (f[, 1L] - f[, 2L]),
    b = 200 * (f[, 2L] - f[, 3L])
  )
}

.cerebro_palette_cache <- new.env(parent = emptyenv())

.cerebro_palette_max_distinct <- 2000L

#' `n` colours: the first `n` of `base`, then as many more as needed, each the
#' candidate farthest from all the colours chosen so far.
#'
#' The candidates are a grid over sRGB without the very dark, very light and
#' grey colours, so an added colour is never lost against the white background.
#' The result depends only on `base` and `n`, and is cached: the Viewer asks for
#' a palette every time it draws a Group, and 1,201 levels take about 0.2 s.
#' @noRd
.cerebroExtendPalette <- function(base, n) {
  n <- max(0L, as.integer(n))
  base <- as.character(base)
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  ## Past a couple of thousand levels more colours do not help the eye, and the
  ## candidate grid (about 12,000 colours) would run out: cycle instead.
  if (n > .cerebro_palette_max_distinct) {
    return(rep_len(
      .cerebroExtendPalette(base, .cerebro_palette_max_distinct),
      n
    ))
  }
  key <- paste(c(base, n), collapse = " ")
  cached <- .cerebro_palette_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  steps <- seq(0, 255, length.out = 24L)
  grid <- as.matrix(expand.grid(r = steps, g = steps, b = steps))
  lab <- .cerebroSrgbToLab(grid)
  keep <- lab[, 1L] >= 25 &
    lab[, 1L] <= 92 &
    sqrt(lab[, 2L]^2 + lab[, 3L]^2) >= 20
  grid <- grid[keep, , drop = FALSE]
  lab <- lab[keep, , drop = FALSE]
  distance_to <- function(point) {
    (lab[, 1L] - point[[1L]])^2 +
      (lab[, 2L] - point[[2L]])^2 +
      (lab[, 3L] - point[[3L]])^2
  }
  nearest <- rep(Inf, nrow(lab))
  if (length(base)) {
    chosen <- .cerebroSrgbToLab(t(grDevices::col2rgb(base)))
    for (i in seq_len(nrow(chosen))) {
      nearest <- pmin(nearest, distance_to(chosen[i, ]))
    }
  }
  added <- character(n - length(base))
  for (i in seq_along(added)) {
    best <- which.max(nearest)
    added[[i]] <- grDevices::rgb(
      grid[best, 1L],
      grid[best, 2L],
      grid[best, 3L],
      maxColorValue = 255
    )
    nearest <- pmin(nearest, distance_to(lab[best, ]))
  }
  out <- c(base, added)
  if (length(ls(.cerebro_palette_cache)) > 200L) {
    rm(list = ls(.cerebro_palette_cache), envir = .cerebro_palette_cache)
  }
  .cerebro_palette_cache[[key]] <- out
  out
}
