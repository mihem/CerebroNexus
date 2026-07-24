##----------------------------------------------------------------------------##
## Spatial helper functions, sourced into the app so the Spatial tab works in a
## plain `runApp("inst")` session without the package installed.
##
## This file is the single implementation used by the Shiny runtime and the
## unit tests. Keep runtime-only helpers here instead of duplicating them under
## R/.
##
## External calls stay namespaced (ape::Moran.I, grDevices::chull, stats::*), so
## only the host packages need to be installed, not CerebroNexus itself.
##----------------------------------------------------------------------------##

resolve_spatial_image_preset <- function(
  option_name,
  fallback,
  options,
  crb_files,
  selected
) {
  if (
    is.null(options) ||
      is.null(options[[option_name]]) ||
      is.null(crb_files) ||
      is.null(selected)
  ) {
    return(fallback)
  }
  idx <- which(crb_files == selected)
  if (length(idx) == 0) {
    return(fallback)
  }
  current_name <- names(crb_files)[idx[1]]
  if (
    is.null(current_name) ||
      !(current_name %in% names(options[[option_name]]))
  ) {
    return(fallback)
  }
  val <- options[[option_name]][[current_name]]
  if (is.null(val) || length(val) != 1 || is.na(val)) fallback else val
}

format_spatial_preset_code <- function(
  label,
  offset_x,
  offset_y,
  scale_x,
  scale_y,
  flip_x,
  flip_y
) {
  key <- function(value) paste0('c("', label, '" = ', value, ")")
  lines <- character(0)
  add <- function(option_name, value) {
    lines[[length(lines) + 1]] <<- paste0(
      '"',
      option_name,
      '" = ',
      key(value)
    )
  }
  if (isTRUE(offset_x != 0)) {
    add("spatial_images_offset_x", offset_x)
  }
  if (isTRUE(offset_y != 0)) {
    add("spatial_images_offset_y", offset_y)
  }
  if (isTRUE(scale_x != 1)) {
    add("spatial_images_scale_x", scale_x)
  }
  if (isTRUE(scale_y != 1)) {
    add("spatial_images_scale_y", scale_y)
  }
  if (isTRUE(flip_x)) {
    add("spatial_images_flip_x", "TRUE")
  }
  if (isTRUE(flip_y)) {
    add("spatial_images_flip_y", "TRUE")
  }
  if (length(lines) == 0) {
    return(
      "## No adjustments to persist — the overlay is at its default alignment."
    )
  }
  paste(lines, collapse = ",\n")
}

compute_group_hulls <- function(x, y, group) {
  result <- list()
  if (length(x) == 0) {
    return(result)
  }
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  group <- group[ok]
  for (g in unique(group)) {
    in_g <- group == g
    gx <- x[in_g]
    gy <- y[in_g]
    if (length(gx) < 3) {
      next
    }
    ## chull needs at least 3 non-collinear points; collinear input returns a
    ## degenerate hull (< 3 vertices) that encloses no area — skip it.
    idx <- grDevices::chull(gx, gy)
    if (length(idx) < 3) {
      next
    }
    ## close the ring by repeating the first vertex
    idx <- c(idx, idx[1])
    result[[g]] <- list(x = gx[idx], y = gy[idx])
  }
  result
}

blend_genes_to_rgb <- function(r = NULL, g = NULL, b = NULL) {
  ## Determine the cell count from whichever channel is supplied.
  n <- max(length(r), length(g), length(b))
  channel <- function(values) {
    if (is.null(values)) {
      return(rep(0L, n))
    }
    values[is.na(values)] <- 0
    mx <- max(values)
    if (mx <= 0) {
      return(rep(0L, n))
    }
    as.integer(round(values / mx * 255))
  }
  rc <- channel(r)
  gc <- channel(g)
  bc <- channel(b)
  paste0("rgb(", rc, ",", gc, ",", bc, ")")
}

morans_i <- function(x, y, values, k = 6) {
  ok <- !is.na(x) & !is.na(y) & !is.na(values)
  x <- x[ok]
  y <- y[ok]
  values <- values[ok]
  n <- length(values)
  if (n < k + 1) {
    return(NA_real_)
  }
  if (stats::sd(values) == 0) {
    return(0)
  }
  ## Euclidean distance matrix, then a binary weight for each cell's k nearest
  ## neighbours (excluding itself). O(n^2); callers down-sample large inputs.
  dmat <- as.matrix(stats::dist(cbind(x, y)))
  weight <- matrix(0, n, n)
  for (i in seq_len(n)) {
    di <- dmat[i, ]
    di[i] <- Inf # never neighbour itself
    nn <- order(di)[seq_len(k)]
    weight[i, nn] <- 1
  }
  ## kNN adjacency is directional (i may be j's neighbour without the reverse),
  ## which yields an asymmetric, un-normalised weight matrix and pushes
  ## ape::Moran.I's statistic outside the documented [-1, 1] range. Symmetrise
  ## (undirected edge if either cell lists the other) then row-normalise so the
  ## weights sum to 1 per cell, giving a well-scaled statistic.
  weight <- pmax(weight, t(weight))
  row_sums <- rowSums(weight)
  row_sums[row_sums == 0] <- 1 # avoid 0/0 for isolated cells
  weight <- weight / row_sums
  ## Moran's I observed statistic, computed natively (matches ape::Moran.I()
  ## $observed to floating-point precision) so the viewer needs no ape dependency:
  ##   I = (n / W) * sum_ij w_ij (x_i - xbar)(x_j - xbar) / sum_i (x_i - xbar)^2
  z <- values - mean(values)
  W <- sum(weight)
  (n / W) * sum(weight * outer(z, z)) / sum(z^2)
}
