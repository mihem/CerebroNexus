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

spatial_dataset_name <- function(crb_files, selected) {
  if (is.null(crb_files) || is.null(selected) || is.null(names(crb_files))) {
    return(NULL)
  }
  index <- which(crb_files == selected)
  if (length(index) == 0L) {
    return(NULL)
  }
  dataset <- names(crb_files)[[index[[1L]]]]
  if (is.na(dataset) || !nzchar(dataset)) NULL else dataset
}

## Resolve only options$spatial_images[[dataset]][[spatial_name]]. Each result
## is a descriptor so its display label is never confused with a filesystem
## path, and descriptor bounds survive all the way to the renderer.
configured_spatial_images <- function(options, dataset, spatial_name) {
  if (
    is.null(options) ||
      is.null(dataset) ||
      is.null(spatial_name) ||
      is.null(options[["spatial_images"]][[dataset]][[spatial_name]])
  ) {
    return(list())
  }
  leaf <- options[["spatial_images"]][[dataset]][[spatial_name]]
  if (length(leaf) == 0L || is.null(names(leaf))) {
    return(list())
  }
  normalize <- function(value) {
    if (is.character(value) && length(value) == 1L && !is.na(value)) {
      return(list(path = unname(value), bounds = NULL))
    }
    if (
      is.list(value) &&
        is.character(value[["path"]]) &&
        length(value[["path"]]) == 1L &&
        !is.na(value[["path"]])
    ) {
      return(list(path = value[["path"]], bounds = value[["bounds"]]))
    }
    NULL
  }
  images <- lapply(as.list(leaf), normalize)
  images[!vapply(images, is.null, logical(1))]
}

## Canonical .crb files carry histology_images; older files carry one singular
## histology_image. Prefer the canonical manifest when both happen to exist.
embedded_spatial_images <- function(spatial_data) {
  manifest <- spatial_data[["histology_images"]]
  if (is.list(manifest) && length(manifest) > 0L && !is.null(names(manifest))) {
    normalize <- function(payload) {
      if (!is.list(payload) || is.null(payload[["histology_image"]])) {
        return(NULL)
      }
      list(
        image = payload[["histology_image"]],
        bounds = payload[["histology_image_bounds"]]
      )
    }
    images <- lapply(manifest, normalize)
    return(images[!vapply(images, is.null, logical(1))])
  }
  if (!is.null(spatial_data[["histology_image"]])) {
    return(list(
      "Tissue background" = list(
        image = spatial_data[["histology_image"]],
        bounds = spatial_data[["histology_image_bounds"]]
      )
    ))
  }
  list()
}

spatial_background_key <- function(source, label) {
  paste0(source, "::", label)
}

spatial_background_choices <- function(embedded_images, external_images) {
  c(
    "No Background" = "none",
    if (length(embedded_images) > 0L) {
      stats::setNames(
        paste0("embedded::", names(embedded_images)),
        names(embedded_images)
      )
    },
    if (length(external_images) > 0L) {
      stats::setNames(
        paste0("external::", names(external_images)),
        names(external_images)
      )
    }
  )
}

normalize_spatial_background_choice <- function(background_image, choices) {
  values <- unname(choices)
  if (
    is.character(background_image) &&
      length(background_image) == 1L &&
      !is.na(background_image) &&
      background_image %in% values
  ) {
    return(background_image)
  }
  if (length(values) > 1L) values[[2L]] else "none"
}

resolve_spatial_background <- function(
  background_image,
  embedded_images,
  external_images
) {
  if (
    !is.character(background_image) ||
      length(background_image) != 1L ||
      is.na(background_image) ||
      identical(background_image, "none")
  ) {
    return(NULL)
  }
  source <- if (startsWith(background_image, "embedded::")) {
    "embedded"
  } else if (startsWith(background_image, "external::")) {
    "external"
  } else {
    return(NULL)
  }
  label <- sub("^[^:]+::", "", background_image)
  images <- if (identical(source, "embedded")) {
    embedded_images
  } else {
    external_images
  }
  descriptor <- images[[label]]
  if (is.null(descriptor)) {
    return(NULL)
  }
  c(list(source = source, label = label), descriptor)
}

## The browser must distinguish a logical image from its encoded bytes. Two
## datasets can legitimately reuse an identical data URI while owning different
## presets, so keep the full resolved location as structured metadata instead
## of building a delimiter-based string that could collide on user labels.
spatial_background_identity <- function(dataset, spatial_name, descriptor) {
  if (is.null(descriptor)) {
    return(NULL)
  }
  values <- list(
    dataset = dataset,
    spatial_name = spatial_name,
    source = descriptor[["source"]],
    label = descriptor[["label"]]
  )
  if (
    any(vapply(
      values,
      function(value) {
        !is.character(value) || length(value) != 1L || is.na(value)
      },
      logical(1)
    ))
  ) {
    return(NULL)
  }
  values
}

format_spatial_preset_code <- function(
  dataset,
  spatial_name,
  image_label,
  offset_x,
  offset_y,
  scale_x,
  scale_y,
  flip_x,
  flip_y,
  rotation,
  image_opacity = 0.6
) {
  targets <- list(dataset, spatial_name, image_label)
  if (
    any(vapply(
      targets,
      function(value) {
        is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)
      },
      logical(1)
    ))
  ) {
    return(NULL)
  }
  quote_name <- function(value) encodeString(value, quote = '"')
  paste0(
    "spatial_image_settings = list(\n",
    "  ",
    quote_name(dataset),
    " = list(\n",
    "    ",
    quote_name(spatial_name),
    " = list(\n",
    "      ",
    quote_name(image_label),
    " = list(\n",
    "        flip_x = ",
    if (isTRUE(flip_x)) "TRUE" else "FALSE",
    ",\n",
    "        flip_y = ",
    if (isTRUE(flip_y)) "TRUE" else "FALSE",
    ",\n",
    "        scale_x = ",
    format(scale_x, scientific = FALSE),
    ",\n",
    "        scale_y = ",
    format(scale_y, scientific = FALSE),
    ",\n",
    "        offset_x = ",
    format(offset_x, scientific = FALSE),
    ",\n",
    "        offset_y = ",
    format(offset_y, scientific = FALSE),
    ",\n",
    "        rotation = ",
    format(rotation, scientific = FALSE),
    ",\n",
    "        image_opacity = ",
    format(image_opacity, scientific = FALSE),
    "\n",
    "      )\n",
    "    )\n",
    "  )\n",
    ")"
  )
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
