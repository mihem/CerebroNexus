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
  if (
    is.null(crb_files) ||
      length(selected) != 1L ||
      is.na(selected) ||
      is.null(names(crb_files))
  ) {
    return(NULL)
  }
  ## The selector carries the configured dataset key. Accept paths only for
  ## legacy callers; a shared CRB file must not collapse two configured entries.
  if (selected %in% names(crb_files)) {
    return(as.character(selected))
  }
  index <- which(crb_files == selected)
  if (length(index) == 0L) {
    return(NULL)
  }
  dataset <- names(crb_files)[[index[[1L]]]]
  if (is.na(dataset) || !nzchar(dataset)) NULL else dataset
}

## Reusing the stored Spatial coordinates is safe only for the exact canonical
## full-order view. Any filtered, sampled, reordered, incomplete, or non-
## canonical index must keep using the general match/subset path.
spatial_coordinates_are_canonical_full_order <- function(
  cells_to_show,
  canonical_index,
  coordinate_count
) {
  coordinate_count <- suppressWarnings(as.integer(coordinate_count))
  if (
    length(coordinate_count) != 1L ||
      is.na(coordinate_count) ||
      coordinate_count < 0L ||
      length(canonical_index) != coordinate_count ||
      length(cells_to_show) != coordinate_count ||
      !is.integer(canonical_index) ||
      !is.integer(cells_to_show)
  ) {
    return(FALSE)
  }
  identical(canonical_index, seq_len(coordinate_count)) &&
    identical(cells_to_show, canonical_index)
}

## Resolve only options$spatial_images[[dataset]][[spatial_name]]. Each result
## is a descriptor so its display label is never confused with a filesystem
## path, and descriptor bounds survive all the way to the renderer.
spatial_images_for_roi <- function(images, roi_value = NULL) {
  if (is.null(roi_value)) {
    return(images)
  }
  if (identical(roi_value, "__separate__")) {
    return(images)
  }
  roi_value <- spatial_roi_value(as.character(roi_value %||% ""))
  if (length(roi_value) != 1L || is.na(roi_value)) {
    roi_value <- ""
  }
  scopes <- vapply(
    images,
    function(image) {
      as.character(image$roi_value %||% "")[[1L]]
    },
    character(1)
  )
  if (nzchar(roi_value)) {
    matching <- scopes == roi_value
    if (any(matching)) {
      return(images[matching])
    }
  }
  images[!nzchar(scopes)]
}

configured_spatial_images <- function(
  options,
  dataset,
  spatial_name,
  roi_value = NULL
) {
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
      return(c(
        list(
          path = value[["path"]],
          bounds = value[["bounds"]],
          viewport_bounds = value[["viewport_bounds"]]
        ),
        if (!is.null(value[["label"]])) list(label = value[["label"]]),
        value[intersect(c("roi_field", "roi_value"), names(value))]
      ))
    }
    NULL
  }
  images <- lapply(as.list(leaf), normalize)
  spatial_images_for_roi(
    images[!vapply(images, is.null, logical(1))],
    roi_value
  )
}

## Canonical .crb files carry histology_images; older files carry one singular
## histology_image. Prefer the canonical manifest when both happen to exist.
embedded_spatial_images <- function(spatial_data, roi_value = NULL) {
  manifest <- spatial_data[["histology_images"]]
  if (is.list(manifest) && length(manifest) > 0L && !is.null(names(manifest))) {
    normalize <- function(payload) {
      if (!is.list(payload) || is.null(payload[["histology_image"]])) {
        return(NULL)
      }
      c(
        list(
          image = payload[["histology_image"]],
          bounds = payload[["histology_image_bounds"]],
          alignment = payload[["histology_alignment"]]
        ),
        if (!is.null(payload[["image_label"]])) {
          list(label = payload[["image_label"]])
        },
        payload[intersect(c("roi_field", "roi_value"), names(payload))]
      )
    }
    images <- lapply(manifest, normalize)
    return(spatial_images_for_roi(
      images[!vapply(images, is.null, logical(1))],
      roi_value
    ))
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

spatial_background_choices <- function(embedded_images, external_images) {
  labels <- function(images) {
    keys <- names(images) %||% character()
    vapply(
      seq_along(images),
      function(index) {
        label <- images[[index]][["label"]]
        if (
          is.character(label) &&
            length(label) == 1L &&
            !is.na(label) &&
            nzchar(label)
        ) {
          spatial_background_display_label(label)
        } else {
          spatial_background_display_label(keys[[index]])
        }
      },
      character(1)
    )
  }
  c(
    if (length(embedded_images) > 0L) {
      stats::setNames(
        paste0("embedded::", names(embedded_images)),
        labels(embedded_images)
      )
    },
    if (length(external_images) > 0L) {
      stats::setNames(
        paste0("external::", names(external_images)),
        labels(external_images)
      )
    }
  )
}

spatial_roi_background_groups <- function(
  spatial_data,
  options,
  dataset,
  spatial_name,
  roi_values
) {
  roi_values <- unique(as.character(roi_values))
  roi_values <- roi_values[!is.na(roi_values) & nzchar(roi_values)]
  groups <- lapply(seq_along(roi_values), function(index) {
    roi <- roi_values[[index]]
    embedded <- embedded_spatial_images(spatial_data, roi)
    external <- configured_spatial_images(
      options,
      dataset,
      spatial_name,
      roi
    )
    if (!length(embedded) && !length(external)) {
      return(NULL)
    }
    choices <- spatial_background_choices(embedded, external)
    list(
      roi = roi,
      embedded = embedded,
      external = external,
      choices = choices,
      tokens = stats::setNames(
        paste0("roi-", index, "-background-", seq_along(choices)),
        unname(choices)
      )
    )
  })
  groups <- groups[!vapply(groups, is.null, logical(1))]
  stats::setNames(groups, vapply(groups, `[[`, character(1), "roi"))
}

spatial_roi_background_selections <- function(groups, selected_tokens) {
  selected_tokens <- as.character(selected_tokens %||% character())
  selections <- lapply(groups, function(group) {
    selected <- intersect(unname(group$tokens), selected_tokens)
    selected_choice <- if (length(selected)) {
      names(group$tokens)[match(selected[[1L]], group$tokens)]
    } else {
      NULL
    }
    if (is.null(selected_choice)) {
      "none"
    } else {
      normalize_spatial_background_choice(selected_choice, group$choices)
    }
  })
  stats::setNames(selections, names(groups))
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
  "none"
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
  key <- sub("^[^:]+::", "", background_image)
  images <- if (identical(source, "embedded")) {
    embedded_images
  } else {
    external_images
  }
  descriptor <- images[[key]]
  if (is.null(descriptor)) {
    return(NULL)
  }
  label <- descriptor[["label"]]
  if (
    !is.character(label) ||
      length(label) != 1L ||
      is.na(label) ||
      !nzchar(label)
  ) {
    label <- key
  }
  c(
    list(source = source, key = key, label = label),
    descriptor[setdiff(names(descriptor), "label")]
  )
}

spatial_background_preset <- function(
  options,
  dataset,
  spatial_name,
  descriptor
) {
  image_key <- if (is.null(descriptor)) {
    NULL
  } else {
    descriptor$key %||% descriptor$label
  }
  preset <- spatialImagePreset(options, dataset, spatial_name, image_key)
  if (!is.null(descriptor) && identical(descriptor$source, "embedded")) {
    preset <- spatialEmbeddedImagePreset(preset, descriptor$alignment)
  }
  preset
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
    key = descriptor[["key"]] %||% descriptor[["label"]],
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

spatial_background_identity_equal <- function(left, right) {
  if (!is.list(left) || !is.list(right)) {
    return(FALSE)
  }
  left_names <- sort(names(left) %||% character())
  right_names <- sort(names(right) %||% character())
  if (!identical(left_names, right_names) || !length(left_names)) {
    return(FALSE)
  }
  all(vapply(
    left_names,
    function(name) identical(left[[name]], right[[name]]),
    logical(1)
  ))
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
  image_opacity = 0.5
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

spatial_cell_boundaries <- function(
  boundaries,
  cells,
  max_vertices = 200000L
) {
  required <- c("cell_barcode", "x", "y", "part")
  if (
    !identical(class(boundaries), "data.frame") ||
      !identical(sort(names(boundaries)), sort(required)) ||
      !is.character(boundaries$cell_barcode) ||
      !is.character(boundaries$part) ||
      !is.numeric(boundaries$x) ||
      !is.numeric(boundaries$y) ||
      any(vapply(boundaries, is.object, logical(1))) ||
      !is.character(cells) ||
      is.object(cells) ||
      !is.numeric(max_vertices) ||
      length(max_vertices) != 1L ||
      is.na(max_vertices) ||
      !is.finite(max_vertices) ||
      max_vertices < 3L
  ) {
    return(list())
  }
  keep <- boundaries$cell_barcode %in%
    cells &
    !is.na(boundaries$cell_barcode) &
    nzchar(boundaries$cell_barcode) &
    !is.na(boundaries$part) &
    nzchar(boundaries$part) &
    is.finite(boundaries$x) &
    is.finite(boundaries$y)
  boundaries <- boundaries[keep, required, drop = FALSE]
  if (!nrow(boundaries)) {
    return(list())
  }
  cell_order <- unique(boundaries$cell_barcode)
  counts <- tabulate(match(boundaries$cell_barcode, cell_order))
  allowed <- cell_order[cumsum(counts) <= floor(max_vertices)]
  boundaries <- boundaries[boundaries$cell_barcode %in% allowed, , drop = FALSE]
  if (!nrow(boundaries)) {
    return(list())
  }
  list(
    cell_barcode = unname(boundaries$cell_barcode),
    part = unname(boundaries$part),
    x = unname(as.numeric(boundaries$x)),
    y = unname(as.numeric(boundaries$y))
  )
}

spatial_molecule_overlay <- function(
  molecules,
  gene,
  max_points = 50000L,
  scoped = FALSE
) {
  if (isTRUE(scoped) || !is.list(molecules) || is.object(molecules)) {
    return(list())
  }
  data <- molecules[["data"]]
  if (
    !identical(class(data), "data.frame") ||
      !identical(names(data), c("gene", "x", "y")) ||
      !is.character(data$gene) ||
      !is.numeric(data$x) ||
      !is.numeric(data$y) ||
      any(vapply(data, is.object, logical(1))) ||
      !is.character(gene) ||
      length(gene) != 1L ||
      is.na(gene) ||
      !is.numeric(max_points) ||
      length(max_points) != 1L ||
      is.na(max_points) ||
      !is.finite(max_points) ||
      max_points < 1L
  ) {
    return(list())
  }
  keep <- !is.na(data$gene) & data$gene == gene &
    is.finite(data$x) & is.finite(data$y)
  data <- data[keep, , drop = FALSE]
  if (!nrow(data)) {
    return(list())
  }
  if (nrow(data) > max_points) {
    keep <- unique(round(seq.int(
      1L,
      nrow(data),
      length.out = floor(max_points)
    )))
    data <- data[keep, , drop = FALSE]
  }
  list(x = unname(as.numeric(data$x)), y = unname(as.numeric(data$y)))
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


spatial_roi_is_specific <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value) &&
    !value %in% c("__all__", "__separate__")
}

spatial_roi_value <- function(value) {
  if (spatial_roi_is_specific(value)) value else ""
}

spatial_metadata_facet <- function(metadata, cells, candidates) {
  empty <- list(
    field = NULL,
    by_cell = stats::setNames(rep(NA_character_, length(cells)), cells),
    values = character()
  )
  if (
    !is.data.frame(metadata) ||
      !is.character(cells) ||
      !is.character(candidates) ||
      !length(candidates)
  ) {
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
  field <- colnames(metadata)[matches[[1L]]]
  column <- metadata[[field]]
  if (!is.atomic(column) || is.list(column)) {
    return(empty)
  }
  metadata_cells <- if ("cell_barcode" %in% colnames(metadata)) {
    as.character(metadata[["cell_barcode"]])
  } else {
    rownames(metadata)
  }
  rows <- match(cells, metadata_cells)
  by_cell <- rep(NA_character_, length(cells))
  valid <- !is.na(rows)
  by_cell[valid] <- as.character(column[rows[valid]])
  by_cell[is.na(by_cell) | !nzchar(by_cell)] <- NA_character_
  names(by_cell) <- cells
  list(
    field = field,
    by_cell = by_cell,
    values = unique(unname(by_cell[!is.na(by_cell)]))
  )
}

spatial_roi_transform_context <- function(
  coordinates,
  metadata,
  sample_value = "",
  degrees = 0
) {
  cells <- rownames(coordinates) %||% character()
  roi <- spatial_metadata_facet(
    metadata,
    cells,
    c("sample_roi", "roi", "roi_id", "region_of_interest")
  )
  scoped_cells <- cells
  if (
    is.character(sample_value) &&
      length(sample_value) == 1L &&
      !is.na(sample_value) &&
      nzchar(sample_value)
  ) {
    sample <- spatial_metadata_facet(
      metadata,
      cells,
      c("sample", "sample_id", "orig.ident")
    )
    scoped_cells <- names(sample$by_cell)[
      !is.na(sample$by_cell) & sample$by_cell == sample_value
    ]
  }
  scoped_coordinates <- coordinates[scoped_cells, , drop = FALSE]
  list(
    roi_by_cell = roi$by_cell,
    scoped_cells = scoped_cells,
    pivots = spatialRoiPivots(
      scoped_coordinates,
      as.character(roi$by_cell[scoped_cells]),
      degrees
    )
  )
}

spatial_roi_extents <- function(coordinates, roi_values) {
  if (
    !(is.data.frame(coordinates) || is.matrix(coordinates)) ||
      ncol(coordinates) < 2L ||
      length(roi_values) != nrow(coordinates)
  ) {
    return(list())
  }
  x <- coordinates[, 1L]
  y <- coordinates[, 2L]
  roi_values <- as.character(roi_values)
  valid <- !is.na(roi_values) & nzchar(roi_values) & is.finite(x) & is.finite(y)
  stats::setNames(
    lapply(unique(roi_values[valid]), function(roi) {
      rows <- valid & roi_values == roi
      list(
        x = range(x[rows]),
        y = range(y[rows])
      )
    }),
    unique(roi_values[valid])
  )
}

spatial_split_columns <- function(
  metadata,
  cells,
  groups = character()
) {
  if (
    !is.data.frame(metadata) ||
      !is.character(cells) ||
      !length(cells) ||
      !is.character(groups)
  ) {
    return(character())
  }
  metadata_cells <- if ("cell_barcode" %in% colnames(metadata)) {
    as.character(metadata[["cell_barcode"]])
  } else {
    rownames(metadata)
  }
  rows <- match(cells, metadata_cells)
  rows <- rows[!is.na(rows)]
  n_rows <- length(unique(rows))
  candidates <- setdiff(colnames(metadata), "cell_barcode")
  candidates[vapply(
    candidates,
    function(name) {
      values <- metadata[[name]][rows]
      if (
        !name %in% groups &&
          !(is.character(values) || is.factor(values) || is.logical(values))
      ) {
        return(FALSE)
      }
      values <- as.character(values)
      levels <- unique(values[!is.na(values) & nzchar(values)])
      length(levels) >= 2L &&
        length(levels) < n_rows
    },
    logical(1)
  )]
}

spatial_scene_choices <- function(spatial_names, spatial_data, metadata) {
  stats::setNames(spatial_names, spatial_names)
}

spatial_default_roi_selection <- function(roi_values) {
  roi_values <- unique(as.character(roi_values %||% character()))
  roi_values <- roi_values[!is.na(roi_values) & nzchar(roi_values)]
  if (!length(roi_values)) {
    return("__all__")
  }
  if (length(roi_values) == 1L) {
    return(roi_values[[1L]])
  }
  "__separate__"
}

## Resolve only options$spatial_images[[dataset]][[spatial_name]]. Each result
## is a descriptor so its display label is never confused with a filesystem
## path, and descriptor bounds survive all the way to the renderer.

spatial_background_key <- function(source, label) {
  paste0(source, "::", label)
}

spatial_background_display_label <- function(label) {
  stripped <- sub(
    "[.](png|jpe?g|tiff?|webp|bmp|gif|svg)$",
    "",
    label,
    ignore.case = TRUE
  )
  if (nzchar(stripped)) stripped else label
}

## Shared by Spatial, Linked views and Trekker; Spatial's page files load
## lazily, so the image boundary check must live with the eager helpers.
## Defense in depth at the file-read boundary: require both an exact allowlist
## match and canonical containment, including after resolving symbolic links.
authorized_spatial_image_path <- function(
  background_image,
  allowlist,
  cerebro_root
) {
  if (
    !is.character(background_image) ||
      length(background_image) != 1L ||
      is.na(background_image) ||
      !is.character(allowlist) ||
      !(background_image %in% allowlist) ||
      !is.character(cerebro_root) ||
      length(cerebro_root) != 1L ||
      is.na(cerebro_root)
  ) {
    return(NULL)
  }

  canonicalize <- function(path) {
    tryCatch(
      suppressWarnings(
        normalizePath(path, winslash = "/", mustWork = TRUE)
      ),
      error = function(error) NULL
    )
  }
  image_path <- canonicalize(file.path(cerebro_root, background_image))
  trusted_roots <- lapply(
    c("spatial-assets", "extdata"),
    function(root) canonicalize(file.path(cerebro_root, root))
  )
  trusted_roots <- Filter(Negate(is.null), trusted_roots)
  if (length(trusted_roots) == 0L || is.null(image_path)) {
    return(NULL)
  }

  if (.Platform$OS.type == "windows") {
    trusted_roots <- lapply(trusted_roots, tolower)
    image_path_comparison <- tolower(image_path)
  } else {
    image_path_comparison <- image_path
  }
  inside_trusted_root <- any(vapply(
    trusted_roots,
    function(root) {
      startsWith(
        image_path_comparison,
        paste0(sub("/+$", "", root), "/")
      )
    },
    logical(1)
  ))
  if (!inside_trusted_root) {
    return(NULL)
  }

  image_path
}
