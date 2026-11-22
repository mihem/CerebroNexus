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

## Metadata columns naming a cell's sample and its region of interest, in order
## of preference.
spatial_sample_fields <- c("sample", "sample_id", "orig.ident")
spatial_roi_fields <- c("sample_roi", "roi", "roi_id", "region_of_interest")

spatial_dataset_name <- function(crb_files, selected) {
  if (
    is.null(crb_files) ||
      length(selected) != 1L ||
      is.na(selected) ||
      is.null(names(crb_files))
  ) {
    return(NULL)
  }
  ## The selection is the configured dataset id, never a path: two ids may
  ## share one CRB file and keep their own spatial settings.
  if (selected %in% names(crb_files)) as.character(selected) else NULL
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

## Embedded images travel in the entry's `histology_images` manifest.
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
  image_opacity = 1
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
  keep <- !is.na(data$gene) &
    data$gene == gene &
    is.finite(data$x) &
    is.finite(data$y)
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

## Canonical metadata rows of a section's cells: the Viewer Pack's index of the
## section, all rows when the section is the Pack's cell order, otherwise the
## cells matched against the metadata names (which hydrates a thin CRB).
spatial_section_rows <- function(
  spatial_name,
  coordinates = viewerSpatialCoordinates(spatial_name)
) {
  spatial_cells <- rownames(coordinates)
  pack <- viewerPackCurrent()
  pack_cell_count <- suppressWarnings(as.integer(pack$cell_count))
  valid_pack_count <- length(pack_cell_count) == 1L &&
    !is.na(pack_cell_count)
  index <- viewerPackSpatialIndex(pack, spatial_name)
  valid_index <- length(index) == nrow(coordinates) &&
    !anyNA(index) &&
    isTRUE(all(index >= 1L))
  if (!isTRUE(valid_index)) {
    index <- NULL
  }
  pack_order_matches <-
    is.null(index) &&
    !is.null(pack) &&
    is.list(pack) &&
    valid_pack_count &&
    length(spatial_cells) == pack_cell_count &&
    identical(
      viewerPackCellOrderFingerprint(spatial_cells),
      as.character(pack$manifest$cell_order_fingerprint)
    )
  if (isTRUE(pack_order_matches)) {
    index <- seq_len(pack_cell_count)
  }
  if (is.null(index)) {
    index <- match(spatial_cells, getMetaData()[["cell_barcode"]])
  }
  if (length(index) != nrow(coordinates) || anyNA(index)) {
    stop("Spatial cells do not match the canonical dataset order.")
  }
  as.integer(index)
}

## The metadata rows of a section as a thin CRB stores them (the attribute
## "cerebro_cell_index_rownames" of its coordinates), or NULL: the section has
## its cell names, or never stored its rows. Reads no cell name, and does not
## name the cells of the section as viewerSpatialCoordinates() does.
spatial_stored_section_rows <- function(spatial_name) {
  coordinates <- tryCatch(
    data_set()$spatial[[spatial_name]][["coordinates"]],
    error = function(error) NULL
  )
  if (
    !is.data.frame(coordinates) ||
      !isTRUE(attr(coordinates, "cerebro_cell_index_rownames", exact = TRUE))
  ) {
    return(NULL)
  }
  rows <- attr(coordinates, "row.names")
  if (is.integer(rows) && !anyNA(rows)) rows else NULL
}

## Canonical metadata rows of `cells` (NA when absent). Cells of one section,
## all of them or some, are found through the section's rows, which reads no
## cell names; anything else is matched against the metadata names.
## The sections whose cells are named are looked at first: `cells` are nearly
## always the cells of one of them, and the other sections of a thin CRB then
## keep their rows instead of being given 100,000 names each for a comparison.
spatial_canonical_rows <- function(cells) {
  section_rows <- function(name, coordinates) {
    tryCatch(
      spatial_section_rows(name, coordinates),
      error = function(error) NULL
    )
  }
  sections <- availableSpatial()
  named <- vapply(
    sections,
    function(name) is.null(spatial_stored_section_rows(name)),
    logical(1)
  )
  for (group in list(sections[named], sections[!named])) {
    coordinates <- lapply(group, viewerSpatialCoordinates)
    for (i in seq_along(group)) {
      if (identical(rownames(coordinates[[i]]), cells)) {
        rows <- section_rows(group[[i]], coordinates[[i]])
        if (!is.null(rows)) {
          return(rows)
        }
      }
    }
    for (i in seq_along(group)) {
      position <- match(cells, rownames(coordinates[[i]]))
      if (!anyNA(position)) {
        rows <- section_rows(group[[i]], coordinates[[i]])
        if (!is.null(rows)) {
          return(rows[position])
        }
      }
    }
  }
  match(cells, getMetaData()[["cell_barcode"]])
}

## Metadata row of each cell (NA when absent). match() hashes every metadata
## cell name on each call, and the Spatial controls ask for the facets of the
## same sections again and again (several per section on the first visit), so
## the rows of recent cell vectors are kept. identical() is immediate for the
## same vector and a pointer comparison per name for an equal copy.
## Metadata without cell names and with automatic row names is the first-frame
## metadata of a thin CRB, in canonical cell order; its rows come from the
## sections (see spatial_canonical_rows()), kept against that metadata: without
## a Viewer Pack section index each lookup matches every cell name.
.spatial_metadata_rows_cache <- new.env(parent = emptyenv())
spatial_metadata_rows <- function(metadata, cells) {
  thin <- !"cell_barcode" %in% colnames(metadata) &&
    .row_names_info(metadata) < 0L
  metadata_cells <- if (thin) {
    metadata
  } else if ("cell_barcode" %in% colnames(metadata)) {
    as.character(metadata[["cell_barcode"]])
  } else {
    rownames(metadata)
  }
  cache <- .spatial_metadata_rows_cache
  if (!identical(cache$metadata_cells, metadata_cells)) {
    cache$metadata_cells <- metadata_cells
    cache$entries <- list()
  }
  for (entry in cache$entries) {
    if (identical(entry$cells, cells)) {
      return(entry$rows)
    }
  }
  rows <- if (thin) {
    spatial_canonical_rows(cells)
  } else {
    match(cells, metadata_cells)
  }
  cache$entries <- c(
    list(list(cells = cells, rows = rows)),
    utils::head(cache$entries, 31L)
  )
  rows
}

## Sample values of each section. They change only with the dataset, while the
## Spatial scene asks for them on every sample, FOV or ROI change.
.spatial_section_samples_cache <- new.env(parent = emptyenv())
spatial_section_samples <- function() {
  cache <- .spatial_section_samples_cache
  dataset <- data_set()
  if (isTRUE(cache$set) && identical(cache$dataset, dataset)) {
    return(cache$value)
  }
  spatial_names <- availableSpatial()
  ## A section's rows (the Viewer Pack's index, or the rows a thin CRB stores)
  ## say which samples it has; its cells are named only when it is shown.
  facets <- lapply(spatial_names, function(name) {
    sample <- spatial_section_entry(name, roi = FALSE)$sample
    list(
      field = sample$field,
      values = unique(sample$values[!is.na(sample$values)])
    )
  })
  cache$value <- list(
    fields = unique(unlist(lapply(facets, `[[`, "field"), use.names = FALSE)),
    values = stats::setNames(lapply(facets, `[[`, "values"), spatial_names)
  )
  cache$dataset <- dataset
  cache$set <- TRUE
  cache$value
}

## The canonical metadata rows of a section, in the order of its coordinates,
## found without naming its cells: the Viewer Pack's index of the section, or
## the rows a thin CRB stores for it. NULL when there is neither.
spatial_section_index <- function(spatial_name) {
  coordinates <- tryCatch(
    data_set()$spatial[[spatial_name]][["coordinates"]],
    error = function(error) NULL
  )
  if (!is.data.frame(coordinates)) {
    return(NULL)
  }
  index <- tryCatch(
    viewerPackSpatialIndex(viewerPackCurrent(), spatial_name),
    error = function(error) NULL
  )
  if (
    is.integer(index) &&
      length(index) == nrow(coordinates) &&
      !anyNA(index) &&
      isTRUE(all(index >= 1L))
  ) {
    return(index)
  }
  spatial_stored_section_rows(spatial_name)
}

## The cell names of a section, as its getter gives them.
spatial_section_cell_names <- function(spatial_name) {
  record <- tryCatch(
    getSpatialData(spatial_name),
    error = function(error) list()
  )
  rownames(record[["coordinates"]]) %||% character()
}

## The canonical metadata rows of a section's cells (NA for a cell the
## metadata lacks): by integer index when there is one, otherwise by matching
## the names `section_cells()` gives against the metadata.
spatial_section_metadata_rows <- function(
  spatial_name,
  metadata = viewerProjectionFirstFrameMetadata(),
  section_cells = function() spatial_section_cell_names(spatial_name)
) {
  rows <- spatial_section_index(spatial_name)
  if (!is.null(rows)) {
    return(rows)
  }
  spatial_metadata_rows(metadata, section_cells())
}

## The values at `rows` of the first metadata column `candidates` name, as
## character, NA for a missing row, value or empty string.
spatial_rows_facet <- function(metadata, rows, candidates) {
  field <- spatial_metadata_field(metadata, candidates)
  values <- rep(NA_character_, length(rows))
  if (!is.null(field) && length(rows)) {
    valid <- !is.na(rows)
    values[valid] <- as.character(metadata[[field]][rows[valid]])
    values[is.na(values) | !nzchar(values)] <- NA_character_
  }
  list(field = field, values = values)
}

## A section's metadata rows and the sample and ROI of each of its cells, by
## position in its coordinates. The scene, the background picker, the plot
## parameters and the ROI rotation all ask for them: they are computed once
## for the metadata and what the rows were found from (the section's integer
## index, or its cell names), which is checked again on each call and is the
## same object while the data set is. Without a sample or ROI column no row is
## looked up.
.spatial_section_cache <- new.env(parent = emptyenv())
spatial_section_entry <- function(
  spatial_name,
  metadata = viewerProjectionFirstFrameMetadata(),
  section_cells = function() spatial_section_cell_names(spatial_name),
  roi = TRUE
) {
  fields <- c(
    spatial_metadata_field(metadata, spatial_sample_fields),
    spatial_metadata_field(metadata, spatial_roi_fields)
  )
  if (!length(fields)) {
    return(list(
      rows = NULL,
      sample = list(field = NULL, values = character()),
      roi = list(field = NULL, values = character())
    ))
  }
  cache <- .spatial_section_cache
  if (!isTRUE(cache$set) || !identical(cache$metadata, metadata)) {
    cache$metadata <- metadata
    cache$entries <- list()
    cache$set <- TRUE
  }
  index <- spatial_section_index(spatial_name)
  source <- if (is.null(index)) section_cells() else index
  key <- as.character(spatial_name)
  entry <- cache$entries[[key]]
  if (is.null(entry) || !identical(entry$source, source)) {
    rows <- if (is.null(index)) {
      spatial_metadata_rows(metadata, source)
    } else {
      index
    }
    entry <- list(
      source = source,
      rows = rows,
      sample = spatial_rows_facet(metadata, rows, spatial_sample_fields)
    )
    cache$entries[[key]] <- entry
  }
  ## The ROIs of every section are not needed for the samples.
  if (isTRUE(roi) && is.null(entry$roi)) {
    entry$roi <- spatial_rows_facet(metadata, entry$rows, spatial_roi_fields)
    cache$entries[[key]] <- entry
  }
  entry
}

## The cells of a section shown for `sample_value` ("" for every sample), as
## positions in its coordinates, with their metadata rows and ROI values.
## `rows` and `positions` are NULL when the metadata has no sample or ROI
## column (then the section has no sample or ROI to scope by).
spatial_section_scope <- function(
  spatial_name,
  sample_value = "",
  metadata = viewerProjectionFirstFrameMetadata(),
  section_cells = function() spatial_section_cell_names(spatial_name)
) {
  entry <- spatial_section_entry(spatial_name, metadata, section_cells)
  positions <- if (!is.null(entry$rows)) seq_along(entry$rows)
  if (
    !is.null(positions) &&
      is.character(sample_value) &&
      length(sample_value) == 1L &&
      !is.na(sample_value) &&
      nzchar(sample_value)
  ) {
    samples <- entry$sample$values
    positions <- which(!is.na(samples) & samples == sample_value)
  }
  roi <- entry$roi$values
  shown_roi <- if (is.null(positions)) roi else roi[positions]
  list(
    rows = entry$rows,
    positions = positions,
    scoped_rows = if (!is.null(positions)) entry$rows[positions],
    sample_field = entry$sample$field,
    roi_field = entry$roi$field,
    roi_by_position = roi,
    roi_values = unique(shown_roi[!is.na(shown_roi)])
  )
}

## The canonical metadata codes of a colour column (`resource`, as
## viewerMetadataCodesAsset() describes them for the levels of
## `color_assignments`) drawn for some cells only, whose `values` are given: the
## levels in the order a frame of these values lists them
## (cerebroCellViewScatterPayload(): the assigned levels the cells have, then
## "(missing)"), and the code map onto them. With the colours of those levels,
## or NULL when the codes cannot be mapped so.
spatial_section_codes_resource <- function(
  resource,
  values,
  color_assignments
) {
  map <- suppressWarnings(as.integer(resource[["code_map"]]))
  full_levels <- as.character(resource[["levels"]])
  if (
    !is.list(resource) ||
      length(map) < 1L ||
      anyNA(map) ||
      !length(color_assignments) ||
      is.null(names(color_assignments)) ||
      !(is.factor(values) || is.character(values) || is.logical(values))
  ) {
    return(NULL)
  }
  present <- if (is.factor(values)) {
    levels(values)[
      tabulate(as.integer(values)[!is.na(values)], nbins = nlevels(values)) > 0L
    ]
  } else {
    unique(as.character(values[!is.na(values)]))
  }
  missing <- anyNA(values)
  if (missing && !"(missing)" %in% names(color_assignments)) {
    color_assignments <- c(color_assignments, `(missing)` = "#7b8794")
  }
  in_view <- c(present, if (missing) "(missing)")
  if (!all(in_view %in% names(color_assignments))) {
    return(NULL)
  }
  traces <- names(color_assignments)[names(color_assignments) %in% in_view]
  to_trace <- match(full_levels, traces) - 1L
  to_trace[is.na(to_trace)] <- -1L
  ## Code map entry 1 is the Pack's code of a missing value.
  code_map <- rep(
    if (missing) match("(missing)", traces) - 1L else -1L,
    length(map)
  )
  coded <- map >= 0L
  if (any(map[coded] >= length(to_trace))) {
    return(NULL)
  }
  code_map[coded] <- to_trace[map[coded] + 1L]
  ## The browser types the codes by the size of the map (cell_views.js,
  ## fetchMetadataCodesResource()).
  limit <- if (length(code_map) - 2L <= 127L) {
    127L
  } else if (length(code_map) - 2L <= 32767L) {
    32767L
  } else {
    .Machine$integer.max
  }
  if (max(code_map) > limit) {
    return(NULL)
  }
  resource[["levels"]] <- I(traces)
  resource[["code_map"]] <- I(as.integer(code_map))
  list(resource = resource, color_assignments = color_assignments)
}

## spatial_roi_transform_context() by position: the ROI of each row of a
## section's `coordinates`, the rows of the selected sample (`scoped`) and the
## rotation pivots of its ROIs. `scope` is the section's
## spatial_section_scope(); coordinates of another length take the cell-name
## path.
spatial_roi_section_context <- function(
  coordinates,
  scope,
  metadata,
  sample_value = "",
  degrees = 0
) {
  n <- nrow(coordinates)
  roi <- scope$roi_by_position
  scoped <- scope$positions
  if (is.null(scope$rows)) {
    ## No sample or ROI column: no cell has a sample to be shown for.
    roi <- rep(NA_character_, n)
    scoped <- if (
      is.character(sample_value) &&
        length(sample_value) == 1L &&
        !is.na(sample_value) &&
        nzchar(sample_value)
    ) {
      integer()
    } else {
      seq_len(n)
    }
  }
  if (length(roi) != n) {
    context <- spatial_roi_transform_context(
      coordinates,
      metadata,
      sample_value,
      degrees
    )
    return(list(
      roi_by_position = unname(context$roi_by_cell),
      scoped = match(context$scoped_cells, rownames(coordinates)),
      pivots = context$pivots
    ))
  }
  list(
    roi_by_position = roi,
    scoped = scoped,
    pivots = spatialRoiPivots(
      coordinates[scoped, , drop = FALSE],
      as.character(roi[scoped]),
      degrees
    )
  )
}

## The sample the Spatial main controls show for `requested` (the first sample
## when it is none of them) and the sections showing it, as in the scene
## (UI_projection_main_parameters.R).
spatial_sample_sections <- function(requested) {
  samples <- spatial_section_samples()$values
  values <- unique(unlist(samples, use.names = FALSE))
  sample <- if (
    is.character(requested) &&
      length(requested) == 1L &&
      requested %in% values
  ) {
    requested
  } else if (length(values)) {
    values[[1L]]
  } else {
    ""
  }
  sections <- names(samples)
  if (nzchar(sample)) {
    sections <- sections[vapply(
      samples,
      function(section_values) sample %in% section_values,
      logical(1)
    )]
  }
  list(sample = sample, sections = sections)
}

## The metadata column a facet is read from: the first of `candidates` that
## names one (without regard to case), when its values are atomic. NULL
## otherwise.
spatial_metadata_field <- function(metadata, candidates) {
  if (
    !is.data.frame(metadata) ||
      !is.character(candidates) ||
      !length(candidates)
  ) {
    return(NULL)
  }
  matches <- match(
    tolower(candidates),
    tolower(colnames(metadata)),
    nomatch = 0L
  )
  matches <- matches[matches > 0L]
  if (!length(matches)) {
    return(NULL)
  }
  field <- colnames(metadata)[matches[[1L]]]
  column <- metadata[[field]]
  if (!is.atomic(column) || is.list(column)) {
    return(NULL)
  }
  field
}

spatial_metadata_facet <- function(metadata, cells, candidates) {
  empty <- list(
    field = NULL,
    by_cell = stats::setNames(rep(NA_character_, length(cells)), cells),
    values = character()
  )
  field <- if (is.character(cells)) {
    spatial_metadata_field(metadata, candidates)
  }
  if (is.null(field)) {
    return(empty)
  }
  column <- metadata[[field]]
  rows <- spatial_metadata_rows(metadata, cells)
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
    spatial_roi_fields
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
      spatial_sample_fields
    )
    scoped_cells <- names(sample$by_cell)[
      !is.na(sample$by_cell) & sample$by_cell == sample_value
    ]
  }
  ## Every cell of the section (no sample chosen): its rows as they are.
  ## Looking 66,000 cells up by name, rows and ROIs, took 2 s for the 59
  ## sections of the Allen atlas in Linked views.
  whole <- identical(scoped_cells, cells)
  scoped_coordinates <- if (whole) {
    coordinates
  } else {
    coordinates[scoped_cells, , drop = FALSE]
  }
  scoped_roi <- if (whole) unname(roi$by_cell) else roi$by_cell[scoped_cells]
  list(
    roi_by_cell = roi$by_cell,
    scoped_cells = scoped_cells,
    pivots = spatialRoiPivots(
      scoped_coordinates,
      as.character(scoped_roi),
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
  spatial_split_columns_at(
    metadata,
    spatial_metadata_rows(metadata, cells),
    groups
  )
}

## spatial_split_columns() for the cells at metadata `rows`.
spatial_split_columns_at <- function(metadata, rows, groups = character()) {
  if (
    !is.data.frame(metadata) ||
      !is.numeric(rows) ||
      !length(rows) ||
      !is.character(groups)
  ) {
    return(character())
  }
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

##----------------------------------------------------------------------------##
## Section grid: every section of the shown sample side by side.
##
## The grid is one frame of the cells of all sections, section after section,
## and one panel per section that draws its rows [start, start + count) of the
## frame (cell_views.js, buildSpecialistPanels()). The panels therefore share
## the frame's colour values and colour scale, and the browser's selection,
## which is by cell, spans the sections. Each panel carries the background
## image of its own section only.
##
## These helpers take their data through arguments, so tests can call them
## without a session.
##----------------------------------------------------------------------------##

## The layout Spatial opens with: the author's (Builder's display setting
## `spatial_layout`, read here without its page prefix), and the grid only
## for a data set with more than one section.
spatial_default_layout <- function(display, sections) {
  layout <- if (is.list(display)) display[["layout"]] else NULL
  if (identical(layout, "grid") && length(sections) > 1L) "grid" else "single"
}

## The sections the Sections field names, in its order: one is the single
## view, several the grid. `input` is the session's, `sections` the data
## set's. Before the field binds, the single section the older FOV input
## names (still set by saved views and test harnesses), or the author's
## default (spatial_projection_default_layout()).
spatial_projection_chosen_sections <- function(input, sections) {
  chosen <- input[["spatial_projection_sections"]]
  if (is.null(chosen)) {
    chosen <- input[["spatial_projection_to_display"]]
  }
  chosen <- as.character(chosen %||% character())
  chosen <- chosen[chosen %in% sections]
  if (
    !length(chosen) &&
      !"spatial_projection_sections" %in% shiny::isolate(names(input)) &&
      identical(spatial_projection_default_layout(), "grid")
  ) {
    chosen <- as.character(sections)
  }
  chosen
}
## The section of the single view (the first chosen), or NULL.
spatial_projection_single_section <- function(input, sections) {
  chosen <- spatial_projection_chosen_sections(input, sections)
  if (length(chosen)) chosen[[1L]] else NULL
}

## The layout the author chose for the session's data set. A Viewer without
## page display settings (or a test harness) opens one section.
spatial_projection_default_layout <- function() {
  display <- tryCatch(
    current_page_display_defaults("spatial"),
    error = function(error) list()
  )
  sections <- tryCatch(availableSpatial(), error = function(error) character())
  spatial_default_layout(display, sections)
}

## ImageFeaturePlot before a gene is chosen colours every cell by this one
## placeholder group, in light grey (hover reads "Gene: No gene selected").
spatial_no_gene_variable <- "Gene"
spatial_no_gene_label <- "No gene selected"
spatial_no_gene_color <- "#d5d9dd"

## Adds the placeholder column to `metadata` when the frame colours by it.
spatial_with_no_gene_column <- function(metadata, color_variable) {
  if (
    identical(color_variable, spatial_no_gene_variable) && !is.null(metadata)
  ) {
    metadata[[spatial_no_gene_variable]] <- factor(
      rep(spatial_no_gene_label, nrow(metadata)),
      levels = spatial_no_gene_label
    )
  }
  metadata
}

## The grid colours by a metadata column, one gene or the RGB blend of up to
## three genes.
spatial_grid_plot_types <- c(
  "ImageDimPlot",
  "ImageFeaturePlot",
  "Co-expression (RGB)"
)

## Whether a frame is a grid: asked for, for a plot type the grid draws, with
## more than one section to show.
spatial_grid_active <- function(layout, sections, plot_type) {
  identical(layout, "grid") &&
    length(sections) > 1L &&
    isTRUE(plot_type %in% spatial_grid_plot_types)
}

## The sections a grid frame holds. Up to `budget` cells in all it holds every
## section, so that a change among the chosen ones asks for no frame. Past it
## (the Allen atlas: 3.9 million cells in 59 sections) a first grid would wait
## for every section: the frame holds the chosen sections and as many others as
## fit, the nearest to a chosen one first, so the sections a user picks next
## are usually drawn already. The sections of the frame on screen (`previous`)
## stay while they hold every chosen one. In the order of `all`.
spatial_grid_frame_budget <- 1e6
spatial_grid_frame_sections <- function(
  all,
  chosen,
  cells_of,
  previous = character(),
  budget = spatial_grid_frame_budget
) {
  all <- as.character(all)
  counts <- vapply(all, function(section) as.numeric(cells_of(section)), 0)
  if (sum(counts) <= budget) {
    return(all)
  }
  if (
    length(previous) &&
      all(chosen %in% previous) &&
      all(previous %in% all)
  ) {
    return(all[all %in% previous])
  }
  keep <- all %in% chosen
  total <- sum(counts[keep])
  picked <- which(keep)
  others <- which(!keep)
  if (length(picked)) {
    distance <- vapply(others, function(i) min(abs(i - picked)), 0)
    others <- others[order(distance, others)]
  }
  for (i in others) {
    if (total + counts[[i]] <= budget) {
      keep[[i]] <- TRUE
      total <- total + counts[[i]]
    }
  }
  all[keep]
}

## The number of cells of a section, from its stored coordinates (a thin CRB
## keeps them as metadata rows: no cell name is read).
spatial_section_cell_count <- function(spatial_name) {
  coordinates <- tryCatch(
    data_set()$spatial[[spatial_name]][["coordinates"]],
    error = function(error) NULL
  )
  if (is.data.frame(coordinates)) nrow(coordinates) else 0L
}

## A section's positions for the grid. The grid places a section's rows by the
## Pack's index of the section (spatial_section_index()), in stored order, so it
## needs no cell names: naming every section's cells took 6 to 11 s of a first
## grid of the 3.9 million cells of the Allen atlas. Without that index, or with
## coordinates that are not plain x and y, as viewerSpatialCoordinates().
spatial_grid_section_xy <- function(spatial_name) {
  if (!is.null(spatial_section_index(spatial_name))) {
    coordinates <- tryCatch(
      data_set()$spatial[[spatial_name]][["coordinates"]],
      error = function(error) NULL
    )
    if (
      is.data.frame(coordinates) &&
        nrow(coordinates) > 0L &&
        is.numeric(coordinates[["x"]]) &&
        is.numeric(coordinates[["y"]])
    ) {
      return(coordinates)
    }
  }
  viewerSpatialCoordinates(spatial_name)
}

## The cells of a grid frame: the canonical rows of each section in turn,
## narrowed by `keep` (sample scope and group filters), each section sampled
## on its own to `percentage`. The sections and their cell counts travel as
## the "spatial_grid" attribute of the result.
spatial_grid_cells <- function(
  sections,
  index_of,
  keep = function(rows) rows,
  percentage = 100
) {
  sections <- as.character(sections)
  parts <- lapply(sections, function(section) {
    rows <- as.integer(keep(as.integer(index_of(section))))
    if (length(rows) && is.finite(percentage) && percentage < 100) {
      take <- ceiling(length(rows) * percentage / 100)
      rows <- rows[sort(sample.int(length(rows), take))]
    }
    rows
  })
  cells <- as.integer(unlist(parts, use.names = FALSE))
  attr(cells, "spatial_grid") <- list(
    sections = sections,
    counts = as.integer(lengths(parts))
  )
  cells
}

## The sections and counts of a grid frame's cells, or NULL for a frame of
## one section.
spatial_grid_of <- function(cells) {
  grid <- attr(cells, "spatial_grid", exact = TRUE)
  if (
    !is.list(grid) ||
      length(grid$sections) != length(grid$counts) ||
      sum(grid$counts) != length(cells)
  ) {
    return(NULL)
  }
  grid
}

## A data frame of x and y. data.frame() deparses a vector argument to name
## it: 0.5 s of a grid of the 3.9 million cells of the Allen atlas.
spatial_grid_xy_frame <- function(x, y) {
  structure(
    list(x = x, y = y),
    class = "data.frame",
    row.names = c(NA_integer_, -length(x))
  )
}

## The x and y columns of a section's coordinates.
spatial_grid_xy <- function(coordinates) {
  columns <- if (all(c("x", "y") %in% colnames(coordinates))) {
    c("x", "y")
  } else {
    1:2
  }
  spatial_grid_xy_frame(
    as.numeric(coordinates[[columns[[1L]]]]),
    as.numeric(coordinates[[columns[[2L]]]])
  )
}

## The extent of a section with a 2% margin, as the single-section frame pins
## its axes (spatial_projection_full_ranges()).
spatial_grid_range <- function(values) {
  if (!any(is.finite(values))) {
    return(NULL)
  }
  extent <- range(values, finite = TRUE)
  margin <- if (diff(extent) > 0) diff(extent) * 0.02 else 1
  c(extent[[1L]] - margin, extent[[2L]] + margin)
}

## Coordinates of a grid frame, in the frame's row order, each section rotated
## by its own rotation, with every section's full extent (all its cells, not
## only the sampled ones) for its panel's axes.
##   coordinates_of(section): the section's coordinates (rows in index order)
##   index_of(section): canonical rows of those coordinates
##   rotation_of(section): degrees
##   geometry_of(section, rows): the Viewer Pack file of the section's
##     positions for these rows, or NULL. A section with one has no positions
##     in the frame (NA): the browser reads them from the file
##     (`layout$geometry`, one entry per section).
spatial_grid_coordinates <- function(
  cells,
  coordinates_of,
  index_of,
  rotation_of = function(section) 0,
  geometry_of = function(section, rows) NULL
) {
  grid <- spatial_grid_of(cells)
  stopifnot(!is.null(grid))
  ends <- cumsum(grid$counts)
  starts <- ends - grid$counts
  xs <- ys <- vector("list", length(grid$sections))
  x_ranges <- y_ranges <- geometry <- vector("list", length(grid$sections))
  for (i in seq_along(grid$sections)) {
    section <- grid$sections[[i]]
    full <- spatial_grid_xy(coordinates_of(section))
    full <- rotateSpatialCoordinates(full, rotation_of(section))
    x_ranges[i] <- list(spatial_grid_range(full$x))
    y_ranges[i] <- list(spatial_grid_range(full$y))
    rows <- if (grid$counts[[i]]) {
      as.integer(cells[(starts[[i]] + 1L):ends[[i]]])
    } else {
      integer()
    }
    file <- if (length(rows)) geometry_of(section, rows)
    if (!is.null(file)) {
      geometry[i] <- list(file)
      xs[[i]] <- ys[[i]] <- rep(NA_real_, length(rows))
      next
    }
    index <- as.integer(index_of(section))
    ## Every cell of the section in its own order (no filter, no sampling):
    ## its positions as they are.
    if (identical(rows, index)) {
      xs[[i]] <- full$x
      ys[[i]] <- full$y
      next
    }
    at <- match(rows, index)
    if (anyNA(at)) {
      stop("Grid cells are not in their section: ", section)
    }
    xs[[i]] <- full$x[at]
    ys[[i]] <- full$y[at]
  }
  coordinates <- spatial_grid_xy_frame(
    as.numeric(unlist(xs, use.names = FALSE)),
    as.numeric(unlist(ys, use.names = FALSE))
  )
  list(
    coordinates = coordinates,
    sections = grid$sections,
    starts = as.integer(starts),
    counts = grid$counts,
    x_ranges = x_ranges,
    y_ranges = y_ranges,
    geometry = geometry
  )
}

## The grid's picture picker: one group per shown section that has pictures,
## offering each of the section's pictures. A token names the section by its
## place among all sections and the picture by its place in the section, so it
## stays the same while sections come and go.
spatial_grid_background_groups <- function(
  options,
  dataset,
  sections,
  all_sections,
  record
) {
  groups <- lapply(sections, function(section) {
    spatial_data <- record(section)
    embedded <- if (is.null(spatial_data)) {
      list()
    } else {
      embedded_spatial_images(spatial_data, "")
    }
    external <- configured_spatial_images(options, dataset, section, "")
    choices <- spatial_background_choices(embedded, external)
    if (!length(choices)) {
      return(NULL)
    }
    prefix <- paste0("grid-", match(section, all_sections), "-background-")
    list(
      section = section,
      tokens = stats::setNames(
        paste0(prefix, seq_along(choices)),
        names(choices)
      )
    )
  })
  groups <- groups[!vapply(groups, is.null, logical(1))]
  stats::setNames(groups, vapply(groups, `[[`, character(1), "section"))
}

## The picker's tokens as section -> picture index (0: no picture).
spatial_grid_background_picks <- function(tokens, all_sections) {
  parts <- regmatches(
    as.character(tokens %||% character()),
    regexec("^grid-([0-9]+)-background-([0-9]+)$", tokens %||% character())
  )
  picks <- list()
  for (part in parts) {
    if (length(part) != 3L) {
      next
    }
    section <- all_sections[as.integer(part[[2L]])]
    if (length(section) == 1L && !is.na(section)) {
      picks[[section]] <- as.integer(part[[3L]])
    }
  }
  picks
}

## The background a section shows in the grid: picture `pick` of the section
## (spatial_grid_background_picks(); its first without a pick), embedded or
## configured, for the whole section (no ROI). NULL without one or for 0.
spatial_grid_section_background <- function(
  options,
  dataset,
  spatial_name,
  spatial_data,
  pick = NULL
) {
  if (identical(pick, 0L)) {
    return(NULL)
  }
  embedded <- if (is.null(spatial_data)) {
    list()
  } else {
    embedded_spatial_images(spatial_data, "")
  }
  external <- configured_spatial_images(options, dataset, spatial_name, "")
  choices <- spatial_background_choices(embedded, external)
  if (!length(choices)) {
    return(NULL)
  }
  index <- if (is.null(pick) || pick > length(choices)) 1L else pick
  descriptor <- resolve_spatial_background(
    unname(choices[[index]]),
    embedded,
    external
  )
  if (is.null(descriptor)) {
    return(NULL)
  }
  list(
    descriptor = descriptor,
    identity = spatial_background_identity(dataset, spatial_name, descriptor),
    preset = spatial_background_preset(
      options,
      dataset,
      spatial_name,
      descriptor
    ),
    image_allowlist = vapply(external, `[[`, character(1), "path")
  )
}

## The image fields of one panel from its configured background (see
## spatial_grid_section_background()), placed over `coordinates`. An empty
## list draws the panel without an image.
spatial_panel_background <- function(configured, coordinates, cerebro_root) {
  if (is.null(configured) || is.null(configured$descriptor)) {
    return(list())
  }
  rendered <- spatial_background_render_payload(
    configured$descriptor,
    configured$image_allowlist,
    configured$identity,
    configured$preset,
    coordinates,
    cerebro_root
  )
  if (is.null(rendered$background_image)) {
    return(list())
  }
  list(
    background_image = rendered$background_image,
    image_bounds = rendered$image_bounds,
    image_identity = rendered$background_identity,
    image_label = configured$descriptor$label,
    image_preset = configured$preset
  )
}

## One panel per section, drawing its slice of the frame. `backgrounds` is
## named by section; a section without one has no image fields.
spatial_grid_panels <- function(
  layout,
  backgrounds = list(),
  cerebro_root = NULL
) {
  lapply(seq_along(layout$sections), function(i) {
    section <- layout$sections[[i]]
    rows <- layout$starts[[i]] + seq_len(layout$counts[[i]])
    image <- spatial_panel_background(
      backgrounds[[section]],
      layout$coordinates[rows, , drop = FALSE],
      cerebro_root
    )
    c(
      list(
        id = paste0("section-", i),
        label = section,
        section = section,
        start = as.integer(layout$starts[[i]]),
        count = as.integer(layout$counts[[i]]),
        spatial = TRUE,
        preserve_aspect = TRUE,
        x_range = layout$x_ranges[[i]],
        y_range = layout$y_ranges[[i]]
      ),
      if (!is.null(layout$geometry[[i]])) {
        list(geometry_resource = layout$geometry[[i]])
      },
      image
    )
  })
}

## The embedding panel of a grid (a list of one panel, or none): its own x
## and y for every row of the frame, so a selection in it is a selection of
## the cells in every section, and the reverse.
spatial_grid_projection_panel <- function(projection) {
  if (
    !is.list(projection) ||
      !length(projection$x) ||
      length(projection$x) != length(projection$y)
  ) {
    return(list())
  }
  list(list(
    id = "projection",
    label = projection$label,
    projection = TRUE,
    x = as.numeric(projection$x),
    y = as.numeric(projection$y),
    x_range = spatial_grid_range(projection$x),
    y_range = spatial_grid_range(projection$y)
  ))
}

## The colour range shared by all panels: the finite range of the frame's
## values over every section, so one value reads as one colour in each.
spatial_grid_color_range <- function(values) {
  if (!is.numeric(values)) {
    return(NULL)
  }
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(NULL)
  }
  range(values)
}

## The metadata columns a frame reads: the colour column of a group plot and
## the split column.
spatial_projection_metadata_columns <- function(parameters) {
  color_variable <- if (identical(parameters[["plot_type"]], "ImageDimPlot")) {
    parameters[["color_variable"]]
  } else {
    character()
  }
  unique(Filter(nzchar, c(color_variable, parameters$split_by)))
}

## The metadata of a grid's cells: those columns and the cells' canonical
## rows, without row names. Subsetting the data frame named every one of the
## 3.9 million cells of the Allen atlas (0.4 s).
spatial_grid_metadata <- function(full_metadata, cells, columns) {
  values <- lapply(columns, function(column) full_metadata[[column]][cells])
  names(values) <- columns
  values$cell_index <- cells
  structure(
    values,
    class = "data.frame",
    row.names = c(NA_integer_, -length(cells))
  )
}

## A grid frame of many cells streams (cell_views.js, onGridRows()): the first
## message holds every panel and the cells' names, but the positions and
## colours of the first sections shown only; the others follow a batch of
## sections at a time, so the first panels draw while the rest are on their
## way. All 59 sections of the Allen atlas (3.9 million cells) drew only after
## their 51 MB frame had arrived and been read, about 5 s.
spatial_grid_stream_min <- 4e5
## The cells of the first batch (a few sections, drawn about 1.5 s sooner than
## the whole grid) and of every other.
spatial_grid_stream_first <- 1.5e5
spatial_grid_stream_step <- 1e6

## The batches of a streaming grid: lists of indices into `counts` (the cells
## of each section panel, in frame order). `order` names the panels in the
## order they show; the others follow in frame order. The first batch ends at
## `first` cells, every other at `step`; a batch is at least one section.
spatial_grid_stream_batches <- function(
  counts,
  order = seq_along(counts),
  first = spatial_grid_stream_first,
  step = spatial_grid_stream_step
) {
  panels <- seq_along(counts)
  order <- unique(as.integer(order))
  order <- c(order[order %in% panels], setdiff(panels, order))
  batches <- list()
  current <- integer()
  total <- 0
  limit <- first
  for (i in order) {
    current <- c(current, i)
    total <- total + counts[[i]]
    if (total >= limit) {
      batches[[length(batches) + 1L]] <- current
      current <- integer()
      total <- 0
      limit <- step
    }
  }
  if (length(current)) {
    batches[[length(batches) + 1L]] <- current
  }
  batches
}

## The frame rows (1-based) of the section panels `batch`, given each panel's
## 0-based `starts` and `counts`.
spatial_grid_stream_rows <- function(batch, starts, counts) {
  as.integer(unlist(
    lapply(batch, function(i) starts[[i]] + seq_len(counts[[i]])),
    use.names = FALSE
  ))
}

## Splits the data of a grid frame (`data`, flat x, y and colours over the
## frame's rows, one panel per section) for streaming: the first batch as the
## head, with `grid_stream` naming its sections' rows, and part(k) the
## positions and colours of batch k. `shown` names the sections in the order
## they show. NULL for a frame too small to stream, or one whose colours do not
## travel per cell (RGB, sparse or packed colours).
spatial_grid_stream_split <- function(
  data,
  shown = character(),
  min_cells = spatial_grid_stream_min,
  ...
) {
  n <- length(data$x)
  panels <- data$panels
  if (
    n < min_cells ||
      !is.list(panels) ||
      length(data$y) != n ||
      !is.null(data$rgb) ||
      !is.null(data$sparse_color) ||
      !is.null(data$packed_colors)
  ) {
    return(NULL)
  }
  section_at <- which(vapply(
    panels,
    function(panel) !is.null(panel$section) && !is.null(panel$start),
    logical(1)
  ))
  if (length(section_at) < 2L) {
    return(NULL)
  }
  sections <- vapply(
    panels[section_at],
    function(panel) as.character(panel$section),
    ""
  )
  starts <- vapply(
    panels[section_at],
    function(panel) as.integer(panel$start),
    0L
  )
  counts <- vapply(
    panels[section_at],
    function(panel) as.integer(panel$count),
    0L
  )
  batches <- spatial_grid_stream_batches(counts, match(shown, sections), ...)
  ## Sections whose positions come from a Viewer Pack file carry none here,
  ## and a frame with such sections always streams: its first message names
  ## the files.
  from_file <- vapply(
    panels[section_at],
    function(panel) is.list(panel$geometry_resource),
    logical(1)
  )
  if (length(batches) < 2L && !any(from_file)) {
    return(NULL)
  }
  per_cell <- Filter(
    function(field) length(data[[field]]) == n,
    intersect(c("x", "y", "group_code", "color"), names(data))
  )
  projection_at <- which(vapply(
    panels,
    function(panel) isTRUE(panel$projection) && length(panel$x) == n,
    logical(1)
  ))
  rows_of <- function(k) spatial_grid_stream_rows(batches[[k]], starts, counts)
  part <- function(k) {
    batch <- batches[[k]]
    rows <- rows_of(k)
    out <- list(
      section = sections[batch],
      start = starts[batch],
      count = counts[batch]
    )
    positioned <- batch[!from_file[batch]]
    xy_rows <- spatial_grid_stream_rows(positioned, starts, counts)
    for (field in per_cell) {
      out[[field]] <- unclass(data[[field]])[
        if (field %in% c("x", "y")) xy_rows else rows
      ]
    }
    if (length(projection_at)) {
      projection <- panels[[projection_at[[1L]]]]
      out$projection <- list(x = projection$x[rows], y = projection$y[rows])
    }
    out
  }
  first <- part(1L)
  head <- data
  for (field in per_cell) {
    head[[field]] <- first[[field]]
  }
  if (length(projection_at)) {
    head$panels[[projection_at[[1L]]]]$x <- first$projection$x
    head$panels[[projection_at[[1L]]]]$y <- first$projection$y
  }
  head$grid_stream <- list(
    rows = n,
    section = I(first$section),
    start = I(first$start),
    count = I(first$count),
    batches = length(batches)
  )
  list(head = head, batches = length(batches), part = part, rows_of = rows_of)
}

## Canonical rows (0-based) as the runs of consecutive rows they are, when
## there are few: a section whose cells are one block of the data set travels
## as its start and length instead of a number per cell.
spatial_grid_row_runs <- function(rows) {
  rows <- as.integer(rows)
  n <- length(rows)
  if (n < 2L || anyNA(rows)) {
    return(rows)
  }
  breaks <- which(diff(rows) != 1L)
  if (length(breaks) + 1L > n %/% 8L) {
    return(rows)
  }
  starts <- c(1L, breaks + 1L)
  ends <- c(breaks, n)
  list(
    kind = "ranges",
    start = I(rows[starts]),
    length = I(ends - starts + 1L)
  )
}

## The cells' keys of a streaming grid travel with their batches as well: the
## first message names the cells of the first batch only. `aux` is the frame's
## auxiliary data (cerebroCellViewDeferredAux()): canonical rows with a
## cell-name resource, else the names themselves. Every 4 bytes a cell of the
## Allen atlas were 16 MB in its first message.
spatial_grid_stream_keys <- function(stream, aux) {
  index <- aux$selection_index
  if (is.list(index) && identical(index$kind, "range")) {
    index <- index$start + seq_len(index$length) - 1L
  }
  index <- if (!is.null(index)) as.integer(unclass(index))
  keys <- if (!is.null(aux$selection_key)) unclass(aux$selection_key)
  slice <- function(rows) {
    out <- list()
    if (!is.null(index)) {
      out$selection_index <- spatial_grid_row_runs(index[rows])
    }
    if (!is.null(keys)) {
      out$selection_key <- keys[rows]
    }
    out
  }
  head_aux <- aux
  head_aux$selection_index <- NULL
  head_aux$selection_key <- NULL
  stream$head_aux <- c(head_aux, slice(stream$rows_of(1L)))
  part <- stream$part
  stream$part <- function(k) c(part(k), slice(stream$rows_of(k)))
  stream
}
