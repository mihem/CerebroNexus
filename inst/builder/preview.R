##----------------------------------------------------------------------------##
## Show the projection before it is written.
##
## Cheap insurance: an object can pass every structural check and still be the
## wrong one, or be coloured by a variable that turns out to be a batch label.
## One look answers that before a file exists.
##
## Pure: object in, bounded preview data out. No Shiny.
##----------------------------------------------------------------------------##

## Above this many cells the plot stops being informative and starts being slow.
BUILDER_PREVIEW_MAX <- 1000L
BUILDER_SPATIAL_PREVIEW_MAX <- 5000L

.builder_alignment_sample <- function(rows, max_cells) {
  max_cells <- suppressWarnings(as.integer(max_cells %||% BUILDER_PREVIEW_MAX))
  if (is.na(max_cells) || max_cells < 1L) {
    max_cells <- BUILDER_PREVIEW_MAX
  }
  if (rows <= max_cells) {
    return(seq_len(rows))
  }
  set.seed(42L)
  sort(sample.int(rows, max_cells))
}

.builder_alignment_bounds <- function(frame) {
  if (is.null(frame) || !nrow(frame)) {
    return(NULL)
  }
  xr <- range(frame$x, finite = TRUE)
  yr <- range(frame$y, finite = TRUE)
  if (length(xr) != 2L || length(yr) != 2L || !all(is.finite(c(xr, yr)))) {
    return(NULL)
  }
  list(xmin = xr[[1L]], xmax = xr[[2L]], ymin = yr[[1L]], ymax = yr[[2L]])
}

.builder_alignment_unavailable <- function(sections = list(), message) {
  list(
    available = FALSE,
    message = message,
    sections = sections,
    section = NULL,
    projection_name = NULL,
    transcriptome = NULL,
    spatial = NULL,
    bounds = NULL,
    coordinate_frame = NULL,
    roi_bounds = list(),
    total_cells = 0L,
    capped = FALSE
  )
}

#' Resolve the selected layer's cell cohort without reading expression values.
#'
#' Spatial alignment only needs cell identities. Materialising a logical split
#' layer with JoinLayers can be orders of magnitude more expensive than the
#' preview itself, so reproduce the resolver's partition proof using layer
#' membership metadata and never request LayerData for an Assay5 object.
builder_alignment_layer_cells <- function(object, assay, layer) {
  if (
    !is.character(assay) ||
      length(assay) != 1L ||
      is.na(assay) ||
      !nzchar(assay) ||
      !assay %in% names(object@assays)
  ) {
    stop("The selected assay is not available.", call. = FALSE)
  }
  if (
    !is.character(layer) ||
      length(layer) != 1L ||
      is.na(layer) ||
      !nzchar(layer)
  ) {
    stop("The selected layer is not available.", call. = FALSE)
  }

  assay_object <- object[[assay]]
  if (!inherits(assay_object, "Assay5")) {
    if (!layer %in% methods::slotNames(assay_object)) {
      stop("The selected layer is not available.", call. = FALSE)
    }
    cells <- colnames(methods::slot(assay_object, layer))
    if (!length(cells)) {
      stop("The selected layer does not contain cells.", call. = FALSE)
    }
    return(as.character(cells))
  }

  layer_names <- SeuratObject::Layers(assay_object)
  if (layer %in% layer_names) {
    cells <- SeuratObject::Cells(assay_object, layer = layer)
    if (!length(cells)) {
      stop("The selected layer does not contain cells.", call. = FALSE)
    }
    return(as.character(cells))
  }

  protected_candidates <- .layer_prefix_candidates(assay_object, layer)
  candidates <- .layer_partition_candidates(protected_candidates, layer)
  if (!length(candidates)) {
    stop("The selected layer is not available.", call. = FALSE)
  }
  memberships <- lapply(candidates, function(candidate) {
    SeuratObject::Cells(assay_object, layer = candidate)
  })
  names(memberships) <- candidates
  assay_cells <- SeuratObject::Cells(assay_object)
  direct_candidates <- .direct_layer_partition_candidates(candidates, layer)
  direct_partition <- .find_layer_partition(
    assay_cells,
    memberships[direct_candidates]
  )
  partition <- if (identical(direct_partition$status, "unique")) {
    direct_partition
  } else {
    .find_layer_partition(assay_cells, memberships)
  }
  if (identical(partition$status, "ambiguous")) {
    stop(
      "The selected layer has more than one valid cell partition.",
      call. = FALSE
    )
  }
  nested_root <- if (identical(direct_partition$status, "unique")) {
    NULL
  } else if (identical(partition$status, "unique")) {
    .nested_partition_root(layer, partition$layers)
  } else {
    NULL
  }
  if (!is.null(nested_root)) {
    stop(
      "The selected layer has an ambiguous nested cell partition.",
      call. = FALSE
    )
  }
  if (!identical(partition$status, "unique")) {
    stop(
      "The selected split layers do not form one complete cell partition.",
      call. = FALSE
    )
  }
  as.character(assay_cells)
}

#' A bounded pair of transcriptome and physical coordinates for alignment.
#'
#' Both frames are sampled with one shared index after joining by canonical
#' cell barcode. That makes linked selection exact and prevents row-order drift.
#' Full-data physical bounds are retained for deterministic image fitting.
#' All-ROI previews stay raw for live section transforms in the browser; ROI
#' previews receive the section transform first so the browser only applies the
#' ROI-local transform, matching the built Viewer.
builder_alignment_preview_model <- function(
  object,
  default_projection = NULL,
  group = NULL,
  roi = NULL,
  section_id = NULL,
  assay = NULL,
  layer = "data",
  base_coordinate_transform = NULL,
  max_cells = BUILDER_PREVIEW_MAX
) {
  roi <- as.character(roi %||% "")
  if (length(roi) != 1L || is.na(roi)) {
    roi <- ""
  }
  sections <- builder_spatial_alignment_sections(object)
  if (!length(sections)) {
    return(.builder_alignment_unavailable(
      sections,
      "Spatial or Trekker coordinates are not available for this dataset."
    ))
  }
  section_ids <- vapply(sections, `[[`, character(1), "id")
  if (is.null(section_id) || !section_id %in% section_ids) {
    section_id <- section_ids[[1L]]
  }
  section <- sections[[match(section_id, section_ids)]]

  if (identical(section$kind, "trekker")) {
    payload <- tryCatch(object@misc$trekker, error = function(error) NULL)
    required <- c("barcodes", "x", "y", "ux", "uy")
    lengths <- vapply(
      required,
      function(name) {
        length(payload[[name]] %||% NULL)
      },
      integer(1)
    )
    valid <- is.list(payload) &&
      length(unique(lengths)) == 1L &&
      lengths[[1L]] > 0L &&
      !anyNA(payload$barcodes) &&
      all(nzchar(as.character(payload$barcodes))) &&
      all(vapply(
        c("x", "y", "ux", "uy"),
        function(name) {
          values <- payload[[name]]
          is.numeric(values) &&
            !is.object(values) &&
            !anyNA(values) &&
            all(is.finite(values))
        },
        logical(1)
      ))
    if (!valid) {
      return(.builder_alignment_unavailable(
        sections,
        "Trekker does not contain a valid paired physical projection."
      ))
    }
    groups <- as.character(payload$clusters %||% rep("cells", lengths[[1L]]))
    if (length(groups) != lengths[[1L]]) {
      groups <- rep("cells", lengths[[1L]])
    }
    ## Trekker carries its own cluster vector for backwards compatibility, but
    ## alignment colours follow the Viewer Group selected in Core whenever
    ## that metadata is available for the mapped cells.
    if (!is.null(group) && group %in% colnames(object@meta.data)) {
      metadata_rows <- match(
        as.character(payload$barcodes),
        rownames(object@meta.data)
      )
      if (!anyNA(metadata_rows)) {
        groups <- as.character(object@meta.data[[group]][metadata_rows])
      }
    }
    groups[is.na(groups) | !nzchar(groups)] <- "N/A"
    transcriptome_full <- data.frame(
      cell_barcode = as.character(payload$barcodes),
      x = as.numeric(payload$ux),
      y = as.numeric(payload$uy),
      group = groups,
      stringsAsFactors = FALSE
    )
    spatial_full <- data.frame(
      cell_barcode = as.character(payload$barcodes),
      x = as.numeric(payload$x),
      y = as.numeric(payload$y),
      group = groups,
      stringsAsFactors = FALSE
    )
    keep <- .builder_alignment_sample(nrow(spatial_full), max_cells)
    return(list(
      available = TRUE,
      message = NULL,
      sections = sections,
      section = section,
      projection_name = "Trekker UMAP",
      transcriptome = transcriptome_full[keep, , drop = FALSE],
      spatial = spatial_full[keep, , drop = FALSE],
      bounds = .builder_alignment_bounds(spatial_full),
      coordinate_frame = .builder_alignment_bounds(spatial_full),
      roi_bounds = list(),
      roi = list(field = NULL, values = character(), selected = ""),
      total_cells = nrow(spatial_full),
      capped = nrow(spatial_full) > length(keep)
    ))
  }

  reduction <- builder_alignment_projection(
    setdiff(
      tryCatch(names(object@reductions), error = function(error) character()),
      if (identical(section$kind, "spatial_reduction")) {
        section$source_id
      } else {
        character()
      }
    ),
    default_projection
  )
  if (is.null(reduction)) {
    return(.builder_alignment_unavailable(
      sections,
      "No UMAP, current projection, or PCA is available for transcriptome space."
    ))
  }
  transcriptome_full <- builder_preview_frame(
    object,
    reduction,
    group,
    max_cells = .Machine$integer.max
  )
  assay <- assay %||%
    tryCatch(
      SeuratObject::DefaultAssay(object),
      error = function(error) NULL
    )
  expression_cohort <- tryCatch(
    list(cells = builder_alignment_layer_cells(object, assay, layer)),
    error = function(error) list(error = conditionMessage(error))
  )
  expression_cells <- expression_cohort$cells
  if (
    is.null(expression_cells) ||
      !is.character(expression_cells) ||
      !length(expression_cells) ||
      anyNA(expression_cells) ||
      any(!nzchar(expression_cells))
  ) {
    return(.builder_alignment_unavailable(
      sections,
      expression_cohort$error %||%
        "The selected assay and layer do not provide a safe expression cohort."
    ))
  }
  physical <- if (identical(section$kind, "spatial_reduction")) {
    tryCatch(
      builder_preview_frame(
        object,
        section$source_id,
        max_cells = .Machine$integer.max
      ),
      error = function(error) NULL
    )
  } else {
    tryCatch(
      builder_spatial_contract(
        object,
        cells = expression_cells,
        image = section$source_id
      )$coordinates,
      error = function(error) NULL
    )
  }
  if (!is.null(physical)) {
    physical <- physical[
      physical$cell_barcode %in% expression_cells,
      c("cell_barcode", "x", "y"),
      drop = FALSE
    ]
    if (nrow(physical) && !is.null(base_coordinate_transform)) {
      physical <- .spx_apply_coordinate_transform(
        physical,
        base_coordinate_transform
      )
    }
  }
  if (is.null(transcriptome_full) || is.null(physical) || !nrow(physical)) {
    return(.builder_alignment_unavailable(
      sections,
      "The selected section has no safe paired spatial coordinates."
    ))
  }
  common <- intersect(
    transcriptome_full$cell_barcode,
    physical$cell_barcode
  )
  if (!length(common)) {
    return(.builder_alignment_unavailable(
      sections,
      "The transcriptome and spatial views share no cell identities."
    ))
  }
  roi_field <- section$annotations$roi$field %||% NULL
  roi_values <- character()
  roi_bounds <- list()
  roi_by_cell <- stats::setNames(
    rep(NA_character_, length(common)),
    common
  )
  if (!is.null(roi_field) && roi_field %in% colnames(object@meta.data)) {
    rows <- match(common, rownames(object@meta.data))
    roi_by_cell <- as.character(object@meta.data[[roi_field]][rows])
    roi_by_cell[is.na(roi_by_cell) | !nzchar(roi_by_cell)] <- NA_character_
    names(roi_by_cell) <- common
    roi_values <- unique(unname(roi_by_cell[!is.na(roi_by_cell)]))
    roi_bounds <- stats::setNames(
      lapply(roi_values, function(value) {
        cells <- names(roi_by_cell)[
          !is.na(roi_by_cell) & roi_by_cell == value
        ]
        .builder_alignment_bounds(physical[
          match(cells, physical$cell_barcode),
          c("x", "y"),
          drop = FALSE
        ])
      }),
      roi_values
    )
    if (nzchar(roi)) {
      if (!roi %in% roi_values) {
        return(.builder_alignment_unavailable(
          sections,
          paste0("ROI `", roi, "` is not available in this spatial section.")
        ))
      }
      common <- common[!is.na(roi_by_cell[common]) & roi_by_cell[common] == roi]
    }
  } else {
    roi <- ""
  }
  transcriptome_full <- transcriptome_full[
    match(common, transcriptome_full$cell_barcode),
    c("cell_barcode", "x", "y", "group"),
    drop = FALSE
  ]
  spatial_full <- physical[
    match(common, physical$cell_barcode),
    c("cell_barcode", "x", "y"),
    drop = FALSE
  ]
  spatial_full$group <- transcriptome_full$group
  spatial_full$roi <- unname(roi_by_cell[common])
  spatial_full$roi[is.na(spatial_full$roi)] <- ""
  coordinate_frame <- .builder_alignment_bounds(spatial_full)
  keep <- .builder_alignment_sample(nrow(spatial_full), max_cells)
  list(
    available = TRUE,
    message = NULL,
    sections = sections,
    section = section,
    projection_name = reduction,
    transcriptome = transcriptome_full[keep, , drop = FALSE],
    spatial = spatial_full[keep, , drop = FALSE],
    bounds = .builder_alignment_bounds(spatial_full),
    coordinate_frame = coordinate_frame,
    roi_bounds = roi_bounds,
    coordinate_transform = NULL,
    roi = list(field = roi_field, values = roi_values, selected = roi),
    total_cells = nrow(spatial_full),
    capped = nrow(spatial_full) > length(keep)
  )
}

builder_spatial_canvas_cache_sources <- function(scene, known = character()) {
  known <- unique(as.character(known))
  cache <- function(image) {
    if (is.null(image) || is.null(image$uri) || is.null(image$sourceKey)) {
      return(image)
    }
    key <- as.character(image$sourceKey)[[1L]]
    if (key %in% known) {
      image$uri <- NULL
    } else {
      known <<- c(known, key)
    }
    image
  }
  scene$image <- cache(scene$image)
  scene$roiImages <- lapply(
    scene$roiImages,
    function(images) lapply(images, cache)
  )
  list(scene = scene, known = known)
}

builder_spatial_canvas_scene <- function(
  preview,
  colors,
  record = NULL,
  point_appearance = NULL,
  coordinate_transform = NULL,
  roi_point_appearance = list(),
  roi_coordinate_transforms = list(),
  roi_images = list(),
  layout = "overlay",
  active_roi = NULL,
  identity,
  generation,
  reset_token = 0L,
  cell_percentage = 100,
  dataset = NULL,
  snapshot_identity = NULL,
  section = NULL
) {
  if (!isTRUE(preview$available)) {
    return(list(
      available = FALSE,
      message = preview$message %||% "Spatial preview is unavailable.",
      viewKey = identity,
      dataset = dataset,
      snapshotIdentity = snapshot_identity,
      section = section,
      generation = generation,
      resetToken = reset_token
    ))
  }
  points <- preview$spatial
  levels <- unique(as.character(points$group))
  palette <- builder_level_colors(levels)
  shared <- intersect(levels, names(colors %||% character()))
  palette[shared] <- colors[shared]
  counts <- table(points$group)
  roi_images <- lapply(roi_images, function(records) {
    records <- records %||% list()
    Filter(
      Negate(is.null),
      lapply(names(records), function(key) {
        image <- builder_alignment_normalize(records[[key]])
        if (is.null(image)) {
          return(NULL)
        }
        list(
          id = key,
          sourceKey = image$source_content_md5 %||%
            paste0(identity, "::roi::", key),
          uri = image$source_uri %||% image$uri,
          baseBounds = image$base_bounds,
          controls = .builder_alignment_parameters(image),
          active = isTRUE(records[[key]]$active)
        )
      })
    )
  })
  list(
    available = TRUE,
    viewKey = identity,
    dataset = dataset,
    snapshotIdentity = snapshot_identity,
    section = section,
    generation = generation,
    resetToken = reset_token,
    capped = isTRUE(preview$capped),
    layout = layout,
    activeRoi = active_roi,
    roiBounds = preview$roi_bounds %||% list(),
    roiPointAppearance = roi_point_appearance,
    roiCoordinateTransforms = roi_coordinate_transforms,
    roiImages = roi_images,
    points = list(
      x = unname(as.numeric(points$x)),
      y = unname(as.numeric(points$y)),
      barcode = as.character(points$cell_barcode),
      group = as.character(points$group),
      roi = as.character(points$roi %||% rep("", nrow(points))),
      color = unname(palette[as.character(points$group)]),
      count = unname(as.integer(counts[as.character(points$group)]))
    ),
    bounds = preview$coordinate_frame %||% preview$bounds,
    image = if (is.null(record)) {
      NULL
    } else {
      list(
        sourceKey = record$source_content_md5 %||%
          paste0(identity, "::active"),
        uri = record$source_uri %||% record$uri,
        baseBounds = record$base_bounds
      )
    },
    controls = utils::modifyList(
      list(
        coordinateRotation = coordinate_transform$rotation_degrees %||% 0,
        coordinateScale = coordinate_transform$scale %||% 1,
        cell_percentage = as.numeric(cell_percentage)
      ),
      as.list(
        if (is.null(record)) {
          utils::modifyList(
            builder_alignment_defaults(),
            point_appearance %||% list()
          )
        } else {
          .builder_alignment_parameters(record)
        }
      )
    )
  )
}

#' The points to draw: three short columns, whatever the object's size.
#'
#' This runs in the worker process, so what crosses back to the page is a
#' data.frame of at most `max_cells` rows rather than anything holding a
#' reference to the object.
#'
#' @return A data.frame with x, y, group -- or `NULL` when there is nothing
#'   to draw.
builder_preview_frame <- function(
  object,
  reduction,
  group = NULL,
  max_cells = BUILDER_PREVIEW_MAX
) {
  if (is.null(reduction) || !nzchar(reduction)) {
    return(NULL)
  }
  if (!reduction %in% names(object@reductions)) {
    return(NULL)
  }
  emb <- SeuratObject::Embeddings(object[[reduction]])
  if (is.null(emb) || ncol(emb) < 2) {
    return(NULL)
  }
  cells <- SeuratObject::Cells(object)
  embedding_match <- builder_match_cells(
    rownames(emb),
    cells,
    mode = "exact"
  )
  metadata_match <- builder_match_cells(
    rownames(object@meta.data),
    cells,
    mode = "exact"
  )
  assert_preview_identity <- function(match, component) {
    if (isTRUE(match$valid)) {
      return(invisible(match))
    }
    if (length(match$duplicates) || length(match$expected_duplicates)) {
      stop(
        component,
        " preview identity contains duplicate cell barcodes.",
        call. = FALSE
      )
    }
    if (length(match$blanks) || length(match$expected_blanks)) {
      stop(
        component,
        " preview identity contains blank cell barcodes.",
        call. = FALSE
      )
    }
    stop(
      component,
      " preview identity does not match the dataset.",
      call. = FALSE
    )
  }
  assert_preview_identity(embedding_match, "Projection")
  assert_preview_identity(metadata_match, "Metadata")
  emb <- emb[embedding_match$reorder_index, , drop = FALSE]
  metadata <- object@meta.data[
    metadata_match$reorder_index,
    ,
    drop = FALSE
  ]

  df <- data.frame(
    cell_barcode = cells,
    x = as.numeric(emb[, 1]),
    y = as.numeric(emb[, 2]),
    stringsAsFactors = FALSE
  )
  if (!is.null(group) && group %in% colnames(metadata)) {
    df$group <- as.character(metadata[[group]])
    df$group[is.na(df$group)] <- "N/A"
  } else {
    df$group <- "cells"
  }

  ## Deterministic downsampling: the same object always previews the same way,
  ## so a difference on screen means a difference in the data.
  if (nrow(df) > max_cells) {
    set.seed(42L)
    df <- df[sort(sample.int(nrow(df), max_cells)), , drop = FALSE]
  }
  attr(df, "reduction") <- reduction
  attr(df, "capped") <- nrow(emb) > max_cells
  df
}

#' Build every projection thumbnail in one bounded worker pass.
#'
#' Only the existing two-dimensional embeddings and one metadata column are
#' touched. Expression assays are deliberately outside this contract.
builder_projection_preview_catalog <- function(
  object,
  projections,
  group = NULL,
  max_cells = BUILDER_PREVIEW_MAX
) {
  projections <- unique(as.character(projections %||% character()))
  projections <- projections[
    !is.na(projections) &
      nzchar(projections) &
      projections %in% names(object@reductions)
  ]
  if (!length(projections)) {
    return(list())
  }
  max_cells <- suppressWarnings(as.integer(max_cells))
  if (is.na(max_cells) || max_cells < 1L) {
    max_cells <- BUILDER_PREVIEW_MAX
  }
  previews <- lapply(projections, function(projection) {
    builder_preview_frame(
      object,
      reduction = projection,
      group = group,
      max_cells = max_cells
    )
  })
  names(previews) <- projections
  Filter(Negate(is.null), previews)
}

.builder_trajectory_preview_key <- function(method, name) {
  paste0(method, "::", name)
}

#' Build bounded trajectory thumbnails without consulting expression assays.
builder_trajectory_preview_catalog <- function(
  object,
  trajectories,
  max_cells = BUILDER_PREVIEW_MAX,
  max_edges = 250L
) {
  if (!is.list(trajectories) || !length(trajectories)) {
    return(list())
  }
  source <- tryCatch(object@misc$trajectories, error = function(error) NULL)
  if (!is.list(source) || !length(source)) {
    return(list())
  }
  max_cells <- suppressWarnings(as.integer(max_cells))
  max_edges <- suppressWarnings(as.integer(max_edges))
  if (is.na(max_cells) || max_cells < 1L) {
    max_cells <- BUILDER_PREVIEW_MAX
  }
  if (is.na(max_edges) || max_edges < 1L) {
    max_edges <- 250L
  }

  previews <- list()
  for (method in intersect(names(trajectories), names(source))) {
    names_for_method <- unique(as.character(
      trajectories[[method]] %||% character()
    ))
    available <- source[[method]]
    if (!is.list(available)) {
      next
    }
    for (name in intersect(names_for_method, names(available))) {
      payload <- available[[name]]
      if (!is.list(payload)) {
        next
      }
      meta <- payload$meta
      edges <- payload$edges
      if (
        !is.data.frame(meta) ||
          !all(c("DR_1", "DR_2", "pseudotime", "state") %in% names(meta))
      ) {
        next
      }
      x <- suppressWarnings(as.numeric(meta$DR_1))
      y <- suppressWarnings(as.numeric(meta$DR_2))
      pseudotime <- suppressWarnings(as.numeric(meta$pseudotime))
      cells <- rownames(meta)
      valid <- is.finite(x) &
        is.finite(y) &
        is.finite(pseudotime) &
        !is.na(cells) &
        nzchar(cells)
      if (!any(valid)) {
        next
      }
      points <- data.frame(
        cell_barcode = cells[valid],
        x = x[valid],
        y = y[valid],
        group = as.character(meta$state[valid]),
        pseudotime = pseudotime[valid],
        stringsAsFactors = FALSE
      )
      points$group[is.na(points$group) | !nzchar(points$group)] <- "N/A"
      keep <- .builder_alignment_sample(nrow(points), max_cells)
      points <- points[keep, , drop = FALSE]

      edge_frame <- data.frame(
        x = numeric(),
        y = numeric(),
        xend = numeric(),
        yend = numeric()
      )
      edge_columns <- c(
        "source_dim_1",
        "source_dim_2",
        "target_dim_1",
        "target_dim_2"
      )
      if (is.data.frame(edges) && all(edge_columns %in% names(edges))) {
        edge_frame <- data.frame(
          x = suppressWarnings(as.numeric(edges$source_dim_1)),
          y = suppressWarnings(as.numeric(edges$source_dim_2)),
          xend = suppressWarnings(as.numeric(edges$target_dim_1)),
          yend = suppressWarnings(as.numeric(edges$target_dim_2))
        )
        edge_frame <- edge_frame[
          apply(is.finite(as.matrix(edge_frame)), 1L, all),
          ,
          drop = FALSE
        ]
        if (nrow(edge_frame) > max_edges) {
          set.seed(43L)
          edge_frame <- edge_frame[
            sort(sample.int(nrow(edge_frame), max_edges)),
            ,
            drop = FALSE
          ]
        }
      }
      previews[[.builder_trajectory_preview_key(method, name)]] <- list(
        points = points,
        edges = edge_frame
      )
    }
  }
  previews
}

#' The viewer's own qualitative palette, so the preview is not a different
#' picture from the one the data set will produce.
builder_preview_palette <- function(n) {
  base <- c(
    "#4C78A8",
    "#E07B39",
    "#59A14F",
    "#B279A2",
    "#76B7B2",
    "#E15759",
    "#9C755F",
    "#D6B84C",
    "#6F6F78",
    "#86A873",
    "#5B8E9E",
    "#C26D8A",
    "#7A6FA8",
    "#B88A44",
    "#4F8A76",
    "#A8644A",
    "#7896C1",
    "#8C7B6B",
    "#6E9E9A",
    "#A77B9D"
  )
  n <- max(1L, as.integer(n))
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  grDevices::colorRampPalette(base)(n)
}


#' One bounded legend shared by both alignment plots.
builder_alignment_legend_ui <- function(frame, colors = NULL) {
  if (is.null(frame) || !nrow(frame)) {
    return(NULL)
  }
  counts <- sort(table(as.character(frame$group)), decreasing = TRUE)
  levels <- names(counts)
  palette <- builder_level_colors(levels)
  if (length(colors)) {
    shared <- intersect(levels, names(colors))
    palette[shared] <- colors[shared]
  }
  htmltools::tags$div(
    class = "spatial-alignment-legend",
    role = "list",
    lapply(levels, function(level) {
      htmltools::tags$span(
        class = "spatial-alignment-legend-item",
        role = "listitem",
        htmltools::tags$i(
          class = "spatial-alignment-legend-swatch",
          style = paste0("background:", palette[[level]])
        ),
        htmltools::tags$span(level),
        htmltools::tags$small(format(unname(counts[[level]]), big.mark = ","))
      )
    })
  )
}

## ---------------------------------------------------------------------------
## Palettes
## ---------------------------------------------------------------------------

#' The palettes on offer, as functions of the number of levels.
#'
#' `cerebro` is the viewer's own, so "leave it alone" is the default and the
#' preview matches what the app will draw. The rest exist because a lab usually
#' has a convention to match, and because the viewer's palette is tuned for
#' distinctness rather than for being readable by someone with a colour vision
#' deficiency.
builder_palettes <- function() {
  list(
    list(
      id = "cerebro",
      label = "Cerebro default",
      note = "The viewer's built-in palette",
      colors = function(n) builder_preview_palette(n)
    ),
    list(
      id = "okabe_ito",
      label = "Okabe–Ito (colour-vision friendly)",
      note = "Eight colours designed for red–green colour-vision deficiency; interpolated beyond eight groups",
      colors = function(n) {
        base <- c(
          "#E69F00",
          "#56B4E9",
          "#009E73",
          "#F0E442",
          "#0072B2",
          "#D55E00",
          "#CC79A7",
          "#000000"
        )
        builder_ramp(base, n)
      }
    ),
    list(
      id = "tol_bright",
      label = "Paul Tol Bright (colour-vision friendly)",
      note = "Seven colours that remain distinct in print and projection",
      colors = function(n) {
        base <- c(
          "#4477AA",
          "#EE6677",
          "#228833",
          "#CCBB44",
          "#66CCEE",
          "#AA3377",
          "#BBBBBB"
        )
        builder_ramp(base, n)
      }
    ),
    list(
      id = "grey",
      label = "Greyscale",
      note = "Emphasises density without a categorical colour rhythm",
      colors = function(n) {
        grDevices::colorRampPalette(c("#1c1c1e", "#c8c8cc"))(max(1L, n))
      }
    )
  )
}

#' Take the first n of a base palette, interpolating only when it runs out.
#'
#' Interpolating a colour-blind-safe palette does not stay colour-blind-safe, so
#' the interface says how many colours each one really has.
builder_ramp <- function(base, n) {
  n <- max(1L, as.integer(n))
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }
  grDevices::colorRampPalette(base)(n)
}

builder_normalize_hex_color <- function(value) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !grepl("^#[0-9A-Fa-f]{6}$", value)
  ) {
    return(NULL)
  }
  toupper(value)
}

builder_group_level_label <- function(level) {
  if (identical(as.character(level), "N/A")) "Missing" else as.character(level)
}

builder_settings_color_overrides <- function(settings) {
  normalize_group <- function(values) {
    if (!is.atomic(values) || is.null(names(values))) {
      return(stats::setNames(character(), character()))
    }
    normalized <- lapply(as.character(values), builder_normalize_hex_color)
    valid <- !vapply(normalized, is.null, logical(1)) & nzchar(names(values))
    stats::setNames(
      unlist(normalized[valid], use.names = FALSE),
      names(values)[valid]
    )
  }
  legacy <- settings$colors %||% list()
  previous <- settings$color_overrides %||% list()
  canonical <- settings$group_color_overrides %||% list()
  groups <- unique(c(names(legacy), names(previous), names(canonical)))
  out <- list()
  for (group in groups) {
    source <- if (group %in% names(canonical)) {
      canonical[[group]]
    } else if (group %in% names(previous)) {
      previous[[group]]
    } else {
      legacy[[group]]
    }
    normalized <- normalize_group(source)
    if (length(normalized)) {
      out[[group]] <- normalized
    }
  }
  out
}

builder_update_color_override <- function(overrides, group, level, value) {
  normalized <- builder_normalize_hex_color(value)
  if (
    is.null(normalized) ||
      !is.character(group) ||
      length(group) != 1L ||
      is.na(group) ||
      !nzchar(group) ||
      !is.character(level) ||
      length(level) != 1L ||
      is.na(level) ||
      !nzchar(level)
  ) {
    return(overrides %||% list())
  }
  next_overrides <- overrides %||% list()
  group_overrides <- next_overrides[[group]] %||%
    stats::setNames(character(), character())
  group_overrides[[level]] <- normalized
  next_overrides[[group]] <- group_overrides
  next_overrides
}

builder_reset_color_overrides <- function(overrides, group) {
  next_overrides <- overrides %||% list()
  if (
    is.character(group) &&
      length(group) == 1L &&
      !is.na(group) &&
      nzchar(group)
  ) {
    next_overrides[[group]] <- NULL
  }
  next_overrides
}

#' A level -> hex vector for one grouping variable.
#'
#' `overrides` are the swatches the user has actually touched; everything else
#' comes from the chosen palette, so switching palette does not throw away a
#' hand-picked colour.
builder_level_colors <- function(
  levels,
  palette_id = "cerebro",
  overrides = NULL
) {
  if (!length(levels)) {
    return(character())
  }
  pal <- Filter(function(p) identical(p$id, palette_id), builder_palettes())
  fn <- if (length(pal)) pal[[1]]$colors else builder_palettes()[[1]]$colors
  out <- toupper(fn(length(levels)))
  names(out) <- levels
  ## The viewer forces N/A to grey; matching it keeps the preview honest.
  if ("N/A" %in% levels) {
    out[["N/A"]] <- "#898989"
  }
  if (length(overrides)) {
    shared <- intersect(levels, names(overrides))
    if (length(shared)) {
      normalized <- lapply(overrides[shared], builder_normalize_hex_color)
      valid <- !vapply(normalized, is.null, logical(1))
      if (any(valid)) {
        out[shared[valid]] <- unlist(normalized[valid], use.names = FALSE)
      }
    }
  }
  out
}
