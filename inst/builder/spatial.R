##----------------------------------------------------------------------------##
## Barcode identity, normalized spatial coordinates, and image normalization.
##
## These contracts are pure and are sourced before every Builder consumer.
## Coordinates are matched by barcode before their row order can affect a
## preview or export, and every supported raster becomes RGBA exactly once.
##----------------------------------------------------------------------------##

.builder_match_ids <- function(ids) {
  if (is.null(ids)) {
    return(character())
  }
  as.character(ids)
}

#' Match one cell-associated component to canonical dataset barcodes.
#'
#' Exact matches require the complete dataset identity. Spatial matches may be
#' a subset and may report rows outside the dataset, but only dataset rows are
#' returned in `matched_ids` and `input_index`.
builder_match_cells <- function(ids, expected, mode = c("exact", "subset")) {
  mode <- match.arg(mode)
  ids <- .builder_match_ids(ids)
  expected <- .builder_match_ids(expected)

  if (
    length(ids) > 0L &&
      identical(ids, expected) &&
      !anyNA(ids) &&
      all(nzchar(ids)) &&
      !anyDuplicated(ids)
  ) {
    index <- seq_along(ids)
    return(list(
      ids = ids,
      expected = expected,
      count = length(ids),
      valid = TRUE,
      relation = "full",
      duplicates = character(),
      expected_duplicates = character(),
      blanks = character(),
      expected_blanks = character(),
      missing = character(),
      extra = character(),
      order_matches = TRUE,
      coverage = 1,
      canonical_ids = expected,
      reorder_index = index,
      matched_ids = expected,
      input_index = index
    ))
  }

  blank <- is.na(ids) | !nzchar(ids)
  expected_blank <- is.na(expected) | !nzchar(expected)
  usable <- ids[!blank]
  expected_usable <- expected[!expected_blank]
  duplicates <- unique(usable[duplicated(usable)])
  expected_duplicates <- unique(
    expected_usable[duplicated(expected_usable)]
  )
  ## Each id vector is de-duplicated and hashed once (at 1M cells every extra
  ## unique() or %in% costs about half a second).
  unique_usable <- unique(usable)
  unique_expected <- unique(expected_usable)
  in_usable <- unique_expected %in% unique_usable
  missing <- unique_expected[!in_usable]
  extra <- unique_usable[!unique_usable %in% unique_expected]
  matched_ids <- unique_expected[in_usable]
  input_index <- match(matched_ids, ids)
  denominator <- length(unique_expected)
  coverage <- if (!denominator) 1 else length(matched_ids) / denominator
  structurally_valid <- !any(blank) &&
    !any(expected_blank) &&
    !length(duplicates) &&
    !length(expected_duplicates)
  exact <- structurally_valid &&
    !length(missing) &&
    !length(extra) &&
    length(ids) == length(expected)
  subset <- structurally_valid && length(matched_ids) > 0L
  valid <- if (identical(mode, "exact")) exact else subset
  relation <- if (!valid) {
    "invalid"
  } else if (exact) {
    "full"
  } else {
    "partial"
  }

  list(
    ids = ids,
    expected = expected,
    count = length(ids),
    valid = valid,
    relation = relation,
    duplicates = duplicates,
    expected_duplicates = expected_duplicates,
    blanks = unique(ids[blank]),
    expected_blanks = unique(expected[expected_blank]),
    missing = missing,
    extra = extra,
    order_matches = identical(ids, expected),
    coverage = coverage,
    canonical_ids = expected,
    reorder_index = match(expected, ids),
    matched_ids = matched_ids,
    input_index = input_index
  )
}

.builder_spatial_abort <- function(message) {
  stop(message, call. = FALSE)
}

.builder_spatial_plain_list <- function(value) {
  is.list(value) && !is.object(value) && length(value) > 0L
}

builder_spatial_scene_layers <- function(object, id, kind) {
  layers <- "points"
  if (!identical(kind, "spatial")) {
    return(layers)
  }
  image <- tryCatch(object[[id]], error = function(error) NULL)
  slots <- if (isS4(image)) methods::slotNames(image) else character()
  slot_value <- function(name) {
    if (!name %in% slots) {
      return(NULL)
    }
    tryCatch(methods::slot(image, name), error = function(error) NULL)
  }

  raster <- slot_value("image")
  declared_raster <- tryCatch(
    object@misc[["cerebro_spatial_images"]][[id]],
    error = function(error) NULL
  )
  raster_dimensions <- tryCatch(dim(raster), error = function(error) NULL)
  if (
    length(raster_dimensions) >= 2L ||
      .builder_spatial_plain_list(declared_raster)
  ) {
    layers <- c(layers, "raster")
  }

  boundaries <- slot_value("boundaries")
  if (.builder_spatial_plain_list(boundaries)) {
    has_boundaries <- any(vapply(
      boundaries,
      function(boundary) {
        methods::is(boundary, "Segmentation") ||
          (!methods::is(boundary, "Centroids") && !is.null(boundary))
      },
      logical(1)
    ))
    if (has_boundaries) {
      layers <- c(layers, "boundaries")
    }
  }
  if (.builder_spatial_plain_list(slot_value("molecules"))) {
    layers <- c(layers, "molecules")
  }
  layers
}

.builder_spatial_scene <- function(
  object,
  id,
  kind,
  cells,
  unit,
  label = id
) {
  annotations <- list(
    sample = builder_viewer_spatial_annotation(
      object@meta.data,
      cells,
      c("sample", "sample_id", "orig.ident")
    ),
    roi = builder_viewer_spatial_annotation(
      object@meta.data,
      cells,
      c("sample_roi", "roi", "roi_id", "region_of_interest")
    )
  )
  builder_viewer_spatial_scene(
    id = id,
    label = builder_viewer_spatial_scene_label(label, annotations),
    kind = kind,
    source_id = id,
    unit = unit,
    observations = list(
      kind = "cell_or_spot",
      count = as.integer(length(unique(cells)))
    ),
    annotations = annotations,
    layers = builder_spatial_scene_layers(object, id, kind)
  )
}

#' Sections that can participate in the Builder alignment workbench.
#'
#' Seurat image names remain the stable section identifiers used by the
#' existing CRB spatial payload. A two-dimensional reduction named `spatial`
#' is also a valid physical coordinate space even when the object has no image.
#' Trekker has one physical coordinate space and therefore receives one
#' explicit pseudo-section. This function never changes coordinates.
builder_spatial_alignment_sections <- function(object) {
  if (!isS4(object) || !methods::is(object, "Seurat")) {
    return(list())
  }
  image_names <- tryCatch(
    names(methods::slot(object, "images")),
    error = function(error) character()
  )
  image_names <- image_names[!is.na(image_names) & nzchar(image_names)]
  reduction_names <- tryCatch(
    names(methods::slot(object, "reductions")),
    error = function(error) character()
  )
  reduction_names <- reduction_names[
    tolower(reduction_names) == "spatial" &
      !reduction_names %in% image_names
  ]
  reduction_names <- Filter(
    function(name) {
      dimensions <- tryCatch(
        dim(SeuratObject::Embeddings(object[[name]])),
        error = function(error) NULL
      )
      length(dimensions) == 2L && dimensions[[2L]] >= 2L
    },
    reduction_names
  )
  sections <- lapply(reduction_names, function(name) {
    cells <- tryCatch(
      rownames(SeuratObject::Embeddings(object[[name]])),
      error = function(error) character()
    )
    .builder_spatial_scene(
      object,
      name,
      "spatial_reduction",
      cells,
      "Spatial coordinate units",
      paste0(name, " (reduction)")
    )
  })
  sections <- c(
    sections,
    lapply(image_names, function(name) {
      cells <- tryCatch(
        SeuratObject::Cells(object[[name]]),
        error = function(error) character()
      )
      .builder_spatial_scene(
        object,
        name,
        "spatial",
        cells,
        "Spatial coordinate units"
      )
    })
  )

  trekker <- tryCatch(object@misc$trekker, error = function(error) NULL)
  has_trekker_coordinates <- is.list(trekker) &&
    length(trekker$barcodes %||% character()) > 0L &&
    length(trekker$x %||% numeric()) > 0L &&
    length(trekker$y %||% numeric()) > 0L
  if (has_trekker_coordinates) {
    sections[[length(sections) + 1L]] <- .builder_spatial_scene(
      object,
      "trekker",
      "trekker",
      as.character(trekker$barcodes),
      "Physical coordinate units",
      "Trekker physical space"
    )
  }
  sections
}

builder_profile_spatial_reductions <- function(profile) {
  reductions <- as.character(profile$reductions %||% character())
  reductions[
    !is.na(reductions) &
      nzchar(reductions) &
      tolower(reductions) == "spatial"
  ]
}

builder_spatial_section_is_spatial <- function(kind) {
  kind %in% c("spatial", "spatial_reduction")
}

#' Pick the transcriptome projection shown beside physical space.
#'
#' UMAP is the most familiar view and wins whenever present. Otherwise the
#' user's current projection is honoured, with PCA as the final honest fallback.
builder_alignment_projection <- function(reductions, current = NULL) {
  reductions <- as.character(reductions %||% character())
  reductions <- reductions[!is.na(reductions) & nzchar(reductions)]
  if (!length(reductions)) {
    return(NULL)
  }
  find_named <- function(name) {
    match <- which(tolower(reductions) == tolower(name))
    if (length(match)) reductions[[match[[1L]]]] else NULL
  }
  umap <- find_named("umap")
  if (!is.null(umap)) {
    return(umap)
  }
  current <- as.character(current %||% character())
  if (length(current) && !is.na(current[[1L]]) && nzchar(current[[1L]])) {
    selected <- find_named(current[[1L]])
    if (!is.null(selected)) {
      return(selected)
    }
  }
  find_named("pca")
}

.builder_spatial_assert_match <- function(match) {
  if (isTRUE(match$valid)) {
    return(invisible(match))
  }
  if (length(match$duplicates) || length(match$expected_duplicates)) {
    .builder_spatial_abort(
      "Spatial cell identity contains duplicate barcodes."
    )
  }
  if (length(match$blanks) || length(match$expected_blanks)) {
    .builder_spatial_abort("Spatial cell identity contains blank barcodes.")
  }
  .builder_spatial_abort(
    "Spatial cell identity has no valid dataset barcode overlap."
  )
}

#' Admit coordinate tables without dispatching container methods.
builder_spatial_coordinate_table <- function(data) {
  invalid <- function() {
    list(valid = FALSE, data = NULL)
  }
  if (isS4(data)) {
    return(invalid())
  }

  explicit_class <- attr(data, "class", exact = TRUE)
  if (identical(explicit_class, "data.frame")) {
    row_names <- attr(data, "row.names", exact = TRUE)
    column_names <- attr(data, "names", exact = TRUE)
    safe_row_names <- !isS4(row_names) &&
      !is.object(row_names) &&
      typeof(row_names) %in% c("integer", "character")
    safe_column_names <- is.character(column_names) &&
      !is.object(column_names)
    if (!safe_row_names || !safe_column_names) {
      return(invalid())
    }
    keep <- !is.na(column_names) & nzchar(column_names)
    data <- data[, keep, drop = FALSE]
    names(data) <- make.unique(names(data))
    return(list(valid = TRUE, data = data))
  }

  dimensions <- attr(data, "dim", exact = TRUE)
  unclassed_matrix <- is.null(explicit_class) &&
    !isS4(dimensions) &&
    !is.object(dimensions) &&
    typeof(dimensions) == "integer" &&
    length(dimensions) == 2L &&
    !anyNA(dimensions) &&
    all(dimensions >= 0L)
  if (!unclassed_matrix) {
    return(invalid())
  }

  converted <- tryCatch(
    base::as.data.frame.matrix(data, stringsAsFactors = FALSE),
    error = function(error) NULL
  )
  if (is.null(converted)) {
    return(invalid())
  }
  list(valid = TRUE, data = converted)
}

#' Read coordinate columns without dispatching methods on classed numerics.
builder_spatial_coordinate_values <- function(data, columns, rows = NULL) {
  coordinate_table <- builder_spatial_coordinate_table(data)
  data <- coordinate_table$data
  valid_table <- coordinate_table$valid &&
    is.character(columns) &&
    length(columns) == 2L &&
    all(columns %in% names(data))
  if (!valid_table) {
    return(list(
      valid = FALSE,
      finite = FALSE,
      values = matrix(numeric(), nrow = 0L, ncol = 0L)
    ))
  }
  coordinate_columns <- lapply(columns, function(column) {
    .subset2(data, column)
  })
  safe_columns <- vapply(
    coordinate_columns,
    function(column) {
      !is.object(column) &&
        !isS4(column) &&
        typeof(column) %in% c("integer", "double")
    },
    logical(1)
  )
  if (!all(safe_columns)) {
    return(list(
      valid = FALSE,
      finite = FALSE,
      values = matrix(numeric(), nrow = 0L, ncol = 0L)
    ))
  }

  values <- matrix(
    unlist(coordinate_columns, recursive = FALSE, use.names = FALSE),
    nrow = nrow(data),
    ncol = 2L
  )
  colnames(values) <- columns
  if (!is.null(rows)) {
    values <- values[rows, , drop = FALSE]
  }
  list(
    valid = TRUE,
    finite = !anyNA(values) && all(is.finite(values)),
    values = values
  )
}

.builder_spatial_image_table <- function(object, image, cells) {
  images <- tryCatch(names(object@images), error = function(error) character())
  if (!length(images)) {
    .builder_spatial_abort("The object contains no spatial images.")
  }
  if (is.null(image) || !length(image) || !image[[1L]] %in% images) {
    image <- images[[1L]]
  } else {
    image <- image[[1L]]
  }
  coordinates <- tryCatch(
    SeuratObject::GetTissueCoordinates(object[[image]]),
    error = function(error) NULL
  )
  coordinate_table <- builder_spatial_coordinate_table(coordinates)
  if (!coordinate_table$valid) {
    .builder_spatial_abort(paste0(
      "The spatial image has no safe coordinate table; coordinates require ",
      "an unclassed base matrix or exact base data frame."
    ))
  }
  coordinates <- coordinate_table$data

  row_barcodes <- rownames(coordinates)
  barcode_column <- .spx_find_barcode_column(coordinates, cells)
  column_barcodes <- if (is.null(barcode_column)) {
    NULL
  } else {
    .spx_contract_barcode_values(coordinates, barcode_column)
  }
  row_overlap <- sum(row_barcodes %in% cells, na.rm = TRUE)
  column_overlap <- if (is.null(column_barcodes)) {
    0L
  } else {
    sum(column_barcodes %in% cells, na.rm = TRUE)
  }
  barcodes <- if (!is.null(column_barcodes) && column_overlap >= row_overlap) {
    column_barcodes
  } else {
    row_barcodes
  }

  list(data = coordinates, barcodes = barcodes, image = image)
}

#' Normalize spatial coordinates once for previews and exports.
#'
#' Metadata coordinates require an explicit two-column selection. Coordinate
#' aliases are inferred only for Seurat image coordinate tables, using the
#' shared bundle-safe coordinate contract.
builder_spatial_contract <- function(
  data,
  cells = NULL,
  coord_cols = NULL,
  barcodes = NULL,
  source = c("metadata", "seurat_image"),
  image = NULL
) {
  is_seurat <- isS4(data) && methods::is(data, "Seurat")
  selected_image <- NULL
  if (is_seurat) {
    object <- data
    cells <- SeuratObject::Cells(object)
    use_image <- is.null(coord_cols) || !is.null(image)
    if (use_image) {
      extracted <- .builder_spatial_image_table(object, image, cells)
      data <- extracted$data
      barcodes <- extracted$barcodes
      selected_image <- extracted$image
      source <- "seurat_image"
    } else {
      data <- object@meta.data
      source <- "metadata"
    }
  } else {
    source <- match.arg(source)
  }

  coordinate_table <- builder_spatial_coordinate_table(data)
  if (!coordinate_table$valid) {
    .builder_spatial_abort(paste0(
      "Spatial coordinates require an unclassed base matrix or exact base ",
      "data frame."
    ))
  }
  data <- coordinate_table$data
  if (!nrow(data)) {
    .builder_spatial_abort("Spatial coordinates require a non-empty table.")
  }
  if (is.null(cells)) {
    .builder_spatial_abort("Spatial coordinates require dataset cell barcodes.")
  }
  if (is.null(barcodes)) {
    barcodes <- rownames(data)
  }
  if (length(barcodes) != nrow(data)) {
    .builder_spatial_abort(
      "Spatial cell identity does not align with coordinate rows."
    )
  }

  if (identical(source, "metadata") && is.null(coord_cols)) {
    .builder_spatial_abort(
      "Metadata spatial mapping requires explicit x and y columns."
    )
  }
  if (
    is.character(coord_cols) &&
      length(coord_cols) == 2L &&
      !anyNA(coord_cols) &&
      identical(coord_cols[[1L]], coord_cols[[2L]])
  ) {
    .builder_spatial_abort(
      "Spatial x and y coordinate columns must be distinct."
    )
  }
  coordinate_match <- .spx_find_coordinate_columns(
    data,
    coord_cols = coord_cols,
    hard_error = TRUE
  )
  if (is.null(coordinate_match)) {
    .builder_spatial_abort(
      "The spatial x and y coordinate columns were not recognized."
    )
  }
  coordinate_columns <- c(
    x = unname(coordinate_match$x),
    y = unname(coordinate_match$y)
  )
  match <- builder_match_cells(barcodes, cells, mode = "subset")
  .builder_spatial_assert_match(match)
  coordinate_values <- builder_spatial_coordinate_values(
    data,
    unname(coordinate_columns),
    rows = match$input_index
  )
  if (!coordinate_values$valid) {
    .builder_spatial_abort(paste0(
      "Spatial coordinates must use unclassed base integer or ",
      "base double values."
    ))
  }
  if (!coordinate_values$finite) {
    .builder_spatial_abort("Spatial coordinates must be numeric and finite.")
  }
  values <- coordinate_values$values

  normalized <- data.frame(
    cell_barcode = match$matched_ids,
    x = as.numeric(values[, 1L]),
    y = as.numeric(values[, 2L]),
    stringsAsFactors = FALSE
  )

  list(
    coordinates = normalized,
    preview = normalized,
    export = normalized,
    match = match,
    coordinate_columns = coordinate_columns,
    source = source,
    image = selected_image
  )
}

##----------------------------------------------------------------------------##
## Region subset: keep only the cells inside drawn regions of a section.
##
## A region is a polygon in the section's raw coordinate space (the space the
## preview and the exported coordinates start from, before any rotation or
## scale of the Viewer). A section with regions keeps the cells of that section
## that fall inside at least one polygon; cells of other sections are not
## touched. The Builder resolves the regions once, in the worker, and freezes
## the identity of the cells that remain; the build repeats the same cut and
## refuses to continue when it does not reproduce that identity.
##----------------------------------------------------------------------------##

BUILDER_REGION_MAX_POLYGONS <- 32L
BUILDER_REGION_MAX_VERTICES <- 2000L

.builder_region_plain_names <- function(value) {
  is.list(value) &&
    !is.object(value) &&
    (!length(value) ||
      (is.character(names(value)) &&
        !anyNA(names(value)) &&
        all(nzchar(names(value))) &&
        !anyDuplicated(names(value))))
}

.builder_region_number_vector <- function(value) {
  is.numeric(value) &&
    !is.object(value) &&
    !anyNA(value) &&
    all(is.finite(value))
}

#' Validate the polygons drawn for one section.
#'
#' Returns a plain list of `list(x = numeric, y = numeric)`; every polygon has
#' at least three vertices and is stored without attributes.
builder_region_polygons_normalize <- function(value, context = "region") {
  fail <- function(reason) {
    stop(
      "Region polygons for ",
      context,
      " are invalid: ",
      reason,
      ".",
      call. = FALSE
    )
  }
  if (!is.list(value) || is.object(value) || !length(value)) {
    fail("at least one polygon is required")
  }
  if (length(value) > BUILDER_REGION_MAX_POLYGONS) {
    fail(paste0(
      "at most ",
      BUILDER_REGION_MAX_POLYGONS,
      " polygons are allowed"
    ))
  }
  lapply(unname(value), function(polygon) {
    x <- if (is.list(polygon)) polygon[["x"]] else NULL
    y <- if (is.list(polygon)) polygon[["y"]] else NULL
    x <- if (is.list(x)) unlist(x, use.names = FALSE) else x
    y <- if (is.list(y)) unlist(y, use.names = FALSE) else y
    if (
      !.builder_region_number_vector(x) || !.builder_region_number_vector(y)
    ) {
      fail("vertices must be finite numbers")
    }
    if (length(x) != length(y) || length(x) < 3L) {
      fail("a polygon needs at least three vertices")
    }
    if (length(x) > BUILDER_REGION_MAX_VERTICES) {
      fail(paste0(
        "a polygon may have at most ",
        BUILDER_REGION_MAX_VERTICES,
        " vertices"
      ))
    }
    list(x = as.numeric(x), y = as.numeric(y))
  })
}

#' Which points lie inside at least one polygon (even-odd rule).
builder_region_points_inside <- function(x, y, polygons) {
  inside <- rep(FALSE, length(x))
  for (polygon in polygons) {
    px <- polygon$x
    py <- polygon$y
    count <- length(px)
    within <- rep(FALSE, length(x))
    previous <- count
    for (current in seq_len(count)) {
      crossing <- (py[[current]] > y) != (py[[previous]] > y)
      if (any(crossing)) {
        slope <- (px[[previous]] - px[[current]]) /
          (py[[previous]] - py[[current]])
        at <- slope * (y - py[[current]]) + px[[current]]
        within <- xor(within, crossing & x < at)
      }
      previous <- current
    }
    inside <- inside | within
  }
  inside
}

#' Raw coordinates of one section's cells, or `NULL` when it has none.
builder_region_section_coordinates <- function(object, section) {
  sections <- builder_spatial_alignment_sections(object)
  ids <- vapply(sections, `[[`, character(1), "id")
  if (!section %in% ids) {
    return(NULL)
  }
  kind <- sections[[match(section, ids)]]$kind
  frame <- tryCatch(
    if (identical(kind, "trekker")) {
      payload <- object@misc$trekker
      data.frame(
        cell_barcode = as.character(payload$barcodes),
        x = as.numeric(payload$x),
        y = as.numeric(payload$y),
        stringsAsFactors = FALSE
      )
    } else if (identical(kind, "spatial_reduction")) {
      embeddings <- SeuratObject::Embeddings(object[[section]])
      data.frame(
        cell_barcode = rownames(embeddings),
        x = as.numeric(embeddings[, 1L]),
        y = as.numeric(embeddings[, 2L]),
        stringsAsFactors = FALSE
      )
    } else {
      builder_spatial_contract(
        object,
        cells = SeuratObject::Cells(object),
        image = section
      )$coordinates[, c("cell_barcode", "x", "y")]
    },
    error = function(error) NULL
  )
  if (
    is.null(frame) ||
      !nrow(frame) ||
      anyNA(frame$cell_barcode) ||
      !all(is.finite(frame$x)) ||
      !all(is.finite(frame$y))
  ) {
    return(NULL)
  }
  frame
}

#' Validate the stored region subset of one dataset (or `NULL`/empty).
builder_region_subset_normalize <- function(value) {
  if (is.null(value) || (is.list(value) && !length(value))) {
    return(list())
  }
  fail <- function(reason) {
    stop("Region subset is invalid: ", reason, ".", call. = FALSE)
  }
  if (
    !.builder_region_plain_names(value) ||
      !identical(sort(names(value)), c("resolved", "sections"))
  ) {
    fail("expected sections and resolved")
  }
  sections <- value$sections
  if (!.builder_region_plain_names(sections) || !length(sections)) {
    fail("sections must be a non-empty named list")
  }
  sections <- stats::setNames(
    lapply(names(sections), function(name) {
      record <- sections[[name]]
      if (!.builder_region_plain_names(record)) {
        fail(paste0("section ", name, " is not a record"))
      }
      list(
        polygons = builder_region_polygons_normalize(
          record$polygons,
          paste0("section ", name)
        )
      )
    }),
    names(sections)
  )
  resolved <- value$resolved
  scalar_count <- function(number) {
    is.numeric(number) &&
      length(number) == 1L &&
      !is.na(number) &&
      is.finite(number) &&
      number >= 0 &&
      number == floor(number)
  }
  cells <- builder_axis_identity_normalize(resolved$cells)
  source_md5 <- resolved$source_md5
  if (
    !.builder_region_plain_names(resolved) ||
      is.null(cells) ||
      !is.character(source_md5) ||
      length(source_md5) != 1L ||
      is.na(source_md5) ||
      !grepl("^[[:xdigit:]]{32}$", source_md5) ||
      !scalar_count(resolved$kept) ||
      !scalar_count(resolved$total) ||
      resolved$kept < 1 ||
      resolved$kept > resolved$total ||
      !identical(as.numeric(cells$count), as.numeric(resolved$kept))
  ) {
    fail("the resolved cell identity is incomplete")
  }
  levels <- resolved$group_levels %||% list()
  ## A Project file reads arrays back as lists.
  if (.builder_region_plain_names(levels)) {
    levels <- lapply(levels, function(entry) {
      if (is.list(entry)) {
        as.character(unlist(entry, use.names = FALSE))
      } else {
        entry
      }
    })
  }
  if (
    !.builder_region_plain_names(levels) ||
      !all(vapply(
        levels,
        function(entry) is.character(entry) && !anyNA(entry),
        logical(1)
      ))
  ) {
    fail("group levels must be named character vectors")
  }
  counts <- resolved$sections %||% list()
  if (!.builder_region_plain_names(counts)) {
    fail("section counts must be a named list")
  }
  list(
    sections = sections,
    resolved = list(
      source_md5 = tolower(source_md5),
      cells = cells,
      kept = as.integer(resolved$kept),
      total = as.integer(resolved$total),
      group_levels = lapply(levels, unname),
      sections = lapply(counts, function(entry) {
        list(
          kept = as.integer(entry$kept %||% 0L),
          total = as.integer(entry$total %||% 0L)
        )
      })
    )
  )
}

#' The cells that remain after cutting every section that has regions.
#'
#' @return `list(kept, total, sections)`; `kept` keeps the object's cell order.
builder_region_subset_cells <- function(object, sections) {
  cells <- SeuratObject::Cells(object)
  dropped <- character()
  counts <- list()
  for (section in names(sections)) {
    polygons <- builder_region_polygons_normalize(
      sections[[section]]$polygons,
      paste0("section ", section)
    )
    frame <- builder_region_section_coordinates(object, section)
    if (is.null(frame)) {
      stop(
        "Section ",
        section,
        " has no spatial coordinates to cut a region from.",
        call. = FALSE
      )
    }
    inside <- builder_region_points_inside(frame$x, frame$y, polygons)
    if (!any(inside)) {
      stop(
        "The region drawn in section ",
        section,
        " contains no cells.",
        call. = FALSE
      )
    }
    dropped <- c(dropped, frame$cell_barcode[!inside])
    counts[[section]] <- list(
      kept = as.integer(sum(inside)),
      total = as.integer(length(inside))
    )
  }
  kept <- cells[!cells %in% dropped]
  if (!length(kept)) {
    stop("The drawn regions leave no cells to keep.", call. = FALSE)
  }
  list(
    kept = kept,
    total = length(cells),
    sections = counts
  )
}

#' Resolve drawn regions against the open object (runs in the worker).
#'
#' Returns what the plan needs without the cell names: the identity of the
#' remaining cells, the counts, and the group levels still present.
builder_region_subset_resolve <- function(object, sections, levels = list()) {
  cut <- builder_region_subset_cells(object, sections)
  metadata <- object@meta.data
  present <- lapply(names(levels %||% list()), function(group) {
    full <- as.character(levels[[group]])
    if (!group %in% colnames(metadata)) {
      return(full)
    }
    values <- as.character(metadata[cut$kept, group])
    full[full %in% values]
  })
  names(present) <- names(levels %||% list())
  list(
    cells = builder_axis_identity(cut$kept),
    kept = length(cut$kept),
    total = cut$total,
    group_levels = present,
    sections = cut$sections
  )
}

#' Cut the Trekker payload down to the kept cells.
#'
#' Per-cell vectors, confidence, fields and evidence follow the kept cells.
#' Summaries computed on the whole slide (Moran's I, per-type field medians,
#' the QC table) are kept as they were.
builder_region_subset_trekker <- function(trekker, keep) {
  barcodes <- as.character(trekker$barcodes)
  index <- which(barcodes %in% keep)
  if (!length(index)) {
    stop("The drawn regions leave no Trekker cells to keep.", call. = FALSE)
  }
  pick <- function(values) values[index]
  for (name in c("barcodes", "x", "y", "ux", "uy", "clusters")) {
    if (!is.null(trekker[[name]])) {
      trekker[[name]] <- pick(trekker[[name]])
    }
  }
  trekker$conf <- lapply(trekker$conf, pick)
  trekker$fields <- lapply(trekker$fields, function(field) {
    field$v <- pick(field$v)
    field
  })
  new_position <- stats::setNames(seq_along(index) - 1L, index - 1L)
  evidence <- Filter(
    function(entry) as.character(entry$cell) %in% names(new_position),
    trekker$evidence %||% list()
  )
  trekker$evidence <- lapply(unname(evidence), function(entry) {
    entry$cell <- as.integer(new_position[[as.character(entry$cell)]])
    entry
  })
  if (is.list(trekker$meta)) {
    trekker$meta$n_cells <- length(index)
  }
  trekker
}

#' Cut a Seurat object down to the cells inside the frozen regions.
builder_region_subset_apply <- function(
  object,
  region_subset,
  expected = NULL
) {
  cut <- builder_region_subset_cells(object, region_subset$sections)
  if (!is.null(expected)) {
    observed <- builder_axis_identity(cut$kept)
    if (!identical(observed, builder_axis_identity_normalize(expected))) {
      stop(
        "The cells inside the drawn regions differ from the frozen plan. ",
        "Draw the regions again.",
        call. = FALSE
      )
    }
  }
  trekker <- object@misc$trekker
  object <- subset(object, cells = cut$kept)
  ## Drop precomputed whole-slide summaries so they are never exported out of
  ## step with the kept cells; any analyses selected in the Builder run fresh
  ## on the kept cells after this cut.
  for (slot in c(
    "marker_genes",
    "most_expressed_genes",
    "enriched_pathways",
    "mean_expression",
    "trees"
  )) {
    object@misc[[slot]] <- NULL
  }
  if (is.list(trekker) && length(trekker$barcodes %||% character())) {
    object@misc$trekker <- builder_region_subset_trekker(trekker, cut$kept)
  }
  object
}
