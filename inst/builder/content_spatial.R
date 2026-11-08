##----------------------------------------------------------------------------##
## Stable Viewer-content facts for Spatial and Trekker data.
##
## These profiles deliberately keep only bounded summaries. Coordinates,
## per-cell Trekker vectors, and evidence images stay in the source object and
## are read again by the worker after the BuildPlan has made its decisions.
##----------------------------------------------------------------------------##

# Shared records ----

.builder_content_spatial_record <- function(
  detected,
  valid,
  normalized,
  diagnostics,
  requirements,
  page_candidates
) {
  list(
    detected = isTRUE(detected),
    valid = isTRUE(valid),
    normalized = normalized,
    diagnostics = unique(as.character(diagnostics)),
    requirements = unique(as.character(requirements)),
    page_candidates = unique(as.character(page_candidates))
  )
}

.builder_content_spatial_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.builder_content_spatial_number <- function(value) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value)
}

.builder_content_spatial_integerish <- function(value) {
  is.numeric(value) &&
    !anyNA(value) &&
    all(is.finite(value)) &&
    all(value == floor(value))
}

.builder_content_spatial_plain <- function(value, depth = 0L) {
  if (depth > 20L) {
    return(FALSE)
  }
  if (
    is.object(value) ||
      is.function(value) ||
      is.environment(value) ||
      is.language(value) ||
      is.symbol(value) ||
      typeof(value) == "externalptr"
  ) {
    return(FALSE)
  }
  if (is.list(value)) {
    return(all(vapply(
      value,
      .builder_content_spatial_plain,
      logical(1),
      depth = depth + 1L
    )))
  }
  is.null(value) || is.atomic(value)
}

.builder_content_spatial_names_valid <- function(value, allow_empty = TRUE) {
  if (!is.list(value)) {
    return(FALSE)
  }
  if (!length(value)) {
    return(isTRUE(allow_empty))
  }
  names <- names(value)
  !is.null(names) &&
    length(names) == length(value) &&
    !anyNA(names) &&
    all(nzchar(names)) &&
    !anyDuplicated(names)
}

.builder_content_spatial_bound_text <- function(
  value,
  max_chars = 64L,
  max_bytes = 256L
) {
  if (!.builder_content_spatial_text(value)) {
    return(list(
      value = "",
      truncated = FALSE,
      characters = 0L,
      bytes = 0L
    ))
  }
  text <- tryCatch(enc2utf8(value), error = function(error) "")
  characters <- nchar(text, type = "chars", allowNA = TRUE)
  bytes <- nchar(text, type = "bytes", allowNA = TRUE)
  if (is.na(characters) || is.na(bytes)) {
    return(list(
      value = "",
      truncated = TRUE,
      characters = NA_integer_,
      bytes = NA_integer_
    ))
  }
  bounded <- substr(text, 1L, min(characters, max_chars))
  while (nchar(bounded, type = "bytes") > max_bytes && nzchar(bounded)) {
    bounded <- substr(bounded, 1L, nchar(bounded, type = "chars") - 1L)
  }
  list(
    value = bounded,
    truncated = characters > max_chars || bytes > max_bytes,
    characters = characters,
    bytes = bytes
  )
}

.builder_content_spatial_preview_summary <- function(value, limit = 32L) {
  value <- as.character(value)
  value <- value[!is.na(value)]
  selected <- utils::head(value, limit)
  bounded <- lapply(selected, .builder_content_spatial_bound_text)
  list(
    values = vapply(bounded, function(item) item$value, character(1)),
    truncated = length(value) > limit ||
      any(vapply(bounded, function(item) item$truncated, logical(1))),
    item_truncated = any(vapply(
      bounded,
      function(item) item$truncated,
      logical(1)
    )),
    count = length(value)
  )
}

.builder_content_spatial_preview <- function(value, limit = 32L) {
  .builder_content_spatial_preview_summary(value, limit)$values
}

.builder_content_spatial_identifier_summary <- function(value, limit = 32L) {
  if (
    !is.character(value) ||
      is.object(value) ||
      isS4(value)
  ) {
    return(list(
      exact = character(),
      preview = character(),
      count = 0L,
      truncated = FALSE,
      item_truncated = FALSE,
      unsafe = TRUE,
      valid = FALSE
    ))
  }
  attributes(value) <- NULL
  count <- length(value)
  shown <- min(count, limit)
  bounded <- lapply(
    value[seq_len(shown)],
    .builder_content_spatial_bound_text
  )
  item_truncated <- any(vapply(
    bounded,
    function(item) item$truncated,
    logical(1)
  ))
  truncated <- count > shown || item_truncated
  values_valid <- !anyNA(value) && all(nzchar(value))
  list(
    exact = if (values_valid && !truncated) value else character(),
    preview = vapply(bounded, function(item) item$value, character(1)),
    count = as.integer(count),
    truncated = truncated,
    item_truncated = item_truncated,
    unsafe = FALSE,
    valid = values_valid
  )
}

.builder_profile_spatial_assay_layer_facts <- function(assay, context) {
  assay_summary <- .builder_content_spatial_identifier_summary(assay, 1L)
  assay_exact <- if (
    assay_summary$valid &&
      assay_summary$count == 1L &&
      !assay_summary$truncated
  ) {
    assay_summary$exact[[1L]]
  } else {
    NA_character_
  }

  context_safe <- is.list(context) && !is.object(context) && !isS4(context)
  context_names <- if (context_safe) {
    attr(context, "names", exact = TRUE)
  } else {
    character()
  }
  context_names_safe <- is.character(context_names) &&
    !is.object(context_names) &&
    !isS4(context_names)
  assays_index <- if (context_names_safe) {
    match("assays", context_names)
  } else {
    NA_integer_
  }
  context_assays <- if (!is.na(assays_index)) {
    context[[assays_index]]
  } else {
    list()
  }
  assays_safe <- is.list(context_assays) &&
    !is.object(context_assays) &&
    !isS4(context_assays)
  assay_names <- if (assays_safe) {
    attr(context_assays, "names", exact = TRUE)
  } else {
    character()
  }
  assay_names_safe <- is.character(assay_names) &&
    !is.object(assay_names) &&
    !isS4(assay_names)
  if (assay_names_safe) {
    attributes(assay_names) <- NULL
  } else {
    assay_names <- character()
  }
  assay_index <- if (!is.na(assay_exact)) {
    match(assay_exact, assay_names)
  } else {
    NA_integer_
  }
  assay_valid <- !is.na(assay_index)
  assay_profile <- if (assay_valid) {
    context_assays[[assay_index]]
  } else {
    list()
  }
  assay_profile_safe <- is.list(assay_profile) &&
    !is.object(assay_profile) &&
    !isS4(assay_profile)
  assay_profile_names <- if (assay_profile_safe) {
    attr(assay_profile, "names", exact = TRUE)
  } else {
    character()
  }
  assay_profile_names_safe <- is.character(assay_profile_names) &&
    !is.object(assay_profile_names) &&
    !isS4(assay_profile_names)
  layer_index <- if (assay_profile_names_safe) {
    match("exportable_layers", assay_profile_names)
  } else {
    NA_integer_
  }
  layer_values <- if (!is.na(layer_index)) {
    assay_profile[[layer_index]]
  } else {
    character()
  }
  layer_summary <- .builder_content_spatial_identifier_summary(
    layer_values,
    32L
  )
  layer_identifiers_valid <- assay_valid &&
    layer_summary$valid &&
    layer_summary$count > 0L &&
    !layer_summary$truncated
  compatible_layers <- if (layer_identifiers_valid) {
    layer_summary$exact
  } else {
    character()
  }
  diagnostics <- c(
    if (assay_summary$item_truncated) {
      "oversized_assay_name"
    } else {
      character()
    },
    if (assay_summary$unsafe) "unsafe_assay_name" else character(),
    if (layer_summary$item_truncated) {
      "oversized_layer_name"
    } else {
      character()
    },
    if (layer_summary$count > 32L) {
      "too_many_compatible_layers"
    } else {
      character()
    },
    if (layer_summary$unsafe) "unsafe_layer_name" else character(),
    if (!assay_valid || !layer_identifiers_valid) {
      "incompatible_assay"
    } else {
      character()
    }
  )

  list(
    valid = assay_valid && layer_identifiers_valid,
    diagnostics = unique(diagnostics),
    assay = assay_exact,
    assay_count = assay_summary$count,
    assay_preview = assay_summary$preview,
    assay_truncated = assay_summary$truncated,
    compatible_layers = compatible_layers,
    compatible_layer_count = layer_summary$count,
    compatible_layer_preview = layer_summary$preview,
    compatible_layers_truncated = layer_summary$truncated
  )
}

.builder_content_spatial_section_limit <- 32L

# Spatial sections ----

.builder_profile_spatial_barcode_summary <- function(
  ids,
  expected,
  allow_outside = FALSE
) {
  match <- builder_match_cells(ids, expected, mode = "subset")
  blanks <- is.na(match$ids) | !nzchar(match$ids)
  usable <- match$ids[!blanks]
  duplicate_rows <- duplicated(usable) |
    duplicated(usable, fromLast = TRUE)
  outside <- !usable %in% match$expected
  overlap_count <- length(match$matched_ids)
  relation <- match$relation
  if (!overlap_count && !any(blanks) && !any(duplicate_rows)) {
    relation <- "zero"
  } else if (any(outside) && !isTRUE(allow_outside)) {
    relation <- "invalid"
  }

  list(
    count = match$count,
    unique_count = length(unique(usable)),
    blank_count = sum(blanks),
    duplicate_count = sum(duplicate_rows),
    outside_count = sum(outside),
    overlap_count = overlap_count,
    dataset_count = length(match$expected),
    coverage = if (length(match$expected)) match$coverage else 0,
    relation = relation,
    valid = relation %in% c("partial", "full")
  )
}

.builder_profile_spatial_raster <- function(raster) {
  absent <- function() {
    list(
      present = FALSE,
      valid = TRUE,
      width = 0L,
      height = 0L,
      channels = 0L,
      pixel_bounds = NULL,
      diagnostics = character()
    )
  }
  unsafe <- function() {
    list(
      present = TRUE,
      valid = FALSE,
      width = 0L,
      height = 0L,
      channels = 0L,
      pixel_bounds = NULL,
      diagnostics = "unsafe_raster"
    )
  }
  if (is.null(raster)) {
    return(absent())
  }
  if (is.object(raster) || isS4(raster) || !is.atomic(raster)) {
    return(unsafe())
  }
  if (!length(raster)) {
    return(absent())
  }

  dimensions <- attr(raster, "dim", exact = TRUE)
  dimensions_safe <- (is.integer(dimensions) || is.double(dimensions)) &&
    !is.object(dimensions) &&
    !isS4(dimensions)
  if (dimensions_safe) {
    attributes(dimensions) <- NULL
  } else {
    dimensions <- numeric()
  }
  numeric <- is.numeric(raster)
  finite <- numeric && !anyNA(raster) && all(is.finite(raster))
  valid_dimensions <- dimensions_safe &&
    length(dimensions) >= 2L &&
    !anyNA(dimensions) &&
    all(is.finite(dimensions)) &&
    all(dimensions > 0L) &&
    all(dimensions == floor(dimensions))
  diagnostics <- c(
    if (!numeric) "non_numeric_raster" else character(),
    if (numeric && !finite) "non_finite_raster" else character(),
    if (!valid_dimensions) "invalid_raster_dimensions" else character()
  )
  height <- if (valid_dimensions) as.integer(dimensions[[1L]]) else 0L
  width <- if (valid_dimensions) as.integer(dimensions[[2L]]) else 0L
  channels <- if (valid_dimensions && length(dimensions) >= 3L) {
    as.integer(prod(dimensions[-c(1L, 2L)]))
  } else if (valid_dimensions) {
    1L
  } else {
    0L
  }

  list(
    present = TRUE,
    valid = numeric && finite && valid_dimensions,
    width = width,
    height = height,
    channels = channels,
    pixel_bounds = if (valid_dimensions) {
      list(xmin = 0, xmax = width, ymin = 0, ymax = height)
    } else {
      NULL
    },
    diagnostics = unique(diagnostics)
  )
}

.builder_profile_spatial_coordinate_record <- function(
  name,
  coordinates,
  assay,
  context,
  raster = NULL,
  source = NA_character_,
  barcode_ids = NULL
) {
  name_summary <- .builder_content_spatial_bound_text(as.character(name))
  assay_facts <- .builder_profile_spatial_assay_layer_facts(assay, context)
  coordinate_table <- builder_spatial_coordinate_table(coordinates)
  matrix_like <- isTRUE(coordinate_table$valid)
  if (matrix_like) {
    coordinates <- coordinate_table$data
  }
  ids <- if (!is.null(barcode_ids)) {
    barcode_ids
  } else if (matrix_like) {
    rownames(coordinates)
  } else {
    character()
  }
  coordinate_match <- if (matrix_like) {
    .spx_find_coordinate_columns(coordinates)
  } else {
    NULL
  }
  coordinate_columns <- if (is.null(coordinate_match)) {
    character()
  } else {
    unname(unlist(coordinate_match, use.names = FALSE))
  }
  dimensions <- length(coordinate_columns)
  cell_match <- builder_match_cells(ids, context$cells, mode = "subset")
  barcodes_aligned <- matrix_like && length(ids) == nrow(coordinates)
  coordinate_values <- if (dimensions == 2L) {
    builder_spatial_coordinate_values(
      coordinates,
      coordinate_columns,
      rows = if (barcodes_aligned) cell_match$input_index else NULL
    )
  } else {
    list(
      valid = FALSE,
      finite = FALSE,
      values = matrix(numeric(), nrow = 0L, ncol = 0L)
    )
  }
  coordinates_numeric <- dimensions == 2L && coordinate_values$valid
  matched_coordinate_values <- coordinate_values$values
  finite <- coordinates_numeric && coordinate_values$finite
  barcodes <- .builder_profile_spatial_barcode_summary(
    ids,
    context$cells,
    allow_outside = TRUE
  )

  raster_profile <- .builder_profile_spatial_raster(raster)
  bounds <- if (
    dimensions >= 2L &&
      finite &&
      nrow(matched_coordinate_values)
  ) {
    list(
      xmin = unname(min(matched_coordinate_values[, 1L])),
      xmax = unname(max(matched_coordinate_values[, 1L])),
      ymin = unname(min(matched_coordinate_values[, 2L])),
      ymax = unname(max(matched_coordinate_values[, 2L]))
    )
  } else {
    NULL
  }

  diagnostics <- c(
    if (!matrix_like) "invalid_coordinate_table" else character(),
    if (name_summary$truncated) "oversized_section_name" else character(),
    assay_facts$diagnostics,
    if (matrix_like && length(ids) != nrow(coordinates)) {
      "misaligned_barcodes"
    } else {
      character()
    },
    if (matrix_like && is.null(coordinate_match)) {
      "unrecognized_coordinate_columns"
    } else {
      character()
    },
    if (dimensions < 2L) {
      "fewer_than_two_coordinate_dimensions"
    } else {
      character()
    },
    if (dimensions == 2L && !coordinates_numeric) {
      "non_numeric_coordinates"
    } else {
      character()
    },
    if (coordinates_numeric && !finite) {
      "non_finite_coordinates"
    } else {
      character()
    },
    if (barcodes$blank_count) "blank_barcodes" else character(),
    if (barcodes$duplicate_count) "duplicate_barcodes" else character(),
    if (barcodes$outside_count) "outside_canonical_barcodes" else character(),
    if (!barcodes$overlap_count) {
      "no_canonical_barcode_overlap"
    } else {
      character()
    },
    raster_profile$diagnostics
  )
  valid <- matrix_like &&
    !name_summary$truncated &&
    length(ids) == nrow(coordinates) &&
    dimensions >= 2L &&
    coordinates_numeric &&
    finite &&
    barcodes$valid &&
    assay_facts$valid &&
    raster_profile$valid

  list(
    name = name_summary$value,
    name_truncated = name_summary$truncated,
    name_characters = name_summary$characters,
    name_bytes = name_summary$bytes,
    valid = isTRUE(valid),
    source = as.character(source),
    assay = assay_facts$assay,
    assay_count = assay_facts$assay_count,
    assay_preview = assay_facts$assay_preview,
    assay_truncated = assay_facts$assay_truncated,
    compatible_layers = assay_facts$compatible_layers,
    compatible_layer_count = assay_facts$compatible_layer_count,
    compatible_layer_preview = assay_facts$compatible_layer_preview,
    compatible_layers_truncated = assay_facts$compatible_layers_truncated,
    coordinates = list(
      dimensions = dimensions,
      column_count = if (matrix_like) ncol(coordinates) else 0L,
      columns = .builder_content_spatial_preview(coordinate_columns, 16L),
      finite = isTRUE(finite)
    ),
    coordinate_bounds = bounds,
    barcodes = barcodes,
    raster = raster_profile,
    diagnostics = unique(diagnostics)
  )
}

.builder_profile_spatial_image_assay <- function(image) {
  if (!isS4(image) || !"assay" %in% methods::slotNames(image)) {
    return(NA_character_)
  }
  tryCatch(
    methods::slot(image, "assay"),
    error = function(error) NA_character_
  )
}

.builder_profile_spatial_image_raster <- function(image) {
  if (!isS4(image) || !"image" %in% methods::slotNames(image)) {
    return(NULL)
  }
  tryCatch(methods::slot(image, "image"), error = function(error) NULL)
}

.builder_profile_spatial_image_coordinates <- function(image, expected_cells) {
  coordinates <- tryCatch(
    SeuratObject::GetTissueCoordinates(image),
    error = function(error) NULL
  )
  coordinate_table <- builder_spatial_coordinate_table(coordinates)
  if (!coordinate_table$valid) {
    return(list(coordinates = NULL, source = NA_character_))
  }
  coordinates <- coordinate_table$data
  if (!nrow(coordinates)) {
    return(list(coordinates = NULL, source = NA_character_))
  }

  row_barcodes <- rownames(coordinates)
  row_overlap <- sum(row_barcodes %in% expected_cells, na.rm = TRUE)
  selected <- .spx_find_barcode_column(coordinates, expected_cells)
  selected_barcodes <- if (is.null(selected)) {
    NULL
  } else {
    .spx_contract_barcode_values(coordinates, selected)
  }
  selected_overlap <- if (is.null(selected_barcodes)) {
    0L
  } else {
    sum(selected_barcodes %in% expected_cells, na.rm = TRUE)
  }
  use_selected <- !is.null(selected) && selected_overlap >= row_overlap
  if (use_selected) {
    coordinates[[selected]] <- NULL
    return(list(
      coordinates = coordinates,
      source = selected,
      barcodes = selected_barcodes
    ))
  }
  list(
    coordinates = coordinates,
    source = "rownames",
    barcodes = row_barcodes
  )
}

.builder_profile_spatial_record <- function(object, context) {
  requirements <- c(
    "seurat_images_or_spatial_reduction",
    "named_unique_sections",
    "finite_two_dimensional_coordinates",
    "canonical_barcode_overlap",
    "compatible_assay_layer",
    "valid_raster_dimensions",
    "bounded_text_fields"
  )
  empty_normalized <- list(
    section_count = 0L,
    valid_section_count = 0L,
    invalid_section_count = 0L,
    invalid_section_preview = list(),
    invalid_section_preview_truncated = FALSE,
    sections = list(),
    sections_truncated = FALSE,
    section_names_truncated = FALSE
  )
  images <- tryCatch(
    methods::slot(object, "images"),
    error = function(error) list()
  )
  if (is.null(images)) {
    images <- list()
  }
  images_safe <- is.list(images) && !is.object(images) && !isS4(images)
  if (!images_safe) {
    return(.builder_content_spatial_record(
      detected = TRUE,
      valid = FALSE,
      normalized = empty_normalized,
      diagnostics = "unsafe_images_container",
      requirements = requirements,
      page_candidates = character()
    ))
  }
  image_names <- attr(images, "names", exact = TRUE)
  names_valid <- !length(images) ||
    (is.character(image_names) &&
      !is.object(image_names) &&
      !isS4(image_names) &&
      length(image_names) == length(images) &&
      !anyNA(image_names) &&
      all(nzchar(image_names)) &&
      !anyDuplicated(image_names))
  labels <- if (names_valid) {
    image_names
  } else {
    paste0("section_", seq_along(images))
  }
  label_summaries <- lapply(labels, .builder_content_spatial_bound_text)
  section_names_truncated <- any(vapply(
    label_summaries,
    function(item) item$truncated,
    logical(1)
  ))
  bounded_labels <- vapply(
    label_summaries,
    function(item) item$value,
    character(1)
  )
  section_keys <- if (
    section_names_truncated || anyDuplicated(bounded_labels)
  ) {
    paste0("section_", seq_along(labels))
  } else {
    bounded_labels
  }
  sections <- lapply(seq_along(images), function(index) {
    image <- images[[index]]
    name_summary <- label_summaries[[index]]
    extracted <- .builder_profile_spatial_image_coordinates(
      image,
      context$cells
    )
    if (is.null(extracted$coordinates)) {
      assay_facts <- .builder_profile_spatial_assay_layer_facts(
        .builder_profile_spatial_image_assay(image),
        context
      )
      raster_profile <- .builder_profile_spatial_raster(
        .builder_profile_spatial_image_raster(image)
      )
      return(list(
        name = name_summary$value,
        name_truncated = name_summary$truncated,
        name_characters = name_summary$characters,
        name_bytes = name_summary$bytes,
        valid = FALSE,
        source = NA_character_,
        assay = assay_facts$assay,
        assay_count = assay_facts$assay_count,
        assay_preview = assay_facts$assay_preview,
        assay_truncated = assay_facts$assay_truncated,
        compatible_layers = assay_facts$compatible_layers,
        compatible_layer_count = assay_facts$compatible_layer_count,
        compatible_layer_preview = assay_facts$compatible_layer_preview,
        compatible_layers_truncated = assay_facts$compatible_layers_truncated,
        coordinates = list(
          dimensions = 0L,
          column_count = 0L,
          columns = character(),
          finite = FALSE
        ),
        coordinate_bounds = NULL,
        barcodes = .builder_profile_spatial_barcode_summary(
          character(),
          context$cells
        ),
        raster = raster_profile,
        diagnostics = unique(c(
          "coordinate_extraction_failed",
          if (name_summary$truncated) {
            "oversized_section_name"
          } else {
            character()
          },
          assay_facts$diagnostics,
          raster_profile$diagnostics
        ))
      ))
    }
    .builder_profile_spatial_coordinate_record(
      name = labels[[index]],
      coordinates = extracted$coordinates,
      assay = .builder_profile_spatial_image_assay(image),
      context = context,
      raster = .builder_profile_spatial_image_raster(image),
      source = extracted$source,
      barcode_ids = extracted$barcodes
    )
  })
  names(sections) <- section_keys
  reduction_sections <- Filter(
    function(section) identical(section$kind, "spatial_reduction"),
    builder_spatial_alignment_sections(object)
  )
  reduction_sections <- lapply(reduction_sections, function(section) {
    embeddings <- SeuratObject::Embeddings(object[[section$source_id]])
    coordinates <- data.frame(
      x = as.numeric(embeddings[, 1L]),
      y = as.numeric(embeddings[, 2L]),
      row.names = rownames(embeddings),
      stringsAsFactors = FALSE
    )
    assay <- tryCatch(
      methods::slot(object[[section$source_id]], "assay.used"),
      error = function(error) context$default_assay
    )
    .builder_profile_spatial_coordinate_record(
      name = section$id,
      coordinates = coordinates,
      assay = assay,
      context = context,
      source = paste0("reduction.", section$source_id),
      barcode_ids = rownames(coordinates)
    )
  })
  names(reduction_sections) <- vapply(
    reduction_sections,
    `[[`,
    character(1),
    "name"
  )
  sections <- c(reduction_sections, sections)
  if (!length(sections)) {
    return(.builder_content_spatial_record(
      detected = FALSE,
      valid = TRUE,
      normalized = empty_normalized,
      diagnostics = character(),
      requirements = requirements,
      page_candidates = character()
    ))
  }
  valid_sections <- vapply(
    sections,
    function(section) isTRUE(section$valid),
    logical(1)
  )
  diagnostics <- c(
    if (!names_valid) "invalid_section_names" else character(),
    if (section_names_truncated) {
      "oversized_section_names"
    } else {
      character()
    },
    unique(unlist(lapply(
      sections[!valid_sections],
      function(section) section$diagnostics
    )))
  )
  valid <- names_valid && !section_names_truncated && all(valid_sections)
  candidate <- names_valid && !section_names_truncated && any(valid_sections)
  section_count <- length(sections)
  invalid_section_indices <- which(!valid_sections)
  invalid_section_preview <- lapply(
    utils::head(
      invalid_section_indices,
      .builder_content_spatial_section_limit
    ),
    function(index) {
      list(
        name = sections[[index]]$name,
        diagnostics = sections[[index]]$diagnostics
      )
    }
  )
  section_preview <- utils::head(
    sections,
    .builder_content_spatial_section_limit
  )

  .builder_content_spatial_record(
    detected = TRUE,
    valid = valid,
    normalized = list(
      section_count = section_count,
      valid_section_count = sum(valid_sections),
      invalid_section_count = length(invalid_section_indices),
      invalid_section_preview = invalid_section_preview,
      invalid_section_preview_truncated = length(invalid_section_indices) >
        .builder_content_spatial_section_limit,
      sections = section_preview,
      sections_truncated = section_count >
        .builder_content_spatial_section_limit,
      section_names_truncated = section_names_truncated
    ),
    diagnostics = diagnostics,
    requirements = requirements,
    page_candidates = if (candidate) "spatial" else character()
  )
}

# Trekker ----

.builder_profile_trekker_requirements <- function() {
  c(
    "canonical_barcode_subset",
    "aligned_finite_spatial_and_umap_coordinates",
    "zero_based_cluster_lookup",
    "aligned_confidence_vectors",
    "quantized_continuous_fields",
    "known_moran_genes",
    "embedded_raster_evidence_only",
    "bounded_profile_summary"
  )
}

.builder_profile_trekker_absent <- function() {
  .builder_content_spatial_record(
    detected = FALSE,
    valid = TRUE,
    normalized = list(),
    diagnostics = character(),
    requirements = .builder_profile_trekker_requirements(),
    page_candidates = character()
  )
}

.builder_profile_trekker_meta_valid <- function(meta, count) {
  if (!is.list(meta) || is.object(meta)) {
    return(FALSE)
  }
  numeric_fields <- c("n_cells_full", "n_cells", "n_genes_obj")
  text_fields <- c("unit")
  if (
    !all(numeric_fields %in% names(meta)) ||
      !all(text_fields %in% names(meta))
  ) {
    return(FALSE)
  }
  numeric_valid <- all(vapply(
    meta[numeric_fields],
    .builder_content_spatial_number,
    logical(1)
  ))
  text_valid <- all(vapply(
    meta[text_fields],
    .builder_content_spatial_text,
    logical(1)
  ))
  numeric_valid &&
    text_valid &&
    meta$n_cells == count &&
    meta$n_cells_full >= meta$n_cells &&
    meta$n_genes_obj > 0
}

.builder_profile_trekker_qc_valid <- function(qc) {
  if (!is.list(qc) || is.object(qc)) {
    return(FALSE)
  }
  text_fields <- c("sample_id", "assay", "tile_id", "eps", "min_sb")
  numeric_fields <- c(
    "total_nuclei",
    "in_lib",
    "pct_in_lib",
    "pct_valid_sb",
    "positioned",
    "pct_positioned",
    "conf",
    "pct_conf",
    "pct_2plus",
    "o_1",
    "salv_2",
    "salv_3",
    "n_0",
    "n_1",
    "n_2",
    "n_3",
    "n_4p"
  )
  if (
    !all(text_fields %in% names(qc)) ||
      !all(numeric_fields %in% names(qc))
  ) {
    return(FALSE)
  }
  text_valid <- all(vapply(
    qc[text_fields],
    .builder_content_spatial_text,
    logical(1)
  ))
  numeric_valid <- all(vapply(
    qc[numeric_fields],
    .builder_content_spatial_number,
    logical(1)
  ))
  nonnegative <- numeric_valid &&
    all(
      unlist(qc[numeric_fields], use.names = FALSE) >= 0
    )
  percentages <- c(
    "pct_in_lib",
    "pct_valid_sb",
    "pct_positioned",
    "pct_conf",
    "pct_2plus"
  )
  percentage_valid <- numeric_valid &&
    all(
      unlist(qc[percentages], use.names = FALSE) >= 0 &
        unlist(qc[percentages], use.names = FALSE) <= 100
    )
  text_valid && nonnegative && percentage_valid
}

.builder_profile_trekker_confidence <- function(conf, count) {
  required <- c("prop_top", "prop_noise", "sb_total", "sb_umi_top")
  if (!.builder_content_spatial_names_valid(conf)) {
    return(list(
      valid = FALSE,
      diagnostics = "missing_confidence_fields",
      names = character()
    ))
  }
  missing <- setdiff(required, names(conf))
  if (length(missing)) {
    return(list(
      valid = FALSE,
      diagnostics = "missing_confidence_fields",
      names = intersect(required, names(conf))
    ))
  }
  vectors_valid <- all(vapply(
    conf[required],
    function(values) {
      is.numeric(values) &&
        length(values) == count &&
        !anyNA(values) &&
        all(is.finite(values))
    },
    logical(1)
  ))
  if (!vectors_valid) {
    return(list(
      valid = FALSE,
      diagnostics = "invalid_confidence_vectors",
      names = required
    ))
  }
  ranges_valid <- all(conf$prop_top >= 0 & conf$prop_top <= 1) &&
    all(conf$prop_noise >= 0 & conf$prop_noise <= 1) &&
    all(conf$sb_total >= 0) &&
    all(conf$sb_umi_top >= 0)
  list(
    valid = ranges_valid,
    diagnostics = if (ranges_valid) character() else "confidence_out_of_range",
    names = required
  )
}

.builder_profile_trekker_field_summary_valid <- function(by_type, celltype) {
  if (is.null(by_type)) {
    return(TRUE)
  }
  if (!is.list(by_type) || is.object(by_type)) {
    return(FALSE)
  }
  types <- character()
  for (entry in by_type) {
    if (!is.list(entry) || is.object(entry)) {
      return(FALSE)
    }
    if (
      !.builder_content_spatial_text(entry$type) ||
        !entry$type %in% celltype ||
        !.builder_content_spatial_number(entry$median)
    ) {
      return(FALSE)
    }
    if (
      !is.null(entry$n) &&
        (!.builder_content_spatial_number(entry$n) || entry$n < 0)
    ) {
      return(FALSE)
    }
    if (
      !is.null(entry$dispersed) &&
        (!.builder_content_spatial_number(entry$dispersed) ||
          entry$dispersed < 0 ||
          entry$dispersed > 1)
    ) {
      return(FALSE)
    }
    types <- c(types, entry$type)
  }
  !anyDuplicated(types)
}

.builder_profile_trekker_fields <- function(fields, count, celltype) {
  if (!.builder_content_spatial_names_valid(fields)) {
    return(list(
      valid = FALSE,
      diagnostics = "invalid_field_names",
      names = character(),
      names_truncated = FALSE
    ))
  }
  name_summary <- .builder_content_spatial_preview_summary(names(fields))
  diagnostics <- if (name_summary$item_truncated) {
    "oversized_field_names"
  } else {
    character()
  }
  for (field in fields) {
    if (!is.list(field) || is.object(field)) {
      diagnostics <- c(diagnostics, "invalid_field_values")
      next
    }
    values_valid <- is.numeric(field$v) &&
      length(field$v) == count &&
      !anyNA(field$v) &&
      all(is.finite(field$v)) &&
      all(field$v == floor(field$v)) &&
      all(field$v >= 0 & field$v <= 255)
    if (!values_valid) {
      diagnostics <- c(diagnostics, "invalid_field_values")
    }
    range_valid <- .builder_content_spatial_number(field$max) &&
      (is.null(field$min) || .builder_content_spatial_number(field$min)) &&
      (is.null(field$min) || field$min <= field$max)
    if (!range_valid) {
      diagnostics <- c(diagnostics, "invalid_field_range")
    }
    label_valid <- .builder_content_spatial_text(field$label) &&
      .builder_content_spatial_text(field$desc)
    if (!label_valid) {
      diagnostics <- c(diagnostics, "invalid_field_label")
    }
    if (
      !.builder_profile_trekker_field_summary_valid(field$by_type, celltype)
    ) {
      diagnostics <- c(diagnostics, "invalid_field_summary")
    }
  }
  diagnostics <- unique(diagnostics)
  list(
    valid = !length(diagnostics),
    diagnostics = diagnostics,
    names = name_summary$values,
    names_truncated = name_summary$truncated
  )
}

.builder_profile_trekker_moran <- function(moran, features) {
  if (!is.list(moran) || is.object(moran)) {
    return(list(
      valid = FALSE,
      diagnostics = "invalid_moran_value",
      genes = character(),
      genes_truncated = FALSE
    ))
  }
  diagnostics <- character()
  genes <- character()
  ranks <- numeric()
  for (entry in moran) {
    if (!is.list(entry) || is.object(entry)) {
      diagnostics <- c(diagnostics, "invalid_moran_value")
      next
    }
    rank_valid <- .builder_content_spatial_integerish(entry$rank) &&
      length(entry$rank) == 1L &&
      entry$rank > 0
    gene_valid <- .builder_content_spatial_text(entry$gene)
    value_valid <- .builder_content_spatial_number(entry$I)
    if (!rank_valid || !gene_valid || !value_valid) {
      diagnostics <- c(diagnostics, "invalid_moran_value")
    }
    if (gene_valid && !entry$gene %in% features) {
      diagnostics <- c(diagnostics, "unknown_moran_gene")
    }
    if (gene_valid) {
      genes <- c(genes, entry$gene)
    }
    if (rank_valid) {
      ranks <- c(ranks, entry$rank)
    }
  }
  if (anyDuplicated(genes) || anyDuplicated(ranks)) {
    diagnostics <- c(diagnostics, "invalid_moran_value")
  }
  gene_summary <- .builder_content_spatial_preview_summary(genes)
  if (gene_summary$item_truncated) {
    diagnostics <- c(diagnostics, "oversized_moran_genes")
  }
  list(
    valid = !length(diagnostics),
    diagnostics = unique(diagnostics),
    genes = gene_summary$values,
    genes_truncated = gene_summary$truncated
  )
}

.builder_profile_trekker_evidence_uri <- function(value) {
  .builder_content_spatial_text(value) &&
    grepl(
      paste0(
        "^data:image/(png|jpeg|gif|webp);base64,",
        "[A-Za-z0-9+/]+={0,2}$"
      ),
      value,
      ignore.case = TRUE,
      perl = TRUE
    )
}

.builder_profile_trekker_evidence <- function(evidence, barcodes) {
  if (!is.list(evidence) || is.object(evidence)) {
    return(list(
      valid = FALSE,
      diagnostics = "invalid_evidence_index",
      count = 0L,
      mime_types = character()
    ))
  }
  diagnostics <- character()
  cells <- integer()
  mime_types <- character()
  for (entry in evidence) {
    if (!is.list(entry) || is.object(entry)) {
      diagnostics <- c(diagnostics, "invalid_evidence_index")
      next
    }
    cell_valid <- .builder_content_spatial_integerish(entry$cell) &&
      length(entry$cell) == 1L &&
      entry$cell >= 0 &&
      entry$cell < length(barcodes)
    if (!cell_valid) {
      diagnostics <- c(diagnostics, "invalid_evidence_index")
    } else {
      cell <- as.integer(entry$cell)
      cells <- c(cells, cell)
      if (
        !.builder_content_spatial_text(entry$bc) ||
          !identical(entry$bc, barcodes[[cell + 1L]])
      ) {
        diagnostics <- c(diagnostics, "mismatched_evidence_barcode")
      }
    }
    if (!.builder_profile_trekker_evidence_uri(entry$img)) {
      diagnostics <- c(diagnostics, "unsafe_evidence_uri")
    } else {
      mime_types <- c(
        mime_types,
        sub(
          "^data:image/([^;]+);.*$",
          "\\1",
          entry$img,
          ignore.case = TRUE
        )
      )
    }
  }
  if (anyDuplicated(cells)) {
    diagnostics <- c(diagnostics, "invalid_evidence_index")
  }
  list(
    valid = !length(diagnostics),
    diagnostics = unique(diagnostics),
    count = length(evidence),
    mime_types = unique(tolower(mime_types))
  )
}

builder_profile_trekker_payload <- function(payload, context) {
  if (is.null(payload)) {
    return(.builder_profile_trekker_absent())
  }
  requirements <- .builder_profile_trekker_requirements()
  if (
    !is.list(payload) ||
      is.object(payload) ||
      !.builder_content_spatial_plain(payload)
  ) {
    return(.builder_content_spatial_record(
      detected = TRUE,
      valid = FALSE,
      normalized = list(),
      diagnostics = "invalid_payload_type",
      requirements = requirements,
      page_candidates = character()
    ))
  }

  required <- c(
    "meta",
    "qc",
    "barcodes",
    "x",
    "y",
    "ux",
    "uy",
    "clusters",
    "celltype",
    "fields",
    "conf",
    "moran",
    "evidence"
  )
  missing <- setdiff(required, names(payload))
  if (length(missing)) {
    return(.builder_content_spatial_record(
      detected = TRUE,
      valid = FALSE,
      normalized = list(
        missing_fields = .builder_content_spatial_preview(missing)
      ),
      diagnostics = "missing_trekker_fields",
      requirements = requirements,
      page_candidates = character()
    ))
  }

  barcodes_raw <- payload$barcodes
  barcodes <- if (is.character(barcodes_raw)) barcodes_raw else character()
  barcode_summary <- .builder_profile_spatial_barcode_summary(
    barcodes,
    context$cells
  )
  count <- length(barcodes)
  diagnostics <- c(
    if (!is.character(barcodes_raw) || barcode_summary$blank_count) {
      "invalid_barcodes"
    } else {
      character()
    },
    if (barcode_summary$duplicate_count) "duplicate_barcodes" else character(),
    if (
      barcode_summary$outside_count ||
        identical(barcode_summary$relation, "zero")
    ) {
      "unknown_barcodes"
    } else {
      character()
    }
  )

  coordinate_names <- c("x", "y", "ux", "uy")
  coordinate_types_valid <- all(vapply(
    payload[coordinate_names],
    is.numeric,
    logical(1)
  ))
  coordinate_lengths_valid <- all(
    vapply(
      payload[coordinate_names],
      length,
      integer(1)
    ) ==
      count
  )
  coordinate_finite <- coordinate_types_valid &&
    coordinate_lengths_valid &&
    all(vapply(
      payload[coordinate_names],
      function(values) !anyNA(values) && all(is.finite(values)),
      logical(1)
    ))
  if (!coordinate_types_valid) {
    diagnostics <- c(diagnostics, "invalid_coordinates")
  }
  if (!coordinate_lengths_valid) {
    diagnostics <- c(diagnostics, "misaligned_coordinates")
  }
  if (
    coordinate_types_valid &&
      coordinate_lengths_valid &&
      !coordinate_finite
  ) {
    diagnostics <- c(diagnostics, "non_finite_coordinates")
  }

  clusters <- payload$clusters
  clusters_valid <- .builder_content_spatial_integerish(clusters) &&
    length(clusters) == count &&
    length(clusters) > 0L &&
    all(clusters >= 0)
  if (!clusters_valid) {
    diagnostics <- c(diagnostics, "invalid_clusters")
  }
  celltype <- payload$celltype
  celltype_valid <- is.character(celltype) &&
    length(celltype) > 0L &&
    !anyNA(celltype) &&
    all(nzchar(celltype))
  if (!celltype_valid) {
    diagnostics <- c(diagnostics, "blank_celltype")
  }
  lookup_valid <- clusters_valid &&
    celltype_valid &&
    max(clusters) < length(celltype)
  if (clusters_valid && celltype_valid && !lookup_valid) {
    diagnostics <- c(diagnostics, "cluster_without_label")
  }

  confidence <- .builder_profile_trekker_confidence(payload$conf, count)
  fields <- .builder_profile_trekker_fields(
    payload$fields,
    count,
    if (celltype_valid) celltype else character()
  )
  moran <- .builder_profile_trekker_moran(payload$moran, context$features)
  evidence <- .builder_profile_trekker_evidence(payload$evidence, barcodes)
  meta_valid <- .builder_profile_trekker_meta_valid(payload$meta, count)
  qc_valid <- .builder_profile_trekker_qc_valid(payload$qc)
  sample_id_valid <- is.list(payload$qc) &&
    .builder_content_spatial_text(payload$qc$sample_id)
  sample_id_summary <- .builder_content_spatial_bound_text(
    if (sample_id_valid) payload$qc$sample_id else ""
  )
  diagnostics <- unique(c(
    diagnostics,
    confidence$diagnostics,
    fields$diagnostics,
    moran$diagnostics,
    evidence$diagnostics,
    if (!meta_valid) "invalid_meta" else character(),
    if (!qc_valid) "invalid_qc" else character(),
    if (sample_id_summary$truncated) {
      "oversized_sample_id"
    } else {
      character()
    }
  ))
  valid <- barcode_summary$valid &&
    count > 0L &&
    coordinate_types_valid &&
    coordinate_lengths_valid &&
    coordinate_finite &&
    lookup_valid &&
    confidence$valid &&
    fields$valid &&
    moran$valid &&
    evidence$valid &&
    meta_valid &&
    qc_valid &&
    !length(diagnostics)

  ranges <- lapply(coordinate_names, function(name) {
    values <- payload[[name]]
    if (
      is.numeric(values) &&
        length(values) == count &&
        !anyNA(values) &&
        all(is.finite(values))
    ) {
      unname(range(values))
    } else {
      numeric()
    }
  })
  names(ranges) <- coordinate_names
  normalized <- list(
    cell_count = count,
    dataset_cell_count = length(context$cells),
    barcode_relation = barcode_summary$relation,
    barcode_coverage = barcode_summary$coverage,
    coordinate_ranges = ranges,
    cluster_count = if (clusters_valid) length(unique(clusters)) else 0L,
    celltype_count = if (celltype_valid) length(celltype) else 0L,
    field_count = length(payload$fields),
    field_names = fields$names,
    field_names_truncated = fields$names_truncated,
    confidence_fields = confidence$names,
    moran_count = length(payload$moran),
    moran_genes = moran$genes,
    moran_genes_truncated = moran$genes_truncated,
    evidence_count = evidence$count,
    evidence_mime_types = evidence$mime_types,
    sample_id = if (sample_id_valid) sample_id_summary$value else NA_character_,
    sample_id_truncated = sample_id_summary$truncated,
    sample_id_characters = sample_id_summary$characters,
    sample_id_bytes = sample_id_summary$bytes
  )
  .builder_content_spatial_record(
    detected = TRUE,
    valid = valid,
    normalized = normalized,
    diagnostics = diagnostics,
    requirements = requirements,
    page_candidates = if (valid) "trekker" else character()
  )
}

# Public profiling entry ----

.builder_profile_trekker_from_misc <- function(misc, context) {
  raw_names <- attr(misc, "names", exact = TRUE)
  names_safe <- is.character(raw_names) &&
    !is.object(raw_names) &&
    !isS4(raw_names)
  if (!names_safe) {
    return(.builder_profile_trekker_absent())
  }

  attributes(raw_names) <- NULL
  trekker_index <- match("trekker", raw_names, nomatch = 0L)
  if (trekker_index == 0L) {
    return(.builder_profile_trekker_absent())
  }

  if (!is.list(misc) || is.object(misc) || isS4(misc)) {
    return(.builder_content_spatial_record(
      detected = TRUE,
      valid = FALSE,
      normalized = list(),
      diagnostics = "invalid_misc_container",
      requirements = .builder_profile_trekker_requirements(),
      page_candidates = character()
    ))
  }
  ordinary_names <- length(raw_names) == length(misc) &&
    !anyNA(raw_names) &&
    all(nzchar(raw_names)) &&
    !anyDuplicated(raw_names)
  if (!ordinary_names) {
    return(.builder_content_spatial_record(
      detected = TRUE,
      valid = FALSE,
      normalized = list(),
      diagnostics = "invalid_misc_container",
      requirements = .builder_profile_trekker_requirements(),
      page_candidates = character()
    ))
  }
  builder_profile_trekker_payload(.subset2(misc, trekker_index), context)
}

builder_profile_spatial_content <- function(object, context) {
  misc <- tryCatch(
    methods::slot(object, "misc"),
    error = function(error) list()
  )
  list(
    spatial = .builder_profile_spatial_record(object, context),
    trekker = .builder_profile_trekker_from_misc(misc, context)
  )
}
