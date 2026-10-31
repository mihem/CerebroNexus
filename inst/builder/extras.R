##----------------------------------------------------------------------------##
## Two things that attach to a data set without touching the expression matrix:
## supplementary tables, and a histology background for spatial data.
##
## Both are the kind of thing a collaborator asks for and that otherwise means
## going back to R: "can I see the DE table next to the UMAP", "can you put the
## H&E behind the spots". Neither needs an analysis to run.
##
## Pure: no Shiny.
##----------------------------------------------------------------------------##

## ---------------------------------------------------------------------------
## Supplementary tables -> @misc$extra_material$tables
## ---------------------------------------------------------------------------

builder_table_default_name <- function(filename) {
  name <- tools::file_path_sans_ext(basename(as.character(filename %||% "")))
  name <- trimws(name)
  if (nzchar(name)) name else "Table"
}

builder_table_unique_name <- function(name, existing = character()) {
  if (!name %in% existing) {
    return(name)
  }
  suffix <- 2L
  repeat {
    candidate <- paste0(name, " ", suffix)
    if (!candidate %in% existing) {
      return(candidate)
    }
    suffix <- suffix + 1L
  }
}

#' List the tables available in one uploaded file without reading their data.
builder_table_inventory <- function(path, filename = path) {
  if (!file.exists(path) || dir.exists(path)) {
    return(list(error = "File not found."))
  }
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("csv", "tsv", "txt")) {
    name <- builder_table_default_name(filename)
    return(stats::setNames(
      list(list(
        name = name,
        workbook_name = basename(filename),
        sheet_name = name,
        display_name = name,
        source_path = path
      )),
      name
    ))
  }
  if (!ext %in% c("xls", "xlsx", "xlsm")) {
    return(list(
      error = paste0(
        "Supported table formats are CSV, TSV, TXT, XLS, XLSX and XLSM, not .",
        ext,
        "."
      )
    ))
  }
  sheets <- suppressWarnings(try(readxl::excel_sheets(path), silent = TRUE))
  if (inherits(sheets, "try-error") || !length(sheets)) {
    return(list(error = "Could not read this Excel workbook."))
  }
  workbook <- builder_table_default_name(filename)
  records <- lapply(sheets, function(sheet) {
    list(
      name = paste(workbook, sheet, sep = " · "),
      workbook_name = basename(filename),
      sheet_name = sheet,
      display_name = sheet,
      sheet = sheet,
      source_path = path
    )
  })
  names(records) <- sheets
  records
}

builder_table_inventory_metadata <- function(path, filename = path) {
  records <- builder_table_inventory(path, filename)
  if (!is.null(records$error)) {
    return(records)
  }
  lapply(records, function(record) {
    record$source_path <- NULL
    record
  })
}

#' Read one selected table only when it becomes part of a build.
builder_read_table_source <- function(record) {
  if (is.data.frame(record$table)) {
    return(record)
  }
  path <- record$source_path %||% ""
  filename <- record$file_name %||% record$workbook_name %||% path
  if (!file.exists(path)) {
    return(list(error = paste0(filename, ": File not found.")))
  }
  extension <- tolower(tools::file_ext(filename))
  table <- suppressWarnings(try(
    if (extension %in% c("csv", "tsv", "txt")) {
      utils::read.table(
        path,
        header = TRUE,
        sep = if (extension == "csv") "," else "\t",
        stringsAsFactors = FALSE,
        check.names = FALSE,
        comment.char = "",
        quote = "\""
      )
    } else if (extension %in% c("xls", "xlsx", "xlsm")) {
      readxl::read_excel(path, sheet = record$sheet_name %||% record$sheet)
    } else {
      stop("Unsupported table format.")
    },
    silent = TRUE
  ))
  if (inherits(table, "try-error") || !is.data.frame(table)) {
    return(list(
      error = paste0(
        filename,
        ": ",
        record$sheet_name %||%
          record$sheet %||%
          record$display_name %||%
          "Table",
        " could not be read."
      )
    ))
  }
  if (!nrow(table) || !ncol(table)) {
    record$empty <- TRUE
    return(record)
  }
  record$table <- as.data.frame(table, stringsAsFactors = FALSE)
  record
}

builder_materialize_tables <- function(tables) {
  records <- lapply(tables %||% list(), builder_read_table_source)
  errors <- vapply(records, function(record) !is.null(record$error), logical(1))
  if (any(errors)) {
    stop(records[[which(errors)[[1L]]]]$error, call. = FALSE)
  }
  records <- Filter(function(record) !isTRUE(record$empty), records)
  if (!length(records)) {
    stop("No selected attachments contain tabular data.", call. = FALSE)
  }
  records
}

#' Safe client-side file metadata for compact Builder file lists.
builder_safe_file_name <- function(name, fallback = "File") {
  name <- as.character(name %||% character())
  if (length(name) != 1L || is.na(name) || !nzchar(name)) {
    return(fallback)
  }
  name <- basename(gsub("\\", "/", name, fixed = TRUE))
  if (nzchar(name)) name else fallback
}

builder_file_type_label <- function(name, type = NULL) {
  extension <- toupper(tools::file_ext(builder_safe_file_name(name, "")))
  if (nzchar(extension)) {
    return(extension)
  }
  type <- as.character(type %||% character())
  if (length(type) == 1L && !is.na(type) && nzchar(type)) {
    return(toupper(sub("^.*/", "", type)))
  }
  "FILE"
}

builder_file_human_size <- function(bytes) {
  bytes <- suppressWarnings(as.numeric(bytes %||% 0))
  if (length(bytes) != 1L || is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return("Size unavailable")
  }
  units <- c("bytes", "KB", "MB", "GB", "TB")
  unit <- 1L
  while (bytes >= 1024 && unit < length(units)) {
    bytes <- bytes / 1024
    unit <- unit + 1L
  }
  value <- if (unit == 1L) {
    round(bytes)
  } else {
    round(bytes, if (bytes < 10) 1L else 0L)
  }
  paste(format(value, trim = TRUE, scientific = FALSE), units[[unit]])
}

#' Put the collected tables on the object, where exportFromSeurat looks.
builder_attach_tables <- function(object, tables) {
  if (!length(tables)) {
    return(object)
  }
  existing <- object@misc$extra_material$tables
  if (is.null(existing)) {
    existing <- list()
  }
  table_index <- object@misc$extra_material$table_index
  if (is.null(table_index)) {
    table_index <- list()
  }
  for (t in tables) {
    existing[[t$name]] <- t$table
    table_index[[t$name]] <- list(
      workbook_name = t$workbook_name %||% basename(t$file_name %||% ""),
      file_name = t$file_name %||% "",
      sheet_name = t$sheet_name %||% t$sheet %||% t$name,
      display_name = t$display_name %||% t$sheet_name %||% t$sheet %||% t$name
    )
  }
  object@misc$extra_material$tables <- existing
  object@misc$extra_material$table_index <- table_index
  object
}

## ---------------------------------------------------------------------------
## Histology background -> the spatial slot of the written .crb
## ---------------------------------------------------------------------------

builder_alignment_defaults <- function() {
  list(
    dx = 0,
    dy = 0,
    scale = 1,
    rotation = 0,
    flip_x = FALSE,
    flip_y = FALSE,
    image_opacity = 0.8,
    point_opacity = 0.85,
    point_size = 5
  )
}

.builder_alignment_valid_bounds <- function(bounds) {
  is.list(bounds) &&
    all(c("xmin", "xmax", "ymin", "ymax") %in% names(bounds)) &&
    all(is.finite(as.numeric(unlist(bounds[c(
      "xmin",
      "xmax",
      "ymin",
      "ymax"
    )])))) &&
    bounds$xmax > bounds$xmin &&
    bounds$ymax > bounds$ymin
}

#' Expand a coordinate viewport to its displayed extent after rotation.
builder_alignment_rotated_bounds <- function(bounds, rotation = 0) {
  if (!.builder_alignment_valid_bounds(bounds)) {
    stop("Alignment requires finite, non-empty physical bounds.", call. = FALSE)
  }
  rotation <- suppressWarnings(as.numeric(rotation))
  if (length(rotation) != 1L || is.na(rotation) || !is.finite(rotation)) {
    stop("Alignment rotation must be finite.", call. = FALSE)
  }
  radians <- rotation * pi / 180
  width <- bounds$xmax - bounds$xmin
  height <- bounds$ymax - bounds$ymin
  extent_width <- abs(width * cos(radians)) + abs(height * sin(radians))
  extent_height <- abs(width * sin(radians)) + abs(height * cos(radians))
  centre_x <- (bounds$xmin + bounds$xmax) / 2
  centre_y <- (bounds$ymin + bounds$ymax) / 2
  list(
    xmin = centre_x - extent_width / 2,
    xmax = centre_x + extent_width / 2,
    ymin = centre_y - extent_height / 2,
    ymax = centre_y + extent_height / 2
  )
}

#' Fit the original image inside the physical coordinate viewport.
#'
#' The image is centred, aspect-preserving, and fills the available viewport.
builder_alignment_fit_bounds <- function(bounds, image_dimensions) {
  if (!.builder_alignment_valid_bounds(bounds)) {
    stop("Alignment requires finite, non-empty physical bounds.", call. = FALSE)
  }
  image_dimensions <- as.numeric(image_dimensions)
  if (
    length(image_dimensions) != 2L ||
      anyNA(image_dimensions) ||
      !all(is.finite(image_dimensions)) ||
      any(image_dimensions <= 0)
  ) {
    stop("Alignment requires positive image dimensions.", call. = FALSE)
  }
  available_width <- bounds$xmax - bounds$xmin
  available_height <- bounds$ymax - bounds$ymin
  fit <- min(
    available_width / image_dimensions[[1L]],
    available_height / image_dimensions[[2L]]
  )
  width <- image_dimensions[[1L]] * fit
  height <- image_dimensions[[2L]] * fit
  centre_x <- (bounds$xmin + bounds$xmax) / 2
  centre_y <- (bounds$ymin + bounds$ymax) / 2
  list(
    xmin = centre_x - width / 2,
    xmax = centre_x + width / 2,
    ymin = centre_y - height / 2,
    ymax = centre_y + height / 2
  )
}

.builder_alignment_parameters <- function(parameters = list()) {
  supplied <- parameters %||% list()
  parameters <- builder_alignment_defaults()
  shared <- intersect(names(parameters), names(supplied))
  parameters[shared] <- supplied[shared]
  numeric_fields <- c(
    "dx",
    "dy",
    "scale",
    "rotation",
    "image_opacity",
    "point_opacity",
    "point_size"
  )
  for (name in numeric_fields) {
    value <- suppressWarnings(as.numeric(parameters[[name]]))
    if (length(value) != 1L || is.na(value) || !is.finite(value)) {
      stop("Alignment parameters must be finite.", call. = FALSE)
    }
    parameters[[name]] <- value
  }
  parameters$dx <- round(parameters$dx)
  parameters$dy <- round(parameters$dy)
  if (
    parameters$scale <= 0 ||
      parameters$point_size <= 0 ||
      parameters$image_opacity < 0 ||
      parameters$image_opacity > 1 ||
      parameters$point_opacity < 0 ||
      parameters$point_opacity > 1
  ) {
    stop(
      "Alignment scale, opacity, or point size is outside its range.",
      call. = FALSE
    )
  }
  parameters$flip_x <- isTRUE(parameters$flip_x)
  parameters$flip_y <- isTRUE(parameters$flip_y)
  parameters
}

#' Choose a slider step that preserves an existing positive image scale.
#'
#' Keep the familiar 0.02 increment when the restored value lies on that
#' grid. Older projects can contain smaller or more precise positive values;
#' IonRangeSlider would otherwise round them during initialization. In that
#' case, use the stored value's decimal precision so the control never becomes
#' an accidental migration of the persisted transform.
builder_alignment_scale_step <- function(scale, default = 0.02) {
  scale <- suppressWarnings(as.numeric(scale))
  default <- suppressWarnings(as.numeric(default))
  if (
    length(default) != 1L ||
      is.na(default) ||
      !is.finite(default) ||
      default <= 0
  ) {
    stop("Alignment scale step must be positive and finite.", call. = FALSE)
  }
  if (
    length(scale) != 1L ||
      is.na(scale) ||
      !is.finite(scale) ||
      scale <= 0
  ) {
    return(default)
  }
  ratio <- scale / default
  if (isTRUE(all.equal(ratio, round(ratio), tolerance = 1e-12))) {
    return(default)
  }
  rendered <- format(
    scale,
    scientific = FALSE,
    trim = TRUE,
    digits = 15L
  )
  rendered <- sub("0+$", "", rendered)
  decimal <- regexpr("\\.", rendered)
  digits <- if (decimal[[1L]] < 0L) {
    0L
  } else {
    nchar(rendered) - decimal[[1L]]
  }
  min(default, 10^(-digits))
}

#' Derive alignment slider ranges without discarding a restored transform.
#'
#' During project hydration the saved alignment is available before a fresh
#' spatial preview has reported its coordinate bounds.  Slider updates must
#' therefore include both the known coordinate span and the restored value;
#' otherwise Shiny clamps an out-of-range saved offset back to the temporary
#' default range.
builder_alignment_control_ranges <- function(record = NULL, bounds = NULL) {
  parameters <- .builder_alignment_parameters(record %||% list())
  saved_bounds <- if (is.list(record)) {
    record$base_bounds %||% record$bounds
  } else {
    NULL
  }
  effective_bounds <- if (.builder_alignment_valid_bounds(bounds)) {
    bounds
  } else if (.builder_alignment_valid_bounds(saved_bounds)) {
    saved_bounds
  } else {
    NULL
  }
  span_x <- if (is.null(effective_bounds)) {
    1
  } else {
    effective_bounds$xmax - effective_bounds$xmin
  }
  span_y <- if (is.null(effective_bounds)) {
    1
  } else {
    effective_bounds$ymax - effective_bounds$ymin
  }
  nice <- function(value) max(signif(value, 2), .Machine$double.eps)
  x_limit <- nice(max(1, abs(parameters$dx), abs(span_x)))
  y_limit <- nice(max(1, abs(parameters$dy), abs(span_y)))
  list(
    dx = list(
      min = -x_limit,
      max = x_limit,
      step = 10
    ),
    dy = list(
      min = -y_limit,
      max = y_limit,
      step = 10
    )
  )
}

#' Apply translation and scale to the immutable default-fit bounds.
builder_alignment_transform_bounds <- function(
  base_bounds,
  parameters = list()
) {
  if (!.builder_alignment_valid_bounds(base_bounds)) {
    stop("Alignment base bounds are invalid.", call. = FALSE)
  }
  parameters <- .builder_alignment_parameters(parameters)
  builder_adjust_bounds(
    base_bounds,
    dx = parameters$dx,
    dy = parameters$dy,
    scale = parameters$scale
  )
}

#' Center an existing image on the active spatial viewport.
builder_alignment_center <- function(record, bounds) {
  if (
    !is.list(record) ||
      !.builder_alignment_valid_bounds(record$base_bounds) ||
      !.builder_alignment_valid_bounds(bounds)
  ) {
    stop(
      "Alignment centering requires valid image and viewport bounds.",
      call. = FALSE
    )
  }
  parameters <- .builder_alignment_parameters(record)
  parameters$dx <- round(
    (bounds$xmin +
      bounds$xmax -
      record$base_bounds$xmin -
      record$base_bounds$xmax) /
      2
  )
  parameters$dy <- round(
    (bounds$ymin +
      bounds$ymax -
      record$base_bounds$ymin -
      record$base_bounds$ymax) /
      2
  )
  record[names(parameters)] <- parameters
  record$bounds <- builder_alignment_transform_bounds(
    builder_alignment_oriented_bounds(record$base_bounds, record),
    parameters
  )
  record$viewport_bounds <- bounds
  record
}

#' Expand the immutable source-image bounds to the encoded rotation canvas.
#'
#' Arbitrary image rotation grows a transparent canvas around the source. The
#' encoded canvas must keep the same data-units-per-pixel in both axes; fitting
#' that larger canvas back into the unrotated bounds would stretch it.
builder_alignment_oriented_bounds <- function(base_bounds, image_geometry) {
  if (!.builder_alignment_valid_bounds(base_bounds)) {
    stop("Alignment base bounds are invalid.", call. = FALSE)
  }
  geometry <- image_geometry %||% list()
  values <- suppressWarnings(as.numeric(c(
    geometry$source_width,
    geometry$source_height,
    geometry$extent_width,
    geometry$extent_height
  )))
  if (
    length(values) != 4L ||
      anyNA(values) ||
      any(!is.finite(values)) ||
      any(values <= 0)
  ) {
    return(base_bounds)
  }
  source_width <- values[[1L]]
  source_height <- values[[2L]]
  extent_width <- values[[3L]]
  extent_height <- values[[4L]]
  units_per_pixel <- mean(c(
    (base_bounds$xmax - base_bounds$xmin) / source_width,
    (base_bounds$ymax - base_bounds$ymin) / source_height
  ))
  centre_x <- (base_bounds$xmin + base_bounds$xmax) / 2
  centre_y <- (base_bounds$ymin + base_bounds$ymax) / 2
  width <- extent_width * units_per_pixel
  height <- extent_height * units_per_pixel
  list(
    xmin = centre_x - width / 2,
    xmax = centre_x + width / 2,
    ymin = centre_y - height / 2,
    ymax = centre_y + height / 2
  )
}

#' Create the canonical per-section alignment record.
builder_alignment_record <- function(
  source,
  source_uri = NULL,
  uri = NULL,
  base_bounds,
  parameters = list(),
  image_geometry = NULL,
  section = list(),
  source_path = NULL,
  project_asset = NULL
) {
  parameters <- .builder_alignment_parameters(parameters)
  oriented_bounds <- builder_alignment_oriented_bounds(
    base_bounds,
    image_geometry
  )
  record <- c(
    list(
      source = source,
      source_path = source_path,
      project_asset = project_asset,
      source_uri = source_uri,
      uri = uri,
      base_bounds = base_bounds,
      bounds = builder_alignment_transform_bounds(oriented_bounds, parameters)
    ),
    parameters,
    list(
      section_id = as.character(section$id %||% "")[[1L]],
      section_kind = as.character(section$kind %||% "spatial")[[1L]]
    )
  )
  record[vapply(record, is.null, logical(1))] <- NULL
  record
}

.builder_alignment_roi_scope <- function(record) {
  fields <- c("roi_field", "roi_value")
  present <- fields %in% names(record)
  if (!any(present)) {
    return(list())
  }
  if (!all(present)) {
    stop(
      "ROI image scope requires both roi_field and roi_value.",
      call. = FALSE
    )
  }
  values <- record[fields]
  valid <- vapply(
    values,
    function(value) {
      is.character(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        nzchar(trimws(value))
    },
    logical(1)
  )
  if (!all(valid)) {
    stop("ROI image scope values must be non-empty strings.", call. = FALSE)
  }
  lapply(values, trimws)
}

#' Upgrade an older URI/bounds record without invalidating existing projects.
builder_alignment_normalize <- function(
  record,
  section_id = NULL,
  section_kind = NULL
) {
  if (!is.list(record)) {
    return(NULL)
  }
  source_path <- record[["source_path", exact = TRUE]]
  project_asset <- record[["project_asset", exact = TRUE]]
  legacy_uri <- record[["source_uri", exact = TRUE]] %||%
    record[["uri", exact = TRUE]]
  source <- record[["source", exact = TRUE]]
  has_file <- is.character(source_path) &&
    length(source_path) == 1L &&
    !is.na(source_path) &&
    nzchar(source_path)
  has_project_asset <- is.list(project_asset)
  has_legacy_uri <- is.character(legacy_uri) &&
    length(legacy_uri) == 1L &&
    !is.na(legacy_uri) &&
    nzchar(legacy_uri)
  if (
    is.null(record$base_bounds %||% record$bounds) ||
      !(has_file || has_project_asset || has_legacy_uri)
  ) {
    return(NULL)
  }
  parameters <- .builder_alignment_parameters(record)
  base_bounds <- record$base_bounds %||% record$bounds
  normalized <- builder_alignment_record(
    source = source %||% list(name = "Tissue image", type = "image/png"),
    source_uri = legacy_uri,
    uri = record[["uri", exact = TRUE]],
    base_bounds = base_bounds,
    parameters = parameters,
    image_geometry = record,
    section = list(
      id = section_id %||% record$section_id %||% "",
      kind = section_kind %||% record$section_kind %||% "spatial"
    ),
    source_path = source_path %||% NULL,
    project_asset = project_asset %||% NULL
  )
  carried <- setdiff(names(record), c(names(normalized), "saved"))
  normalized[carried] <- record[carried]
  normalized[c("roi_field", "roi_value")] <- NULL
  scope <- .builder_alignment_roi_scope(record)
  normalized[names(scope)] <- scope
  normalized
}

#' The placement a picture arrived with (scale factors from the source file),
#' or NULL for pictures that were added by hand.
builder_alignment_source_registration <- function(record) {
  registration <- record$source_registration %||% NULL
  if (
    !is.list(registration) ||
      !.builder_alignment_valid_bounds(registration$base_bounds)
  ) {
    return(NULL)
  }
  registration
}

#' Reset one image to its deterministic fit without changing point appearance.
#' A picture registered by its source file returns to that registration.
builder_alignment_reset <- function(record) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  parameters <- builder_alignment_defaults()
  parameters[c("point_opacity", "point_size")] <- normalized[c(
    "point_opacity",
    "point_size"
  )]
  registration <- builder_alignment_source_registration(normalized)
  base_bounds <- normalized$base_bounds
  if (!is.null(registration)) {
    base_bounds <- registration$base_bounds
    parameters$flip_x <- isTRUE(registration$flip_x)
    parameters$flip_y <- isTRUE(registration$flip_y)
  }
  reset <- builder_alignment_record(
    source = normalized$source,
    source_uri = normalized$source_uri,
    uri = normalized$source_uri,
    base_bounds = base_bounds,
    parameters = parameters,
    image_geometry = list(
      source_width = normalized$source_width,
      source_height = normalized$source_height,
      extent_width = normalized$source_width,
      extent_height = normalized$source_height
    ),
    section = list(
      id = normalized$section_id,
      kind = normalized$section_kind
    ),
    source_path = normalized$source_path %||% NULL,
    project_asset = normalized$project_asset %||% NULL
  )
  carried <- intersect(
    c(
      "bytes",
      "width",
      "height",
      "source_width",
      "source_height",
      "extent_width",
      "extent_height",
      "display_width",
      "display_height",
      "source_content_md5",
      "image_label",
      "source_registration",
      "roi_field",
      "roi_value",
      "outside",
      "total"
    ),
    names(normalized)
  )
  reset[carried] <- normalized[carried]
  if (!is.null(reset$source_width) && !is.null(reset$source_height)) {
    reset$extent_width <- reset$source_width
    reset$extent_height <- reset$source_height
    reset$display_width <- reset$source_width
    reset$display_height <- reset$source_height
    reset$width <- reset$source_width
    reset$height <- reset$source_height
  }
  reset
}

#' Viewer image settings that draw exactly what the Builder preview shows.
#'
#' The Builder preview (builder-spatial-canvas.js) flips and scales the image
#' in its own frame, rotates it about its own centre c, then moves it by
#' (dx, dy). The Viewer (cell_views.js) rotates the image bounds about the
#' data origin, so a rotated image follows cells rotated by
#' spatial_plot_rotation, and flips after rotating. Both are kept; only the
#' parameters handed to the Viewer are converted. With R the rotation:
#'   Builder: c + d + R(t) F s (p - c)
#'   Viewer:  R(t') c + o + s F R(t') (p - c)
#' F R(t') equals R(t) F when t' = t for no or both flips and t' = -t for one
#' flip, and the centres agree when o = c + d - R(t') c.
builder_alignment_viewer_setting <- function(record) {
  bounds <- record$base_bounds
  centre_x <- (bounds$xmin + bounds$xmax) / 2
  centre_y <- (bounds$ymin + bounds$ymax) / 2
  rotation <- record$rotation
  if (xor(isTRUE(record$flip_x), isTRUE(record$flip_y))) {
    rotation <- -rotation
  }
  angle <- rotation * pi / 180
  list(
    flip_x = record$flip_x,
    flip_y = record$flip_y,
    scale_x = record$scale,
    scale_y = record$scale,
    offset_x = centre_x +
      record$dx -
      (cos(angle) * centre_x - sin(angle) * centre_y),
    offset_y = centre_y +
      record$dy -
      (sin(angle) * centre_x + cos(angle) * centre_y),
    rotation = rotation,
    image_opacity = record$image_opacity,
    point_opacity = record$point_opacity,
    point_size = record$point_size
  )
}

#' The small alignment contract written to a generated Viewer payload.
builder_alignment_payload <- function(record) {
  normalized <- builder_alignment_normalize(record)
  if (is.null(normalized)) {
    return(NULL)
  }
  payload <- list(
    source = basename(as.character(normalized$source$name %||% "Tissue image")),
    builder_managed = TRUE,
    dx = normalized$dx,
    dy = normalized$dy,
    scale = normalized$scale,
    rotation = normalized$rotation,
    flip_x = normalized$flip_x,
    flip_y = normalized$flip_y,
    image_opacity = normalized$image_opacity,
    point_opacity = normalized$point_opacity,
    point_size = normalized$point_size
  )
  scope <- intersect(c("roi_field", "roi_value"), names(normalized))
  payload[scope] <- normalized[scope]
  if (.builder_alignment_valid_bounds(normalized$viewport_bounds)) {
    payload$viewport_bounds <- normalized$viewport_bounds[c(
      "xmin",
      "xmax",
      "ymin",
      "ymax"
    )]
  }
  payload
}

#' Keep Trekker's physical image out of Seurat spatial section matching.
builder_image_collection_normalize <- function(images) {
  images <- images %||% list()
  if (!is.list(images) || is.object(images)) {
    stop("Spatial image collection must be a named list.", call. = FALSE)
  }
  sections <- names(images)
  if (is.null(sections)) {
    sections <- character()
  }
  if (
    length(images) &&
      (anyNA(sections) || any(!nzchar(sections)) || anyDuplicated(sections))
  ) {
    stop("Spatial section names must be unique and non-empty.", call. = FALSE)
  }
  normalized <- list()
  for (section_id in sections) {
    section <- images[[section_id]]
    legacy <- builder_alignment_normalize(section, section_id = section_id)
    if (!is.null(legacy)) {
      label <- builder_safe_file_name(
        legacy$source$name %||% "Tissue image",
        fallback = "Tissue image"
      )
      legacy$image_label <- label
      normalized[[section_id]] <- stats::setNames(list(legacy), label)
      next
    }
    if (!is.list(section) || is.object(section)) {
      stop(
        "Each Spatial section must contain named image records.",
        call. = FALSE
      )
    }
    labels <- names(section)
    if (
      is.null(labels) ||
        anyNA(labels) ||
        any(!nzchar(trimws(labels)))
    ) {
      stop("Spatial image labels must be non-empty.", call. = FALSE)
    }
    if (anyDuplicated(labels)) {
      stop(
        "Spatial image labels must be unique within each section.",
        call. = FALSE
      )
    }
    records <- lapply(labels, function(label) {
      record <- builder_alignment_normalize(
        section[[label]],
        section_id = section_id,
        section_kind = "spatial"
      )
      if (is.null(record)) {
        stop(
          "Spatial image records must contain an image file and bounds.",
          call. = FALSE
        )
      }
      display_label <- record$image_label %||% label
      if (
        !is.character(display_label) ||
          length(display_label) != 1L ||
          is.na(display_label) ||
          !nzchar(trimws(display_label))
      ) {
        stop("Spatial image labels must be non-empty.", call. = FALSE)
      }
      record$image_label <- trimws(display_label)
      record
    })
    names(records) <- labels
    scopes <- vapply(
      records,
      function(record) {
        as.character(record$roi_value %||% "")[[1L]]
      },
      character(1)
    )
    display_labels <- vapply(records, `[[`, character(1), "image_label")
    if (anyDuplicated(data.frame(scopes, display_labels))) {
      stop(
        "Spatial image labels must be unique within the same ROI.",
        call. = FALSE
      )
    }
    normalized[[section_id]] <- records
  }
  normalized
}

builder_image_collection_choices <- function(images, section, roi = "") {
  images <- builder_image_collection_normalize(images)
  records <- images[[section]] %||% list()
  keys <- names(records) %||% character()
  roi <- as.character(roi %||% "")[[1L]]
  scopes <- vapply(
    records,
    function(record) {
      as.character(record$roi_value %||% "")[[1L]]
    },
    character(1)
  )
  keys <- keys[scopes == roi]
  stats::setNames(
    keys,
    vapply(
      keys,
      function(key) {
        records[[key]]$image_label %||% key
      },
      character(1)
    )
  )
}

builder_image_collection_flatten <- function(images) {
  images <- builder_image_collection_normalize(images)
  unlist(
    lapply(names(images), function(section_id) {
      lapply(names(images[[section_id]]), function(image_label) {
        record <- images[[section_id]][[image_label]]
        c(
          list(
            section_id = section_id,
            image_key = image_label,
            image_label = record$image_label %||% image_label
          ),
          record[setdiff(names(record), "image_label")]
        )
      })
    }),
    recursive = FALSE,
    use.names = FALSE
  )
}

builder_image_collection_rename <- function(images, section, from, to) {
  images <- builder_image_collection_normalize(images)
  to <- trimws(as.character(to %||% ""))
  section_images <- images[[section]] %||% list()
  if (!from %in% names(section_images)) {
    stop("The spatial image to rename does not exist.", call. = FALSE)
  }
  record <- section_images[[from]]
  scope <- as.character(record$roi_value %||% "")[[1L]]
  other_keys <- setdiff(names(section_images), from)
  duplicate <- any(vapply(
    other_keys,
    function(key) {
      other <- section_images[[key]]
      identical(as.character(other$roi_value %||% "")[[1L]], scope) &&
        identical(other$image_label %||% key, to)
    },
    logical(1)
  ))
  if (!nzchar(to) || duplicate) {
    stop("Spatial image labels must be non-empty and unique.", call. = FALSE)
  }
  if (identical(record$image_label %||% from, to)) {
    attr(images, "renamed_image_key") <- from
    return(images)
  }
  position <- match(from, names(section_images))
  key <- if (!to %in% other_keys) {
    to
  } else {
    utils::tail(make.unique(c(other_keys, to)), 1L)
  }
  record$image_label <- to
  section_images[[position]] <- record
  names(section_images)[[position]] <- key
  images[[section]] <- section_images
  attr(images, "renamed_image_key") <- key
  images
}

builder_image_collection_remove <- function(images, section, label) {
  images <- builder_image_collection_normalize(images)
  section_images <- images[[section]] %||% list()
  if (!label %in% names(section_images)) {
    return(images)
  }
  section_images[[label]] <- NULL
  images[[section]] <- if (length(section_images)) section_images else NULL
  images
}

builder_coordinate_drafts_get <- function(drafts, dataset, section) {
  drafts <- drafts %||% list()
  drafts[[dataset]][[section]] %||% NULL
}

builder_coordinate_drafts_put <- function(
  drafts,
  dataset,
  snapshot_identity,
  section,
  spec,
  sequence = NULL,
  force = FALSE
) {
  scalar_text <- function(value) {
    is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  }
  if (
    !scalar_text(dataset) ||
      !scalar_text(snapshot_identity) ||
      !scalar_text(section)
  ) {
    stop("Coordinate draft identity must be non-empty text.", call. = FALSE)
  }
  normalized <- .spx_coordinate_transform_spec_normalize(
    spec,
    context = "Coordinate draft"
  )
  current <- builder_coordinate_drafts_get(drafts, dataset, section)
  if (!isTRUE(force)) {
    if (
      !is.numeric(sequence) ||
        length(sequence) != 1L ||
        is.na(sequence) ||
        !is.finite(sequence) ||
        sequence < 1 ||
        sequence != floor(sequence)
    ) {
      stop(
        "Coordinate draft sequence must be a positive integer.",
        call. = FALSE
      )
    }
    if (!is.null(current) && sequence <= current$sequence) {
      return(list(drafts = drafts, accepted = FALSE, record = current))
    }
  } else {
    sequence <- current$sequence %||% 0
  }
  record <- list(
    dataset = dataset,
    snapshot_identity = snapshot_identity,
    section = section,
    spec = normalized,
    sequence = as.numeric(sequence)
  )
  drafts <- drafts %||% list()
  drafts[[dataset]][[section]] <- record
  list(drafts = drafts, accepted = TRUE, record = record)
}

builder_coordinate_drafts_drop <- function(drafts, dataset, section = NULL) {
  drafts <- drafts %||% list()
  if (is.null(section)) {
    drafts[[dataset]] <- NULL
    return(drafts)
  }
  drafts[[dataset]][[section]] <- NULL
  if (!length(drafts[[dataset]] %||% list())) {
    drafts[[dataset]] <- NULL
  }
  drafts
}

builder_coordinate_drafts_prune <- function(drafts, entries) {
  drafts <- drafts %||% list()
  identities <- stats::setNames(
    vapply(
      entries,
      function(entry) as.character(entry$snapshot_identity %||% "")[[1L]],
      character(1)
    ),
    vapply(entries, function(entry) entry$id, character(1))
  )
  removed <- character()
  for (dataset in names(drafts) %||% character()) {
    for (section in names(drafts[[dataset]]) %||% character()) {
      record <- drafts[[dataset]][[section]]
      if (
        !dataset %in% names(identities) ||
          !identical(record$snapshot_identity, identities[[dataset]])
      ) {
        removed <- c(removed, paste(dataset, section, sep = "::"))
        drafts <- builder_coordinate_drafts_drop(drafts, dataset, section)
      }
    }
  }
  list(drafts = drafts, removed = removed)
}

builder_coordinate_drafts_apply_entry <- function(
  entry,
  records,
  snapshot_identity
) {
  records <- records %||% list()
  transforms <- entry$settings$spatial_coordinate_transforms %||% list()
  if (is.null(names(transforms))) {
    transforms <- list()
  }
  images <- entry$settings$images %||% list()
  changed_sections <- character()
  for (section in names(records) %||% character()) {
    record <- records[[section]]
    if (
      !identical(record$dataset, entry$id) ||
        !identical(record$snapshot_identity, snapshot_identity) ||
        !identical(record$section, section)
    ) {
      next
    }
    spec <- .spx_coordinate_transform_spec_normalize(
      record$spec,
      context = paste0("Coordinate draft ", section)
    )
    previous <- transforms[[section]]
    identity <- identical(spec$rotation_degrees, 0) &&
      identical(spec$scale, 1)
    if (identity) {
      transforms[[section]] <- NULL
    } else {
      transforms[[section]] <- spec
    }
    if (!identical(previous, transforms[[section]])) {
      changed_sections <- c(changed_sections, section)
    }
  }
  entry$settings$spatial_coordinate_transforms <- transforms
  entry$settings$images <- images
  list(
    entry = entry,
    sections = changed_sections,
    changed = length(changed_sections) > 0L
  )
}

builder_roi_drafts_drop <- function(drafts, dataset, section = NULL) {
  drafts <- drafts %||% list()
  if (is.null(section)) {
    drafts[[dataset]] <- NULL
    return(drafts)
  }
  drafts[[dataset]][[section]] <- NULL
  if (!length(drafts[[dataset]] %||% list())) {
    drafts[[dataset]] <- NULL
  }
  drafts
}

builder_roi_drafts_prune <- function(drafts, entries) {
  drafts <- drafts %||% list()
  identities <- stats::setNames(
    vapply(
      entries,
      function(entry) as.character(entry$snapshot_identity %||% "")[[1L]],
      character(1)
    ),
    vapply(entries, function(entry) entry$id, character(1))
  )
  for (dataset in names(drafts) %||% character()) {
    for (section in names(drafts[[dataset]]) %||% character()) {
      records <- drafts[[dataset]][[section]] %||% list()
      valid <- vapply(
        records,
        function(record) {
          dataset %in%
            names(identities) &&
            identical(record$snapshot_identity, identities[[dataset]])
        },
        logical(1)
      )
      drafts[[dataset]][[section]] <- records[valid]
      if (!length(drafts[[dataset]][[section]])) {
        drafts <- builder_roi_drafts_drop(drafts, dataset, section)
      }
    }
  }
  drafts
}

builder_roi_drafts_apply_entry <- function(
  entry,
  coordinate_records,
  appearance_records,
  snapshot_identity
) {
  settings <- entry$settings$spatial_roi_settings %||% list()
  before <- settings
  sections <- union(
    names(coordinate_records) %||% character(),
    names(appearance_records) %||% character()
  )
  changed_sections <- character()
  for (section in sections) {
    coordinates <- coordinate_records[[section]] %||% list()
    appearances <- appearance_records[[section]] %||% list()
    rois <- union(
      names(coordinates) %||% character(),
      names(appearances) %||% character()
    )
    for (roi in rois) {
      coordinate <- coordinates[[roi]]
      appearance <- appearances[[roi]]
      valid_record <- function(record) {
        is.list(record) &&
          identical(record$dataset, entry$id) &&
          identical(record$snapshot_identity, snapshot_identity) &&
          identical(record$section, section) &&
          identical(record$roi, roi)
      }
      leaf <- settings[[section]][[roi]] %||%
        list(
          rotation_degrees = 0,
          point_opacity = builder_alignment_defaults()$point_opacity,
          point_size = builder_alignment_defaults()$point_size
        )
      if (valid_record(coordinate)) {
        spec <- .spx_coordinate_transform_spec_normalize(
          coordinate$spec,
          context = paste0("ROI coordinate draft ", section, "/", roi)
        )
        leaf$rotation_degrees <- spec$rotation_degrees
      }
      if (valid_record(appearance)) {
        leaf$point_opacity <- appearance$point_opacity
        leaf$point_size <- appearance$point_size
      }
      leaf <- .builder_state_spatial_roi_leaf(
        leaf,
        paste0(section, "/", roi)
      )
      defaults <- builder_alignment_defaults()
      if (
        identical(leaf$rotation_degrees, 0) &&
          identical(leaf$point_opacity, defaults$point_opacity) &&
          identical(leaf$point_size, defaults$point_size)
      ) {
        settings[[section]][[roi]] <- NULL
      } else {
        settings[[section]][[roi]] <- leaf
      }
    }
    if (!length(settings[[section]] %||% list())) {
      settings[[section]] <- NULL
    }
    if (!identical(before[[section]], settings[[section]])) {
      changed_sections <- c(changed_sections, section)
    }
  }
  entry$settings$spatial_roi_settings <- settings
  list(
    entry = entry,
    sections = changed_sections,
    changed = length(changed_sections) > 0L
  )
}

builder_partition_alignments <- function(images) {
  spatial <- list()
  trekker <- NULL
  for (name in names(images %||% list())) {
    record <- builder_alignment_normalize(images[[name]], section_id = name)
    if (
      identical(record$section_kind %||% "", "trekker") ||
        identical(name, "trekker")
    ) {
      if (is.null(record)) {
        stop(
          "Trekker alignment must remain a single image record.",
          call. = FALSE
        )
      }
      trekker <- record
    } else {
      spatial[[name]] <- images[[name]]
    }
  }
  list(
    spatial = builder_image_collection_normalize(spatial),
    trekker = trekker
  )
}

# Builder keeps the original encoded image on disk and therefore does not need
# an application-level file-size or decoded-pixel ceiling. Small PNGs are still
# decoded once for strict payload validation; large scientific images use the
# bounded structural checks below so validation never creates a full R raster.
BUILDER_IMAGE_MAX_BYTES <- Inf
BUILDER_IMAGE_MAX_PIXELS <- Inf
BUILDER_IMAGE_FULL_DECODE_THRESHOLD <- 32 * 1024^2

.builder_image_uint32_be <- function(bytes) {
  if (length(bytes) != 4L) {
    return(NA_real_)
  }
  values <- as.numeric(as.integer(bytes))
  sum(values * c(256^3, 256^2, 256, 1))
}

.builder_png_crc32 <- local({
  table <- integer(256L)
  polynomial <- -306674912L
  for (index in 0:255) {
    value <- as.integer(index)
    for (bit in 1:8) {
      value <- if (bitwAnd(value, 1L)) {
        bitwXor(bitwShiftR(value, 1L), polynomial)
      } else {
        bitwShiftR(value, 1L)
      }
    }
    table[[index + 1L]] <- value
  }
  function(bytes) {
    value <- -1L
    for (byte in as.integer(bytes)) {
      index <- bitwAnd(bitwXor(value, byte), 255L) + 1L
      value <- bitwXor(bitwShiftR(value, 8L), table[[index]])
    }
    value <- bitwXor(value, -1L)
    as.raw(c(
      bitwAnd(bitwShiftR(value, 24L), 255L),
      bitwAnd(bitwShiftR(value, 16L), 255L),
      bitwAnd(bitwShiftR(value, 8L), 255L),
      bitwAnd(value, 255L)
    ))
  }
})

.builder_png_dimensions <- function(bytes) {
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (
    length(bytes) < 24L ||
      !identical(bytes[seq_len(8L)], signature) ||
      !identical(bytes[9:12], as.raw(c(0x00, 0x00, 0x00, 0x0d))) ||
      !identical(bytes[13:16], charToRaw("IHDR"))
  ) {
    return(NULL)
  }
  width <- .builder_image_uint32_be(bytes[17:20])
  height <- .builder_image_uint32_be(bytes[21:24])
  if (!all(is.finite(c(width, height))) || width < 1 || height < 1) {
    return(NULL)
  }
  if (width <= .Machine$integer.max && height <= .Machine$integer.max) {
    return(c(width = as.integer(width), height = as.integer(height)))
  }
  c(width = width, height = height)
}

.builder_png_zlib_header_valid <- function(bytes) {
  if (length(bytes) != 2L) {
    return(FALSE)
  }
  compression <- as.integer(bytes[[1L]])
  flags <- as.integer(bytes[[2L]])
  bitwAnd(compression, 0x0fL) == 8L &&
    bitwShiftR(compression, 4L) <= 7L &&
    bitwAnd(flags, 0x20L) == 0L &&
    (compression * 256L + flags) %% 31L == 0L
}

.builder_png_has_pixel_data <- function(path) {
  size <- suppressWarnings(as.numeric(file.info(path)$size[[1L]]))
  if (!is.finite(size) || size < 45) {
    return(FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (!identical(readBin(connection, what = "raw", n = 8L), signature)) {
    return(FALSE)
  }
  position <- 8
  has_pixel_data <- FALSE
  seen_idat <- FALSE
  ended_idat <- FALSE
  bit_depth <- NULL
  color_type <- NULL
  seen_palette <- FALSE
  zlib_header <- raw()
  repeat {
    header <- readBin(connection, what = "raw", n = 8L)
    if (length(header) != 8L) {
      return(FALSE)
    }
    chunk_length <- .builder_image_uint32_be(header[1:4])
    if (!is.finite(chunk_length) || chunk_length > size - position - 12) {
      return(FALSE)
    }
    chunk_type <- header[5:8]
    is_idat <- identical(chunk_type, charToRaw("IDAT"))
    if (
      (position == 8 && !identical(chunk_type, charToRaw("IHDR"))) ||
        (position != 8 && identical(chunk_type, charToRaw("IHDR"))) ||
        (is_idat && ended_idat)
    ) {
      return(FALSE)
    }
    if (seen_idat && !is_idat) {
      ended_idat <- TRUE
    }
    if (identical(chunk_type, charToRaw("IHDR"))) {
      if (chunk_length != 13) {
        return(FALSE)
      }
      chunk_data <- readBin(connection, what = "raw", n = chunk_length)
      checksum <- readBin(connection, what = "raw", n = 4L)
      if (
        length(chunk_data) != chunk_length ||
          !identical(
            checksum,
            .builder_png_crc32(c(chunk_type, chunk_data))
          )
      ) {
        return(FALSE)
      }
      bit_depth <- as.integer(chunk_data[[9L]])
      color_type <- as.integer(chunk_data[[10L]])
      allowed_depths <- switch(
        as.character(color_type),
        `0` = c(1L, 2L, 4L, 8L, 16L),
        `2` = c(8L, 16L),
        `3` = c(1L, 2L, 4L, 8L),
        `4` = c(8L, 16L),
        `6` = c(8L, 16L),
        integer()
      )
      if (
        !bit_depth %in% allowed_depths ||
          as.integer(chunk_data[[11L]]) != 0L ||
          as.integer(chunk_data[[12L]]) != 0L ||
          !as.integer(chunk_data[[13L]]) %in% 0:1
      ) {
        return(FALSE)
      }
    } else if (identical(chunk_type, charToRaw("PLTE"))) {
      if (
        seen_palette ||
          seen_idat ||
          color_type %in% c(0L, 4L) ||
          chunk_length < 3L ||
          chunk_length > 768L ||
          chunk_length %% 3L != 0L ||
          (color_type == 3L && chunk_length / 3L > 2^bit_depth)
      ) {
        return(FALSE)
      }
      chunk_data <- readBin(connection, what = "raw", n = chunk_length)
      checksum <- readBin(connection, what = "raw", n = 4L)
      if (
        length(chunk_data) != chunk_length ||
          !identical(
            checksum,
            .builder_png_crc32(c(chunk_type, chunk_data))
          )
      ) {
        return(FALSE)
      }
      seen_palette <- TRUE
    } else if (is_idat) {
      if (color_type == 3L && !seen_palette) {
        return(FALSE)
      }
      seen_idat <- TRUE
      take <- min(2L - length(zlib_header), chunk_length)
      if (take > 0L) {
        zlib_header <- c(
          zlib_header,
          readBin(connection, what = "raw", n = take)
        )
      }
      seek(connection, where = chunk_length - take + 4L, origin = "current")
      if (
        length(zlib_header) == 2L &&
          !.builder_png_zlib_header_valid(zlib_header)
      ) {
        return(FALSE)
      }
      has_pixel_data <- length(zlib_header) == 2L
    } else if (identical(chunk_type, charToRaw("IEND"))) {
      checksum <- readBin(connection, what = "raw", n = 4L)
      return(
        chunk_length == 0 &&
          has_pixel_data &&
          position + 12 == size &&
          identical(checksum, .builder_png_crc32(chunk_type))
      )
    } else {
      seek(connection, where = chunk_length + 4, origin = "current")
    }
    position <- position + chunk_length + 12
  }
}

.builder_jpeg_exif_orientation <- function(segment) {
  signature <- c(charToRaw("Exif"), raw(2L))
  if (length(segment) < 14L || !identical(segment[1:6], signature)) {
    return(NULL)
  }
  tiff <- segment[-seq_len(6L)]
  little_endian <- if (identical(tiff[1:2], charToRaw("II"))) {
    TRUE
  } else if (identical(tiff[1:2], charToRaw("MM"))) {
    FALSE
  } else {
    return(NULL)
  }
  uint <- function(index, size) {
    if (
      !is.finite(index) ||
        index < 1 ||
        index + size - 1 > length(tiff)
    ) {
      return(NA_real_)
    }
    values <- as.numeric(as.integer(tiff[seq.int(index, length.out = size)]))
    powers <- if (little_endian) seq_len(size) - 1L else rev(seq_len(size)) - 1L
    sum(values * 256^powers)
  }
  if (!identical(uint(3L, 2L), 42)) {
    return(NULL)
  }
  ifd_index <- uint(5L, 4L) + 1
  entry_count <- uint(ifd_index, 2L)
  entry_start <- ifd_index + 2
  available_entries <- floor((length(tiff) - entry_start + 1) / 12)
  if (!is.finite(entry_count) || available_entries < 1L) {
    return(NULL)
  }
  for (entry_index in seq_len(min(entry_count, available_entries))) {
    entry <- entry_start + (entry_index - 1L) * 12L
    if (
      identical(uint(entry, 2L), 0x0112) &&
        identical(uint(entry + 2L, 2L), 3) &&
        identical(uint(entry + 4L, 4L), 1)
    ) {
      orientation <- uint(entry + 8L, 2L)
      if (orientation %in% 1:8) {
        return(as.integer(orientation))
      }
    }
  }
  NULL
}

.builder_jpeg_dimensions_from_connection <- function(connection, total) {
  unsafe <- function() {
    list(
      error = "JPEG metadata could not be read. Check that the file is valid."
    )
  }
  missing_pixels <- function() {
    list(error = "The image file has no valid encoded pixel data.")
  }
  incomplete <- function() {
    list(error = "The image file is incomplete or truncated.")
  }
  read_bytes <- function(size) {
    readBin(connection, what = "raw", n = size)
  }
  read_uint16 <- function() {
    bytes <- read_bytes(2L)
    if (length(bytes) != 2L) {
      return(NA_integer_)
    }
    as.integer(bytes[[1L]]) * 256L + as.integer(bytes[[2L]])
  }
  quantization_tables <- function(segment) {
    tables <- integer()
    cursor <- 1L
    while (cursor <= length(segment)) {
      descriptor <- as.integer(segment[[cursor]])
      precision <- bitwShiftR(descriptor, 4L)
      table <- bitwAnd(descriptor, 0x0fL)
      values <- if (precision == 0L) {
        64L
      } else if (precision == 1L) {
        128L
      } else {
        0L
      }
      if (table > 3L || values == 0L || cursor + values > length(segment)) {
        return(NULL)
      }
      payload <- as.integer(segment[seq.int(
        cursor + 1L,
        length.out = values
      )])
      invalid_value <- if (precision == 0L) {
        any(payload == 0L)
      } else {
        any(
          payload[seq.int(1L, values, by = 2L)] == 0L &
            payload[seq.int(2L, values, by = 2L)] == 0L
        )
      }
      if (invalid_value) {
        return(NULL)
      }
      tables <- c(tables, table)
      cursor <- cursor + values + 1L
    }
    if (length(tables)) unique(tables) else NULL
  }
  if (
    !is.finite(total) ||
      total < 2L ||
      !identical(read_bytes(2L), as.raw(c(0xff, 0xd8)))
  ) {
    return(NULL)
  }
  start_of_frame <- c(
    0xc0,
    0xc1,
    0xc2,
    0xc3,
    0xc5,
    0xc6,
    0xc7,
    0xc9,
    0xca,
    0xcb,
    0xcd,
    0xce,
    0xcf
  )
  dct_frame <- c(0xc0, 0xc1, 0xc2, 0xc5, 0xc6, 0xc9, 0xca, 0xcd, 0xce)
  standalone <- c(0x01, 0xd8, 0xd9, 0xd0:0xd7)
  orientation <- 1L
  width <- NULL
  height <- NULL
  frame_marker <- NULL
  frame_components <- NULL
  available_quantization_tables <- integer()
  repeat {
    marker_prefix <- read_bytes(1L)
    if (length(marker_prefix) != 1L) {
      return(if (!is.null(width)) incomplete() else unsafe())
    }
    if (!identical(marker_prefix, as.raw(0xff))) {
      return(unsafe())
    }
    marker_raw <- read_bytes(1L)
    while (length(marker_raw) == 1L && identical(marker_raw, as.raw(0xff))) {
      marker_raw <- read_bytes(1L)
    }
    if (length(marker_raw) != 1L) {
      return(unsafe())
    }
    marker <- as.integer(marker_raw)
    if (marker == 0L) {
      return(unsafe())
    }
    if (marker == 0xd9L) {
      return(
        if (is.null(width) || is.null(height)) unsafe() else missing_pixels()
      )
    }
    if (marker %in% standalone) {
      next
    }
    segment_length <- read_uint16()
    payload_length <- segment_length - 2L
    position <- seek(connection, where = NA, origin = "current")
    if (
      !is.finite(segment_length) ||
        segment_length < 2L ||
        position + payload_length > total
    ) {
      return(unsafe())
    }
    if (marker == 0xe1L) {
      segment <- read_bytes(payload_length)
      orientation <- .builder_jpeg_exif_orientation(
        segment
      ) %||%
        orientation
      next
    } else if (marker == 0xdbL) {
      tables <- quantization_tables(read_bytes(payload_length))
      if (is.null(tables)) {
        return(missing_pixels())
      }
      available_quantization_tables <- unique(c(
        available_quantization_tables,
        tables
      ))
      next
    } else if (marker %in% start_of_frame) {
      if (payload_length < 6L) {
        return(unsafe())
      }
      frame <- read_bytes(payload_length)
      if (length(frame) != payload_length) {
        return(unsafe())
      }
      height <- as.integer(frame[[2L]]) * 256L + as.integer(frame[[3L]])
      width <- as.integer(frame[[4L]]) * 256L + as.integer(frame[[5L]])
      frame_marker <- marker
      sample_precision <- as.integer(frame[[1L]])
      component_count <- as.integer(frame[[6L]])
      expected_length <- 6L + component_count * 3L
      if (
        !all(is.finite(c(width, height))) ||
          width < 1 ||
          height < 1 ||
          component_count < 1L ||
          payload_length != expected_length
      ) {
        return(unsafe())
      }
      if (
        !marker %in% c(0xc0L, 0xc1L, 0xc2L) ||
          sample_precision != 8L
      ) {
        return(missing_pixels())
      }
      component_starts <- seq.int(7L, by = 3L, length.out = component_count)
      component_ids <- as.integer(frame[component_starts])
      component_sampling <- as.integer(frame[component_starts + 1L])
      horizontal_sampling <- bitwShiftR(component_sampling, 4L)
      vertical_sampling <- bitwAnd(component_sampling, 0x0fL)
      component_tables <- as.integer(frame[component_starts + 2L])
      if (
        anyDuplicated(component_ids) ||
          any(horizontal_sampling < 1L | horizontal_sampling > 4L) ||
          any(vertical_sampling < 1L | vertical_sampling > 4L) ||
          any(component_tables < 0L | component_tables > 3L)
      ) {
        return(missing_pixels())
      }
      frame_components <- stats::setNames(
        component_tables,
        as.character(component_ids)
      )
      next
    } else if (marker == 0xdaL) {
      scan <- read_bytes(payload_length)
      scan_component_count <- if (length(scan)) as.integer(scan[[1L]]) else 0L
      expected_length <- 1L + scan_component_count * 2L + 3L
      scan_starts <- if (scan_component_count > 0L) {
        seq.int(2L, by = 2L, length.out = scan_component_count)
      } else {
        integer()
      }
      scan_components <- as.character(as.integer(scan[scan_starts]))
      scan_tables <- as.integer(scan[scan_starts + 1L])
      dc_tables <- bitwShiftR(scan_tables, 4L)
      ac_tables <- bitwAnd(scan_tables, 0x0fL)
      if (
        is.null(width) ||
          is.null(height) ||
          is.null(frame_components) ||
          scan_component_count < 1L ||
          payload_length != expected_length ||
          anyDuplicated(scan_components) ||
          !all(scan_components %in% names(frame_components)) ||
          any(dc_tables > 3L) ||
          any(ac_tables > 3L) ||
          (frame_marker %in%
            dct_frame &&
            !all(
              unname(frame_components[scan_components]) %in%
                available_quantization_tables
            ))
      ) {
        return(
          if (is.null(width) || is.null(height)) unsafe() else missing_pixels()
        )
      }
      entropy_start <- seek(connection, where = NA, origin = "current")
      if (total - entropy_start < 3L) {
        return(missing_pixels())
      }
      seek(connection, where = total - 2L, origin = "start")
      if (!identical(read_bytes(2L), as.raw(c(0xff, 0xd9)))) {
        return(incomplete())
      }
      if (orientation %in% 5:8) {
        swap <- width
        width <- height
        height <- swap
      }
      return(c(width = as.integer(width), height = as.integer(height)))
    } else {
      seek(connection, where = payload_length, origin = "current")
    }
  }
}

#' Read PNG/JPEG dimensions without decoding the raster.
builder_image_file_dimensions <- function(path, filename = path) {
  ext <- tolower(tools::file_ext(filename))
  if (!file.exists(path) || !ext %in% c("png", "jpg", "jpeg")) {
    return(NULL)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)

  if (identical(ext, "png")) {
    header <- readBin(connection, what = "raw", n = 24L)
    return(.builder_png_dimensions(header))
  }
  .builder_jpeg_dimensions_from_connection(connection, file.size(path))
}

.builder_image_has_complete_terminator <- function(path, mime) {
  size <- suppressWarnings(as.numeric(file.info(path)$size[[1L]]))
  if (!is.finite(size) || size < 12) {
    return(FALSE)
  }
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  if (identical(mime, "image/png")) {
    seek(connection, where = size - 12, origin = "start")
    tail <- readBin(connection, what = "raw", n = 12L)
    return(
      length(tail) == 12L &&
        identical(tail[1:4], as.raw(c(0x00, 0x00, 0x00, 0x00))) &&
        identical(tail[5:8], charToRaw("IEND"))
    )
  }
  seek(connection, where = size - 2L, origin = "start")
  identical(readBin(connection, what = "raw", n = 2L), as.raw(c(0xff, 0xd9)))
}

#' Validate PNG/JPEG metadata while retaining its canonical source path.
builder_read_image <- function(
  path,
  filename = path,
  max_bytes = BUILDER_IMAGE_MAX_BYTES,
  max_pixels = BUILDER_IMAGE_MAX_PIXELS
) {
  valid_budget <- is.numeric(max_bytes) &&
    length(max_bytes) == 1L &&
    !is.na(max_bytes) &&
    max_bytes >= 1
  if (!valid_budget) {
    return(list(error = "The image file-size limit is invalid."))
  }
  valid_pixel_budget <- is.numeric(max_pixels) &&
    length(max_pixels) == 1L &&
    !is.na(max_pixels) &&
    max_pixels >= 1
  if (!valid_pixel_budget) {
    return(list(error = "The decoded-pixel limit is invalid."))
  }
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("png", "jpg", "jpeg")) {
    source_bytes <- suppressWarnings(as.numeric(file.info(path)$size[[1L]]))
    if (!is.finite(source_bytes)) {
      return(list(error = "Could not read this image."))
    }
    if (is.finite(max_bytes) && source_bytes > max_bytes) {
      return(list(error = "This image exceeds the configured file-size limit."))
    }
    dimensions <- builder_image_file_dimensions(path, filename)
    if (
      is.list(dimensions) &&
        is.character(dimensions$error %||% NULL) &&
        length(dimensions$error) == 1L &&
        !is.na(dimensions$error) &&
        nzchar(dimensions$error)
    ) {
      return(list(error = dimensions$error))
    }
    if (is.null(dimensions)) {
      return(list(
        error = paste0(
          "Could not read this image. Check that it is a valid ",
          "PNG or JPEG file."
        )
      ))
    }
  } else if (ext %in% c("tif", "tiff")) {
    return(list(
      error = paste0(
        "TIFF and OME-TIFF images are not decoded by the Builder; ",
        "convert the image to PNG or JPEG first."
      )
    ))
  } else {
    return(list(
      error = paste0(
        "Supported image formats are PNG and JPEG, not .",
        ext,
        "."
      )
    ))
  }
  mime <- if (identical(ext, "png")) "image/png" else "image/jpeg"
  width <- unname(dimensions[["width"]])
  height <- unname(dimensions[["height"]])
  if (is.finite(max_pixels) && width > max_pixels / height) {
    return(list(error = "This image exceeds the decoded-pixel limit."))
  }
  if (!.builder_image_has_complete_terminator(path, mime)) {
    return(list(error = "The image file is incomplete or truncated."))
  }
  if (identical(mime, "image/png") && !.builder_png_has_pixel_data(path)) {
    return(list(error = "The image file has no valid encoded pixel data."))
  }
  if (
    identical(mime, "image/png") &&
      width <= BUILDER_IMAGE_FULL_DECODE_THRESHOLD / height &&
      requireNamespace("png", quietly = TRUE) &&
      !isTRUE(tryCatch(
        {
          suppressWarnings(png::readPNG(path, native = TRUE))
          TRUE
        },
        error = function(error) FALSE
      ))
  ) {
    return(list(error = "The image file has no valid encoded pixel data."))
  }
  canonical_path <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    is.null(canonical_path) || !isTRUE(utils::file_test("-f", canonical_path))
  ) {
    return(list(error = "Could not read this image."))
  }
  list(
    mime = mime,
    source_path = canonical_path,
    source_content_md5 = unname(as.character(tools::md5sum(canonical_path))),
    bytes = unname(file.size(canonical_path)),
    width = width,
    height = height,
    source_width = width,
    source_height = height,
    extent_width = width,
    extent_height = height,
    display_width = width,
    display_height = height,
    source_dimensions = c(width = width, height = height)
  )
}

#' Where the image sits, in the same coordinate space as the cells.
#'
#' Three ways to answer, because nothing in the data says which is right:
#' the cells may already be in image pixels, or in physical units with a known
#' scale, or the user may only know "it covers the tissue".
builder_image_bounds <- function(mode, coords, image, um_per_px = 1) {
  x <- coords[[1]]
  y <- coords[[2]]
  image_width <- if (!is.null(image$extent_width)) {
    image$extent_width
  } else if (!is.null(image$source_width)) {
    image$source_width
  } else {
    image$width
  }
  image_height <- if (!is.null(image$extent_height)) {
    image$extent_height
  } else if (!is.null(image$source_height)) {
    image$source_height
  } else {
    image$height
  }
  if (identical(mode, "pixels")) {
    return(list(xmin = 0, xmax = image_width, ymin = 0, ymax = image_height))
  }
  if (identical(mode, "physical")) {
    if (!is.finite(um_per_px) || um_per_px <= 0) {
      return(list(error = "Physical units per pixel must be positive."))
    }
    return(list(
      xmin = 0,
      xmax = image_width * um_per_px,
      ymin = 0,
      ymax = image_height * um_per_px
    ))
  }
  ## Last resort: the cells' own bounding box. Usually wrong -- a slide is
  ## bigger than the area the cells cover -- so it is labelled as such.
  list(
    xmin = min(x, na.rm = TRUE),
    xmax = max(x, na.rm = TRUE),
    ymin = min(y, na.rm = TRUE),
    ymax = max(y, na.rm = TRUE)
  )
}

#' Does the image cover every cell?
#'
#' Cells outside the image render on empty background, which reads as a bad
#' alignment rather than a bad extent, so it is worth saying out loud.
builder_bounds_cover <- function(bounds, coords) {
  x <- coords[[1]]
  y <- coords[[2]]
  outside <- sum(
    x < bounds$xmin | x > bounds$xmax | y < bounds$ymin | y > bounds$ymax,
    na.rm = TRUE
  )
  list(outside = outside, total = length(x))
}

#' Pair one shared picture with the extent computed for each section.
#'
#' Sections cut from one block share a slide scan but not a position: they sit
#' at different offsets in the coordinate space. An earlier version of "apply to
#' all" copied the whole entry, extent included, so four slides out of five were
#' written thousands of units from their own cells -- selectable in the viewer,
#' invisible on screen, and reported as done. Only the source file and its own
#' dimensions may be shared; `bounds` and the coverage count belong to the
#' section.
#'
#' @param picture The validated image file metadata and canonical source path.
#' @param per_section Named list, one entry per section, each `list(bounds =,
#'   cover = list(outside =, total =))`.
builder_pair_sections <- function(picture, per_section) {
  out <- list()
  for (nm in names(per_section)) {
    got <- per_section[[nm]]
    out[[nm]] <- list(
      source_path = picture$source_path,
      project_asset = picture$project_asset,
      bounds = got$bounds,
      bytes = picture$bytes,
      width = picture$width,
      height = picture$height,
      source_width = picture$source_width,
      source_height = picture$source_height,
      extent_width = picture$extent_width,
      extent_height = picture$extent_height,
      display_width = picture$display_width,
      display_height = picture$display_height,
      outside = got$cover$outside,
      total = got$cover$total
    )
  }
  out
}

#' Attach post-export payloads with one atomic CRB replacement.
#'
#' The default Builder path prepares Trekker before `exportFromSeurat()`, while
#' direct callers may still supply it here. This helper validates and applies all
#' requested payloads in memory, writes a sibling temporary file, then replaces
#' the original with rollback.
.builder_apply_external_spatial_appearance <- function(crb, images) {
  collection <- builder_image_collection_normalize(images)
  available <- try(crb$availableSpatial(), silent = TRUE)
  if (inherits(available, "try-error")) {
    if (length(collection)) {
      return(list(error = "The .crb contains no spatial data."))
    }
    available <- character()
  }
  applied <- intersect(names(collection) %||% character(), available)
  changed <- character()
  for (section_id in available) {
    spatial <- crb$getSpatialData(section_id)
    has_embedded <- length(spatial$histology_images %||% list()) > 0L ||
      !is.null(spatial[["histology_image", exact = TRUE]]) ||
      !is.null(spatial[["histology_image_bounds", exact = TRUE]])
    spatial$histology_images <- list()
    spatial$histology_image <- NULL
    spatial$histology_image_bounds <- NULL
    if (section_id %in% applied) {
      active_label <- utils::tail(names(collection[[section_id]]), 1L)
      active <- collection[[section_id]][[active_label]]
      alignment <- builder_alignment_payload(active)
      alignment$source <- active_label
      spatial$histology_alignment <- alignment
    }
    if (has_embedded || section_id %in% applied) {
      crb$addSpatialData(section_id, spatial)
      changed <- c(changed, section_id)
    }
  }
  list(object = crb, applied = applied, changed = changed)
}

builder_attach_crb_extras <- function(
  crb_path,
  images = list(),
  trekker = NULL,
  trekker_alignment = NULL,
  external_images = list(),
  .open_gz = gzfile
) {
  if (length(images)) {
    return(list(
      error = "Builder no longer embeds Spatial image bytes in CRB files."
    ))
  }
  if (!is.null(trekker) && length(trekker) && !is.list(trekker)) {
    return(list(error = "Trekker data must be a list."))
  }
  if (!length(trekker) && !length(external_images)) {
    return(list(applied = character(), trekker = FALSE))
  }

  codec <- try(
    .builder_build_runtime_function(".cerebroPayloadCodec")(crb_path),
    silent = TRUE
  )
  crb <- try(readCerebro(crb_path), silent = TRUE)
  if (inherits(crb, "try-error")) {
    return(list(error = "The exported .crb could not be read back."))
  }

  applied <- character()
  trekker_applied <- FALSE
  if (!is.null(trekker) && length(trekker)) {
    alignment <- builder_alignment_normalize(
      trekker_alignment,
      section_id = "trekker",
      section_kind = "trekker"
    )
    trekker$histology_image <- NULL
    trekker$histology_image_bounds <- NULL
    if (!is.null(alignment)) {
      trekker$histology_alignment <- builder_alignment_payload(alignment)
    }
    added <- try(crb$addTrekker(trekker), silent = TRUE)
    if (inherits(added, "try-error")) {
      return(list(
        error = paste0(
          "Could not attach Trekker data: ",
          conditionMessage(attr(added, "condition"))
        )
      ))
    }
    trekker_applied <- TRUE
  }

  external <- .builder_apply_external_spatial_appearance(crb, external_images)
  if (!is.null(external$error)) {
    return(list(error = external$error))
  }
  crb <- external$object
  applied <- external$applied
  if (!isTRUE(trekker_applied) && !length(external$changed)) {
    return(list(applied = applied, trekker = FALSE))
  }

  temporary <- tempfile(
    ".crb-s-",
    tmpdir = dirname(crb_path)
  )
  backup <- tempfile(
    ".crb-b-",
    tmpdir = dirname(crb_path)
  )
  on.exit(unlink(c(temporary, backup), force = TRUE), add = TRUE)

  written <- try(
    {
      payload <- .builder_build_runtime_function(".thinCerebroPayload")(
        crb,
        crb_path
      )
      if (identical(codec, "rds")) {
        connection <- .open_gz(temporary, open = "wb", compression = 1L)
        tryCatch(saveRDS(payload, connection), finally = close(connection))
      } else {
        .builder_build_runtime_function(".writeCerebroPayload")(
          payload,
          temporary,
          codec
        )
      }
    },
    silent = TRUE
  )
  if (inherits(written, "try-error") || !file.exists(temporary)) {
    return(list(error = "Could not write the augmented .crb."))
  }
  if (!file.rename(crb_path, backup)) {
    return(list(
      error = "Could not protect the exported .crb before updating it."
    ))
  }
  if (!file.rename(temporary, crb_path)) {
    file.rename(backup, crb_path)
    return(list(
      error = "Could not replace the exported .crb; it was restored."
    ))
  }
  unlink(backup, force = TRUE)

  list(applied = applied, trekker = trekker_applied)
}

#' The coordinates of the first spatial slice, for bounds decisions.
#'
#' `@images` entries differ per platform; the coordinate slot is what they
#' agree on.
builder_spatial_coords <- function(object, image = NULL) {
  images <- tryCatch(names(object@images), error = function(e) NULL)
  if (!length(images)) {
    return(NULL)
  }
  ## Which section. Taking the first unconditionally is what made the alignment
  ## preview draw section one's cells no matter which section was being aligned.
  if (is.null(image) || !(image %in% images)) {
    image <- images[1]
  }
  contract <- builder_spatial_contract(object, image = image)
  list(contract$coordinates$x, contract$coordinates$y)
}

#' Shift and scale the image extent, the way a user nudges an overlay.
#'
#' Position is expressed as bounds rather than baked into the picture, so
#' moving it costs nothing and can be undone.
builder_adjust_bounds <- function(bounds, dx = 0, dy = 0, scale = 1) {
  w <- (bounds$xmax - bounds$xmin) * scale
  h <- (bounds$ymax - bounds$ymin) * scale
  cx <- (bounds$xmin + bounds$xmax) / 2 + dx
  cy <- (bounds$ymin + bounds$ymax) / 2 + dy
  list(
    xmin = cx - w / 2,
    xmax = cx + w / 2,
    ymin = cy - h / 2,
    ymax = cy + h / 2
  )
}

## ---------------------------------------------------------------------------
## Tissue pictures that arrive with the source file (AnnData / SingleCellExperiment)
## ---------------------------------------------------------------------------

#' Image settings for pictures that came with the source file.
#'
#' Writes each picture into `dir` and describes it the way an uploaded one is
#' described, so planning, building and saving a project treat it like any other
#' image. The picture's extent in coordinate space is known from its scale
#' factor, so it is placed exactly there instead of being fitted by eye.
#' Pictures keep their own orientation (row 0 on top) while the Viewer draws y
#' upward, so they are flipped vertically, as for Seurat Visium data.
#'
#' @param payload From builder_source_image_payload().
#' @param dir A directory the caller owns.
#' @return `list(section = list(label = alignment_record))`; pictures that
#'   cannot be used are left out.
builder_source_image_settings <- function(payload, dir) {
  if (!is.list(payload) || !length(payload)) {
    return(list())
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  result <- list()
  for (section in names(payload)) {
    for (label in names(payload[[section]])) {
      item <- payload[[section]][[label]]
      staged <- tempfile("source-image-", tmpdir = dir, fileext = ".png")
      writeBin(item$bytes, staged)
      md5 <- unname(as.character(tools::md5sum(staged)))
      path <- file.path(dir, paste0(md5, ".png"))
      if (file.exists(path)) {
        unlink(staged)
      } else if (!file.rename(staged, path)) {
        unlink(staged)
        next
      }
      name <- paste0(section, " ", label, ".png")
      image <- builder_read_image(path, name)
      if (!is.null(image$error)) {
        next
      }
      parameters <- utils::modifyList(
        builder_alignment_defaults(),
        list(flip_y = TRUE)
      )
      record <- builder_alignment_record(
        source = list(name = name, type = image$mime, size = image$bytes),
        base_bounds = item$bounds,
        parameters = parameters,
        section = list(id = section, kind = "spatial"),
        source_path = image$source_path
      )
      record$viewport_bounds <- item$bounds
      ## Remember where the picture belongs so Reset can return to it instead
      ## of refitting a registered picture by eye.
      record$source_registration <- list(
        base_bounds = item$bounds,
        flip_x = FALSE,
        flip_y = TRUE
      )
      record$image_label <- label
      facts <- intersect(
        names(image),
        c(
          "bytes",
          "width",
          "height",
          "source_width",
          "source_height",
          "extent_width",
          "extent_height",
          "display_width",
          "display_height"
        )
      )
      record[facts] <- image[facts]
      record$source_content_md5 <- image$source_content_md5
      result[[section]][[label]] <- record
    }
  }
  result
}
