##----------------------------------------------------------------------------##
## Coordinate-frame transform contract shared by exporter and Builder.
## `spatial_coordinate_contract.R` provides the coordinate/barcode aliases.
##----------------------------------------------------------------------------##

.spx_coordinate_transform_coordinates <- function(coordinates, context) {
  if (!is.data.frame(coordinates)) {
    stop(context, " requires a data.frame with x and y columns.", call. = FALSE)
  }
  column_names <- attr(coordinates, "names", exact = TRUE)
  if (
    !is.character(column_names) ||
      is.object(column_names) ||
      !all(c("x", "y") %in% column_names)
  ) {
    stop(context, " requires x and y columns.", call. = FALSE)
  }
  x <- .subset2(coordinates, "x")
  y <- .subset2(coordinates, "y")
  if (
    !is.numeric(x) ||
      !is.numeric(y) ||
      is.object(x) ||
      is.object(y) ||
      length(x) != nrow(coordinates) ||
      length(y) != nrow(coordinates) ||
      any(!is.finite(x)) ||
      any(!is.finite(y))
  ) {
    stop(
      context,
      " coordinates must be finite numeric x and y values.",
      call. = FALSE
    )
  }
  list(
    x = as.numeric(x),
    y = as.numeric(y),
    pivot = c(
      x = (min(x) + max(x)) / 2,
      y = (min(y) + max(y)) / 2
    )
  )
}

.spx_coordinate_transform_identity <- function(coordinates) {
  details <- .spx_coordinate_transform_coordinates(
    coordinates,
    "Spatial coordinate transform"
  )
  list(
    schema_version = 1L,
    rotation_degrees = 0,
    scale = 1,
    pivot = details$pivot,
    pivot_method = "bounds_center",
    convention = "counterclockwise_degrees"
  )
}

.spx_coordinate_transform_scalar <- function(
  value,
  name,
  context,
  positive = FALSE
) {
  if (
    !is.numeric(value) ||
      is.object(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    stop(context, name, " must be one finite numeric scalar.", call. = FALSE)
  }
  value <- as.numeric(value)
  if (isTRUE(positive) && value <= 0) {
    stop(context, name, " must be strictly positive.", call. = FALSE)
  }
  value
}

.spx_coordinate_transform_spec_normalize <- function(
  record,
  context = "Spatial coordinate transform"
) {
  identity <- list(
    schema_version = 1L,
    rotation_degrees = 0,
    scale = 1
  )
  if (is.null(record)) {
    return(identity)
  }
  if (!is.list(record) || is.object(record)) {
    stop(context, " must be a list.", call. = FALSE)
  }
  names_record <- attr(record, "names", exact = TRUE)
  if (
    is.null(names_record) ||
      !is.character(names_record) ||
      is.object(names_record) ||
      anyNA(names_record) ||
      any(!nzchar(names_record)) ||
      anyDuplicated(names_record)
  ) {
    stop(
      context,
      " must be a named list with unique non-blank keys.",
      call. = FALSE
    )
  }
  ## `schema_version` is emitted by this very normalizer so persisted Builder
  ## state can safely make a round trip through the same validator.  Callers
  ## may omit it; versions other than 1 are deliberately rejected rather than
  ## guessed at.
  allowed <- c("schema_version", "rotation_degrees", "scale")
  unknown <- setdiff(names_record, allowed)
  if (length(unknown)) {
    stop(
      context,
      " contains unknown key(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  if (
    "schema_version" %in%
      names_record &&
      (!is.integer(record$schema_version) ||
        length(record$schema_version) != 1L ||
        is.na(record$schema_version) ||
        !identical(record$schema_version, 1L))
  ) {
    stop(context, " schema_version must be integer 1.", call. = FALSE)
  }
  missing <- setdiff(c("rotation_degrees", "scale"), names_record)
  if (length(missing)) {
    stop(
      context,
      " is missing required key(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  rotation <- .spx_coordinate_transform_scalar(
    record$rotation_degrees,
    "rotation_degrees",
    context
  )
  ## Keep the persisted value directly representable by Builder's
  ## -180..180 degree slider.  -180 and 180 describe the same orientation;
  ## use -180 as the canonical endpoint.
  rotation <- ((rotation + 180) %% 360) - 180
  if (identical(rotation, -0)) {
    rotation <- 0
  }
  list(
    schema_version = 1L,
    rotation_degrees = rotation,
    scale = .spx_coordinate_transform_scalar(
      record$scale,
      "scale",
      context,
      positive = TRUE
    )
  )
}

.spx_coordinate_transform_normalize <- function(
  record,
  coordinates,
  context = "Spatial coordinate transform"
) {
  identity <- .spx_coordinate_transform_identity(coordinates)
  spec <- .spx_coordinate_transform_spec_normalize(record, context)
  normalized <- identity
  normalized$rotation_degrees <- spec$rotation_degrees
  normalized$scale <- spec$scale
  normalized
}

.spx_coordinate_transforms_normalize <- function(
  transforms,
  spatial_names,
  coordinates_by_spatial,
  context = "spatial_coordinate_transforms"
) {
  if (
    !is.character(spatial_names) ||
      is.object(spatial_names) ||
      anyNA(spatial_names) ||
      any(!nzchar(spatial_names)) ||
      anyDuplicated(spatial_names)
  ) {
    stop(
      context,
      " spatial_names must contain unique non-blank names.",
      call. = FALSE
    )
  }
  if (
    !is.list(coordinates_by_spatial) ||
      is.object(coordinates_by_spatial) ||
      !identical(names(coordinates_by_spatial), spatial_names)
  ) {
    stop(
      context,
      " coordinates_by_spatial must be a named list matching spatial_names.",
      call. = FALSE
    )
  }
  if (is.null(transforms)) {
    return(list())
  }
  if (
    !is.list(transforms) ||
      is.object(transforms) ||
      is.null(names(transforms)) ||
      !is.character(names(transforms)) ||
      is.object(names(transforms)) ||
      anyNA(names(transforms)) ||
      any(!nzchar(names(transforms))) ||
      anyDuplicated(names(transforms))
  ) {
    stop(
      context,
      " must be an ordinary named list with unique non-blank FOV names.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(transforms), spatial_names)
  if (length(unknown)) {
    stop(
      context,
      " contains unknown FOV(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  normalized <- vector("list", length(transforms))
  names(normalized) <- names(transforms)
  for (spatial_name in names(transforms)) {
    normalized[[spatial_name]] <- .spx_coordinate_transform_normalize(
      transforms[[spatial_name]],
      coordinates_by_spatial[[spatial_name]],
      paste0(context, "$", spatial_name)
    )
  }
  normalized
}

.spx_apply_coordinate_transform <- function(coordinates, transform) {
  normalized <- .spx_coordinate_transform_normalize(transform, coordinates)
  if (
    identical(normalized$rotation_degrees, 0) &&
      identical(normalized$scale, 1)
  ) {
    return(coordinates)
  }
  details <- .spx_coordinate_transform_coordinates(
    coordinates,
    "Spatial coordinate transform"
  )
  angle <- normalized$rotation_degrees * pi / 180
  centered_x <- details$x - normalized$pivot[["x"]]
  centered_y <- details$y - normalized$pivot[["y"]]
  rotated_x <- centered_x * cos(angle) - centered_y * sin(angle)
  rotated_y <- centered_x * sin(angle) + centered_y * cos(angle)
  coordinates$x <- normalized$pivot[["x"]] + normalized$scale * rotated_x
  coordinates$y <- normalized$pivot[["y"]] + normalized$scale * rotated_y
  if (any(!is.finite(coordinates$x)) || any(!is.finite(coordinates$y))) {
    stop(
      "Spatial coordinate transform produced non-finite coordinates.",
      call. = FALSE
    )
  }
  coordinates
}

.spx_coordinate_transform_fingerprint <- function(coordinates) {
  details <- .spx_coordinate_transform_coordinates(
    coordinates,
    "Spatial coordinate fingerprint"
  )
  row_ids <- rownames(coordinates)
  if (is.null(row_ids)) {
    row_ids <- as.character(seq_len(nrow(coordinates)))
  }
  if (
    !is.character(row_ids) ||
      is.object(row_ids) ||
      length(row_ids) != nrow(coordinates) ||
      anyNA(row_ids) ||
      anyDuplicated(row_ids)
  ) {
    stop(
      "Spatial coordinate fingerprint requires unique non-missing row identities.",
      call. = FALSE
    )
  }
  canonical_number <- function(value) {
    value[abs(value) < 1e-12] <- 0
    sprintf("%.12g", value)
  }
  payload <- serialize(
    list(
      row_ids = row_ids,
      x = canonical_number(details$x),
      y = canonical_number(details$y)
    ),
    connection = NULL,
    version = 3L
  )
  path <- tempfile("spatial-coordinate-fingerprint-")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeBin(payload, path, useBytes = TRUE)
  md5sum <- getExportedValue("tools", "md5sum")
  unname(as.character(md5sum(path)))
}

.spx_invert_coordinate_transform <- function(coordinates, transform) {
  details <- .spx_coordinate_transform_coordinates(
    coordinates,
    "Spatial coordinate inverse transform"
  )
  if (!is.list(transform) || is.object(transform)) {
    stop(
      "Spatial coordinate inverse transform requires provenance.",
      call. = FALSE
    )
  }
  rotation <- .spx_coordinate_transform_scalar(
    transform$rotation_degrees,
    "rotation_degrees",
    "Spatial coordinate inverse transform"
  )
  scale <- .spx_coordinate_transform_scalar(
    transform$scale,
    "scale",
    "Spatial coordinate inverse transform",
    positive = TRUE
  )
  pivot <- transform$pivot
  if (
    !is.numeric(pivot) ||
      is.object(pivot) ||
      length(pivot) != 2L ||
      !identical(names(pivot), c("x", "y")) ||
      any(!is.finite(pivot))
  ) {
    stop(
      "Spatial coordinate inverse transform requires a finite x/y pivot.",
      call. = FALSE
    )
  }
  angle <- rotation * pi / 180
  scaled_x <- (details$x - pivot[["x"]]) / scale
  scaled_y <- (details$y - pivot[["y"]]) / scale
  coordinates$x <- pivot[["x"]] + cos(angle) * scaled_x + sin(angle) * scaled_y
  coordinates$y <- pivot[["y"]] - sin(angle) * scaled_x + cos(angle) * scaled_y
  if (any(!is.finite(coordinates$x)) || any(!is.finite(coordinates$y))) {
    stop(
      "Spatial coordinate inverse transform produced non-finite coordinates.",
      call. = FALSE
    )
  }
  coordinates
}
