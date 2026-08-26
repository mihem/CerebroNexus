##----------------------------------------------------------------------------##
## Bundle-safe coordinate and barcode-column recognition shared by the Seurat
## exporter and the Builder. Keep the copy under inst/.../core byte-identical.
##----------------------------------------------------------------------------##

.spx_coordinate_contract <- function() {
  list(
    x = c(
      "x",
      "X",
      "coord_x",
      "coordinate_x",
      "spatial_x",
      "spatial_1",
      "sdimx",
      "center_x",
      "centroid_x",
      "x_centroid",
      "x_center",
      "global_x",
      "x_global",
      "aligned_x",
      "x_aligned",
      "cell_x",
      "cell.global.x",
      "cell_global_x",
      "nucleus_x",
      "nucleus.global.x",
      "nucleus_global_x",
      "CenterX_global_px",
      "CenterX_local_px",
      "CenterX_global_mm",
      "xcoord",
      "x_coord",
      "imagecol",
      "image_col",
      "pxl_col_in_fullres",
      "pixel_col",
      "col",
      "column"
    ),
    y = c(
      "y",
      "Y",
      "coord_y",
      "coordinate_y",
      "spatial_y",
      "spatial_2",
      "sdimy",
      "center_y",
      "centroid_y",
      "y_centroid",
      "y_center",
      "global_y",
      "y_global",
      "aligned_y",
      "y_aligned",
      "cell_y",
      "cell.global.y",
      "cell_global_y",
      "nucleus_y",
      "nucleus.global.y",
      "nucleus_global_y",
      "CenterY_global_px",
      "CenterY_local_px",
      "CenterY_global_mm",
      "ycoord",
      "y_coord",
      "imagerow",
      "image_row",
      "pxl_row_in_fullres",
      "pixel_row",
      "row"
    ),
    barcode = c(
      "cell",
      "cells",
      "cell_id",
      "cellid",
      "cell.id",
      "barcode",
      "barcodes",
      "Barcode",
      "CELL",
      "Cell",
      "object",
      "object_id",
      "ObjectID",
      "ID",
      "id",
      "name"
    )
  )
}

.spx_contract_column_names <- function(data) {
  column_names <- attr(data, "names", exact = TRUE)
  if (
    !is.character(column_names) ||
      is.object(column_names) ||
      anyNA(column_names) ||
      any(!nzchar(column_names))
  ) {
    return(NULL)
  }
  column_names
}

.spx_contract_clean_names <- function(value) {
  if (!is.character(value) || is.object(value) || anyNA(value)) {
    return(NULL)
  }
  tolower(gsub("[^a-z0-9]+", "", value))
}

.spx_contract_find_alias <- function(column_names, candidates) {
  clean_columns <- .spx_contract_clean_names(column_names)
  clean_candidates <- .spx_contract_clean_names(candidates)
  if (is.null(clean_columns) || is.null(clean_candidates)) {
    return(NULL)
  }
  index <- match(clean_candidates, clean_columns, nomatch = 0L)
  index <- index[index > 0L]
  if (!length(index)) {
    return(NULL)
  }
  column_names[[index[[1L]]]]
}

.spx_find_coordinate_columns <- function(
  data,
  coord_cols = NULL,
  hard_error = FALSE
) {
  column_names <- .spx_contract_column_names(data)
  if (!is.null(coord_cols)) {
    valid_override <- is.character(coord_cols) &&
      !is.object(coord_cols) &&
      length(coord_cols) == 2L &&
      !anyNA(coord_cols) &&
      all(nzchar(coord_cols))
    if (!valid_override) {
      if (isTRUE(hard_error)) {
        stop("`coord_cols` must be length 2.", call. = FALSE)
      }
      return(NULL)
    }
    if (is.null(column_names) || !all(coord_cols %in% column_names)) {
      if (isTRUE(hard_error)) {
        stop(
          "`coord_cols` not found: ",
          paste(setdiff(coord_cols, column_names), collapse = ", "),
          call. = FALSE
        )
      }
      return(NULL)
    }
    return(list(x = coord_cols[[1L]], y = coord_cols[[2L]]))
  }
  if (is.null(column_names)) {
    return(NULL)
  }
  contract <- .spx_coordinate_contract()
  x <- .spx_contract_find_alias(column_names, contract$x)
  y <- .spx_contract_find_alias(column_names, contract$y)
  if (is.null(x) || is.null(y)) {
    return(NULL)
  }
  list(x = x, y = y)
}

.spx_contract_barcode_values <- function(data, column) {
  values <- .subset2(data, column)
  if (is.factor(values)) {
    levels <- attr(values, "levels", exact = TRUE)
    codes <- unclass(values)
    if (
      !is.character(levels) ||
        is.object(levels) ||
        !is.integer(codes)
    ) {
      return(NULL)
    }
    return(levels[codes])
  }
  if (isS4(values) || !is.atomic(values)) {
    return(NULL)
  }
  if (is.object(values)) {
    values <- unclass(values)
  }
  attributes(values) <- NULL
  as.character(values)
}

.spx_find_barcode_column <- function(data, valid_cells) {
  column_names <- .spx_contract_column_names(data)
  if (
    is.null(column_names) ||
      !is.character(valid_cells) ||
      is.object(valid_cells) ||
      !length(valid_cells)
  ) {
    return(NULL)
  }
  candidates <- .spx_coordinate_contract()$barcode
  candidates <- candidates[candidates %in% column_names]
  if (!length(candidates)) {
    return(NULL)
  }
  overlaps <- vapply(
    candidates,
    function(column) {
      values <- .spx_contract_barcode_values(data, column)
      if (is.null(values)) {
        return(0)
      }
      sum(values %in% valid_cells, na.rm = TRUE)
    },
    numeric(1)
  )
  if (max(overlaps, na.rm = TRUE) == 0) {
    return(NULL)
  }
  candidates[[which.max(overlaps)]]
}
