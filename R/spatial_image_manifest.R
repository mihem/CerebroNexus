#' Normalize spatial image coordinate bounds
#'
#' @keywords internal
#' @noRd
.spatialImageBounds <- function(
  bounds,
  coordinates,
  context,
  allow_outside = FALSE
) {
  ## NULL `coordinates` check explicit `bounds` on their own.
  check_coordinates <- !is.null(coordinates) || is.null(bounds)
  valid_coordinates <- !check_coordinates ||
    is.data.frame(coordinates) &&
      all(c("x", "y") %in% colnames(coordinates)) &&
      is.numeric(coordinates[["x"]]) &&
      is.numeric(coordinates[["y"]]) &&
      nrow(coordinates) > 0L
  if (!valid_coordinates) {
    stop(
      context,
      " requires non-empty numeric x and y coordinates.",
      call. = FALSE
    )
  }

  if (
    check_coordinates &&
      any(!is.finite(as.matrix(coordinates[, c("x", "y"), drop = FALSE])))
  ) {
    stop(context, " coordinates must be finite.", call. = FALSE)
  }

  required <- c("xmin", "xmax", "ymin", "ymax")
  if (is.null(bounds)) {
    bounds <- c(
      xmin = min(coordinates[["x"]]),
      xmax = max(coordinates[["x"]]),
      ymin = min(coordinates[["y"]]),
      ymax = max(coordinates[["y"]])
    )
  } else {
    valid_bounds <- is.numeric(bounds) &&
      length(bounds) == 4L &&
      !is.null(names(bounds)) &&
      setequal(names(bounds), required) &&
      !anyDuplicated(names(bounds))
    if (!valid_bounds) {
      stop(
        context,
        " bounds must contain exactly xmin, xmax, ymin, and ymax.",
        call. = FALSE
      )
    }
    bounds <- bounds[required]
  }

  if (any(!is.finite(bounds))) {
    stop(context, " bounds must be finite.", call. = FALSE)
  }
  if (bounds[["xmin"]] >= bounds[["xmax"]]) {
    stop(context, " requires xmin to be less than xmax.", call. = FALSE)
  }
  if (bounds[["ymin"]] >= bounds[["ymax"]]) {
    stop(context, " requires ymin to be less than ymax.", call. = FALSE)
  }

  outside <- coordinates[["x"]] < bounds[["xmin"]] |
    coordinates[["x"]] > bounds[["xmax"]] |
    coordinates[["y"]] < bounds[["ymin"]] |
    coordinates[["y"]] > bounds[["ymax"]]
  if (check_coordinates && !isTRUE(allow_outside) && any(outside)) {
    stop(
      context,
      " has coordinates outside its declared bounds.",
      call. = FALSE
    )
  }

  bounds
}

#' Validate the image labels of one spatial entry
#'
#' @keywords internal
#' @noRd
.spatialImageLabels <- function(labels, context) {
  if (is.null(labels) || anyNA(labels) || any(!nzchar(labels))) {
    invalid_label <- if (is.null(labels)) {
      "<unnamed>"
    } else if (anyNA(labels)) {
      "<NA>"
    } else {
      "<empty>"
    }
    stop(
      context,
      " image label `",
      invalid_label,
      "` is invalid; labels must be non-empty.",
      call. = FALSE
    )
  }
  if (anyDuplicated(labels)) {
    stop(
      context,
      " has duplicate image label `",
      unique(labels[duplicated(labels)])[[1L]],
      "`; labels must be unique.",
      call. = FALSE
    )
  }
  labels
}

#' Normalize embedded images for one spatial entry
#'
#' @keywords internal
#' @noRd
.normalizeEmbeddedSpatialImages <- function(images, coordinates, context) {
  if (!is.list(images)) {
    stop(context, " `histology_images` must be a named list.", call. = FALSE)
  }
  if (length(images) == 0L) {
    return(list())
  }

  image_names <- .spatialImageLabels(names(images), context)

  normalized <- lapply(seq_along(images), function(i) {
    label <- image_names[[i]]
    payload <- images[[i]]
    payload_context <- paste0(context, " image `", label, "`")
    valid_fields <- c(
      "histology_image",
      "histology_image_bounds",
      "histology_alignment",
      "image_label",
      "roi_field",
      "roi_value"
    )
    if (
      !is.list(payload) ||
        is.null(names(payload)) ||
        !"histology_image" %in% names(payload) ||
        anyDuplicated(names(payload)) ||
        any(!names(payload) %in% valid_fields)
    ) {
      stop(
        payload_context,
        " must contain `histology_image` and optional ",
        "`histology_image_bounds`.",
        call. = FALSE
      )
    }

    image <- payload[["histology_image"]]
    valid_image <- is.character(image) &&
      length(image) == 1L &&
      !is.na(image) &&
      grepl(
        "^data:image/[A-Za-z0-9.+-]+;base64,[A-Za-z0-9+/]+={0,2}$",
        image
      )
    if (!valid_image) {
      stop(
        payload_context,
        " must contain one base64 `data:image/...` URI.",
        call. = FALSE
      )
    }

    alignment <- payload[["histology_alignment"]]
    if (
      !is.null(alignment) &&
        (!is.list(alignment) ||
          is.null(names(alignment)) ||
          anyDuplicated(names(alignment)))
    ) {
      stop(payload_context, " alignment must be a named list.", call. = FALSE)
    }

    scope <- .spatialImageRoiScope(payload, payload_context)
    display_label <- payload[["image_label"]]
    if (
      !is.null(display_label) &&
        (!is.character(display_label) ||
          length(display_label) != 1L ||
          is.na(display_label) ||
          !nzchar(display_label))
    ) {
      stop(payload_context, " display label must be non-empty.", call. = FALSE)
    }
    result <- c(
      list(
        histology_image = image,
        histology_image_bounds = .spatialImageBounds(
          payload[["histology_image_bounds"]],
          coordinates,
          payload_context,
          allow_outside = length(scope) > 0L
        )
      ),
      scope
    )
    if (!is.null(display_label)) {
      result$image_label <- display_label
    }
    if (!is.null(alignment)) {
      result$histology_alignment <- alignment
    }
    result
  })
  names(normalized) <- image_names
  normalized
}

.spatialImageRoiScope <- function(value, context) {
  fields <- c("roi_field", "roi_value")
  present <- fields %in% names(value)
  if (!any(present)) {
    return(list())
  }
  if (!all(present)) {
    stop(
      context,
      " must declare both `roi_field` and `roi_value`.",
      call. = FALSE
    )
  }
  valid <- vapply(
    value[fields],
    function(item) {
      is.character(item) && length(item) == 1L && !is.na(item) && nzchar(item)
    },
    logical(1)
  )
  if (!all(valid)) {
    stop(context, " ROI scope must contain non-empty strings.", call. = FALSE)
  }
  value[fields]
}

#' Normalize embedded images in one spatial-data entry
#'
#' @keywords internal
#' @noRd
.normalizeSpatialDataImages <- function(data, spatial_name) {
  if (any(c("histology_image", "histology_image_bounds") %in% names(data))) {
    stop(
      "Spatial data `",
      spatial_name,
      "` carries the singular `histology_image` / `histology_image_bounds` ",
      "fields, which are no longer accepted. Use `histology_images = ",
      "list(<label> = list(histology_image = <data URI>, ",
      "histology_image_bounds = <bounds>))` instead.",
      call. = FALSE
    )
  }
  images <- data[["histology_images"]] %||% list()

  data[["histology_images"]] <- .normalizeEmbeddedSpatialImages(
    images,
    data[["coordinates"]],
    paste0("Spatial data `", spatial_name, "`")
  )
  data
}

#' Normalize file-backed image declarations for Seurat spatial entries
#'
#' @keywords internal
#' @noRd
.normalizeSpatialImagePaths <- function(images, spatial_names, context) {
  if (is.null(images)) {
    return(NULL)
  }
  if (!is.list(images)) {
    stop(context, " must be a named list.", call. = FALSE)
  }
  image_spatials <- names(images)
  if (
    is.null(image_spatials) ||
      anyNA(image_spatials) ||
      any(!nzchar(image_spatials))
  ) {
    stop(context, " must use non-empty spatial names.", call. = FALSE)
  }
  if (anyDuplicated(image_spatials)) {
    stop(context, " must use unique spatial names.", call. = FALSE)
  }
  unknown <- setdiff(image_spatials, spatial_names)
  if (length(unknown) > 0L) {
    stop(
      context,
      " spatial `",
      unknown[[1L]],
      "` is not present in `Seurat::Images(object)`.",
      call. = FALSE
    )
  }

  normalized <- lapply(seq_along(images), function(i) {
    spatial_name <- image_spatials[[i]]
    spatial_context <- paste0(context, " spatial `", spatial_name, "`")
    declarations <- images[[i]]
    if (is.character(declarations)) {
      labels <- names(declarations)
      declarations <- lapply(declarations, function(path) list(path = path))
      names(declarations) <- labels
    }
    if (!is.list(declarations)) {
      stop(
        spatial_context,
        " must contain named image declarations.",
        call. = FALSE
      )
    }
    if (length(declarations) == 0L) {
      return(list())
    }
    labels <- .spatialImageLabels(names(declarations), spatial_context)

    descriptors <- lapply(seq_along(declarations), function(j) {
      label <- labels[[j]]
      descriptor <- declarations[[j]]
      descriptor_context <- paste0(spatial_context, " image `", label, "`")
      if (is.character(descriptor)) {
        descriptor <- list(path = descriptor)
      }
      valid_descriptor <- is.list(descriptor) &&
        !is.null(names(descriptor)) &&
        "path" %in% names(descriptor) &&
        !anyDuplicated(names(descriptor)) &&
        all(
          names(descriptor) %in%
            c(
              "path",
              "bounds",
              "viewport_bounds",
              "label",
              "roi_field",
              "roi_value"
            )
        )
      if (!valid_descriptor) {
        stop(
          descriptor_context,
          " must contain `path` and optional `bounds`.",
          call. = FALSE
        )
      }
      path <- descriptor[["path"]]
      if (
        !is.character(path) ||
          length(path) != 1L ||
          is.na(path) ||
          !nzchar(path)
      ) {
        stop(
          descriptor_context,
          " path must be one non-empty string.",
          call. = FALSE
        )
      }
      if (!file.exists(path)) {
        stop(descriptor_context, " path does not exist: ", path, call. = FALSE)
      }
      if (dir.exists(path) || is.na(file.info(path)$isdir)) {
        stop(
          descriptor_context,
          " path must be a regular file: ",
          path,
          call. = FALSE
        )
      }
      extension <- tolower(tools::file_ext(path))
      if (!(extension %in% c("png", "jpg", "jpeg", "svg"))) {
        stop(
          descriptor_context,
          " must use a png, jpg, jpeg, or svg file.",
          call. = FALSE
        )
      }
      bounds <- descriptor[["bounds"]]
      if (!is.null(bounds)) {
        bounds <- .spatialImageBounds(bounds, NULL, descriptor_context)
      }
      viewport_bounds <- descriptor[["viewport_bounds"]]
      if (!is.null(viewport_bounds)) {
        required <- c("xmin", "xmax", "ymin", "ymax")
        valid_viewport <- is.numeric(viewport_bounds) &&
          length(viewport_bounds) == 4L &&
          !is.null(names(viewport_bounds)) &&
          setequal(names(viewport_bounds), required) &&
          !anyDuplicated(names(viewport_bounds))
        if (!valid_viewport) {
          stop(
            descriptor_context,
            " viewport_bounds must contain exactly xmin, xmax, ymin, and ymax.",
            call. = FALSE
          )
        }
        viewport_bounds <- viewport_bounds[required]
        if (
          any(!is.finite(viewport_bounds)) ||
            viewport_bounds[["xmin"]] >= viewport_bounds[["xmax"]] ||
            viewport_bounds[["ymin"]] >= viewport_bounds[["ymax"]]
        ) {
          stop(
            descriptor_context,
            " viewport_bounds are invalid.",
            call. = FALSE
          )
        }
      }
      compact <- list(path = path)
      if (!is.null(bounds)) {
        compact$bounds <- bounds
      }
      if (!is.null(viewport_bounds)) {
        compact$viewport_bounds <- viewport_bounds
      }
      if (!is.null(descriptor[["label"]])) {
        display_label <- descriptor[["label"]]
        if (
          !is.character(display_label) ||
            length(display_label) != 1L ||
            is.na(display_label) ||
            !nzchar(display_label)
        ) {
          stop(
            descriptor_context,
            " display label must be non-empty.",
            call. = FALSE
          )
        }
        compact$label <- display_label
      }
      compact <- c(
        compact,
        .spatialImageRoiScope(descriptor, descriptor_context)
      )
      compact
    })
    names(descriptors) <- labels
    descriptors
  })
  names(normalized) <- image_spatials
  normalized
}

#' Encode one file-backed spatial image as a canonical embedded payload
#'
#' @keywords internal
#' @noRd
.encodeSpatialImageDescriptor <- function(descriptor, coordinates, context) {
  scope <- .spatialImageRoiScope(descriptor, context)
  extension <- tolower(tools::file_ext(descriptor$path))
  mime <- switch(
    extension,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    svg = "image/svg+xml",
    stop(context, " has an unsupported image format.", call. = FALSE)
  )
  c(
    list(
      histology_image = paste0(
        "data:",
        mime,
        ";base64,",
        gsub(
          "\n",
          "",
          jsonlite::base64_enc(readBin(
            descriptor$path,
            "raw",
            file.info(descriptor$path)$size
          )),
          fixed = TRUE
        )
      ),
      histology_image_bounds = .spatialImageBounds(
        descriptor$bounds,
        coordinates,
        context,
        allow_outside = length(scope) > 0L
      )
    ),
    c(
      if (!is.null(descriptor[["viewport_bounds"]])) {
        list(
          histology_alignment = list(
            viewport_bounds = as.list(descriptor[["viewport_bounds"]])
          )
        )
      },
      if (!is.null(descriptor[["label"]])) {
        list(image_label = descriptor[["label"]])
      },
      scope
    )
  )
}

#' Merge misc payloads with file-backed spatial image declarations
#'
#' @keywords internal
#' @noRd
.mergeSpatialImageDeclarations <- function(misc, argument, coordinates = NULL) {
  spatial_names <- union(names(misc), names(argument))
  merged <- setNames(vector("list", length(spatial_names)), spatial_names)
  for (spatial_name in spatial_names) {
    misc_images <- misc[[spatial_name]]
    argument_images <- argument[[spatial_name]]
    conflicts <- intersect(names(misc_images), names(argument_images))
    if (length(conflicts) > 0L) {
      stop(
        "Spatial `",
        spatial_name,
        "` image label `",
        conflicts[[1L]],
        "` is declared in both `object@misc$cerebro_spatial_images` and ",
        "`spatial_images`.",
        call. = FALSE
      )
    }
    if (is.null(coordinates) || is.null(coordinates[[spatial_name]])) {
      merged[[spatial_name]] <- c(misc_images, argument_images)
      next
    }
    coords <- coordinates[[spatial_name]]
    normalized_misc <- if (length(misc_images) > 0L) {
      .normalizeEmbeddedSpatialImages(
        misc_images,
        coords,
        paste0("Spatial image payload `", spatial_name, "`")
      )
    } else {
      list()
    }
    encoded_argument <- lapply(names(argument_images), function(label) {
      .encodeSpatialImageDescriptor(
        argument_images[[label]],
        coords,
        paste0(
          "`spatial_images` spatial `",
          spatial_name,
          "` image `",
          label,
          "`"
        )
      )
    })
    names(encoded_argument) <- names(argument_images)
    merged[[spatial_name]] <- c(normalized_misc, encoded_argument)
  }
  merged
}

#' Validate one level of a named spatial manifest
#'
#' @keywords internal
#' @noRd
.spatialManifestNames <- function(x, context) {
  labels <- names(x)
  if (
    is.null(labels) ||
      anyNA(labels) ||
      any(!nzchar(labels)) ||
      anyDuplicated(labels)
  ) {
    stop(context, " must use unique non-empty names.", call. = FALSE)
  }
  labels
}

#' Stop when a manifest names a dataset that is not in `cerebro_data`
#'
#' @keywords internal
#' @noRd
.assertSpatialDatasets <- function(datasets, catalogs, argument) {
  unknown <- setdiff(datasets, names(catalogs))
  if (length(unknown) > 0L) {
    stop(
      "`",
      argument,
      "` dataset `",
      unknown[[1L]],
      "` is not present in `cerebro_data`.",
      call. = FALSE
    )
  }
}

#' Spatial entries a manifest may name, for error messages
#'
#' @keywords internal
#' @noRd
.spatialEntryChoices <- function(spatial_names) {
  if (length(spatial_names) == 0L) {
    return("no available spatial entries")
  }
  paste0("available spatial entries: ", paste(spatial_names, collapse = ", "))
}

#' Normalize createShinyApp external spatial-image declarations
#'
#' @keywords internal
#' @noRd
.normalizeAppSpatialImages <- function(images, catalogs) {
  if (is.null(images)) {
    return(NULL)
  }
  if (!is.list(images)) {
    stop(
      "`spatial_images` must be a named list in `dataset -> spatial entry -> ",
      "image label -> path` form, for example `spatial_images = ",
      "list(Dataset = list(<spatial entry> = c(label = path)))`; the plain ",
      "`c(Dataset = path)` vector is no longer accepted.",
      call. = FALSE
    )
  }
  if (!is.null(names(images)) && anyDuplicated(names(images))) {
    stop("spatial_images names must be unique.", call. = FALSE)
  }
  datasets <- .spatialManifestNames(images, "`spatial_images`")
  .assertSpatialDatasets(datasets, catalogs, "spatial_images")

  normalized <- lapply(seq_along(images), function(i) {
    dataset <- datasets[[i]]
    declaration <- images[[i]]
    catalog <- catalogs[[dataset]]
    spatial_names <- names(catalog)
    if (!is.list(declaration)) {
      stop(
        "`spatial_images` dataset `",
        dataset,
        "` must contain spatial-entry declarations: ",
        "`list(<spatial entry> = c(label = path))`; a plain path vector per ",
        "dataset is no longer accepted.",
        call. = FALSE
      )
    }

    declared_spatials <- .spatialManifestNames(
      declaration,
      paste0("`spatial_images` dataset `", dataset, "`")
    )
    unknown_spatials <- setdiff(declared_spatials, spatial_names)
    if (length(unknown_spatials) > 0L) {
      stop(
        "`spatial_images` dataset `",
        dataset,
        "` spatial `",
        unknown_spatials[[1L]],
        "` is not available; ",
        .spatialEntryChoices(spatial_names),
        ".",
        call. = FALSE
      )
    }

    dataset_images <- .normalizeSpatialImagePaths(
      declaration,
      spatial_names,
      paste0("`spatial_images` dataset `", dataset, "`")
    )
    for (spatial_name in names(dataset_images)) {
      conflicts <- intersect(
        names(dataset_images[[spatial_name]]),
        catalog[[spatial_name]]
      )
      if (length(conflicts) > 0L) {
        stop(
          "`spatial_images` dataset `",
          dataset,
          "` spatial `",
          spatial_name,
          "` image `",
          conflicts[[1L]],
          "` conflicts with an embedded image label.",
          call. = FALSE
        )
      }
    }
    dataset_images
  })
  names(normalized) <- datasets
  normalized
}

#' Normalize per-image createShinyApp settings
#'
#' @keywords internal
#' @noRd
.normalizeAppSpatialImageSettings <- function(settings, catalogs, images) {
  if (is.null(settings)) {
    return(NULL)
  }
  if (!is.list(settings)) {
    stop("`spatial_image_settings` must be a named list.", call. = FALSE)
  }
  datasets <- .spatialManifestNames(settings, "`spatial_image_settings`")
  .assertSpatialDatasets(datasets, catalogs, "spatial_image_settings")
  allowed <- c(
    "flip_x",
    "flip_y",
    "scale_x",
    "scale_y",
    "offset_x",
    "offset_y",
    "rotation",
    "image_opacity"
  )
  logical_fields <- c("flip_x", "flip_y")

  normalized <- lapply(seq_along(settings), function(i) {
    dataset <- datasets[[i]]
    spatial_settings <- settings[[i]]
    if (!is.list(spatial_settings)) {
      stop(
        "`spatial_image_settings` dataset `",
        dataset,
        "` must contain spatial-entry settings.",
        call. = FALSE
      )
    }
    spatial_names <- .spatialManifestNames(
      spatial_settings,
      paste0("`spatial_image_settings` dataset `", dataset, "`")
    )
    unknown_spatials <- setdiff(spatial_names, names(catalogs[[dataset]]))
    if (length(unknown_spatials) > 0L) {
      stop(
        "`spatial_image_settings` dataset `",
        dataset,
        "` spatial `",
        unknown_spatials[[1L]],
        "` is not available.",
        call. = FALSE
      )
    }

    result <- lapply(spatial_names, function(spatial_name) {
      image_settings <- spatial_settings[[spatial_name]]
      if (!is.list(image_settings)) {
        stop(
          "`spatial_image_settings` dataset `",
          dataset,
          "` spatial `",
          spatial_name,
          "` must contain per-image settings.",
          call. = FALSE
        )
      }
      image_names <- .spatialManifestNames(
        image_settings,
        paste0(
          "`spatial_image_settings` dataset `",
          dataset,
          "` spatial `",
          spatial_name,
          "`"
        )
      )
      available_images <- union(
        catalogs[[dataset]][[spatial_name]],
        names(images[[dataset]][[spatial_name]])
      )
      unknown_images <- setdiff(image_names, available_images)
      if (length(unknown_images) > 0L) {
        stop(
          "`spatial_image_settings` image `",
          unknown_images[[1L]],
          "` does not exist for dataset `",
          dataset,
          "` spatial `",
          spatial_name,
          "`.",
          call. = FALSE
        )
      }
      leaves <- lapply(image_names, function(image_name) {
        leaf <- image_settings[[image_name]]
        context <- paste0(
          "`spatial_image_settings` dataset `",
          dataset,
          "` spatial `",
          spatial_name,
          "` image `",
          image_name,
          "`"
        )
        if (
          !is.list(leaf) || is.null(names(leaf)) || anyDuplicated(names(leaf))
        ) {
          stop(
            context,
            " must be a uniquely named settings list.",
            call. = FALSE
          )
        }
        unknown_fields <- setdiff(names(leaf), allowed)
        if (length(unknown_fields) > 0L) {
          stop(
            context,
            " has unknown setting `",
            unknown_fields[[1L]],
            "`.",
            call. = FALSE
          )
        }
        if (length(leaf) == 0L) {
          return(list())
        }
        for (field in names(leaf)) {
          value <- leaf[[field]]
          valid <- if (field %in% logical_fields) {
            is.logical(value) && length(value) == 1L && !is.na(value)
          } else {
            is.numeric(value) && length(value) == 1L && is.finite(value)
          }
          if (!valid) {
            type <- if (field %in% logical_fields) {
              "logical"
            } else {
              "finite numeric"
            }
            stop(
              context,
              " setting `",
              field,
              "` must be one ",
              type,
              " scalar.",
              call. = FALSE
            )
          }
          if (
            identical(field, "image_opacity") &&
              (value < 0 || value > 1)
          ) {
            stop(
              context,
              " setting `image_opacity` must be between 0 and 1.",
              call. = FALSE
            )
          }
        }
        leaf
      })
      names(leaves) <- image_names
      leaves
    })
    names(result) <- spatial_names
    result
  })
  names(normalized) <- datasets
  normalized
}

#' Normalize per-spatial-entry plot rotations
#'
#' @keywords internal
#' @noRd
.normalizeAppSpatialPlotRotation <- function(rotation, catalogs) {
  if (is.null(rotation)) {
    return(NULL)
  }
  if (!is.list(rotation)) {
    stop(
      "`spatial_plot_rotation` must be a named list in `dataset -> spatial ",
      "entry -> degrees` form, for example `spatial_plot_rotation = ",
      "list(Dataset = list(<spatial entry> = 90))`; a per-dataset numeric ",
      "vector is no longer accepted.",
      call. = FALSE
    )
  }
  datasets <- .spatialManifestNames(rotation, "`spatial_plot_rotation`")
  .assertSpatialDatasets(datasets, catalogs, "spatial_plot_rotation")
  normalized <- lapply(datasets, function(dataset) {
    values <- rotation[[dataset]]
    spatial_names <- .spatialManifestNames(
      values,
      paste0("`spatial_plot_rotation` dataset `", dataset, "`")
    )
    unknown_spatials <- setdiff(spatial_names, names(catalogs[[dataset]]))
    if (length(unknown_spatials)) {
      stop(
        "`spatial_plot_rotation` dataset `",
        dataset,
        "` spatial `",
        unknown_spatials[[1L]],
        "` is not available.",
        call. = FALSE
      )
    }
    valid <- vapply(
      values,
      function(value) {
        is.numeric(value) && length(value) == 1L && is.finite(value)
      },
      logical(1)
    )
    if (!all(valid)) {
      stop(
        "`spatial_plot_rotation` values must be finite numeric rotations.",
        call. = FALSE
      )
    }
    values <- vapply(values, as.numeric, numeric(1))
    stats::setNames(values, spatial_names)
  })
  stats::setNames(normalized, datasets)
}
