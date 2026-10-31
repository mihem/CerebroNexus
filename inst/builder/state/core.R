## Builder state: core.

.builder_state_or <- function(value, fallback) {
  if (is.null(value)) fallback else value
}

builder_default_percentage_cells_to_show <- function(
  n_cells,
  baseline = 20,
  minimum_cells = 1000
) {
  n_cells <- suppressWarnings(as.numeric(n_cells))
  baseline <- suppressWarnings(as.numeric(baseline))
  minimum_cells <- suppressWarnings(as.numeric(minimum_cells))
  if (
    length(baseline) != 1L ||
      is.na(baseline) ||
      !is.finite(baseline)
  ) {
    baseline <- 20
  }
  baseline <- max(10, min(100, baseline))
  if (
    length(n_cells) != 1L ||
      is.na(n_cells) ||
      !is.finite(n_cells) ||
      n_cells <= 0 ||
      length(minimum_cells) != 1L ||
      is.na(minimum_cells) ||
      !is.finite(minimum_cells) ||
      minimum_cells <= 0
  ) {
    return(as.numeric(ceiling(baseline)))
  }
  as.numeric(min(100, max(baseline, ceiling(100 * minimum_cells / n_cells))))
}

.builder_state_abort <- function(code, message) {
  condition <- structure(
    list(message = message, call = NULL, code = code),
    class = c("builder_state_error", "error", "condition")
  )
  stop(condition)
}

.builder_state_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(trimws(value))
}

.builder_state_has_reference <- function(value, depth = 0L) {
  if (depth > 50L) {
    return(TRUE)
  }
  if (
    is.environment(value) ||
      is.function(value) ||
      isS4(value) ||
      is.language(value) ||
      is.symbol(value) ||
      typeof(value) %in% c("externalptr", "weakref") ||
      inherits(value, "connection")
  ) {
    return(TRUE)
  }
  value_attributes <- attributes(value)
  if (
    !is.null(value_attributes) &&
      any(vapply(
        value_attributes,
        .builder_state_has_reference,
        logical(1),
        depth = depth + 1L
      ))
  ) {
    return(TRUE)
  }
  if (is.list(value) || is.pairlist(value)) {
    return(any(vapply(
      value,
      .builder_state_has_reference,
      logical(1),
      depth = depth + 1L
    )))
  }
  FALSE
}

.builder_state_plain_record <- function(value, recursive = TRUE) {
  if (!is.list(value) || is.object(value)) {
    return(FALSE)
  }
  value_attributes <- attributes(value)
  plain_attributes <- is.null(value_attributes) ||
    identical(names(value_attributes), "names")
  unsafe_reference <- if (isTRUE(recursive)) {
    .builder_state_has_reference(value)
  } else {
    !is.null(value_attributes) &&
      any(vapply(
        value_attributes,
        .builder_state_has_reference,
        logical(1)
      ))
  }
  if (
    !plain_attributes ||
      unsafe_reference
  ) {
    return(FALSE)
  }
  value_names <- attr(value, "names", exact = TRUE)
  !length(value) ||
    (is.character(value_names) &&
      length(value_names) == length(value) &&
      !anyNA(value_names) &&
      all(nzchar(value_names)) &&
      !anyDuplicated(value_names))
}

.builder_state_plain_list <- function(value) {
  if (!is.list(value) || is.object(value)) {
    return(FALSE)
  }
  value_attributes <- attributes(value)
  if (
    (!is.null(value_attributes) &&
      !identical(names(value_attributes), "names")) ||
      .builder_state_has_reference(value)
  ) {
    return(FALSE)
  }
  value_names <- attr(value, "names", exact = TRUE)
  is.null(value_names) ||
    (is.character(value_names) &&
      length(value_names) == length(value) &&
      !anyNA(value_names) &&
      all(nzchar(value_names)) &&
      !anyDuplicated(value_names))
}

.builder_state_viewer_ids <- function(value) {
  if (!is.character(value) || is.object(value)) {
    return(character())
  }
  value <- as.character(value)
  attributes(value) <- NULL
  unique(value[!is.na(value) & nzchar(trimws(value))])
}

.builder_state_viewer_catalog <- function(entry) {
  modern <- if (is.list(entry$dataset_profile)) {
    entry$dataset_profile
  } else if (inherits(entry$profile, "builder_dataset_profile")) {
    entry$profile
  } else {
    list()
  }
  legacy <- if (is.list(entry$profile)) entry$profile else list()
  viewer <- modern$viewer_content
  if (!is.list(viewer)) {
    viewer <- legacy$viewer_content
  }
  if (!is.list(viewer)) {
    viewer <- list()
  }

  metadata <- viewer$metadata
  groups <- if (is.list(metadata) && length(metadata)) {
    ids <- names(metadata)
    ids[vapply(
      metadata,
      function(column) is.list(column) && isTRUE(column$group_eligible),
      logical(1)
    )]
  } else {
    unname(legacy$group_candidates)
  }
  groups <- .builder_state_viewer_ids(groups)
  cell_cycle <- builder_cell_cycle_candidate_ids(metadata)
  if (identical(entry$load_state %||% "loaded", "artifact_ready")) {
    cell_cycle <- unique(c(
      cell_cycle,
      .builder_state_viewer_ids(
        entry$project_artifact$plan_item$cell_cycle %||% character()
      )
    ))
  }

  projection_catalog <- viewer$projections
  projections <- if (
    is.list(projection_catalog) && length(projection_catalog)
  ) {
    ids <- names(projection_catalog)
    ids[vapply(
      projection_catalog,
      function(projection) {
        is.list(projection) && isTRUE(projection$available)
      },
      logical(1)
    )]
  } else {
    legacy$reductions
  }
  projections <- .builder_state_viewer_ids(projections)

  trajectory_catalog <- viewer$trajectories
  trajectories <- list()
  if (is.list(trajectory_catalog) && length(trajectory_catalog)) {
    for (record in trajectory_catalog) {
      if (
        !is.list(record) ||
          !isTRUE(record$selectable) ||
          !.builder_state_text(record$method) ||
          !.builder_state_text(record$name)
      ) {
        next
      }
      method <- as.character(record$method)
      name <- as.character(record$name)
      trajectories[[method]] <- unique(c(trajectories[[method]], name))
    }
  }
  list(
    groups = groups,
    cell_cycle = cell_cycle,
    projections = projections,
    trajectories = trajectories
  )
}

.builder_state_trajectory_selection <- function(value) {
  if (is.null(value) || !length(value)) {
    return(list())
  }
  if (!is.list(value) || is.object(value)) {
    return(list())
  }
  methods <- names(value)
  if (
    is.null(methods) ||
      anyNA(methods) ||
      any(!nzchar(methods)) ||
      anyDuplicated(methods)
  ) {
    return(list())
  }
  out <- list()
  for (method in methods) {
    names_for_method <- .builder_state_viewer_ids(value[[method]])
    if (length(names_for_method)) {
      out[[method]] <- names_for_method
    }
  }
  out
}

.builder_state_filter_trajectories <- function(selected, available) {
  if (!length(available)) {
    return(list())
  }
  selected <- .builder_state_trajectory_selection(selected)
  out <- list()
  for (method in names(available)) {
    kept <- intersect(
      .builder_state_viewer_ids(available[[method]]),
      .builder_state_viewer_ids(selected[[method]])
    )
    if (length(kept)) {
      out[[method]] <- kept
    }
  }
  out
}

.builder_state_first_trajectory <- function(included) {
  if (!length(included)) {
    return(NULL)
  }
  method <- names(included)[[1L]]
  names_for_method <- included[[method]]
  if (!length(names_for_method)) {
    return(NULL)
  }
  list(method = method, name = names_for_method[[1L]])
}

.builder_state_trajectory_included <- function(value, included) {
  is.list(value) &&
    identical(names(value), c("method", "name")) &&
    .builder_state_text(value$method) &&
    .builder_state_text(value$name) &&
    value$method %in% names(included) &&
    value$name %in% included[[value$method]]
}

#' Upgrade one dataset to the canonical Viewer-content settings shape.
#'
#' This is deliberately an import/restore boundary operation: it does not
#' increment the dataset revision, and current-schema settings are never
#' silently repaired after a user edit.
builder_upgrade_viewer_content_entry <- function(entry) {
  if (!is.list(entry) || !is.list(entry$settings)) {
    return(entry)
  }
  settings <- entry$settings
  settings$spatial_image_storage <- "external"
  entry$settings <- settings
  if (is.null(settings$spatial_coordinate_transforms)) {
    settings$spatial_coordinate_transforms <- list()
    entry$settings <- settings
  }
  if (is.null(settings$spatial_point_appearance)) {
    settings$spatial_point_appearance <- list()
    entry$settings <- settings
  }
  if (is.null(settings$spatial_roi_settings)) {
    settings$spatial_roi_settings <- list()
    entry$settings <- settings
  }
  if (!is.null(settings$spatial_coordinate_transforms)) {
    settings$spatial_coordinate_transforms <-
      .builder_state_spatial_coordinate_transforms(
        settings$spatial_coordinate_transforms
      )
    entry$settings <- settings
  }
  settings$spatial_point_appearance <- .builder_state_spatial_point_appearance(
    settings$spatial_point_appearance
  )
  settings$spatial_roi_settings <- .builder_state_spatial_roi_settings(
    settings$spatial_roi_settings
  )
  entry$settings <- settings
  ## Schema-1 entries created before the shared opacity control may contain a
  ## missing or zero value.  Normalize that legacy field before the canonical
  ## state validator sees it; current UI edits cannot produce an invalid value.
  point_opacity <- suppressWarnings(as.numeric(settings$overview_point_opacity))
  if (
    length(point_opacity) != 1L ||
      is.na(point_opacity) ||
      !is.finite(point_opacity) ||
      point_opacity < 0.1 ||
      point_opacity > 1
  ) {
    settings$overview_point_opacity <- 1
    entry$settings <- settings
  }
  # Legacy Builder allowed zero; Viewer requires a visible point diameter.
  if (
    is.numeric(settings$overview_point_size) &&
      length(settings$overview_point_size) == 1L &&
      identical(as.numeric(settings$overview_point_size), 0)
  ) {
    settings$overview_point_size <- 1
    entry$settings <- settings
  }
  if (identical(settings$viewer_content_schema_version, 1L)) {
    if (!"overview_percentage_cells_to_show" %in% names(settings)) {
      settings$overview_percentage_cells_to_show <-
        builder_default_percentage_cells_to_show(entry$profile$n_cells)
    }
    if (!"initial_projections" %in% names(settings)) {
      projections <- .builder_state_viewer_ids(
        settings$included_projections %||% character()
      )
      default_projection <- settings$default_projection
      settings$initial_projections <- if (
        .builder_state_text(default_projection) &&
          default_projection %in% projections
      ) {
        default_projection
      } else if (length(projections)) {
        projections[[1L]]
      } else {
        character()
      }
    }
    entry$settings <- settings
    return(entry)
  }
  catalog <- .builder_state_viewer_catalog(entry)
  recommendations <- settings$recommendations
  group_recommendation <- if (
    is.list(recommendations) && is.list(recommendations$groups)
  ) {
    recommendations$groups
  } else {
    list()
  }
  projection_recommendation <- if (
    is.list(recommendations) && is.list(recommendations$projections)
  ) {
    recommendations$projections
  } else {
    list()
  }

  groups <- settings$included_groups
  if (is.null(groups)) {
    groups <- group_recommendation$included
  }
  if (is.null(groups)) {
    groups <- settings$groups
  }
  if (is.null(groups)) {
    groups <- catalog$groups
  }
  groups <- .builder_state_viewer_ids(groups)
  if (length(catalog$groups)) {
    groups <- intersect(catalog$groups, groups)
  }
  if (!length(groups) && length(catalog$groups)) {
    suggested <- group_recommendation$value
    groups <- if (
      .builder_state_text(suggested) && suggested %in% catalog$groups
    ) {
      suggested
    } else {
      catalog$groups[[1L]]
    }
  }
  default_group <- settings$default_group
  if (!.builder_state_text(default_group) || !default_group %in% groups) {
    suggested <- group_recommendation$value
    default_group <- if (
      .builder_state_text(suggested) && suggested %in% groups
    ) {
      suggested
    } else if (length(groups)) {
      groups[[1L]]
    } else {
      NULL
    }
  }

  cell_cycle <- settings$cell_cycle_columns
  if (is.null(cell_cycle)) {
    cell_cycle <- catalog$cell_cycle
  }
  cell_cycle <- intersect(
    catalog$cell_cycle,
    .builder_state_viewer_ids(cell_cycle)
  )

  projections <- settings$included_projections
  if (is.null(projections)) {
    projections <- projection_recommendation$included
  }
  if (is.null(projections)) {
    projections <- settings$reductions
  }
  if (is.null(projections)) {
    projections <- catalog$projections
  }
  projections <- .builder_state_viewer_ids(projections)
  if (length(catalog$projections)) {
    projections <- intersect(catalog$projections, projections)
  }
  if (!length(projections) && length(catalog$projections)) {
    suggested <- projection_recommendation$value
    projections <- if (
      .builder_state_text(suggested) && suggested %in% catalog$projections
    ) {
      suggested
    } else {
      catalog$projections[[1L]]
    }
  }
  default_projection <- settings$default_projection
  if (
    !.builder_state_text(default_projection) ||
      !default_projection %in% projections
  ) {
    suggested <- projection_recommendation$value
    default_projection <- if (
      .builder_state_text(suggested) && suggested %in% projections
    ) {
      suggested
    } else if (length(projections)) {
      projections[[1L]]
    } else {
      NULL
    }
  }
  initial_projections <- .builder_state_viewer_ids(
    settings$initial_projections %||% character()
  )
  initial_projections <- projections[projections %in% initial_projections]
  if (!length(initial_projections) && !is.null(default_projection)) {
    initial_projections <- default_projection
  }
  if (
    !is.null(default_projection) &&
      !default_projection %in% initial_projections
  ) {
    initial_projections <- c(default_projection, initial_projections)
  }

  included_trajectories <- if ("included_trajectories" %in% names(settings)) {
    .builder_state_filter_trajectories(
      settings$included_trajectories,
      catalog$trajectories
    )
  } else {
    catalog$trajectories
  }
  default_trajectory <- settings$default_trajectory
  if (
    !.builder_state_trajectory_included(
      default_trajectory,
      included_trajectories
    )
  ) {
    default_trajectory <- .builder_state_first_trajectory(
      included_trajectories
    )
  }

  overrides <- settings$group_color_overrides
  if (is.null(overrides)) {
    overrides <- settings$color_overrides
  }
  if (is.null(overrides)) {
    overrides <- settings$colors
  }
  if (!is.list(overrides) || is.object(overrides)) {
    overrides <- list()
  }
  point_size <- settings$overview_point_size
  if (
    !is.numeric(point_size) ||
      length(point_size) != 1L ||
      is.na(point_size) ||
      !is.finite(point_size) ||
      point_size < 1 ||
      point_size > 20
  ) {
    point_size <- 5
  }
  percentage_cells_to_show <- settings$overview_percentage_cells_to_show
  if (
    !is.numeric(percentage_cells_to_show) ||
      length(percentage_cells_to_show) != 1L ||
      is.na(percentage_cells_to_show) ||
      !is.finite(percentage_cells_to_show) ||
      percentage_cells_to_show < 10 ||
      percentage_cells_to_show > 100
  ) {
    percentage_cells_to_show <- builder_default_percentage_cells_to_show(
      entry$profile$n_cells
    )
  }

  settings$viewer_content_schema_version <- 1L
  settings$included_groups <- groups
  settings["default_group"] <- list(default_group)
  settings$cell_cycle_columns <- cell_cycle
  settings$group_color_overrides <- overrides
  settings$included_projections <- projections
  settings["default_projection"] <- list(default_projection)
  settings$initial_projections <- initial_projections
  settings$overview_point_size <- as.numeric(point_size)
  point_opacity <- suppressWarnings(as.numeric(settings$overview_point_opacity))
  if (
    length(point_opacity) != 1L ||
      is.na(point_opacity) ||
      !is.finite(point_opacity) ||
      point_opacity < 0.1 ||
      point_opacity > 1
  ) {
    point_opacity <- 1
  }
  settings$overview_point_opacity <- point_opacity
  settings$spatial_percentage_cells_to_show <- settings$spatial_percentage_cells_to_show %||%
    100
  settings$overview_percentage_cells_to_show <- as.numeric(
    percentage_cells_to_show
  )
  settings$included_trajectories <- included_trajectories
  settings["default_trajectory"] <- list(default_trajectory)
  # Keep the two legacy aliases synchronized while older planning code is
  # upgraded to consume the canonical fields.
  settings$groups <- groups
  settings$reductions <- projections
  entry$settings <- settings
  entry
}

.builder_state_spatial_coordinate_transforms <- function(value) {
  if (is.null(value)) {
    return(list())
  }
  transform_names <- names(value)
  if (
    !is.list(value) ||
      is.object(value) ||
      (length(value) &&
        (is.null(transform_names) ||
          !is.character(transform_names) ||
          is.object(transform_names) ||
          anyNA(transform_names) ||
          any(!nzchar(transform_names)) ||
          anyDuplicated(transform_names)))
  ) {
    .builder_state_abort(
      "invalid_spatial_coordinate_transform",
      "Spatial coordinate transforms must be an ordinary named list of FOV specifications."
    )
  }
  normalized <- tryCatch(
    lapply(
      transform_names,
      function(section) {
        .spx_coordinate_transform_spec_normalize(
          value[[section]],
          context = paste0("spatial_coordinate_transforms$", section)
        )
      }
    ),
    error = function(error) error
  )
  if (inherits(normalized, "condition")) {
    .builder_state_abort(
      "invalid_spatial_coordinate_transform",
      conditionMessage(normalized)
    )
  }
  names(normalized) <- transform_names
  normalized
}

.builder_state_spatial_point_appearance <- function(value) {
  if (is.null(value)) {
    return(list())
  }
  sections <- names(value)
  if (
    !is.list(value) ||
      is.object(value) ||
      (length(value) &&
        (is.null(sections) ||
          !is.character(sections) ||
          is.object(sections) ||
          anyNA(sections) ||
          any(!nzchar(sections)) ||
          anyDuplicated(sections)))
  ) {
    .builder_state_abort(
      "invalid_spatial_point_appearance",
      "Spatial point appearance must be an ordinary named list of FOV settings."
    )
  }
  normalized <- lapply(sections, function(section) {
    record <- value[[section]]
    opacity <- suppressWarnings(as.numeric(record$point_opacity))
    size <- suppressWarnings(as.numeric(record$point_size))
    if (
      !is.list(record) ||
        !identical(sort(names(record)), c("point_opacity", "point_size")) ||
        length(opacity) != 1L ||
        is.na(opacity) ||
        !is.finite(opacity) ||
        opacity < 0 ||
        opacity > 1 ||
        length(size) != 1L ||
        is.na(size) ||
        !is.finite(size) ||
        size <= 0
    ) {
      .builder_state_abort(
        "invalid_spatial_point_appearance",
        paste0("Spatial point appearance for ", section, " is invalid.")
      )
    }
    list(point_opacity = opacity, point_size = size)
  })
  names(normalized) <- sections
  normalized
}

.builder_state_spatial_roi_leaf <- function(value, context) {
  fields <- c("rotation_degrees", "point_opacity", "point_size")
  numbers <- suppressWarnings(as.numeric(unlist(value[fields])))
  if (
    !.builder_state_plain_record(value) ||
      !identical(sort(names(value)), sort(fields)) ||
      length(numbers) != 3L ||
      anyNA(numbers) ||
      any(!is.finite(numbers)) ||
      numbers[[2L]] < 0 ||
      numbers[[2L]] > 1 ||
      numbers[[3L]] <= 0
  ) {
    .builder_state_abort(
      "invalid_spatial_roi_settings",
      paste0("Spatial ROI settings for ", context, " are invalid.")
    )
  }
  list(
    rotation_degrees = numbers[[1L]],
    point_opacity = numbers[[2L]],
    point_size = numbers[[3L]]
  )
}

.builder_state_spatial_roi_settings <- function(value) {
  if (is.null(value)) {
    return(list())
  }
  if (!.builder_state_plain_record(value)) {
    .builder_state_abort(
      "invalid_spatial_roi_settings",
      "Spatial ROI settings must be an ordinary named list of FOVs."
    )
  }
  normalized <- lapply(names(value), function(section) {
    rois <- value[[section]]
    if (!.builder_state_plain_record(rois)) {
      .builder_state_abort(
        "invalid_spatial_roi_settings",
        paste0("Spatial ROI settings for ", section, " are invalid.")
      )
    }
    result <- lapply(names(rois), function(roi) {
      .builder_state_spatial_roi_leaf(
        rois[[roi]],
        paste0(section, "/", roi)
      )
    })
    names(result) <- names(rois)
    result
  })
  names(normalized) <- names(value)
  normalized
}

.builder_state_validate_viewer_content_settings <- function(entry) {
  settings <- entry$settings
  if (!is.null(settings$viewer_display)) {
    builderViewerDisplayDefaults(settings$viewer_display)
  }
  if (!identical(settings$viewer_content_schema_version, 1L)) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Viewer content settings require schema version 1."
    )
  }
  validate_ids <- function(value, label) {
    if (
      !is.character(value) ||
        is.object(value) ||
        anyNA(value) ||
        any(!nzchar(trimws(value))) ||
        anyDuplicated(value)
    ) {
      .builder_state_abort(
        "invalid_viewer_content_settings",
        paste(label, "must contain unique stable names.")
      )
    }
    value
  }
  groups <- validate_ids(settings$included_groups, "Included groups")
  cell_cycle <- validate_ids(
    settings$cell_cycle_columns %||% character(),
    "Cell-cycle annotations"
  )
  available_cell_cycle <- .builder_state_viewer_catalog(entry)$cell_cycle
  if (length(setdiff(cell_cycle, available_cell_cycle))) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Cell-cycle annotations must belong to the detected phase catalog."
    )
  }
  projections <- validate_ids(
    settings$included_projections,
    "Included projections"
  )
  validate_default <- function(value, included, label) {
    if (!length(included)) {
      if (!is.null(value)) {
        .builder_state_abort(
          "invalid_viewer_content_default",
          paste(label, "must be empty when nothing is included.")
        )
      }
      return(invisible(NULL))
    }
    if (!.builder_state_fact_text(value) || !value %in% included) {
      .builder_state_abort(
        "invalid_viewer_content_default",
        paste(label, "must belong to its included set.")
      )
    }
    invisible(value)
  }
  validate_default(settings$default_group, groups, "Default group")
  validate_default(
    settings$default_projection,
    projections,
    "Default projection"
  )
  initial_projections <- validate_ids(
    settings$initial_projections,
    "Initial projections"
  )
  if (
    length(initial_projections) !=
      length(intersect(
        initial_projections,
        projections
      ))
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Initial projections must belong to the included projections."
    )
  }
  if (length(projections) && !length(initial_projections)) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Keep at least one projection visible initially."
    )
  }
  if (
    length(projections) &&
      !settings$default_projection %in% initial_projections
  ) {
    .builder_state_abort(
      "invalid_viewer_content_default",
      "Default projection must be visible initially."
    )
  }
  overrides <- settings$group_color_overrides
  if (!.builder_state_plain_list(overrides)) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Group color overrides must be an inert named list."
    )
  }
  for (group in names(overrides)) {
    values <- overrides[[group]]
    if (
      !is.character(values) ||
        is.null(names(values)) ||
        anyNA(names(values)) ||
        any(!nzchar(names(values))) ||
        anyDuplicated(names(values))
    ) {
      .builder_state_abort(
        "invalid_viewer_content_settings",
        "Each group color override requires stable level names."
      )
    }
  }
  point_size <- settings$overview_point_size
  if (
    !is.numeric(point_size) ||
      length(point_size) != 1L ||
      is.na(point_size) ||
      !is.finite(point_size) ||
      point_size < 1 ||
      point_size > 20
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Initial point size must be between 0 and 20."
    )
  }
  point_opacity <- settings$overview_point_opacity
  if (
    !is.numeric(point_opacity) ||
      length(point_opacity) != 1L ||
      is.na(point_opacity) ||
      !is.finite(point_opacity) ||
      point_opacity < 0.1 ||
      point_opacity > 1
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Initial point opacity must be between 0.1 and 1."
    )
  }
  percentage_cells_to_show <- settings$overview_percentage_cells_to_show
  if (
    !is.numeric(percentage_cells_to_show) ||
      length(percentage_cells_to_show) != 1L ||
      is.na(percentage_cells_to_show) ||
      !is.finite(percentage_cells_to_show) ||
      percentage_cells_to_show < 10 ||
      percentage_cells_to_show > 100
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Initial cells shown must be between 10 and 100 percent."
    )
  }

  spatial_percentage <- settings$spatial_percentage_cells_to_show %||% 100
  if (
    !is.numeric(spatial_percentage) ||
      length(spatial_percentage) != 1L ||
      !is.finite(spatial_percentage) ||
      spatial_percentage < 10 ||
      spatial_percentage > 100
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Spatial cells shown must be between 10 and 100 percent."
    )
  }
  trajectories <- settings$included_trajectories
  if (!.builder_state_plain_list(trajectories)) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Included trajectories must be an inert method catalog."
    )
  }
  methods <- names(trajectories)
  if (
    length(trajectories) &&
      (is.null(methods) ||
        anyNA(methods) ||
        any(!nzchar(methods)) ||
        anyDuplicated(methods))
  ) {
    .builder_state_abort(
      "invalid_viewer_content_settings",
      "Included trajectories require unique method names."
    )
  }
  invisible(lapply(
    trajectories,
    validate_ids,
    label = "Included trajectory names"
  ))
  default_trajectory <- settings$default_trajectory
  if (length(trajectories)) {
    if (!.builder_state_trajectory_included(default_trajectory, trajectories)) {
      .builder_state_abort(
        "invalid_viewer_content_default",
        "Default trajectory must belong to the included trajectories."
      )
    }
  } else if (!is.null(default_trajectory)) {
    .builder_state_abort(
      "invalid_viewer_content_default",
      "Default trajectory must be empty when no trajectory is included."
    )
  }
  invisible(entry)
}

.builder_state_validate_entry <- function(entry) {
  if (!is.list(entry) || is.object(entry)) {
    .builder_state_abort(
      "invalid_dataset_entry",
      "Dataset state requires one inert dataset entry."
    )
  }
  modern_profile <- .subset2(entry, "dataset_profile")
  legacy_profile <- .subset2(entry, "profile")
  invalid_profile <-
    (!is.null(modern_profile) && !is.list(modern_profile)) ||
    (!is.null(legacy_profile) && !is.list(legacy_profile))
  modern <- is.list(modern_profile) &&
    any(
      c(
        "schema_version",
        "identity",
        "metadata",
        "manifest",
        "content"
      ) %in%
        names(modern_profile)
    )
  typed_modern <- inherits(legacy_profile, "builder_dataset_profile")
  legacy <- is.list(legacy_profile) &&
    any(
      c(
        "default_assay",
        "assay_profiles",
        "nUMI",
        "nGene",
        "extras"
      ) %in%
        names(legacy_profile)
    )
  artifact <- .subset2(entry, "project_artifact")
  artifact_ready <- identical(
    .subset2(entry, "load_state"),
    "artifact_ready"
  ) &&
    is.list(artifact) &&
    !is.object(artifact) &&
    identical(.subset2(artifact, "status"), "ready") &&
    isTRUE(.subset2(artifact, "reusable")) &&
    .builder_state_fact_text(.subset2(artifact, "path")) &&
    is.list(.subset2(artifact, "plan_item"))
  if (
    invalid_profile ||
      !(modern || typed_modern || legacy || artifact_ready)
  ) {
    .builder_state_abort(
      "invalid_dataset_entry",
      "Dataset state requires a recognized modern or legacy profile."
    )
  }
  settings <- .subset2(entry, "settings")
  for (field in c("id", "source_id", "output_id")) {
    value <- .subset2(entry, field)
    if (!is.null(value) && !.builder_state_fact_text(value)) {
      .builder_state_abort(
        "invalid_dataset_entry",
        paste("Dataset", field, "must be plain scalar text.")
      )
    }
  }
  if (!.builder_state_plain_record(settings, recursive = FALSE)) {
    .builder_state_abort(
      "invalid_dataset_settings",
      "Dataset settings must be a plain inert record."
    )
  }
  storage <- .subset2(settings, "spatial_image_storage")
  if (!is.null(storage) && !identical(storage, "external")) {
    .builder_state_abort(
      "invalid_spatial_image_storage",
      "Builder Spatial images must use external storage."
    )
  }
  .builder_state_spatial_coordinate_transforms(
    .subset2(settings, "spatial_coordinate_transforms")
  )
  .builder_state_spatial_point_appearance(
    .subset2(settings, "spatial_point_appearance")
  )
  .builder_state_spatial_roi_settings(
    .subset2(settings, "spatial_roi_settings")
  )
  text_vector <- function(value) {
    is.character(value) &&
      !is.object(value) &&
      !anyNA(value) &&
      is.null(attributes(value))
  }
  for (field in c("groups", "reductions")) {
    value <- .subset2(settings, field)
    if (!is.null(value) && !text_vector(value)) {
      .builder_state_abort(
        "invalid_dataset_settings",
        paste("Dataset setting", field, "must be an inert character vector.")
      )
    }
  }
  for (field in c("layer", "nUMI", "nGene")) {
    value <- .subset2(settings, field)
    if (!is.null(value) && !.builder_state_fact_text(value)) {
      .builder_state_abort(
        "invalid_dataset_settings",
        paste("Dataset setting", field, "must be one plain text value.")
      )
    }
  }
  if (!is.null(.subset2(entry, "output_id")) && !artifact_ready) {
    required <- c("name", "groups", "reductions", "layer", "nUMI", "nGene")
    if (!all(required %in% names(settings))) {
      .builder_state_abort(
        "invalid_dataset_settings",
        "An output dataset requires the complete core settings shape."
      )
    }
  }
  invisible(entry)
}

.builder_state_validate_recommendations <- function(entry) {
  settings <- .subset2(entry, "settings")
  recommendations <- .subset2(settings, "recommendations")
  if (is.null(recommendations)) {
    return(invisible(NULL))
  }
  if (!.builder_state_plain_record(recommendations)) {
    .builder_state_abort(
      "invalid_recommendations",
      "Dataset recommendations must be a plain inert record."
    )
  }
  for (id in c("groups", "projections")) {
    record <- .subset2(recommendations, id)
    if (is.null(record)) {
      next
    }
    if (!.builder_state_plain_record(record)) {
      .builder_state_abort(
        "invalid_recommendations",
        paste("The", id, "recommendation must be a plain inert record.")
      )
    }
    included <- .subset2(record, "included")
    if (
      !is.null(included) &&
        (!is.character(included) ||
          anyNA(included) ||
          any(!nzchar(trimws(included))) ||
          anyDuplicated(included))
    ) {
      .builder_state_abort(
        "invalid_recommendations",
        paste("The", id, "included set is invalid.")
      )
    }
  }
  invisible(recommendations)
}
