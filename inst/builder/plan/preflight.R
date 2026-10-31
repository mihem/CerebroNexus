.builder_plan_alignment_outside_count <- function(record) {
  if (!is.list(record)) {
    return(NA_integer_)
  }
  if (is.null(record[["outside"]])) {
    return(0L)
  }
  value <- record[["outside"]]
  if (
    !is.numeric(value) ||
      is.object(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value != floor(value) ||
      value > .Machine$integer.max
  ) {
    return(NA_integer_)
  }
  as.integer(value)
}

.builder_plan_coordinate_transform_specs <- function(
  transforms,
  spatial_sections
) {
  if (is.null(transforms)) {
    return(list())
  }
  names_transforms <- names(transforms)
  if (is.list(transforms) && !is.object(transforms) && !length(transforms)) {
    return(list())
  }
  if (
    !is.list(transforms) ||
      is.object(transforms) ||
      is.null(names_transforms) ||
      !is.character(names_transforms) ||
      is.object(names_transforms) ||
      anyNA(names_transforms) ||
      any(!nzchar(names_transforms)) ||
      anyDuplicated(names_transforms)
  ) {
    stop(
      "Spatial coordinate transforms must be an ordinary named list with unique non-blank FOV names.",
      call. = FALSE
    )
  }
  unknown <- setdiff(names_transforms, spatial_sections)
  if (length(unknown)) {
    stop(
      "Spatial coordinate transforms contain unknown FOV(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  normalizer <- get0(
    ".spx_coordinate_transform_spec_normalize",
    mode = "function",
    inherits = TRUE
  )
  if (is.null(normalizer)) {
    stop(
      "Spatial coordinate transform validation is unavailable.",
      call. = FALSE
    )
  }
  normalized <- lapply(names_transforms, function(section) {
    spec <- normalizer(
      transforms[[section]],
      context = paste0("spatial_coordinate_transforms$", section)
    )
    spec[c("rotation_degrees", "scale")]
  })
  names(normalized) <- names_transforms
  normalized
}

builder_plan_requires_app <- function(entries) {
  if (!is.list(entries)) {
    stop("Builder plan entries must be a list.", call. = FALSE)
  }
  requires_app <- FALSE
  for (entry in entries) {
    if (!is.list(entry) || !is.list(entry$settings)) {
      next
    }
    roi_settings <- entry$settings$spatial_roi_settings %||% list()
    if (!length(roi_settings)) {
      roi_settings <- entry$project_artifact$plan_item$spatial_roi_settings %||%
        list()
    }
    if (length(roi_settings)) {
      requires_app <- TRUE
    }
    alignments <- .builder_plan_partition_alignments(
      entry$settings$images %||% list()
    )
    images <- .builder_plan_flatten_spatial_images(alignments$spatial)
    if (length(images) || !is.null(alignments$trekker)) {
      requires_app <- TRUE
    }
  }
  requires_app
}

.builder_plan_file_stat_key <- function(path) {
  stat <- tryCatch(fs::file_info(path), error = function(error) NULL)
  if (!is.data.frame(stat) || nrow(stat) != 1L) {
    return(NULL)
  }
  value <- function(name) {
    if (!name %in% names(stat)) {
      return(NA_real_)
    }
    suppressWarnings(as.double(stat[[name]][[1L]]))
  }
  values <- vapply(
    c("size", "modification_time", "change_time", "inode", "device_id"),
    value,
    numeric(1)
  )
  if (!all(is.finite(values[c("size", "modification_time")]))) {
    return(NULL)
  }
  paste(
    c(
      normalizePath(path, winslash = "/", mustWork = FALSE),
      sprintf("%.17g", values)
    ),
    collapse = "|"
  )
}

.builder_plan_image_cache_limit <- 512L

# Full-resolution images can be hundreds of MB, and every BuildPlan freeze
# runs on the Shiny process. A session cache keyed by path, size, timestamps
# and file id lets repeated freezes skip rehashing an unchanged file.
.builder_plan_source_md5 <- function(path, cache = NULL) {
  key <- if (is.environment(cache)) {
    .builder_plan_file_stat_key(path)
  } else {
    NULL
  }
  if (!is.null(key)) {
    cached <- get0(key, envir = cache, inherits = FALSE)
    if (is.character(cached) && length(cached) == 1L) {
      return(cached)
    }
  }
  md5 <- unname(as.character(tools::md5sum(path)))
  if (
    !is.null(key) &&
      length(md5) == 1L &&
      !is.na(md5) &&
      identical(.builder_plan_file_stat_key(path), key)
  ) {
    if (
      length(ls(cache, all.names = TRUE)) >= .builder_plan_image_cache_limit
    ) {
      rm(list = ls(cache, all.names = TRUE), envir = cache)
    }
    assign(key, md5, envir = cache)
  }
  md5
}

.builder_plan_preflight_entries <- function(
  entries,
  make_app = FALSE,
  file_verification_cache = NULL
) {
  for (entry in entries) {
    state_error <- tryCatch(
      {
        .builder_state_validate_entry(entry)
        .builder_state_validate_recommendations(entry)
        NULL
      },
      builder_state_error = function(error) error
    )
    if (!is.null(state_error)) {
      code <- if (
        state_error$code %in%
          c(
            "invalid_dataset_entry",
            "invalid_dataset_settings"
          )
      ) {
        "invalid_entries"
      } else {
        state_error$code
      }
      return(builder_plan_error(conditionMessage(state_error), code))
    }
  }

  for (entry in entries) {
    storage <- entry$settings$spatial_image_storage %||% "external"
    if (
      !is.character(storage) ||
        length(storage) != 1L ||
        is.na(storage) ||
        !identical(storage, "external")
    ) {
      return(builder_plan_error(
        "Builder Spatial images must use external storage.",
        "invalid_spatial_image_storage"
      ))
    }
    alignments <- tryCatch(
      .builder_plan_partition_alignments(entry$settings$images %||% list()),
      error = function(error) error
    )
    if (inherits(alignments, "condition")) {
      code <- if (grepl("unique", conditionMessage(alignments), fixed = TRUE)) {
        "duplicate_spatial_image_label"
      } else {
        "invalid_spatial_alignment_diagnostics"
      }
      return(builder_plan_error(conditionMessage(alignments), code))
    }
    images <- tryCatch(
      .builder_plan_flatten_spatial_images(alignments$spatial),
      error = function(error) error
    )
    if (inherits(images, "condition")) {
      code <- if (grepl("unique", conditionMessage(images), fixed = TRUE)) {
        "duplicate_spatial_image_label"
      } else {
        "invalid_spatial_alignment_diagnostics"
      }
      return(builder_plan_error(conditionMessage(images), code))
    }
    if (!is.null(alignments$trekker)) {
      images <- c(
        images,
        list(c(
          list(section_id = "trekker", image_label = "trekker"),
          alignments$trekker
        ))
      )
    }
    for (image in images) {
      source_path <- image[["source_path", exact = TRUE]]
      valid_path <- is.character(source_path) &&
        length(source_path) == 1L &&
        !is.na(source_path) &&
        nzchar(source_path) &&
        file.exists(source_path) &&
        !dir.exists(source_path)
      expected_md5 <- image[["source_content_md5", exact = TRUE]]
      observed_md5 <- if (valid_path) {
        .builder_plan_source_md5(source_path, file_verification_cache)
      } else {
        NULL
      }
      valid_md5 <- is.character(expected_md5) &&
        length(expected_md5) == 1L &&
        !is.na(expected_md5) &&
        grepl("^[[:xdigit:]]{32}$", expected_md5) &&
        identical(
          expected_md5,
          observed_md5
        )
      if (!isTRUE(valid_path) || !isTRUE(valid_md5)) {
        return(builder_plan_error(
          paste0(
            "Dataset “",
            entry$settings$name,
            "”, section “",
            image$section_id,
            "”, image “",
            image$image_label,
            "” has no valid external source file. Re-upload it before building."
          ),
          "invalid_external_spatial_image"
        ))
      }
    }
    if (
      length(images) &&
        !isTRUE(make_app)
    ) {
      return(builder_plan_error(
        "External spatial images require CRB files + Viewer App output.",
        "external_images_require_app"
      ))
    }
    if (
      length(entry$settings$spatial_roi_settings %||% list()) &&
        !isTRUE(make_app)
    ) {
      return(builder_plan_error(
        "Spatial ROI settings require CRB files + Viewer App output.",
        "spatial_roi_settings_require_app"
      ))
    }
    outside_counts <- vapply(
      images,
      .builder_plan_alignment_outside_count,
      integer(1)
    )
    invalid_outside <- which(is.na(outside_counts))
    if (length(invalid_outside)) {
      first <- images[[invalid_outside[[1L]]]]
      return(builder_plan_error(
        paste0(
          "Dataset “",
          entry$settings$name,
          "”, section “",
          first$section_id,
          "”, image “",
          first$image_label,
          "” has invalid image-coverage diagnostics. Re-open and save its alignment before building."
        ),
        "invalid_spatial_alignment_diagnostics"
      ))
    }
    outside <- which(outside_counts > 0L)
    if (length(outside)) {
      first <- images[[outside[[1L]]]]
      return(builder_plan_error(
        paste0(
          "Dataset “",
          entry$settings$name,
          "”, section “",
          first$section_id,
          "”, image “",
          first$image_label,
          "” has cells outside its saved image bounds. Adjust the alignment ",
          "until every cell is covered before building."
        ),
        "spatial_alignment_outside"
      ))
    }
  }

  dataset_ids <- vapply(entries, function(entry) entry$id %||% "", character(1))
  if (any(!vapply(dataset_ids, builder_has_text, logical(1)))) {
    return(builder_plan_error(
      "Every dataset needs a non-empty scalar id.",
      "invalid_dataset_id"
    ))
  }
  if (anyDuplicated(dataset_ids)) {
    return(builder_plan_error(
      "Every dataset needs a unique id.",
      "duplicate_dataset_id"
    ))
  }

  valid_names <- vapply(
    entries,
    function(entry) {
      builder_has_text(entry$settings$name)
    },
    logical(1)
  )
  if (!all(valid_names)) {
    return(builder_plan_error(
      "Every dataset needs a non-empty scalar name.",
      "invalid_dataset_name"
    ))
  }
  labels <- trimws(vapply(
    entries,
    function(entry) entry$settings$name,
    character(1)
  ))
  valid_analyses <- vapply(
    entries,
    function(entry) {
      analyses <- entry$settings$analyses
      isTRUE(tryCatch(
        {
          .builder_state_validate_analyses(analyses)
          TRUE
        },
        builder_state_error = function(error) FALSE
      ))
    },
    logical(1)
  )
  if (!all(valid_analyses)) {
    return(builder_plan_error(
      "Selected analyses must be unique supported analysis ids.",
      "invalid_analyses"
    ))
  }

  valid_core <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      .builder_plan_character_set(settings$groups) &&
        length(settings$groups) > 0L &&
        .builder_plan_character_set(settings$reductions) &&
        length(settings$reductions) > 0L &&
        builder_has_text(settings$organism) &&
        builder_has_text(settings$assay) &&
        builder_has_text(settings$layer)
    },
    logical(1)
  )
  if (!all(valid_core)) {
    return(builder_plan_error(
      paste(
        "Every dataset needs an explicit organism, assay, layer,",
        "grouping variable and reduction."
      ),
      "missing_core_selection"
    ))
  }

  valid_layers <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      profile <- entry$profile %||% list()
      assay_profile <- profile$assay_profiles[[settings$assay]] %||% list()
      layers <- assay_profile$layers %||% NULL
      is.null(layers) || settings$layer %in% layers
    },
    logical(1)
  )
  if (!all(valid_layers)) {
    index <- which(!valid_layers)[[1L]]
    settings <- entries[[index]]$settings
    return(builder_plan_error(
      paste0(
        "Layer `",
        settings$layer,
        "` is no longer available in assay `",
        settings$assay,
        "`. Select an available layer and check this dataset again."
      ),
      "missing_layer"
    ))
  }

  valid_qc <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      profile <- if (is.list(entry$profile)) entry$profile else list()
      builder_has_text(settings$nUMI %||% profile$nUMI) &&
        builder_has_text(settings$nGene %||% profile$nGene)
    },
    logical(1)
  )
  if (!all(valid_qc)) {
    return(builder_plan_error(
      "Every dataset needs explicit UMI/count and feature/gene fields.",
      "missing_qc_selection"
    ))
  }

  included_groups <- lapply(entries, .builder_state_included_groups)
  included_projections <- lapply(entries, function(entry) {
    settings <- entry$settings
    recommendations <- settings$recommendations
    settings$included_projections %||%
      recommendations$projections$included %||%
      settings$reductions
  })
  initial_projections <- Map(
    function(entry, included) {
      values <- entry$settings$initial_projections %||% character()
      if (!length(values) && length(included)) {
        values <- entry$settings$default_projection %||% included[[1L]]
      }
      values
    },
    entries,
    included_projections
  )
  included_trajectories <- lapply(entries, function(entry) {
    if ("included_trajectories" %in% names(entry$settings)) {
      entry$settings$included_trajectories
    } else {
      NULL
    }
  })
  cell_cycle <- lapply(entries, function(entry) {
    entry$settings$cell_cycle_columns %||% character()
  })
  if (
    !all(vapply(
      included_groups,
      .builder_plan_character_set,
      logical(1)
    ))
  ) {
    return(builder_plan_error(
      "The final included group set is invalid.",
      "invalid_included_groups"
    ))
  }
  if (!all(vapply(cell_cycle, .builder_plan_character_set, logical(1)))) {
    return(builder_plan_error(
      "The selected cell-cycle annotation set is invalid.",
      "invalid_cell_cycle_selection"
    ))
  }
  if (
    !all(vapply(
      included_projections,
      .builder_plan_character_set,
      logical(1)
    ))
  ) {
    return(builder_plan_error(
      "The final included projection set is invalid.",
      "invalid_included_projections"
    ))
  }
  if (
    !all(vapply(
      initial_projections,
      .builder_plan_character_set,
      logical(1)
    ))
  ) {
    return(builder_plan_error(
      "The initial projection set is invalid.",
      "invalid_initial_projections"
    ))
  }

  invalid_group_selection <- vapply(
    seq_along(entries),
    function(index) {
      !all(entries[[index]]$settings$groups %in% included_groups[[index]])
    },
    logical(1)
  )
  if (any(invalid_group_selection)) {
    return(builder_plan_error(
      "Selected groups must remain inside the final included group set.",
      "invalid_group_selection"
    ))
  }
  invalid_projection_selection <- vapply(
    seq_along(entries),
    function(index) {
      !all(
        entries[[index]]$settings$reductions %in%
          included_projections[[index]]
      )
    },
    logical(1)
  )
  if (any(invalid_projection_selection)) {
    return(builder_plan_error(
      paste0(
        "Selected projections must remain inside the final included ",
        "projection set."
      ),
      "invalid_projection_selection"
    ))
  }
  invalid_default_group <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      value <- settings$default_group
      if (is.null(settings$recommendations) && is.null(value)) {
        return(FALSE)
      }
      !builder_has_text(value) || !value %in% settings$groups
    },
    logical(1)
  )
  if (any(invalid_default_group)) {
    return(builder_plan_error(
      "Every recommended dataset needs a valid selected default group.",
      "invalid_default_group"
    ))
  }
  invalid_default_projection <- vapply(
    entries,
    function(entry) {
      settings <- entry$settings
      value <- settings$default_projection
      if (is.null(settings$recommendations) && is.null(value)) {
        return(FALSE)
      }
      !builder_has_text(value) || !value %in% settings$reductions
    },
    logical(1)
  )
  if (any(invalid_default_projection)) {
    return(builder_plan_error(
      "Every recommended dataset needs a valid selected default projection.",
      "invalid_default_projection"
    ))
  }
  invalid_initial_projection_selection <- vapply(
    seq_along(entries),
    function(index) {
      (length(included_projections[[index]]) &&
        !length(initial_projections[[index]])) ||
        !all(
          initial_projections[[index]] %in%
            included_projections[[index]]
        )
    },
    logical(1)
  )
  if (any(invalid_initial_projection_selection)) {
    return(builder_plan_error(
      paste0(
        "Initial projections must be a non-empty subset of the included ",
        "projections."
      ),
      "invalid_initial_projection_selection"
    ))
  }
  invalid_initial_default <- vapply(
    seq_along(entries),
    function(index) {
      default <- entries[[index]]$settings$default_projection
      builder_has_text(default) &&
        !default %in% initial_projections[[index]]
    },
    logical(1)
  )
  if (any(invalid_initial_default)) {
    return(builder_plan_error(
      "The default projection must be shown initially.",
      "invalid_initial_default_projection"
    ))
  }

  included_groups <- Map(
    function(values, entry) {
      builder_default_first(values, entry$settings$default_group)
    },
    included_groups,
    entries
  )
  included_projections <- Map(
    function(values, entry) {
      builder_default_first(values, entry$settings$default_projection)
    },
    included_projections,
    entries
  )
  initial_projections <- Map(
    function(values, entry) {
      builder_default_first(values, entry$settings$default_projection)
    },
    initial_projections,
    entries
  )
  included_trajectories <- Map(
    function(values, entry) {
      builder_trajectory_default_first(
        values,
        entry$settings$default_trajectory
      )
    },
    included_trajectories,
    entries
  )

  coordinate_transforms <- tryCatch(
    Map(
      function(entry, groups, projections, trajectories, cycle) {
        saved_identity <- if (
          identical(entry$load_state %||% "loaded", "artifact_ready")
        ) {
          entry$project_artifact$plan_item$artifact_identity %||% NULL
        } else {
          NULL
        }
        identity <- if (is.list(saved_identity)) {
          saved_identity
        } else {
          .builder_plan_artifact_identity(
            entry,
            groups,
            projections,
            entry$settings$analyses %||% character(),
            trajectories,
            cycle
          )
        }
        .builder_plan_coordinate_transform_specs(
          entry$settings$spatial_coordinate_transforms %||% list(),
          identity$spatial_sections %||% character()
        )
      },
      entries,
      included_groups,
      included_projections,
      included_trajectories,
      cell_cycle
    ),
    error = function(error) error
  )
  if (inherits(coordinate_transforms, "condition")) {
    message <- conditionMessage(coordinate_transforms)
    code <- if (identical(message, "invalid_artifact_identity")) {
      "invalid_artifact_identity"
    } else if (grepl("unknown FOV", message, fixed = TRUE)) {
      "unknown_spatial_coordinate_transform"
    } else {
      "invalid_spatial_coordinate_transform"
    }
    return(builder_plan_error(message, code))
  }

  list(
    labels = labels,
    included_groups = included_groups,
    included_projections = included_projections,
    initial_projections = initial_projections,
    included_trajectories = included_trajectories,
    cell_cycle = cell_cycle,
    spatial_coordinate_transforms = coordinate_transforms
  )
}

.builder_plan_app_options_valid <- function(app_options) {
  if (!is.list(app_options) || is.object(app_options)) {
    return(FALSE)
  }
  option_names <- names(app_options)
  if (
    length(app_options) &&
      (is.null(option_names) ||
        anyNA(option_names) ||
        any(!nzchar(option_names)) ||
        anyDuplicated(option_names) ||
        length(setdiff(
          option_names,
          c(
            "show_upload_ui",
            "initial_dataset",
            "initial_page",
            "welcome_message",
            "point_size",
            "variable_to_compare",
            "host",
            "port",
            "max_request_size",
            "display_mode",
            "launch_browser"
          )
        )))
  ) {
    return(FALSE)
  }
  show_upload_ui_supplied <- "show_upload_ui" %in% option_names
  show_upload_ui <- app_options$show_upload_ui
  if (
    show_upload_ui_supplied &&
      (!is.logical(show_upload_ui) ||
        length(show_upload_ui) != 1L ||
        is.na(show_upload_ui))
  ) {
    return(FALSE)
  }
  initial_dataset_supplied <- "initial_dataset" %in% option_names
  initial_dataset <- app_options$initial_dataset
  if (initial_dataset_supplied && !builder_has_text(initial_dataset)) {
    return(FALSE)
  }
  if (
    "initial_page" %in%
      option_names &&
      (!builder_has_text(app_options$initial_page) ||
        !app_options$initial_page %in% builder_viewer_known_page_ids())
  ) {
    return(FALSE)
  }
  if (
    "welcome_message" %in%
      option_names &&
      !builder_has_text(app_options$welcome_message)
  ) {
    return(FALSE)
  }
  if ("point_size" %in% option_names) {
    point_size <- app_options$point_size
    point_names <- if (is.list(point_size)) names(point_size) else NULL
    if (
      !is.list(point_size) ||
        is.object(point_size) ||
        is.null(point_names) ||
        !length(point_names) ||
        !identical(point_names[[1L]], "overview_projection_point_size") ||
        any(
          !point_names %in%
            c(
              "overview_projection_point_size",
              "projection_point_opacity"
            )
        )
    ) {
      return(FALSE)
    }
    opacity <- point_size$projection_point_opacity
    if (
      !is.null(opacity) &&
        (!is.numeric(opacity) ||
          length(opacity) != 1L ||
          is.na(opacity) ||
          !is.finite(opacity) ||
          opacity < 0.1 ||
          opacity > 1)
    ) {
      return(FALSE)
    }
    value <- point_size$overview_projection_point_size
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0 ||
        value > 20
    ) {
      return(FALSE)
    }
  }
  if ("variable_to_compare" %in% option_names) {
    value <- app_options$variable_to_compare
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(FALSE)
    }
  }
  if ("host" %in% option_names && !builder_has_text(app_options$host)) {
    return(FALSE)
  }
  if ("port" %in% option_names) {
    value <- app_options$port
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value != floor(value) ||
        value < 1 ||
        value > 65535
    ) {
      return(FALSE)
    }
  }
  if ("max_request_size" %in% option_names) {
    value <- app_options$max_request_size
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value <= 0 ||
        !is.finite(value * 1024^2)
    ) {
      return(FALSE)
    }
  }
  if (
    "display_mode" %in%
      option_names &&
      (!is.character(app_options$display_mode) ||
        length(app_options$display_mode) != 1L ||
        is.na(app_options$display_mode) ||
        !app_options$display_mode %in% c("auto", "normal", "showcase"))
  ) {
    return(FALSE)
  }
  if ("launch_browser" %in% option_names) {
    value <- app_options$launch_browser
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      return(FALSE)
    }
  }
  TRUE
}
