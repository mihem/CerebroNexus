## -------------------------------------------------------------------------
## Private generated-App assembly contracts.
##
## This file is loaded by the Builder parent and disposable worker. Generated
## Apps never source it and never need the CerebroNexus package at runtime.
## -------------------------------------------------------------------------

.builder_app_request_fields <- c(
  "contract_version",
  "stage",
  "cerebro_data",
  "crb_identities",
  "selector_order",
  "dataset_labels",
  "initial_dataset",
  "initial_dataset_mode",
  "initial_page",
  "show_upload_ui",
  "welcome_message",
  "point_size",
  "viewer_content",
  "variable_to_compare",
  "host",
  "port",
  "max_request_size",
  "display_mode",
  "launch_browser",
  "auth",
  "colors",
  "crb_pick_smallest_file",
  "backend_plan",
  "backend_identities",
  "spatial_molecule_identities",
  "content_identities",
  "spatial_images",
  "spatial_image_settings",
  "spatial_image_identities"
)

.builder_app_identity_fields <- c(
  "label",
  "path",
  "size",
  "modification_time",
  "change_time",
  "device_id",
  "inode",
  "hard_links",
  "permissions",
  "md5"
)

.builder_app_backend_identity_fields <- c(
  "type",
  "root",
  "root_fingerprint",
  "entries"
)
.builder_app_backend_file_entry_fields <- c("path", "type", "identity")
.builder_app_backend_directory_entry_fields <- c(
  "path",
  "type",
  "fingerprint"
)
.builder_app_fingerprint_fields <- c(
  "type",
  "size",
  "permissions",
  "device_id",
  "inode",
  "hard_links",
  "modification_time",
  "change_time"
)
.builder_app_config_max_bytes <- 16 * 1024^2
.builder_app_inert_max_objects <- 100000
.builder_app_inert_max_elements <- 100000
.builder_app_inert_max_bytes <- 16 * 1024^2

.builder_app_spatial_setting_fields <- c(
  "flip_x",
  "flip_y",
  "scale_x",
  "scale_y",
  "offset_x",
  "offset_y",
  "rotation",
  "image_opacity",
  "point_opacity",
  "point_size"
)

.builder_app_spatial_path_digest <- function(bytes) {
  path <- tempfile("cerebro-builder-spatial-path-")
  on.exit(unlink(path), add = TRUE)
  writeBin(bytes, path)
  unname(tools::md5sum(path))
}

.builder_app_spatial_path_field <- function(value) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop(
      "Spatial image bundle path components must be non-empty strings.",
      call. = FALSE
    )
  }
  bytes <- charToRaw(enc2utf8(value))
  c(charToRaw(sprintf("%08x:", length(bytes))), bytes)
}

.builder_app_spatial_target <- function(dataset, section, label, path) {
  filename <- basename(path)
  fields <- lapply(
    list(dataset, section, label, filename),
    .builder_app_spatial_path_field
  )
  digest <- .builder_app_spatial_path_digest(c(
    charToRaw("cerebro-spatial-image-v1:"),
    do.call(c, fields)
  ))
  extension <- tools::file_ext(filename)
  extension_is_safe <- grepl("^[A-Za-z0-9]{1,16}$", extension)
  target <- paste0(
    "spatial-assets/u",
    digest,
    if (extension_is_safe) paste0(".", extension) else ""
  )
  if (!.builder_app_safe_relative(target)) {
    stop("The spatial image bundle target is unsafe.", call. = FALSE)
  }
  target
}

.builder_app_spatial_target_valid <- function(path, dataset, section, label) {
  fields_are_valid <- tryCatch(
    {
      lapply(
        list(dataset, section, label),
        .builder_app_spatial_path_field
      )
      TRUE
    },
    error = function(error) FALSE
  )
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  isTRUE(fields_are_valid) &&
    length(parts) == 2L &&
    identical(parts[[1L]], "spatial-assets") &&
    nchar(parts[[2L]], type = "bytes") <= 50L &&
    grepl(
      "^u[0-9a-f]{32}([.][A-Za-z0-9]{1,16})?$",
      parts[[2L]]
    )
}

.builder_app_spatial_roi_scope_valid <- function(descriptor) {
  scope <- intersect(c("roi_field", "roi_value"), names(descriptor))
  if (!length(scope)) {
    return(TRUE)
  }
  identical(scope, c("roi_field", "roi_value")) &&
    all(vapply(
      descriptor[scope],
      function(value) {
        is.character(value) &&
          length(value) == 1L &&
          !is.na(value) &&
          nzchar(value)
      },
      logical(1)
    ))
}

.builder_app_config_spatial_manifest_valid <- function(images, selector_order) {
  if (is.null(images)) {
    return(TRUE)
  }
  if (
    !is.list(images) ||
      is.object(images) ||
      any(!names(images) %in% selector_order) ||
      length(names(images)) != length(images) ||
      anyDuplicated(names(images))
  ) {
    return(FALSE)
  }
  for (dataset in names(images)) {
    sections <- images[[dataset]]
    if (
      !is.list(sections) ||
        is.object(sections) ||
        !length(sections) ||
        length(names(sections)) != length(sections) ||
        anyNA(names(sections)) ||
        any(!nzchar(names(sections))) ||
        anyDuplicated(names(sections))
    ) {
      return(FALSE)
    }
    for (section in names(sections)) {
      declarations <- sections[[section]]
      if (
        !is.list(declarations) ||
          is.object(declarations) ||
          !length(declarations) ||
          length(names(declarations)) != length(declarations) ||
          anyNA(names(declarations)) ||
          any(!nzchar(names(declarations))) ||
          anyDuplicated(names(declarations))
      ) {
        return(FALSE)
      }
      for (label in names(declarations)) {
        descriptor <- declarations[[label]]
        path <- if (is.character(descriptor)) descriptor else descriptor$path
        bounds <- if (is.list(descriptor)) descriptor$bounds else NULL
        viewport_bounds <- if (is.list(descriptor)) {
          descriptor$viewport_bounds
        } else {
          NULL
        }
        descriptor_names <- if (is.list(descriptor)) names(descriptor) else NULL
        if (
          !is.character(path) ||
            length(path) != 1L ||
            is.na(path) ||
            !.builder_app_safe_relative(path) ||
            !.builder_app_spatial_target_valid(
              path,
              dataset,
              section,
              label
            ) ||
            (is.list(descriptor) &&
              (is.null(descriptor_names) ||
                anyDuplicated(descriptor_names) ||
                !identical(descriptor_names[[1L]], "path") ||
                any(
                  !descriptor_names %in%
                    c(
                      "path",
                      "bounds",
                      "viewport_bounds",
                      "label",
                      "roi_field",
                      "roi_value"
                    )
                ) ||
                (!is.null(descriptor$label) &&
                  (!is.character(descriptor$label) ||
                    length(descriptor$label) != 1L ||
                    is.na(descriptor$label) ||
                    !nzchar(descriptor$label))) ||
                !.builder_app_spatial_roi_scope_valid(descriptor) ||
                (!is.null(bounds) &&
                  (!is.numeric(bounds) ||
                    length(bounds) != 4L ||
                    anyNA(bounds) ||
                    any(!is.finite(bounds)) ||
                    !identical(
                      names(bounds),
                      c("xmin", "xmax", "ymin", "ymax")
                    ))) ||
                (!is.null(viewport_bounds) &&
                  (!is.numeric(viewport_bounds) ||
                    length(viewport_bounds) != 4L ||
                    anyNA(viewport_bounds) ||
                    any(!is.finite(viewport_bounds)) ||
                    !identical(
                      names(viewport_bounds),
                      c("xmin", "xmax", "ymin", "ymax")
                    ))))) ||
            (!is.character(descriptor) && !is.list(descriptor))
        ) {
          return(FALSE)
        }
      }
    }
  }
  TRUE
}

.builder_app_spatial_manifest_valid <- function(
  images,
  settings,
  identities,
  selector_order,
  stage
) {
  if (
    !is.list(images) ||
      is.object(images) ||
      !is.list(settings) ||
      is.object(settings) ||
      !is.list(identities) ||
      is.object(identities) ||
      !identical(names(images), names(settings)) ||
      !identical(names(images), names(identities)) ||
      any(!names(images) %in% selector_order) ||
      anyDuplicated(names(images))
  ) {
    return(FALSE)
  }
  source_paths <- character()
  targets <- character()
  valid <- TRUE
  for (dataset in names(images)) {
    dataset_images <- images[[dataset]]
    dataset_settings <- settings[[dataset]]
    dataset_identities <- identities[[dataset]]
    if (
      !is.list(dataset_images) ||
        is.object(dataset_images) ||
        !length(dataset_images) ||
        length(names(dataset_images)) != length(dataset_images) ||
        anyDuplicated(names(dataset_images)) ||
        anyNA(names(dataset_images)) ||
        any(!nzchar(names(dataset_images))) ||
        !identical(names(dataset_images), names(dataset_settings)) ||
        !identical(names(dataset_images), names(dataset_identities))
    ) {
      valid <- FALSE
      break
    }
    for (section in names(dataset_images)) {
      section_images <- dataset_images[[section]]
      section_settings <- dataset_settings[[section]]
      section_identities <- dataset_identities[[section]]
      if (
        !is.list(section_images) ||
          is.object(section_images) ||
          !length(section_images) ||
          length(names(section_images)) != length(section_images) ||
          anyDuplicated(names(section_images)) ||
          anyNA(names(section_images)) ||
          any(!nzchar(names(section_images))) ||
          !identical(names(section_images), names(section_settings)) ||
          !identical(names(section_images), names(section_identities))
      ) {
        valid <- FALSE
        break
      }
      for (label in names(section_images)) {
        descriptor <- section_images[[label]]
        setting <- section_settings[[label]]
        identity <- section_identities[[label]]
        path <- if (is.list(descriptor)) descriptor$path else NULL
        bounds <- if (is.list(descriptor)) descriptor$bounds else NULL
        viewport_bounds <- if (is.list(descriptor)) {
          descriptor$viewport_bounds
        } else {
          NULL
        }
        canonical <- tryCatch(
          normalizePath(path, winslash = "/", mustWork = TRUE),
          error = function(error) NULL
        )
        identity_label <- paste(dataset, section, label, sep = "/")
        descriptor_names <- names(descriptor)
        if (
          !is.list(descriptor) ||
            is.object(descriptor) ||
            is.null(descriptor_names) ||
            anyDuplicated(descriptor_names) ||
            !identical(descriptor_names[[1L]], "path") ||
            !"bounds" %in% descriptor_names ||
            any(
              !descriptor_names %in%
                c(
                  "path",
                  "bounds",
                  "viewport_bounds",
                  "label",
                  "roi_field",
                  "roi_value"
                )
            ) ||
            (!is.null(descriptor$label) &&
              (!is.character(descriptor$label) ||
                length(descriptor$label) != 1L ||
                is.na(descriptor$label) ||
                !nzchar(descriptor$label))) ||
            !.builder_app_spatial_roi_scope_valid(descriptor) ||
            !is.character(path) ||
            length(path) != 1L ||
            is.na(path) ||
            is.null(canonical) ||
            !identical(path, canonical) ||
            !.builder_app_path_within(path, stage) ||
            .builder_app_is_link(path) ||
            dir.exists(path) ||
            !is.numeric(bounds) ||
            length(bounds) != 4L ||
            anyNA(bounds) ||
            any(!is.finite(bounds)) ||
            !identical(names(bounds), c("xmin", "xmax", "ymin", "ymax")) ||
            (!is.null(viewport_bounds) &&
              (!is.numeric(viewport_bounds) ||
                length(viewport_bounds) != 4L ||
                anyNA(viewport_bounds) ||
                any(!is.finite(viewport_bounds)) ||
                !identical(
                  names(viewport_bounds),
                  c("xmin", "xmax", "ymin", "ymax")
                ))) ||
            !is.list(setting) ||
            is.object(setting) ||
            !identical(names(setting), .builder_app_spatial_setting_fields) ||
            !all(vapply(
              setting[c("flip_x", "flip_y")],
              function(value) {
                is.logical(value) && length(value) == 1L && !is.na(value)
              },
              logical(1)
            )) ||
            !all(vapply(
              setting[setdiff(
                .builder_app_spatial_setting_fields,
                c("flip_x", "flip_y")
              )],
              function(value) {
                is.numeric(value) &&
                  length(value) == 1L &&
                  !is.na(value) &&
                  is.finite(value)
              },
              logical(1)
            )) ||
            setting$image_opacity < 0 ||
            setting$image_opacity > 1 ||
            setting$point_opacity < 0 ||
            setting$point_opacity > 1 ||
            setting$point_size <= 0 ||
            !.builder_app_identity_valid(identity, identity_label, path)
        ) {
          valid <- FALSE
          break
        }
        source_paths <- c(source_paths, path)
        targets <- c(
          targets,
          .builder_app_spatial_target(dataset, section, label, path)
        )
      }
    }
  }
  isTRUE(valid) && !anyDuplicated(source_paths) && !anyDuplicated(targets)
}

.builder_app_options_valid <- function(options, dataset_order) {
  expected <- c(
    "enabled",
    "show_upload_ui",
    "initial_dataset",
    "initial_dataset_mode",
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
  point_size <- if (is.list(options)) options$point_size else NULL
  point_value <- if (is.list(point_size)) {
    point_size$overview_projection_point_size
  } else {
    NULL
  }
  is.list(options) &&
    !is.object(options) &&
    identical(sort(names(options)), sort(expected)) &&
    isTRUE(options$enabled) &&
    is.logical(options$show_upload_ui) &&
    length(options$show_upload_ui) == 1L &&
    !is.na(options$show_upload_ui) &&
    is.character(options$initial_dataset) &&
    length(options$initial_dataset) == 1L &&
    !is.na(options$initial_dataset) &&
    options$initial_dataset %in% dataset_order &&
    is.character(options$initial_dataset_mode) &&
    length(options$initial_dataset_mode) == 1L &&
    !is.na(options$initial_dataset_mode) &&
    options$initial_dataset_mode %in% c("automatic", "explicit") &&
    (!identical(options$initial_dataset_mode, "automatic") ||
      identical(options$initial_dataset, dataset_order[[1L]])) &&
    is.character(options$initial_page) &&
    length(options$initial_page) == 1L &&
    !is.na(options$initial_page) &&
    options$initial_page %in% builder_viewer_known_page_ids() &&
    is.character(options$welcome_message) &&
    length(options$welcome_message) == 1L &&
    !is.na(options$welcome_message) &&
    nzchar(trimws(options$welcome_message)) &&
    is.list(point_size) &&
    !is.object(point_size) &&
    length(names(point_size)) >= 1L &&
    identical(names(point_size)[[1L]], "overview_projection_point_size") &&
    all(
      names(point_size) %in%
        c(
          "overview_projection_point_size",
          "projection_point_opacity"
        )
    ) &&
    is.numeric(point_value) &&
    length(point_value) == 1L &&
    !is.na(point_value) &&
    is.finite(point_value) &&
    point_value >= 0 &&
    point_value <= 20 &&
    (is.null(point_size$projection_point_opacity) ||
      (is.numeric(point_size$projection_point_opacity) &&
        length(point_size$projection_point_opacity) == 1L &&
        !is.na(point_size$projection_point_opacity) &&
        is.finite(point_size$projection_point_opacity) &&
        point_size$projection_point_opacity >= 0.1 &&
        point_size$projection_point_opacity <= 1)) &&
    is.logical(options$variable_to_compare) &&
    length(options$variable_to_compare) == 1L &&
    !is.na(options$variable_to_compare) &&
    is.character(options$host) &&
    length(options$host) == 1L &&
    !is.na(options$host) &&
    nzchar(options$host) &&
    is.numeric(options$port) &&
    length(options$port) == 1L &&
    !is.na(options$port) &&
    is.finite(options$port) &&
    options$port == floor(options$port) &&
    options$port >= 1 &&
    options$port <= 65535 &&
    is.numeric(options$max_request_size) &&
    length(options$max_request_size) == 1L &&
    !is.na(options$max_request_size) &&
    is.finite(options$max_request_size) &&
    options$max_request_size > 0 &&
    is.character(options$display_mode) &&
    length(options$display_mode) == 1L &&
    !is.na(options$display_mode) &&
    options$display_mode %in% c("auto", "normal", "showcase") &&
    is.logical(options$launch_browser) &&
    length(options$launch_browser) == 1L &&
    !is.na(options$launch_browser)
}

.builder_app_auth_request_valid <- function(value) {
  expected <- c(
    "enabled",
    "account_count",
    "timeout_minutes",
    "passphrase_env"
  )
  is.list(value) &&
    !is.object(value) &&
    identical(names(value), expected) &&
    is.logical(value$enabled) &&
    length(value$enabled) == 1L &&
    !is.na(value$enabled) &&
    is.integer(value$account_count) &&
    length(value$account_count) == 1L &&
    !is.na(value$account_count) &&
    value$account_count >= 0L &&
    value$account_count <= .builder_auth_max_accounts &&
    identical(value$timeout_minutes, .builder_auth_timeout_minutes) &&
    identical(
      value$passphrase_env,
      if (isTRUE(value$enabled)) .builder_auth_env_name else NULL
    ) &&
    if (isTRUE(value$enabled)) {
      value$account_count >= 1L
    } else {
      identical(value$account_count, 0L)
    }
}

.builder_app_auth_summary_valid <- function(value) {
  is.list(value) &&
    !is.object(value) &&
    identical(
      names(value),
      c(
        "enabled",
        "account_count",
        "timeout_minutes"
      )
    ) &&
    is.logical(value$enabled) &&
    length(value$enabled) == 1L &&
    !is.na(value$enabled) &&
    is.integer(value$account_count) &&
    length(value$account_count) == 1L &&
    !is.na(value$account_count) &&
    value$account_count >= 0L &&
    value$account_count <= .builder_auth_max_accounts &&
    identical(value$timeout_minutes, .builder_auth_timeout_minutes) &&
    if (isTRUE(value$enabled)) {
      value$account_count >= 1L
    } else {
      identical(value$account_count, 0L)
    }
}

.builder_app_auth_request <- function(app_auth) {
  list(
    enabled = isTRUE(app_auth$enabled),
    account_count = as.integer(app_auth$account_count),
    timeout_minutes = .builder_auth_timeout_minutes,
    passphrase_env = if (isTRUE(app_auth$enabled)) {
      .builder_auth_env_name
    } else {
      NULL
    }
  )
}

.builder_app_viewer_content_valid <- function(value, selector_order) {
  if (
    !is.list(value) ||
      is.object(value) ||
      !identical(names(value), selector_order)
  ) {
    return(FALSE)
  }
  all(vapply(
    value,
    function(item) {
      if (
        !is.list(item) ||
          is.object(item) ||
          !identical(
            names(item),
            c(
              "default_projection",
              "initial_projections",
              "default_trajectory",
              "overview_point_size",
              "overview_point_opacity",
              "overview_percentage_cells_to_show",
              "spatial_percentage_cells_to_show",
              "spatial_point_appearance",
              "spatial_roi_settings",
              "display",
              "gene_expression",
              "extra_material_table_index"
            )
          )
      ) {
        return(FALSE)
      }
      projection <- item$default_projection
      initial_projections <- item$initial_projections
      trajectory <- item$default_trajectory
      point_size <- item$overview_point_size
      point_opacity <- item$overview_point_opacity
      percentage_cells_to_show <- item$overview_percentage_cells_to_show
      spatial_point_appearance <- item$spatial_point_appearance
      spatial_roi_settings <- item$spatial_roi_settings
      projection_valid <- is.null(projection) ||
        (is.character(projection) &&
          length(projection) == 1L &&
          !is.na(projection) &&
          nzchar(projection))
      initial_projections_valid <-
        is.character(initial_projections) &&
        !is.object(initial_projections) &&
        !anyNA(initial_projections) &&
        all(nzchar(trimws(initial_projections))) &&
        !anyDuplicated(initial_projections) &&
        if (is.null(projection)) {
          !length(initial_projections)
        } else {
          length(initial_projections) &&
            identical(initial_projections[[1L]], projection)
        }
      trajectory_valid <- is.null(trajectory) ||
        (is.list(trajectory) &&
          !is.object(trajectory) &&
          identical(names(trajectory), c("method", "name")) &&
          all(vapply(
            trajectory,
            function(field) {
              is.character(field) &&
                length(field) == 1L &&
                !is.na(field) &&
                nzchar(field)
            },
            logical(1)
          )))
      isTRUE(tryCatch(
        identical(item$display, builderViewerDisplayDefaults(item$display)),
        error = function(e) FALSE
      )) &&
        identical(
          item$extra_material_table_index,
          viewerExtraMaterialTableIndex(item$extra_material_table_index)
        ) &&
        identical(
          item$gene_expression,
          list(
            palette = item$display$gene_palette,
            panel_mode = item$display$gene_panel_mode,
            panel_palette = item$display$gene_panel_palette
          )
        ) &&
        projection_valid &&
        initial_projections_valid &&
        trajectory_valid &&
        is.numeric(point_size) &&
        length(point_size) == 1L &&
        !is.na(point_size) &&
        is.finite(point_size) &&
        point_size >= 1 &&
        point_size <= 20 &&
        is.numeric(point_opacity) &&
        length(point_opacity) == 1L &&
        !is.na(point_opacity) &&
        is.finite(point_opacity) &&
        point_opacity >= 0.1 &&
        point_opacity <= 1 &&
        is.numeric(percentage_cells_to_show) &&
        length(percentage_cells_to_show) == 1L &&
        !is.na(percentage_cells_to_show) &&
        is.finite(percentage_cells_to_show) &&
        percentage_cells_to_show >= 10 &&
        percentage_cells_to_show <= 100 &&
        is.numeric(item$spatial_percentage_cells_to_show) &&
        length(item$spatial_percentage_cells_to_show) == 1L &&
        is.finite(item$spatial_percentage_cells_to_show) &&
        item$spatial_percentage_cells_to_show >= 10 &&
        item$spatial_percentage_cells_to_show <= 100 &&
        .builder_app_spatial_point_appearance_valid(
          spatial_point_appearance
        ) &&
        .builder_app_spatial_roi_settings_valid(spatial_roi_settings)
    },
    logical(1)
  ))
}

.builder_app_spatial_point_appearance_valid <- function(value) {
  viewerNamedSettings(value) &&
    all(vapply(
      value,
      function(leaf) {
        !is.null(viewerSpatialPointSetting(leaf))
      },
      logical(1)
    ))
}

.builder_app_spatial_roi_settings_valid <- function(value) {
  viewerNamedSettings(value) &&
    all(vapply(
      value,
      function(rois) {
        viewerNamedSettings(rois) &&
          all(vapply(
            rois,
            function(leaf) {
              !is.null(viewerSpatialPointSetting(leaf, roi = TRUE))
            },
            logical(1)
          ))
      },
      logical(1)
    ))
}

.builder_app_table_index <- function(tables) {
  if (!is.list(tables) || is.object(tables)) {
    return(list())
  }
  index <- list()
  for (table in tables) {
    if (!is.list(table) || is.object(table)) {
      next
    }
    key <- table$name
    if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
      next
    }
    index[[key]] <- list(
      workbook_name = table$workbook_name %||%
        basename(table$file_name %||% ""),
      display_name = table$display_name %||%
        table$sheet_name %||%
        table$sheet %||%
        key
    )
  }
  viewerExtraMaterialTableIndex(index)
}

.builder_app_viewer_content <- function(items, labels, fallback_point_size) {
  fallback <- fallback_point_size$overview_projection_point_size
  fallback_opacity <- fallback_point_size$projection_point_opacity %||% 1
  values <- lapply(items, function(item) {
    point_size <- item$overview_point_size
    if (
      !is.numeric(point_size) ||
        length(point_size) != 1L ||
        is.na(point_size) ||
        !is.finite(point_size) ||
        point_size < 1 ||
        point_size > 20
    ) {
      point_size <- fallback
    }
    point_opacity <- item$overview_point_opacity
    if (
      !is.numeric(point_opacity) ||
        length(point_opacity) != 1L ||
        is.na(point_opacity) ||
        !is.finite(point_opacity) ||
        point_opacity < 0.1 ||
        point_opacity > 1
    ) {
      point_opacity <- fallback_opacity
    }
    percentage_cells_to_show <- item$overview_percentage_cells_to_show
    if (
      !is.numeric(percentage_cells_to_show) ||
        length(percentage_cells_to_show) != 1L ||
        is.na(percentage_cells_to_show) ||
        !is.finite(percentage_cells_to_show) ||
        percentage_cells_to_show < 10 ||
        percentage_cells_to_show > 100
    ) {
      percentage_cells_to_show <- 100
    }
    projection <- item$default_projection
    if (
      !is.character(projection) ||
        length(projection) != 1L ||
        is.na(projection) ||
        !nzchar(projection)
    ) {
      projection <- NULL
    }
    initial_projections <- item$initial_projections
    if (
      !is.character(initial_projections) ||
        is.object(initial_projections) ||
        anyNA(initial_projections) ||
        any(!nzchar(trimws(initial_projections))) ||
        anyDuplicated(initial_projections)
    ) {
      initial_projections <- character()
    }
    if (is.null(projection) && length(initial_projections)) {
      projection <- initial_projections[[1L]]
    }
    if (!is.null(projection)) {
      initial_projections <- c(
        projection,
        initial_projections[initial_projections != projection]
      )
    }
    trajectory <- item$default_trajectory
    if (
      !is.list(trajectory) ||
        is.object(trajectory) ||
        !identical(names(trajectory), c("method", "name")) ||
        any(vapply(
          trajectory,
          function(field) {
            !is.character(field) ||
              length(field) != 1L ||
              is.na(field) ||
              !nzchar(field)
          },
          logical(1)
        ))
    ) {
      trajectory <- NULL
    }
    list(
      default_projection = projection,
      initial_projections = initial_projections,
      default_trajectory = trajectory,
      overview_point_size = as.double(point_size),
      overview_point_opacity = as.double(point_opacity),
      overview_percentage_cells_to_show = as.double(
        percentage_cells_to_show
      ),
      spatial_percentage_cells_to_show = as.double(
        item$spatial_percentage_cells_to_show %||% 100
      ),
      spatial_point_appearance = item$spatial_point_appearance %||% list(),
      spatial_roi_settings = item$spatial_roi_settings %||% list(),
      display = builderViewerDisplayDefaults(item$viewer_display),
      gene_expression = local({
        d <- builderViewerDisplayDefaults(item$viewer_display)
        list(
          palette = d$gene_palette,
          panel_mode = d$gene_panel_mode,
          panel_palette = d$gene_panel_palette
        )
      }),
      extra_material_table_index = viewerExtraMaterialTableIndex(
        item$extra_material_table_index
      )
    )
  })
  names(values) <- labels
  if (!.builder_app_viewer_content_valid(values, labels)) {
    stop("Frozen Viewer-content defaults are invalid.", call. = FALSE)
  }
  values
}

.builder_app_demo_data <- c(
  "extdata/examples/demo_full_tcr_bcr.crb" = "158785eaba63ce40d2287e7fe604919c",
  "extdata/examples/demo_hla_tcr_dextramer.crb" = "e5e962959b1e0349483aafa88ca48aca",
  "extdata/examples/demo_spatial_merfish.crb" = "b0c01e44b6d91e3681819ec1535d1585",
  "extdata/examples/demo_spatial_slideseq.crb" = "c93472444c76588e6a68c3d082f79a02",
  "extdata/examples/demo_spatial_visium.crb" = "d90af44866611f1ed0a8b6bee69fecaf",
  "extdata/examples/demo_spatial_xenium.crb" = "48400923c2ae408858f32c65bef13e19",
  "extdata/examples/demo_spatial.crb" = "b4b6683a1190c1d8e1b0587c65d0d88e",
  "extdata/examples/demo_trekker.crb" = "e1dc3ab3ba79a03b638ee7b5b7985f1d",
  "extdata/examples/example.crb" = "73edd91dea8cdc2f52f1b0d52246de8e",
  "extdata/examples/pbmc_SCE.rds" = "7b388677c44186cc8a6c13036065e1cb",
  "extdata/examples/pbmc_seurat.rds" = "7c0515903aa08f9aead17f190e4d328e"
)

.builder_app_value_bytes <- function(value, kind = typeof(value)) {
  length_value <- length(value)
  switch(
    kind,
    logical = 4 * length_value,
    integer = 4 * length_value,
    double = 8 * length_value,
    complex = 16 * length_value,
    raw = length_value,
    character = {
      sizes <- nchar(value, type = "bytes", keepNA = FALSE)
      8 * length_value + sum(sizes, na.rm = TRUE)
    },
    list = 8 * length_value,
    0
  )
}

.builder_app_has_reference <- function(value, depth = 0L, budget = NULL) {
  kind <- typeof(value)
  if (
    base::isS4(value) ||
      inherits(value, "connection") ||
      kind %in%
        c(
          "environment",
          "externalptr",
          "weakref",
          "closure",
          "builtin",
          "special",
          "language",
          "symbol",
          "expression",
          "pairlist"
        )
  ) {
    return(TRUE)
  }
  class_value <- attr(value, "class", exact = TRUE)
  allowed_classes <- list(
    c("builder_build_plan", "list"),
    c("builder_app_bundle_request", "list"),
    c("builder_app_verification", "list"),
    c("builder_asset_claim", "list"),
    c("builder_manifest_entry", "list"),
    c("builder_content_manifest", "list"),
    c("POSIXct", "POSIXt")
  )
  if (
    !is.null(class_value) &&
      !any(vapply(
        allowed_classes,
        identical,
        logical(1),
        class_value
      ))
  ) {
    return(TRUE)
  }
  if (is.null(budget)) {
    budget <- new.env(parent = emptyenv())
    budget$objects <- 0
    budget$elements <- 0
    budget$bytes <- 0
  }
  plain <- if (is.object(value)) {
    tryCatch(unclass(value), error = function(error) NULL)
  } else {
    value
  }
  if (is.null(plain) && !is.null(value)) {
    return(TRUE)
  }
  value_length <- length(plain)
  budget$objects <- budget$objects + 1
  budget$elements <- budget$elements + max(1, value_length)
  if (
    depth > 64L ||
      budget$objects > .builder_app_inert_max_objects ||
      budget$elements > .builder_app_inert_max_elements
  ) {
    return(TRUE)
  }
  budget$bytes <- budget$bytes + .builder_app_value_bytes(plain, kind)
  if (budget$bytes > .builder_app_inert_max_bytes) {
    return(TRUE)
  }
  attributes_value <- attributes(value)
  if (
    !is.null(attributes_value) &&
      any(vapply(
        unname(attributes_value),
        .builder_app_has_reference,
        logical(1),
        depth = depth + 1L,
        budget = budget
      ))
  ) {
    return(TRUE)
  }
  if (is.list(plain)) {
    return(any(vapply(
      plain,
      .builder_app_has_reference,
      logical(1),
      depth = depth + 1L,
      budget = budget
    )))
  }
  FALSE
}

.builder_app_plain_value <- function(value, depth = 0L) {
  if (depth > 64L) {
    stop("An inert App value is too deep to normalize.", call. = FALSE)
  }
  plain <- value
  attr(plain, "class") <- NULL
  if (is.list(plain)) {
    for (index in seq_along(plain)) {
      plain[index] <- list(
        .builder_app_plain_value(
          plain[[index]],
          depth = depth + 1L
        )
      )
    }
  }
  plain
}

.builder_app_assert_readable_file <- function(
  path,
  .open_file = base::file
) {
  mode <- file.info(path)$mode
  if (
    !identical(.Platform$OS.type, "windows") &&
      (length(mode) != 1L ||
        is.na(mode) ||
        bitwAnd(as.integer(mode), 292L) == 0L)
  ) {
    stop("A staged App regular file is not readable.", call. = FALSE)
  }
  connection <- tryCatch(
    .open_file(path, open = "rb"),
    error = function(error) NULL
  )
  if (is.null(connection) || !inherits(connection, "connection")) {
    stop("A staged App regular file is not readable.", call. = FALSE)
  }
  on.exit(try(close(connection), silent = TRUE), add = TRUE)
  readable <- tryCatch(
    {
      readBin(connection, what = "raw", n = 1L)
      TRUE
    },
    error = function(error) FALSE
  )
  if (!readable) {
    stop("A staged App regular file is not readable.", call. = FALSE)
  }
  invisible(TRUE)
}

.builder_app_regular_file_identity <- function(
  path,
  label = NULL,
  .digest_file = tools::md5sum,
  .previous = NULL
) {
  before <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(before) ||
      !identical(as.character(before$type), "file") ||
      .builder_app_is_link(path)
  ) {
    stop("A staged App regular file is missing or invalid.", call. = FALSE)
  }
  before_fingerprint <- .builder_app_file_fingerprint(before)
  if (!identical(before_fingerprint$hard_links, 1)) {
    stop("A staged App regular file cannot be a hard link.", call. = FALSE)
  }
  .builder_app_assert_readable_file(path)
  canonical <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (is.null(canonical)) {
    stop("A staged App regular file has no stable path.", call. = FALSE)
  }
  fingerprint_fields <- setdiff(names(before_fingerprint), "type")
  reuse_digest <- .builder_app_identity_valid(
    .previous,
    label,
    canonical
  ) &&
    identical(
      .previous[fingerprint_fields],
      before_fingerprint[fingerprint_fields]
    )
  digest <- if (reuse_digest) {
    .previous$md5
  } else {
    tryCatch(
      unname(as.character(.digest_file(path))),
      error = function(error) NA_character_
    )
  }
  after <- tryCatch(
    fs::file_info(path, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    length(digest) != 1L ||
      is.na(digest) ||
      !grepl("^[[:xdigit:]]{32}$", digest) ||
      is.null(after) ||
      !identical(
        before_fingerprint,
        .builder_app_file_fingerprint(after)
      )
  ) {
    stop("A staged App regular file changed while it was read.", call. = FALSE)
  }
  list(
    label = label,
    path = canonical,
    size = before_fingerprint$size,
    modification_time = before_fingerprint$modification_time,
    change_time = before_fingerprint$change_time,
    device_id = before_fingerprint$device_id,
    inode = before_fingerprint$inode,
    hard_links = before_fingerprint$hard_links,
    permissions = before_fingerprint$permissions,
    md5 = tolower(digest)
  )
}

.builder_app_capture_file_identity <- function(
  path,
  label = NULL,
  .digest_file = tools::md5sum,
  .previous = NULL
) {
  .builder_app_regular_file_identity(path, label, .digest_file, .previous)
}

.builder_app_request_error <- function() {
  stop("The generated-App request contract is invalid.", call. = FALSE)
}

.builder_app_permissions_valid <- function(
  value,
  .os_type = .Platform$OS.type
) {
  pattern <- if (identical(.os_type, "windows")) {
    "^[r-][w-][xsS-]$"
  } else {
    "^[r-][w-][xsS-][r-][w-][xsS-][r-][w-][xtT-]$"
  }
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    grepl(pattern, value)
}

.builder_app_identity_valid <- function(identity, label, path) {
  is.list(identity) &&
    !is.object(identity) &&
    identical(names(identity), .builder_app_identity_fields) &&
    identical(identity$label, label) &&
    identical(identity$path, path) &&
    is.double(identity$size) &&
    length(identity$size) == 1L &&
    is.finite(identity$size) &&
    identity$size >= 0 &&
    identity$size == floor(identity$size) &&
    all(vapply(
      identity[c("modification_time", "change_time")],
      function(value) {
        is.double(value) &&
          length(value) == 1L &&
          is.finite(value)
      },
      logical(1)
    )) &&
    all(vapply(
      identity[c("device_id", "inode")],
      function(value) {
        is.double(value) &&
          length(value) == 1L &&
          is.finite(value) &&
          value >= 0 &&
          value == floor(value)
      },
      logical(1)
    )) &&
    is.double(identity$hard_links) &&
    length(identity$hard_links) == 1L &&
    is.finite(identity$hard_links) &&
    identical(identity$hard_links, 1) &&
    .builder_app_permissions_valid(identity$permissions) &&
    is.character(identity$md5) &&
    length(identity$md5) == 1L &&
    !is.na(identity$md5) &&
    grepl("^[0-9a-f]{32}$", identity$md5)
}

.builder_app_portable_file <- function(path, size, md5) {
  list(path = path, type = "file", size = size, md5 = md5)
}

.builder_app_content_identities <- function(
  crb_identities,
  backend_identities,
  backend_plan,
  spatial_molecule_identities
) {
  relative_crbs <- names(backend_plan$entries)
  identities <- lapply(seq_along(relative_crbs), function(index) {
    relative_crb <- relative_crbs[[index]]
    closure <- backend_identities[[relative_crb]]
    backend <- if (identical(closure$type, "embedded")) {
      list(type = "embedded", root = NULL, entries = list())
    } else {
      entries <- lapply(closure$entries, function(entry) {
        path <- paste0("private-data/", entry$path)
        if (identical(entry$type, "directory")) {
          return(list(path = path, type = "directory"))
        }
        .builder_app_portable_file(
          path,
          entry$identity$size,
          entry$identity$md5
        )
      })
      names(entries) <- if (length(entries)) {
        paste0("private-data/", names(closure$entries))
      } else {
        character()
      }
      list(
        type = closure$type,
        root = paste0(
          "private-data/",
          backend_plan$entries[[relative_crb]]$location
        ),
        entries = entries
      )
    }
    spatial_molecules <- spatial_molecule_identities[[relative_crb]]
    spatial_molecules <- if (is.null(spatial_molecules)) {
      list(type = "none", root = NULL, entries = list())
    } else {
      entries <- lapply(spatial_molecules$entries, function(entry) {
        path <- paste0("private-data/", entry$path)
        if (identical(entry$type, "directory")) {
          return(list(path = path, type = "directory"))
        }
        .builder_app_portable_file(
          path,
          entry$identity$size,
          entry$identity$md5
        )
      })
      names(entries) <- if (length(entries)) {
        paste0("private-data/", names(spatial_molecules$entries))
      } else {
        character()
      }
      list(
        type = "directory",
        root = paste0(
          "private-data/",
          basename(spatial_molecules$root)
        ),
        entries = entries
      )
    }
    list(
      crb = .builder_app_portable_file(
        relative_crb,
        crb_identities[[index]]$size,
        crb_identities[[index]]$md5
      ),
      backend = backend,
      spatial_molecules = spatial_molecules
    )
  })
  names(identities) <- relative_crbs
  identities
}
