.builder_app_colors_valid <- function(colors, selector_order) {
  if (
    !is.list(colors) ||
      is.object(colors) ||
      !identical(names(colors), selector_order)
  ) {
    return(FALSE)
  }
  all(vapply(
    colors,
    function(palettes) {
      if (!is.list(palettes) || is.object(palettes)) {
        return(FALSE)
      }
      palette_names <- names(palettes)
      if (
        !length(palettes) ||
          is.null(palette_names) ||
          anyNA(palette_names) ||
          any(!nzchar(palette_names)) ||
          anyDuplicated(palette_names)
      ) {
        return(FALSE)
      }
      all(vapply(
        palettes,
        function(value) {
          level_names <- names(value)
          is.character(value) &&
            !anyNA(value) &&
            !is.null(level_names) &&
            length(level_names) == length(value) &&
            !anyNA(level_names) &&
            all(nzchar(level_names)) &&
            !anyDuplicated(level_names) &&
            isTRUE(tryCatch(
              {
                grDevices::col2rgb(value)
                TRUE
              },
              error = function(error) FALSE
            ))
        },
        logical(1)
      ))
    },
    logical(1)
  ))
}

.builder_app_backend_plan_valid <- function(plan, cerebro_data) {
  if (
    !is.list(plan) ||
      is.object(plan) ||
      !identical(names(plan), c("schema_version", "entries")) ||
      !identical(plan$schema_version, 1L) ||
      !is.list(plan$entries) ||
      is.object(plan$entries) ||
      !identical(
        names(plan$entries),
        file.path("private-data", basename(cerebro_data))
      )
  ) {
    return(FALSE)
  }
  all(vapply(
    seq_along(plan$entries),
    function(index) {
      entry <- plan$entries[[index]]
      if (
        !is.list(entry) ||
          is.object(entry) ||
          !identical(names(entry), c("type", "mode", "location")) ||
          !is.character(entry$type) ||
          length(entry$type) != 1L ||
          is.na(entry$type) ||
          !entry$type %in% c("embedded", "h5") ||
          !is.character(entry$mode) ||
          length(entry$mode) != 1L ||
          is.na(entry$mode)
      ) {
        return(FALSE)
      }
      if (identical(entry$type, "embedded")) {
        identical(entry$mode, "embedded") && is.null(entry$location)
      } else {
        expected_location <- paste0(
          tools::file_path_sans_ext(basename(cerebro_data[[index]])),
          ".h5"
        )
        identical(entry$mode, "bundled") &&
          identical(entry$location, expected_location)
      }
    },
    logical(1)
  ))
}

.builder_app_previous_backend_tree <- function(previous, location) {
  if (
    !is.list(previous) ||
      !identical(previous$type, "directory") ||
      !is.list(previous$entries)
  ) {
    return(NULL)
  }
  prefix <- paste0(location, "/")
  entry_names <- names(previous$entries)
  if (
    is.null(entry_names) ||
      any(!startsWith(entry_names, prefix))
  ) {
    return(NULL)
  }
  relative <- substring(entry_names, nchar(prefix) + 1L)
  entries <- lapply(seq_along(previous$entries), function(index) {
    entry <- previous$entries[[index]]
    if (identical(entry$type, "directory")) {
      return(list(
        path = relative[[index]],
        type = "directory",
        fingerprint = entry$fingerprint
      ))
    }
    identity <- entry$identity
    list(
      path = relative[[index]],
      type = "file",
      size = identity$size,
      modification_time = identity$modification_time,
      change_time = identity$change_time,
      device_id = identity$device_id,
      inode = identity$inode,
      hard_links = identity$hard_links,
      permissions = identity$permissions,
      md5 = identity$md5
    )
  })
  names(entries) <- relative
  list(
    schema_version = 1L,
    root_fingerprint = previous$root_fingerprint,
    entries = entries
  )
}

.builder_app_capture_backend_identity <- function(
  entry,
  crb_path,
  .tree_identity = .builder_app_tree_identity,
  .previous = NULL
) {
  if (identical(entry$type, "embedded")) {
    return(list(
      type = "embedded",
      root = NULL,
      root_fingerprint = NULL,
      entries = list()
    ))
  }
  sidecar <- file.path(dirname(crb_path), entry$location)
  expected_type <- if (identical(entry$type, "h5")) "file" else "directory"
  sidecar_info <- tryCatch(
    fs::file_info(sidecar, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(sidecar_info) ||
      !identical(as.character(sidecar_info$type), expected_type) ||
      .builder_app_is_link(sidecar)
  ) {
    stop(
      "A verified input backend closure is missing or invalid.",
      call. = FALSE
    )
  }
  root <- tryCatch(
    normalizePath(sidecar, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  if (
    is.null(root) ||
      !identical(dirname(root), dirname(crb_path)) ||
      !identical(basename(root), entry$location)
  ) {
    stop("A verified input backend closure has an invalid path.", call. = FALSE)
  }
  if (identical(entry$type, "h5")) {
    previous_identity <- if (
      is.list(.previous) &&
        is.list(.previous$entries) &&
        is.list(.previous$entries[[entry$location]])
    ) {
      .previous$entries[[entry$location]]$identity
    } else {
      NULL
    }
    identity <- .builder_app_capture_file_identity(
      root,
      label = entry$location,
      .previous = previous_identity
    )
    entries <- stats::setNames(
      list(list(path = entry$location, type = "file", identity = identity)),
      entry$location
    )
    return(list(
      type = "h5",
      root = root,
      root_fingerprint = NULL,
      entries = entries
    ))
  }

  tree <- .tree_identity(
    root,
    .previous = .builder_app_previous_backend_tree(
      .previous,
      entry$location
    )
  )
  entries <- lapply(names(tree$entries), function(relative) {
    value <- tree$entries[[relative]]
    mapping <- file.path(entry$location, relative)
    if (identical(value$type, "directory")) {
      return(list(
        path = mapping,
        type = "directory",
        fingerprint = value$fingerprint
      ))
    }
    identity <- list(
      label = mapping,
      path = file.path(root, relative),
      size = value$size,
      modification_time = value$modification_time,
      change_time = value$change_time,
      device_id = value$device_id,
      inode = value$inode,
      hard_links = value$hard_links,
      permissions = value$permissions,
      md5 = value$md5
    )
    list(path = mapping, type = "file", identity = identity)
  })
  names(entries) <- file.path(entry$location, names(tree$entries))
  list(
    type = "directory",
    root = root,
    root_fingerprint = tree$root_fingerprint,
    entries = entries
  )
}

.builder_app_source_runtime <- new.env(parent = globalenv())
.builder_app_source_runtime_root <- NULL

.builder_app_runtime_function <- function(
  name,
  .source_root = {
    source_root <- get0(
      "builder_source_package_root",
      mode = "function",
      inherits = TRUE
    )
    if (is.function(source_root)) source_root() else NULL
  },
  .namespace = function() {
    if (requireNamespace("CerebroNexus", quietly = TRUE)) {
      asNamespace("CerebroNexus")
    } else {
      NULL
    }
  }
) {
  value <- NULL
  if (!is.null(.source_root)) {
    source_root <- normalizePath(.source_root, winslash = "/", mustWork = TRUE)
    runtime_file <- file.path(source_root, "R", "cerebro_io.R")
    if (!file.exists(runtime_file)) {
      stop("The Cerebro source runtime is incomplete.", call. = FALSE)
    }
    if (!identical(.builder_app_source_runtime_root, source_root)) {
      rm(
        list = ls(
          envir = .builder_app_source_runtime,
          all.names = TRUE
        ),
        envir = .builder_app_source_runtime
      )
      builder_source_utf8(runtime_file, envir = .builder_app_source_runtime)
      .builder_app_source_runtime_root <<- source_root
    }
    value <- get0(
      name,
      envir = .builder_app_source_runtime,
      mode = "function",
      inherits = FALSE
    )
  } else {
    value <- get0(name, mode = "function", inherits = TRUE)
    if (is.null(value)) {
      namespace <- .namespace()
      if (is.environment(namespace)) {
        value <- get0(
          name,
          envir = namespace,
          mode = "function",
          inherits = FALSE
        )
      }
    }
  }
  if (!is.function(value)) {
    stop("The Cerebro runtime is unavailable.", call. = FALSE)
  }
  value
}

.builder_app_capture_spatial_molecule_identity <- function(
  crb_path,
  .previous = NULL,
  .verified_backend = NULL,
  .backend_is_verified = FALSE
) {
  backend <- if (isTRUE(.backend_is_verified)) {
    .verified_backend
  } else {
    object <- .builder_app_runtime_function(".readCerebroPayload")(crb_path)
    if (!is.environment(object)) {
      return(NULL)
    }
    value <- .builder_app_runtime_function(".spatialMoleculeBackend")(
      object,
      crb_path
    )
    object <- NULL
    value
  }
  if (is.null(backend)) {
    return(NULL)
  }
  expected <- paste0(
    tools::file_path_sans_ext(basename(crb_path)),
    ".spatial"
  )
  expected_root <- tryCatch(
    normalizePath(
      file.path(dirname(crb_path), expected),
      winslash = "/",
      mustWork = TRUE
    ),
    error = function(error) NULL
  )
  backend_root <- if (isTRUE(.backend_is_verified) && is.null(backend$root)) {
    expected_root
  } else {
    tryCatch(
      normalizePath(backend$root, winslash = "/", mustWork = TRUE),
      error = function(error) NULL
    )
  }
  if (
    !identical(backend$type, "directory") ||
      !identical(backend$location, expected) ||
      is.null(expected_root) ||
      !identical(backend_root, expected_root)
  ) {
    stop(
      "A verified spatial molecule sidecar has an invalid location.",
      call. = FALSE
    )
  }
  .builder_app_capture_backend_identity(
    list(type = "directory", mode = "bundled", location = backend$location),
    crb_path,
    .previous = .previous
  )
}

.builder_app_spatial_molecule_identities_valid <- function(
  identities,
  cerebro_data,
  relative_crbs
) {
  if (
    !is.list(identities) ||
      is.object(identities) ||
      !identical(names(identities), relative_crbs)
  ) {
    return(FALSE)
  }
  all(vapply(
    seq_along(identities),
    function(index) {
      closure <- identities[[index]]
      if (is.null(closure)) {
        return(TRUE)
      }
      location <- paste0(
        tools::file_path_sans_ext(basename(cerebro_data[[index]])),
        ".spatial"
      )
      plan <- list(
        schema_version = 1L,
        entries = stats::setNames(
          list(list(type = "directory", mode = "bundled", location = location)),
          relative_crbs[[index]]
        )
      )
      candidate <- stats::setNames(list(closure), relative_crbs[[index]])
      .builder_app_backend_identities_valid(
        candidate,
        plan,
        cerebro_data[[index]]
      )
    },
    logical(1)
  ))
}

.builder_app_fingerprint_valid <- function(fingerprint, expected_type) {
  if (
    !is.list(fingerprint) ||
      is.object(fingerprint) ||
      !identical(names(fingerprint), .builder_app_fingerprint_fields) ||
      !identical(fingerprint$type, expected_type) ||
      !.builder_app_permissions_valid(fingerprint$permissions)
  ) {
    return(FALSE)
  }
  nonnegative_integer <- function(value) {
    is.double(value) &&
      length(value) == 1L &&
      is.finite(value) &&
      value >= 0 &&
      value == floor(value)
  }
  finite_time <- function(value) {
    is.double(value) && length(value) == 1L && is.finite(value)
  }
  all(vapply(
    fingerprint[c("size", "device_id", "inode")],
    nonnegative_integer,
    logical(1)
  )) &&
    nonnegative_integer(fingerprint$hard_links) &&
    fingerprint$hard_links >= 1 &&
    all(vapply(
      fingerprint[c("modification_time", "change_time")],
      finite_time,
      logical(1)
    ))
}

.builder_app_backend_identities_valid <- function(
  identities,
  plan,
  cerebro_data
) {
  if (
    !is.list(identities) ||
      is.object(identities) ||
      !identical(names(identities), names(plan$entries))
  ) {
    return(FALSE)
  }
  all(vapply(
    seq_along(identities),
    function(index) {
      closure <- identities[[index]]
      entry <- plan$entries[[index]]
      if (
        !is.list(closure) ||
          is.object(closure) ||
          !identical(names(closure), .builder_app_backend_identity_fields) ||
          !identical(closure$type, entry$type) ||
          !is.list(closure$entries) ||
          is.object(closure$entries)
      ) {
        return(FALSE)
      }
      if (identical(entry$type, "embedded")) {
        return(
          is.null(closure$root) &&
            is.null(closure$root_fingerprint) &&
            !length(closure$entries)
        )
      }
      expected_root <- file.path(dirname(cerebro_data[[index]]), entry$location)
      mappings <- names(closure$entries)
      mappings_invalid <- length(closure$entries) &&
        (is.null(mappings) ||
          anyNA(mappings) ||
          any(!nzchar(mappings)) ||
          anyDuplicated(mappings) ||
          !identical(mappings, sort(mappings, method = "radix")))
      h5_invalid <- identical(entry$type, "h5") &&
        (!is.null(closure$root_fingerprint) ||
          !identical(mappings, entry$location))
      directory_invalid <- identical(entry$type, "directory") &&
        (!.builder_app_fingerprint_valid(
          closure$root_fingerprint,
          "directory"
        ) ||
          (length(mappings) &&
            !all(startsWith(mappings, paste0(entry$location, "/")))))
      if (
        !identical(closure$root, expected_root) ||
          mappings_invalid ||
          h5_invalid ||
          directory_invalid
      ) {
        return(FALSE)
      }
      all(vapply(
        seq_along(closure$entries),
        function(file_index) {
          mapping <- mappings[[file_index]]
          relative <- if (identical(entry$type, "h5")) {
            entry$location
          } else {
            substring(mapping, nchar(entry$location) + 2L)
          }
          tree_entry <- closure$entries[[file_index]]
          if (
            !.builder_app_safe_relative(relative) ||
              !is.list(tree_entry) ||
              is.object(tree_entry) ||
              !identical(tree_entry$path, mapping)
          ) {
            return(FALSE)
          }
          if (identical(tree_entry$type, "file")) {
            identical(
              names(tree_entry),
              .builder_app_backend_file_entry_fields
            ) &&
              .builder_app_identity_valid(
                tree_entry$identity,
                mapping,
                file.path(dirname(cerebro_data[[index]]), mapping)
              )
          } else if (identical(tree_entry$type, "directory")) {
            identical(entry$type, "directory") &&
              identical(
                names(tree_entry),
                .builder_app_backend_directory_entry_fields
              ) &&
              .builder_app_fingerprint_valid(
                tree_entry$fingerprint,
                "directory"
              )
          } else {
            FALSE
          }
        },
        logical(1)
      ))
    },
    logical(1)
  ))
}

.builder_app_validate_request <- function(request) {
  if (
    !identical(typeof(request), "list") ||
      !identical(
        attr(request, "class", exact = TRUE),
        c("builder_app_bundle_request", "list")
      ) ||
      .builder_app_has_reference(request)
  ) {
    .builder_app_request_error()
  }
  plain <- .builder_app_plain_value(request)
  canonical_stage <- tryCatch(
    normalizePath(plain$stage, winslash = "/", mustWork = TRUE),
    error = function(error) NULL
  )
  canonical_data <- tryCatch(
    vapply(
      plain$cerebro_data,
      normalizePath,
      character(1),
      winslash = "/",
      mustWork = TRUE
    ),
    error = function(error) NULL
  )
  if (
    !identical(names(plain), .builder_app_request_fields) ||
      !identical(plain$contract_version, 1L) ||
      !is.character(plain$stage) ||
      length(plain$stage) != 1L ||
      is.na(plain$stage) ||
      !nzchar(plain$stage) ||
      is.null(canonical_stage) ||
      !identical(canonical_stage, plain$stage) ||
      !dir.exists(canonical_stage) ||
      .builder_app_is_link(canonical_stage) ||
      !is.character(plain$cerebro_data) ||
      !length(plain$cerebro_data) ||
      anyNA(plain$cerebro_data) ||
      any(!nzchar(plain$cerebro_data)) ||
      anyDuplicated(plain$cerebro_data) ||
      is.null(canonical_data) ||
      !identical(unname(canonical_data), unname(plain$cerebro_data)) ||
      !all(dirname(canonical_data) == canonical_stage) ||
      anyDuplicated(tolower(basename(canonical_data))) ||
      !is.character(plain$selector_order) ||
      !length(plain$selector_order) ||
      anyNA(plain$selector_order) ||
      any(!nzchar(plain$selector_order)) ||
      anyDuplicated(plain$selector_order) ||
      !identical(names(plain$cerebro_data), plain$selector_order) ||
      !is.character(plain$dataset_labels) ||
      anyNA(plain$dataset_labels) ||
      any(!nzchar(trimws(plain$dataset_labels))) ||
      !identical(names(plain$dataset_labels), plain$selector_order) ||
      !is.list(plain$crb_identities) ||
      is.object(plain$crb_identities) ||
      !identical(names(plain$crb_identities), plain$selector_order) ||
      length(plain$crb_identities) != length(plain$cerebro_data) ||
      !is.character(plain$initial_dataset) ||
      length(plain$initial_dataset) != 1L ||
      is.na(plain$initial_dataset) ||
      !plain$initial_dataset %in% plain$selector_order ||
      !is.character(plain$initial_dataset_mode) ||
      length(plain$initial_dataset_mode) != 1L ||
      is.na(plain$initial_dataset_mode) ||
      !plain$initial_dataset_mode %in% c("automatic", "explicit") ||
      (identical(plain$initial_dataset_mode, "automatic") &&
        !identical(
          plain$initial_dataset,
          plain$selector_order[[1L]]
        )) ||
      !is.character(plain$initial_page) ||
      length(plain$initial_page) != 1L ||
      is.na(plain$initial_page) ||
      !plain$initial_page %in% builder_viewer_known_page_ids() ||
      !is.logical(plain$show_upload_ui) ||
      length(plain$show_upload_ui) != 1L ||
      is.na(plain$show_upload_ui) ||
      !is.character(plain$welcome_message) ||
      length(plain$welcome_message) != 1L ||
      is.na(plain$welcome_message) ||
      !nzchar(trimws(plain$welcome_message)) ||
      !is.list(plain$point_size) ||
      is.null(names(plain$point_size)) ||
      !length(names(plain$point_size)) ||
      !identical(
        names(plain$point_size)[[1L]],
        "overview_projection_point_size"
      ) ||
      any(
        !names(plain$point_size) %in%
          c(
            "overview_projection_point_size",
            "projection_point_opacity"
          )
      ) ||
      !is.numeric(plain$point_size$overview_projection_point_size) ||
      length(plain$point_size$overview_projection_point_size) != 1L ||
      is.na(plain$point_size$overview_projection_point_size) ||
      !is.finite(plain$point_size$overview_projection_point_size) ||
      plain$point_size$overview_projection_point_size < 1 ||
      plain$point_size$overview_projection_point_size > 20 ||
      (!is.null(plain$point_size$projection_point_opacity) &&
        (!is.numeric(plain$point_size$projection_point_opacity) ||
          length(plain$point_size$projection_point_opacity) != 1L ||
          is.na(plain$point_size$projection_point_opacity) ||
          !is.finite(plain$point_size$projection_point_opacity) ||
          plain$point_size$projection_point_opacity < 0.1 ||
          plain$point_size$projection_point_opacity > 1)) ||
      !.builder_app_viewer_content_valid(
        plain$viewer_content,
        plain$selector_order
      ) ||
      !is.logical(plain$variable_to_compare) ||
      length(plain$variable_to_compare) != 1L ||
      is.na(plain$variable_to_compare) ||
      !is.character(plain$host) ||
      length(plain$host) != 1L ||
      is.na(plain$host) ||
      !nzchar(plain$host) ||
      !is.numeric(plain$port) ||
      length(plain$port) != 1L ||
      is.na(plain$port) ||
      !is.finite(plain$port) ||
      plain$port != floor(plain$port) ||
      plain$port < 1 ||
      plain$port > 65535 ||
      !is.numeric(plain$max_request_size) ||
      length(plain$max_request_size) != 1L ||
      is.na(plain$max_request_size) ||
      !is.finite(plain$max_request_size) ||
      plain$max_request_size <= 0 ||
      !is.character(plain$display_mode) ||
      length(plain$display_mode) != 1L ||
      is.na(plain$display_mode) ||
      !plain$display_mode %in% c("auto", "normal", "showcase") ||
      !is.logical(plain$launch_browser) ||
      length(plain$launch_browser) != 1L ||
      is.na(plain$launch_browser) ||
      !.builder_app_auth_request_valid(plain$auth) ||
      !.builder_app_colors_valid(plain$colors, plain$selector_order) ||
      !identical(plain$crb_pick_smallest_file, FALSE) ||
      !.builder_app_backend_plan_valid(
        plain$backend_plan,
        plain$cerebro_data
      ) ||
      !.builder_app_backend_identities_valid(
        plain$backend_identities,
        plain$backend_plan,
        plain$cerebro_data
      ) ||
      !.builder_app_spatial_molecule_identities_valid(
        plain$spatial_molecule_identities,
        plain$cerebro_data,
        names(plain$backend_plan$entries)
      ) ||
      !.builder_app_spatial_manifest_valid(
        plain$spatial_images,
        plain$spatial_image_settings,
        plain$spatial_image_identities,
        plain$selector_order,
        plain$stage
      )
  ) {
    .builder_app_request_error()
  }
  identities_valid <- vapply(
    seq_along(plain$crb_identities),
    function(index) {
      .builder_app_identity_valid(
        plain$crb_identities[[index]],
        plain$selector_order[[index]],
        plain$cerebro_data[[index]]
      )
    },
    logical(1)
  )
  if (!all(identities_valid)) {
    .builder_app_request_error()
  }
  expected_content <- tryCatch(
    .builder_app_content_identities(
      plain$crb_identities,
      plain$backend_identities,
      plain$backend_plan,
      plain$spatial_molecule_identities
    ),
    error = function(error) NULL
  )
  if (
    is.null(expected_content) ||
      !identical(plain$content_identities, expected_content)
  ) {
    .builder_app_request_error()
  }
  plain
}

.builder_app_assert_crb_identities <- function(request) {
  current <- lapply(
    seq_along(request$cerebro_data),
    function(index) {
      .builder_app_capture_file_identity(
        request$cerebro_data[[index]],
        request$selector_order[[index]],
        .previous = request$crb_identities[[index]]
      )
    }
  )
  names(current) <- request$selector_order
  if (!identical(current, request$crb_identities)) {
    stop("A verified input CRB changed after request creation.", call. = FALSE)
  }
  invisible(TRUE)
}

.builder_app_capture_backend_identities <- function(request) {
  current <- lapply(seq_along(request$cerebro_data), function(index) {
    relative_crb <- names(request$backend_plan$entries)[[index]]
    .builder_app_capture_backend_identity(
      request$backend_plan$entries[[relative_crb]],
      request$cerebro_data[[index]],
      .previous = request$backend_identities[[relative_crb]]
    )
  })
  names(current) <- names(request$backend_plan$entries)
  current
}

.builder_app_assert_backend_identities <- function(request) {
  current <- tryCatch(
    .builder_app_capture_backend_identities(request),
    error = function(error) NULL
  )
  if (is.null(current) || !identical(current, request$backend_identities)) {
    stop(
      "A verified input backend closure changed after request creation.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_capture_spatial_molecule_identities <- function(request) {
  current <- lapply(seq_along(request$cerebro_data), function(index) {
    previous <- request$spatial_molecule_identities[[index]]
    ## The CRB identity is asserted immediately before this function.  Once
    ## those bytes are unchanged, reopening the same CRB cannot reveal a new
    ## spatial descriptor.  Revalidate only the sidecar closure recorded by
    ## the request instead of deserializing the CRB again.
    if (is.null(previous)) {
      return(NULL)
    }
    location <- paste0(
      tools::file_path_sans_ext(basename(request$cerebro_data[[index]])),
      ".spatial"
    )
    .builder_app_capture_spatial_molecule_identity(
      request$cerebro_data[[index]],
      .previous = previous,
      .verified_backend = list(
        type = "directory",
        location = location,
        root = previous$root
      ),
      .backend_is_verified = TRUE
    )
  })
  names(current) <- names(request$backend_plan$entries)
  current
}

.builder_app_assert_input_identities <- function(request) {
  .builder_app_assert_crb_identities(request)
  .builder_app_assert_backend_identities(request)
  current_molecules <- tryCatch(
    .builder_app_capture_spatial_molecule_identities(request),
    error = function(error) NULL
  )
  if (
    is.null(current_molecules) ||
      !identical(
        current_molecules,
        request$spatial_molecule_identities
      )
  ) {
    stop(
      "A verified input spatial molecule closure changed after request creation.",
      call. = FALSE
    )
  }
  current_spatial <- tryCatch(
    .builder_app_capture_spatial_identities(
      request$spatial_images,
      .previous = request$spatial_image_identities
    ),
    error = function(error) NULL
  )
  if (
    is.null(current_spatial) ||
      !identical(current_spatial, request$spatial_image_identities)
  ) {
    stop(
      "A verified input spatial image changed after request creation.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.builder_app_backend_entry <- function(item) {
  mode <- item$expression_backend
  if (
    !is.character(mode) ||
      length(mode) != 1L ||
      is.na(mode) ||
      !mode %in% c("embedded", "h5")
  ) {
    stop("BuildPlan contains an invalid expression backend.", call. = FALSE)
  }
  if (identical(mode, "embedded")) {
    return(list(type = "embedded", mode = "embedded", location = NULL))
  }
  sidecars <- item$sidecars
  if (
    !is.character(sidecars) ||
      length(sidecars) != 1L ||
      is.na(sidecars) ||
      !nzchar(sidecars)
  ) {
    stop("BuildPlan contains an invalid backend sidecar.", call. = FALSE)
  }
  list(type = mode, mode = "bundled", location = sidecars)
}
