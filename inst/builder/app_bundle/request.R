.builder_app_plan_contract <- function(plan, context = "App assembly") {
  invalid <- function() {
    stop(
      context,
      paste0(
        " requires an inert, reference-free frozen contract-v1 ",
        "BuildPlan."
      ),
      call. = FALSE
    )
  }
  if (
    !identical(typeof(plan), "list") ||
      !identical(
        attr(plan, "class", exact = TRUE),
        c("builder_build_plan", "list")
      )
  ) {
    invalid()
  }
  raw_items <- .subset2(plan, "items")
  if (
    !is.list(raw_items) ||
      any(vapply(
        raw_items,
        function(item) {
          !is.list(item) || !is.null(attr(item, "class", exact = TRUE))
        },
        logical(1)
      ))
  ) {
    invalid()
  }
  app_items <- lapply(raw_items, function(item) {
    list(
      id = .subset2(item, "id"),
      name = .subset2(item, "name"),
      filename = .subset2(item, "filename"),
      colors = .subset2(item, "colors"),
      default_projection = .subset2(item, "default_projection"),
      initial_projections = .subset2(item, "initial_projections"),
      default_trajectory = .subset2(item, "default_trajectory"),
      viewer_display = .subset2(item, "viewer_display"),
      extra_material_table_index = if (!is.null(.subset2(item, "tables"))) {
        .builder_app_table_index(.subset2(item, "tables"))
      } else {
        viewerExtraMaterialTableIndex(.subset2(
          item,
          "extra_material_table_index"
        ))
      },
      overview_point_size = .subset2(item, "overview_point_size"),
      overview_point_opacity = .subset2(item, "overview_point_opacity"),
      overview_percentage_cells_to_show = .subset2(
        item,
        "overview_percentage_cells_to_show"
      ),
      spatial_percentage_cells_to_show = .subset2(
        item,
        "spatial_percentage_cells_to_show"
      ) %||%
        100,
      spatial_point_appearance = .subset2(
        item,
        "spatial_point_appearance"
      ) %||%
        list(),
      spatial_roi_settings = .subset2(item, "spatial_roi_settings") %||%
        list(),
      expression_backend = .subset2(item, "expression_backend"),
      sidecars = .subset2(item, "sidecars"),
      spatial_image_storage = "external",
      external_images = .subset2(item, "external_images") %||% list(),
      external_image_settings = .subset2(item, "external_image_settings") %||%
        list()
    )
  })
  plan <- list(
    app_contract_version = .subset2(plan, "app_contract_version"),
    make_app = .subset2(plan, "make_app"),
    dataset_order = .subset2(plan, "dataset_order"),
    items = app_items,
    app_options = .subset2(plan, "app_options"),
    app_auth = .subset2(plan, "app_auth")
  )
  if (.builder_app_has_reference(plan)) {
    invalid()
  }
  .builder_app_plain_value(plan)
}

.builder_app_capture_spatial_identities <- function(images, .previous = NULL) {
  identities <- lapply(names(images), function(dataset) {
    sections <- lapply(names(images[[dataset]]), function(section) {
      values <- lapply(names(images[[dataset]][[section]]), function(label) {
        previous <- tryCatch(
          .previous[[dataset]][[section]][[label]],
          error = function(error) NULL
        )
        .builder_app_capture_file_identity(
          images[[dataset]][[section]][[label]]$path,
          paste(dataset, section, label, sep = "/"),
          .previous = previous
        )
      })
      names(values) <- names(images[[dataset]][[section]])
      values
    })
    names(sections) <- names(images[[dataset]])
    sections
  })
  names(identities) <- names(images)
  identities
}

.builder_app_previous_closure <- function(type, location, identities) {
  if (identical(type, "embedded") || !is.list(identities)) {
    return(NULL)
  }
  paths <- names(identities)
  if (is.null(paths)) {
    return(NULL)
  }
  selected <- if (identical(type, "h5")) {
    paths == location
  } else {
    startsWith(paths, paste0(location, "/"))
  }
  identities <- identities[selected]
  if (!length(identities)) {
    return(NULL)
  }
  entries <- lapply(seq_along(identities), function(index) {
    list(
      path = names(identities)[[index]],
      type = "file",
      identity = identities[[index]]
    )
  })
  names(entries) <- names(identities)
  list(type = type, root = NULL, root_fingerprint = NULL, entries = entries)
}

builder_app_bundle_request <- function(
  plan,
  built,
  labels,
  verified_spatial_backends = NULL,
  previous_file_identities = NULL,
  .on_progress = NULL
) {
  # Progress only: `.on_progress(label, index, count)` names each dataset pass.
  report <- if (is.function(.on_progress)) .on_progress else function(...) NULL
  plan <- .builder_app_plan_contract(plan)
  if (
    !identical(plan[["app_contract_version"]], 1L) ||
      !isTRUE(plan[["make_app"]])
  ) {
    stop(
      "App assembly requires a contract-v1 BuildPlan with App output enabled.",
      call. = FALSE
    )
  }
  order <- plan$dataset_order
  items <- plan$items
  if (
    !is.character(order) ||
      !length(order) ||
      anyNA(order) ||
      any(!nzchar(order)) ||
      anyDuplicated(order) ||
      !is.list(items) ||
      length(items) != length(order)
  ) {
    stop("BuildPlan dataset order is invalid.", call. = FALSE)
  }
  if (
    .builder_app_has_reference(built) ||
      .builder_app_has_reference(labels)
  ) {
    stop("Verified CRB labels must be inert values.", call. = FALSE)
  }
  report("Checking datasets", 0L, length(order))
  built <- .builder_app_plain_value(built)
  labels <- .builder_app_plain_value(labels)
  item_ids <- vapply(items, `[[`, character(1), "id")
  item_labels <- vapply(items, `[[`, character(1), "name")
  dataset_labels <- stats::setNames(item_labels, item_ids)
  filenames <- vapply(items, `[[`, character(1), "filename")
  if (
    !identical(item_ids, order) ||
      anyNA(item_labels) ||
      any(!nzchar(trimws(item_labels))) ||
      !is.character(labels) ||
      !identical(unname(labels), item_labels) ||
      !is.character(built) ||
      length(built) != length(items) ||
      !identical(names(built), item_ids) ||
      anyDuplicated(tolower(built)) ||
      anyDuplicated(tolower(filenames))
  ) {
    stop("Verified CRB labels do not match BuildPlan.", call. = FALSE)
  }
  valid_files <- vapply(
    built,
    function(path) {
      length(path) == 1L &&
        !is.na(path) &&
        nzchar(path) &&
        file.exists(path) &&
        !dir.exists(path) &&
        !.builder_app_is_link(path)
    },
    logical(1)
  )
  if (!all(valid_files)) {
    stop("A verified staged CRB is missing or invalid.", call. = FALSE)
  }
  if (is.null(previous_file_identities)) {
    previous_file_identities <- stats::setNames(
      vector("list", length(item_labels)),
      item_ids
    )
  } else if (
    .builder_app_has_reference(previous_file_identities) ||
      !is.list(previous_file_identities) ||
      is.object(previous_file_identities) ||
      !identical(names(previous_file_identities), item_ids) ||
      any(!vapply(previous_file_identities, is.list, logical(1)))
  ) {
    stop("Prior staged file identities are invalid.", call. = FALSE)
  } else {
    previous_file_identities <- .builder_app_plain_value(
      previous_file_identities
    )
  }
  resolved <- vapply(
    built,
    normalizePath,
    character(1),
    winslash = "/",
    mustWork = TRUE
  )
  stage <- dirname(resolved[[1L]])
  if (
    !all(dirname(resolved) == stage) ||
      !identical(unname(basename(resolved)), filenames)
  ) {
    stop(
      "Verified CRBs must be exact files inside one assigned stage.",
      call. = FALSE
    )
  }
  options <- plan$app_options
  app_auth <- plan$app_auth
  if (
    is.list(options) &&
      identical(options$initial_dataset_mode, "automatic") &&
      !identical(options$initial_dataset, order[[1L]])
  ) {
    stop(
      "An automatic initial dataset must be the first ordered dataset.",
      call. = FALSE
    )
  }
  if (
    .builder_app_has_reference(options) ||
      !.builder_app_options_valid(options, order)
  ) {
    stop("Frozen generated-App options are invalid.", call. = FALSE)
  }
  if (!.builder_app_auth_summary_valid(app_auth)) {
    stop("Frozen generated-App login settings are invalid.", call. = FALSE)
  }
  if (
    identical(options$initial_dataset_mode, "automatic") &&
      !identical(options$initial_dataset, order[[1L]])
  ) {
    stop(
      "An automatic initial dataset must be the first ordered dataset.",
      call. = FALSE
    )
  }
  initial_index <- match(options$initial_dataset, order)
  colors <- lapply(items, `[[`, "colors")
  names(colors) <- item_ids
  viewer_content <- .builder_app_viewer_content(
    items,
    item_ids,
    options$point_size
  )
  backend_entries <- lapply(items, .builder_app_backend_entry)
  names(backend_entries) <- paste0("private-data/", filenames)
  crb_identities <- lapply(
    seq_along(resolved),
    function(index) {
      report("Checking datasets", index, length(resolved))
      .builder_app_capture_file_identity(
        resolved[[index]],
        item_ids[[index]],
        .previous = previous_file_identities[[index]][[filenames[[index]]]]
      )
    }
  )
  names(crb_identities) <- item_ids
  backend_plan <- list(schema_version = 1L, entries = backend_entries)
  backend_identities <- lapply(seq_along(resolved), function(index) {
    relative_crb <- names(backend_entries)[[index]]
    .builder_app_capture_backend_identity(
      backend_entries[[relative_crb]],
      resolved[[index]],
      .previous = .builder_app_previous_closure(
        backend_entries[[relative_crb]]$type,
        backend_entries[[relative_crb]]$location,
        previous_file_identities[[index]]
      )
    )
  })
  names(backend_identities) <- names(backend_entries)
  verified_spatial_backends <- if (is.null(verified_spatial_backends)) {
    NULL
  } else {
    if (
      .builder_app_has_reference(verified_spatial_backends) ||
        !is.list(verified_spatial_backends) ||
        is.object(verified_spatial_backends) ||
        !identical(names(verified_spatial_backends), item_ids)
    ) {
      stop("Verified spatial molecule backends are invalid.", call. = FALSE)
    }
    .builder_app_plain_value(verified_spatial_backends)
  }
  spatial_molecule_identities <- lapply(seq_along(resolved), function(index) {
    report("Checking spatial data", index, length(resolved))
    previous <- .builder_app_previous_closure(
      "directory",
      paste0(tools::file_path_sans_ext(filenames[[index]]), ".spatial"),
      previous_file_identities[[index]]
    )
    if (is.null(verified_spatial_backends)) {
      return(.builder_app_capture_spatial_molecule_identity(
        resolved[[index]],
        .previous = previous
      ))
    }
    .builder_app_capture_spatial_molecule_identity(
      resolved[[index]],
      .previous = previous,
      .verified_backend = verified_spatial_backends[[index]],
      .backend_is_verified = TRUE
    )
  })
  names(spatial_molecule_identities) <- names(backend_entries)
  content_identities <- .builder_app_content_identities(
    crb_identities,
    backend_identities,
    backend_plan,
    spatial_molecule_identities
  )
  storage_valid <- vapply(
    items,
    function(item) {
      storage <- item$spatial_image_storage
      images <- item$external_images
      settings <- item$external_image_settings
      is.character(storage) &&
        length(storage) == 1L &&
        !is.na(storage) &&
        identical(storage, "external") &&
        is.list(images) &&
        !is.object(images) &&
        is.list(settings) &&
        !is.object(settings) &&
        identical(names(images), names(settings)) &&
        (identical(storage, "external") ||
          (!length(images) && !length(settings)))
    },
    logical(1)
  )
  if (!all(storage_valid)) {
    stop("BuildPlan external spatial image contract is invalid.", call. = FALSE)
  }
  external <- vapply(
    items,
    function(item) identical(item$spatial_image_storage, "external"),
    logical(1)
  )
  spatial_images <- lapply(items[external], `[[`, "external_images")
  spatial_image_settings <- lapply(
    items[external],
    `[[`,
    "external_image_settings"
  )
  names(spatial_images) <- item_ids[external]
  names(spatial_image_settings) <- item_ids[external]
  populated <- vapply(spatial_images, length, integer(1)) > 0L
  spatial_images <- spatial_images[populated]
  spatial_image_settings <- spatial_image_settings[populated]
  spatial_image_identities <- .builder_app_capture_spatial_identities(
    spatial_images
  )

  request <- structure(
    list(
      contract_version = 1L,
      stage = stage,
      cerebro_data = stats::setNames(unname(resolved), item_ids),
      crb_identities = crb_identities,
      selector_order = item_ids,
      dataset_labels = dataset_labels,
      initial_dataset = item_ids[[initial_index]],
      initial_dataset_mode = options$initial_dataset_mode,
      initial_page = options$initial_page,
      show_upload_ui = options$show_upload_ui,
      welcome_message = options$welcome_message,
      point_size = options$point_size,
      viewer_content = viewer_content,
      variable_to_compare = options$variable_to_compare,
      host = options$host,
      port = as.integer(options$port),
      max_request_size = options$max_request_size,
      display_mode = options$display_mode,
      launch_browser = options$launch_browser,
      auth = .builder_app_auth_request(app_auth),
      colors = colors,
      crb_pick_smallest_file = FALSE,
      backend_plan = backend_plan,
      backend_identities = backend_identities,
      spatial_molecule_identities = spatial_molecule_identities,
      content_identities = content_identities,
      spatial_images = spatial_images,
      spatial_image_settings = spatial_image_settings,
      spatial_image_identities = spatial_image_identities
    ),
    class = c("builder_app_bundle_request", "list")
  )
  .builder_app_validate_request(request)
  request
}
