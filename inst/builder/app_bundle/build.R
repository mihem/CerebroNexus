.builder_app_scatter_options <- function(request) {
  labels <- request$selector_order
  viewer_content <- request$viewer_content[labels]
  list(
    spatial_percentage_cells_to_show = vapply(
      viewer_content,
      `[[`,
      numeric(1),
      "spatial_percentage_cells_to_show"
    ),
    point_size = vapply(
      viewer_content,
      `[[`,
      numeric(1),
      "overview_point_size"
    ),
    point_opacity = vapply(
      viewer_content,
      `[[`,
      numeric(1),
      "overview_point_opacity"
    ),
    percentage_cells_to_show = vapply(
      viewer_content,
      `[[`,
      numeric(1),
      "overview_percentage_cells_to_show"
    )
  )
}

.builder_app_request_size_bytes <- function(request) {
  bytes <- as.double(request$max_request_size * 1024^2)
  if (isTRUE(request$show_upload_ui)) bytes else min(bytes, 6 * 1024^2)
}

.builder_app_spatial_settings <- function(request) {
  lapply(request$spatial_image_settings, function(sections) {
    lapply(sections, function(images) {
      lapply(images, function(settings) {
        settings[setdiff(
          .builder_app_spatial_setting_fields,
          c("point_opacity", "point_size")
        )]
      })
    })
  })
}

builder_build_app <- function(
  request,
  stage,
  create_app = createShinyApp,
  auth_material = NULL
) {
  passphrase <- NULL
  on.exit(passphrase <- NULL, add = TRUE)
  request <- .builder_app_validate_request(request)
  auth_enabled <- isTRUE(request$auth$enabled)
  stage <- normalizePath(stage, winslash = "/", mustWork = TRUE)
  if (!identical(stage, request$stage) || .builder_app_is_link(stage)) {
    stop("App assembly requires the request's assigned stage.", call. = FALSE)
  }
  app_dir <- file.path(stage, "cerebro_app")
  if (.builder_app_path_exists(app_dir)) {
    stop("The staged App directory already exists.", call. = FALSE)
  }
  .builder_app_assert_input_identities(request)
  scatter <- .builder_app_scatter_options(request)
  create_arguments <- list(
    cerebro_data = request$cerebro_data,
    dataset_labels = request$dataset_labels,
    result_dir = app_dir,
    colors = request$colors,
    spatial_images = request$spatial_images,
    spatial_image_settings = .builder_app_spatial_settings(request),
    cerebro_options = list(
      exclude_trivial_metadata = TRUE,
      viewer_content = request$viewer_content
    ),
    overwrite = FALSE,
    quiet = TRUE,
    verbose = FALSE,
    crb_pick_smallest_file = FALSE,
    show_upload_ui = request$show_upload_ui,
    initial_dataset = request$initial_dataset,
    initial_page = request$initial_page,
    welcome_message = request$welcome_message,
    spatial_percentage_cells_to_show = scatter$spatial_percentage_cells_to_show,
    point_size = scatter$point_size,
    point_opacity = scatter$point_opacity,
    percentage_cells_to_show = scatter$percentage_cells_to_show,
    variable_to_compare = request$variable_to_compare,
    host = request$host,
    port = request$port,
    max_request_size = request$max_request_size,
    display_mode = request$display_mode,
    launch_browser = request$launch_browser
  )
  if (auth_enabled) {
    auth_material <- builder_auth_validate_material(auth_material, stage)
    passphrase <- builder_auth_read_env_file(auth_material$env_file)
    create_arguments$auth <- auth_material$descriptor
    .builder_auth_with_passphrase(
      passphrase,
      function() do.call(create_app, create_arguments)
    )
  } else {
    if (!is.null(auth_material)) {
      stop(
        "Public App assembly cannot use authentication material.",
        call. = FALSE
      )
    }
    do.call(create_app, create_arguments)
  }
  .builder_app_assert_input_identities(request)
  if (
    !dir.exists(app_dir) ||
      .builder_app_is_link(app_dir) ||
      !.builder_app_path_within(app_dir, stage)
  ) {
    stop(
      "App assembly did not create the assigned private directory.",
      call. = FALSE
    )
  }
  if (auth_enabled) {
    auth_env_file <- file.path(app_dir, "viewer-auth.env")
    copied <- tryCatch(
      file.copy(
        auth_material$env_file,
        auth_env_file,
        overwrite = FALSE,
        copy.mode = TRUE
      ),
      error = function(error) FALSE,
      warning = function(warning) FALSE
    )
    protected <- tryCatch(
      Sys.chmod(auth_env_file, mode = "0600", use_umask = FALSE),
      error = function(error) FALSE,
      warning = function(warning) FALSE
    )
    valid <- isTRUE(copied) &&
      isTRUE(protected) &&
      file.exists(auth_env_file) &&
      !dir.exists(auth_env_file) &&
      !.builder_app_is_link(auth_env_file) &&
      (.Platform$OS.type == "windows" ||
        identical(as.integer(file.info(auth_env_file)$mode), 384L))
    if (!valid) {
      unlink(auth_env_file, force = TRUE)
      stop(
        "The App-local authentication environment could not be installed.",
        call. = FALSE
      )
    }
    auth_material <- NULL
  }
  app_dir
}

builder_verify_app <- function(
  app_dir,
  request,
  auth_env_file = NULL,
  .tree_identity = .builder_app_tree_identity,
  .retain_tree_identity = FALSE,
  .on_progress = NULL
) {
  # Progress only: `.on_progress(label)` names the check that is running.
  report <- if (is.function(.on_progress)) .on_progress else function(...) NULL
  report("Checking Viewer files")
  request <- .builder_app_validate_request(request)
  if (!dir.exists(app_dir) || .builder_app_is_link(app_dir)) {
    stop("The staged App directory is missing or symbolic.", call. = FALSE)
  }
  app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  expected_app_dir <- file.path(request$stage, "cerebro_app")
  if (!identical(app_dir, expected_app_dir)) {
    stop("The staged App is outside its assigned stage.", call. = FALSE)
  }
  tree_before <- .tree_identity(app_dir)
  legacy <- file.path(app_dir, "data")
  if (.builder_app_path_exists(legacy)) {
    stop(
      "The staged App contains the forbidden legacy data directory.",
      call. = FALSE
    )
  }
  app_file <- file.path(app_dir, "app.R")
  config_file <- file.path(app_dir, "cerebro_config.rds")
  if (
    !file.exists(app_file) ||
      dir.exists(app_file) ||
      .builder_app_is_link(app_file)
  ) {
    stop("The staged app.R is missing or symbolic.", call. = FALSE)
  }
  tryCatch(
    parse(file = app_file, keep.source = FALSE),
    error = function(error) {
      stop("The staged app.R cannot be parsed.", call. = FALSE)
    }
  )
  if (
    !file.exists(config_file) ||
      dir.exists(config_file) ||
      .builder_app_is_link(config_file)
  ) {
    stop("The staged App config is missing or symbolic.", call. = FALSE)
  }
  config_info <- tryCatch(
    fs::file_info(config_file, fail = TRUE, follow = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(config_info) ||
      nrow(config_info) != 1L ||
      !identical(as.character(config_info$type), "file") ||
      !is.finite(as.double(config_info$size)) ||
      as.double(config_info$size) > .builder_app_config_max_bytes
  ) {
    stop("The staged App config is too large to read safely.", call. = FALSE)
  }
  config <- tryCatch(readRDS(config_file), error = function(error) error)
  if (
    inherits(config, "condition") ||
      !is.list(config) ||
      .builder_app_has_reference(config)
  ) {
    stop("The staged App config is not an inert readable list.", call. = FALSE)
  }
  config <- .builder_app_plain_value(config)
  crbs <- config[["crb_file_to_load"]]
  expected_crbs <- stats::setNames(
    file.path("private-data", basename(request$cerebro_data)),
    request$selector_order
  )
  if (!identical(crbs, expected_crbs)) {
    stop(
      "The staged App selector labels or order differ from request.",
      call. = FALSE
    )
  }
  if (!identical(config[["dataset_labels"]], request$dataset_labels)) {
    stop("The staged App dataset labels differ from request.", call. = FALSE)
  }
  if (!identical(config[["initial_dataset"]], request$initial_dataset)) {
    stop("The staged App initial dataset differs from request.", call. = FALSE)
  }
  if (!identical(config[["initial_page"]], request$initial_page)) {
    stop("The staged App starting page differs from request.", call. = FALSE)
  }
  if (!identical(config[["show_upload_ui"]], request$show_upload_ui)) {
    stop("The staged App upload policy differs from request.", call. = FALSE)
  }
  if (!identical(config[["welcome_message"]], request$welcome_message)) {
    stop("The staged App welcome message differs from request.", call. = FALSE)
  }
  scatter <- .builder_app_scatter_options(request)
  if (!identical(config[["point_size"]], scatter$point_size)) {
    stop("The staged App point sizes differ from request.", call. = FALSE)
  }
  if (!identical(config[["point_opacity"]], scatter$point_opacity)) {
    stop("The staged App point opacity differs from request.", call. = FALSE)
  }
  if (
    !identical(
      config[["percentage_cells_to_show"]],
      scatter$percentage_cells_to_show
    )
  ) {
    stop("The staged App cell percentages differ from request.", call. = FALSE)
  }
  if (!identical(config[["viewer_content"]], request$viewer_content)) {
    difference <- all.equal(
      config[["viewer_content"]],
      request$viewer_content,
      tolerance = 0
    )
    stop(
      "The staged App Viewer defaults differ from request: ",
      paste(difference, collapse = "; "),
      call. = FALSE
    )
  }
  if (
    !identical(config[["variable_to_compare"]], request$variable_to_compare)
  ) {
    stop(
      "The staged App comparison option differs from request.",
      call. = FALSE
    )
  }
  expected_run_options <- list(
    schema_version = 1L,
    max_request_size_bytes = .builder_app_request_size_bytes(request),
    shiny_app_options = list(
      port = as.integer(request$port),
      host = request$host,
      launch.browser = request$launch_browser,
      quiet = TRUE,
      display.mode = request$display_mode
    )
  )
  if (!identical(config[[".bundle_run_options"]], expected_run_options)) {
    stop("The staged App launch options differ from request.", call. = FALSE)
  }
  if (!identical(config[["colors"]], request$colors)) {
    stop("The staged App palettes differ from request.", call. = FALSE)
  }
  expected_spatial_images <- if (length(request$spatial_images)) {
    values <- lapply(names(request$spatial_images), function(dataset) {
      sections <- lapply(
        names(request$spatial_images[[dataset]]),
        function(section) {
          values <- Map(
            function(descriptor, label) {
              descriptor$path <- .builder_app_spatial_target(
                dataset,
                section,
                label,
                descriptor$path
              )
              descriptor
            },
            request$spatial_images[[dataset]][[section]],
            names(request$spatial_images[[dataset]][[section]])
          )
          names(values) <- names(request$spatial_images[[dataset]][[section]])
          values
        }
      )
      names(sections) <- names(request$spatial_images[[dataset]])
      sections
    })
    names(values) <- names(request$spatial_images)
    values
  } else {
    list()
  }
  observed_spatial_images <- config[["spatial_images"]]
  spatial_images_match <- .builder_app_config_spatial_manifest_valid(
    observed_spatial_images,
    request$selector_order
  )
  for (dataset in names(expected_spatial_images)) {
    for (section in names(expected_spatial_images[[dataset]])) {
      for (label in names(expected_spatial_images[[dataset]][[section]])) {
        spatial_images_match <- spatial_images_match &&
          identical(
            observed_spatial_images[[dataset]][[section]][[label]],
            expected_spatial_images[[dataset]][[section]][[label]]
          )
      }
    }
  }
  if (!spatial_images_match) {
    stop(
      "The staged App spatial image manifest differs from request.",
      call. = FALSE
    )
  }
  observed_spatial_settings <- config[["spatial_image_settings"]]
  expected_spatial_settings <- .builder_app_spatial_settings(request)
  spatial_settings_match <- TRUE
  for (dataset in names(expected_spatial_settings)) {
    for (section in names(expected_spatial_settings[[dataset]])) {
      for (label in names(expected_spatial_settings[[dataset]][[
        section
      ]])) {
        spatial_settings_match <- spatial_settings_match &&
          identical(
            observed_spatial_settings[[dataset]][[section]][[label]],
            expected_spatial_settings[[dataset]][[section]][[label]]
          )
      }
    }
  }
  if (!spatial_settings_match) {
    stop(
      "The staged App spatial image settings differ from request.",
      call. = FALSE
    )
  }
  if (
    !identical(
      config[["crb_pick_smallest_file"]],
      request$crb_pick_smallest_file
    )
  ) {
    stop(
      "The staged App smallest-file policy differs from request.",
      call. = FALSE
    )
  }
  backend_plan <- config[[".bundle_backend_plan"]]
  if (!identical(backend_plan, request$backend_plan)) {
    stop("The staged App backend plan differs from request.", call. = FALSE)
  }

  auth_database <- NULL
  expected_auth_env <- file.path(app_dir, "viewer-auth.env")
  auth_config <- config[[".viewer_auth"]]
  if (isTRUE(request$auth$enabled)) {
    expected_auth_config <- list(
      credentials_path = "private-data/auth/credentials.sqlite",
      passphrase_env = .builder_auth_env_name,
      timeout_minutes = .builder_auth_timeout_minutes
    )
    auth_database <- file.path(
      app_dir,
      "private-data",
      "auth",
      "credentials.sqlite"
    )
    if (
      !identical(auth_config, expected_auth_config) ||
        !identical(auth_env_file, expected_auth_env) ||
        !file.exists(auth_database) ||
        dir.exists(auth_database) ||
        .builder_app_is_link(auth_database) ||
        !file.exists(auth_env_file) ||
        dir.exists(auth_env_file) ||
        .builder_app_is_link(auth_env_file) ||
        (.Platform$OS.type != "windows" &&
          !identical(as.integer(file.info(auth_env_file)$mode), 384L))
    ) {
      stop("The staged App authentication topology is invalid.", call. = FALSE)
    }
    builder_auth_read_env_file(auth_env_file)
    builder_auth_verify_database_pair(auth_database, auth_env_file)
    auth_database <- normalizePath(
      auth_database,
      winslash = "/",
      mustWork = TRUE
    )
  } else {
    auth_dir <- file.path(app_dir, "private-data", "auth")
    if (
      !is.null(auth_config) ||
        !is.null(auth_env_file) ||
        .builder_app_path_exists(auth_dir) ||
        .builder_app_path_exists(expected_auth_env)
    ) {
      stop(
        "The public staged App contains authentication material.",
        call. = FALSE
      )
    }
  }

  private_root <- file.path(app_dir, "private-data")
  if (!dir.exists(private_root) || .builder_app_is_link(private_root)) {
    stop(
      "The staged App private-data directory is missing or symbolic.",
      call. = FALSE
    )
  }
  private_root <- normalizePath(private_root, winslash = "/", mustWork = TRUE)
  report("Checking Viewer runtime")
  .builder_app_validate_private_locations(tree_before, app_dir)
  .builder_app_assert_trusted_templates(tree_before)
  .builder_app_assert_root_topology(
    tree_before,
    observed_spatial_images,
    auth_enabled = request$auth$enabled
  )
  .builder_app_assert_spatial_topology(
    tree_before,
    request,
    observed_spatial_images
  )
  configured_files <- character()
  for (index in seq_along(crbs)) {
    relative_crb <- unname(crbs[[index]])
    if (
      !.builder_app_safe_relative(relative_crb) ||
        !startsWith(relative_crb, "private-data/")
    ) {
      stop("A configured CRB path escapes private-data.", call. = FALSE)
    }
    crb <- file.path(app_dir, relative_crb)
    if (
      !file.exists(crb) ||
        dir.exists(crb) ||
        .builder_app_is_link(crb) ||
        !.builder_app_path_within(crb, private_root)
    ) {
      stop(
        "A configured CRB is missing, symbolic, or outside private-data.",
        call. = FALSE
      )
    }
    configured_files <- c(configured_files, normalizePath(crb, winslash = "/"))
    entry <- backend_plan$entries[[relative_crb]]
    if (identical(entry$mode, "bundled")) {
      if (!.builder_app_safe_relative(entry$location)) {
        stop(
          "A configured backend sidecar path escapes private-data.",
          call. = FALSE
        )
      }
      sidecar <- file.path(dirname(crb), entry$location)
      expected_type <- switch(
        entry$type,
        h5 = "file",
        directory = "directory",
        NULL
      )
      sidecar_info <- tryCatch(
        fs::file_info(sidecar, fail = TRUE, follow = FALSE),
        error = function(error) NULL
      )
      if (
        is.null(expected_type) ||
          !.builder_app_path_exists(sidecar) ||
          .builder_app_is_link(sidecar) ||
          is.null(sidecar_info) ||
          !identical(as.character(sidecar_info$type), expected_type) ||
          !.builder_app_path_within(sidecar, private_root)
      ) {
        stop(
          paste0(
            "A configured backend sidecar must be the expected ",
            if (identical(expected_type, "file")) {
              "regular file"
            } else {
              "directory"
            },
            " inside private-data."
          ),
          call. = FALSE
        )
      }
      configured_files <- c(
        configured_files,
        normalizePath(sidecar, winslash = "/", mustWork = TRUE)
      )
    }
  }

  report("Checking Viewer content")
  .builder_app_assert_private_topology(tree_before, request)
  copied_content <- .builder_app_output_content_identities(
    tree_before,
    request
  )
  if (!identical(copied_content, request$content_identities)) {
    stop(
      "The staged App copied content differs from frozen input.",
      call. = FALSE
    )
  }

  tree_after <- .tree_identity(app_dir, .previous = tree_before)
  if (!identical(tree_before, tree_after)) {
    stop(
      "The staged App tree changed during verification.",
      call. = FALSE
    )
  }

  verification <- structure(
    list(
      valid = TRUE,
      contract_version = 1L,
      app_dir = app_dir,
      selector_order = request$selector_order,
      dataset_labels = request$dataset_labels,
      initial_dataset = request$initial_dataset,
      show_upload_ui = request$show_upload_ui,
      colors = request$colors,
      backend_plan = request$backend_plan,
      private_files = configured_files,
      legacy_data_absent = TRUE,
      auth_enabled = isTRUE(request$auth$enabled),
      auth_database = auth_database,
      diagnostic_tree_identity = .builder_app_tree_summary(tree_after)
    ),
    class = c("builder_app_verification", "list")
  )
  if (isTRUE(.retain_tree_identity)) {
    attr(verification, "parent_tree_identity") <- tree_after
  }
  verification
}
