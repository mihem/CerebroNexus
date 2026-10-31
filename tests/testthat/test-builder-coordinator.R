builder_task9_source <- function(local = parent.frame()) {
  root <- builder_test_inst_path("builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  source(file.path(root, "core", "bundle_path_contract.R"), local = local)
  source(file.path(root, "prerequisite.R"), local = local)
  source(file.path(root, "publish.R"), local = local)
  source(file.path(root, "app_bundle.R"), local = local)
  source(file.path(root, "report.R"), local = local)
  source(file.path(root, "coordinator.R"), local = local)
  invisible(root)
}

builder_app_coordinator_plan_fixture <- function(
  target,
  make_app = TRUE,
  backend = "embedded"
) {
  filenames <- c("dataset-a.crb", "dataset-b.crb")
  labels <- c("Dataset A", "Dataset B")
  items <- list(
    list(
      id = "dataset-a",
      name = labels[[1L]],
      filename = filenames[[1L]],
      colors = list(cluster = c(A = "#000000")),
      default_projection = "umap",
      default_trajectory = NULL,
      overview_point_size = 5,
      expression_backend = "embedded",
      sidecars = character()
    ),
    list(
      id = "dataset-b",
      name = labels[[2L]],
      filename = filenames[[2L]],
      colors = list(cluster = c(B = "#ffffff")),
      default_projection = "pca",
      default_trajectory = list(method = "slingshot", name = "lineage"),
      overview_point_size = 7,
      expression_backend = "embedded",
      sidecars = character()
    )
  )
  if (!identical(backend, "embedded")) {
    items[[1L]]$expression_backend <- backend
    items[[1L]]$sidecars <- paste0(
      tools::file_path_sans_ext(filenames[[1L]]),
      ".h5"
    )
  }
  targets <- if (isTRUE(make_app)) {
    file.path(target, "cerebro_app")
  } else {
    values <- c(filenames, items[[1L]]$sidecars %||% character())
    file.path(target, values)
  }
  structure(
    list(
      revision = 1L,
      readiness = "ready",
      out_dir = target,
      overwrite = FALSE,
      targets = targets,
      output_release = list(targets = targets),
      make_app = isTRUE(make_app),
      app_contract_version = if (isTRUE(make_app)) 1L else 0L,
      dataset_order = c("dataset-a", "dataset-b"),
      items = items,
      manifest = list(),
      acknowledgements = list(),
      viewer_bundle_assets = character(),
      private_assets = filenames,
      app_options = list(
        enabled = isTRUE(make_app),
        show_upload_ui = FALSE,
        initial_dataset = "dataset-b",
        initial_dataset_mode = "explicit",
        initial_page = "data_info",
        welcome_message = "Welcome, team!",
        point_size = list(overview_projection_point_size = 6),
        variable_to_compare = FALSE,
        host = "127.0.0.1",
        port = 4242L,
        max_request_size = 512,
        display_mode = "showcase",
        launch_browser = FALSE
      ),
      app_auth = list(
        enabled = isTRUE(make_app),
        account_count = if (isTRUE(make_app)) 2L else 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

builder_crb_coordinator_plan <- function(
  target,
  filenames,
  overwrite = FALSE,
  expected_prior_identity = NULL
) {
  items <- lapply(seq_along(filenames), function(index) {
    list(
      id = paste0("dataset-", index),
      name = paste0("Dataset ", index),
      filename = filenames[[index]],
      organism = "hg",
      analyses = character(),
      included_groups = character(),
      included_projections = character(),
      metadata_policy = list(included = character()),
      expression_backend = "embedded",
      sidecars = character(),
      viewer_page_expectations = list(visible_conditional = character())
    )
  })
  targets <- file.path(target, filenames)
  structure(
    list(
      revision = 1L,
      readiness = "ready",
      out_dir = target,
      overwrite = overwrite,
      targets = targets,
      output_release = list(targets = targets),
      expected_prior_identity = expected_prior_identity,
      make_app = FALSE,
      dataset_order = vapply(items, `[[`, character(1), "id"),
      items = items,
      manifest = list(),
      acknowledgements = list(),
      viewer_bundle_assets = character(),
      private_assets = filenames,
      app_options = list(enabled = FALSE),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
}

test_that("temporary App input removal retries transient Windows locks", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    stage <- file.path(root, "stage")
    dir.create(stage)
    path <- file.path(stage, "dataset.crb")
    writeLines("temporary", path)
    attempts <- 0L
    delays <- numeric()

    expect_silent(.builder_coordinator_remove_app_input(
      path,
      "dataset.crb",
      stage,
      .unlink = function(path, recursive, force) {
        attempts <<- attempts + 1L
        if (attempts < 3L) {
          warning("Permission denied: sharing violation")
          return(1L)
        }
        base::unlink(path, recursive = recursive, force = force)
      },
      .sleep = function(delay) delays <<- c(delays, delay),
      .retry_delays = c(0, 0.05, 0.1)
    ))

    expect_identical(attempts, 3L)
    expect_equal(delays, c(0.05, 0.1))
    expect_false(file.exists(path))
  })
})

test_that("temporary App input removal reports the failed path and OS error", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    stage <- file.path(root, "stage")
    dir.create(stage)
    path <- file.path(stage, "dataset.crb")
    writeLines("temporary", path)
    attempts <- 0L

    error <- expect_error(
      .builder_coordinator_remove_app_input(
        path,
        "dataset.crb",
        stage,
        .unlink = function(...) {
          attempts <<- attempts + 1L
          warning("Permission denied: sharing violation")
          1L
        },
        .sleep = function(delay) invisible(delay),
        .retry_delays = c(0, 0, 0)
      ),
      "could not be removed after 3 attempts",
      fixed = TRUE
    )

    message <- conditionMessage(error)
    expect_identical(attempts, 3L)
    expect_match(
      message,
      normalizePath(path, winslash = "/", mustWork = TRUE),
      fixed = TRUE
    )
    expect_match(message, 'Relative path: "dataset.crb"', fixed = TRUE)
    expect_match(message, "(Windows delete|Delete) status: 1")
    expect_match(message, "Permission denied: sharing violation", fixed = TRUE)
  })
})

test_that("temporary App input removal rejects a path swapped during retry", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    stage <- file.path(root, "stage")
    dir.create(stage)
    path <- file.path(stage, "dataset.crb")
    writeLines("temporary", path)
    original_link <- .builder_release_link
    attempted <- FALSE
    .builder_release_link <- function(candidate) {
      if (identical(candidate, path) && attempted) {
        TRUE
      } else {
        original_link(candidate)
      }
    }

    expect_error(
      .builder_coordinator_remove_app_input(
        path,
        "dataset.crb",
        stage,
        .unlink = function(...) {
          attempted <<- TRUE
          1L
        },
        .sleep = function(delay) invisible(delay),
        .retry_delays = c(0, 0)
      ),
      "became unsafe during removal",
      fixed = TRUE
    )
  })
})

builder_crb_coordinator_result <- function(handle, artifacts, labels = NULL) {
  if (is.null(labels)) {
    labels <- paste0("Dataset ", seq_along(artifacts))
  }
  list(
    state = "success",
    publishable = TRUE,
    stage = handle$stage,
    built = artifacts,
    labels = labels,
    verifications = lapply(artifacts, function(path) {
      list(valid = TRUE, path = path, metadata = character())
    }),
    app_dir = NULL,
    app_verification = NULL,
    app_tree_identity = NULL,
    auth_enabled = FALSE,
    auth_env_file = NULL
  )
}

test_that("release settlement never replaces the first error with cleanup", {
  local({
    builder_task9_source()
    handle <- list(target = "/release")
    recovery <- list(
      state = "recovery_required",
      message = "Restore the preserved release."
    )

    build_failure <- builder_coordinator_settle(
      handle,
      list(state = "failure", error = "original build failure"),
      .abort = function(handle) stop("stage cleanup failure", call. = FALSE),
      .recovery = function(target) recovery
    )
    expect_false(build_failure$ok)
    expect_identical(build_failure$error, "original build failure")
    expect_identical(build_failure$cleanup_error, "stage cleanup failure")
    expect_identical(build_failure$recovery, recovery)

    publication_failure <- builder_coordinator_settle(
      handle,
      list(state = "success", publishable = TRUE),
      .publish = function(handle, result) {
        stop("original publication failure", call. = FALSE)
      },
      .abort = function(handle) stop("stage cleanup failure", call. = FALSE),
      .recovery = function(target) recovery
    )
    expect_false(publication_failure$ok)
    expect_identical(
      publication_failure$error,
      "original publication failure"
    )
    expect_identical(publication_failure$cleanup_error, "stage cleanup failure")

    lost_coordinator <- builder_coordinator_settle(
      list(),
      list(state = "failure", error = "original build failure"),
      .recovery = function(target) recovery
    )
    expect_false(lost_coordinator$ok)
    expect_identical(lost_coordinator$error, "original build failure")
    expect_identical(
      lost_coordinator$settlement_error,
      "The parent release coordinator identity was lost."
    )
  })
})

test_that("CRB publication owns descriptor-backed spatial molecule sidecars", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")
    handle <- builder_coordinator_prepare(plan, "spatial-molecule-release")
    sidecar <- file.path(handle$stage, "dataset.spatial")
    dir.create(sidecar)
    molecule_file <- file.path(sidecar, "0001.rds")
    molecules <- data.frame(x = 1, y = 2, gene = "A")
    saveRDS(molecules, molecule_file)
    crb <- file.path(handle$stage, "dataset.crb")
    saveRDS(list(dataset = "spatial"), crb)

    result <- builder_crb_coordinator_result(
      handle,
      c(`dataset-1` = crb)
    )
    result$verifications[[1L]]$spatial_molecule_backend <- list(
      type = "directory",
      location = basename(sidecar)
    )
    result$verifications[[1L]]$spatial_molecule_path <- sidecar
    .builder_app_capture_spatial_molecule_identity <- function(...) {
      stop("The coordinator repeated the authoritative sidecar scan.")
    }
    phases <- character()
    published <- builder_coordinator_publish(
      handle,
      result,
      .on_progress = function(phase) phases <<- c(phases, phase)
    )

    expect_true(file.exists(file.path(target, "dataset.crb")))
    expect_true(dir.exists(file.path(target, "dataset.spatial")))
    expect_true(file.exists(file.path(target, "dataset.spatial", "0001.rds")))
    expect_identical(phases, c("verify", "publish"))
    expect_identical(
      normalizePath(
        published$verifications[[1L]]$spatial_molecule_path,
        winslash = "/"
      ),
      normalizePath(file.path(target, "dataset.spatial"), winslash = "/")
    )
  })
})

test_that("a needs-decision result keeps finished CRBs for the retry", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    retention <- file.path(root, "retained")
    dir.create(retention)
    plan <- builder_crb_coordinator_plan(target, c("first.crb", "second.crb"))
    plan$items[[1L]]$expression_backend <- "h5"
    plan$items[[1L]]$sidecars <- "first.h5"
    handle <- builder_coordinator_prepare(plan, "needs-decision-retry")
    crb <- file.path(handle$stage, "first.crb")
    saveRDS(list(dataset = "first"), crb)
    writeBin(charToRaw("h5 bytes"), file.path(handle$stage, "first.h5"))
    spatial <- file.path(handle$stage, "first.spatial")
    dir.create(spatial)
    writeBin(charToRaw("molecules"), file.path(spatial, "0001.qs2"))
    crb_md5 <- unname(tools::md5sum(crb))
    value <- builder_crb_coordinator_result(handle, c(`dataset-1` = crb))
    value$state <- "needs_decision"
    value$publishable <- FALSE
    value$verifications[[1L]]$spatial_molecule_backend <- list(
      type = "directory",
      location = "first.spatial"
    )
    value$verifications[[1L]]$spatial_molecule_path <- spatial

    failed <- value
    failed$state <- "failure"
    expect_identical(
      builder_coordinator_retain_partial(handle, failed, retention),
      list()
    )
    retained <- builder_coordinator_retain_partial(handle, value, retention)
    settled <- builder_coordinator_settle(handle, value)

    expect_true(settled$ok)
    expect_false(dir.exists(handle$stage))
    expect_identical(names(retained), "dataset-1")
    record <- retained[["dataset-1"]]
    expect_true(file.exists(record$path))
    expect_true(startsWith(
      record$path,
      normalizePath(retention, winslash = "/")
    ))
    expect_identical(record$fingerprint$md5, crb_md5)
    expect_identical(
      sort(vapply(record$members, `[[`, character(1), "target")),
      c("first.h5", "first.spatial/0001.qs2")
    )
    for (member in record$members) {
      expect_identical(
        member$fingerprint$md5,
        unname(tools::md5sum(member$resolved_path))
      )
    }
    expect_identical(record$verification$path, record$path)
    expect_identical(
      record$verification$spatial_molecule_path,
      file.path(dirname(record$path), "first.spatial")
    )
  })
})

test_that("publication reuses verified spatial sidecar metadata", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$items[[1L]]$sidecars <- "dataset-a.spatial"
    handle <- list(
      app_plan = plan,
      stage = file.path(root, "stage"),
      transient_app_inputs = character(),
      expected_payload_targets = character()
    )
    built <- c(
      `dataset-a` = file.path(handle$stage, "dataset-a.crb"),
      `dataset-b` = file.path(handle$stage, "dataset-b.crb")
    )
    verifications <- list(
      `dataset-a` = list(
        spatial_molecule_backend = list(
          type = "directory",
          location = "dataset-a.spatial"
        )
      ),
      `dataset-b` = list(spatial_molecule_backend = NULL)
    )
    .builder_app_capture_spatial_molecule_identity <- function(...) {
      stop("The coordinator repeated the authoritative sidecar scan.")
    }

    updated <- .builder_coordinator_register_spatial_molecule_inputs(
      handle,
      built,
      app_expected = TRUE,
      verifications = verifications
    )

    expect_identical(
      updated$transient_app_inputs,
      "dataset-a.spatial"
    )
    crb_handle <- handle
    crb_handle$app_plan$make_app <- FALSE
    crb_handle$transient_app_inputs <- character()
    crb_updated <- .builder_coordinator_register_spatial_molecule_inputs(
      crb_handle,
      built,
      app_expected = FALSE,
      verifications = verifications
    )
    expect_identical(
      crb_updated$expected_payload_targets,
      "dataset-a.spatial"
    )
    expect_error(
      .builder_coordinator_register_spatial_molecule_inputs(
        handle,
        built,
        app_expected = TRUE,
        verifications = list(
          `dataset-a` = list(
            spatial_molecule_backend = list(
              type = "directory",
              location = "../dataset-a.spatial"
            )
          )
        )
      ),
      "sidecar closure is invalid",
      fixed = TRUE
    )
    expect_error(
      .builder_coordinator_register_spatial_molecule_inputs(
        handle,
        built,
        app_expected = TRUE,
        verifications = list(
          `dataset-a` = verifications[["dataset-a"]],
          `dataset-b` = verifications[["dataset-a"]]
        )
      ),
      "sidecar closure is invalid",
      fixed = TRUE
    )
  })
})

test_that("coordinator contract inspection never dispatches plan methods", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    calls <- 0L
    `$.builder_build_plan` <- function(value, ...) {
      calls <<- calls + 1L
      stop("executed hostile method")
    }

    contract <- .builder_coordinator_app_contract(plan)

    expect_true(contract$expectation$expected)
    expect_identical(calls, 0L)
  })
})

test_that("coordinator prepare never dispatches hostile plan methods", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    calls <- character()
    `$.builder_build_plan` <- function(value, name) {
      calls <<- c(calls, name)
      if (identical(name, "out_dir")) {
        return(target)
      }
      stop("hostile-plan-method-sentinel-5e82")
    }
    plan <- structure(
      list(
        out_dir = target,
        make_app = FALSE,
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L,
          accounts = "forged-auth-sentinel-5e82"
        )
      ),
      class = c("builder_build_plan", "list")
    )

    error <- tryCatch(
      builder_coordinator_prepare(plan, "hostile-plan"),
      error = identity
    )

    expect_s3_class(error, "error")
    expect_identical(
      conditionMessage(error),
      "The App publication expectation is invalid."
    )
    expect_identical(calls, character())
    expect_false(builder_auth_value_contains(
      error,
      "hostile-plan-method-sentinel-5e82"
    ))
    expect_false(builder_auth_value_contains(
      error,
      "forged-auth-sentinel-5e82"
    ))
  })
})

builder_app_coordinator_fake_app <- function(
  request,
  app_dir,
  auth_material = NULL
) {
  dir.create(app_dir)
  file.copy(
    builder_profile_inst_path("viewer"),
    app_dir,
    recursive = TRUE
  )
  extdata_source <- builder_profile_inst_path("extdata")
  extdata_target <- file.path(app_dir, "extdata")
  dir.create(extdata_target)
  extdata_files <- list.files(extdata_source, full.names = TRUE)
  extdata_files <- extdata_files[file.info(extdata_files)$isdir %in% FALSE]
  file.copy(extdata_files, extdata_target)
  .removeBundleSystemMetadata(app_dir)
  dir.create(file.path(app_dir, "private-data"))
  relative_crbs <- file.path(
    "private-data",
    basename(request$cerebro_data)
  )
  for (index in seq_along(relative_crbs)) {
    file.copy(
      request$cerebro_data[[index]],
      file.path(app_dir, relative_crbs[[index]])
    )
  }
  for (index in seq_along(request$backend_plan$entries)) {
    entry <- request$backend_plan$entries[[index]]
    if (!identical(entry$mode, "bundled")) {
      next
    }
    source <- file.path(request$stage, entry$location)
    target <- file.path(app_dir, "private-data", entry$location)
    if (dir.exists(source)) {
      dir.create(target)
      children <- list.files(
        source,
        all.files = TRUE,
        no.. = TRUE,
        full.names = TRUE
      )
      if (length(children)) {
        file.copy(children, target, recursive = TRUE)
      }
    } else {
      file.copy(source, target)
    }
  }
  file.copy(
    builder_profile_inst_path("viewer", "_bundle_app.R"),
    file.path(app_dir, "app.R")
  )
  labels <- request$selector_order
  viewer_content <- request$viewer_content[labels]
  point_opacity <- request$point_size[["projection_point_opacity"]] %||% 1
  scatter <- list(
    point_size = vapply(
      viewer_content,
      `[[`,
      numeric(1),
      "overview_point_size"
    ),
    point_opacity = stats::setNames(
      rep(as.double(point_opacity), length(labels)),
      labels
    ),
    percentage_cells_to_show = vapply(
      viewer_content,
      `[[`,
      numeric(1),
      "overview_percentage_cells_to_show"
    )
  )
  request_bytes <- as.double(request$max_request_size * 1024^2)
  if (!isTRUE(request$show_upload_ui)) {
    request_bytes <- min(request_bytes, 6 * 1024^2)
  }
  config <- c(
    list(
      crb_file_to_load = stats::setNames(
        relative_crbs,
        request$selector_order
      ),
      dataset_labels = request$dataset_labels,
      initial_dataset = request$initial_dataset,
      initial_page = request$initial_page,
      show_upload_ui = request$show_upload_ui,
      welcome_message = request$welcome_message,
      viewer_content = request$viewer_content,
      variable_to_compare = request$variable_to_compare,
      .bundle_run_options = list(
        schema_version = 1L,
        max_request_size_bytes = request_bytes,
        shiny_app_options = list(
          port = as.integer(request$port),
          host = request$host,
          launch.browser = request$launch_browser,
          quiet = TRUE,
          display.mode = request$display_mode
        )
      ),
      colors = request$colors,
      crb_pick_smallest_file = request$crb_pick_smallest_file,
      .bundle_backend_plan = request$backend_plan
    ),
    scatter
  )
  if (isTRUE(request$auth$enabled)) {
    auth_dir <- file.path(app_dir, "private-data", "auth")
    dir.create(auth_dir)
    file.copy(
      auth_material$credentials,
      file.path(auth_dir, "credentials.sqlite")
    )
    file.copy(
      auth_material$env_file,
      file.path(app_dir, "viewer-auth.env"),
      copy.mode = TRUE
    )
    config$.viewer_auth <- list(
      credentials_path = "private-data/auth/credentials.sqlite",
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE",
      timeout_minutes = 15L
    )
  }
  saveRDS(config, file.path(app_dir, "cerebro_config.rds"))
  app_dir
}

builder_app_coordinator_fixture <- function(
  make_app = TRUE,
  backend = "embedded",
  valid_crb = FALSE,
  root = NULL,
  target = NULL,
  plan = NULL,
  .local_envir = parent.frame(),
  coordinator_prepare,
  bundle_request,
  verify_app,
  build_app = NULL
) {
  if (is.null(root)) {
    root <- withr::local_tempdir(.local_envir = .local_envir)
  }
  if (is.null(target)) {
    target <- file.path(root, "release")
  }
  if (is.null(plan)) {
    plan <- builder_app_coordinator_plan_fixture(target, make_app, backend)
  }
  handle <- coordinator_prepare(plan, "build-app")
  built <- file.path(
    handle$stage,
    vapply(plan$items, `[[`, character(1), "filename")
  )
  labels <- vapply(plan$items, `[[`, character(1), "name")
  names(built) <- vapply(plan$items, `[[`, character(1), "id")
  lapply(seq_along(built), function(index) {
    value <- if (isTRUE(valid_crb)) Cerebro$new() else list(dataset = index)
    saveRDS(value, built[[index]])
  })
  if (identical(backend, "h5")) {
    writeBin(as.raw(1:32), file.path(handle$stage, plan$items[[1L]]$sidecars))
  }
  result <- list(
    state = "success",
    publishable = TRUE,
    stage = handle$stage,
    build_id = "build-app",
    built = built,
    labels = labels,
    verifications = lapply(built, function(path) {
      list(valid = TRUE, path = unname(path))
    }),
    app_dir = NULL,
    app_verification = NULL,
    auth_enabled = FALSE,
    auth_env_file = NULL
  )
  if (isTRUE(make_app)) {
    request <- bundle_request(plan, built, labels)
    auth_runtime <- environment(bundle_request)
    auth_material <- if (isTRUE(request$auth$enabled)) {
      get("builder_auth_create_material", envir = auth_runtime)(
        get("builder_auth_validate_payload", envir = auth_runtime)(
          TRUE,
          list(
            list(
              id = "auth-account-1",
              username = "fixture-a",
              password = "fixture-password-a"
            ),
            list(
              id = "auth-account-2",
              username = "fixture-b",
              password = "fixture-password-b"
            )
          )
        )$accounts,
        handle$stage,
        .capability = function() list(available = TRUE, reason = NULL)
      )
    } else {
      NULL
    }
    result$app_dir <- if (is.function(build_app)) {
      build_app(
        request,
        handle$stage,
        create_app = function(result_dir, ...) {
          builder_app_coordinator_fake_app(
            request,
            result_dir,
            auth_material = auth_material
          )
        },
        auth_material = auth_material
      )
    } else {
      builder_app_coordinator_fake_app(
        request,
        file.path(handle$stage, "cerebro_app"),
        auth_material = auth_material
      )
    }
    result$app_verification <- verify_app(
      result$app_dir,
      request,
      auth_env_file = if (is.null(auth_material)) {
        NULL
      } else {
        file.path(result$app_dir, "viewer-auth.env")
      }
    )
    result$app_tree_identity <- get(
      ".builder_app_tree_summary",
      envir = environment(verify_app)
    )(get(
      ".builder_app_tree_identity",
      envir = environment(verify_app)
    )(result$app_dir))
    if (!is.null(auth_material)) {
      get("builder_auth_cleanup_material", envir = auth_runtime)(
        auth_material,
        handle$stage,
        keep_env = FALSE
      )
      result$auth_enabled <- TRUE
      result$auth_env_file <- file.path(
        result$app_dir,
        "viewer-auth.env"
      )
    } else {
      result$auth_enabled <- FALSE
      result$auth_env_file <- NULL
    }
  }
  list(
    root = root,
    target = target,
    plan = plan,
    handle = handle,
    result = result
  )
}

write_builder_coordinator_record_fixture <- function(root, members) {
  lines <- c(
    "CEREBRO_BUILDER_RELEASE_V1",
    vapply(
      members,
      function(member) {
        paste(member$type, member$path, sep = "\t")
      },
      ""
    )
  )
  writeBin(
    charToRaw(paste0(paste(lines, collapse = "\n"), "\n")),
    file.path(root, ".cerebro-builder-release-v1")
  )
}

test_that("the coordinator preregisters an owner-only assigned stage", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")

    handle <- builder_coordinator_prepare(
      list(
        out_dir = target,
        expected_prior_identity = NULL,
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
      ),
      build_id = "build-1"
    )

    expect_s3_class(handle, "builder_release_coordinator")
    expect_true(dir.exists(handle$stage))
    expect_true(.pathWithin(handle$stage, handle$control))
    if (!identical(.Platform$OS.type, "windows")) {
      expect_identical(
        as.octmode(file.info(handle$control)$mode),
        as.octmode("700")
      )
    }
    journal <- readRDS(handle$journal)
    expect_identical(journal$phase, "prepared")
    expect_identical(journal$build_id, "build-1")
    expect_identical(journal$stage, handle$stage)
    expect_true(dir.exists(handle$lock))

    expect_true(builder_coordinator_abort(handle)$aborted)
    expect_false(dir.exists(handle$stage))
    expect_false(dir.exists(handle$lock))
  })
})

test_that("Windows path budget fails before a stage is created", {
  local({
    builder_task9_source()
    plan <- list(
      items = list(list(
        filename = paste0(strrep("d", 96L), ".crb"),
        sidecars = character()
      ))
    )
    short_stage <- paste0(
      "C:/CerebroBuild/.cbr-0123456789abcdef/stages/s-123456789abc"
    )
    expect_true(.builder_coordinator_assert_windows_path_budget(
      plan,
      short_stage,
      app_expected = TRUE,
      os_type = "windows"
    ))

    h5_plan <- list(
      items = list(list(
        id = "ds3",
        filename = "03-17-0-durafibro-sample-roi-2-ds3.crb",
        sidecars = "03-17-0-durafibro-sample-roi-2-ds3.h5",
        expression_backend = "h5"
      ))
    )
    realistic_stage <- paste0(
      "C:/Users/example/Projects/spatial-project/",
      ".output.cerebro-control/stages/stage-764819176784"
    )
    h5_candidates <- .builder_coordinator_windows_path_candidates(
      h5_plan,
      realistic_stage,
      app_expected = FALSE,
      .tempfile = function(pattern, tmpdir, fileext = "") {
        file.path(tmpdir, paste0(pattern, "76481c31183a", fileext))
      }
    )
    expect_true(any(endsWith(h5_candidates, "-ds3.h5")))
    expect_false(any(endsWith(h5_candidates, strrep("x", 96L))))
    expect_true(.builder_coordinator_assert_windows_path_budget(
      h5_plan,
      realistic_stage,
      app_expected = TRUE,
      os_type = "windows"
    ))

    long_component <- strrep("spatial-user-name-", 20L)
    spatial_plan <- list(
      items = list(list(
        id = long_component,
        filename = "1-dataset-ds1.crb",
        sidecars = character(),
        images = setNames(
          list(setNames(
            list(list(source = list(name = paste0(long_component, ".tiff")))),
            long_component
          )),
          long_component
        )
      ))
    )
    spatial_candidates <- .builder_coordinator_windows_path_candidates(
      spatial_plan,
      short_stage,
      app_expected = FALSE,
      .tempfile = function(pattern, tmpdir, fileext = "") {
        file.path(tmpdir, paste0(pattern, "123456789abc", fileext))
      }
    )
    spatial_candidates <- spatial_candidates[grepl(
      "/[.]spatial/",
      spatial_candidates
    )]
    expect_length(spatial_candidates, 1L)
    expect_match(
      spatial_candidates,
      "/[.]spatial/d-[0-9a-f]{12}/s-[0-9a-f]{12}/i-[0-9a-f]{12}[.]jpg$"
    )
    expect_false(any(grepl(long_component, spatial_candidates, fixed = TRUE)))

    formerly_overreserved_stage <- paste0(
      "F:/Sciebo/mzhlab/David/snRNA-Seq Dura deep/human Dura/",
      "CerebroNexus/app/saved/final_app.cerebro-control/stages/",
      "stage-123456789abc"
    )
    expect_true(.builder_coordinator_assert_windows_path_budget(
      list(items = list()),
      formerly_overreserved_stage,
      app_expected = TRUE,
      os_type = "windows"
    ))
    expect_lte(
      nchar(.builder_release_token("release"), type = "chars"),
      .builder_release_token_max_length("release")
    )
    expect_lte(
      nchar(.builder_release_token("atomic"), type = "chars"),
      .builder_release_token_max_length("atomic")
    )

    project_checkpoint_stage <- paste0(
      "C:/Users/example/Projects/2026_09_14_cerebroNexus/saved/checkpoints/",
      ".20260916T154023.cerebro-control/stages/stage-9c6019424fd8"
    )
    actual_viewer_paths <- file.path(
      "viewer",
      c(
        "gene_expression/UI_projection_genes_separate_panels.R",
        "www/custom.css"
      )
    )
    project_candidates <- .builder_coordinator_windows_path_candidates(
      h5_plan,
      project_checkpoint_stage,
      app_expected = TRUE,
      .tempfile = function(pattern, tmpdir, fileext = "") {
        file.path(tmpdir, paste0(pattern, "9c60742a6a1b", fileext))
      },
      .viewer_relative = actual_viewer_paths
    )
    expect_false(any(grepl(
      "viewer-resource",
      project_candidates,
      fixed = TRUE
    )))
    expect_true(any(endsWith(
      project_candidates,
      actual_viewer_paths[[1L]]
    )))
    expect_true(.builder_coordinator_assert_windows_path_budget(
      h5_plan,
      project_checkpoint_stage,
      app_expected = TRUE,
      os_type = "windows",
      .tempfile = function(pattern, tmpdir, fileext = "") {
        file.path(tmpdir, paste0(pattern, "9c60742a6a1b", fileext))
      },
      .viewer_relative = actual_viewer_paths
    ))

    deep_stage <- paste0(
      "C:/",
      paste(rep("deep-output-folder", 14L), collapse = "/"),
      "/stage-123456789abc"
    )
    expect_error(
      .builder_coordinator_assert_windows_path_budget(
        plan,
        deep_stage,
        app_expected = TRUE,
        os_type = "windows"
      ),
      "selected output folder is too deep"
    )
    expect_true(.builder_coordinator_assert_windows_path_budget(
      plan,
      deep_stage,
      app_expected = TRUE,
      os_type = "unix"
    ))

    plan$items[[1L]]$id <- "dataset-a"
    plan$items[[1L]]$spatial_image_storage <- "external"
    plan$items[[1L]]$images <- list(
      section = list(
        image = list(
          source = list(name = paste0(strrep("histology", 22L), ".jpg"))
        )
      )
    )
    expect_true(
      .builder_coordinator_assert_windows_path_budget(
        plan,
        short_stage,
        app_expected = TRUE,
        os_type = "windows"
      )
    )
  })
})

test_that("coordinator rejects a dangling release-root link before prepare", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    outside <- file.path(root, "outside", "release")
    dir.create(dirname(outside))
    linked <- tryCatch(
      file.symlink(outside, target),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(linked), "Symbolic links are unavailable")

    attempt <- tryCatch(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = file.path(target, "dataset.crb"),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
        ),
        "build-dangling-root"
      ),
      error = function(error) error
    )
    if (inherits(attempt, "builder_release_coordinator")) {
      artifact <- file.path(attempt$stage, "dataset.crb")
      writeLines("new", artifact)
      builder_coordinator_publish(
        attempt,
        list(
          state = "success",
          publishable = TRUE,
          auth_enabled = FALSE,
          auth_env_file = NULL,
          stage = attempt$stage,
          built = artifact
        )
      )
    }

    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "symbolic link")
    }
    expect_false(dir.exists(outside))
    expect_identical(Sys.readlink(target), outside)
  })
})

test_that("coordinator rejects an existing release-root link", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    outside <- file.path(root, "outside")
    target <- file.path(root, "release")
    dir.create(outside)
    linked <- tryCatch(
      file.symlink(outside, target),
      error = function(error) FALSE
    )
    skip_if_not(isTRUE(linked), "Symbolic links are unavailable")

    expect_error(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = file.path(target, "dataset.crb"),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
        ),
        "build-existing-root-link"
      ),
      "symbolic link"
    )
    expect_true(dir.exists(outside))
    expect_identical(Sys.readlink(target), outside)
  })
})

test_that("coordinator rejects an unreadable prior payload", {
  skip_if(identical(.Platform$OS.type, "windows"))
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    payload <- file.path(target, "dataset.crb")
    dir.create(target)
    writeLines("prior", payload)
    Sys.chmod(payload, mode = "0000")
    withr::defer(Sys.chmod(payload, mode = "0600"))
    skip_if(file.access(payload, mode = 4L) == 0L)

    attempt <- tryCatch(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = payload,
          output_release = list(targets = payload),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
        ),
        "build-unreadable-prior"
      ),
      error = function(error) error
    )
    if (inherits(attempt, "builder_release_coordinator")) {
      artifact <- file.path(attempt$stage, "dataset.crb")
      writeLines("new", artifact)
      builder_coordinator_publish(
        attempt,
        list(
          state = "success",
          publishable = TRUE,
          auth_enabled = FALSE,
          auth_env_file = NULL,
          stage = attempt$stage,
          built = artifact
        )
      )
    }

    expect_s3_class(attempt, "error")
    if (inherits(attempt, "error")) {
      expect_match(conditionMessage(attempt), "read")
    }
    expect_true(dir.exists(target))
    expect_true(file.exists(payload))
    Sys.chmod(payload, mode = "0600")
    expect_identical(readLines(payload), "prior")
  })
})

test_that("only one process can coordinate one release", {
  skip_if_not_installed("callr")
  local({
    builder_root <- builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    barrier <- file.path(root, "go")
    runner <- function(id, builder_root, target, barrier) {
      source(file.path(builder_root, "core", "bundle_path_contract.R"))
      source(file.path(builder_root, "publish.R"))
      source(file.path(builder_root, "app_bundle.R"))
      source(file.path(builder_root, "report.R"))
      source(file.path(builder_root, "coordinator.R"))
      while (!file.exists(barrier)) {
        Sys.sleep(0.01)
      }
      tryCatch(
        {
          item <- list(
            id = "dataset-1",
            name = id,
            filename = "dataset.crb",
            analyses = character(),
            included_groups = character(),
            included_projections = character(),
            metadata_policy = list(included = character()),
            expression_backend = "embedded",
            sidecars = character(),
            viewer_page_expectations = list(visible_conditional = character())
          )
          plan <- structure(
            list(
              revision = 1L,
              readiness = "ready",
              out_dir = target,
              overwrite = FALSE,
              targets = file.path(target, "dataset.crb"),
              output_release = list(targets = file.path(target, "dataset.crb")),
              expected_prior_identity = NULL,
              make_app = FALSE,
              dataset_order = "dataset-1",
              items = list(item),
              manifest = list(),
              acknowledgements = list(),
              viewer_bundle_assets = character(),
              private_assets = "dataset.crb",
              app_options = list(enabled = FALSE),
              app_auth = list(
                enabled = FALSE,
                account_count = 0L,
                timeout_minutes = 15L
              )
            ),
            class = c("builder_build_plan", "list")
          )
          handle <- builder_coordinator_prepare(plan, build_id = id)
          writeLines(id, file.path(handle$stage, "dataset.crb"))
          Sys.sleep(0.25)
          result <- builder_coordinator_publish(
            handle,
            list(
              state = "success",
              publishable = TRUE,
              auth_enabled = FALSE,
              auth_env_file = NULL,
              stage = handle$stage,
              built = file.path(handle$stage, "dataset.crb"),
              labels = id,
              verifications = list(list(
                valid = TRUE,
                path = file.path(handle$stage, "dataset.crb"),
                metadata = character()
              )),
              app_dir = NULL,
              app_verification = NULL
            )
          )
          list(published = isTRUE(result$published), error = result$error)
        },
        error = function(error) {
          list(published = FALSE, error = conditionMessage(error))
        }
      )
    }
    first <- callr::r_bg(runner, list("one", builder_root, target, barrier))
    second <- callr::r_bg(runner, list("two", builder_root, target, barrier))
    withr::defer(
      for (process in list(first, second)) {
        if (process$is_alive()) try(process$kill(), silent = TRUE)
      }
    )
    writeLines("go", barrier)
    ## Each process sources the Builder before the race; on a busy machine
    ## that alone can take more than ten seconds.
    first$wait(timeout = 120000)
    second$wait(timeout = 120000)
    results <- list(first$get_result(), second$get_result())

    expect_identical(
      sum(vapply(results, `[[`, logical(1), "published")),
      1L,
      info = paste(
        vapply(results, function(value) value$error %||% "", ""),
        collapse = " | "
      )
    )
    expect_true(file.exists(file.path(target, "dataset.crb")))
  })
})

test_that("coordinator publishes only its verified assigned stage", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    handle <- builder_coordinator_prepare(
      list(
        out_dir = file.path(root, "release"),
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
      ),
      "build-verified"
    )
    writeLines("new", file.path(handle$stage, "dataset.crb"))

    expect_error(
      builder_coordinator_publish(
        handle,
        list(state = "failure", publishable = FALSE, stage = handle$stage)
      ),
      "verified"
    )
    expect_error(
      builder_coordinator_publish(
        handle,
        list(
          state = "success",
          publishable = TRUE,
          auth_enabled = FALSE,
          auth_env_file = NULL,
          stage = root,
          built = file.path(root, "foreign.crb")
        )
      ),
      "assigned stage"
    )
    expect_false(dir.exists(handle$target))
    builder_coordinator_abort(handle)
  })
})

test_that("publication stays in the parent and uses the registered stage", {
  root <- builder_test_inst_path("builder")
  if (!dir.exists(root)) {
    root <- system.file("builder", package = "CerebroNexus")
  }
  app <- builder_app_source_lines()
  worker <- readLines(file.path(root, "worker.R"), warn = FALSE)
  expect_false(any(grepl(
    'source(file.path(dir, "publish.R"))',
    worker,
    fixed = TRUE
  )))
  expect_false(any(grepl("builder_publish_release", worker, fixed = TRUE)))
  expect_false(any(grepl(
    ".cerebro-builder-release-v1",
    worker,
    fixed = TRUE
  )))
  expect_true(any(grepl("builder_coordinator_publish", app, fixed = TRUE)))
})

test_that("parent ownership permits safe release shrinkage", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(file.path(target, "cerebro_app"), recursive = TRUE)
    writeLines("old-a", file.path(target, "01-a.crb"))
    writeLines("old-b", file.path(target, "02-b.crb"))
    writeLines("old-app", file.path(target, "cerebro_app", "app.R"))
    write_builder_coordinator_record_fixture(
      target,
      list(
        list(type = "D", path = "cerebro_app"),
        list(type = "F", path = "01-a.crb"),
        list(type = "F", path = "02-b.crb"),
        list(type = "F", path = "cerebro_app/app.R")
      )
    )
    plan <- builder_crb_coordinator_plan(target, "01-a.crb", overwrite = TRUE)
    handle <- builder_coordinator_prepare(
      plan,
      "build-shrink"
    )
    artifact <- file.path(handle$stage, "01-a.crb")
    writeLines("new-a", artifact)

    result <- builder_coordinator_publish(
      handle,
      builder_crb_coordinator_result(handle, artifact, "Dataset 1")
    )

    expect_true(result$published)
    expect_identical(readLines(file.path(target, "01-a.crb")), "new-a")
    expect_false(file.exists(file.path(target, "02-b.crb")))
    expect_false(dir.exists(file.path(target, "cerebro_app")))
    expect_true(file.exists(file.path(
      target,
      ".cerebro-builder-release-v1"
    )))
  })
})

test_that("unrecorded nested release members remain foreign and untouched", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(file.path(target, "cerebro_app", "private"), recursive = TRUE)
    writeLines("app", file.path(target, "cerebro_app", "app.R"))
    foreign <- file.path(target, "cerebro_app", "private", "notes.txt")
    writeLines("keep me", foreign)
    write_builder_coordinator_record_fixture(
      target,
      list(
        list(type = "D", path = "cerebro_app"),
        list(type = "D", path = "cerebro_app/private"),
        list(type = "F", path = "cerebro_app/app.R")
      )
    )
    planned <- file.path(target, "dataset.crb")

    expect_error(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = planned,
          output_release = list(targets = planned),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
        ),
        "build-nested-foreign"
      ),
      "foreign release entr"
    )
    expect_identical(readLines(foreign), "keep me")
    expect_identical(
      readLines(file.path(target, "cerebro_app", "app.R")),
      "app"
    )
  })
})

test_that("one prior snapshot prevents record ABA from claiming foreign files", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old-a", file.path(target, "a.crb"))
    foreign <- file.path(target, "notes.txt")
    writeLines("keep me", foreign)
    narrow <- list(list(type = "F", path = "a.crb"))
    broad <- list(
      list(type = "F", path = "a.crb"),
      list(type = "F", path = "notes.txt")
    )
    write_builder_coordinator_record_fixture(target, narrow)
    expected_prior <- builder_release_identity(target)
    original_read <- .builder_release_read_record
    reads <- 0L
    .builder_release_read_record <- function(...) {
      record <- original_read(...)
      reads <<- reads + 1L
      if (identical(reads, 1L)) {
        write_builder_coordinator_record_fixture(target, broad)
      } else if (identical(reads, 2L)) {
        write_builder_coordinator_record_fixture(target, narrow)
      }
      record
    }
    planned <- file.path(target, "a.crb")
    result <- tryCatch(
      {
        handle <- builder_coordinator_prepare(
          list(
            out_dir = target,
            overwrite = TRUE,
            targets = planned,
            output_release = list(targets = planned),
            expected_prior_identity = expected_prior,
            app_auth = list(
              enabled = FALSE,
              account_count = 0L,
              timeout_minutes = 15L
            )
          ),
          "build-record-aba"
        )
        artifact <- file.path(handle$stage, "a.crb")
        writeLines("new-a", artifact)
        builder_coordinator_publish(
          handle,
          list(
            state = "success",
            publishable = TRUE,
            auth_enabled = FALSE,
            auth_env_file = NULL,
            stage = handle$stage,
            built = artifact,
            labels = "A"
          )
        )
      },
      error = function(error) error
    )

    expect_s3_class(result, "error")
    expect_true(file.exists(foreign))
    expect_identical(readLines(foreign), "keep me")
  })
})

test_that("legacy releases cannot silently shrink", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old-a", file.path(target, "01-a.crb"))
    writeLines("old-b", file.path(target, "02-b.crb"))

    expect_error(
      builder_coordinator_prepare(
        list(
          out_dir = target,
          overwrite = TRUE,
          targets = file.path(target, "01-a.crb"),
          output_release = list(targets = file.path(target, "01-a.crb")),
          app_auth = list(
            enabled = FALSE,
            account_count = 0L,
            timeout_minutes = 15L
          )
        ),
        "build-legacy-shrink"
      ),
      "foreign release entr"
    )
    expect_identical(readLines(file.path(target, "02-b.crb")), "old-b")

    plan <- builder_crb_coordinator_plan(
      target,
      c("01-a.crb", "02-b.crb"),
      overwrite = TRUE
    )
    handle <- builder_coordinator_prepare(
      plan,
      "build-legacy-same-topology"
    )
    staged <- file.path(handle$stage, c("01-a.crb", "02-b.crb"))
    writeLines("new-a", staged[[1L]])
    writeLines("new-b", staged[[2L]])
    result <- builder_coordinator_publish(
      handle,
      builder_crb_coordinator_result(handle, staged)
    )
    expect_true(result$published)
    expect_true(file.exists(file.path(
      target,
      ".cerebro-builder-release-v1"
    )))
  })
})

test_that("legacy nested topology cannot silently shrink", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    app <- file.path(target, "nested-output")
    dir.create(app, recursive = TRUE)
    writeLines("old-a", file.path(app, "a.txt"))
    writeLines("old-b", file.path(app, "b.txt"))
    plan <- builder_crb_coordinator_plan(
      target,
      "nested-output",
      overwrite = TRUE
    )
    handle <- builder_coordinator_prepare(
      plan,
      "build-legacy-nested-shrink"
    )
    staged_app <- file.path(handle$stage, "nested-output")
    dir.create(staged_app)
    writeLines("new-a", file.path(staged_app, "a.txt"))

    expect_error(
      builder_coordinator_publish(
        handle,
        builder_crb_coordinator_result(handle, staged_app)
      ),
      "legacy release topology"
    )
    expect_identical(readLines(file.path(app, "a.txt")), "old-a")
    expect_identical(readLines(file.path(app, "b.txt")), "old-b")
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("record write failure leaves the verified stage unpublished", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")
    handle <- builder_coordinator_prepare(
      plan,
      "build-record-failure"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("new", artifact)

    expect_error(
      builder_coordinator_publish(
        handle,
        builder_crb_coordinator_result(handle, artifact),
        .record_move = function(from, to) FALSE
      ),
      "atomically"
    )
    expect_false(dir.exists(target))
    expect_identical(readLines(artifact), "new")
    expect_false(file.exists(file.path(
      handle$stage,
      ".cerebro-builder-release-v1"
    )))
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("payload changes during record commit cannot be published", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")
    handle <- builder_coordinator_prepare(
      plan,
      "build-record-race"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("verified", artifact)

    result <- tryCatch(
      builder_coordinator_publish(
        handle,
        builder_crb_coordinator_result(handle, artifact),
        .record_move = function(from, to) {
          writeLines("tampered", artifact)
          file.rename(from, to)
        }
      ),
      error = function(error) error
    )

    expect_s3_class(result, "error")
    expect_match(conditionMessage(result), "changed during ownership")
    expect_false(dir.exists(target))
    expect_identical(readLines(artifact), "tampered")
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("whole-release publication preserves foreign output occupants", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    foreign <- file.path(target, "notes.txt")
    writeLines("keep me", foreign)
    plan <- list(
      out_dir = target,
      overwrite = TRUE,
      targets = file.path(target, "dataset.crb"),
      output_release = list(targets = file.path(target, "dataset.crb")),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    )

    expect_error(
      builder_coordinator_prepare(plan, "build-foreign-release"),
      "foreign release"
    )
    expect_identical(readLines(foreign), "keep me")
    expect_false(dir.exists(builder_release_control_path(target)))
  })
})

test_that("output preflight reports foreign occupants before staging", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    foreign <- c("builder-project.json", "datasets", "sources")
    writeLines("project", file.path(target, foreign[[1L]]))
    dir.create(file.path(target, foreign[[2L]]))
    dir.create(file.path(target, foreign[[3L]]))
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")

    preflight <- builder_coordinator_output_preflight(plan)

    expect_setequal(preflight$foreign, foreign)
    expect_false(dir.exists(builder_release_control_path(target)))
  })
})

test_that("output preflight can reuse a previously verified release state", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    plan <- builder_crb_coordinator_plan(
      target,
      "dataset.crb",
      overwrite = TRUE
    )
    verified <- builder_coordinator_output_preflight(plan)

    reused <- builder_coordinator_output_preflight(
      plan,
      prior_state = verified$prior_state
    )

    expect_identical(reused, verified)
  })
})

test_that("output preflight ignores Finder metadata only", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("finder", file.path(target, ".DS_Store"))
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")

    clean <- builder_coordinator_output_preflight(plan)
    expect_length(clean$foreign, 0L)

    writeLines("foreign", file.path(target, ".unknown"))
    foreign <- builder_coordinator_output_preflight(plan)
    expect_identical(foreign$foreign, ".unknown")
  })
})

test_that("known prior outputs still require explicit replacement", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    writeLines("old", file.path(target, "dataset.crb"))
    plan <- list(
      out_dir = target,
      overwrite = FALSE,
      targets = file.path(target, "dataset.crb"),
      output_release = list(targets = file.path(target, "dataset.crb")),
      app_auth = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    )

    expect_error(
      builder_coordinator_prepare(plan, "build-no-replace"),
      "Replace existing outputs"
    )
    expect_identical(readLines(file.path(target, "dataset.crb")), "old")
  })
})

test_that("an abandoned Builder release shell can be reused without replacement", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    write_builder_coordinator_record_fixture(
      target,
      list(
        list(type = "D", path = "cerebro_app"),
        list(type = "F", path = "cerebro_app/app.R"),
        list(type = "F", path = "dataset.crb")
      )
    )
    writeLines("finder", file.path(target, ".DS_Store"))
    planned <- file.path(target, "dataset.crb")

    handle <- builder_coordinator_prepare(
      list(
        out_dir = target,
        overwrite = FALSE,
        targets = planned,
        output_release = list(targets = planned),
        app_auth = list(
          enabled = FALSE,
          account_count = 0L,
          timeout_minutes = 15L
        )
      ),
      "build-abandoned-shell"
    )

    expect_true(handle$expected_prior_state$record$abandoned)
    expect_true(file.exists(file.path(target, ".cerebro-builder-release-v1")))
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("unplanned staged artifacts cannot enter the release", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")
    handle <- builder_coordinator_prepare(
      plan,
      "build-unplanned"
    )
    artifact <- file.path(handle$stage, "dataset.crb")
    writeLines("planned", artifact)
    writeLines("unexpected", file.path(handle$stage, "surprise.txt"))

    expect_error(
      builder_coordinator_publish(
        handle,
        builder_crb_coordinator_result(handle, artifact)
      ),
      "unplanned"
    )
    expect_false(dir.exists(target))
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("coordinator freezes the complete App publication expectation", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_app_coordinator_plan_fixture(target)
    handle <- builder_coordinator_prepare(plan, "build-frozen-app")

    expect_true(handle$app_expectation$expected)
    expect_identical(
      names(handle$app_expectation),
      c(
        "expected",
        "contract_version",
        "dataset_ids",
        "labels",
        "dataset_labels",
        "filenames",
        "initial_dataset",
        "initial_dataset_mode",
        "initial_page",
        "show_upload_ui",
        "welcome_message",
        "point_size",
        "variable_to_compare",
        "host",
        "port",
        "max_request_size",
        "display_mode",
        "launch_browser",
        "auth",
        "colors",
        "backend_plan",
        "app_dir"
      )
    )
    frozen <- unserialize(serialize(handle$app_expectation, NULL))
    plan$items[[1L]]$name <- "Mutated"
    plan$items[[1L]]$colors$cluster[["A"]] <- "#ff0000"
    plan$app_options$initial_dataset <- "dataset-a"
    expect_identical(handle$app_expectation, frozen)
    expect_identical(
      handle$app_expectation$app_dir,
      file.path(handle$stage, "cerebro_app")
    )
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("coordinator preserves per-dataset Viewer defaults for parent verification", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    roi_settings <- list(
      "fov-a" = list(
        lesion = list(
          rotation_degrees = 37.5,
          point_opacity = 0.65,
          point_size = 7
        )
      )
    )
    point_appearance <- list(
      "fov-a" = list(point_opacity = 0.6, point_size = 7)
    )
    plan$items[[2L]]$spatial_point_appearance <- point_appearance
    plan$items[[2L]]$spatial_roi_settings <- roi_settings
    plan$items[[2L]]$spatial_image_storage <- "external"
    contract <- .builder_coordinator_app_contract(plan)

    expect_identical(
      contract$plan$items[[2L]]$spatial_point_appearance,
      point_appearance
    )
    expect_identical(
      contract$plan$items[[2L]]$spatial_roi_settings,
      roi_settings
    )
    expect_identical(
      contract$plan$items[[2L]]$spatial_image_storage,
      "external"
    )
    expect_identical(
      contract$plan$items[[1L]]$spatial_image_storage,
      "external"
    )
    expect_identical(
      contract$plan$items[[1L]]$default_projection,
      "umap"
    )
    expect_null(contract$plan$items[[1L]]$default_trajectory)
    expect_identical(
      contract$plan$items[[1L]]$overview_point_size,
      5
    )
    expect_identical(
      contract$plan$items[[2L]]$default_projection,
      "pca"
    )
    expect_identical(
      contract$plan$items[[2L]]$default_trajectory,
      list(method = "slingshot", name = "lineage")
    )
    expect_identical(
      contract$plan$items[[2L]]$overview_point_size,
      7
    )
  })
})

test_that("coordinator exact targets allow declared App tree members", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    handle <- builder_coordinator_prepare(plan, "app-tree-members")
    dir.create(
      file.path(handle$stage, "cerebro_app", "viewer"),
      recursive = TRUE
    )
    writeLines("app", file.path(handle$stage, "cerebro_app", "app.R"))
    writeLines(
      "viewer",
      file.path(handle$stage, "cerebro_app", "viewer", "ui.R")
    )
    writeLines("report", file.path(handle$stage, "build-report.json"))

    identity <- .builder_coordinator_stage_identity(
      handle,
      expected = c("cerebro_app", "build-report.json"),
      exact = TRUE
    )
    expect_true(any(vapply(
      identity$entries,
      function(entry) identical(entry$path, "cerebro_app/viewer/ui.R"),
      logical(1)
    )))
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("parent and worker requests retain identical Viewer defaults", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$items[[1L]]$overview_percentage_cells_to_show <- 65
    plan$items[[2L]]$overview_percentage_cells_to_show <- 80
    plan$items[[2L]]$spatial_point_appearance <- list(
      "fov-a" = list(point_opacity = 0.6, point_size = 7)
    )
    handle <- builder_coordinator_prepare(plan, "viewer-defaults")
    artifacts <- file.path(
      handle$stage,
      vapply(
        plan$items,
        `[[`,
        character(1),
        "filename"
      )
    )
    lapply(artifacts, function(path) saveRDS(list(), path))
    names(artifacts) <- vapply(plan$items, `[[`, character(1), "id")
    labels <- vapply(plan$items, `[[`, character(1), "name")

    worker_request <- builder_app_bundle_request(
      plan,
      artifacts,
      labels
    )
    parent_request <- builder_app_bundle_request(
      handle$app_plan,
      artifacts,
      labels
    )

    expect_identical(
      parent_request$viewer_content,
      worker_request$viewer_content
    )
    expect_identical(
      parent_request$viewer_content[["dataset-a"]][[
        "overview_percentage_cells_to_show"
      ]],
      65
    )
    expect_identical(
      parent_request$viewer_content[["dataset-b"]][[
        "overview_percentage_cells_to_show"
      ]],
      80
    )
    expect_identical(
      parent_request$viewer_content[["dataset-b"]][[
        "spatial_point_appearance"
      ]],
      plan$items[[2L]]$spatial_point_appearance
    )
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("App publication requires an exact worker tree identity", {
  local({
    builder_task9_source()

    missing <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    missing$result$app_tree_identity <- NULL
    expect_error(
      builder_coordinator_publish(missing$handle, missing$result),
      "App identity"
    )
    expect_true(builder_coordinator_abort(missing$handle)$aborted)

    forged <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    forged$result$app_tree_identity <- new.env(parent = emptyenv())
    expect_error(
      builder_coordinator_publish(forged$handle, forged$result),
      "App identity"
    )
    expect_true(builder_coordinator_abort(forged$handle)$aborted)

    wrong_build <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    wrong_build$result$build_id <- "another-build"
    expect_error(
      builder_coordinator_publish(wrong_build$handle, wrong_build$result),
      "build identity"
    )
    expect_true(builder_coordinator_abort(wrong_build$handle)$aborted)

    escaped <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    escaped$result$app_dir <- escaped$root
    expect_error(
      builder_coordinator_publish(escaped$handle, escaped$result),
      "assigned App directory"
    )
    expect_true(builder_coordinator_abort(escaped$handle)$aborted)

    missing_dir <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    unlink(missing_dir$result$app_dir, recursive = TRUE)
    expect_error(
      builder_coordinator_publish(missing_dir$handle, missing_dir$result),
      "assigned App directory"
    )
    expect_true(builder_coordinator_abort(missing_dir$handle)$aborted)
  })
})

test_that("Windows App publication accepts canonical slash variants", {
  skip_if_not(identical(.Platform$OS.type, "windows"))
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_app_coordinator_plan_fixture(target)
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    fixture <- builder_app_coordinator_fixture(
      root = root,
      target = target,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    fixture$result$app_dir <- gsub(
      "/",
      "\\\\",
      fixture$result$app_dir,
      fixed = TRUE
    )

    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result
    )
    expect_true(published$published)
    expect_true(dir.exists(published$app_dir))
  })
})

test_that("App verification reports its sub-steps to the Build card", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_app_coordinator_plan_fixture(target)
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    fixture <- builder_app_coordinator_fixture(
      root = root,
      target = target,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    reported <- list()
    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result,
      .on_progress = function(phase, detail = NULL) {
        reported[[length(reported) + 1L]] <<- c(list(phase = phase), detail)
      }
    )

    expect_true(published$published)
    phases <- vapply(reported, `[[`, character(1), "phase")
    expect_identical(unique(phases), c("verify", "publish"))
    labels <- unique(unlist(lapply(reported, `[[`, "step_label")))
    expect_identical(
      labels,
      c(
        "Checking build result",
        "Checking datasets",
        "Checking spatial data",
        "Checking Viewer files",
        "Checking Viewer runtime",
        "Checking Viewer content",
        "Checking files",
        "Writing build report",
        "Checking final files",
        "Recording release"
      )
    )
    datasets <- Filter(
      function(value) identical(value$step_label, "Checking datasets"),
      reported
    )
    indexes <- vapply(datasets, function(value) value$step_index, integer(1))
    expect_identical(indexes[[1L]], 0L)
    expect_identical(indexes[[length(indexes)]], 2L)
    expect_true(all(vapply(datasets, `[[`, integer(1), "step_count") == 2L))
    # Every file pass ends on its last file, even when throttled.
    for (label in c(
      "Checking files",
      "Checking final files",
      "Recording release"
    )) {
      pass <- Filter(
        function(value) identical(value$step_label, label),
        reported
      )
      last <- pass[[length(pass)]]
      expect_gt(last$step_count, 0L)
      expect_identical(last$step_index, last$step_count, info = label)
    }
  })
})

test_that("verification sub-steps are throttled and need a detail callback", {
  local({
    builder_task9_source()
    clock <- 0
    seen <- list()
    step <- .builder_coordinator_verify_reporter(
      function(phase, detail) seen[[length(seen) + 1L]] <<- detail,
      interval = 0.25,
      .now = function() clock
    )
    step("Checking files", 0L, 3L)
    step("Checking files", 1L, 3L)
    step("Checking files", 2L, 3L)
    clock <- 0.3
    step("Checking files", 2L, 3L)
    step("Checking files", 3L, 3L)
    step("Writing build report")
    expect_identical(
      vapply(
        seen,
        function(value) {
          paste(value$step_label, value$step_index %||% "-", sep = "|")
        },
        character(1)
      ),
      c(
        "Checking files|0",
        "Checking files|2",
        "Checking files|3",
        "Writing build report|-"
      )
    )

    phases <- character()
    quiet <- .builder_coordinator_verify_reporter(
      function(phase) phases <<- c(phases, phase)
    )
    expect_false(quiet("Checking files", 1L, 3L))
    expect_length(phases, 0L)
    broken <- .builder_coordinator_verify_reporter(
      function(phase, detail) stop("disk full")
    )
    expect_true(broken("Checking files", 1L, 3L))
  })
})

test_that("coordinator requires exact scalar authentication evidence", {
  local({
    builder_task9_source()
    login <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    for (value in list(NULL, NA, "TRUE", FALSE)) {
      forged <- login$result$app_verification
      forged["auth_enabled"] <- list(value)
      expect_error(
        .builder_coordinator_app_verification(
          forged,
          login$handle$app_expectation
        ),
        "differs from the frozen plan"
      )
    }
    expect_true(builder_coordinator_abort(login$handle)$aborted)

    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    public <- builder_app_coordinator_fixture(
      root = root,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    for (value in list(NULL, NA, "FALSE", TRUE)) {
      forged <- public$result$app_verification
      forged["auth_enabled"] <- list(value)
      expect_error(
        .builder_coordinator_app_verification(
          forged,
          public$handle$app_expectation
        ),
        "differs from the frozen plan"
      )
    }
    expect_true(builder_coordinator_abort(public$handle)$aborted)
  })
})

test_that("coordinator binds top-level build authentication to all plan modes", {
  local({
    builder_task9_source()
    check_forged <- function(fixture, enabled, env_file) {
      valid <- fixture$result
      expect_silent(.builder_coordinator_validate_build_auth(
        fixture$handle,
        valid
      ))
      for (value in list(NULL, NA, "TRUE", 0, !enabled)) {
        forged <- valid
        forged["auth_enabled"] <- list(value)
        expect_error(
          .builder_coordinator_validate_build_auth(fixture$handle, forged),
          "authentication evidence"
        )
      }
      forged_env <- valid
      forged_env["auth_env_file"] <- list(env_file)
      expect_error(
        .builder_coordinator_validate_build_auth(fixture$handle, forged_env),
        "authentication evidence"
      )
      expect_true(builder_coordinator_abort(fixture$handle)$aborted)
    }

    login <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    check_forged(login, TRUE, file.path(login$root, "wrong.env"))

    public_root <- withr::local_tempdir()
    public_plan <- builder_app_coordinator_plan_fixture(
      file.path(public_root, "release")
    )
    public_plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    public_plan$targets <- file.path(public_root, "release", "cerebro_app")
    public_plan$output_release$targets <- public_plan$targets
    public <- builder_app_coordinator_fixture(
      root = public_root,
      plan = public_plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    check_forged(
      public,
      FALSE,
      file.path(public$handle$stage, "viewer-auth.env")
    )

    crb_root <- withr::local_tempdir()
    crb_plan <- builder_crb_coordinator_plan(
      file.path(crb_root, "release"),
      "dataset.crb"
    )
    crb <- builder_coordinator_prepare(crb_plan, "crb-auth")
    artifact <- file.path(crb$stage, "dataset.crb")
    saveRDS(list(ok = TRUE), artifact)
    crb_fixture <- list(
      root = crb_root,
      handle = crb,
      result = builder_crb_coordinator_result(crb, artifact)
    )
    check_forged(
      crb_fixture,
      FALSE,
      file.path(crb$stage, "viewer-auth.env")
    )
  })
})

test_that("CRB-only publication rejects an unexpected staged App", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      make_app = FALSE,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    fixture$result$app_dir <- file.path(fixture$handle$stage, "cerebro_app")
    dir.create(fixture$result$app_dir)
    fixture$result$app_verification <- structure(
      list(valid = TRUE),
      class = c("builder_app_verification", "list")
    )

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "unexpected App"
    )
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("parent rejects an App changed after worker assembly", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    staged_crb <- unname(fixture$result$built[[1L]])
    app_crb <- file.path(
      fixture$result$app_dir,
      "private-data",
      basename(staged_crb)
    )
    saveRDS(list(dataset = "changed"), staged_crb)
    file.copy(staged_crb, app_crb, overwrite = TRUE)

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "changed after worker assembly|differs from its trusted template"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("parent binds its App verification to final payload identity", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    app_crb <- file.path(
      fixture$result$app_dir,
      "private-data",
      basename(unname(fixture$result$built[[1L]]))
    )
    original_identity <- .builder_coordinator_stage_identity
    original_md5 <- .builder_release_payload_md5
    injected <- FALSE
    hashed <- character()
    .builder_release_payload_md5 <- function(path) {
      hashed <<- c(
        hashed,
        normalizePath(path, winslash = "/", mustWork = TRUE)
      )
      original_md5(path)
    }
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        original_mtime <- file.info(app_crb)$mtime[[1L]]
        bytes <- readBin(app_crb, "raw", n = file.info(app_crb)$size)
        bytes[[length(bytes)]] <- as.raw(bitwXor(
          as.integer(bytes[[length(bytes)]]),
          1L
        ))
        writeBin(bytes, app_crb)
        Sys.setFileTime(app_crb, original_mtime)
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "changed after parent verification"
    )
    expect_true(
      normalizePath(app_crb, winslash = "/", mustWork = TRUE) %in% hashed
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("parent binds staged CRB closure to final payload identity", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        saveRDS(
          list(dataset = "post-parent-source-change"),
          unname(fixture$result$built[[1L]])
        )
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("same-byte staged CRB hard-link swaps stay unpublished", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    staged_crb <- unname(fixture$result$built[[1L]])
    outside_copy <- file.path(fixture$root, "same-byte-source.crb")
    copied <- file.copy(staged_crb, outside_copy)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        unlink(staged_crb)
        stopifnot(file.link(outside_copy, staged_crb))
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "input closure changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("an H5 sidecar type remains bound after parent verification", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      backend = "h5",
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    sidecar <- file.path(fixture$handle$stage, "dataset-a.h5")
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        unlink(sidecar, recursive = TRUE)
        dir.create(sidecar)
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "input closure changed after parent verification"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("same-byte App hard-link swaps fail after parent verification", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    app_crb <- file.path(
      fixture$result$app_dir,
      "private-data",
      basename(fixture$result$built[[1L]])
    )
    outside_link <- file.path(fixture$root, "same-bytes.crb")
    copied <- file.copy(app_crb, outside_link)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")
    original_identity <- .builder_coordinator_stage_identity
    injected <- FALSE
    .builder_coordinator_stage_identity <- function(...) {
      if (!injected) {
        injected <<- TRUE
        unlink(app_crb)
        stopifnot(file.link(outside_link, app_crb))
      }
      original_identity(...)
    }

    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "Publication verification failed"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("App metadata races during ownership commit stay unpublished", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    app_crb <- file.path(
      fixture$result$app_dir,
      "private-data",
      basename(fixture$result$built[[1L]])
    )
    outside_link <- file.path(fixture$root, "ownership-race.crb")
    copied <- file.copy(app_crb, outside_link)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")

    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .record_move = function(from, to) {
          unlink(app_crb)
          stopifnot(file.link(outside_link, app_crb))
          file.rename(from, to)
        }
      ),
      "Publication verification failed"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("input closure races fail the ownership record guard", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    staged_crb <- unname(fixture$result$built[[1L]])
    outside_copy <- file.path(fixture$root, "ownership-source-race.crb")
    copied <- file.copy(staged_crb, outside_copy)
    skip_if_not(isTRUE(copied), "A same-byte fixture could not be created")

    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .record_move = function(from, to) {
          unlink(staged_crb)
          stopifnot(file.link(outside_copy, staged_crb))
          file.rename(from, to)
        }
      ),
      "release ownership record does not match the complete release"
    )
    expect_false(dir.exists(fixture$target))
    expect_true(builder_coordinator_abort(fixture$handle)$aborted)
  })
})

test_that("verified Apps publish with final paths and parent ownership", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    stage <- fixture$handle$stage

    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result
    )

    expect_true(published$published)
    expect_true(file.exists(published$report_path))
    expect_identical(
      published$app_dir,
      file.path(published$release$target, "cerebro_app")
    )
    expect_identical(
      unname(published$built),
      file.path(
        published$release$target,
        "cerebro_app",
        "private-data",
        basename(fixture$result$built)
      )
    )
    expect_identical(names(published$built), names(fixture$result$built))
    expect_identical(
      unname(vapply(
        published$verifications,
        `[[`,
        character(1),
        "path"
      )),
      unname(published$built)
    )
    expect_null(published$app_verification)
    expect_null(published$stage)
    expect_false(any(grepl(
      stage,
      unlist(published, recursive = TRUE, use.names = FALSE),
      fixed = TRUE
    )))
    record <- .builder_release_read_record(
      published$release$target,
      exact = TRUE
    )
    member_paths <- vapply(record$members, `[[`, character(1), "path")
    expect_true(all(
      c(
        "build-report.json",
        "cerebro_app",
        "cerebro_app/app.R",
        "cerebro_app/private-data/dataset-a.crb",
        "cerebro_app/private-data/dataset-b.crb"
      ) %in%
        member_paths
    ))
  })
})

test_that("App publication reuses full release identities", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    original_identity <- .builder_app_tree_identity
    identity_calls <- 0L
    .builder_app_tree_identity <- function(...) {
      identity_calls <<- identity_calls + 1L
      original_identity(...)
    }

    published <- builder_coordinator_publish(fixture$handle, fixture$result)

    expect_true(published$published)
    expect_identical(identity_calls, 6L)
  })
})

test_that("App build and publication hash an H5 payload three times", {
  local({
    builder_task9_source()
    original_tree_identity <- .builder_app_tree_identity
    original_release_md5 <- .builder_release_payload_md5
    hashed <- character()
    record_hash <- function(path, digest) {
      if (identical(basename(path), "dataset-a.h5")) {
        hashed <<- c(hashed, normalizePath(path, winslash = "/"))
      }
      digest(path)
    }
    .builder_app_tree_identity <- function(
      root,
      .digest_file = tools::md5sum,
      .previous = NULL
    ) {
      original_tree_identity(
        root,
        .digest_file = function(path) {
          record_hash(path, .digest_file)
        },
        .previous = .previous
      )
    }
    .builder_release_payload_md5 <- function(path) {
      record_hash(path, original_release_md5)
    }
    root <- withr::local_tempdir(.local_envir = environment())
    plan <- builder_app_coordinator_plan_fixture(
      file.path(root, "release"),
      backend = "h5"
    )
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    fixture <- builder_app_coordinator_fixture(
      root = root,
      plan = plan,
      backend = "h5",
      valid_crb = TRUE,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app,
      build_app = builder_build_app
    )

    published <- builder_coordinator_publish(fixture$handle, fixture$result)

    expect_true(published$published)
    expect_identical(length(hashed), 3L)
  })
})

test_that("in-place App publication tolerates only root relocation metadata", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    stage <- file.path(root, "stage")
    app_stage <- file.path(stage, "cerebro_app")
    dir.create(app_stage, recursive = TRUE)
    writeLines("same bytes", file.path(app_stage, "app.R"))

    parent <- .builder_app_tree_identity(app_stage)
    portable <- .builder_app_tree_summary(parent)
    current <- parent
    current$root_fingerprint$change_time <-
      current$root_fingerprint$change_time + 1

    expect_identical(.builder_app_tree_summary(current), portable)
    expect_false(identical(current, parent))
    expect_true(.builder_coordinator_publication_tree_matches(
      current,
      parent,
      portable,
      "after_in_place"
    ))
    expect_false(.builder_coordinator_publication_tree_matches(
      current,
      parent,
      portable,
      "after_rename"
    ))

    swapped <- current
    swapped$entries[[1L]]$inode <- swapped$entries[[1L]]$inode + 1024
    expect_identical(.builder_app_tree_summary(swapped), portable)
    expect_false(.builder_coordinator_publication_tree_matches(
      swapped,
      parent,
      portable,
      "after_in_place"
    ))

    writeLines("changed", file.path(app_stage, "app.R"))
    changed <- .builder_app_tree_identity(app_stage)
    expect_false(.builder_coordinator_publication_tree_matches(
      changed,
      parent,
      portable,
      "after_in_place"
    ))
  })
})

test_that("a verified App publishes through a locked output root", {
  local({
    builder_task9_source()
    .builder_app_package_path <- function(...) builder_profile_inst_path(...)
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    plan <- builder_app_coordinator_plan_fixture(target)
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    fixture <- builder_app_coordinator_fixture(
      root = root,
      target = target,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    locked_root <- function(from, to) {
      if (
        identical(.builder_release_path(from), .builder_release_path(target)) &&
          identical(
            .builder_release_path(to),
            .builder_release_path(fixture$handle$backup)
          )
      ) {
        return(FALSE)
      }
      file.rename(from, to)
    }
    publish <- function(handle, .verify_payload, .digest_cache) {
      builder_publish_release(
        handle,
        .move = locked_root,
        .verify_payload = .verify_payload,
        .digest_cache = .digest_cache,
        .allow_in_place = TRUE
      )
    }

    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result,
      .publish = publish
    )

    expect_true(published$published)
    expect_true(dir.exists(file.path(target, "cerebro_app")))
    expect_true(file.exists(file.path(target, "build-report.json")))
    expect_identical(
      published$release$publication_mode,
      "in_place"
    )
    expect_true(published$release$control_cleaned)
    expect_false(dir.exists(fixture$handle$control))
  })
})

test_that("coordinator reports after App verification and before ownership", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    events <- character()

    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result,
      .verify_app = function(...) {
        events <<- c(events, "app_verify")
        builder_verify_app(...)
      },
      .write_report = function(stage, report) {
        events <<- c(events, "report")
        builder_write_build_report(stage, report)
      },
      .record_move = function(from, to) {
        events <<- c(events, "ownership")
        file.rename(from, to)
      }
    )

    expect_identical(events, c("app_verify", "report", "ownership"))
    expect_true(published$published)
    expect_true(file.exists(published$report_path))
  })
})

test_that("coordinator reuses parent-verified App digests for publication", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    fixture <- builder_app_coordinator_fixture(
      root = root,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    tree <- .builder_app_tree_identity(fixture$result$app_dir)
    file_entries <- Filter(
      function(entry) identical(entry$type, "file"),
      tree$entries
    )
    key_fields <- c(
      "type",
      "permissions",
      "size",
      "device_id",
      "inode",
      "hard_links",
      "modification_time",
      "change_time"
    )
    expect_true(length(file_entries) > 0L)
    expect_true(all(vapply(
      file_entries,
      function(entry) {
        all(vapply(
          entry[key_fields],
          function(value) length(value) == 1L && !is.na(value),
          logical(1)
        ))
      },
      logical(1)
    )))
    cache_keys <- vapply(
      file_entries,
      .builder_release_digest_cache_key,
      character(1)
    )
    expect_false(anyNA(cache_keys))
    expect_identical(anyDuplicated(cache_keys), 0L)
    original_md5 <- .builder_release_payload_md5
    hashed <- character()
    .builder_release_payload_md5 <- function(path) {
      hashed <<- c(
        hashed,
        normalizePath(
          path,
          winslash = "/",
          mustWork = TRUE
        )
      )
      original_md5(path)
    }

    published <- builder_coordinator_publish(fixture$handle, fixture$result)

    expect_true(published$published)
    expect_false(any(
      basename(hashed) %in%
        c(
          "dataset-a.crb",
          "dataset-b.crb"
        )
    ))
    expect_false(any(grepl("/cerebro_app/", hashed, fixed = TRUE)))
  })
})

test_that("coordinator reuses the ownership identity before final publication", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    stage <- normalizePath(
      fixture$handle$stage,
      winslash = "/",
      mustWork = TRUE
    )
    original_identity <- builder_release_identity
    events <- character()
    builder_release_identity <- function(target, ...) {
      path <- normalizePath(target, winslash = "/", mustWork = FALSE)
      if (identical(path, stage)) {
        events <<- c(events, "stage_identity")
      }
      original_identity(target, ...)
    }

    published <- builder_coordinator_publish(
      fixture$handle,
      fixture$result,
      .record_move = function(from, to) {
        events <<- c(events, "ownership")
        file.rename(from, to)
      },
      .publish = function(...) {
        events <<- c(events, "publish_entry")
        builder_publish_release(...)
      }
    )

    after_ownership <- events[seq.int(
      match("ownership", events),
      length(events)
    )]
    expect_identical(
      after_ownership,
      c(
        "ownership",
        "stage_identity",
        "publish_entry",
        "stage_identity"
      )
    )
    expect_true(published$published)
  })
})

test_that("coordinator freezes only the portable report-plan projection", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_app_coordinator_plan_fixture(file.path(root, "release"))
    plan$runtime_only <- new.env(parent = emptyenv())

    handle <- builder_coordinator_prepare(plan, "portable-report-plan")

    expect_s3_class(handle$report_plan, "builder_build_plan")
    expect_false(.builder_app_has_reference(handle$report_plan))
    expect_false("runtime_only" %in% names(handle$report_plan))
    expect_identical(handle$report_plan$dataset_order, plan$dataset_order)
    expect_identical(
      handle$report_plan$output_release$targets,
      plan$output_release$targets
    )
    expect_identical(
      handle$report_plan$items[[1L]]$spatial_image_storage,
      plan$items[[1L]]$spatial_image_storage %||% "external"
    )
    expect_identical(
      handle$report_plan$items[[1L]]$spatial_alignment,
      list(
        section_count = as.integer(
          plan$items[[1L]]$spatial_alignment$section_count %||% 0L
        ),
        image_count = as.integer(
          plan$items[[1L]]$spatial_alignment$image_count %||% 0L
        )
      )
    )
    expect_true(builder_coordinator_abort(handle)$aborted)
  })
})

test_that("coordinator report projection rebuilds only safe auth fields", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_crb_coordinator_plan(
      file.path(root, "release"),
      "dataset-a.crb"
    )
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L,
      accounts = "auth-report-account-sentinel-91c4",
      passphrase = "auth-report-passphrase-sentinel-91c4"
    )

    report_plan <- .builder_coordinator_report_plan(plan)

    expect_identical(
      report_plan$app_auth,
      list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
    )
    expect_false(builder_auth_value_contains(
      report_plan,
      "auth-report-account-sentinel-91c4"
    ))
    expect_false(builder_auth_value_contains(
      report_plan,
      "auth-report-passphrase-sentinel-91c4"
    ))
  })
})

test_that("coordinator rejects forged auth on CRB-only plans", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    plan <- builder_crb_coordinator_plan(
      file.path(root, "release"),
      "dataset-a.crb"
    )
    cases <- list(
      extra_fields = list(
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L,
        accounts = "auth-forged-account-sentinel-91c4",
        passphrase = "auth-forged-passphrase-sentinel-91c4"
      ),
      login_without_app = list(
        enabled = TRUE,
        account_count = 1L,
        timeout_minutes = 15L
      )
    )
    for (name in names(cases)) {
      plan$app_auth <- cases[[name]]
      error <- tryCatch(
        builder_coordinator_prepare(plan, paste0("forged-crb-auth-", name)),
        error = identity
      )

      expect_s3_class(error, "error")
      expect_match(
        conditionMessage(error),
        "invalid",
        ignore.case = TRUE,
        info = name
      )
      expect_false(builder_auth_value_contains(
        error,
        "auth-forged-account-sentinel-91c4"
      ))
      expect_false(builder_auth_value_contains(
        error,
        "auth-forged-passphrase-sentinel-91c4"
      ))
    }
  })
})

test_that("report failure preserves the prior release unpublished", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    dir.create(target)
    saveRDS("prior", file.path(target, "dataset-a.crb"))
    prior <- builder_release_identity(target)
    plan <- builder_app_coordinator_plan_fixture(target, make_app = FALSE)
    plan$overwrite <- TRUE
    plan$expected_prior_identity <- prior
    fixture <- builder_app_coordinator_fixture(
      root = root,
      target = target,
      make_app = FALSE,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app,
      plan = plan
    )

    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .write_report = function(...) stop("injected report failure")
      ),
      "report failure"
    )
    expect_identical(readRDS(file.path(target, "dataset-a.crb")), "prior")
    expect_false(file.exists(file.path(target, "build-report.json")))
    expect_true(dir.exists(fixture$handle$stage))
  })
})

test_that("public Apps reach publication guards without authentication material", {
  local({
    builder_task9_source()
    root <- withr::local_tempdir()
    target <- file.path(root, "release")
    plan <- builder_app_coordinator_plan_fixture(target)
    plan$app_auth <- list(
      enabled = FALSE,
      account_count = 0L,
      timeout_minutes = 15L
    )
    plan$targets <- file.path(target, "cerebro_app")
    plan$output_release$targets <- plan$targets
    fixture <- builder_app_coordinator_fixture(
      root = root,
      target = target,
      plan = plan,
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    published <- builder_coordinator_publish(fixture$handle, fixture$result)
    expect_true(published$published)
    expect_false(file.exists(file.path(target, "viewer-auth.env")))
    expect_true(dir.exists(file.path(target, "cerebro_app")))
  })
})

test_that("login publication removes only transient root inputs after reporting", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    stage <- fixture$handle$stage
    published <- builder_coordinator_publish(fixture$handle, fixture$result)
    target <- published$release$target
    root_inputs <- basename(unname(fixture$result$built))
    expect_false(any(file.exists(file.path(target, root_inputs))))
    expect_true(all(file.exists(file.path(
      target,
      "cerebro_app",
      "private-data",
      root_inputs
    ))))
    expect_false(file.exists(file.path(target, "viewer-auth.env")))
    expect_true(file.exists(file.path(
      target,
      "cerebro_app",
      "viewer-auth.env"
    )))
    expect_true(isTRUE(builder_auth_verify_database_pair(
      file.path(
        target,
        "cerebro_app",
        "private-data",
        "auth",
        "credentials.sqlite"
      ),
      file.path(target, "cerebro_app", "viewer-auth.env")
    )))
    expect_false(dir.exists(stage))
  })
})

test_that("login publication rejects an environment changed after parent verification", {
  local({
    skip_on_os("windows")
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    env_file <- file.path(
      fixture$handle$stage,
      "cerebro_app",
      "viewer-auth.env"
    )
    expect_error(
      builder_coordinator_publish(
        fixture$handle,
        fixture$result,
        .write_report = function(stage, report) {
          path <- builder_write_build_report(stage, report)
          Sys.chmod(env_file, mode = "0644")
          path
        }
      ),
      "authentication secret file is not private"
    )
    expect_true(dir.exists(fixture$handle$stage))
    expect_false(dir.exists(fixture$handle$target))
  })
})

test_that("publication rejects a mapped verification path that is absent", {
  local({
    builder_task9_source()
    fixture <- builder_app_coordinator_fixture(
      .local_envir = environment(),
      coordinator_prepare = builder_coordinator_prepare,
      bundle_request = builder_app_bundle_request,
      verify_app = builder_verify_app
    )
    fixture$result$verifications[[1L]]$path <- file.path(
      fixture$root,
      "missing-verification.crb"
    )
    expect_error(
      builder_coordinator_publish(fixture$handle, fixture$result),
      "Publication verification failed"
    )
  })
})

test_that("login publication detects same-content inode and hard-link env replacement", {
  local({
    builder_task9_source()
    run_case <- function(
      name,
      mutate,
      expected = "staged App changed after parent verification"
    ) {
      fixture <- builder_app_coordinator_fixture(
        .local_envir = environment(),
        coordinator_prepare = builder_coordinator_prepare,
        bundle_request = builder_app_bundle_request,
        verify_app = builder_verify_app
      )
      env_file <- file.path(
        fixture$handle$stage,
        "cerebro_app",
        "viewer-auth.env"
      )
      expect_error(
        builder_coordinator_publish(
          fixture$handle,
          fixture$result,
          .write_report = function(stage, report) {
            path <- builder_write_build_report(stage, report)
            mutate(env_file, stage)
            path
          }
        ),
        expected,
        info = name
      )
    }
    run_case("inode", function(env_file, stage) {
      value <- readLines(env_file, warn = FALSE)
      unlink(env_file)
      writeLines(value, env_file, useBytes = TRUE)
      Sys.chmod(env_file, mode = "0600")
    })
    probe <- withr::local_tempfile()
    writeLines("probe", probe)
    skip_if_not(
      isTRUE(file.link(probe, paste0(probe, "-link"))),
      "Hard links are unavailable"
    )
    run_case(
      "hard-link",
      function(env_file, stage) {
        source <- file.path(stage, "same-content-env")
        copied <- file.copy(env_file, source)
        stopifnot(isTRUE(copied))
        unlink(env_file)
        stopifnot(isTRUE(file.link(source, env_file)))
      },
      "staged App changed after parent verification"
    )
  })
})

test_that("login publisher guard rejects env changes in both rename windows", {
  local({
    builder_task9_source()
    run_case <- function(window, mutate) {
      fixture <- builder_app_coordinator_fixture(
        .local_envir = environment(),
        coordinator_prepare = builder_coordinator_prepare,
        bundle_request = builder_app_bundle_request,
        verify_app = builder_verify_app
      )
      publish <- function(handle, .verify_payload, .digest_cache = NULL) {
        builder_publish_release(
          handle,
          .verify_payload = .verify_payload,
          .digest_cache = .digest_cache,
          .after_phase = function(phase) {
            if (identical(window, "old_moved") && identical(phase, window)) {
              mutate(
                file.path(handle$stage, "cerebro_app", "viewer-auth.env"),
                handle$stage
              )
            }
          },
          .after_move = function(move) {
            if (identical(window, "new_to_target") && identical(move, window)) {
              mutate(
                file.path(handle$target, "cerebro_app", "viewer-auth.env"),
                handle$target
              )
            }
          }
        )
      }
      expect_error(
        builder_coordinator_publish(
          fixture$handle,
          fixture$result,
          .publish = publish
        ),
        "Publication verification failed"
      )
      expect_true(dir.exists(fixture$handle$stage))
      expect_false(dir.exists(fixture$handle$target))
    }
    replace_env <- function(env_file, root) {
      value <- readLines(env_file, warn = FALSE)
      unlink(env_file)
      writeLines(value, env_file, useBytes = TRUE)
      Sys.chmod(env_file, mode = "0600")
    }
    run_case("old_moved", replace_env)
    run_case("new_to_target", replace_env)
  })
})

test_that("background release preparation transfers ownership and retains runtime checks", {
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Test has no data worker.")
  }
  target <- withr::local_tempdir()
  shiny::testServer(app_env$server, {
    plan <- builder_crb_coordinator_plan(target, "dataset.crb")
    process <- builder_start_release_preparation_process(
      plan,
      "background-preparation"
    )
    expect_false(identical(process$get_pid(), Sys.getpid()))
    process$wait(60000)
    expect_false(process$is_alive())
    prepared <- process$get_result()
    on.exit(app_env$builder_coordinator_abort(prepared$coordinator), add = TRUE)
    expect_identical(prepared$coordinator$pid, as.integer(Sys.getpid()))
    expect_true(dir.exists(prepared$coordinator$stage))
    expect_identical(
      prepared$runtime$identity,
      app_env$builder_runtime_file_identity(prepared$runtime$files)
    )
    expect_true(any(grepl("viewer", prepared$runtime$files, fixed = TRUE)))
  })
})
