## Slow Builder integration tests (app sourcing, worker processes): CI only.
skip_on_cran()

builder_app_bundle_path <- builder_profile_inst_path("builder", "app_bundle.R")
builder_app_bundle_test_env <- new.env(parent = asNamespace("CerebroNexus"))
builder_app_bundle_test_runtime <- function() {
  runtime <- new.env(parent = asNamespace("CerebroNexus"))
  withr::with_dir(
    dirname(builder_app_bundle_path),
    sys.source(builder_app_bundle_path, envir = runtime)
  )
  runtime
}
if (file.exists(builder_app_bundle_path)) {
  withr::with_dir(
    dirname(builder_app_bundle_path),
    sys.source(builder_app_bundle_path, envir = builder_app_bundle_test_env)
  )
  list2env(
    as.list(builder_app_bundle_test_env, all.names = TRUE),
    envir = environment()
  )
}

test_that("trusted App templates prefer the active source tree", {
  source_root <- withr::local_tempdir()
  file.copy(builder_test_description_path(), source_root)
  dir.create(file.path(source_root, "R"))
  file.create(file.path(source_root, "R", "source.R"))
  template <- file.path(
    source_root,
    "inst",
    "viewer",
    "_bundle_app.R"
  )
  dir.create(dirname(template), recursive = TRUE)
  writeLines("source template", template)
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = source_root)

  expect_identical(
    .builder_app_package_path("viewer", "_bundle_app.R"),
    normalizePath(template, winslash = "/", mustWork = TRUE)
  )
})

test_that("trusted templates may be hard-linked in the package library", {
  source_root <- withr::local_tempdir()
  file.copy(builder_test_description_path(), source_root)
  dir.create(file.path(source_root, "R"))
  file.create(file.path(source_root, "R", "source.R"))
  viewer <- file.path(source_root, "inst", "viewer")
  extdata <- file.path(source_root, "inst", "extdata")
  dir.create(viewer, recursive = TRUE)
  dir.create(extdata)
  templates <- c(
    file.path(viewer, "_bundle_app.R"),
    file.path(viewer, "ui.R"),
    file.path(extdata, "colors.txt")
  )
  for (path in templates) {
    writeLines(basename(path), path)
  }
  # Second names for every library file, as conda, a Nix store optimise or
  # cp -al leave them.
  aliases <- withr::local_tempdir()
  linked <- file.link(templates, file.path(aliases, seq_along(templates)))
  skip_if_not(all(linked), "hard links are unavailable")
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = source_root)

  app_dir <- file.path(withr::local_tempdir(), "cerebro_app")
  dir.create(file.path(app_dir, "extdata"), recursive = TRUE)
  expect_true(file.copy(viewer, app_dir, recursive = TRUE))
  expect_true(file.copy(templates[[3L]], file.path(app_dir, "extdata")))
  expect_true(file.copy(templates[[1L]], file.path(app_dir, "app.R")))

  expect_no_error(.builder_app_assert_trusted_templates(
    .builder_app_tree_identity(app_dir)
  ))

  # A staged file with a second name is still refused.
  expect_true(file.link(
    file.path(app_dir, "app.R"),
    file.path(aliases, "staged")
  ))
  expect_error(.builder_app_tree_identity(app_dir), "hard link")
})

builder_app_bundle_fixture <- function(
  stage = withr::local_tempdir(.local_envir = parent.frame()),
  initial_dataset = "dataset-b",
  initial_dataset_mode = "explicit"
) {
  paths <- file.path(stage, c("dataset-a.crb", "dataset-b.crb"))
  lapply(paths, function(path) saveRDS(list(valid = TRUE), path))
  labels <- c("Dataset A", "Dataset B")
  ids <- c("dataset-a", "dataset-b")
  names(paths) <- ids
  plan <- structure(
    list(
      app_contract_version = 1L,
      dataset_order = ids,
      make_app = TRUE,
      items = list(
        list(
          id = "dataset-a",
          name = labels[[1L]],
          filename = basename(paths[[1L]]),
          colors = list(cluster = c(A = "#000000")),
          default_projection = "umap",
          default_trajectory = NULL,
          overview_point_size = 4,
          overview_point_opacity = 0.2,
          expression_backend = "embedded",
          sidecars = character()
        ),
        list(
          id = "dataset-b",
          name = labels[[2L]],
          filename = basename(paths[[2L]]),
          colors = list(cluster = c(B = "#ffffff")),
          default_projection = "pca",
          default_trajectory = list(method = "monocle2", name = "lineage"),
          overview_point_size = 7,
          overview_point_opacity = 0.8,
          expression_backend = "embedded",
          sidecars = character()
        )
      ),
      app_options = list(
        enabled = TRUE,
        show_upload_ui = FALSE,
        initial_dataset = initial_dataset,
        initial_dataset_mode = initial_dataset_mode,
        initial_page = "projection",
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
        enabled = FALSE,
        account_count = 0L,
        timeout_minutes = 15L
      )
    ),
    class = c("builder_build_plan", "list")
  )
  list(stage = stage, paths = paths, ids = ids, labels = labels, plan = plan)
}

test_that("spatial molecule identity capture does not hide CRB read failures", {
  withr::local_options(list(warn = 1))
  runtime <- builder_app_bundle_test_runtime()
  runtime$.builder_app_runtime_function <- function(name) {
    function(...) stop("payload read failed", call. = FALSE)
  }

  expect_error(
    runtime$.builder_app_capture_spatial_molecule_identity("dataset.crb"),
    "payload read failed",
    fixed = TRUE
  )
})

test_that("App request carries only the fixed safe login summary", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )

  expect_identical(
    request$auth,
    list(
      enabled = TRUE,
      account_count = 2L,
      timeout_minutes = 15L,
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE"
    )
  )
  expect_false("accounts" %in% names(request))
  expect_false("passphrase" %in% names(request$auth))
  expect_false(builder_auth_value_contains(request, "auth-user-a-7f31"))
  expect_false(builder_auth_value_contains(request, "auth-password-a-7f31"))
})

test_that("App assembly reads authentication material without passing a secret", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  source_dir <- file.path(fixture$stage, ".builder-auth-source")
  dir.create(source_dir, mode = "0700")
  Sys.chmod(source_dir, "0700", use_umask = FALSE)
  credentials <- file.path(source_dir, "credentials.sqlite")
  writeBin(as.raw(1:8), credentials)
  Sys.chmod(credentials, "0600", use_umask = FALSE)
  env_file <- .builder_auth_write_env(
    file.path(fixture$stage, "viewer-auth.env"),
    strrep("d", 64L)
  )
  material <- list(
    source_dir = normalizePath(source_dir, winslash = "/"),
    credentials = normalizePath(credentials, winslash = "/"),
    env_file = env_file,
    descriptor = list(
      credentials = normalizePath(credentials, winslash = "/"),
      passphrase_env = "CEREBRO_AUTH_PASSPHRASE",
      timeout_minutes = 15L
    )
  )
  observed <- NULL
  create_app <- function(...) {
    observed <<- list(...)
    dir.create(observed$result_dir)
  }

  builder_build_app(
    request,
    fixture$stage,
    create_app,
    auth_material = material
  )

  expect_identical(observed$auth, material$descriptor)
  expect_identical(
    Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_),
    NA_character_
  )
  expect_false(builder_auth_value_contains(observed, strrep("d", 64L)))
})

test_that("App request rejects altered login summary fields", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  mutations <- list(
    enabled = FALSE,
    account_count = 0L,
    timeout_minutes = 30L,
    passphrase_env = "NOT_CEREBRO_AUTH_PASSPHRASE",
    extra_field = "unexpected"
  )

  for (field in names(mutations)) {
    altered <- request
    if (identical(field, "extra_field")) {
      altered$auth[[field]] <- mutations[[field]]
    } else {
      altered$auth[[field]] <- mutations[[field]]
    }
    expect_error(
      .builder_app_validate_request(altered),
      "generated-App request contract is invalid",
      info = field
    )
  }
})

builder_app_backend_fixture <- function(
  mode = "h5",
  stage = withr::local_tempdir(.local_envir = parent.frame())
) {
  fixture <- builder_app_bundle_fixture(stage = stage)
  location <- "dataset-a.h5"
  fixture$plan$items[[1L]]$expression_backend <- mode
  fixture$plan$items[[1L]]$sidecars <- location
  sidecar <- file.path(fixture$stage, location)
  writeBin(as.raw(1:32), sidecar)
  fixture$sidecar <- sidecar
  fixture
}

builder_fake_app <- function(request, app_dir) {
  dir.create(app_dir)
  expect_true(file.copy(
    builder_profile_inst_path("viewer"),
    app_dir,
    recursive = TRUE
  ))
  extdata_source <- builder_profile_inst_path("extdata")
  extdata_target <- file.path(app_dir, "extdata")
  dir.create(extdata_target)
  extdata_files <- list.files(extdata_source, full.names = TRUE)
  extdata_files <- extdata_files[file.info(extdata_files)$isdir %in% FALSE]
  expect_true(all(file.copy(extdata_files, extdata_target)))
  dir.create(file.path(app_dir, "private-data"))
  targets <- file.path("private-data", basename(request$cerebro_data))
  for (index in seq_along(targets)) {
    file.copy(
      request$cerebro_data[[index]],
      file.path(app_dir, targets[[index]])
    )
  }
  bundled_spatial_images <- list()
  for (dataset in names(request$spatial_images)) {
    for (section in names(request$spatial_images[[dataset]])) {
      for (label in names(request$spatial_images[[dataset]][[section]])) {
        descriptor <- request$spatial_images[[dataset]][[section]][[label]]
        target <- .builder_app_spatial_target(
          dataset,
          section,
          label,
          descriptor$path
        )
        dir.create(dirname(file.path(app_dir, target)), recursive = TRUE)
        file.copy(descriptor$path, file.path(app_dir, target))
        descriptor$path <- target
        bundled_spatial_images[[dataset]][[section]][[label]] <- descriptor
      }
    }
  }
  expect_true(file.copy(
    builder_profile_inst_path("viewer", "_bundle_app.R"),
    file.path(app_dir, "app.R")
  ))
  saveRDS(
    c(
      list(
        crb_file_to_load = stats::setNames(targets, request$selector_order),
        dataset_labels = request$dataset_labels,
        initial_dataset = request$initial_dataset,
        initial_page = request$initial_page,
        show_upload_ui = request$show_upload_ui,
        welcome_message = request$welcome_message,
        viewer_content = request$viewer_content,
        variable_to_compare = request$variable_to_compare,
        .bundle_run_options = list(
          schema_version = 1L,
          max_request_size_bytes = .builder_app_request_size_bytes(request),
          shiny_app_options = list(
            port = as.integer(request$port),
            host = request$host,
            launch.browser = request$launch_browser,
            quiet = TRUE,
            display.mode = request$display_mode
          )
        ),
        colors = request$colors,
        spatial_images = if (length(bundled_spatial_images)) {
          bundled_spatial_images
        } else {
          NULL
        },
        spatial_image_settings = if (length(request$spatial_image_settings)) {
          .builder_app_spatial_settings(request)
        } else {
          NULL
        },
        crb_pick_smallest_file = request$crb_pick_smallest_file,
        .bundle_backend_plan = request$backend_plan
      ),
      .builder_app_scatter_options(request)
    ),
    file.path(app_dir, "cerebro_config.rds")
  )
  app_dir
}

test_that("Builder spatial targets match the package canonical target", {
  cases <- list(
    c("Dataset A", "FOV/one", "H&E", "histology.png"),
    c(strrep("dataset ", 20L), "unsafe/section", "label one", "same.png")
  )
  for (case in cases) {
    expect_identical(
      .builder_app_spatial_target(
        case[[1L]],
        case[[2L]],
        case[[3L]],
        case[[4L]]
      ),
      CerebroNexus:::.spatialImageBundleTarget(
        case[[1L]],
        case[[2L]],
        case[[3L]],
        case[[4L]]
      )
    )
  }
  first <- .builder_app_spatial_target("Dataset", "FOV", "H&E", "same.png")
  second <- .builder_app_spatial_target("Dataset", "FOV", "DAPI", "same.png")
  expect_false(identical(first, second))
  expect_match(first, "^spatial-assets/u[0-9a-f]{32}\\.png$")
  expect_lte(nchar(first, type = "bytes"), 64L)
  expect_true(.builder_app_safe_relative(first))
  expect_true(.builder_app_safe_relative(second))
})

builder_app_external_fixture <- function(
  stage = withr::local_tempdir(.local_envir = parent.frame())
) {
  fixture <- builder_app_bundle_fixture(stage = stage)
  image_dir <- file.path(
    fixture$stage,
    ".builder-spatial-assets",
    "dataset-a",
    "section-a"
  )
  dir.create(image_dir, recursive = TRUE)
  image <- file.path(image_dir, "histology.png")
  writeBin(as.raw(1:32), image)
  fixture$plan$items[[1L]]$spatial_image_storage <- "external"
  fixture$plan$items[[1L]]$external_images <- list(
    `section-a` = list(
      `H&E` = list(
        path = normalizePath(image, winslash = "/"),
        bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 8)
      )
    )
  )
  fixture$plan$items[[1L]]$external_image_settings <- list(
    `section-a` = list(
      `H&E` = list(
        flip_x = FALSE,
        flip_y = TRUE,
        scale_x = 1,
        scale_y = 1,
        offset_x = 2,
        offset_y = -1,
        rotation = 90,
        image_opacity = 0.8,
        point_opacity = 0.65,
        point_size = 6
      )
    )
  )
  fixture$image <- image
  fixture
}

builder_flip_file_byte <- function(path) {
  size <- file.info(path)$size
  bytes <- readBin(path, "raw", n = size)
  index <- max(1L, min(length(bytes), 16L))
  bytes[[index]] <- as.raw(bitwXor(as.integer(bytes[[index]]), 1L))
  writeBin(bytes, path)
}

test_that("App arguments come only from the frozen plan", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$items[[2L]]$spatial_point_appearance <- list(
    "fov-a" = list(point_opacity = 0.42, point_size = 8)
  )
  fixture$plan$items[[2L]]$spatial_roi_settings <- list(
    "fov-a" = list(
      lesion = list(
        rotation_degrees = 37.5,
        point_opacity = 0.65,
        point_size = 7
      )
    )
  )
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )

  expect_identical(names(request$cerebro_data), request$selector_order)
  expect_identical(request$selector_order, fixture$ids)
  expect_identical(
    request$dataset_labels,
    stats::setNames(fixture$labels, fixture$ids)
  )
  expect_identical(request$initial_dataset, "dataset-b")
  expect_identical(request$initial_dataset_mode, "explicit")
  expect_false(request$show_upload_ui)
  expect_identical(request$contract_version, 1L)
  expect_identical(names(request$colors), fixture$ids)
  expect_identical(names(request$viewer_content), fixture$ids)
  expect_identical(
    request$viewer_content[["dataset-b"]],
    list(
      default_projection = "pca",
      initial_projections = "pca",
      default_trajectory = list(method = "monocle2", name = "lineage"),
      overview_point_size = 7,
      overview_point_opacity = 0.8,
      overview_percentage_cells_to_show = 100,
      spatial_percentage_cells_to_show = 100,
      spatial_point_appearance = fixture$plan$items[[
        2L
      ]]$spatial_point_appearance,
      spatial_roi_settings = fixture$plan$items[[2L]]$spatial_roi_settings,
      display = builderViewerDisplayDefaults(),
      gene_expression = list(
        palette = "cerebro_orange",
        panel_mode = "shared",
        panel_palette = "default"
      ),
      extra_material_table_index = list(),
      immune_repertoire_tabs = viewerImmuneRepertoireTabs()$id
    )
  )
  expect_false(request$crb_pick_smallest_file)
  expect_identical(
    names(request),
    c(
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
      "viewer_pack_identities",
      "immune_sidecar_identities",
      "content_identities",
      "spatial_images",
      "spatial_image_settings",
      "spatial_image_identities"
    )
  )
  expect_identical(names(request$crb_identities), fixture$ids)
  expect_true(all(vapply(
    request$crb_identities,
    function(identity) {
      identical(identity$path, request$cerebro_data[[identity$label]]) &&
        is.character(identity$md5) &&
        grepl("^[[:xdigit:]]{32}$", identity$md5)
    },
    logical(1)
  )))
})

test_that("App requests freeze spatial molecule sidecar identities", {
  fixture <- builder_app_bundle_fixture()
  crb <- fixture$paths[[1L]]
  sidecar <- file.path(fixture$stage, "dataset-a.spatial")
  dir.create(sidecar)
  molecule_file <- file.path(sidecar, "0001.rds")
  molecules <- data.frame(x = 1, y = 2, gene = "A")
  saveRDS(molecules, molecule_file)
  object <- Cerebro$new()
  object$spatial <- list(
    fov1 = list(
      coordinates = data.frame(x = 0, y = 0),
      expression = matrix(numeric(), nrow = 0L, ncol = 1L),
      molecules = structure(
        list(
          file = basename(molecule_file),
          md5 = unname(tools::md5sum(molecule_file))
        ),
        class = "CerebroSpatialMoleculeRef"
      )
    )
  )
  object$spatial_molecule_backend <- list(
    type = "directory",
    location = basename(sidecar)
  )
  saveRDS(object, crb)

  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  relative_crb <- "private-data/dataset-a.crb"
  identity <- request$spatial_molecule_identities[[relative_crb]]
  content <- request$content_identities[[relative_crb]]$spatial_molecules

  expect_identical(identity$type, "directory")
  expect_identical(basename(identity$root), "dataset-a.spatial")
  expect_true("dataset-a.spatial/0001.rds" %in% names(identity$entries))
  expect_identical(content$type, "directory")
  expect_identical(content$root, "private-data/dataset-a.spatial")
  expect_true(
    "private-data/dataset-a.spatial/0001.rds" %in% names(content$entries)
  )

  trusted <- .builder_app_capture_spatial_molecule_identity(
    crb,
    .verified_backend = list(
      type = "directory",
      location = basename(sidecar)
    ),
    .backend_is_verified = TRUE
  )
  expect_identical(trusted$root, identity$root)
  expect_identical(trusted$entries, identity$entries)

  runtime <- builder_app_bundle_test_runtime()
  runtime$.builder_app_runtime_function <- function(name) {
    function(...) stop("CRB must not be reopened", call. = FALSE)
  }
  expect_identical(
    runtime$.builder_app_capture_spatial_molecule_identities(request),
    request$spatial_molecule_identities
  )
})

test_that("spatial molecule identities reject stale absolute roots", {
  fixture <- builder_app_bundle_fixture()
  crb <- fixture$paths[[1L]]
  sidecar <- file.path(fixture$stage, "dataset-a.spatial")
  stale <- file.path(fixture$stage, "stale.spatial")
  dir.create(sidecar)
  dir.create(stale)
  saveRDS(data.frame(x = 1, y = 2), file.path(sidecar, "0001.rds"))
  object <- Cerebro$new()
  object$spatial_molecule_backend <- list(
    type = "directory",
    location = basename(sidecar),
    root = normalizePath(stale, winslash = "/", mustWork = TRUE)
  )
  saveRDS(object, crb)

  expect_error(
    .builder_app_capture_spatial_molecule_identity(crb),
    "invalid location",
    fixed = TRUE
  )
})

test_that("App assembly rejects backend closure drift before dispatch", {
  fixture <- builder_app_backend_fixture("h5")
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  builder_flip_file_byte(fixture$sidecar)
  called <- FALSE

  expect_error(
    builder_build_app(
      request,
      fixture$stage,
      create_app = function(...) called <<- TRUE
    ),
    "backend closure"
  )
  expect_false(called)
})

test_that("stable identities reuse digests across checkpoints", {
  root <- withr::local_tempdir()
  first <- file.path(root, "a.bin")
  second <- file.path(root, "b.bin")
  writeBin(as.raw(1:4), first)
  writeBin(as.raw(5:8), second)
  calls <- 0L
  digest <- function(path) {
    calls <<- calls + 1L
    tools::md5sum(path)
  }

  first_identity <- .builder_app_capture_file_identity(
    first,
    .digest_file = digest
  )
  expect_identical(calls, 1L)
  .builder_app_capture_file_identity(
    first,
    .digest_file = digest,
    .previous = first_identity
  )
  expect_identical(calls, 1L)

  calls <- 0L
  tree_identity <- .builder_app_tree_identity(root, .digest_file = digest)
  expect_identical(calls, 2L)
  .builder_app_tree_identity(
    root,
    .digest_file = digest,
    .previous = tree_identity
  )
  expect_identical(calls, 2L)
})

test_that("App requests reuse identities captured while sidecars were staged", {
  for (mode in "h5") {
    fixture <- builder_app_backend_fixture(mode)
    first <- builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    )
    relative_crb <- "private-data/dataset-a.crb"
    files <- Filter(
      function(entry) identical(entry$type, "file"),
      first$backend_identities[[relative_crb]]$entries
    )
    previous <- stats::setNames(
      lapply(first$crb_identities, function(identity) {
        stats::setNames(list(identity), basename(identity$path))
      }),
      fixture$ids
    )
    previous[[1L]] <- c(
      previous[[1L]],
      stats::setNames(lapply(files, `[[`, "identity"), names(files))
    )
    uncached <- character()
    runtime <- builder_app_bundle_test_runtime()
    capture <- runtime$.builder_app_capture_file_identity
    runtime$.builder_app_capture_file_identity <- function(
      path,
      label = NULL,
      .digest_file = tools::md5sum,
      .previous = NULL
    ) {
      if (is.null(.previous)) {
        uncached <<- c(uncached, path)
      }
      capture(path, label, .digest_file, .previous)
    }
    second <- runtime$builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels,
      previous_file_identities = previous
    )

    expect_length(uncached, 0L)
    expect_identical(
      second$backend_identities,
      first$backend_identities,
      info = mode
    )
  }
})

test_that("App assembly rejects CRBs changed before or during create_app", {
  before <- builder_app_bundle_fixture()
  before_request <- builder_app_bundle_request(
    before$plan,
    before$paths,
    before$labels
  )
  old_time <- file.info(before$paths[[1L]])$mtime
  builder_flip_file_byte(before$paths[[1L]])
  Sys.setFileTime(before$paths[[1L]], old_time)
  called <- FALSE

  expect_error(
    builder_build_app(
      before_request,
      before$stage,
      create_app = function(...) {
        called <<- TRUE
      }
    ),
    "changed"
  )
  expect_false(called)

  during <- builder_app_bundle_fixture()
  during_request <- builder_app_bundle_request(
    during$plan,
    during$paths,
    during$labels
  )
  create_app <- function(...) {
    arguments <- list(...)
    builder_fake_app(during_request, arguments$result_dir)
    old_time <- file.info(during$paths[[1L]])$mtime
    builder_flip_file_byte(during$paths[[1L]])
    Sys.setFileTime(during$paths[[1L]], old_time)
  }

  expect_error(
    builder_build_app(during_request, during$stage, create_app),
    "changed"
  )
})

test_that("App identities reject files with external hard-link aliases", {
  fixture <- builder_app_bundle_fixture()
  alias <- tempfile(fileext = ".crb")
  linked <- file.link(fixture$paths[[1L]], alias)
  withr::defer(unlink(alias))
  skip_if_not(linked, "hard links are unavailable")

  expect_error(
    builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    ),
    "hard link"
  )

  app_fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    app_fixture$plan,
    app_fixture$paths,
    app_fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(app_fixture$stage, "cerebro_app")
  )
  public <- file.path(app_dir, "viewer", "www")
  dir.create(public, recursive = TRUE, showWarnings = FALSE)
  expect_true(file.link(
    file.path(app_dir, "private-data", "dataset-a.crb"),
    file.path(public, "leak.bin")
  ))

  expect_error(builder_verify_app(app_dir, request), "hard link")
})

test_that("Builder and verifier share an exact request-v1 schema", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  wrong_upload <- request
  wrong_upload$show_upload_ui <- 0L
  wrong_identity <- request
  wrong_identity$crb_identities[[1L]]$md5 <- 1L
  wrong_backend <- request
  wrong_backend$backend_plan$entries[[1L]]$unknown <- TRUE
  invalid_colors <- request
  invalid_colors$colors[[1L]]$cluster <- c(A = "not-a-color")
  outside <- tempfile(fileext = ".crb")
  saveRDS(list(valid = TRUE), outside)
  withr::defer(unlink(outside))
  outside_data <- request
  outside_data$cerebro_data[[1L]] <- normalizePath(
    outside,
    winslash = "/",
    mustWork = TRUE
  )
  outside_data$crb_identities[[1L]] <- .builder_app_capture_file_identity(
    outside_data$cerebro_data[[1L]],
    outside_data$selector_order[[1L]]
  )
  invalid <- list(
    within(request, contract_version <- 2L),
    wrong_upload,
    wrong_identity,
    wrong_backend,
    invalid_colors,
    outside_data,
    structure(
      c(unclass(request), list(unknown = TRUE)),
      class = class(request)
    ),
    structure(
      unclass(request)[rev(names(request))],
      class = class(request)
    ),
    structure(
      unclass(request)[setdiff(names(request), "crb_identities")],
      class = class(request)
    )
  )

  for (value in invalid) {
    expect_error(.builder_app_validate_request(value), "request contract")
    expect_error(builder_verify_app(app_dir, value), "request contract")
    expect_error(
      builder_build_app(value, fixture$stage),
      "request contract"
    )
  }
})

test_that("App requests reject non-frozen and mismatched build evidence", {
  fixture <- builder_app_bundle_fixture()
  invalid <- list(
    unclass(fixture$plan),
    within(fixture$plan, app_contract_version <- 0L),
    within(fixture$plan, dataset_order <- rev(dataset_order))
  )
  for (plan in invalid) {
    expect_error(
      builder_app_bundle_request(plan, fixture$paths, fixture$labels)
    )
  }

  expect_error(builder_app_bundle_request(
    fixture$plan,
    unname(fixture$paths),
    fixture$labels
  ))
  expect_error(builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    rev(fixture$labels)
  ))
  expect_error(builder_app_bundle_request(
    fixture$plan,
    fixture$paths[c(1L, 1L)],
    fixture$labels
  ))
  outside <- fixture$paths
  outside[[2L]] <- tempfile(fileext = ".crb")
  saveRDS(list(valid = TRUE), outside[[2L]])
  withr::defer(unlink(outside[[2L]]))
  expect_error(
    builder_app_bundle_request(
      fixture$plan,
      outside,
      fixture$labels
    ),
    "stage"
  )

  referenced <- fixture$plan
  attr(referenced$app_options, "mutable") <- new.env(parent = emptyenv())
  expect_error(
    builder_app_bundle_request(
      referenced,
      fixture$paths,
      fixture$labels
    ),
    "reference"
  )
})

test_that("App frozen values reject executable and unbounded structures", {
  connection <- file(tempfile(), open = "w")
  withr::defer(close(connection))
  unsafe <- list(
    function() system("id"),
    quote(system("id")),
    as.name("callable"),
    new.env(parent = emptyenv()),
    connection,
    Reduce(function(value, index) list(value), seq_len(70L), init = TRUE)
  )

  expect_true(all(vapply(unsafe, .builder_app_has_reference, logical(1))))
  expect_true(.builder_app_has_reference(rep(
    list(rep(TRUE, 1000L)),
    101L
  )))
  wide_bytes <- vapply(
    seq_len(20L),
    function(index) paste0(strrep("x", 1024^2), index),
    character(1)
  )
  expect_true(.builder_app_has_reference(wide_bytes))
  expect_false(.builder_app_has_reference(list(
    schema_version = 1L,
    labels = c("A", "B"),
    enabled = FALSE
  )))
})

test_that("App requests accept typed inert leaves from production plans", {
  fixture <- builder_app_bundle_fixture()
  claim <- structure(
    list(source = "/source/a", target = "a", artifact = "crb"),
    class = c("builder_asset_claim", "list")
  )
  manifest_entry <- structure(
    list(id = "expression"),
    class = c("builder_manifest_entry", "list")
  )
  fixture$plan$private_asset_claims <- list(claim)
  fixture$plan$manifest <- structure(
    list(expression = manifest_entry),
    class = c("builder_content_manifest", "list")
  )
  fixture$plan$created_at <- as.POSIXct(
    "2026-08-05 12:00:00",
    tz = "UTC"
  )

  expect_s3_class(
    builder_app_bundle_request(
      fixture$plan,
      fixture$paths,
      fixture$labels
    ),
    "builder_app_bundle_request"
  )
})

test_that("automatic and explicit initial selections stay distinct", {
  automatic <- builder_app_bundle_fixture(
    initial_dataset = "dataset-a",
    initial_dataset_mode = "automatic"
  )
  explicit <- builder_app_bundle_fixture(
    initial_dataset = "dataset-a",
    initial_dataset_mode = "explicit"
  )

  expect_identical(
    builder_app_bundle_request(
      automatic$plan,
      automatic$paths,
      automatic$labels
    )$initial_dataset_mode,
    "automatic"
  )
  expect_identical(
    builder_app_bundle_request(
      explicit$plan,
      explicit$paths,
      explicit$labels
    )$initial_dataset_mode,
    "explicit"
  )

  forged <- builder_app_bundle_fixture(
    initial_dataset = "dataset-b",
    initial_dataset_mode = "automatic"
  )
  expect_error(
    builder_app_bundle_request(
      forged$plan,
      forged$paths,
      forged$labels
    ),
    "automatic"
  )
})

test_that("App assembly calls only the accepted staged interface", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  observed <- NULL
  create_app <- function(...) {
    observed <<- list(...)
    builder_fake_app(request, observed$result_dir)
  }

  app_dir <- builder_build_app(request, fixture$stage, create_app)

  expect_identical(
    app_dir,
    file.path(normalizePath(fixture$stage, winslash = "/"), "cerebro_app")
  )
  expect_identical(observed$cerebro_data, request$cerebro_data)
  expect_identical(observed$dataset_labels, request$dataset_labels)
  expect_identical(observed$result_dir, app_dir)
  expect_identical(observed$colors, request$colors)
  expect_false(observed$overwrite)
  expect_false(observed$launch_browser)
  expect_true(observed$quiet)
  expect_false(observed$verbose)
  expect_false(observed$crb_pick_smallest_file)
  expect_identical(observed$show_upload_ui, request$show_upload_ui)
  expect_identical(observed$initial_dataset, request$initial_dataset)
  expect_identical(observed$initial_page, request$initial_page)
  expect_identical(observed$welcome_message, request$welcome_message)
  expect_identical(
    observed$point_size,
    .builder_app_scatter_options(request)$point_size
  )
  expect_identical(
    observed$point_opacity,
    .builder_app_scatter_options(request)$point_opacity
  )
  expect_identical(
    observed$point_opacity,
    c(`dataset-a` = 0.2, `dataset-b` = 0.8)
  )
  expect_identical(
    observed$percentage_cells_to_show,
    .builder_app_scatter_options(request)$percentage_cells_to_show
  )
  expect_identical(observed$variable_to_compare, request$variable_to_compare)
  expect_identical(observed$host, request$host)
  expect_identical(observed$port, request$port)
  expect_identical(observed$max_request_size, request$max_request_size)
  expect_identical(observed$display_mode, request$display_mode)
  expect_identical(observed$launch_browser, request$launch_browser)
})

test_that("App request and config read-back preserve every Review option", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  verification <- builder_verify_app(app_dir, request)

  expect_true(verification$valid)
  expect_false(dir.exists(file.path(app_dir, "extdata", "examples")))
  expect_identical(request$welcome_message, "Welcome, team!")
  expect_identical(request$point_size, list(overview_projection_point_size = 6))
  expect_false(request$variable_to_compare)
  expect_identical(request$host, "127.0.0.1")
  expect_identical(request$port, 4242L)
  expect_identical(request$max_request_size, 512)
  expect_identical(request$display_mode, "showcase")
  expect_false(request$launch_browser)

  config_file <- file.path(app_dir, "cerebro_config.rds")
  config <- readRDS(config_file)
  config$welcome_message <- "drifted"
  saveRDS(config, config_file)
  expect_error(builder_verify_app(app_dir, request), "welcome message")

  config$welcome_message <- request$welcome_message
  config$initial_page <- "groups"
  saveRDS(config, config_file)
  expect_error(builder_verify_app(app_dir, request), "starting page")
})

test_that("App bundle rejects NULL rendered scalar options", {
  fixture <- builder_app_bundle_fixture()
  for (field in c("point_size", "variable_to_compare")) {
    plan <- fixture$plan
    if (identical(field, "point_size")) {
      plan$app_options$point_size$overview_projection_point_size <- NULL
    } else {
      plan$app_options$variable_to_compare <- NULL
    }
    expect_error(
      builder_app_bundle_request(plan, fixture$paths, fixture$labels),
      "options are invalid"
    )
  }
})

builder_app_bundle_save_embedded <- function(paths) {
  for (path in paths) {
    object <- Cerebro$new()
    object$setExpressionBackend(type = "embedded")
    saveRDS(object, path)
  }
}

test_that("real createShinyApp output round trips the frozen request", {
  fixture <- builder_app_bundle_fixture()
  builder_app_bundle_save_embedded(fixture$paths)
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )

  app_dir <- builder_build_app(request, fixture$stage)
  verification <- builder_verify_app(app_dir, request)

  expect_true(verification$valid)
  expect_identical(verification$selector_order, fixture$ids)
  expect_identical(verification$dataset_labels, request$dataset_labels)
  expect_identical(verification$initial_dataset, "dataset-b")
  expect_false(verification$auth_enabled)
  expect_null(verification$auth_database)
})

test_that("real login App verifies its exact database and external env pair", {
  fixture <- builder_app_bundle_fixture()
  fixture$plan$app_auth <- list(
    enabled = TRUE,
    account_count = 2L,
    timeout_minutes = 15L
  )
  builder_app_bundle_save_embedded(fixture$paths)
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  material <- builder_auth_create_material(
    accounts,
    fixture$stage,
    .capability = function() list(available = TRUE, reason = NULL)
  )

  app_dir <- builder_build_app(
    request,
    fixture$stage,
    auth_material = material
  )
  app_env <- file.path(app_dir, "viewer-auth.env")
  verification <- builder_verify_app(
    app_dir,
    request,
    auth_env_file = app_env
  )

  expect_true(verification$auth_enabled)
  expect_true(file.exists(app_env))
  if (!identical(.Platform$OS.type, "windows")) {
    expect_identical(as.integer(file.info(app_env)$mode), 384L)
  }
  expect_identical(
    verification$auth_database,
    file.path(app_dir, "private-data", "auth", "credentials.sqlite")
  )
  expect_false("auth_env_file" %in% names(verification))
  expect_false(builder_auth_value_contains(
    verification,
    builder_auth_read_env_file(app_env)
  ))
})

test_that("App verification rejects stable copied data replacements", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  builder_flip_file_byte(file.path(app_dir, "private-data", "dataset-a.crb"))

  expect_error(builder_verify_app(app_dir, request), "copied content")
})

test_that("App verification binds executable code to package templates", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  writeLines(
    c(
      "shiny::addResourcePath('leak', 'private-data')",
      "shiny::shinyApp(ui = NULL, server = function(...) NULL)"
    ),
    file.path(app_dir, "app.R")
  )

  expect_error(builder_verify_app(app_dir, request), "trusted template")
})

test_that("App verification rejects extra Shiny root entrypoints", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  writeLines(
    "shiny::addResourcePath('leak', 'private-data')",
    file.path(app_dir, "global.R")
  )

  expect_error(builder_verify_app(app_dir, request), "trusted topology")
})

test_that("App verification reads back a private inert bundle", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )

  verification <- builder_verify_app(app_dir, request)

  expect_s3_class(verification, "builder_app_verification")
  expect_true(verification$valid)
  expect_identical(verification$selector_order, fixture$ids)
  expect_identical(verification$dataset_labels, request$dataset_labels)
  expect_identical(verification$initial_dataset, "dataset-b")
  expect_identical(verification$backend_plan, request$backend_plan)
  expect_false(.builder_app_has_reference(verification))
  expect_false("initial_dataset_mode" %in% names(verification))
  expect_true("diagnostic_tree_identity" %in% names(verification))
  expect_identical(
    names(verification$diagnostic_tree_identity),
    c(
      "schema_version",
      "entry_count",
      "file_count",
      "directory_count",
      "aggregate_md5"
    )
  )
  expect_false("entries" %in% names(verification$diagnostic_tree_identity))
  expect_match(
    verification$diagnostic_tree_identity$aggregate_md5,
    "^[0-9a-f]{32}$"
  )

  dir.create(file.path(app_dir, "data"))
  expect_error(builder_verify_app(app_dir, request), "legacy")
})

test_that("App verification rejects a tree changed during one verification", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  calls <- 0L
  identity <- function(path, .previous = NULL) {
    result <- .builder_app_tree_identity(path, .previous = .previous)
    calls <<- calls + 1L
    if (calls == 1L) {
      writeLines(
        c(
          "# changed during verification",
          "shiny::shinyApp(ui = NULL, server = function(...) NULL)"
        ),
        file.path(path, "app.R")
      )
    }
    result
  }

  expect_error(
    builder_verify_app(app_dir, request, .tree_identity = identity),
    "changed during verification"
  )
})

test_that("App verification reuses stable regular-file digests", {
  fixture <- builder_app_bundle_fixture()
  lapply(fixture$paths, function(path) saveRDS(Cerebro$new(), path))
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  hashed <- character()
  identity <- function(path, .previous = NULL) {
    .builder_app_tree_identity(
      path,
      .digest_file = function(file) {
        hashed <<- c(
          hashed,
          normalizePath(
            file,
            winslash = "/",
            mustWork = TRUE
          )
        )
        tools::md5sum(file)
      },
      .previous = .previous
    )
  }

  expect_true(
    builder_verify_app(
      app_dir,
      request,
      .tree_identity = identity
    )$valid
  )
  expect_true(length(hashed) > 0L)
  expect_true(all(table(hashed) == 1L))
})

test_that("App verification requires every bundled backend sidecar", {
  fixture <- builder_app_backend_fixture("h5")
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )

  expect_error(builder_verify_app(app_dir, request), "sidecar")
  sidecar <- file.path(app_dir, "private-data", "dataset-a.h5")
  dir.create(sidecar)
  expect_error(builder_verify_app(app_dir, request), "regular file")
  unlink(sidecar, recursive = TRUE)
  expect_true(file.copy(fixture$sidecar, sidecar))
  expect_true(builder_verify_app(app_dir, request)$valid)

  writeLines("not valid R code }", file.path(app_dir, "app.R"))
  expect_error(builder_verify_app(app_dir, request), "parsed")
})

test_that("App tree verification fails closed when enumeration fails", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )

  expect_error(
    .builder_app_enumerate_tree(
      app_dir,
      .list_directory = function(path) stop("denied")
    ),
    "enumerated safely"
  )
})

test_that("regular App files must be readable even if the process is privileged", {
  path <- tempfile()
  writeBin(as.raw(1:4), path)
  withr::defer(unlink(path))

  expect_error(
    .builder_app_assert_readable_file(
      path,
      .open_file = function(...) stop("denied")
    ),
    "not readable"
  )

  skip_on_os("windows")
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  app_file <- file.path(app_dir, "app.R")
  Sys.chmod(app_file, mode = "0000")
  withr::defer(Sys.chmod(app_file, mode = "0600"))

  expect_error(builder_verify_app(app_dir, request), "not readable")
})

test_that("App config is size-bounded before deserialization", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  writeBin(
    raw(32L * 1024L * 1024L),
    file.path(app_dir, "cerebro_config.rds")
  )

  expect_error(builder_verify_app(app_dir, request), "config is too large")
})

test_that("App config rejects compressed values over the object byte budget", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  config_file <- file.path(app_dir, "cerebro_config.rds")
  config <- readRDS(config_file)
  config$wide <- rep(strrep("x", 1024^2), 20L)
  saveRDS(config, config_file, compress = "xz")
  expect_lt(file.info(config_file)$size, .builder_app_config_max_bytes)

  expect_error(builder_verify_app(app_dir, request), "inert readable")
})

test_that("App verification rejects symlinks anywhere in the bundle", {
  skip_on_os("windows")
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  public_root <- file.path(app_dir, "viewer", "www")
  dir.create(public_root, recursive = TRUE, showWarnings = FALSE)
  expect_true(file.symlink(
    file.path(app_dir, "private-data", "dataset-a.crb"),
    file.path(public_root, "leak.crb")
  ))

  expect_error(builder_verify_app(app_dir, request), "symbolic")
})

test_that("the privacy allowlist covers every bundled demo data file", {
  examples <- builder_profile_inst_path("extdata", "examples")
  files <- list.files(
    examples,
    pattern = "[.](crb|h5|hdf5|rds)$",
    full.names = TRUE
  )
  relative <- file.path("extdata", "examples", basename(files))

  ## The package tarball leaves the larger demos out, so only the files that
  ## are here are compared.
  expect_true(all(relative %in% names(.builder_app_demo_data)))
  expect_identical(
    unname(tools::md5sum(files)),
    unname(.builder_app_demo_data[relative])
  )
})

test_that("App verification rejects private data outside exact allowed roots", {
  fixture <- builder_app_bundle_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  leak <- file.path(app_dir, "viewer", "www", "leak.crb")
  dir.create(dirname(leak), recursive = TRUE, showWarnings = FALSE)
  writeBin(as.raw(1:4), leak)
  expect_error(builder_verify_app(app_dir, request), "outside private-data")

  # Case-only aliases of private paths are not private on any platform.
  for (path in c("PRIVATE-DATA/rogue.crb", "CEREBRO_CONFIG.RDS")) {
    identity <- list(
      entries = stats::setNames(
        list(list(path = path, type = "file", md5 = strrep("a", 32L))),
        path
      )
    )
    expect_error(
      .builder_app_validate_private_locations(
        identity,
        root = "/app",
        .canonical_path = function(path) path
      ),
      "outside private-data",
      info = path
    )
  }
})

test_that("external spatial assets are frozen and passed as formal arguments", {
  fixture <- builder_app_external_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  identity <- request$spatial_image_identities[["dataset-a"]][["section-a"]][[
    "H&E"
  ]]
  expect_identical(
    request$backend_plan$entries[["private-data/dataset-a.crb"]]$type,
    "embedded"
  )
  expect_identical(identity$label, "dataset-a/section-a/H&E")
  expect_identical(identity$path, normalizePath(fixture$image, winslash = "/"))

  observed <- NULL
  create_app <- function(...) {
    observed <<- list(...)
    dir.create(observed$result_dir)
  }
  builder_build_app(request, fixture$stage, create_app)
  expect_identical(observed$spatial_images, request$spatial_images)
  expect_identical(
    observed$spatial_image_settings,
    .builder_app_spatial_settings(request)
  )

  changed <- builder_app_external_fixture()
  changed_request <- builder_app_bundle_request(
    changed$plan,
    changed$paths,
    changed$labels
  )
  writeBin(as.raw(33:64), changed$image)
  expect_error(
    builder_build_app(changed_request, changed$stage, create_app),
    "spatial image changed"
  )
})

test_that("external spatial assets preserve their ROI scope", {
  fixture <- builder_app_external_fixture()
  descriptor <- fixture$plan$items[[1L]]$external_images[["section-a"]][[
    "H&E"
  ]]
  descriptor$roi_field <- "sample_roi"
  descriptor$roi_value <- "lesion"
  fixture$plan$items[[1L]]$external_images[["section-a"]][["H&E"]] <- descriptor

  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  observed <- request$spatial_images[["dataset-a"]][["section-a"]][["H&E"]]

  expect_identical(observed$roi_field, "sample_roi")
  expect_identical(observed$roi_value, "lesion")
})

test_that("external spatial request rejects duplicates and separates collisions", {
  duplicated <- builder_app_external_fixture()
  duplicated$plan$items[[1L]]$external_images[["section-a"]][["DAPI"]] <-
    duplicated$plan$items[[1L]]$external_images[["section-a"]][["H&E"]]
  duplicated$plan$items[[1L]]$external_image_settings[["section-a"]][[
    "DAPI"
  ]] <-
    duplicated$plan$items[[1L]]$external_image_settings[["section-a"]][["H&E"]]
  expect_error(
    builder_app_bundle_request(
      duplicated$plan,
      duplicated$paths,
      duplicated$labels
    ),
    "request contract"
  )

  collided <- builder_app_external_fixture()
  second_dir <- file.path(
    collided$stage,
    ".builder-spatial-assets",
    "other"
  )
  dir.create(second_dir, recursive = TRUE)
  second <- file.path(second_dir, basename(collided$image))
  writeBin(as.raw(65:96), second)
  collided$plan$items[[1L]]$external_images[["section-a"]][["DAPI"]] <-
    list(
      path = normalizePath(second, winslash = "/"),
      bounds = c(xmin = 0, xmax = 10, ymin = 0, ymax = 8)
    )
  collided$plan$items[[1L]]$external_image_settings[["section-a"]][["DAPI"]] <-
    collided$plan$items[[1L]]$external_image_settings[["section-a"]][["H&E"]]
  collided_request <- builder_app_bundle_request(
    collided$plan,
    collided$paths,
    collided$labels
  )
  collided_images <- collided_request$spatial_images[["dataset-a"]][[
    "section-a"
  ]]
  collided_targets <- vapply(
    names(collided_images),
    function(label) {
      .builder_app_spatial_target(
        "dataset-a",
        "section-a",
        label,
        collided_images[[label]]$path
      )
    },
    character(1)
  )
  expect_length(unique(collided_targets), 2L)
})

test_that("external spatial topology contains only declared relative assets", {
  fixture <- builder_app_external_fixture()
  request <- builder_app_bundle_request(
    fixture$plan,
    fixture$paths,
    fixture$labels
  )
  app_dir <- builder_fake_app(
    request,
    file.path(fixture$stage, "cerebro_app")
  )
  expect_true(builder_verify_app(app_dir, request)$valid)
  config <- readRDS(file.path(app_dir, "cerebro_config.rds"))
  bundled <- config$spatial_images[["dataset-a"]][["section-a"]][["H&E"]]$path
  expect_identical(
    bundled,
    .builder_app_spatial_target(
      "dataset-a",
      "section-a",
      "H&E",
      fixture$image
    )
  )
  expect_false(grepl(
    normalizePath(fixture$stage, winslash = "/"),
    bundled,
    fixed = TRUE
  ))

  writeBin(as.raw(1:4), file.path(app_dir, "spatial-assets", "undeclared.png"))
  expect_error(
    builder_verify_app(app_dir, request),
    "spatial-assets topology"
  )
})
