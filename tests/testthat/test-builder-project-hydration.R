builder_project_open_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_test_inst_path(
      "builder",
      "core",
      "bundle_path_contract.R"
    ),
    envir = runtime
  )
  for (file in c("io.R", "project.R", "build.R")) {
    sys.source(
      builder_test_inst_path("builder", file),
      envir = runtime
    )
  }
  runtime
}

builder_stage_contract_source_runtime(environment())

test_that("resume applies its verified configuration directly", {
  runtime <- builder_project_open_runtime()
  root <- withr::local_tempdir()
  loaded <- list(
    id = "ds1",
    source_id = "ds1",
    output_id = "ds1",
    selector_value = "ds1",
    snapshot = list(path = "/runtime/snapshot", owner_token = "owner"),
    revision = 1L,
    profile = list(),
    settings = list(
      name = "Source default",
      organism = "hg",
      assay = "RNA",
      layer = "data",
      nUMI = "nCount_RNA",
      nGene = "nFeature_RNA",
      expression_backend = "embedded",
      included_projections = "umap",
      default_projection = "umap",
      overview_point_size = 5,
      overview_percentage_cells_to_show = 100,
      analyses = character(),
      images = list()
    )
  )
  saved <- loaded
  saved$settings$name <- "Saved project name"
  saved$settings$organism <- "mm"
  saved$settings$included_projections <- c("umap", "pca")
  saved$settings$default_projection <- "pca"
  saved$settings$overview_point_size <- 8
  saved$settings$overview_percentage_cells_to_show <- 60
  saved$settings$group_color_overrides <- list(
    cluster = c(B = "#E76F51")
  )
  saved$settings$included_groups <- c("cluster", "sample")
  saved$settings$default_group <- "sample"
  saved$settings$metadata_policy <- list(
    retained = c("cluster", "sample", "batch"),
    groups = c("cluster", "sample")
  )
  saved$settings$included_trajectories <- list(
    slingshot = list(method = "slingshot", name = "lineage-2")
  )
  saved$settings$default_trajectory <- list(
    method = "slingshot",
    name = "lineage-2"
  )
  saved$settings$palette <- "okabe_ito"
  saved$settings$content_sources <- list(immune_repertoire = "vdj")
  saved$settings$analyses <- c("most_expressed", "marker_genes")
  saved$acknowledgements <- "metadata"
  record <- runtime$builder_project_dataset_record(
    saved,
    source = list(kind = "example", example = "complete_viewer_data"),
    checked = TRUE,
    root = root
  )
  status <- runtime$builder_project_dataset_status(record, root)
  expect_true("resume" %in% status$actions)
  record$runtime_restore_status <- status
  record$runtime_restore_action <- "resume"
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  materialize <- runtime$builder_project_restore_entry
  runtime$builder_project_restore_entry <- function(...) {
    calls$n <- calls$n + 1L
    materialize(...)
  }

  resumed <- runtime$builder_project_finalize_loaded_entry(loaded, record, root)

  expect_identical(calls$n, 0L)
  expect_identical(resumed$settings, saved$settings)
  expect_identical(resumed$acknowledgements, "metadata")
  expect_identical(resumed$snapshot, loaded$snapshot)
  expect_identical(resumed$revision, saved$revision)

  expect_false(exists(
    "builder_project_hydrate_pending_entries",
    envir = runtime,
    inherits = FALSE
  ))

  mismatched <- record
  mismatched$id <- "another-dataset"
  expect_error(
    runtime$builder_project_finalize_loaded_entry(loaded, mismatched, root),
    "does not match the loaded entry"
  )
})

test_that("resume rejects metadata or content drift without revalidation", {
  runtime <- builder_project_open_runtime()
  root <- withr::local_tempdir()
  saved <- list(
    id = "ds1",
    revision = 1L,
    settings = list(name = "Saved project name", images = list())
  )
  record <- runtime$builder_project_dataset_record(
    saved,
    source = list(kind = "example", example = "complete_viewer_data"),
    checked = TRUE,
    root = root
  )
  status <- runtime$builder_project_dataset_status(record, root)
  record$runtime_restore_status <- status
  record$runtime_restore_action <- "resume"
  loaded <- list(
    id = "ds1",
    revision = 1L,
    settings = list(name = "Source default", images = list())
  )
  config_path <- runtime$builder_project_resolve_path(
    record$configuration$path,
    root,
    "managed"
  )

  Sys.setFileTime(config_path, Sys.time() + 2)
  expect_false(runtime$builder_project_status_snapshot_fresh(
    status,
    record,
    root,
    needs = c("configuration", "spatial", "table")
  ))
  expect_error(
    runtime$builder_project_finalize_loaded_entry(loaded, record, root),
    "inputs changed during Project restore",
    fixed = TRUE
  )

  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )

  writeBin(
    c(
      readBin(config_path, "raw", n = file.info(config_path)$size),
      as.raw(0)
    ),
    config_path
  )
  expect_error(
    runtime$builder_project_finalize_loaded_entry(loaded, record, root),
    "inputs changed during Project restore",
    fixed = TRUE
  )
})

test_that("managed source restore requires the worker's verified source state", {
  runtime <- builder_project_open_runtime()
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "input.rds")
  writeBin(charToRaw("source"), source_path)
  fingerprint <- runtime$builder_project_file_fingerprint(
    source_path,
    content = TRUE
  )
  loaded <- list(
    id = "ds1",
    load_state = "loaded",
    revision = 1L,
    snapshot = list(
      path = "/runtime/ds1",
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "input.rds",
        fingerprint$bytes,
        fingerprint$modified_at,
        fingerprint$md5,
        sep = ":"
      )
    ),
    settings = list(name = "Source default", images = list())
  )
  record <- runtime$builder_project_dataset_record(
    loaded,
    source = list(
      kind = "managed",
      path = "sources/ds1/input.rds",
      fingerprint = fingerprint
    ),
    checked = TRUE,
    root = root
  )
  record$source$fingerprint <- NULL
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )
  expect_identical(record$runtime_restore_status$actions, c("reload", "skip"))
  record$runtime_restore_action <- "reload"

  expect_error(
    runtime$builder_project_finalize_loaded_entry(loaded, record, root),
    "did not return a verified file identity"
  )

  loaded$snapshot$source_state <- c(
    list(
      location = normalizePath(source_path, winslash = "/", mustWork = TRUE)
    ),
    runtime$builder_project_file_metadata(source_path)
  )
  finalized <- runtime$builder_project_finalize_loaded_entry(
    loaded,
    record,
    root
  )
  expect_identical(finalized$project_source_fingerprint$md5, fingerprint$md5)
  expect_identical(finalized$settings, loaded$settings)

  writeBin(charToRaw("source changed after worker read"), source_path)
  expect_error(
    runtime$builder_project_finalize_loaded_entry(loaded, record, root),
    "changed during Project restore"
  )
})

test_that("file adapter source state crosses the Project restore boundary", {
  runtime <- builder_project_open_runtime()
  sys.source(
    builder_test_inst_path("builder", "adapters.R"),
    envir = runtime
  )
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "input.rds")
  saveRDS(1:3, source_path)
  adapter <- runtime$builder_seurat_file_adapter(source_path)
  source_md5 <- unname(tools::md5sum(source_path))
  loaded <- list(
    id = "ds1",
    revision = 1L,
    snapshot = list(
      path = "/runtime/ds1",
      source_fingerprint = paste(
        "builder-snapshot-v2",
        basename(adapter$source_state$location),
        adapter$source_state$bytes,
        adapter$source_state$modified_at,
        source_md5,
        sep = ":"
      ),
      source_state = adapter$source_state
    ),
    settings = list(name = "Source default", images = list())
  )
  record <- runtime$builder_project_dataset_record(
    loaded,
    source = list(
      kind = "managed",
      path = "sources/ds1/input.rds",
      fingerprint = NULL
    ),
    checked = TRUE,
    root = root
  )
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )
  record$runtime_restore_action <- "reload"

  finalized <- runtime$builder_project_finalize_loaded_entry(
    loaded,
    record,
    root
  )

  expect_identical(
    finalized$project_source_fingerprint$md5,
    source_md5
  )
})

test_that("configuration identity is stable across Spatial asset hydration", {
  runtime <- builder_project_open_runtime()
  source_md5 <- unclass(as.character(digest::digest(
    charToRaw(
      "same-source-image"
    ),
    algo = "md5",
    serialize = FALSE
  )))
  image <- list(
    source = list(name = "H&E"),
    source_path = "C:/session/same-source-image.png",
    source_content_md5 = source_md5,
    base_bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
    bounds = list(xmin = 306, xmax = 406, ymin = -25, ymax = 55),
    dx = 306,
    dy = -25,
    scale = 1.42,
    rotation = -17,
    flip_x = TRUE,
    flip_y = TRUE,
    image_opacity = 0.55,
    point_opacity = 0.65,
    point_size = 8,
    section_id = "fov-a",
    section_kind = "spatial"
  )
  before <- list(
    id = "ds1",
    settings = list(
      images = list(`fov-a` = list(`H&E` = image))
    )
  )
  after <- before
  after$settings$images[["fov-a"]][["H&E"]]$source_path <-
    "C:/project/spatial-assets/same-source-image.png"

  expect_identical(
    runtime$builder_project_configuration_digest(before),
    runtime$builder_project_configuration_digest(after)
  )

  changed <- after
  changed$settings$images[["fov-a"]][["H&E"]]$source_content_md5 <-
    unclass(as.character(digest::digest(
      charToRaw("different-source-image"),
      algo = "md5",
      serialize = FALSE
    )))
  expect_false(identical(
    runtime$builder_project_configuration_digest(before),
    runtime$builder_project_configuration_digest(changed)
  ))
})

test_that("Spatial UI model carries every saved image control", {
  image <- list(
    source = list(name = "H&E"),
    source_uri = "data:image/png;base64,AA==",
    uri = "data:image/png;base64,AA==",
    base_bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
    bounds = list(xmin = 306, xmax = 448, ymin = -25, ymax = 89),
    dx = 306,
    dy = -25,
    scale = 1.42,
    rotation = -17,
    flip_x = TRUE,
    flip_y = TRUE,
    image_opacity = 0.55,
    point_opacity = 0.65,
    point_size = 8,
    section_id = "fov-a",
    section_kind = "spatial"
  )
  dapi <- image
  dapi$source$name <- "DAPI"
  dapi$rotation <- 23
  model <- builder_enhance_model(
    id = "ds1",
    profile = list(
      images = "fov-a",
      spatial_scenes = list(list(
        id = "fov-a",
        label = "fov-a · sample: S1 · ROI: lesion"
      )),
      extras = list()
    ),
    state = list(manifest = list()),
    settings = list(
      tables = list(),
      images = list(`fov-a` = list(`H&E` = image, DAPI = dapi)),
      spatial_coordinate_transforms = list(
        `fov-a` = list(rotation_degrees = 32.8, scale = 1)
      ),
      spatial_image_storage = "external"
    ),
    modules = list(),
    active_section = "fov-a",
    active_image = "DAPI"
  )
  controls <- model$attachments$histology$controls

  expect_identical(model$attachments$histology$active_image, "DAPI")
  expect_identical(
    model$attachments$histology$section_labels,
    c(`fov-a` = "fov-a · sample: S1 · ROI: lesion")
  )
  expect_identical(model$attachments$histology$scenes[[1L]]$id, "fov-a")
  expect_identical(controls$dx, 306)
  expect_identical(controls$dy, -25)
  expect_identical(controls$scale, 1.42)
  expect_identical(controls$rotation, 23)
  expect_true(controls$flip_x)
  expect_true(controls$flip_y)
  expect_identical(controls$image_opacity, 0.55)
  expect_identical(controls$point_opacity, 0.65)
  expect_identical(controls$point_size, 8)
  expect_lte(controls$dx_min, 306)
  expect_gte(controls$dx_max, 306)
  expect_lte(controls$dy_min, -25)
  expect_gte(controls$dy_max, -25)

  html <- paste(
    as.character(builder_spatial_alignment_ui(
      "enhance",
      model$attachments$histology
    )),
    collapse = ""
  )
  expect_match(html, 'id="enhance-img_dx"[^>]+value="306"', perl = TRUE)
  expect_match(html, 'id="enhance-img_dy"[^>]+value="-25"', perl = TRUE)
  expect_match(html, 'id="enhance-img_scale"[^>]+data-from="1.42"', perl = TRUE)
  expect_match(html, 'id="enhance-img_rotate"[^>]+data-from="23"', perl = TRUE)
  expect_match(html, 'id="enhance-image_flip_x"[^>]+checked', perl = TRUE)
  expect_match(html, 'id="enhance-image_flip_y"[^>]+checked', perl = TRUE)
})

test_that("Spatial UI model restores active ROI coordinate settings", {
  model <- builder_enhance_model(
    id = "ds1",
    profile = list(
      images = "fov-a",
      spatial_scenes = list(list(id = "fov-a", label = "fov-a")),
      extras = list()
    ),
    state = list(manifest = list()),
    settings = list(
      images = list(),
      spatial_coordinate_transforms = list(
        `fov-a` = list(rotation_degrees = 0, scale = 1)
      ),
      spatial_roi_settings = list(
        `fov-a` = list(
          lesion = list(
            rotation_degrees = -51.8,
            point_opacity = 0.85,
            point_size = 12
          )
        )
      )
    ),
    modules = list(),
    active_section = "fov-a",
    active_roi = "lesion"
  )

  histology <- model$attachments$histology
  expect_identical(histology$coordinate_rotation, -51.8)
  expect_identical(histology$controls$point_opacity, 0.85)
  expect_identical(histology$controls$point_size, 12)
})

test_that("Spatial control ranges retain saved offsets before preview bounds arrive", {
  record <- list(
    base_bounds = list(xmin = 0, xmax = 100, ymin = 0, ymax = 80),
    bounds = list(xmin = 306, xmax = 448, ymin = -25, ymax = 89),
    dx = 306,
    dy = -25
  )

  pending <- builder_alignment_control_ranges(record, bounds = NULL)
  expect_lte(pending$dx$min, 306)
  expect_gte(pending$dx$max, 306)
  expect_lte(pending$dy$min, -25)
  expect_gte(pending$dy$max, -25)
  expect_identical(pending$dx$step, 10)
  expect_identical(pending$dy$step, 10)

  ready <- builder_alignment_control_ranges(
    record,
    bounds = list(xmin = 0, xmax = 500, ymin = -100, ymax = 100)
  )
  expect_identical(ready$dx$min, -500)
  expect_identical(ready$dx$max, 500)
  expect_identical(ready$dy$min, -200)
  expect_identical(ready$dy$max, 200)
})

test_that("projects of an earlier schema are not read", {
  runtime <- builder_project_open_runtime()
  root <- withr::local_tempdir()
  path <- runtime$builder_project_manifest_path(root)
  for (version in c(1L, 2L)) {
    jsonlite::write_json(
      list(
        schema_version = version,
        project = list(id = "project-safe", revision = 2L),
        datasets = list(),
        last_ui = list(stage = "configure", selected_dataset = "ds1")
      ),
      path,
      auto_unbox = TRUE,
      null = "null"
    )
    expect_error(
      runtime$builder_project_read(path),
      "not a supported Builder project"
    )
  }
})

test_that("project preferences retain only validated non-secret values", {
  runtime <- builder_project_open_runtime()
  options <- list(
    welcome_message = "Saved welcome",
    initial_page = "overview",
    point_size = 7,
    variable_to_compare = TRUE,
    host = "0.0.0.0",
    port = 9090L,
    max_request_size = 1234,
    display_mode = "showcase",
    launch_browser = FALSE,
    show_upload_ui = TRUE,
    password = "must-not-be-saved"
  )

  saved <- runtime$builder_project_configuration(
    review_options = options,
    build_mode = TRUE,
    auth_enabled = TRUE,
    initial_dataset = "ds2"
  )

  expect_identical(saved$review_options$welcome_message, "Saved welcome")
  expect_identical(saved$review_options$initial_page, "overview")
  expect_identical(saved$review_options$port, 9090L)
  expect_true(saved$build_mode)
  expect_true(saved$auth_enabled)
  expect_identical(saved$initial_dataset, "ds2")
  expect_false(any(
    c(
      "accounts",
      "password",
      "token",
      "username",
      "account_count"
    ) %in%
      names(saved)
  ))
})

test_that("schema v3 preferences survive a manifest write and read", {
  runtime <- builder_project_open_runtime()
  root <- withr::local_tempdir()
  manifest <- runtime$builder_project_new_manifest(root, "Round trip")
  manifest$configuration <- runtime$builder_project_configuration(
    review_options = list(
      welcome_message = "Saved welcome",
      initial_page = "overview",
      point_size = 7,
      variable_to_compare = TRUE,
      host = "0.0.0.0",
      port = 9090L,
      max_request_size = 1234,
      display_mode = "showcase",
      launch_browser = FALSE,
      show_upload_ui = TRUE
    ),
    build_mode = TRUE,
    auth_enabled = TRUE,
    initial_dataset = "ds2"
  )
  written <- runtime$builder_project_write(manifest, root)
  restored <- runtime$builder_project_read(written$path)

  expect_identical(restored$schema_version, 3L)
  expect_identical(restored$configuration, written$manifest$configuration)
})

test_that("schema v3 excludes source-derived profiles from project storage", {
  runtime <- builder_project_open_runtime()
  root <- withr::local_tempdir()
  entry <- builder_minimal_entry("ds1", "Dataset one")
  entry$profile <- list(
    n_cells = 123L,
    n_genes = 45L,
    large_derived_value = rep("profile-only", 1000L)
  )
  entry$dataset_profile <- list(
    assays = list(large_derived_value = rep(1, 1000L))
  )
  entry$levels <- list(cluster = c("A", "B"))

  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = "sources/ds1/source.rds",
      fingerprint = list(md5 = "source-md5")
    ),
    checked = TRUE,
    root = root
  )

  expect_identical(record$configuration$schema_version, 1L)
  expect_true(file.exists(file.path(root, record$configuration$path)))
  expect_null(record$cache)
  expect_null(record$configuration$payload)

  status <- runtime$builder_project_dataset_status(record, root)
  restored <- runtime$builder_project_restore_entry(
    record,
    root,
    status$configuration_entry
  )
  expect_identical(restored$settings, entry$settings)
  expect_null(restored$profile$large_derived_value)
})

test_that("last UI restoration uses safe dataset and workflow fallbacks", {
  runtime <- builder_project_open_runtime()

  restored <- runtime$builder_project_last_ui_target(
    list(stage = "build", selected_dataset = "ds2"),
    available_ids = c("ds1", "ds2")
  )
  expect_identical(restored$selected_dataset, "ds2")
  expect_identical(restored$stage, "configure")

  spatial <- runtime$builder_project_last_ui_target(
    list(
      stage = "configure",
      selected_dataset = "ds2",
      spatial = list(dataset = "ds2", section = "fov-b", image = "DAPI")
    ),
    available_ids = c("ds1", "ds2")
  )
  expect_identical(
    spatial$spatial,
    list(dataset = "ds2", section = "fov-b", image = "DAPI")
  )

  fallback <- runtime$builder_project_last_ui_target(
    list(
      stage = "review",
      selected_dataset = "skipped",
      spatial = list(dataset = "skipped", section = "fov-z", image = "old")
    ),
    available_ids = "ds1"
  )
  expect_identical(fallback$selected_dataset, "ds1")
  expect_identical(fallback$stage, "configure")
  expect_null(fallback$spatial)

  empty <- runtime$builder_project_last_ui_target(
    list(stage = "review", selected_dataset = "ds1"),
    available_ids = character()
  )
  expect_null(empty$selected_dataset)
  expect_identical(empty$stage, "upload")
})

test_that("Spatial project UI restoration selects the saved FOV and image", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  record <- function(section, label) {
    app_env$builder_alignment_record(
      source = list(name = label),
      source_uri = "data:image/png;base64,AA==",
      uri = "data:image/png;base64,AA==",
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section = list(id = section, kind = "spatial")
    )
  }
  entry <- list(
    id = "ds1",
    snapshot = list(
      path = "/runtime/ds1",
      owner_token = "owner",
      object_md5 = strrep("a", 32L)
    ),
    profile = list(images = c("fov-a", "fov-b"), extras = list()),
    settings = list(
      name = "Dataset 1",
      images = list(
        `fov-a` = list(`H&E` = record("fov-a", "H&E")),
        `fov-b` = list(
          `H&E` = record("fov-b", "H&E"),
          DAPI = record("fov-b", "DAPI")
        )
      ),
      spatial_coordinate_transforms = list(),
      default_group = "cluster",
      default_projection = "umap"
    )
  )
  current <- shiny::reactiveVal("ds1")

  shiny::testServer(
    function(input, output, session) {
      alignment <- app_env$builder_spatial_alignment_server(
        input = input,
        output = output,
        session = session,
        current = current,
        entry_of = function(id) entry,
        entries = shiny::reactiveVal(list(entry)),
        worker = shiny::reactiveVal(list()),
        enqueue = function(request) TRUE,
        commit_images = function(entry, images) invisible(entry),
        alignment_preview = shiny::reactiveVal(NULL),
        spatial_coords = shiny::reactiveVal(NULL)
      )
    },
    {
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      expect_identical(alignment$active_image(), "H&E")

      expect_true(alignment$restore_project_selection(list(
        dataset = "ds1",
        section = "fov-b",
        image = "DAPI"
      )))
      session$flushReact()

      expect_identical(alignment$active_section(), "fov-b")
      expect_identical(alignment$active_image(), "DAPI")

      session$setInputs(`enhance-active_section` = "fov-a")
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      current(NULL)
      session$flushReact()
      current("ds1")
      session$flushReact()
      expect_identical(alignment$active_section(), "fov-a")
      expect_identical(alignment$active_image(), "H&E")
    }
  )
})

test_that("the assembled Builder server finalizes a restored entry exactly once", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this restore test.")
  }
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "input.rds")
  writeBin(charToRaw("source"), source_path)
  source_identity <- app_env$builder_project_file_fingerprint(
    source_path,
    content = TRUE
  )
  source_state <- app_env$builder_project_file_metadata(source_path)
  source_state$md5 <- NULL
  source_state <- c(
    list(
      location = normalizePath(source_path, winslash = "/", mustWork = TRUE)
    ),
    source_state
  )
  source_descriptor <- list(
    kind = "managed",
    path = "sources/ds1/input.rds",
    status = "ready",
    fingerprint = source_identity
  )
  loaded <- builder_minimal_entry("ds1", "Source default")
  loaded$revision <- 1L
  loaded$snapshot <- list(
    path = "/runtime/new",
    owner_token = "new-owner",
    object_md5 = "new-object-md5",
    source_fingerprint = paste(
      "builder-snapshot-v2",
      "input.rds",
      source_identity$bytes,
      source_identity$modified_at,
      source_identity$md5,
      sep = ":"
    ),
    source_state = source_state
  )
  saved <- loaded
  saved$settings$name <- "Saved project name"
  saved$settings$organism <- "mm"
  saved$settings$overview_point_size <- 9
  saved$settings$spatial_coordinate_transforms <- list(
    fov_a = list(schema_version = 1L, rotation_degrees = 66.9, scale = 1)
  )
  saved$settings$spatial_point_appearance <- list(
    fov_a = list(point_opacity = 0.7, point_size = 6)
  )
  artifact_path <- file.path(root, "artifacts", "ds1", "dataset.crb")
  dir.create(dirname(artifact_path), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw("ready"), artifact_path)
  artifact_record <- list(
    status = "ready",
    reusable = TRUE,
    path = app_env$builder_project_relative_path(artifact_path, root),
    fingerprint = app_env$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    plan_item = list(
      artifact_identity = list(spatial_sections = character())
    ),
    built_from_source_fingerprint = saved$snapshot$source_fingerprint,
    built_from_configuration = app_env$builder_project_artifact_configuration_digest(
      saved
    )
  )
  record <- app_env$builder_project_dataset_record(
    saved,
    source = source_descriptor,
    checked = TRUE,
    artifact = artifact_record,
    root = root
  )
  record$runtime_restore_status <- app_env$builder_project_dataset_status(
    record,
    root
  )
  record$runtime_restore_action <- "resume"
  finalize_calls <- new.env(parent = emptyenv())
  finalize_calls$n <- 0L
  finalize_entry <- app_env$builder_project_finalize_loaded_entry
  app_env$builder_project_finalize_loaded_entry <- function(...) {
    finalize_calls$n <- finalize_calls$n + 1L
    finalize_entry(...)
  }

  shiny::testServer(app_env$server, {
    builder_project_pending_entries(list(
      ds1 = record,
      holding = list(id = "holding")
    ))
    builder_project_restore_progress(list(
      mode = "restoring",
      total = 2L,
      remaining = 2L
    ))
    ## Keep the abandoned-entry observer parked while this test drives each
    ## terminal transition synchronously.
    builder_project_operation_phase("opening")
    builder_project(list(
      root = root,
      manifest = list(
        datasets = list(record),
        last_ui = list(stage = "configure")
      )
    ))
    builder_project_crb_jobs(list())
    builder_project_artifacts(list(ds1 = artifact_record))

    artifact <- saved
    artifact$load_state <- "artifact_ready"
    artifact$snapshot <- NULL
    artifact$project_artifact <- list(
      status = "ready",
      path = "artifacts/ds1.crb"
    )
    use_state_only_fixture(list(artifact))
    dataset_check_marks(stats::setNames(
      app_env$builder_project_check_identity(artifact),
      "ds1"
    ))

    expect_true(builder_project_commit_restored_entry(
      list(id = "ds1"),
      error = "first restore attempt failed"
    ))
    expect_null(isolate(builder_project_pending_entries())[["ds1"]])
    expect_true("ds1" %in% isolate(builder_project_skipped_ids()))
    expect_false("ds1" %in% names(isolate(dataset_check_marks())))
    expect_identical(isolate(builder_project_restore_progress())$remaining, 1L)

    pending <- isolate(builder_project_pending_entries())
    pending[["ds1"]] <- record
    builder_project_pending_entries(pending)

    prepared <- builder_prepare_loaded_entry_attachment(loaded)
    expect_identical(finalize_calls$n, 1L)
    expect_length(prepared$state$datasets, 1L)
    expect_identical(prepared$state$datasets[[1L]]$id, "ds1")
    expect_identical(prepared$state$current_dataset, "ds1")
    expect_identical(prepared$entry$settings, saved$settings)
    expect_identical(prepared$entry$snapshot, loaded$snapshot)
    expect_identical(
      prepared$entry$settings$spatial_coordinate_transforms$fov_a$rotation_degrees,
      66.9
    )
    expect_identical(
      prepared$entry$settings$spatial_point_appearance$fov_a,
      list(point_opacity = 0.7, point_size = 6)
    )

    store(prepared$state)
    expect_true(builder_project_commit_restored_entry(prepared$entry))
    session$flushReact()
    expect_identical(checked_dataset_ids(), "ds1")
    expect_true(app_env$builder_project_artifact_available(
      isolate(builder_project_artifacts())[["ds1"]],
      root
    ))
    expect_true(app_env$builder_project_artifact_matches_entry(
      isolate(builder_project_artifacts())[["ds1"]],
      prepared$entry
    ))
    expect_false(app_env$builder_project_artifact_matches_entry(
      isolate(builder_project_artifacts())[["ds1"]],
      isolate(sets())[[1L]]
    ))
    expect_length(isolate(builder_project_crb_jobs()), 0L)
    expect_identical(dataset_checkpoint_states()[["ds1"]]$status, "no_crb")
    expect_identical(checked_dataset_ids(), "ds1")
    expect_null(isolate(builder_project_pending_entries())[["ds1"]])
    expect_false("ds1" %in% isolate(builder_project_skipped_ids()))
    expect_identical(
      isolate(dataset_check_marks())[["ds1"]],
      app_env$builder_project_check_identity(prepared$entry)
    )
    expect_identical(isolate(builder_project_restore_progress())$remaining, 1L)
    expect_identical(finalize_calls$n, 1L)

    assign(
      "save_builder_project_state",
      function(...) stop("Restoring must not rewrite the Project"),
      envir = environment(builder_project_commit_restored_entry)
    )
    assign(
      "queue_builder_project_crbs",
      function(...) TRUE,
      envir = environment(builder_project_commit_restored_entry)
    )
    builder_project_operation_phase("restoring")
    expect_true(builder_project_commit_restored_entry(
      list(id = "holding"),
      error = "terminal fixture"
    ))
    expect_length(isolate(builder_project_pending_entries()), 0L)
    expect_identical(isolate(builder_project_restore_progress())$mode, "idle")
    expect_identical(isolate(builder_project_operation_phase()), "idle")

    use_state_only_fixture(list(prepared$entry))
    edited <- prepared$entry
    edited$settings$overview_point_size <- 10
    expect_true(replace_entry(edited, internal = TRUE))
    expect_identical(finalize_calls$n, 1L)
  })
})

test_that("restored duplicate sources keep independent dataset identities", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this restore test.")
  }

  shiny::testServer(app_env$server, {
    first <- builder_minimal_entry("ds4", "Repeated source")
    first$snapshot$source_fingerprint <- "same-source-fingerprint"
    use_state_only_fixture(list(first))

    register_loaded_entry_finalizer(function(entry) {
      entry$settings$name <- "Repeated source"
      entry
    })
    second <- builder_minimal_entry("ds7", "Import label")
    second$snapshot$source_fingerprint <- "same-source-fingerprint"

    prepared <- builder_prepare_loaded_entry_attachment(second)

    expect_length(prepared$state$datasets, 2L)
    expect_identical(
      vapply(
        prepared$state$datasets,
        function(entry) entry$settings$name,
        character(1)
      ),
      c("Repeated source", "Repeated source")
    )
    expect_identical(
      vapply(
        prepared$state$datasets,
        function(entry) entry$snapshot$source_fingerprint,
        character(1)
      ),
      rep("same-source-fingerprint", 2L)
    )
  })
})

test_that("skipped Project records reserve ids but not display names", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this reservation test.")
  }

  shiny::testServer(app_env$server, {
    builder_project(list(
      manifest = list(
        datasets = list(
          list(id = "ds9", name = "Skipped dataset")
        )
      )
    ))

    expect_identical(
      app_env$builder_project_dataset_reservations(builder_project()$manifest),
      list(ids = "ds9")
    )
  })
})

test_that("failed source resume preserves the checked artifact entry", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this resume test.")
  }
  root <- withr::local_tempdir()
  source_dir <- file.path(root, "sources", "ds1")
  dir.create(source_dir, recursive = TRUE)
  source_path <- file.path(source_dir, "input.rds")
  writeBin(charToRaw("source"), source_path)
  saved <- builder_minimal_entry("ds1", "Saved dataset")
  record <- app_env$builder_project_dataset_record(
    saved,
    source = list(
      kind = "managed",
      path = "sources/ds1/input.rds",
      filename = "input.rds"
    ),
    checked = TRUE,
    root = root
  )
  artifact <- saved
  artifact$load_state <- "artifact_ready"
  artifact$snapshot <- NULL
  artifact$project_artifact <- list(
    status = "ready",
    path = "artifacts/ds1.crb"
  )

  shiny::testServer(app_env$server, {
    use_state_only_fixture(list(artifact))
    current("ds1")
    mark <- app_env$builder_project_check_identity(artifact)
    dataset_check_marks(stats::setNames(mark, "ds1"))
    builder_project(list(
      root = root,
      manifest = list(datasets = list(record))
    ))
    builder_project_pending_entries(list())
    session$flushReact()

    session$setInputs(project_resume_current_source = 1)
    session$flushReact()

    expect_length(sets(), 1L)
    expect_identical(sets()[[1L]]$id, "ds1")
    expect_identical(sets()[[1L]]$load_state, "artifact_ready")
    expect_identical(isolate(dataset_check_marks())[["ds1"]], mark)
    expect_null(isolate(builder_project_pending_entries())[["ds1"]])
  })
})

test_that("source resume catches start failures and restores pending state", {
  source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  observer <- sub(
    ".*observeEvent\\(input\\$project_resume_current_source, \\{",
    "",
    source
  )
  observer <- sub(
    "\\n\\}\\)\\n\\nobserve\\(\\{\\n  value <- result\\(\\).*",
    "",
    observer
  )

  expect_match(observer, "started <- tryCatch(", fixed = TRUE)
  expect_match(observer, "conditionMessage(started)", fixed = TRUE)
  expect_match(observer, "pending[[id]] <- previous_pending", fixed = TRUE)
  expect_match(
    observer,
    'record$runtime_restore_action <- "edit"',
    fixed = TRUE
  )
  expect_match(
    observer,
    'send_builder_project_source_load_state(id, "error", message)',
    fixed = TRUE
  )
  expect_match(
    observer,
    'send_builder_project_source_load_state(id, "loading")',
    fixed = TRUE
  )
})

test_that("source edit completion reports ready and error UI states", {
  source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    source,
    '"builder_project_source_load_state"',
    fixed = TRUE
  )
  expect_match(
    source,
    'if (is.null(error)) "ready" else "error"',
    fixed = TRUE
  )
})

test_that("background CRB runtime includes the snapshot adapter", {
  source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  profile <- regexpr(
    'file.path(builder_dir, "profile.R")',
    source,
    fixed = TRUE
  )
  inspect <- regexpr(
    'file.path(builder_dir, "inspect.R")',
    source,
    fixed = TRUE
  )
  adapter <- regexpr(
    'file.path(builder_dir, "adapters.R")',
    source,
    fixed = TRUE
  )
  analysis <- regexpr(
    'file.path(builder_dir, "analysis.R")',
    source,
    fixed = TRUE
  )
  build <- regexpr('file.path(builder_dir, "build.R")', source, fixed = TRUE)
  expect_gt(profile, 0L)
  expect_gt(inspect, profile)
  expect_gt(adapter, inspect)
  expect_gt(analysis, adapter)
  expect_gt(adapter, 0L)
  expect_gt(build, analysis)
})

test_that("background CRB runtime includes package exports", {
  runtime <- new.env(parent = globalenv())
  builder_dir <- builder_profile_inst_path("builder")
  sys.source(
    file.path(builder_dir, "worker.R"),
    envir = runtime
  )
  source_root <- runtime$.builder_worker_package_source(builder_dir)
  if (is.null(source_root)) {
    package_runtime <- asNamespace("CerebroNexus")
  } else {
    expect_true(dir.exists(file.path(source_root, "R")))
    description <- read.dcf(file.path(source_root, "DESCRIPTION"))
    collate <- if ("Collate" %in% colnames(description)) {
      description[1L, "Collate"]
    } else {
      NA_character_
    }
    files <- if (is.na(collate) || !nzchar(collate)) {
      sort(list.files(
        file.path(source_root, "R"),
        pattern = "[.][Rr]$",
        full.names = TRUE
      ))
    } else {
      file.path(
        source_root,
        "R",
        scan(text = collate, what = character(), quiet = TRUE)
      )
    }
    package_runtime <- new.env(parent = globalenv())
    for (file in files) {
      sys.source(file, envir = package_runtime)
    }
  }
  expect_true(is.function(package_runtime$exportFromSeurat))
})

test_that("a Source opened for editing stays in Configure", {
  source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    source,
    'focus_source <- editing_source',
    fixed = TRUE
  )
  expect_match(
    source,
    'project$manifest$last_ui$stage <- "configure"',
    fixed = TRUE
  )
})

test_that("project pending settles only after a restored load is terminal", {
  read_server <- function(name) {
    paste(
      readLines(
        builder_test_inst_path(
          "builder",
          "server",
          name
        ),
        warn = FALSE
      ),
      collapse = "\n"
    )
  }
  import_source <- read_server("imports.R")
  recovery <- read_server("foundation.R")
  register_failures <- strsplit(
    import_source,
    'if (inherits(updated_worker, "try-error")) {',
    fixed = TRUE
  )[[1L]]
  register_failure <- register_failures[[length(register_failures)]]
  register_failure <- strsplit(
    register_failure,
    "\n    worker(updated_worker)",
    fixed = TRUE
  )[[1L]][[1L]]
  recovery <- strsplit(
    recovery,
    "apply_protocol_recovery <- function(",
    fixed = TRUE
  )[[1L]][[2L]]
  recovery <- strsplit(
    recovery,
    "\nrestart_worker_protocol <- function(",
    fixed = TRUE
  )[[1L]][[1L]]

  expect_lt(
    regexpr("store(next_state)", import_source, fixed = TRUE)[[1L]],
    regexpr(
      "builder_project_commit_restored_entry(entry)",
      import_source,
      fixed = TRUE
    )[[1L]]
  )
  expect_match(register_failure, "restart_worker_protocol(", fixed = TRUE)
  expect_false(any(vapply(
    c(
      "builder_project_commit_restored_entry(",
      "builder_project_pending_entries("
    ),
    grepl,
    logical(1),
    x = register_failure,
    fixed = TRUE
  )))
  expect_identical(
    lengths(regmatches(
      recovery,
      gregexpr(
        "builder_project_commit_restored_entry(",
        recovery,
        fixed = TRUE
      )
    )),
    1L
  )
  expect_lt(
    regexpr("lapply(failed_requests", recovery, fixed = TRUE)[[1L]],
    regexpr("builder_project_commit_restored_entry(", recovery, fixed = TRUE)[[
      1L
    ]]
  )
  expect_match(recovery, "failed_requests <- recovered$failed", fixed = TRUE)
  expect_match(recovery, 'identical(payload$kind, "load")', fixed = TRUE)
  expect_match(recovery, "error = reason", fixed = TRUE)
  expect_identical(
    lengths(regmatches(
      import_source,
      gregexpr(
        "builder_project_commit_restored_entry(",
        import_source,
        fixed = TRUE
      )
    )),
    4L
  )
  expect_identical(
    lengths(regmatches(
      import_source,
      gregexpr("release_pending_source(p)", import_source, fixed = TRUE)
    )),
    6L
  )
  expect_identical(
    lengths(regmatches(
      import_source,
      gregexpr(
        "release_pending_source(p, drop_import = FALSE)",
        import_source,
        fixed = TRUE
      )
    )),
    2L
  )
})

test_that("failed pre-store finalization releases its unattached snapshot", {
  source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "imports.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  branch <- sub(
    ".*if \\(inherits\\(finalized_entry, \"try-error\"\\)\\) \\{",
    "",
    source
  )
  branch <- sub("\\n    entry <- finalized_entry.*", "", branch)

  expect_match(
    branch,
    ".builder_snapshot_release(value$snapshot)",
    fixed = TRUE
  )
  expect_match(
    branch,
    "builder_protocol_acknowledge(protocol(), request$request_id)",
    fixed = TRUE
  )
  expect_match(branch, "builder_import_progress_remove", fixed = TRUE)
})

test_that("failed pre-store load acknowledgement releases the persistent queue", {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_test_inst_path("builder", "worker.R"),
    envir = runtime
  )
  protocol <- runtime$builder_request_protocol("worker-1")
  protocol <- runtime$builder_enqueue(
    protocol,
    runtime$builder_command("load", "ds1", payload = list(path = "ds1.rds"))
  )
  protocol <- runtime$builder_enqueue(
    protocol,
    runtime$builder_command("load", "ds2", payload = list(path = "ds2.rds"))
  )

  dispatched <- runtime$builder_protocol_dispatch(protocol)
  request <- dispatched$request
  expect_identical(request$kind, "load")
  expect_true(isTRUE(request$persistent))

  completed <- runtime$builder_protocol_complete(
    dispatched$protocol,
    runtime$builder_worker_response(request, value = list(snapshot = "invalid"))
  )
  expect_length(completed$protocol$awaiting_ack, 1L)

  acknowledged <- runtime$builder_protocol_acknowledge(
    completed$protocol,
    request$request_id
  )
  expect_length(acknowledged$awaiting_ack, 0L)

  next_dispatched <- runtime$builder_protocol_dispatch(acknowledged)
  expect_identical(next_dispatched$request$dataset, "ds2")
})

test_that("restore progress reports selected datasets rather than source queue size", {
  runtime <- builder_project_open_runtime()
  server_source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_identical(
    runtime$builder_project_restore_progress_detail(list(
      total = 2L,
      remaining = 1L,
      selected_total = 3L,
      ready = 2L
    )),
    "2 of 3 selected datasets ready"
  )
  expect_identical(
    runtime$builder_project_restore_progress_detail(list(
      total = 2L,
      remaining = 2L,
      selected_total = 3L,
      ready = 1L
    )),
    "1 of 3 selected datasets ready"
  )
  expect_identical(
    runtime$builder_project_restore_progress_detail(list(
      total = 2L,
      remaining = 1L
    )),
    "1 of 2 selected datasets ready"
  )
  expect_match(
    server_source,
    "builder_project_restore_progress_detail(restore_progress)",
    fixed = TRUE
  )
  expect_match(
    server_source,
    "selected_total = length(reusable_entries) + restore_total",
    fixed = TRUE
  )
  expect_match(
    server_source,
    "ready = length(reusable_entries)",
    fixed = TRUE
  )
  expect_match(
    server_source,
    "restore_entry$filename %||% restore_entry$label",
    fixed = TRUE
  )
  expect_match(
    server_source,
    'paste0("Loading ", restore_file',
    fixed = TRUE
  )
  expect_match(
    server_source,
    "builder_file_human_size(restore_bytes)",
    fixed = TRUE
  )
  expect_match(server_source, '"s elapsed"', fixed = TRUE)
  expect_match(
    server_source,
    "shiny::invalidateLater(1000, session)",
    fixed = TRUE
  )
  app_source <- paste(
    readLines(
      builder_test_inst_path("builder", "app.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl("text-overflow:ellipsis", app_source, fixed = TRUE))
  expect_false(grepl("white-space:nowrap", app_source, fixed = TRUE))
  expect_match(app_source, 'role = "dialog"', fixed = TRUE)
  expect_match(app_source, '`aria-modal` = "true"', fixed = TRUE)
})
