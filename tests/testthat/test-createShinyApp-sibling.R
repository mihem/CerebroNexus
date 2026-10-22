new_embedded_cerebro <- function() {
  object <- Cerebro$new()
  object$setExpressionBackend(type = "embedded")
  object
}

copy_cerebro_fixture_bindings <- function(exclude = character()) {
  source <- new_embedded_cerebro()
  payload <- new.env(parent = emptyenv())
  bindings <- setdiff(ls(source, all.names = TRUE), exclude)
  for (binding in bindings) {
    payload[[binding]] <- source[[binding]]
  }
  class(payload) <- class(source)
  payload
}

legacy_cerebro_v1_3_fixture <- function() {
  ## `Cerebro_v1.3` was the serialized class immediately before b13fee58
  ## introduced spatial storage and its three accessors. Keep the historical
  ## class identity here so this fixture cannot disguise a damaged current CRB.
  payload <- copy_cerebro_fixture_bindings(
    c("spatial", "addSpatialData", "availableSpatial", "getSpatialData")
  )
  class(payload) <- c("Cerebro_v1.3", "R6")
  lockEnvironment(payload, bindings = FALSE)
  payload
}

add_lazy_cerebro_binding <- function(payload, binding, value, sentinel) {
  evaluation <- new.env(parent = baseenv())
  evaluation$value <- value
  evaluation$sentinel <- sentinel
  delayedAssign(
    binding,
    {
      writeLines("FORCED", sentinel)
      value
    },
    eval.env = evaluation,
    assign.env = payload
  )
  invisible(payload)
}

write_bundle_crb <- function(
  directory,
  name = "dataset.crb",
  backend = list(type = "embedded", location = NULL),
  legacy = FALSE
) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, name)
  if (legacy) {
    payload <- copy_cerebro_fixture_bindings(
      c("getExpressionBackend", "expression_backend")
    )
    ## Match an R6 object: the object environment is locked against new
    ## members, while mutable data fields such as expression remain writable.
    lockEnvironment(payload, bindings = FALSE)
  } else {
    payload <- Cerebro$new()
    if (!is.null(backend)) {
      payload$setExpressionBackend(
        type = backend$type,
        location = backend$location
      )
    }
  }
  saveRDS(payload, path)
  path
}

## A CRB without an expression_backend descriptor is a retired format; keep the
## builder explicit so a test never gets one by accident.
write_retired_crb <- function(directory, name = "retired.crb") {
  write_bundle_crb(directory, name, backend = NULL)
}

test_that("CRB preflight reads, inspects, and releases one dataset at a time", {
  events <- character()
  paths <- c(First = "first.crb", Second = "second.crb")

  preflight <- .preflightBundleData(
    paths,
    read_object = function(path) {
      events <<- c(events, paste("read", path))
      list(path = path)
    },
    inspect_backend = function(path, object) {
      events <<- c(events, paste("backend", object$path))
      list(type = "embedded", location = NULL, legacy = FALSE)
    },
    inspect_spatial = function(object, dataset) {
      events <<- c(events, paste("spatial", dataset))
      list(section = character())
    },
    release_object = function(object) {
      events <<- c(events, paste("release", object$path))
    }
  )

  expect_identical(
    events,
    c(
      "read first.crb",
      "backend first.crb",
      "spatial First",
      "release first.crb",
      "read second.crb",
      "backend second.crb",
      "spatial Second",
      "release second.crb"
    )
  )
  expect_named(preflight$backends, c("First", "Second"))
  expect_named(preflight$spatial_catalogs, c("First", "Second"))
})

test_that("CRB preflight keeps each spatial backend at its dataset index", {
  sidecar <- list(type = "directory", location = "second.spatial")
  local_mocked_bindings(
    .isRecognizedCerebroObject = function(object) TRUE,
    .readBundleDatasetInfo = function(object, label, path) list(),
    .spatialMoleculeBackend = function(object, file) {
      if (identical(file, "second.crb")) sidecar else NULL
    },
    .immuneRepertoireBackend = function(object, file) NULL
  )
  paths <- c(First = "first.crb", Second = "second.crb", Third = "third.crb")

  preflight <- .preflightBundleData(
    paths,
    read_object = function(path) list(path = path),
    inspect_backend = function(path, object) list(type = "embedded"),
    inspect_spatial = function(object, dataset) list()
  )

  ## A NULL backend for the first dataset must not shift the second one.
  expect_identical(
    preflight$spatial_backends,
    list(First = NULL, Second = sidecar, Third = NULL)
  )
})

test_that("spatial preflight does not hydrate molecule sidecars", {
  hydrate_values <- logical()
  object <- new.env(parent = emptyenv())
  object$availableSpatial <- function() "fov1"
  object$getSpatialData <- function(name, hydrate_molecules = TRUE) {
    hydrate_values <<- c(hydrate_values, hydrate_molecules)
    list(
      coordinates = data.frame(x = 1:2, y = 3:4),
      expression = matrix(numeric(), nrow = 0L, ncol = 2L),
      histology_images = list()
    )
  }

  catalog <- .readBundleSpatialCatalog(object, "Dataset")

  expect_named(catalog, "fov1")
  expect_null(catalog$fov1)
  expect_identical(hydrate_values, FALSE)
})

test_that("CRB preflight releases the current dataset after an inspection error", {
  events <- character()

  expect_error(
    .preflightBundleData(
      c(First = "first.crb"),
      read_object = function(path) {
        events <<- c(events, paste("read", path))
        list(path = path)
      },
      inspect_backend = function(path, object) {
        events <<- c(events, paste("backend", object$path))
        stop("backend inspection failed")
      },
      inspect_spatial = function(object, dataset) {
        events <<- c(events, paste("spatial", dataset))
        list(section = character())
      },
      release_object = function(object) {
        events <<- c(events, paste("release", object$path))
      }
    ),
    "backend inspection failed"
  )

  expect_identical(
    events,
    c("read first.crb", "backend first.crb", "release first.crb")
  )
})

test_that("CRB preflight preserves an inspection error when release also fails", {
  events <- character()

  expect_error(
    .preflightBundleData(
      c(First = "first.crb"),
      read_object = function(path) list(path = path),
      inspect_backend = function(path, object) {
        events <<- c(events, "backend")
        stop("backend inspection failed")
      },
      inspect_spatial = function(object, dataset) list(section = character()),
      release_object = function(object) {
        events <<- c(events, "release")
        stop("release failed")
      }
    ),
    "backend inspection failed"
  )
  expect_identical(events, c("backend", "release"))
})

write_spatial_bundle_crb <- function(
  directory,
  name = "dataset.crb",
  spatials = list(section = character())
) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  object <- new_embedded_cerebro()
  coordinates <- data.frame(
    x = c(0, 100),
    y = c(0, 100),
    row.names = c("cell-1", "cell-2")
  )
  expression <- matrix(
    1:4,
    nrow = 2,
    dimnames = list(c("gene-1", "gene-2"), rownames(coordinates))
  )
  for (spatial_name in names(spatials)) {
    labels <- spatials[[spatial_name]]
    images <- lapply(labels, function(label) {
      list(
        histology_image = "data:image/png;base64,AA==",
        histology_image_bounds = c(
          xmin = 0,
          xmax = 100,
          ymin = 0,
          ymax = 100
        )
      )
    })
    names(images) <- labels
    object$addSpatialData(
      spatial_name,
      list(
        coordinates = coordinates,
        expression = expression,
        histology_images = images
      )
    )
  }
  path <- file.path(directory, name)
  saveRDS(object, path)
  path
}

write_backend_artifact <- function(directory, backend, contents = "MATRIX") {
  path <- file.path(directory, backend$location)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(contents, path)
  path
}

build_test_app <- function(cerebro_data, result_dir, ...) {
  createShinyApp(
    cerebro_data = cerebro_data,
    result_dir = result_dir,
    launch_browser = FALSE,
    verbose = FALSE,
    ...
  )
}

test_that("valid Viewer Packs are bundled beside their CRB", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  crb <- write_bundle_crb(source)
  object <- readCerebro(crb)
  object$setMetaData(data.frame(
    cell_barcode = c("cell-1", "cell-2"),
    group = c("A", "B"),
    stringsAsFactors = FALSE
  ))
  object$addProjection(
    "umap",
    data.frame(x = 1:2, y = 2:1, row.names = c("cell-1", "cell-2"))
  )
  saveCerebro(object, crb, codec = "rds", viewer_binary = "always")

  app <- file.path(root, "app")
  build_test_app(c(Dataset = crb), app)

  expect_true(dir.exists(file.path(app, "private-data", "dataset.viewer")))
  expect_true(
    validateViewerPack(
      file.path(app, "private-data", "dataset.crb"),
      file.path(app, "private-data", "dataset.viewer")
    )$valid
  )
})

test_that("scatter defaults accept one value or one value per dataset", {
  labels <- c("PBMC", "Xenium")

  expect_identical(
    .normalizeDatasetNumericOption(4, labels, "point_size", 1, 20),
    c(PBMC = 4, Xenium = 4)
  )
  expect_identical(
    .normalizeDatasetNumericOption(
      list(PBMC = 0.4, Xenium = 0.9),
      labels,
      "point_opacity",
      0.1,
      1
    ),
    c(PBMC = 0.4, Xenium = 0.9)
  )
  expect_identical(
    .normalizeDatasetNumericOption(
      100,
      labels,
      "percentage_cells_to_show",
      10,
      100
    ),
    c(PBMC = 100, Xenium = 100)
  )
  expect_error(
    .normalizeDatasetNumericOption(c(2, 5), labels, "point_size", 1, 20),
    "must be named"
  )
  expect_error(
    .normalizeDatasetNumericOption(
      list(PBMC = 0.5),
      labels,
      "point_opacity",
      0.1,
      1
    ),
    "exactly match"
  )
  expect_error(
    .normalizeDatasetNumericOption(0, labels, "point_size", 1, 20),
    "between 1 and 20"
  )
})

test_that("Viewer resolves scatter defaults for the selected dataset", {
  runtime <- viewer_runtime_env()
  options <- list(
    point_size = c(PBMC = 2, Xenium = 5),
    point_opacity = c(PBMC = 0.4, Xenium = 1),
    percentage_cells_to_show = c(PBMC = 40, Xenium = 100)
  )

  expect_identical(
    runtime$viewerScatterDefaults(options, "Xenium"),
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 100)
  )
  expect_identical(
    runtime$viewerScatterDefaults(list(), "Xenium"),
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 100)
  )
  expect_identical(
    runtime$viewerScatterDefaults(options, "Missing"),
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 100)
  )
  ## Ordinary projection views share one author configuration.
  expect_identical(
    runtime$viewerScatterDefaults(options, "Xenium", "expression"),
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 100)
  )
  expect_identical(
    runtime$viewerScatterDefaults(options, "Missing", "expression"),
    list(point_size = 5, point_opacity = 1, percentage_cells_to_show = 100)
  )
})

test_that("createShinyApp stores dataset scatter defaults directly", {
  root <- tempfile("point-appearance-")
  dir.create(root)
  first <- write_bundle_crb(root, "first.crb")
  second <- write_bundle_crb(root, "second.crb")
  app <- file.path(root, "app")

  build_test_app(
    c(PBMC = first, Xenium = second),
    app,
    point_size = list(PBMC = 2, Xenium = 5),
    point_opacity = 0.8,
    percentage_cells_to_show = list(PBMC = 60, Xenium = 100)
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))

  expect_identical(config$point_size, c(PBMC = 2, Xenium = 5))
  expect_identical(config$point_opacity, c(PBMC = 0.8, Xenium = 0.8))
  expect_identical(
    config$percentage_cells_to_show,
    c(PBMC = 60, Xenium = 100)
  )
})

test_that("createShinyApp freezes compact dataset information", {
  root <- withr::local_tempdir()
  paths <- vapply(
    list(
      Human = list(cells = c("cell-1", "cell-2"), organism = "Homo sapiens"),
      Mouse = list(cells = c("cell-a", "cell-b", "cell-c"), organism = "Mouse")
    ),
    function(info) {
      object <- new_embedded_cerebro()
      object$setMetaData(data.frame(
        cell_barcode = info$cells,
        row.names = info$cells
      ))
      object$addExperiment("organism", info$organism)
      object$addExperiment("date_of_export", as.Date("2026-09-17"))
      if (identical(info$organism, "Mouse")) {
        object$trajectories <- list(
          marker_guided = list(E18_neurogenesis = list())
        )
      }
      path <- file.path(root, paste0(tolower(info$organism), ".crb"))
      saveRDS(object, path)
      path
    },
    character(1)
  )
  app <- file.path(root, "app")

  build_test_app(paths, app)
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  configured <- unname(config$crb_file_to_load)

  expect_named(config[[".dataset_catalog"]], configured)
  entry <- config[[".dataset_catalog"]][[configured[[1L]]]]
  expect_identical(
    entry[c(
      "label",
      "path",
      "cells",
      "organism",
      "date",
      "immune_repertoire",
      "tcr_repertoire"
    )],
    list(
      label = "Human",
      path = configured[[1L]],
      cells = 2L,
      organism = "Homo sapiens",
      date = "2026-09-17",
      immune_repertoire = FALSE,
      tcr_repertoire = FALSE
    )
  )
  expect_identical(
    entry$capabilities,
    list(
      marker_genes = FALSE,
      most_expressed_genes = FALSE,
      enriched_pathways = FALSE,
      extra_material = FALSE,
      trajectory = FALSE,
      spatial = FALSE,
      trekker = FALSE
    )
  )
  ## The catalog also carries the projection list and the overview summary the
  ## sidebar reads before a data set is opened.
  expect_true(all(c("projection_names", "overview") %in% names(entry)))
  expect_identical(
    config[[".dataset_catalog"]][[configured[[2L]]]]$cells,
    3L
  )
  expect_true(
    config[[".dataset_catalog"]][[configured[[2L]]]]$capabilities$trajectory
  )
})

## External backgrounds are delivered as session-private URLs, so the renderer
## needs a session. This stand-in records what it registers: the file stays on
## disk behind a revalidating handler and the browser only ever sees the URL.
fake_private_session <- function() {
  registered <- new.env(parent = emptyenv())
  session <- list(
    userData = new.env(parent = emptyenv()),
    registerDataObj = function(name, data, filter) {
      registered$name <- name
      registered$data <- data
      registered$filter <- filter
      paste0("session/", name)
    }
  )
  attr(session, "registered") <- registered
  session
}

render_bundle_spatial_background <- function(
  app,
  config,
  background_image,
  background_image_allowlist,
  session = NULL,
  background_descriptor = list(
    source = "external",
    label = basename(background_image),
    path = background_image,
    bounds = NULL
  )
) {
  renderer <- new.env(parent = globalenv())
  renderer$Cerebro.options <- config
  if (!is.null(session)) {
    ## viewerPrivateImageUrl() falls back to a `session` binding in the frame
    ## that called it, which is this renderer environment.
    renderer$session <- session
  }
  viewer_runtime_env(renderer, root = file.path(app, "viewer"))
  sys.source(
    file.path(app, "viewer", "spatial", "func_spatial_helpers.R"),
    envir = renderer
  )
  sys.source(
    file.path(
      app,
      "viewer",
      "spatial",
      "func_projection_update_plot.R"
    ),
    envir = renderer
  )

  rendered_meta <- NULL
  renderer$cerebroCellViewRender <- function(id, meta, ...) {
    rendered_meta <<- meta
  }

  withr::with_dir(
    app,
    renderer$spatial_projection_update_plot(list(
      cells_df = data.frame(score = c(1, 2)),
      coordinates = data.frame(x = c(1, 2), y = c(3, 4)),
      reset_axes = TRUE,
      color_assignments = character(),
      hover_info = c("first", "second"),
      plot_parameters = list(
        color_variable = "score",
        background_image = background_image,
        background_descriptor = background_descriptor,
        background_image_allowlist = background_image_allowlist,
        n_dimensions = 2,
        x_range = NULL,
        y_range = NULL,
        background_flip_x = FALSE,
        background_flip_y = FALSE,
        background_scale_x = 1,
        background_scale_y = 1,
        background_offset_x = 0,
        background_offset_y = 0,
        background_opacity = 1,
        plot_type = "Feature plot",
        point_size = 5,
        point_opacity = 1,
        draw_border = FALSE,
        hover_info = FALSE
      )
    ))
  )

  rendered_meta
}

source_bundle_runtime <- function(app = NULL) {
  root <- if (is.null(app)) viewer_test_path() else file.path(app, "viewer")
  stopifnot(file.exists(file.path(root, "utility_functions.R")))
  viewer_runtime_env(root = root)
}

capture_backend_contract <- function(operation) {
  tryCatch(
    list(accepted = TRUE, value = operation()),
    error = function(error) list(accepted = FALSE, value = NULL)
  )
}

canonical_backend_descriptor <- function(backend) {
  list(
    type = backend$type,
    location = backend$location,
    legacy = isTRUE(backend$legacy)
  )
}

new_backend_contract_object <- function(backend = NULL, legacy = FALSE) {
  if (legacy) {
    object <- copy_cerebro_fixture_bindings(
      c("getExpressionBackend", "expression_backend")
    )
    lockEnvironment(object, bindings = TRUE)
    return(object)
  }
  object <- Cerebro$new()
  object$expression_backend <- backend
  object
}

local_cerebro_options <- function(options, .local_envir = parent.frame()) {
  had_options <- exists(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (had_options) {
    old_options <- get("Cerebro.options", envir = .GlobalEnv)
  }
  withr::defer(
    if (had_options) {
      assign("Cerebro.options", old_options, envir = .GlobalEnv)
    } else if (
      exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
    ) {
      rm(list = "Cerebro.options", envir = .GlobalEnv)
    },
    envir = .local_envir
  )
  assign("Cerebro.options", options, envir = .GlobalEnv)
  invisible(options)
}

expect_bundle_backend_entry <- function(config, path, expected) {
  manifest <- config[[".bundle_backend_plan"]]
  expect_identical(manifest$schema_version, 1L)
  expect_true(path %in% names(manifest$entries))
  expect_identical(manifest$entries[[path]], expected)
}

write_divergent_backend_crb <- function(
  directory,
  field_backend,
  getter_backend,
  sentinel,
  name = "dataset.crb"
) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  payload <- copy_cerebro_fixture_bindings(
    c("getExpressionBackend", "expression_backend")
  )
  payload$expression_backend <- field_backend
  payload$getExpressionBackend <- base::local({
    marker <- sentinel
    backend <- getter_backend
    function() {
      writeLines("EXECUTED", marker)
      backend
    }
  })
  lockEnvironment(payload, bindings = TRUE)
  path <- file.path(directory, name)
  saveRDS(payload, path)
  path
}

write_real_h5_matrix <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  expected <- matrix(
    c(0, 1, 4, 2, 0, 5, 3, 6, 0, 7, 8, 9),
    nrow = 3L,
    dimnames = list(
      c("GeneA", "GeneB", "GeneC"),
      c("Cell1", "Cell2", "Cell3", "Cell4")
    )
  )
  sparse <- methods::as(
    Matrix::Matrix(expected, sparse = TRUE),
    "CsparseMatrix"
  )
  HDF5Array::writeTENxMatrix(
    methods::as(Matrix::t(sparse), "CsparseMatrix"),
    path,
    group = "expression"
  )
  expected
}

expect_attached_matrix <- function(object, expected) {
  realized <- as.matrix(object$expression)
  expect_identical(dim(realized), dim(expected))
  expect_identical(dimnames(realized), dimnames(expected))
  expect_equal(unname(realized), unname(expected), tolerance = 0)
}

expect_real_backend_bundle_roundtrip <- function() {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  dir.create(source)
  location <- "expression.h5"
  backend_path <- file.path(source, location)
  object <- Cerebro$new()
  object$setMetaData(data.frame(
    group = c("A", "A", "B", "B"),
    row.names = c("Cell1", "Cell2", "Cell3", "Cell4")
  ))
  expected <- write_real_h5_matrix(backend_path)
  object$setExpressionBackend(type = "h5", location = location)
  crb <- file.path(source, "dataset.crb")
  saveRDS(object, crb)
  app <- file.path(root, "app")

  build_test_app(c("Dataset" = crb), app)
  unlink(source, recursive = TRUE, force = TRUE)

  bundled_crb <- file.path(app, "private-data", "dataset.crb")
  expect_true(file.exists(bundled_crb))
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  configured_path <- unname(config$crb_file_to_load[[1L]])
  expect_bundle_backend_entry(
    config,
    configured_path,
    list(type = "h5", mode = "bundled", location = location)
  )
  runtime <- source_bundle_runtime(app)
  local_cerebro_options(config)
  withr::local_dir(app)

  attached <- runtime$get_or_load_crb(
    configured_path,
    config[[".bundle_backend_plan"]],
    unname(config$crb_file_to_load)
  )
  expect_attached_matrix(attached, expected)
}

test_that("tagged H5 backends are copied to their exact paths", {
  root <- withr::local_tempdir()
  backend <- list(type = "h5", location = "matrices/original-name.h5")
  source <- file.path(root, "source")
  crb <- write_bundle_crb(
    source,
    name = "renamed-dataset.crb",
    backend = backend
  )
  write_backend_artifact(source, backend)
  app <- file.path(root, "app")

  build_test_app(c("Dataset" = crb), app)

  expect_true(file.exists(file.path(
    app,
    "private-data",
    "renamed-dataset.crb"
  )))
  target <- file.path(app, "private-data", backend$location)
  expect_true(file.exists(target))
  expect_identical(readLines(target), "MATRIX")
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_bundle_backend_entry(
    config,
    "private-data/renamed-dataset.crb",
    list(type = "h5", mode = "bundled", location = backend$location)
  )
  expect_false(file.exists(file.path(
    app,
    "private-data",
    "renamed-dataset.h5"
  )))
})

test_that("build and runtime share the portable backend path contract", {
  runtime <- source_bundle_runtime()
  valid <- list(
    root = "matrix.h5",
    nested = "matrices/expression.h5",
    punctuation = "nested/a.b-c_1.h5"
  )
  invalid <- list(
    null = NULL,
    zero_length = character(),
    missing = NA_character_,
    multiple = c("first.h5", "second.h5"),
    non_character = 1L,
    empty = "",
    current = "./matrix.h5",
    nested_current = "nested/./matrix.h5",
    parent = "../matrix.h5",
    nested_parent = "nested/../matrix.h5",
    unix_absolute = "/tmp/matrix.h5",
    home = "~/matrix.h5",
    drive = "C:/matrix.h5",
    unc = "//server/share/matrix.h5",
    empty_segment = "nested//matrix.h5",
    backslash = "nested\\matrix.h5",
    trailing_separator = "matrix.h5/",
    reserved = "nested/COM1.bin",
    nested_reserved = "nested/NUL/matrix.h5",
    trailing_dot = "nested/name.",
    nested_trailing_dot = "nested/name./matrix.h5",
    trailing_space = "nested/name ",
    nested_trailing_space = "nested/name /matrix.h5",
    invalid_character = "nested/a:b.h5",
    control_character = paste0("nested/", intToUtf8(1L), "matrix.h5")
  )
  cases <- c(
    lapply(valid, function(path) list(path = path, accepted = TRUE)),
    lapply(invalid, function(path) list(path = path, accepted = FALSE))
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    build <- capture_backend_contract(function() {
      .portableBundlePath(case$path, "The backend location")
    })
    app_runtime <- capture_backend_contract(function() {
      runtime$.runtimePortableBackendPath(
        case$path,
        "The backend location"
      )
    })

    expect_identical(build$accepted, case$accepted, info = case_name)
    expect_identical(app_runtime$accepted, build$accepted, info = case_name)
    if (build$accepted) {
      expect_identical(app_runtime$value, build$value, info = case_name)
    }
  }
})

test_that("build and runtime share the backend descriptor contract", {
  root <- withr::local_tempdir()
  runtime <- source_bundle_runtime()
  getter_sentinel <- file.path(root, "getter-was-called")
  guarded <- copy_cerebro_fixture_bindings(
    c("getExpressionBackend", "expression_backend")
  )
  guarded$expression_backend <- list(
    type = "h5",
    location = "matrices/expression.h5"
  )
  guarded$getExpressionBackend <- base::local({
    marker <- getter_sentinel
    function() {
      writeLines("CALLED", marker)
      list(type = "embedded", location = NULL)
    }
  })
  lockEnvironment(guarded, bindings = TRUE)

  field_only <- copy_cerebro_fixture_bindings("getExpressionBackend")
  field_only$expression_backend <- list(type = "embedded", location = NULL)
  lockEnvironment(field_only, bindings = TRUE)
  getter_only <- copy_cerebro_fixture_bindings("expression_backend")
  lockEnvironment(getter_only, bindings = TRUE)
  broken_getter <- copy_cerebro_fixture_bindings(
    c("getExpressionBackend", "expression_backend")
  )
  broken_getter$getExpressionBackend <- "not a function"
  broken_getter$expression_backend <- list(
    type = "embedded",
    location = NULL
  )
  lockEnvironment(broken_getter, bindings = TRUE)

  active_sentinel <- file.path(root, "active-binding-was-called")
  active <- copy_cerebro_fixture_bindings("getExpressionBackend")
  makeActiveBinding(
    "getExpressionBackend",
    base::local({
      marker <- active_sentinel
      function(replacement) {
        if (!missing(replacement)) {
          stop("fixture binding is read-only")
        }
        writeLines("CALLED", marker)
        function() list(type = "embedded", location = NULL)
      }
    }),
    active
  )
  lockEnvironment(active, bindings = TRUE)

  lazy_sentinel <- file.path(root, "lazy-binding-was-called")
  lazy <- copy_cerebro_fixture_bindings("expression_backend")
  add_lazy_cerebro_binding(
    lazy,
    "expression_backend",
    list(type = "embedded", location = NULL),
    lazy_sentinel
  )
  lockEnvironment(lazy, bindings = TRUE)

  # Compare only the descriptor semantics shared by both paths. The build's
  # object identity checks and the runtime class gate are tested separately.
  # The inverse active/lazy binding pairs have focused build and runtime tests
  # below; one of each kind is sufficient for this direct parity contract.
  cases <- list(
    legacy = list(
      object = new_backend_contract_object(legacy = TRUE),
      accepted = FALSE
    ),
    null = list(
      object = new_backend_contract_object(NULL),
      accepted = FALSE
    ),
    embedded = list(
      object = new_backend_contract_object(
        list(type = "embedded", location = NULL)
      ),
      accepted = TRUE
    ),
    h5 = list(
      object = new_backend_contract_object(
        list(type = "h5", location = "matrix.h5")
      ),
      accepted = TRUE
    ),
    guarded_getter = list(object = guarded, accepted = TRUE),
    field_only = list(object = field_only, accepted = FALSE),
    getter_only = list(object = getter_only, accepted = FALSE),
    broken_getter = list(object = broken_getter, accepted = FALSE),
    invalid_type = list(
      object = new_backend_contract_object(
        list(type = "unknown", location = "matrix.h5")
      ),
      accepted = FALSE
    ),
    inconsistent_embedded = list(
      object = new_backend_contract_object(
        list(type = "embedded", location = "matrix.h5")
      ),
      accepted = FALSE
    ),
    non_portable = list(
      object = new_backend_contract_object(
        list(type = "h5", location = "nested//matrix.h5")
      ),
      accepted = FALSE
    ),
    active = list(object = active, accepted = FALSE),
    lazy = list(object = lazy, accepted = FALSE)
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    path <- file.path(root, paste0(case_name, ".crb"))
    saveRDS(case$object, path)
    build <- capture_backend_contract(function() {
      backend <- .readBundleBackend(path)
      if (!identical(backend$type, "embedded")) {
        backend$location <- .portableBundlePath(
          backend$location,
          "The backend location"
        )
      }
      canonical_backend_descriptor(backend)
    })
    app_runtime <- capture_backend_contract(function() {
      canonical_backend_descriptor(
        runtime$.readRuntimeBackendDescriptor(readRDS(path), path)
      )
    })

    expect_identical(build$accepted, case$accepted, info = case_name)
    expect_identical(app_runtime$accepted, build$accepted, info = case_name)
    if (build$accepted) {
      expect_identical(app_runtime$value, build$value, info = case_name)
    }
  }
  expect_false(file.exists(getter_sentinel))
  expect_false(file.exists(active_sentinel))
  expect_false(file.exists(lazy_sentinel))
})

test_that("build and runtime share the backend plan decision contract", {
  root <- withr::local_tempdir()
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())
  cases <- list(
    embedded = list(type = "embedded", location = NULL, legacy = FALSE),
    h5 = list(type = "h5", location = "matrix.h5", legacy = FALSE)
  )

  for (case_name in names(cases)) {
    backend <- cases[[case_name]]
    object <- new_backend_contract_object(backend[c("type", "location")])
    build <- .crbContract()$.effectiveRuntimeBackendPlan(backend)
    app_runtime <- runtime$.fallbackRuntimeBackendPlan(
      object,
      file.path(root, paste0(case_name, ".crb"))
    )

    expect_identical(app_runtime, build, info = case_name)
  }
  expect_identical(
    .crbContract()$.effectiveRuntimeBackendPlan(cases$embedded),
    list(type = "embedded", mode = "embedded", location = NULL)
  )
  expect_identical(
    .crbContract()$.effectiveRuntimeBackendPlan(cases$h5),
    list(type = "h5", mode = "bundled", location = "matrix.h5")
  )
})

test_that("backend locations must be portable relative paths", {
  root <- withr::local_tempdir()
  invalid <- c(
    "../matrix.h5",
    "./matrix.h5",
    "/tmp/matrix.h5",
    "C:/matrix.h5",
    "nested//matrix.h5",
    "nested\\matrix.h5",
    "matrix?.h5",
    "nested/a:b.h5",
    "NUL",
    "nested/COM1.bin",
    "nested/name.",
    "nested/name "
  )

  for (index in seq_along(invalid)) {
    source <- file.path(root, paste0("source-", index))
    crb <- write_bundle_crb(
      source,
      backend = list(type = "h5", location = invalid[[index]])
    )
    expect_error(
      build_test_app(
        c("Dataset" = crb),
        file.path(root, paste0("app-", index))
      ),
      "portable relative path"
    )
  }
})

test_that("backend locations reject a trailing separator before staging", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  crb <- write_bundle_crb(
    source,
    backend = list(type = "h5", location = "matrix.h5/")
  )
  writeLines("MATRIX", file.path(source, "matrix.h5"))
  app <- file.path(root, "app")

  expect_error(
    build_test_app(c("Dataset" = crb), app),
    "portable relative path"
  )
  expect_false(dir.exists(app))
})

test_that("every copied artifact has a portable bundle target", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  reserved_crb <- write_bundle_crb(root, "NUL.crb")

  expect_error(
    build_test_app(
      c("Dataset" = reserved_crb),
      file.path(root, "reserved-crb-app")
    ),
    "portable relative path"
  )

  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  reserved_image <- file.path(root, "AUX.png")
  writeLines("IMAGE", reserved_image)
  app <- file.path(root, "reserved-image-app")
  build_test_app(
    c("Dataset" = crb),
    app,
    spatial_images = list(
      Dataset = list(section = c("Tissue background" = reserved_image))
    )
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_true(file.exists(file.path(
    app,
    config$spatial_images$Dataset$section[["Tissue background"]]
  )))
})

test_that("malformed backend descriptors are rejected", {
  root <- withr::local_tempdir()
  broken <- copy_cerebro_fixture_bindings(
    c("getExpressionBackend", "expression_backend")
  )
  broken$getExpressionBackend <- "not a function"
  broken$expression_backend <- list(type = "embedded", location = NULL)
  lockEnvironment(broken, bindings = TRUE)
  broken_path <- file.path(root, "broken.crb")
  saveRDS(broken, broken_path)
  inconsistent <- Cerebro$new()
  inconsistent$expression_backend <- list(
    type = "embedded",
    location = "matrix.h5"
  )
  inconsistent_path <- file.path(root, "inconsistent.crb")
  saveRDS(inconsistent, inconsistent_path)

  expect_error(
    build_test_app(c("Broken" = broken_path), file.path(root, "broken-app")),
    "unsupported expression-backend descriptor"
  )
  expect_error(
    build_test_app(
      c("Inconsistent" = inconsistent_path),
      file.path(root, "inconsistent-app")
    ),
    "unsupported expression-backend descriptor"
  )
})

test_that("arbitrary RDS objects are not accepted as Cerebro data", {
  root <- withr::local_tempdir()
  path <- file.path(root, "number.rds")
  saveRDS(42, path)

  expect_error(
    build_test_app(c("Number" = path), file.path(root, "app")),
    "recognized Cerebro"
  )
  expect_false(dir.exists(file.path(root, "app")))
})

test_that("a Cerebro class label alone is not a valid Cerebro object", {
  root <- withr::local_tempdir()
  path <- file.path(root, "impostor.rds")
  saveRDS(structure(42, class = "Cerebro"), path)

  expect_error(
    build_test_app(c("Impostor" = path), file.path(root, "app")),
    "recognized Cerebro"
  )
  expect_false(dir.exists(file.path(root, "app")))
})

test_that("an empty classed environment is not a Cerebro object", {
  root <- withr::local_tempdir()
  empty <- new.env(parent = emptyenv())
  class(empty) <- c("Cerebro", "R6")
  path <- file.path(root, "empty.crb")
  saveRDS(empty, path)

  expect_error(
    build_test_app(c("Empty" = path), file.path(root, "app")),
    "recognized Cerebro"
  )
  expect_false(dir.exists(file.path(root, "app")))
})

test_that("the minimum runtime Cerebro API is required before publication", {
  root <- withr::local_tempdir()
  incomplete <- new.env(parent = emptyenv())
  class(incomplete) <- c("Cerebro", "R6")
  for (method in c("getVersion", "getMetaData", "getCellNames")) {
    assign(method, function(...) NULL, envir = incomplete)
  }
  lockEnvironment(incomplete, bindings = TRUE)
  path <- file.path(root, "incomplete.crb")
  saveRDS(incomplete, path)
  app <- file.path(root, "app")
  dir.create(app)
  writeLines("KEEP", file.path(app, "marker.txt"))

  expect_error(
    build_test_app(c("Incomplete" = path), app, overwrite = TRUE),
    "recognized Cerebro"
  )
  expect_identical(readLines(file.path(app, "marker.txt")), "KEEP")
})

test_that("every mandatory runtime Cerebro method is checked", {
  expect_true(exists(".bundleRequiredCerebroMethods", mode = "character"))
  expect_true(all(
    c(
      "getGroupsWithMostExpressedGenes",
      "getMethodsForEnrichedPathways",
      "getExtraMaterialCategories"
    ) %in%
      .bundleRequiredCerebroMethods
  ))

  root <- withr::local_tempdir()
  for (method in .bundleRequiredCerebroMethods) {
    payload <- copy_cerebro_fixture_bindings(method)
    lockEnvironment(payload, bindings = TRUE)
    path <- file.path(root, paste0(method, ".crb"))
    saveRDS(payload, path)

    expect_error(
      .readBundleBackend(path),
      "recognized Cerebro",
      info = method
    )
  }
})

test_that("preflight never invokes a serialized backend getter", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "getter-was-run")
  malicious <- copy_cerebro_fixture_bindings(
    c("getExpressionBackend", "expression_backend")
  )
  malicious$getExpressionBackend <- base::local({
    marker <- sentinel
    function() {
      writeLines("EXECUTED", marker)
      list(type = "embedded", location = "matrix.h5")
    }
  })
  malicious$expression_backend <- list(
    type = "embedded",
    location = "matrix.h5"
  )
  lockEnvironment(malicious, bindings = TRUE)
  path <- file.path(root, "malicious.crb")
  saveRDS(malicious, path)

  expect_error(
    build_test_app(c("Malicious" = path), file.path(root, "app")),
    "unsupported expression-backend descriptor"
  )
  expect_false(file.exists(sentinel))
  expect_false(dir.exists(file.path(root, "app")))
})

test_that("bundle config freezes the validated field backend", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "getter-was-run")
  crb <- write_divergent_backend_crb(
    file.path(root, "source"),
    field_backend = list(type = "embedded", location = NULL),
    getter_backend = list(type = "h5", location = "../../escaped.h5"),
    sentinel = sentinel
  )
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    cerebro_options = list(
      .bundle_backend_plan = list(
        schema_version = 999L,
        entries = list(fake = "caller-controlled")
      )
    )
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  manifest <- config[[".bundle_backend_plan"]]
  expect_identical(manifest$schema_version, 1L)
  expect_named(manifest$entries, "private-data/dataset.crb")
  expect_identical(
    manifest$entries[["private-data/dataset.crb"]],
    list(type = "embedded", mode = "embedded", location = NULL)
  )
  expect_false(file.exists(sentinel))
})

test_that("configured runtime consumes the frozen plan without calling getter", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "runtime-getter-was-run")
  crb <- write_divergent_backend_crb(
    file.path(root, "source"),
    field_backend = list(type = "embedded", location = NULL),
    getter_backend = list(type = "h5", location = "../../escaped.h5"),
    sentinel = sentinel
  )
  app <- file.path(root, "app")
  build_test_app(c("Dataset" = crb), app)
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  configured_path <- unname(config$crb_file_to_load[[1L]])
  runtime <- source_bundle_runtime(app)
  local_cerebro_options(config)
  withr::local_dir(app)

  loaded <- runtime$get_or_load_crb(
    configured_path,
    config[[".bundle_backend_plan"]],
    unname(config$crb_file_to_load)
  )

  expect_true(is.environment(loaded))
  expect_false(file.exists(sentinel))
})

test_that("cached CRBs reject a different effective backend plan", {
  root <- withr::local_tempdir()
  path <- write_bundle_crb(root)
  embedded <- list(
    schema_version = 1L,
    entries = stats::setNames(
      list(list(type = "embedded", mode = "embedded", location = NULL)),
      path
    )
  )
  changed <- list(
    schema_version = 1L,
    entries = stats::setNames(
      list(list(type = "h5", mode = "bundled", location = "missing.h5")),
      path
    )
  )
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())

  first <- runtime$get_or_load_crb(path, embedded, path)
  second <- runtime$get_or_load_crb(path, embedded, path)
  expect_false(identical(first, second))
  expect_identical(first$meta_data, second$meta_data)
  expect_error(
    runtime$get_or_load_crb(path, changed, path),
    "cached CRB.*backend configuration changed"
  )
})

test_that("configured runtime fails closed when its plan entry is missing", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(file.path(root, "source"))
  app <- file.path(root, "app")
  build_test_app(c("Dataset" = crb), app)
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  configured_path <- unname(config$crb_file_to_load[[1L]])
  config[[".bundle_backend_plan"]] <- list(
    schema_version = 1L,
    entries = list()
  )
  runtime <- source_bundle_runtime(app)
  local_cerebro_options(config)
  withr::local_dir(app)

  expect_error(
    runtime$get_or_load_crb(
      configured_path,
      config[[".bundle_backend_plan"]],
      unname(config$crb_file_to_load)
    ),
    "backend plan.*configured CRB"
  )
})

test_that("configured runtime rejects malformed backend manifests", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(file.path(root, "source"))
  app <- file.path(root, "app")
  build_test_app(c("Dataset" = crb), app)
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  configured_path <- unname(config$crb_file_to_load[[1L]])
  valid_entry <- list(type = "embedded", mode = "embedded", location = NULL)
  malformed <- list(
    list(schema_version = 2L, entries = config$.bundle_backend_plan$entries),
    list(schema_version = 1L, entries = list(valid_entry)),
    list(
      schema_version = 1L,
      entries = stats::setNames(
        list(valid_entry, valid_entry),
        rep(configured_path, 2L)
      )
    ),
    list(
      schema_version = 1L,
      entries = stats::setNames(
        list(list(type = "unknown", mode = "embedded", location = NULL)),
        configured_path
      )
    ),
    list(
      schema_version = 1L,
      entries = stats::setNames(
        list(list(type = "h5", mode = "unknown", location = "matrix.h5")),
        configured_path
      )
    ),
    list(
      schema_version = 1L,
      entries = stats::setNames(
        list(list(type = "h5", mode = "bundled", location = "../matrix.h5")),
        configured_path
      )
    ),
    list(
      schema_version = 1L,
      entries = stats::setNames(
        ## The removed host-override mode is no longer a valid plan mode.
        list(list(
          type = "h5",
          mode = "host_override",
          location = "/host/matrix.h5"
        )),
        configured_path
      )
    ),
    list(
      schema_version = 1L,
      entries = stats::setNames(
        list(list(
          type = "embedded",
          mode = "embedded",
          location = "matrix.h5"
        )),
        configured_path
      )
    )
  )

  for (index in seq_along(malformed)) {
    runtime <- source_bundle_runtime(app)
    local_cerebro_options(config)
    withr::local_dir(app)
    expect_error(
      runtime$get_or_load_crb(
        configured_path,
        malformed[[index]],
        unname(config$crb_file_to_load)
      ),
      "backend plan",
      info = paste("malformed manifest", index)
    )
  }
})

test_that("runtime field fallback never invokes a serialized getter", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "fallback-getter-was-run")
  path <- write_divergent_backend_crb(
    root,
    field_backend = list(type = "embedded", location = NULL),
    getter_backend = list(type = "embedded", location = NULL),
    sentinel = sentinel
  )
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())
  object <- readRDS(path)

  attached <- runtime$.attachExternalExpression(object, path)

  expect_true(is.environment(attached))
  expect_false(file.exists(sentinel))
})

test_that("uploaded CRBs use field fallback even when a manifest exists", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "upload-getter-was-run")
  upload <- write_divergent_backend_crb(
    root,
    field_backend = list(type = "embedded", location = NULL),
    getter_backend = list(type = "h5", location = "../../escaped.h5"),
    sentinel = sentinel,
    name = "upload.crb"
  )
  configured_path <- "data/configured.crb"
  manifest <- list(
    schema_version = 1L,
    entries = stats::setNames(
      list(list(type = "embedded", mode = "embedded", location = NULL)),
      configured_path
    )
  )
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())

  loaded <- runtime$get_or_load_crb(upload, manifest, configured_path)

  expect_true(is.environment(loaded))
  expect_false(file.exists(sentinel))
})

test_that("uploaded CRBs ignore an unrelated malformed manifest", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "upload-getter-was-run")
  upload <- write_divergent_backend_crb(
    root,
    field_backend = list(type = "embedded", location = NULL),
    getter_backend = list(type = "h5", location = "../../escaped.h5"),
    sentinel = sentinel,
    name = "upload.crb"
  )
  malformed <- list(schema_version = 999L, entries = list())
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())

  loaded <- runtime$get_or_load_crb(
    upload,
    malformed,
    "data/configured.crb"
  )

  expect_true(is.environment(loaded))
  expect_false(file.exists(sentinel))
})

test_that("runtime field fallback rejects non-portable backend locations", {
  root <- withr::local_tempdir()
  invalid <- c(
    "../../escaped.h5",
    "./matrix.h5",
    "/tmp/matrix.h5",
    "~/matrix.h5",
    "C:/matrix.h5",
    "nested//matrix.h5",
    "nested\\matrix.h5",
    "matrix?.h5",
    "nested/a:b.h5",
    "NUL",
    "nested/COM1.bin",
    "nested/name.",
    "nested/name "
  )
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())

  for (index in seq_along(invalid)) {
    sentinel <- file.path(root, paste0("fallback-getter-was-run-", index))
    path <- write_divergent_backend_crb(
      file.path(root, paste0("case-", index)),
      field_backend = list(type = "h5", location = invalid[[index]]),
      getter_backend = list(type = "embedded", location = NULL),
      sentinel = sentinel
    )
    object <- readRDS(path)

    expect_error(
      runtime$.attachExternalExpression(object, path),
      "portable relative path",
      info = invalid[[index]]
    )
    expect_false(file.exists(sentinel))
  }
})

test_that("preflight rejects field-only modern backend state", {
  root <- withr::local_tempdir()
  payload <- copy_cerebro_fixture_bindings("getExpressionBackend")
  payload$expression_backend <- list(type = "embedded", location = NULL)
  lockEnvironment(payload, bindings = TRUE)
  path <- file.path(root, "field-only.crb")
  saveRDS(payload, path)

  expect_error(
    build_test_app(c("Field only" = path), file.path(root, "app")),
    "unsupported expression-backend descriptor"
  )
})

test_that("preflight rejects getter-only modern backend state", {
  root <- withr::local_tempdir()
  payload <- copy_cerebro_fixture_bindings("expression_backend")
  lockEnvironment(payload, bindings = TRUE)
  path <- file.path(root, "getter-only.crb")
  saveRDS(payload, path)

  expect_error(
    build_test_app(c("Getter only" = path), file.path(root, "app")),
    "unsupported expression-backend descriptor"
  )
})

test_that("runtime fallback rejects mixed backend binding states", {
  root <- withr::local_tempdir()
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())
  cases <- list(
    copy_cerebro_fixture_bindings("getExpressionBackend"),
    copy_cerebro_fixture_bindings("expression_backend")
  )
  cases[[1L]]$expression_backend <- list(type = "embedded", location = NULL)
  cases[[2L]]$getExpressionBackend <- function() {
    list(type = "embedded", location = NULL)
  }

  for (index in seq_along(cases)) {
    lockEnvironment(cases[[index]], bindings = TRUE)
    ## A getter without its field reads as a CRB with no descriptor.
    expect_error(
      runtime$.attachExternalExpression(
        cases[[index]],
        file.path(root, paste0("mixed-", index, ".crb"))
      ),
      "unsupported expression-backend descriptor|retired format"
    )
  }
})

test_that("runtime fallback rejects active and lazy bindings without forcing", {
  root <- withr::local_tempdir()
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())
  cases <- expand.grid(
    binding = c("getExpressionBackend", "expression_backend"),
    kind = c("active", "lazy"),
    stringsAsFactors = FALSE
  )

  for (index in seq_len(nrow(cases))) {
    binding <- cases$binding[[index]]
    kind <- cases$kind[[index]]
    sentinel <- file.path(root, paste(binding, kind, "was-forced", sep = "-"))
    payload <- copy_cerebro_fixture_bindings(binding)
    value <- if (identical(binding, "getExpressionBackend")) {
      function() list(type = "embedded", location = NULL)
    } else {
      list(type = "embedded", location = NULL)
    }
    if (identical(kind, "active")) {
      makeActiveBinding(
        binding,
        base::local({
          marker <- sentinel
          binding_value <- value
          function(replacement) {
            if (!missing(replacement)) {
              stop("fixture binding is read-only")
            }
            writeLines("FORCED", marker)
            binding_value
          }
        }),
        payload
      )
    } else {
      add_lazy_cerebro_binding(payload, binding, value, sentinel)
    }
    lockEnvironment(payload, bindings = TRUE)

    expect_error(
      runtime$.attachExternalExpression(
        payload,
        file.path(root, paste0(binding, "-", kind, ".crb"))
      ),
      "unsupported expression-backend descriptor|retired format"
    )
    expect_false(file.exists(sentinel))
  }
})

test_that("preflight rejects active backend bindings without reading them", {
  root <- withr::local_tempdir()

  for (binding in c("getExpressionBackend", "expression_backend")) {
    sentinel <- file.path(root, paste0(binding, "-was-read"))
    payload <- copy_cerebro_fixture_bindings(
      c("getExpressionBackend", "expression_backend")
    )
    if (identical(binding, "getExpressionBackend")) {
      payload$expression_backend <- list(
        type = "embedded",
        location = NULL
      )
    } else {
      payload$getExpressionBackend <- function() {
        list(type = "embedded", location = NULL)
      }
    }
    makeActiveBinding(
      binding,
      base::local({
        marker <- sentinel
        value <- if (identical(binding, "getExpressionBackend")) {
          function() list(type = "embedded", location = NULL)
        } else {
          list(type = "embedded", location = NULL)
        }
        function(replacement) {
          if (!missing(replacement)) {
            stop("fixture binding is read-only")
          }
          writeLines("READ", marker)
          value
        }
      }),
      payload
    )
    lockEnvironment(payload, bindings = TRUE)
    path <- file.path(root, paste0(binding, ".crb"))
    saveRDS(payload, path)

    expect_error(
      build_test_app(
        c("Active" = path),
        file.path(root, paste0(binding, "-app"))
      ),
      "unsupported expression-backend descriptor"
    )
    expect_false(file.exists(sentinel))
  }
})

test_that("legacy detection rejects an active backend field", {
  root <- withr::local_tempdir()
  sentinel <- file.path(root, "legacy-field-was-read")
  payload <- copy_cerebro_fixture_bindings(
    c("getExpressionBackend", "expression_backend")
  )
  makeActiveBinding(
    "expression_backend",
    base::local({
      marker <- sentinel
      function(replacement) {
        if (!missing(replacement)) {
          stop("fixture binding is read-only")
        }
        writeLines("READ", marker)
        list(type = "embedded", location = NULL)
      }
    }),
    payload
  )
  lockEnvironment(payload, bindings = TRUE)
  path <- file.path(root, "active-legacy-field.crb")
  saveRDS(payload, path)

  expect_error(
    build_test_app(c("Active legacy" = path), file.path(root, "app")),
    "unsupported expression-backend descriptor"
  )
  expect_false(file.exists(sentinel))
})

test_that("preflight rejects lazy CRB bindings without forcing them", {
  root <- withr::local_tempdir()
  cases <- list(
    list(
      binding = "getVersion",
      value = function() NULL,
      error = "recognized Cerebro"
    ),
    list(
      binding = "getExpressionBackend",
      value = function() list(type = "embedded", location = NULL),
      error = "unsupported expression-backend descriptor"
    ),
    list(
      binding = "expression_backend",
      value = list(type = "embedded", location = NULL),
      error = "unsupported expression-backend descriptor"
    )
  )

  for (case in cases) {
    binding <- case$binding
    sentinel <- file.path(root, paste0(binding, "-was-forced"))
    payload <- copy_cerebro_fixture_bindings(binding)
    add_lazy_cerebro_binding(
      payload,
      binding,
      case$value,
      sentinel
    )
    lockEnvironment(payload, bindings = TRUE)
    path <- file.path(root, paste0("lazy-", binding, ".crb"))
    saveRDS(payload, path)

    expect_error(
      build_test_app(
        c("Lazy" = path),
        file.path(root, paste0(binding, "-app"))
      ),
      case$error
    )
    expect_false(file.exists(sentinel))
  }
})

test_that("a missing tagged backend stops the build", {
  root <- withr::local_tempdir()
  backend <- list(type = "h5", location = "matrix.h5")
  crb <- write_bundle_crb(root, backend = backend)

  expect_error(
    build_test_app(c("Dataset" = crb), file.path(root, "app")),
    "matrix.h5"
  )
})

test_that("failed preflight leaves an existing deployment untouched", {
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  dir.create(app)
  sentinel <- file.path(app, "sentinel.txt")
  writeLines("KEEP", sentinel)
  crb <- write_bundle_crb(
    file.path(root, "source"),
    backend = list(type = "h5", location = "missing.h5")
  )

  expect_error(
    build_test_app(c("Dataset" = crb), app, overwrite = TRUE),
    "missing.h5"
  )
  expect_true(file.exists(sentinel))
  if (file.exists(sentinel)) {
    expect_identical(readLines(sentinel), "KEEP")
  }
  expect_false(dir.exists(file.path(app, "shiny")))
})

test_that("inputs inside result_dir survive until the staged copy completes", {
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  input_dir <- file.path(app, "inputs")
  crb <- write_bundle_crb(input_dir)

  build_test_app(c("Dataset" = crb), app, overwrite = TRUE)

  expect_true(file.exists(file.path(app, "private-data", "dataset.crb")))
  expect_false(dir.exists(file.path(app, "inputs")))
})

test_that("native resolver canonicalizes reparse prefixes without readlink", {
  path_exists <- function(path) {
    path %in% c("C:/work", "C:/work/host-link")
  }
  normalize_existing <- function(path) {
    if (identical(path, "C:/work/host-link")) {
      return("D:/published")
    }
    path
  }

  resolved <- .resolveNativeSymbolicLinks(
    "C:/work/host-link/app/data/../matrix.h5",
    style = "drive",
    os_type = "windows",
    read_link = function(path) "",
    path_exists = path_exists,
    normalize_existing = normalize_existing,
    entry_exists = function(path) FALSE
  )

  expect_identical(resolved, "D:/published/app/matrix.h5")
})

test_that("native resolver rejects opaque broken filesystem entries", {
  path_exists <- function(path) identical(path, "C:/work")
  entry_exists <- function(path) identical(path, "C:/work/host-link")

  expect_error(
    .resolveNativeSymbolicLinks(
      "C:/work/host-link/app/data/matrix.h5",
      style = "drive",
      os_type = "windows",
      read_link = function(path) "",
      path_exists = path_exists,
      normalize_existing = identity,
      entry_exists = entry_exists
    ),
    "cannot be resolved safely"
  )
})

test_that("one CRB cannot be reused under two labels", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(file.path(root, "source"))
  duplicate_paths <- list(
    identical = crb,
    lexical_alias = file.path(dirname(crb), ".", basename(crb))
  )

  for (case_name in names(duplicate_paths)) {
    app <- file.path(root, paste0("app-", case_name))
    dir.create(app)
    marker <- file.path(app, "marker.txt")
    writeLines("KEEP", marker)

    expect_error(
      build_test_app(
        c(
          "First label" = crb,
          "Second label" = duplicate_paths[[case_name]]
        ),
        app
      ),
      "resolve to the same Cerebro data file"
    )

    expect_identical(
      list.files(app, all.files = TRUE, no.. = TRUE),
      "marker.txt"
    )
    marker_exists <- file.exists(marker)
    expect_true(marker_exists)
    if (marker_exists) {
      expect_identical(readLines(marker), "KEEP")
    }
    expect_length(
      list.files(
        root,
        all.files = TRUE,
        no.. = TRUE,
        pattern = paste0(
          "^\\.",
          basename(app),
          "-(build\\.lock|stage-)"
        )
      ),
      0L
    )
  }
})

test_that("different sources cannot write the same private bundle target", {
  root <- withr::local_tempdir()
  backend <- list(type = "h5", location = "matrix.h5")
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  first <- write_bundle_crb(first_dir, "first.crb", backend)
  second <- write_bundle_crb(second_dir, "second.crb", backend)
  write_backend_artifact(first_dir, backend, "FIRST")
  write_backend_artifact(second_dir, backend, "SECOND")

  expect_error(
    build_test_app(
      c("First" = first, "Second" = second),
      file.path(root, "app")
    ),
    "same bundle target"
  )

  upper_backend <- list(type = "h5", location = "MATRIX.H5")
  upper <- write_bundle_crb(second_dir, "upper.crb", upper_backend)
  write_backend_artifact(second_dir, upper_backend, "UPPER")
  expect_error(
    build_test_app(
      c("First" = first, "Upper" = upper),
      file.path(root, "case-app")
    ),
    "same bundle target"
  )

  cross_backend <- list(type = "h5", location = "second.crb")
  cross_first <- write_bundle_crb(first_dir, "cross-first.crb", cross_backend)
  write_backend_artifact(first_dir, cross_backend)
  expect_error(
    build_test_app(
      c("First" = cross_first, "Second" = second),
      file.path(root, "cross-app")
    ),
    "same bundle target"
  )
})

test_that("private and spatial targets may share a basename", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(
    file.path(root, "source"),
    "first.png"
  )
  image <- file.path(root, "image", "first.png")
  dir.create(dirname(image))
  writeLines("IMAGE", image)
  app <- file.path(root, "app")

  suppressWarnings(build_test_app(
    c("First" = crb),
    app,
    spatial_images = list(
      First = list(section = c("Tissue background" = image))
    )
  ))

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_true(file.exists(file.path(app, "private-data", "first.png")))
  expect_identical(
    config$spatial_images$First$section[["Tissue background"]],
    expected_spatial_image_target(
      "First",
      "section",
      "Tissue background",
      "first.png"
    )
  )
  expect_identical(
    readLines(file.path(
      app,
      config$spatial_images$First$section[["Tissue background"]]
    )),
    "IMAGE"
  )
})

test_that("different datasets isolate equal spatial image basenames", {
  root <- withr::local_tempdir()
  first_crb <- write_spatial_bundle_crb(
    file.path(root, "first-crb"),
    "first.crb"
  )
  second_crb <- write_spatial_bundle_crb(
    file.path(root, "second-crb"),
    "second.crb"
  )
  first <- file.path(root, "first", "histology.png")
  second <- file.path(root, "second", "histology.png")
  dir.create(dirname(first))
  dir.create(dirname(second))
  writeLines("FIRST", first)
  writeLines("SECOND", second)

  app <- file.path(root, "app")
  build_test_app(
    c("First" = first_crb, "Second" = second_crb),
    app,
    spatial_images = list(
      First = list(section = c("Tissue background" = first)),
      Second = list(section = c("Tissue background" = second))
    )
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(
    config$spatial_images$First$section[["Tissue background"]],
    expected_spatial_image_target(
      "First",
      "section",
      "Tissue background",
      "histology.png"
    )
  )
  expect_identical(
    config$spatial_images$Second$section[["Tissue background"]],
    expected_spatial_image_target(
      "Second",
      "section",
      "Tissue background",
      "histology.png"
    )
  )
})

test_that("duplicate spatial image data set names are rejected", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(file.path(root, "source"))
  first <- file.path(root, "first.png")
  second <- file.path(root, "second.png")
  writeLines("FIRST", first)
  writeLines("SECOND", second)

  expect_error(
    build_test_app(
      c("Dataset" = crb),
      file.path(root, "app"),
      spatial_images = list(
        Dataset = list(section = c("Tissue background" = first)),
        Dataset = list(section = c("Tissue background" = second))
      )
    ),
    "spatial_images names must be unique"
  )
})

test_that("spatial images and settings preserve dataset spatial image nesting", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(
    file.path(root, "source"),
    spatials = list(`section-a` = "Embedded", `section-b` = character())
  )
  first <- file.path(root, "first.png")
  second <- file.path(root, "second.jpg")
  writeLines("FIRST", first)
  writeLines("SECOND", second)
  app <- file.path(root, "app")

  build_test_app(
    c(Dataset = crb),
    app,
    spatial_images = list(
      Dataset = list(
        `section-a` = c(`H&E` = first),
        `section-b` = list(
          DAPI = list(
            path = second,
            bounds = c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
          )
        )
      )
    ),
    spatial_image_settings = list(
      Dataset = list(
        `section-a` = list(
          `H&E` = list(
            flip_x = TRUE,
            scale_x = 1.5,
            rotation = 90,
            image_opacity = 0.7
          )
        ),
        `section-b` = list(
          DAPI = list(
            flip_y = FALSE,
            scale_y = 0.5,
            offset_x = 2,
            offset_y = -3
          )
        )
      )
    )
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(
    config$spatial_images$Dataset[["section-a"]][["H&E"]],
    expected_spatial_image_target("Dataset", "section-a", "H&E", "first.png")
  )
  expect_identical(
    config$spatial_images$Dataset[["section-b"]]$DAPI,
    list(
      path = file.path(
        expected_spatial_image_target(
          "Dataset",
          "section-b",
          "DAPI",
          "second.jpg"
        )
      ),
      bounds = c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
    )
  )
  expect_identical(
    config$spatial_image_settings$Dataset[["section-a"]][["H&E"]],
    list(
      flip_x = TRUE,
      scale_x = 1.5,
      rotation = 90,
      image_opacity = 0.7
    )
  )
  expect_identical(
    config$spatial_image_settings$Dataset[["section-b"]]$DAPI,
    list(flip_y = FALSE, scale_y = 0.5, offset_x = 2, offset_y = -3)
  )
  expect_true(file.exists(file.path(
    app,
    config$spatial_images$Dataset[["section-a"]][["H&E"]]
  )))
})

test_that("spatial image bundle targets use encoded portable components", {
  target <- .spatialImageBundleTarget(
    "Dataset",
    "section",
    "Image",
    "image.png"
  )

  expect_identical(
    target,
    "spatial-assets/u44617461736574/u73656374696f6e/u496d616765/u696d6167652e706e67.png"
  )
  expect_false(grepl("\\\\", target))
})

test_that("retired per-page scatter options are rejected, not dropped", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(root, "data.crb")
  app <- file.path(root, "app")
  expect_error(
    build_test_app(
      c(Dataset = crb),
      app,
      cerebro_options = list(
        expression_point_size = c(Dataset = 3),
        expression_point_opacity = c(Dataset = 0.5)
      )
    ),
    "expression_point_size, expression_point_opacity.*point_size"
  )
  expect_false(dir.exists(app))
})

test_that("builder-owned spatial options cannot bypass the parameters", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  keys <- c(
    "spatial_images",
    "spatial_image_settings"
  )

  for (key in keys) {
    app <- file.path(root, paste0("bypass-", key))
    expect_error(
      build_test_app(
        c(Dataset = crb),
        app,
        cerebro_options = stats::setNames(list(TRUE), key)
      ),
      paste0("cerebro_options.*", key, ".*createShinyApp[(][)] parameters")
    )
    expect_false(dir.exists(app))
  }

  image <- file.path(root, "image.png")
  writeLines("IMAGE", image)
  app <- file.path(root, "conflict")
  expect_error(
    build_test_app(
      c(Dataset = crb),
      app,
      spatial_images = list(
        Dataset = list(section = c(Histology = image))
      ),
      cerebro_options = list(
        spatial_images = list(bypass = TRUE),
        spatial_image_settings = list(bypass = TRUE)
      )
    ),
    "cerebro_options.*spatial_images.*spatial_image_settings.*createShinyApp[(][)] parameters"
  )
  expect_false(dir.exists(app))
})

test_that("spatial image preflight rejects unknown and conflicting references", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(
    file.path(root, "source"),
    spatials = list(first = "Embedded", second = character())
  )
  image <- file.path(root, "image.png")
  writeLines("IMAGE", image)

  expect_error(
    build_test_app(
      c(Dataset = crb),
      file.path(root, "unknown-dataset"),
      spatial_images = list(Typo = list(first = c(Image = image)))
    ),
    "dataset `Typo`.*cerebro_data"
  )
  expect_error(
    build_test_app(
      c(Dataset = crb),
      file.path(root, "unknown-spatial"),
      spatial_images = list(Dataset = list(typo = c(Image = image)))
    ),
    "spatial `typo`.*available.*first.*second"
  )
  expect_error(
    build_test_app(
      c(Dataset = crb),
      file.path(root, "embedded-conflict"),
      spatial_images = list(Dataset = list(first = c(Embedded = image)))
    ),
    "first.*Embedded.*embedded"
  )
  expect_error(
    build_test_app(
      c(Dataset = crb),
      file.path(root, "unknown-image"),
      spatial_image_settings = list(
        Dataset = list(first = list(Missing = list(flip_x = TRUE)))
      )
    ),
    "settings.*Missing.*not exist"
  )
})

test_that("spatial image targets distinguish equal basenames and preserve logical names", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(
    file.path(root, "source"),
    spatials = stats::setNames(list(character()), "section/a")
  )
  first <- file.path(root, "first", "histology.png")
  second <- file.path(root, "second", "histology.png")
  dir.create(dirname(first))
  dir.create(dirname(second))
  writeLines("FIRST", first)
  writeLines("SECOND", second)

  app <- file.path(root, "app")
  build_test_app(
    c("Patient 1: baseline" = crb),
    app,
    spatial_images = list(
      "Patient 1: baseline" = list(
        "section/a" = c("H&E baseline" = first, "H&E follow-up" = second)
      )
    )
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  images <- config$spatial_images[["Patient 1: baseline"]][["section/a"]]

  expect_named(images, c("H&E baseline", "H&E follow-up"))
  expect_false(identical(images[[1L]], images[[2L]]))
  expect_true(file.exists(file.path(app, images[[1L]])))
  expect_true(file.exists(file.path(app, images[[2L]])))
  expect_identical(readLines(file.path(app, images[[1L]])), "FIRST")
  expect_identical(readLines(file.path(app, images[[2L]])), "SECOND")
})

test_that("spatial image targets bound long logical names and filenames", {
  root <- withr::local_tempdir()
  dataset <- paste(rep("dataset", 25L), collapse = "-")
  spatial_name <- paste(rep("spatial", 25L), collapse = "-")
  image_label <- paste(rep("image", 35L), collapse = "-")
  filename <- paste0(paste(rep("x", 180L), collapse = ""), ".png")
  crb <- write_spatial_bundle_crb(
    file.path(root, "source"),
    spatials = stats::setNames(list(character()), spatial_name)
  )
  image <- file.path(root, filename)
  writeLines("IMAGE", image)
  app <- file.path(root, "app")

  build_test_app(
    stats::setNames(crb, dataset),
    app,
    spatial_images = stats::setNames(
      list(stats::setNames(
        list(stats::setNames(image, image_label)),
        spatial_name
      )),
      dataset
    )
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  target <- config$spatial_images[[dataset]][[spatial_name]][[image_label]]
  expect_true(file.exists(file.path(app, target)))
  expect_true(all(
    nchar(strsplit(target, "/", fixed = TRUE)[[1L]], type = "bytes") <= 255L
  ))
  ## Keep enough headroom for an ordinary Windows app directory, plus its
  ## sibling staging directory, beneath the 260-character MAX_PATH limit.
  ## Per-segment validity alone is insufficient when four encoded logical
  ## identifiers are nested.
  expect_lte(nchar(target, type = "bytes"), 120L)
  expect_match(target, "\\.png$")
})

test_that("spatial image settings accept only strict scalar fields", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(
    file.path(root, "source"),
    spatials = list(section = "Embedded")
  )
  cases <- list(
    list(settings = list(opacity = 0.5), error = "unknown setting.*opacity"),
    list(
      settings = list(image_opacity = 1.1),
      error = "image_opacity.*between 0 and 1"
    ),
    list(settings = list(flip_x = 1), error = "flip_x.*logical scalar"),
    list(settings = list(flip_y = NA), error = "flip_y.*logical scalar"),
    list(settings = list(scale_x = Inf), error = "scale_x.*finite numeric"),
    list(
      settings = list(rotation = c(0, 90)),
      error = "rotation.*finite numeric"
    )
  )

  for (i in seq_along(cases)) {
    case <- cases[[i]]
    expect_error(
      build_test_app(
        c(Dataset = crb),
        file.path(root, paste0("app-", i)),
        spatial_image_settings = list(
          Dataset = list(section = list(Embedded = case$settings))
        )
      ),
      case$error
    )
  }
})

test_that("the old c(Dataset = path) spatial_images form is rejected", {
  root <- withr::local_tempdir()
  image <- file.path(root, "histology.png")
  writeLines("IMAGE", image)
  single <- write_spatial_bundle_crb(
    file.path(root, "single"),
    spatials = list(section = character())
  )

  for (declared in list(c(Dataset = image), list(Dataset = image))) {
    app <- file.path(root, "single-app")
    expect_error(
      build_test_app(c(Dataset = single), app, spatial_images = declared),
      "spatial_images.*c[(]label = path[)].*no longer accepted"
    )
    expect_false(dir.exists(app))
  }
})

test_that("Cerebro objects missing spatial accessors are rejected", {
  root <- withr::local_tempdir()
  malformed <- copy_cerebro_fixture_bindings(
    c("spatial", "addSpatialData", "availableSpatial", "getSpatialData")
  )
  lockEnvironment(malformed, bindings = FALSE)
  crb <- file.path(root, "malformed-current.crb")
  saveRDS(malformed, crb)

  ## The format gate names the missing accessors as a pre-spatial object ...
  expect_error(
    build_test_app(c(Dataset = crb), file.path(root, "app")),
    "retired format: it predates spatial support"
  )
  ## ... and the catalog reader still refuses an object without them.
  expect_error(
    .readBundleSpatialCatalog(malformed, "Dataset"),
    "has no spatial accessor methods"
  )
})

test_that("partial, active, and lazy spatial accessors are rejected", {
  root <- withr::local_tempdir()
  partial <- copy_cerebro_fixture_bindings("getSpatialData")
  active <- copy_cerebro_fixture_bindings("availableSpatial")
  lazy <- copy_cerebro_fixture_bindings("getSpatialData")
  sentinel <- file.path(root, "lazy-accessor-was-forced")

  makeActiveBinding(
    "availableSpatial",
    function(replacement) {
      if (!missing(replacement)) {
        stop("fixture binding is read-only")
      }
      character()
    },
    active
  )
  add_lazy_cerebro_binding(lazy, "getSpatialData", function(...) NULL, sentinel)

  cases <- list(partial = partial, active = active, lazy = lazy)
  for (case_name in names(cases)) {
    payload <- cases[[case_name]]
    lockEnvironment(payload, bindings = FALSE)
    path <- file.path(root, paste0(case_name, ".crb"))
    saveRDS(payload, path)
    expect_error(
      build_test_app(
        c(Dataset = path),
        file.path(root, paste0(case_name, "-app"))
      ),
      "invalid spatial accessor methods",
      info = case_name
    )
  }
  expect_false(file.exists(sentinel))
})

test_that("several labelled images of one spatial entry bundle every image", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  paths <- file.path(root, c("first.png", "second.png"))
  writeLines("FIRST", paths[[1L]])
  writeLines("SECOND", paths[[2L]])
  app <- file.path(root, "app")

  build_test_app(
    c(Dataset = crb),
    app,
    spatial_images = list(
      Dataset = list(section = c(first = paths[[1L]], second = paths[[2L]]))
    )
  )

  images <- readRDS(file.path(
    app,
    "cerebro_config.rds"
  ))$spatial_images$Dataset$section
  expect_named(images, c("first", "second"))
  expect_identical(readLines(file.path(app, images[[1L]])), "FIRST")
  expect_identical(readLines(file.path(app, images[[2L]])), "SECOND")
})

test_that("one spatial image can be shared by multiple data sets", {
  root <- withr::local_tempdir()
  first <- write_spatial_bundle_crb(file.path(root, "first"), "first.crb")
  second <- write_spatial_bundle_crb(file.path(root, "second"), "second.crb")
  image <- file.path(root, "histology.png")
  writeLines("IMAGE", image)
  app <- file.path(root, "app")

  build_test_app(
    c("First" = first, "Second" = second),
    app,
    spatial_images = list(
      First = list(section = c("Tissue background" = image)),
      Second = list(section = c("Tissue background" = image))
    )
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(
    unname(unlist(config$spatial_images, use.names = FALSE)),
    c(
      expected_spatial_image_target(
        "First",
        "section",
        "Tissue background",
        "histology.png"
      ),
      expected_spatial_image_target(
        "Second",
        "section",
        "Tissue background",
        "histology.png"
      )
    )
  )
  expect_identical(
    readLines(file.path(
      app,
      config$spatial_images$First$section[["Tissue background"]]
    )),
    "IMAGE"
  )
})

test_that("external spatial images render from disk without an HTTP mapping", {
  skip_if_not_installed("base64enc")
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  image <- file.path(root, "histology.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), image)
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    spatial_images = list(
      Dataset = list(section = c("Tissue background" = image))
    )
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  stored <- config$spatial_images$Dataset$section[["Tissue background"]]
  expect_identical(
    stored,
    expected_spatial_image_target(
      "Dataset",
      "section",
      "Tissue background",
      "histology.png"
    )
  )

  session <- fake_private_session()
  rendered_meta <- render_bundle_spatial_background(
    app,
    config,
    stored,
    stored,
    session = session
  )

  registered <- attr(session, "registered")
  ## The image is served from its bundled file through a session-scoped URL:
  ## never as a data URI, and never as a filesystem path the browser could read.
  expect_identical(
    rendered_meta$background_image,
    paste0("session/cerebro-image-", registered$data$md5)
  )
  expect_false(grepl(app, rendered_meta$background_image, fixed = TRUE))
  expect_identical(
    registered$data$path,
    normalizePath(file.path(app, stored), winslash = "/")
  )
  expect_identical(registered$data$mime, "image/png")
  expect_gt(registered$data$bytes, 0)
})

test_that("spatial backgrounds need a resolved descriptor, not a path", {
  skip_if_not_installed("base64enc")
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  image <- file.path(root, "histology.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), image)
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    spatial_images = list(
      Dataset = list(section = c("Tissue background" = image))
    )
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  stored <- config$spatial_images$Dataset$section[["Tissue background"]]
  ## Even a configured, allowlisted path draws nothing when no descriptor was
  ## resolved for the current dataset / spatial / image selection.
  session <- fake_private_session()
  rendered_meta <- render_bundle_spatial_background(
    app,
    config,
    stored,
    stored,
    session = session,
    background_descriptor = NULL
  )
  expect_null(rendered_meta$background_image)
  expect_null(attr(session, "registered")$name)
})

test_that("forged spatial backgrounds cannot read unconfigured files", {
  skip_if_not_installed("base64enc")
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  image <- file.path(root, "histology.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)), image)
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    spatial_images = list(
      Dataset = list(section = c("Tissue background" = image))
    )
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  allowed <- config$spatial_images$Dataset$section[["Tissue background"]]
  outside <- file.path(root, "outside-secret.png")
  unconfigured <- file.path(app, "spatial-assets", "unconfigured.png")
  writeLines("OUTSIDE-PRIVATE-SENTINEL", outside)
  writeLines("UNCONFIGURED-PRIVATE-SENTINEL", unconfigured)

  attacks <- list(
    "private-data/dataset.crb" = c(
      allowed,
      "private-data/dataset.crb"
    ),
    "../outside-secret.png" = c(allowed, "../outside-secret.png"),
    "spatial-assets/unconfigured.png" = allowed
  )
  for (attack in names(attacks)) {
    ## With a session available the refusal has to come from the allowlist and
    ## the containment check, not from a missing private-URL handler.
    rendered_meta <- render_bundle_spatial_background(
      app,
      config,
      attack,
      attacks[[attack]],
      session = fake_private_session()
    )
    expect_null(
      rendered_meta$background_image,
      info = paste("forged background was returned:", attack)
    )
  }

  linked <- file.path(app, "spatial-assets", "linked.png")
  if (isTRUE(file.symlink(outside, linked))) {
    rendered_meta <- render_bundle_spatial_background(
      app,
      config,
      "spatial-assets/linked.png",
      c(allowed, "spatial-assets/linked.png"),
      session = fake_private_session()
    )
    expect_null(
      rendered_meta$background_image,
      info = "configured symlink escaped the public spatial-assets directory"
    )
  }
})

test_that("unknown spatial image datasets fail before bundling", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  matched <- file.path(root, "matched.png")
  unmatched <- file.path(root, "unmatched.png")
  writeLines("MATCHED", matched)
  writeLines("UNMATCHED", unmatched)
  app <- file.path(root, "app")

  expect_error(
    build_test_app(
      c("Dataset" = crb),
      app,
      spatial_images = list(
        Dataset = list(section = c("Tissue background" = matched)),
        Typo = list(section = c("Tissue background" = unmatched))
      )
    ),
    "dataset `Typo`.*cerebro_data"
  )
  expect_false(dir.exists(app))
})

test_that("missing spatial images fail before bundling", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  missing <- file.path(root, "missing.png")
  app <- file.path(root, "app")

  expect_error(
    build_test_app(
      c("Dataset" = crb),
      app,
      spatial_images = list(
        Dataset = list(section = c("Tissue background" = missing))
      )
    ),
    "does not exist"
  )
  expect_false(dir.exists(app))
})

test_that("parent and child bundle targets conflict before copying", {
  root <- withr::local_tempdir()
  tree_backend <- list(type = "h5", location = "tree")
  nested_backend <- list(type = "h5", location = "tree/injected.h5")
  first_dir <- file.path(root, "first")
  second_dir <- file.path(root, "second")
  first <- write_bundle_crb(first_dir, "first.crb", tree_backend)
  second <- write_bundle_crb(second_dir, "second.crb", nested_backend)
  write_backend_artifact(first_dir, tree_backend, "FIRST")
  write_backend_artifact(second_dir, nested_backend, "SECOND")

  expect_error(
    build_test_app(
      c("First" = first, "Second" = second),
      file.path(root, "app")
    ),
    "parent or child"
  )
})

test_that("backend paths cannot resolve through symbolic links", {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  outside <- file.path(root, "outside")
  dir.create(source)
  dir.create(outside)
  writeLines("OUTSIDE", file.path(outside, "matrix.h5"))
  linked <- file.symlink(outside, file.path(source, "alias"))
  if (!isTRUE(linked)) {
    skip("Symbolic links are not available on this platform")
  }
  crb <- write_bundle_crb(
    source,
    backend = list(type = "h5", location = "alias/matrix.h5")
  )

  expect_error(
    build_test_app(c("Dataset" = crb), file.path(root, "app")),
    "symbolic link"
  )

  app <- file.path(root, "existing-app")
  dir.create(app)
  destination_linked <- file.symlink(
    outside,
    file.path(app, "private-data")
  )
  if (!isTRUE(destination_linked)) {
    skip("Destination symbolic links are not available on this platform")
  }
  embedded <- write_bundle_crb(file.path(root, "embedded"))

  build_test_app(c("Dataset" = embedded), app, overwrite = TRUE)

  expect_identical(
    readLines(file.path(outside, "matrix.h5")),
    "OUTSIDE"
  )
  expect_false(.pathIsSymbolicLink(file.path(app, "private-data")))
  expect_true(file.exists(file.path(
    app,
    "private-data",
    "dataset.crb"
  )))
})

test_that("embedded CRBs do not require sibling files", {
  root <- withr::local_tempdir()
  embedded <- write_bundle_crb(
    file.path(root, "embedded"),
    "embedded.crb"
  )
  app <- file.path(root, "app")

  build_test_app(c("Embedded" = embedded), app)

  expect_true(file.exists(file.path(app, "private-data", "embedded.crb")))
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_bundle_backend_entry(
    config,
    "private-data/embedded.crb",
    list(type = "embedded", mode = "embedded", location = NULL)
  )
})

test_that("old configs attach through field fallback", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  modern_source <- file.path(root, "modern")
  expected <- write_real_h5_matrix(
    file.path(modern_source, "matrix.h5")
  )
  modern <- write_bundle_crb(
    modern_source,
    "modern.crb",
    backend = list(type = "h5", location = "matrix.h5")
  )
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())

  modern_attached <- runtime$get_or_load_crb(modern, NULL, modern)
  expect_attached_matrix(modern_attached, expected)
})

## ---- Retired formats ----------------------------------------------------- ##

retired_payload <- function(modify = function(payload) payload) {
  payload <- copy_cerebro_fixture_bindings()
  payload <- modify(payload)
  lockEnvironment(payload, bindings = FALSE)
  payload
}

retired_crb_cases <- function() {
  list(
    no_descriptor = list(
      payload = function() {
        retired_payload(function(payload) {
          payload$expression_backend <- NULL
          payload
        })
      },
      reason = "no expression-backend descriptor"
    ),
    bcr_data = list(
      payload = function() {
        retired_payload(function(payload) {
          payload$bcr_data <- list(
            donor = data.frame(barcode = "cell-1", CTgene = "IGHV1_IGKV1")
          )
          payload
        })
      },
      reason = "bcr_data/tcr_data"
    ),
    tcr_data = list(
      payload = function() {
        retired_payload(function(payload) {
          payload$tcr_data <- list(
            donor = data.frame(barcode = "cell-1", CTgene = "TRAV1_TRBV1")
          )
          payload
        })
      },
      reason = "bcr_data/tcr_data"
    ),
    pre_spatial_v1_3 = list(
      payload = function() legacy_cerebro_v1_3_fixture(),
      reason = "predates spatial support"
    ),
    schema_v2 = list(
      payload = function() {
        retired_payload(function(payload) {
          payload$expression_backend <- list(
            type = "h5",
            location = "dataset.h5"
          )
          payload$crb_schema <- list(
            version = 2L,
            cell_names = c("cell-1", "cell-2"),
            cell_names_md5 = strrep("a", 32L)
          )
          payload
        })
      },
      reason = "thin-CRB schema version 2"
    ),
    retired_immune_file = list(
      payload = function() {
        retired_payload(function(payload) {
          payload$immune_repertoire_backend <- list(
            type = "retired-file",
            file = "dataset.immune.qs2",
            md5 = strrep("a", 32L)
          )
          payload
        })
      },
      reason = "immune repertoire sidecar uses a retired layout"
    )
  )
}

test_that("retired CRB formats are rejected with the convertCerebro message", {
  root <- withr::local_tempdir()
  cases <- retired_crb_cases()
  ## A form convertCerebro() cannot repair says to re-export instead.
  retired <- paste0(
    "retired format: .*(",
    "Convert it once with CerebroNexus::convertCerebro\\(\\)|",
    "convertCerebro\\(\\) cannot repair it; re-export it from the source",
    ")"
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    crb <- file.path(root, paste0(case_name, ".crb"))
    saveRDS(case$payload(), crb)
    app <- file.path(root, paste0(case_name, "-app"))

    expect_error(
      build_test_app(c(Retired = crb), app),
      retired,
      info = case_name
    )
    expect_error(
      build_test_app(c(Retired = crb), app),
      case$reason,
      info = case_name
    )
    expect_false(dir.exists(app), info = case_name)
    expect_error(.readBundleBackend(crb), retired, info = case_name)
    expect_error(
      .preflightBundleData(c(Retired = crb)),
      retired,
      info = case_name
    )
    expect_error(
      .datasetCatalogFromFiles(c(Retired = crb)),
      retired,
      info = case_name
    )
    expect_error(
      .datasetCatalogFromFiles(c(Retired = crb)),
      case$reason,
      info = case_name
    )
  }
})

test_that("a CRB with an unknown expression-backend type is not bundled", {
  root <- withr::local_tempdir()
  crb <- file.path(root, "unknown-backend.crb")
  saveRDS(
    retired_payload(function(payload) {
      payload$expression_backend <- list(
        type = "retired-store",
        location = "dataset.retired"
      )
      payload
    }),
    crb
  )

  expect_error(
    build_test_app(c(Unknown = crb), file.path(root, "app")),
    "unsupported expression-backend descriptor"
  )
  expect_false(dir.exists(file.path(root, "app")))
  expect_error(
    .readBundleBackend(crb),
    "unsupported expression-backend descriptor"
  )
})

test_that("a CRB without the descriptor members is also a retired format", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(root, "no-members.crb", legacy = TRUE)

  expect_error(
    build_test_app(c(Retired = crb), file.path(root, "app")),
    "retired format: it has no expression-backend descriptor.*convertCerebro"
  )
  expect_error(
    .readBundleBackend(crb),
    "retired format: it has no expression-backend descriptor"
  )
  expect_error(
    .datasetCatalogFromFiles(c(Retired = crb)),
    "retired format: it has no expression-backend descriptor"
  )
})

test_that("a retired CRB stops the whole build before anything is staged", {
  root <- withr::local_tempdir()
  current <- write_bundle_crb(file.path(root, "current"), "current.crb")
  retired <- write_retired_crb(file.path(root, "retired"), "retired.crb")
  app <- file.path(root, "app")

  expect_error(
    build_test_app(c(Current = current, Retired = retired), app),
    "'retired.crb' uses a retired format"
  )
  expect_false(dir.exists(app))
})

test_that("a pre-spatial Cerebro object cannot be given external spatial images", {
  root <- withr::local_tempdir()
  crb <- file.path(root, "legacy-non-spatial.crb")
  image <- file.path(root, "histology.png")
  saveRDS(legacy_cerebro_v1_3_fixture(), crb)
  writeLines("IMAGE", image)

  expect_error(
    build_test_app(
      c(Legacy = crb),
      file.path(root, "app"),
      spatial_images = c(Legacy = image)
    ),
    "retired format: it predates spatial support"
  )
})

## ---- Immune repertoire and thin H5 CRBs ----------------------------------- ##

immune_test_repertoire <- function(
  chain = "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1"
) {
  list(
    donorA = data.frame(
      barcode = c("cell-1", "cell-2"),
      CTgene = chain,
      CTaa = c("CAVRF_CASSLF", "CAVQF_CASSMF"),
      stringsAsFactors = FALSE
    )
  )
}

## An embedded CRB with its immune repertoire in a sidecar next to it.
write_immune_sidecar_crb <- function(
  directory,
  name = "dataset.crb",
  chain = "TRAV1.TRAJ2.TRAC_TRBV3.TRBJ1.TRBC1"
) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  object <- new_embedded_cerebro()
  cells <- c("cell-1", "cell-2")
  object$setMetaData(data.frame(
    cell_barcode = cells,
    row.names = cells,
    stringsAsFactors = FALSE
  ))
  object$addImmuneRepertoire(immune_test_repertoire(chain))
  crb <- file.path(directory, name)
  saveCerebro(
    object,
    crb,
    codec = "rds",
    viewer_binary = "never",
    immune_sidecar = "always"
  )
  crb
}

test_that("an immune repertoire sidecar is bundled beside the CRB copy", {
  root <- withr::local_tempdir()
  crb <- write_immune_sidecar_crb(file.path(root, "source"))
  sidecar <- file.path(root, "source", "dataset.immune.rds")
  expect_true(file.exists(sidecar))
  descriptor <- readRDS(crb)$immune_repertoire_backend
  expect_identical(descriptor$type, "file")
  expect_identical(descriptor$file, "dataset.immune.rds")
  app <- file.path(root, "app")

  build_test_app(c(Dataset = crb), app)

  bundled <- file.path(app, "private-data", "dataset.immune.rds")
  expect_true(file.exists(file.path(app, "private-data", "dataset.crb")))
  expect_true(file.exists(bundled))
  expect_identical(
    unname(tools::md5sum(bundled)),
    unname(tools::md5sum(sidecar))
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_bundle_backend_entry(
    config,
    "private-data/dataset.crb",
    list(type = "embedded", mode = "embedded", location = NULL)
  )
  entry <- config[[".dataset_catalog"]][["private-data/dataset.crb"]]
  expect_true(entry$immune_repertoire)
  expect_true(entry$tcr_repertoire)
  ## The bundled pair opens as a unit.
  reloaded <- readCerebro(file.path(app, "private-data", "dataset.crb"))
  expect_named(reloaded$getImmuneRepertoire(), "donorA")
})

test_that("preflight reports the immune sidecar descriptor of each CRB", {
  root <- withr::local_tempdir()
  with_sidecar <- write_immune_sidecar_crb(file.path(root, "with"))
  plain <- write_bundle_crb(file.path(root, "plain"), "plain.crb")

  preflight <- .preflightBundleData(c(With = with_sidecar, Plain = plain))

  expect_named(preflight$immune_backends, c("With", "Plain"))
  expect_identical(preflight$immune_backends$With$type, "file")
  expect_identical(preflight$immune_backends$With$file, "dataset.immune.rds")
  expect_identical(
    normalizePath(preflight$immune_backends$With$root, winslash = "/"),
    normalizePath(dirname(with_sidecar), winslash = "/")
  )
  expect_null(preflight$immune_backends$Plain)
})

test_that("a missing immune repertoire sidecar fails preflight", {
  root <- withr::local_tempdir()
  crb <- write_immune_sidecar_crb(file.path(root, "source"))
  unlink(file.path(root, "source", "dataset.immune.rds"))
  app <- file.path(root, "app")

  expect_error(
    .preflightBundleData(c(Dataset = crb)),
    "immune repertoire sidecar is missing"
  )
  expect_error(
    build_test_app(c(Dataset = crb), app),
    "immune repertoire sidecar"
  )
  expect_false(dir.exists(app))
})

test_that("equal immune sidecar names from different directories collide", {
  root <- withr::local_tempdir()
  first <- write_immune_sidecar_crb(file.path(root, "first"), "sample.crb")
  second <- write_immune_sidecar_crb(file.path(root, "second"), "sample.crb")
  app <- file.path(root, "app")

  expect_error(
    build_test_app(c(First = first, Second = second), app),
    "resolve to the same bundle target"
  )
  expect_false(dir.exists(app))
})

test_that("a symlinked immune repertoire sidecar is refused", {
  root <- withr::local_tempdir()
  crb <- write_immune_sidecar_crb(file.path(root, "source"))
  sidecar <- file.path(root, "source", "dataset.immune.rds")
  real <- file.path(root, "elsewhere.rds")
  file.copy(sidecar, real)
  unlink(sidecar)
  linked <- suppressWarnings(file.symlink(real, sidecar))
  if (!isTRUE(linked)) {
    skip("Symbolic links are not available on this platform")
  }
  app <- file.path(root, "app")

  expect_error(
    build_test_app(c(Dataset = crb), app),
    "immune repertoire sidecar.*symbolic link"
  )
  expect_false(dir.exists(app))
})

test_that("an immune sidecar reported as a symbolic link is refused", {
  root <- withr::local_tempdir()
  crb <- write_immune_sidecar_crb(file.path(root, "source"))
  app <- file.path(root, "app")
  ## Windows cannot always create links, so report the sidecar as one.
  testthat::local_mocked_bindings(
    .pathIsSymbolicLink = function(path, ...) {
      identical(basename(path), "dataset.immune.rds")
    },
    .package = "CerebroNexus"
  )

  expect_error(
    build_test_app(c(Dataset = crb), app),
    "immune repertoire sidecar.*symbolic link"
  )
  expect_false(dir.exists(app))
})

test_that("dataset information reads immune chains from a file descriptor", {
  cells <- c("cell-1", "cell-2")
  describe <- function(chains) {
    object <- new_embedded_cerebro()
    object$setMetaData(data.frame(
      cell_barcode = cells,
      row.names = cells,
      stringsAsFactors = FALSE
    ))
    object$immune_repertoire_backend <- list(
      type = "file",
      file = "dataset.immune.rds",
      md5 = strrep("a", 32L),
      samples = "donorA",
      chains = chains
    )
    .readBundleDatasetInfo(object, "Dataset", "dataset.crb")
  }

  tcr <- describe(c("TRA", "TRB"))
  expect_true(tcr$immune_repertoire)
  expect_true(tcr$tcr_repertoire)
  bcr <- describe(c("IGH", "IGK"))
  expect_true(bcr$immune_repertoire)
  expect_false(bcr$tcr_repertoire)
  empty <- describe(character())
  expect_true(empty$immune_repertoire)
  expect_false(empty$tcr_repertoire)
})

## An H5-backed object saved as a thin schema-v3 CRB.
write_thin_h5_crb <- function(
  directory,
  name = "dataset.crb",
  immune_sidecar = "never"
) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  stem <- tools::file_path_sans_ext(name)
  expected <- write_real_h5_matrix(file.path(directory, paste0(stem, ".h5")))
  cells <- colnames(expected)
  object <- Cerebro$new()
  object$setMetaData(data.frame(
    cell_barcode = cells,
    group = c("A", "A", "B", "B"),
    row.names = cells,
    stringsAsFactors = FALSE
  ))
  object$addProjection(
    "umap",
    data.frame(x = 1:4, y = 4:1, row.names = cells)
  )
  object$setExpressionBackend(type = "h5", location = paste0(stem, ".h5"))
  object$addImmuneRepertoire(list(
    donorA = data.frame(
      barcode = cells[1:2],
      CTgene = "IGHV1.IGHJ1.IGHC_IGKV1.IGKJ1.IGKC",
      stringsAsFactors = FALSE
    )
  ))
  crb <- file.path(directory, name)
  saveCerebro(
    object,
    crb,
    codec = "rds",
    viewer_binary = "never",
    immune_sidecar = immune_sidecar
  )
  list(crb = crb, expected = expected)
}

test_that("a thin H5 CRB is bundled with its H5 file and keeps its gene count", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  root <- withr::local_tempdir()
  built <- write_thin_h5_crb(file.path(root, "source"))
  expect_identical(readRDS(built$crb)$crb_schema$version, 3L)
  app <- file.path(root, "app")

  build_test_app(c(Dataset = built$crb), app)

  expect_setequal(
    list.files(file.path(app, "private-data")),
    c("dataset.crb", "dataset.h5")
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_bundle_backend_entry(
    config,
    "private-data/dataset.crb",
    list(type = "h5", mode = "bundled", location = "dataset.h5")
  )
  ## A thin CRB carries no matrix: the gene count comes from the H5 shape.
  entry <- config[[".dataset_catalog"]][["private-data/dataset.crb"]]
  expect_identical(entry$cells, ncol(built$expected))
  expect_identical(entry$overview$genes, nrow(built$expected))
  info <- .readBundleDatasetInfo(
    .readCerebroPayload(built$crb),
    "Dataset",
    built$crb
  )
  expect_identical(info$overview$genes, nrow(built$expected))
  expect_identical(info$cells, ncol(built$expected))

  ## The bundled pair attaches at runtime with its cell names restored.
  unlink(file.path(root, "source"), recursive = TRUE, force = TRUE)
  configured_path <- unname(config$crb_file_to_load[[1L]])
  runtime <- source_bundle_runtime(app)
  local_cerebro_options(config)
  withr::local_dir(app)
  attached <- runtime$get_or_load_crb(
    configured_path,
    config[[".bundle_backend_plan"]],
    unname(config$crb_file_to_load)
  )
  expect_identical(
    as.character(attached$meta_data$cell_barcode),
    colnames(built$expected)
  )
  expect_attached_matrix(attached, built$expected)
})

test_that("a thin H5 CRB bundles its H5 file and immune sidecar together", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  root <- withr::local_tempdir()
  built <- write_thin_h5_crb(
    file.path(root, "source"),
    immune_sidecar = "always"
  )
  expect_true(file.exists(file.path(root, "source", "dataset.immune.rds")))
  app <- file.path(root, "app")

  build_test_app(c(Dataset = built$crb), app)

  expect_setequal(
    list.files(file.path(app, "private-data")),
    c("dataset.crb", "dataset.h5", "dataset.immune.rds")
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  entry <- config[[".dataset_catalog"]][["private-data/dataset.crb"]]
  expect_true(entry$immune_repertoire)
  expect_false(entry$tcr_repertoire)
})

test_that("process cache clones keep thin H5 expression lazy", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("rhdf5")
  root <- withr::local_tempdir()
  built <- write_thin_h5_crb(file.path(root, "source"))
  app <- file.path(root, "app")
  build_test_app(c(Dataset = built$crb), app)
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  configured_path <- unname(config$crb_file_to_load[[1L]])
  runtime <- source_bundle_runtime(app)
  local_cerebro_options(config)
  withr::local_dir(app)

  first <- runtime$get_or_load_crb(
    configured_path,
    config[[".bundle_backend_plan"]],
    unname(config$crb_file_to_load)
  )
  second <- runtime$get_or_load_crb(
    configured_path,
    config[[".bundle_backend_plan"]],
    unname(config$crb_file_to_load)
  )

  expect_false(identical(first, second))
  expect_true(rlang::env_binding_are_lazy(first, "expression"))
  expect_true(rlang::env_binding_are_lazy(second, "expression"))
  expect_attached_matrix(first, built$expected)
  expect_true(rlang::env_binding_are_lazy(second, "expression"))
})

test_that("overwrite FALSE rejects a non-empty destination without mutation", {
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  dir.create(app)
  sentinel <- file.path(app, "sentinel.txt")
  writeLines("KEEP", sentinel)
  crb <- write_bundle_crb(file.path(root, "source"))

  expect_error(
    build_test_app(
      c("Dataset" = crb),
      app,
      overwrite = FALSE
    ),
    "non-empty"
  )
  expect_identical(
    list.files(app, all.files = TRUE, no.. = TRUE),
    "sentinel.txt"
  )
  expect_identical(readLines(sentinel), "KEEP")
})

test_that("staged replacement preserves deployment root permissions", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  app <- file.path(root, "app")
  dir.create(app)
  Sys.chmod(app, mode = "0750")
  expect_identical(as.character(file.info(app)$mode), "750")
  crb <- write_bundle_crb(file.path(root, "source"))

  build_test_app(c("Dataset" = crb), app, overwrite = TRUE)

  expect_identical(as.character(file.info(app)$mode), "750")
})

test_that("named lists of scalar paths are accepted", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(file.path(root, "source"))
  app <- file.path(root, "app")

  build_test_app(list("Dataset" = crb), app)

  expect_true(file.exists(file.path(app, "private-data", "dataset.crb")))
})

test_that("Cerebro data labels cannot be missing", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(file.path(root, "source"))
  app <- file.path(root, "app")

  expect_error(
    build_test_app(setNames(crb, NA_character_), app),
    "identities must be non-empty and non-missing"
  )
  expect_false(dir.exists(app))
})

test_that("Cerebro data labels must be unique", {
  root <- withr::local_tempdir()
  first <- write_bundle_crb(file.path(root, "first"), "first.crb")
  second <- write_bundle_crb(file.path(root, "second"), "second.crb")
  app <- file.path(root, "app")

  expect_error(
    build_test_app(c("Dataset" = first, "Dataset" = second), app),
    "identities must be unique"
  )
  expect_false(dir.exists(app))
})

test_that("at least one Cerebro data set is required", {
  root <- withr::local_tempdir()

  expect_error(
    build_test_app(
      setNames(character(), character()),
      file.path(root, "app")
    ),
    "at least one"
  )
})

test_that("a bundled real H5 backend attaches with exact data", {
  skip_if_not_installed("HDF5Array")
  skip_if_not_installed("Matrix")

  expect_real_backend_bundle_roundtrip()
})

test_that("the runtime rejects an H5 backend that is a directory", {
  skip_if_not_installed("HDF5Array")
  runtime <- source_bundle_runtime()
  root <- withr::local_tempdir()
  dir.create(file.path(root, "matrix.h5"))
  object <- new.env(parent = emptyenv())
  class(object) <- "Cerebro"
  for (accessor in names(runtime$.runtimeRequiredCerebroMethods())) {
    object[[accessor]] <- function(...) NULL
  }
  object$expression_backend <- list(type = "h5", location = "matrix.h5")
  object$getExpressionBackend <- function() {
    list(type = "h5", location = "matrix.h5")
  }
  had_options <- exists(
    "Cerebro.options",
    envir = .GlobalEnv,
    inherits = FALSE
  )
  if (had_options) {
    old_options <- get("Cerebro.options", envir = .GlobalEnv)
  }
  withr::defer(
    if (had_options) {
      assign("Cerebro.options", old_options, envir = .GlobalEnv)
    } else {
      rm(list = "Cerebro.options", envir = .GlobalEnv)
    }
  )
  assign("Cerebro.options", list(), envir = .GlobalEnv)

  expect_error(
    runtime$.attachExternalExpression(
      object,
      file.path(root, "dataset.crb")
    ),
    "missing or not a file"
  )
})

test_that("colliding CRB basenames keep each pack beside its final bundled name", {
  for (both in c(FALSE, TRUE)) {
    root <- tempfile("bundle-pack-collision-")
    dir.create(root)
    on.exit(unlink(root, recursive = TRUE), add = TRUE)
    sources <- file.path(root, c("study-a", "study-b"), "sample.crb")
    for (i in seq_along(sources)) {
      dir.create(dirname(sources[i]))
      object <- Cerebro$new()
      object$setMetaData(data.frame(
        cell_barcode = c("c1", "c2"),
        group = c("A", "B")
      ))
      object$addProjection(
        "umap",
        data.frame(x = c(i, i + 1), y = 2:1, row.names = c("c1", "c2"))
      )
      saveCerebro(
        object,
        sources[i],
        codec = "rds",
        viewer_binary = if (i == 1L || both) "always" else "never"
      )
    }
    labels <- c("Study A", "Study B")
    app <- file.path(root, "app")
    build_test_app(stats::setNames(sources, labels), app)
    scope <- new.env(parent = globalenv())
    sys.source(viewer_test_path("core", "viewer_pack.R"), scope)
    for (i in seq_along(sources)) {
      target <- file.path(app, CerebroNexus:::.datasetBundleTarget(labels[i]))
      expect_true(file.exists(target))
      pack <- scope$viewerPackOpen(target, readCerebro(target))
      if (i == 1L || both) {
        expect_true(validateViewerPack(target)$valid)
        expect_false(is.null(pack))
        expect_identical(scope$viewerPackCellBarcodes(pack), c("c1", "c2"))
      } else {
        expect_null(pack)
      }
    }
  }
})

test_that("the retired host path overrides are rejected in cerebro_options", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))

  for (key in c("expression_matrix_h5", "expression_matrix_BPCells")) {
    app <- file.path(root, paste0("retired-", key))
    expect_error(
      build_test_app(
        c(Dataset = crb),
        app,
        cerebro_options = stats::setNames(list("somewhere"), key)
      ),
      paste0("retired key.*", key)
    )
    expect_false(dir.exists(app))
  }
})
