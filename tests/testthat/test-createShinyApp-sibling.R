copy_cerebro_fixture_bindings <- function(exclude = character()) {
  source <- Cerebro$new()
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
  object <- Cerebro$new()
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
  if (identical(backend$type, "bpcells")) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    writeLines(contents, file.path(path, "payload"))
  } else {
    writeLines(contents, path)
  }
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
  runtime <- new.env(parent = globalenv())
  sys.source(
    viewer_test_path("utility_functions.R"),
    envir = runtime
  )
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

render_bundle_spatial_background <- function(
  app,
  config,
  background_image,
  background_image_allowlist
) {
  renderer <- new.env(parent = globalenv())
  renderer$Cerebro.options <- config
  sys.source(
    file.path(app, "viewer", "utility_functions.R"),
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
  utility <- if (is.null(app)) {
    testthat::test_path("../../inst/viewer/utility_functions.R")
  } else {
    file.path(app, "viewer", "utility_functions.R")
  }
  if (!file.exists(utility)) {
    utility <- system.file(
      "viewer",
      "utility_functions.R",
      package = "CerebroNexus"
    )
  }
  stopifnot(file.exists(utility))
  runtime <- new.env(parent = globalenv())
  sys.source(utility, envir = runtime)
  runtime
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

override_backend_cases <- function() {
  list(
    h5 = list(
      key = "expression_matrix_h5",
      type = "h5",
      leaf = "host-matrix.h5"
    ),
    bpcells = list(
      key = "expression_matrix_BPCells",
      type = "bpcells",
      leaf = "host-matrix.bpcells"
    )
  )
}

override_options <- function(key, path) {
  stats::setNames(list(path), key)
}

write_override_artifact <- function(path, type, contents = "HOST") {
  if (identical(type, "bpcells")) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
    writeLines(contents, file.path(path, "payload"))
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(contents, path)
  }
  path
}

read_override_artifact <- function(path, type) {
  if (identical(type, "bpcells")) {
    readLines(file.path(path, "payload"))
  } else {
    readLines(path)
  }
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

expect_real_backend_bundle_roundtrip <- function(type) {
  root <- withr::local_tempdir()
  source <- file.path(root, "source")
  dir.create(source)
  location <- if (identical(type, "h5")) {
    "expression.h5"
  } else {
    "expression.bpcells"
  }
  backend_path <- file.path(source, location)
  object <- Cerebro$new()
  object$setMetaData(data.frame(
    group = c("A", "A", "B", "B"),
    row.names = c("Cell1", "Cell2", "Cell3", "Cell4")
  ))

  if (identical(type, "h5")) {
    expected <- write_real_h5_matrix(backend_path)
  } else {
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
    BPCells::write_matrix_dir(
      mat = methods::as(sparse, "IterableMatrix"),
      dir = backend_path
    )
    object$setExpression(
      BPCells::open_matrix_dir(dir = backend_path),
      backend = "external"
    )
  }
  object$setExpressionBackend(type = type, location = location)
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
    list(type = type, mode = "bundled", location = location)
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

test_that("tagged H5 and BPCells backends are copied to their exact paths", {
  root <- withr::local_tempdir()
  backends <- list(
    list(type = "h5", location = "matrices/original-name.h5"),
    list(type = "bpcells", location = "matrices/expression.bpcells")
  )

  for (index in seq_along(backends)) {
    source <- file.path(root, paste0("source-", index))
    backend <- backends[[index]]
    crb <- write_bundle_crb(
      source,
      name = "renamed-dataset.crb",
      backend = backend
    )
    write_backend_artifact(source, backend)
    app <- file.path(root, paste0("app-", index))

    build_test_app(c("Dataset" = crb), app)

    expect_true(file.exists(file.path(
      app,
      "private-data",
      "renamed-dataset.crb"
    )))
    target <- file.path(app, "private-data", backend$location)
    if (identical(backend$type, "bpcells")) {
      expect_true(dir.exists(target))
      expect_identical(
        readLines(file.path(target, "payload")),
        "MATRIX"
      )
    } else {
      expect_true(file.exists(target))
      expect_identical(readLines(target), "MATRIX")
    }
    config <- readRDS(file.path(app, "cerebro_config.rds"))
    expect_bundle_backend_entry(
      config,
      "private-data/renamed-dataset.crb",
      list(
        type = backend$type,
        mode = "bundled",
        location = backend$location
      )
    )
    expect_false(file.exists(file.path(
      app,
      "private-data",
      "renamed-dataset.h5"
    )))
  }
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
      accepted = TRUE
    ),
    null = list(
      object = new_backend_contract_object(NULL),
      accepted = TRUE
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
    bpcells = list(
      object = new_backend_contract_object(
        list(type = "bpcells", location = "matrix.bpcells")
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

test_that("build and runtime share the backend override decision contract", {
  root <- normalizePath(
    withr::local_tempdir(),
    winslash = "/",
    mustWork = TRUE
  )
  h5_override <- file.path(root, "host-matrix.h5")
  bpcells_override <- file.path(root, "host-matrix.bpcells")
  writeLines("HOST", h5_override)
  dir.create(bpcells_override)
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())
  embedded <- list(type = "embedded", location = NULL, legacy = FALSE)
  h5 <- list(type = "h5", location = "matrix.h5", legacy = FALSE)
  bpcells <- list(
    type = "bpcells",
    location = "matrix.bpcells",
    legacy = FALSE
  )
  legacy <- list(type = "embedded", location = NULL, legacy = TRUE)
  h5_options <- list(expression_matrix_h5 = h5_override)
  bpcells_options <- list(expression_matrix_BPCells = bpcells_override)
  both_options <- c(h5_options, bpcells_options)
  cases <- list(
    embedded = list(backend = embedded, options = list()),
    embedded_ignores_override = list(
      backend = embedded,
      options = h5_options
    ),
    h5_bundled = list(backend = h5, options = list()),
    h5_override = list(backend = h5, options = h5_options),
    h5_ignores_bpcells = list(backend = h5, options = bpcells_options),
    bpcells_bundled = list(backend = bpcells, options = list()),
    bpcells_override = list(
      backend = bpcells,
      options = bpcells_options
    ),
    bpcells_ignores_h5 = list(backend = bpcells, options = h5_options),
    legacy_embedded = list(backend = legacy, options = list()),
    legacy_h5 = list(backend = legacy, options = h5_options),
    legacy_bpcells = list(backend = legacy, options = bpcells_options),
    legacy_prefers_h5 = list(backend = legacy, options = both_options)
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    object <- new_backend_contract_object(
      case$backend[c("type", "location")],
      legacy = isTRUE(case$backend$legacy)
    )
    build <- .effectiveBundleBackendPlan(case$backend, case$options)
    assign("Cerebro.options", case$options, envir = .GlobalEnv)
    app_runtime <- runtime$.fallbackRuntimeBackendPlan(
      object,
      file.path(root, paste0(case_name, ".crb"))
    )

    expect_identical(
      app_runtime[c("type", "mode")],
      build[c("type", "mode")],
      info = case_name
    )
  }
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

test_that("runtime overrides do not bypass backend path validation", {
  root <- withr::local_tempdir()

  for (case_name in names(override_backend_cases())) {
    case <- override_backend_cases()[[case_name]]
    case_root <- file.path(root, case_name)
    crb <- write_bundle_crb(
      file.path(case_root, "source"),
      backend = list(
        type = case$type,
        location = paste0(case$leaf, "/")
      )
    )
    override <- write_override_artifact(
      file.path(case_root, "external", case$leaf),
      case$type
    )
    app <- file.path(case_root, "app")

    expect_error(
      build_test_app(
        c("Dataset" = crb),
        app,
        cerebro_options = override_options(case$key, override)
      ),
      "portable relative path"
    )
    expect_false(dir.exists(app))
  }
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
    spatial_images = list("Dataset" = reserved_image)
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
      list(list(
        type = "h5",
        mode = "host_override",
        location = file.path(root, "missing.h5")
      )),
      path
    )
  )
  runtime <- source_bundle_runtime()
  local_cerebro_options(list())

  first <- runtime$get_or_load_crb(path, embedded, path)
  expect_identical(runtime$get_or_load_crb(path, embedded, path), first)
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
        list(list(
          type = "h5",
          mode = "host_override",
          location = "relative/matrix.h5"
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
    expect_error(
      runtime$.attachExternalExpression(
        cases[[index]],
        file.path(root, paste0("mixed-", index, ".crb"))
      ),
      "unsupported expression-backend descriptor"
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
      "unsupported expression-backend descriptor"
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

test_that("runtime override paths must be absolute for both backends", {
  root <- withr::local_tempdir()

  for (case_name in names(override_backend_cases())) {
    case <- override_backend_cases()[[case_name]]
    source <- file.path(root, case_name, "source")
    crb <- write_bundle_crb(
      source,
      backend = list(type = case$type, location = "missing-backend")
    )
    app <- file.path(root, case_name, "app")

    expect_error(
      build_test_app(
        c("Dataset" = crb),
        app,
        cerebro_options = override_options(case$key, case$leaf)
      ),
      "absolute path"
    )
    expect_false(dir.exists(app))
  }
})

test_that("forward-slash UNC overrides are rejected outside Windows", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  cases <- override_backend_cases()
  unc_paths <- c(
    "//server/share/matrix.h5",
    "//tmp/share/matrix.h5"
  )

  for (index in seq_along(cases)) {
    case <- cases[[index]]
    case_root <- file.path(root, names(cases)[[index]])
    crb <- write_bundle_crb(
      file.path(case_root, "source"),
      backend = list(type = case$type, location = "missing-backend")
    )
    app <- file.path(case_root, "app")

    expect_error(
      build_test_app(
        c("Dataset" = crb),
        app,
        cerebro_options = override_options(case$key, unc_paths[[index]])
      ),
      "Forward-slash UNC.*Windows"
    )
    expect_false(dir.exists(app))
  }

  backslash <- intToUtf8(92L)
  backslash_unc <- paste0(
    backslash,
    backslash,
    "server",
    backslash,
    "share",
    backslash,
    "matrix.h5"
  )
  drive <- "C:/host-managed/matrix.h5"
  expect_identical(
    .normalizeOverridePath(backslash_unc, "expression_matrix_h5"),
    backslash_unc
  )
  expect_identical(
    .normalizeOverridePath(drive, "expression_matrix_h5"),
    drive
  )
})

test_that("runtime overrides cannot target the result tree before publishing", {
  root <- withr::local_tempdir()
  withr::local_dir(root)

  for (case_name in names(override_backend_cases())) {
    case <- override_backend_cases()[[case_name]]
    for (relation in c("equal", "descendant")) {
      case_root <- file.path(case_name, relation)
      source <- file.path(root, case_root, "source")
      crb <- write_bundle_crb(
        source,
        backend = list(type = case$type, location = "missing-backend")
      )
      app <- file.path(case_root, "app")
      dir.create(app, recursive = TRUE)
      app_abs <- normalizePath(app, winslash = "/", mustWork = TRUE)
      sentinel <- file.path(app_abs, "sentinel.txt")
      writeLines("KEEP", sentinel)
      override <- if (identical(relation, "equal")) {
        app_abs
      } else {
        write_override_artifact(
          file.path(app_abs, case$leaf),
          case$type,
          contents = paste(case_name, relation, sep = "-")
        )
      }

      expect_error(
        build_test_app(
          c("Dataset" = crb),
          app,
          overwrite = TRUE,
          cerebro_options = override_options(case$key, override)
        ),
        "outside 'result_dir'"
      )
      expect_true(file.exists(sentinel))
      if (file.exists(sentinel)) {
        expect_identical(readLines(sentinel), "KEEP")
      }
      if (identical(relation, "descendant")) {
        artifact_exists <- if (identical(case$type, "bpcells")) {
          dir.exists(override)
        } else {
          file.exists(override)
        }
        expect_true(artifact_exists)
        if (artifact_exists) {
          expect_identical(
            read_override_artifact(override, case$type),
            paste(case_name, relation, sep = "-")
          )
        }
      }
    }
  }
})

test_that("runtime override symlinks cannot resolve into the result tree", {
  root <- withr::local_tempdir()

  for (case_name in names(override_backend_cases())) {
    case <- override_backend_cases()[[case_name]]
    case_root <- file.path(root, case_name)
    source <- file.path(case_root, "source")
    crb <- write_bundle_crb(
      source,
      backend = list(type = case$type, location = "missing-backend")
    )
    app <- file.path(case_root, "app")
    dir.create(app, recursive = TRUE)
    sentinel <- file.path(app, "sentinel.txt")
    writeLines("KEEP", sentinel)
    target <- write_override_artifact(
      file.path(app, case$leaf),
      case$type,
      contents = case_name
    )
    override <- file.path(case_root, paste0(case_name, "-override"))
    linked <- file.symlink(target, override)
    if (!isTRUE(linked)) {
      skip("Symbolic links are not available on this platform")
    }

    expect_error(
      build_test_app(
        c("Dataset" = crb),
        app,
        overwrite = TRUE,
        cerebro_options = override_options(case$key, override)
      ),
      "outside 'result_dir'"
    )
    expect_true(file.exists(sentinel))
    if (file.exists(sentinel)) {
      expect_identical(readLines(sentinel), "KEEP")
    }
    target_exists <- if (identical(case$type, "bpcells")) {
      dir.exists(target)
    } else {
      file.exists(target)
    }
    expect_true(target_exists)
    if (target_exists) {
      expect_identical(read_override_artifact(target, case$type), case_name)
    }
  }
})

test_that("broken override symlinks cannot become result-tree paths", {
  root <- withr::local_tempdir()

  for (case_name in names(override_backend_cases())) {
    case <- override_backend_cases()[[case_name]]
    for (link_kind in c("absolute", "relative")) {
      case_root <- file.path(root, case_name, link_kind)
      crb <- write_bundle_crb(
        file.path(case_root, "source"),
        backend = list(type = case$type, location = "missing-backend")
      )
      app <- file.path(case_root, "app")
      linked_parent <- file.path(case_root, "host-link")
      link_target <- if (identical(link_kind, "absolute")) {
        file.path(app, "private-data")
      } else {
        file.path("app", "private-data")
      }
      linked <- file.symlink(link_target, linked_parent)
      if (!isTRUE(linked)) {
        skip("Symbolic links are not available on this platform")
      }
      override <- file.path(linked_parent, case$leaf)
      expect_false(file.exists(override))

      expect_error(
        build_test_app(
          c("Dataset" = crb),
          app,
          cerebro_options = override_options(case$key, override)
        ),
        "outside 'result_dir'"
      )
      expect_false(dir.exists(app))
    }
  }
})

test_that("broken external override symlinks remain valid host paths", {
  root <- withr::local_tempdir()
  crb <- write_bundle_crb(
    file.path(root, "source"),
    backend = list(type = "h5", location = "missing.h5")
  )
  external <- file.path(root, "host", "future")
  linked_parent <- file.path(root, "host-link")
  linked <- file.symlink(external, linked_parent)
  if (!isTRUE(linked)) {
    skip("Symbolic links are not available on this platform")
  }
  override <- file.path(linked_parent, "matrix.h5")
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    cerebro_options = list(expression_matrix_h5 = override)
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expected <- file.path(
    normalizePath(root, winslash = "/", mustWork = TRUE),
    "host",
    "future",
    "matrix.h5"
  )
  expect_identical(
    config$expression_matrix_h5,
    expected
  )
  expect_false(file.exists(config$expression_matrix_h5))
})

test_that("override symlink cycles fail instead of hanging", {
  root <- withr::local_tempdir()
  first <- file.path(root, "first")
  second <- file.path(root, "second")
  linked <- file.symlink("second", first) && file.symlink("first", second)
  if (!isTRUE(linked)) {
    skip("Symbolic links are not available on this platform")
  }

  expect_error(
    .normalizeOverridePath(
      file.path(first, "matrix.h5"),
      "expression_matrix_h5"
    ),
    "too many symbolic links"
  )
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

test_that("override canonicalization resolves symlinks before dot segments", {
  root <- withr::local_tempdir()
  outside <- file.path(root, "outside")
  app <- file.path(root, "app")
  target <- file.path(app, "subdir")
  dir.create(outside)
  dir.create(target, recursive = TRUE)
  sentinel <- file.path(app, "sentinel.txt")
  writeLines("KEEP", sentinel)
  link <- file.path(outside, "link")
  linked <- file.symlink(target, link)
  if (!isTRUE(linked)) {
    skip("Symbolic links are not available on this platform")
  }
  override <- file.path(link, "..", "future.h5")
  expect_false(file.exists(override))
  crb <- write_bundle_crb(
    file.path(root, "source"),
    backend = list(type = "h5", location = "missing.h5")
  )

  expect_error(
    build_test_app(
      c("Dataset" = crb),
      app,
      overwrite = TRUE,
      cerebro_options = list(expression_matrix_h5 = override)
    ),
    "outside 'result_dir'"
  )
  expect_true(file.exists(sentinel))
  if (file.exists(sentinel)) {
    expect_identical(readLines(sentinel), "KEEP")
  }
})

test_that("external absolute runtime overrides are normalized and preserved", {
  root <- withr::local_tempdir()

  for (case_name in names(override_backend_cases())) {
    case <- override_backend_cases()[[case_name]]
    case_root <- file.path(root, case_name)
    source <- file.path(case_root, "source")
    crb <- write_bundle_crb(
      source,
      backend = list(type = case$type, location = "missing-backend")
    )
    external <- file.path(case_root, "app2")
    dir.create(file.path(external, "lexical"), recursive = TRUE)
    target <- write_override_artifact(
      file.path(external, case$leaf),
      case$type,
      contents = case_name
    )
    override <- file.path(external, "lexical", "..", case$leaf)
    app <- file.path(case_root, "app")

    build_test_app(
      c("Dataset" = crb),
      app,
      cerebro_options = override_options(case$key, override)
    )

    config <- readRDS(file.path(app, "cerebro_config.rds"))
    expect_identical(
      config[[case$key]],
      normalizePath(target, winslash = "/", mustWork = TRUE)
    )
    expect_bundle_backend_entry(
      config,
      "private-data/dataset.crb",
      list(
        type = case$type,
        mode = "host_override",
        location = normalizePath(target, winslash = "/", mustWork = TRUE)
      )
    )
    expect_identical(read_override_artifact(target, case$type), case_name)
  }
})

test_that("non-existing absolute host-managed overrides remain supported", {
  root <- normalizePath(
    withr::local_tempdir(),
    winslash = "/",
    mustWork = TRUE
  )

  for (case_name in names(override_backend_cases())) {
    case <- override_backend_cases()[[case_name]]
    source <- file.path(root, case_name, "source")
    crb <- write_bundle_crb(
      source,
      backend = list(type = case$type, location = "missing-backend")
    )
    override <- file.path(root, case_name, "host-managed", case$leaf)
    app <- file.path(root, case_name, "app")

    build_test_app(
      c("Dataset" = crb),
      app,
      cerebro_options = override_options(case$key, override)
    )

    config <- readRDS(file.path(app, "cerebro_config.rds"))
    expect_identical(config[[case$key]], override)
    expect_bundle_backend_entry(
      config,
      "private-data/dataset.crb",
      list(type = case$type, mode = "host_override", location = override)
    )
    expect_false(file.exists(override))
  }
})

test_that("a configured runtime override skips the tagged backend copy", {
  root <- withr::local_tempdir()
  backend <- list(type = "h5", location = "missing.h5")
  crb <- write_bundle_crb(root, backend = backend)
  override <- file.path(root, "host-matrix.h5")
  writeLines("HOST", override)
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    cerebro_options = list(expression_matrix_h5 = override)
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(
    config$expression_matrix_h5,
    normalizePath(override, winslash = "/", mustWork = TRUE)
  )
  expect_bundle_backend_entry(
    config,
    "private-data/dataset.crb",
    list(
      type = "h5",
      mode = "host_override",
      location = normalizePath(override, winslash = "/", mustWork = TRUE)
    )
  )
  expect_false(file.exists(file.path(
    app,
    "private-data",
    backend$location
  )))
})

test_that("a configured host override attaches the generated H5 plan", {
  skip_if_not_installed("HDF5Array")
  root <- withr::local_tempdir()
  override <- file.path(root, "host", "matrix.h5")
  expected <- write_real_h5_matrix(override)
  crb <- write_bundle_crb(
    file.path(root, "source"),
    backend = list(type = "h5", location = "missing.h5")
  )
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    cerebro_options = list(expression_matrix_h5 = override)
  )

  config <- readRDS(file.path(app, "cerebro_config.rds"))
  configured_path <- unname(config$crb_file_to_load[[1L]])
  runtime <- source_bundle_runtime(app)
  local_cerebro_options(config)
  withr::local_dir(app)
  attached <- runtime$get_or_load_crb(
    configured_path,
    config$.bundle_backend_plan,
    unname(config$crb_file_to_load)
  )

  expect_attached_matrix(attached, expected)
})

test_that("one global override cannot serve multiple Cerebro data files", {
  root <- withr::local_tempdir()
  override <- file.path(root, "host-matrix.h5")
  writeLines("HOST", override)
  tagged_backend <- list(type = "h5", location = "missing.h5")
  first <- write_bundle_crb(
    file.path(root, "first"),
    "first.crb",
    tagged_backend
  )
  second <- write_bundle_crb(
    file.path(root, "second"),
    "second.crb",
    tagged_backend
  )

  expect_error(
    build_test_app(
      c("First" = first, "Second" = second),
      file.path(root, "tagged-app"),
      cerebro_options = list(expression_matrix_h5 = override)
    ),
    "same expression matrix"
  )

  legacy_first <- write_bundle_crb(
    file.path(root, "legacy-first"),
    "legacy-first.crb",
    legacy = TRUE
  )
  legacy_second <- write_bundle_crb(
    file.path(root, "legacy-second"),
    "legacy-second.crb",
    legacy = TRUE
  )
  expect_error(
    build_test_app(
      c("First" = legacy_first, "Second" = legacy_second),
      file.path(root, "legacy-app"),
      cerebro_options = list(expression_matrix_h5 = override)
    ),
    "same expression matrix"
  )
})

test_that("one CRB cannot be reused under two labels", {
  root <- withr::local_tempdir()
  override <- file.path(root, "host-matrix.h5")
  writeLines("HOST", override)
  crb <- write_bundle_crb(
    file.path(root, "source"),
    backend = list(type = "h5", location = "missing.h5")
  )
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
        app,
        cerebro_options = list(expression_matrix_h5 = override)
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
    spatial_images = list("First" = image)
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
    spatial_images = list("First" = first, "Second" = second)
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
      spatial_images = list("Dataset" = first, "Dataset" = second)
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

test_that("builder-owned spatial options cannot bypass formal parameters", {
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
      paste0("cerebro_options.*", key, ".*formal.*parameter")
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
    "cerebro_options.*spatial_images.*spatial_image_settings.*formal.*parameter"
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
  ## Keep enough headroom for an ordinary Windows app directory beneath the
  ## historical 260-character full-path limit. Per-segment validity alone is
  ## insufficient when four encoded logical identifiers are nested.
  expect_lte(nchar(target, type = "bytes"), 180L)
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

test_that("legacy spatial image paths require one unambiguous spatial", {
  root <- withr::local_tempdir()
  image <- file.path(root, "histology.png")
  writeLines("IMAGE", image)
  single <- write_spatial_bundle_crb(
    file.path(root, "single"),
    spatials = list(section = character())
  )
  app <- file.path(root, "single-app")

  build_test_app(
    c(Dataset = single),
    app,
    spatial_images = c(Dataset = image)
  )
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_identical(
    config$spatial_images$Dataset$section[["Tissue background"]],
    expected_spatial_image_target(
      "Dataset",
      "section",
      "Tissue background",
      "histology.png"
    )
  )
  multiple <- write_spatial_bundle_crb(
    file.path(root, "multiple"),
    spatials = list(first = character(), second = character())
  )
  expect_error(
    build_test_app(
      c(Dataset = multiple),
      file.path(root, "multiple-app"),
      spatial_images = c(Dataset = image)
    ),
    "legacy spatial_images.*first.*second"
  )
  none <- write_bundle_crb(file.path(root, "none"))
  expect_error(
    build_test_app(
      c(Dataset = none),
      file.path(root, "none-app"),
      spatial_images = c(Dataset = image)
    ),
    "legacy spatial_images.*no available spatial"
  )
})

test_that("legacy non-spatial Cerebro objects remain bundleable", {
  root <- withr::local_tempdir()
  legacy <- legacy_cerebro_v1_3_fixture()
  crb <- file.path(root, "legacy-non-spatial.crb")
  saveRDS(legacy, crb)

  app <- file.path(root, "app")
  expect_silent(build_test_app(c(Legacy = crb), app))
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_null(config$spatial_images)
})

test_that("legacy non-spatial Cerebro objects reject external spatial images clearly", {
  root <- withr::local_tempdir()
  legacy <- legacy_cerebro_v1_3_fixture()
  crb <- file.path(root, "legacy-non-spatial.crb")
  image <- file.path(root, "histology.png")
  saveRDS(legacy, crb)
  writeLines("IMAGE", image)

  expect_error(
    build_test_app(
      c(Legacy = crb),
      file.path(root, "app"),
      spatial_images = c(Legacy = image)
    ),
    "legacy spatial_images.*no available spatial"
  )
})

test_that("current Cerebro objects missing spatial accessors are rejected", {
  root <- withr::local_tempdir()
  malformed <- copy_cerebro_fixture_bindings(
    c("spatial", "addSpatialData", "availableSpatial", "getSpatialData")
  )
  lockEnvironment(malformed, bindings = FALSE)
  crb <- file.path(root, "malformed-current.crb")
  saveRDS(malformed, crb)

  expect_error(
    build_test_app(c(Dataset = crb), file.path(root, "app")),
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

test_that("unambiguous legacy multi-path vectors bundle every image", {
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  paths <- file.path(root, c("first.png", "second.png"))
  writeLines("FIRST", paths[[1L]])
  writeLines("SECOND", paths[[2L]])
  app <- file.path(root, "app")

  build_test_app(c(Dataset = crb), app, spatial_images = list(Dataset = paths))

  images <- readRDS(file.path(
    app,
    "cerebro_config.rds"
  ))$spatial_images$Dataset$section
  expect_named(images, c("Tissue background 1", "Tissue background 2"))
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
    spatial_images = list("First" = image, "Second" = image)
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
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), image)
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    spatial_images = list("Dataset" = image)
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
  app_source <- paste(readLines(file.path(app, "app.R")), collapse = "\n")
  expect_false(grepl("addResourcePath", app_source, fixed = TRUE))

  rendered_meta <- render_bundle_spatial_background(
    app,
    config,
    stored,
    stored
  )

  expect_match(rendered_meta$background_image, "^data:image/png;base64,")
  expect_gt(nchar(rendered_meta$background_image), 22L)
})

test_that("forged spatial backgrounds cannot read unconfigured files", {
  skip_if_not_installed("base64enc")
  root <- withr::local_tempdir()
  crb <- write_spatial_bundle_crb(file.path(root, "source"))
  image <- file.path(root, "histology.png")
  writeBin(as.raw(c(0x89, 0x50, 0x4e, 0x47)), image)
  app <- file.path(root, "app")

  build_test_app(
    c("Dataset" = crb),
    app,
    spatial_images = list("Dataset" = image)
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
    rendered_meta <- render_bundle_spatial_background(
      app,
      config,
      attack,
      attacks[[attack]]
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
      c(allowed, "spatial-assets/linked.png")
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
      spatial_images = list("Dataset" = matched, "Typo" = unmatched)
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
      spatial_images = list("Dataset" = missing)
    ),
    "does not exist"
  )
  expect_false(dir.exists(app))
})

test_that("parent and child bundle targets conflict before copying", {
  root <- withr::local_tempdir()
  tree_backend <- list(type = "bpcells", location = "tree")
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

test_that("embedded and legacy CRBs do not require sibling files", {
  root <- withr::local_tempdir()
  embedded <- write_bundle_crb(
    file.path(root, "embedded"),
    "embedded.crb"
  )
  legacy <- write_bundle_crb(
    file.path(root, "legacy"),
    "legacy.crb",
    legacy = TRUE
  )
  current_null <- write_bundle_crb(
    file.path(root, "current-null"),
    "current-null.crb",
    backend = NULL
  )
  app <- file.path(root, "app")

  build_test_app(
    c(
      "Embedded" = embedded,
      "Legacy" = legacy,
      "Current NULL" = current_null
    ),
    app
  )

  expect_true(file.exists(file.path(app, "private-data", "embedded.crb")))
  expect_true(file.exists(file.path(app, "private-data", "legacy.crb")))
  expect_true(file.exists(file.path(
    app,
    "private-data",
    "current-null.crb"
  )))
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  for (path in c(
    "private-data/embedded.crb",
    "private-data/legacy.crb",
    "private-data/current-null.crb"
  )) {
    expect_bundle_backend_entry(
      config,
      path,
      list(type = "embedded", mode = "embedded", location = NULL)
    )
  }
})

test_that("legacy CRBs freeze the selected global override", {
  root <- withr::local_tempdir()
  legacy <- write_bundle_crb(
    file.path(root, "source"),
    "legacy.crb",
    legacy = TRUE
  )
  override <- file.path(root, "host", "matrix.h5")
  dir.create(dirname(override), recursive = TRUE)
  writeLines("HOST", override)
  app <- file.path(root, "app")

  build_test_app(
    c("Legacy" = legacy),
    app,
    cerebro_options = list(expression_matrix_h5 = override)
  )

  normalized <- normalizePath(override, winslash = "/", mustWork = TRUE)
  config <- readRDS(file.path(app, "cerebro_config.rds"))
  expect_bundle_backend_entry(
    config,
    "private-data/legacy.crb",
    list(type = "h5", mode = "host_override", location = normalized)
  )
})

test_that("old configs and legacy overrides attach through field fallback", {
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

  override <- file.path(root, "host", "matrix.h5")
  expected_override <- write_real_h5_matrix(override)
  legacy <- write_bundle_crb(
    file.path(root, "legacy"),
    "legacy.crb",
    legacy = TRUE
  )
  local_cerebro_options(list(expression_matrix_h5 = override))

  legacy_attached <- runtime$get_or_load_crb(legacy, NULL, legacy)
  expect_attached_matrix(legacy_attached, expected_override)
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
    "labels must be non-empty and non-missing"
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
    "labels must be unique"
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

  expect_real_backend_bundle_roundtrip("h5")
})

test_that("a bundled real BPCells backend attaches with exact data", {
  skip_if_not_installed("BPCells")
  skip_if_not_installed("Matrix")

  expect_real_backend_bundle_roundtrip("bpcells")
})

test_that("the runtime rejects an H5 backend that is a directory", {
  skip_if_not_installed("HDF5Array")
  runtime <- new.env(parent = globalenv())
  source_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "viewer",
    "utility_functions.R"
  )
  if (!file.exists(source_path)) {
    source_path <- system.file(
      "viewer",
      "utility_functions.R",
      package = "CerebroNexus"
    )
  }
  expect_true(file.exists(source_path))
  sys.source(source_path, envir = runtime)
  root <- withr::local_tempdir()
  dir.create(file.path(root, "matrix.h5"))
  object <- new.env(parent = emptyenv())
  class(object) <- "Cerebro"
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
