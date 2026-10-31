builder_repo_source("prerequisite.R")
builder_repo_source("profile.R")
builder_repo_source("state.R")
builder_plan_contract_source_runtime(environment())

test_that("runtime files retain UTF-8 text in the Windows C locale", {
  withr::local_locale(c(LC_CTYPE = "C"))
  payload <- as.raw(c(0xe2, 0x80, 0xa6))
  path <- withr::local_tempfile(fileext = ".R")
  writeBin(
    c(charToRaw('loaded <- "'), payload, charToRaw('"\n')),
    path
  )
  runtime <- new.env(parent = baseenv())

  builder_source_utf8(path, runtime)

  expect_identical(
    as.integer(charToRaw(enc2utf8(runtime$loaded))),
    as.integer(payload)
  )
})

test_that("the app privacy marker must be the exact integer contract", {
  namespace <- new.env(parent = emptyenv())

  expect_identical(builder_installed_app_contract_version(namespace), 0L)

  assign(".cerebro_bundle_privacy_contract_version", 1, namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)

  namespace <- new.env(parent = emptyenv())
  assign(".cerebro_bundle_privacy_contract_version", 1L, namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 1L)

  namespace <- new.env(parent = emptyenv())
  assign(".cerebro_bundle_privacy_contract_version", c(1L, 1L), namespace)
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)
  expect_identical(builder_installed_app_contract_version(namespace), 0L)
})

test_that("installed privacy contract v1 is eager and locked", {
  namespace <- asNamespace("CerebroNexus")
  marker <- ".cerebro_bundle_privacy_contract_version"

  expect_true(exists(marker, namespace, inherits = FALSE))
  expect_false(bindingIsActive(marker, namespace))
  expect_false(rlang::env_binding_are_lazy(namespace, marker))
  expect_true(bindingIsLocked(marker, namespace))
  expect_identical(get(marker, namespace, inherits = FALSE), 1L)
})

test_that("a function-valued app privacy marker is never executed", {
  namespace <- new.env(parent = emptyenv())
  called <- FALSE
  marker <- function() {
    called <<- TRUE
    1L
  }
  assign(".cerebro_bundle_privacy_contract_version", marker, namespace)
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)

  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  expect_false(called)
})

test_that("an active app privacy marker is never executed", {
  namespace <- new.env(parent = emptyenv())
  called <- FALSE
  makeActiveBinding(
    ".cerebro_bundle_privacy_contract_version",
    function(value) {
      called <<- TRUE
      1L
    },
    namespace
  )
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)

  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  expect_false(called)
})

test_that("a delayed app privacy marker is never forced", {
  namespace <- new.env(parent = emptyenv())
  forced <- FALSE
  delayedAssign(
    ".cerebro_bundle_privacy_contract_version",
    {
      forced <<- TRUE
      1L
    },
    assign.env = namespace
  )
  lockBinding(".cerebro_bundle_privacy_contract_version", namespace)

  expect_identical(builder_installed_app_contract_version(namespace), 0L)
  expect_false(forced)
})

test_that("source App contract takes precedence over the installed package", {
  source_root <- withr::local_tempdir()
  marker <- file.path(
    source_root,
    "inst",
    "builder",
    "app_bundle",
    "privacy-contract-version"
  )
  dir.create(dirname(marker), recursive = TRUE)
  writeLines("1", marker)

  expect_identical(builder_source_app_contract_version(source_root), 1L)

  available <- builder_app_capability(
    installed_contract_version = 0L,
    source_contract_version = 1L
  )
  expect_true(available$available)
  expect_identical(available$version, 1L)
  expect_null(available$reason)
})

test_that("an installed package directory is not mistaken for source", {
  installed_like <- withr::local_tempdir()
  file.copy(builder_test_description_path(), installed_like)
  dir.create(file.path(installed_like, "R"))

  expect_null(builder_source_package_root(installed_like))
})

test_that("Builder startup exports its active source to parent verification", {
  source_root <- withr::local_tempdir()
  file.copy(builder_test_description_path(), source_root)
  dir.create(file.path(source_root, "R"))
  file.create(file.path(source_root, "R", "source.R"))
  withr::local_envvar(CEREBRO_PACKAGE_SOURCE = NA_character_)

  expect_identical(
    builder_activate_source_package(source_root),
    normalizePath(source_root, winslash = "/", mustWork = TRUE)
  )
  expect_identical(
    Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = ""),
    normalizePath(source_root, winslash = "/", mustWork = TRUE)
  )
})

test_that("app capability explains a genuinely unavailable App contract", {
  unavailable <- builder_app_capability(
    installed_contract_version = 0L,
    source_contract_version = 0L
  )

  expect_identical(
    names(unavailable),
    c("available", "version", "reason")
  )
  expect_false(unavailable$available)
  expect_identical(unavailable$version, 0L)
  expect_identical(
    unavailable$reason,
    "This Builder cannot find a secure Viewer App export runtime."
  )
  expect_false(grepl("PR #", unavailable$reason, fixed = TRUE))

  available <- builder_app_capability(1L, source_contract_version = NULL)
  expect_true(available$available)
  expect_identical(available$version, 1L)
  expect_null(available$reason)

  expect_false(
    builder_app_capability(1, source_contract_version = NULL)$available
  )
})

test_that("login capability identifies only missing package requirements", {
  available <- function(package) package %in% c("shinymanager", "digest")
  supported <- function(package) base::package_version("1.1.0")

  capability <- builder_auth_capability(available, supported)
  expect_true(capability$available)
  expect_identical(capability$missing, character())
  expect_null(capability$reason)

  missing_manager <- builder_auth_capability(
    function(package) identical(package, "digest"),
    supported
  )
  expect_false(missing_manager$available)
  expect_identical(missing_manager$missing, "shinymanager (>= 1.1.0)")
  expect_match(missing_manager$reason, "Login is unavailable", fixed = TRUE)
  expect_match(
    missing_manager$reason,
    'install.packages("shinymanager")',
    fixed = TRUE
  )
  expect_match(missing_manager$reason, "restart Builder", fixed = TRUE)

  old_manager <- builder_auth_capability(
    available,
    function(package) base::package_version("1.0.9")
  )
  expect_false(old_manager$available)
  expect_identical(old_manager$missing, "shinymanager (>= 1.1.0)")

  unreadable_manager <- builder_auth_capability(
    available,
    function(package) stop("internal package read failure")
  )
  expect_false(unreadable_manager$available)
  expect_match(
    unreadable_manager$reason,
    "shinymanager (>= 1.1.0)",
    fixed = TRUE
  )
  expect_false(grepl(
    "internal package read failure",
    unreadable_manager$reason
  ))

  manager_only <- builder_auth_capability(
    function(package) identical(package, "shinymanager"),
    supported
  )
  expect_true(manager_only$available)
  expect_length(manager_only$missing, 0L)
})

test_that("Builder runtime capability blocks startup with exact guidance", {
  capability <- builder_runtime_capability(
    function(package) identical(package, "callr"),
    requirements = c("callr", "digest")
  )

  expect_false(capability$available)
  expect_identical(capability$missing, "digest")
  expect_match(
    capability$reason,
    "Builder cannot start because required R dependencies failed to load",
    fixed = TRUE
  )
  expect_match(capability$reason, "digest", fixed = TRUE)

  unavailable <- builder_runtime_capability(
    function(package) FALSE,
    requirements = c("callr", "digest")
  )
  expect_identical(
    unavailable$missing,
    c("callr", "digest")
  )
  expect_match(
    unavailable$reason,
    "Repair or install the listed packages",
    fixed = TRUE
  )
})

test_that("full dependency inventory includes every declared hard dependency", {
  imports <- read.dcf(
    builder_test_description_path(),
    fields = "Imports"
  )[[1L]]
  declared <- strsplit(
    gsub("[\r\n]", " ", imports),
    ",",
    fixed = TRUE
  )[[1L]]
  requirements <- .builder_prerequisite_requirements(
    sub("[[:space:]]*\\(.*$", "", declared)
  )

  expect_true(all(
    c("qs2", "scRepertoire", "callr", "digest", "processx", "ps") %in%
      requirements
  ))
  expect_false(any(grepl("\\(", requirements)))

  loaded <- character()
  capability <- builder_runtime_capability(
    .available = function(package) {
      loaded <<- c(loaded, package)
      if (identical(package, "qs2")) {
        stop(
          "unable to load shared object 'qs2.dll': LoadLibrary failure",
          call. = FALSE
        )
      }
      TRUE
    },
    requirements = c("callr", "qs2", "digest")
  )

  expect_identical(loaded, c("callr", "qs2", "digest"))
  expect_false(capability$available)
  expect_identical(capability$missing, "qs2")
  expect_match(capability$reason, "qs2.dll", fixed = TRUE)
  expect_match(capability$reason, "LoadLibrary failure", fixed = TRUE)
})

test_that("Builder startup preflight excludes deferred feature dependencies", {
  requirements <- builder_startup_package_requirements()

  expect_identical(
    requirements,
    c("callr", "jsonlite", "later", "processx", "ps", "shiny")
  )
  expect_false(any(
    c("biomaRt", "GSVA", "scRepertoire", "qs2") %in%
      requirements
  ))

  loaded <- character()
  capability <- builder_runtime_capability(
    .available = function(package) {
      loaded <<- c(loaded, package)
      TRUE
    }
  )

  expect_true(capability$available)
  expect_identical(loaded, requirements)
})

test_that("Build dependency preflight includes conditional plan requirements", {
  withr::local_envvar(
    CEREBRO_PACKAGE_SOURCE = normalizePath(
      testthat::test_path("..", ".."),
      winslash = "/",
      mustWork = TRUE
    )
  )
  plan <- list(
    items = list(list(
      expression_backend = "h5",
      reused_artifact = NULL,
      source_snapshot_identity = list(
        snapshot = list(serialization = "qs")
      )
    )),
    app_auth = list(enabled = TRUE)
  )
  requirements <- builder_build_package_requirements(plan)

  expect_true(all(
    c(
      builder_build_base_package_requirements(),
      "Seurat (>= 3.0.0)",
      "SeuratObject",
      "HDF5Array (>= 1.18.1)",
      "qs",
      "shinymanager (>= 1.1.0)",
      "digest"
    ) %in%
      requirements
  ))
  expect_false(any(
    c("biomaRt", "GSVA", "scRepertoire") %in%
      requirements
  ))
})

test_that("dependency preflight reports every load and version failure", {
  capability <- builder_dependency_capability(
    c("healthy", "native", "old (>= 2.0.0)"),
    context = "Build cannot start",
    .load = function(package) {
      if (identical(package, "native")) {
        stop("LoadLibrary failure for native.dll", call. = FALSE)
      }
      TRUE
    },
    .version = function(package) base::package_version("1.0.0")
  )

  expect_false(capability$available)
  expect_identical(capability$missing, c("native", "old (>= 2.0.0)"))
  expect_match(capability$reason, "native.dll", fixed = TRUE)
  expect_match(capability$reason, "installed version 1.0.0", fixed = TRUE)
})

test_that("CRB-only plans remain available by default", {
  local({
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    expect_identical(formals(builder_make_plan)$make_app, FALSE)
    expect_false("app_capability" %in% names(formals(builder_make_plan)))
    plan <- builder_make_plan(
      list(builder_task6_entry()),
      tempdir()
    )

    expect_null(plan$error)
    expect_false(plan$make_app)
    expect_identical(plan$app_contract_version, 0L)
    expect_length(plan$output_release$targets, 1L)
    expect_false(any(basename(plan$output_release$targets) == "cerebro_app"))
  })
})

test_that("app plans fail closed without privacy contract v1", {
  local({
    builder_repo_source("prerequisite.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 0L
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")
    capability <- builder_app_capability(0L)

    plan <- builder_make_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE
    )

    expect_identical(plan$error, capability$reason)
  })
})

test_that("app plans freeze the accepted privacy contract version", {
  local({
    builder_repo_source("prerequisite.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 1L
    builder_repo_source("preview.R")
    builder_repo_source("plan.R")

    plan <- builder_make_plan(
      list(builder_task6_entry()),
      tempdir(),
      make_app = TRUE
    )

    expect_null(plan$error)
    expect_true(plan$make_app)
    expect_identical(plan$app_contract_version, 1L)
    expect_true(any(basename(plan$output_release$targets) == "cerebro_app"))
  })
})

test_that("builder UI loads the prerequisite before its plan", {
  lines <- builder_app_source_lines()
  text <- paste(lines, collapse = "\n")
  prerequisite_source <- grep(
    '"prerequisite.R"',
    lines,
    fixed = TRUE
  )
  plan_source <- grep('source("plan.R", local = TRUE)', lines, fixed = TRUE)

  expect_length(prerequisite_source, 1L)
  expect_length(plan_source, 1L)
  expect_lt(prerequisite_source, plan_source)
  expect_match(text, "base::source(", fixed = TRUE)
  expect_match(text, 'encoding = "UTF-8"', fixed = TRUE)
  expect_match(
    text,
    "app_capability <- builder_app_capability\\(\\)"
  )
})

test_that("builder UI activates source only for the running App", {
  lines <- builder_app_source_lines()
  text <- paste(lines, collapse = "\n")
  lifecycle_start <- grep(
    "builder_app_start <- function(",
    lines,
    fixed = TRUE
  )
  activate_source <- grep(
    "builder_activate_source_package(builder_source_package)",
    lines,
    fixed = TRUE
  )
  app_bundle_source <- grep(
    'source("app_bundle.R", local = TRUE)',
    lines,
    fixed = TRUE
  )

  expect_length(activate_source, 1L)
  expect_length(app_bundle_source, 1L)
  expect_length(lifecycle_start, 1L)
  expect_gt(activate_source, lifecycle_start)
  expect_match(text, "builder_app_restore_process_state", fixed = TRUE)
  expect_match(text, 'Sys.unsetenv("CEREBRO_PACKAGE_SOURCE")', fixed = TRUE)
  expect_match(
    text,
    "shinyApp(ui, server, onStart = builder_app_start)",
    fixed = TRUE
  )
})

test_that("worker setup loads the app prerequisite before planning", {
  session_path <- builder_test_inst_path(
    "builder",
    "worker.R"
  )
  if (!file.exists(session_path)) {
    session_path <- system.file(
      file.path("builder", "worker.R"),
      package = "CerebroNexus"
    )
  }
  lines <- readLines(session_path, warn = FALSE)
  prerequisite_source <- grep(
    'source_utf8(file.path(dir, "prerequisite.R"), globalenv())',
    lines,
    fixed = TRUE
  )
  plan_source <- grep(
    'source_utf8(file.path(dir, "plan.R"), globalenv())',
    lines,
    fixed = TRUE
  )

  expect_length(prerequisite_source, 1L)
  expect_length(plan_source, 1L)
  expect_lt(prerequisite_source, plan_source)
})

test_that("worker rejects a forged app plan before createShinyApp", {
  local({
    builder_repo_source("session.R")
    builder_installed_app_contract_version <- function(namespace = NULL) 1L
    create_called <- FALSE
    testthat::local_mocked_bindings(
      createShinyApp = function(...) {
        create_called <<- TRUE
        stop("createShinyApp must not be called")
      },
      .package = "CerebroNexus"
    )
    rs <- list(call = function(fun, args) do.call(fun, args))
    plan <- list(
      out_dir = withr::local_tempdir(),
      items = list(),
      make_app = TRUE,
      app_contract_version = 0L,
      overwrite = FALSE
    )

    result <- builder_session_build(rs, plan)

    expect_match(result$error, "private app publication", ignore.case = TRUE)
    expect_false(create_called)
  })
})

test_that("worker rechecks the current active App contract", {
  local({
    builder_repo_source("session.R")
    builder_app_capability <- function(...) list(version = 0L)
    create_called <- FALSE
    testthat::local_mocked_bindings(
      createShinyApp = function(...) {
        create_called <<- TRUE
        stop("createShinyApp must not be called")
      },
      .package = "CerebroNexus"
    )
    rs <- list(call = function(fun, args) do.call(fun, args))
    plan <- list(
      out_dir = withr::local_tempdir(),
      items = list(),
      make_app = TRUE,
      app_contract_version = 1L,
      overwrite = FALSE
    )

    result <- builder_session_build(rs, plan)

    expect_match(result$error, "private app publication", ignore.case = TRUE)
    expect_false(create_called)
  })
})

test_that("Build dependency preflight executes in the worker process", {
  local({
    builder_repo_source("session.R")
    required <- NULL
    builder_worker_require_capability <- function(name) {
      required <<- name
      invisible(TRUE)
    }
    builder_build_dependency_capability <- function(plan) {
      list(
        available = FALSE,
        missing = "nativePackage",
        failures = "nativePackage: LoadLibrary failure",
        reason = "Build cannot start: nativePackage LoadLibrary failure"
      )
    }
    .builder_stage_owner <- function(stage) list(stage = stage)
    builder_worker_response <- function(request, value = NULL, error = NULL) {
      if (!is.null(error)) list(error = error) else value
    }
    snapshot <- list(path = "fresh")
    identity <- c(list(available = TRUE, snapshot = snapshot), snapshot)
    plan <- list(
      items = list(list(
        id = "fresh",
        name = "Fresh",
        reused_artifact = NULL,
        source_snapshot_identity = identity
      )),
      make_app = FALSE,
      app_auth = list(enabled = FALSE)
    )
    snapshots <- new.env(parent = emptyenv())
    assign("fresh", snapshot, envir = snapshots)
    previous_snapshots <- get0(
      ".builder_snapshots",
      envir = globalenv(),
      inherits = FALSE
    )
    assign(".builder_snapshots", snapshots, envir = globalenv())
    on.exit(
      {
        if (is.null(previous_snapshots)) {
          rm(".builder_snapshots", envir = globalenv())
        } else {
          assign(".builder_snapshots", previous_snapshots, envir = globalenv())
        }
      },
      add = TRUE
    )
    process <- list(call = function(fun, args) do.call(fun, args))
    worker <- structure(
      list(
        process = process,
        snapshot_root = withr::local_tempdir(),
        snapshot_registry = list(fresh = snapshot)
      ),
      class = c("builder_worker", "list")
    )

    capability <- builder_session_build(worker, plan)

    expect_identical(required, "build")
    expect_match(capability$error, "LoadLibrary failure", fixed = TRUE)
  })
})

test_that("session binds successful App results to the dispatched build id", {
  local({
    builder_repo_source("build.R")
    builder_repo_source("session.R")
    builder_app_capability <- function(...) list(version = 1L)
    builder_worker_require_capability <- function(name) invisible(TRUE)
    .builder_build_progress_callback <- function(path) {
      function(...) invisible(NULL)
    }
    builder_execute_plan <- function(
      plan,
      stage,
      registry,
      auth_material = NULL,
      objects = list(),
      on_progress = NULL,
      check_cancel = NULL
    ) {
      expect_true(is.function(on_progress))
      list(state = "success", publishable = TRUE, stage = stage)
    }
    builder_worker_response <- function(
      request,
      value = NULL,
      error = NULL,
      diagnostics = NULL
    ) {
      if (!is.null(error)) list(error = error) else value
    }
    rs <- list(call = function(fun, args) do.call(fun, args))
    plan <- list(
      out_dir = withr::local_tempdir(),
      items = list(),
      make_app = TRUE,
      app_contract_version = 1L,
      overwrite = FALSE
    )

    result <- builder_session_build(
      rs,
      plan,
      request = list(build_id = "build-session")
    )

    expect_identical(result$build_id, "build-session")
  })
})

test_that("worker preloads only fresh objects that are already resident", {
  local({
    builder_repo_source("build.R")
    builder_repo_source("session.R")
    builder_worker_require_capability <- function(name) invisible(TRUE)
    builder_build_dependency_capability <- function(plan) {
      list(available = TRUE, reason = NULL)
    }
    .builder_stage_owner <- function(stage) list(stage = stage)
    .builder_build_progress_callback <- function(path) {
      function(...) invisible(NULL)
    }
    builder_worker_response <- function(
      request,
      value = NULL,
      error = NULL,
      diagnostics = NULL
    ) {
      if (!is.null(error)) list(error = error) else value
    }
    .builder_worker_object <- function(...) {
      stop("must not eagerly open snapshots")
    }
    builder_execute_plan <- function(
      plan,
      stage,
      registry,
      auth_material = NULL,
      objects = list(),
      on_progress = NULL,
      check_cancel = NULL
    ) {
      expect_identical(names(registry), c("resident", "cold"))
      expect_identical(names(objects), "resident")
      list(state = "success", publishable = TRUE, stage = stage)
    }
    resident_snapshot <- list(path = "resident")
    cold_snapshot <- list(path = "cold")
    reused_snapshot <- list(path = "reused")
    snapshots <- new.env(parent = emptyenv())
    assign("resident", resident_snapshot, envir = snapshots)
    assign("cold", cold_snapshot, envir = snapshots)
    assign("reused", reused_snapshot, envir = snapshots)
    objects <- new.env(parent = emptyenv())
    assign("resident", list(id = "resident"), envir = objects)
    previous_snapshots <- get0(
      ".builder_snapshots",
      envir = globalenv(),
      inherits = FALSE
    )
    previous_objects <- get0(
      ".builder_objects",
      envir = globalenv(),
      inherits = FALSE
    )
    assign(".builder_snapshots", snapshots, envir = globalenv())
    assign(".builder_objects", objects, envir = globalenv())
    on.exit(
      {
        if (is.null(previous_snapshots)) {
          rm(".builder_snapshots", envir = globalenv())
        } else {
          assign(".builder_snapshots", previous_snapshots, envir = globalenv())
        }
        if (is.null(previous_objects)) {
          rm(".builder_objects", envir = globalenv())
        } else {
          assign(".builder_objects", previous_objects, envir = globalenv())
        }
      },
      add = TRUE
    )
    snapshot_identity <- function(snapshot) {
      c(list(available = TRUE, snapshot = snapshot), snapshot)
    }
    plan <- list(
      items = list(
        list(
          id = "resident",
          name = "Resident",
          reused_artifact = NULL,
          source_snapshot_identity = snapshot_identity(resident_snapshot)
        ),
        list(
          id = "cold",
          name = "Cold",
          reused_artifact = NULL,
          source_snapshot_identity = snapshot_identity(cold_snapshot)
        ),
        list(
          id = "reused",
          name = "Reused",
          reused_artifact = list(path = "artifact.crb")
        )
      ),
      make_app = FALSE,
      app_auth = list(enabled = FALSE)
    )
    process <- list(call = function(fun, args) do.call(fun, args))
    worker <- structure(
      list(
        process = process,
        snapshot_root = withr::local_tempdir(),
        snapshot_registry = list(
          resident = resident_snapshot,
          cold = cold_snapshot,
          reused = reused_snapshot
        )
      ),
      class = c("builder_worker", "list")
    )

    result <- builder_session_build(worker, plan)

    expect_identical(result$state, "success")
  })
})
