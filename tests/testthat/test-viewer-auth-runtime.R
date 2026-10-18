viewer_auth_runtime_environment <- function() {
  runtime <- new.env(parent = globalenv())
  source_file <- viewer_test_path("auth.R")
  if (!file.exists(source_file)) {
    source_file <- system.file(
      "viewer/auth.R",
      package = "CerebroNexus"
    )
  }
  sys.source(source_file, envir = runtime)
  runtime
}

viewer_auth_runtime_config <- function() {
  list(
    credentials_path = "private-data/auth/credentials.sqlite",
    passphrase_env = "CEREBRO_AUTH_TEST_KEY",
    timeout_minutes = 15L
  )
}

viewer_auth_runtime_brand_assets <- function(root) {
  www <- file.path(root, "viewer", "www")
  dir.create(www, recursive = TRUE)
  writeLines(
    ".panel-auth { min-height: 100vh; }",
    file.path(www, "auth.css")
  )
  writeLines("/* Login interaction */", file.path(www, "auth.js"))
  writeLines(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg">',
      "<title>CerebroNexus</title></svg>"
    ),
    file.path(www, "cerebronexus.svg")
  )
}

test_that("Viewer authentication runtime is a no-op when disabled", {
  runtime <- viewer_auth_runtime_environment()
  ui <- shiny::fluidPage("viewer")
  server <- function(input, output, session) NULL

  app <- runtime$viewer_auth_apply(ui, server, NULL, ".")

  expect_identical(app$ui, ui)
  expect_identical(app$server, server)
})

test_that("Viewer accepts read-only credentials and requires its secret", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), database)
  if (!identical(.Platform$OS.type, "windows")) {
    withr::defer({
      Sys.chmod(dirname(database), mode = "0700")
      Sys.chmod(database, mode = "0600")
    })
    expect_true(isTRUE(unname(Sys.chmod(database, mode = "0400"))))
    expect_true(isTRUE(unname(Sys.chmod(dirname(database), mode = "0500"))))
  }
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)

  expect_error(
    runtime$viewer_auth_apply(
      shiny::fluidPage("viewer"),
      function(input, output, session) NULL,
      viewer_auth_runtime_config(),
      root
    ),
    "viewer-auth.env was not found"
  )
})

test_that("Viewer authentication loads only its app-local secret", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  app_dir <- file.path(root, "cerebro_app")
  dir.create(app_dir)
  env_name <- "CEREBRO_AUTH_TEST_KEY"
  app_secret <- strrep("a", 64L)
  parent_secret <- strrep("b", 64L)
  app_env <- file.path(app_dir, "viewer-auth.env")
  parent_env <- file.path(root, "viewer-auth.env")
  writeLines(paste0(env_name, "=", app_secret), app_env)
  writeLines(paste0(env_name, "=", parent_secret), parent_env)
  Sys.chmod(c(app_env, parent_env), mode = "0600", use_umask = FALSE)
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)
  load_passphrase <- get(
    ".viewer_auth_load_local_passphrase",
    envir = runtime,
    inherits = FALSE
  )

  expect_true(load_passphrase(app_dir, env_name))
  expect_identical(Sys.getenv(env_name), app_secret)

  Sys.unsetenv(env_name)
  unlink(app_env)
  expect_error(
    load_passphrase(app_dir, env_name),
    "viewer-auth.env was not found"
  )
  expect_identical(Sys.getenv(env_name, unset = NA_character_), NA_character_)
})

test_that("process authentication keys bypass every local-file check", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  path <- file.path(root, "viewer-auth.env")
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = "externally supplied key")
  load <- runtime$.viewer_auth_load_local_passphrase
  expect_false(load(root, "CEREBRO_AUTH_TEST_KEY"))
  writeLines("invalid content", path)
  Sys.chmod(path, "0664", use_umask = FALSE)
  expect_false(load(root, "CEREBRO_AUTH_TEST_KEY"))
  unlink(path)
  dir.create(path)
  expect_false(load(root, "CEREBRO_AUTH_TEST_KEY"))
  expect_identical(
    Sys.getenv("CEREBRO_AUTH_TEST_KEY"),
    "externally supplied key"
  )
})

test_that("local authentication content errors never disclose the content", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  path <- file.path(root, "viewer-auth.env")
  secret <- strrep("c", 64L)
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)
  for (lines in list(
    character(),
    "",
    paste0("WRONG_NAME=", secret),
    paste0("export CEREBRO_AUTH_TEST_KEY=", secret),
    paste0('CEREBRO_AUTH_TEST_KEY="', secret, '"'),
    c(paste0("CEREBRO_AUTH_TEST_KEY=", secret), "extra line"),
    "CEREBRO_AUTH_TEST_KEY=login-password-must-not-appear"
  )) {
    writeLines(lines, path)
    Sys.chmod(path, "0600", use_umask = FALSE)
    error <- tryCatch(
      runtime$.viewer_auth_load_local_passphrase(root, "CEREBRO_AUTH_TEST_KEY"),
      error = identity
    )
    expect_s3_class(error, "error")
    message <- conditionMessage(error)
    expect_match(message, "invalid content", fixed = TRUE)
    expect_match(message, "CEREBRO_AUTH_TEST_KEY=<64", fixed = TRUE)
    expect_false(grepl(secret, message, fixed = TRUE))
    expect_false(grepl("login-password-must-not-appear", message, fixed = TRUE))
    expect_identical(
      Sys.getenv("CEREBRO_AUTH_TEST_KEY", unset = NA),
      NA_character_
    )
  }
})

test_that("local authentication rejects directories and symbolic links", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  path <- file.path(root, "viewer-auth.env")
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)
  dir.create(path)
  expect_error(
    runtime$.viewer_auth_load_local_passphrase(root, "CEREBRO_AUTH_TEST_KEY"),
    "not a regular file"
  )
  unlink(path, recursive = TRUE)
  target <- file.path(root, "actual-secret")
  writeLines(paste0("CEREBRO_AUTH_TEST_KEY=", strrep("d", 64L)), target)
  Sys.chmod(target, "0600", use_umask = FALSE)
  skip_if_not(
    isTRUE(suppressWarnings(file.symlink(target, path))),
    "This host cannot create a symbolic link"
  )
  expect_error(
    runtime$.viewer_auth_load_local_passphrase(root, "CEREBRO_AUTH_TEST_KEY"),
    "symbolic link"
  )
  unlink(target)
  expect_error(
    runtime$.viewer_auth_load_local_passphrase(root, "CEREBRO_AUTH_TEST_KEY"),
    "symbolic link"
  )
})

test_that("POSIX local authentication requires exactly 0600", {
  skip_on_os("windows")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  path <- file.path(root, "viewer-auth.env")
  writeLines(paste0("CEREBRO_AUTH_TEST_KEY=", strrep("e", 64L)), path)
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)
  for (mode in c("0664", "0644", "0660", "0400")) {
    expect_true(Sys.chmod(path, mode, use_umask = FALSE))
    expect_error(
      runtime$.viewer_auth_load_local_passphrase(root, "CEREBRO_AUTH_TEST_KEY"),
      paste0("insecure permissions: ", mode, ". Required permissions: 0600"),
      fixed = TRUE
    )
    expect_identical(
      Sys.getenv("CEREBRO_AUTH_TEST_KEY", unset = NA),
      NA_character_
    )
  }
  Sys.chmod(path, "0600", use_umask = FALSE)
  expect_true(runtime$.viewer_auth_load_local_passphrase(
    root,
    "CEREBRO_AUTH_TEST_KEY"
  ))
})

test_that("authentication read failures report access without leaking errors", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  path <- file.path(root, "viewer-auth.env")
  writeLines(paste0("CEREBRO_AUTH_TEST_KEY=", strrep("f", 64L)), path)
  Sys.chmod(path, "0600", use_umask = FALSE)
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = NA)
  # Fault injection covers the error branches, not real cross-UID permissions.
  runtime$file.access <- function(...) -1L
  expect_error(
    runtime$.viewer_auth_load_local_passphrase(root, "CEREBRO_AUTH_TEST_KEY"),
    "not readable by the current application user"
  )
  runtime$file.access <- base::file.access
  runtime$readLines <- function(...) stop("sensitive-reader-error")
  error <- tryCatch(
    runtime$.viewer_auth_load_local_passphrase(root, "CEREBRO_AUTH_TEST_KEY"),
    error = identity
  )
  expect_match(conditionMessage(error), "runtime UID/GID", fixed = TRUE)
  expect_false(grepl(
    "sensitive-reader-error",
    conditionMessage(error),
    fixed = TRUE
  ))
})

test_that("Viewer authentication supplies bundled CerebroNexus branding", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), database)
  viewer_auth_runtime_brand_assets(root)
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = "runtime test passphrase")
  captured <- NULL
  testthat::local_mocked_bindings(
    read_db_decrypt = function(...) data.frame(user = "alice"),
    check_credentials = function(...) {
      function(user, password) list(result = TRUE)
    },
    secure_app = function(
      ui,
      enable_admin,
      head_auth,
      tags_top,
      tags_bottom
    ) {
      captured <<- list(
        enable_admin = enable_admin,
        head = head_auth,
        top = tags_top,
        bottom = tags_bottom
      )
      ui
    },
    .package = "shinymanager"
  )

  runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    function(input, output, session) NULL,
    viewer_auth_runtime_config(),
    root
  )

  expect_identical(captured$enable_admin, FALSE)
  rendered <- as.character(shiny::tagList(
    captured$head,
    captured$top,
    captured$bottom
  ))
  expect_match(rendered, "min-height: 100vh", fixed = TRUE)
  expect_match(rendered, "cerebro-auth-brand", fixed = TRUE)
  expect_match(rendered, "CerebroNexus", fixed = TRUE)
  expect_match(rendered, 'aria-label="CerebroNexus"', fixed = TRUE)
  expect_match(
    rendered,
    "Need access? Contact the person who shared this app.",
    fixed = TRUE
  )
})

test_that("Viewer login presentation preserves provider inputs and improves semantics", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  rendered <- as.character(runtime$.viewer_auth_present(shinymanager::auth_ui(
    "auth"
  )))
  expect_match(
    rendered,
    '<h1 id="auth-shinymanager-auth-head">Sign in</h1>',
    fixed = TRUE
  )
  expect_match(rendered, 'id="auth-user_id"', fixed = TRUE)
  expect_match(rendered, 'autocomplete="username"', fixed = TRUE)
  expect_match(rendered, 'autocomplete="current-password"', fixed = TRUE)
  expect_match(rendered, 'aria-live="polite"', fixed = TRUE)
  expect_false(grepl("autofocus", rendered, fixed = TRUE))
  ui <- shiny::fluidPage(shiny::h1("Viewer"))
  expect_identical(runtime$.viewer_auth_present(ui), ui)
})

test_that("Viewer authentication fails closed when branding is incomplete", {
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  dir.create(file.path(root, "viewer", "www"), recursive = TRUE)

  expect_error(
    runtime$.viewer_auth_brand(root),
    "Authentication branding assets are unavailable"
  )
})

test_that("Viewer starts after server-authoritative authentication", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  viewer_auth_runtime_brand_assets(root)
  passphrase <- "runtime test passphrase"
  shinymanager::create_db(
    credentials_data = data.frame(
      user = "alice",
      password = "alice-login-password",
      stringsAsFactors = FALSE
    ),
    sqlite_path = database,
    passphrase = passphrase
  )
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = passphrase)
  auth_state <- shiny::reactiveValues(user = NULL)
  captured_checker <- NULL
  testthat::local_mocked_bindings(
    secure_app = function(ui, ...) structure(ui, viewer_auth_secured = TRUE),
    secure_server = function(check_credentials, ...) {
      captured_checker <<- check_credentials
      auth_state
    },
    .package = "shinymanager"
  )
  starts <- 0L
  reloaded <- FALSE
  viewer_server <- function(input, output, session) {
    starts <<- starts + 1L
  }

  app <- runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    viewer_server,
    viewer_auth_runtime_config(),
    root
  )

  expect_identical(attr(app$ui, "viewer_auth_secured"), TRUE)
  shiny::testServer(app$server, {
    session$flushReact()
    expect_true(is.function(captured_checker))
    expect_true(captured_checker("alice", "alice-login-password")$result)
    expect_false(captured_checker("alice", "wrong-password")$result)
    expect_identical(starts, 0L)
    auth_state$user <- "alice"
    session$flushReact()
    expect_identical(starts, 1L)
    session$reload <- function() {
      reloaded <<- TRUE
    }
    do.call(
      session$setInputs,
      setNames(list(1L), ".shinymanager_logout")
    )
    session$flushReact()
    expect_true(reloaded)
    expect_true(session$isClosed())
  })
})

test_that("Viewer keeps account management outside the runtime", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  viewer_auth_runtime_brand_assets(root)
  passphrase <- "runtime test passphrase"
  shinymanager::create_db(
    credentials_data = data.frame(
      user = c("alice", "bob"),
      password = c("alice-login-password", "bob-login-password"),
      stringsAsFactors = FALSE
    ),
    sqlite_path = database,
    passphrase = passphrase
  )
  source_hash <- unname(tools::md5sum(database))
  if (!identical(.Platform$OS.type, "windows")) {
    withr::defer({
      Sys.chmod(dirname(database), mode = "0700")
      Sys.chmod(database, mode = "0600")
    })
    Sys.chmod(database, mode = "0400")
    Sys.chmod(dirname(database), mode = "0500")
  }
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = passphrase)
  auth_state <- shiny::reactiveValues(user = NULL)
  captured_checker <- NULL
  testthat::local_mocked_bindings(
    secure_app = function(ui, ...) ui,
    secure_server = function(check_credentials, ...) {
      captured_checker <<- check_credentials
      auth_state
    },
    .package = "shinymanager"
  )
  starts <- 0L
  app <- runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    function(input, output, session) {
      starts <<- starts + 1L
    },
    viewer_auth_runtime_config(),
    root
  )

  token_store <- getFromNamespace(".tok", "shinymanager")
  expect_null(token_store$get_sqlite_path())
  expect_null(token_store$get_sql_config_db())

  shiny::testServer(app$server, {
    expect_true(is.function(captured_checker))
    expect_true(captured_checker("alice", "alice-login-password")$result)
    expect_true(captured_checker("bob", "bob-login-password")$result)
    expect_false(captured_checker("alice", "wrong-password")$result)
    update_pwd <- getFromNamespace("update_pwd", "shinymanager")
    expect_false(update_pwd("alice", "new-login-password")$result)
    expect_false(captured_checker("alice", "new-login-password")$result)
    auth_state$user <- "alice"
    session$flushReact()
    expect_identical(starts, 1L)
  })
  expect_identical(unname(tools::md5sum(database)), source_hash)
})

test_that("Viewer session closes when the authenticated user changes", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  viewer_auth_runtime_brand_assets(root)
  passphrase <- "runtime test passphrase"
  shinymanager::create_db(
    credentials_data = data.frame(
      user = c("alice", "bob"),
      password = c("alice-login-password", "bob-login-password"),
      stringsAsFactors = FALSE
    ),
    sqlite_path = database,
    passphrase = passphrase
  )
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = passphrase)
  auth_state <- shiny::reactiveValues(user = NULL)
  testthat::local_mocked_bindings(
    secure_app = function(ui, ...) ui,
    secure_server = function(...) auth_state,
    .package = "shinymanager"
  )
  app <- runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    function(input, output, session) NULL,
    viewer_auth_runtime_config(),
    root
  )

  shiny::testServer(app$server, {
    session$setInputs(shinymanager_where = "application")
    auth_state$user <- "alice"
    session$flushReact()
    expect_false(session$isClosed())
    auth_state$user <- "bob"
    session$flushReact()
    expect_true(session$isClosed())
  })
})

test_that("Viewer session closes when authentication times out", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  viewer_auth_runtime_brand_assets(root)
  passphrase <- "runtime test passphrase"
  shinymanager::create_db(
    credentials_data = data.frame(
      user = "alice",
      password = "alice-login-password",
      stringsAsFactors = FALSE
    ),
    sqlite_path = database,
    passphrase = passphrase
  )
  withr::local_envvar(CEREBRO_AUTH_TEST_KEY = passphrase)
  auth_state <- shiny::reactiveValues(user = NULL)
  testthat::local_mocked_bindings(
    secure_app = function(ui, ...) ui,
    secure_server = function(...) auth_state,
    .package = "shinymanager"
  )
  starts <- 0L
  app <- runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    function(input, output, session) {
      starts <<- starts + 1L
    },
    utils::modifyList(
      viewer_auth_runtime_config(),
      list(timeout_minutes = 1L)
    ),
    root
  )

  shiny::testServer(app$server, {
    session$setInputs(shinymanager_where = "application")
    auth_state$user <- "alice"
    session$flushReact()
    expect_identical(starts, 1L)
    expect_false(session$isClosed())
    session$elapse(60001)
    session$flushReact()
    expect_true(session$isClosed())
  })
})
