viewer_auth_runtime_environment <- function() {
  runtime <- new.env(parent = globalenv())
  admin_core <- viewer_test_path("admin", "core.R")
  sys.source(admin_core, envir = runtime)
  source_file <- file.path("inst", "viewer", "auth.R")
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
  writeLines(
    paste0(
      '<svg xmlns="http://www.w3.org/2000/svg">',
      "<title>CerebroNexus</title></svg>"
    ),
    file.path(www, "cerebronexus.svg")
  )
}

test_that("configured Viewer Admin can pass the outer login", {
  skip_if_not_installed("shinymanager", minimum_version = "1.1.0")
  runtime <- viewer_auth_runtime_environment()
  root <- withr::local_tempdir()
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  dir.create(dirname(database), recursive = TRUE)
  writeBin(as.raw(c(0x53, 0x51, 0x4c)), database)
  viewer_auth_runtime_brand_assets(root)
  withr::local_envvar(
    CEREBRO_AUTH_TEST_KEY = "runtime test passphrase",
    CEREBRO_ADMIN_PASSWORD = "runtime-admin-password"
  )
  old_options <- get0("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)
  withr::defer({
    if (is.null(old_options)) {
      if (exists("Cerebro.options", envir = .GlobalEnv, inherits = FALSE)) {
        rm("Cerebro.options", envir = .GlobalEnv)
      }
    } else {
      assign("Cerebro.options", old_options, envir = .GlobalEnv)
    }
  })
  assign(
    "Cerebro.options",
    list(
      .share_admin = list(
        account = "owner",
        password_env = "CEREBRO_ADMIN_PASSWORD"
      )
    ),
    envir = .GlobalEnv
  )
  auth_state <- shiny::reactiveValues(user = NULL, admin = NULL)
  captured_checker <- NULL
  testthat::local_mocked_bindings(
    read_db_decrypt = function(...) data.frame(user = "alice"),
    check_credentials = function(credentials) {
      users <- credentials$user
      function(user, password) {
        admin <- identical(user, "owner") &&
          identical(password, "runtime-admin-password") &&
          "owner" %in% users
        regular <- identical(user, "alice") &&
          identical(password, "alice-password") &&
          "alice" %in% users
        list(result = admin || regular, user = user, admin = admin)
      }
    },
    secure_app = function(ui, ...) ui,
    secure_server = function(check_credentials, ...) {
      captured_checker <<- check_credentials
      auth_state
    },
    .package = "shinymanager"
  )
  observed <- NULL
  app <- runtime$viewer_auth_apply(
    shiny::fluidPage("viewer"),
    function(input, output, session) {
      observed <<- session$userData$viewer_auth
    },
    viewer_auth_runtime_config(),
    root
  )

  shiny::testServer(app$server, {
    expect_true(captured_checker("owner", "runtime-admin-password")$result)
    expect_false(captured_checker("owner", "wrong")$result)
    auth_state$admin <- TRUE
    auth_state$user <- "owner"
    session$flushReact()
    expect_identical(
      observed,
      list(authenticated = TRUE, user = "owner", is_admin = TRUE)
    )
  })
})

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
    "CEREBRO_AUTH_TEST_KEY is not set"
  )
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
  expect_match(rendered, "Secure viewer", fixed = TRUE)
  expect_match(rendered, "Protected access", fixed = TRUE)
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
