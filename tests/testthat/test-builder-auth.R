builder_repo_source("prerequisite.R")
builder_repo_source("app_bundle.R")

test_that("Builder App bundle loads the auth contract before request helpers", {
  path <- builder_profile_inst_path("builder", "app_bundle.R")
  lines <- readLines(path, warn = FALSE)
  auth_source <- grep('"auth.R"', lines, fixed = TRUE)
  contract_source <- grep('"contract.R"', lines, fixed = TRUE)

  expect_length(auth_source, 1L)
  expect_length(contract_source, 1L)
  expect_lt(auth_source, contract_source)
})

test_that("Builder auth normalizes usernames but never passwords", {
  parsed <- builder_auth_validate_payload(
    enabled = TRUE,
    accounts = builder_auth_test_accounts()
  )

  expect_true(parsed$ok)
  expect_s3_class(parsed$accounts, "builder_auth_accounts")
  expect_identical(parsed$accounts[[1L]]$username, "auth-user-a-7f31")
  expect_identical(
    parsed$accounts[[1L]]$password,
    "auth-password-a-7f31"
  )
  expect_identical(
    builder_auth_summary(TRUE, parsed$accounts),
    list(enabled = TRUE, account_count = 2L, timeout_minutes = 15L)
  )
})

test_that("Builder auth rejects incomplete or duplicate accounts safely", {
  cases <- list(
    empty = list(),
    blank_user = list(list(
      id = "auth-account-1",
      username = "  ",
      password = "auth-password-blank-13a9"
    )),
    short_password = list(list(
      id = "auth-account-1",
      username = "auth-user-short-24b8",
      password = "p24b8"
    )),
    duplicate_user = list(
      list(
        id = "auth-account-1",
        username = "auth-user-duplicate-35c7",
        password = "auth-password-first-35c7"
      ),
      list(
        id = "auth-account-2",
        username = " auth-user-duplicate-35c7 ",
        password = "auth-password-second-46d6"
      )
    )
  )
  forbidden <- c(
    "auth-password-blank-13a9",
    "auth-user-short-24b8",
    "p24b8",
    "auth-user-duplicate-35c7",
    "auth-password-first-35c7",
    "auth-password-second-46d6"
  )

  for (name in names(cases)) {
    parsed <- builder_auth_validate_payload(TRUE, cases[[name]])
    expect_false(parsed$ok, info = name)
    expect_null(parsed$accounts, info = name)
    expect_false(
      any(vapply(forbidden, grepl, logical(1), x = parsed$error, fixed = TRUE)),
      info = name
    )
  }
  expect_identical(
    builder_auth_summary(FALSE, builder_auth_empty_accounts()),
    list(enabled = FALSE, account_count = 0L, timeout_minutes = 15L)
  )
})

test_that("Builder auth accepts only the strict account payload boundary", {
  valid_password <- "auth-password-boundary-91c4"
  cases <- list(
    invalid_enabled = list(
      enabled = NA,
      accounts = builder_auth_test_accounts()
    ),
    non_list = list(enabled = TRUE, accounts = "not-a-list"),
    missing_field = list(
      enabled = TRUE,
      accounts = list(list(
        id = "auth-account-1",
        username = "auth-user-missing-52e1"
      ))
    ),
    duplicate_id = list(
      enabled = TRUE,
      accounts = list(
        list(
          id = "auth-account-1",
          username = "auth-user-id-a-62f1",
          password = valid_password
        ),
        list(
          id = "auth-account-1",
          username = "auth-user-id-b-73a2",
          password = valid_password
        )
      )
    ),
    invalid_row_id = list(
      enabled = TRUE,
      accounts = list(list(
        id = "auth-row-sentinel-91c4",
        username = "auth-user-row-91c4",
        password = valid_password
      ))
    ),
    extra_field = list(
      enabled = TRUE,
      accounts = list(list(
        id = "auth-account-1",
        username = "auth-user-extra-91c4",
        password = valid_password,
        secret_note = "auth-extra-sentinel-91c4"
      ))
    ),
    too_many = list(
      enabled = TRUE,
      accounts = lapply(seq_len(51L), function(i) {
        list(
          id = paste0("auth-account-", i),
          username = paste0("auth-user-many-", i),
          password = valid_password
        )
      })
    )
  )

  for (name in names(cases)) {
    parsed <- builder_auth_validate_payload(
      cases[[name]]$enabled,
      cases[[name]]$accounts
    )
    expect_false(parsed$ok, info = name)
    expect_null(parsed$accounts, info = name)
    expect_identical(
      names(parsed),
      c("ok", "error", "accounts"),
      info = name
    )
    expect_false(
      any(vapply(
        c(
          "auth-row-sentinel-91c4",
          "auth-user-row-91c4",
          "auth-password-boundary-91c4",
          "auth-user-extra-91c4",
          "auth-extra-sentinel-91c4"
        ),
        grepl,
        logical(1),
        x = parsed$error,
        fixed = TRUE
      )),
      info = name
    )
  }
})

test_that("Builder auth preserves an exactly eight-character password", {
  parsed <- builder_auth_validate_payload(
    TRUE,
    list(list(
      id = "auth-account-1",
      username = " auth-user-eight-84b3 ",
      password = "eight888"
    ))
  )

  expect_true(parsed$ok)
  expect_identical(parsed$accounts[[1L]]$username, "auth-user-eight-84b3")
  expect_identical(parsed$accounts[[1L]]$password, "eight888")
})

test_that("disabled Builder auth discards browser account residue", {
  parsed <- builder_auth_validate_payload(TRUE, builder_auth_test_accounts())
  disabled <- builder_auth_validate_payload(FALSE, parsed$accounts)

  expect_true(disabled$ok)
  expect_s3_class(disabled$accounts, "builder_auth_accounts")
  expect_length(disabled$accounts, 0L)
  expect_false(builder_auth_value_contains(disabled, "auth-user-a-7f31"))
  expect_false(builder_auth_value_contains(disabled, "auth-password-a-7f31"))
})

test_that("authentication secret files are private and strictly parsed", {
  stage <- withr::local_tempdir()
  path <- file.path(stage, "viewer-auth.env")
  passphrase <- strrep("a", 64L)

  expect_identical(
    .builder_auth_random_passphrase(function(n) as.raw(rep(1L, n))),
    strrep("01", 32L)
  )
  expect_error(
    .builder_auth_random_passphrase(function(n) as.raw(rep(1L, n - 1L))),
    "secure authentication key"
  )
  written <- .builder_auth_write_env(path, passphrase)
  expect_identical(written, normalizePath(path, winslash = "/"))
  expect_identical(builder_auth_read_env_file(path), passphrase)
  expect_false(builder_auth_value_contains(list(path = path), passphrase))
})

test_that("authentication env reader rejects wrong name multiline mode and links", {
  stage <- withr::local_tempdir()
  path <- file.path(stage, "viewer-auth.env")
  cases <- list(
    wrong_name = "WRONG_AUTH_NAME=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    multiline = c(
      "CEREBRO_AUTH_PASSPHRASE=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      "EXTRA=value"
    )
  )
  for (name in names(cases)) {
    writeLines(cases[[name]], path)
    Sys.chmod(path, "0600", use_umask = FALSE)
    expect_error(builder_auth_read_env_file(path), "invalid", info = name)
    unlink(path)
  }
  if (!identical(.Platform$OS.type, "windows")) {
    writeLines(
      "CEREBRO_AUTH_PASSPHRASE=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      path
    )
    Sys.chmod(path, "0644", use_umask = FALSE)
    expect_error(builder_auth_read_env_file(path), "not private")
    unlink(path)
  }

  target <- file.path(stage, "target.env")
  writeLines(
    "CEREBRO_AUTH_PASSPHRASE=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    target
  )
  Sys.chmod(target, "0600", use_umask = FALSE)
  if (isTRUE(suppressWarnings(file.symlink(target, path)))) {
    expect_error(builder_auth_read_env_file(path), "missing or unsafe")
  }
})

test_that("authentication material contains paths but no login secrets", {
  stage <- withr::local_tempdir()
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  create_db <- function(credentials_data, sqlite_path, passphrase) {
    expect_identical(names(credentials_data), c("user", "password"))
    expect_identical(passphrase, strrep("0b", 32L))
    writeBin(as.raw(1:8), sqlite_path)
  }
  material <- builder_auth_create_material(
    accounts,
    stage,
    .random_bytes = function(n) as.raw(rep(11L, n)),
    .create_db = create_db,
    .capability = function() list(available = TRUE, reason = NULL)
  )

  expect_identical(
    names(material),
    c("source_dir", "credentials", "env_file", "descriptor")
  )
  expect_true(file.exists(material$credentials))
  expect_true(file.exists(material$env_file))
  expect_false(builder_auth_value_contains(material, "auth-user-a-7f31"))
  expect_false(builder_auth_value_contains(material, "auth-password-a-7f31"))
  expect_false(builder_auth_value_contains(material, strrep("0b", 32L)))
})

test_that("authentication passphrase exists only during the supplied action", {
  original <- Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
  on.exit(
    {
      if (is.na(original)) {
        Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE")
      } else {
        Sys.setenv(CEREBRO_AUTH_PASSPHRASE = original)
      }
    },
    add = TRUE
  )
  Sys.unsetenv("CEREBRO_AUTH_PASSPHRASE")
  passphrase <- strrep("c", 64L)
  seen <- .builder_auth_with_passphrase(passphrase, function() {
    Sys.getenv("CEREBRO_AUTH_PASSPHRASE", unset = NA_character_)
  })

  expect_identical(seen, passphrase)
  expect_true(is.na(Sys.getenv(
    "CEREBRO_AUTH_PASSPHRASE",
    unset = NA_character_
  )))
})

test_that("authentication passphrase restores the exact pre-existing value", {
  old <- "pre-existing-passphrase-value-64c2"
  withr::local_envvar(CEREBRO_AUTH_PASSPHRASE = old)
  passphrase <- strrep("d", 64L)

  seen <- .builder_auth_with_passphrase(passphrase, function() {
    expect_identical(Sys.getenv("CEREBRO_AUTH_PASSPHRASE"), passphrase)
    "done"
  })

  expect_identical(seen, "done")
  expect_identical(Sys.getenv("CEREBRO_AUTH_PASSPHRASE"), old)
})

test_that("partial authentication cleanup rejects escapes and verifies deletion", {
  stage <- withr::local_tempdir()
  source <- file.path(stage, ".builder-auth-source")
  env <- file.path(stage, "viewer-auth.env")
  dir.create(source)
  writeLines("x", env)

  expect_silent(.builder_auth_remove_partial_material(stage))
  expect_false(file.exists(source))
  expect_false(file.exists(env))

  dir.create(source)
  expect_error(
    .builder_auth_remove_partial_material(
      stage,
      .unlink = function(...) 0L
    ),
    "cleaned up"
  )
})

test_that("authentication database verification accepts only exact TRUE", {
  database <- tempfile(fileext = ".sqlite")
  writeBin(as.raw(1:8), database)
  Sys.chmod(database, "0600", use_umask = FALSE)
  stage <- withr::local_tempdir()
  env <- .builder_auth_write_env(
    file.path(stage, "viewer-auth.env"),
    strrep("e", 64L)
  )
  expect_true(builder_auth_verify_database_pair(
    database,
    env,
    .validate = function(...) TRUE
  ))
  called <- FALSE
  validate <- function(...) {
    called <<- TRUE
    TRUE
  }
  for (unsafe_database in list(
    file.path(stage, "missing.sqlite"),
    "",
    c(database, database)
  )) {
    expect_error(
      builder_auth_verify_database_pair(
        unsafe_database,
        env,
        .validate = validate
      ),
      "database could not be verified"
    )
    expect_false(called)
  }
  linked_database <- file.path(stage, "linked.sqlite")
  if (isTRUE(suppressWarnings(file.symlink(database, linked_database)))) {
    expect_error(
      builder_auth_verify_database_pair(
        linked_database,
        env,
        .validate = validate
      ),
      "database could not be verified"
    )
  }
  expect_false(called)
  expect_error(
    builder_auth_verify_database_pair(database, env, .validate = TRUE),
    "database could not be verified"
  )
  for (value in list(FALSE, NULL, simpleError("sentinel-secret-2f84"))) {
    validate <- if (inherits(value, "condition")) {
      function(...) stop(value)
    } else {
      local({
        result <- value
        function(...) result
      })
    }
    error <- tryCatch(
      builder_auth_verify_database_pair(database, env, .validate = validate),
      error = identity
    )
    expect_s3_class(error, "error")
    expect_false(grepl(
      "sentinel-secret-2f84",
      conditionMessage(error),
      fixed = TRUE
    ))
  }
})

test_that("authentication material validation rechecks private file modes", {
  skip_on_os("windows")
  stage <- withr::local_tempdir()
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  material <- builder_auth_create_material(accounts, stage)

  expect_silent(builder_auth_validate_material(material, stage))
  Sys.chmod(material$source_dir, "0755", use_umask = FALSE)
  expect_error(
    builder_auth_validate_material(material, stage),
    "files could not be verified"
  )
  Sys.chmod(material$source_dir, "0700", use_umask = FALSE)
  Sys.chmod(material$credentials, "0644", use_umask = FALSE)
  expect_error(
    builder_auth_validate_material(material, stage),
    "files could not be verified"
  )
})

test_that("real encrypted authentication database accepts only correct passwords", {
  stage <- withr::local_tempdir()
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  material <- builder_auth_create_material(accounts, stage)
  passphrase <- builder_auth_read_env_file(material$env_file)
  withr::defer(passphrase <- NULL)

  expect_true(builder_auth_verify_database_pair(
    material$credentials,
    material$env_file
  ))
  check <- shinymanager::check_credentials(
    material$credentials,
    passphrase = passphrase
  )
  expect_true(check("auth-user-a-7f31", "auth-password-a-7f31")$result)
  expect_true(check("auth-user-b-8c42", "auth-password-b-8c42")$result)
  expect_false(check("auth-user-a-7f31", "wrong-password-9c55")$result)
  database_raw <- readBin(
    material$credentials,
    "raw",
    n = file.info(material$credentials)$size
  )
  env_raw <- readBin(
    material$env_file,
    "raw",
    n = file.info(material$env_file)$size
  )
  expect_false(builder_auth_raw_contains(database_raw, "auth-user-a-7f31"))
  expect_false(builder_auth_raw_contains(database_raw, "auth-password-a-7f31"))
  expect_false(builder_auth_raw_contains(database_raw, passphrase))
  expect_true(builder_auth_raw_contains(env_raw, passphrase))
})

test_that("database creator output and conditions cannot disclose secrets", {
  stage <- withr::local_tempdir()
  sentinel <- "creator-secret-stderr-6d91"
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  create_db <- function(credentials_data, sqlite_path, passphrase) {
    cat(sentinel, "\n")
    cat(sentinel, "\n", file = stderr())
    message(sentinel)
    warning(sentinel)
    writeBin(as.raw(1:8), sqlite_path)
  }

  output <- capture.output(
    messages <- capture.output(
      material <- builder_auth_create_material(
        accounts,
        stage,
        .create_db = create_db,
        .capability = function() list(available = TRUE, reason = NULL)
      ),
      type = "message"
    ),
    type = "output"
  )

  expect_true(file.exists(material$credentials))
  expect_false(builder_auth_value_contains(c(output, messages), sentinel))
})

test_that("authentication creation faults fail closed with generic cleanup", {
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  cases <- list(
    db_chmod = list(.db_chmod = function(...) stop("fault-secret-db")),
    env_write = list(.write_env = function(...) stop("fault-secret-env")),
    validate = list(.validate_material = function(...) {
      stop("fault-secret-validate")
    }),
    rollback = list(
      .create_db = function(...) stop("fault-secret-create"),
      .rollback = function(...) FALSE
    )
  )
  for (name in names(cases)) {
    stage <- withr::local_tempdir()
    args <- c(
      list(
        accounts = accounts,
        stage = stage,
        .capability = function() list(available = TRUE, reason = NULL)
      ),
      cases[[name]]
    )
    error <- tryCatch(
      do.call(builder_auth_create_material, args),
      error = identity
    )
    expect_s3_class(error, "error")
    expect_false(
      grepl("fault-secret", conditionMessage(error), fixed = TRUE),
      info = name
    )
    if (!identical(name, "rollback")) {
      expect_false(
        file.exists(file.path(stage, ".builder-auth-source")),
        info = name
      )
      expect_false(
        file.exists(file.path(stage, "viewer-auth.env")),
        info = name
      )
    } else {
      expect_match(conditionMessage(error), "could not be cleaned up")
    }
  }
})

test_that("environment writer rejects chmod rename and mode faults", {
  passphrase <- strrep("f", 64L)
  cases <- list(
    chmod = list(.chmod = function(...) FALSE),
    rename = list(.rename = function(...) FALSE)
  )
  if (!identical(.Platform$OS.type, "windows")) {
    cases$mode <- list(.file_info = function(...) data.frame(mode = 420L))
  }
  for (name in names(cases)) {
    stage <- withr::local_tempdir()
    path <- file.path(stage, "viewer-auth.env")
    error <- tryCatch(
      do.call(
        .builder_auth_write_env,
        c(list(path, passphrase), cases[[name]])
      ),
      error = identity
    )
    expect_s3_class(error, "error")
    expect_false(file.exists(path), info = name)
  }
})

test_that("environment writer fails closed on symlinks and lying temp cleanup", {
  passphrase <- strrep("b", 64L)
  stage <- withr::local_tempdir()
  target <- file.path(stage, "target.env")
  writeLines("safe", target)
  link <- file.path(stage, "viewer-auth.env")
  if (isTRUE(suppressWarnings(file.symlink(target, link)))) {
    expect_error(.builder_auth_write_env(link, passphrase), "target is invalid")
    expect_identical(readLines(target), "safe")
  }

  path <- file.path(stage, "another-viewer-auth.env")
  ## The cleanup failure raised on exit supersedes the write failure.
  error <- tryCatch(
    .builder_auth_write_env(
      path,
      passphrase,
      .write_lines = function(lines, con, ...) {
        writeLines(lines, con)
        stop("write-fault-secret")
      },
      .unlink = function(...) 0L
    ),
    error = identity
  )
  expect_match(
    conditionMessage(error),
    "temporary file could not be cleaned up"
  )
  expect_false(file.exists(path))
})

test_that("strict material cleanup rejects tampering links and lying unlink", {
  accounts <- builder_auth_validate_payload(
    TRUE,
    builder_auth_test_accounts()
  )$accounts
  stage <- withr::local_tempdir()
  material <- builder_auth_create_material(
    accounts,
    stage,
    .capability = function() list(available = TRUE, reason = NULL)
  )
  tampered <- material
  tampered$source_dir <- dirname(stage)
  expect_error(
    builder_auth_cleanup_material(tampered, stage),
    "cleanup request is unsafe"
  )
  expect_true(file.exists(material$credentials))

  expect_error(
    builder_auth_cleanup_material(
      material,
      stage,
      .unlink = function(...) 0L
    ),
    "could not be cleaned up"
  )
  expect_true(file.exists(material$credentials))

  unlink(material$env_file)
  outside <- file.path(dirname(stage), paste0(basename(stage), "-outside.env"))
  withr::defer(unlink(outside))
  writeLines("outside", outside)
  if (isTRUE(suppressWarnings(file.symlink(outside, material$env_file)))) {
    expect_error(
      builder_auth_cleanup_material(material, stage),
      "cleanup request is unsafe"
    )
    expect_identical(readLines(outside), "outside")
  }
})
