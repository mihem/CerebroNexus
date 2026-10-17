linux_launcher_fixture <- function(local_env = parent.frame()) {
  skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  skip_if(identical(Sys.info()[["effective_user"]], "root"))
  root <- withr::local_tempdir(
    pattern = "viewer with spaces ",
    .local_envir = local_env
  )
  dir.create(file.path(root, "viewer"))
  dir.create(file.path(root, "private-data", "auth"), recursive = TRUE)
  launcher <- file.path(root, "viewer", "start-linux.sh")
  expect_true(file.copy(viewer_test_path("start-linux.sh"), launcher))
  writeLines("# fixture", file.path(root, "app.R"))
  database <- file.path(root, "private-data", "auth", "credentials.sqlite")
  secret <- file.path(root, "viewer-auth.env")
  writeLines("encrypted database fixture", database)
  writeLines(paste0("CEREBRO_AUTH_PASSPHRASE=", strrep("a", 64)), secret)
  Sys.chmod(c(secret, database), "0644", use_umask = FALSE)
  Sys.chmod(dirname(database), "0755", use_umask = FALSE)
  list(root = root, launcher = launcher, database = database, secret = secret)
}

linux_launcher_run <- function(fixture, args = "--prepare-only") {
  output <- suppressWarnings(system2(
    "bash",
    c(shQuote(fixture$launcher), args),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(output, "status")
  list(
    status = if (is.null(status)) 0L else status,
    output = paste(output, collapse = "\n")
  )
}

test_that("Linux entry point repairs copied modes without changing authentication", {
  fixture <- linux_launcher_fixture()
  before <- tools::md5sum(c(fixture$database, fixture$secret))
  result <- linux_launcher_run(fixture)
  expect_identical(result$status, 0L, info = result$output)
  expect_identical(as.integer(file.info(fixture$secret)$mode), 384L)
  expect_identical(as.integer(file.info(fixture$database)$mode), 384L)
  expect_identical(as.integer(file.info(dirname(fixture$database))$mode), 448L)
  expect_identical(tools::md5sum(c(fixture$database, fixture$secret)), before)
  expect_false(grepl(strrep("a", 64), result$output, fixed = TRUE))
  # Re-running setup must not chmod/chown already-correct files.
  bin <- file.path(fixture$root, "bin")
  dir.create(bin)
  for (command in c("chmod", "chown")) {
    path <- file.path(bin, command)
    writeLines(c("#!/bin/sh", "exit 91"), path)
    Sys.chmod(path, "0755")
  }
  withr::local_envvar(PATH = paste(bin, Sys.getenv("PATH"), sep = ":"))
  result <- linux_launcher_run(fixture)
  expect_identical(result$status, 0L, info = result$output)
  auth <- new.env(parent = globalenv())
  sys.source(viewer_test_path("auth.R"), auth)
  withr::local_envvar(CEREBRO_AUTH_PASSPHRASE = NA)
  expect_true(auth$.viewer_auth_load_local_passphrase(
    fixture$root,
    "CEREBRO_AUTH_PASSPHRASE"
  ))
})

test_that("Linux entry point rejects links before changing any permissions", {
  fixture <- linux_launcher_fixture()
  outside <- file.path(fixture$root, "outside.env")
  file.rename(fixture$secret, outside)
  expect_true(file.symlink(outside, fixture$secret))
  result <- linux_launcher_run(fixture)
  expect_identical(result$status, 1L)
  expect_match(result$output, "regular viewer-auth.env")
  expect_identical(as.integer(file.info(outside)$mode), 420L)
  expect_identical(as.integer(file.info(dirname(fixture$database))$mode), 493L)
  unlink(fixture$secret)
  expect_true(file.link(outside, fixture$secret))
  result <- linux_launcher_run(fixture)
  expect_identical(result$status, 1L)
  expect_match(result$output, "hard-linked")
  unlink(fixture$secret)
  file.rename(outside, fixture$secret)
  auth_dir <- dirname(fixture$database)
  moved <- file.path(fixture$root, "moved-auth")
  file.rename(auth_dir, moved)
  expect_true(file.symlink(moved, auth_dir))
  expect_match(linux_launcher_run(fixture)$output, "real directory")
})

test_that("Linux entry point stops on failed or ineffective chmod", {
  fixture <- linux_launcher_fixture()
  bin <- file.path(fixture$root, "bin")
  dir.create(bin)
  chmod <- file.path(bin, "chmod")
  writeLines(c("#!/bin/sh", "exit 1"), chmod)
  Sys.chmod(chmod, "0755")
  withr::local_envvar(PATH = paste(bin, Sys.getenv("PATH"), sep = ":"))
  result <- linux_launcher_run(fixture)
  expect_identical(result$status, 1L)
  expect_match(result$output, "read-only deployment")
  writeLines(c("#!/bin/sh", "exit 0"), chmod)
  result <- linux_launcher_run(fixture)
  expect_identical(result$status, 1L)
  expect_match(result$output, "Permissions did not persist")
})

test_that("Linux entry point dispatches R only after setup and keeps options strict", {
  fixture <- linux_launcher_fixture()
  bin <- file.path(fixture$root, "bin")
  dir.create(bin)
  rscript <- file.path(bin, "Rscript")
  writeLines(c("#!/bin/sh", "pwd", "printf '%s\\n' \"$@\""), rscript)
  Sys.chmod(rscript, "0755")
  withr::local_envvar(PATH = paste(bin, Sys.getenv("PATH"), sep = ":"))
  result <- linux_launcher_run(fixture, character())
  expect_identical(result$status, 0L, info = result$output)
  expect_match(result$output, fixture$root, fixed = TRUE)
  expect_match(
    result$output,
    'shiny::runApp(".", launch.browser = FALSE)',
    fixed = TRUE
  )
  expect_match(
    linux_launcher_run(fixture, "--unknown")$output,
    "Unknown argument"
  )
  expect_match(
    linux_launcher_run(fixture, "--user")$output,
    "requires an account name"
  )
  expect_match(
    linux_launcher_run(fixture, "--user nobody")$output,
    "requires --prepare-only"
  )
  expect_match(
    linux_launcher_run(fixture, "--prepare-only --user root")$output,
    "non-root"
  )
  expect_match(
    linux_launcher_run(fixture, "--prepare-only --user nobody")$output,
    "Run as the runtime user"
  )
  unlink(fixture$secret)
  # Scripted exports may supply the key through the process environment instead.
  expect_identical(linux_launcher_run(fixture)$status, 0L)
  unlink(dirname(fixture$database), recursive = TRUE)
  # Public Apps remain usable through the same entry point.
  expect_identical(linux_launcher_run(fixture)$status, 0L)
})
