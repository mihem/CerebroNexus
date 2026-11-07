builder_project_integrity_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_test_inst_path("builder", "prerequisite.R"),
    envir = runtime
  )
  runtime$builder_source_utf8(
    builder_test_inst_path(
      "builder",
      "core",
      "bundle_path_contract.R"
    ),
    envir = runtime
  )
  runtime$builder_source_utf8(
    builder_test_inst_path(
      "viewer",
      "core",
      "spatial_coordinate_transform.R"
    ),
    envir = runtime
  )
  for (file in c("io.R", "worker.R", "extras.R", "project.R", "build.R")) {
    runtime$builder_source_utf8(
      builder_test_inst_path("builder", file),
      envir = runtime
    )
  }
  runtime
}

builder_project_integrity_entry <- function(name = "dataset", revision = 1L) {
  list(
    id = "ds1",
    revision = as.integer(revision),
    settings = list(name = name),
    acknowledgements = character(),
    spatial_drafts = list()
  )
}

builder_project_integrity_manifest <- function(runtime, root, entry) {
  manifest <- runtime$builder_project_new_manifest(root, "Integrity fixture")
  artifact_digest <- runtime$builder_project_artifact_configuration_digest(
    entry
  )
  artifact_path <- file.path(
    root,
    "artifacts",
    entry$id,
    paste0(artifact_digest, ".crb")
  )
  dir.create(dirname(artifact_path), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(artifact_path)) {
    writeBin(charToRaw(artifact_digest), artifact_path)
  }
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = runtime$builder_project_relative_path(artifact_path, root),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_configuration = artifact_digest,
    built_from_source_fingerprint = NULL
  )
  manifest$datasets <- list(runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    checked = TRUE,
    artifact = artifact,
    root = root
  ))
  manifest
}

builder_project_integrity_bytes <- function(path) {
  readBin(path, what = "raw", n = as.integer(file.info(path)$size[[1L]]))
}

test_that("configuration publication closes, parses, and verifies before commit", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  entry <- builder_project_integrity_entry()

  expect_error(
    runtime$builder_project_write_dataset_config(
      entry,
      root,
      .write = function(payload, path) stop("injected write failure")
    ),
    "could not be written",
    fixed = TRUE
  )
  expect_length(list.files(root, pattern = "\\.json$", recursive = TRUE), 0L)

  expect_error(
    runtime$builder_project_write_dataset_config(
      entry,
      root,
      .move = function(from, to) FALSE
    ),
    "could not be committed",
    fixed = TRUE
  )
  expect_length(list.files(root, pattern = "\\.part$", recursive = TRUE), 0L)

  descriptor <- runtime$builder_project_write_dataset_config(entry, root)
  record <- list(
    id = entry$id,
    configuration = utils::modifyList(
      descriptor,
      list(digest = runtime$builder_project_configuration_digest(entry))
    )
  )
  expect_identical(
    runtime$builder_project_read_dataset_config(record, root)$revision,
    entry$revision
  )
})

test_that("a configuration write into a read-only folder names the cause", {
  skip_on_os("windows")
  skip_if(identical(unname(Sys.info()[["effective_user"]]), "root"))
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  configs <- file.path(root, "datasets", "ds1", "configs")
  dir.create(configs, recursive = TRUE)
  Sys.chmod(configs, "0555")
  withr::defer(Sys.chmod(configs, "0755"))

  expect_error(
    runtime$builder_project_write_dataset_config(
      builder_project_integrity_entry(),
      root
    ),
    "could not be written: .*Permission denied.*write to the project folder"
  )
})

test_that("unchanged immutable configurations skip temporary publication", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  entry <- builder_project_integrity_entry()
  first <- runtime$builder_project_write_dataset_config(entry, root)
  writes <- 0L

  second <- runtime$builder_project_write_dataset_config(
    entry,
    root,
    .write = function(...) {
      writes <<- writes + 1L
      stop("unchanged configuration should not be rewritten")
    }
  )

  expect_identical(second$path, first$path)
  expect_identical(writes, 0L)
  writeBin(charToRaw("corrupt"), file.path(root, second$path))
  expect_error(
    runtime$builder_project_write_dataset_config(entry, root),
    "integrity",
    fixed = TRUE
  )
})

test_that("manifest write and Windows rename failures preserve manifest and backup", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  first_entry <- builder_project_integrity_entry("first", 1L)
  first <- runtime$builder_project_write(
    builder_project_integrity_manifest(runtime, root, first_entry),
    root
  )
  second_entry <- builder_project_integrity_entry("second", 2L)
  second_manifest <- builder_project_integrity_manifest(
    runtime,
    root,
    second_entry
  )
  second_manifest$project <- first$manifest$project
  second <- runtime$builder_project_write(
    second_manifest,
    root,
    expected_revision = first$manifest$project$revision
  )
  target <- second$path
  backup <- paste0(target, ".bak")
  target_before <- builder_project_integrity_bytes(target)
  backup_before <- builder_project_integrity_bytes(backup)

  expect_error(
    runtime$builder_project_write(
      second$manifest,
      root,
      expected_revision = second$manifest$project$revision,
      .write_json = function(...) stop("injected manifest write failure")
    ),
    "could not be written"
  )
  expect_identical(builder_project_integrity_bytes(target), target_before)
  expect_identical(builder_project_integrity_bytes(backup), backup_before)

  move <- function(from, to) {
    if (
      startsWith(basename(from), "builder-project-") &&
        identical(basename(to), "builder-project.json")
    ) {
      return(FALSE)
    }
    file.rename(from, to)
  }
  expect_error(
    runtime$builder_project_write(
      second$manifest,
      root,
      expected_revision = second$manifest$project$revision,
      .move = move
    ),
    "could not be committed"
  )
  expect_identical(builder_project_integrity_bytes(target), target_before)
  expect_identical(builder_project_integrity_bytes(backup), backup_before)
})

test_that("interruption after manifest rename rolls back without a dangling record", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  first_entry <- builder_project_integrity_entry("first", 1L)
  first <- runtime$builder_project_write(
    builder_project_integrity_manifest(runtime, root, first_entry),
    root
  )
  target_before <- builder_project_integrity_bytes(first$path)
  second_entry <- builder_project_integrity_entry("second", 2L)
  second_manifest <- builder_project_integrity_manifest(
    runtime,
    root,
    second_entry
  )
  second_manifest$project <- first$manifest$project
  second_sidecar <- runtime$builder_project_resolve_path(
    second_manifest$datasets[[1L]]$configuration$path,
    root,
    "managed"
  )

  expect_error(
    runtime$builder_project_write(
      second_manifest,
      root,
      expected_revision = first$manifest$project$revision,
      .after_commit = function(path) unlink(second_sidecar, force = TRUE)
    ),
    "post-commit validation"
  )
  expect_identical(builder_project_integrity_bytes(first$path), target_before)
  expect_false(file.exists(paste0(first$path, ".bak")))
  expect_true(runtime$builder_project_diagnose(first$path)$manifest$valid)
})

test_that("verified Project backups require explicit atomic recovery", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  first_entry <- builder_project_integrity_entry("first", 1L)
  first <- runtime$builder_project_write(
    builder_project_integrity_manifest(runtime, root, first_entry),
    root
  )
  second_entry <- builder_project_integrity_entry("second", 2L)
  second_manifest <- builder_project_integrity_manifest(
    runtime,
    root,
    second_entry
  )
  second_manifest$project <- first$manifest$project
  second <- runtime$builder_project_write(
    second_manifest,
    root,
    expected_revision = first$manifest$project$revision
  )
  backup <- paste0(second$path, ".bak")
  backup_before <- builder_project_integrity_bytes(backup)
  writeLines("{damaged", second$path, useBytes = TRUE)
  damaged_before <- builder_project_integrity_bytes(second$path)

  offer <- runtime$builder_project_backup_recovery_offer(second$path)
  expect_identical(offer$project_revision, first$manifest$project$revision)
  expect_identical(offer$dataset_count, 1L)
  expect_error(
    runtime$builder_project_read(second$path),
    "project file is damaged"
  )

  recovered <- runtime$builder_project_recover_backup(
    second$path,
    offer$primary_fingerprint,
    offer$backup_fingerprint
  )
  expect_identical(
    runtime$builder_project_read(second$path)$project$revision,
    first$manifest$project$revision
  )
  expect_identical(builder_project_integrity_bytes(backup), backup_before)
  expect_identical(
    builder_project_integrity_bytes(recovered$displaced_path),
    damaged_before
  )
  expect_true(runtime$builder_project_diagnose(second$path)$manifest$valid)
})

test_that("v2 configuration identity survives JSON normalization", {
  runtime <- builder_project_integrity_runtime()
  entry <- builder_project_integrity_entry("typed", 4L)
  entry$settings$typed <- list(
    integer = 1:3,
    factor = factor(c("b", "a"), levels = c("a", "b")),
    matrix = matrix(seq_len(4L), nrow = 2L),
    timestamp = as.POSIXct("2026-09-23 12:34:56", tz = "UTC")
  )
  persisted <- jsonlite::unserializeJSON(jsonlite::serializeJSON(
    runtime$builder_project_configuration_entry(entry),
    digits = NA,
    pretty = TRUE
  ))

  expect_identical(
    runtime$builder_project_configuration_digest(entry),
    runtime$builder_project_configuration_digest(persisted)
  )
})

test_that("a missing configuration file is rejected on the next save", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  entry <- builder_project_integrity_entry("missing", 5L)
  manifest <- builder_project_integrity_manifest(runtime, root, entry)

  saved <- runtime$builder_project_write(
    manifest,
    root,
    expected_revision = 0L
  )
  saved_configuration <- saved$manifest$datasets[[1L]]$configuration
  expect_identical(
    saved_configuration$contract_version,
    runtime$.builder_project_configuration_contract_version
  )
  expect_true(runtime$builder_project_diagnose(saved$path)$manifest$valid)

  sidecar <- runtime$builder_project_resolve_path(
    saved_configuration$path,
    root,
    "managed"
  )
  unlink(sidecar, force = TRUE)
  expect_error(
    runtime$builder_project_write(
      saved$manifest,
      root,
      expected_revision = saved$manifest$project$revision
    ),
    "incomplete"
  )
})

test_that("checkpoint cleanup cannot remove manifest or backup configurations", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  first_entry <- builder_project_integrity_entry("first", 1L)
  first <- runtime$builder_project_write(
    builder_project_integrity_manifest(runtime, root, first_entry),
    root
  )
  second_entry <- builder_project_integrity_entry("second", 2L)
  second_manifest <- builder_project_integrity_manifest(
    runtime,
    root,
    second_entry
  )
  second_manifest$project <- first$manifest$project
  second <- runtime$builder_project_write(
    second_manifest,
    root,
    expected_revision = first$manifest$project$revision
  )
  current_config <- runtime$builder_project_resolve_path(
    second$manifest$datasets[[1L]]$configuration$path,
    root,
    "managed"
  )
  backup_manifest <- runtime$builder_project_read(paste0(second$path, ".bak"))
  backup_config <- runtime$builder_project_resolve_path(
    backup_manifest$datasets[[1L]]$configuration$path,
    root,
    "managed"
  )
  checkpoint <- file.path(root, "checkpoints", "failed-generation")
  dir.create(checkpoint, recursive = TRUE)
  writeLines("temporary", file.path(checkpoint, "worker.txt"))

  expect_true(runtime$builder_project_cleanup_checkpoint(checkpoint, root))
  expect_true(file.exists(current_config))
  expect_true(file.exists(backup_config))
  expect_true(runtime$builder_project_diagnose(second$path)$manifest$valid)
  expect_true(runtime$builder_project_diagnose(second$path)$backup$valid)
})

test_that("a negative zero setting saves and reads back with its identity", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  entry <- builder_project_integrity_entry("zero", 6L)
  entry$settings$alignment <- list(dx = round(-0.3), dy = c(0, -0))

  descriptor <- runtime$builder_project_write_dataset_config(entry, root)
  record <- list(
    id = entry$id,
    configuration = utils::modifyList(
      descriptor,
      list(digest = runtime$builder_project_configuration_digest(entry))
    )
  )
  restored <- runtime$builder_project_read_dataset_config(record, root)

  expect_identical(restored$settings$alignment$dx, 0)
  expect_identical(
    runtime$builder_project_configuration_digest(restored),
    record$configuration$digest
  )
})

test_that("non-ASCII text survives two saves in a non-UTF-8 locale", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  withr::local_locale(c(LC_CTYPE = "C"))
  skip_if_not(identical(Sys.getlocale("LC_CTYPE"), "C"))
  name <- "Zürich"
  entry <- builder_project_integrity_entry(name, 7L)

  first <- runtime$builder_project_write(
    builder_project_integrity_manifest(runtime, root, entry),
    root,
    expected_revision = 0L
  )
  reopened <- runtime$builder_project_read(first$path)
  restored <- runtime$builder_project_read_dataset_config(
    reopened$datasets[[1L]],
    root
  )
  expect_identical(restored$settings$name, name)
  expect_identical(reopened$datasets[[1L]]$name, name)

  second_descriptor <- runtime$builder_project_write_dataset_config(
    restored,
    root
  )
  expect_identical(
    second_descriptor$path,
    first$manifest$datasets[[1L]]$configuration$path
  )
})

test_that("Save, Open and Save again under LC_ALL=C keep non-ASCII bytes", {
  skip_if_not_installed("callr")
  skip_on_os("windows")
  files <- vapply(
    c(
      "builder/prerequisite.R",
      "builder/core/bundle_path_contract.R",
      "viewer/core/spatial_coordinate_transform.R",
      "builder/io.R",
      "builder/worker.R",
      "builder/extras.R",
      "builder/project.R",
      "builder/build.R"
    ),
    function(file) normalizePath(builder_test_inst_path(file)),
    character(1)
  )
  make_entry <- builder_project_integrity_entry
  make_manifest <- builder_project_integrity_manifest
  environment(make_entry) <- globalenv()
  environment(make_manifest) <- globalenv()
  root <- withr::local_tempdir()
  utf8 <- intToUtf8(c(90L, 252L, 114L, 105L, 99L, 104L, 32L, 8230L))

  cycle <- callr::r(
    function(files, root, make_entry, make_manifest, utf8) {
      runtime <- new.env(parent = globalenv())
      sys.source(files[[1L]], envir = runtime)
      for (file in files[-1L]) {
        runtime$builder_source_utf8(file, envir = runtime)
      }
      bytes <- function(path) {
        readBin(path, what = "raw", n = file.info(path)$size)
      }
      # Text the way R holds it after readRDS() of an object saved by a UTF-8
      # session or from a file name: UTF-8 bytes without an encoding mark.
      native <- rawToChar(charToRaw(utf8))
      entry <- make_entry(native, 7L)
      entry$settings$groups <- list(
        column = native,
        levels = factor(native),
        labels = stats::setNames(native, native)
      )
      first <- runtime$builder_project_write(
        make_manifest(runtime, root, entry),
        root,
        expected_revision = 0L
      )
      sidecar <- file.path(
        root,
        first$manifest$datasets[[1L]]$configuration$path
      )
      first_sidecar <- bytes(sidecar)
      first_manifest <- bytes(first$path)

      reopened <- runtime$builder_project_read(first$path)
      restored <- runtime$builder_project_read_dataset_config(
        reopened$datasets[[1L]],
        root
      )
      second <- runtime$builder_project_write(
        make_manifest(runtime, root, restored),
        root,
        expected_revision = 1L
      )
      list(
        locale = Sys.getlocale("LC_CTYPE"),
        native_encoding = Encoding(native),
        reopened_name = restored$settings$name,
        reopened_level = levels(restored$settings$groups$levels),
        first_sidecar = first_sidecar,
        second_sidecar = bytes(
          file.path(root, second$manifest$datasets[[1L]]$configuration$path)
        ),
        first_manifest = first_manifest,
        second_manifest = bytes(second$path)
      )
    },
    args = list(
      files = files,
      root = root,
      make_entry = make_entry,
      make_manifest = make_manifest,
      utf8 = utf8
    ),
    env = c(callr::rcmd_safe_env(), LC_ALL = "C", LANG = "C")
  )

  expect_identical(cycle$locale, "C")
  expect_identical(cycle$native_encoding, "unknown")
  expect_identical(enc2utf8(cycle$reopened_name), utf8)
  expect_identical(enc2utf8(cycle$reopened_level), utf8)
  has_bytes <- function(haystack, needle) {
    grepl(needle, rawToChar(haystack), fixed = TRUE, useBytes = TRUE)
  }
  for (payload in cycle[c(
    "first_sidecar",
    "second_sidecar",
    "first_manifest",
    "second_manifest"
  )]) {
    expect_true(has_bytes(payload, rawToChar(charToRaw(utf8))))
    expect_false(has_bytes(payload, "<c3>"))
  }
  expect_identical(cycle$second_sidecar, cycle$first_sidecar)
})

builder_project_integrity_lock_owner <- function(path, pid, host) {
  dir.create(path)
  writeLines("stale-token", file.path(path, "owner-token"))
  writeLines(
    c(
      "schema=1",
      paste0("pid=", pid),
      paste0("host=", encodeString(host, quote = '"'))
    ),
    file.path(path, "owner.txt")
  )
}

test_that("a stale Project write lock is taken over, a live one is not", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  path <- runtime$builder_project_manifest_lock_path(root)
  host <- runtime$.builder_project_host()
  candidates <- seq(4194303L, by = -1L, length.out = 200L)
  dead <- candidates[
    !vapply(
      candidates,
      runtime$.builder_project_pid_alive,
      logical(1)
    )
  ][[1L]]

  builder_project_integrity_lock_owner(path, Sys.getpid(), host)
  expect_error(
    runtime$builder_project_acquire_manifest_lock(root),
    "Another Builder window",
    fixed = TRUE
  )
  unlink(path, recursive = TRUE)

  builder_project_integrity_lock_owner(path, dead, "another-host")
  expect_error(
    runtime$builder_project_acquire_manifest_lock(root),
    "Another Builder window",
    fixed = TRUE
  )
  unlink(path, recursive = TRUE)

  builder_project_integrity_lock_owner(path, dead, host)
  lock <- runtime$builder_project_acquire_manifest_lock(root)
  expect_true(runtime$builder_project_release_manifest_lock(lock))
  expect_false(dir.exists(path))

  ## An owner file that was never written is stale after the grace period.
  dir.create(path)
  file.create(file.path(path, "owner.txt"))
  expect_error(
    runtime$builder_project_acquire_manifest_lock(root),
    "Another Builder window",
    fixed = TRUE
  )
  Sys.setFileTime(path, Sys.time() - 120)
  lock <- runtime$builder_project_acquire_manifest_lock(root)
  expect_true(runtime$builder_project_release_manifest_lock(lock))
  expect_length(list.files(root, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("a Project write lock that cannot be written fails and is removed", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  runtime$writeLines <- function(...) {
    warning("No space left on device")
  }
  expect_error(
    runtime$builder_project_acquire_manifest_lock(root),
    "could not be written (No space left on device)",
    fixed = TRUE
  )
  expect_false(dir.exists(runtime$builder_project_manifest_lock_path(root)))
  rm("writeLines", envir = runtime)
  lock <- runtime$builder_project_acquire_manifest_lock(root)
  expect_true(runtime$builder_project_release_manifest_lock(lock))
})

test_that("an unwritable project folder is not reported as a busy lock", {
  skip_on_os("windows")
  skip_if(identical(unname(Sys.info()[["effective_user"]]), "root"))
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  Sys.chmod(root, mode = "0500")
  withr::defer(Sys.chmod(root, mode = "0700"))
  expect_error(
    runtime$builder_project_acquire_manifest_lock(root),
    "The project folder cannot be written",
    fixed = TRUE
  )
})

test_that("a project left with only its backup can be opened and restored", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  first_entry <- builder_project_integrity_entry("first", 1L)
  first <- runtime$builder_project_write(
    builder_project_integrity_manifest(runtime, root, first_entry),
    root
  )
  second_entry <- builder_project_integrity_entry("second", 2L)
  second_manifest <- builder_project_integrity_manifest(
    runtime,
    root,
    second_entry
  )
  second_manifest$project <- first$manifest$project
  second <- runtime$builder_project_write(
    second_manifest,
    root,
    expected_revision = first$manifest$project$revision
  )
  ## Killed after "manifest -> .bak", before "staged -> manifest".
  backup <- paste0(second$path, ".bak")
  expect_true(file.rename(second$path, backup))

  expect_identical(
    runtime$builder_project_open_manifest_path(backup),
    second$path
  )
  expect_error(runtime$builder_project_open_snapshot(second$path))
  offer <- runtime$builder_project_backup_recovery_offer(second$path)
  expect_true(offer$primary_missing)
  expect_null(offer$primary_fingerprint)

  recovered <- runtime$builder_project_recover_backup(
    second$path,
    offer$primary_fingerprint,
    offer$backup_fingerprint
  )
  expect_null(recovered$displaced_path)
  expect_identical(
    runtime$builder_project_read(second$path)$project$revision,
    second$manifest$project$revision
  )
  expect_true(file.exists(backup))
  ## With the manifest back, choosing the backup opens the backup itself.
  expect_identical(runtime$builder_project_open_manifest_path(backup), backup)
})

test_that("a skipped dataset whose source is gone no longer blocks Save", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  uploads <- withr::local_tempdir()
  entry_for <- function(id, name) {
    source <- file.path(uploads, paste0(id, ".rds"))
    saveRDS(list(id = id), source)
    list(
      id = id,
      revision = 1L,
      path = source,
      filename = "sample.rds",
      settings = list(name = name),
      acknowledgements = character(),
      spatial_drafts = list(),
      snapshot = list(
        source_fingerprint = paste0(
          "builder-snapshot-v2:",
          unname(tools::md5sum(source))
        )
      )
    )
  }
  first_entry <- entry_for("ds1", "Sample A")
  second_entry <- entry_for("ds2", "Sample B")
  context <- list(
    checked = character(),
    artifacts = list(),
    skipped_ids = character(),
    configuration = list(),
    last_ui = list()
  )
  save <- function(manifest, entries, context) {
    built <- runtime$builder_project_build_manifest(
      entries,
      list(root = root, manifest = manifest),
      context
    )
    results <- lapply(built$jobs, runtime$builder_project_copy_source_job)
    runtime$builder_project_write(
      runtime$builder_project_apply_source_results(
        built$manifest,
        results,
        root
      ),
      root,
      expected_revision = manifest$project$revision
    )
  }
  saved <- save(
    runtime$builder_project_new_manifest(root, "Skip"),
    list(first_entry, second_entry),
    context
  )
  expect_true(runtime$builder_project_diagnose(saved$path)$manifest$valid)

  ## ds2's source is deleted; the Project is reopened and ds2 skipped.
  unlink(file.path(root, "sources", "ds2"), recursive = TRUE)
  expect_error(
    runtime$builder_project_assert_manifest_integrity(saved$manifest, root),
    "Sample B: required dataset source is missing or changed",
    fixed = TRUE
  )
  context$skipped_ids <- "ds2"
  again <- save(saved$manifest, list(first_entry), context)
  kept <- Filter(function(record) record$id == "ds2", again$manifest$datasets)
  expect_length(kept, 1L)
  expect_identical(kept[[1L]]$source$status, "failed")
  expect_identical(
    kept[[1L]]$configuration$path,
    Filter(function(record) record$id == "ds2", saved$manifest$datasets)[[
      1L
    ]]$configuration$path
  )
  expect_identical(again$manifest$project$revision, 2L)

  third <- save(again$manifest, list(first_entry), context)
  expect_identical(third$manifest$project$revision, 3L)

  ## A broken configuration still blocks the overwrite.
  sidecar <- runtime$builder_project_resolve_path(
    kept[[1L]]$configuration$path,
    root,
    "managed"
  )
  unlink(sidecar)
  expect_error(
    save(third$manifest, list(first_entry), context),
    "incomplete"
  )
})

test_that("Open accepts a source blob that finished copying after the Save", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  uploads <- withr::local_tempdir()
  source <- file.path(uploads, "ds1.rds")
  saveRDS(list(id = "ds1"), source)
  md5 <- unname(tools::md5sum(source))
  entry <- list(
    id = "ds1",
    revision = 1L,
    path = source,
    filename = "sample.rds",
    settings = list(name = "Sample A"),
    acknowledgements = character(),
    spatial_drafts = list(),
    snapshot = list(source_fingerprint = paste0("builder-snapshot-v2:", md5))
  )
  built <- runtime$builder_project_build_manifest(
    list(entry),
    list(root = root, manifest = runtime$builder_project_new_manifest(root)),
    list(
      checked = character(),
      artifacts = list(),
      skipped_ids = character(),
      configuration = list(),
      last_ui = list()
    )
  )
  saved <- runtime$builder_project_write(
    built$manifest,
    root,
    expected_revision = 0L
  )
  expect_identical(saved$manifest$datasets[[1L]]$source$status, "pending")

  pending <- runtime$builder_project_open_snapshot(saved$path)
  expect_identical(
    pending$manifest$datasets[[1L]]$runtime_restore_status$actions,
    "skip"
  )

  ## The copy finishes, but the page is gone before the manifest is updated.
  copied <- runtime$builder_project_copy_source_job(built$jobs[[1L]])
  expect_identical(copied$status, "ready")
  opened <- runtime$builder_project_open_snapshot(saved$path)
  record <- opened$manifest$datasets[[1L]]
  expect_identical(record$source$status, "ready")
  expect_identical(record$source$fingerprint$md5, md5)
  expect_true("resume" %in% record$runtime_restore_status$actions)

  ## A blob whose bytes do not match its address is not accepted.
  writeBin(charToRaw("truncated"), copied$path)
  damaged <- runtime$builder_project_open_snapshot(saved$path)
  expect_identical(damaged$manifest$datasets[[1L]]$source$status, "pending")
})

## Two saved datasets; the source of "Sample B" is deleted afterwards.
builder_project_gone_source_fixture <- function(runtime, root, uploads) {
  entry_for <- function(id, name) {
    source <- file.path(uploads, paste0(id, ".rds"))
    saveRDS(list(id = id), source)
    list(
      id = id,
      revision = 1L,
      path = source,
      filename = "sample.rds",
      settings = list(name = name, groups = c("a", "b"), default_group = "b"),
      acknowledgements = character(),
      spatial_drafts = list(),
      snapshot = list(
        source_fingerprint = paste0(
          "builder-snapshot-v2:",
          unname(tools::md5sum(source))
        )
      )
    )
  }
  entries <- list(entry_for("ds1", "Sample A"), entry_for("ds2", "Sample B"))
  context <- list(
    checked = character(),
    artifacts = list(),
    skipped_ids = character(),
    configuration = list(),
    last_ui = list()
  )
  built <- runtime$builder_project_build_manifest(
    entries,
    list(root = root, manifest = runtime$builder_project_new_manifest(root)),
    context
  )
  results <- lapply(built$jobs, runtime$builder_project_copy_source_job)
  saved <- runtime$builder_project_write(
    runtime$builder_project_apply_source_results(built$manifest, results, root),
    root,
    expected_revision = 0L
  )
  unlink(file.path(root, "sources", "ds2"), recursive = TRUE)
  opened <- runtime$builder_project_open_snapshot(saved$path)
  list(entries = entries, context = context, manifest = opened$manifest)
}

test_that("Open can remove a dataset whose source is gone", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  fixture <- builder_project_gone_source_fixture(
    runtime,
    root,
    withr::local_tempdir()
  )
  manifest <- fixture$manifest
  expect_identical(
    manifest$datasets[[2L]]$runtime_restore_status$actions,
    "skip"
  )
  prepared <- runtime$builder_project_prepare_open_selection(
    manifest,
    root,
    list(ds1 = "resume", ds2 = "remove")
  )
  expect_identical(prepared$removed_ids, "ds2")
  expect_identical(prepared$skipped_ids, "ds2")
  expect_named(prepared$pending_entries, "ds1")

  context <- fixture$context
  context$skipped_ids <- prepared$skipped_ids
  context$removed_ids <- prepared$removed_ids
  built <- runtime$builder_project_build_manifest(
    fixture$entries[1L],
    list(root = root, manifest = manifest),
    context
  )
  expect_identical(
    vapply(built$manifest$datasets, `[[`, character(1), "id"),
    "ds1"
  )
  ## The gone source of the removed dataset does not block the overwrite.
  written <- runtime$builder_project_write(
    built$manifest,
    root,
    expected_revision = manifest$project$revision
  )
  expect_length(written$manifest$datasets, 1L)
  expect_true(runtime$builder_project_diagnose(written$path)$manifest$valid)
})

test_that("Open can relink a dataset whose source is gone to a chosen file", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  uploads <- withr::local_tempdir()
  fixture <- builder_project_gone_source_fixture(runtime, root, uploads)
  record <- fixture$manifest$datasets[[2L]]
  expect_error(
    runtime$builder_project_relink_record(
      record,
      file.path(uploads, "absent.rds"),
      root
    )
  )

  ## The same bytes again: the saved settings come back unchanged.
  same <- file.path(withr::local_tempdir(), "moved.rds")
  file.copy(file.path(uploads, "ds2.rds"), same)
  relinked <- runtime$builder_project_relink_record(record, same, root)
  status <- relinked$runtime_restore_status
  expect_identical(status$actions, c("relink", "reload", "skip"))
  expect_identical(status$default_action, "relink")
  expect_identical(relinked$source$kind, "external")
  expect_identical(relinked$runtime_relink$source, record$source)
  loaded_from <- function(path, settings) {
    list(
      id = "ds2",
      settings = settings,
      profile = list(group_candidates = c("a", "c")),
      snapshot = list(
        source_state = c(
          runtime$builder_project_file_metadata(path),
          list(location = path)
        ),
        source_fingerprint = paste0(
          "builder-snapshot-v2:",
          unname(tools::md5sum(path))
        )
      )
    )
  }
  relinked$runtime_restore_action <- "relink"
  defaults <- list(name = "moved", groups = "c", default_group = "c")
  restored <- runtime$builder_project_finalize_loaded_entry(
    loaded_from(same, defaults),
    relinked,
    root
  )
  expect_true(runtime$builder_project_relink_matched(relinked, restored))
  expect_identical(restored$settings, status$configuration_entry$settings)
  expect_identical(restored$settings$default_group, "b")

  ## Different bytes: settings the new file cannot use fall back.
  changed <- file.path(withr::local_tempdir(), "changed.rds")
  saveRDS(list(id = "other"), changed)
  relinked <- runtime$builder_project_relink_record(record, changed, root)
  relinked$runtime_restore_action <- "relink"
  restored <- runtime$builder_project_finalize_loaded_entry(
    loaded_from(changed, defaults),
    relinked,
    root
  )
  expect_false(runtime$builder_project_relink_matched(relinked, restored))
  expect_identical(restored$settings$name, "Sample B")
  expect_identical(restored$settings$groups, "a")
  expect_identical(restored$settings$default_group, "c")
  expect_null(runtime$builder_project_restored_check_identity(
    relinked,
    restored
  ))

  ## Save copies the chosen file in although the old source is gone.
  restored$path <- changed
  restored$filename <- "changed.rds"
  manifest <- fixture$manifest
  manifest$datasets[[2L]]$source$status <- "failed"
  built <- runtime$builder_project_build_manifest(
    c(fixture$entries[1L], list(restored)),
    list(root = root, manifest = manifest),
    fixture$context
  )
  written <- runtime$builder_project_write(
    built$manifest,
    root,
    expected_revision = manifest$project$revision
  )
  results <- lapply(built$jobs, runtime$builder_project_copy_source_job)
  final <- runtime$builder_project_write(
    runtime$builder_project_apply_source_results(
      written$manifest,
      results,
      root
    ),
    root,
    expected_revision = written$manifest$project$revision
  )
  expect_identical(final$manifest$datasets[[2L]]$source$status, "ready")
  expect_identical(
    final$manifest$datasets[[2L]]$source$fingerprint$md5,
    unname(tools::md5sum(changed))
  )
  expect_true(runtime$builder_project_diagnose(final$path)$manifest$valid)
})

test_that("a relinked different file keeps the saved choices it still has", {
  runtime <- builder_project_integrity_runtime()
  saved <- list(
    name = "Sample B",
    organism = "mm",
    palette = "viridis",
    assay = "RNA",
    layer = "counts",
    groups = c("a", "b"),
    included_groups = c("a", "b"),
    default_group = "b",
    reductions = c("UMAP", "PCA"),
    included_projections = c("UMAP", "PCA"),
    initial_projections = "PCA",
    default_projection = "PCA",
    tables = list(list(name = "old table"))
  )
  loaded <- list(
    profile = list(
      assay_profiles = list(RNA = list(layers = c("counts", "data"))),
      group_candidates = c("a", "c"),
      reductions = c("UMAP", "TSNE")
    ),
    settings = list(
      name = "moved",
      organism = "hg",
      palette = "cerebro",
      assay = "RNA",
      layer = "data",
      groups = "c",
      included_groups = "c",
      default_group = "c",
      reductions = "TSNE",
      included_projections = "TSNE",
      initial_projections = "TSNE",
      default_projection = "TSNE",
      tables = list()
    )
  )
  settings <- runtime$builder_project_relinked_settings(saved, loaded)
  expect_identical(settings$name, "Sample B")
  expect_identical(settings$organism, "mm")
  expect_identical(settings$palette, "viridis")
  expect_identical(settings$layer, "counts")
  expect_identical(settings$included_groups, "a")
  expect_identical(settings$default_group, "a")
  expect_identical(settings$included_projections, "UMAP")
  ## PCA is gone, so the initial projection falls back to a kept one.
  expect_identical(settings$initial_projections, "UMAP")
  expect_identical(settings$default_projection, "UMAP")
  expect_identical(settings$tables, list())

  ## An assay or layer the new file lacks takes the new defaults.
  saved$layer <- "scaled"
  expect_identical(
    runtime$builder_project_relinked_settings(saved, loaded)$layer,
    "data"
  )
})

## Two datasets saved with their sources copied into the Project.
builder_project_two_source_fixture <- function(runtime, root, uploads) {
  entry_for <- function(id, name) {
    source <- file.path(uploads, paste0(id, ".rds"))
    saveRDS(list(id = id), source)
    list(
      id = id,
      revision = 1L,
      path = source,
      filename = "sample.rds",
      settings = list(name = name),
      acknowledgements = character(),
      spatial_drafts = list(),
      snapshot = list(
        source_fingerprint = paste0(
          "builder-snapshot-v2:",
          unname(tools::md5sum(source))
        )
      )
    )
  }
  entries <- list(entry_for("ds1", "Sample A"), entry_for("ds2", "Sample B"))
  context <- list(
    checked = character(),
    artifacts = list(),
    skipped_ids = character(),
    configuration = list(),
    last_ui = list()
  )
  built <- runtime$builder_project_build_manifest(
    entries,
    list(root = root, manifest = runtime$builder_project_new_manifest(root)),
    context
  )
  results <- lapply(built$jobs, runtime$builder_project_copy_source_job)
  saved <- runtime$builder_project_write(
    runtime$builder_project_apply_source_results(built$manifest, results, root),
    root,
    expected_revision = 0L
  )
  list(entries = entries, context = context, saved = saved)
}

test_that("a Save prunes Project files that nothing refers to any more", {
  runtime <- builder_project_integrity_runtime()
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  fixture <- builder_project_two_source_fixture(
    runtime,
    root,
    withr::local_tempdir()
  )
  saved <- fixture$saved
  record_of <- function(manifest, id) {
    Filter(function(record) record$id == id, manifest$datasets)[[1L]]
  }
  managed <- function(relative) file.path(root, relative)
  ds2_source <- managed(record_of(saved$manifest, "ds2")$source$path)
  ds2_config <- managed(record_of(saved$manifest, "ds2")$configuration$path)

  ## ds2 is removed in the session and the Project saved again: the backup
  ## still lists it, so its files stay.
  context <- fixture$context
  context$removed_ids <- "ds2"
  built <- runtime$builder_project_build_manifest(
    fixture$entries[1L],
    list(root = root, manifest = saved$manifest),
    context
  )
  second <- runtime$builder_project_write(
    built$manifest,
    root,
    expected_revision = saved$manifest$project$revision,
    keep = list()
  )

  write_stray <- function(relative) {
    path <- managed(relative)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw("stray"), path)
    path
  }
  ds1 <- record_of(second$manifest, "ds1")
  ds1_blob <- managed(ds1$source$path)
  old_blob <- write_stray(paste0(
    "sources/ds1/blobs/",
    strrep("0", 32L),
    "/source.rds"
  ))
  old_config <- write_stray(paste0(
    "datasets/ds1/configs/",
    strrep("1", 32L),
    ".json"
  ))
  old_asset <- write_stray("spatial-assets/ds9/fov/image.png")
  killed_part <- paste0(ds1_blob, ".999999999-dead.part")
  writeBin(charToRaw("partial"), killed_part)
  live_part <- paste0(ds1_blob, ".", Sys.getpid(), "-live.part")
  writeBin(charToRaw("partial"), live_part)
  outside <- file.path(root, "notes.txt")
  writeBin(charToRaw("mine"), outside)
  checkpoint <- write_stray("checkpoints/run/file.txt")

  ## Every stray is new: within the grace period nothing is removed.
  expect_length(
    runtime$builder_project_prune_unreferenced(root, second$manifest),
    0L
  )
  expect_true(file.exists(old_blob))

  later <- Sys.time() + 2 * runtime$.builder_project_prune_grace_seconds
  removed <- runtime$builder_project_prune_unreferenced(
    root,
    second$manifest,
    now = later
  )
  expect_setequal(
    removed,
    c(
      paste0("sources/ds1/blobs/", strrep("0", 32L), "/source.rds"),
      paste0("datasets/ds1/configs/", strrep("1", 32L), ".json"),
      "spatial-assets/ds9/fov/image.png",
      substring(killed_part, nchar(root) + 2L)
    )
  )
  expect_false(file.exists(old_config))
  expect_false(dir.exists(managed("spatial-assets/ds9")))
  expect_false(dir.exists(dirname(old_blob)))
  expect_true(file.exists(live_part))
  expect_true(file.exists(outside))
  expect_true(file.exists(checkpoint))
  expect_true(file.exists(ds1_blob))
  expect_true(file.exists(ds2_source))
  expect_true(file.exists(ds2_config))
  expect_true(runtime$builder_project_diagnose(second$path)$manifest$valid)
  expect_no_error(runtime$builder_project_assert_manifest_integrity(
    runtime$builder_project_read(paste0(second$path, ".bak")),
    root
  ))

  ## Once the backup no longer lists ds2 either, its files go, except what
  ## the live workspace still refers to (a dataset Undo can bring back).
  third <- runtime$builder_project_write(
    second$manifest,
    root,
    expected_revision = second$manifest$project$revision,
    keep = list()
  )
  runtime$builder_project_prune_unreferenced(
    root,
    third$manifest,
    keep = list(entry = list(path = ds2_source)),
    now = later
  )
  expect_true(file.exists(ds2_source))
  expect_false(file.exists(ds2_config))
  runtime$builder_project_prune_unreferenced(root, third$manifest, now = later)
  expect_false(file.exists(ds2_source))
  expect_true(runtime$builder_project_diagnose(third$path)$manifest$valid)
})

test_that("Open offers no resume for an unreadable or changed Project source", {
  runtime <- builder_project_integrity_runtime()
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  saved <- builder_project_two_source_fixture(
    runtime,
    root,
    withr::local_tempdir()
  )$saved
  blob_of <- function(id) {
    record <- Filter(function(record) record$id == id, saved$manifest$datasets)
    file.path(root, record[[1L]]$source$path)
  }
  ## A file of another size was put in place of ds1's copy.
  writeBin(charToRaw("swapped"), blob_of("ds1"))
  opened <- runtime$builder_project_open_snapshot(saved$path)
  status <- opened$manifest$datasets[[1L]]$runtime_restore_status
  expect_false(any(c("resume", "reload") %in% status$actions))
  expect_match(status$problem, "changed outside the Builder", fixed = TRUE)
  expect_identical(status$message, "Source cannot be used")
  expect_true(
    "resume" %in% opened$manifest$datasets[[2L]]$runtime_restore_status$actions
  )

  skip_on_os("windows")
  skip_if(identical(unname(Sys.info()[["effective_user"]]), "root"))
  Sys.chmod(blob_of("ds2"), mode = "0000")
  withr::defer(Sys.chmod(blob_of("ds2"), mode = "0600"))
  opened <- runtime$builder_project_open_snapshot(saved$path)
  status <- opened$manifest$datasets[[2L]]$runtime_restore_status
  expect_false(any(c("resume", "reload") %in% status$actions))
  expect_match(status$problem, "cannot be read", fixed = TRUE)
})

test_that("a damaged or unsupported project file says what is wrong", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  path <- runtime$builder_project_manifest_path(root)
  writeLines("{damaged", path, useBytes = TRUE)
  expect_error(
    runtime$builder_project_read(path),
    "The project file is damaged and cannot be read",
    fixed = TRUE
  )
  write_version <- function(version) {
    jsonlite::write_json(
      list(
        schema_version = version,
        project = list(id = "project-safe", revision = 1L),
        datasets = list()
      ),
      path,
      auto_unbox = TRUE
    )
  }
  write_version(9L)
  expect_error(
    runtime$builder_project_read(path),
    "uses format version 9, and this Builder reads version 3. Update",
    fixed = TRUE
  )
  write_version(2L)
  expect_error(
    runtime$builder_project_read(path),
    "uses format version 2, and this Builder reads version 3. Open it with",
    fixed = TRUE
  )
  jsonlite::write_json(list(project = list()), path, auto_unbox = TRUE)
  expect_error(
    runtime$builder_project_read(path),
    "it has no project format version",
    fixed = TRUE
  )
})

test_that("attachments count as a managed Project folder", {
  runtime <- builder_project_integrity_runtime()
  root <- withr::local_tempdir()
  dir.create(file.path(root, "attachments"))
  expect_identical(
    runtime$builder_project_folder_state(root)$managed_conflicts,
    "attachments"
  )
})
