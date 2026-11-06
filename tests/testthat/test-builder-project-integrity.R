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
    "lexical error|parse error"
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
