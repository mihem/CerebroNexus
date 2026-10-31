builder_project_test_runtime <- function() {
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
    path <- builder_test_inst_path("builder", file)
    runtime$builder_source_utf8(path, envir = runtime)
  }
  runtime
}

## Direct test_file() runs on Windows can start in the C locale. Keep static
## source assertions on the same UTF-8 contract as the Builder runtime.
readLines <- function(
  con,
  n = -1L,
  ok = TRUE,
  warn = TRUE,
  encoding = "UTF-8",
  skipNul = FALSE
) {
  base::readLines(
    con,
    n = n,
    ok = ok,
    warn = warn,
    encoding = encoding,
    skipNul = skipNul
  )
}

builder_project_test_manifest <- function(ids = character()) {
  list(
    schema_version = 3L,
    project = list(id = "project-safe", revision = 0L),
    datasets = lapply(ids, function(id) list(id = id))
  )
}

test_that("project dataset ids are allocated without reusing restored ids", {
  runtime <- builder_project_test_runtime()

  allocated <- runtime$builder_project_allocate_dataset_id(
    sequence = 0L,
    existing_ids = c("ds1", "ds2")
  )
  expect_identical(allocated, list(id = "ds3", sequence = 3L))

  restored <- runtime$builder_project_allocate_dataset_id(
    sequence = 1L,
    existing_ids = "ds1",
    restored_id = "ds7"
  )
  expect_identical(restored, list(id = "ds7", sequence = 7L))
  expect_error(
    runtime$builder_project_allocate_dataset_id(1L, "ds1", "ds1"),
    "already in use"
  )

  replacement_ids <- runtime$builder_project_allocation_ids(
    entries = list(list(id = "ds1"), list(id = "ds2")),
    import_entries = list(list(id = "ds3")),
    replacing_id = "ds1",
    reserved_ids = c("ds1", "ds7")
  )
  expect_identical(replacement_ids, c("ds2", "ds3", "ds7"))

  reservations <- runtime$builder_project_dataset_reservations(
    list(
      datasets = list(
        list(id = "ds1", name = "Loaded"),
        list(id = "ds7", name = "Skipped")
      )
    ),
    excluding_id = "ds1"
  )
  expect_identical(reservations, list(ids = "ds7"))
})

test_that("uploaded sources use bounded internal names", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- file.path(root, "0.qs2")
  second <- file.path(root, "1.qs2")
  writeBin(charToRaw("first"), first)
  writeBin(charToRaw("second"), second)

  retained_first <- runtime$builder_project_retain_session_source(
    first,
    "sample-alpha.qs2",
    root,
    "ds1"
  )
  retained_second <- runtime$builder_project_retain_session_source(
    second,
    "sample-beta.qs2",
    root,
    "ds2"
  )

  expect_identical(basename(retained_first), "source.qs2")
  expect_identical(basename(retained_second), "source.qs2")
  expect_false(identical(dirname(retained_first), dirname(retained_second)))
  expect_true(file.exists(retained_first))
  expect_true(file.exists(retained_second))
})

test_that("long upload display names never enter managed paths", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  filename <- paste0(strrep("large-seurat-object-", 12L), ".qs2")
  upload <- file.path(root, "upload.qs2")
  writeBin(charToRaw("source"), upload)
  retained <- runtime$builder_project_session_source_path(
    upload,
    filename,
    root,
    "ds1"
  )

  expect_identical(basename(retained), "source.qs2")
  expect_false(grepl(filename, retained, fixed = TRUE))

  project <- file.path(root, "project")
  dir.create(project)
  job <- runtime$builder_project_source_job(
    list(id = "ds1", path = upload, filename = filename),
    project
  )
  expect_identical(job$filename, filename)
  expect_identical(job$storage_name, "source.qs2")
  expect_false(grepl(filename, job$target, fixed = TRUE))
})

test_that("retained upload bytes do not alias the transient source", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "0.qs2")
  writeBin(charToRaw("first"), source)

  retained <- runtime$builder_project_retain_session_source(
    source,
    "sample.qs2",
    root,
    "ds1"
  )
  writeBin(charToRaw("second"), source)

  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("first")
  )
})

test_that("browser uploads are retained by the background loader", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "browser-upload.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("uploaded-bytes"), source)

  loaded <- runtime$builder_project_load_retained_source(
    "ds1",
    list(source = source, retained_path = retained),
    progress = NULL,
    .adapter = function(path) list(path = path),
    .register = function(adapter, id, progress) {
      list(adapter = adapter, id = id)
    }
  )

  expect_identical(
    loaded$retained_path,
    normalizePath(retained, winslash = "/")
  )
  expect_identical(loaded$adapter$path, normalizePath(retained, winslash = "/"))
  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("uploaded-bytes")
  )

  unlink(source)
  retried <- runtime$builder_project_load_retained_source(
    "ds1",
    list(source = source, retained_path = retained),
    progress = NULL,
    .adapter = function(path) list(path = path),
    .register = function(adapter, id, progress) list(adapter = adapter, id = id)
  )
  expect_identical(
    retried$retained_path,
    normalizePath(retained, winslash = "/")
  )
})

test_that("a Builder-owned upload is moved into session sources", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "browser-upload.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("uploaded-bytes"), source)

  loaded <- runtime$builder_project_load_retained_source(
    "ds1",
    list(source = source, retained_path = retained, transport_owned = TRUE),
    progress = NULL,
    .adapter = function(path) list(path = path),
    .register = function(adapter, id, progress) list(adapter = adapter),
    .copy = function(...) stop("an owned upload must not be copied")
  )

  expect_false(file.exists(source))
  expect_identical(
    loaded$retained_path,
    normalizePath(retained, winslash = "/")
  )
  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("uploaded-bytes")
  )
  expect_length(
    list.files(dirname(retained), pattern = "[.]part$", all.files = TRUE),
    0L
  )
})

test_that("a source that is not Builder-owned is copied, never moved", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "user-file.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("user-bytes"), source)
  copies <- 0L

  for (owned in list(NULL, FALSE, NA)) {
    unlink(retained)
    runtime$builder_project_load_retained_source(
      "ds1",
      list(source = source, retained_path = retained, transport_owned = owned),
      progress = NULL,
      .adapter = function(path) list(path = path),
      .register = function(adapter, id, progress) list(adapter = adapter),
      .copy = function(source, target) {
        copies <<- copies + 1L
        file.copy(source, target)
      }
    )
    expect_true(file.exists(source))
    expect_identical(
      readBin(retained, "raw", n = 100L),
      charToRaw("user-bytes")
    )
  }
  expect_identical(copies, 3L)
})

test_that("failed background retention removes its partial file", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "browser-upload.qs2")
  retained <- file.path(root, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("uploaded-bytes"), source)

  expect_error(
    runtime$builder_project_load_retained_source(
      "ds1",
      list(source = source, retained_path = retained),
      progress = NULL,
      .adapter = identity,
      .register = function(...) stop("register should not run"),
      .copy = function(source, target, ...) {
        writeBin(charToRaw("partial"), target)
        FALSE
      }
    ),
    "could not be retained"
  )

  expect_false(file.exists(retained))
  expect_length(
    list.files(dirname(retained), pattern = "[.]part$", all.files = TRUE),
    0L
  )
})

test_that("example sources use the same retained file contract as uploads", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  bundled <- file.path(root, "complete_viewer_data.rds")
  writeBin(charToRaw("example-bytes"), bundled)
  catalog <- list(
    complete_viewer_data = list(
      id = "complete_viewer_data",
      serialized_path = bundled
    )
  )

  source <- runtime$builder_project_example_source(
    "complete_viewer_data",
    catalog
  )
  retained <- runtime$builder_project_retain_session_source(
    source$path,
    source$filename,
    root,
    "ds1"
  )

  expect_identical(source$origin, "example")
  expect_identical(source$filename, "complete_viewer_data.rds")
  expect_identical(
    readBin(retained, "raw", n = 100L),
    charToRaw("example-bytes")
  )
})

test_that("background source jobs commit exact bytes through a part file", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-sources", "ds1", "sample.qs2")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("immutable-source"), source)
  project <- file.path(root, "project")
  dir.create(project)

  job <- runtime$builder_project_source_job(
    list(
      id = "ds1",
      path = source,
      filename = "sample.qs2",
      source_origin = "upload"
    ),
    project
  )
  result <- runtime$builder_project_copy_source_job(job)

  expect_identical(result$status, "ready")
  expect_identical(
    readBin(result$path, "raw", n = 100L),
    charToRaw("immutable-source")
  )
  expect_false(file.exists(job$part))
  expect_identical(result$fingerprint$md5, unname(tools::md5sum(result$path)))
  expect_match(
    runtime$builder_project_relative_path(result$path, project),
    "^sources/ds1/blobs/[0-9a-f]+/source\\.qs2$"
  )
  expect_identical(job$filename, "sample.qs2")
  expect_identical(job$storage_name, "source.qs2")
})

test_that("source jobs reuse the MD5 frozen with a dataset snapshot", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  entry <- list(
    id = "ds1",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "sample.rds",
        "12",
        "2026-08-22T00:00:00.000000+0000",
        "ABCDEF0123456789ABCDEF0123456789",
        sep = ":"
      )
    )
  )

  expect_identical(
    runtime$builder_project_snapshot_source_md5(entry),
    "abcdef0123456789abcdef0123456789"
  )
  job <- runtime$builder_project_source_job(entry, root)
  expect_identical(
    job$relative,
    "sources/ds1/blobs/abcdef0123456789abcdef0123456789/source.rds"
  )
})

test_that("source save retries reuse an existing verified blob", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-source.rds")
  writeBin(charToRaw("already copied"), source)
  fingerprint <- runtime$builder_project_file_fingerprint(
    source,
    content = TRUE
  )
  entry <- list(
    id = "ds1",
    path = source,
    filename = "source.rds",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "source.rds",
        fingerprint$bytes,
        fingerprint$modified_at,
        fingerprint$md5,
        sep = ":"
      )
    )
  )
  job <- runtime$builder_project_source_job(entry, root)
  dir.create(dirname(job$target), recursive = TRUE)
  expect_true(file.copy(source, job$target))

  prepared <- runtime$builder_project_prepare_source(entry, root)

  expect_identical(prepared$source$status, "ready")
  expect_identical(prepared$source$path, job$relative)
  expect_identical(prepared$source$fingerprint$md5, fingerprint$md5)
  expect_null(prepared$job)
})

test_that("source save retries reject a corrupted content-addressed blob", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-source.rds")
  writeBin(charToRaw("original source"), source)
  fingerprint <- runtime$builder_project_file_fingerprint(
    source,
    content = TRUE
  )
  entry <- list(
    id = "ds1",
    path = source,
    filename = "source.rds",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "source.rds",
        fingerprint$bytes,
        fingerprint$modified_at,
        fingerprint$md5,
        sep = ":"
      )
    )
  )
  job <- runtime$builder_project_source_job(entry, root)
  dir.create(dirname(job$target), recursive = TRUE)
  writeBin(charToRaw("corrupted source"), job$target)

  expect_error(
    runtime$builder_project_prepare_source(entry, root),
    "failed its integrity check",
    fixed = TRUE
  )
})

test_that("legacy retained snapshots save and restore through verified hashing", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "session-sources", "ds1", "sample.rds")
  project <- file.path(root, "project")
  dir.create(dirname(source), recursive = TRUE)
  dir.create(project)
  writeBin(charToRaw("legacy-retained-source"), source)
  entry <- list(
    id = "ds1",
    path = source,
    filename = "sample.rds",
    source_origin = "upload",
    settings = list(name = "Dataset"),
    snapshot = list(
      source_fingerprint = paste(
        "builder-retained-v1",
        source,
        file.info(source)$size,
        "2026-08-22T00:00:00.000000+0000",
        sep = ":"
      )
    )
  )

  job <- runtime$builder_project_source_job(entry, project)
  expect_null(job$source_md5)
  result <- runtime$builder_project_copy_source_job(job)
  expect_identical(result$status, "ready")
  source_record <- list(
    kind = "managed",
    origin = "upload",
    filename = "sample.rds",
    path = runtime$builder_project_relative_path(result$path, project),
    status = "ready",
    fingerprint = result$fingerprint
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source_record,
    root = project
  )
  status <- runtime$builder_project_dataset_status(record, project)
  restored <- runtime$builder_project_restore_entry(
    record,
    project,
    status$configuration_entry
  )

  expect_false(grepl(
    "builder-retained-v1",
    paste(capture.output(dput(record)), collapse = "")
  ))
  expect_true("resume" %in% status$actions)
  expect_true(is.list(status$configuration_entry))
  expect_identical(
    restored$path,
    normalizePath(result$path, winslash = "/", mustWork = TRUE)
  )
})

test_that("only content-addressed managed sources reuse metadata on restore", {
  runtime <- builder_project_test_runtime()

  expect_true(runtime$builder_project_content_addressed_source(
    "sources/ds1/blobs/abcdef0123456789abcdef0123456789/sample.rds",
    "ds1"
  ))
  expect_false(runtime$builder_project_content_addressed_source(
    "sources/ds1/sample.rds",
    "ds1"
  ))
})

test_that("a changed managed blob is saved under its new content identity", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  old_md5 <- strrep("a", 32L)
  relative <- paste(
    "sources",
    "ds1",
    "blobs",
    old_md5,
    "source.rds",
    sep = "/"
  )
  source <- do.call(
    file.path,
    c(list(root), as.list(strsplit(relative, "/", fixed = TRUE)[[1L]]))
  )
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("changed source"), source)
  actual <- unname(tools::md5sum(source))
  current <- runtime$builder_project_file_fingerprint(source, content = FALSE)
  prior_fingerprint <- current
  prior_fingerprint$md5 <- old_md5
  prior <- list(
    source = list(
      kind = "managed",
      status = "ready",
      path = relative,
      filename = "source.rds",
      fingerprint = prior_fingerprint
    )
  )
  verified_fingerprint <- current
  verified_fingerprint$md5 <- actual
  entry <- list(
    id = "ds1",
    path = source,
    filename = "source.rds",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "source.rds",
        current$bytes,
        current$modified_at,
        actual,
        sep = ":"
      )
    ),
    project_source_fingerprint = verified_fingerprint
  )

  prepared <- runtime$builder_project_prepare_source(entry, root, prior)

  expect_identical(prepared$source$status, "pending")
  expect_true(is.list(prepared$job))
  result <- runtime$builder_project_copy_source_job(prepared$job)
  expect_identical(result$status, "ready")
  expect_match(
    gsub("\\\\", "/", result$path),
    paste0("/blobs/", actual, "/"),
    fixed = TRUE
  )
})

test_that("restored examples reuse the matching managed source", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  staged <- file.path(root, "complete_viewer_data.rds")
  writeBin(charToRaw("managed-example"), staged)
  source_md5 <- unname(tools::md5sum(staged))
  relative <- paste(
    "sources",
    "ds1",
    "blobs",
    source_md5,
    "complete_viewer_data.rds",
    sep = "/"
  )
  managed <- file.path(root, relative)
  dir.create(dirname(managed), recursive = TRUE)
  expect_true(file.rename(staged, managed))
  fingerprint <- runtime$builder_project_file_fingerprint(
    managed,
    content = TRUE
  )
  prior <- list(
    source = list(
      kind = "managed",
      origin = "example",
      example = "complete_viewer_data",
      filename = "complete_viewer_data.rds",
      path = relative,
      status = "ready",
      fingerprint = fingerprint
    )
  )
  entry <- list(
    id = "ds1",
    path = NULL,
    filename = "complete_viewer_data.rds",
    example = "complete_viewer_data",
    source_origin = "example",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        "complete_viewer_data.rds",
        "15",
        "2026-08-22T00:00:00.000000+0000",
        source_md5,
        sep = ":"
      )
    )
  )

  prepared <- runtime$builder_project_prepare_source(entry, root, prior)

  expect_identical(prepared$source$status, "ready")
  expect_identical(prepared$source$path, relative)
  expect_null(prepared$job)
})

test_that("background source generations never overwrite prior content", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  source <- file.path(root, "session-sources", "ds1", "sample.qs2")
  dir.create(dirname(source), recursive = TRUE)
  dir.create(project)
  writeBin(charToRaw("first-source"), source)
  job <- runtime$builder_project_source_job(
    list(id = "ds1", path = source, filename = "sample.qs2"),
    project
  )
  first <- runtime$builder_project_copy_source_job(job)
  first_bytes <- readBin(first$path, "raw", n = 100L)

  writeBin(charToRaw("second-source"), source)
  second <- runtime$builder_project_copy_source_job(job)

  expect_false(identical(first$path, second$path))
  expect_identical(readBin(first$path, "raw", n = 100L), first_bytes)
  expect_identical(
    readBin(second$path, "raw", n = 100L),
    charToRaw("second-source")
  )
})

test_that("background source jobs report a missing source without a partial target", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  job <- runtime$builder_project_source_job(
    list(
      id = "ds1",
      path = file.path(root, "missing.qs2"),
      filename = "missing.qs2",
      source_origin = "upload"
    ),
    project
  )

  result <- runtime$builder_project_copy_source_job(job)

  expect_identical(result$status, "failed")
  expect_match(result$error, "no longer available", fixed = TRUE)
  expect_false(file.exists(job$part))
  expect_false(file.exists(job$target))
})

test_that("project source preparation records pending work without copying", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(
    root,
    "session-sources",
    "ds1",
    "complete_viewer_data.rds"
  )
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("example"), source)
  project <- file.path(root, "project")
  dir.create(project)
  entry <- list(
    id = "ds1",
    path = source,
    filename = "complete_viewer_data.rds",
    example = "complete_viewer_data",
    source_origin = "example"
  )

  prepared <- runtime$builder_project_prepare_source(entry, project)

  expect_identical(prepared$source$status, "pending")
  expect_identical(prepared$source$origin, "example")
  expect_identical(
    prepared$source$path,
    "sources/ds1/source.rds"
  )
  expect_false(file.exists(prepared$job$target))
})

test_that("completed background sources are merged into the latest manifest", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  target <- file.path(project, "sources", "ds1", "sample.qs2")
  dir.create(dirname(target), recursive = TRUE)
  writeBin(charToRaw("ready"), target)
  manifest <- builder_project_test_manifest("ds1")
  manifest$datasets[[1L]]$source <- list(
    kind = "managed",
    origin = "upload",
    filename = "sample.qs2",
    path = "sources/ds1/sample.qs2",
    status = "pending"
  )
  result <- list(
    id = "ds1",
    status = "ready",
    path = target,
    fingerprint = runtime$builder_project_file_fingerprint(
      target,
      content = TRUE
    )
  )

  updated <- runtime$builder_project_apply_source_results(
    manifest,
    list(result),
    project
  )

  expect_identical(updated$datasets[[1L]]$source$status, "ready")
  expect_identical(
    updated$datasets[[1L]]$source$fingerprint$md5,
    result$fingerprint$md5
  )
})

test_that("completed source sync repoints live entries and releases owned session copies", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  session_root <- file.path(root, "session")
  retained <- file.path(session_root, "session-sources", "ds1", "sample.qs2")
  target <- file.path(
    project,
    "sources",
    "ds1",
    "blobs",
    "content",
    "sample.qs2"
  )
  dir.create(dirname(retained), recursive = TRUE)
  dir.create(dirname(target), recursive = TRUE)
  dir.create(project, showWarnings = FALSE)
  writeBin(charToRaw("ready"), retained)
  writeBin(charToRaw("ready"), target)
  manifest <- builder_project_test_manifest("ds1")
  manifest$datasets[[1L]]$source <- list(
    kind = "managed",
    origin = "upload",
    filename = "sample.qs2",
    path = runtime$builder_project_relative_path(target, project),
    status = "ready",
    fingerprint = runtime$builder_project_file_fingerprint(
      target,
      content = TRUE
    )
  )
  entries <- list(list(
    id = "ds1",
    path = retained,
    source_origin = "upload",
    snapshot = list(path = file.path(session_root, "snapshot-ds1"))
  ))

  committed <- runtime$builder_project_commit_source_entries(
    entries,
    manifest,
    results = list(list(id = "ds1", status = "ready", path = target)),
    root = project,
    session_root = session_root
  )

  expect_identical(
    committed$entries[[1L]]$path,
    normalizePath(target, winslash = "/")
  )
  expect_identical(committed$entries[[1L]]$source$kind, "managed")
  expect_false(file.exists(retained))
  expect_identical(committed$released, "ds1")
})

test_that("session source cleanup refuses paths outside the owned session root", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  owned <- file.path(root, "owned")
  outside <- file.path(root, "outside.qs2")
  dir.create(file.path(owned, "session-sources", "ds1"), recursive = TRUE)
  retained <- file.path(owned, "session-sources", "ds1", "sample.qs2")
  writeBin(charToRaw("owned"), retained)
  writeBin(charToRaw("outside"), outside)

  expect_true(runtime$builder_project_release_session_source(retained, owned))
  expect_false(file.exists(retained))
  expect_false(runtime$builder_project_release_session_source(outside, owned))
  expect_true(file.exists(outside))
})

test_that("session source release rejects lexical symlinks without deleting targets", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  owned <- file.path(root, "owned")
  outside <- file.path(root, "outside.qs2")
  link <- file.path(owned, "session-sources", "ds1", "linked.qs2")
  dir.create(dirname(link), recursive = TRUE)
  writeBin(charToRaw("outside"), outside)
  skip_if_not(file.symlink(outside, link), "symbolic links are unavailable")

  expect_false(runtime$builder_project_release_session_source(link, owned))
  expect_true(nzchar(Sys.readlink(link)))
  expect_true(file.exists(outside))
})

test_that("managed path checks allow only an explicitly missing create leaf", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  checkpoint_parent <- file.path(root, "checkpoints")

  expect_true(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root
  ))
  expect_false(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root,
    allow_missing_leaf = TRUE
  ))
  expect_true(runtime$.builder_project_path_has_link_within(
    file.path(root, "missing-parent", "checkpoints"),
    root,
    allow_missing_leaf = TRUE
  ))

  expect_true(dir.create(checkpoint_parent))
  expect_false(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root,
    allow_missing_leaf = TRUE
  ))

  unlink(checkpoint_parent, recursive = TRUE)
  skip_if_not(
    file.symlink(file.path(root, "missing-target"), checkpoint_parent),
    "symbolic links are unavailable"
  )
  expect_true(runtime$.builder_project_path_has_link_within(
    checkpoint_parent,
    root,
    allow_missing_leaf = TRUE
  ))
})

test_that("checkpoint cleanup accepts the lexical macOS alias of its project root", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  lexical_root <- withr::local_tempdir()
  normalized_root <- normalizePath(
    lexical_root,
    winslash = "/",
    mustWork = TRUE
  )
  skip_if(
    identical(lexical_root, normalized_root),
    "the temporary root has no lexical filesystem alias"
  )
  checkpoint <- file.path(
    lexical_root,
    "checkpoints",
    "20260820T000000"
  )
  dir.create(checkpoint, recursive = TRUE)
  writeLines("checkpoint", file.path(checkpoint, "dataset.crb"))

  expect_true(runtime$builder_project_cleanup_checkpoint(
    checkpoint,
    normalized_root
  ))
  expect_false(dir.exists(checkpoint))
})

test_that("source progress records stay compact while final results remain complete", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  sources <- file.path(root, paste0("source-", 1:3, ".rds"))
  lapply(seq_along(sources), function(index) {
    writeBin(charToRaw(paste0("source-", index)), sources[[index]])
  })
  jobs <- lapply(seq_along(sources), function(index) {
    runtime$builder_project_source_job(
      list(id = paste0("ds", index), path = sources[[index]]),
      project
    )
  })
  progress_path <- file.path(root, "progress.rds")

  results <- runtime$builder_project_copy_source_jobs(jobs, progress_path)
  progress <- readRDS(progress_path)

  expect_length(results, 3L)
  expect_identical(
    names(progress),
    c(
      "completed",
      "total",
      "failed",
      "bytes_done",
      "bytes_total",
      "current",
      "last"
    )
  )
  expect_false("results" %in% names(progress))
  expect_identical(progress$completed, 3L)
})

test_that("background save runtimes include verified copy support", {
  skip_if_not_installed("callr")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  source <- file.path(root, "source.rds")
  table <- file.path(root, "table.csv")
  writeBin(charToRaw("dataset-source"), source)
  writeBin(charToRaw("column\nvalue\n"), table)
  source_job <- runtime$builder_project_source_job(
    list(id = "ds1", path = source, filename = "source.rds"),
    project
  )
  table_jobs <- runtime$builder_project_table_asset_jobs(
    list(list(
      id = "ds1",
      settings = list(
        tables = list(
          table = list(
            file_name = "table.csv",
            source_path = table
          )
        )
      )
    )),
    project
  )
  runtime_files <- normalizePath(
    builder_test_inst_path(
      "builder",
      c("io.R", "project.R")
    ),
    winslash = "/",
    mustWork = TRUE
  )

  result <- callr::r(
    function(source_job, table_jobs, project, runtime_files) {
      runtime <- new.env(parent = globalenv())
      runtime$`%||%` <- function(left, right) {
        if (is.null(left)) right else left
      }
      for (runtime_file in runtime_files) {
        sys.source(runtime_file, envir = runtime)
      }
      list(
        source = runtime$builder_project_copy_source_job(source_job),
        tables = runtime$builder_project_stage_table_asset_jobs(
          table_jobs,
          project
        )
      )
    },
    args = list(
      source_job = source_job,
      table_jobs = table_jobs,
      project = project,
      runtime_files = runtime_files
    )
  )

  expect_identical(result$source$status, "ready")
  expect_true(file.exists(result$source$path))
  expect_length(result$tables, 1L)
  expect_true(is.list(result$tables[[1L]]$asset$fingerprint))
})

test_that("managed example sources retain their example identity when restored", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "complete_viewer_data.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("example"), source)
  entry <- list(id = "ds1", settings = list(name = "Complete Viewer data"))
  configuration <- runtime$builder_project_write_dataset_config(entry, root)
  record <- list(
    id = "ds1",
    source = list(
      kind = "managed",
      origin = "example",
      example = "complete_viewer_data",
      path = "sources/ds1/complete_viewer_data.rds"
    ),
    configuration = configuration
  )

  status <- runtime$builder_project_dataset_status(record, root)
  restored <- runtime$builder_project_restore_entry(
    record,
    root,
    status$configuration_entry
  )

  expect_identical(restored$source_origin, "example")
  expect_identical(restored$example, "complete_viewer_data")
  expect_identical(restored$path, normalizePath(source, winslash = "/"))
})

test_that("project restore disables Sources whose reader package is missing", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "source.qs")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("qs-placeholder"), source)
  entry <- list(id = "ds1", settings = list(name = "Legacy QS"))
  configuration <- runtime$builder_project_write_dataset_config(entry, root)
  record <- list(
    id = "ds1",
    name = "Legacy QS",
    source = list(
      kind = "managed",
      path = "sources/ds1/source.qs",
      filename = "source.qs"
    ),
    configuration = configuration
  )
  capabilities <- runtime$builder_format_capabilities(
    .require_namespace = function(package) !identical(package, "qs")
  )

  status <- runtime$builder_project_dataset_status(
    record,
    root,
    format_capabilities = capabilities
  )

  expect_identical(status$actions, "skip")
  expect_identical(status$default_action, "skip")
  expect_identical(status$message, "Source reader unavailable")
  expect_match(status$problem, 'QS unavailable', fixed = TRUE)
})

test_that("project payload preserves saved Spatial FOV controls", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  entry <- list(
    id = "ds1",
    settings = list(
      spatial_coordinate_transforms = list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      ),
      spatial_point_appearance = list(
        section_a_1_fov_1 = list(point_opacity = 0.7, point_size = 6)
      )
    )
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    root = root
  )

  status <- runtime$builder_project_dataset_status(record, root)
  restored <- runtime$builder_project_restore_entry(
    record,
    root,
    status$configuration_entry
  )

  expect_identical(
    restored$settings$spatial_coordinate_transforms[[
      "section_a_1_fov_1"
    ]]$rotation_degrees,
    66.9
  )
  expect_identical(
    restored$settings$spatial_point_appearance[["section_a_1_fov_1"]],
    list(point_opacity = 0.7, point_size = 6)
  )
})

test_that("project spatial assets are externalized per dataset and FOV", {
  skip_if_not_installed("png")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source_path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), source_path)
  inspected <- runtime$builder_read_image(source_path)
  image_record <- function(section) {
    list(
      source = list(
        name = "tissue.png",
        type = "image/png",
        size = inspected$bytes
      ),
      source_path = inspected$source_path,
      source_content_md5 = inspected$source_content_md5,
      base_bounds = list(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      section_id = section
    )
  }
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        `section/a` = list(`H&E` = image_record("section/a")),
        `section-a` = list(`H&E` = image_record("section-a"))
      )
    )
  )

  payload_entry <- runtime$builder_project_stage_spatial_assets(entry, root)
  adopted <- runtime$builder_project_adopt_spatial_assets(
    entry,
    payload_entry,
    root
  )
  expect_true(file.exists(
    adopted$settings$images[["section/a"]][["H&E"]]$source_path
  ))
  expect_identical(
    adopted$settings$images[["section/a"]][["H&E"]]$project_asset,
    payload_entry$settings$images[["section/a"]][["H&E"]]$project_asset
  )
  original_fingerprint <- runtime$builder_project_file_fingerprint
  fingerprint_content <- logical()
  runtime$builder_project_file_fingerprint <- function(..., content = FALSE) {
    fingerprint_content <<- c(fingerprint_content, content)
    original_fingerprint(..., content = content)
  }
  restaged <- runtime$builder_project_stage_spatial_assets(adopted, root)
  expect_null(restaged$settings$images[["section/a"]][["H&E"]]$source_uri)
  expect_false(any(fingerprint_content))
  expect_false(grepl(
    "serialize(",
    paste(
      deparse(body(runtime$builder_project_stage_spatial_assets)),
      collapse = "\n"
    ),
    fixed = TRUE
  ))
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    payload_entry = restaged,
    root = root
  )
  runtime$builder_project_file_fingerprint <- original_fingerprint
  payload <- runtime$builder_project_read_dataset_config(record, root)
  first <- payload$settings$images[["section/a"]][["H&E"]]
  second <- payload$settings$images[["section-a"]][["H&E"]]

  expect_null(first$source_uri)
  expect_null(first$uri)
  expect_match(first$project_asset$path, "^spatial-assets/ds1/")
  expect_false(identical(first$project_asset$path, second$project_asset$path))
  expect_true(file.exists(file.path(root, first$project_asset$path)))
  expect_null(record$configuration$payload)

  status <- runtime$builder_project_dataset_status(record, root)
  restored <- runtime$builder_project_restore_entry(
    record,
    root,
    status$configuration_entry
  )
  restored_source <- restored$settings$images[["section/a"]][[
    "H&E"
  ]]$source_path
  expect_true(file.exists(restored_source))
  expect_null(restored$settings$images[["section/a"]][["H&E"]]$source_uri)
  expect_null(restored$settings$images[["section-a"]][["H&E"]]$uri)
  expect_identical(
    unname(as.character(tools::md5sum(restored_source))),
    inspected$source_content_md5
  )
  expect_identical(
    runtime$builder_project_configuration_digest(restored),
    record$configuration$digest
  )
})

test_that("project restores multi-sheet attachments with one file fingerprint", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "supplement.xlsx")
  writeBin(charToRaw("workbook-bytes"), source)
  sheet <- function(name) {
    list(
      file_name = "supplement.xlsx",
      sheet_name = name,
      display_name = name,
      source_path = source
    )
  }
  entry <- list(
    id = "ds1",
    settings = list(
      tables = list(Clinical = sheet("Clinical"), QC = sheet("QC"))
    )
  )

  staged <- runtime$builder_project_stage_table_assets(entry, root)
  expect_null(staged$settings$tables$Clinical$source_path)
  expect_identical(
    staged$settings$tables$Clinical$project_asset,
    staged$settings$tables$QC$project_asset
  )

  original_fingerprint <- runtime$builder_project_file_fingerprint
  calls <- 0L
  runtime$builder_project_file_fingerprint <- function(...) {
    calls <<- calls + 1L
    original_fingerprint(...)
  }
  restored <- runtime$builder_project_restore_table_assets(staged, root)

  expect_identical(calls, 1L)
  expect_identical(
    restored$settings$tables$Clinical$source_path,
    restored$settings$tables$QC$source_path
  )

  adopted <- runtime$builder_project_adopt_table_assets(entry, staged, root)
  expect_identical(
    runtime$builder_project_configuration_digest(entry),
    runtime$builder_project_configuration_digest(staged)
  )
  expect_identical(
    runtime$builder_project_configuration_digest(entry),
    runtime$builder_project_configuration_digest(adopted)
  )
  renamed <- adopted
  renamed$settings$tables$Clinical$display_name <- "Patients"
  expect_false(identical(
    runtime$builder_project_configuration_digest(entry),
    runtime$builder_project_configuration_digest(renamed)
  ))
  fingerprint_content <- logical()
  runtime$builder_project_file_fingerprint <- function(..., content = FALSE) {
    fingerprint_content <<- c(fingerprint_content, content)
    original_fingerprint(..., content = content)
  }
  runtime$builder_project_stage_table_assets(adopted, root)
  expect_false(any(fingerprint_content))
})

test_that("project table asset jobs deduplicate sheets from one workbook", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "supplement.xlsx")
  writeBin(charToRaw("workbook-bytes"), source)
  table <- function(sheet) {
    list(
      file_name = "supplement.xlsx",
      sheet_name = sheet,
      source_path = source
    )
  }
  entries <- list(list(
    id = "ds1",
    revision = 4L,
    settings = list(
      tables = list(
        Clinical = table("Clinical"),
        QC = table("QC")
      )
    )
  ))

  jobs <- runtime$builder_project_table_asset_jobs(entries, root)

  expect_length(jobs, 1L)
  expect_identical(jobs[[1L]]$entry_id, "ds1")
  expect_identical(jobs[[1L]]$source, normalizePath(source, winslash = "/"))
  expect_identical(jobs[[1L]]$filename, "supplement.xlsx")

  results <- runtime$builder_project_stage_table_asset_jobs(jobs, root)
  expect_true(runtime$builder_project_table_asset_results_match(jobs, results))
  applied <- runtime$builder_project_apply_table_asset_results(
    entries,
    results,
    root
  )
  expect_identical(
    applied[[1L]]$settings$tables$Clinical$source_path,
    applied[[1L]]$settings$tables$QC$source_path
  )
  expect_true(file.exists(
    applied[[1L]]$settings$tables$Clinical$source_path
  ))
  original_fingerprint <- runtime$builder_project_file_fingerprint
  fingerprint_content <- logical()
  runtime$builder_project_file_fingerprint <- function(..., content = FALSE) {
    fingerprint_content <<- c(fingerprint_content, content)
    original_fingerprint(..., content = content)
  }
  runtime$builder_project_stage_table_assets(
    runtime$builder_project_configuration_entry(applied[[1L]]),
    root
  )
  expect_false(any(fingerprint_content))

  writeBin(charToRaw("changed-workbook-bytes"), source)
  expect_false(identical(
    jobs,
    runtime$builder_project_table_asset_jobs(entries, root)
  ))
})

test_that("project table save signature changes with manifest or configuration", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- list(
    root = root,
    manifest = list(project = list(id = "project-safe", revision = 2L))
  )
  entries <- list(list(
    id = "ds1",
    revision = 3L,
    settings = list(name = "Dataset A", tables = list())
  ))
  signature <- runtime$builder_project_table_save_signature(project, entries)

  changed_project <- project
  changed_project$manifest$project$revision <- 3L
  expect_false(identical(
    signature,
    runtime$builder_project_table_save_signature(changed_project, entries)
  ))
  changed_entries <- entries
  changed_entries[[1L]]$settings$name <- "Dataset B"
  expect_false(identical(
    signature,
    runtime$builder_project_table_save_signature(project, changed_entries)
  ))
})

test_that("missing project spatial assets identify the affected FOV and image", {
  skip_if_not_installed("png")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source_path <- withr::local_tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 16L), nrow = 4L), source_path)
  inspected <- runtime$builder_read_image(source_path)
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        fov_1 = list(
          DAPI = list(
            source = list(name = "DAPI.png", type = "image/png"),
            source_path = inspected$source_path,
            source_content_md5 = inspected$source_content_md5,
            base_bounds = list(xmin = 0, xmax = 1, ymin = 0, ymax = 1)
          )
        )
      )
    )
  )
  payload_entry <- runtime$builder_project_stage_spatial_assets(entry, root)
  dataset_source <- file.path(root, "source.rds")
  writeBin(charToRaw("source"), dataset_source)
  record <- runtime$builder_project_dataset_record(
    entry,
    list(
      kind = "managed",
      path = runtime$builder_project_relative_path(dataset_source, root)
    ),
    payload_entry = payload_entry,
    root = root
  )
  asset <- payload_entry$settings$images$fov_1$DAPI$project_asset$path
  unlink(file.path(root, asset))

  status <- runtime$builder_project_dataset_status(record, root)
  expect_null(status$configuration_entry)
  expect_identical(status$actions, c("reload", "skip"))
  expect_identical(status$default_action, "reload")
  expect_identical(status$message, "Settings unavailable")
  expect_match(status$problem, "ds1.*fov_1.*DAPI")
  expect_error(
    runtime$builder_project_restore_entry(
      record,
      root,
      status$configuration_entry
    ),
    "saved dataset configuration is unavailable"
  )
})

test_that("open validation checks spatial descriptors without reading image payloads", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  asset <- file.path(root, "spatial-assets", "ds1", "fov", "image.png")
  dir.create(dirname(asset), recursive = TRUE)
  writeBin(charToRaw("asset"), asset)
  entry <- list(
    id = "ds1",
    settings = list(
      images = list(
        fov = list(
          image = list(
            project_asset = list(
              path = runtime$builder_project_relative_path(asset, root),
              mime = "image/png",
              fingerprint = runtime$builder_project_file_fingerprint(
                asset,
                content = TRUE
              )
            )
          )
        )
      )
    )
  )
  record <- list(
    id = "ds1",
    configuration = runtime$builder_project_write_dataset_config(entry, root)
  )
  runtime$builder_read_image <- function(...) {
    stop("open validation must not decode image payloads")
  }

  status <- runtime$builder_project_dataset_status(record, root)

  expect_true(is.list(status$configuration_entry))
  expect_null(status$problem)
  expect_identical(
    status$configuration_entry$settings$images$fov$image$source_path,
    normalizePath(asset, winslash = "/")
  )
})

test_that("status signatures retain references and resolved paths", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  relative_paths <- c(
    "datasets/ds1/config.json",
    "sources/ds1/source.rds",
    "artifacts/ds1/ds1.crb",
    "artifacts/ds1/metadata.json"
  )
  for (relative_path in relative_paths) {
    path <- file.path(root, relative_path)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(relative_path), path)
  }
  record <- list(
    id = "ds1",
    configuration = list(path = relative_paths[[1L]]),
    source = list(kind = "managed", path = relative_paths[[2L]]),
    artifact = list(
      path = relative_paths[[3L]],
      members = list(
        list(path = relative_paths[[4L]]),
        list(path = relative_paths[[3L]])
      )
    )
  )

  signature <- runtime$builder_project_status_stat_signature(
    record,
    root
  )

  expect_identical(
    names(signature),
    relative_paths
  )
  expect_identical(
    unname(vapply(signature, `[[`, character(1), "path")),
    normalizePath(file.path(root, relative_paths), winslash = "/")
  )
  expect_true(all(vapply(signature, is.list, logical(1))))

  record$source <- list(kind = "external", path = record$artifact$path)
  expect_error(
    runtime$builder_project_status_stat_signature(
      record,
      root,
      needs = c("source", "artifact")
    ),
    "reference is ambiguous"
  )
  record$source$path <- record$configuration$path
  expect_error(
    runtime$builder_project_saved_configuration_status(record, root),
    "reference is ambiguous"
  )
})

test_that("status signatures batch unique file metadata and normalize root once", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  relative_paths <- c(
    "datasets/ds1/config.json",
    "sources/ds1/source.rds",
    "artifacts/ds1/ds1.crb"
  )
  for (relative_path in relative_paths) {
    path <- file.path(root, relative_path)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeBin(charToRaw(relative_path), path)
  }
  record <- list(
    configuration = list(path = relative_paths[[1L]]),
    source = list(path = relative_paths[[2L]]),
    artifact = list(
      path = relative_paths[[3L]],
      members = list(list(path = relative_paths[[3L]]))
    )
  )
  metadata <- runtime$.builder_project_files_metadata
  metadata_calls <- list()
  runtime$.builder_project_files_metadata <- function(paths) {
    metadata_calls[[length(metadata_calls) + 1L]] <<- paths
    metadata(paths)
  }
  normalize_root <- runtime$builder_project_normalize_root
  root_calls <- 0L
  runtime$builder_project_normalize_root <- function(...) {
    root_calls <<- root_calls + 1L
    normalize_root(...)
  }

  signature <- runtime$builder_project_status_stat_signature(record, root)

  expect_length(signature, 3L)
  expect_length(metadata_calls, 1L)
  expect_identical(names(signature), relative_paths)
  expect_setequal(
    metadata_calls[[1L]],
    normalizePath(file.path(root, relative_paths), winslash = "/")
  )
  expect_identical(root_calls, 1L)
})

test_that("file metadata batches files and safely follows symbolic-link targets", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- file.path(root, "first.bin")
  second <- file.path(root, "second.bin")
  directory <- file.path(root, "directory")
  missing <- file.path(root, "missing.bin")
  link <- file.path(root, "linked.bin")
  chained <- file.path(root, "chained.bin")
  broken <- file.path(root, "broken.bin")
  cycle_a <- file.path(root, "cycle-a.bin")
  cycle_b <- file.path(root, "cycle-b.bin")
  writeBin(charToRaw("first"), first)
  writeBin(charToRaw("second"), second)
  dir.create(directory)
  skip_if_not(file.symlink(first, link), "symbolic links are unavailable")
  skip_if_not(file.symlink(link, chained), "symbolic links are unavailable")
  skip_if_not(file.symlink(missing, broken), "symbolic links are unavailable")
  skip_if_not(file.symlink(cycle_b, cycle_a), "symbolic links are unavailable")
  skip_if_not(file.symlink(cycle_a, cycle_b), "symbolic links are unavailable")

  metadata <- runtime$.builder_project_files_metadata(c(
    first,
    second,
    directory,
    missing
  ))

  expect_length(metadata, 4L)
  expect_identical(metadata[[1L]], runtime$builder_project_file_metadata(first))
  expect_identical(
    metadata[[2L]],
    runtime$builder_project_file_metadata(second)
  )
  expect_null(metadata[[3L]])
  expect_null(metadata[[4L]])
  expect_identical(
    runtime$builder_project_file_metadata(link),
    metadata[[1L]]
  )
  expect_identical(
    runtime$builder_project_file_metadata(chained),
    metadata[[1L]]
  )
  expect_null(runtime$builder_project_file_metadata(broken))
  expect_null(runtime$builder_project_file_metadata(cycle_a))
})

test_that("restore status snapshots are reused without weakening default validation", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "source.rds")
  artifact_path <- file.path(root, "artifacts", "ds1", "bundle", "ds1.crb")
  dir.create(dirname(source), recursive = TRUE)
  dir.create(dirname(artifact_path), recursive = TRUE)
  writeBin(charToRaw("source"), source)
  writeBin(charToRaw("artifact"), artifact_path)
  entry <- list(id = "ds1", settings = list(name = "Dataset"))
  source_fingerprint <- runtime$builder_project_file_fingerprint(
    source,
    content = TRUE
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = runtime$builder_project_relative_path(source, root),
      fingerprint = source_fingerprint
    ),
    artifact = list(
      status = "ready",
      path = runtime$builder_project_relative_path(artifact_path, root),
      fingerprint = runtime$builder_project_file_fingerprint(
        artifact_path,
        content = TRUE
      ),
      members = list(),
      built_from_revision = 0L,
      built_from_source_fingerprint = paste(
        "builder-snapshot-v2",
        "source.rds",
        source_fingerprint$bytes,
        source_fingerprint$modified_at,
        source_fingerprint$md5,
        sep = ":"
      ),
      built_from_configuration = runtime$builder_project_configuration_digest(
        entry
      )
    ),
    checked = TRUE,
    root = root
  )
  calls <- 0L
  original <- runtime$builder_project_artifact_available
  runtime$builder_project_artifact_available <- function(...) {
    calls <<- calls + 1L
    original(...)
  }
  config_reads <- 0L
  original_read <- runtime$builder_project_read_dataset_config
  runtime$builder_project_read_dataset_config <- function(...) {
    config_reads <<- config_reads + 1L
    original_read(...)
  }

  status <- runtime$builder_project_dataset_status(record, root)
  expect_true(runtime$builder_project_status_snapshot_fresh(
    status,
    record,
    root
  ))
  resolve_path <- runtime$builder_project_resolve_path
  resolve_calls <- 0L
  runtime$builder_project_resolve_path <- function(...) {
    resolve_calls <<- resolve_calls + 1L
    resolve_path(...)
  }
  record$runtime_restore_status <- status
  prepared <- runtime$builder_project_prepare_open_selection(
    list(datasets = list(record)),
    root,
    list(ds1 = "reuse")
  )

  expect_identical(calls, 1L)
  expect_identical(config_reads, 1L)
  expect_identical(resolve_calls, 2L)
  expect_true("reuse" %in% status$actions)
  expect_length(prepared$reusable_entries, 1L)
  expect_match(prepared$marks[["ds1"]], "^artifact:")
  substituted_record <- record
  substituted_record$artifact$path <- "artifacts/ds1/bundle/substituted.crb"
  expect_error(
    runtime$builder_project_prepare_open_selection(
      list(datasets = list(substituted_record)),
      root,
      list(ds1 = "reuse")
    ),
    "inputs changed"
  )
  writeBin(charToRaw("source-changed"), source)
  expect_true(runtime$builder_project_status_snapshot_fresh(
    status,
    record,
    root,
    needs = c("configuration", "artifact", "spatial", "table")
  ))
  writeBin(charToRaw("artifact-changed"), artifact_path)
  expect_false(runtime$builder_project_status_snapshot_fresh(
    status,
    record,
    root,
    needs = c("configuration", "artifact", "spatial", "table")
  ))
  expect_error(
    runtime$builder_project_prepare_open_selection(
      list(datasets = list(record)),
      root,
      list(ds1 = "reuse")
    ),
    "inputs changed"
  )
  expect_identical(calls, 1L)
  expect_identical(config_reads, 1L)
})

test_that("checkpoint entries keep spatial images external", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    settings = list(
      spatial_image_storage = "external",
      images = list(
        fov_1 = list(image = list(source_path = "C:/external/image.png"))
      )
    )
  )

  checkpoint <- runtime$builder_project_checkpoint_entries(list(entry))

  expect_identical(
    checkpoint[[1L]]$settings$spatial_image_storage,
    "external"
  )
  expect_identical(entry$settings$spatial_image_storage, "external")
  expect_identical(checkpoint[[1L]]$settings$images, entry$settings$images)
})

test_that("checkpoint CRBs are reusable when their private closure is bundled", {
  runtime <- builder_project_test_runtime()
  item <- list(
    filename = "dataset.crb",
    sidecars = "dataset.h5",
    spatial_image_storage = "external",
    private_assets = c("dataset.crb", "dataset.h5")
  )

  expect_true(runtime$builder_project_plan_artifact_reusable(item))

  item$private_assets <- c(item$private_assets, "images/section.png")
  expect_false(runtime$builder_project_plan_artifact_reusable(item))
})

test_that("managed examples keep their origin across the async import boundary", {
  imports_path <- builder_test_inst_path(
    "builder",
    "server",
    "imports.R"
  )
  project_path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  imports <- paste(readLines(imports_path, warn = FALSE), collapse = "\n")
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(imports, "source_origin = NULL", fixed = TRUE)
  expect_match(imports, "example_id = NULL", fixed = TRUE)
  expect_match(project, "source_origin = source$origin", fixed = TRUE)
  expect_match(project, "example_id = source$example", fixed = TRUE)
})

test_that("the project status only declares safe close after source sync", {
  path <- builder_test_inst_path(
    "builder",
    "ui",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "Saved · Safe to close", fixed = TRUE)
  expect_match(source, "Stopping file copy", fixed = TRUE)
})

test_that("source synchronization warns on close without locking the workspace", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "clean",
    source_syncing = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$warn_before_unload)
  expect_true(capabilities$select_dataset)
  expect_true(capabilities$add_dataset)
  expect_true(capabilities$edit_dataset)
  expect_true(capabilities$save_project)
  expect_false(capabilities$page_inert)
})

test_that("project server uses a dedicated source copy worker", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "request_builder_project_source_sync", fixed = TRUE)
  expect_match(source, "builder_project_copier,", fixed = TRUE)
  expect_match(source, "later::later", fixed = TRUE)
  expect_match(source, "shiny::withReactiveDomain(session, {", fixed = TRUE)
  expect_match(source, "builder_project_apply_source_results", fixed = TRUE)
})

test_that("source sync keeps queued jobs until its worker starts", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  start <- regexpr(
    "builder_project_start_source_sync <- function()",
    source,
    fixed = TRUE
  )[[1L]]
  finish <- regexpr(
    "builder_project_poll_source_sync <- function()",
    source,
    fixed = TRUE
  )[[1L]]
  block <- substr(source, start, finish - 1L)

  spawn <- regexpr(
    "builder_project_worker_task_call(",
    block,
    fixed = TRUE
  )[[1L]]
  dequeue <- regexpr(
    "builder_project_source_queue(queued)",
    block,
    fixed = TRUE
  )[[1L]]
  expect_gt(spawn, 0L)
  expect_gt(dequeue, spawn)
  expect_false(grepl(
    "builder_project_source_queue(list())",
    block,
    fixed = TRUE
  ))
  expect_match(block, 'inherits(process, "condition")', fixed = TRUE)
})

test_that("source sync cancellation owns and terminates its process", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "process$kill_tree()", fixed = TRUE)
  expect_match(
    source,
    "invalidate_builder_project_source_sync(kill = TRUE)",
    fixed = TRUE
  )
  expect_match(source, "source_sync_started <-", fixed = TRUE)
})

test_that("source sync retains ownership when process termination is unconfirmed", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  first <- grep(
    "builder_project_stop_source_sync_process <- function(kill = TRUE)",
    lines,
    fixed = TRUE
  )[[1L]]
  last <- grep(
    "invalidate_builder_project_source_sync <- function(kill = TRUE)",
    lines,
    fixed = TRUE
  )[[1L]]
  runtime <- new.env(parent = baseenv())
  runtime$isolate <- function(value) value
  holder <- function(initial) {
    value <- initial
    function(next_value) {
      if (!missing(next_value)) {
        value <<- next_value
      }
      value
    }
  }
  alive <- TRUE
  fake_process <- list(
    is_alive = function() alive,
    kill_tree = function() invisible(FALSE),
    kill = function() invisible(FALSE),
    wait = function(timeout) invisible(FALSE)
  )
  progress <- withr::local_tempfile(fileext = ".rds")
  writeBin(charToRaw("progress"), progress)
  runtime$builder_project_source_process <- holder(fake_process)
  runtime$builder_project_source_run <- holder(list(owner = "project-a"))
  runtime$builder_project_source_progress <- holder(progress)
  runtime$.builder_project_text <- function(value) {
    is.character(value) && length(value) == 1L && nzchar(value)
  }
  eval(parse(text = paste(lines[first:(last - 1L)], collapse = "\n")), runtime)

  expect_false(runtime$builder_project_stop_source_sync_process(kill = TRUE))
  expect_identical(runtime$builder_project_source_process(), fake_process)
  expect_identical(
    runtime$builder_project_source_run(),
    list(owner = "project-a")
  )
  expect_identical(runtime$builder_project_source_progress(), progress)
  expect_true(file.exists(progress))

  alive <- FALSE
  expect_true(runtime$builder_project_stop_source_sync_process(kill = FALSE))
  expect_null(runtime$builder_project_source_process())
  expect_null(runtime$builder_project_source_run())
  expect_null(runtime$builder_project_source_progress())
  expect_false(file.exists(progress))
})

test_that("source sync invalidation remains cancelling after a failed stop", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  first <- grep(
    "invalidate_builder_project_source_sync <- function(kill = TRUE)",
    lines,
    fixed = TRUE
  )[[1L]]
  last <- grep(
    "builder_project_start_source_sync <- function()",
    lines,
    fixed = TRUE
  )[[1L]]
  runtime <- new.env(parent = baseenv())
  runtime$`%||%` <- function(left, right) if (is.null(left)) right else left
  runtime$isolate <- function(value) value
  holder <- function(initial) {
    value <- initial
    function(next_value) {
      if (!missing(next_value)) {
        value <<- next_value
      }
      value
    }
  }
  runtime$builder_project_source_generation <- holder(3)
  runtime$builder_project_source_queue <- holder(list(old = list(id = "old")))
  runtime$builder_project_source_run <- holder(list(owner = "project-a"))
  runtime$builder_project_source_sync <- holder(list(
    status = "syncing",
    completed = 1L,
    total = 2L,
    failed = 0L
  ))
  runtime$builder_project_stop_source_sync_process <- function(kill) FALSE
  scheduled <- 0L
  runtime$builder_project_schedule_source_sync_poll <- function(delay) {
    scheduled <<- scheduled + 1L
    invisible(TRUE)
  }
  eval(parse(text = paste(lines[first:(last - 1L)], collapse = "\n")), runtime)

  expect_false(runtime$invalidate_builder_project_source_sync(kill = TRUE))
  expect_identical(runtime$builder_project_source_generation(), 4)
  expect_length(runtime$builder_project_source_queue(), 0L)
  expect_identical(runtime$builder_project_source_sync()$status, "cancelling")
  expect_true(runtime$builder_project_source_run()$cancelling)
  expect_identical(runtime$builder_project_source_run()$owner, "project-a")
  expect_identical(scheduled, 1L)
})

test_that("a finished source process remains owned until its result is collected", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    paste(
      "  process <- isolate(builder_project_source_process())",
      "  if (!is.null(process)) {",
      sep = "\n"
    ),
    fixed = TRUE
  )
  expect_false(grepl(
    "!is.null(process) && isTRUE(process$is_alive())",
    source,
    fixed = TRUE
  ))
})

test_that("project manifests reject unsafe and duplicate dataset ids", {
  runtime <- builder_project_test_runtime()
  path <- withr::local_tempfile(fileext = ".json")

  jsonlite::write_json(
    builder_project_test_manifest(c("../outside")),
    path,
    auto_unbox = TRUE
  )
  expect_error(runtime$builder_project_read(path), "unsafe or duplicate")

  jsonlite::write_json(
    builder_project_test_manifest(c("ds1", "ds1")),
    path,
    auto_unbox = TRUE
  )
  expect_error(runtime$builder_project_read(path), "unsafe or duplicate")
})

test_that("reusable artifacts are staged without replacing existing files", {
  runtime <- builder_project_test_runtime()
  source <- withr::local_tempfile(fileext = ".crb")
  target <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("artifact"), source)
  unlink(target)

  expect_true(runtime$.builder_build_copy_file(source, target))
  expect_identical(readBin(target, "raw", n = 100L), charToRaw("artifact"))

  writeBin(charToRaw("existing"), target)
  expect_false(runtime$.builder_build_copy_file(source, target))
  expect_identical(readBin(target, "raw", n = 100L), charToRaw("existing"))
})

test_that("reusable artifact copies are verified after staging", {
  runtime <- builder_project_test_runtime()
  source <- withr::local_tempfile(fileext = ".crb")
  target <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("original"), source)
  unlink(target)
  fingerprint <- list(md5 = unname(tools::md5sum(source)))
  mutating_copy <- function(source, target) {
    copied <- file.copy(source, target)
    writeBin(charToRaw("mutated!"), target)
    copied
  }

  expect_false(runtime$.builder_build_copy_verified(
    source,
    target,
    fingerprint,
    .copy = mutating_copy
  ))
  expect_false(file.exists(target))
})

test_that("reusable artifact staging uses portable clone fallbacks", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "source.crb")
  target <- file.path(root, "darwin.crb")
  writeBin(charToRaw("artifact"), source)
  commands <- list()
  fallbacks <- 0L
  command <- function(command, args, stdout, stderr) {
    commands[[length(commands) + 1L]] <<- c(command, args)
    file.copy(source, target)
    0L
  }
  fallback <- function(...) {
    fallbacks <<- fallbacks + 1L
    TRUE
  }

  expect_true(runtime$.builder_build_copy_file(
    source,
    target,
    .sysname = "Darwin",
    .command = command,
    .fallback = fallback
  ))
  expect_true("-c" %in% commands[[1L]])

  target <- file.path(root, "linux.crb")
  expect_true(runtime$.builder_build_copy_file(
    source,
    target,
    .sysname = "Linux",
    .command = command,
    .fallback = fallback
  ))
  expect_true("--reflink=auto" %in% commands[[2L]])

  target <- file.path(root, "windows.crb")
  expect_true(runtime$.builder_build_copy_file(
    source,
    target,
    .sysname = "Windows",
    .command = command,
    .fallback = fallback
  ))
  expect_identical(length(commands), 2L)
  expect_identical(fallbacks, 1L)
})

test_that("persisted artifact fingerprints contain only plain JSON values", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("artifact"), artifact)

  fingerprint <- runtime$builder_project_file_fingerprint(
    artifact,
    content = TRUE
  )

  expect_identical(class(fingerprint$md5), "character")
  expect_silent(jsonlite::toJSON(fingerprint, auto_unbox = TRUE))
})

test_that("configuration digests are plain JSON strings", {
  runtime <- builder_project_test_runtime()

  digest <- runtime$builder_project_configuration_digest(list())

  expect_identical(class(digest), "character")
  expect_silent(jsonlite::toJSON(list(digest = digest), auto_unbox = TRUE))
})

test_that("legacy qs reminders identify only qs datasets", {
  runtime <- builder_project_test_runtime()
  found <- runtime$builder_project_legacy_qs_datasets(list(
    list(id = "ds1", format = "qs", settings = list(name = "Legacy one")),
    list(id = "ds2", format = "QS", name = "Legacy two"),
    list(id = "ds3", format = "qs2", name = "Current"),
    list(id = "ds4", format = "rds", name = "RDS")
  ))

  expect_identical(vapply(found, `[[`, character(1), "id"), c("ds1", "ds2"))
  expect_identical(
    vapply(found, `[[`, character(1), "name"),
    c("Legacy one", "Legacy two")
  )
})

test_that("legacy qs reminder explains the qs2 migration", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )

  html <- htmltools::renderTags(runtime$builder_legacy_qs_notice_dialog(
    list(
      list(id = "ds1", name = "Legacy one"),
      list(id = "ds2", name = "Legacy two")
    ),
    origin = "project"
  ))$html

  expect_match(html, "Legacy QS files", fixed = TRUE)
  expect_equal(
    lengths(regmatches(
      html,
      gregexpr("Legacy QS files", html, fixed = TRUE)
    )),
    1L
  )
  expect_match(html, "2 datasets in the project", fixed = TRUE)
  expect_match(html, "deprecated .qs format", fixed = TRUE)
  expect_match(html, "Use QS2 for future projects.", fixed = TRUE)
  expect_match(html, "Datasets (2)", fixed = TRUE)
  expect_match(html, "Legacy one", fixed = TRUE)
  expect_match(html, "Legacy two", fixed = TRUE)
  expect_match(html, "continue_builder_project_legacy_qs", fixed = TRUE)
  expect_match(html, "Review Datasets", fixed = TRUE)
  expect_match(html, "btn btn-primary", fixed = TRUE)
  expect_match(html, "cancel_builder_project_open", fixed = TRUE)

  duplicate_html <- htmltools::renderTags(
    runtime$builder_legacy_qs_notice_content(
      list(
        list(id = "ds1", name = "Repeated name"),
        list(id = "ds2", name = "Repeated name")
      ),
      origin = "project"
    )
  )$html
  expect_match(duplicate_html, "2 datasets in the project", fixed = TRUE)
  expect_match(duplicate_html, "Datasets (2)", fixed = TRUE)
  expect_match(duplicate_html, "Repeated name (ds1)", fixed = TRUE)
  expect_match(duplicate_html, "Repeated name (ds2)", fixed = TRUE)

  add_html <- htmltools::renderTags(runtime$builder_legacy_qs_notice_dialog(
    list(list(id = "ds1", name = "Legacy one")),
    origin = "add"
  ))$html
  expect_match(add_html, "Continue", fixed = TRUE)
  expect_match(add_html, "btn btn-primary", fixed = TRUE)
  expect_match(add_html, "data-dismiss=\"modal\"", fixed = TRUE)
})

test_that("project opening dialog provides immediate progress feedback", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )

  html <- htmltools::renderTags(runtime$builder_project_opening_dialog())$html

  expect_match(html, "Checking project", fixed = TRUE)
  expect_match(html, "Preparing saved datasets", fixed = TRUE)
  expect_match(html, "builder-project-opening-progress", fixed = TRUE)
  expect_match(html, "cancel_builder_project_open", fixed = TRUE)
})

test_that("Project backup recovery is an explicit verified choice", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )

  html <- htmltools::renderTags(runtime$builder_project_backup_recovery_dialog(
    list(
      project_name = "Recovered fixture",
      project_revision = 7L,
      dataset_count = 3L
    )
  ))$html

  expect_match(html, "damaged", fixed = TRUE)
  expect_match(html, "verified backup", fixed = TRUE)
  expect_match(html, "Recovered fixture", fixed = TRUE)
  expect_match(html, "confirm_builder_project_backup_recovery", fixed = TRUE)
  expect_match(html, "cancel_builder_project_backup_recovery", fixed = TRUE)
})

test_that("project qs warning stays separate from dataset selection", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$selectInput <- shiny::selectInput
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )
  status <- list(
    actions = c("resume", "skip"),
    default_action = "resume",
    message = "Source required",
    detail = NULL,
    problem = NULL
  )
  manifest <- list(
    project = list(name = "Project"),
    datasets = list(
      list(
        id = "ds1",
        name = "Legacy source",
        format = "qs",
        source = list(fingerprint = list(bytes = 10)),
        runtime_restore_status = status
      ),
      list(
        id = "ds2",
        name = "Current source",
        format = "qs2",
        source = list(fingerprint = list(bytes = 20)),
        runtime_restore_status = status
      )
    )
  )
  warning_html <- htmltools::renderTags(
    runtime$builder_legacy_qs_notice_dialog(
      runtime$builder_project_legacy_qs_datasets(manifest$datasets),
      origin = "project"
    )
  )$html
  selection_html <- htmltools::renderTags(
    runtime$builder_project_restore_dialog(manifest)
  )$html

  expect_match(warning_html, "Legacy QS files", fixed = TRUE)
  expect_match(warning_html, "Legacy source", fixed = TRUE)
  expect_false(grepl("Choose Datasets to Open", warning_html, fixed = TRUE))
  expect_match(selection_html, "Choose Datasets to Open", fixed = TRUE)
  expect_false(grepl(
    "Legacy QS files",
    selection_html,
    fixed = TRUE
  ))
})

test_that("configuration contract v3 hashes canonical JSON bytes", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    revision = 4L,
    settings = list(name = "Dataset", included_projections = c("umap", "pca")),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  value <- runtime$builder_project_configuration_digest_value(entry)
  canonical <- jsonlite::serializeJSON(value, digits = NA, pretty = FALSE)
  expected <- unclass(as.character(digest::digest(
    charToRaw(enc2utf8(canonical)),
    algo = "md5",
    serialize = FALSE
  )))

  expect_identical(runtime$.builder_project_configuration_contract_version, 3L)
  expect_identical(
    runtime$builder_project_configuration_digest(entry),
    expected
  )
})

test_that("only the current configuration contract is supported", {
  runtime <- builder_project_test_runtime()

  expect_identical(runtime$.builder_project_contract_version(3L), 3L)
  expect_identical(runtime$.builder_project_contract_version("3"), 3L)
  for (version in list(NULL, 0L, 1L, 2L, 4L, 999L, 1.5, 2.9, 3.5, "3x")) {
    expect_error(
      runtime$.builder_project_contract_version(version),
      "not supported",
      fixed = TRUE
    )
  }
})

test_that("persisted schema and revision integers are exact", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  path <- file.path(root, "builder-project.json")
  manifest <- runtime$builder_project_new_manifest(root, "Strict integers")
  write_manifest <- function(value) {
    writeBin(
      charToRaw(enc2utf8(jsonlite::serializeJSON(
        value,
        digits = NA,
        pretty = FALSE
      ))),
      path
    )
  }
  for (bad in list(3.5, "3x", -1, .Machine$integer.max + 1)) {
    candidate <- manifest
    candidate$schema_version <- bad
    write_manifest(candidate)
    expect_error(
      runtime$builder_project_read_manifest_payload(path),
      "not a supported Builder project",
      fixed = TRUE
    )
    candidate <- manifest
    candidate$project$revision <- bad
    write_manifest(candidate)
    expect_error(
      runtime$builder_project_read_manifest_payload(path),
      "not a supported Builder project",
      fixed = TRUE
    )
  }

  config <- list(
    id = "ds1",
    revision = 1.1,
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  config_path <- file.path(root, "config.json")
  writeBin(
    charToRaw(enc2utf8(jsonlite::serializeJSON(
      config,
      digits = NA,
      pretty = FALSE
    ))),
    config_path
  )
  expect_error(
    runtime$builder_project_verify_configuration(
      config_path,
      "ds1",
      1.9,
      runtime$builder_project_configuration_digest(config),
      runtime$builder_project_file_fingerprint(config_path, content = TRUE)
    ),
    "persisted integer is invalid",
    fixed = TRUE
  )
})

test_that("build execution rejects a reusable CRB changed after checkpoint", {
  runtime <- builder_project_test_runtime()
  artifact <- withr::local_tempfile(fileext = ".crb")
  writeBin(charToRaw("original"), artifact)
  fingerprint <- list(md5 = unname(tools::md5sum(artifact)))
  writeBin(charToRaw("replacement"), artifact)
  stage <- withr::local_tempdir()
  plan <- structure(
    list(
      items = list(list(
        id = "ds1",
        name = "Dataset 1",
        filename = "dataset-1.crb",
        reused_artifact = list(
          path = artifact,
          fingerprint = fingerprint,
          members = list()
        )
      )),
      make_app = FALSE,
      app_auth = list(enabled = FALSE)
    ),
    class = c("builder_build_plan", "list")
  )
  unused <- function(...) stop("changed artifacts must fail before hooks run")
  hooks <- list(
    open_snapshot = unused,
    prepare = unused,
    run_analyses = unused,
    export = unused,
    attach_extras = unused,
    verify = unused
  )

  result <- runtime$builder_execute_plan(plan, stage, list(), hooks = hooks)

  expect_identical(result$state, "failure")
  expect_match(result$error, "could not be staged", fixed = TRUE)
  expect_false(file.exists(file.path(stage, "dataset-1.crb")))
})

test_that("saved plan items are reused only when artifact references agree", {
  runtime <- builder_project_test_runtime()
  saved <- list(
    id = "ds1",
    name = "Dataset 1",
    filename = "dataset-1.crb",
    sidecars = "dataset-1.h5",
    viewer_bundle_assets = character(),
    private_assets = c("dataset-1.crb", "dataset-1.h5"),
    viewer_bundle_asset_claims = list(),
    private_asset_claims = list()
  )

  expect_true(runtime$builder_project_reused_plan_matches(saved, saved))
  renamed <- saved
  renamed$name <- "Repeated display name"
  expect_true(runtime$builder_project_reused_plan_matches(saved, renamed))
  changed <- saved
  changed$filename <- "renamed.crb"
  expect_false(runtime$builder_project_reused_plan_matches(saved, changed))
})

test_that("duplicate-name Project entries round trip and delete independently", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  manifest <- runtime$builder_project_new_manifest(root, "Duplicate identities")
  make_entry <- function(id, point_size, rotation) {
    list(
      id = id,
      revision = 1L,
      settings = list(
        name = "Repeated",
        overview_point_size = point_size,
        spatial_coordinate_transforms = list(
          fov = list(rotation_degrees = rotation, scale = 1)
        )
      ),
      acknowledgements = character(),
      spatial_drafts = list()
    )
  }
  entries <- list(make_entry("ds4", 4, 15), make_entry("ds7", 9, 75))
  manifest$datasets <- lapply(entries, function(entry) {
    runtime$builder_project_dataset_record(
      entry,
      source = list(
        kind = "missing",
        path = NULL,
        fingerprint = list(md5 = strrep("a", 32L))
      ),
      root = root
    )
  })

  saved <- runtime$builder_project_write(manifest, root)
  reopened <- runtime$builder_project_read(saved$path)
  restored <- lapply(reopened$datasets, function(record) {
    runtime$builder_project_read_dataset_config(record, root)
  })

  expect_identical(vapply(restored, `[[`, character(1), "id"), c("ds4", "ds7"))
  expect_identical(
    vapply(restored, function(entry) entry$settings$name, character(1)),
    c("Repeated", "Repeated")
  )
  expect_identical(
    vapply(
      restored,
      function(entry) entry$settings$overview_point_size,
      numeric(1)
    ),
    c(4, 9)
  )
  expect_identical(
    vapply(
      restored,
      function(entry) {
        entry$settings$spatial_coordinate_transforms$fov$rotation_degrees
      },
      numeric(1)
    ),
    c(15, 75)
  )

  reopened$datasets <- reopened$datasets[2L]
  deleted <- runtime$builder_project_write(
    reopened,
    root,
    expected_revision = reopened$project$revision
  )
  expect_identical(
    vapply(deleted$manifest$datasets, `[[`, character(1), "id"),
    "ds7"
  )
  surviving <- runtime$builder_project_read_dataset_config(
    deleted$manifest$datasets[[1L]],
    root
  )
  expect_identical(surviving$settings$name, "Repeated")
  expect_identical(surviving$settings$overview_point_size, 9)
})

test_that("same-source artifacts retain separate ownership and reuse records", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build <- file.path(root, "build")
  project <- file.path(root, "project")
  dir.create(build)
  dir.create(project)
  primaries <- file.path(build, c("first.crb", "second.crb"))
  lapply(primaries, function(path) writeBin(charToRaw("same CRB bytes"), path))
  entries <- list(
    list(
      id = "ds4",
      revision = 2L,
      snapshot = list(source_fingerprint = "same-source"),
      settings = list(name = "Repeated")
    ),
    list(
      id = "ds7",
      revision = 3L,
      snapshot = list(source_fingerprint = "same-source"),
      settings = list(name = "Repeated")
    )
  )
  items <- Map(
    function(id, path) {
      list(
        id = id,
        name = "Repeated",
        filename = basename(path),
        sidecars = character(),
        private_assets = basename(path)
      )
    },
    c("ds4", "ds7"),
    primaries
  )

  prepared <- runtime$builder_project_prepare_build_artifacts(
    plan = list(items = items),
    value = list(
      state = "success",
      built = stats::setNames(primaries, c("ds4", "ds7")),
      verifications = list(ds4 = list(), ds7 = list())
    ),
    root = project,
    previous_artifacts = list(),
    entries = entries
  )

  expect_null(prepared$error)
  expect_identical(names(prepared$artifacts), c("ds4", "ds7"))
  payloads <- lapply(
    prepared$artifacts,
    function(artifact) jsonlite::unserializeJSON(artifact$plan_payload)
  )
  expect_identical(
    unname(vapply(payloads, `[[`, character(1), "id")),
    c("ds4", "ds7")
  )
  expect_identical(
    unname(vapply(payloads, `[[`, character(1), "name")),
    c("Repeated", "Repeated")
  )
  expect_false(any(vapply(
    prepared$artifacts,
    function(artifact) {
      any(c("dataset_id", "display_name") %in% names(artifact))
    },
    logical(1)
  )))
  expect_false(identical(
    prepared$artifacts$ds4$path,
    prepared$artifacts$ds7$path
  ))
  expect_identical(
    unname(vapply(
      prepared$artifacts,
      `[[`,
      character(1),
      "built_from_source_fingerprint"
    )),
    c("same-source", "same-source")
  )

  manifest <- runtime$builder_project_new_manifest(
    project,
    "Independent owners"
  )
  manifest$datasets <- Map(
    function(entry, artifact, index) {
      runtime$builder_project_dataset_record(
        entry,
        source = list(kind = "missing", path = NULL),
        checked = TRUE,
        artifact = artifact,
        order = index,
        root = project
      )
    },
    entries,
    prepared$artifacts,
    seq_along(entries)
  )
  saved <- runtime$builder_project_write(manifest, project)
  survivor_artifact <- saved$manifest$datasets[[2L]]$artifact
  survivor_path <- runtime$builder_project_resolve_path(
    survivor_artifact$path,
    project,
    "managed"
  )
  saved$manifest$datasets <- saved$manifest$datasets[2L]
  deleted <- runtime$builder_project_write(
    saved$manifest,
    project,
    expected_revision = saved$manifest$project$revision
  )
  expect_identical(deleted$manifest$datasets[[1L]]$id, "ds7")
  expect_true(file.exists(survivor_path))
  expect_true(runtime$builder_project_artifact_available(
    survivor_artifact,
    project
  ))
})

test_that("ready CRB workbench keeps the Review continuation actions", {
  project_ui <- paste(
    readLines(
      builder_test_inst_path(
        "builder",
        "ui",
        "project.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  project_server <- paste(
    readLines(
      builder_test_inst_path(
        "builder",
        "server",
        "review.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  workflow_ui <- paste(
    readLines(
      builder_test_inst_path("builder", "ui", "workflow.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(
    project_ui,
    'uiOutput("configure_actions")',
    fixed = TRUE
  )
  expect_match(
    project_ui,
    "builder_configure_primary_action_ui(",
    fixed = TRUE
  )
  expect_match(
    workflow_ui,
    '"continue_to_review",',
    fixed = TRUE
  )
  expect_match(
    workflow_ui,
    '"Continue to Review",',
    fixed = TRUE
  )
  expect_match(
    project_server,
    "builder_project_artifact_actions_ui(",
    fixed = TRUE
  )
})

test_that("restore choices render descriptive labels and prefer checked CRB reuse", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$selectInput <- shiny::selectInput
  ui_path <- builder_test_inst_path(
    "builder",
    "ui",
    "project.R"
  )
  runtime$builder_source_utf8(ui_path, envir = runtime)
  root <- withr::local_tempdir()
  source_path <- file.path(root, "source.rds")
  artifact_path <- file.path(root, "artifact.crb")
  writeBin(charToRaw("source"), source_path)
  writeBin(charToRaw("artifact"), artifact_path)
  entry <- list(id = "ds1", settings = list(name = "Dataset A"))
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      status = "ready",
      path = runtime$builder_project_relative_path(source_path, root),
      fingerprint = runtime$builder_project_file_fingerprint(
        source_path,
        content = TRUE
      )
    ),
    checked = TRUE,
    artifact = list(
      status = "ready",
      reusable = TRUE,
      path = runtime$builder_project_relative_path(artifact_path, root),
      fingerprint = runtime$builder_project_file_fingerprint(
        artifact_path,
        content = TRUE
      ),
      members = list(),
      built_from_revision = 0L,
      built_from_source_fingerprint = paste(
        "builder-snapshot-v2",
        "source.rds",
        file.info(source_path)$size[[1L]],
        "timestamp",
        unname(tools::md5sum(source_path)),
        sep = ":"
      ),
      built_from_configuration = runtime$builder_project_configuration_digest(
        entry
      )
    ),
    root = root
  )

  artifact_digest_calls <- 0L
  original_artifact_digest <-
    runtime$builder_project_artifact_configuration_digest
  runtime$builder_project_artifact_configuration_digest <- function(...) {
    artifact_digest_calls <<- artifact_digest_calls + 1L
    original_artifact_digest(...)
  }
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )
  expect_identical(artifact_digest_calls, 1L)
  runtime$builder_project_artifact_configuration_digest <-
    original_artifact_digest
  html <- htmltools::renderTags(runtime$builder_project_restore_row_ui(
    record
  ))$html

  expect_match(html, "Open from cache (fast)", fixed = TRUE)
  expect_match(html, "Reload from source file", fixed = TRUE)
  expect_match(html, '<option value="reuse" selected>', fixed = TRUE)
  expect_match(html, ">Checked</span>", fixed = TRUE)
  expect_match(html, 'data-tone="ready">Cached</span>', fixed = TRUE)
  expect_match(
    html,
    "The cached copy can be used immediately; the source is not loaded.",
    fixed = TRUE
  )
  expect_match(html, "Source 6 B", fixed = TRUE)
  expect_match(html, "Cache 8 B", fixed = TRUE)
  expect_identical(
    runtime$builder_project_restore_artifact_size(list(
      artifact = list(
        fingerprint = list(bytes = 8),
        members = list(list(fingerprint = list(bytes = 4)))
      )
    )),
    "12 B"
  )

  no_crb <- record
  no_crb$artifact <- NULL
  no_crb$runtime_restore_status <- list(
    actions = c("resume", "reload", "skip"),
    default_action = "resume",
    checked = TRUE,
    crb_ready = FALSE,
    message = "Configuration ready",
    detail = "The original source will load with saved settings.",
    problem = NULL
  )
  no_crb_html <- htmltools::renderTags(
    runtime$builder_project_restore_row_ui(no_crb)
  )$html
  expect_match(no_crb_html, 'data-tone="none">Not cached</span>', fixed = TRUE)
  expect_false(grepl(
    'data-tone="ready">Cached</span>',
    no_crb_html,
    fixed = TRUE
  ))

  original_mtime <- file.info(source_path)$mtime[[1L]]
  Sys.setFileTime(source_path, Sys.time() + 120)
  deferred <- runtime$builder_project_dataset_status(record, root)
  expect_identical(deferred$default_action, "reuse")
  expect_identical(deferred$message, "Checked · Cached")
  expect_true(deferred$checked)
  expect_true(deferred$crb_ready)
  expect_match(
    deferred$detail,
    "The source will be verified only if you load it.",
    fixed = TRUE
  )
  Sys.setFileTime(source_path, original_mtime)
  record$source$fingerprint <- runtime$builder_project_file_fingerprint(
    source_path,
    content = TRUE
  )

  record$configuration$checked <- FALSE
  record$configuration$reviewed <- FALSE
  record$artifact$status <- "stale"
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )
  html <- htmltools::renderTags(runtime$builder_project_restore_row_ui(
    record
  ))$html
  expect_match(html, "Needs checking", fixed = TRUE)

  record$configuration$reviewed <- TRUE
  record$configuration$reviewed_digest <- "stale-configuration"
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )
  html <- htmltools::renderTags(runtime$builder_project_restore_row_ui(
    record
  ))$html
  expect_match(html, "Needs checking", fixed = TRUE)
})

test_that("saved CRB workbench states the active data source", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$actionButton <- shiny::actionButton
  runtime$uiOutput <- shiny::uiOutput
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "workflow.R"),
    envir = runtime
  )
  runtime$builder_stage_header_ui <- function(kicker, title, description) {
    shiny::tags$header(kicker, title, description)
  }
  runtime$builder_stage_section_ui <- function(title, ..., description = NULL) {
    shiny::tags$section(title, description, ...)
  }
  runtime$builder_stage_footer_ui <- function(message, ...) {
    shiny::tags$footer(message, ...)
  }
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )
  entry <- list(
    id = "ds1",
    settings = list(name = "Dataset A"),
    project_artifact = list(path = "artifacts/ds1/dataset.crb")
  )

  workbench <- htmltools::renderTags(
    runtime$builder_project_artifact_workbench_ui(entry)
  )$html
  actions <- htmltools::renderTags(
    runtime$builder_project_artifact_actions_ui("Ready", TRUE)
  )$html
  next_actions <- htmltools::renderTags(
    runtime$builder_project_artifact_actions_ui(
      "1 of 2 datasets checked · 1 remaining",
      FALSE,
      dataset_checked = TRUE,
      remaining = 1L
    )
  )$html

  expect_match(
    workbench,
    "Cached copy ready to build.",
    fixed = TRUE
  )
  expect_match(workbench, "Ready to build", fixed = TRUE)
  expect_match(
    workbench,
    "Saved settings are kept.",
    fixed = TRUE
  )
  expect_match(workbench, 'id="project-source-load-status"', fixed = TRUE)
  expect_match(workbench, 'aria-live="polite"', fixed = TRUE)
  expect_match(workbench, "The cached copy remains available.", fixed = TRUE)
  expect_match(actions, "Load original source to edit", fixed = TRUE)
  expect_match(actions, "builder-project-source-load-button", fixed = TRUE)
  expect_match(
    actions,
    'aria-describedby="project-source-load-status"',
    fixed = TRUE
  )
  expect_match(next_actions, 'id="complete_dataset_check"', fixed = TRUE)
  expect_match(
    next_actions,
    '<span class="action-label">Next</span>',
    fixed = TRUE
  )
  expect_false(grepl("continue_to_review", next_actions, fixed = TRUE))
})

test_that("missing saved settings reload the source instead of forcing a skip", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$selectInput <- shiny::selectInput
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )
  root <- withr::local_tempdir()
  source_path <- file.path(root, "source.rds")
  writeBin(charToRaw("source"), source_path)
  record <- runtime$builder_project_dataset_record(
    list(id = "ds1", settings = list(name = "Long source dataset")),
    source = list(
      kind = "managed",
      path = runtime$builder_project_relative_path(source_path, root)
    ),
    checked = TRUE,
    root = root
  )
  unlink(runtime$builder_project_resolve_path(
    record$configuration$path,
    root,
    "managed"
  ))

  status <- runtime$builder_project_dataset_status(record, root)
  expect_null(status$configuration_entry)
  expect_identical(status$actions, c("reload", "skip"))
  expect_identical(status$default_action, "reload")
  expect_identical(
    status$message,
    "Settings unavailable"
  )
  expect_match(status$problem, "configuration is missing")

  record$runtime_restore_status <- status
  html <- htmltools::renderTags(runtime$builder_project_restore_row_ui(
    record
  ))$html
  expect_match(html, "Re-import source · Reset settings", fixed = TRUE)
  expect_match(html, '<option value="reload" selected>', fixed = TRUE)
  expect_match(html, "Settings unavailable", fixed = TRUE)
  expect_match(html, 'data-current-action="reload"', fixed = TRUE)
  expect_identical(
    runtime$builder_project_restore_source_size(list(
      source = list(fingerprint = list(bytes = 6))
    )),
    "6 B"
  )
  expect_null(runtime$builder_project_restore_artifact_size(record))
  expect_match(html, "Not cached", fixed = TRUE)
  expect_false(grepl("Cache none", html, fixed = TRUE))

  record$release$included <- FALSE
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )
  excluded_html <- htmltools::renderTags(
    runtime$builder_project_restore_row_ui(record)
  )$html
  expect_match(excluded_html, 'data-current-action="skip"', fixed = TRUE)
  expect_match(excluded_html, '<option value="skip" selected>', fixed = TRUE)

  prepared <- runtime$builder_project_prepare_open_selection(
    list(datasets = list(record)),
    root,
    list(ds1 = "reload")
  )
  expect_named(prepared$pending_entries, "ds1")
  expect_identical(
    prepared$pending_entries$ds1$runtime_restore_action,
    "reload"
  )
  expect_identical(prepared$skipped_ids, character())
})

test_that("project restore rows keep long labels inside their grid tracks", {
  css <- paste(
    readLines(builder_test_inst_path(
      "builder",
      "www",
      "builder.components.css"
    )),
    collapse = "\n"
  )

  expect_match(
    css,
    ".builder-project-restore-summary {\n  display: grid;\n  min-width: 0;",
    fixed = TRUE
  )
  expect_match(css, "overflow-wrap: anywhere;", fixed = TRUE)
  expect_match(
    css,
    ".builder-project-restore-row .form-group {\n  min-width: 0;",
    fixed = TRUE
  )
  expect_match(css, "overscroll-behavior: contain;", fixed = TRUE)
  expect_match(css, ".builder-project-restore-row:focus-within", fixed = TRUE)
  expect_match(css, 'data-current-action="skip"', fixed = TRUE)
  # One select style for every open mode; reuse no longer paints the select.
  expect_match(
    css,
    "#shiny-modal .builder-project-restore-row select {",
    fixed = TRUE
  )
  expect_false(grepl('data-current-action="reuse"] select', css, fixed = TRUE))
  expect_match(css, "select:focus-visible", fixed = TRUE)
  expect_match(css, 'crb-tag[data-tone="ready"]', fixed = TRUE)
  expect_match(css, 'crb-tag[data-tone="none"]', fixed = TRUE)
})

test_that("project lifecycle capabilities lock only conflicting operations", {
  runtime <- builder_project_test_runtime()

  importing <- runtime$builder_activity_state(
    client_imports = 1L,
    project_phase = "dirty",
    has_project = TRUE,
    has_datasets = TRUE
  )
  import_capabilities <- runtime$builder_activity_capabilities(importing)
  expect_true(import_capabilities$add_dataset)
  expect_true(import_capabilities$edit_dataset)
  expect_true(import_capabilities$check_dataset)
  expect_true(import_capabilities$navigate_workflow)
  expect_false(import_capabilities$save_project)
  expect_false(import_capabilities$build)

  saving <- runtime$builder_activity_state(
    project_phase = "saving",
    has_project = TRUE,
    has_datasets = TRUE
  )
  save_capabilities <- runtime$builder_activity_capabilities(saving)
  expect_false(save_capabilities$page_inert)
  expect_true(save_capabilities$navigate_workflow)
  expect_true(save_capabilities$select_dataset)
  expect_true(save_capabilities$warn_before_unload)
  expect_false(save_capabilities$mutate_datasets)
  expect_false(save_capabilities$save_project)
})

test_that("background manifest writes keep the workspace editable", {
  runtime <- builder_project_test_runtime()
  for (phase in c("autosaving", "registering")) {
    activity <- runtime$builder_activity_state(
      project_phase = phase,
      has_project = TRUE,
      has_datasets = TRUE
    )
    capabilities <- runtime$builder_activity_capabilities(activity)

    expect_true(capabilities$add_dataset, info = phase)
    expect_true(capabilities$edit_dataset, info = phase)
    expect_true(capabilities$mutate_datasets, info = phase)
    expect_true(capabilities$check_dataset, info = phase)
    expect_true(capabilities$navigate_workflow, info = phase)
    expect_false(capabilities$page_inert, info = phase)
    expect_true(capabilities$warn_before_unload, info = phase)
    ## Other manifest writers still wait for the background write.
    expect_false(capabilities$save_project, info = phase)
    expect_false(capabilities$open_project, info = phase)
    expect_false(capabilities$build, info = phase)
    expect_identical(
      runtime$builder_activity_reason(activity, "save_project"),
      if (identical(phase, "registering")) {
        "Wait for the dataset cache to finish saving."
      } else {
        "Wait for the project operation to finish."
      },
      info = phase
    )
  }
})

test_that("Build says it waits for the dataset cache after a Build", {
  runtime <- builder_project_test_runtime()
  registering <- runtime$builder_activity_state(
    project_phase = "registering",
    has_project = TRUE,
    has_datasets = TRUE
  )
  expect_identical(
    runtime$builder_activity_reason(registering, "build"),
    "Wait for the dataset cache to finish saving."
  )
  clean <- runtime$builder_activity_state(
    project_phase = "clean",
    has_project = TRUE,
    has_datasets = TRUE
  )
  expect_null(runtime$builder_activity_reason(clean, "build"))

  ## The browser shows the reason beside "Build again to same folder", which
  ## is gated on the same capability as Build.
  server <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    server,
    'build_reason = builder_activity_reason(activity, "build")',
    fixed = TRUE
  )
})

test_that("background saves merge edits made while the manifest was written", {
  runtime <- builder_project_test_runtime()
  entry <- function(id, name, saved = FALSE) {
    list(
      id = id,
      settings = list(name = name),
      project_source = if (saved) paste0("sources/", id) else NULL
    )
  }
  base <- list(entry("ds1", "One"), entry("ds2", "Two"))
  saved <- list(entry("ds1", "One", TRUE), entry("ds2", "Two", TRUE))

  expect_identical(
    runtime$builder_project_merge_saved_entries(base, saved, base),
    saved
  )

  live <- list(
    entry("ds2", "Renamed"),
    entry("ds1", "One"),
    entry("ds3", "New")
  )
  merged <- runtime$builder_project_merge_saved_entries(base, saved, live)

  expect_identical(
    vapply(merged, `[[`, character(1), "id"),
    c("ds2", "ds1", "ds3")
  )
  expect_identical(merged[[1L]], live[[1L]])
  expect_identical(merged[[2L]], saved[[1L]])
  expect_identical(merged[[3L]], live[[3L]])

  removed <- runtime$builder_project_merge_saved_entries(
    base,
    saved,
    list(entry("ds2", "Two"))
  )
  expect_identical(removed, list(saved[[2L]]))
})

test_that("Save releases the workspace once the manifest commits", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this save test.")
  }

  shiny::testServer(app_env$server, {
    server_env <- environment(complete_builder_project_save_request)
    server_env$save_builder_project_state <- function(..., on_complete) {
      on_complete(TRUE)
      invisible("pending")
    }
    calls <- new.env(parent = emptyenv())
    calls$ok <- logical()
    after <- function(ok) {
      calls$ok <- c(calls$ok, ok)
      invisible(ok)
    }
    builder_project_source_sync(list(
      status = "syncing",
      completed = 0L,
      total = 2L,
      failed = 0L
    ))
    builder_project_operation_phase("saving")

    complete_builder_project_save_request(show_actions = TRUE, after = after)

    expect_identical(isolate(builder_project_operation_phase()), "idle")
    waiter <- isolate(builder_project_source_save_waiter())
    expect_true(waiter$waiting)
    expect_length(calls$ok, 0L)

    ## A second Save while the first copy runs keeps the first follow-up.
    builder_project_operation_phase("saving")
    complete_builder_project_save_request(show_actions = TRUE, after = after)
    expect_identical(isolate(builder_project_operation_phase()), "idle")
    waiter <- isolate(builder_project_source_save_waiter())

    ## The late source result reports to the caller without taking over the
    ## phase of a newer project operation.
    builder_project_operation_phase("autosaving")
    finish_builder_project_save_request(
      FALSE,
      waiter$after,
      only_when_idle = TRUE
    )
    expect_identical(isolate(builder_project_operation_phase()), "autosaving")
    expect_identical(calls$ok, c(FALSE, FALSE))

    builder_project_operation_phase("idle")
    finish_builder_project_save_request(TRUE, NULL, only_when_idle = TRUE)
    expect_identical(isolate(builder_project_operation_phase()), "idle")
  })
})

test_that("autosave runs as a background manifest write", {
  skip_if_not_installed("shiny")
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"))
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this autosave test.")
  }

  shiny::testServer(app_env$server, {
    server_env <- environment(request_builder_project_save)
    started <- new.env(parent = emptyenv())
    server_env$start_builder_project_save_request <- function(...) {
      started$args <- list(...)
      invisible(TRUE)
    }

    expect_true(request_builder_project_save(
      show_actions = FALSE,
      materialize = FALSE,
      notify = FALSE,
      background = TRUE
    ))
    expect_identical(isolate(builder_project_operation_phase()), "autosaving")
    expect_false(request_builder_project_save(show_actions = TRUE))
    expect_identical(isolate(builder_project_operation_phase()), "autosaving")
  })
})

test_that("background CRB generation permits editing but protects project boundaries", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "dirty",
    crb_generating = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$edit_dataset)
  expect_true(capabilities$check_dataset)
  expect_true(capabilities$navigate_workflow)
  expect_true(capabilities$save_project)
  ## Opening a Project cancels its optional CRB builds instead of waiting.
  expect_true(capabilities$open_project)
  expect_null(capabilities$prepare_crbs)
  expect_true(capabilities$warn_before_unload)
  expect_null(runtime$builder_activity_reason(activity, "open_project"))
})

test_that("project autosave waits for both import queues to drain", {
  runtime <- builder_project_test_runtime()
  activity <- function(client_imports = 0L, server_imports = FALSE) {
    runtime$builder_activity_state(
      client_imports = client_imports,
      server_imports = server_imports,
      project_phase = "dirty",
      has_project = TRUE,
      has_datasets = TRUE
    )
  }

  expect_false(
    runtime$builder_activity_capabilities(activity(client_imports = 1L))[[
      "save_project"
    ]]
  )
  expect_false(
    runtime$builder_activity_capabilities(activity(server_imports = TRUE))[[
      "save_project"
    ]]
  )
  expect_true(runtime$builder_activity_capabilities(activity())$save_project)
})

test_that("terminal retry rows keep project operations blocked", {
  runtime <- builder_project_test_runtime()
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "loading.R"),
    envir = runtime
  )
  queue <- runtime$builder_import_queue()
  entry <- runtime$builder_import_entry(
    "ds2",
    "Failed dataset",
    list(kind = "file", path = "failed.qs2")
  )
  queue <- runtime$builder_import_add(queue, entry)
  queue <- runtime$builder_import_transition(queue, "ds2", "reading", 1L)
  activity <- function(imports) {
    ## The successful import is already attached; the retry row remains active.
    runtime$builder_activity_state(
      server_imports = length(imports$entries) > 0L,
      project_phase = "dirty",
      has_project = TRUE,
      has_datasets = TRUE
    )
  }

  expect_false(
    runtime$builder_activity_capabilities(activity(queue))$save_project
  )

  queue <- runtime$builder_import_transition(
    queue,
    "ds2",
    "error",
    1L,
    error = "Could not read the dataset."
  )

  expect_length(queue$entries, 1L)
  expect_identical(queue$entries$ds2$load_state, "error")
  expect_false(
    runtime$builder_activity_capabilities(activity(queue))$save_project
  )

  project_server <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(
    project_server,
    "server_imports = length(imports()$entries %||% list()) > 0L",
    fixed = TRUE
  )
})

test_that("project autosave checks save capability before requesting a save", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  observer_start <- regexpr(
    "observe({\n  project <- builder_project()",
    source,
    fixed = TRUE
  )
  observer_end <- regexpr(
    "show_builder_project_folder_error <- function",
    source,
    fixed = TRUE
  )
  expect_gt(observer_start[[1L]], 0L)
  expect_gt(observer_end[[1L]], observer_start[[1L]])
  autosave <- substr(
    source,
    observer_start[[1L]],
    observer_end[[1L]] - 1L
  )
  guard <- regexpr(
    paste(
      "if (!isTRUE(builder_capabilities()$save_project)) {",
      "    return()",
      "  }",
      sep = "\n"
    ),
    autosave,
    fixed = TRUE
  )[[1L]]
  request <- regexpr(
    "request_builder_project_save(",
    autosave,
    fixed = TRUE
  )[[1L]]

  expect_gt(guard, 0L)
  expect_gt(request, guard)
})

test_that("project save waits for spatial editing to settle", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  save_start <- regexpr(
    "save_builder_project_state <- function(",
    source,
    fixed = TRUE
  )[[1L]]
  save_end <- regexpr(
    "complete_builder_project_save_request <- function(",
    source,
    fixed = TRUE
  )[[1L]]
  expect_gt(save_start, 0L)
  expect_gt(save_end, save_start)
  save <- substr(source, save_start, save_end - 1L)

  expect_match(
    save,
    paste(
      "alignment_server$materialize_coordinate_drafts(",
      "      notify = FALSE,",
      "      require_settled = TRUE",
      "    )",
      sep = "\n"
    ),
    fixed = TRUE
  )
})

test_that("project creation is available only with data", {
  runtime <- builder_project_test_runtime()
  empty <- runtime$builder_activity_capabilities(
    runtime$builder_activity_state()
  )
  populated <- runtime$builder_activity_capabilities(
    runtime$builder_activity_state(has_datasets = TRUE)
  )
  importing <- runtime$builder_activity_capabilities(
    runtime$builder_activity_state(client_imports = 1L, has_datasets = TRUE)
  )

  expect_false(empty$create_project)
  expect_true(populated$create_project)
  expect_true(importing$create_project)
})

test_that("a project folder can be chosen while another dataset is importing", {
  runtime <- builder_project_test_runtime()
  importing <- runtime$builder_activity_state(
    client_imports = 1L,
    server_imports = TRUE,
    project_phase = "none",
    has_project = FALSE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(importing)

  expect_true(capabilities$create_project)
  expect_true(capabilities$add_dataset)
  expect_true(capabilities$check_dataset)
  expect_true(capabilities$navigate_workflow)
  expect_false(capabilities$save_project)
  expect_false(capabilities$build)
})

test_that("project folders distinguish empty, existing, and unrelated content", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()

  expect_identical(runtime$builder_project_folder_state(root)$kind, "empty")

  writeLines("keep", file.path(root, ".keep"))
  unrelated <- runtime$builder_project_folder_state(root)
  expect_identical(unrelated$kind, "nonempty")
  expect_identical(unrelated$managed_conflicts, character())

  dir.create(file.path(root, "Sources"))
  reserved <- runtime$builder_project_folder_state(root)
  expect_identical(reserved$kind, "nonempty")
  expect_identical(reserved$managed_conflicts, "Sources")

  writeLines("{}", runtime$builder_project_manifest_path(root))
  expect_identical(runtime$builder_project_folder_state(root)$kind, "project")
})

test_that("non-empty project folders require explicit confirmation", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )

  html <- htmltools::renderTags(
    runtime$builder_project_nonempty_folder_dialog("/tmp/existing-files")
  )$html

  expect_match(html, "Folder already contains files", fixed = TRUE)
  expect_match(html, "Existing files will be kept", fixed = TRUE)
  expect_match(
    html,
    'id="choose_another_builder_project_folder"',
    fixed = TRUE
  )
  expect_match(html, 'id="confirm_builder_project_folder"', fixed = TRUE)
  expect_match(html, 'id="cancel_builder_project_folder"', fixed = TRUE)
})

test_that("existing Builder projects require update confirmation", {
  skip_if_not_installed("shiny")
  runtime <- builder_project_test_runtime()
  runtime$tags <- shiny::tags
  runtime$modalDialog <- shiny::modalDialog
  runtime$modalButton <- shiny::modalButton
  runtime$tagList <- shiny::tagList
  runtime$actionButton <- shiny::actionButton
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "ui", "project.R"),
    envir = runtime
  )

  html <- htmltools::renderTags(
    runtime$builder_project_existing_folder_dialog("/tmp/project")
  )$html

  expect_match(html, "Update existing project?", fixed = TRUE)
  expect_match(
    html,
    "Other files in the folder will not be changed",
    fixed = TRUE
  )
  expect_match(
    html,
    'id="confirm_existing_builder_project_folder"',
    fixed = TRUE
  )
  expect_match(
    html,
    'id="choose_another_builder_project_folder"',
    fixed = TRUE
  )
})

test_that("the top bar omits the format capability summary", {
  path <- builder_test_inst_path(
    "builder",
    "app.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl(
    'span(class = "formats", textOutput("format_line", inline = TRUE))',
    source,
    fixed = TRUE
  ))
})

test_that("spatial drafts are committed by workflow actions instead of blocking them", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "dirty",
    spatial_dirty = TRUE,
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$check_dataset)
  expect_true(capabilities$navigate_workflow)
  expect_null(capabilities$prepare_crbs)
  expect_true(capabilities$build)
  expect_true(capabilities$warn_before_unload)
})

test_that("project saving can commit spatial drafts through the internal mutation path", {
  imports_path <- builder_test_inst_path(
    "builder",
    "server",
    "imports.R"
  )
  enhancements_path <- builder_test_inst_path(
    "builder",
    "server",
    "enhancements.R"
  )
  project_path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  imports <- paste(readLines(imports_path, warn = FALSE), collapse = "\n")
  enhancements <- paste(
    readLines(enhancements_path, warn = FALSE),
    collapse = "\n"
  )
  project <- paste(readLines(project_path, warn = FALSE), collapse = "\n")

  expect_match(
    imports,
    "commit_enhance_images <- function(entry, images, internal = FALSE)",
    fixed = TRUE
  )
  expect_match(enhancements, "internal = TRUE", fixed = TRUE)
  expect_match(
    project,
    "materialize_coordinate_drafts(\n      notify = FALSE",
    fixed = TRUE
  )
})

test_that("project transitions lock writes while background restore permits browsing", {
  runtime <- builder_project_test_runtime()

  for (phase in c("choosing", "opening", "restoring")) {
    activity <- runtime$builder_activity_state(project_phase = phase)
    capabilities <- runtime$builder_activity_capabilities(activity)
    expect_identical(
      capabilities$page_inert,
      phase %in% c("choosing", "opening")
    )
    expect_false(capabilities$edit_dataset)
    expect_false(capabilities$save_project)
    expect_false(capabilities$build)
    expect_true(capabilities$warn_before_unload)
    expect_false(capabilities$open_project)
  }
})

test_that("Open remains available for switching an idle workspace", {
  runtime <- builder_project_test_runtime()

  for (phase in c("none", "clean", "dirty", "save_failed", "conflict")) {
    activity <- runtime$builder_activity_state(
      project_phase = phase,
      has_project = !identical(phase, "none"),
      has_datasets = TRUE
    )
    expect_true(
      runtime$builder_activity_capabilities(activity)$open_project,
      info = phase
    )
  }
})

test_that("Open confirms before replacing unsaved workspace state", {
  runtime <- builder_project_test_runtime()

  expect_false(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state()
  ))
  expect_false(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state(
      project_phase = "clean",
      has_project = TRUE,
      has_datasets = TRUE
    )
  ))
  expect_true(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state(has_datasets = TRUE)
  ))
  expect_true(runtime$builder_activity_requires_open_confirmation(
    runtime$builder_activity_state(
      project_phase = "clean",
      spatial_dirty = TRUE,
      has_project = TRUE,
      has_datasets = TRUE
    )
  ))
  for (phase in c("dirty", "save_failed", "conflict")) {
    expect_true(
      runtime$builder_activity_requires_open_confirmation(
        runtime$builder_activity_state(
          project_phase = phase,
          has_project = TRUE,
          has_datasets = TRUE
        )
      ),
      info = phase
    )
  }
})

test_that("unsaved workspaces warn before the browser unloads", {
  runtime <- builder_project_test_runtime()

  empty <- runtime$builder_activity_state()
  expect_false(runtime$builder_activity_capabilities(empty)$warn_before_unload)

  unsaved <- runtime$builder_activity_state(has_datasets = TRUE)
  expect_true(runtime$builder_activity_capabilities(unsaved)$warn_before_unload)

  saved <- runtime$builder_activity_state(
    project_phase = "clean",
    has_project = TRUE,
    has_datasets = TRUE
  )
  expect_false(runtime$builder_activity_capabilities(saved)$warn_before_unload)
})

test_that("pending project settings survive until source imports are dispatched", {
  runtime <- builder_project_test_runtime()
  pending <- list(ds1 = list(id = "ds1"))

  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      operation = "opening"
    ),
    character()
  )
  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      import_ids = "ds1",
      operation = "restoring"
    ),
    character()
  )
  expect_identical(
    runtime$builder_project_abandoned_entries(
      pending,
      operation = "restoring"
    ),
    "ds1"
  )
})

test_that("project open work resolves without timer polling", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    'builder_project_operation_phase("opening")',
    fixed = TRUE
  )
  expect_match(source, "builder_async_then(", fixed = TRUE)
  expect_match(source, "builder_project_receive_open", fixed = TRUE)
  expect_match(source, "runtime$builder_project_open_snapshot(", fixed = TRUE)
  expect_match(source, "opened$manifest_fingerprint", fixed = TRUE)
  expect_match(
    source,
    "builder_project_file_metadata(pending$path)",
    fixed = TRUE
  )
  expect_false(grepl(
    "builder_project_finish_open_preview",
    source,
    fixed = TRUE
  ))
  expect_false(grepl(
    "builder_project_receive_open_snapshot",
    source,
    fixed = TRUE
  ))
  expect_false(grepl(
    "builder_project_submit_open_snapshot",
    source,
    fixed = TRUE
  ))
  expect_false(grepl(
    "builder_project_schedule_open_poll",
    source,
    fixed = TRUE
  ))
  expect_match(source, "builder_project_restore_progress", fixed = TRUE)
  expect_match(source, 'mode = "restoring"', fixed = TRUE)
})

test_that("project restore stays in the opening overlay", {
  path <- builder_test_inst_path(
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    'builderActivityState.phase === "opening" || builderActivityState.phase === "restoring"',
    fixed = TRUE
  )
})

test_that("project open snapshots attach validated runtime status", {
  runtime <- builder_project_test_runtime()
  project_path <- file.path("project-root", "builder-project.json")
  runtime$builder_project_read_stable_manifest <- function(path) {
    list(
      manifest = list(datasets = list(list(id = "ds1"), list(id = "ds2"))),
      fingerprint = list(md5 = strrep("a", 32L))
    )
  }
  capability_calls <- 0L
  capabilities <- list(qs = TRUE)
  runtime$builder_format_capabilities <- function() {
    capability_calls <<- capability_calls + 1L
    capabilities
  }
  runtime$builder_project_dataset_status <- function(
    record,
    root,
    format_capabilities
  ) {
    expect_identical(root, dirname(project_path))
    expect_identical(format_capabilities, capabilities)
    list(message = if (identical(record$id, "ds1")) "first" else "second")
  }
  runtime$builder_project_status_snapshot_fresh <- function(...) {
    stop("preview-only statuses must not be revalidated while being attached")
  }

  opened <- runtime$builder_project_open_snapshot(project_path)

  expect_identical(opened$path, project_path)
  expect_identical(opened$root, dirname(project_path))
  expect_identical(
    opened$manifest$datasets[[1L]]$runtime_restore_status$message,
    "first"
  )
  expect_identical(
    opened$manifest$datasets[[2L]]$runtime_restore_status$message,
    "second"
  )
  expect_identical(capability_calls, 1L)
})

test_that("manifest preview fingerprint covers the manifest read window", {
  runtime <- builder_project_test_runtime()
  path <- withr::local_tempfile()
  writeBin(charToRaw("0123456789"), path)
  metadata <- list(
    list(
      bytes = 10,
      modified_at = "2026-09-29T00:00:00.000Z",
      changed_at = "1",
      stat_version = 1L,
      file_id = "1:1"
    ),
    list(
      bytes = 10,
      modified_at = "2026-09-29T00:00:00.000Z",
      changed_at = "2",
      stat_version = 1L,
      file_id = "1:1"
    )
  )
  next_metadata <- function(path) {
    value <- metadata[[1L]]
    metadata <<- metadata[-1L]
    value
  }

  expect_error(
    runtime$builder_project_read_stable_manifest(
      path,
      .read = function(path, payload) {
        stop("an unstable payload must not be parsed")
      },
      .metadata = next_metadata
    ),
    "changed while it was being read",
    fixed = TRUE
  )
})

test_that("stable manifest reading rejects a short payload", {
  runtime <- builder_project_test_runtime()
  path <- withr::local_tempfile()
  writeBin(charToRaw("0123456789"), path)
  metadata <- function(path) {
    list(
      bytes = 11,
      modified_at = "2026-09-29T00:00:00.000Z",
      changed_at = "1",
      stat_version = 1L,
      file_id = "1:1"
    )
  }

  expect_error(
    runtime$builder_project_read_stable_manifest(
      path,
      .read = function(path, payload) {
        stop("a short payload must not be parsed")
      },
      .metadata = metadata
    ),
    "changed while it was being read",
    fixed = TRUE
  )
})

test_that("stable manifest reading parses and reads content once", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  written <- runtime$builder_project_write(
    runtime$builder_project_new_manifest(root, "Single read"),
    root
  )
  parse_calls <- 0L
  metadata_calls <- 0L
  snapshot <- runtime$builder_project_read_stable_manifest(
    written$path,
    .read = function(path, payload) {
      parse_calls <<- parse_calls + 1L
      runtime$builder_project_read_manifest_payload(path, payload)
    },
    .metadata = function(path) {
      metadata_calls <<- metadata_calls + 1L
      runtime$builder_project_file_metadata(path)
    }
  )

  expect_identical(parse_calls, 1L)
  expect_identical(metadata_calls, 2L)
  expect_identical(
    snapshot$fingerprint$md5,
    unclass(as.character(digest::digest(
      readBin(
        written$path,
        what = "raw",
        n = file.info(written$path)$size[[1L]]
      ),
      algo = "md5",
      serialize = FALSE
    )))
  )
})

test_that("ready CRB restore selection is prepared transactionally in manifest order", {
  runtime <- builder_project_test_runtime()
  freshness_checks <- 0L
  runtime$builder_project_status_snapshot_fresh <- function(...) {
    freshness_checks <<- freshness_checks + 1L
    TRUE
  }
  runtime$builder_project_restore_entry <- function(
    record,
    root,
    entry,
    observed = NULL
  ) {
    list(id = record$id, load_state = "reload_required")
  }
  runtime$builder_project_artifact_entry <- function(
    entry,
    artifact,
    root,
    observed = NULL
  ) {
    entry$load_state <- "artifact_ready"
    entry$project_artifact <- artifact
    entry
  }
  runtime$builder_project_restored_check_identity <- function(record, entry) {
    paste0("mark-", entry$id)
  }
  manifest <- list(
    datasets = list(
      list(
        id = "ds2",
        artifact = list(path = "artifacts/ds2.crb"),
        runtime_restore_status = list(
          actions = c("reuse", "resume", "skip"),
          configuration_entry = list(id = "ds2")
        )
      ),
      list(
        id = "ds1",
        artifact = list(path = "artifacts/ds1.crb"),
        runtime_restore_status = list(
          actions = c("reuse", "skip"),
          configuration_entry = list(id = "ds1")
        )
      )
    )
  )
  original_manifest <- manifest

  prepared <- runtime$builder_project_prepare_open_selection(
    manifest,
    "project-root",
    list(ds2 = "reuse", ds1 = "reuse")
  )

  expect_identical(
    vapply(prepared$reusable_entries, `[[`, character(1), "id"),
    c("ds2", "ds1")
  )
  expect_identical(names(prepared$artifacts), c("ds2", "ds1"))
  expect_identical(
    prepared$marks,
    c(ds2 = "mark-ds2", ds1 = "mark-ds1")
  )
  expect_identical(prepared$pending_entries, list())
  expect_identical(prepared$skipped_ids, character())
  expect_identical(freshness_checks, 2L)
  expect_identical(manifest, original_manifest)

  skipped <- runtime$builder_project_prepare_open_selection(
    manifest,
    "project-root",
    list(ds2 = "skip", ds1 = "skip")
  )
  expect_identical(skipped$reusable_entries, list())
  expect_identical(skipped$skipped_ids, c("ds2", "ds1"))
  expect_identical(freshness_checks, 2L)
  expect_identical(manifest, original_manifest)

  restoring <- runtime$builder_project_prepare_open_selection(
    manifest,
    "project-root",
    list(ds2 = "resume", ds1 = "skip")
  )
  expect_identical(
    restoring$pending_entries$ds2$runtime_restore_action,
    "resume"
  )
  expect_identical(manifest, original_manifest)
})

test_that("ready CRB confirmation validates the complete state before committing", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  start <- regexpr(
    "complete_builder_project_open <- function(actions = NULL) {",
    source,
    fixed = TRUE
  )[[1L]]
  finish <- regexpr(
    "\nobserveEvent(input$confirm_builder_project_open, {",
    source,
    fixed = TRUE
  )[[1L]]
  observer <- substr(source, start, finish - 1L)

  prepare_position <- regexpr(
    "prepared <- tryCatch(",
    observer,
    fixed = TRUE
  )[[1L]]
  manifest_check_position <- regexpr(
    "current_manifest <- builder_project_file_metadata(pending$path)",
    observer,
    fixed = TRUE
  )[[1L]]
  state_position <- regexpr(
    "next_store <- tryCatch(",
    observer,
    fixed = TRUE
  )[[1L]]
  commit_position <- regexpr("store(next_store)", observer, fixed = TRUE)[[1L]]

  expect_gt(manifest_check_position, 0L)
  expect_gt(prepare_position, manifest_check_position)
  expect_gt(state_position, prepare_position)
  expect_gt(commit_position, state_position)
  expect_false(grepl("order(vapply(", observer, fixed = TRUE))
  expect_match(
    observer,
    "The dataset caches could not be attached to this session",
    fixed = TRUE
  )
})

test_that("a revision conflict permits reopening but rejects mutation", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "conflict",
    has_project = TRUE,
    has_datasets = TRUE
  )
  capabilities <- runtime$builder_activity_capabilities(activity)

  expect_true(capabilities$open_project)
  expect_false(capabilities$edit_dataset)
  expect_false(capabilities$save_project)
  expect_match(
    runtime$builder_activity_reason(activity, "save_project"),
    "Reopen the project",
    fixed = TRUE
  )
})

test_that("an existing project can persist removal of its final dataset", {
  runtime <- builder_project_test_runtime()
  activity <- runtime$builder_activity_state(
    project_phase = "dirty",
    has_project = TRUE,
    has_datasets = FALSE
  )

  expect_true(runtime$builder_activity_capabilities(activity)$save_project)
})

test_that("project dirty state depends on checked settings, not CRB readiness", {
  runtime <- builder_project_test_runtime()
  first <- list(
    id = "ds1",
    settings = list(name = "First"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  second <- list(
    id = "ds2",
    settings = list(name = "Second"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  manifest <- list(
    datasets = list(
      list(
        id = "ds1",
        order = 1L,
        configuration = list(
          digest = runtime$builder_project_configuration_digest(first),
          checked = FALSE
        )
      ),
      list(
        id = "ds2",
        order = 2L,
        configuration = list(
          digest = runtime$builder_project_configuration_digest(second),
          checked = TRUE
        )
      )
    )
  )

  expect_false(runtime$builder_project_live_dirty(
    list(first, second),
    "ds2",
    manifest
  ))
  pending_manifest <- list(
    datasets = list(list(
      id = "ds1",
      order = 1L,
      configuration = list(
        digest = runtime$builder_project_configuration_digest(first),
        reviewed = TRUE,
        reviewed_digest = runtime$builder_project_configuration_digest(first),
        checked = FALSE
      )
    ))
  )
  expect_false(runtime$builder_project_live_dirty(
    list(first),
    "ds1",
    pending_manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(first, second),
    character(),
    manifest
  ))
  changed <- first
  changed$settings$name <- "Renamed"
  expect_true(runtime$builder_project_live_dirty(
    list(changed, second),
    "ds2",
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(second, first),
    "ds2",
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(first),
    character(),
    manifest
  ))
  expect_true(runtime$builder_project_live_dirty(
    list(),
    character(),
    manifest
  ))
  expect_false(runtime$builder_project_live_dirty(
    list(),
    character(),
    list(datasets = list())
  ))
  expect_false(runtime$builder_project_live_dirty(
    list(),
    character(),
    manifest,
    ignored_ids = c("ds1", "ds2")
  ))
  inactive_manifest <- manifest
  inactive_manifest$datasets <- lapply(
    inactive_manifest$datasets,
    function(record) {
      record$release <- list(included = FALSE)
      record
    }
  )
  expect_false(runtime$builder_project_live_dirty(
    list(),
    character(),
    inactive_manifest
  ))
})

test_that("a changed project revision marks dirty without hashing settings", {
  runtime <- builder_project_test_runtime()
  runtime$builder_project_cached_configuration_digest <- function(...) {
    stop("changed revisions must not hash configuration", call. = FALSE)
  }
  entry <- list(
    id = "ds1",
    revision = 4L,
    settings = list(name = "Renamed")
  )
  manifest <- list(
    datasets = list(list(
      id = "ds1",
      order = 1L,
      configuration = list(
        revision = 3L,
        digest = "saved-digest",
        checked = FALSE
      )
    ))
  )

  expect_true(runtime$builder_project_live_dirty(
    list(entry),
    character(),
    manifest
  ))
})

test_that("checked identities survive only while removal remains undoable", {
  runtime <- builder_project_test_runtime()
  marks <- c(ds1 = "mark-one", ds2 = "mark-two", stale = "mark-stale")
  removed <- list(id = "ds2", entry = list(id = "ds2"))

  expect_identical(
    runtime$builder_project_retain_check_marks(
      marks,
      live_ids = "ds1",
      last_removed = removed,
      can_undo_remove = TRUE
    ),
    c(ds1 = "mark-one", ds2 = "mark-two")
  )
  expect_identical(
    runtime$builder_project_retain_check_marks(
      marks,
      live_ids = "ds1",
      last_removed = removed,
      can_undo_remove = FALSE
    ),
    c(ds1 = "mark-one")
  )
})

test_that("checked project records bind confirmation to the restored entry", {
  runtime <- builder_project_test_runtime()
  restored <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/new-snapshot",
      owner_token = "new-owner",
      object_md5 = "new-md5"
    ),
    settings = list(
      name = "Spatial dataset",
      spatial_coordinate_transforms = list(
        section_a_1_fov_1 = list(rotation_degrees = 66.9, scale = 1)
      )
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  mark <- runtime$builder_project_check_identity(restored)
  expect_identical(mark, runtime$builder_project_check_identity(restored))

  changed <- restored
  changed$settings$spatial_coordinate_transforms[[
    "section_a_1_fov_1"
  ]]$rotation_degrees <- 70
  expect_false(identical(
    mark,
    runtime$builder_project_check_identity(changed)
  ))
})

test_that("runtime snapshot replacement does not invalidate checked configuration", {
  runtime <- builder_project_test_runtime()
  first <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/first",
      owner_token = "first-owner",
      object_md5 = "first-md5"
    ),
    settings = list(name = "Dataset", default_group = "cell_type")
  )
  replacement <- first
  replacement$snapshot <- list(
    path = "/session/replacement",
    owner_token = "replacement-owner",
    object_md5 = "replacement-md5"
  )

  expect_identical(
    runtime$builder_project_check_identity(first),
    runtime$builder_project_check_identity(replacement)
  )

  replacement$settings$default_group <- "cluster"
  expect_false(identical(
    runtime$builder_project_check_identity(first),
    runtime$builder_project_check_identity(replacement)
  ))
})

test_that("live checked identity uses the monotonic dataset revision", {
  runtime <- builder_project_test_runtime()
  runtime$builder_project_cached_configuration_digest <- function(...) {
    stop("live identity must not digest configuration", call. = FALSE)
  }
  entry <- list(
    id = "ds1",
    revision = 7L,
    settings = list(name = "Dataset", included_projections = "umap")
  )

  expect_identical(
    runtime$builder_project_check_identity(entry),
    "configuration:ds1:revision:7"
  )
  changed <- entry
  changed$settings$included_projections <- c("umap", "pca")
  changed$revision <- 8L
  expect_identical(
    runtime$builder_project_check_identity(changed),
    "configuration:ds1:revision:8"
  )
})

test_that("pending coordinate drafts participate in checked identity", {
  runtime <- builder_project_test_runtime()
  entry <- list(
    id = "ds1",
    snapshot = list(
      path = "/session/ds1",
      owner_token = "owner-ds1",
      object_md5 = strrep("a", 32L)
    ),
    settings = list(
      name = "Spatial dataset",
      images = list(),
      spatial_coordinate_transforms = list()
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  confirmed <- runtime$builder_project_effective_check_identity(entry, list())
  snapshot_identity <- runtime$.builder_worker_identity(entry$snapshot)
  draft <- function(rotation) {
    list(
      ds1 = list(
        `fov-a` = list(
          dataset = "ds1",
          section = "fov-a",
          snapshot_identity = snapshot_identity,
          spec = list(
            schema_version = 1L,
            rotation_degrees = rotation,
            scale = 1
          )
        )
      )
    )
  }

  expect_false(identical(
    confirmed,
    runtime$builder_project_effective_check_identity(entry, draft(37.5))
  ))
  expect_identical(
    runtime$builder_project_effective_check_identity(entry, draft(0)),
    confirmed
  )
  marks <- stats::setNames(confirmed, entry$id)
  expect_identical(
    runtime$builder_project_checked_ids(list(entry), marks, draft(37.5)),
    character()
  )
  expect_identical(
    runtime$builder_project_checked_ids(list(entry), marks, draft(0)),
    entry$id
  )
})

test_that("all pending spatial draft families invalidate dataset checks", {
  runtime <- builder_project_test_runtime()

  expect_identical(
    runtime$builder_project_pending_draft_ids(list(
      coordinates = list(ds1 = list(`fov-a` = list())),
      roi_coordinates = list(ds2 = list(`fov-b` = list())),
      roi_appearance = list(
        ds1 = list(`fov-a` = list()),
        ds3 = list(`fov-c` = list())
      )
    )),
    c("ds1", "ds2", "ds3")
  )
  expect_identical(
    runtime$builder_project_pending_draft_ids(list()),
    character()
  )
  expect_identical(
    runtime$builder_project_pending_draft_ids(list(coordinates = NULL)),
    character()
  )
})

test_that("reusable CRB preparation skips current artifacts", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset.crb")
  writeLines("ready", artifact_path)
  entry <- list(
    id = "ds1",
    revision = 4L,
    snapshot = list(source_fingerprint = "source-a"),
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 4L,
    built_from_source_fingerprint = "source-a",
    built_from_configuration = runtime$builder_project_configuration_digest(
      entry
    )
  )

  expect_length(
    runtime$builder_project_entries_requiring_crb(
      list(entry),
      list(ds1 = artifact),
      root
    ),
    0L
  )
  entry$settings$name <- "Changed"
  expect_identical(
    vapply(
      runtime$builder_project_entries_requiring_crb(
        list(entry),
        list(ds1 = artifact),
        root
      ),
      `[[`,
      character(1),
      "id"
    ),
    "ds1"
  )
})

test_that("checkpoint status reuses verification but rejects changed members", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  paths <- file.path(root, c("dataset.crb", "matrix.bin"))
  lapply(paths, function(path) writeLines("original", path))
  fingerprints <- lapply(
    paths,
    runtime$builder_project_file_fingerprint,
    content = TRUE
  )
  # A copied checkpoint has a different file identity from its saved manifest.
  fingerprints <- lapply(fingerprints, function(value) {
    value$file_id <- "previous-host"
    value
  })
  artifact <- list(
    status = "ready",
    path = basename(paths[1]),
    fingerprint = fingerprints[[1]],
    members = list(list(
      path = basename(paths[2]),
      fingerprint = fingerprints[[2]]
    )),
    plan_item = list(artifact_identity = list(spatial_sections = character()))
  )
  cache <- new.env(parent = emptyenv())
  original <- runtime$builder_project_file_fingerprint
  reads <- 0L
  runtime$builder_project_file_fingerprint <- function(
    path,
    content = FALSE,
    ...
  ) {
    if (isTRUE(content)) {
      reads <<- reads + 1L
    }
    original(path, content = content, ...)
  }
  available <- function() {
    runtime$builder_project_artifact_available(
      artifact,
      root,
      verification_cache = cache
    )
  }
  expect_true(available())
  expect_identical(reads, 2L)
  expect_true(available())
  expect_identical(reads, 2L)
  writeLines("changed member contents", paths[2])
  expect_false(available())
  expect_identical(reads, 3L)
  unlink(paths[2])
  expect_false(available())
})

test_that("checked settings and CRB availability are independent", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset.crb")
  writeLines("ready", artifact_path)
  entry <- list(
    id = "ds1",
    revision = 4L,
    snapshot = list(source_fingerprint = "source-a"),
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_source_fingerprint = "source-a",
    built_from_configuration = runtime$builder_project_artifact_configuration_digest(
      entry
    )
  )
  cases <- list(
    unchecked = list(checked = FALSE, artifact = NULL, job = NULL),
    no_crb = list(checked = TRUE, artifact = NULL, job = NULL),
    queued = list(
      checked = TRUE,
      artifact = NULL,
      job = list(status = "queued")
    ),
    generating = list(
      checked = TRUE,
      artifact = NULL,
      job = list(status = "generating")
    ),
    registering = list(
      checked = TRUE,
      artifact = NULL,
      job = list(status = "registering")
    ),
    failed = list(
      checked = TRUE,
      artifact = NULL,
      job = list(status = "failed", error = "export failed")
    ),
    ready = list(checked = TRUE, artifact = artifact, job = NULL),
    stale = list(
      checked = TRUE,
      artifact = utils::modifyList(
        artifact,
        list(built_from_configuration = strrep("a", 32L))
      ),
      job = NULL
    )
  )
  states <- lapply(cases, function(case) {
    runtime$builder_project_checkpoint_state(
      entry,
      checked = case$checked,
      artifact = case$artifact,
      root = root,
      job = case$job
    )
  })

  expect_identical(
    unname(vapply(states, `[[`, character(1), "status")),
    c(
      "unchecked",
      "no_crb",
      "generating",
      "generating",
      "generating",
      "no_crb",
      "ready",
      "no_crb"
    )
  )
  expect_identical(
    names(Filter(function(state) isTRUE(state$checked), states)),
    setdiff(names(cases), "unchecked")
  )
  expect_identical(
    names(Filter(function(state) isTRUE(state$crb_ready), states)),
    "ready"
  )

  without_crb <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    checked = TRUE,
    root = root
  )
  with_crb <- runtime$builder_project_dataset_record(
    entry,
    source = list(kind = "missing", path = NULL),
    checked = TRUE,
    artifact = artifact,
    root = root
  )
  expect_true(without_crb$configuration$checked)
  expect_true(without_crb$configuration$reviewed)
  expect_true(with_crb$configuration$checked)
  expect_true(with_crb$configuration$reviewed)
  without_crb$runtime_restore_action <- "resume"
  expect_identical(
    runtime$builder_project_restored_check_identity(without_crb, entry),
    runtime$builder_project_check_identity(entry)
  )

  compatible <- entry
  compatible$settings$assay <- "RNA"
  compatible$settings$layer <- "data"
  compatible$profile$assay_profiles <- list(
    RNA = list(layers = c("counts", "data"))
  )
  compatible_record <- runtime$builder_project_dataset_record(
    compatible,
    source = list(kind = "missing", path = NULL),
    checked = TRUE,
    root = root
  )
  compatible_record$runtime_restore_action <- "resume"
  expect_identical(
    runtime$builder_project_restored_check_identity(
      compatible_record,
      compatible
    ),
    runtime$builder_project_check_identity(compatible)
  )
  incompatible <- compatible
  incompatible$settings$layer <- "scale.data"
  expect_null(runtime$builder_project_restored_check_identity(
    compatible_record,
    incompatible
  ))
})

test_that("dataset rail keeps checked state independent from CRB lifecycle", {
  runtime <- builder_project_test_runtime()
  runtime$builder_source_utf8(
    builder_test_inst_path(
      "builder",
      "ui",
      "dataset_rail.R"
    ),
    envir = runtime
  )
  status <- function(checkpoint_status, checked = FALSE) {
    runtime$builder_dataset_rail_status(list(
      checkpoint_status = checkpoint_status,
      readiness_label = "Ready",
      checked = checked
    ))
  }

  expect_identical(
    status("generating", checked = TRUE),
    list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Generating cache…",
      crb_status = "generating"
    )
  )
  expect_identical(
    status("no_crb", checked = TRUE),
    list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Not cached",
      crb_status = "no-crb"
    )
  )
  expect_identical(
    status("ready", checked = TRUE),
    list(
      label = "Checked",
      ready = TRUE,
      crb_label = "Cached · opens fast",
      crb_status = "ready"
    )
  )
})

test_that("reusable CRB checks share the revision-bound artifact digest", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset.crb")
  writeLines("ready", artifact_path)
  entry <- list(
    id = "ds1",
    revision = 4L,
    snapshot = list(source_fingerprint = "source-a"),
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  digest <- runtime$builder_project_artifact_configuration_digest(entry)
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_source_fingerprint = "source-a",
    built_from_configuration = digest
  )
  original_digest <- runtime$builder_project_artifact_configuration_digest
  calls <- 0L
  runtime$builder_project_artifact_configuration_digest <- function(entry) {
    calls <<- calls + 1L
    original_digest(entry)
  }
  cache <- new.env(parent = emptyenv())
  required <- function(entry) {
    runtime$builder_project_entries_requiring_crb(
      list(entry),
      list(ds1 = artifact),
      root,
      identity_cache = cache
    )
  }

  expect_length(required(entry), 0L)
  expect_length(required(entry), 0L)
  expect_identical(calls, 1L)
  entry$revision <- 5L
  expect_length(required(entry), 0L)
  expect_identical(calls, 2L)
})

test_that("CRB inputs and source identity still invalidate artifacts", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset.crb")
  writeLines("ready", artifact_path)
  source_a <- paste(
    "builder-snapshot-v2",
    "source.rds",
    "10",
    "1",
    strrep("a", 32L),
    sep = ":"
  )
  entry <- list(
    id = "ds1",
    revision = 4L,
    snapshot = list(source_fingerprint = source_a),
    settings = list(
      name = "Dataset",
      layer = "data",
      reductions = c("umap", "pca"),
      default_projection = "umap",
      spatial_coordinate_transforms = list(
        fov = list(rotation_degrees = 0, scale = 1)
      ),
      images = list(
        fov = list(
          histology = list(
            source_content_md5 = strrep("b", 32L),
            scale = 1,
            dx = 0,
            dy = 0
          )
        )
      )
    ),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 4L,
    built_from_source_fingerprint = source_a,
    built_from_configuration = runtime$builder_project_artifact_configuration_digest(
      entry
    )
  )
  viewer_changed <- entry
  viewer_changed$revision <- 5L
  viewer_changed$settings$spatial_point_appearance <- list(
    fov = list(point_opacity = 0.6, point_size = 7)
  )
  viewer_changed$settings$images$fov$histology$point_opacity <- 0.6
  viewer_changed$settings$images$fov$histology$point_size <- 7
  expect_length(
    runtime$builder_project_entries_requiring_crb(
      list(viewer_changed),
      list(ds1 = artifact),
      root
    ),
    0L
  )

  changes <- list(
    layer = function(value) {
      value$settings$layer <- "counts"
      value
    },
    default_projection = function(value) {
      value$settings$default_projection <- "pca"
      value
    },
    coordinate_transform = function(value) {
      value$settings$spatial_coordinate_transforms$fov$rotation_degrees <- 90
      value
    },
    image_alignment = function(value) {
      value$settings$images$fov$histology$dx <- 12
      value
    },
    image_source_md5 = function(value) {
      value$settings$images$fov$histology$source_content_md5 <- strrep("c", 32L)
      value
    },
    source_identity = function(value) {
      value$snapshot$source_fingerprint <- sub(
        paste0(strrep("a", 32L), "$"),
        strrep("d", 32L),
        value$snapshot$source_fingerprint
      )
      value
    }
  )

  for (name in names(changes)) {
    changed <- changes[[name]](entry)
    changed$revision <- 5L
    required <- runtime$builder_project_entries_requiring_crb(
      list(changed),
      list(ds1 = artifact),
      root
    )
    expect_identical(
      vapply(required, `[[`, character(1), "id"),
      "ds1",
      info = name
    )
  }
})

test_that("current project CRBs replace only temporary Build entries", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  artifact_path <- file.path(root, "dataset.crb")
  writeLines("ready", artifact_path)
  entry <- list(
    id = "ds1",
    revision = 4L,
    load_state = "loaded",
    snapshot = list(source_fingerprint = "source-a"),
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = basename(artifact_path),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 4L,
    built_from_source_fingerprint = "source-a",
    built_from_configuration = runtime$builder_project_configuration_digest(
      entry
    )
  )

  build_entries <- runtime$builder_project_entries_for_build(
    list(entry),
    list(ds1 = artifact),
    root
  )

  expect_identical(entry$load_state, "loaded")
  expect_identical(build_entries[[1L]]$load_state, "artifact_ready")
  expect_identical(
    build_entries[[1L]]$project_artifact$resolved_path,
    normalizePath(artifact_path, winslash = "/", mustWork = TRUE)
  )

  changed <- entry
  changed$settings$name <- "Changed"
  expect_identical(
    runtime$builder_project_entries_for_build(
      list(changed),
      list(ds1 = artifact),
      root
    ),
    list(changed)
  )

  changed <- entry
  changed$revision <- 5L
  changed$snapshot$source_fingerprint <- "source-b"
  expect_identical(
    runtime$builder_project_entries_for_build(
      list(changed),
      list(ds1 = artifact),
      root
    ),
    list(changed)
  )

  unlink(artifact_path)
  expect_identical(
    runtime$builder_project_entries_for_build(
      list(entry),
      list(ds1 = artifact),
      root
    ),
    list(entry)
  )
})

test_that("re-registered artifacts retain their source identity", {
  runtime <- builder_project_test_runtime()
  previous <- list(built_from_source_fingerprint = "source-a")
  reused <- list(reused_artifact = list(path = "dataset.crb"))

  expect_identical(
    runtime$builder_project_artifact_source_fingerprint(
      list(snapshot = NULL),
      reused,
      previous
    ),
    "source-a"
  )
  expect_identical(
    runtime$builder_project_artifact_source_fingerprint(
      list(snapshot = list(source_fingerprint = "source-b")),
      reused,
      previous
    ),
    "source-b"
  )
  expect_null(runtime$builder_project_artifact_source_fingerprint(
    list(snapshot = NULL),
    list(),
    previous
  ))
})

test_that("reopened projects reject artifacts from an older source", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source_path <- file.path(root, "sources", "ds1", "source.rds")
  artifact_path <- file.path(root, "artifacts", "ds1", "dataset.crb")
  dir.create(dirname(source_path), recursive = TRUE)
  dir.create(dirname(artifact_path), recursive = TRUE)
  writeBin(charToRaw("new-source"), source_path)
  writeBin(charToRaw("old-artifact"), artifact_path)
  source_fingerprint <- runtime$builder_project_file_fingerprint(
    source_path,
    content = TRUE
  )
  entry <- list(
    id = "ds1",
    revision = 5L,
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = runtime$builder_project_relative_path(artifact_path, root),
    fingerprint = runtime$builder_project_file_fingerprint(
      artifact_path,
      content = TRUE
    ),
    members = list(),
    built_from_revision = 4L,
    built_from_source_fingerprint = "source-a",
    built_from_configuration = runtime$builder_project_configuration_digest(
      entry
    )
  )
  record <- runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "managed",
      path = runtime$builder_project_relative_path(source_path, root),
      fingerprint = source_fingerprint
    ),
    artifact = artifact,
    checked = TRUE,
    root = root
  )

  status <- runtime$builder_project_dataset_status(record, root)
  expect_false("reuse" %in% status$actions)
  record$runtime_restore_status <- status
  expect_error(
    runtime$builder_project_prepare_open_selection(
      list(datasets = list(record)),
      root,
      list(ds1 = "reuse")
    ),
    "no longer valid"
  )

  record$artifact$built_from_revision <- 5L
  record$artifact$built_from_source_fingerprint <- paste(
    "builder-snapshot-v2",
    "source.rds",
    source_fingerprint$bytes,
    source_fingerprint$modified_at,
    source_fingerprint$md5,
    sep = ":"
  )
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    root
  )
  expect_true("reuse" %in% record$runtime_restore_status$actions)
  reopened <- runtime$builder_project_prepare_open_selection(
    list(datasets = list(record)),
    root,
    list(ds1 = "reuse")
  )
  expect_length(reopened$reusable_entries, 1L)
})

test_that("legacy reviewed settings migrate to the checked flag", {
  runtime <- builder_project_test_runtime()
  record <- list(
    configuration = list(
      checked = FALSE,
      digest = "same-configuration"
    ),
    artifact = list(
      status = "ready",
      built_from_configuration = "same-configuration"
    )
  )

  expect_false(runtime$builder_project_record_configuration_confirmed(record))

  record$configuration$reviewed <- TRUE
  record$configuration$reviewed_digest <- "same-configuration"
  expect_true(runtime$builder_project_record_configuration_confirmed(record))

  record$configuration$checked <- TRUE
  record$configuration$checked_digest <- "same-configuration"
  expect_true(runtime$builder_project_record_configuration_confirmed(record))

  record$configuration$digest <- "newer-configuration"
  expect_false(runtime$builder_project_record_configuration_confirmed(record))
})

test_that("browser lifecycle code includes queue sync and close protection", {
  path <- builder_test_inst_path(
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(source, "builder_client_import_state", fixed = TRUE)
  expect_match(source, "builder_activity_state", fixed = TRUE)
  expect_match(source, "beforeunload", fixed = TRUE)
  expect_match(source, "builder-operation-overlay", fixed = TRUE)
  expect_match(
    source,
    'complete_dataset_check: activityCapability("check_dataset")',
    fixed = TRUE
  )
  expect_match(
    source,
    'continue_to_review: activityCapability("navigate_workflow")',
    fixed = TRUE
  )
})

## The background expression a Project cache job evaluates, taken from
## project.R without sourcing the rest of the file.
builder_project_crb_job_expression_source <- function() {
  exprs <- parse(
    builder_test_inst_path("builder", "project.R"),
    keep.source = FALSE
  )
  for (expr in exprs) {
    if (
      is.call(expr) &&
        identical(expr[[1L]], as.name("<-")) &&
        identical(expr[[2L]], as.name("builder_project_crb_job_expression"))
    ) {
      return(eval(expr[[3L]], new.env(parent = baseenv()))())
    }
  }
  stop("builder_project_crb_job_expression() is missing from project.R.")
}

test_that("Save starts optional CRB work only after checked settings persist", {
  project <- paste(
    readLines(
      builder_test_inst_path(
        "builder",
        "server",
        "project.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  review <- paste(
    readLines(
      builder_test_inst_path(
        "builder",
        "server",
        "review.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  browser <- paste(
    readLines(
      builder_test_inst_path(
        "builder",
        "www",
        "builder.js"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_false(grepl("queue_builder_project_crbs(id)", review, fixed = TRUE))
  expect_false(grepl("builder_project_crb_save_barrier", project, fixed = TRUE))
  expect_false(grepl(
    "resume_builder_project_save_if_ready",
    project,
    fixed = TRUE
  ))
  expect_match(
    project,
    "queue_builder_project_crbs(isolate(checked_dataset_ids()))",
    fixed = TRUE
  )
  expect_match(
    project,
    paste(
      "finish_builder_project_save_request(",
      "        failed == 0L,",
      "        save_waiter$after,",
      "        only_when_idle = TRUE",
      "      )",
      sep = "
"
    ),
    fixed = TRUE
  )
  expect_false(grepl("Project save incomplete", browser, fixed = TRUE))
  expect_false(grepl(
    "startBuilderProjectCrbRequest(remaining);",
    browser,
    fixed = TRUE
  ))
  worker <- substr(
    project,
    regexpr(
      "start_next_builder_project_crb_job <- function()",
      project,
      fixed = TRUE
    )[[1L]],
    regexpr(
      "queue_builder_project_crbs <- function",
      project,
      fixed = TRUE
    )[[1L]] -
      1L
  )
  expect_match(
    worker,
    "builder_project_registration_runtime_files()",
    fixed = TRUE
  )
  expect_match(worker, "builder_async_submit(", fixed = TRUE)
  expect_match(worker, "builder_project_crb_job_expression()", fixed = TRUE)
  expect_match(
    worker,
    "load_package = builder_project_runtime_needs_package()",
    fixed = TRUE
  )
  job <- paste(
    deparse(builder_project_crb_job_expression_source()),
    collapse = "
"
  )
  expect_match(job, "promote = TRUE", fixed = TRUE)
  expect_match(
    project,
    "builder_project_manifest_with_artifact(",
    fixed = TRUE
  )
  expect_match(
    project,
    "builder_project_artifact_matches_entry(",
    fixed = TRUE
  )
  expect_match(
    project,
    "builder_project_record_crb(",
    fixed = TRUE
  )
  expect_match(
    project,
    'source_status %in% c("syncing", "cancelling")',
    fixed = TRUE
  )
  expect_match(project, 'job$status <- "registering"', fixed = TRUE)
  expect_match(project, "builder_async_cancel(crb_task)", fixed = TRUE)
  record_crb <- substr(
    project,
    regexpr(
      "builder_project_record_crb <- function",
      project,
      fixed = TRUE
    )[[1L]],
    regexpr(
      "builder_project_finish_crb_job <- function",
      project,
      fixed = TRUE
    )[[1L]] -
      1L
  )
  expect_match(
    record_crb,
    "builder_project_manifest_with_artifact(",
    fixed = TRUE
  )
  expect_false(grepl("save_builder_project_state(", record_crb, fixed = TRUE))
  expect_false(grepl(
    "builder_release_execution_runtime()",
    worker,
    fixed = TRUE
  ))
})

test_that("connection flush sends a message captured in reactive context", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "activity_message <- builder_activity_message(",
    fixed = TRUE
  )
  expect_match(
    source,
    "build_overlay = isolate(builder_build_overlay())",
    fixed = TRUE
  )
  expect_match(
    source,
    paste0(
      'function\\(\\)\\s*\\{\\s*session\\$sendCustomMessage\\(\\s*',
      '"builder_activity_state",\\s*activity_message'
    ),
    perl = TRUE
  )
})

test_that("only the saved-project background CRB queue remains", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl("prepare_builder_project_crbs", source, fixed = TRUE))
  expect_false(grepl("builder_project_crb_progress", source, fixed = TRUE))
  expect_false(grepl(
    "builder_project_build_crb_request_id",
    source,
    fixed = TRUE
  ))
  expect_match(source, "queue_builder_project_crbs <- function(", fixed = TRUE)
  expect_match(
    source,
    "builder_project_record_crb(",
    fixed = TRUE
  )
  expect_match(
    paste(
      deparse(builder_project_crb_job_expression_source()),
      collapse = "
"
    ),
    "promote = TRUE",
    fixed = TRUE
  )

  capture_start <- regexpr(
    "builder_project_capture_build_plan <- function(",
    source,
    fixed = TRUE
  )[[1L]]
  capture_end <- regexpr(
    "builder_project_source_runtime_file <- function()",
    source,
    fixed = TRUE
  )[[1L]]
  capture <- substr(source, capture_start, capture_end - 1L)
  expect_false(grepl("serialize(", capture, fixed = TRUE))
  expect_match(capture, "builder_project_build_plan(plan)", fixed = TRUE)
  expect_match(source, "promote = FALSE", fixed = TRUE)
})

test_that("deferred project saves keep the Shiny session domain", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "builder_project_schedule_source_sync_poll <- function(delay = 0.2)",
    fixed = TRUE
  )
  expect_match(
    source,
    "shiny::withReactiveDomain(session, {",
    fixed = TRUE
  )
})

test_that("post-build Project registration has one manifest commit path", {
  source <- paste(
    readLines(
      builder_test_inst_path(
        "builder",
        "server",
        "project.R"
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  start <- regexpr("\nobserve({\n  value <- result()", source, fixed = TRUE)[[
    1L
  ]]
  expect_gt(start, 0L)
  registration <- substr(source, start, nchar(source))
  expect_identical(
    lengths(regmatches(
      registration,
      gregexpr("save_builder_project_state(", registration, fixed = TRUE)
    )),
    1L
  )
  expect_match(registration, "artifacts_changed <-", fixed = TRUE)
  expect_match(
    registration,
    "builder_project_artifacts(previous_artifacts)",
    fixed = TRUE
  )
  expect_false(grepl(
    "builder_project_crb_progress",
    registration,
    fixed = TRUE
  ))
  expect_false(grepl(
    "prepare_builder_project_crbs",
    registration,
    fixed = TRUE
  ))
})

test_that("activity locks do not rewrite unchanged text during DOM enhancement", {
  path <- builder_test_inst_path(
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "title.textContent !== builderActivityState.busy_title",
    fixed = TRUE
  )
  expect_match(
    source,
    "message.textContent !== builderActivityState.busy_message",
    fixed = TRUE
  )
})

test_that("activity locks keep the loading workspace visible and cancellable", {
  path <- builder_test_inst_path(
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_false(grepl(
    "workspace.inert = workspaceLocked",
    source,
    fixed = TRUE
  ))
  expect_match(
    source,
    "#builder-workspace input, #builder-workspace select, ",
    fixed = TRUE
  )
  expect_match(
    source,
    ".builder-retry-import, .builder-remove-import",
    fixed = TRUE
  )
})

test_that("dynamic content enhancement is coalesced per animation frame", {
  path <- builder_test_inst_path(
    "builder",
    "www",
    "builder.js"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(
    source,
    "function matchingDynamicElements(roots, selector)",
    fixed = TRUE
  )
  expect_match(
    source,
    "function matchingDynamicContainers(roots, selector)",
    fixed = TRUE
  )
  expect_match(source, "root.closest(selector)", fixed = TRUE)
  expect_match(
    source,
    "function scheduleDynamicContentEnhancement(root)",
    fixed = TRUE
  )
  expect_false(grepl("setupDatasetDropzones", source, fixed = TRUE))
  expect_match(source, "    setupWindowFileDrop();", fixed = TRUE)
  expect_match(source, "registerStages(roots);", fixed = TRUE)
  expect_match(source, "setupCreatableSelects(roots);", fixed = TRUE)
  expect_match(source, "updateDatasetLoadTimes(roots);", fixed = TRUE)
  expect_match(
    source,
    "Array.from(mutation.addedNodes || []).forEach",
    fixed = TRUE
  )
  expect_match(
    source,
    "new MutationObserver(handleDynamicContentMutations)",
    fixed = TRUE
  )
  expect_false(grepl("characterData: true", source, fixed = TRUE))
  expect_match(
    source,
    'sidebar.addEventListener("scroll", scheduleSpatialAlignmentScrollbars',
    fixed = TRUE
  )
  expect_match(
    source,
    'window.addEventListener("scroll", scheduleSpatialAlignmentScrollbars',
    fixed = TRUE
  )
  expect_match(
    source,
    "if (spatialScrollbarFrame !== null) return;",
    fixed = TRUE
  )
})

test_that("spatial canvas is cleared before switching datasets", {
  server_path <- builder_test_inst_path(
    "builder",
    "spatial_alignment_server.R"
  )
  client_path <- builder_test_inst_path(
    "builder",
    "www",
    "builder-spatial-canvas.js"
  )
  server_source <- paste(readLines(server_path, warn = FALSE), collapse = "\n")
  client_source <- paste(readLines(client_path, warn = FALSE), collapse = "\n")

  expect_match(
    server_source,
    'send_canvas_clear <- function() {',
    fixed = TRUE
  )
  expect_match(server_source, '"builder_spatial_canvas_clear"', fixed = TRUE)
  expect_match(
    client_source,
    'Shiny.addCustomMessageHandler("builder_spatial_canvas_clear", function (message) {',
    fixed = TRUE
  )
  expect_match(client_source, "generation < state.generation", fixed = TRUE)
  expect_match(client_source, "clear();", fixed = TRUE)
})

test_that("dataset config generations survive manifest backups and stale writers", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  manifest <- runtime$builder_project_new_manifest(root, "Generation safety")
  entry <- function(name) {
    list(
      id = "ds1",
      revision = 1L,
      settings = list(name = name),
      acknowledgements = character(),
      spatial_drafts = list()
    )
  }
  record <- function(name) {
    runtime$builder_project_dataset_record(
      entry(name),
      source = list(kind = "missing", path = NULL),
      root = root
    )
  }

  manifest$datasets <- list(record("first"))
  first <- runtime$builder_project_write(manifest, root)
  stale <- first$manifest

  winner <- first$manifest
  winner$datasets <- list(record("winner"))
  winner <- runtime$builder_project_write(
    winner,
    root,
    expected_revision = first$manifest$project$revision
  )

  stale$datasets <- list(record("stale"))
  expect_error(
    runtime$builder_project_write(
      stale,
      root,
      expected_revision = first$manifest$project$revision
    ),
    "updated by another Builder window",
    fixed = TRUE
  )

  expect_identical(
    runtime$builder_project_read_dataset_config(
      winner$manifest$datasets[[1L]],
      root
    )$settings$name,
    "winner"
  )
  backup <- runtime$builder_project_read(paste0(winner$path, ".bak"))
  expect_identical(
    runtime$builder_project_read_dataset_config(
      backup$datasets[[1L]],
      root
    )$settings$name,
    "first"
  )
  expect_false(identical(
    backup$datasets[[1L]]$configuration$path,
    winner$manifest$datasets[[1L]]$configuration$path
  ))
})

test_that("manifest publish failure restores the canonical manifest", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- runtime$builder_project_write(
    runtime$builder_project_new_manifest(root, "Restore safety"),
    root
  )
  target <- first$path
  backup <- paste0(target, ".bak")
  move <- function(from, to) {
    if (
      startsWith(basename(from), "builder-project-") &&
        identical(basename(to), "builder-project.json")
    ) {
      warning("Access is denied (Win32 error 5).", call. = FALSE)
      return(FALSE)
    }
    file.rename(from, to)
  }

  failure <- tryCatch(
    runtime$builder_project_write(
      first$manifest,
      root,
      expected_revision = first$manifest$project$revision,
      .move = move
    ),
    error = identity
  )
  expect_s3_class(failure, "error")
  message <- conditionMessage(failure)
  expect_match(message, "could not be committed", fixed = TRUE)
  expect_match(message, "commit Project manifest", fixed = TRUE)
  expect_match(message, target, fixed = TRUE)
  expect_match(message, "Win32 error 5", fixed = TRUE)
  expect_true(file.exists(target))
  expect_false(file.exists(backup))
  expect_identical(
    runtime$builder_project_read(target)$project$revision,
    first$manifest$project$revision
  )
})

test_that("manifest restore failure reports the retained backup", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  first <- runtime$builder_project_write(
    runtime$builder_project_new_manifest(root, "Recovery path"),
    root
  )
  target <- first$path
  backup <- paste0(target, ".bak")
  move <- function(from, to) {
    if (
      identical(basename(from), "builder-project.json") &&
        identical(basename(to), "builder-project.json.bak")
    ) {
      return(file.rename(from, to))
    }
    FALSE
  }

  expect_error(
    runtime$builder_project_write(
      first$manifest,
      root,
      expected_revision = first$manifest$project$revision,
      .move = move
    ),
    paste(
      "Recover it from:",
      normalizePath(backup, winslash = "/", mustWork = FALSE)
    ),
    fixed = TRUE
  )
  expect_false(file.exists(target))
  expect_true(file.exists(backup))
  expect_identical(
    runtime$builder_project_read(backup)$project$revision,
    first$manifest$project$revision
  )
})

test_that("manifest revision commits are serialized by an owned project lock", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()

  first <- runtime$builder_project_acquire_manifest_lock(root)
  expect_error(
    runtime$builder_project_acquire_manifest_lock(root),
    "Another Builder window",
    fixed = TRUE
  )
  expect_true(dir.exists(first$path))
  expect_true(runtime$builder_project_release_manifest_lock(first))

  second <- runtime$builder_project_acquire_manifest_lock(root)
  expect_true(runtime$builder_project_release_manifest_lock(second))

  implementation <- paste(
    deparse(body(runtime$builder_project_write)),
    collapse = "\n"
  )
  acquire <- regexpr(
    "builder_project_acquire_manifest_lock(root)",
    implementation,
    fixed = TRUE
  )[[1L]]
  read_disk <- regexpr(
    "builder_project_read(target)",
    implementation,
    fixed = TRUE
  )[[1L]]
  expect_gt(acquire, 0L)
  expect_gt(read_disk, acquire)
})

test_that("project input is bounded and Builder has no inline image decoder", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  oversized <- file.path(root, "oversized.json")
  writeBin(
    rep(as.raw(0x20), runtime$.builder_project_manifest_max_bytes + 1L),
    oversized
  )

  expect_error(
    runtime$builder_project_read(oversized),
    "size limit",
    fixed = TRUE
  )
  implementation <- paste(
    readLines(builder_test_inst_path("builder", "project.R")),
    collapse = "\n"
  )
  expect_false(grepl("base64decode", implementation, fixed = TRUE))
  expect_false(grepl(
    "builder_project_decode_image_uri",
    implementation,
    fixed = TRUE
  ))
})

test_that("restored source identity requires the recorded content fingerprint", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  source <- file.path(root, "sources", "ds1", "source.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("AAAA"), source)
  recorded_time <- file.info(source)$mtime[[1L]]
  record <- runtime$builder_project_dataset_record(
    list(id = "ds1", settings = list(name = "Dataset")),
    source = list(
      kind = "managed",
      path = "sources/ds1/source.rds",
      status = "ready",
      fingerprint = runtime$builder_project_file_fingerprint(
        source,
        content = TRUE
      )
    ),
    checked = TRUE,
    root = root
  )

  writeBin(charToRaw("BBBB"), source)
  Sys.setFileTime(source, recorded_time)
  status <- runtime$builder_project_dataset_status(record, root)

  expect_true("resume" %in% status$actions)
  record$runtime_restore_status <- status
  record$runtime_restore_action <- "resume"
  loaded <- list(
    id = "ds1",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        basename(source),
        file.info(source)$size[[1L]],
        "timestamp",
        unname(tools::md5sum(source)),
        sep = ":"
      ),
      source_state = c(
        list(location = normalizePath(source, winslash = "/")),
        runtime$builder_project_file_metadata(source)
      )
    )
  )
  expect_error(
    runtime$builder_project_finalize_loaded_entry(loaded, record, root),
    "source changed"
  )
})

test_that("content-addressed sources detect same-metadata tampering", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  staged <- file.path(root, "source.rds")
  writeBin(charToRaw("AAAA"), staged)
  source_md5 <- unname(tools::md5sum(staged))
  relative <- paste(
    "sources",
    "ds1",
    "blobs",
    source_md5,
    "source.rds",
    sep = "/"
  )
  source <- file.path(root, relative)
  dir.create(dirname(source), recursive = TRUE)
  expect_true(file.rename(staged, source))
  recorded_time <- file.info(source)$mtime[[1L]]
  record <- runtime$builder_project_dataset_record(
    list(id = "ds1", settings = list(name = "Dataset")),
    source = list(
      kind = "managed",
      path = relative,
      status = "ready",
      fingerprint = runtime$builder_project_file_fingerprint(
        source,
        content = TRUE
      )
    ),
    checked = TRUE,
    root = root
  )

  writeBin(charToRaw("BBBB"), source)
  Sys.setFileTime(source, recorded_time)
  status <- runtime$builder_project_dataset_status(record, root)

  expect_true("resume" %in% status$actions)
  record$runtime_restore_status <- status
  record$runtime_restore_action <- "resume"
  loaded <- list(
    id = "ds1",
    snapshot = list(
      source_fingerprint = paste(
        "builder-snapshot-v2",
        basename(source),
        file.info(source)$size[[1L]],
        "timestamp",
        unname(tools::md5sum(source)),
        sep = ":"
      ),
      source_state = c(
        list(location = normalizePath(source, winslash = "/")),
        runtime$builder_project_file_metadata(source)
      )
    )
  )
  expect_error(
    runtime$builder_project_finalize_loaded_entry(loaded, record, root),
    "source changed"
  )
})

test_that("persisted fingerprints retain the unchanged-file fast path", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  path <- withr::local_tempfile()
  writeBin(charToRaw("artifact"), path)
  recorded <- runtime$builder_project_file_fingerprint(path, content = TRUE)
  persisted <- jsonlite::unserializeJSON(jsonlite::serializeJSON(
    recorded,
    digits = NA
  ))
  content_reads <- 0L
  fingerprint <- runtime$builder_project_file_fingerprint
  runtime$builder_project_file_fingerprint <- function(path, content = FALSE) {
    if (isTRUE(content)) {
      content_reads <<- content_reads + 1L
    }
    fingerprint(path, content = content)
  }

  expect_true(runtime$builder_project_managed_file_matches(persisted, path))
  expect_identical(content_reads, 0L)
  expect_identical(persisted$changed_at, recorded$changed_at)
})

test_that("invalid change times fall back to content validation", {
  runtime <- builder_project_test_runtime()
  path <- withr::local_tempfile()
  writeBin(charToRaw("AAAA"), path)
  recorded_time <- file.info(path)$mtime[[1L]]
  recorded <- runtime$builder_project_file_fingerprint(path, content = TRUE)
  recorded$changed_at <- "NA"
  writeBin(charToRaw("BBBB"), path)
  Sys.setFileTime(path, recorded_time)
  content_reads <- 0L
  fingerprint <- runtime$builder_project_file_fingerprint
  runtime$builder_project_file_fingerprint <- function(path, content = FALSE) {
    value <- fingerprint(path, content = content)
    if (isTRUE(content)) {
      content_reads <<- content_reads + 1L
    } else {
      value$changed_at <- "NA"
    }
    value
  }

  expect_false(runtime$builder_project_managed_file_matches(recorded, path))
  expect_identical(content_reads, 1L)
})

test_that("artifact availability validates the primary file and every member", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  primary <- file.path(root, "artifacts", "ds1", "bundle", "ds1.crb")
  member <- file.path(dirname(primary), "ds1.h5")
  dir.create(dirname(primary), recursive = TRUE)
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("member"), member)
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = "artifacts/ds1/bundle/ds1.crb",
    fingerprint = runtime$builder_project_file_fingerprint(
      primary,
      content = TRUE
    ),
    members = list(list(
      target = "ds1.h5",
      path = "artifacts/ds1/bundle/ds1.h5",
      fingerprint = runtime$builder_project_file_fingerprint(
        member,
        content = TRUE
      )
    ))
  )

  expect_true(runtime$builder_project_artifact_available(artifact, root))
  primary_time <- file.info(primary)$mtime[[1L]]
  writeBin(charToRaw("altered"), primary)
  Sys.setFileTime(primary, primary_time)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
  writeBin(charToRaw("primary"), primary)
  Sys.setFileTime(primary, primary_time)
  expect_true(runtime$builder_project_artifact_available(artifact, root))
  member_time <- file.info(member)$mtime[[1L]]
  writeBin(charToRaw("tamper"), member)
  Sys.setFileTime(member, member_time)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
  writeBin(charToRaw("member"), member)
  Sys.setFileTime(member, member_time)
  expect_true(runtime$builder_project_artifact_available(artifact, root))
  writeBin(charToRaw("changed"), member)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
  unlink(member)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
})

test_that("artifact availability rejects an untracked spatial molecule closure", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  primary <- file.path(root, "artifacts", "ds1", "bundle", "ds1.crb")
  sidecar <- file.path(dirname(primary), "ds1.spatial", "0001.qs2")
  dir.create(dirname(sidecar), recursive = TRUE)
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("molecules"), sidecar)
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = "artifacts/ds1/bundle/ds1.crb",
    fingerprint = runtime$builder_project_file_fingerprint(
      primary,
      content = TRUE
    ),
    members = list()
  )
  runtime$.builder_app_capture_spatial_molecule_identity <- function(path) {
    list(
      entries = list(list(
        path = "ds1.spatial/0001.qs2",
        type = "file"
      ))
    )
  }

  expect_false(runtime$builder_project_artifact_available(artifact, root))

  artifact$members <- list(list(
    target = "ds1.spatial/0001.qs2",
    path = "artifacts/ds1/bundle/ds1.spatial/0001.qs2",
    fingerprint = runtime$builder_project_file_fingerprint(
      sidecar,
      content = TRUE
    )
  ))
  expect_true(runtime$builder_project_artifact_available(artifact, root))

  unlink(sidecar)
  expect_false(runtime$builder_project_artifact_available(artifact, root))
})

test_that("only registered spatial bundle contracts bypass closure reopening", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  primary <- file.path(root, "artifacts", "ds1", "bundle", "ds1.crb")
  sidecar <- file.path(dirname(primary), "ds1.spatial", "0001.qs2")
  dir.create(dirname(sidecar), recursive = TRUE)
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("molecules"), sidecar)
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    path = "artifacts/ds1/bundle/ds1.crb",
    fingerprint = runtime$builder_project_file_fingerprint(
      primary,
      content = TRUE
    ),
    built_from_configuration = "configuration",
    plan_item = list(
      id = "ds1",
      artifact_identity = list(spatial_sections = "slice-a")
    ),
    members = list(list(
      target = "ds1.spatial/0001.qs2",
      path = "artifacts/ds1/bundle/ds1.spatial/0001.qs2",
      fingerprint = runtime$builder_project_file_fingerprint(
        sidecar,
        content = TRUE
      )
    ))
  )
  captures <- 0L
  runtime$.builder_app_capture_spatial_molecule_identity <- function(path) {
    captures <<- captures + 1L
    list(entries = list(list(path = "ds1.spatial/0001.qs2", type = "file")))
  }

  expect_true(runtime$builder_project_artifact_available(artifact, root))
  expect_identical(captures, 1L)

  artifact$bundle_contract_version <- 1L
  expect_true(runtime$builder_project_artifact_available(artifact, root))
  expect_identical(captures, 1L)
})

test_that("artifact registration rejects a nonportable spatial closure", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build <- file.path(root, "build")
  project <- file.path(root, "project")
  sidecar <- file.path(build, "ds1.spatial")
  dir.create(sidecar, recursive = TRUE)
  dir.create(project)
  primary <- file.path(build, "ds1.crb")
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("molecules"), file.path(sidecar, "0001.qs2"))
  runtime$.builder_app_capture_spatial_molecule_identity <- function(path) {
    stop("stale spatial root", call. = FALSE)
  }

  expect_error(
    runtime$builder_project_store_artifact_bundle(
      primary,
      sidecars = "ds1.spatial",
      dataset_id = "ds1",
      root = project
    ),
    "not portable",
    fixed = TRUE
  )
  generations <- file.path(project, "artifacts", "ds1", "generations")
  expect_length(list.dirs(generations, recursive = FALSE), 0L)
})

test_that("completed build artifacts use the formal Project registration path", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build <- file.path(root, "build")
  project <- file.path(root, "project")
  dir.create(build)
  dir.create(project)
  primary <- file.path(build, "dataset.crb")
  writeBin(charToRaw("completed-build"), primary)
  entry <- list(
    id = "ds1",
    revision = 7L,
    snapshot = list(source_fingerprint = "source-fingerprint"),
    settings = list(name = "Dataset"),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  item <- list(
    id = "ds1",
    name = "Dataset",
    filename = basename(primary),
    sidecars = character(),
    private_assets = basename(primary),
    recommendations = list(duplicate = TRUE),
    readiness = "ready",
    nomenclature = "duplicate",
    histology_coverage = list(duplicate = TRUE),
    group_color_overrides = list(duplicate = "#000000"),
    color_custom_count = 1L
  )
  prepared <- runtime$builder_project_prepare_build_artifacts(
    plan = list(items = list(item)),
    value = list(
      state = "success",
      built = stats::setNames(list(primary), "ds1"),
      verifications = list(ds1 = list())
    ),
    root = project,
    previous_artifacts = list(),
    entries = list(entry)
  )

  expect_null(prepared$error)
  expect_true(prepared$build_succeeded)
  expect_identical(prepared$completed, 1L)
  expect_identical(prepared$artifacts$ds1$built_from_revision, 7L)
  expect_false(any(
    c("dataset_id", "display_name") %in%
      names(prepared$artifacts$ds1)
  ))
  payload <- jsonlite::unserializeJSON(prepared$artifacts$ds1$plan_payload)
  expect_identical(payload$id, "ds1")
  expect_identical(payload$name, "Dataset")
  expect_null(payload$recommendations)
  expect_null(payload$readiness)
  expect_null(payload$nomenclature)
  expect_null(payload$histology_coverage)
  expect_null(payload$group_color_overrides)
  expect_null(payload$color_custom_count)
  expect_identical(
    prepared$artifacts$ds1$built_from_source_fingerprint,
    "source-fingerprint"
  )
  expect_true(file.exists(file.path(project, prepared$artifacts$ds1$path)))
  expect_true(file.exists(primary))

  reused_item <- item
  reused_item$name <- "Renamed Dataset"
  reused_item$reused_artifact <- prepared$artifacts$ds1
  reused_item$reused_artifact$path <- runtime$builder_project_resolve_path(
    prepared$artifacts$ds1$path,
    project,
    "managed"
  )
  reused <- runtime$builder_project_prepare_build_artifacts(
    plan = list(items = list(reused_item)),
    value = list(
      state = "success",
      built = stats::setNames(list(reused_item$reused_artifact$path), "ds1")
    ),
    root = project,
    previous_artifacts = prepared$artifacts,
    entries = list(entry)
  )
  expect_null(reused$error)
  reused_payload <- jsonlite::unserializeJSON(
    reused$artifacts$ds1$plan_payload
  )
  expect_identical(reused_payload$id, "ds1")
  expect_identical(reused_payload$name, "Renamed Dataset")
  expect_identical(reused_payload$filename, basename(primary))
  expect_null(reused_payload$reused_artifact)

  missing <- runtime$builder_project_prepare_build_artifacts(
    plan = list(items = list(item)),
    value = list(
      state = "success",
      built = stats::setNames(list(file.path(build, "missing.crb")), "ds1"),
      verifications = list(ds1 = list())
    ),
    root = project,
    previous_artifacts = list(),
    entries = list(entry)
  )
  expect_match(missing$error, "is missing", fixed = TRUE)
  expect_length(missing$artifacts, 0L)
})

test_that("a CRB built from a saved example dataset is registered", {
  runtime <- builder_project_test_runtime()
  runtime$builder_source_utf8(
    builder_test_inst_path("builder", "adapters.R"),
    envir = runtime
  )
  # Load: the example records the content hash of the file it was read from.
  example <- runtime$builder_example_catalog()$complete_viewer_data
  made <- example$make()
  example_md5 <- unname(tools::md5sum(example$serialized_path))
  expect_identical(made$source_md5, example_md5)
  adapter <- runtime$.builder_example_source_adapter(
    runtime$builder_example_adapter(example$id, made$object),
    made$source_file,
    made$source_md5
  )
  source_fingerprint <- runtime$.builder_adapter_source_fingerprint(adapter)
  expect_identical(
    runtime$builder_project_source_fingerprint_md5(source_fingerprint),
    example_md5
  )
  root <- withr::local_tempdir()
  entry <- list(
    id = "ds1",
    revision = 3L,
    snapshot = list(source_fingerprint = source_fingerprint),
    settings = list(name = example$label, organism = "hg"),
    acknowledgements = character(),
    spatial_drafts = list(),
    path = example$serialized_path,
    filename = basename(example$serialized_path),
    source_origin = "example",
    example = example$id
  )

  # Save: the Project keeps its own copy of the example file, then the
  # dataset is checked and saved again.
  job <- runtime$builder_project_source_job(entry, root)
  dir.create(dirname(job$target), recursive = TRUE)
  expect_true(file.copy(example$serialized_path, job$target))
  source <- list(
    kind = "managed",
    origin = "example",
    example = example$id,
    filename = job$filename,
    path = job$relative,
    status = "ready",
    fingerprint = runtime$builder_project_file_fingerprint(
      job$target,
      content = TRUE
    )
  )
  manifest <- runtime$builder_project_new_manifest(root, "Examples")
  manifest$datasets <- list(runtime$builder_project_dataset_record(
    entry,
    source = source,
    checked = TRUE,
    root = root
  ))
  saved <- runtime$builder_project_write(manifest, root)

  # The dataset cache built for the checked dataset.
  build <- withr::local_tempdir()
  primary <- file.path(build, "01-complete-viewer-demo-ds1.crb")
  writeBin(charToRaw("example-build"), primary)
  prepared <- runtime$builder_project_prepare_build_artifacts(
    plan = list(
      items = list(list(
        id = "ds1",
        name = example$label,
        filename = basename(primary),
        sidecars = character(),
        private_assets = basename(primary)
      ))
    ),
    value = list(
      state = "success",
      built = stats::setNames(list(primary), "ds1"),
      verifications = list(ds1 = list())
    ),
    root = root,
    previous_artifacts = list(),
    entries = list(entry[c(
      "id",
      "revision",
      "snapshot",
      "settings",
      "acknowledgements",
      "spatial_drafts"
    )])
  )
  expect_null(prepared$error)
  artifact <- prepared$artifacts$ds1

  registered <- runtime$builder_project_manifest_with_artifact(
    saved$manifest,
    root,
    "ds1",
    artifact
  )
  expect_type(registered, "list")
  written <- runtime$builder_project_write(
    registered,
    root,
    expected_revision = saved$manifest$project$revision
  )
  reopened <- runtime$builder_project_read(written$path)
  expect_identical(reopened$datasets[[1L]]$artifact$status, "ready")
  status <- runtime$builder_project_dataset_status(
    reopened$datasets[[1L]],
    root
  )
  expect_true("reuse" %in% status$actions)

  # A cache that names only the example, not the file the Project keeps,
  # is still refused, and the refusal says which check failed.
  legacy <- artifact
  legacy$built_from_source_fingerprint <- "example:complete_viewer_data:5.4.0"
  refused <- runtime$builder_project_manifest_with_artifact(
    reopened,
    root,
    "ds1",
    legacy
  )
  expect_false(is.list(refused))
  expect_identical(
    attr(refused, "reason"),
    "The dataset cache was built from a different source file."
  )
  runtime$.builder_app_capture_spatial_molecule_identity <- function(path) {
    list(entries = list())
  }
  expect_error(
    runtime$builder_project_write_task(
      function(full) runtime,
      list(
        manifest = reopened,
        root = root,
        artifact_record = list(id = "ds1", artifact = legacy),
        expected_revision = reopened$project$revision,
        configuration_cache = list()
      )
    ),
    paste(
      "The dataset cache no longer matches this project.",
      "The dataset cache was built from a different source file."
    ),
    fixed = TRUE
  )
})

test_that("trimmed artifact plans reopen and freeze through the Project", {
  runtime <- builder_project_test_runtime()
  builder_plan_contract_source_runtime(runtime)
  for (file in c("preview.R", "recommend.R", "plan.R")) {
    builder_repo_source(file, local = runtime)
  }
  runtime$.builder_app_capture_spatial_molecule_identity <- function(path) {
    list(entries = list())
  }
  root <- withr::local_tempdir()
  build <- file.path(root, "build")
  project <- file.path(root, "project")
  dir.create(build)
  dir.create(project)
  entry <- builder_task6_entry()
  entry$revision <- 1L
  plan <- runtime$builder_freeze_plan(
    list(entry),
    file.path(root, "release"),
    make_app = FALSE
  )
  expect_null(plan$error)
  primary <- file.path(build, plan$items[[1L]]$filename)
  writeBin(charToRaw("completed-build"), primary)

  prepared <- runtime$builder_project_prepare_build_artifacts(
    plan = plan,
    value = list(
      state = "success",
      built = stats::setNames(list(primary), entry$id),
      verifications = stats::setNames(list(list()), entry$id)
    ),
    root = project,
    previous_artifacts = list(),
    entries = list(entry)
  )
  expect_null(prepared$error)
  stored <- jsonlite::unserializeJSON(
    prepared$artifacts[[entry$id]]$plan_payload
  )
  expect_null(stored$recommendations)
  expect_null(stored$readiness)
  expect_null(stored$nomenclature)
  expect_null(stored$histology_coverage)
  expect_null(stored$group_color_overrides)
  expect_null(stored$color_custom_count)

  manifest <- runtime$builder_project_new_manifest(project, "Trim round trip")
  manifest$datasets <- list(runtime$builder_project_dataset_record(
    entry,
    source = list(
      kind = "example",
      origin = "example",
      example = "complete_viewer_data"
    ),
    checked = TRUE,
    artifact = prepared$artifacts[[entry$id]],
    root = project
  ))
  written <- runtime$builder_project_write(manifest, project)
  reopened <- runtime$builder_project_read(written$path)
  record <- reopened$datasets[[1L]]
  record$runtime_restore_status <- runtime$builder_project_dataset_status(
    record,
    project
  )
  expect_true("reuse" %in% record$runtime_restore_status$actions)
  restored <- runtime$builder_project_prepare_open_selection(
    list(datasets = list(record)),
    project,
    stats::setNames(list("reuse"), entry$id)
  )$reusable_entries[[1L]]
  expect_null(restored$project_artifact$plan_payload)

  frozen <- runtime$builder_freeze_plan(
    list(restored),
    file.path(root, "reopened-release"),
    make_app = FALSE
  )
  expect_null(frozen$error)
  expect_identical(frozen$items[[1L]]$id, plan$items[[1L]]$id)
  expect_identical(frozen$items[[1L]]$filename, plan$items[[1L]]$filename)
  expect_identical(
    frozen$items[[1L]]$artifact_identity,
    plan$items[[1L]]$artifact_identity
  )
  expect_identical(
    frozen$items[[1L]]$histology_coverage,
    plan$items[[1L]]$histology_coverage
  )
  expect_true(is.list(frozen$items[[1L]]$reused_artifact))
})

test_that("verified spatial molecule sidecars stay paired with their CRB", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  crb <- file.path(root, "dataset.crb")
  sidecar <- file.path(root, "dataset.spatial")
  dir.create(sidecar)
  writeBin(charToRaw("dataset"), crb)

  expect_identical(
    runtime$builder_project_verified_spatial_sidecar(
      crb,
      list(spatial_molecule_path = sidecar)
    ),
    "dataset.spatial"
  )
  expect_null(runtime$builder_project_verified_spatial_sidecar(crb, list()))

  wrong <- file.path(root, "other.spatial")
  dir.create(wrong)
  expect_error(
    runtime$builder_project_verified_spatial_sidecar(
      crb,
      list(spatial_molecule_path = wrong)
    ),
    "does not match its dataset cache",
    fixed = TRUE
  )
})

test_that("managed writes reject symlink ancestors before creating external files", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  outside <- file.path(root, "outside")
  dir.create(project)
  dir.create(outside)
  if (!isTRUE(file.symlink(outside, file.path(project, "datasets")))) {
    skip("symlinks are unavailable")
  }

  expect_error(
    runtime$builder_project_write_dataset_config(
      list(id = "ds1", settings = list(name = "Dataset")),
      project
    ),
    "symbolic link",
    fixed = TRUE
  )
  expect_length(list.files(outside, recursive = TRUE, all.files = TRUE), 0L)
})

test_that("project integrity reuses verified configuration content", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  entry <- list(
    id = "ds1",
    revision = 1L,
    settings = list(name = "Dataset")
  )
  configuration <- runtime$builder_project_write_dataset_config(entry, root)
  record <- list(id = "ds1", configuration = configuration)
  original_read <- runtime$builder_project_read_configuration_file
  reads <- 0L
  runtime$builder_project_read_configuration_file <- function(...) {
    reads <<- reads + 1L
    original_read(...)
  }
  cache <- new.env(parent = emptyenv())

  first <- runtime$builder_project_configuration_integrity(
    record,
    root,
    verification_cache = cache
  )
  second <- runtime$builder_project_configuration_integrity(
    record,
    root,
    verification_cache = cache
  )

  expect_true(first$valid)
  expect_identical(second, first)
  expect_identical(reads, 1L)
})

test_that("artifact bundles are immutable generations", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build_dir <- file.path(root, "build")
  project <- file.path(root, "project")
  dir.create(build_dir)
  dir.create(project)
  primary <- file.path(build_dir, "ds1.crb")
  member <- file.path(build_dir, "ds1.h5")
  writeBin(charToRaw("first-primary"), primary)
  writeBin(charToRaw("first-member"), member)

  first <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = project
  )
  expect_true(file.exists(primary))
  expect_true(file.exists(member))
  first_bytes <- readBin(file.path(project, first$path), "raw", n = 100L)

  writeBin(charToRaw("second-primary"), primary)
  writeBin(charToRaw("second-member"), member)
  second <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = project
  )

  expect_false(identical(first$path, second$path))
  expect_identical(
    readBin(file.path(project, first$path), "raw", n = 100L),
    first_bytes
  )
  expect_identical(length(second$members), 1L)
})

test_that("artifact registration reuses the committed release identity safely", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  release <- file.path(root, "release")
  project <- file.path(root, "project")
  dir.create(release)
  dir.create(project)
  primary <- file.path(release, "ds1.crb")
  writeBin(charToRaw("published-primary"), primary)
  identity <- list(
    schema_version = 1L,
    exists = TRUE,
    entries = list(list(
      path = "ds1.crb",
      type = "file",
      size = file.info(primary)$size[[1L]],
      md5 = unname(tools::md5sum(primary))
    ))
  )
  verified_release <- list(target = release, identity = identity)

  bundle <- runtime$builder_project_store_artifact_bundle(
    primary,
    dataset_id = "ds1",
    root = project,
    verified_release = verified_release
  )
  expect_true(file.exists(file.path(project, bundle$path)))

  writeBin(charToRaw("tampered-primary"), primary)
  expect_error(
    runtime$builder_project_store_artifact_bundle(
      primary,
      dataset_id = "ds1",
      root = project,
      verified_release = verified_release
    ),
    "changed before registration",
    fixed = TRUE
  )
})

test_that("artifact bundles retain directory sidecar contents", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build_dir <- file.path(root, "build")
  project <- file.path(root, "project")
  sidecar <- file.path(build_dir, "ds1.spatial")
  dir.create(file.path(sidecar, "matrix"), recursive = TRUE)
  dir.create(project)
  primary <- file.path(build_dir, "ds1.crb")
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("index"), file.path(sidecar, ".index"))
  writeBin(charToRaw("data"), file.path(sidecar, "matrix", "data"))

  bundle <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.spatial",
    dataset_id = "ds1",
    root = project,
    promote = TRUE
  )

  expect_false(file.exists(primary))
  expect_false(file.exists(file.path(sidecar, ".index")))
  expect_false(file.exists(file.path(sidecar, "matrix", "data")))
  expect_setequal(
    vapply(bundle$members, `[[`, character(1), "target"),
    c("ds1.spatial/.index", "ds1.spatial/matrix/data")
  )
  expect_true(runtime$builder_project_artifact_available(
    c(list(status = "ready"), bundle),
    project
  ))
})

test_that("directory sidecars of artifact bundles reject symbolic links", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  build_dir <- file.path(root, "build")
  project <- file.path(root, "project")
  sidecar <- file.path(build_dir, "ds1.spatial")
  dir.create(sidecar, recursive = TRUE)
  dir.create(project)
  primary <- file.path(build_dir, "ds1.crb")
  data <- file.path(sidecar, "data")
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("data"), data)
  skip_if_not(file.symlink(data, file.path(sidecar, "linked")))

  expect_error(
    runtime$builder_project_store_artifact_bundle(
      primary,
      sidecars = "ds1.spatial",
      dataset_id = "ds1",
      root = project
    ),
    "escaped the build folder",
    fixed = TRUE
  )
})

test_that("checkpoint artifacts promote to immutable shared generations", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  checkpoint <- file.path(root, "checkpoints", "build-1")
  dir.create(checkpoint, recursive = TRUE)
  primary <- file.path(checkpoint, "ds1.crb")
  member <- file.path(checkpoint, "ds1.h5")
  writeBin(charToRaw("primary"), primary)
  writeBin(charToRaw("member"), member)

  bundle <- runtime$builder_project_store_artifact_bundle(
    primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = root,
    promote = TRUE
  )

  expect_false(file.exists(primary))
  expect_false(file.exists(member))
  expect_true(runtime$builder_project_artifact_available(
    c(list(status = "ready"), bundle),
    root
  ))

  second_checkpoint <- file.path(root, "checkpoints", "build-2")
  dir.create(second_checkpoint, recursive = TRUE)
  second_primary <- file.path(second_checkpoint, "ds1.crb")
  second_member <- file.path(second_checkpoint, "ds1.h5")
  writeBin(charToRaw("primary"), second_primary)
  writeBin(charToRaw("member"), second_member)
  adopted <- runtime$builder_project_store_artifact_bundle(
    second_primary,
    sidecars = "ds1.h5",
    dataset_id = "ds1",
    root = root,
    promote = TRUE
  )

  expect_identical(adopted$path, bundle$path)
  expect_true(file.exists(file.path(root, bundle$path)))
  expect_true(file.exists(second_primary))
  expect_true(file.exists(second_member))

  implementation <- paste(
    deparse(body(runtime$builder_project_store_artifact_bundle)),
    collapse = "\n"
  )
  rename_failure <- regexpr(
    "if (!file.rename(staging, generation_dir))",
    implementation,
    fixed = TRUE
  )[[1L]]
  concurrent_adoption <- regexpr(
    "if (dir.exists(generation_dir))",
    implementation,
    fixed = TRUE
  )[[1L]]
  expect_gt(rename_failure, 0L)
  expect_gt(concurrent_adoption, rename_failure)
  expect_match(
    implementation,
    "return(read_existing_generation())",
    fixed = TRUE
  )
})

test_that("configuration identity cache invalidates only on entry revision", {
  runtime <- builder_project_test_runtime()
  cache <- new.env(parent = emptyenv())
  calls <- 0L
  digest <- function(entry) {
    calls <<- calls + 1L
    paste0("digest-", entry$revision)
  }
  entry <- list(id = "ds1", revision = 1L, settings = list(name = "Dataset"))

  first <- runtime$builder_project_cached_configuration_digest(
    entry,
    cache,
    digest
  )
  second <- runtime$builder_project_cached_configuration_digest(
    entry,
    cache,
    digest
  )
  entry$revision <- 2L
  third <- runtime$builder_project_cached_configuration_digest(
    entry,
    cache,
    digest
  )

  expect_identical(first, second)
  expect_false(identical(second, third))
  expect_identical(calls, 2L)
})

test_that("configuration identity reuses a spatial source content digest", {
  runtime <- builder_project_test_runtime()
  content_md5 <- strrep("a", 32L)
  entry <- list(
    settings = list(
      images = list(
        section = list(
          image = list(
            source_path = "C:/private/image.png",
            source_content_md5 = content_md5
          )
        )
      )
    )
  )
  expect_match(
    runtime$builder_project_configuration_digest(entry),
    "^[[:xdigit:]]{32}$"
  )
})

test_that("configuration identity cache bounds variants", {
  runtime <- builder_project_test_runtime()
  cache <- new.env(parent = emptyenv())
  entry <- list(id = "ds1", revision = 1L, settings = list())
  for (index in 1:20) {
    runtime$builder_project_cached_configuration_digest(
      entry,
      cache,
      digest = function(entry) paste0("digest-", index),
      variant = paste0("draft-", index)
    )
  }
  expect_lte(length(ls(cache)), 8L)
})

test_that("checkpoint cleanup refuses lexical symlinks without deleting targets", {
  skip_on_os("windows")
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  checkpoint_root <- file.path(root, "checkpoints")
  target <- file.path(checkpoint_root, "retained")
  link <- file.path(checkpoint_root, "build-link")
  dir.create(target, recursive = TRUE)
  writeBin(charToRaw("checkpoint"), file.path(target, "dataset.crb"))
  skip_if_not(file.symlink(target, link), "symbolic links are unavailable")

  expect_false(runtime$builder_project_cleanup_checkpoint(link, root))
  expect_true(nzchar(Sys.readlink(link)))
  expect_true(file.exists(file.path(target, "dataset.crb")))
})

test_that("project open clears all cross-project runtime caches", {
  source <- paste(
    readLines(builder_test_inst_path(
      "builder",
      "server",
      "project.R"
    )),
    collapse = "\n"
  )

  expect_match(source, "projection_previews(list())", fixed = TRUE)
  expect_match(source, "trajectory_previews(list())", fixed = TRUE)
  expect_match(source, "spatial_previews(list())", fixed = TRUE)
  expect_match(
    source,
    "builder_project_configuration_cache_clear(",
    fixed = TRUE
  )
  expect_match(
    source,
    "builder_configuration_identity_cache",
    fixed = TRUE
  )
  expect_match(source, "invalidate_builder_project_source_sync()", fixed = TRUE)
  expect_match(source, "reset_builder_project_crb_pipeline()", fixed = TRUE)
})

test_that("source sync context is bound to project identity, not revision", {
  runtime <- builder_project_test_runtime()
  root <- withr::local_tempdir()
  project_a <- list(
    root = root,
    path = file.path(root, "builder-project.json"),
    manifest = list(project = list(id = "project-a", revision = 3L))
  )
  context <- runtime$builder_project_source_context(
    project_a,
    generation = 4L,
    active_ids = "ds1"
  )

  expect_true(runtime$builder_project_source_context_matches(
    context,
    project_a,
    generation = 4L
  ))
  project_b <- project_a
  project_b$manifest$project$id <- "project-b"
  expect_false(runtime$builder_project_source_context_matches(
    context,
    project_b,
    generation = 4L
  ))
  ## Saves during a copy commit new revisions; the copy stays current and is
  ## merged into the latest manifest.
  project_a$manifest$project$revision <- 4L
  expect_true(runtime$builder_project_source_context_matches(
    context,
    project_a,
    generation = 4L
  ))
  expect_false(runtime$builder_project_source_context_matches(
    context,
    project_a,
    generation = 5L
  ))
  expect_false(runtime$builder_project_source_context_matches(
    context,
    project_b,
    generation = 5L
  ))

  server <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(server, "builder_project_source_queue_add(", fixed = TRUE)
  expect_false(grepl("if (!job$id %in% active)", server, fixed = TRUE))
})

test_that("background CRBs are queued only after a successful visible save", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  save_start <- regexpr(
    "request_builder_project_save <- function(",
    source,
    fixed = TRUE
  )[[1L]]
  save_end <- regexpr("output$project_status <-", source, fixed = TRUE)[[1L]]
  save <- substr(source, save_start, save_end - 1L)

  expect_match(save, "if (isTRUE(ok)) {", fixed = TRUE)
  expect_match(
    save,
    "queue_builder_project_crbs(isolate(checked_dataset_ids()))",
    fixed = TRUE
  )
  expect_match(source, "queue_builder_project_crbs <- function(", fixed = TRUE)
  expect_match(
    source,
    "builder_project_cleanup_checkpoint(output, dirname(dirname(output)))",
    fixed = TRUE
  )
})

test_that("artifact registration is dispatched without a second state machine", {
  path <- builder_test_inst_path(
    "builder",
    "server",
    "project.R"
  )
  source <- paste(readLines(path, warn = FALSE), collapse = "\n")
  registration <- substr(
    source,
    regexpr("ordinary_registration_required <-", source, fixed = TRUE)[[1L]],
    nchar(source)
  )

  expect_match(registration, "builder_async_submit(", fixed = TRUE)
  expect_match(
    registration,
    "runtime$builder_project_prepare_build_artifacts(",
    fixed = TRUE
  )
  expect_false(grepl(
    "builder_project_store_artifact_bundle(",
    registration,
    fixed = TRUE
  ))
})
