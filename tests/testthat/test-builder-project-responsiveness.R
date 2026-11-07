## Project open/save responsiveness: background copies, queued writes and
## work skipped when nothing changed.

builder_responsiveness_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_test_inst_path("builder", "prerequisite.R"),
    envir = runtime
  )
  for (file in c("core/bundle_path_contract.R", "io.R", "project.R")) {
    runtime$builder_source_utf8(
      builder_test_inst_path("builder", file),
      envir = runtime
    )
  }
  runtime
}

builder_responsiveness_app <- function(env = parent.frame()) {
  withr::local_options(
    CerebroNexus.builder.project_worker_prewarm = FALSE,
    .local_envir = env
  )
  app_env <- new.env(parent = globalenv())
  withr::local_dir(builder_test_inst_path("builder"), .local_envir = env)
  sys.source("app.R", envir = app_env)
  app_env$builder_session_start <- function(...) {
    list(error = "Worker startup is disabled in this responsiveness test.")
  }
  app_env
}

builder_responsiveness_run_later <- function(done, timeout = 10) {
  deadline <- Sys.time() + timeout
  while (!isTRUE(done()) && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  isTRUE(done())
}

test_that("a source job skips copying when its blob already verifies", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  source <- file.path(root, "session-sources", "ds1", "source.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(charToRaw("source-bytes"), source)
  md5 <- unname(tools::md5sum(source))
  entry <- list(
    id = "ds1",
    path = source,
    filename = "sample.rds",
    snapshot = list(source_fingerprint = paste0("builder-snapshot-v2:", md5))
  )
  job <- runtime$builder_project_source_job(entry, project)
  expect_true(runtime$builder_project_content_addressed_source(
    job$relative,
    "ds1"
  ))

  first <- runtime$builder_project_copy_source_job(job)
  expect_identical(first$status, "ready")
  expect_identical(first$planned, job$relative)

  ## A copy queued again for the same bytes (for example after an earlier
  ## run was cut short) publishes nothing new and needs no source.
  unlink(source)
  info_before <- file.info(first$path)$mtime
  second <- runtime$builder_project_copy_source_job(job)
  expect_identical(second$status, "ready")
  expect_identical(second$path, first$path)
  expect_identical(second$fingerprint$md5, md5)
  expect_identical(file.info(second$path)$mtime, info_before)
  expect_false(file.exists(job$part))

  ## A blob that no longer matches is never reused.
  writeBin(charToRaw("tampered"), first$path)
  third <- runtime$builder_project_copy_source_job(job)
  expect_identical(third$status, "failed")
})

test_that("finished copies merge only into records still waiting for them", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  target <- file.path(
    root,
    "sources",
    "ds1",
    "blobs",
    strrep("a", 32),
    "source.rds"
  )
  dir.create(dirname(target), recursive = TRUE)
  writeBin(charToRaw("ready"), target)
  planned <- paste(
    "sources",
    "ds1",
    "blobs",
    strrep("a", 32),
    "source.rds",
    sep = "/"
  )
  record <- function(id, status, path) {
    list(
      id = id,
      source = list(kind = "managed", path = path, status = status)
    )
  }
  manifest <- list(
    project = list(id = "p1", revision = 7L),
    datasets = list(
      record("ds1", "pending", planned),
      record("ds2", "pending", "sources/ds2/blobs/other/source.rds"),
      record("ds3", "ready", "sources/ds3/source.rds")
    )
  )
  result <- function(id) {
    list(
      id = id,
      status = "ready",
      path = target,
      planned = planned,
      fingerprint = list(md5 = strrep("a", 32))
    )
  }
  updated <- runtime$builder_project_apply_source_results(
    manifest,
    list(result("ds1"), result("ds2"), result("ds3")),
    root,
    only_pending = TRUE
  )
  expect_identical(updated$datasets[[1L]]$source$status, "ready")
  expect_identical(updated$datasets[[1L]]$source$path, planned)
  ## ds2 now waits for a different source; ds3 was already saved meanwhile.
  expect_identical(updated$datasets[[2L]], manifest$datasets[[2L]])
  expect_identical(updated$datasets[[3L]], manifest$datasets[[3L]])
})

test_that("a source job identical to the running copy is not queued again", {
  runtime <- builder_responsiveness_runtime()
  job <- function(id, md5, source = paste0("/s/", id)) {
    list(
      id = id,
      source = source,
      source_md5 = md5,
      relative = paste("sources", id, "blobs", md5, "source.rds", sep = "/")
    )
  }
  running <- list(job("ds1", strrep("a", 32)))
  queued <- runtime$builder_project_source_queue_add(
    list(ds1 = job("ds1", strrep("0", 32))),
    list(job("ds1", strrep("a", 32)), job("ds2", strrep("b", 32))),
    running
  )
  expect_identical(names(queued), "ds2")

  ## New bytes for the same dataset are queued behind the running copy.
  queued <- runtime$builder_project_source_queue_add(
    list(),
    list(job("ds1", strrep("c", 32))),
    running
  )
  expect_identical(queued$ds1$source_md5, strrep("c", 32))
})

test_that("queued manifest writes start from the latest Project revision", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(start_builder_project_manifest_write)
    calls <- new.env(parent = emptyenv())
    calls$revisions <- integer()
    calls$manifests <- list()
    server_env$builder_project_write_process <- function(
      manifest,
      root,
      expected_revision,
      ...
    ) {
      calls$revisions <- c(calls$revisions, as.integer(expected_revision))
      calls$manifests[[length(calls$manifests) + 1L]] <- manifest
      manifest$project$revision <- as.integer(expected_revision) + 1L
      list(
        is_alive = function() FALSE,
        get_result = function() {
          list(
            manifest = manifest,
            path = file.path(root, "builder-project.json")
          )
        }
      )
    }
    project <- list(
      root = "project-root",
      path = "project-root/builder-project.json",
      name = "P",
      manifest = list(
        project = list(id = "p1", name = "P", revision = 3L),
        datasets = list()
      )
    )
    builder_project(project)
    apply_written <- function(written) {
      current <- isolate(builder_project())
      current$manifest <- written$manifest
      builder_project(current)
    }

    start_builder_project_manifest_write(
      project$manifest,
      project,
      apply_written
    )
    start_builder_project_manifest_write(
      function(current) {
        manifest <- current$manifest
        manifest$derived_from <- current$manifest$project$revision
        manifest
      },
      project,
      apply_written
    )
    expect_true(builder_project_manifest_write_pending())
    expect_true(builder_responsiveness_run_later(
      function() !builder_project_manifest_write_pending()
    ))
    ## The second write waited for the first and built on its revision
    ## instead of failing the revision check.
    expect_identical(calls$revisions, c(3L, 4L))
    expect_identical(calls$manifests[[2L]]$derived_from, 4L)
    expect_identical(isolate(builder_project())$manifest$project$revision, 5L)

    ## A queued write never lands on a different Project.
    other <- project
    other$root <- "another-root"
    outcome <- new.env(parent = emptyenv())
    start_builder_project_manifest_write(
      other$manifest,
      other,
      function(written) outcome$value <- written
    )
    expect_true(builder_responsiveness_run_later(
      function() !is.null(outcome$value)
    ))
    expect_s3_class(outcome$value, "error")
    expect_length(calls$revisions, 2L)
  })
})

test_that("an unchanged Save is recognised without a manifest transaction", {
  runtime <- builder_responsiveness_runtime()
  configuration <- runtime$builder_project_configuration()
  last_ui <- list(stage = "configure", selected_dataset = "ds1")
  record <- list(
    id = "ds1",
    configuration = list(path = "datasets/ds1/config.json", checked = TRUE),
    source = list(
      kind = "managed",
      status = "ready",
      path = "sources/ds1/source.rds"
    )
  )
  manifest <- list(
    schema_version = 3L,
    project = list(id = "p1", revision = 4L),
    datasets = list(record),
    configuration = configuration,
    last_ui = last_ui
  )
  entries <- list(list(id = "ds1", revision = 2L))
  context <- list(configuration = configuration, last_ui = last_ui)
  base_manifest <- manifest
  base_entries <- entries
  base_context <- context
  unchanged <- function(
    manifest = base_manifest,
    entries = base_entries,
    context = base_context,
    jobs = list()
  ) {
    runtime$builder_project_save_unchanged(manifest, entries, context, jobs)
  }

  expect_true(unchanged())
  ## A JSON round trip of the saved manifest still compares equal.
  reread <- jsonlite::fromJSON(
    jsonlite::toJSON(manifest, auto_unbox = TRUE, null = "null", digits = NA),
    simplifyVector = FALSE
  )
  expect_true(unchanged(manifest = reread))

  pending <- manifest
  pending$datasets[[1L]]$source$status <- "pending"
  expect_false(unchanged(manifest = pending))
  ## An artifact-backed dataset needs no source copy.
  expect_true(unchanged(
    manifest = pending,
    entries = list(list(id = "ds1", load_state = "artifact_ready"))
  ))
  expect_false(unchanged(entries = list(list(id = "ds2"))))
  expect_false(unchanged(jobs = list(list(kind = "spatial"))))
  moved <- context
  moved$last_ui$stage <- "review"
  expect_false(unchanged(context = moved))
  options <- context
  options$configuration$build_mode <- !isTRUE(configuration$build_mode)
  expect_false(unchanged(context = options))
  earlier <- manifest
  earlier$schema_version <- 2L
  expect_false(unchanged(manifest = earlier))
  interrupted <- manifest
  interrupted$pending_build <- list(status = "interrupted")
  expect_false(unchanged(manifest = interrupted))
})

test_that("Save with nothing to write reports Saved immediately", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(builder_project_save_is_noop)
    calls <- new.env(parent = emptyenv())
    calls$saves <- 0L
    calls$reports <- 0L
    server_env$request_builder_project_save <- function(...) {
      calls$saves <- calls$saves + 1L
      invisible(TRUE)
    }
    server_env$report_builder_project_unchanged_save <- function() {
      calls$reports <- calls$reports + 1L
      invisible(TRUE)
    }
    server_env$builder_operation_allowed <- function(...) TRUE
    context <- builder_project_manifest_context()
    builder_project(list(
      root = withr::local_tempdir(),
      path = "builder-project.json",
      name = "P",
      manifest = list(
        schema_version = 3L,
        project = list(id = "p1", name = "P", revision = 2L),
        datasets = list(),
        configuration = context$configuration,
        last_ui = context$last_ui
      )
    ))
    session$flushReact()
    expect_true(builder_project_save_is_noop())

    session$setInputs(save_builder_project = 1)
    expect_identical(calls$reports, 1L)
    expect_identical(calls$saves, 0L)

    ## Pending source work keeps the ordinary Save.
    builder_project_source_queue(list(ds1 = list(id = "ds1")))
    expect_false(builder_project_save_is_noop())
    session$setInputs(save_builder_project = 2)
    expect_identical(calls$saves, 1L)
    builder_project_source_queue(list())

    builder_project_operation_phase("save_failed")
    expect_false(builder_project_save_is_noop())
  })
})

test_that("CRB builds run in their own pool and never starve short tasks", {
  runtime <- new.env(parent = globalenv())
  sys.source(builder_test_inst_path("builder", "async.R"), envir = runtime)
  on.exit(runtime$builder_async_stop(), add = TRUE)
  processes <- list()
  runtime$.builder_async$spawn <- function(...) {
    process <- new.env(parent = emptyenv())
    process$alive <- TRUE
    process$is_alive <- function() process$alive
    process$kill_tree <- function() process$alive <- FALSE
    process$kill <- function() process$alive <- FALSE
    process$wait <- function(...) invisible(TRUE)
    process$get_result <- function() TRUE
    processes[[length(processes) + 1L]] <<- process
    process
  }
  short <- lapply(1:3, function(index) {
    runtime$builder_async_submit(quote(TRUE))
  })
  crb <- runtime$builder_async_submit(quote(TRUE), .pool = "crb")
  second_crb <- runtime$builder_async_submit(quote(TRUE), .pool = "crb")

  expect_identical(
    vapply(short, function(task) task$state, character(1)),
    c("running", "running", "queued")
  )
  expect_identical(crb$state, "running")
  expect_identical(second_crb$state, "queued")
  expect_identical(crb$pool, "crb")
  expect_error(
    runtime$builder_async_submit(quote(TRUE), .pool = "missing"),
    "Unknown Builder background pool"
  )
})

test_that("CRB queue progress and Cancel keep only the record being written", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    output_dir <- withr::local_tempdir()
    builder_project_crb_batch(3L)
    builder_project_crb_jobs(list(
      ds1 = list(status = "registering", recording = TRUE, token = "t1"),
      ds2 = list(status = "generating", output = output_dir, token = "t2"),
      ds3 = list(status = "queued", token = "t3")
    ))
    progress <- isolate(builder_project_crb_queue_status())
    expect_identical(progress$total, 3L)
    expect_identical(progress$position, 1L)
    expect_identical(progress$name, "ds1")

    cancel_builder_project_crb_queue()
    expect_identical(names(isolate(builder_project_crb_jobs())), "ds1")
    progress <- isolate(builder_project_crb_queue_status())
    expect_identical(progress$position, 3L)

    builder_project_crb_jobs(list())
    session$flushReact()
    expect_null(isolate(builder_project_crb_queue_status()))
    expect_identical(isolate(builder_project_crb_batch()), 0L)
  })

  source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  open_block <- substr(
    source,
    regexpr("complete_builder_project_open <- function", source, fixed = TRUE),
    regexpr(
      "observeEvent(input$confirm_builder_project_open",
      source,
      fixed = TRUE
    )
  )
  expect_match(open_block, "cancel_builder_project_crb_queue()", fixed = TRUE)
  expect_match(source, '.pool = "crb"', fixed = TRUE)
})

test_that("a promoted CRB is not hashed again after its rename", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  build <- file.path(root, "build")
  dir.create(project)
  dir.create(build)
  write_build <- function() {
    built <- file.path(build, "dataset.crb")
    writeBin(as.raw(sample.int(255L, 4096L, replace = TRUE)), built)
    writeBin(charToRaw("sidecar"), file.path(build, "dataset.extra"))
    built
  }
  hashed <- character()
  real_md5sum <- tools::md5sum
  testthat::local_mocked_bindings(
    md5sum = function(files) {
      hashed <<- c(hashed, files)
      real_md5sum(files)
    },
    .package = "tools"
  )
  stored <- runtime$builder_project_store_artifact_bundle(
    write_build(),
    sidecars = "dataset.extra",
    dataset_id = "ds1",
    root = project,
    promote = TRUE
  )
  expect_false(any(grepl(
    "/.artifact-",
    gsub("\\\\", "/", hashed),
    fixed = TRUE
  )))
  expect_length(hashed, 2L)
  stored_path <- file.path(project, stored$path)
  expect_identical(
    unname(real_md5sum(stored_path)),
    stored$fingerprint$md5
  )
  ## A copied (not promoted) bundle still verifies the staged bytes.
  hashed <- character()
  unlink(file.path(project, "artifacts"), recursive = TRUE)
  runtime$builder_project_store_artifact_bundle(
    write_build(),
    sidecars = "dataset.extra",
    dataset_id = "ds1",
    root = project,
    promote = FALSE
  )
  expect_true(any(grepl(
    "/.artifact-",
    gsub("\\\\", "/", hashed),
    fixed = TRUE
  )))
})

test_that("Project writes replace only the datasets they changed", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    entry <- function(id, name) {
      list(
        id = id,
        revision = 1L,
        settings = list(name = name),
        profile = list(extras = list())
      )
    }
    sets(list(entry("ds1", "One"), entry("ds2", "Two")))
    initial <- isolate(store())$datasets
    for (id in c("ds1", "ds2")) {
      assign(paste0(id, "::1::loaded"), id, envir = builder_dataset_state_cache)
      assign(
        paste0(id, "::1::live"),
        id,
        envir = builder_configuration_identity_cache
      )
    }
    before <- isolate(store())
    expect_false(builder_project_update_entries(initial))
    expect_identical(isolate(store()), before)

    saved <- initial
    saved[[2L]]$path <- "sources/ds2/source.rds"
    expect_true(builder_project_update_entries(saved))
    expect_identical(
      isolate(store())$datasets[[2L]]$path,
      "sources/ds2/source.rds"
    )
    expect_identical(isolate(store())$datasets[[1L]], initial[[1L]])
    ## The untouched dataset keeps its derived state; the saved one is redone.
    expect_true(exists("ds1::1::loaded", envir = builder_dataset_state_cache))
    expect_false(exists("ds2::1::loaded", envir = builder_dataset_state_cache))
    expect_true(exists(
      "ds1::1::live",
      envir = builder_configuration_identity_cache
    ))
    expect_false(exists(
      "ds2::1::live",
      envir = builder_configuration_identity_cache
    ))

    ## A removed dataset is dropped from the caches too.
    expect_true(builder_project_update_entries(list(isolate(store())$datasets[[
      2L
    ]])))
    expect_false(exists("ds1::1::loaded", envir = builder_dataset_state_cache))
  })
})

test_that("CRB availability checks reuse verified content by stat identity", {
  runtime <- builder_responsiveness_runtime()
  root <- normalizePath(withr::local_tempdir(), winslash = "/")
  path <- file.path(root, "artifacts", "ds1", "dataset.crb")
  dir.create(dirname(path), recursive = TRUE)
  writeBin(charToRaw("crb-bytes"), path)
  entry <- list(id = "ds1", revision = 1L, settings = list(name = "One"))
  fingerprint <- runtime$builder_project_file_fingerprint(path, content = TRUE)
  ## A Project copied from another machine: same bytes, different file id.
  fingerprint$file_id <- "0:0"
  artifact <- list(
    status = "ready",
    reusable = TRUE,
    bundle_contract_version = 1L,
    path = "artifacts/ds1/dataset.crb",
    fingerprint = fingerprint,
    members = list(),
    built_from_configuration = runtime$builder_project_artifact_configuration_digest(
      entry
    ),
    plan_item = list(artifact_identity = list(spatial_sections = character()))
  )
  hashed <- 0L
  real_md5sum <- tools::md5sum
  testthat::local_mocked_bindings(
    md5sum = function(files) {
      hashed <<- hashed + length(files)
      real_md5sum(files)
    },
    .package = "tools"
  )
  cache <- new.env(parent = emptyenv())
  for (attempt in 1:3) {
    expect_length(
      runtime$builder_project_entries_requiring_crb(
        list(entry),
        list(ds1 = artifact),
        root,
        verification_cache = cache
      ),
      0L
    )
  }
  expect_identical(hashed, 1L)
  runtime$builder_project_entries_requiring_crb(
    list(entry),
    list(ds1 = artifact),
    root
  )
  expect_identical(hashed, 2L)
})

builder_responsiveness_session_source <- function(root, bytes = 300000L) {
  session_root <- file.path(root, "session")
  source <- file.path(session_root, "session-sources", "ds1", "source.rds")
  dir.create(dirname(source), recursive = TRUE)
  writeBin(as.raw(sample.int(255L, bytes, replace = TRUE)), source)
  list(
    session_root = normalizePath(session_root, winslash = "/"),
    source = normalizePath(source, winslash = "/"),
    md5 = unname(tools::md5sum(source))
  )
}

test_that("a Save moves the Builder's own session copy instead of copying it", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  session <- builder_responsiveness_session_source(root)
  entry <- list(
    id = "ds1",
    path = session$source,
    filename = "sample.rds",
    snapshot = list(
      source_fingerprint = paste0("builder-snapshot-v2:", session$md5)
    )
  )
  expect_true(runtime$builder_project_session_owned_source(
    session$source,
    session$session_root
  ))
  outside <- file.path(root, "upload.rds")
  writeBin(charToRaw("upload"), outside)
  expect_false(runtime$builder_project_session_owned_source(
    outside,
    session$session_root
  ))

  job <- runtime$builder_project_source_job(entry, project)
  job$session_owned <- TRUE
  file_id <- runtime$builder_project_file_metadata(session$source)$file_id
  seen <- numeric()
  result <- runtime$builder_project_copy_source_job(
    job,
    progress = function(bytes) seen <<- c(seen, bytes)
  )
  expect_identical(result$status, "ready")
  expect_identical(result$moved_from, session$source)
  expect_false(file.exists(session$source))
  expect_identical(unname(tools::md5sum(result$path)), session$md5)
  expect_identical(result$fingerprint$md5, session$md5)
  ## Same file, not a copy.
  expect_identical(result$fingerprint$file_id, file_id)
  expect_identical(max(seen), 300000)

  ## Live entries still pointing at the moved session copy follow it.
  relocated <- runtime$builder_project_relocate_moved_sources(
    list(entry, list(id = "ds2", path = outside)),
    list(result)
  )
  expect_identical(
    relocated[[1L]]$path,
    normalizePath(result$path, winslash = "/")
  )
  expect_identical(relocated[[2L]]$path, outside)
})

test_that("a moved session copy that fails verification goes back", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  session <- builder_responsiveness_session_source(root, bytes = 1000L)
  wrong <- strrep("0", 32)
  entry <- list(
    id = "ds1",
    path = session$source,
    filename = "sample.rds",
    snapshot = list(source_fingerprint = paste0("builder-snapshot-v2:", wrong))
  )
  job <- runtime$builder_project_source_job(entry, project)
  job$session_owned <- TRUE
  result <- runtime$builder_project_copy_source_job(job)
  expect_identical(result$status, "failed")
  expect_true(file.exists(session$source))
  expect_identical(unname(tools::md5sum(session$source)), session$md5)
  expect_false(file.exists(job$part))
  expect_false(file.exists(job$target))
})

test_that("other sources are streamed with byte progress and one hash pass", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  session <- builder_responsiveness_session_source(root, bytes = 250000L)
  entry <- list(id = "ds1", path = session$source, filename = "sample.rds")
  jobs <- list(runtime$builder_project_source_job(entry, project))
  progress_path <- file.path(root, "progress.rds")
  for (streaming in c(TRUE, FALSE)) {
    if (!streaming) {
      ## Without openssl the copy falls back to one md5sum of the result.
      runtime$.builder_project_md5_stream <- function() NULL
    }
    results <- runtime$builder_project_copy_source_jobs(jobs, progress_path)
    expect_identical(results[[1L]]$status, "ready", info = streaming)
    expect_null(results[[1L]]$moved_from, info = streaming)
    expect_true(file.exists(session$source), info = streaming)
    expect_identical(
      results[[1L]]$fingerprint$md5,
      session$md5,
      info = streaming
    )
    progress <- readRDS(progress_path)
    expect_identical(progress$bytes_total, 250000, info = streaming)
    expect_identical(progress$bytes_done, 250000, info = streaming)
    expect_identical(progress$completed, 1L, info = streaming)
    unlink(file.path(project, "sources"), recursive = TRUE)
  }

  copied <- withr::local_tempfile()
  seen <- numeric()
  value <- runtime$builder_project_stream_copy(
    session$source,
    copied,
    expected_md5 = session$md5,
    progress = function(bytes) seen <<- c(seen, bytes),
    buffer_bytes = 65536L
  )
  expect_identical(value$md5, session$md5)
  expect_true(length(seen) >= 4L)
  expect_error(
    runtime$builder_project_stream_copy(
      session$source,
      withr::local_tempfile(),
      expected_md5 = strrep("0", 32)
    ),
    "does not match"
  )
})

test_that("reopening the dataset choices keeps the user's selections", {
  runtime <- builder_responsiveness_runtime()
  record <- function(id, default, actions) {
    list(
      id = id,
      runtime_restore_status = list(default_action = default, actions = actions)
    )
  }
  manifest <- list(
    datasets = list(
      record("ds1", "reuse", c("reuse", "reload", "skip")),
      record("ds2", "resume", c("resume", "skip")),
      record("ds3", "skip", "skip")
    )
  )
  updated <- runtime$builder_project_manifest_with_restore_actions(
    manifest,
    list(ds1 = "skip", ds2 = "reload", ds3 = "skip")
  )
  defaults <- vapply(
    updated$datasets,
    function(record) record$runtime_restore_status$default_action,
    character(1)
  )
  ## ds2 cannot be reloaded, so its saved default stays.
  expect_identical(defaults, c("skip", "resume", "skip"))
  expect_identical(
    runtime$builder_project_manifest_with_restore_actions(manifest, NULL),
    manifest
  )
})

test_that("Open asks to discard unsaved work before the picker", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(request_builder_project_open)
    calls <- new.env(parent = emptyenv())
    calls$pickers <- 0L
    server_env$builder_operation_allowed <- function(...) TRUE
    server_env$builder_project_open_needs_confirmation <- function() TRUE
    server_env$builder_schedule_native_picker <- function(...) {
      calls$pickers <- calls$pickers + 1L
      invisible(TRUE)
    }

    session$setInputs(open_builder_project = 1)
    expect_identical(calls$pickers, 0L)
    expect_false(isolate(builder_project_open_discard_confirmed()))

    ## Keeping the workspace opens nothing.
    session$setInputs(cancel_discard_builder_project_open = 1)
    expect_identical(calls$pickers, 0L)

    session$setInputs(open_builder_project = 2)
    session$setInputs(confirm_discard_builder_project_open = 1)
    expect_identical(calls$pickers, 1L)
    ## The choices dialog will not ask the same question again.
    expect_true(isolate(builder_project_open_discard_confirmed()))
  })
})

test_that("declining the discard keeps the opened project and its choices", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(request_builder_project_open)
    shown <- new.env(parent = emptyenv())
    server_env$builder_project_restore_dialog <- function(manifest) {
      shown$manifest <- manifest
      shiny::modalDialog("choices")
    }
    manifest <- list(
      project = list(name = "P"),
      datasets = list(list(
        id = "ds1",
        runtime_restore_status = list(
          default_action = "reuse",
          actions = c("reuse", "reload", "skip")
        )
      ))
    )
    builder_project_restore(list(manifest = manifest, root = "r", path = "p"))
    builder_project_open_actions(list(ds1 = "reload"))

    session$setInputs(cancel_discard_builder_project_open = 1)
    expect_false(is.null(isolate(builder_project_restore())))
    expect_identical(
      shown$manifest$datasets[[1L]]$runtime_restore_status$default_action,
      "reload"
    )
  })
})

test_that("Open waits for a busy worker instead of asking to confirm again", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(wait_for_builder_project_open_worker)
    state <- new.env(parent = emptyenv())
    state$ready <- list(protocol = FALSE, worker = TRUE)
    state$completed <- NULL
    server_env$builder_project_open_worker_ready <- function(...) state$ready
    server_env$complete_builder_project_open <- function(actions = NULL) {
      state$completed <- actions
      invisible(TRUE)
    }
    server_env$builder_project_restore_dialog <- function(manifest) {
      state$choices <- manifest
      shiny::modalDialog("choices")
    }
    builder_project_restore(list(
      manifest = list(datasets = list()),
      root = "r",
      path = "p"
    ))
    actions <- list(ds1 = "reuse")
    expect_identical(
      wait_for_builder_project_open_worker(actions, state$ready),
      "waiting"
    )
    builder_responsiveness_run_later(function() FALSE, timeout = 0.6)
    expect_null(state$completed)
    expect_false(is.null(isolate(builder_project_open_wait())))

    state$ready <- list(protocol = TRUE, worker = TRUE)
    expect_true(builder_responsiveness_run_later(
      function() !is.null(state$completed)
    ))
    expect_identical(state$completed, actions)
    expect_null(isolate(builder_project_open_wait()))

    ## Cancelling the wait returns to the choices, selections kept.
    state$ready <- list(protocol = FALSE, worker = TRUE)
    state$completed <- NULL
    wait_for_builder_project_open_worker(actions, state$ready)
    session$setInputs(cancel_builder_project_open_wait = 1)
    expect_null(isolate(builder_project_open_wait()))
    builder_responsiveness_run_later(function() FALSE, timeout = 0.6)
    expect_null(state$completed)
    expect_false(is.null(state$choices))
  })

  source <- paste(
    readLines(
      builder_test_inst_path("builder", "server", "project.R"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_false(grepl("confirm the dataset choices again", source, fixed = TRUE))
  expect_match(
    source,
    "return(wait_for_builder_project_open_worker(actions, ready))",
    fixed = TRUE
  )
})

builder_responsiveness_project <- function(
  runtime,
  root,
  ids = c("ds1", "ds2")
) {
  manifest <- runtime$builder_project_new_manifest(root, "Warm writes")
  manifest$datasets <- lapply(ids, function(id) {
    runtime$builder_project_dataset_record(
      list(
        id = id,
        revision = 1L,
        settings = list(name = id, overview_point_size = 3),
        acknowledgements = character(),
        spatial_drafts = list()
      ),
      source = list(
        kind = "missing",
        path = NULL,
        fingerprint = list(md5 = strrep("a", 32L))
      ),
      root = root
    )
  })
  manifest
}

test_that("configuration digests are memoized by the value they hash", {
  runtime <- builder_responsiveness_runtime()
  entry <- list(
    id = "ds1",
    revision = 1L,
    settings = list(name = "First", overview_point_size = 3),
    acknowledgements = character(),
    spatial_drafts = list()
  )
  memo <- new.env(parent = emptyenv())
  plain <- runtime$builder_project_configuration_digest(entry, memo = NULL)

  expect_identical(
    runtime$builder_project_configuration_digest(entry, memo = memo),
    plain
  )
  expect_length(ls(memo), 1L)

  ## The second call is answered from the memo, not by serializing again.
  assign(ls(memo), "memoized", envir = memo)
  expect_identical(
    runtime$builder_project_configuration_digest(entry, memo = memo),
    "memoized"
  )

  ## Any change to the hashed value is a different key.
  entry$settings$name <- "Renamed"
  renamed <- runtime$builder_project_configuration_digest(entry, memo = memo)
  expect_false(identical(renamed, "memoized"))
  expect_identical(
    renamed,
    runtime$builder_project_configuration_digest(entry, memo = NULL)
  )
})

test_that("a warm writer verifies unchanged sidecars once across Saves", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  manifest <- builder_responsiveness_project(runtime, root)
  cache <- new.env(parent = emptyenv())
  first <- runtime$builder_project_write(
    manifest,
    root,
    verification_cache = cache
  )

  reads <- new.env(parent = emptyenv())
  reads$n <- 0L
  read_configuration <- runtime$builder_project_read_configuration_file
  runtime$builder_project_read_configuration_file <- function(...) {
    reads$n <- reads$n + 1L
    read_configuration(...)
  }
  second <- runtime$builder_project_write(
    first$manifest,
    root,
    expected_revision = first$manifest$project$revision,
    verification_cache = cache
  )
  expect_identical(
    second$manifest$project$revision,
    first$manifest$project$revision + 1L
  )
  expect_identical(reads$n, 0L)

  ## Without the shared cache every integrity pass reads every sidecar.
  runtime$builder_project_write(
    second$manifest,
    root,
    expected_revision = second$manifest$project$revision
  )
  expect_gt(reads$n, 0L)

  ## A rewritten sidecar has a new stat identity and is read and rejected.
  config <- file.path(root, second$manifest$datasets[[1L]]$configuration$path)
  bytes <- readBin(config, "raw", file.size(config))
  text <- rawToChar(bytes)
  writeBin(charToRaw(sub("\"ds1\"", "\"dsX\"", text, fixed = TRUE)), config)
  reads$n <- 0L
  expect_error(
    runtime$builder_project_write(
      second$manifest,
      root,
      expected_revision = second$manifest$project$revision + 1L,
      verification_cache = cache
    ),
    "incomplete|integrity"
  )
  expect_gt(reads$n, 0L)
})

test_that("the writer is readied before the first Save and per Project", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    withr::local_options(CerebroNexus.builder.project_worker_prewarm = TRUE)
    server_env <- environment(builder_project_prewarm_workers)
    calls <- new.env(parent = emptyenv())
    calls$warmups <- list()
    server_env$builder_project_worker_task_call <- function(
      worker,
      task,
      args
    ) {
      if ("root" %in% names(args)) {
        calls$warmups[[length(calls$warmups) + 1L]] <- args$root %||% "<none>"
      }
      list(is_alive = function() FALSE)
    }
    project <- function(root, id) {
      list(root = root, manifest = list(project = list(id = id)))
    }

    ## Nothing to save yet: no worker is started.
    builder_project_prewarm_workers(NULL, has_datasets = FALSE)
    expect_length(calls$warmups, 0L)
    ## Datasets but no Project: ready the writer for the Save that creates it.
    builder_project_prewarm_workers(NULL, has_datasets = TRUE)
    builder_project_prewarm_workers(NULL, has_datasets = TRUE)
    expect_identical(calls$warmups, list("<none>"))
    calls$warmups <- list()

    builder_project_prewarm_workers(project("C:/p1", "a"))
    builder_project_prewarm_workers(project("C:/p1", "a"))
    expect_identical(calls$warmups, list("C:/p1"))

    ## A restarted writer lost its caches; ready it again.
    builder_project_writer$starts <- builder_project_writer$starts + 1L
    builder_project_prewarm_workers(project("C:/p1", "a"))
    builder_project_prewarm_workers(project("C:/p2", "b"))
    expect_identical(calls$warmups, list("C:/p1", "C:/p1", "C:/p2"))
    builder_project_prewarm_workers(NULL, has_datasets = FALSE)
    expect_length(calls$warmups, 3L)
  })
})

test_that("a new configuration is verified once, not again after its rename", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  cache <- new.env(parent = emptyenv())
  reads <- new.env(parent = emptyenv())
  reads$n <- 0L
  read_configuration <- runtime$builder_project_read_configuration_file
  runtime$builder_project_read_configuration_file <- function(...) {
    reads$n <- reads$n + 1L
    read_configuration(...)
  }
  manifest <- runtime$builder_project_new_manifest(root, "First Save")
  manifest$datasets <- lapply(c("ds1", "ds2"), function(id) {
    runtime$builder_project_dataset_record(
      list(
        id = id,
        revision = 1L,
        settings = list(name = id),
        acknowledgements = character(),
        spatial_drafts = list()
      ),
      source = list(kind = "missing", path = NULL),
      root = root,
      verification_cache = cache
    )
  })
  ## One read per sidecar: its staged part file.
  expect_identical(reads$n, 2L)

  reads$n <- 0L
  written <- runtime$builder_project_write(
    manifest,
    root,
    verification_cache = cache
  )
  expect_identical(reads$n, 0L)
  expect_true(
    runtime$builder_project_manifest_integrity(written$manifest, root)$valid
  )

  ## A sidecar changed after it was written is read again and rejected.
  config <- file.path(root, written$manifest$datasets[[1L]]$configuration$path)
  text <- rawToChar(readBin(config, "raw", file.size(config)))
  writeBin(charToRaw(sub("\"ds1\"", "\"dsX\"", text, fixed = TRUE)), config)
  reads$n <- 0L
  status <- runtime$builder_project_manifest_integrity(
    written$manifest,
    root,
    verification_cache = cache
  )
  expect_false(status$valid)
  expect_gt(reads$n, 0L)

  ## No carry when the target is not the verified file.
  staged <- list(
    md5 = strrep("a", 32L),
    file_id = "elsewhere",
    bytes = 1,
    modified_at = "x"
  )
  expect_false(runtime$builder_project_carry_configuration_verification(
    config,
    list(config = list(), digest = "d", contract_version = 3L),
    staged,
    list(id = "ds1", revision = 1L),
    "d",
    cache
  ))
})

test_that("a manual Save keeps the workspace editable unless it stages attachments", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(request_builder_project_save)
    started <- new.env(parent = emptyenv())
    server_env$start_builder_project_save_request <- function(...) {
      started$args <- list(...)
      invisible(TRUE)
    }
    jobs <- new.env(parent = emptyenv())
    jobs$value <- list()
    server_env$builder_project_attachment_jobs <- function(entries, root) {
      if (inherits(jobs$value, "condition")) {
        stop(jobs$value)
      }
      jobs$value
    }
    builder_project(list(
      root = "project-root",
      path = "project-root/builder-project.json",
      name = "P",
      manifest = list(
        project = list(id = "p1", revision = 1L),
        datasets = list()
      )
    ))
    phase_after <- function(...) {
      builder_project_operation_phase("idle")
      request_builder_project_save(...)
      isolate(builder_project_operation_phase())
    }

    ## Nothing to stage: the manifest is written in the background.
    expect_identical(phase_after(show_actions = TRUE), "autosaving")
    expect_true(started$args$background)
    expect_true(isTRUE(builder_capabilities()$edit_dataset))

    ## New attachments are staged under the lock, decided before the request
    ## runs so an edit in between is never accepted and then refused.
    jobs$value <- list(list(entry_id = "ds1"))
    expect_identical(phase_after(show_actions = TRUE), "saving")
    jobs$value <- simpleError("source missing")
    expect_identical(phase_after(show_actions = TRUE), "saving")
    jobs$value <- list()
    expect_identical(
      phase_after(show_actions = TRUE, background = FALSE),
      "saving"
    )
  })
})

test_that("staging attachments unlocks the workspace again for the manifest write", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(start_builder_project_table_asset_save)
    calls <- new.env(parent = emptyenv())
    server_env$builder_project_worker_task_call <- function(...) {
      list(is_alive = function() FALSE, get_result = function() list("staged"))
    }
    server_env$builder_project_attachment_results_match <- function(
      jobs,
      results
    ) {
      TRUE
    }
    server_env$builder_project_table_save_signature <- function(
      project,
      entries
    ) {
      "sig"
    }
    server_env$builder_project_attachment_jobs <- function(entries, root) {
      list("job")
    }
    server_env$complete_builder_project_save_request <- function(...) {
      calls$phase <- isolate(builder_project_operation_phase())
      invisible(TRUE)
    }
    builder_project(list(root = "project-root", manifest = list()))

    for (background in c(TRUE, FALSE)) {
      calls$phase <- NULL
      builder_project_operation_phase("saving")
      start_builder_project_table_asset_save(
        list("job"),
        "sig",
        show_actions = TRUE,
        materialize = TRUE,
        notify = FALSE,
        after = NULL,
        background = background
      )
      expect_true(builder_responsiveness_run_later(function() {
        !is.null(calls$phase)
      }))
      expect_identical(
        calls$phase,
        if (background) "autosaving" else "saving",
        info = background
      )
    }
  })
})

test_that("a Save adopts the staged attachment copies into the workspace", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(save_builder_project_state)
    entry <- list(
      id = "ds1",
      revision = 0L,
      snapshot = list(
        path = "/private/ds1",
        owner_token = "owner",
        object_md5 = strrep("a", 32L)
      ),
      profile = list(marker = "a", extras = list()),
      settings = list(
        name = "A",
        tables = list(t1 = list(source_path = "C:/session/t.csv"))
      )
    )
    use_state_only_fixture(list(entry))
    staged <- entry
    staged$settings$tables$t1 <- list(
      source_path = "project-root/attachments/ds1/a",
      project_asset = list(path = "attachments/ds1/a")
    )
    project <- list(
      root = "project-root",
      path = "project-root/builder-project.json",
      name = "P",
      manifest = list(
        project = list(id = "p1", name = "P", revision = 1L),
        datasets = list()
      )
    )
    builder_project(project)
    pending <- new.env(parent = emptyenv())
    server_env$builder_project_apply_attachment_results <- function(
      entries,
      results,
      root
    ) {
      list(staged)
    }
    server_env$builder_project_manifest_context <- function(...) list()
    server_env$start_builder_project_manifest_write <- function(
      manifest,
      project,
      complete,
      ...
    ) {
      pending$complete <- complete
      invisible(TRUE)
    }

    save_builder_project_state(
      materialize = FALSE,
      notify = FALSE,
      manage_lifecycle = FALSE,
      table_asset_results = list("staged"),
      on_complete = function(ok) pending$ok <- ok
    )
    pending$complete(list(
      manifest = project$manifest,
      path = project$path,
      built = list(entries = list(staged), jobs = list())
    ))

    ## The workspace now refers to the Project copy, so the next Save has no
    ## attachment to stage again.
    expect_true(isTRUE(pending$ok))
    expect_identical(
      isolate(sets())[[1L]]$settings$tables$t1$project_asset$path,
      "attachments/ds1/a"
    )
  })
})

test_that("two copies of the same source never share a partial file", {
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  project <- file.path(root, "project")
  dir.create(project)
  source <- file.path(root, "upload", "source.rds")
  dir.create(dirname(source))
  writeBin(as.raw(seq_len(200000L) %% 251L), source)
  md5 <- unname(tools::md5sum(source))
  entry <- list(
    id = "ds1",
    path = source,
    filename = "sample.rds",
    snapshot = list(source_fingerprint = paste0("builder-snapshot-v2:", md5))
  )
  first <- runtime$builder_project_source_job(entry, project)
  second <- runtime$builder_project_source_job(entry, project)
  expect_identical(first$target, second$target)
  expect_false(identical(first$part, second$part))
  expect_identical(dirname(first$part), dirname(first$target))

  ## The second copy is still writing its own part when the first publishes.
  interleaved <- NULL
  result <- runtime$builder_project_copy_source_job(
    first,
    progress = function(bytes) {
      if (is.null(interleaved)) {
        interleaved <<- runtime$builder_project_copy_source_job(second)
      }
    }
  )
  expect_identical(interleaved$status, "ready")
  expect_identical(result$status, "ready")
  expect_identical(result$path, interleaved$path)
  expect_identical(unname(tools::md5sum(result$path)), md5)
  expect_false(file.exists(first$part))
  expect_false(file.exists(second$part))
})

test_that("a Save drops datasets removed in this session", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    server_env <- environment(builder_project_session_removed_ids)
    server_env$sets <- function(value) list(list(id = "ds1"))
    builder_project(list(
      root = withr::local_tempdir(),
      path = "builder-project.json",
      name = "P",
      manifest = list(
        schema_version = 3L,
        project = list(id = "p1", name = "P", revision = 2L),
        datasets = lapply(
          c("ds1", "ds2", "ds3", "ds4"),
          function(id) list(id = id)
        )
      )
    ))
    ## ds3 was skipped at Open and ds4 is still loading; ds2 was removed.
    builder_project_skipped_ids("ds3")
    builder_project_pending_entries(list(ds4 = list(id = "ds4")))
    expect_identical(builder_project_session_removed_ids(), "ds2")
    expect_identical(builder_project_manifest_context()$removed_ids, "ds2")
  })
})

test_that("a Project copy changed after Save is reported as such on load", {
  skip_if_not_installed("shiny")
  app_env <- builder_responsiveness_app()

  shiny::testServer(app_env$server, {
    builder_project_pending_entries(list(
      ds1 = list(id = "ds1", source = list(kind = "managed")),
      ds2 = list(id = "ds2", source = list(kind = "external"))
    ))
    md5_error <- "The copied file does not match its saved MD5."
    expect_match(
      builder_project_restore_error_message("ds1", md5_error),
      "changed outside the Builder",
      fixed = TRUE
    )
    expect_identical(
      builder_project_restore_error_message("ds2", md5_error),
      md5_error
    )
    expect_identical(
      builder_project_restore_error_message("ds1", "Out of memory."),
      "Out of memory."
    )
    expect_identical(
      builder_project_restore_error_message("ds9", md5_error),
      md5_error
    )
  })
})

test_that("a session callback that runs after the page closed does not stop the Builder", {
  program <- parse(
    builder_test_inst_path("builder", "server", "foundation.R"),
    keep.source = FALSE,
    encoding = "UTF-8"
  )
  definition <- Filter(
    function(expression) {
      is.call(expression) &&
        identical(expression[[1L]], as.name("<-")) &&
        identical(expression[[2L]], as.name("builder_later"))
    },
    as.list(program)
  )
  expect_length(definition, 1L)
  env <- new.env(parent = globalenv())
  eval(definition[[1L]], envir = env)

  ## What Shiny raises when a callback reads a reactive of a closed session.
  destroyed <- structure(
    class = c("shiny.destroyed.error", "error", "condition"),
    list(message = "its module session has been destroyed", call = NULL)
  )
  ran <- FALSE
  env$builder_later(function() {
    ran <<- TRUE
    stop(destroyed)
  })
  expect_no_error(later::run_now(0.5))
  expect_true(ran)

  env$builder_later(function() stop("a real failure"))
  expect_error(later::run_now(0.5), "a real failure")
})

test_that("an Open that works clears the error of an earlier failed Open", {
  skip_if_not_installed("shiny")
  runtime <- builder_responsiveness_runtime()
  root <- withr::local_tempdir()
  written <- runtime$builder_project_write(
    runtime$builder_project_new_manifest(root),
    root,
    expected_revision = 0L
  )
  app_env <- builder_responsiveness_app()
  shiny::testServer(app_env$server, {
    shown <- character()
    cleared <- character()
    fn_env <- environment(complete_builder_project_open)
    assign(
      "builder_notify",
      function(type, message, target = NULL, id = NULL, ...) {
        if (identical(type, "error")) {
          shown <<- c(shown, id %||% "<none>")
        }
        invisible(TRUE)
      },
      envir = fn_env
    )
    assign(
      "builder_notify_clear",
      function(id, ...) {
        cleared <<- c(cleared, as.character(id))
        invisible(TRUE)
      },
      envir = fn_env
    )

    builder_project_fail_open(
      isolate(builder_project_open_generation()),
      simpleError("The project file is damaged and cannot be read.")
    )
    expect_identical(shown, "builder-project-open-error")
    expect_false("builder-project-open-error" %in% cleared)

    builder_project_restore(list(
      path = written$path,
      root = root,
      manifest = written$manifest,
      manifest_fingerprint = runtime$builder_project_file_metadata(written$path)
    ))
    expect_true(isTRUE(complete_builder_project_open(list())))
    expect_true("builder-project-open-error" %in% cleared)
    expect_identical(isolate(builder_project())$root, root)
  })
})
