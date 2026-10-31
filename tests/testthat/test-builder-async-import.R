test_that("worker progress records are private, atomic, and bounded", {
  loading_path <- builder_profile_inst_path("builder", "loading.R")
  sys.source(loading_path, envir = globalenv())
  root <- tempfile("builder-progress-")
  dir.create(root, mode = "0700")
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  path <- builder_import_progress_path(root, "dataset/private name", 4L)
  expect_true(startsWith(normalizePath(dirname(path)), normalizePath(root)))
  expect_false(grepl("private name", basename(path), fixed = TRUE))

  expect_true(builder_import_progress_write(
    path,
    stage = "inspecting",
    generation = 4L,
    elapsed_ms = 125.4
  ))
  record <- builder_import_progress_read(path, generation = 4L)
  expect_identical(record$stage, "inspecting")
  expect_identical(record$generation, 4L)
  expect_equal(record$elapsed_ms, 125.4)
  expect_false(any(grepl("private", unlist(record), fixed = TRUE)))
  expect_null(builder_import_progress_read(path, generation = 3L))

  expect_true(builder_import_progress_remove(path))
  expect_false(file.exists(path))
})

test_that("session cleanup removes only owned import coordination records", {
  loading_path <- builder_profile_inst_path("builder", "loading.R")
  sys.source(loading_path, envir = globalenv())
  root <- tempfile("builder-progress-cleanup-")
  dir.create(root, mode = "0700")
  withr::defer(unlink(root, recursive = TRUE, force = TRUE))

  progress <- builder_import_progress_path(root, "ds1", 1L)
  expect_true(builder_import_progress_write(progress, "reading", 1L, 4))
  transport_retained <- paste0(progress, ".transport-retained.rds")
  saveRDS(list(id = "ds1", retained_path = "retained"), transport_retained)
  transport_part <- file.path(root, ".transport-retained-interrupted.part")
  writeLines("interrupted atomic write", transport_part)
  unrelated <- file.path(root, "keep-me.rds")
  saveRDS("owned by another subsystem", unrelated)

  expect_identical(builder_import_progress_cleanup(root), 3L)
  expect_false(file.exists(progress))
  expect_false(file.exists(transport_retained))
  expect_false(file.exists(transport_part))
  expect_true(file.exists(unrelated))
})

test_that("a controlled slow importer runs outside the caller", {
  skip_if_not_installed("callr")
  app_env <- new.env(parent = globalenv())
  builder_dir <- builder_profile_inst_path("builder")
  withr::local_dir(builder_dir)
  sys.source("app.R", envir = app_env)

  expect_true(all(
    c(
      "progress_path",
      "import_generation",
      ".importer"
    ) %in%
      names(formals(app_env$builder_session_example))
  ))

  started <- app_env$builder_session_start(builder_dir)
  expect_null(started$error)
  worker <- started$worker
  withr::defer(try(app_env$builder_worker_stop(worker), silent = TRUE))

  protocol <- app_env$builder_request_protocol(worker$epoch)
  queued <- app_env$builder_enqueue(
    protocol,
    app_env$builder_command(
      "load",
      "slow",
      payload = list(kind = "load", import_generation = 1L)
    )
  )
  dispatched <- app_env$builder_protocol_dispatch(queued)
  gate <- tempfile("builder-import-gate-")
  progress <- app_env$builder_import_progress_path(
    worker$snapshot_root,
    "slow",
    1L
  )
  importer <- local({
    gate_path <- gate
    function(id, source, progress) {
      progress("reading")
      while (!file.exists(gate_path)) {
        Sys.sleep(0.01)
      }
      progress("inspecting")
      list(id = id, marker = "finished")
    }
  })

  dispatch_elapsed <- unname(system.time(app_env$builder_session_example(
    worker,
    "slow",
    "complete_viewer_data",
    dispatched$request,
    progress_path = progress,
    import_generation = 1L,
    .importer = importer
  ))[["elapsed"]])
  ## The importer waits for a gate that is still closed, so returning at all
  ## shows the dispatch did not run it in this process. The bound only keeps
  ## a regression from being mistaken for a slow machine.
  expect_lt(dispatch_elapsed, 5)

  first <- app_env$builder_session_poll(worker)
  worker <- first$worker
  expect_null(first$result)
  expect_true(worker$process$is_alive())

  deadline <- Sys.time() + 60
  repeat {
    stage <- app_env$builder_import_progress_read(progress, 1L)
    if (identical(stage$stage, "reading")) {
      break
    }
    if (Sys.time() >= deadline) {
      stop(
        "The slow importer did not publish its reading stage.",
        call. = FALSE
      )
    }
    Sys.sleep(0.01)
  }

  file.create(gate)
  withr::defer(unlink(gate, force = TRUE))
  deadline <- Sys.time() + 60
  repeat {
    completed <- app_env$builder_session_poll(worker)
    worker <- completed$worker
    if (!is.null(completed$result)) {
      break
    }
    if (Sys.time() >= deadline) {
      stop(
        "The controlled importer did not finish after its gate opened.",
        call. = FALSE
      )
    }
    Sys.sleep(0.01)
  }

  expect_null(completed$result$error)
  expect_identical(completed$result$value$value$marker, "finished")
  expect_identical(
    app_env$builder_import_progress_read(progress, 1L)$stage,
    "inspecting"
  )
  stopped <- app_env$builder_worker_stop(worker)
  expect_true(stopped$stopped)
})
