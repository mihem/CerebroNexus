## -------------------------------------------------------------------------
## Byte-level import progress: worker records, queue state and rail rows.
## -------------------------------------------------------------------------

builder_import_progress_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  sys.source(
    builder_profile_inst_path("builder", "loading.R"),
    envir = runtime
  )
  sys.source(
    builder_profile_inst_path("builder", "ui", "dataset_rail.R"),
    envir = runtime
  )
  runtime
}

builder_import_progress_js_function <- function(js, name) {
  start <- regexpr(paste0("function ", name, "("), js, fixed = TRUE)[[1L]]
  if (start < 1L) {
    stop("JavaScript function not found: ", name, call. = FALSE)
  }
  chars <- strsplit(substring(js, start), "", fixed = TRUE)[[1L]]
  depth <- 0L
  for (index in seq_along(chars)) {
    if (identical(chars[[index]], "{")) {
      depth <- depth + 1L
    } else if (identical(chars[[index]], "}")) {
      depth <- depth - 1L
      if (depth == 0L) {
        return(paste0(chars[seq_len(index)], collapse = ""))
      }
    }
  }
  stop("JavaScript function is incomplete: ", name, call. = FALSE)
}

builder_import_progress_run_node <- function(functions, body) {
  testthat::skip_if(Sys.which("node") == "", "node not on PATH")
  runner <- tempfile(fileext = ".js")
  on.exit(unlink(runner), add = TRUE)
  writeLines(
    enc2utf8(c("'use strict';", functions, body)),
    runner,
    useBytes = TRUE
  )
  system2("node", runner, stdout = TRUE, stderr = TRUE)
}

test_that("progress records carry optional bounded byte counts", {
  runtime <- builder_import_progress_runtime()
  root <- withr::local_tempdir()
  path <- runtime$builder_import_progress_path(root, "ds1", 2L)

  expect_true(runtime$builder_import_progress_write(
    path,
    "preparing",
    2L,
    10,
    bytes_done = 25,
    bytes_total = 200
  ))
  record <- runtime$builder_import_progress_read(path, 2L)
  expect_identical(record$stage, "preparing")
  expect_identical(record$bytes_done, 25)
  expect_identical(record$bytes_total, 200)

  expect_true(runtime$builder_import_progress_write(path, "reading", 2L, 5))
  record <- runtime$builder_import_progress_read(path, 2L)
  expect_identical(names(record), c("stage", "generation", "elapsed_ms"))

  for (bytes in list(c(-1, 10), c(11, 10), c(1, 0), c(NA, 10), c(1, Inf))) {
    expect_error(
      runtime$builder_import_progress_write(
        path,
        "preparing",
        2L,
        1,
        bytes_done = bytes[[1L]],
        bytes_total = bytes[[2L]]
      ),
      "byte progress"
    )
  }
  saveRDS(
    list(stage = "preparing", generation = 2L, elapsed_ms = 1, extra = 1),
    path
  )
  expect_null(runtime$builder_import_progress_read(path, 2L))
})

test_that("byte progress callbacks are throttled to whole-percent changes", {
  runtime <- builder_import_progress_runtime()
  root <- withr::local_tempdir()
  path <- runtime$builder_import_progress_path(root, "ds1", 1L)
  writes <- list()
  runtime$builder_import_progress_write <- function(
    path,
    stage,
    generation,
    elapsed_ms,
    bytes_done = NULL,
    bytes_total = NULL
  ) {
    writes[[length(writes) + 1L]] <<- list(
      stage = stage,
      bytes_done = bytes_done
    )
    TRUE
  }
  progress <- runtime$.builder_import_progress_callback(
    path,
    1L,
    min_interval = 0
  )

  progress("preparing")
  progress("preparing", 10, 1000)
  progress("preparing", 12, 1000)
  progress("preparing", 25, 1000)
  progress("preparing", 1000, 1000)
  progress("preparing", 1000, 1000)

  expect_identical(
    vapply(writes, function(write) write$bytes_done %||% -1, numeric(1)),
    c(-1, 10, 25, 1000, 1000)
  )

  writes <- list()
  slow <- runtime$.builder_import_progress_callback(
    path,
    1L,
    min_interval = 3600
  )
  slow("preparing", 10, 1000)
  slow("preparing", 500, 1000)
  slow("preparing", 1000, 1000)
  # Within the interval only the first and the completing write land.
  expect_identical(
    vapply(writes, `[[`, numeric(1), "bytes_done"),
    c(10, 1000)
  )
})

test_that("queue entries keep the percent of the current stage only", {
  runtime <- builder_import_progress_runtime()
  queue <- runtime$builder_import_add(
    runtime$builder_import_queue(),
    runtime$builder_import_entry("ds1", "PBMC", list(kind = "file"))
  )
  queue <- runtime$builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- runtime$builder_import_transition(queue, "ds1", "preparing", 1L)

  updated <- runtime$builder_import_set_progress_bytes(
    queue,
    "ds1",
    1L,
    425,
    1000
  )
  expect_identical(updated$entries$ds1$progress_percent, 42L)
  expect_identical(updated$revision, queue$revision + 1L)
  same <- runtime$builder_import_set_progress_bytes(
    updated,
    "ds1",
    1L,
    429,
    1000
  )
  expect_identical(same, updated)
  stale <- runtime$builder_import_set_progress_bytes(
    updated,
    "ds1",
    2L,
    900,
    1000
  )
  expect_identical(stale, updated)
  cleared <- runtime$builder_import_set_progress_bytes(updated, "ds1", 1L)
  expect_null(cleared$entries$ds1$progress_percent)

  finished <- runtime$builder_import_transition(updated, "ds1", "ready", 1L)
  expect_null(finished$entries$ds1$progress_percent)
})

test_that("the dataset rail row shows the copy percent", {
  withr::local_package("shiny")
  runtime <- builder_import_progress_runtime()
  queue <- runtime$builder_import_add(
    runtime$builder_import_queue(),
    runtime$builder_import_entry("ds1", "PBMC", list(kind = "file"))
  )
  queue <- runtime$builder_import_transition(queue, "ds1", "reading", 1L)
  queue <- runtime$builder_import_transition(queue, "ds1", "preparing", 1L)
  plain <- runtime$builder_import_rail_row_model(queue$entries$ds1, 1L)
  queue <- runtime$builder_import_set_progress_bytes(queue, "ds1", 1L, 42, 100)
  model <- runtime$builder_import_rail_row_model(queue$entries$ds1, 1L)

  expect_identical(model$progress_percent, 42L)
  expect_false(identical(
    runtime$builder_import_rail_row_fingerprint(model),
    runtime$builder_import_rail_row_fingerprint(plain)
  ))
  html <- as.character(runtime$builder_import_rail_row_ui(model))
  expect_match(html, "Preparing dataset settings… 42%", fixed = TRUE)
  expect_match(html, 'role="progressbar"', fixed = TRUE)
  expect_match(html, 'aria-valuenow="42"', fixed = TRUE)

  plain_html <- as.character(runtime$builder_import_rail_row_ui(plain))
  expect_false(grepl("progressbar", plain_html, fixed = TRUE))

  cancelling <- runtime$builder_import_mark_cancelling(queue, "ds1")
  model <- runtime$builder_import_rail_row_model(cancelling$entries$ds1, 1L)
  expect_null(model$progress_percent)
})

test_that("snapshot copies report bytes through the import progress", {
  builder_profile_source_runtime(globalenv())
  builder_repo_source("io.R", local = globalenv())
  builder_repo_source("inspect.R", local = globalenv())
  builder_repo_source("adapters.R", local = globalenv())
  root <- withr::local_tempdir()
  path <- file.path(root, "pbmc.rds")
  saveRDS(SeuratObject::pbmc_small, path)
  names <- c(
    ".builder_objects",
    ".builder_snapshots",
    ".builder_snapshot_root",
    ".builder_object_ledger"
  )
  prior <- mget(names, envir = globalenv(), ifnotfound = list(NULL))
  withr::defer(
    for (name in names) {
      if (is.null(prior[[name]])) {
        if (exists(name, envir = globalenv(), inherits = FALSE)) {
          rm(list = name, envir = globalenv())
        }
      } else {
        assign(name, prior[[name]], envir = globalenv())
      }
    }
  )
  assign(".builder_objects", new.env(parent = emptyenv()), envir = globalenv())
  assign(
    ".builder_snapshots",
    new.env(parent = emptyenv()),
    envir = globalenv()
  )
  assign(".builder_snapshot_root", root, envir = globalenv())

  events <- list()
  progress <- function(stage, bytes_done = NULL, bytes_total = NULL) {
    events[[length(events) + 1L]] <<- list(
      stage = stage,
      done = bytes_done,
      total = bytes_total
    )
  }
  .builder_register_adapter(builder_seurat_file_adapter(path), "ds1", progress)

  stages <- vapply(events, `[[`, character(1), "stage")
  expect_identical(
    unique(stages),
    c("reading", "inspecting", "validating", "preparing")
  )
  bytes <- Filter(function(event) !is.null(event$done), events)
  expect_gte(length(bytes), 1L)
  last <- bytes[[length(bytes)]]
  expect_identical(last$stage, "preparing")
  expect_identical(last$done, file.size(path))
  expect_identical(last$total, file.size(path))

  # A stage-only callback keeps working.
  stage_only <- character()
  path_b <- file.path(root, "pbmc-b.rds")
  saveRDS(SeuratObject::pbmc_small, path_b)
  .builder_register_adapter(
    builder_seurat_file_adapter(path_b),
    "ds2",
    function(stage) stage_only <<- c(stage_only, stage)
  )
  expect_identical(
    stage_only,
    c("reading", "inspecting", "validating", "preparing")
  )
})

test_that("Shiny upload progress is mirrored into the client queue row", {
  app <- paste(
    readLines(builder_profile_inst_path("builder", "app.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(app, 'id = "dataset_files_progress"', fixed = TRUE)
  expect_match(
    app,
    'class = "shiny-file-input-progress builder-upload-transport-progress"',
    fixed = TRUE
  )

  js <- paste(
    readLines(
      builder_profile_inst_path("builder", "www", "builder.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )
  expect_match(js, "observeClientUploadProgress();", fixed = TRUE)
  functions <- vapply(
    c("clientQueueStatus", "clientUploadPercent"),
    builder_import_progress_js_function,
    character(1),
    js = js
  )
  out <- builder_import_progress_run_node(
    functions,
    c(
      "const bar = { style: { width: '' } };",
      "const results = [];",
      "results.push(clientUploadPercent(bar));",
      "bar.style.width = '42%'; results.push(clientUploadPercent(bar));",
      "bar.style.width = '99.6%'; results.push(clientUploadPercent(bar));",
      "bar.style.width = '140%'; results.push(clientUploadPercent(bar));",
      "results.push(clientQueueStatus({ state: 'uploading' }, 0));",
      "results.push(clientQueueStatus({ state: 'uploading', uploadPercent: 0 }, 0));",
      "results.push(clientQueueStatus({ state: 'awaiting_accept', uploadPercent: 37 }, 0));",
      "results.push(clientQueueStatus({ state: 'awaiting_accept', uploadPercent: 100 }, 0));",
      "results.push(clientQueueStatus({ state: 'awaiting_accept' }, 0));",
      "console.log(JSON.stringify(results));"
    )
  )
  expect_identical(
    jsonlite::fromJSON(out[[length(out)]], simplifyVector = FALSE),
    list(
      NULL,
      42L,
      100L,
      100L,
      "Uploading…",
      "Uploading… 0%",
      "Uploading… 37%",
      "Waiting for the server…",
      "Waiting for the server…"
    )
  )
})
