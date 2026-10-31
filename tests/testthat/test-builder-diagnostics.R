builder_diagnostics_test_runtime <- function() {
  runtime <- new.env(parent = globalenv())
  runtime$`%||%` <- function(left, right) {
    if (is.null(left)) right else left
  }
  sys.source(
    builder_profile_inst_path("builder", "diagnostics.R"),
    envir = runtime
  )
  runtime
}

test_that("subprocess diagnostics retain bounded stdout and stderr tails", {
  runtime <- builder_diagnostics_test_runtime()
  streams <- new.env(parent = emptyenv())
  streams$stdout <- list(charToRaw("abcdef"), charToRaw("ghij"))
  streams$stderr <- list(charToRaw("error-one"), raw())
  take <- function(name) {
    values <- streams[[name]]
    value <- values[[1L]]
    streams[[name]] <- values[-1L]
    value
  }
  process <- list(
    read_output_bytes = function() take("stdout"),
    read_error_bytes = function() take("stderr"),
    get_pid = function() 123L,
    get_exit_status = function() 17L
  )

  capture <- runtime$builder_diagnostics_capture(limit = 8L)
  capture <- runtime$builder_diagnostics_drain(process, capture)
  capture <- runtime$builder_diagnostics_drain(process, capture)

  expect_identical(rawToChar(capture$stdout), "cdefghij")
  expect_identical(rawToChar(capture$stderr), "rror-one")
  expect_true(capture$stdout_truncated)
  expect_true(capture$stderr_truncated)
  expect_identical(capture$pid, 123L)
})

test_that("worker transport output seeds settlement diagnostics", {
  runtime <- builder_diagnostics_test_runtime()
  capture <- runtime$builder_diagnostics_capture(limit = 32L)
  capture <- runtime$builder_diagnostics_merge_transport(
    capture,
    list(
      stdout = "worker-stdout",
      stderr = "worker-stderr",
      stdout_truncated = TRUE,
      stderr_truncated = FALSE
    )
  )

  expect_identical(rawToChar(capture$stdout), "worker-stdout")
  expect_identical(rawToChar(capture$stderr), "worker-stderr")
  expect_true(capture$stdout_truncated)
  expect_false(capture$stderr_truncated)
})

test_that("failed release diagnostics preserve result, trace, and output", {
  runtime <- builder_diagnostics_test_runtime()
  root <- withr::local_tempdir()
  control <- file.path(root, ".cbr-control")
  dir.create(control, mode = "0700")
  release <- list(
    handle = list(
      control = control,
      build_id = "build-diagnostics"
    )
  )
  capture <- runtime$builder_diagnostics_capture(limit = 64L)
  capture$stdout <- charToRaw("worker stdout\n")
  capture$stderr <- charToRaw("worker stderr\n")
  capture$pid <- 321L
  settlement_error <- simpleError("settlement failed", call = quote(publish()))
  result <- list(
    worker_result = list(
      state = "failure",
      error = "original build failure",
      diagnostics = list(
        export = list(
          class = c("simpleError", "error", "condition"),
          message = "original build failure",
          call = "export_data()",
          calls = c("builder_execute_plan()", "export_data()")
        )
      )
    ),
    worker_transport = list(
      stdout = "worker stdout\n",
      stderr = "worker stderr\n",
      error_condition = NULL,
      response = list(
        error = "original build failure",
        diagnostics = list(build = list(message = "original build failure"))
      )
    ),
    settlement_result = settlement_error,
    process = list(pid = 321L, exit_status = 1L)
  )
  process <- list(
    get_pid = function() 321L,
    get_exit_status = function() 1L,
    get_start_time = function() NULL,
    get_end_time = function() NULL
  )

  path <- runtime$builder_diagnostics_persist_release(
    release,
    result,
    capture = capture,
    process = process
  )

  expect_true(dir.exists(path))
  expect_setequal(
    list.files(path),
    c("metadata.rds", "result.rds", "stderr.log", "stdout.log", "trace.txt")
  )
  expect_identical(
    readRDS(file.path(path, "result.rds"))$worker_result$error,
    "original build failure"
  )
  expect_identical(
    readRDS(file.path(path, "result.rds"))$worker_transport$response$error,
    "original build failure"
  )
  metadata <- readRDS(file.path(path, "metadata.rds"))
  expect_identical(metadata$pid, 321L)
  expect_identical(metadata$exit_status, 1L)
  expect_match(
    paste(readLines(file.path(path, "trace.txt")), collapse = "\n"),
    "original build failure",
    fixed = TRUE
  )
  expect_identical(
    readChar(
      file.path(path, "stdout.log"),
      file.info(file.path(path, "stdout.log"))$size
    ),
    "worker stdout\n"
  )
})

test_that("diagnostics never recreate a retired release control", {
  runtime <- builder_diagnostics_test_runtime()
  root <- withr::local_tempdir()
  control <- file.path(root, ".retired-control")
  release <- list(handle = list(control = control, build_id = "build-retired"))

  path <- runtime$builder_diagnostics_persist_release(
    release,
    list(worker_result = NULL, settlement_result = simpleError("failed"))
  )

  expect_null(path)
  expect_false(dir.exists(control))
})

test_that("settlement diagnostics are retained only for terminal failures", {
  runtime <- builder_diagnostics_test_runtime()

  expect_false(runtime$builder_diagnostics_settlement_failed(list(
    ok = TRUE,
    value = list(state = "success", published = TRUE)
  )))
  expect_true(runtime$builder_diagnostics_settlement_failed(list(
    ok = TRUE,
    value = list(state = "success", published = FALSE)
  )))
  expect_false(runtime$builder_diagnostics_settlement_failed(list(
    ok = TRUE,
    value = list(state = "needs_decision")
  )))
  expect_true(runtime$builder_diagnostics_settlement_failed(list(
    ok = TRUE,
    value = list(state = "failure")
  )))
  expect_true(runtime$builder_diagnostics_settlement_failed(list(
    ok = FALSE,
    value = NULL
  )))
  expect_true(runtime$builder_diagnostics_settlement_failed(simpleError(
    "boom"
  )))
})

test_that("settlement drains and persists before dropping process ownership", {
  imports <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  finish_start <- grep(
    "finish_builder_release_settlement <- function",
    imports,
    fixed = TRUE
  )[[1L]]
  finish_end <- grep(
    "schedule_builder_release_settlement_poll <- function",
    imports,
    fixed = TRUE
  )[[1L]] -
    1L
  finish <- imports[finish_start:finish_end]
  persisted <- grep(
    "builder_persist_release_process_diagnostics(",
    finish,
    fixed = TRUE
  )[[1L]]
  forgot_process <- grep(
    "release_settlement_process(NULL)",
    finish,
    fixed = TRUE
  )[[1L]]
  forgot_context <- grep(
    "release_settlement_context(NULL)",
    finish,
    fixed = TRUE
  )[[1L]]

  expect_lt(persisted, forgot_process)
  expect_lt(persisted, forgot_context)
  expect_match(
    paste(imports, collapse = "\n"),
    "context$diagnostics <- drained",
    fixed = TRUE
  )
  expect_match(
    paste(imports, collapse = "\n"),
    "worker_result = value",
    fixed = TRUE
  )
  expect_match(
    paste(imports, collapse = "\n"),
    ".abort = abort_with_diagnostics",
    fixed = TRUE
  )
  expect_match(
    paste(imports, collapse = "\n"),
    "runtime$.builder_release_atomic_directory(",
    fixed = TRUE
  )
  expect_match(
    paste(finish, collapse = "\n"),
    "unlink(pending, recursive = FALSE, force = TRUE)",
    fixed = TRUE
  )
})

test_that("abnormal cancellation persists before dropping process ownership", {
  imports <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  cancel_start <- grep(
    "finish_builder_release_cancellation <- function",
    imports,
    fixed = TRUE
  )[[1L]]
  cancel_end <- grep(
    "poll_builder_release_settlement <- function",
    imports,
    fixed = TRUE
  )[[1L]] -
    1L
  cancellation <- imports[cancel_start:cancel_end]
  abort_start <- grep("if (!isTRUE(aborted))", cancellation, fixed = TRUE)[[1L]]
  warning_start <- grep(
    "if (.builder_diagnostics_text(context$error %||% NULL))",
    cancellation,
    fixed = TRUE
  )[[1L]]
  abort_failure <- cancellation[abort_start:(warning_start - 1L)]
  warning_failure <- cancellation[warning_start:length(cancellation)]

  expect_lt(
    grep(
      "builder_persist_release_process_diagnostics(",
      abort_failure,
      fixed = TRUE
    )[[1L]],
    grep("release_settlement_process(NULL)", abort_failure, fixed = TRUE)[[1L]]
  )
  expect_lt(
    grep(
      "builder_persist_release_process_diagnostics(",
      warning_failure,
      fixed = TRUE
    )[[1L]],
    grep("release_settlement_process(NULL)", warning_failure, fixed = TRUE)[[
      1L
    ]]
  )
})

test_that("worker transport failure persists before synchronous release abort", {
  imports <- readLines(
    builder_profile_inst_path("builder", "server", "imports.R"),
    warn = FALSE
  )
  failure_start <- grep(
    "if (!is.null(got$result$error))",
    imports,
    fixed = TRUE
  )[[1L]]
  completion_lines <- grep(
    "completed <- builder_protocol_complete(",
    imports,
    fixed = TRUE
  )
  failure_end <- completion_lines[completion_lines > failure_start][[1L]] - 1L
  failure <- imports[failure_start:failure_end]

  persisted <- grep(
    "builder_persist_worker_transport_failure(",
    failure,
    fixed = TRUE
  )[[1L]]
  aborted <- grep("abort_release_result(", failure, fixed = TRUE)[[1L]]
  released <- grep("active_release(NULL)", failure, fixed = TRUE)[[1L]]
  restarted <- grep("restart_worker_protocol(", failure, fixed = TRUE)[[1L]]
  restored <- tail(grep("result(release_result)", failure, fixed = TRUE), 1L)

  expect_lt(persisted, aborted)
  expect_lt(aborted, released)
  expect_lt(restarted, restored)
  expect_match(
    paste(imports, collapse = "\n"),
    "worker_transport = context$worker_transport",
    fixed = TRUE
  )
  expect_match(
    paste(imports, collapse = "\n"),
    "builder_diagnostics_merge_transport(",
    fixed = TRUE
  )

  finish_start <- grep(
    "finish_builder_release_settlement <- function",
    imports,
    fixed = TRUE
  )[[1L]]
  finish_end <- grep(
    "schedule_builder_release_settlement_poll <- function",
    imports,
    fixed = TRUE
  )[[1L]] -
    1L
  finish <- imports[finish_start:finish_end]
  persisted_before_abort <- grep(
    "builder_persist_release_process_diagnostics(",
    finish,
    fixed = TRUE
  )[[1L]]
  deferred_abort <- grep(
    "builder_coordinator_abort(context$release$handle)",
    finish,
    fixed = TRUE
  )[[1L]]
  expect_lt(persisted_before_abort, deferred_abort)

  start_start <- grep(
    "start_builder_release_settlement <- function",
    imports,
    fixed = TRUE
  )[[1L]]
  start_end <- grep(
    "stop_builder_release_settlement <- function",
    imports,
    fixed = TRUE
  )[[1L]] -
    1L
  start <- imports[start_start:start_end]
  expect_match(
    paste(start, collapse = "\n"),
    "abort_required = TRUE",
    fixed = TRUE
  )
  expect_false(any(grepl(
    "builder_coordinator_abort(release$handle)",
    start,
    fixed = TRUE
  )))
})

test_that("callr output is drained before a pipe can hide the failure", {
  skip_if_not_installed("callr")
  runtime <- builder_diagnostics_test_runtime()
  process <- callr::r_bg(
    function() {
      cat(strrep("o", 256L * 1024L))
      message("stderr sentinel")
      stop("remote diagnostic failure")
    },
    supervise = TRUE,
    stdout = "|",
    stderr = "|"
  )
  withr::defer(try(process$kill_tree(), silent = TRUE))
  capture <- runtime$builder_diagnostics_capture(limit = 512L * 1024L)
  deadline <- Sys.time() + 10
  repeat {
    capture <- runtime$builder_diagnostics_drain(process, capture)
    if (!process$is_alive() || Sys.time() >= deadline) {
      break
    }
    Sys.sleep(0.01)
  }
  capture <- runtime$builder_diagnostics_drain(process, capture)
  value <- tryCatch(process$get_result(), error = identity)

  expect_false(process$is_alive())
  expect_s3_class(value, "condition")
  expect_gte(length(capture$stdout), 256L * 1024L)
  expect_match(rawToChar(capture$stderr), "stderr sentinel", fixed = TRUE)
})
