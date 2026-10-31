## Best-effort diagnostics for Builder subprocesses.
##
## Process output is drained while the process is alive so a full pipe cannot
## stall terminal settlement.  Only a bounded tail is retained in memory.  A
## failed settlement writes that tail together with the original worker value,
## the settlement result, and the remote condition trace into the release's
## private control directory.  Diagnostic failures must never replace the
## error that caused them.

.builder_diagnostics_default_limit <- 1024L * 1024L

.builder_diagnostics_text <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

.builder_diagnostics_link <- function(path) {
  link <- tryCatch(Sys.readlink(path), error = function(error) "")
  length(link) == 1L && !is.na(link) && nzchar(link)
}

.builder_diagnostics_owner_only <- function(path) {
  if (identical(.Platform$OS.type, "windows")) {
    return(TRUE)
  }
  mode <- tryCatch(file.info(path)$mode, error = function(error) NA)
  length(mode) == 1L && !is.na(mode) && bitwAnd(as.integer(mode), 63L) == 0L
}

.builder_diagnostics_directory <- function(path) {
  if (.builder_diagnostics_link(path)) {
    return(FALSE)
  }
  if (dir.exists(path)) {
    return(.builder_diagnostics_owner_only(path))
  }
  if (file.exists(path)) {
    return(FALSE)
  }
  created <- tryCatch(
    dir.create(path, mode = "0700", showWarnings = FALSE),
    error = function(error) FALSE
  )
  isTRUE(created) &&
    dir.exists(path) &&
    !.builder_diagnostics_link(path) &&
    .builder_diagnostics_owner_only(path)
}

builder_diagnostics_capture <- function(
  limit = .builder_diagnostics_default_limit
) {
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      !is.finite(limit) ||
      limit < 1 ||
      limit != floor(limit)
  ) {
    stop(
      "The diagnostic output limit must be a positive integer.",
      call. = FALSE
    )
  }
  list(
    schema_version = 1L,
    limit = as.integer(min(limit, .Machine$integer.max)),
    stdout = raw(),
    stderr = raw(),
    stdout_truncated = FALSE,
    stderr_truncated = FALSE,
    read_errors = character(),
    pid = NULL
  )
}

.builder_diagnostics_append <- function(current, incoming, limit) {
  if (!is.raw(current)) {
    current <- raw()
  }
  if (!is.raw(incoming) || !length(incoming)) {
    return(list(value = current, truncated = FALSE))
  }
  combined <- c(current, incoming)
  truncated <- length(combined) > limit
  if (truncated) {
    combined <- tail(combined, limit)
  }
  list(value = combined, truncated = truncated)
}

builder_diagnostics_merge_transport <- function(
  capture = NULL,
  transport = NULL
) {
  if (!is.list(capture) || !identical(capture$schema_version, 1L)) {
    capture <- builder_diagnostics_capture()
  }
  if (!is.list(transport)) {
    return(capture)
  }
  for (stream in c("stdout", "stderr")) {
    value <- transport[[stream]]
    incoming <- tryCatch(
      {
        if (is.raw(value)) {
          value
        } else {
          charToRaw(paste0(as.character(value %||% character()), collapse = ""))
        }
      },
      error = function(error) raw()
    )
    appended <- .builder_diagnostics_append(
      capture[[stream]],
      incoming,
      capture$limit
    )
    capture[[stream]] <- appended$value
    truncated <- paste0(stream, "_truncated")
    capture[[truncated]] <- isTRUE(capture[[truncated]]) ||
      isTRUE(transport[[truncated]]) ||
      isTRUE(appended$truncated)
  }
  capture
}

.builder_diagnostics_method <- function(process, name) {
  method <- tryCatch(process[[name]], error = function(error) NULL)
  if (is.function(method)) method else NULL
}

builder_diagnostics_process_facts <- function(process) {
  if (is.null(process)) {
    return(list(pid = NULL, exit_status = NULL))
  }
  read_fact <- function(name) {
    method <- .builder_diagnostics_method(process, name)
    if (is.null(method)) {
      return(NULL)
    }
    tryCatch(method(), error = function(error) NULL)
  }
  list(
    pid = read_fact("get_pid"),
    exit_status = read_fact("get_exit_status"),
    started_at = read_fact("get_start_time"),
    ended_at = read_fact("get_end_time")
  )
}

builder_diagnostics_drain <- function(process, capture = NULL) {
  if (!is.list(capture) || !identical(capture$schema_version, 1L)) {
    capture <- builder_diagnostics_capture()
  }
  facts <- builder_diagnostics_process_facts(process)
  if (is.null(capture$pid) && !is.null(facts$pid)) {
    capture$pid <- facts$pid
  }
  for (stream in c("stdout", "stderr")) {
    process_stream <- if (identical(stream, "stdout")) "output" else "error"
    method <- .builder_diagnostics_method(
      process,
      paste0("read_", process_stream, "_bytes")
    )
    if (is.null(method)) {
      next
    }
    bytes <- tryCatch(method(), error = identity)
    if (inherits(bytes, "condition")) {
      capture$read_errors <- tail(
        unique(c(
          capture$read_errors,
          paste0(stream, ": ", conditionMessage(bytes))
        )),
        16L
      )
      next
    }
    appended <- .builder_diagnostics_append(
      capture[[stream]],
      bytes,
      capture$limit
    )
    capture[[stream]] <- appended$value
    truncated <- paste0(stream, "_truncated")
    capture[[truncated]] <- isTRUE(capture[[truncated]]) ||
      isTRUE(appended$truncated)
  }
  capture
}

builder_diagnostics_settlement_failed <- function(value) {
  if (inherits(value, "condition")) {
    return(TRUE)
  }
  if (
    !is.list(value) ||
      !is.logical(value$ok) ||
      length(value$ok) != 1L ||
      is.na(value$ok) ||
      !isTRUE(value$ok)
  ) {
    return(TRUE)
  }
  terminal <- value$value
  if (!is.list(terminal)) {
    return(TRUE)
  }
  if (identical(terminal$state, "needs_decision")) {
    return(FALSE)
  }
  !identical(terminal$state, "success") || !isTRUE(terminal$published)
}

.builder_diagnostics_trace_lines <- function(result) {
  worker_transport <- result$worker_transport
  condition <- if (inherits(result$settlement_result, "condition")) {
    result$settlement_result
  } else if (
    is.list(worker_transport) &&
      inherits(worker_transport$error_condition, "condition")
  ) {
    worker_transport$error_condition
  } else {
    NULL
  }
  worker_diagnostics <- list()
  if (
    is.list(result$worker_result) &&
      !is.null(result$worker_result$diagnostics)
  ) {
    worker_diagnostics$result <- result$worker_result$diagnostics
  }
  if (
    is.list(worker_transport) &&
      is.list(worker_transport$response) &&
      !is.null(worker_transport$response$diagnostics)
  ) {
    worker_diagnostics$transport_response <-
      worker_transport$response$diagnostics
  }
  lines <- character()
  if (!is.null(condition)) {
    lines <- c(
      lines,
      paste0("condition class: ", paste(class(condition), collapse = ", ")),
      paste0("condition message: ", conditionMessage(condition)),
      "",
      tryCatch(
        utils::capture.output(print(condition)),
        error = function(error) {
          paste0(
            "condition trace could not be rendered: ",
            conditionMessage(error)
          )
        }
      )
    )
  }
  if (length(worker_diagnostics)) {
    lines <- c(
      lines,
      if (length(lines)) {
        c("", "worker diagnostics:")
      } else {
        "worker diagnostics:"
      },
      tryCatch(
        utils::capture.output(str(worker_diagnostics, max.level = 4L)),
        error = function(error) {
          paste0(
            "worker diagnostics could not be rendered: ",
            conditionMessage(error)
          )
        }
      )
    )
  }
  if (!length(lines)) {
    lines <- "No condition trace was returned; inspect result.rds."
  }
  enc2utf8(lines)
}

.builder_diagnostics_write_raw <- function(value, path, truncated = FALSE) {
  marker <- if (isTRUE(truncated)) {
    charToRaw("[... earlier subprocess output truncated ...]\n")
  } else {
    raw()
  }
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(c(marker, if (is.raw(value)) value else raw()), connection)
  invisible(path)
}

builder_diagnostics_persist_release <- function(
  release,
  result,
  capture = NULL,
  process = NULL,
  kind = "settlement"
) {
  tryCatch(
    {
      handle <- if (is.list(release) && is.list(release$handle)) {
        release$handle
      } else {
        release
      }
      if (
        !is.list(handle) ||
          !.builder_diagnostics_text(handle$control %||% NULL) ||
          !.builder_diagnostics_text(handle$build_id %||% NULL)
      ) {
        return(NULL)
      }
      control <- handle$control
      parent <- dirname(control)
      if (
        !dir.exists(parent) ||
          .builder_diagnostics_link(parent) ||
          !dir.exists(control) ||
          .builder_diagnostics_link(control) ||
          !.builder_diagnostics_owner_only(control)
      ) {
        return(NULL)
      }
      diagnostics <- file.path(control, "diagnostics")
      if (!.builder_diagnostics_directory(diagnostics)) {
        return(NULL)
      }
      safe_kind <- gsub("[^A-Za-z0-9._-]", "-", as.character(kind)[[1L]])
      safe_id <- gsub(
        "[^A-Za-z0-9._-]",
        "-",
        substr(handle$build_id, 1L, 80L)
      )
      run <- tempfile(
        pattern = paste0(safe_kind, "-", safe_id, "-"),
        tmpdir = diagnostics
      )
      if (!.builder_diagnostics_directory(run)) {
        return(NULL)
      }
      capture <- if (
        is.list(capture) && identical(capture$schema_version, 1L)
      ) {
        capture
      } else {
        builder_diagnostics_capture()
      }
      .builder_diagnostics_write_raw(
        capture$stdout,
        file.path(run, "stdout.log"),
        capture$stdout_truncated
      )
      .builder_diagnostics_write_raw(
        capture$stderr,
        file.path(run, "stderr.log"),
        capture$stderr_truncated
      )
      saveRDS(result, file.path(run, "result.rds"), version = 2)
      writeLines(
        .builder_diagnostics_trace_lines(result),
        file.path(run, "trace.txt"),
        useBytes = TRUE
      )
      facts <- builder_diagnostics_process_facts(process)
      metadata <- list(
        schema_version = 1L,
        kind = safe_kind,
        build_id = handle$build_id,
        recorded_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
        pid = facts$pid %||% capture$pid,
        exit_status = facts$exit_status,
        started_at = facts$started_at,
        ended_at = facts$ended_at,
        stdout_truncated = isTRUE(capture$stdout_truncated),
        stderr_truncated = isTRUE(capture$stderr_truncated),
        read_errors = capture$read_errors %||% character()
      )
      saveRDS(metadata, file.path(run, "metadata.rds"), version = 2)
      normalizePath(run, winslash = "/", mustWork = TRUE)
    },
    error = function(error) NULL
  )
}
