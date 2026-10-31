## Shared asynchronous runtime for short, read-only Builder tasks.

.builder_async <- new.env(parent = emptyenv())
.builder_async$n <- 2L
## Long jobs get their own slots so they never starve the short pool above.
## Project CRB generation runs a full dataset build per job.
.builder_async$pools <- list(crb = 1L)
.builder_async$memory <- 64L
.builder_async$tasks <- list()
.builder_async$started <- FALSE
.builder_async$stopping <- FALSE
.builder_async$poll_scheduled <- FALSE
.builder_async$poll_generation <- 0
.builder_async$poll_delay <- 0.02
.builder_async$spawn <- function(...) callr::r_bg(...)
.builder_async$callback_errors <- list()
.builder_async$last_callback_error <- NULL
.builder_async$last_stop <- list(
  stopped = TRUE,
  requested = FALSE,
  remaining = 0L,
  error = NULL
)

builder_async_evaluate <- function(.expr, .args) {
  list2env(.args, envir = environment())
  eval(.expr, envir = environment())
}

builder_async_active <- function() {
  Filter(
    function(task) task$state %in% c("queued", "running", "cancelling"),
    .builder_async$tasks
  )
}

.builder_async_forget <- function(task) {
  .builder_async$tasks <- Filter(
    function(candidate) !identical(candidate, task),
    .builder_async$tasks
  )
  invisible(task)
}

.builder_async_session_closed <- function(session) {
  if (is.null(session)) {
    return(FALSE)
  }
  tryCatch(
    is.function(session$isClosed) && isTRUE(session$isClosed()),
    error = function(error) FALSE
  )
}

.builder_async_run <- function(session, callback, value) {
  if (.builder_async_session_closed(session)) {
    return(invisible(FALSE))
  }
  if (!is.null(session)) {
    return(shiny::withReactiveDomain(session, callback(value)))
  }
  callback(value)
}

.builder_async_record_callback_error <- function(task, error, phase) {
  record <- list(
    phase = phase,
    message = conditionMessage(error),
    error = error,
    recorded_at = Sys.time()
  )
  task$callback_error <- record
  errors <- c(.builder_async$callback_errors, list(record))
  if (length(errors) > .builder_async$memory) {
    errors <- tail(errors, .builder_async$memory)
  }
  .builder_async$callback_errors <- errors
  .builder_async$last_callback_error <- record
  try(
    message(
      "Builder asynchronous ",
      phase,
      " callback failed: ",
      record$message
    ),
    silent = TRUE
  )
  invisible(FALSE)
}

.builder_async_request_poll <- function(delay = .builder_async$poll_delay) {
  if (
    isTRUE(.builder_async$stopping) ||
      isTRUE(.builder_async$poll_scheduled)
  ) {
    return(invisible(FALSE))
  }
  generation <- .builder_async$poll_generation
  .builder_async$poll_scheduled <- TRUE
  scheduled <- tryCatch(
    {
      later::later(
        function() {
          if (!identical(generation, .builder_async$poll_generation)) {
            return(invisible(FALSE))
          }
          builder_async_poll()
        },
        delay = max(0, as.numeric(delay))
      )
      TRUE
    },
    error = identity
  )
  if (inherits(scheduled, "condition")) {
    .builder_async$poll_scheduled <- FALSE
    stop(scheduled)
  }
  invisible(TRUE)
}

builder_async_schedule <- function() {
  if (isTRUE(.builder_async$stopping)) {
    return(invisible(FALSE))
  }
  active <- builder_async_active()
  pool_of <- function(task) {
    if (is.null(task$pool)) "short" else task$pool
  }
  pool_size <- function(pool) {
    if (identical(pool, "short")) {
      return(.builder_async$n)
    }
    size <- .builder_async$pools[[pool]]
    if (is.null(size)) 1L else as.integer(size)
  }
  starting <- list()
  queued <- Filter(function(task) identical(task$state, "queued"), active)
  for (pool in unique(vapply(queued, pool_of, character(1)))) {
    running <- sum(vapply(
      active,
      function(task) {
        identical(pool_of(task), pool) &&
          task$state %in% c("running", "cancelling")
      },
      logical(1)
    ))
    in_pool <- Filter(function(task) identical(pool_of(task), pool), queued)
    slots <- max(0L, pool_size(pool) - running)
    starting <- c(starting, head(in_pool, slots))
  }
  for (task in starting) {
    process <- tryCatch(
      .builder_async$spawn(
        builder_async_evaluate,
        list(task$expr, task$args),
        supervise = TRUE,
        ## Nothing reads the task's output; an undrained pipe blocks the child
        ## once it has written about 64 KB.
        stdout = NULL,
        stderr = NULL
      ),
      error = identity
    )
    if (inherits(process, "condition")) {
      task$error <- process
      task$state <- "failed"
    } else {
      task$process <- process
      task$state <- "running"
    }
  }
  pollable <- Filter(
    function(task) {
      task$state %in%
        c("queued", "running", "cancelling") ||
        (isTRUE(task$callbacks_registered) &&
          task$state %in% c("completed", "failed"))
    },
    .builder_async$tasks
  )
  if (length(pollable)) {
    .builder_async_request_poll()
  }
  invisible(TRUE)
}

builder_async_submit <- function(.expr, .args = list(), .pool = "short") {
  if (
    !is.character(.pool) ||
      length(.pool) != 1L ||
      is.na(.pool) ||
      (!identical(.pool, "short") && is.null(.builder_async$pools[[.pool]]))
  ) {
    stop("Unknown Builder background pool.", call. = FALSE)
  }
  .builder_async$tasks <- Filter(
    function(task) !task$state %in% c("cancelled", "delivered"),
    .builder_async$tasks
  )
  if (
    length(builder_async_active()) >= .builder_async$n + .builder_async$memory
  ) {
    stop("The Builder background queue is full.", call. = FALSE)
  }
  task <- new.env(parent = emptyenv())
  task$expr <- .expr
  task$args <- .args
  task$process <- NULL
  task$state <- "queued"
  task$error <- NULL
  task$result <- NULL
  task$session <- NULL
  task$on_fulfilled <- NULL
  task$on_rejected <- NULL
  task$callbacks_registered <- FALSE
  task$callback_error <- NULL
  task$cancel_started <- NULL
  task$cancel_deadline <- NULL
  task$cancel_error <- NULL
  task$pool <- .pool
  class(task) <- "builder_async_task"
  .builder_async$tasks[[length(.builder_async$tasks) + 1L]] <- task
  .builder_async$started <- TRUE
  builder_async_schedule()
  task
}

builder_async_then <- function(
  task,
  session,
  on_fulfilled,
  on_rejected
) {
  if (!inherits(task, "builder_async_task")) {
    stop("Expected a Builder asynchronous task.", call. = FALSE)
  }
  if (!is.function(on_fulfilled) || !is.function(on_rejected)) {
    stop("Builder asynchronous callbacks must be functions.", call. = FALSE)
  }
  task$session <- session
  task$on_fulfilled <- on_fulfilled
  task$on_rejected <- on_rejected
  task$callbacks_registered <- TRUE
  .builder_async_request_poll(delay = 0)
  invisible(task)
}

.builder_async_finish_running <- function(task) {
  alive <- tryCatch(task$process$is_alive(), error = identity)
  if (inherits(alive, "condition")) {
    task$error <- alive
    task$state <- "failed"
    return(TRUE)
  }
  if (isTRUE(alive)) {
    return(FALSE)
  }
  value <- tryCatch(task$process$get_result(), error = identity)
  if (inherits(value, "condition")) {
    task$error <- value
    task$state <- "failed"
  } else {
    task$result <- value
    task$state <- "completed"
  }
  TRUE
}

.builder_async_deliver <- function(task) {
  session <- task$session
  fulfilled <- task$on_fulfilled
  rejected <- task$on_rejected
  state <- task$state
  value <- if (identical(state, "failed")) task$error else task$result
  task$state <- "delivered"
  .builder_async_forget(task)

  if (.builder_async_session_closed(session)) {
    return(invisible(FALSE))
  }
  if (identical(state, "failed")) {
    return(tryCatch(
      .builder_async_run(session, rejected, value),
      error = function(error) {
        .builder_async_record_callback_error(task, error, "rejection")
      }
    ))
  }
  fulfilled_result <- tryCatch(
    .builder_async_run(session, fulfilled, value),
    error = identity
  )
  if (!inherits(fulfilled_result, "condition")) {
    return(fulfilled_result)
  }
  .builder_async_record_callback_error(
    task,
    fulfilled_result,
    "fulfillment"
  )
  tryCatch(
    .builder_async_run(session, rejected, fulfilled_result),
    error = function(error) {
      .builder_async_record_callback_error(task, error, "rejection")
    }
  )
}

builder_async_poll <- function() {
  .builder_async$poll_scheduled <- FALSE
  if (isTRUE(.builder_async$stopping)) {
    return(invisible(FALSE))
  }
  changed <- FALSE
  on.exit(
    {
      if (!isTRUE(.builder_async$stopping)) {
        active_before_schedule <- length(builder_async_active())
        .builder_async$poll_delay <- if (!active_before_schedule) {
          0.02
        } else if (changed) {
          0.02
        } else {
          min(0.2, max(0.02, .builder_async$poll_delay * 1.5))
        }
        try(builder_async_schedule(), silent = TRUE)
        if (length(builder_async_active())) {
          .builder_async_request_poll()
        } else {
          .builder_async$poll_delay <- 0.02
        }
      }
    },
    add = TRUE
  )
  for (task in .builder_async$tasks) {
    if (.builder_async_session_closed(task$session)) {
      if (task$state %in% c("queued", "running", "cancelling")) {
        builder_async_cancel(task)
      } else {
        task$state <- "delivered"
        .builder_async_forget(task)
      }
      changed <- TRUE
      next
    }
    if (identical(task$state, "running")) {
      changed <- .builder_async_finish_running(task) || changed
    } else if (identical(task$state, "cancelling")) {
      alive <- tryCatch(task$process$is_alive(), error = identity)
      if (identical(alive, FALSE)) {
        task$state <- "cancelled"
        .builder_async_forget(task)
        changed <- TRUE
      } else {
        if (inherits(alive, "condition")) {
          task$cancel_error <- conditionMessage(alive)
        } else if (!isTRUE(alive)) {
          task$cancel_error <- "The background task returned an invalid liveness state."
        } else {
          cancel_deadline <- if (is.null(task$cancel_deadline)) {
            Sys.time()
          } else {
            task$cancel_deadline
          }
          if (Sys.time() < cancel_deadline) {
            try(task$process$kill_tree(), silent = TRUE)
            try(task$process$kill(), silent = TRUE)
          } else {
            task$cancel_error <- paste(
              "The background task has not stopped yet;",
              "its process remains tracked for cleanup."
            )
          }
        }
      }
    }
  }

  cancelling <- Filter(
    function(task) identical(task$state, "cancelling"),
    .builder_async$tasks
  )
  if (!length(cancelling) && !isTRUE(.builder_async$last_stop$stopped)) {
    .builder_async$last_stop <- list(
      stopped = TRUE,
      requested = TRUE,
      remaining = 0L,
      error = NULL,
      reaped_at = Sys.time()
    )
  }

  ready <- Filter(
    function(task) {
      isTRUE(task$callbacks_registered) &&
        task$state %in% c("completed", "failed")
    },
    .builder_async$tasks
  )
  for (task in ready) {
    tryCatch(
      .builder_async_deliver(task),
      error = function(error) {
        .builder_async_record_callback_error(task, error, "delivery")
      }
    )
    changed <- TRUE
  }
  invisible(changed)
}

builder_async_cancel <- function(task) {
  if (
    !inherits(task, "builder_async_task") ||
      !task$state %in% c("queued", "running", "cancelling")
  ) {
    return(invisible(FALSE))
  }
  if (identical(task$state, "queued")) {
    task$state <- "cancelled"
    .builder_async_forget(task)
    builder_async_schedule()
    return(invisible(TRUE))
  }
  alive <- tryCatch(task$process$is_alive(), error = identity)
  if (isTRUE(alive)) {
    try(task$process$kill_tree(), silent = TRUE)
    try(task$process$wait(timeout = 1000L), silent = TRUE)
    alive <- tryCatch(task$process$is_alive(), error = identity)
    if (isTRUE(alive)) {
      try(task$process$kill(), silent = TRUE)
      try(task$process$wait(timeout = 250L), silent = TRUE)
      alive <- tryCatch(task$process$is_alive(), error = identity)
    }
  }
  if (!identical(alive, FALSE)) {
    task$state <- "cancelling"
    if (is.null(task$cancel_started)) {
      task$cancel_started <- Sys.time()
    }
    if (is.null(task$cancel_deadline)) {
      task$cancel_deadline <- Sys.time() + 10
    }
    task$cancel_error <- if (inherits(alive, "condition")) {
      conditionMessage(alive)
    } else {
      "The background task is still stopping."
    }
    .builder_async_request_poll(delay = 0.1)
    return(invisible(FALSE))
  }
  task$state <- "cancelled"
  .builder_async_forget(task)
  builder_async_schedule()
  invisible(TRUE)
}

builder_async_stop <- function() {
  .builder_async$poll_generation <- .builder_async$poll_generation + 1
  .builder_async$poll_scheduled <- FALSE
  active <- builder_async_active()
  if (!length(active)) {
    .builder_async$tasks <- list()
    .builder_async$started <- FALSE
    .builder_async$last_stop <- list(
      stopped = TRUE,
      requested = FALSE,
      remaining = 0L,
      error = NULL
    )
    return(invisible(FALSE))
  }
  .builder_async$stopping <- TRUE
  on.exit(
    {
      .builder_async$stopping <- FALSE
      if (
        any(vapply(
          .builder_async$tasks,
          function(task) identical(task$state, "cancelling"),
          logical(1)
        ))
      ) {
        .builder_async_request_poll(delay = 0.1)
      }
    },
    add = TRUE
  )
  lapply(active, builder_async_cancel)
  lingering <- Filter(
    function(task) identical(task$state, "cancelling"),
    .builder_async$tasks
  )
  .builder_async$tasks <- lingering
  .builder_async$started <- length(lingering) > 0L
  .builder_async$last_stop <- list(
    stopped = !length(lingering),
    requested = TRUE,
    remaining = length(lingering),
    error = if (length(lingering)) {
      "One or more background tasks are still stopping."
    } else {
      NULL
    }
  )
  invisible(!length(lingering))
}

shiny::onStop(builder_async_stop)
