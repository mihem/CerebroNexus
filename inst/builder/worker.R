##----------------------------------------------------------------------------##
## Builder request protocol and isolated worker lifecycle.
##
## The Shiny process owns all durable session facts, including the immutable
## snapshot registry. A callr process only holds reopened working objects. It
## can therefore be killed at any time and reconstructed from those facts.
##----------------------------------------------------------------------------##

.builder_worker_scalar_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

.builder_worker_scalar_integer <- function(value) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value >= 0 &&
    value == floor(value)
}

.builder_worker_source_utf8 <- function(file, envir = globalenv()) {
  ofile <- normalizePath(file, winslash = "/", mustWork = TRUE)
  lines <- readLines(ofile, encoding = "UTF-8", warn = FALSE)
  if (length(lines)) {
    lines[[1L]] <- sub("^\ufeff", "", lines[[1L]])
  }
  expressions <- parse(
    text = lines,
    encoding = "UTF-8",
    keep.source = FALSE
  )
  value <- NULL
  for (index in seq_along(expressions)) {
    value <- eval(expressions[[index]], envir = envir)
  }
  invisible(value)
}
environment(.builder_worker_source_utf8) <- baseenv()

builder_worker_capability_registry <- function(loaders) {
  if (
    !is.list(loaders) || is.null(names(loaders)) || any(!nzchar(names(loaders)))
  ) {
    stop("Worker capability loaders must be a named list.", call. = FALSE)
  }
  if (length(loaders) && any(!vapply(loaders, is.function, logical(1)))) {
    stop("Every worker capability loader must be a function.", call. = FALSE)
  }
  registry <- new.env(parent = emptyenv())
  registry$loaders <- loaders
  registry$states <- setNames(rep("unloaded", length(loaders)), names(loaders))
  registry$errors <- setNames(vector("list", length(loaders)), names(loaders))
  class(registry) <- c("builder_worker_capabilities", "environment")
  registry
}

builder_worker_ensure_capability <- function(registry, name) {
  if (
    !inherits(registry, "builder_worker_capabilities") ||
      !.builder_worker_scalar_text(name) ||
      !name %in% names(registry$loaders)
  ) {
    stop("The requested worker capability is unavailable.", call. = FALSE)
  }
  state <- registry$states[[name]]
  if (identical(state, "ready")) {
    return(TRUE)
  }
  if (identical(state, "loading")) {
    stop("A worker capability loader re-entered itself.", call. = FALSE)
  }
  if (identical(state, "failed")) {
    stop(registry$errors[[name]], call. = FALSE)
  }
  registry$states[[name]] <- "loading"
  loaded <- tryCatch(
    {
      registry$loaders[[name]]()
      TRUE
    },
    error = function(error) error
  )
  if (inherits(loaded, "error")) {
    registry$states[[name]] <- "failed"
    registry$errors[[name]] <- conditionMessage(loaded)
    stop(registry$errors[[name]], call. = FALSE)
  }
  registry$states[[name]] <- "ready"
  TRUE
}

.builder_worker_epoch <- function() {
  paste0(
    "worker-",
    Sys.getpid(),
    "-",
    format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC"),
    "-",
    sprintf("%08x", sample.int(.Machine$integer.max, 1L))
  )
}

.builder_worker_identity <- function(snapshot) {
  if (is.null(snapshot)) {
    return(NULL)
  }
  if (!is.list(snapshot)) {
    stop("Snapshot identity must be an inert descriptor.", call. = FALSE)
  }
  values <- c(
    snapshot$path,
    snapshot$owner_token,
    snapshot$object_md5,
    snapshot$serialization %||% "rds"
  )
  if (
    length(values) != 4L ||
      anyNA(values) ||
      any(!nzchar(values)) ||
      !values[[4L]] %in% c("rds", "qs2", "qs")
  ) {
    stop("Snapshot identity is incomplete.", call. = FALSE)
  }
  paste(values, collapse = "\n")
}

#' Create the main-process request protocol.
builder_request_protocol <- function(epoch = .builder_worker_epoch()) {
  if (!.builder_worker_scalar_text(epoch)) {
    stop("A worker epoch is required.", call. = FALSE)
  }
  structure(
    list(
      epoch = epoch,
      queue = list(),
      pending = NULL,
      awaiting_ack = list(),
      invalidated = character(),
      latest_generation = list(),
      latest_dataset = list(),
      datasets = list(),
      next_token = 0L,
      persistent_sequence = 0L,
      acknowledged_sequence = 0L,
      build_status = "idle"
    ),
    class = c("builder_request_protocol", "list")
  )
}

builder_protocol_is_quiescent <- function(protocol) {
  .builder_protocol_assert(protocol)
  all(c("queue", "awaiting_ack", "build_status") %in% names(protocol)) &&
    is.list(protocol$queue) &&
    is.list(protocol$awaiting_ack) &&
    is.null(protocol$pending) &&
    !length(protocol$queue) &&
    !length(protocol$awaiting_ack) &&
    identical(protocol$build_status, "idle")
}

.builder_worker_request <- function(
  kind,
  dataset,
  slot,
  generation,
  persistent,
  payload,
  revision,
  snapshot_identity
) {
  if (!.builder_worker_scalar_text(kind)) {
    stop("A request kind is required.", call. = FALSE)
  }
  if (!.builder_worker_scalar_text(dataset)) {
    stop("A request dataset is required.", call. = FALSE)
  }
  if (!.builder_worker_scalar_text(slot)) {
    stop("A request slot is required.", call. = FALSE)
  }
  if (!.builder_worker_scalar_integer(generation)) {
    stop("A request generation must be a non-negative integer.", call. = FALSE)
  }
  if (!.builder_worker_scalar_integer(revision)) {
    stop("A dataset revision must be a non-negative integer.", call. = FALSE)
  }
  if (!is.list(payload)) {
    stop("A request payload must be an inert list.", call. = FALSE)
  }
  structure(
    list(
      kind = kind,
      dataset = dataset,
      slot = slot,
      generation = as.integer(generation),
      persistent = isTRUE(persistent),
      barrier = isTRUE(persistent) && kind %in% c("review", "build"),
      payload = payload,
      dataset_revision = as.integer(revision),
      snapshot_identity = snapshot_identity,
      transport_retries = 0L,
      replace_key = if (isTRUE(persistent)) {
        NULL
      } else {
        paste(kind, dataset, slot, sep = ":")
      },
      id = if (isTRUE(persistent)) {
        paste(kind, dataset, sep = ":")
      } else {
        paste(kind, dataset, slot, as.integer(generation), sep = ":")
      }
    ),
    class = c("builder_worker_request", "list")
  )
}

#' Describe a replaceable query.
builder_query <- function(
  kind,
  dataset,
  generation,
  slot = "default",
  payload = list(),
  revision = 0L,
  snapshot_identity = NULL
) {
  .builder_worker_request(
    kind = kind,
    dataset = dataset,
    slot = slot,
    generation = generation,
    persistent = FALSE,
    payload = payload,
    revision = revision,
    snapshot_identity = snapshot_identity
  )
}

#' Describe a persistent command.
builder_command <- function(
  kind,
  dataset,
  payload = list(),
  revision = 0L,
  snapshot_identity = NULL
) {
  .builder_worker_request(
    kind = kind,
    dataset = dataset,
    slot = "command",
    generation = 0L,
    persistent = TRUE,
    payload = payload,
    revision = revision,
    snapshot_identity = snapshot_identity
  )
}

.builder_protocol_assert <- function(protocol) {
  if (!inherits(protocol, "builder_request_protocol") || !is.list(protocol)) {
    stop("Expected a Builder request protocol.", call. = FALSE)
  }
  invisible(protocol)
}

#' Record the latest main-process identity for a dataset.
builder_protocol_dataset <- function(
  protocol,
  dataset,
  revision,
  snapshot_identity = NULL
) {
  .builder_protocol_assert(protocol)
  if (!.builder_worker_scalar_text(dataset)) {
    stop("A dataset id is required.", call. = FALSE)
  }
  if (!.builder_worker_scalar_integer(revision)) {
    stop("A dataset revision must be a non-negative integer.", call. = FALSE)
  }
  protocol$datasets[[dataset]] <- list(
    revision = as.integer(revision),
    snapshot_identity = snapshot_identity
  )
  protocol
}

#' Queue one request while preserving command order and query freshness.
builder_enqueue <- function(protocol, request) {
  .builder_protocol_assert(protocol)
  if (!inherits(request, "builder_worker_request") || !is.list(request)) {
    stop("Expected a Builder worker request.", call. = FALSE)
  }
  if (isTRUE(request$barrier)) {
    if (!is.null(protocol$pending) && !isTRUE(protocol$pending$persistent)) {
      protocol$invalidated <- unique(c(
        protocol$invalidated,
        protocol$pending$request_id
      ))
    }
    protocol$queue <- Filter(
      function(queued) isTRUE(queued$persistent),
      protocol$queue
    )
  }
  if (identical(request$kind, "build")) {
    if (!identical(protocol$build_status, "idle")) {
      stop("A Builder build is already in flight.", call. = FALSE)
    }
    protocol$build_status <- "queued"
  }
  protocol$next_token <- as.integer(protocol$next_token) + 1L
  request$token <- paste0(protocol$epoch, ":request-", protocol$next_token)
  request$request_id <- request$token
  request$epoch <- protocol$epoch
  request$build_id <- if (identical(request$kind, "build")) {
    if (is.null(request$payload$id)) request$token else request$payload$id
  } else {
    NULL
  }
  if (isTRUE(request$persistent)) {
    protocol$persistent_sequence <-
      as.integer(protocol$persistent_sequence) + 1L
    request$persistent_sequence <- protocol$persistent_sequence
    request$cutoff <- request$persistent_sequence - 1L
  } else {
    key <- request$replace_key
    protocol$queue <- Filter(
      function(queued) {
        isTRUE(queued$persistent) ||
          !identical(queued$replace_key, key)
      },
      protocol$queue
    )
    latest <- protocol$latest_generation[[key]]
    if (!is.null(latest) && request$generation <= latest) {
      stop("A replaceable query generation must increase.", call. = FALSE)
    }
    protocol$latest_generation[[key]] <- request$generation
    protocol$latest_dataset[[key]] <- request$dataset
  }
  position <- length(protocol$queue)
  if (!isTRUE(request$persistent)) {
    while (
      position > 0L &&
        .builder_protocol_query_passes(protocol$queue[[position]], request)
    ) {
      position <- position - 1L
    }
  }
  protocol$queue <- append(protocol$queue, list(request), after = position)
  protocol
}

## A replaceable query reads an already-open dataset in well under a second,
## while each queued load can decode a multi-GB file. Let the query overtake
## the trailing run of queued (never running) loads of other datasets so a
## preview does not wait behind every file chosen in one picker. Any other
## persistent command keeps its FIFO position relative to queries.
.builder_protocol_query_passes <- function(queued, request) {
  isTRUE(queued$persistent) &&
    !isTRUE(queued$barrier) &&
    identical(queued$kind, "load") &&
    !identical(queued$dataset, request$dataset)
}

#' List queued request ids in dispatch order.
builder_pending_ids <- function(protocol) {
  .builder_protocol_assert(protocol)
  vapply(protocol$queue, `[[`, character(1), "id")
}

#' Dispatch the next request if the acknowledgement barrier is open.
builder_protocol_dispatch <- function(protocol) {
  .builder_protocol_assert(protocol)
  if (!is.null(protocol$pending)) {
    return(list(protocol = protocol, request = NULL, blocked = "pending"))
  }
  if (length(protocol$awaiting_ack)) {
    return(list(
      protocol = protocol,
      request = NULL,
      blocked = "awaiting_ack"
    ))
  }
  if (!length(protocol$queue)) {
    return(list(protocol = protocol, request = NULL, blocked = "empty"))
  }
  request <- protocol$queue[[1L]]
  if (
    isTRUE(request$barrier) &&
      protocol$acknowledged_sequence < request$cutoff
  ) {
    return(list(protocol = protocol, request = NULL, blocked = "cutoff"))
  }
  if (isTRUE(request$persistent)) {
    current <- protocol$datasets[[request$dataset]]
    if (!is.null(current)) {
      request$dataset_revision <- current$revision
      request$snapshot_identity <- current$snapshot_identity
    }
  }
  protocol$queue <- protocol$queue[-1L]
  protocol$pending <- request
  if (identical(request$kind, "build")) {
    protocol$build_status <- "running"
  }
  list(protocol = protocol, request = request, blocked = NULL)
}

builder_request_redact_auth <- function(request) {
  if (
    is.list(request) &&
      identical(request$kind, "build") &&
      is.list(request$payload)
  ) {
    request$payload$auth_accounts <- NULL
  }
  request
}

builder_protocol_redact_auth <- function(protocol) {
  .builder_protocol_assert(protocol)
  protocol$queue <- lapply(protocol$queue, builder_request_redact_auth)
  protocol$pending <- builder_request_redact_auth(protocol$pending)
  protocol$awaiting_ack <- lapply(
    protocol$awaiting_ack,
    builder_request_redact_auth
  )
  protocol
}

.builder_protocol_response_field <- function(response, field) {
  value <- response[[field]]
  if (identical(field, "request_id") && is.null(value)) {
    value <- response$token
  }
  value
}

#' Capture one worker condition before its call stack unwinds.
.builder_worker_condition_diagnostics <- function(
  condition,
  calls = sys.calls()
) {
  render_call <- function(call) {
    if (is.null(call)) {
      return("")
    }
    paste(deparse(call, width.cutoff = 500L), collapse = " ")
  }
  rendered_calls <- vapply(
    tail(as.list(calls), 64L),
    render_call,
    character(1),
    USE.NAMES = FALSE
  )
  call <- tryCatch(conditionCall(condition), error = function(error) NULL)
  list(
    class = class(condition),
    message = tryCatch(
      conditionMessage(condition),
      error = function(error) "The worker condition message was unavailable."
    ),
    call = if (is.null(call)) NULL else render_call(call),
    calls = rendered_calls,
    calls_truncated = length(calls) > length(rendered_calls),
    captured_at = Sys.time()
  )
}

#' Echo the immutable request identity with a worker-produced value.
builder_worker_response <- function(
  request,
  value = NULL,
  error = NULL,
  diagnostics = NULL
) {
  if (is.null(request)) {
    if (!is.null(error)) {
      response <- list(error = error)
      if (!is.null(diagnostics)) {
        response$diagnostics <- diagnostics
      }
      return(response)
    }
    return(value)
  }
  if (
    !inherits(request, "builder_worker_request") || is.null(request$request_id)
  ) {
    stop("A dispatched Builder request is required.", call. = FALSE)
  }
  response <- list(
    epoch = request$epoch,
    request_id = request$request_id,
    token = request$token,
    generation = request$generation,
    dataset_revision = request$dataset_revision,
    snapshot_identity = request$snapshot_identity,
    build_id = request$build_id,
    value = value,
    error = error
  )
  if (!is.null(diagnostics)) {
    response$diagnostics <- diagnostics
  }
  response
}

#' Validate one worker response before changing protocol ownership state.
builder_protocol_complete <- function(protocol, response) {
  .builder_protocol_assert(protocol)
  protocol <- builder_protocol_redact_auth(protocol)
  request <- protocol$pending
  if (is.null(request) || !is.list(response)) {
    return(list(protocol = protocol, accepted = FALSE, value = NULL))
  }
  identity_fields <- c(
    "epoch",
    "request_id",
    "generation",
    "dataset_revision",
    "snapshot_identity"
  )
  for (field in identity_fields) {
    expected <- if (identical(field, "request_id")) {
      request$request_id
    } else {
      request[[field]]
    }
    if (
      !identical(.builder_protocol_response_field(response, field), expected)
    ) {
      return(list(protocol = protocol, accepted = FALSE, value = NULL))
    }
  }
  if (
    identical(request$kind, "build") &&
      !identical(response$build_id, request$build_id)
  ) {
    return(list(protocol = protocol, accepted = FALSE, value = NULL))
  }

  protocol$pending <- NULL
  invalidated <- request$request_id %in% protocol$invalidated
  protocol$invalidated <- setdiff(
    protocol$invalidated,
    request$request_id
  )
  current <- protocol$datasets[[request$dataset]]
  revision_independent <- isTRUE(request$payload$revision_independent)
  current_matches <- is.null(current) ||
    (identical(current$snapshot_identity, request$snapshot_identity) &&
      (revision_independent ||
        identical(current$revision, request$dataset_revision)))
  latest_matches <- isTRUE(request$persistent) ||
    identical(
      protocol$latest_generation[[request$replace_key]],
      request$generation
    )
  accepted <- !invalidated && current_matches && latest_matches
  if (isTRUE(request$persistent)) {
    protocol$awaiting_ack[[request$request_id]] <- request
  }
  list(
    protocol = protocol,
    accepted = accepted,
    value = if (accepted) response$value else NULL,
    error = if (accepted) response$error else NULL,
    request = builder_request_redact_auth(request)
  )
}

#' Acknowledge that the main process applied a persistent command result.
builder_protocol_acknowledge <- function(protocol, token) {
  .builder_protocol_assert(protocol)
  protocol <- builder_protocol_redact_auth(protocol)
  if (!.builder_worker_scalar_text(token)) {
    stop("An acknowledgement token is required.", call. = FALSE)
  }
  request <- protocol$awaiting_ack[[token]]
  if (is.null(request)) {
    stop(
      "The persistent command is not awaiting acknowledgement.",
      call. = FALSE
    )
  }
  protocol$awaiting_ack[[token]] <- NULL
  protocol$acknowledged_sequence <- max(
    protocol$acknowledged_sequence,
    request$persistent_sequence
  )
  if (identical(request$kind, "build")) {
    protocol$build_status <- "idle"
  }
  protocol
}

#' Mark the running Build request as cancelling without opening a new flight.
builder_protocol_cancel <- function(protocol) {
  .builder_protocol_assert(protocol)
  protocol <- builder_protocol_redact_auth(protocol)
  if (
    !identical(protocol$build_status, "running") ||
      is.null(protocol$pending) ||
      !identical(protocol$pending$kind, "build")
  ) {
    stop("No running Builder build can be cancelled.", call. = FALSE)
  }
  protocol$build_status <- "cancelling"
  protocol$pending$cancel_requested <- TRUE
  protocol
}

.builder_protocol_terminal_request <- function(request, reason) {
  request <- builder_request_redact_auth(request)
  request$terminal_reason <- reason
  request$terminal_status <- "failed"
  request
}

.builder_protocol_request_retries <- function(request) {
  retries <- request$transport_retries
  if (is.null(retries)) {
    return(0L)
  }
  if (!.builder_worker_scalar_integer(retries)) {
    stop("A transport retry counter is invalid.", call. = FALSE)
  }
  as.integer(retries)
}

.builder_protocol_order_persistent <- function(requests) {
  if (!length(requests)) {
    return(requests)
  }
  sequences <- vapply(
    requests,
    function(request) {
      sequence <- request$persistent_sequence
      if (is.null(sequence)) Inf else as.numeric(sequence)
    },
    numeric(1)
  )
  requests[order(sequences)]
}

.builder_protocol_rebase_queue <- function(protocol) {
  active <- c(
    if (is.null(protocol$pending)) list() else list(protocol$pending),
    unname(protocol$awaiting_ack)
  )
  active_sequences <- vapply(
    Filter(function(request) isTRUE(request$persistent), active),
    function(request) as.integer(request$persistent_sequence),
    integer(1)
  )
  sequence <- max(
    c(as.integer(protocol$acknowledged_sequence), active_sequences),
    na.rm = TRUE
  )
  for (index in seq_along(protocol$queue)) {
    request <- protocol$queue[[index]]
    if (isTRUE(request$persistent)) {
      sequence <- as.integer(sequence) + 1L
      request$persistent_sequence <- sequence
      request$cutoff <- sequence - 1L
      protocol$queue[[index]] <- request
    }
  }
  protocol$persistent_sequence <- as.integer(sequence)
  protocol
}

#' Fail the current request without discarding later FIFO work.
builder_protocol_fail_pending <- function(protocol, reason) {
  .builder_protocol_assert(protocol)
  protocol <- builder_protocol_redact_auth(protocol)
  if (!.builder_worker_scalar_text(reason)) {
    stop("A pending request failure reason is required.", call. = FALSE)
  }
  request <- protocol$pending
  if (!is.null(request)) {
    protocol$pending <- NULL
  } else {
    if (length(protocol$awaiting_ack) != 1L) {
      stop("No single Builder request can be failed.", call. = FALSE)
    }
    request <- protocol$awaiting_ack[[1L]]
    protocol$awaiting_ack[[request$request_id]] <- NULL
  }
  protocol$invalidated <- setdiff(protocol$invalidated, request$request_id)
  if (isTRUE(request$persistent)) {
    protocol$acknowledged_sequence <- max(
      protocol$acknowledged_sequence,
      request$persistent_sequence
    )
  }
  if (identical(request$kind, "build")) {
    protocol$build_status <- "idle"
  }
  protocol <- .builder_protocol_rebase_queue(protocol)
  list(
    protocol = protocol,
    failed = .builder_protocol_terminal_request(request, reason),
    reason = reason
  )
}

.builder_protocol_session_request <- function(request) {
  identical(request$dataset, "session") &&
    request$kind %in% c("review", "build")
}

#' Remove one dataset and explicitly terminate only its remaining work.
builder_protocol_forget_dataset <- function(
  protocol,
  id,
  reason = "dataset_forgotten"
) {
  .builder_protocol_assert(protocol)
  protocol <- builder_protocol_redact_auth(protocol)
  if (!.builder_worker_scalar_text(id)) {
    stop("A dataset id is required for protocol cleanup.", call. = FALSE)
  }
  if (!.builder_worker_scalar_text(reason)) {
    stop("A dataset cleanup reason is required.", call. = FALSE)
  }
  belongs <- function(request) {
    identical(request$dataset, id) &&
      !.builder_protocol_session_request(request)
  }

  failed <- discarded <- list()
  if (!is.null(protocol$pending) && belongs(protocol$pending)) {
    if (isTRUE(protocol$pending$persistent)) {
      stop(
        "A dataset cannot be forgotten while its command is pending.",
        call. = FALSE
      )
    }
    protocol$invalidated <- unique(c(
      protocol$invalidated,
      protocol$pending$request_id
    ))
    discarded <- list(.builder_protocol_terminal_request(
      protocol$pending,
      reason
    ))
  }

  awaiting_same <- vapply(
    protocol$awaiting_ack,
    belongs,
    logical(1)
  )
  if (length(awaiting_same) && any(awaiting_same)) {
    removed <- unname(protocol$awaiting_ack[awaiting_same])
    failed <- c(
      failed,
      lapply(removed, .builder_protocol_terminal_request, reason = reason)
    )
    for (request in removed) {
      protocol$acknowledged_sequence <- max(
        protocol$acknowledged_sequence,
        request$persistent_sequence
      )
      if (identical(request$kind, "build")) {
        protocol$build_status <- "idle"
      }
    }
    protocol$awaiting_ack <- protocol$awaiting_ack[!awaiting_same]
  }

  queued_same <- vapply(protocol$queue, belongs, logical(1))
  if (length(queued_same) && any(queued_same)) {
    removed <- protocol$queue[queued_same]
    persistent <- Filter(function(request) isTRUE(request$persistent), removed)
    queries <- Filter(function(request) !isTRUE(request$persistent), removed)
    failed <- c(
      failed,
      lapply(persistent, .builder_protocol_terminal_request, reason = reason)
    )
    discarded <- c(
      discarded,
      lapply(queries, .builder_protocol_terminal_request, reason = reason)
    )
    if (
      any(vapply(
        persistent,
        function(request) {
          identical(request$kind, "build")
        },
        logical(1)
      ))
    ) {
      protocol$build_status <- "idle"
    }
    protocol$queue <- protocol$queue[!queued_same]
  }

  latest_dataset <- protocol$latest_dataset
  if (!is.list(latest_dataset)) {
    latest_dataset <- list()
  }
  forgotten_keys <- names(latest_dataset)[vapply(
    latest_dataset,
    identical,
    logical(1),
    y = id
  )]
  for (key in forgotten_keys) {
    protocol$latest_dataset[[key]] <- NULL
    protocol$latest_generation[[key]] <- NULL
  }
  protocol$datasets[[id]] <- NULL
  protocol <- .builder_protocol_rebase_queue(protocol)
  list(
    protocol = protocol,
    failed = .builder_protocol_order_persistent(failed),
    discarded = discarded,
    reason = reason
  )
}

.builder_protocol_recovery_entries <- function(protocol) {
  wrap <- function(requests, origin) {
    lapply(requests, function(request) {
      list(origin = origin, request = request)
    })
  }
  entries <- c(
    wrap(unname(protocol$awaiting_ack), "awaiting_ack"),
    wrap(
      if (is.null(protocol$pending)) list() else list(protocol$pending),
      "pending"
    ),
    wrap(protocol$queue, "queued")
  )
  sequences <- vapply(
    entries,
    function(entry) {
      request <- entry$request
      if (isTRUE(request$persistent)) {
        as.numeric(request$persistent_sequence)
      } else {
        Inf
      }
    },
    numeric(1)
  )
  entries[order(sequences)]
}

#' Reconcile protocol ownership after replacing or losing a worker.
builder_protocol_recover <- function(
  protocol,
  epoch = .builder_worker_epoch(),
  reason = "worker_restarted",
  retry_persistent = TRUE
) {
  .builder_protocol_assert(protocol)
  protocol <- builder_protocol_redact_auth(protocol)
  if (!.builder_worker_scalar_text(epoch)) {
    stop("A recovery worker epoch is required.", call. = FALSE)
  }
  if (!.builder_worker_scalar_text(reason)) {
    stop("A protocol terminal reason is required.", call. = FALSE)
  }
  if (
    !is.logical(retry_persistent) ||
      length(retry_persistent) != 1L ||
      is.na(retry_persistent)
  ) {
    stop("Protocol retry policy must be one logical value.", call. = FALSE)
  }

  entries <- .builder_protocol_recovery_entries(protocol)
  discarded <- lapply(
    lapply(
      Filter(function(entry) !isTRUE(entry$request$persistent), entries),
      `[[`,
      "request"
    ),
    .builder_protocol_terminal_request,
    reason = reason
  )

  persistent <- Filter(
    function(entry) isTRUE(entry$request$persistent),
    entries
  )
  retryable <- Filter(
    function(entry) {
      request <- entry$request
      retries <- .builder_protocol_request_retries(request)
      isTRUE(retry_persistent) &&
        !identical(request$kind, "build") &&
        !identical(entry$origin, "awaiting_ack") &&
        (!identical(entry$origin, "pending") || retries < 1L)
    },
    persistent
  )
  failed <- lapply(
    lapply(
      Filter(
        function(entry) {
          request <- entry$request
          retries <- .builder_protocol_request_retries(request)
          !isTRUE(retry_persistent) ||
            identical(request$kind, "build") ||
            identical(entry$origin, "awaiting_ack") ||
            (identical(entry$origin, "pending") && retries >= 1L)
        },
        persistent
      ),
      `[[`,
      "request"
    ),
    .builder_protocol_terminal_request,
    reason = reason
  )

  recovered <- builder_request_protocol(epoch = epoch)
  recovered$datasets <- protocol$datasets
  retried <- list()
  for (entry in retryable) {
    request <- entry$request
    replacement <- builder_command(
      kind = request$kind,
      dataset = request$dataset,
      payload = request$payload,
      revision = request$dataset_revision,
      snapshot_identity = request$snapshot_identity
    )
    replacement$transport_retries <-
      .builder_protocol_request_retries(request) +
      as.integer(identical(entry$origin, "pending"))
    recovered <- builder_enqueue(recovered, replacement)
    retried[[length(retried) + 1L]] <- recovered$queue[[length(
      recovered$queue
    )]]
  }

  list(
    protocol = recovered,
    retried = retried,
    failed = failed,
    discarded = discarded,
    reason = reason
  )
}

.builder_worker_process_state <- function(process, timeout = 0) {
  state <- try(process$poll_process(timeout), silent = TRUE)
  if (inherits(state, "try-error")) {
    return("closed")
  }
  as.character(state)[1L]
}

.builder_worker_transport_output_limit <- 1024L * 1024L

.builder_worker_transport_stream <- function(
  value,
  limit = .builder_worker_transport_output_limit
) {
  bytes <- tryCatch(
    {
      if (is.raw(value)) {
        value
      } else {
        charToRaw(paste0(as.character(value %||% character()), collapse = ""))
      }
    },
    error = function(error) raw()
  )
  original_bytes <- length(bytes)
  truncated <- original_bytes > limit
  if (truncated) {
    bytes <- tail(bytes, limit)
  }
  text <- tryCatch(rawToChar(bytes), error = function(error) "")
  if (truncated && nzchar(text)) {
    Encoding(text) <- "bytes"
  }
  list(
    value = text,
    truncated = truncated,
    original_bytes = original_bytes
  )
}

.builder_worker_transport_condition <- function(condition, stdout, stderr) {
  if (!inherits(condition, "condition")) {
    return(NULL)
  }
  copied <- tryCatch(
    unserialize(serialize(condition, NULL, version = 2)),
    error = function(error) NULL
  )
  if (is.null(copied)) {
    message <- tryCatch(
      conditionMessage(condition),
      error = function(error) "The worker condition could not be serialized."
    )
    call <- tryCatch(conditionCall(condition), error = function(error) NULL)
    return(structure(
      list(message = message, call = call, original_class = class(condition)),
      class = c("builder_worker_condition_summary", "error", "condition")
    ))
  }
  if (is.list(copied)) {
    copied$stdout <- stdout
    copied$stderr <- stderr
  }
  copied
}

.builder_worker_transport <- function(message, code = NULL) {
  stdout <- .builder_worker_transport_stream(message$stdout %||% character())
  stderr <- .builder_worker_transport_stream(message$stderr %||% character())
  list(
    schema_version = 1L,
    code = code,
    stdout = stdout$value,
    stderr = stderr$value,
    stdout_truncated = stdout$truncated,
    stderr_truncated = stderr$truncated,
    stdout_original_bytes = stdout$original_bytes,
    stderr_original_bytes = stderr$original_bytes,
    error_condition = .builder_worker_transport_condition(
      message$error %||% NULL,
      stdout$value,
      stderr$value
    ),
    response = message$result %||% NULL
  )
}

.builder_worker_read_ready <- function(process) {
  message <- try(process$read(), silent = TRUE)
  if (inherits(message, "try-error")) {
    condition <- attr(message, "condition")
    return(list(
      error = "The background worker response could not be read.",
      transport = .builder_worker_transport(list(
        stdout = character(),
        stderr = character(),
        error = condition,
        result = NULL
      )),
      done = TRUE
    ))
  }
  if (is.null(message)) {
    return(NULL)
  }
  code <- message$code
  if (is.null(code)) {
    code <- 200L
  }
  transport <- .builder_worker_transport(message, code = code)
  if (identical(code, 200L)) {
    if (!is.null(message$error)) {
      error <- tryCatch(
        conditionMessage(message$error),
        error = function(error) "The background worker returned an error."
      )
      return(list(error = error, transport = transport, done = TRUE))
    }
    return(list(value = message$result, transport = transport, done = TRUE))
  }
  if (code >= 500L) {
    error <- if (inherits(message$error, "condition")) {
      tryCatch(conditionMessage(message$error), error = function(error) NULL)
    } else {
      NULL
    }
    if (!.builder_worker_scalar_text(error)) {
      error <- message$message
    }
    if (!.builder_worker_scalar_text(error)) {
      error <- paste0("Background worker error (code ", code, ").")
    }
    return(list(
      error = error,
      transport = transport,
      done = TRUE
    ))
  }
  NULL
}

#' Start one isolated worker from main-owned immutable snapshot descriptors.
.builder_worker_package_source <- function(builder_dir) {
  is_source_package <- function(candidate) {
    .builder_worker_scalar_text(candidate) &&
      file.exists(file.path(candidate, "DESCRIPTION")) &&
      dir.exists(file.path(candidate, "R")) &&
      length(list.files(
        file.path(candidate, "R"),
        pattern = "[.][Rr]$",
        full.names = FALSE
      )) >
        0L
  }
  configured_source <- Sys.getenv("CEREBRO_PACKAGE_SOURCE", unset = "")
  if (is_source_package(configured_source)) {
    return(normalizePath(
      configured_source,
      winslash = "/",
      mustWork = TRUE
    ))
  }
  candidate <- normalizePath(
    file.path(builder_dir, "..", ".."),
    winslash = "/",
    mustWork = FALSE
  )
  if (
    identical(basename(dirname(builder_dir)), "inst") &&
      is_source_package(candidate)
  ) {
    return(candidate)
  }
  namespace_source <- tryCatch(
    getNamespaceInfo(asNamespace("CerebroNexus"), "path"),
    error = function(error) NULL
  )
  if (is_source_package(namespace_source)) {
    return(normalizePath(namespace_source, winslash = "/", mustWork = TRUE))
  }
  NULL
}

## Bytes of decoded datasets the worker keeps open between requests. The
## `CerebroNexus.builder.worker_object_budget` option overrides the default of a
## quarter of physical memory; 0 keeps only the dataset in use.
builder_worker_object_budget <- function(
  budget = getOption("CerebroNexus.builder.worker_object_budget", NULL),
  .total_memory = function() ps::ps_system_memory()$total
) {
  if (!is.null(budget)) {
    budget <- suppressWarnings(as.double(budget)[1L])
    if (length(budget) == 1L && !is.na(budget) && budget >= 0) {
      return(budget)
    }
  }
  total <- tryCatch(
    suppressWarnings(as.double(.total_memory())[1L]),
    error = function(error) NA_real_
  )
  if (length(total) != 1L || is.na(total) || !is.finite(total) || total <= 0) {
    return(2 * 1024^3)
  }
  total / 4
}

builder_worker_start <- function(
  builder_dir,
  snapshot_root = NULL,
  snapshot_registry = list(),
  epoch = NULL,
  .new_session = callr::r_session$new,
  .startup_timeout_ms = 30000L,
  .async = FALSE,
  .bootstrap = NULL,
  .object_budget = builder_worker_object_budget()
) {
  if (!requireNamespace("callr", quietly = TRUE)) {
    return(list(error = "The callr package is required for background work."))
  }
  if (!.builder_worker_scalar_text(builder_dir) || !dir.exists(builder_dir)) {
    return(list(error = "The Builder runtime directory does not exist."))
  }
  if (
    !is.list(snapshot_registry) ||
      (length(snapshot_registry) && is.null(names(snapshot_registry)))
  ) {
    return(list(error = "The snapshot registry must be a named list."))
  }
  if (
    !is.function(.new_session) ||
      !is.numeric(.startup_timeout_ms) ||
      length(.startup_timeout_ms) != 1L ||
      is.na(.startup_timeout_ms) ||
      !is.finite(.startup_timeout_ms) ||
      .startup_timeout_ms < 1 ||
      .startup_timeout_ms != floor(.startup_timeout_ms)
  ) {
    return(list(
      error = "The background worker startup configuration is invalid."
    ))
  }
  owns_root <- is.null(snapshot_root) || !dir.exists(snapshot_root)
  if (is.null(snapshot_root)) {
    snapshot_root <- tempfile("cerebro-builder-session-")
  }
  if (
    !dir.exists(snapshot_root) &&
      !dir.create(
        snapshot_root,
        mode = "0700",
        recursive = TRUE
      )
  ) {
    return(list(
      error = "Could not create the private Builder session directory."
    ))
  }
  if (!dir.exists(snapshot_root)) {
    return(list(error = "The private Builder snapshot directory is missing."))
  }
  process <- try(
    .new_session(wait_timeout = as.integer(.startup_timeout_ms)),
    silent = TRUE
  )
  if (inherits(process, "try-error")) {
    return(list(
      error = paste0(
        "Could not start the background worker: ",
        conditionMessage(attr(process, "condition"))
      )
    ))
  }
  setup_call <- function(
    dir,
    root,
    registry,
    package_source,
    bootstrap,
    source_utf8,
    object_budget
  ) {
    if (is.function(bootstrap)) {
      return(bootstrap(dir, root, registry, package_source))
    }
    if (!is.null(package_source)) {
      Sys.setenv(CEREBRO_PACKAGE_SOURCE = package_source)
    }
    load_package_runtime <- local({
      loaded <- FALSE
      function() {
        if (isTRUE(loaded)) {
          return(invisible(TRUE))
        }
        if (!is.null(package_source)) {
          description <- read.dcf(file.path(package_source, "DESCRIPTION"))
          collate <- if ("Collate" %in% colnames(description)) {
            description[1L, "Collate"]
          } else {
            NA_character_
          }
          if (is.na(collate) || !nzchar(collate)) {
            runtime_files <- sort(list.files(
              file.path(package_source, "R"),
              pattern = "[.][Rr]$",
              full.names = TRUE
            ))
          } else {
            runtime_files <- scan(
              text = collate,
              what = character(),
              quiet = TRUE
            )
            runtime_files <- file.path(package_source, "R", runtime_files)
          }
          missing_runtime <- runtime_files[!file.exists(runtime_files)]
          if (length(missing_runtime)) {
            stop("The source-tree worker runtime is incomplete.")
          }
          for (runtime_file in runtime_files) {
            source_utf8(runtime_file, globalenv())
          }
        } else {
          suppressMessages(library(CerebroNexus))
        }
        loaded <<- TRUE
        invisible(TRUE)
      }
    })
    source_utf8(file.path(dir, "io.R"), globalenv())
    source_utf8(file.path(dir, "loading.R"), globalenv())
    source_utf8(
      file.path(
        dir,
        "..",
        "viewer",
        "core",
        "viewer_content_contract.R"
      ),
      globalenv()
    )
    source_utf8(file.path(dir, "manifest.R"), globalenv())
    source_utf8(file.path(dir, "spatial.R"), globalenv())
    source_utf8(file.path(dir, "content_tables.R"), globalenv())
    source_utf8(file.path(dir, "content.R"), globalenv())
    source_utf8(file.path(dir, "profile.R"), globalenv())
    source_utf8(file.path(dir, "inspect.R"), globalenv())
    source_utf8(file.path(dir, "adapters.R"), globalenv())
    source_utf8(file.path(dir, "prerequisite.R"), globalenv())
    source_utf8(file.path(dir, "state.R"), globalenv())
    source_utf8(file.path(dir, "plan.R"), globalenv())
    source_utf8(file.path(dir, "worker.R"), globalenv())
    source_files <- function(paths) {
      for (path in paths) {
        source_utf8(path, globalenv())
      }
      invisible(TRUE)
    }
    capabilities <- builder_worker_capability_registry(list(
      package = load_package_runtime,
      spatial = function() {
        source_files(c(
          file.path(
            dir,
            "..",
            "viewer",
            "core",
            "spatial_coordinate_contract.R"
          ),
          file.path(
            dir,
            "..",
            "viewer",
            "core",
            "spatial_coordinate_transform.R"
          ),
          file.path(dir, "content_spatial.R")
        ))
      },
      immune = function() {
        ## The HLA core is part of the package runtime; an installed
        ## package keeps it internal, so bind it into the worker too.
        builder_worker_ensure_capability(capabilities, "package")
        if (is.null(package_source)) {
          list2env(
            utils::getFromNamespace(".viewerHLACore", "CerebroNexus")(),
            envir = globalenv()
          )
        }
        source_files(file.path(dir, "content_immune.R"))
      },
      analysis = function() {
        source_files(c(
          file.path(dir, "preview.R"),
          file.path(dir, "extras.R"),
          file.path(dir, "analysis.R"),
          file.path(dir, "marker_import.R")
        ))
      },
      build = function() {
        builder_worker_ensure_capability(capabilities, "package")
        builder_worker_ensure_capability(capabilities, "analysis")
        source_files(c(
          file.path(dir, "app_bundle.R"),
          file.path(dir, "build.R")
        ))
      }
    ))
    assign(".builder_worker_capabilities", capabilities, envir = globalenv())
    assign(
      "builder_worker_require_capability",
      function(name) builder_worker_ensure_capability(capabilities, name),
      envir = globalenv()
    )
    objects <- new.env(parent = emptyenv())
    snapshots <- new.env(parent = emptyenv())
    snapshot_cache <- new.env(parent = emptyenv())
    # Stat and owner-marker checks reject replaced or foreign snapshots; a
    # full-content MD5 of every registered snapshot would delay each
    # (re)start by the total size of the session's datasets.
    for (id in names(registry)) {
      snapshot <- registry[[id]]
      if (!.builder_snapshot_owned(snapshot, verify_content = FALSE)) {
        stop("A registered Builder snapshot is no longer owned.")
      }
      assign(id, snapshot, envir = snapshots)
    }
    assign(".builder_objects", objects, envir = globalenv())
    assign(".builder_snapshots", snapshots, envir = globalenv())
    assign(".builder_snapshot_cache", snapshot_cache, envir = globalenv())
    assign(".builder_snapshot_root", root, envir = globalenv())
    assign(".builder_object_budget", object_budget, envir = globalenv())
    names(registry)
  }
  setup_args <- list(
    dir = normalizePath(builder_dir, mustWork = TRUE),
    root = normalizePath(snapshot_root, mustWork = TRUE),
    registry = snapshot_registry,
    package_source = .builder_worker_package_source(builder_dir),
    bootstrap = .bootstrap,
    source_utf8 = .builder_worker_source_utf8,
    object_budget = builder_worker_object_budget(.object_budget)
  )
  setup <- try(
    if (isTRUE(.async)) {
      process$call(setup_call, args = setup_args)
      NULL
    } else {
      process$run(setup_call, args = setup_args)
    },
    silent = TRUE
  )
  if (inherits(setup, "try-error")) {
    try(process$close(), silent = TRUE)
    alive <- try(process$is_alive(), silent = TRUE)
    if (isTRUE(alive)) {
      try(process$kill(), silent = TRUE)
      try(process$wait(timeout = 5000L), silent = TRUE)
    }
    if (owns_root && !length(snapshot_registry)) {
      unlink(snapshot_root, recursive = TRUE, force = TRUE)
    }
    return(list(
      error = paste0(
        "The background worker could not be initialized: ",
        conditionMessage(attr(setup, "condition"))
      )
    ))
  }
  if (is.null(epoch)) {
    epoch <- .builder_worker_epoch()
  }
  structure(
    list(
      process = process,
      builder_dir = normalizePath(builder_dir, mustWork = TRUE),
      snapshot_root = normalizePath(snapshot_root, mustWork = TRUE),
      snapshot_registry = snapshot_registry,
      snapshot_cache = list(),
      epoch = epoch,
      state = if (isTRUE(.async)) "starting" else "ready",
      ready = !isTRUE(.async),
      startup_started_at = Sys.time(),
      startup_timeout_ms = as.integer(.startup_timeout_ms),
      owns_root = owns_root,
      cleanup_safe = TRUE,
      restored = setup
    ),
    class = c("builder_worker", "list")
  )
}

builder_worker_poll_startup <- function(worker, timeout = 0) {
  .builder_worker_assert(worker)
  state <- worker$state %||% if (isTRUE(worker$ready)) "ready" else "failed"
  if (!identical(state, "starting")) {
    return(list(worker = worker, state = state, error = worker$error %||% NULL))
  }
  elapsed_ms <- as.numeric(difftime(
    Sys.time(),
    worker$startup_started_at,
    units = "secs"
  )) *
    1000
  if (is.finite(elapsed_ms) && elapsed_ms >= worker$startup_timeout_ms) {
    .builder_worker_interrupt(worker$process)
    worker$state <- "failed"
    worker$ready <- FALSE
    worker$error <- "The background worker startup timed out."
    return(list(worker = worker, state = "failed", error = worker$error))
  }
  process_state <- .builder_worker_process_state(worker$process, timeout)
  if (identical(process_state, "busy")) {
    return(list(worker = worker, state = "starting", error = NULL))
  }
  if (identical(process_state, "closed")) {
    worker$state <- "failed"
    worker$ready <- FALSE
    worker$error <- "The background worker exited during startup."
    return(list(worker = worker, state = "failed", error = worker$error))
  }
  if (!identical(process_state, "ready")) {
    return(list(worker = worker, state = "starting", error = NULL))
  }
  result <- .builder_worker_read_ready(worker$process)
  if (is.null(result)) {
    return(list(worker = worker, state = "starting", error = NULL))
  }
  if (!is.null(result$error)) {
    worker$state <- "failed"
    worker$ready <- FALSE
    worker$error <- result$error
    return(list(worker = worker, state = "failed", error = worker$error))
  }
  worker$restored <- result$value
  worker$state <- "ready"
  worker$ready <- TRUE
  worker$error <- NULL
  list(worker = worker, state = "ready", error = NULL)
}

.builder_worker_assert <- function(worker) {
  if (!inherits(worker, "builder_worker") || !is.list(worker)) {
    stop("Expected a Builder worker.", call. = FALSE)
  }
  invisible(worker)
}

.builder_worker_cleanup_report <- function() {
  list(
    snapshots = character(),
    stages = character(),
    registered = character(),
    preserved = character(),
    errors = character()
  )
}

.builder_worker_owner_token <- function(value) {
  .builder_worker_scalar_text(value) &&
    grepl("-owner-[0-9a-fA-F]{8,}$", value)
}

.builder_worker_owner_time <- function(value) {
  inherits(value, "POSIXt") &&
    length(value) == 1L &&
    !is.na(value)
}

.builder_worker_snapshot_marker <- function(marker) {
  serialization <- if (is.list(marker)) {
    marker$serialization %||% "rds"
  } else {
    NULL
  }
  is.list(marker) &&
    .builder_worker_scalar_text(marker$path) &&
    .builder_worker_owner_token(marker$owner_token) &&
    .builder_worker_owner_time(marker$created_at) &&
    .builder_worker_scalar_text(marker$object_md5) &&
    grepl("^[0-9a-fA-F]{32}$", marker$object_md5) &&
    is.list(marker$object_state) &&
    .builder_worker_scalar_text(serialization) &&
    serialization %in% c("rds", "qs2", "qs")
}

.builder_worker_stage_marker <- function(marker) {
  is.list(marker) &&
    .builder_worker_scalar_text(marker$path) &&
    .builder_worker_owner_token(marker$owner_token) &&
    .builder_worker_owner_time(marker$created_at)
}

.builder_worker_cleanup_orphans <- function(worker) {
  report <- .builder_worker_cleanup_report()
  root <- worker$snapshot_root
  if (!.builder_worker_scalar_text(root) || !dir.exists(root)) {
    return(report)
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  registry <- worker$snapshot_registry
  if (!is.list(registry)) {
    registry <- list()
  }
  protected <- character()
  for (snapshot in registry) {
    path <- snapshot$path
    if (.builder_worker_scalar_text(path) && dir.exists(path)) {
      protected <- c(
        protected,
        normalizePath(path, winslash = "/", mustWork = TRUE)
      )
    }
  }
  protected <- unique(protected)

  candidates <- list.files(
    root,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE,
    recursive = FALSE,
    include.dirs = TRUE
  )
  for (candidate in candidates) {
    key <- basename(candidate)
    if (!dir.exists(candidate) || .builder_path_is_link(candidate)) {
      report$preserved <- c(report$preserved, key)
      next
    }
    canonical <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
    if (!identical(dirname(canonical), root)) {
      report$preserved <- c(report$preserved, key)
      report$errors <- c(report$errors, key)
      next
    }
    if (canonical %in% protected) {
      report$registered <- c(report$registered, key)
      next
    }

    snapshot_marker_path <- .builder_snapshot_marker_path(canonical)
    stage_marker_path <- .builder_stage_marker_path(canonical)
    if (file.exists(snapshot_marker_path)) {
      marker <- if (.builder_path_is_link(snapshot_marker_path)) {
        NULL
      } else {
        tryCatch(readRDS(snapshot_marker_path), error = function(error) NULL)
      }
      snapshot <- if (.builder_worker_snapshot_marker(marker)) {
        list(
          path = marker$path,
          object_file = file.path(canonical, "object.rds"),
          owner_token = marker$owner_token,
          created_at = marker$created_at,
          object_md5 = marker$object_md5,
          object_state = marker$object_state,
          serialization = marker$serialization %||% "rds"
        )
      } else {
        NULL
      }
      if (
        !is.list(snapshot) ||
          !identical(snapshot$path, canonical) ||
          !isTRUE(.builder_snapshot_owned_at(
            canonical,
            snapshot,
            verify_content = FALSE
          )) ||
          !isTRUE(.builder_snapshot_release(snapshot))
      ) {
        report$preserved <- c(report$preserved, key)
        report$errors <- c(report$errors, key)
      } else {
        report$snapshots <- c(report$snapshots, key)
      }
      next
    }
    if (file.exists(stage_marker_path)) {
      owner <- if (.builder_path_is_link(stage_marker_path)) {
        NULL
      } else {
        tryCatch(readRDS(stage_marker_path), error = function(error) NULL)
      }
      if (
        !.builder_worker_stage_marker(owner) ||
          !identical(owner$path, canonical) ||
          !isTRUE(.builder_stage_owned_at(canonical, owner)) ||
          !isTRUE(.builder_stage_release(canonical, owner))
      ) {
        report$preserved <- c(report$preserved, key)
        report$errors <- c(report$errors, key)
      } else {
        report$stages <- c(report$stages, key)
      }
      next
    }
    report$preserved <- c(report$preserved, key)
  }
  report
}

.builder_worker_grace_ms <- function(grace_ms) {
  if (!.builder_worker_scalar_integer(grace_ms)) {
    stop(
      "Worker grace must be a non-negative number of milliseconds.",
      call. = FALSE
    )
  }
  as.integer(min(as.numeric(grace_ms), .Machine$integer.max))
}

.builder_worker_cleanup_safe <- function(worker) {
  !identical(worker$cleanup_safe, FALSE)
}

.builder_worker_session_state <- function(process) {
  state <- try(process$get_state(), silent = TRUE)
  if (inherits(state, "try-error")) {
    return(NULL)
  }
  as.character(state)[1L]
}

.builder_worker_remaining_ms <- function(deadline) {
  remaining <- max(
    0,
    floor(as.numeric(difftime(deadline, Sys.time(), units = "secs")) * 1000)
  )
  as.integer(min(remaining, .Machine$integer.max))
}

.builder_worker_tree_handles <- function(process) {
  if (!requireNamespace("ps", quietly = TRUE)) {
    return(list(
      handles = list(),
      error = "The ps package is required to verify worker descendants."
    ))
  }
  # processx and ps can retain different boot-time estimates after a clock
  # adjustment (observed under WSL). Let ps create its own handle, while
  # processx verifies the owned child on both sides of the PID lookup. A PID
  # alone must never authorize inspection or cleanup of a replacement process.
  parent <- try(
    {
      if (!isTRUE(process$is_alive())) {
        stop("The owned background worker has exited.")
      }
      handle <- ps::ps_handle(process$get_pid())
      if (!isTRUE(process$is_alive())) {
        stop("The owned background worker exited during inspection.")
      }
      handle
    },
    silent = TRUE
  )
  if (inherits(parent, "try-error")) {
    return(list(
      handles = list(),
      error = "The background worker process tree could not be inspected."
    ))
  }
  # processx tags the worker and every descendant that inherits its environment
  # with a private marker. One marker scan costs ~10 ms, whereas
  # ps_children() opens a handle on every process on the machine to read its
  # parent (~40 s with ~650 processes on Windows). A marker match, not a PID,
  # identifies each member, and ps re-checks the creation time.
  marker <- .builder_worker_tree_marker(process)
  descendants <- try(
    if (is.null(marker)) {
      ps::ps_children(parent, recursive = TRUE)
    } else {
      ps::ps_find_tree(marker)
    },
    silent = TRUE
  )
  if (inherits(descendants, "try-error")) {
    return(list(
      handles = list(),
      error = paste0(
        "The background worker descendants could not be inspected: ",
        conditionMessage(attr(descendants, "condition"))
      )
    ))
  }
  list(
    handles = .builder_worker_merge_tree_handles(list(parent), descendants),
    error = NULL
  )
}

## The processx process-tree marker (an environment variable name inherited by
## every child), or NULL for a process that was not started by processx.
.builder_worker_tree_marker <- function(process) {
  private <- tryCatch(
    process$.__enclos_env__$private,
    error = function(error) NULL
  )
  marker <- if (is.environment(private)) {
    get0("tree_id", envir = private, inherits = FALSE)
  } else {
    NULL
  }
  if (
    !.builder_worker_scalar_text(marker) ||
      !grepl("^PS[0-9A-Za-z]+_[0-9]+$", marker)
  ) {
    return(NULL)
  }
  marker
}

.builder_worker_handle_alive <- function(handle) {
  tryCatch(
    {
      if (!isTRUE(ps::ps_is_running(handle))) {
        return(FALSE)
      }
      status <- tryCatch(ps::ps_status(handle), error = identity)
      if (inherits(status, "no_such_process")) {
        return(FALSE)
      }
      if (inherits(status, "condition")) {
        return(TRUE)
      }
      !tolower(as.character(status)[[1L]]) %in% c("zombie", "dead")
    },
    error = function(error) !inherits(error, "no_such_process")
  )
}

.builder_worker_merge_tree_handles <- function(handles, additions) {
  combined <- c(handles, additions)
  if (!length(combined)) {
    return(combined)
  }
  pids <- vapply(
    combined,
    function(handle) {
      tryCatch(
        as.character(ps::ps_pid(handle)),
        error = function(error) NA_character_
      )
    },
    character(1)
  )
  missing <- is.na(pids)
  pids[missing] <- paste0("unknown-", which(missing))
  combined[!duplicated(pids)]
}

.builder_worker_refresh_tree <- function(process, handles) {
  tree <- .builder_worker_tree_handles(process)
  if (!is.null(tree$error)) {
    return(list(handles = handles, error = tree$error))
  }
  list(
    handles = .builder_worker_merge_tree_handles(handles, tree$handles),
    error = NULL
  )
}

.builder_worker_kill_handles <- function(handles) {
  for (handle in rev(handles)) {
    if (.builder_worker_handle_alive(handle)) {
      try(ps::ps_kill(handle), silent = TRUE)
    }
  }
  invisible(handles)
}

.builder_worker_wait_tree <- function(handles, deadline) {
  repeat {
    alive <- vapply(
      handles,
      .builder_worker_handle_alive,
      logical(1)
    )
    if (!any(alive)) {
      return(TRUE)
    }
    if (Sys.time() >= deadline) {
      return(FALSE)
    }
    Sys.sleep(0.01)
  }
}

#' Stop a worker, confirm process death, then clean verified orphan artifacts.
#'
#' `verify_tree = TRUE` lets an idle or busy worker unwind first and confirms
#' every marked descendant is gone before orphan cleanup. `verify_tree = FALSE`
#' kills the marked tree at once without waiting for an interrupt (which callr
#' cannot deliver on Windows); user-requested stops use it.
## Asks a busy worker session to stop its current call, so the call's
## on.exit() handlers (rollbacks) run before any forced kill. On Windows a
## CTRL+C event (what callr's interrupt() sends) does not reach a busy R
## session; CTRL+BREAK does, and R handles it as the same user interrupt.
.builder_worker_interrupt <- function(
  process,
  os_type = .Platform$OS.type,
  .ctrl_break = function(pid) {
    ps::ps_interrupt(ps::ps_handle(pid), ctrl_c = FALSE)
  }
) {
  if (identical(os_type, "windows")) {
    sent <- tryCatch(
      {
        .ctrl_break(process$get_pid())
        TRUE
      },
      error = function(error) FALSE
    )
    if (isTRUE(sent)) {
      return(invisible(TRUE))
    }
  }
  invisible(!inherits(try(process$interrupt(), silent = TRUE), "try-error"))
}

builder_worker_stop <- function(worker, grace_ms = 5000L, verify_tree = TRUE) {
  .builder_worker_assert(worker)
  grace_ms <- .builder_worker_grace_ms(grace_ms)
  graceful_deadline <- Sys.time() + grace_ms / 1000
  force_ms <- max(2000L, min(grace_ms, 5000L))
  force_deadline <- graceful_deadline + force_ms / 1000
  cleanup_safe <- .builder_worker_cleanup_safe(worker)
  alive <- try(worker$process$is_alive(), silent = TRUE)
  if (inherits(alive, "try-error")) {
    worker$ready <- FALSE
    worker$cleanup_safe <- FALSE
    return(list(
      worker = worker,
      stopped = FALSE,
      tree_verified = FALSE,
      restart_allowed = FALSE,
      quarantined = TRUE,
      error = "The background worker state could not be inspected.",
      cleanup = NULL
    ))
  }

  if (!isTRUE(alive)) {
    worker$ready <- FALSE
    worker$cleanup_safe <- FALSE
    return(list(
      worker = worker,
      stopped = FALSE,
      tree_verified = FALSE,
      restart_allowed = TRUE,
      quarantined = TRUE,
      error = paste0(
        "The background worker parent already exited; its process tree ",
        "cannot be verified."
      ),
      cleanup = NULL
    ))
  }

  tree <- if (isTRUE(verify_tree)) {
    .builder_worker_tree_handles(worker$process)
  } else {
    list(handles = list(), error = NULL)
  }
  if (!is.null(tree$error)) {
    worker$ready <- FALSE
    worker$cleanup_safe <- FALSE
    return(list(
      worker = worker,
      stopped = FALSE,
      tree_verified = FALSE,
      restart_allowed = FALSE,
      quarantined = TRUE,
      error = tree$error,
      cleanup = NULL
    ))
  }
  handles <- tree$handles
  tree_verified <- TRUE

  state <- .builder_worker_session_state(worker$process)
  if (!isTRUE(verify_tree)) {
    try(worker$process$kill_tree(), silent = TRUE)
  } else if (identical(worker$state, "starting")) {
    try(worker$process$kill_tree(), silent = TRUE)
    .builder_worker_kill_handles(handles)
  } else if (identical(state, "idle")) {
    try(worker$process$close(), silent = TRUE)
  } else {
    .builder_worker_interrupt(worker$process)
    repeat {
      alive_now <- try(worker$process$is_alive(), silent = TRUE)
      if (inherits(alive_now, "try-error")) {
        tree_verified <- FALSE
        break
      }
      if (!isTRUE(alive_now)) {
        break
      }
      refreshed <- .builder_worker_refresh_tree(worker$process, handles)
      handles <- refreshed$handles
      if (!is.null(refreshed$error)) {
        parent_alive <- try(
          worker$process$is_alive(),
          silent = TRUE
        )
        if (isTRUE(parent_alive)) {
          tree_verified <- FALSE
        }
      }
      remaining_ms <- .builder_worker_remaining_ms(graceful_deadline)
      if (remaining_ms <= 0L) {
        break
      }
      process_state <- .builder_worker_process_state(
        worker$process,
        min(remaining_ms, 50L)
      )
      if (identical(process_state, "ready")) {
        result <- .builder_worker_read_ready(worker$process)
        if (isTRUE(result$done)) {
          refreshed <- .builder_worker_refresh_tree(worker$process, handles)
          handles <- refreshed$handles
          if (!is.null(refreshed$error)) {
            parent_alive <- try(
              worker$process$is_alive(),
              silent = TRUE
            )
            if (isTRUE(parent_alive)) {
              tree_verified <- FALSE
            }
          }
          try(worker$process$close(), silent = TRUE)
          break
        }
      } else if (identical(process_state, "closed")) {
        break
      }
    }
  }

  alive <- try(worker$process$is_alive(), silent = TRUE)
  known_alive <- any(vapply(
    handles,
    .builder_worker_handle_alive,
    logical(1)
  ))
  if (isTRUE(alive) || known_alive) {
    if (isTRUE(alive) && isTRUE(verify_tree)) {
      refreshed <- .builder_worker_refresh_tree(worker$process, handles)
      handles <- refreshed$handles
      if (!is.null(refreshed$error)) {
        parent_alive <- try(
          worker$process$is_alive(),
          silent = TRUE
        )
        if (isTRUE(parent_alive)) {
          tree_verified <- FALSE
        }
      }
    }
    if (isTRUE(alive)) {
      try(worker$process$kill_tree(), silent = TRUE)
    }
    .builder_worker_kill_handles(handles)
    try(
      worker$process$wait(
        timeout = .builder_worker_remaining_ms(force_deadline)
      ),
      silent = TRUE
    )
  }

  try(
    worker$process$wait(
      timeout = .builder_worker_remaining_ms(force_deadline)
    ),
    silent = TRUE
  )
  alive <- try(worker$process$is_alive(), silent = TRUE)
  tree_stopped <- .builder_worker_wait_tree(handles, force_deadline)
  parent_stopped <- !inherits(alive, "try-error") && !isTRUE(alive)
  stopped <- parent_stopped && tree_stopped && tree_verified
  if (!isTRUE(stopped)) {
    cleanup_safe <- FALSE
  }

  worker$ready <- FALSE
  worker$cleanup_safe <- cleanup_safe

  cleanup <- NULL
  if (isTRUE(stopped) && cleanup_safe) {
    cleanup <- try(.builder_worker_cleanup_orphans(worker), silent = TRUE)
    if (inherits(cleanup, "try-error")) {
      cleanup <- .builder_worker_cleanup_report()
      cleanup$errors <- "snapshot-root"
    }
  }
  error <- NULL
  if (!isTRUE(stopped)) {
    error <- if (!parent_stopped) {
      paste0(
        "The background worker did not stop within ",
        grace_ms,
        " milliseconds."
      )
    } else {
      "The background worker process tree cannot be verified."
    }
  }
  list(
    worker = worker,
    stopped = stopped,
    tree_verified = isTRUE(stopped),
    restart_allowed = parent_stopped,
    quarantined = !cleanup_safe,
    error = error,
    cleanup = cleanup
  )
}

.builder_worker_restart_failure <- function(worker, error, cleanup = NULL) {
  worker$ready <- FALSE
  worker$error <- error
  structure(
    list(
      worker = worker,
      result = NULL,
      event = "restart_failed",
      error = error,
      cleanup = cleanup
    ),
    class = c("builder_worker_restart_failed", "list")
  )
}

#' Recreate a worker from the same main-owned snapshot registry.
builder_worker_restart <- function(
  worker,
  grace_ms = 5000L,
  .async = FALSE,
  verify_tree = TRUE
) {
  .builder_worker_assert(worker)
  stopped <- builder_worker_stop(
    worker,
    grace_ms = grace_ms,
    verify_tree = verify_tree
  )
  if (!isTRUE(stopped$stopped) && !isTRUE(stopped$restart_allowed)) {
    return(.builder_worker_restart_failure(
      stopped$worker,
      stopped$error,
      cleanup = stopped$cleanup
    ))
  }
  restarted <- builder_worker_start(
    builder_dir = stopped$worker$builder_dir,
    snapshot_root = stopped$worker$snapshot_root,
    snapshot_registry = stopped$worker$snapshot_registry,
    .async = .async
  )
  if (!is.null(restarted$error)) {
    return(.builder_worker_restart_failure(
      stopped$worker,
      restarted$error,
      cleanup = stopped$cleanup
    ))
  }
  restarted$owns_root <- worker$owns_root
  restarted$cleanup_safe <- .builder_worker_cleanup_safe(stopped$worker)
  restarted["cleanup"] <- list(stopped$cleanup)
  restarted
}

#' Count cells in one restored object without returning the object itself.
builder_worker_cell_count <- function(worker, id) {
  .builder_worker_assert(worker)
  worker$process$run(
    function(id) {
      object <- .builder_worker_object(id)
      as.integer(ncol(object))
    },
    args = list(id = id)
  )
}

#' Add a worker-created snapshot descriptor to main-process ownership.
builder_worker_register_snapshot <- function(worker, id, snapshot) {
  .builder_worker_assert(worker)
  if (!.builder_worker_scalar_text(id)) {
    stop("A dataset id is required for snapshot registration.", call. = FALSE)
  }
  # The worker hashed object.rds while writing it; repeating the full-file
  # MD5 here would block the Shiny process for every load. Stat and owner
  # marker checks still reject replaced or foreign snapshots.
  if (!isTRUE(.builder_snapshot_owned(snapshot, verify_content = FALSE))) {
    stop(
      "The worker did not return an owned immutable snapshot.",
      call. = FALSE
    )
  }
  snapshot_path <- normalizePath(snapshot$path, winslash = "/", mustWork = TRUE)
  snapshot_root <- normalizePath(
    worker$snapshot_root,
    winslash = "/",
    mustWork = TRUE
  )
  if (
    identical(snapshot_path, snapshot_root) ||
      !isTRUE(.builder_path_within(snapshot_path, snapshot_root))
  ) {
    stop("The snapshot is outside this worker's snapshot root.", call. = FALSE)
  }
  worker$snapshot_registry[[id]] <- snapshot
  fingerprint <- snapshot$source_fingerprint %||% NULL
  if (.builder_worker_scalar_text(fingerprint)) {
    worker$snapshot_cache[[fingerprint]] <- snapshot
  }
  worker
}

#' Release one main-owned snapshot after its persistent drop is acknowledged.
builder_worker_release_snapshot <- function(
  worker,
  id,
  expected_identity
) {
  .builder_worker_assert(worker)
  if (!.builder_worker_scalar_text(id)) {
    stop("A dataset id is required for snapshot release.", call. = FALSE)
  }
  snapshot <- worker$snapshot_registry[[id]]
  if (is.null(snapshot)) {
    stop("The dataset has no registered snapshot.", call. = FALSE)
  }
  if (
    !.builder_worker_scalar_text(expected_identity) ||
      !identical(.builder_worker_identity(snapshot), expected_identity)
  ) {
    stop("The snapshot release identity is stale.", call. = FALSE)
  }
  fingerprint <- snapshot$source_fingerprint %||% NULL
  cacheable <- .builder_worker_scalar_text(fingerprint)
  if (.builder_worker_cleanup_safe(worker) && !cacheable) {
    if (!isTRUE(.builder_snapshot_release(snapshot))) {
      stop("The owned immutable snapshot could not be released.", call. = FALSE)
    }
  }
  if (cacheable) {
    worker$snapshot_cache[[fingerprint]] <- snapshot
  }
  worker$snapshot_registry[[id]] <- NULL
  worker
}

## The replacement starts in the background (state "starting"); callers poll
## builder_worker_poll_startup() before dispatching to it.
.builder_worker_poll_restart <- function(worker) {
  restarted <- builder_worker_restart(worker, .async = TRUE)
  if (inherits(restarted, "builder_worker_restart_failed")) {
    return(restarted)
  }
  structure(
    list(
      worker = restarted,
      result = NULL,
      event = "restarted",
      error = NULL,
      cleanup = restarted$cleanup
    ),
    class = c("builder_worker_restart_event", "list")
  )
}

#' Poll once, restarting after a closed worker pipe.
builder_worker_poll <- function(worker, timeout = 0) {
  .builder_worker_assert(worker)
  state <- .builder_worker_process_state(worker$process, timeout)
  if (identical(state, "closed")) {
    return(.builder_worker_poll_restart(worker))
  }
  if (identical(state, "ready")) {
    result <- .builder_worker_read_ready(worker$process)
    if (isTRUE(result$done)) {
      return(list(
        worker = worker,
        result = result,
        event = "completed",
        error = NULL
      ))
    }
  }
  list(worker = worker, result = NULL, event = NULL, error = NULL)
}
