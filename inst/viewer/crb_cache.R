## Process-owned CRB prototypes and in-flight reads. Load once per app process.
## Sessions supply an immutable backend identity and clone published prototypes.
.crb_process_cache <- new.env(parent = emptyenv())
.crb_backend_identities <- new.env(parent = emptyenv())
crb_prefetch_tasks <- new.env(parent = emptyenv())
crb_prefetch_started <- new.env(parent = emptyenv())
crb_prefetch_seen <- new.env(parent = emptyenv())

## A background read that a dataset switch has waited this long for is abandoned
## and the dataset is read in the foreground instead. The clock starts when the
## switch begins to wait (crb_prefetch_begin_wait), not when the task started: a
## read that began earlier, while the user was elsewhere, is not penalised.
## Without a limit a daemon that cannot start (a library path the child R does
## not see, a blocked socket) would leave the dataset switch waiting forever.
crb_prefetch_timeout_seconds <- 30
## The loader looks again every second while it waits. A longer gap means the
## user left and came back, which starts a new wait.
crb_prefetch_wait_gap_seconds <- 5

crb_prefetch_begin_wait <- function(key) {
  now <- as.numeric(Sys.time())
  seen <- crb_prefetch_seen[[key]]
  if (
    is.null(crb_prefetch_started[[key]]) ||
      (!is.null(seen) && now - seen > crb_prefetch_wait_gap_seconds)
  ) {
    crb_prefetch_started[[key]] <- now
  }
  crb_prefetch_seen[[key]] <- now
  invisible(NULL)
}

crb_prefetch_waited <- function(key) {
  started <- crb_prefetch_started[[key]]
  if (is.null(started)) 0 else as.numeric(Sys.time()) - started
}

crb_prefetch_abandon <- function(key) {
  if (exists(key, envir = crb_prefetch_tasks, inherits = FALSE)) {
    rm(list = key, envir = crb_prefetch_tasks)
  }
  for (clock in list(crb_prefetch_started, crb_prefetch_seen)) {
    if (exists(key, envir = clock, inherits = FALSE)) {
      rm(list = key, envir = clock)
    }
  }
}

.touchCrbPrototype <- function(key) {
  cached <- .crb_process_cache[[key]]
  if (!is.null(cached)) {
    cached$last_used <- as.numeric(Sys.time())
    .crb_process_cache[[key]] <- cached
  }
  cached
}

.cacheCrbPrototype <- function(path, prototype, backend_identity) {
  cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  .checkCrbBackendIdentity(cache_key, backend_identity)
  cached <- .touchCrbPrototype(cache_key)
  if (!is.null(cached)) {
    return(invisible(cached))
  }
  cached <- list(
    object = prototype,
    backend_identity = backend_identity,
    last_used = as.numeric(Sys.time())
  )
  .crb_process_cache[[cache_key]] <- cached
  ## Bound retained prototypes by count: object.size(R6) cannot measure their
  ## referenced payload reliably. Active session clones remain valid on eviction.
  keys <- ls(.crb_process_cache, all.names = TRUE)
  if (length(keys) > 2L) {
    older <- setdiff(keys, cache_key)
    ages <- vapply(
      older,
      function(key) .crb_process_cache[[key]]$last_used %||% 0,
      numeric(1)
    )
    rm(
      list = older[order(ages)][seq_len(length(keys) - 2L)],
      envir = .crb_process_cache
    )
  }
  invisible(cached)
}

.checkCrbBackendIdentity <- function(key, identity) {
  previous <- .crb_backend_identities[[key]]
  cached <- .crb_process_cache[[key]]
  if (
    (!is.null(previous) && !identical(previous$identity, identity)) ||
      (!is.null(cached) && !identical(cached$backend_identity, identity))
  ) {
    stop(
      "The cached CRB backend configuration changed after it was loaded. Start a new app session.",
      call. = FALSE
    )
  }
  .crb_backend_identities[[key]] <- list(identity = identity)
}

## nanonext attaches a mirai's promise callback without synchronising with
## task completion, so a task that finishes while the callback is attached
## leaves its promise pending forever. Windows scheduling makes this frequent
## for fast reads. Poll instead, and convert only a resolved mirai, which
## as.promise() settles synchronously with mirai's own error semantics.
.miraiPromise <- function(task, interval = 0.05) {
  promises::promise(function(resolve, reject) {
    check <- function() {
      if (mirai::unresolved(task)) {
        later::later(check, interval)
      } else {
        resolve(promises::as.promise(task))
      }
    }
    check()
  })
}

## mirai() for the Viewer; use it for every background task. Without daemons()
## each call starts a fresh Rscript, which sees only the library paths R derives
## from its environment, not those the Viewer added at run time (.libPaths(),
## renv, a launcher script). When mirai or qs2 lives there the child dies on
## loadNamespace() and mirai discards its stderr, so the task never answers. The
## child reads R_LIBS as it starts: set it to the Viewer's paths around the call
## that spawns it, then restore it. `.env`: more variables the child starts
## with (as R_NSIZE and R_VSIZE, its first heap).
.viewerMirai <- function(.expr, ..., .env = character()) {
  expr <- substitute(.expr)
  env <- c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep), .env)
  previous <- Sys.getenv(names(env), unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(env))
  on.exit({
    Sys.unsetenv(names(previous)[is.na(previous)])
    if (any(!is.na(previous))) {
      do.call(Sys.setenv, as.list(previous[!is.na(previous)]))
    }
  })
  mirai::mirai(expr, ...)
}

start_crb_prefetch <- function(path, backend_identity) {
  path <- as.character(path[[1L]])
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  cache_key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (
    !is.null(.crb_process_cache[[cache_key]]) ||
      !is.null(crb_prefetch_tasks[[cache_key]]) ||
      length(crb_prefetch_tasks) >= 2L
  ) {
    return(invisible(NULL))
  }
  # Snapshot the caller's identity before asynchronous completion; retain no
  # unevaluated session expression that could observe a later dataset switch.
  force(backend_identity)
  prefetch_task <- shiny::withReactiveDomain(
    NULL,
    shiny::ExtendedTask$new(function(path) {
      task <- .viewerMirai(
        {
          connection <- file(path, open = "rb")
          on.exit(close(connection), add = TRUE)
          magic <- readBin(connection, "raw", n = 4L)
          prototype <- if (
            identical(magic, as.raw(c(0x0b, 0x0e, 0x0a, 0xc1)))
          ) {
            qs2::qs_read(path)
          } else {
            readRDS(path)
          }
          list(path = path, prototype = prototype)
        },
        path = path
      )
      .miraiPromise(task)
    })
  )
  crb_prefetch_tasks[[cache_key]] <- prefetch_task
  message(glue::glue(
    "[{Sys.time()}] CRB background prefetch started: {basename(path)}"
  ))
  shiny::withReactiveDomain(NULL, prefetch_task$invoke(path))
  ## Completion belongs to the process, not the first requesting session.
  ## Publish once, then release the task's retained result even when the
  ## initiating user has left the dataset or disconnected.
  completion <- shiny::observe(
    {
      status <- prefetch_task$status()
      if (!status %in% c("success", "error")) {
        return()
      }
      on.exit(
        {
          ## An abandoned task must not remove the entries of a newer one.
          if (identical(crb_prefetch_tasks[[cache_key]], prefetch_task)) {
            crb_prefetch_abandon(cache_key)
          }
          completion$destroy()
        },
        add = TRUE
      )
      if (identical(status, "success")) {
        .cacheCrbPrototype(
          path,
          prefetch_task$result()$prototype,
          backend_identity
        )
      } else {
        tryCatch(prefetch_task$result(), error = function(error) {
          warning(
            "CRB background prefetch failed: ",
            conditionMessage(error),
            call. = FALSE
          )
        })
      }
    },
    domain = NULL
  )
  invisible(NULL)
}

## The Viewer process holds millions of R objects from its package imports, so
## every full garbage collection costs about a second. Opening a large CRB
## allocates more than R's default free heap, which forced one or two of them
## while Data info waited. Raise the heap trigger once at startup instead:
## requesting a block larger than the free space makes R collect once and grow
## its trigger, and the unused block is released by the next young collection.
.reserveCrbHeap <- function(paths) {
  sizes <- suppressWarnings(file.size(as.character(paths)))
  largest <- max(c(0, sizes), na.rm = TRUE)
  ## A qs2 CRB expands to about five times its file size in memory; leave room
  ## for that and its transient copies.
  bytes <- min(16 * largest, 4 * 1024^3)
  if (bytes < 256 * 1024^2) {
    return(invisible(FALSE))
  }
  invisible(tryCatch(
    {
      block <- raw(bytes)
      rm(block)
      TRUE
    },
    error = function(error) FALSE
  ))
}

## A thin CRB's H5 file must hold the CRB's cell names before its expression is
## read. Reading and hashing 1.3 M names took 0.86 s of the R thread, after the
## first Gene expression paint and again in every session. A daemon computes
## the hash as soon as the backend is attached; a confirmed file version is
## kept for the process. Without a confirmation the session reads the names.
.crb_h5_cell_checks <- new.env(parent = emptyenv())

.crbH5CellCheckKey <- function(path, expected) {
  info <- file.info(path, extra_cols = FALSE)
  paste(
    normalizePath(path, winslash = "/", mustWork = FALSE),
    info$size,
    format(as.numeric(info$mtime), digits = 17),
    expected,
    sep = "\r"
  )
}

crb_h5_cell_check_start <- function(path, expected) {
  key <- .crbH5CellCheckKey(path, expected)
  if (!exists(key, envir = .crb_h5_cell_checks, inherits = FALSE)) {
    entry <- new.env(parent = emptyenv())
    ## Give the child only the checksum helpers, not the Viewer's process or
    ## session environment. This also works in standalone apps, without the
    ## CerebroNexus package installed in the child.
    helpers <- new.env(parent = baseenv())
    for (name in c(
      ".runtimeCellNamesHash",
      ".runtimeCellNamesMatch"
    )) {
      helpers[[name]] <- get(name, mode = "function")
      environment(helpers[[name]]) <- helpers
    }
    entry$task <- tryCatch(
      .viewerMirai(
        matches(
          as.character(as.vector(rhdf5::h5read(path, "expression/genes"))),
          expected
        ),
        path = path,
        expected = expected,
        matches = helpers$.runtimeCellNamesMatch,
        .timeout = 30000
      ),
      error = function(error) NULL
    )
    assign(key, entry, envir = .crb_h5_cell_checks)
  }
  invisible(key)
}

## TRUE when a daemon (or an earlier session) confirmed the file's cell names.
## Never waits: while the daemon is still hashing (or hung), FALSE lets the
## caller check the names itself, and the task stays for later sessions.
crb_h5_cells_verified <- function(path, expected) {
  entry <- .crb_h5_cell_checks[[.crbH5CellCheckKey(path, expected)]]
  if (is.null(entry)) {
    return(FALSE)
  }
  if (is.null(entry$verified)) {
    if (inherits(entry$task, "mirai") && mirai::unresolved(entry$task)) {
      return(FALSE)
    }
    value <- if (inherits(entry$task, "mirai")) {
      mirai::call_mirai(entry$task)$data
    }
    entry$verified <- isTRUE(value)
    entry$task <- NULL
  }
  isTRUE(entry$verified)
}

## TRUE while a daemon is still hashing the file's cell names.
crb_h5_cells_pending <- function(path, expected) {
  entry <- .crb_h5_cell_checks[[.crbH5CellCheckKey(path, expected)]]
  !is.null(entry) &&
    is.null(entry$verified) &&
    inherits(entry$task, "mirai") &&
    mirai::unresolved(entry$task)
}

## Records a check the session made itself, for the sessions after it.
crb_h5_cells_confirm <- function(path, expected) {
  entry <- new.env(parent = emptyenv())
  entry$verified <- TRUE
  assign(.crbH5CellCheckKey(path, expected), entry, envir = .crb_h5_cell_checks)
  invisible(TRUE)
}
