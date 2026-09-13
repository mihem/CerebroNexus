#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop(
    "Usage: benchmark_million_cell_startup.R LABEL=CRB [LABEL=CRB ...]",
    call. = FALSE
  )
}
separators <- regexpr("=", args, fixed = TRUE)
if (any(separators < 2L)) {
  stop("Each candidate must use LABEL=CRB syntax.", call. = FALSE)
}
labels <- substr(args, 1L, separators - 1L)
paths <- substring(args, separators + 1L)
if (any(!nzchar(labels)) || anyDuplicated(labels)) {
  stop("Candidate labels must be non-empty and unique.", call. = FALSE)
}
paths <- vapply(paths, normalizePath, character(1), mustWork = TRUE)
names(paths) <- labels
repeats <- as.integer(Sys.getenv("CEREBRO_STARTUP_REPEATS", unset = "3"))
if (is.na(repeats) || repeats < 1L) {
  stop("CEREBRO_STARTUP_REPEATS must be a positive integer.", call. = FALSE)
}
Sys.setenv(NOT_CRAN = "true")

wait_for_port <- function(port, process, timeout = 30) {
  deadline <- Sys.time() + timeout
  repeat {
    connection <- suppressWarnings(try(
      socketConnection(
        "127.0.0.1",
        port,
        open = "r+",
        blocking = TRUE,
        timeout = 0.1
      ),
      silent = TRUE
    ))
    if (!inherits(connection, "try-error")) {
      close(connection)
      return(invisible(TRUE))
    }
    if (!process$is_alive()) {
      stop(paste(process$read_all_error(), collapse = "\n"), call. = FALSE)
    }
    if (Sys.time() > deadline) {
      stop("The Viewer did not start listening in time.", call. = FALSE)
    }
    Sys.sleep(0.05)
  }
}

browser <- chromote::Chromote$new()
on.exit(browser$close(), add = TRUE)
library_paths <- .libPaths()
rows <- list()

for (round in seq_len(repeats)) {
  order <- if (round %% 2L) labels else rev(labels)
  for (candidate in order) {
    port <- httpuv::randomPort()
    milestones <- tempfile("cerebro-startup-", fileext = ".tsv")
    started <- as.numeric(Sys.time())
    process <- callr::r_bg(
      function(library_paths, crb, port, milestones) {
        .libPaths(library_paths)
        mark <- function(stage) {
          cat(
            sprintf("%.6f\t%s\n", as.numeric(Sys.time()), stage),
            file = milestones,
            append = TRUE
          )
        }
        mark("process_start")
        library(CerebroNexus)
        mark("library_done")
        app <- launchCerebro(
          mode = "closed",
          crb_file_to_load = c("1M" = crb),
          percentage_cells_to_show = 100
        )
        mark("app_constructed")
        shiny::runApp(
          app,
          host = "127.0.0.1",
          port = port,
          launch.browser = FALSE
        )
      },
      args = list(library_paths, paths[[candidate]], port, milestones),
      env = c(NOT_CRAN = "true"),
      stdout = "|",
      stderr = "|"
    )
    session <- NULL
    result <- tryCatch(
      {
        wait_for_port(port, process)
        server_ready <- as.numeric(Sys.time())
        session <- chromote::ChromoteSession$new(
          parent = browser,
          width = 1619,
          height = 950
        )
        navigated <- as.numeric(Sys.time())
        invisible(session$Page$navigate(sprintf("http://127.0.0.1:%d/", port)))
        invisible(session$Page$loadEventFired())
        loaded <- as.numeric(Sys.time())
        invisible(session$Runtime$evaluate(
          "document.querySelector('a[href=\"#shiny-tab-loadData\"]').click()"
        ))
        deadline <- Sys.time() + 900
        repeat {
          value <- session$Runtime$evaluate(
            paste0(
              "document.getElementById('load_data_number_of_cells')",
              "?.innerText || ''"
            ),
            returnByValue = TRUE
          )$result$value
          if (grepl("1,000,000", value, fixed = TRUE)) {
            break
          }
          if (Sys.time() > deadline) {
            stop("Data Info did not report 1,000,000 cells in time.")
          }
          Sys.sleep(0.05)
        }
        ready <- as.numeric(Sys.time())
        marks <- utils::read.delim(
          milestones,
          header = FALSE,
          col.names = c("time", "stage")
        )
        lookup <- stats::setNames(marks$time, marks$stage)
        data.frame(
          candidate = candidate,
          round = round,
          library_ms = 1000 *
            (lookup[["library_done"]] - lookup[["process_start"]]),
          app_construct_ms = 1000 *
            (lookup[["app_constructed"]] - lookup[["library_done"]]),
          server_listen_ms = 1000 * (server_ready - started),
          browser_load_ms = 1000 * (loaded - navigated),
          load_to_data_ms = 1000 * (ready - loaded),
          browser_to_data_ms = 1000 * (ready - navigated),
          process_to_data_ms = 1000 * (ready - started)
        )
      },
      finally = {
        if (!is.null(session)) {
          session$close()
        }
        if (process$is_alive()) {
          process$kill()
        }
        process$wait(2000)
        unlink(milestones)
      }
    )
    rows[[length(rows) + 1L]] <- result
    print(result, row.names = FALSE)
  }
}

raw <- do.call(rbind, rows)
summary <- stats::aggregate(
  raw[setdiff(names(raw), c("candidate", "round"))],
  raw["candidate"],
  median
)
cat("RAW\n")
print(raw, row.names = FALSE)
cat("SUMMARY\n")
print(summary, row.names = FALSE)

output <- Sys.getenv("CEREBRO_STARTUP_OUTPUT", unset = "")
if (nzchar(output)) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(raw, output, row.names = FALSE)
}

gate_label <- Sys.getenv("CEREBRO_STARTUP_GATE_LABEL", unset = "")
if (nzchar(gate_label)) {
  if (!gate_label %in% summary$candidate) {
    stop("CEREBRO_STARTUP_GATE_LABEL must name a candidate.", call. = FALSE)
  }
  gate_ms <- suppressWarnings(as.numeric(Sys.getenv(
    "CEREBRO_STARTUP_MAX_PROCESS_TO_DATA_MS",
    unset = "3000"
  )))
  if (!is.finite(gate_ms) || gate_ms <= 0) {
    stop(
      "CEREBRO_STARTUP_MAX_PROCESS_TO_DATA_MS must be positive.",
      call. = FALSE
    )
  }
  observed_ms <- summary$process_to_data_ms[
    match(gate_label, summary$candidate)
  ]
  if (observed_ms >= gate_ms) {
    stop(
      sprintf(
        "Startup gate failed for %s: median %.3f ms must be < %.3f ms.",
        gate_label,
        observed_ms,
        gate_ms
      ),
      call. = FALSE
    )
  }
  cat(sprintf(
    "GATE PASS: %s median %.3f ms < %.3f ms\n",
    gate_label,
    observed_ms,
    gate_ms
  ))
}
