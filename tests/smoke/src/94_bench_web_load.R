#!/usr/bin/env Rscript
# 94 — Web load benchmark via callr + chromote (no shinytest2)
#
# Companion to 93_bench_backend_compare.R, covering the dimension that
# 93 cannot: end-to-end "open URL -> dataset visible in browser" wall-
# clock time as perceived by a real headless Chrome session.
#
# Why not shinytest2: AppDriver$new() wraps the app in `test.mode` and
# blocks on its own "Shiny session reaches first idle" detector, which
# never settles for cerebroAppLite within reasonable timeouts on the
# PBMC All Samples fixture. We bypass that by spawning the Shiny app
# directly via callr::r_bg and driving headless Chrome via chromote.
#
# Per backend the script:
#   1. Generates a single-dataset Shiny app via createShinyApp().
#   2. Spawns `shiny::runApp(...)` in a fresh R subprocess (callr::r_bg).
#   3. Polls the port until it accepts TCP — this is "server ready",
#      not part of the user-facing measurement.
#   4. Opens a fresh chromote::ChromoteSession, records t0, navigates
#      to the URL, then polls the DOM for the dataset's cell-count
#      string until it appears.
#   5. cell_count_visible_ms = t1 - t0  (the user-perceived metric).
#   6. Pulls TTFB / DOM-ready / load-event from the browser's
#      Navigation Timing API for context.
#   7. Tears down chromote + the R subprocess before moving on.
#
# Output: result/93_bench_backend_compare/web_load.csv  (overwrites the
#         file consumed by 95_bench_backend_plot.R, so re-running 94
#         after this script regenerates the 5th panel of summary.png).
#
# Run: Rscript src/94_bench_web_load.R
#
# Depends on:
#   - tests/smoke/result/{10_convert_embedded, 11_convert_bpcells,
#     12_convert_h5}/cerebro_*.crb (+ siblings) having been generated.
#   - callr, chromote, httr, jsonlite R packages.
#   - A working headless Chrome (chromote::find_chrome()).
#   - PBMC All Samples fixture: cell count is hard-coded to 147,756
#     below; change `cell_count_text` below if you bench a different
#     dataset.

pkg_root <- file.path(dirname(getwd()), "..")
suppressPackageStartupMessages({
  devtools::load_all(pkg_root, quiet = TRUE)
  library(callr)
  library(chromote)
  library(httr)
  library(jsonlite)
})

result_dir <- "result/93_bench_backend_compare"
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)
log_file <- file.path(result_dir, "web_load.log")
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)

backends <- list(
  embedded = "result/10_convert_embedded/cerebro_PBMC_All_Samples_TCR_BCR.crb",
  bpcells = "result/11_convert_bpcells/cerebro_PBMC_All_Samples_TCR_BCR.crb",
  h5 = "result/12_convert_h5/cerebro_PBMC_All_Samples_TCR_BCR.crb"
)

read_crb_object <- function(crb_path) {
  con <- file(crb_path, "rb")
  on.exit(close(con), add = TRUE)
  hdr <- readBin(con, "raw", n = 2)
  is_gzip <- length(hdr) == 2 && hdr[1] == 0x1f && hdr[2] == 0x8b
  if (is_gzip) {
    return(readRDS(crb_path))
  }
  if (!requireNamespace("qs", quietly = TRUE)) {
    stop("crb is not gzip-RDS and qs is not available", call. = FALSE)
  }
  qs::qread(crb_path, nthreads = 2)
}

get_cell_count_text <- function(crb_path) {
  obj <- read_crb_object(crb_path)
  meta_n <- tryCatch(nrow(obj$getMetaData()), error = function(e) NULL)
  if (is.numeric(meta_n) && length(meta_n) == 1 && !is.na(meta_n)) {
    return(format(meta_n, big.mark = ","))
  }

  expr_n <- tryCatch(ncol(obj$expression), error = function(e) NULL)
  if (is.numeric(expr_n) && length(expr_n) == 1 && !is.na(expr_n)) {
    return(format(expr_n, big.mark = ","))
  }

  stop("Could not determine cell count text from crb: ", crb_path, call. = FALSE)
}

# Tuning knobs (override via env var if needed).
server_ready_timeout_secs <- as.integer(Sys.getenv(
  "BENCH_SERVER_TIMEOUT_SECS",
  120
))
poll_timeout_secs <- as.integer(Sys.getenv("BENCH_POLL_TIMEOUT_SECS", 240))
poll_interval_secs <- 0.5

wait_for_port <- function(port, timeout_secs) {
  url <- sprintf("http://127.0.0.1:%d", port)
  deadline <- Sys.time() + timeout_secs
  while (Sys.time() < deadline) {
    ok <- tryCatch(
      {
        r <- httr::GET(url, httr::timeout(2))
        httr::status_code(r) %in% c(200L, 302L, 404L) # any HTTP response = listening
      },
      error = function(e) FALSE
    )
    if (ok) {
      return(TRUE)
    }
    Sys.sleep(0.5)
  }
  FALSE
}

wait_for_dom_text <- function(b, text, timeout_secs) {
  deadline <- Sys.time() + timeout_secs
  while (Sys.time() < deadline) {
    body_html <- tryCatch(
      b$Runtime$evaluate("document.body ? document.body.innerHTML : ''")$
        result$value,
      error = function(e) ""
    )
    if (is.character(body_html) && grepl(text, body_html, fixed = TRUE)) {
      return(TRUE)
    }
    Sys.sleep(poll_interval_secs)
  }
  FALSE
}

read_perf_timing <- function(b) {
  raw <- tryCatch(
    b$Runtime$evaluate("JSON.stringify(performance.timing.toJSON())")$
      result$value,
    error = function(e) NA_character_
  )
  if (is.na(raw) || !nzchar(raw)) {
    return(list(ttfb_ms = NA, dom_ready_ms = NA, load_event_ms = NA))
  }
  perf <- tryCatch(jsonlite::fromJSON(raw), error = function(e) NULL)
  if (is.null(perf) || is.null(perf$navigationStart)) {
    return(list(ttfb_ms = NA, dom_ready_ms = NA, load_event_ms = NA))
  }
  list(
    ttfb_ms = as.integer(perf$responseStart - perf$navigationStart),
    dom_ready_ms = as.integer(
      perf$domContentLoadedEventEnd - perf$navigationStart
    ),
    load_event_ms = as.integer(perf$loadEventEnd - perf$navigationStart)
  )
}

bench_one <- function(name, crb) {
  if (!file.exists(crb)) {
    message(sprintf("[%s] SKIP — crb not found: %s", name, crb))
    return(data.frame(
      backend = name,
      ttfb_ms = NA_integer_,
      dom_ready_ms = NA_integer_,
      load_event_ms = NA_integer_,
      cell_count_visible_ms = NA_integer_,
      measurement = "skipped_no_crb",
      stringsAsFactors = FALSE
    ))
  }

  cell_count_text <- get_cell_count_text(crb)

  port <- 8090L + match(name, names(backends))
  url <- sprintf("http://127.0.0.1:%d", port)

  message(sprintf(
    "[%s] generating single-dataset app at port %d ...",
    name,
    port
  ))
  app_dir <- file.path(tempdir(), paste0("perftest_", name))
  unlink(app_dir, recursive = TRUE)
  createShinyApp(
    cerebro_data = setNames(crb, paste0(name, "_PBMC_All")),
    result_dir = app_dir,
    port = port,
    launch_browser = FALSE,
    quiet = TRUE,
    overwrite = TRUE,
    show_upload_ui = FALSE,
    welcome_message = sprintf("Bench (%s)", name)
  )

  proc <- NULL
  b <- NULL
  on.exit(
    {
      try(if (!is.null(b)) b$close(), silent = TRUE)
      try(if (!is.null(proc) && proc$is_alive()) proc$kill(), silent = TRUE)
    },
    add = TRUE
  )

  message(sprintf("[%s] spawning Shiny in callr::r_bg ...", name))
  proc <- callr::r_bg(
    function(app_dir, port) {
      shiny::runApp(
        appDir = app_dir,
        host = "127.0.0.1",
        port = port,
        launch.browser = FALSE,
        quiet = TRUE
      )
    },
    args = list(app_dir = app_dir, port = port),
    supervise = TRUE,
    stdout = "|",
    stderr = "|"
  )

  message(sprintf(
    "[%s] waiting for port %d to listen (timeout=%d s) ...",
    name,
    port,
    server_ready_timeout_secs
  ))
  if (!wait_for_port(port, server_ready_timeout_secs)) {
    proc_stderr <- tryCatch(proc$read_all_error(), error = function(e) "")
    message(sprintf(
      "[%s] TIMEOUT waiting for port; proc alive=%s, stderr last 500 chars:\n%s",
      name,
      proc$is_alive(),
      substr(proc_stderr, max(1, nchar(proc_stderr) - 500), nchar(proc_stderr))
    ))
    return(data.frame(
      backend = name,
      ttfb_ms = NA_integer_,
      dom_ready_ms = NA_integer_,
      load_event_ms = NA_integer_,
      cell_count_visible_ms = NA_integer_,
      measurement = "server_did_not_listen",
      stringsAsFactors = FALSE
    ))
  }
  message(sprintf("[%s] server is up at %s", name, url))

  # Fresh chromote session per backend so caches do not bleed across.
  message(sprintf("[%s] launching chromote session ...", name))
  b <- chromote::ChromoteSession$new()

  t0 <- Sys.time()
  b$Page$navigate(url)
  message(sprintf(
    "[%s] navigated; polling DOM for '%s' (timeout=%d s) ...",
    name,
    cell_count_text,
    poll_timeout_secs
  ))
  matched <- wait_for_dom_text(b, cell_count_text, poll_timeout_secs)
  t1 <- Sys.time()

  cell_count_visible_ms <- as.integer(
    as.numeric(difftime(t1, t0, units = "secs")) * 1000
  )
  perf <- read_perf_timing(b)

  message(sprintf(
    "[%s] result: %d ms (matched=%s, ttfb=%s, dom_ready=%s, load=%s)",
    name,
    cell_count_visible_ms,
    matched,
    perf$ttfb_ms,
    perf$dom_ready_ms,
    perf$load_event_ms
  ))

  # explicit teardown so the next backend starts clean
  try(b$close(), silent = TRUE)
  b <- NULL
  try(proc$kill(), silent = TRUE)
  proc <- NULL

  data.frame(
    backend = name,
    ttfb_ms = perf$ttfb_ms,
    dom_ready_ms = perf$dom_ready_ms,
    load_event_ms = perf$load_event_ms,
    cell_count_visible_ms = cell_count_visible_ms,
    measurement = if (matched) {
      "callr_chromote_cold"
    } else {
      "callr_chromote_TIMEOUT"
    },
    stringsAsFactors = FALSE
  )
}

message(sprintf("[%s] starting web load bench ...", Sys.time()))
results <- do.call(
  rbind,
  lapply(names(backends), function(b) bench_one(b, backends[[b]]))
)

out_csv <- file.path(result_dir, "web_load.csv")
write.csv(results, out_csv, row.names = FALSE)
message(sprintf("\n[%s] wrote %s", Sys.time(), out_csv))
print(results)

sink()
close(log_con)
