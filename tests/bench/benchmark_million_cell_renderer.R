args <- commandArgs(trailingOnly = TRUE)
count <- if (length(args)) as.integer(args[[1]]) else 1000000L
repeats <- if (length(args) > 1L) as.integer(args[[2]]) else 15L
output <- if (length(args) > 2L) args[[3]] else ""
screenshot <- if (length(args) > 3L) args[[4]] else ""
if (is.na(count) || count < 1L || is.na(repeats) || repeats < 3L) {
  stop(
    "Usage: benchmark_million_cell_renderer.R [points] [repeats] [output.csv]"
  )
}

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script <- normalizePath(sub("^--file=", "", script_arg[[1]]), mustWork = TRUE)
app_dir <- file.path(dirname(script), "million_cell_renderer_app")
Sys.setenv(NOT_CRAN = "true")
driver <- shinytest2::AppDriver$new(
  app_dir,
  name = "million-cell-renderer",
  load_timeout = 60000,
  timeout = 180000,
  width = 1400,
  height = 900,
  check_names = FALSE,
  clean_logs = TRUE
)
on.exit(driver$stop(), add = TRUE)

result <- driver$get_js(sprintf(
  "runMillionCellBenchmark({count:%d,repeats:%d})",
  count,
  repeats
))
result <- as.data.frame(result, stringsAsFactors = FALSE)
expected <- Sys.getenv("CEREBRO_RENDERER_BACKEND", unset = "")
if (nzchar(expected) && !identical(result$backend, expected)) {
  stop("Expected ", expected, " but benchmarked ", result$backend, ".")
}
if (nzchar(output)) {
  utils::write.csv(result, output, row.names = FALSE)
}
if (nzchar(screenshot)) {
  driver$get_screenshot(screenshot, selector = "#benchmark-canvas")
}
print(result, row.names = FALSE)
