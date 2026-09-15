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
expected <- Sys.getenv("CEREBRO_RENDERER_BACKEND", unset = "")
force_webgl <- identical(expected, "webgl2")
if (force_webgl) {
  chrome_args <- setdiff(chromote::get_chrome_args(), "--disable-gpu")
  chromote::set_chrome_args(c(
    chrome_args,
    "--enable-webgl",
    "--ignore-gpu-blocklist",
    "--use-angle=swiftshader"
  ))
}
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

driver$run_js(sprintf(
  paste0(
    "window.__cerebroMillionResult=null;",
    "window.__cerebroMillionError=null;",
    "window.__cerebroMillionDone=false;",
    if (force_webgl) paste0(
      "window.__cerebroMillionFactory=CerebroPointRenderer.create;",
      "CerebroPointRenderer.create=CerebroPointRenderer.createWebGl;"
    ) else "",
    "runMillionCellBenchmark({count:%d,repeats:%d})",
    ".then(function(result){window.__cerebroMillionResult=result;})",
    ".catch(function(error){window.__cerebroMillionError=String(",
    "error&&(error.stack||error.message)||error);})",
    ".finally(function(){",
    if (force_webgl) paste0(
      "CerebroPointRenderer.create=window.__cerebroMillionFactory;",
      "delete window.__cerebroMillionFactory;"
    ) else "",
    "window.__cerebroMillionDone=true;});"
  ),
  count,
  repeats
))
deadline <- Sys.time() + 180
while (!isTRUE(driver$get_js("window.__cerebroMillionDone === true"))) {
  if (Sys.time() >= deadline) stop("Timed out waiting for renderer benchmark.")
  Sys.sleep(0.05)
}
error <- driver$get_js("window.__cerebroMillionError")
if (length(error) && !is.null(error) && nzchar(error)) stop(error)
result <- as.data.frame(
  driver$get_js("window.__cerebroMillionResult"),
  stringsAsFactors = FALSE
)
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
