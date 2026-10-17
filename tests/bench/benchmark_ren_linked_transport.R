#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L || !args[[3L]] %in% c("cold", "projection")) {
  stop("usage: benchmark_ren_linked_transport.R ROOT CRB cold|projection")
}

root <- normalizePath(args[[1L]], mustWork = TRUE)
crb <- normalizePath(args[[2L]], mustWork = TRUE)
scenario <- args[[3L]]
Sys.setenv(NOT_CRAN = "true")
## Chromote disables GPU acceleration by default on Windows. Linked Views uses
## its GPU canvas for the production million-cell path, so opt the dedicated
## transport benchmark back into the same accelerated surface it is measuring.
chrome_args <- setdiff(chromote::get_chrome_args(), "--disable-gpu")
chromote::set_chrome_args(c(
  chrome_args,
  "--enable-webgl",
  "--ignore-gpu-blocklist",
  "--use-angle=swiftshader"
))
quote_r <- function(value) encodeString(value, quote = '"')
script_argument <- grep("^--file=", commandArgs(), value = TRUE)[[1L]]
source(file.path(
  dirname(normalizePath(sub("^--file=", "", script_argument))),
  "lib",
  "viewer_launch.R"
))
load_mode <- viewer_bench_load_mode()
candidate <- prepare_viewer_candidates(
  list(candidate = c(label = "candidate", root = root)),
  load_mode
)[["candidate"]]

app_dir <- tempfile("ren-linked-transport-")
dir.create(app_dir)
on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
writeLines(
  c(
    viewer_load_line(candidate, load_mode),
    "launchCerebro(",
    "  mode = \"closed\",",
    sprintf("  crb_file_to_load = c(\"Ren\" = %s),", quote_r(crb)),
    "  percentage_cells_to_show = 100,",
    "  projections_show_hover_info = TRUE",
    ")"
  ),
  file.path(app_dir, "app.R")
)

suppressWarnings(shinytest2::local_app_support(app_dir))
app <- shinytest2::AppDriver$new(
  app_dir,
  name = paste0("ren-linked-", scenario),
  height = 950,
  width = 1619,
  load_timeout = 900000,
  timeout = 900000,
  check_names = FALSE
)
on.exit(app$stop(), add = TRUE)
wait_for_data_info_cells(app)

click_tab <- function(tab) {
  app$run_js(sprintf(
    "document.querySelector('a[href=\"#shiny-tab-%s\"]').click();",
    tab
  ))
}
click_linked_with_browser_clock <- function() {
  app$run_js(paste0(
    "(() => {const timing={clickAt:performance.now(),primaryAt:null,",
    "completeAt:null};window.__renLinkedTiming=timing;",
    "window.__cerebroPageBenchClickStart=timing.clickAt;",
    "const handler=e=>{const d=e.detail||{};",
    "if(d.primaryReady&&timing.primaryAt==null)timing.primaryAt=e.timeStamp;",
    "if(d.ready&&timing.completeAt==null){timing.completeAt=e.timeStamp;",
    "window.removeEventListener('cerebro:linkedviews-ready',handler);}};",
    "window.addEventListener('cerebro:linkedviews-ready',handler);",
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();})()"
  ))
}
wait_projection <- function() {
  app$wait_for_js(
    paste0(
      "(() => {const h=document.getElementById('overview_projection_cell_view_host');",
      "const g=h?.querySelector('canvas.cv-gpu-layer');",
      "return g?.style.display==='block'&&Number(g.dataset.pointCount)===1462702&&",
      "g._cerebroPointRenderer?.isReady();})()"
    ),
    timeout = 180000
  )
}
wait_linked <- function(primary = FALSE) {
  app$wait_for_js(
    sprintf(
      "window.cerebroLinkedViewsState?.%s?.()===true",
      if (primary) "primaryReady" else "ready"
    ),
    timeout = 180000
  )
}
heap <- function() {
  app$get_js(
    "performance.memory ? performance.memory.usedJSHeapSize : null"
  )
}

if (identical(scenario, "projection")) {
  click_tab("overview")
  wait_projection()
  app$wait_for_js(
    "Object.keys(window.CerebroSharedDatasetState?.projections||{}).length>0",
    timeout = 30000
  )
}
heap_before <- heap()
started <- proc.time()[["elapsed"]]
click_linked_with_browser_clock()
wait_linked(primary = TRUE)
driver_primary_ms <- (proc.time()[["elapsed"]] - started) * 1000
wait_linked()
driver_complete_ms <- (proc.time()[["elapsed"]] - started) * 1000
browser_timing <- app$get_js("window.__renLinkedTiming")
primary_logical_ms <- browser_timing$primaryAt - browser_timing$clickAt
complete_logical_ms <- browser_timing$completeAt - browser_timing$clickAt
visual <- app$get_js("window.cerebroLinkedViewsState.visualReady()")
if (!isTRUE(visual$ready)) {
  stop("Linked views did not reach visual readiness: ", visual$reason)
}
visual_ready_ms <- (proc.time()[["elapsed"]] - started) * 1000
summary <- app$get_js("window.cerebroLinkedViewsState.summary()")

idle_ok <- tryCatch(
  {
    app$wait_for_idle(timeout = 60000)
    TRUE
  },
  error = function(error) FALSE
)
interactive_ready_ms <- (proc.time()[["elapsed"]] - started) * 1000
auxiliary_idle_ms <- interactive_ready_ms - visual_ready_ms

back_started <- proc.time()[["elapsed"]]
click_tab("overview")
wait_projection()
linked_to_projection_ms <- (proc.time()[["elapsed"]] - back_started) * 1000
repeat_started <- proc.time()[["elapsed"]]
click_tab("coordinated_views")
wait_linked()
repeat_linked_ms <- (proc.time()[["elapsed"]] - repeat_started) * 1000
repeat_visual <- app$get_js("window.cerebroLinkedViewsState.visualReady()")
if (!isTRUE(repeat_visual$ready)) {
  stop("Repeated Linked views did not reach visual readiness")
}
repeat_linked_visual_ms <-
  (proc.time()[["elapsed"]] - repeat_started) * 1000
heap_after <- heap()

result <- list(
  scenario = scenario,
  primary_ms = primary_logical_ms,
  complete_ms = complete_logical_ms,
  primary_logical_ms = primary_logical_ms,
  complete_logical_ms = complete_logical_ms,
  driver_primary_ms = driver_primary_ms,
  driver_complete_ms = driver_complete_ms,
  visual_ready_ms = visual_ready_ms,
  interactive_ready_ms = interactive_ready_ms,
  auxiliary_idle_ms = auxiliary_idle_ms,
  auxiliary_idle_reached = idle_ok,
  visual = visual,
  linked_to_projection_ms = linked_to_projection_ms,
  repeat_linked_ms = repeat_linked_ms,
  repeat_linked_visual_ms = repeat_linked_visual_ms,
  heap_before = heap_before,
  heap_after = heap_after,
  transport = summary$transport
)
cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n")
