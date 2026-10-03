#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true")
script_argument <- grep("^--file=", commandArgs(), value = TRUE)[[1L]]
source(file.path(
  dirname(normalizePath(sub("^--file=", "", script_argument))),
  "page_readiness_protocol.R"
))
if (length(args) < 3L) {
  stop(
    paste(
      "usage: page_readiness.R [LABEL=]REPO_ROOT CRB",
      "OUTPUT_TSV [ROUNDS] [LABEL=REPO_ROOT ...]"
    ),
    call. = FALSE
  )
}

quote_r <- function(value) encodeString(value, quote = '"')

parse_candidate <- function(value, default_label = "candidate") {
  separator <- regexpr("=", value, fixed = TRUE)
  if (separator > 1L) {
    label <- substr(value, 1L, separator - 1L)
    path <- substring(value, separator + 1L)
  } else {
    label <- default_label
    path <- value
  }
  c(label = label, root = normalizePath(path, mustWork = TRUE))
}

first_candidate <- parse_candidate(args[[1L]])
crb <- normalizePath(args[[2L]], mustWork = TRUE)
output <- normalizePath(args[[3L]], mustWork = FALSE)
profile <- Sys.getenv("VIEWER_BENCH_PROFILE", unset = "quick")
rounds <- if (length(args) >= 4L && grepl("^[0-9]+$", args[[4L]])) {
  as.integer(args[[4L]])
} else {
  3L
}
candidate_start <- if (length(args) >= 4L && grepl("^[0-9]+$", args[[4L]])) {
  5L
} else {
  4L
}
extra_candidates <- if (length(args) >= candidate_start) {
  lapply(args[seq.int(candidate_start, length(args))], parse_candidate)
} else {
  list()
}
candidate_specs <- c(list(first_candidate), extra_candidates)
candidate_labels <- vapply(candidate_specs, `[[`, character(1), "label")
validate_page_profile(profile, rounds)
if (any(!nzchar(candidate_labels)) || anyDuplicated(candidate_labels)) {
  stop("Candidate labels must be non-empty and unique.", call. = FALSE)
}
names(candidate_specs) <- candidate_labels

page <- function(
  tab,
  ready,
  budget_ms = 2000,
  required = FALSE,
  wait_idle = TRUE,
  event_view = NULL,
  ready_event = if (is.null(event_view)) NULL else "cerebro:specialist-state",
  correctness = "true",
  point_selector = NULL,
  expected_point_count = NA_real_,
  correctness_detail = "''",
  visual_check = FALSE,
  requires_webgpu = FALSE
) {
  list(
    tab = tab,
    ready = ready,
    budget_ms = budget_ms,
    required = required,
    wait_idle = wait_idle,
    event_view = event_view,
    ready_event = ready_event,
    correctness = correctness,
    point_selector = point_selector,
    expected_point_count = expected_point_count,
    correctness_detail = correctness_detail,
    visual_check = visual_check,
    requires_webgpu = requires_webgpu
  )
}

canvas_page <- function(tab, host, expected_points = NULL, ...) {
  selector <- paste0(host, " canvas:not(.cv-mini)[data-point-count]")
  count_check <- if (is.null(expected_points)) {
    "pointCount>0"
  } else {
    paste0("pointCount===", expected_points)
  }
  page(
    tab,
    sprintf("!!document.querySelector(%s)", quote_r(selector)),
    ...,
    wait_idle = FALSE,
    correctness = sprintf(
      paste0(
        "(() => {const canvas=document.querySelector(%s);",
        "const pointCount=Number(canvas?.getAttribute('data-point-count'));",
        "const detail=window.__cerebroPageBenchEventDetail;",
        "return Number.isFinite(pointCount)&&%s&&",
        "/^md5-cell-set-v1:[0-9a-f]{32}$/.test(",
        "detail?.datasetFingerprint);})()"
      ),
      quote_r(selector),
      count_check
    ),
    point_selector = selector,
    expected_point_count = if (is.null(expected_points)) {
      NA_real_
    } else {
      expected_points
    },
    correctness_detail = "JSON.stringify(window.__cerebroPageBenchEventDetail||{})",
    visual_check = TRUE,
    requires_webgpu = !is.null(expected_points)
  )
}

pages <- list(
  groups = page(
    "groups",
    "!!p.querySelector('#groups_nUMI_plot.js-plotly-plot')",
    required = TRUE,
    correctness = paste0(
      "(() => {const plot=document.querySelector(",
      "'#shiny-tab-groups #groups_nUMI_plot.js-plotly-plot');",
      "return !!plot&&plot.offsetParent!==null&&",
      "Array.isArray(plot.data)&&plot.data.length>0;})()"
    ),
    correctness_detail = paste0(
      "(() => {const plot=document.querySelector(",
      "'#shiny-tab-groups #groups_nUMI_plot.js-plotly-plot');",
      "return 'plotly_traces='+(Array.isArray(plot?.data)?plot.data.length:0);})()"
    )
  ),
  overview = canvas_page(
    "overview",
    "#overview_projection_cell_view_host",
    expected_points = 1000000,
    required = TRUE,
    event_view = "overview_projection"
  ),
  gene_expression = canvas_page(
    "geneExpression",
    "#expression_projection_cell_view_host",
    expected_points = 1000000,
    required = TRUE,
    event_view = "expression_projection"
  ),
  immune_repertoire = canvas_page(
    "immune_repertoire",
    "#ir_clonalUMAP_projection_cell_view_host",
    expected_points = 1000000,
    required = TRUE,
    event_view = "ir_clonalUMAP_projection"
  ),
  trajectory = canvas_page(
    "trajectory",
    "#trajectory_projection_cell_view_host",
    expected_points = 1000000,
    budget_ms = 3000,
    required = TRUE,
    event_view = "trajectory_projection"
  ),
  hla = canvas_page(
    "hla_tcr_motifs",
    "#hla_motif_network_cell_view_host",
    budget_ms = 3000,
    required = TRUE,
    event_view = "hla_motif_network"
  ),
  marker_genes = page("markerGenes", "true"),
  most_expressed_genes = page("mostExpressedGenes", "true"),
  enriched_pathways = page("enrichedPathways", "true"),
  extra_material = page("extra_material", "true"),
  spatial = canvas_page(
    "spatial",
    "#spatial_projection_cell_view_host",
    event_view = "spatial_projection"
  ),
  trekker = page("trekker", "true"),
  gene_id_conversion = page("geneIdConversion", "true"),
  color_management = page("color_management", "true"),
  analysis_info = page("analysis_info", "true"),
  about = page("about", "true"),
  coordinated_views = page(
    "coordinated_views",
    "!!window.cerebroLinkedViewsState&&window.cerebroLinkedViewsState.ready()",
    required = TRUE,
    wait_idle = FALSE,
    visual_check = TRUE,
    requires_webgpu = TRUE,
    ready_event = "cerebro:linkedviews-ready",
    correctness = paste0(
      "(() => {const state=window.cerebroLinkedViewsState;",
      "const summary=state?.summary?.();",
      "return summary?.ready===true&&",
      "typeof summary.datasetFingerprint==='string'&&",
      "summary.datasetFingerprint.length>0;})()"
    ),
    correctness_detail = paste0(
      "(() => {const summary=window.cerebroLinkedViewsState?.summary?.()||{};",
      "return JSON.stringify({ready:summary.ready,",
      "datasetFingerprint:summary.datasetFingerprint,",
      "projections:summary.projections?.length||0,",
      "spatialSections:summary.spatialSections?.length||0});})()"
    )
  )
)

only <- Sys.getenv("VIEWER_PAGES_ONLY")
if (nzchar(only)) {
  only <- trimws(strsplit(only, ",", fixed = TRUE)[[1L]])
  unknown <- setdiff(only, names(pages))
  if (length(unknown)) {
    stop("Unknown VIEWER_PAGES_ONLY page: ", unknown[[1L]], call. = FALSE)
  }
  pages <- pages[only]
}

page_active_js <- function(page, require_event = TRUE) {
  idle <- if (isTRUE(page$wait_idle)) {
    "!document.documentElement.classList.contains('shiny-busy')"
  } else {
    "true"
  }
  event_ready <- if (!isTRUE(require_event) || is.null(page$ready_event)) {
    "true"
  } else {
    "window.__cerebroPageBenchSeen === true"
  }
  sprintf(
    paste0(
      "(() => {const p=document.getElementById('shiny-tab-%s');",
      "return !!p&&p.classList.contains('active')&&%s&&%s&&(%s);})()"
    ),
    page$tab,
    idle,
    event_ready,
    page$ready
  )
}

arm_and_click_page <- function(app, page, selector, require_event = TRUE) {
  listener <- if (!isTRUE(require_event) || is.null(page$ready_event)) {
    ""
  } else {
    condition <- if (is.null(page$event_view)) {
      "e.detail?.ready===true"
    } else {
      sprintf("e.detail?.viewId===%s", quote_r(page$event_view))
    }
    sprintf(
      paste0(
        "window.addEventListener(%s,function h(e){",
        "if(window.__cerebroPageBenchGeneration===generation&&",
        "e.timeStamp >= clickStart&&%s){",
        "window.__cerebroPageBenchSeen=true;",
        "window.__cerebroPageBenchEventDetail=e.detail||null;",
        "window.removeEventListener(%s,h);}});"
      ),
      quote_r(page$ready_event),
      condition,
      quote_r(page$ready_event)
    )
  }
  app$run_js(
    sprintf(
      paste0(
        "(() => {const generation=",
        "(window.__cerebroPageBenchGeneration||0)+1;",
        "window.__cerebroPageBenchGeneration=generation;",
        "const clickStart=performance.now();",
        "window.__cerebroPageBenchClickStart=clickStart;",
        "window.__cerebroPageBenchSeen=false;",
        "window.__cerebroPageBenchEventDetail=null;",
        "%sdocument.querySelector(%s).click();})()"
      ),
      listener,
      quote_r(selector)
    )
  )
}

page_available <- function(app, page) {
  selector <- sprintf("a[href='#shiny-tab-%s']", page$tab)
  app$get_js(sprintf(
    "(() => {const link=document.querySelector(%s);return !!link&&link.offsetParent !== null;})()",
    quote_r(selector)
  ))
}

open_page <- function(app, page, require_event = TRUE) {
  selector <- sprintf("a[href='#shiny-tab-%s']", page$tab)
  if (!isTRUE(page_available(app, page))) {
    if (isTRUE(page$required)) {
      stop("Missing required benchmark page: ", page$tab, call. = FALSE)
    }
    return(NA_real_)
  }
  started <- proc.time()[["elapsed"]]
  arm_and_click_page(app, page, selector, require_event = require_event)
  app$wait_for_js(
    page_active_js(page, require_event = require_event),
    timeout = 120000
  )
  (proc.time()[["elapsed"]] - started) * 1000
}

page_correctness <- function(app, page) {
  pass <- isTRUE(app$get_js(page$correctness))
  point_count <- if (is.null(page$point_selector)) {
    NA_real_
  } else {
    value <- app$get_js(sprintf(
      paste0(
        "(() => {const value=document.querySelector(%s)?.",
        "getAttribute('data-point-count');",
        "return value===null||value===undefined?null:Number(value);})()"
      ),
      quote_r(page$point_selector)
    ))
    if (is.null(value)) NA_real_ else as.numeric(value)
  }
  detail <- app$get_js(page$correctness_detail)
  list(
    pass = pass,
    point_count = point_count,
    detail = if (is.null(detail)) "" else as.character(detail)
  )
}

page_renderer_diagnostics <- function(app, page) {
  value <- app$get_js(sprintf(
    paste0(
      "(() => {const root=document.getElementById('shiny-tab-%s');",
      "const canvases=Array.from(root?.querySelectorAll(",
      "'canvas:not(.cv-mini)')||[]);",
      "const gpu=canvases.find(canvas=>canvas.classList.contains(",
      "'cv-gpu-layer')&&canvas.style.display!=='none');",
      "const renderer=gpu?._cerebroPointRenderer||canvases.find(canvas=>",
      "canvas.classList.contains('cv-gpu-layer'))?._cerebroPointRenderer;",
      "const stats=renderer?.stats?.()||{};",
      "const pointCanvas=canvases.find(canvas=>canvas.dataset.pointCount);",
      "return {navigatorGpu:!!navigator.gpu,",
      "backend:gpu?(stats.backend||'webgpu'):",
      "(pointCanvas?'canvas2d':'not_applicable'),",
      "adapter:String(stats.adapter||''),contextLost:!!stats.contextLost,",
      "error:String(stats.error||'')};})()"
    ),
    page$tab
  ))
  list(
    navigator_gpu = isTRUE(value$navigatorGpu),
    renderer_backend = if (is.null(value$backend)) {
      "not_applicable"
    } else {
      as.character(value$backend)
    },
    renderer_adapter = if (is.null(value$adapter)) {
      ""
    } else {
      as.character(value$adapter)
    },
    renderer_context_lost = isTRUE(value$contextLost),
    renderer_error = if (is.null(value$error)) "" else as.character(value$error)
  )
}

page_visible_pixels <- function(app, page) {
  if (!isTRUE(page$visual_check)) {
    return(list(count = NA_real_, pass = NA))
  }
  screenshot <- tempfile("viewer-page-pixels-", fileext = ".png")
  on.exit(unlink(screenshot), add = TRUE)
  prepared <- app$get_js(sprintf(
    paste0(
      "(async () => {const root=document.getElementById('shiny-tab-%s');",
      "const canvases=Array.from(root?.querySelectorAll(",
      "'canvas:not(.cv-mini)')||[]).filter(canvas=>",
      "canvas.offsetParent!==null&&canvas.width>0&&canvas.height>0);",
      "const gpu=canvases.find(canvas=>canvas.classList.contains(",
      "'cv-gpu-layer')&&canvas.style.display!=='none');",
      "const target=gpu||canvases.find(canvas=>",
      "!canvas.classList.contains('cv-gpu-layer')&&",
      "canvas.dataset.pointCount);if(!target)return false;",
      "if(gpu?._cerebroPointRenderer)await gpu._cerebroPointRenderer.idle();",
      "canvases.forEach(canvas=>{canvas.__cerebroBenchVisibility=",
      "canvas.style.visibility;if(canvas!==target)canvas.style.visibility=",
      "'hidden';});target.dataset.cerebroBenchmarkVisual='true';",
      "return true;})()"
    ),
    page$tab
  ))
  if (!isTRUE(prepared)) {
    return(list(count = 0, pass = FALSE))
  }
  restore <- paste0(
    "(() => {document.querySelectorAll('canvas[data-cerebro-benchmark-visual]')",
    ".forEach(target=>{const root=target.closest('[id^=\"shiny-tab-\"]');",
    "Array.from(root?.querySelectorAll('canvas:not(.cv-mini)')||[])",
    ".forEach(canvas=>{canvas.style.visibility=",
    "canvas.__cerebroBenchVisibility||'';delete canvas.__cerebroBenchVisibility;});",
    "delete target.dataset.cerebroBenchmarkVisual;});})()"
  )
  tryCatch(
    app$get_screenshot(
      screenshot,
      selector = "canvas[data-cerebro-benchmark-visual='true']"
    ),
    finally = app$run_js(restore)
  )
  image <- png::readPNG(screenshot)
  rgb <- image[,, seq_len(min(3L, dim(image)[[3L]])), drop = FALSE]
  height <- dim(rgb)[[1L]]
  width <- dim(rgb)[[2L]]
  border <- rbind(
    matrix(rgb[1L, , ], ncol = dim(rgb)[[3L]]),
    matrix(rgb[height, , ], ncol = dim(rgb)[[3L]]),
    matrix(rgb[, 1L, ], ncol = dim(rgb)[[3L]]),
    matrix(rgb[, width, ], ncol = dim(rgb)[[3L]])
  )
  background <- apply(border, 2L, stats::median)
  rows <- seq.int(
    max(1L, floor(height * 0.1)),
    min(height, ceiling(height * 0.9))
  )
  columns <- seq.int(
    max(1L, floor(width * 0.1)),
    min(width, ceiling(width * 0.9))
  )
  interior <- rgb[rows, columns, , drop = FALSE]
  difference <- sweep(interior, 3L, background, "-")
  count <- sum(apply(abs(difference), c(1L, 2L), max) > 2 / 255)
  list(count = as.numeric(count), pass = count > 0)
}

start_socket_meter <- function(app) {
  app$run_js(paste0(
    "(() => {if(window.__cerebroPageBenchSocketMeter){",
    "throw new Error('Socket meter is already active');}",
    "const socket=window.Shiny&&Shiny.shinyapp&&Shiny.shinyapp.$socket;",
    "if(!socket||typeof socket.addEventListener!=='function'||",
    "typeof socket.send!=='function'){",
    "throw new Error('Shiny socket is unavailable');}",
    "const encoder=new TextEncoder();",
    "const byteLength=data=>{",
    "if(typeof data==='string')return encoder.encode(data).byteLength;",
    "if(data instanceof ArrayBuffer)return data.byteLength;",
    "if(ArrayBuffer.isView(data))return data.byteLength;",
    "if(data instanceof Blob)return data.size;return 0;};",
    "const meter={socket:socket,sent:0,received:0,",
    "originalSend:socket.send,handler:null,wrappedSend:null};",
    "meter.handler=event=>{meter.received+=byteLength(event.data);};",
    "meter.wrappedSend=function(data){meter.sent+=byteLength(data);",
    "return meter.originalSend.apply(this,arguments);};",
    "try{socket.addEventListener('message',meter.handler);",
    "socket.send=meter.wrappedSend;",
    "if(socket.send!==meter.wrappedSend){",
    "throw new Error('Shiny socket send cannot be wrapped');}",
    "window.__cerebroPageBenchSocketMeter=meter;}",
    "catch(error){socket.removeEventListener('message',meter.handler);",
    "if(socket.send===meter.wrappedSend){socket.send=meter.originalSend;}",
    "throw error;}})()"
  ))
  invisible(TRUE)
}

cleanup_socket_meter <- function(app) {
  app$run_js(paste0(
    "(() => {const meter=window.__cerebroPageBenchSocketMeter;",
    "if(!meter)return;",
    "meter.socket.removeEventListener('message',meter.handler);",
    "if(meter.socket.send===meter.wrappedSend){",
    "meter.socket.send=meter.originalSend;}",
    "window.__cerebroPageBenchSocketMeter=null;})()"
  ))
  invisible(TRUE)
}

stop_socket_meter <- function(app) {
  value <- app$get_js(paste0(
    "(() => {const meter=window.__cerebroPageBenchSocketMeter;",
    "if(!meter)throw new Error('Socket meter is not active');",
    "try{return {sent:meter.sent,received:meter.received};}",
    "finally{meter.socket.removeEventListener('message',meter.handler);",
    "if(meter.socket.send===meter.wrappedSend){",
    "meter.socket.send=meter.originalSend;}",
    "window.__cerebroPageBenchSocketMeter=null;}})()"
  ))
  if (
    is.null(value$sent) ||
      !is.finite(value$sent) ||
      is.null(value$received) ||
      !is.finite(value$received)
  ) {
    stop("Socket meter returned invalid byte counts.", call. = FALSE)
  }
  value
}

start_rss_monitor <- function(r_pid, chrome_pid) {
  result <- tempfile("viewer-page-rss-", fileext = ".rds")
  stop_file <- paste0(result, ".stop")
  ready_file <- paste0(result, ".ready")
  process <- callr::r_bg(
    function(r_pid, chrome_pid, result, stop_file, ready_file) {
      tree_rss <- function(root, processes) {
        members <- root
        repeat {
          children <- processes$pid[processes$ppid %in% members]
          children <- setdiff(children, members)
          if (!length(children)) {
            break
          }
          members <- c(members, children)
        }
        sum(processes$rss[processes$pid %in% members], na.rm = TRUE)
      }
      peaks <- c(r_peak_rss_kib = 0, chrome_peak_rss_kib = 0)
      file.create(ready_file)
      repeat {
        lines <- system2(
          "ps",
          c("-axo", "pid=,ppid=,rss="),
          stdout = TRUE,
          stderr = FALSE
        )
        processes <- tryCatch(
          utils::read.table(
            text = lines,
            col.names = c("pid", "ppid", "rss")
          ),
          error = function(error) NULL
        )
        if (!is.null(processes)) {
          peaks[[1L]] <- max(peaks[[1L]], tree_rss(r_pid, processes))
          peaks[[2L]] <- max(peaks[[2L]], tree_rss(chrome_pid, processes))
        }
        if (file.exists(stop_file)) {
          break
        }
        Sys.sleep(0.05)
      }
      saveRDS(peaks, result)
    },
    args = list(r_pid, chrome_pid, result, stop_file, ready_file),
    stdout = "|",
    stderr = "|"
  )
  deadline <- Sys.time() + 5
  while (
    !file.exists(ready_file) && process$is_alive() && Sys.time() < deadline
  ) {
    Sys.sleep(0.01)
  }
  if (!file.exists(ready_file)) {
    process$kill()
    stop("RSS monitor did not start.", call. = FALSE)
  }
  list(process = process, result = result, stop = stop_file, ready = ready_file)
}

stop_rss_monitor <- function(monitor) {
  file.create(monitor$stop)
  monitor$process$wait(5000)
  value <- if (file.exists(monitor$result)) {
    readRDS(monitor$result)
  } else {
    c(r_peak_rss_kib = NA_real_, chrome_peak_rss_kib = NA_real_)
  }
  unlink(c(monitor$result, monitor$stop, monitor$ready))
  value
}

js_heap_used <- function(session) {
  metrics <- session$Performance$getMetrics()$metrics
  names <- vapply(metrics, `[[`, character(1), "name")
  values <- vapply(metrics, `[[`, numeric(1), "value")
  unname(values[match("JSHeapUsedSize", names)])
}

assert_clean_logs <- function(app) {
  logs <- app$get_logs()
  browser_error <- logs$location == "chromote" &
    logs$level %in% c("error", "assert", "throw")
  server_error <- logs$location == "shiny" &
    grepl(
      "Warning: Error|Execution halted|Error in ",
      logs$message
    )
  if (any(browser_error | server_error, na.rm = TRUE)) {
    stop("Browser or Shiny server error occurred.", call. = FALSE)
  }
}

run_observation <- function(schedule_row, candidate, page, crb) {
  app_dir <- tempfile("viewer-1m-page-")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(
      sprintf(
        "devtools::load_all(%s, quiet = TRUE)",
        quote_r(candidate[["root"]])
      ),
      "launchCerebro(",
      "  mode = \"closed\",",
      sprintf("  crb_file_to_load = c(\"1M pages\" = %s),", quote_r(crb)),
      "  percentage_cells_to_show = 100,",
      "  projections_show_hover_info = TRUE",
      ")"
    ),
    file.path(app_dir, "app.R")
  )
  suppressWarnings(shinytest2::local_app_support(app_dir))
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = paste0("viewer_1m_page_", schedule_row$schedule_position),
    height = 950,
    width = 1619,
    load_timeout = 900000,
    timeout = 900000,
    check_names = FALSE
  )
  session <- app$get_chromote_session()
  browser <- session$parent
  on.exit(
    {
      try(app$stop(), silent = TRUE)
      try(browser$close(), silent = TRUE)
    },
    add = TRUE
  )
  app$wait_for_value(output = "load_data_number_of_cells", timeout = 900000)
  count <- app$get_value(output = "load_data_number_of_cells")
  if (is.null(count$html) || !grepl("1,000,000", count$html, fixed = TRUE)) {
    stop("Data Info did not report 1,000,000 cells.", call. = FALSE)
  }
  if (!isTRUE(page_available(app, page))) {
    if (isTRUE(page$required)) {
      stop("Missing required benchmark page: ", page$tab, call. = FALSE)
    }
    return(empty_observation("skipped", "page unavailable"))
  }
  warmed <- FALSE
  if (identical(schedule_row$visit, "repeat")) {
    open_page(app, page, require_event = TRUE)
    warmed <- TRUE
    app$run_js(
      "document.querySelector(\"a[href='#shiny-tab-loadData']\").click();"
    )
    app$wait_for_js(
      "document.getElementById('shiny-tab-loadData').classList.contains('active')",
      timeout = 120000
    )
  }

  session$Performance$enable()
  start_socket_meter(app)
  socket_meter_active <- TRUE
  on.exit(
    {
      if (socket_meter_active) try(cleanup_socket_meter(app), silent = TRUE)
    },
    add = TRUE
  )
  r_pid <- app$.__enclos_env__$private$shiny_process$get_pid()
  chrome_pid <- browser$get_browser()$.__enclos_env__$private$process$get_pid()
  monitor <- start_rss_monitor(r_pid, chrome_pid)
  monitor_stopped <- FALSE
  on.exit(
    {
      if (!monitor_stopped) try(stop_rss_monitor(monitor), silent = TRUE)
    },
    add = TRUE
  )
  require_event <- requires_ready_event(
    schedule_row$page,
    schedule_row$visit,
    warmed
  )
  elapsed_ms <- open_page(app, page, require_event = require_event)
  heap_used_bytes <- js_heap_used(session)
  websocket <- stop_socket_meter(app)
  socket_meter_active <- FALSE
  resources <- stop_rss_monitor(monitor)
  monitor_stopped <- TRUE
  correctness <- page_correctness(app, page)
  renderer <- page_renderer_diagnostics(app, page)
  visible_pixels <- page_visible_pixels(app, page)
  correctness_pass <- correctness$pass &&
    (is.na(visible_pixels$pass) || visible_pixels$pass)
  assert_clean_logs(app)
  data.frame(
    status = if (correctness_pass) "ok" else "error",
    error = if (correctness_pass) "" else "Page correctness check failed.",
    elapsed_ms = elapsed_ms,
    ready_event_required = require_event && !is.null(page$ready_event),
    correctness_pass = correctness_pass,
    rendered_point_count = correctness$point_count,
    expected_point_count = page$expected_point_count,
    correctness_detail = correctness$detail,
    navigator_gpu = renderer$navigator_gpu,
    renderer_backend = renderer$renderer_backend,
    renderer_adapter = renderer$renderer_adapter,
    renderer_context_lost = renderer$renderer_context_lost,
    renderer_error = renderer$renderer_error,
    visible_pixel_count = visible_pixels$count,
    visible_pixels_pass = visible_pixels$pass,
    r_peak_rss_kib = unname(resources[["r_peak_rss_kib"]]),
    chrome_peak_rss_kib = unname(resources[["chrome_peak_rss_kib"]]),
    js_heap_used_bytes = heap_used_bytes,
    websocket_sent_payload_bytes = as.numeric(websocket$sent),
    websocket_received_payload_bytes = as.numeric(websocket$received),
    chrome_version = session$Browser$getVersion()$product,
    stringsAsFactors = FALSE
  )
}

git_value <- function(root, ...) {
  value <- system2("git", c("-C", root, ...), stdout = TRUE, stderr = TRUE)
  if (!length(value)) NA_character_ else value[[1L]]
}

git_dirty <- function(root) {
  length(system2(
    "git",
    c("-C", root, "status", "--porcelain"),
    stdout = TRUE,
    stderr = TRUE
  )) >
    0L
}

sha256_file <- function(path) {
  if (exists("sha256sum", envir = asNamespace("tools"), inherits = FALSE)) {
    return(unname(tools::sha256sum(path)))
  }
  command <- Sys.which("shasum")
  if (!nzchar(command)) {
    stop("No SHA-256 implementation is available.")
  }
  sub(" .*", "", system2(command, c("-a", "256", path), stdout = TRUE)[[1L]])
}

package_version_at <- function(root) {
  description <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")
  unname(description[[1L]])
}

provenance <- do.call(
  rbind,
  lapply(candidate_specs, function(candidate) {
    root <- candidate[["root"]]
    data.frame(
      candidate = candidate[["label"]],
      candidate_root = root,
      candidate_git_sha = git_value(root, "rev-parse", "HEAD"),
      candidate_git_dirty = git_dirty(root),
      artifact = crb,
      artifact_sha256 = sha256_file(crb),
      host = unname(Sys.info()[["nodename"]]),
      os = paste(
        Sys.info()[c("sysname", "release", "machine")],
        collapse = " "
      ),
      r_version = R.version.string,
      package_version = package_version_at(root),
      shinytest2_version = as.character(utils::packageVersion("shinytest2")),
      chromote_version = as.character(utils::packageVersion("chromote")),
      profile = profile,
      rounds = rounds,
      stringsAsFactors = FALSE
    )
  })
)

schedule <- build_balanced_schedule(candidate_labels, names(pages), rounds)
schedule_output <- sub("[.]tsv$", "_schedule.tsv", output)
if (identical(schedule_output, output)) {
  schedule_output <- paste0(output, ".schedule.tsv")
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.table(
  schedule,
  schedule_output,
  row.names = FALSE,
  sep = "\t",
  quote = FALSE
)

empty_observation <- function(status, error) {
  data.frame(
    status = status,
    error = error,
    elapsed_ms = NA_real_,
    ready_event_required = NA,
    correctness_pass = NA,
    rendered_point_count = NA_real_,
    expected_point_count = NA_real_,
    correctness_detail = "",
    navigator_gpu = NA,
    renderer_backend = NA_character_,
    renderer_adapter = NA_character_,
    renderer_context_lost = NA,
    renderer_error = NA_character_,
    visible_pixel_count = NA_real_,
    visible_pixels_pass = NA,
    r_peak_rss_kib = NA_real_,
    chrome_peak_rss_kib = NA_real_,
    js_heap_used_bytes = NA_real_,
    websocket_sent_payload_bytes = NA_real_,
    websocket_received_payload_bytes = NA_real_,
    chrome_version = NA_character_,
    stringsAsFactors = FALSE
  )
}

rows <- lapply(seq_len(nrow(schedule)), function(index) {
  scheduled <- schedule[index, , drop = FALSE]
  candidate <- candidate_specs[[scheduled$candidate]]
  observation <- tryCatch(
    run_observation(scheduled, candidate, pages[[scheduled$page]], crb),
    error = function(error) empty_observation("error", conditionMessage(error))
  )
  page_spec <- pages[[scheduled$page]]
  budget_ms <- if (identical(scheduled$visit, "first")) {
    page_spec$budget_ms
  } else {
    500
  }
  row <- cbind(
    scheduled,
    required = page_spec$required,
    requires_webgpu = page_spec$requires_webgpu,
    budget_ms = budget_ms,
    observation,
    stringsAsFactors = FALSE
  )
  row$performance_applicable <- !row$requires_webgpu ||
    identical(row$renderer_backend, "webgpu")
  row$pass <- if (isTRUE(row$performance_applicable)) {
    row$status == "ok" && page_budget_pass(row$elapsed_ms, row$budget_ms)
  } else {
    NA
  }
  message(
    row$candidate,
    " round ",
    row$round,
    " ",
    row$page,
    " ",
    row$visit,
    ": ",
    row$status,
    if (is.finite(row$elapsed_ms)) {
      paste0(" ", round(row$elapsed_ms), " ms")
    } else {
      ""
    }
  )
  row
})

results <- do.call(rbind, rows)
results <- merge(results, provenance, by = "candidate", sort = FALSE)
results <- results[order(results$schedule_position), ]
write.table(results, output, row.names = FALSE, sep = "\t", quote = FALSE)

chrome_versions <- tapply(
  results$chrome_version,
  results$candidate,
  function(value) {
    value <- value[!is.na(value) & nzchar(value)]
    if (length(value)) value[[1L]] else NA_character_
  }
)
provenance$chrome_version <- unname(chrome_versions[provenance$candidate])
manifest_output <- sub("[.]tsv$", "_manifest.tsv", output)
if (identical(manifest_output, output)) {
  manifest_output <- paste0(output, ".manifest.tsv")
}
write.table(
  provenance,
  manifest_output,
  row.names = FALSE,
  sep = "\t",
  quote = FALSE
)
print(results, row.names = FALSE)

bad_status <- results$status == "error" |
  (results$required & results$status != "ok")
missing_resources <- results$status == "ok" &
  (!is.finite(results$r_peak_rss_kib) |
    !is.finite(results$chrome_peak_rss_kib) |
    !is.finite(results$js_heap_used_bytes) |
    !is.finite(results$websocket_sent_payload_bytes) |
    !is.finite(results$websocket_received_payload_bytes))
if (any(bad_status | missing_resources)) {
  stop(
    "Page observations contain errors or missing required metrics; refusing evidence output.",
    call. = FALSE
  )
}
if (identical(profile, "evidence") && any(provenance$candidate_git_dirty)) {
  stop("Publication requires clean candidate worktrees.", call. = FALSE)
}
if (
  identical(profile, "evidence") &&
    any(
      results$required &
        results$requires_webgpu &
        !results$performance_applicable
    )
) {
  stop(
    "Publication requires WebGPU for million-cell page timings.",
    call. = FALSE
  )
}
if (
  any(results$status == "ok" & results$performance_applicable & !results$pass)
) {
  stop(
    "One or more available 1M page budgets failed; see ",
    output,
    call. = FALSE
  )
}
