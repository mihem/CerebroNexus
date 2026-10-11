#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true")
# chromote disables GPU acceleration by default for test stability. This
# benchmark explicitly measures the production million-cell GPU path, so that
# default would make its WebGPU requirement impossible to satisfy.
chromote::set_chrome_args(setdiff(
  chromote::get_chrome_args(),
  "--disable-gpu"
))
script_argument <- grep("^--file=", commandArgs(), value = TRUE)[[1L]]
source(file.path(
  dirname(normalizePath(sub("^--file=", "", script_argument))),
  "viewer_1m_page_protocol.R"
))
if (length(args) < 3L) {
  stop(
    paste(
      "usage: benchmark_viewer_1m_pages.R [LABEL=]REPO_ROOT CRB",
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
benchmark_mode <- tolower(Sys.getenv("VIEWER_BENCH_MODE", unset = "timing"))
validate_benchmark_mode(benchmark_mode)
memory_mode <- identical(benchmark_mode, "memory")
expected_renderer <- tolower(Sys.getenv(
  "VIEWER_EXPECTED_RENDERER",
  unset = "webgpu"
))
if (!expected_renderer %in% c("webgpu", "webgl2")) {
  stop("VIEWER_EXPECTED_RENDERER must be webgpu or webgl2.", call. = FALSE)
}
expected_cells <- suppressWarnings(as.integer(Sys.getenv(
  "VIEWER_EXPECTED_CELLS",
  unset = "1000000"
)))
if (
  length(expected_cells) != 1L || is.na(expected_cells) || expected_cells < 1L
) {
  stop("VIEWER_EXPECTED_CELLS must be one positive integer.", call. = FALSE)
}
trajectory_method <- Sys.getenv(
  "VIEWER_TRAJECTORY_METHOD",
  unset = "marker_guided"
)
trajectory_name <- Sys.getenv(
  "VIEWER_TRAJECTORY_NAME",
  unset = "E18_neurogenesis"
)
requested_pages <- trimws(Sys.getenv("VIEWER_PAGES_ONLY", unset = ""))
trajectory_requested <- !nzchar(requested_pages) ||
  "trajectory" %in%
    trimws(strsplit(requested_pages, ",", fixed = TRUE)[[1L]])
trajectory_contract <- if (trajectory_requested) {
  benchmark_trajectory_contract(crb, trajectory_method, trajectory_name)
} else {
  data.frame(
    method = trajectory_method,
    name = trajectory_name,
    renderable_rows = expected_cells,
    stringsAsFactors = FALSE
  )
}
spatial_requested <- !nzchar(requested_pages) ||
  "spatial" %in%
    trimws(strsplit(requested_pages, ",", fixed = TRUE)[[1L]])
spatial_contract <- if (spatial_requested) {
  benchmark_spatial_contract(crb)
} else {
  NULL
}
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

viewer_pack_manifest <- file.path(
  dirname(crb),
  paste0(tools::file_path_sans_ext(basename(crb)), ".viewer"),
  "manifest.json"
)
groups_metric_available <- TRUE
if (file.exists(viewer_pack_manifest)) {
  viewer_pack <- jsonlite::read_json(
    viewer_pack_manifest,
    simplifyVector = TRUE
  )
  groups_metric_available <- "nUMI" %in% viewer_pack$metadata_names
}
groups_plot_id <- if (groups_metric_available) {
  "groups_nUMI_plot"
} else {
  "groups_by_other_group_plot"
}
groups_plot_selector <- paste0(
  "#shiny-tab-groups #",
  groups_plot_id,
  ".js-plotly-plot"
)

page <- function(
  tab,
  ready,
  budget_ms = 2000,
  required = FALSE,
  event_view = NULL,
  ready_event = if (is.null(event_view)) NULL else "cerebro:specialist-state",
  ready_event_condition = NULL,
  completion_ready = NULL,
  completion_event_condition = NULL,
  correctness = "true",
  point_selector = NULL,
  expected_point_count = NA_real_,
  correctness_detail = "''",
  visual_check = FALSE,
  requires_webgpu = FALSE,
  expected_trajectory_method = NA_character_,
  expected_trajectory_name = NA_character_
) {
  list(
    tab = tab,
    ready = ready,
    budget_ms = budget_ms,
    required = required,
    event_view = event_view,
    ready_event = ready_event,
    ready_event_condition = ready_event_condition,
    completion_ready = completion_ready,
    completion_event_condition = completion_event_condition,
    correctness = correctness,
    point_selector = point_selector,
    expected_point_count = expected_point_count,
    correctness_detail = correctness_detail,
    visual_check = visual_check,
    requires_webgpu = requires_webgpu,
    expected_trajectory_method = expected_trajectory_method,
    expected_trajectory_name = expected_trajectory_name
  )
}

canvas_page <- function(
  tab,
  host,
  event_view,
  expected_points = NULL,
  expected_trajectory_method = NULL,
  expected_trajectory_name = NULL,
  ...
) {
  selector <- paste0(host, " canvas:not(.cv-mini)[data-point-count]")
  count_check <- if (is.null(expected_points)) {
    "pointCount>0"
  } else {
    paste0("pointCount===", expected_points)
  }
  trajectory_check <- if (
    is.null(expected_trajectory_method) || is.null(expected_trajectory_name)
  ) {
    "true"
  } else {
    sprintf(
      paste0(
        "document.getElementById('trajectory_selected_method')?.value===%s&&",
        "document.getElementById('trajectory_selected_name')?.value===%s"
      ),
      quote_r(expected_trajectory_method),
      quote_r(expected_trajectory_name)
    )
  }
  event_count_check <- if (is.null(expected_points)) {
    "Number(e.detail?.renderedPointCount)>0"
  } else {
    paste0("Number(e.detail?.renderedPointCount)===", expected_points)
  }
  page(
    tab,
    sprintf("!!document.querySelector(%s)", quote_r(selector)),
    ...,
    event_view = event_view,
    ready_event_condition = sprintf(
      paste0(
        "e.detail?.viewId===%s&&",
        "e.detail?.benchmarkGeneration===generation&&",
        "e.detail?.datasetFingerprint===expectedFingerprint&&",
        "%s&&%s"
      ),
      quote_r(event_view),
      event_count_check,
      trajectory_check
    ),
    correctness = sprintf(
      paste0(
        "(() => {const canvas=document.querySelector(%s);",
        "const pointCount=Number(canvas?.getAttribute('data-point-count'));",
        "const detail=window.__cerebroPageBenchEventDetail;",
        "return Number.isFinite(pointCount)&&%s&&%s&&",
        "detail?.benchmarkGeneration===window.__cerebroPageBenchGeneration&&",
        "detail?.datasetFingerprint===",
        "window.__cerebroPageBenchExpectedFingerprint&&",
        "Number(detail?.renderedPointCount)===pointCount;})()"
      ),
      quote_r(selector),
      count_check,
      trajectory_check
    ),
    point_selector = selector,
    expected_point_count = if (is.null(expected_points)) {
      NA_real_
    } else {
      expected_points
    },
    correctness_detail = "JSON.stringify(window.__cerebroPageBenchEventDetail||{})",
    visual_check = TRUE,
    requires_webgpu = !is.null(expected_points),
    expected_trajectory_method = if (is.null(expected_trajectory_method)) {
      NA_character_
    } else {
      expected_trajectory_method
    },
    expected_trajectory_name = if (is.null(expected_trajectory_name)) {
      NA_character_
    } else {
      expected_trajectory_name
    }
  )
}

pages <- list(
  groups = page(
    "groups",
    sprintf("!!p.querySelector(%s)", quote_r(groups_plot_selector)),
    required = TRUE,
    ready_event = "cerebro:groups-primary-ready",
    ready_event_condition = sprintf(
      paste0(
        "e.detail?.page==='groups'&&e.detail?.plotId===%s&&",
        "e.detail?.metric===%s&&",
        "e.detail?.benchmarkGeneration===generation&&",
        "e.detail?.datasetFingerprint===expectedFingerprint&&",
        "Number(e.detail?.traceCount)>0&&",
        "typeof e.detail?.selectedGroup==='string'&&",
        "e.detail.selectedGroup.length>0"
      ),
      quote_r(groups_plot_id),
      quote_r(
        if (identical(groups_plot_id, "groups_nUMI_plot")) {
          "nUMI"
        } else {
          "composition"
        }
      )
    ),
    correctness = sprintf(
      paste0(
        "(() => {const plot=document.querySelector(%s);",
        "const detail=window.__cerebroPageBenchEventDetail||{};",
        "return !!plot&&plot.offsetParent!==null&&",
        "Array.isArray(plot.data)&&plot.data.length>0&&",
        "detail.plotId===plot.id&&detail.traceCount===plot.data.length&&",
        "detail.datasetFingerprint===",
        "window.__cerebroPageBenchExpectedFingerprint&&",
        "detail.benchmarkGeneration===",
        "window.__cerebroPageBenchGeneration&&",
        "detail.selectedGroup===",
        "document.getElementById('groups_selected_group')?.value;})()"
      ),
      quote_r(groups_plot_selector)
    ),
    correctness_detail = sprintf(
      paste0(
        "(() => {const plot=document.querySelector(%s);",
        "const detail=window.__cerebroPageBenchEventDetail||{};",
        "return JSON.stringify({plot:plot?.id||'missing',",
        "plotlyTraces:Array.isArray(plot?.data)?plot.data.length:0,",
        "datasetFingerprint:detail.datasetFingerprint||'',",
        "selectedGroup:detail.selectedGroup||'',",
        "metric:detail.metric||'',generation:detail.benchmarkGeneration});})()"
      ),
      quote_r(groups_plot_selector)
    )
  ),
  overview = canvas_page(
    "overview",
    "#overview_projection_cell_view_host",
    expected_points = expected_cells,
    required = TRUE,
    event_view = "overview_projection"
  ),
  gene_expression = canvas_page(
    "geneExpression",
    "#expression_projection_cell_view_host",
    expected_points = expected_cells,
    required = TRUE,
    event_view = "expression_projection"
  ),
  immune_repertoire = canvas_page(
    "immune_repertoire",
    "#ir_clonalUMAP_projection_cell_view_host",
    required = TRUE,
    event_view = "ir_clonalUMAP_projection"
  ),
  trajectory = canvas_page(
    "trajectory",
    "#trajectory_projection_cell_view_host",
    expected_points = trajectory_contract$renderable_rows[[1L]],
    budget_ms = 3000,
    required = TRUE,
    event_view = "trajectory_projection",
    expected_trajectory_method = trajectory_contract$method[[1L]],
    expected_trajectory_name = trajectory_contract$name[[1L]]
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
    expected_points = if (is.null(spatial_contract)) {
      NULL
    } else {
      spatial_contract$renderable_rows[[1L]]
    },
    event_view = "spatial_projection"
  ),
  trekker = page("trekker", "true"),
  gene_id_conversion = page("geneIdConversion", "true"),
  color_management = page("color_management", "true"),
  about = page("about", "true"),
  coordinated_views = page(
    "coordinated_views",
    paste0(
      "!!window.cerebroLinkedViewsState&&",
      "window.cerebroLinkedViewsState.primaryReady()"
    ),
    required = TRUE,
    visual_check = TRUE,
    requires_webgpu = TRUE,
    ready_event = "cerebro:linkedviews-ready",
    ready_event_condition = paste0(
      "e.detail?.page==='coordinated_views'&&",
      "e.detail?.primaryReady===true&&",
      "e.detail?.benchmarkGeneration===generation&&",
      "e.detail?.datasetFingerprint===expectedFingerprint&&",
      "Number(e.detail?.renderedPointCount)===",
      expected_cells
    ),
    completion_ready = paste0(
      "!!window.cerebroLinkedViewsState&&",
      "window.cerebroLinkedViewsState.ready()"
    ),
    completion_event_condition = "e.detail?.ready===true",
    correctness = paste0(
      "(() => {const state=window.cerebroLinkedViewsState;",
      "const summary=state?.summary?.();",
      "const detail=window.__cerebroPageBenchEventDetail||{};",
      "return summary?.primaryReady===true&&",
      "summary.datasetFingerprint===",
      "window.__cerebroPageBenchExpectedFingerprint&&",
      "detail.datasetFingerprint===summary.datasetFingerprint&&",
      "detail.benchmarkGeneration===",
      "window.__cerebroPageBenchGeneration&&",
      "Number(detail.renderedPointCount)===",
      expected_cells,
      ";})()"
    ),
    correctness_detail = paste0(
      "(() => {const summary=window.cerebroLinkedViewsState?.summary?.()||{};",
      "return JSON.stringify({primaryReady:summary.primaryReady,",
      "ready:summary.ready,",
      "datasetFingerprint:summary.datasetFingerprint,",
      "projections:summary.projections?.length||0,",
      "spatialSections:summary.spatialSections?.length||0});})()"
    )
  )
)

all_pages <- pages
official_page_names <- c(
  "overview",
  "gene_expression",
  "groups",
  "coordinated_views",
  "immune_repertoire",
  "hla",
  "trajectory",
  "spatial"
)
gene_prime_overview <- identical(
  tolower(Sys.getenv("VIEWER_GENE_PRIME_OVERVIEW", unset = "false")),
  "true"
)
trajectory_prime_overview <- identical(
  tolower(Sys.getenv("VIEWER_TRAJECTORY_PRIME_OVERVIEW", unset = "false")),
  "true"
)
skip_visual_check <- identical(
  tolower(Sys.getenv("VIEWER_BENCH_SKIP_VISUAL_CHECK", unset = "false")),
  "true"
)
repeat_skip_warm_socket_meter <- identical(
  tolower(Sys.getenv(
    "VIEWER_REPEAT_SKIP_WARM_SOCKET_METER",
    unset = "false"
  )),
  "true"
)
skip_socket_meter <- identical(
  tolower(Sys.getenv("VIEWER_BENCH_SKIP_SOCKET_METER", unset = "false")),
  "true"
)
skip_settled_wait <- identical(
  tolower(Sys.getenv("VIEWER_BENCH_SKIP_SETTLED_WAIT", unset = "false")),
  "true"
)

only <- Sys.getenv("VIEWER_PAGES_ONLY")
if (nzchar(only)) {
  only <- trimws(strsplit(only, ",", fixed = TRUE)[[1L]])
  unknown <- setdiff(only, names(pages))
  if (length(unknown)) {
    stop("Unknown VIEWER_PAGES_ONLY page: ", unknown[[1L]], call. = FALSE)
  }
  pages <- pages[only]
} else {
  pages <- pages[official_page_names]
}

page_active_js <- function(page, require_event = TRUE) {
  event_ready <- if (!isTRUE(require_event) || is.null(page$ready_event)) {
    "true"
  } else {
    "window.__cerebroPageBenchSeen === true"
  }
  sprintf(
    paste0(
      "(() => {const p=document.getElementById('shiny-tab-%s');",
      "return !!p&&p.classList.contains('active')&&%s&&(%s);})()"
    ),
    page$tab,
    event_ready,
    page$ready
  )
}

arm_and_click_page <- function(app, page, selector, require_event = TRUE) {
  listener <- if (!isTRUE(require_event) || is.null(page$ready_event)) {
    ""
  } else {
    condition <- if (!is.null(page$ready_event_condition)) {
      page$ready_event_condition
    } else if (is.null(page$event_view)) {
      "e.detail?.ready===true"
    } else {
      sprintf(
        paste0(
          "e.detail?.viewId===%s&&",
          "(e.detail?.eventKind==='primary'||e.detail?.eventKind==='cached')"
        ),
        quote_r(page$event_view)
      )
    }
    sprintf(
      paste0(
        "window.addEventListener(%s,function h(e){",
        "if(window.__cerebroPageBenchGeneration===generation&&",
        "e.timeStamp >= clickStart&&%s){",
        "window.__cerebroPageBenchSeen=true;",
        "window.__cerebroPageBenchEventDetail=e.detail||null;",
        "window.__cerebroPageBenchEventAt=e.timeStamp;",
        "if(!completionRequired){",
        "window.__cerebroPageBenchSettledAt=e.timeStamp;}",
        "window.removeEventListener(%s,h);}});"
      ),
      quote_r(page$ready_event),
      condition,
      quote_r(page$ready_event)
    )
  }
  completion_listener <- if (
    is.null(page$ready_event) || is.null(page$completion_event_condition)
  ) {
    ""
  } else {
    sprintf(
      paste0(
        "window.addEventListener(%s,function c(e){",
        "if(window.__cerebroPageBenchGeneration===generation&&",
        "e.timeStamp >= clickStart&&",
        "e.detail?.benchmarkGeneration===generation&&",
        "e.detail?.datasetFingerprint===expectedFingerprint&&%s){",
        "window.__cerebroPageBenchCompleteAt=e.timeStamp;",
        "window.__cerebroPageBenchSettledAt=e.timeStamp;",
        "window.removeEventListener(%s,c);}});"
      ),
      quote_r(page$ready_event),
      page$completion_event_condition,
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
        "const expectedFingerprint=String(",
        "window.__cerebroPageBenchExpectedFingerprint||'');",
        "const completionRequired=%s;",
        "window.__cerebroPageBenchClickStart=clickStart;",
        "window.__cerebroPageBenchSeen=false;",
        "window.__cerebroPageBenchEventDetail=null;",
        "window.__cerebroPageBenchEventAt=null;",
        "window.__cerebroPageBenchCompleteAt=null;",
        "window.__cerebroPageBenchSettledAt=null;",
        "%s%sdocument.querySelector(%s).click();})()"
      ),
      if (is.null(page$completion_ready)) "false" else "true",
      listener,
      completion_listener,
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

load_benchmark_dataset <- function(app) {
  request <- paste0(
    "dataset-",
    format(as.numeric(Sys.time()) * 1000, scientific = FALSE, trim = TRUE),
    "-",
    sample.int(.Machine$integer.max, 1L)
  )
  driver_started <- proc.time()[["elapsed"]]
  app$run_js(sprintf(
    paste0(
      "(() => {const request=%s;",
      "window.__cerebroPageBenchDatasetRequest=request;",
      "window.__cerebroPageBenchDatasetStartedAt=performance.now();",
      "window.__cerebroPageBenchDatasetReadyAt=null;",
      "window.__cerebroPageBenchDatasetDetail=null;",
      "Shiny.addCustomMessageHandler('cerebro_benchmark_dataset_ready',",
      "function(detail){if(String(detail?.request||'')!==request)return;",
      "window.__cerebroPageBenchDatasetDetail=detail;",
      "window.__cerebroPageBenchDatasetReadyAt=performance.now();});",
      "Shiny.setInputValue('cerebro_benchmark_dataset_load_request',request,",
      "{priority:'event'});})()"
    ),
    quote_r(request)
  ))
  app$wait_for_js(
    paste0(
      "Number.isFinite(window.__cerebroPageBenchDatasetReadyAt)&&",
      "window.__cerebroPageBenchDatasetDetail?.cell_count===",
      expected_cells,
      "&&/^md5-cell-set-v1:[0-9a-f]{32}$/.test(",
      "String(window.__cerebroPageBenchDatasetDetail?.dataset_fingerprint||''))"
    ),
    timeout = 900000
  )
  app$wait_for_idle(timeout = 120000)
  driver_ms <- (proc.time()[["elapsed"]] - driver_started) * 1000
  value <- app$get_js(paste0(
    "(() => {const detail=window.__cerebroPageBenchDatasetDetail||{};",
    "const elapsed=window.__cerebroPageBenchDatasetReadyAt-",
    "window.__cerebroPageBenchDatasetStartedAt;",
    "window.__cerebroPageBenchExpectedFingerprint=",
    "String(detail.dataset_fingerprint||'');",
    "return {elapsedMs:elapsed,fingerprint:",
    "window.__cerebroPageBenchExpectedFingerprint,",
    "cellCount:Number(detail.cell_count)||0};})()"
  ))
  data.frame(
    dataset_load_ms = as.numeric(value$elapsedMs),
    dataset_load_driver_ms = driver_ms,
    dataset_fingerprint = as.character(value$fingerprint),
    dataset_cell_count = as.numeric(value$cellCount),
    stringsAsFactors = FALSE
  )
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
  trajectory_method <- if (is.na(page$expected_trajectory_method)) {
    NA_character_
  } else {
    as.character(app$get_js(
      "document.getElementById('trajectory_selected_method')?.value||''"
    ))
  }
  trajectory_name <- if (is.na(page$expected_trajectory_name)) {
    NA_character_
  } else {
    as.character(app$get_js(
      "document.getElementById('trajectory_selected_name')?.value||''"
    ))
  }
  list(
    pass = pass,
    point_count = point_count,
    detail = if (is.null(detail)) "" else as.character(detail),
    trajectory_method = trajectory_method,
    trajectory_name = trajectory_name
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
  if (!isTRUE(page$visual_check) || isTRUE(skip_visual_check)) {
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
  if (isTRUE(skip_socket_meter)) {
    return(invisible(TRUE))
  }
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
    "lastActivityAt:performance.now(),",
    "originalSend:socket.send,handler:null,wrappedSend:null};",
    "meter.handler=event=>{meter.received+=byteLength(event.data);",
    "meter.lastActivityAt=performance.now();};",
    "meter.wrappedSend=function(data){meter.sent+=byteLength(data);",
    "meter.lastActivityAt=performance.now();",
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
  if (isTRUE(skip_socket_meter)) {
    return(list(sent = 0, received = 0))
  }
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
      tree_rss <- function(root) {
        handle <- tryCatch(ps::ps_handle(root), error = function(error) NULL)
        if (is.null(handle) || !ps::ps_is_running(handle)) {
          return(NA_real_)
        }
        children <- tryCatch(
          ps::ps_children(handle, recursive = TRUE),
          error = function(error) list()
        )
        rss <- vapply(
          c(list(handle), children),
          function(member) {
            tryCatch(
              unname(ps::ps_memory_info(member)[["rss"]]),
              error = function(error) NA_real_
            )
          },
          numeric(1L)
        )
        rss <- rss[is.finite(rss)]
        if (!length(rss)) {
          return(NA_real_)
        }
        sum(rss) / 1024
      }
      update_peak <- function(current, observed) {
        if (!is.finite(observed)) {
          return(current)
        }
        if (!is.finite(current)) observed else max(current, observed)
      }
      peaks <- c(r_peak_rss_kib = NA_real_, chrome_peak_rss_kib = NA_real_)
      file.create(ready_file)
      repeat {
        peaks[[1L]] <- update_peak(peaks[[1L]], tree_rss(r_pid))
        peaks[[2L]] <- update_peak(peaks[[2L]], tree_rss(chrome_pid))
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
  # A recursive Windows process-tree sample can take several seconds. Give an
  # in-flight sample time to finish and persist its peaks after the stop signal.
  monitor$process$wait(30000)
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
  known_dashboard_error <- browser_error &
    grepl(
      "Cannot read properties of undefined (reading 'setValue')",
      logs$message,
      fixed = TRUE
    ) &
    grepl("shinydashboard", logs$message, fixed = TRUE)
  browser_error <- browser_error & !known_dashboard_error
  server_error <- logs$location == "shiny" &
    grepl(
      "Warning: Error|Execution halted|Error in ",
      logs$message
    )
  if (any(browser_error | server_error, na.rm = TRUE)) {
    details <- unique(as.character(logs$message[browser_error | server_error]))
    stop(
      "Browser or Shiny server error occurred: ",
      paste(details, collapse = " | "),
      call. = FALSE
    )
  }
  list(
    known_dashboard_error_count = sum(known_dashboard_error, na.rm = TRUE)
  )
}

socket_meter_snapshot <- function(app) {
  if (isTRUE(skip_socket_meter)) {
    return(list(sent = 0, received = 0, quietMs = Inf))
  }
  value <- app$get_js(paste0(
    "(() => {const meter=window.__cerebroPageBenchSocketMeter;",
    "if(!meter)throw new Error('Socket meter is not active');",
    "return {sent:meter.sent,received:meter.received,",
    "quietMs:performance.now()-meter.lastActivityAt};})()"
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

wait_for_socket_quiet <- function(app, quiet_ms = 1200) {
  if (isTRUE(skip_socket_meter)) {
    return(invisible(TRUE))
  }
  app$wait_for_js(
    sprintf(
      paste0(
        "(() => {const meter=window.__cerebroPageBenchSocketMeter;",
        "return !!meter&&performance.now()-meter.lastActivityAt>=%d;})()"
      ),
      as.integer(quiet_ms)
    ),
    timeout = 120000
  )
  invisible(TRUE)
}

page_completion_elapsed <- function(app, page) {
  if (isTRUE(skip_settled_wait)) {
    return(as.numeric(app$get_js(paste0(
      "window.__cerebroPageBenchEventAt-",
      "window.__cerebroPageBenchClickStart"
    ))))
  }
  if (!is.null(page$completion_ready)) {
    app$run_js(sprintf(
      paste0(
        "if(window.__cerebroPageBenchCompleteAt===null&&(%s)){",
        "window.__cerebroPageBenchCompleteAt=performance.now();",
        "window.__cerebroPageBenchSettledAt=",
        "window.__cerebroPageBenchCompleteAt;}"
      ),
      page$completion_ready
    ))
  }
  app$wait_for_js(
    "Number.isFinite(window.__cerebroPageBenchSettledAt)",
    timeout = 120000
  )
  as.numeric(app$get_js(paste0(
    "window.__cerebroPageBenchSettledAt-",
    "window.__cerebroPageBenchClickStart"
  )))
}

page_specialist_timing <- function(app) {
  value <- app$get_js(paste0(
    "(() => {const detail=window.__cerebroPageBenchEventDetail||{};",
    "const timing=detail.timing||{};",
    "const eventAt=window.__cerebroPageBenchEventAt;",
    "const hasCurrentEvent=eventAt!==null&&eventAt!==undefined&&",
    "Number.isFinite(Number(eventAt));",
    "const linked=hasCurrentEvent",
    "?window.cerebroLinkedViewsState?.summary?.()?.transport?.primary||{}:{};",
    "const click=Number(window.__cerebroPageBenchClickStart);",
    "const readyValue=timing.readyAtMs??eventAt;",
    "const ready=readyValue===null||readyValue===undefined",
    "?NaN:Number(readyValue);",
    "return {clickToRequestMs:timing.clickToRequestMs??linked.clickToRequestMs,",
    "serverPrepareMs:timing.serverPrepareMs??linked.serverPrepareMs,",
    "serverResourceMs:linked.serverResourceMs,",
    "serverBundleMs:linked.serverBundleMs,",
    "serializeTransferMs:timing.serializeTransferMs??linked.serializeTransferMs,",
    "decodeMs:timing.decodeMs??linked.decodeMs,",
    "projectionFetchMs:timing.projectionFetchMs??linked.projectionFetchMs,",
    "projectionDownloadMs:timing.projectionDownloadMs??linked.projectionDownloadMs,",
    "projectionDecodeMs:timing.projectionDecodeMs??linked.projectionDecodeMs,",
    "projectionCacheHit:timing.projectionCacheHit,",
    "projectionSubsetFetchMs:timing.projectionSubsetFetchMs,",
    "projectionSubsetDownloadMs:timing.projectionSubsetDownloadMs,",
    "projectionSubsetDecodeMs:timing.projectionSubsetDecodeMs,",
    "projectionSubsetMaterializeMs:timing.projectionSubsetMaterializeMs,",
    "projectionSubsetCacheHit:timing.projectionSubsetCacheHit,",
    "geometryReuseProtocol:timing.geometryReuseProtocol,",
    "geometryReuseSubset:timing.geometryReuseSubset,",
    "metadataFetchMs:linked.metadataFetchMs,",
    "rendererInitializationMs:linked.rendererInitializationMs,",
    "rendererApplyMs:linked.applyMs,",
    "buildSpacesMs:timing.buildSpacesMs,preDrawMs:timing.preDrawMs,",
    "firstDrawMs:timing.firstDrawMs??linked.decodeToDrawMs,",
    "activationMs:timing.activationMs,",
    "requestToReadyMs:timing.requestToReadyMs,",
    "bytes:timing.bytes??linked.bytes,",
    "projectionBytes:timing.projectionBytes??linked.projectionBytes,",
    "projectionSubsetBytes:timing.projectionSubsetBytes,",
    "metadataBytes:linked.metadataBytes,",
    "eventKind:detail.eventKind,",
    "generation:detail.generation??timing.generation,",
    "renderRequestSent:detail.renderRequestSent??timing.renderRequestSent,",
    "cached:timing.cached,geometryReused:timing.geometryReused,",
    "clickToReadyMs:Number.isFinite(click)&&Number.isFinite(ready)",
    "?ready-click:null};})()"
  ))
  number <- function(name) {
    result <- suppressWarnings(as.numeric(value[[name]]))
    if (length(result) != 1L || !is.finite(result)) NA_real_ else result
  }
  text <- function(name) {
    result <- value[[name]]
    if (is.null(result) || !length(result)) {
      NA_character_
    } else {
      as.character(result)
    }
  }
  flag <- function(name) {
    result <- value[[name]]
    if (is.null(result) || !length(result)) NA else isTRUE(result)
  }
  data.frame(
    specialist_event_kind = text("eventKind"),
    specialist_generation = number("generation"),
    render_request_sent = flag("renderRequestSent"),
    cached_activation = flag("cached"),
    geometry_reused = flag("geometryReused"),
    click_to_request_ms = number("clickToRequestMs"),
    server_prepare_ms = number("serverPrepareMs"),
    server_resource_ms = number("serverResourceMs"),
    server_bundle_ms = number("serverBundleMs"),
    serialize_transfer_ms = number("serializeTransferMs"),
    binary_decode_ms = number("decodeMs"),
    projection_fetch_ms = number("projectionFetchMs"),
    projection_download_ms = number("projectionDownloadMs"),
    projection_decode_ms = number("projectionDecodeMs"),
    projection_cache_hit = flag("projectionCacheHit"),
    projection_subset_fetch_ms = number("projectionSubsetFetchMs"),
    projection_subset_download_ms = number("projectionSubsetDownloadMs"),
    projection_subset_decode_ms = number("projectionSubsetDecodeMs"),
    projection_subset_materialize_ms = number("projectionSubsetMaterializeMs"),
    projection_subset_cache_hit = flag("projectionSubsetCacheHit"),
    geometry_reuse_protocol = text("geometryReuseProtocol"),
    geometry_reuse_subset = text("geometryReuseSubset"),
    metadata_fetch_ms = number("metadataFetchMs"),
    renderer_initialization_ms = number("rendererInitializationMs"),
    renderer_apply_ms = number("rendererApplyMs"),
    build_spaces_ms = number("buildSpacesMs"),
    pre_draw_ms = number("preDrawMs"),
    first_draw_ms = number("firstDrawMs"),
    activation_ms = number("activationMs"),
    request_to_ready_ms = number("requestToReadyMs"),
    click_to_ready_ms = number("clickToReadyMs"),
    primary_payload_bytes = number("bytes"),
    projection_asset_bytes = number("projectionBytes"),
    projection_subset_asset_bytes = number("projectionSubsetBytes"),
    metadata_asset_bytes = number("metadataBytes"),
    stringsAsFactors = FALSE
  )
}

specialist_payload_snapshot <- function(app, view_id) {
  value <- app$get_js(sprintf(
    paste0(
      "(() => {const value=window.__cerebroSpecialistBenchSnapshot?.();",
      "const primary=value?.primary?.byId?.[%s]||{};",
      "const aux=value?.aux?.byId?.[%s]||{};",
      "return {primaryCount:Number(primary.count)||0,",
      "primaryBytes:Number(primary.bytes)||0,",
      "auxCount:Number(aux.count)||0,auxBytes:Number(aux.bytes)||0};})()"
    ),
    quote_r(view_id),
    quote_r(view_id)
  ))
  data.frame(
    primary_payload_count = as.numeric(value$primaryCount),
    primary_payload_meter_bytes = as.numeric(value$primaryBytes),
    aux_payload_count = as.numeric(value$auxCount),
    aux_payload_bytes = as.numeric(value$auxBytes),
    stringsAsFactors = FALSE
  )
}

browser_start_attempts <- suppressWarnings(as.integer(Sys.getenv(
  "VIEWER_BROWSER_START_ATTEMPTS",
  unset = "3"
)))
if (
  length(browser_start_attempts) != 1L ||
    is.na(browser_start_attempts) ||
    browser_start_attempts < 1L ||
    browser_start_attempts > 5L
) {
  stop("VIEWER_BROWSER_START_ATTEMPTS must be between 1 and 5.", call. = FALSE)
}

browser_debug_port_error <- function(error) {
  grepl(
    "Chrome debugging port not open after [0-9]+ seconds",
    conditionMessage(error)
  )
}

stop_failed_app_driver <- function(error) {
  failed_app <- tryCatch(error$app, error = function(condition) NULL)
  if (!is.null(failed_app)) {
    try(failed_app$stop(), silent = TRUE)
  }
  invisible(NULL)
}

start_app_driver <- function(app_dir, name) {
  errors <- character()
  for (attempt in seq_len(browser_start_attempts)) {
    app <- tryCatch(
      shinytest2::AppDriver$new(
        app_dir,
        name = name,
        height = 950,
        width = 1619,
        load_timeout = 900000,
        timeout = 900000,
        check_names = FALSE
      ),
      error = function(error) {
        message <- strsplit(conditionMessage(error), "\n", fixed = TRUE)[[1L]][[
          1L
        ]]
        errors <<- c(errors, paste0("attempt ", attempt, ": ", message))
        retry <- browser_debug_port_error(error) &&
          attempt < browser_start_attempts
        stop_failed_app_driver(error)
        if (!retry) {
          stop(error)
        }
        NULL
      }
    )
    if (!is.null(app)) {
      return(list(app = app, attempts = attempt, errors = errors))
    }
  }
  stop("Browser startup retry loop ended without an AppDriver.", call. = FALSE)
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
  startup <- start_app_driver(
    app_dir,
    paste0("viewer_1m_page_", schedule_row$schedule_position)
  )
  app <- startup$app
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
  expected_label <- format(
    expected_cells,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
  if (is.null(count$html) || !grepl(expected_label, count$html, fixed = TRUE)) {
    stop("Data Info did not report ", expected_label, " cells.", call. = FALSE)
  }
  dataset_metrics <- load_benchmark_dataset(app)
  if (!isTRUE(page_available(app, page))) {
    if (isTRUE(page$required)) {
      stop("Missing required benchmark page: ", page$tab, call. = FALSE)
    }
    return(empty_observation("skipped", "page unavailable"))
  }
  shared_projection_primed <- FALSE
  if (
    (isTRUE(gene_prime_overview) &&
      identical(schedule_row$page, "gene_expression")) ||
      (isTRUE(trajectory_prime_overview) &&
        identical(schedule_row$page, "trajectory"))
  ) {
    start_socket_meter(app)
    overview_page <- all_pages[["overview"]]
    open_page(app, overview_page, require_event = TRUE)
    wait_for_socket_quiet(app)
    shared_projection_primed <- TRUE
    app$run_js(
      "document.querySelector(\"a[href='#shiny-tab-loadData']\").click();"
    )
    app$wait_for_js(
      "document.getElementById('shiny-tab-loadData').classList.contains('active')",
      timeout = 120000
    )
    wait_for_socket_quiet(app)
    stop_socket_meter(app)
  }
  warmed <- FALSE
  if (identical(schedule_row$visit, "repeat")) {
    if (!isTRUE(repeat_skip_warm_socket_meter)) {
      start_socket_meter(app)
    }
    open_page(app, page, require_event = TRUE)
    if (isTRUE(repeat_skip_warm_socket_meter)) {
      Sys.sleep(2)
    } else {
      wait_for_socket_quiet(app)
    }
    warmed <- TRUE
    app$run_js(
      "document.querySelector(\"a[href='#shiny-tab-loadData']\").click();"
    )
    app$wait_for_js(
      "document.getElementById('shiny-tab-loadData').classList.contains('active')",
      timeout = 120000
    )
    if (isTRUE(repeat_skip_warm_socket_meter)) {
      Sys.sleep(2)
    } else {
      wait_for_socket_quiet(app)
      stop_socket_meter(app)
    }
  }

  session$Performance$enable()
  app$run_js("window.__cerebroResetSpecialistBench?.();")
  start_socket_meter(app)
  socket_meter_active <- TRUE
  on.exit(
    {
      if (socket_meter_active) try(cleanup_socket_meter(app), silent = TRUE)
    },
    add = TRUE
  )
  monitor <- NULL
  monitor_stopped <- FALSE
  if (isTRUE(memory_mode)) {
    r_pid <- app$.__enclos_env__$private$shiny_process$get_pid()
    chrome_pid <- browser$get_browser()$.__enclos_env__$private$process$get_pid()
    monitor <- start_rss_monitor(r_pid, chrome_pid)
    on.exit(
      {
        if (!monitor_stopped) try(stop_rss_monitor(monitor), silent = TRUE)
      },
      add = TRUE
    )
  }
  require_event <- requires_ready_event(
    schedule_row$page,
    schedule_row$visit,
    warmed
  )
  elapsed_ms <- open_page(app, page, require_event = require_event)
  specialist_timing <- page_specialist_timing(app)
  primary_ready_ms <- if (is.finite(specialist_timing$click_to_ready_ms)) {
    specialist_timing$click_to_ready_ms
  } else {
    elapsed_ms
  }
  correctness <- page_correctness(app, page)
  websocket_at_ready <- socket_meter_snapshot(app)
  specialist_at_ready <- if (is.null(page$event_view)) {
    data.frame(
      primary_payload_count = 0,
      primary_payload_meter_bytes = 0,
      aux_payload_count = 0,
      aux_payload_bytes = 0
    )
  } else {
    specialist_payload_snapshot(app, page$event_view)
  }
  settled_ms <- page_completion_elapsed(app, page)
  wait_for_socket_quiet(app)
  specialist_final <- if (is.null(page$event_view)) {
    specialist_at_ready
  } else {
    specialist_payload_snapshot(app, page$event_view)
  }
  heap_used_bytes <- if (isTRUE(memory_mode)) {
    js_heap_used(session)
  } else {
    NA_real_
  }
  websocket <- stop_socket_meter(app)
  socket_meter_active <- FALSE
  resources <- if (isTRUE(memory_mode)) {
    value <- stop_rss_monitor(monitor)
    monitor_stopped <- TRUE
    value
  } else {
    c(r_peak_rss_kib = NA_real_, chrome_peak_rss_kib = NA_real_)
  }
  renderer <- page_renderer_diagnostics(app, page)
  visible_pixels <- page_visible_pixels(app, page)
  correctness_pass <- correctness$pass &&
    (is.na(visible_pixels$pass) || visible_pixels$pass)
  log_diagnostics <- assert_clean_logs(app)
  cbind(
    dataset_metrics,
    data.frame(
      status = if (correctness_pass) "ok" else "error",
      error = if (correctness_pass) "" else "Page correctness check failed.",
      startup_attempts = startup$attempts,
      startup_errors = paste(startup$errors, collapse = " | "),
      elapsed_ms = elapsed_ms,
      primary_ready_ms = primary_ready_ms,
      performance_ms = primary_ready_ms,
      settled_ms = settled_ms,
      complete_elapsed_ms = settled_ms,
      ready_event_required = require_event && !is.null(page$ready_event),
      correctness_pass = correctness_pass,
      rendered_point_count = correctness$point_count,
      expected_point_count = page$expected_point_count,
      trajectory_method = correctness$trajectory_method,
      trajectory_name = correctness$trajectory_name,
      expected_trajectory_method = page$expected_trajectory_method,
      expected_trajectory_name = page$expected_trajectory_name,
      correctness_detail = correctness$detail,
      navigator_gpu = renderer$navigator_gpu,
      renderer_backend = renderer$renderer_backend,
      renderer_adapter = renderer$renderer_adapter,
      renderer_context_lost = renderer$renderer_context_lost,
      renderer_error = renderer$renderer_error,
      known_dashboard_error_count = log_diagnostics$known_dashboard_error_count,
      visible_pixel_count = visible_pixels$count,
      visible_pixels_pass = visible_pixels$pass,
      r_peak_rss_kib = unname(resources[["r_peak_rss_kib"]]),
      chrome_peak_rss_kib = unname(resources[["chrome_peak_rss_kib"]]),
      js_heap_used_bytes = heap_used_bytes,
      websocket_sent_payload_bytes = as.numeric(websocket$sent),
      websocket_received_payload_bytes = as.numeric(websocket$received),
      websocket_received_at_ready_bytes = as.numeric(
        websocket_at_ready$received
      ),
      websocket_post_ready_received_bytes = as.numeric(
        websocket$received - websocket_at_ready$received
      ),
      primary_payload_count_at_ready = specialist_at_ready$primary_payload_count,
      primary_payload_bytes_at_ready = specialist_at_ready$primary_payload_meter_bytes,
      aux_payload_count_at_ready = specialist_at_ready$aux_payload_count,
      aux_payload_bytes_at_ready = specialist_at_ready$aux_payload_bytes,
      primary_payload_count = specialist_final$primary_payload_count,
      primary_payload_meter_bytes = specialist_final$primary_payload_meter_bytes,
      aux_payload_count = specialist_final$aux_payload_count,
      aux_payload_bytes = specialist_final$aux_payload_bytes,
      chrome_version = session$Browser$getVersion()$product,
      shared_projection_primed = shared_projection_primed,
      stringsAsFactors = FALSE
    ),
    specialist_timing
  )
}

git_args_at <- function(root, ...) {
  marker <- file.path(root, ".git")
  if (
    .Platform$OS.type == "unix" && file.exists(marker) && !dir.exists(marker)
  ) {
    pointer <- readLines(marker, warn = FALSE, n = 1L)
    git_dir <- sub("^gitdir: ", "", pointer)
    if (grepl("^[A-Za-z]:/", git_dir)) {
      drive <- tolower(substr(git_dir, 1L, 1L))
      git_dir <- paste0("/mnt/", drive, substring(git_dir, 3L))
    }
    if (dir.exists(git_dir)) {
      return(c(
        "-c",
        "core.autocrlf=true",
        paste0("--git-dir=", git_dir),
        paste0("--work-tree=", root),
        ...
      ))
    }
  }
  c("-C", root, ...)
}

git_value <- function(root, ...) {
  value <- system2("git", git_args_at(root, ...), stdout = TRUE, stderr = TRUE)
  if (!length(value)) NA_character_ else value[[1L]]
}

git_dirty <- function(root) {
  length(system2(
    "git",
    git_args_at(root, "status", "--porcelain"),
    stdout = TRUE,
    stderr = TRUE
  )) >
    0L
}

package_version_at <- function(root) {
  description <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")
  unname(description[[1L]])
}

artifact_provenance <- benchmark_artifact_provenance(crb)
provenance <- do.call(
  rbind,
  lapply(candidate_specs, function(candidate) {
    root <- candidate[["root"]]
    cbind(
      data.frame(
        candidate = candidate[["label"]],
        candidate_root = root,
        candidate_git_sha = git_value(root, "rev-parse", "HEAD"),
        candidate_git_dirty = git_dirty(root),
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
        benchmark_mode = benchmark_mode,
        expected_renderer = expected_renderer,
        profiler_instrumented = isTRUE(memory_mode),
        rounds = rounds,
        stringsAsFactors = FALSE
      ),
      artifact_provenance
    )
  })
)

schedule <- build_balanced_schedule(candidate_labels, names(pages), rounds)
visits_only <- trimws(Sys.getenv("VIEWER_VISITS_ONLY", unset = ""))
if (nzchar(visits_only)) {
  visits_only <- strsplit(visits_only, ",", fixed = TRUE)[[1L]]
  if (any(!visits_only %in% c("first", "repeat"))) {
    stop("VIEWER_VISITS_ONLY must contain first and/or repeat.", call. = FALSE)
  }
  schedule <- schedule[schedule$visit %in% visits_only, , drop = FALSE]
  schedule$schedule_position <- seq_len(nrow(schedule))
}
schedule_output <- sub("[.]tsv$", "_schedule.tsv", output)
if (identical(schedule_output, output)) {
  schedule_output <- paste0(output, ".schedule.tsv")
}
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write_validated_tsv(schedule, schedule_output)

empty_observation <- function(status, error) {
  data.frame(
    dataset_load_ms = NA_real_,
    dataset_load_driver_ms = NA_real_,
    dataset_fingerprint = NA_character_,
    dataset_cell_count = NA_real_,
    status = status,
    error = error,
    startup_attempts = NA_real_,
    startup_errors = "",
    elapsed_ms = NA_real_,
    primary_ready_ms = NA_real_,
    performance_ms = NA_real_,
    settled_ms = NA_real_,
    complete_elapsed_ms = NA_real_,
    ready_event_required = NA,
    correctness_pass = NA,
    rendered_point_count = NA_real_,
    expected_point_count = NA_real_,
    trajectory_method = NA_character_,
    trajectory_name = NA_character_,
    expected_trajectory_method = NA_character_,
    expected_trajectory_name = NA_character_,
    correctness_detail = "",
    navigator_gpu = NA,
    renderer_backend = NA_character_,
    renderer_adapter = NA_character_,
    renderer_context_lost = NA,
    renderer_error = NA_character_,
    known_dashboard_error_count = NA_real_,
    visible_pixel_count = NA_real_,
    visible_pixels_pass = NA,
    r_peak_rss_kib = NA_real_,
    chrome_peak_rss_kib = NA_real_,
    js_heap_used_bytes = NA_real_,
    websocket_sent_payload_bytes = NA_real_,
    websocket_received_payload_bytes = NA_real_,
    websocket_received_at_ready_bytes = NA_real_,
    websocket_post_ready_received_bytes = NA_real_,
    primary_payload_count_at_ready = NA_real_,
    primary_payload_bytes_at_ready = NA_real_,
    aux_payload_count_at_ready = NA_real_,
    aux_payload_bytes_at_ready = NA_real_,
    primary_payload_count = NA_real_,
    primary_payload_meter_bytes = NA_real_,
    aux_payload_count = NA_real_,
    aux_payload_bytes = NA_real_,
    chrome_version = NA_character_,
    shared_projection_primed = NA,
    specialist_event_kind = NA_character_,
    specialist_generation = NA_real_,
    render_request_sent = NA,
    cached_activation = NA,
    geometry_reused = NA,
    click_to_request_ms = NA_real_,
    server_prepare_ms = NA_real_,
    server_resource_ms = NA_real_,
    server_bundle_ms = NA_real_,
    serialize_transfer_ms = NA_real_,
    binary_decode_ms = NA_real_,
    projection_fetch_ms = NA_real_,
    projection_download_ms = NA_real_,
    projection_decode_ms = NA_real_,
    projection_cache_hit = NA,
    projection_subset_fetch_ms = NA_real_,
    projection_subset_download_ms = NA_real_,
    projection_subset_decode_ms = NA_real_,
    projection_subset_materialize_ms = NA_real_,
    projection_subset_cache_hit = NA,
    geometry_reuse_protocol = NA_character_,
    geometry_reuse_subset = NA_character_,
    metadata_fetch_ms = NA_real_,
    renderer_initialization_ms = NA_real_,
    renderer_apply_ms = NA_real_,
    build_spaces_ms = NA_real_,
    pre_draw_ms = NA_real_,
    first_draw_ms = NA_real_,
    activation_ms = NA_real_,
    request_to_ready_ms = NA_real_,
    click_to_ready_ms = NA_real_,
    primary_payload_bytes = NA_real_,
    projection_asset_bytes = NA_real_,
    projection_subset_asset_bytes = NA_real_,
    metadata_asset_bytes = NA_real_,
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
  row$performance_applicable <- identical(benchmark_mode, "timing") &&
    (!row$requires_webgpu || identical(row$renderer_backend, expected_renderer))
  row$pass <- if (isTRUE(row$performance_applicable)) {
    row$status == "ok" && page_budget_pass(row$performance_ms, row$budget_ms)
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
    if (is.finite(row$performance_ms)) {
      paste0(" ", round(row$performance_ms), " ms primary")
    } else {
      ""
    }
  )
  row
})

results <- do.call(rbind, rows)
results <- merge(results, provenance, by = "candidate", sort = FALSE)
results <- results[order(results$schedule_position), ]
write_validated_tsv(results, output)

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
write_validated_tsv(provenance, manifest_output)
print(results, row.names = FALSE)

bad_status <- results$status == "error" |
  (results$required & results$status != "ok")
missing_resources <- results$status == "ok" &
  ((!is.finite(results$dataset_load_ms)) |
    (!is.finite(results$primary_ready_ms)) |
    (!is.finite(results$settled_ms)) |
    (isTRUE(memory_mode) &
      (!is.finite(results$r_peak_rss_kib) |
        !is.finite(results$chrome_peak_rss_kib) |
        !is.finite(results$js_heap_used_bytes))) |
    !is.finite(results$websocket_sent_payload_bytes) |
    !is.finite(results$websocket_received_payload_bytes))
if (any(bad_status | missing_resources)) {
  stop(
    "Page observations contain errors or missing required metrics; refusing publication.",
    call. = FALSE
  )
}
if (identical(profile, "publication") && any(provenance$candidate_git_dirty)) {
  stop("Publication requires clean candidate worktrees.", call. = FALSE)
}
if (
  identical(profile, "publication") &&
    identical(benchmark_mode, "timing") &&
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
