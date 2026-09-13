#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) {
  stop(
    "Usage: benchmark_million_cell_viewer.R ROOT_A ROOT_B CRB [REPEATS] [PERCENT]",
    call. = FALSE
  )
}
roots <- c(
  a = normalizePath(args[[1L]], mustWork = TRUE),
  b = normalizePath(args[[2L]], mustWork = TRUE)
)
crb <- normalizePath(args[[3L]], mustWork = TRUE)
benchmark_genes <- c("Snap25", "Slc17a7", "Gad1")
sidecar_names <- file.path(sub("[.]crb$", ".bpcells", crb), "row_names")
if (file.exists(sidecar_names)) {
  available_genes <- readLines(sidecar_names, warn = FALSE)
  if (!all(benchmark_genes %in% available_genes)) {
    benchmark_genes <- c(
      "ENSMUSG00000027273",
      "ENSMUSG00000070570",
      "ENSMUSG00000070880"
    )
  }
}
repeats <- if (length(args) > 3L) as.integer(args[[4L]]) else 3L
percentage <- if (length(args) > 4L) as.numeric(args[[5L]]) else 100
if (
  is.na(repeats) ||
    repeats < 1L ||
    is.na(percentage) ||
    percentage < 1 ||
    percentage > 100
) {
  stop("REPEATS and PERCENT must be positive and PERCENT at most 100.")
}
Sys.setenv(NOT_CRAN = "true")
overview_only <- identical(
  Sys.getenv("CEREBRO_VIEWER_BENCH_OVERVIEW_ONLY"),
  "1"
)

quote_r <- function(value) encodeString(value, quote = '"')

run_once <- function(candidate, root, round) {
  has_gpu_renderer <- file.exists(
    file.path(root, "inst", "viewer", "www", "cell_points_gpu.js")
  )
  app_dir <- tempfile(paste0("million-cell-viewer-", candidate, "-"))
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(
      sprintf("devtools::load_all(%s, quiet = TRUE)", quote_r(root)),
      "launchCerebro(",
      "  mode = \"closed\",",
      sprintf("  crb_file_to_load = c(\"1M mouse brain\" = %s),", quote_r(crb)),
      sprintf("  percentage_cells_to_show = %s,", percentage),
      "  point_size = 1,",
      "  point_opacity = 0.5,",
      "  projections_show_hover_info = TRUE",
      ")"
    ),
    file.path(app_dir, "app.R")
  )

  shinytest2::local_app_support(app_dir)
  started <- proc.time()[["elapsed"]]
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = paste0("million-cell-viewer-", candidate, "-", round),
    height = 950,
    width = 1619,
    load_timeout = 900000,
    timeout = 900000,
    check_names = FALSE
  )
  on.exit(app$stop(), add = TRUE)
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-loadData\"]').click()"
  )
  app$wait_for_value(output = "load_data_number_of_cells", timeout = 900000)
  cell_count <- app$get_value(output = "load_data_number_of_cells")
  if (
    is.null(cell_count$html) ||
      !grepl("1,000,000", cell_count$html, fixed = TRUE)
  ) {
    stop("Data Info did not report 1,000,000 cells.", call. = FALSE)
  }
  data_ready <- proc.time()[["elapsed"]]
  message(candidate, " data ready")

  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-overview\"]').click()"
  )
  app$wait_for_js(
    paste0(
      "(() => {const h=document.getElementById(",
      "'overview_projection_cell_view_host');",
      "const g=h?.querySelector('canvas.cv-gpu-layer');",
      "if(",
      tolower(has_gpu_renderer),
      ")return ",
      "g?.style.display==='block'&&",
      "Number(g.dataset.pointCount)>0&&",
      "g._cerebroPointRenderer?.isReady();",
      "const c=h?.querySelector('canvas[id^=\"cv-cv-\"]');",
      "return !!c&&c.width>0&&c.height>0&&",
      "c.toDataURL('image/png').length>10000;})()"
    ),
    timeout = 900000
  )
  overview_ready <- proc.time()[["elapsed"]]
  message(candidate, " Overview ready")
  browser <- app$get_js(paste0(
    "(async()=>{const h=document.getElementById(",
    "'overview_projection_cell_view_host');",
    "const g=h.querySelector('canvas.cv-gpu-layer');",
    "const r=g?.style.display==='block'?g._cerebroPointRenderer:null;",
    "const c=r?g:h.querySelector('canvas[id^=\"cv-cv-\"]');",
    "if(r)await r.idle();",
    "const zoomIn=h.querySelector('[data-act=\"zin\"]');",
    "const zoomOut=h.querySelector('[data-act=\"zout\"]');",
    "if(!zoomIn||!zoomOut)throw new Error('zoom controls missing');",
    "const times=[];for(let i=0;i<10;i++){",
    "const t=performance.now();(i%2?zoomOut:zoomIn).click();",
    "if(r)await r.idle();times.push(performance.now()-t);}",
    "times.sort((a,b)=>a-b);const s=r?r.stats():",
    "{backend:'canvas2d',contextLost:false,error:''};",
    "return{backend:s.backend,points:r?Number(c.dataset.pointCount):",
    as.integer(round(1000000 * percentage / 100)),
    ",",
    "zoomMedianMs:times[4],zoomP95Ms:times[9],",
    "imageBytes:Math.round((c.toDataURL('image/png').length-22)*.75),",
    "contextLost:!!s.contextLost,gpuError:s.error||''};})()"
  ))
  if (
    (has_gpu_renderer && !identical(browser$backend, "webgpu")) ||
      isTRUE(browser$contextLost) ||
      nzchar(browser$gpuError) ||
      as.numeric(browser$imageBytes) < 10000
  ) {
    stop("Viewer canvas validation failed.", call. = FALSE)
  }

  if (overview_only) {
    return(data.frame(
      candidate = candidate,
      backend = browser$backend,
      round = round,
      percentage = percentage,
      rendered_points = as.numeric(browser$points),
      data_ready_ms = (data_ready - started) * 1000,
      overview_ready_ms = (overview_ready - data_ready) * 1000,
      linked_ready_ms = NA_real_,
      gene_ready_ms = NA_real_,
      gene_points = NA_real_,
      rgb_ready_ms = NA_real_,
      rgb_points = NA_real_,
      zoom_median_ms = as.numeric(browser$zoomMedianMs),
      zoom_p95_ms = as.numeric(browser$zoomP95Ms),
      image_bytes = as.numeric(browser$imageBytes),
      check.names = FALSE
    ))
  }

  linked_started <- proc.time()[["elapsed"]]
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click()"
  )
  app$wait_for_js(
    paste0(
      "(() => {if(!window.cerebroLinkedViewsState?.ready())return false;",
      "return Array.from(document.querySelectorAll(",
      "'#shiny-tab-coordinated_views canvas.cv-gpu-layer')).some(c=>",
      "c.style.display==='block'&&Number(c.dataset.pointCount)>0&&",
      "c._cerebroPointRenderer?.isReady());})()"
    ),
    timeout = 900000
  )
  linked_ready <- proc.time()[["elapsed"]]
  message(candidate, " Linked views ready")

  app$click(selector = 'a[href="#shiny-tab-geneExpression"]')
  app$wait_for_js(
    paste0(
      "typeof document.getElementById('expression_genes_input')",
      "?.selectize?.settings.load === 'function'"
    ),
    timeout = 900000
  )
  message(candidate, " Gene controls ready")
  app$wait_for_idle(timeout = 900000)
  app$get_js("new Promise(resolve=>setTimeout(()=>resolve(true),500))")
  genes_json <- jsonlite::toJSON(benchmark_genes, auto_unbox = FALSE)
  gene_stable <- app$get_js(paste0(
    "(async()=>{const gene=",
    genes_json,
    "[0],s=document.getElementById(",
    "'expression_genes_input').selectize;for(let i=0;i<20;i++){",
    "s.addOption({value:gene,text:gene});s.setValue([gene],true);",
    "await new Promise(r=>setTimeout(r,500));if(String(s.getValue())===gene)return true;}",
    "return false;})()"
  ))
  if (!isTRUE(gene_stable)) {
    stop("Gene selector did not stabilise.")
  }
  gene_started <- proc.time()[["elapsed"]]
  app$run_js(paste0(
    "(() => {const s=document.getElementById('expression_genes_input').selectize;",
    "const gene=",
    genes_json,
    "[0];",
    "s.clear(true);s.setValue([gene]);})()"
  ))
  app$wait_for_js(
    paste0(
      "(() => {const h=document.getElementById('expression_projection_cell_view_host');",
      "const g=h?.querySelector('canvas.cv-gpu-layer');",
      "const note=document.getElementById('cv-cbar-note')?.textContent||'';",
      "return g?.style.display==='block'&&Number(g.dataset.pointCount)>0&&",
      "g._cerebroPointRenderer?.isReady()&&note.includes(",
      genes_json,
      "[0]);})()"
    ),
    timeout = 900000
  )
  app$get_js(paste0(
    "(async()=>{const g=document.querySelector(",
    "'#expression_projection_cell_view_host canvas.cv-gpu-layer');",
    "await g._cerebroPointRenderer.idle();return true;})()"
  ))
  gene_ready <- proc.time()[["elapsed"]]
  gene_points <- app$get_js(paste0(
    "Number(document.querySelector(",
    "'#expression_projection_cell_view_host canvas.cv-gpu-layer')",
    ".dataset.pointCount)"
  ))
  message(candidate, " Gene ready")

  app$set_inputs(
    expression_projection_genes_in_separate_panels = "rgb"
  )
  app$wait_for_js(
    paste0(
      "['r','g','b'].every(c=>typeof document.getElementById(",
      "'expression_rgb_gene_'+c)?.selectize?.settings.load==='function')"
    ),
    timeout = 900000
  )
  message(candidate, " RGB controls ready")
  rgb_stable <- app$get_js(paste0(
    "(async()=>{const names=",
    genes_json,
    ",channels=['r','g','b'];",
    "for(let n=0;n<20;n++){channels.forEach((c,i)=>{const s=document.getElementById(",
    "'expression_rgb_gene_'+c).selectize,v=names[i];",
    "s.addOption({value:v,text:v});s.setValue(v,true);});",
    "await new Promise(r=>setTimeout(r,500));if(channels.every((c,i)=>",
    "document.getElementById('expression_rgb_gene_'+c).selectize.getValue()===",
    "names[i]))return true;}return false;})()"
  ))
  if (!isTRUE(rgb_stable)) {
    stop("RGB selectors did not stabilise.")
  }
  rgb_started <- proc.time()[["elapsed"]]
  app$run_js(paste0(
    "(() => {const names=",
    genes_json,
    ";const channels=['r','g','b'];",
    "channels.forEach((c,i)=>{const v=names[i],s=document.getElementById(",
    "'expression_rgb_gene_'+c).selectize;s.clear(true);s.setValue(v);});})()"
  ))
  app$wait_for_js(
    paste0(
      "(() => {const h=document.getElementById('expression_projection_cell_view_host');",
      "const g=h?.querySelector('canvas.cv-gpu-layer');",
      "const legend=document.getElementById('cv-legend')?.textContent||'';",
      "return g?.style.display==='block'&&Number(g.dataset.pointCount)>0&&",
      "g._cerebroPointRenderer?.isReady()&&",
      genes_json,
      ".every(x=>legend.includes(x));})()"
    ),
    timeout = 900000
  )
  app$get_js(paste0(
    "(async()=>{const g=document.querySelector(",
    "'#expression_projection_cell_view_host canvas.cv-gpu-layer');",
    "await g._cerebroPointRenderer.idle();return true;})()"
  ))
  rgb_ready <- proc.time()[["elapsed"]]
  rgb_points <- app$get_js(paste0(
    "Number(document.querySelector(",
    "'#expression_projection_cell_view_host canvas.cv-gpu-layer')",
    ".dataset.pointCount)"
  ))
  message(candidate, " RGB ready")

  data.frame(
    candidate = candidate,
    backend = browser$backend,
    round = round,
    percentage = percentage,
    rendered_points = as.numeric(browser$points),
    data_ready_ms = (data_ready - started) * 1000,
    overview_ready_ms = (overview_ready - data_ready) * 1000,
    linked_ready_ms = (linked_ready - linked_started) * 1000,
    gene_ready_ms = (gene_ready - gene_started) * 1000,
    gene_points = as.numeric(gene_points),
    rgb_ready_ms = (rgb_ready - rgb_started) * 1000,
    rgb_points = as.numeric(rgb_points),
    zoom_median_ms = as.numeric(browser$zoomMedianMs),
    zoom_p95_ms = as.numeric(browser$zoomP95Ms),
    image_bytes = as.numeric(browser$imageBytes),
    check.names = FALSE
  )
}

rows <- list()
for (round in seq_len(repeats)) {
  order <- if (round %% 2L) c("a", "b") else c("b", "a")
  for (candidate in order) {
    message("viewer round ", round, ": ", candidate)
    rows[[length(rows) + 1L]] <- run_once(
      candidate,
      roots[[candidate]],
      round
    )
  }
}
raw <- do.call(rbind, rows)
summary <- aggregate(
  raw[c(
    "rendered_points",
    "data_ready_ms",
    "overview_ready_ms",
    "linked_ready_ms",
    "gene_ready_ms",
    "gene_points",
    "rgb_ready_ms",
    "rgb_points",
    "zoom_median_ms",
    "zoom_p95_ms",
    "image_bytes"
  )],
  raw[c("candidate", "backend")],
  median
)
cat("RAW\n")
write.table(raw, row.names = FALSE, sep = "\t", quote = FALSE)
cat("SUMMARY\n")
write.table(summary, row.names = FALSE, sep = "\t", quote = FALSE)
