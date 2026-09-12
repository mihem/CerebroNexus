#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true")
if (length(args) < 3L || length(args) > 4L) {
  stop(
    "usage: benchmark_viewer_1m_pages.R REPO_ROOT CRB OUTPUT_TSV [REPEATS]",
    call. = FALSE
  )
}

root <- normalizePath(args[[1L]], mustWork = TRUE)
crb <- normalizePath(args[[2L]], mustWork = TRUE)
output <- normalizePath(args[[3L]], mustWork = FALSE)
repeats <- if (length(args) == 4L) as.integer(args[[4L]]) else 3L
if (is.na(repeats) || repeats < 1L) {
  stop("REPEATS must be a positive integer.", call. = FALSE)
}

quote_r <- function(value) encodeString(value, quote = '"')
page <- function(tab, ready = NULL, budget_ms = 2000, required = FALSE) {
  list(tab = tab, ready = ready, budget_ms = budget_ms, required = required)
}
pages <- list(
  overview = page(
    "overview",
    "#overview_projection_cell_view_host canvas:not(.cv-mini)",
    required = TRUE
  ),
  coordinated_views = page("coordinated_views"),
  groups = page(
    "groups",
    "#groups_nUMI_plot.js-plotly-plot",
    required = TRUE
  ),
  marker_genes = page("markerGenes"),
  most_expressed_genes = page("mostExpressedGenes"),
  enriched_pathways = page("enrichedPathways"),
  extra_material = page("extra_material"),
  immune_repertoire = page(
    "immune_repertoire",
    paste(
      "#ir_clonalUMAP_projection_cell_view_host canvas:not(.cv-mini),",
      "#ir_visualizations_UI .js-plotly-plot"
    ),
    required = TRUE
  ),
  trajectory = page(
    "trajectory",
    "#trajectory_projection_cell_view_host canvas:not(.cv-mini)",
    budget_ms = 3000,
    required = TRUE
  ),
  spatial = page("spatial"),
  trekker = page("trekker"),
  hla = page(
    "hla_tcr_motifs",
    "#hla_plot_motifNetwork canvas",
    budget_ms = 3000,
    required = TRUE
  ),
  gene_expression = page(
    "geneExpression",
    "#expression_projection_cell_view_host canvas:not(.cv-mini)",
    required = TRUE
  ),
  gene_id_conversion = page("geneIdConversion"),
  color_management = page("color_management"),
  analysis_info = page("analysis_info"),
  about = page("about")
)

page_active_js <- function(page) {
  ready <- if (is.null(page$ready)) {
    "true"
  } else {
    sprintf("!!p.querySelector(%s)", quote_r(page$ready))
  }
  sprintf(
    "(() => { const p=document.getElementById('shiny-tab-%s'); return !!p && p.classList.contains('active') && !document.documentElement.classList.contains('shiny-busy') && %s; })()",
    page$tab,
    ready
  )
}

page_available <- function(app, page) {
  selector <- sprintf("a[href='#shiny-tab-%s']", page$tab)
  app$get_js(sprintf("!!document.querySelector(%s)", quote_r(selector)))
}

open_page <- function(app, page) {
  selector <- sprintf("a[href='#shiny-tab-%s']", page$tab)
  exists <- page_available(app, page)
  if (!isTRUE(exists)) {
    if (isTRUE(page$required)) {
      stop("Missing required benchmark page: ", page$tab, call. = FALSE)
    }
    return(NA_real_)
  }
  started <- proc.time()[["elapsed"]]
  app$run_js(sprintf("document.querySelector(%s).click();", quote_r(selector)))
  app$wait_for_js(page_active_js(page), timeout = 900000)
  (proc.time()[["elapsed"]] - started) * 1000
}

run_once <- function(round) {
  app_dir <- tempfile("viewer-1m-pages-")
  dir.create(app_dir)
  on.exit(unlink(app_dir, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c(
      sprintf("devtools::load_all(%s, quiet = TRUE)", quote_r(root)),
      "launchCerebro(",
      "  mode = \"closed\",",
      sprintf("  crb_file_to_load = c(\"1M pages\" = %s),", quote_r(crb)),
      "  percentage_cells_to_show = 100,",
      "  projections_show_hover_info = TRUE,",
      "  initial_page = \"data_info\"",
      ")"
    ),
    file.path(app_dir, "app.R")
  )
  suppressWarnings(shinytest2::local_app_support(app_dir))
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = paste0("viewer_1m_pages_", round),
    height = 950,
    width = 1619,
    load_timeout = 900000,
    timeout = 900000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_value(output = "load_data_number_of_cells", timeout = 900000)
  count <- app$get_value(output = "load_data_number_of_cells")
  if (is.null(count$html) || !grepl("1,000,000", count$html, fixed = TRUE)) {
    stop("Data Info did not report 1,000,000 cells.", call. = FALSE)
  }

  rows <- list()
  record <- function(visit, name, elapsed_ms) {
    page <- pages[[name]]
    budget <- if (identical(visit, "first")) page$budget_ms else 500
    rows[[length(rows) + 1L]] <<- data.frame(
      round = round,
      visit = visit,
      page = name,
      elapsed_ms = elapsed_ms,
      budget_ms = budget,
      pass = elapsed_ms <= budget,
      check.names = FALSE
    )
  }

  for (name in names(pages)) {
    elapsed <- open_page(app, pages[[name]])
    if (is.finite(elapsed)) {
      record("first", name, elapsed)
    }
  }
  for (name in names(pages)) {
    if (!isTRUE(page_available(app, pages[[name]]))) {
      next
    }
    app$run_js(
      "document.querySelector(\"a[href='#shiny-tab-loadData']\").click();"
    )
    app$wait_for_js(
      "document.getElementById('shiny-tab-loadData').classList.contains('active')",
      timeout = 900000
    )
    record("repeat", name, open_page(app, pages[[name]]))
  }
  do.call(rbind, rows)
}

results <- do.call(rbind, lapply(seq_len(repeats), run_once))
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.table(results, output, row.names = FALSE, sep = "\t", quote = FALSE)
print(results, row.names = FALSE)
if (any(!results$pass)) {
  stop("One or more 1M page budgets failed; see ", output, call. = FALSE)
}
