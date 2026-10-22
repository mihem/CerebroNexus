skip_if_not_installed("shinytest2")
library(shinytest2)

inst_dir <- viewer_app_test_path()

## The recordings on the default data set navigate to the tab they assert on, so
## they share one driver. Tests that switch data sets, assert a pristine startup
## or build their own bundle boot their own app.
shared_app <- viewer_shared_app("app_inst", inst_dir)
## The three dataset-switching tests below each need a new session that starts
## on the default data set. They share one app process; each after the first
## starts a new Shiny session in it rather than booting another app.
switching_app <- viewer_shared_app("dataset_switching", inst_dir)

## shinytest2's wait_for_idle() tracks server reactivity, NOT the async
## client-side cell-view renderer (www/cell_views.js). After it returns,
## a projection output can still be mid-paint and a renderUI-created input can be
## unbound, so reading a value or setting such an input the instant wait_for_idle
## returns races the render and intermittently fails (a 500 on the value URL, a
## NULL output, or "input binding not found"). These helpers poll for the
## condition instead of reading once, which is what de-flakes these recordings.

## Poll get_value(...) until it returns a non-NULL result or the timeout expires.
## A query that errors (server 500 while the output is still rendering) is
## retried; if no value arrives, report the last real error instead of replacing
## it with an unexplained NULL. `validate`, when supplied, is a predicate the
## value must also satisfy: an async output can return a non-NULL but transient
## error state (e.g. a shiny.silent.error while req() is unmet) that is not yet
## the real payload, so keep polling until validate(val) is TRUE. A predicate
## that errors counts as not-yet-ready and is retried.
retry_get_value <- function(
  app,
  ...,
  timeout = 20000,
  interval = 300,
  validate = NULL
) {
  deadline <- Sys.time() + timeout / 1000
  last_error <- NULL
  repeat {
    val <- tryCatch(
      app$get_value(...),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    ok <- !is.null(val)
    if (ok && !is.null(validate)) {
      ok <- tryCatch(
        isTRUE(validate(val)),
        error = function(e) {
          last_error <<- e
          FALSE
        }
      )
    }
    if (ok) {
      return(val)
    }
    if (Sys.time() > deadline) {
      if (!is.null(last_error)) {
        stop(last_error)
      }
      return(val)
    }
    Sys.sleep(interval / 1000)
  }
}

test_that("retry_get_value reports the last error after timing out", {
  attempts <- 0L
  app <- list(get_value = function(...) {
    attempts <<- attempts + 1L
    stop(sprintf("render attempt %d failed", attempts), call. = FALSE)
  })

  expect_error(
    retry_get_value(app, timeout = 5, interval = 1),
    "render attempt [0-9]+ failed"
  )
  expect_gt(attempts, 1L)
})

## Wait until an input element exists in the DOM (its Shiny binding is registered)
## before set_inputs — a renderUI-created selectize is not present the instant
## wait_for_idle returns.
## The dataset selector is rendered by the server, so a new session may not have
## bound it yet ("Unable to find input binding"). Switch only once it has a value.
select_dataset <- function(app, dataset, timeout = 30000) {
  app$wait_for_js(
    "Shiny.shinyapp.$inputValues.crb_file_selector !== undefined",
    timeout = timeout
  )
  app$set_inputs(crb_file_selector = dataset, wait_ = FALSE)
}

wait_for_input <- function(app, id, timeout = 20000) {
  app$wait_for_js(
    sprintf("document.getElementById('%s') !== null", id),
    timeout = timeout
  )
}

## Activate a conditionally shown sidebar tab. Wait for the menu link to be
## available, then click it so the tab's outputs render reliably on slow CI.
activate_tab <- function(app, tab_name, timeout = 20000) {
  selector <- sprintf("a[href=\"#shiny-tab-%s\"]", tab_name)
  app$wait_for_js(
    sprintf("document.querySelector('%s') !== null", selector),
    timeout = timeout
  )
  app$run_js(sprintf("document.querySelector('%s').click();", selector))
}

test_that("{shinytest2} recording: overview", {
  app <- shared_app()

  ## Data Info tab: verify key values from the loaded example.crb
  cells_box <- retry_get_value(app, output = "load_data_overview")
  expect_true(grepl("1,?000", cells_box$html))

  organism_box <- retry_get_value(app, output = "load_data_overview")
  expect_true(grepl("Human", organism_box$html))

  date_box <- retry_get_value(app, output = "load_data_overview")
  expect_true(grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", date_box$html))
})

test_that("Projection switches categorical spatial datasets coherently", {
  app <- viewer_new_session(switching_app())

  select_dataset(app, "Mouse ileum (MERFISH)")
  app$wait_for_idle(timeout = 30000)
  activate_tab(app, "overview", timeout = 30000)
  app$wait_for_js(
    paste0(
      "document.getElementById('overview_projection_point_color')?.value ",
      "=== 'cell_type'"
    ),
    timeout = 30000
  )

  select_dataset(app, "Mouse brain (Xenium)")
  app$wait_for_js(
    paste0(
      "document.getElementById('overview_projection_point_color')?.value ",
      "=== 'cluster'"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#overview_projection_cell_view_host canvas:not(.cv-mini)') !== null"
    ),
    timeout = 30000
  )

  expect_false(any(grepl(
    "color_assignments are required for categorical cell views",
    app$get_logs()$message,
    fixed = TRUE
  )))
})

test_that("Spatial backgrounds reset when the spatial dataset changes", {
  app <- viewer_new_session(switching_app())

  select_dataset(app, "Mouse brain (Visium)")
  app$wait_for_idle(timeout = 30000)
  activate_tab(app, "spatial", timeout = 30000)
  wait_for_input(app, "spatial_projection_more_button", timeout = 30000)
  ## On a busy CI runner the first click once left the background picker
  ## missing for 30 s. Open a closed drawer, or repeat its render request about
  ## once a second, until the picker is there.
  app$wait_for_js(
    paste0(
      "(() => {if (document.getElementById('spatial_projection_background_image'))",
      " return true;",
      "const drawer = document.getElementById('spatial_projection_more');",
      "if (!drawer || drawer.getAttribute('aria-hidden') !== 'false') {",
      " document.getElementById('spatial_projection_more_button').click();",
      "} else if (!(Date.now() - (window.__spatialDrawerRequest || 0) < 1000)) {",
      " window.__spatialDrawerRequest = Date.now();",
      " Shiny.setInputValue('spatial_projection_more_render_request',",
      " Date.now(), {priority: 'event'});",
      "}",
      "return false;})()"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('spatial_projection_background_image').value === ",
      "'external::Tissue background'"
    ),
    timeout = 30000
  )
  expect_identical(
    retry_get_value(
      app,
      input = "spatial_projection_to_display",
      timeout = 30000,
      validate = function(value) identical(value, "anterior1")
    ),
    "anterior1"
  )
  ## The picker holds one removable chip per eligible image; "no background" is
  ## the empty selection rather than an entry of its own, so the deployment's
  ## external image is the only option for this scope.
  expect_identical(
    unlist(
      app$get_js(paste0(
        "Object.keys(document.getElementById(",
        "'spatial_projection_background_image').selectize.options)"
      )),
      use.names = FALSE
    ),
    "external::Tissue background"
  )
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#spatial_projection_cell_view_host canvas:not(.cv-mini)')"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "(() => {const host=document.getElementById(",
      "'spatial_projection_cell_view_host');",
      "const underlay=host?.querySelector('canvas.cv-underlay-layer');",
      "const overlay=host?.querySelector('canvas.cv-layer-overlay');",
      "return !!underlay&&!!overlay&&",
      "getComputedStyle(underlay).zIndex==='0'&&",
      "getComputedStyle(overlay).zIndex==='2';})()"
    ),
    timeout = 30000
  )
  wait_for_input(app, "spatial_projection_background_scale", timeout = 30000)
  wait_for_input(app, "spatial_projection_background_scale_x", timeout = 30000)
  wait_for_input(app, "spatial_projection_background_scale_y", timeout = 30000)
  app$wait_for_js(
    paste0(
      "document.getElementById('spatial_projection_background_scale').value ",
      "=== '1.55' && ",
      "document.getElementById('spatial_projection_background_scale_x').value ",
      "=== '1.55' && ",
      "document.getElementById('spatial_projection_background_scale_y').value ",
      "=== '1.55'"
    ),
    timeout = 30000
  )
  select_dataset(app, "Mouse brain (Xenium)")
  app$wait_for_idle(timeout = 30000)
  wait_for_input(app, "spatial_projection_background_image", timeout = 30000)
  app$wait_for_js(
    paste0(
      "document.getElementById('spatial_projection_to_display')?.",
      "selectize?.options.fov_colour !== undefined"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('spatial_projection_background_image').value === ",
      "'external::Tissue background'"
    ),
    timeout = 30000
  )
  app$set_inputs(spatial_projection_to_display = "fov_colour", wait_ = FALSE)
  app$wait_for_js(
    paste0(
      "document.getElementById('spatial_projection_background_image').value === ",
      "'external::Pink stain'"
    ),
    timeout = 30000
  )
  expect_identical(
    unlist(
      app$get_js(paste0(
        "Object.keys(document.getElementById(",
        "'spatial_projection_background_image').selectize.options)"
      )),
      use.names = FALSE
    ),
    c("external::Pink stain", "external::Fluorescent yellow")
  )
  app$set_inputs(
    spatial_projection_background_image = "external::Fluorescent yellow",
    wait_ = FALSE
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('spatial_projection_background_image').value === ",
      "'external::Fluorescent yellow'"
    ),
    timeout = 30000
  )
  select_dataset(app, "Mouse hippocampus (Slide-seq v2)")
  app$wait_for_idle(timeout = 30000)
  ## Slide-seq ships no background image, and a data set without images renders
  ## no picker at all: the previous selection cannot survive into one.
  app$wait_for_js(
    "document.getElementById('spatial_projection_background_image') === null",
    timeout = 30000
  )
  expect_identical(
    retry_get_value(
      app,
      input = "spatial_projection_to_display",
      timeout = 30000,
      validate = function(value) identical(value, "image")
    ),
    "image"
  )
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#spatial_projection_cell_view_host canvas:not(.cv-mini)')"
    ),
    timeout = 30000
  )
})

test_that("Linked views resets the active background to its preset", {
  app <- viewer_new_session(switching_app())
  select_dataset(app, "Mouse brain (Xenium)")
  activate_tab(app, "coordinated_views", timeout = 30000)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-pick-spatial')?.selectize ",
      "!== undefined"
    ),
    timeout = 30000
  )
  app$run_js(paste0(
    "document.getElementById('cv-pick-spatial').selectize",
    ".setValue(['fov', 'fov_colour']);"
  ))
  ## Backgrounds are scoped by a section picker now: the section that carries
  ## the Xenium colour image is the one the old tab strip used to select.
  app$wait_for_js(
    paste0(
      "(() => {const s=document.getElementById('cv-bg-space-select');",
      "return !!(s&&s.selectize&&",
      "Object.keys(s.selectize.options).includes('spatial::fov_colour'));})()"
    ),
    timeout = 30000
  )
  app$run_js(paste0(
    "document.getElementById('cv-bg-space-select')",
    ".selectize.setValue('spatial::fov_colour');"
  ))
  app$wait_for_js(
    "document.getElementById('cv-img-rotate-number') !== null",
    timeout = 30000
  )
  app$run_js(paste0(
    "var rotateNumber = document.getElementById('cv-img-rotate-number');",
    "rotateNumber.value = '83.79';",
    "rotateNumber.dispatchEvent(new Event('input', { bubbles: true }));"
  ))
  app$wait_for_js(
    "document.getElementById('cv-img-rotate').value === '83.79'",
    timeout = 30000
  )
  app$run_js(paste0(
    "var rotate = document.getElementById('cv-img-rotate');",
    "rotate.value = '42';",
    "rotate.dispatchEvent(new Event('input', { bubbles: true }));"
  ))
  app$wait_for_js(
    "document.getElementById('cv-img-rotate-number').value === '42'",
    timeout = 30000
  )
  app$run_js(paste0(
    "var rotateNumber = document.getElementById('cv-img-rotate-number');",
    "rotateNumber.value = '83.79';",
    "rotateNumber.dispatchEvent(new Event('input', { bubbles: true }));",
    "var flipX = document.getElementById('cv-img-flipx');",
    "flipX.checked = true;",
    "flipX.dispatchEvent(new Event('change', { bubbles: true }));",
    "var flipY = document.getElementById('cv-img-flipy');",
    "flipY.checked = false;",
    "flipY.dispatchEvent(new Event('change', { bubbles: true }));",
    "document.getElementById('cv-img-reset').click();"
  ))
  Sys.sleep(0.5)
  state <- app$get_js(paste0(
    "({",
    "rotation: document.getElementById('cv-img-rotate').value,",
    "rotationNumber: document.getElementById('cv-img-rotate-number').value,",
    "offsetX: document.getElementById('cv-img-offx').value,",
    "offsetY: document.getElementById('cv-img-offy').value,",
    "flipX: document.getElementById('cv-img-flipx').checked,",
    "flipY: document.getElementById('cv-img-flipy').checked",
    "})"
  ))
  expect_identical(state$rotation, "0")
  expect_identical(state$rotationNumber, "0")
  expect_identical(state$offsetX, "10")
  expect_identical(state$offsetY, "0")
  expect_false(state$flipX)
  expect_true(state$flipY)
})


test_that("{shinytest2} recording: main", {
  app <- shared_app()

  activate_tab(app, "overview")
  app$wait_for_idle(timeout = 10000)

  ## verify the shared Canvas projection renders
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#overview_projection_cell_view_host canvas:not(.cv-mini)') !== null"
    ),
    timeout = 20000
  )
  plot_size <- app$get_js(
    paste0(
      "(function(){var e=document.querySelector(",
      "'#overview_projection_cell_view_host canvas:not(.cv-mini)');",
      "return {w:e.clientWidth,h:e.clientHeight};})()"
    )
  )
  expect_gte(as.numeric(plot_size$w), 300)
  expect_gte(as.numeric(plot_size$h), 240)
  expect_identical(
    as.numeric(app$get_js(
      "Number(document.querySelector('#overview_projection_point_size').value)"
    )),
    6
  )
  expect_identical(
    as.numeric(app$get_js(
      "Number(document.querySelector('#overview_projection_point_opacity').value)"
    )),
    1
  )

  ## get unfiltered cell count
  cells_all <- retry_get_value(app, export = "overview_cells_to_show")
  expect_true(length(cells_all) > 0)

  ## filter to cluster 0 only and verify fewer cells are shown
  app$set_inputs(overview_projection_group_filter_seurat_clusters = "0")
  app$wait_for_idle(timeout = 10000)
  cells_filtered <- retry_get_value(app, export = "overview_cells_to_show")
  expect_true(length(cells_filtered) < length(cells_all))

  ## verify input parameters are applied
  app$set_inputs(overview_projection_point_size = 9)
  app$set_inputs(overview_projection_point_opacity = 0.9)
  app$set_inputs(overview_projection_percentage_cells_to_show = 100)
  app$wait_for_idle(timeout = 10000)

  expect_mapequal(
    app$get_values(
      input = c(
        "overview_projection_point_size",
        "overview_projection_point_opacity",
        "overview_projection_percentage_cells_to_show",
        "overview_projection_group_filter_seurat_clusters"
      )
    )$input,
    list(
      overview_projection_point_size = 9,
      overview_projection_point_opacity = 0.9,
      overview_projection_percentage_cells_to_show = 100,
      overview_projection_group_filter_seurat_clusters = "0"
    )
  )
})


test_that("{shinytest2} recording: groups", {
  app <- shared_app()

  app$set_inputs(sidebar = "groups")
  app$wait_for_idle(timeout = 10000)

  ## composition plot renders
  plot_val <- retry_get_value(app, output = "groups_by_other_group_plot")
  expect_false(is.null(plot_val))

  ## switch to percent view
  app$set_inputs(groups_by_other_group_show_as_percent = TRUE)
  app$wait_for_idle(timeout = 10000)
  plot_pct <- retry_get_value(app, output = "groups_by_other_group_plot")
  expect_false(is.null(plot_pct))

  ## show table
  app$set_inputs(groups_by_other_group_show_table = TRUE)
  app$wait_for_idle(timeout = 10000)
  table_val <- retry_get_value(app, output = "groups_by_other_group_table")
  expect_false(is.null(table_val))

  expect_mapequal(
    app$get_values(
      input = c(
        "groups_by_other_group_show_as_percent",
        "groups_by_other_group_show_table"
      )
    )$input,
    list(
      groups_by_other_group_show_as_percent = TRUE,
      groups_by_other_group_show_table = TRUE
    )
  )
})

test_that("{shinytest2} recording: marker_genes", {
  app <- shared_app()

  # Marker genes is a conditionally shown sidebar item. Wait for it, then click,
  # so it activates on a slow runner instead of navigating too early.
  activate_tab(app, "markerGenes")
  app$wait_for_idle(timeout = 10000)

  ## select seurat_clusters (only group with actual marker genes)
  app$set_inputs(marker_genes_selected_table = "seurat_clusters", wait_ = FALSE)
  app$wait_for_idle(timeout = 10000)

  ## table renders — marker gene results render asynchronously, so the output can
  ## momentarily hold a shiny.silent.error (req() not yet satisfied) that
  ## serialises to non-JSON. Poll until the value parses as the expected DT
  ## payload instead of reading once and letting fromJSON choke on that transient
  ## error state.
  table_val <- retry_get_value(
    app,
    output = "marker_genes_table",
    validate = function(v) {
      !is.null(jsonlite::fromJSON(v, simplifyVector = FALSE)$x$container)
    }
  )
  expect_false(is.null(table_val))

  ## verify expected columns are present in the table header
  parsed <- jsonlite::fromJSON(table_val, simplifyVector = FALSE)
  container_html <- parsed$x$container
  for (col in c(
    "gene",
    "p_val",
    "avg_log2FC",
    "pct.1",
    "pct.2",
    "p_val_adj",
    "on_cell_surface"
  )) {
    expect_true(
      grepl(col, container_html, fixed = TRUE),
      label = paste("column present:", col)
    )
  }

  ## "no markers found" and "no data" messages should not be shown —
  ## table_or_text_UI should contain the table, not a text message
  ui_val <- retry_get_value(app, output = "marker_genes_table_or_text_UI")
  expect_false(grepl("no_markers_found|no_data", ui_val$html, fixed = FALSE))

  expect_mapequal(
    app$get_values(
      input = c(
        "marker_genes_selected_method",
        "marker_genes_selected_table",
        "marker_genes_table_filter_switch"
      )
    )$input,
    list(
      marker_genes_selected_method = "cerebro_seurat",
      marker_genes_selected_table = "seurat_clusters",
      marker_genes_table_filter_switch = FALSE
    )
  )
})


test_that("{shinytest2} recording: gene_expression", {
  app <- shared_app()

  activate_tab(app, "geneExpression")
  app$wait_for_idle(timeout = 10000)

  ## The renderer host is in the initial DOM so the first request does not wait
  ## for the nested projection controls to complete their binding round trips.
  app$wait_for_js(
    "!!document.getElementById('expression_projection_cell_view_host')",
    timeout = 10000
  )

  ## The gene selectize lives in a renderUI, so it is not bound the instant the
  ## tab goes idle. Wait for its element before set_inputs, or the input binding
  ## is "not found" and no gene is ever selected.
  wait_for_input(app, "expression_genes_input")

  ## select MS4A1 and verify the projection controls become available
  viewer_set_selectize(app, "expression_genes_input", "MS4A1")
  app$wait_for_idle(timeout = 15000)

  app$wait_for_js(
    paste0(
      "document.getElementById('expression_projection_genes_in_separate_panels')",
      "?.disabled === false && ",
      "document.querySelector('select[id^=\"expression_projection_gene_color_mode_\"]')",
      " === null"
    ),
    timeout = 10000
  )

  viewer_set_selectize(
    app,
    "expression_genes_input",
    c("MS4A1", "CD3D")
  )
  app$wait_for_js(
    paste0(
      "document.getElementById('expression_projection_genes_in_separate_panels')",
      "?.disabled === false"
    ),
    timeout = 10000
  )
  app$set_inputs(
    expression_projection_genes_in_separate_panels = "separate",
    wait_ = FALSE
  )
  app$wait_for_js(
    paste0(
      "document.querySelector('select[id^=\"expression_projection_gene_color_mode_\"]')",
      "?.disabled === false"
    ),
    timeout = 10000
  )
  enabled_style <- app$get_js(
    paste0(
      "(() => {const c=document.querySelector('.cerebro-gene-control');",
      "const i=c?.querySelector('.selectize-input');",
      "return c&&i?{disabled:c.classList.contains('is-disabled'),",
      "animation:getComputedStyle(i).animationName}:null;})()"
    )
  )
  expect_false(isTRUE(enabled_style$disabled))
  ## Controls now reveal immediately; the old staggered entrance was removed.
  expect_identical(enabled_style$animation, "none")
  panel_colour_id <- app$get_js(
    "document.querySelector('select[id^=\"expression_projection_gene_color_mode_\"]').id"
  )
  app$run_js(sprintf(
    "document.getElementById(%s).selectize.setValue('different');",
    jsonlite::toJSON(panel_colour_id, auto_unbox = TRUE)
  ))
  app$wait_for_js(
    paste0(
      "document.querySelectorAll(",
      "'#expression_projection_cell_view_host ",
      ".cv-pane:not(.cv-hidden)')",
      ".length === 2"
    ),
    timeout = 20000
  )
  app$wait_for_js(
    paste0(
      "(() => {",
      "const panes=Array.from(document.querySelectorAll(",
      "'#expression_projection_cell_view_host .cv-pane:not(.cv-hidden)'));",
      "const note=document.getElementById('cv-cbar')?.textContent || '';",
      "return panes.length===2 && ",
      "panes.every(p => p.querySelector('.cv-panel-scale')) && ",
      "panes.every(p => {const b=p.querySelector('.cv-focus-btn');",
      "return b && Math.abs(p.getBoundingClientRect().right-",
      "b.getBoundingClientRect().right)<=14;}) && ",
      "note.includes('Shared expression range');",
      "})()"
    ),
    timeout = 20000
  )

  viewer_set_selectize(app, "expression_genes_input", "MS4A1")
  app$wait_for_js(
    paste0(
      "(() => {",
      "const display=document.getElementById(",
      "'expression_projection_genes_in_separate_panels');",
      "const colour=document.querySelector(",
      "'select[id^=\"expression_projection_gene_color_mode_\"]');",
      "return !display?.disabled && display.value==='separate' && ",
      "colour===null;",
      "})()"
    ),
    timeout = 10000
  )

  ## shared Canvas projection renders after gene selection
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#expression_projection_cell_view_host canvas:not(.cv-mini)') !== null"
    ),
    timeout = 20000
  )
  proj_size <- app$get_js(
    paste0(
      "(function(){var e=document.querySelector(",
      "'#expression_projection_cell_view_host canvas:not(.cv-mini)');",
      "return {w:e.clientWidth,h:e.clientHeight};})()"
    )
  )
  expect_gte(as.numeric(proj_size$w), 300)
  expect_gte(as.numeric(proj_size$h), 240)

  ## verify expression levels have some non-zero values (cells with color)
  expr_levels <- retry_get_value(app, export = "expression_levels")
  expect_true(length(expr_levels) > 0)
  expect_true(any(unlist(expr_levels, use.names = FALSE) > 0))

  app$set_inputs(
    expression_projection_genes_in_separate_panels = "rgb",
    wait_ = FALSE
  )
  app$wait_for_js(
    "document.querySelectorAll('.cerebro-gene-rgb-channel').length === 3",
    timeout = 10000
  )
  rgb_animations <- app$get_js(
    paste0(
      "Array.from(document.querySelectorAll('.cerebro-gene-rgb-channel'))",
      ".map(e=>getComputedStyle(e).animationName)"
    )
  )
  expect_identical(
    unname(unlist(rgb_animations)),
    rep("none", 3)
  )
})

test_that("{shinytest2} recording: gene_id_conversion", {
  app <- shared_app()

  app$set_inputs(sidebar = "geneIdConversion")
  app$wait_for_idle(timeout = 10000)

  table_val <- retry_get_value(app, output = "gene_info")
  expect_false(is.null(table_val))
})

test_that("{shinytest2} recording: color_management", {
  app <- shared_app()

  app$set_inputs(sidebar = "color_management")
  app$wait_for_idle(timeout = 10000)

  ui_val <- retry_get_value(app, output = "color_assignments_UI")
  expect_false(is.null(ui_val))
})

test_that("{shinytest2} recording: about", {
  app <- shared_app()

  app$set_inputs(sidebar = "about")
  app$wait_for_idle(timeout = 10000)

  about_text <- retry_get_value(app, output = "about")
  expect_false(is.null(about_text))
  expect_true(nchar(about_text) > 0)
})

test_that("createShinyApp bundles a working app", {
  example <- system.file(
    "extdata/examples/example.crb",
    package = "CerebroNexus"
  )
  skip_if_not(nzchar(example), "example.crb not found")

  tmp <- file.path(tempdir(), "demo.crb")
  app_dir <- file.path(tempdir(), "test_create_app")
  file.copy(example, tmp, overwrite = TRUE)
  on.exit(unlink(app_dir, recursive = TRUE), add = TRUE)

  createShinyApp(
    cerebro_data = c("mydata" = tmp),
    result_dir = app_dir,
    launch_browser = FALSE,
    ## The bundle freezes its port; parallel test files must not share one.
    port = httpuv::randomPort(),
    verbose = FALSE
  )

  # Freshly bundled createShinyApp app loads demo.crb at startup, so it is the
  # heaviest to initialise; the default 15s load_timeout is too tight on slow CI
  # runners. Give it 30s (other tabs use pre-built inst apps and idle faster).
  app <- AppDriver$new(
    app_dir,
    height = 950,
    width = 1619,
    load_timeout = 30000
  )
  app$wait_for_idle(timeout = 20000)

  cells <- retry_get_value(app, output = "load_data_overview")
  expect_true(grepl("1,?000", cells$html))

  app$stop()
})
