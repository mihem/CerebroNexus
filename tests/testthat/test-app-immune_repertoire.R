# test-app-immune_repertoire.R — shinytest2 integration tests for immune repertoire module
#
# The example dataset now ships with real TCR data, so the immune repertoire
# tab is present by default and its UI can be exercised directly.

library(shinytest2)

inst_candidates <- c(
  normalizePath("inst", mustWork = FALSE),
  normalizePath("../../inst", mustWork = FALSE),
  normalizePath(testthat::test_path("../../inst"), mustWork = FALSE)
)
inst_dir <- inst_candidates[file.exists(file.path(inst_candidates, "app.R"))][1]
if (is.na(inst_dir)) {
  inst_dir <- system.file(package = "CerebroNexus")
}
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

ir_sidebar_selector <- 'a[href="#shiny-tab-immune_repertoire"]'
ir_canvas_selector <- paste0(
  "#ir_clonalUMAP_projection_cell_view_host ",
  ".cv-canvas-wrap > canvas:not(.cv-mini)"
)

wait_for_ir_sidebar <- function(app, timeout = 60000) {
  app$wait_for_js(
    sprintf("document.querySelector('%s') !== null", ir_sidebar_selector),
    timeout = timeout
  )
}

wait_for_ir_canvas <- function(app, timeout = 60000) {
  app$wait_for_js(
    sprintf("document.querySelector('%s') !== null", ir_canvas_selector),
    timeout = timeout
  )
}

open_ir_tab <- function(app, timeout = 60000) {
  wait_for_ir_sidebar(app, timeout = timeout)
  app$run_js(
    sprintf("document.querySelector('%s').click();", ir_sidebar_selector)
  )
  app$wait_for_js(
    "document.querySelector('#ir_tabs.shiny-bound-input') !== null",
    timeout = timeout
  )
}

activate_ir_tab <- function(app, timeout = 60000) {
  open_ir_tab(app, timeout = timeout)
  ## Every caller below assumes it starts on the default Clonal UMAP plot tab.
  ## On a freshly booted app that is already true, but on the shared driver a
  ## previous test may have left another plot tab active, so put the tabset back
  ## explicitly once it is bound. Selecting the tab it is already on is a no-op.
  app$set_inputs(ir_tabs = "Clonal UMAP", wait_ = FALSE)
  app$wait_for_js(
    paste0(
      "(function(){",
      "var tabs=document.querySelector('#ir_tabs.shiny-bound-input');",
      "var active=document.querySelector(",
      "'#ir_tabs li.active a[data-value=\"Clonal UMAP\"]');",
      "var receptor=document.querySelector('#ir_p_umap_receptor');",
      "return !!tabs && !!active && !!receptor && receptor.options.length>0;",
      "})()"
    ),
    timeout = timeout
  )
}

## The tests in this file each booted their own app, and booting dominates their
## runtime. The ones that only navigate the plot tabset and read the DOM now
## share a single driver, created on first use and stopped when the file ends.
## Ten of the sixteen tests use the shared driver; the other six keep their
## own boot (below). Measured locally this takes the file from ~170s to ~120s.
##
## Sharing is deliberately NOT applied to a test that would be weakened by a
## reused app. Keep giving these their own AppDriver:
##   * the lazy-loading boundary test — it asserts what a PRISTINE app has not
##     loaded, and the shared driver has scRepertoire warm from other tests;
##   * tests that assert INITIAL state (the active landing tab, "Group by"
##     empty, Clonal UMAP ungrouped) — a reused app carries prior selections;
##   * the info-dialog test — it leaves a modal open over the page;
##   * the "does not break the main app" test — it reads the Data info tab of an
##     app that has never opened the repertoire page.
shared_driver <- NULL
shared_driver_env <- environment()
shared_app <- function() {
  if (is.null(shared_driver)) {
    local_app_support(inst_dir, envir = shared_driver_env)
    shared_driver <<- AppDriver$new(
      inst_dir,
      name = "ir_shared",
      height = 950,
      width = 1619,
      load_timeout = 60000
    )
    withr::defer(shared_driver$stop(), envir = shared_driver_env)
  }
  shared_driver
}

test_that("first IR plot tab is Clonal UMAP", {
  # The default/landing tab should be the Clonal UMAP overview, so the first
  # thing shown is where expanded clones sit on the cell projection.
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_default_tab",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  open_ir_tab(app)

  active_tab <- app$get_js(
    paste0(
      "(function(){",
      "var active=document.querySelector('#ir_tabs li.active a');",
      "return active?active.getAttribute('data-value'):null;",
      "})()"
    )
  )
  expect_identical(active_tab, "Clonal UMAP")
})

test_that("immune_repertoire tab is present with example data (has TCR)", {
  app <- shared_app()
  activate_ir_tab(app)

  # example.crb carries real TCR data — the conditional tab should appear
  tab_present <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\') !== null;'
  )
  expect_true(tab_present)
})

test_that("Group by is visible on plots whose grouping it drives", {
  # Group by is a real scRepertoire parameter for Scatter, and the custom BCR
  # Isotype/SHM Proxy renderers also use it as their grouping column. It should
  # only be hidden on Paired Scatter, where comparison is controlled by the
  # paired sample metadata selectors.
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_groupby_scope",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  # Global controls are now rendered server-side per tab (no conditionalPanel),
  # so visibility = the control element exists and is laid out.
  groupby_visible <- function() {
    app$wait_for_js(
      "(function(){var e=document.querySelector('#ir_groupBy');return !!e && e.offsetParent!==null;})()",
      timeout = 45000
    )
    TRUE
  }
  n_options <- function(id) {
    app$get_js(sprintf(
      "(function(){var e=document.querySelector('#%s');return e?e.querySelectorAll('option').length:0;})();",
      id
    ))
  }

  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "Isotype", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "Paired Scatter", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_true(isTRUE(groupby_visible()))
  expect_equal(
    app$get_js(
      "(function(){var e=document.querySelector('#ir_groupBy');return e?e.value:null;})();"
    ),
    ""
  )
  app$wait_for_js(
    "(function(){var x=document.querySelector('#ir_pair_x_group'),y=document.querySelector('#ir_pair_y_group');return !!x && !!y && x.querySelectorAll('option').length>=2 && y.querySelectorAll('option').length>=2;})()",
    timeout = 45000
  )
  expect_true(isTRUE(app$get_js(
    "(function(){return document.querySelector('#ir_pair_x_group') !== null && document.querySelector('#ir_pair_y_group') !== null;})();"
  )))
  expect_gte(as.numeric(n_options("ir_pair_x_group")), 2)
  expect_gte(as.numeric(n_options("ir_pair_y_group")), 2)
  expect_true(isTRUE(app$get_js(
    "(function(){return document.querySelector('#ir_groupBy option[value=\"cell_type\"]') !== null;})();"
  )))
  app$set_inputs(ir_groupBy = "cell_type", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_equal(
    app$get_js(
      "(function(){var e=document.querySelector('#ir_groupBy');return e?e.value:null;})();"
    ),
    "cell_type"
  )
  app$wait_for_js(
    "(function(){var x=document.querySelector('#ir_pair_x_group'),y=document.querySelector('#ir_pair_y_group');return !!x && !!y && x.querySelectorAll('option').length>=2 && y.querySelectorAll('option').length>=2;})()",
    timeout = 45000
  )
  expect_gte(as.numeric(n_options("ir_pair_x_group")), 2)
  expect_gte(as.numeric(n_options("ir_pair_y_group")), 2)
})

test_that("Chain is visible on plots whose scRepertoire API accepts it", {
  app <- shared_app()
  activate_ir_tab(app)

  chain_visible <- function() {
    app$wait_for_js(
      "(function(){var e=document.querySelector('#ir_chain');return !!e && e.offsetParent!==null;})()",
      timeout = 45000
    )
    TRUE
  }

  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_true(isTRUE(chain_visible()))

  app$set_inputs(ir_tabs = "SizeDist", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_true(isTRUE(chain_visible()))
})

test_that("changing 'Group by' keeps the current plot tab", {
  # Changing the grouping must not reset the visualization tabset back to the
  # first tab (Abundance).
  app <- shared_app()
  activate_ir_tab(app)

  active_tab <- function() {
    app$get_js(
      "(function(){var a=document.querySelector('#ir_tabs li.active a');return a?a.textContent.trim():'';})();"
    )
  }

  # Move off the default (Abundance) tab.
  app$set_inputs(ir_tabs = "Diversity", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_identical(active_tab(), "Diversity")

  # Establish the transition's starting state explicitly: a reused driver may
  # otherwise already carry cell_type from an earlier test.
  app$wait_for_js(
    "document.querySelector('#ir_groupBy') !== null",
    timeout = 20000
  )
  app$set_inputs(ir_groupBy = "", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_js(
    "(function(){var e=document.querySelector('#ir_groupBy');return !!e && e.value === '';})()",
    timeout = 20000
  )

  # change grouping; the tab should stay on Diversity, not reset to Abundance
  app$set_inputs(ir_groupBy = "cell_type", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)
  expect_identical(active_tab(), "Diversity")
})

test_that("settings dropdowns render all their options (not just selected)", {
  # selectize widgets rendered inside hidden conditionalPanels / dynamic UI drop
  # all but the selected <option>. We use selectize = FALSE so users can
  # actually choose other values. Assert the real <option> counts here — note
  # set_inputs() bypasses the DOM, so it cannot catch this regression.
  app <- shared_app()
  activate_ir_tab(app)

  n_options <- function(id) {
    app$wait_for_js(
      sprintf(
        "(function(){var e=document.querySelector('#%s');return !!e && e.querySelectorAll('option').length>0;})()",
        id
      ),
      timeout = 45000
    )
    app$get_js(sprintf(
      "(function(){var e=document.querySelector('#%s');return e?e.querySelectorAll('option').length:0;})();",
      id
    ))
  }

  # The global controls (chain / group-by) are hidden on the default Clonal UMAP
  # tab (which uses its own Receptor selector), so move to a tab that shows them.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)

  # Group by: None + grouping variables (sample, seurat_clusters, cell_type)
  expect_gte(as.numeric(n_options("ir_groupBy")), 2)
  # Chain: both + detected chains (TRA/TRB/IGH/IGK/IGL) > 1
  expect_gte(as.numeric(n_options("ir_chain")), 2)
  # Clone call: gene/nt/aa/strict
  expect_gte(as.numeric(n_options("ir_cloneCall")), 2)
})

test_that("immune_repertoire tab can be opened and renders settings", {
  app <- shared_app()
  activate_ir_tab(app)

  # Chain is hidden on the default Clonal UMAP tab; move to one that shows it.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  app$wait_for_js(
    'document.querySelector("#ir_chain") !== null',
    timeout = 45000
  )

  # the chain selector (a core settings control) should be populated
  chain_present <- app$get_js(
    'document.querySelector("#ir_chain") !== null;'
  )
  expect_true(chain_present)
})

test_that("immune_repertoire module loads without breaking main app", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_load",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 20000)

  # Data info tab should still render normally (1476 cells in the new example)
  cells_box <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("1,?476", cells_box$html))
})

test_that("Clonal UMAP tab renders with receptor + projection selectors", {
  app <- shared_app()
  activate_ir_tab(app)

  # The Clonal UMAP tab should exist among the visualization tabs.
  has_umap_tab <- app$get_js(
    "(function(){
      var as = document.querySelectorAll('#ir_tabs > li > a');
      for (var i=0;i<as.length;i++){
        if (as[i].textContent.trim() === 'Clonal UMAP') return true;
      }
      return false;
    })();"
  )
  expect_true(isTRUE(has_umap_tab))

  # Switch to it; the receptor + projection selectors should render with options.
  app$set_inputs(ir_tabs = "Clonal UMAP", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  n_options <- function(id) {
    app$wait_for_js(
      sprintf(
        "(function(){var e=document.querySelector('#%s');return !!e && e.querySelectorAll('option').length>0;})()",
        id
      ),
      timeout = 45000
    )
    app$get_js(sprintf(
      "(function(){var e=document.querySelector('#%s');return e?e.querySelectorAll('option').length:0;})();",
      id
    ))
  }
  expect_gte(as.numeric(n_options("ir_p_umap_receptor")), 1)
  expect_gte(as.numeric(n_options("ir_p_umap_projection")), 1)

  # The non-faceted Clonal UMAP renders through the shared Canvas engine.
  wait_for_ir_canvas(app)
  has_canvas <- app$get_js(
    sprintf("document.querySelector('%s') !== null;", ir_canvas_selector)
  )
  expect_true(isTRUE(has_canvas))
  app$wait_for_js(
    paste0(
      "(function(){",
      "var c=document.querySelector('",
      ir_canvas_selector,
      "');",
      "if(!c||!c.width||!c.height)return false;",
      "var p=c.getContext('2d').getImageData(0,0,c.width,c.height).data;",
      "for(var i=3;i<p.length;i+=4){if(p[i])return true;}",
      "return false;",
      "})()"
    ),
    timeout = 20000
  )
})

test_that("Display options panel exposes scatter params on scatter-type tabs", {
  app <- shared_app()
  activate_ir_tab(app)

  control_exists <- function(id) {
    app$get_js(sprintf(
      "document.querySelector('#%s') !== null;",
      id
    ))
  }

  # Abundance (non-scatter): IR-only font controls and scatter params are absent.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  app$wait_for_js(
    "document.querySelector('#ir_more_analysis_UI') !== null && document.querySelector('#ir_d_base_size') === null && document.querySelector('#ir_d_legend_size') === null && document.querySelector('#ir_d_legend_pos') === null && document.querySelector('#ir_d_legend_key') === null && document.querySelector('#ir_d_point_size') === null",
    timeout = 45000
  )
  expect_false(isTRUE(control_exists("ir_d_base_size")))
  expect_false(isTRUE(control_exists("ir_d_legend_size")))
  expect_false(isTRUE(control_exists("ir_d_legend_pos")))
  expect_false(isTRUE(control_exists("ir_d_legend_key")))
  expect_false(isTRUE(control_exists("ir_d_point_size")))

  # Clonal UMAP (scatter-type): point size + opacity also present.
  app$set_inputs(ir_tabs = "Clonal UMAP", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  app$wait_for_js(
    paste0(
      "document.querySelector('#ir_d_point_size') !== null && ",
      "document.querySelector('#ir_d_alpha') !== null && ",
      "document.querySelector('#ir_d_percentage_cells_to_show') !== null"
    ),
    timeout = 45000
  )
  expect_true(isTRUE(control_exists("ir_d_point_size")))
  expect_true(isTRUE(control_exists("ir_d_alpha")))
  expect_true(isTRUE(control_exists("ir_d_percentage_cells_to_show")))
  expect_identical(
    as.numeric(app$get_js(
      "Number(document.querySelector('#ir_d_point_size').value)"
    )),
    6
  )
  expect_identical(
    as.numeric(app$get_js(
      "Number(document.querySelector('#ir_d_alpha').value)"
    )),
    1
  )
  expect_identical(
    as.numeric(app$get_js(
      "Number(document.querySelector('#ir_d_percentage_cells_to_show').value)"
    )),
    100
  )
})

test_that("Linked views shows the dataset point size in More settings", {
  app <- shared_app()
  app$run_js(
    "document.querySelector('a[href=\"#shiny-tab-coordinated_views\"]').click();"
  )
  app$wait_for_js(
    "document.querySelector('#cv-more-btn') !== null",
    timeout = 45000
  )
  app$run_js("document.querySelector('#cv-more-btn').click();")
  app$wait_for_js(
    paste0(
      "document.querySelector('#cv-ps') !== null && ",
      "Number(document.querySelector('#cv-ps').value) === 6 && ",
      "Number(document.querySelector('#cv-opacity').value) === 1"
    ),
    timeout = 45000
  )
  expect_identical(
    as.numeric(app$get_js("Number(document.querySelector('#cv-ps').value)")),
    6
  )
  expect_identical(
    as.numeric(app$get_js(
      "Number(document.querySelector('#cv-opacity').value)"
    )),
    1
  )
})

test_that("IR page uses the compact top toolbar and settings drawer", {
  app <- shared_app()
  activate_ir_tab(app)

  # The page guide stays in the header; secondary controls remain in the drawer.
  exists_el <- function(sel) {
    app$get_js(sprintf("document.querySelector('%s') !== null;", sel))
  }
  expect_true(isTRUE(exists_el("#ir_visualizations_info")))
  expect_true(isTRUE(exists_el("#ir_more_button")))
  expect_true(isTRUE(exists_el("#ir_additional_parameters_info")))
  expect_true(isTRUE(exists_el("#ir_group_filters_info")))
  expect_true(isTRUE(exists_el("#ir_tabs")))
})

test_that("Clonal UMAP has Show-all toggle and group filters", {
  app <- shared_app()
  activate_ir_tab(app)

  exists_el <- function(sel) {
    app$get_js(sprintf("document.querySelector('%s') !== null;", sel))
  }

  # Default tab is Clonal UMAP: the Show-all checkbox should exist, and at least
  # one per-group filter picker (e.g. ir_group_filter_sample) should render.
  expect_true(isTRUE(exists_el("#ir_p_umap_show_all")))
  has_group_filter <- app$get_js(
    "document.querySelector('[id^=\"ir_group_filter_\"]') !== null;"
  )
  expect_true(isTRUE(has_group_filter))

  # The non-faceted host renders through the shared Canvas engine.
  wait_for_ir_canvas(app)
  has_canvas <- app$get_js(
    sprintf("document.querySelector('%s') !== null;", ir_canvas_selector)
  )
  expect_true(isTRUE(has_canvas))
})

test_that("Clonal UMAP switches to static facets only when grouped", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_umap_grouped_static",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  exists_el <- function(sel) {
    app$get_js(sprintf("document.querySelector('%s') !== null;", sel))
  }

  # Ungrouped uses the shared Canvas host; grouping swaps in the static facet.
  app$wait_for_js(
    paste0(
      "(function(){",
      "var group=document.querySelector('#ir_p_umap_group_by');",
      "var canvas=document.querySelector('",
      ir_canvas_selector,
      "');",
      "var img=document.querySelector('#ir_plot_clonalUMAP_static img');",
      "return !!group && group.value==='' && !!canvas && !img;",
      "})()"
    ),
    timeout = 60000
  )

  app$set_inputs(ir_p_umap_group_by = "sample", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)
  app$wait_for_js(
    paste0(
      "(function(){",
      "var canvas=document.querySelector('",
      ir_canvas_selector,
      "');",
      "var group=document.querySelector('#ir_p_umap_group_by');",
      "var img=document.querySelector('#ir_plot_clonalUMAP_static img');",
      "return !!group && group.value==='sample' && canvas===null && ",
      "!!img && img.complete && img.naturalWidth>0;",
      "})()"
    ),
    timeout = 45000
  )

  expect_false(isTRUE(exists_el(
    ir_canvas_selector
  )))
  expect_true(isTRUE(exists_el("#ir_plot_clonalUMAP_static img")))
  static_size <- app$get_js(
    paste0(
      "(function(){",
      "var e=document.querySelector('#ir_plot_clonalUMAP_static');",
      "var img=e?e.querySelector('img'):null;",
      "return e?{",
      "w:e.clientWidth,h:e.clientHeight,",
      "imgW:img?img.naturalWidth:0,imgH:img?img.naturalHeight:0",
      "}:null;",
      "})();"
    )
  )
  expect_gte(as.numeric(static_size$w), 300)
  expect_gte(as.numeric(static_size$h), 300)
  expect_gte(as.numeric(static_size$imgW), as.numeric(static_size$w) * 0.9)
  expect_gte(as.numeric(static_size$imgH), 300)

  app$set_inputs(ir_p_umap_group_by = "", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  expect_true(isTRUE(exists_el(ir_canvas_selector)))
  expect_false(isTRUE(exists_el("#ir_plot_clonalUMAP_static img")))
  logs <- app$get_logs()
  bad_logs <- grepl(
    "figure margins too large|Invalid IHDR|could not write",
    logs$message
  )
  expect_false(
    any(bad_logs),
    info = paste(
      capture.output(print(logs[bad_logs, , drop = FALSE])),
      collapse = "\n"
    )
  )
})

test_that("Clone call is hidden on the Clonal UMAP tab", {
  app <- shared_app()
  activate_ir_tab(app)

  exists_el <- function(sel) {
    app$get_js(sprintf("document.querySelector('%s') !== null;", sel))
  }

  # Default tab is Clonal UMAP: the global Clone call should be omitted there.
  expect_false(isTRUE(exists_el("#ir_cloneCall")))

  # On Abundance it should be back.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  app$wait_for_js(
    "document.querySelector('#ir_cloneCall') !== null",
    timeout = 45000
  )
  expect_true(isTRUE(exists_el("#ir_cloneCall")))
})

test_that("page info button opens the repertoire guide", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_info_dialog",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  # Move to a tab with several controls, then open the consolidated page guide.
  app$set_inputs(ir_tabs = "Diversity", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  app$run_js("document.querySelector('#ir_visualizations_info').click();")
  app$wait_for_js(
    "(function(){var m=document.querySelector('.modal-body');return !!m && /ir-guide/.test(m.innerHTML);})()",
    timeout = 45000
  )

  # A modal with help cards should appear, containing the param help text.
  modal_html <- app$get_js(
    "(function(){var m=document.querySelector('.modal-body');return m?m.innerHTML:'';})();"
  )
  expect_true(grepl("ir-guide", modal_html))
  expect_true(grepl("Clonal UMAP|Abundance|Diversity", modal_html))
})

test_that("lazy-load boundary: self-made plots stay unloaded, scRepertoire plots load + render", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_lazy_boundary",
    height = 950,
    width = 1619
  )
  withr::defer(app$stop())
  app$wait_for_idle(timeout = 60000)

  ## Startup: nothing repertoire-related has rendered, so scRepertoire is unloaded.
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)

  ## Landing on the IR tab shows the default "Clonal UMAP" — a self-made plot —
  ## which must not drag in scRepertoire.
  activate_ir_tab(app)
  app$wait_for_idle(timeout = 45000)
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)
  ## Clonal UMAP has no standalone help renderer. Do not offer a button that
  ## opens a modal containing only "No example available".
  app$wait_for_js(
    'document.querySelector("#ir_help_panel") !== null',
    timeout = 45000
  )
  expect_true(isTRUE(app$get_js(
    'document.querySelector("#ir_help_example_btn") === null;'
  )))

  ## Clone Sharing is self-made (ir_build_sharing_plot, no scRepertoire:: call).
  ## Opening it must NOT load scRepertoire — this is the over-broad-gate
  ## regression: previously every renderer forced loadNamespace().
  app$set_inputs(ir_tabs = "Clone Sharing", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)

  ## The same boundary must hold for the help example, not only the live plot.
  app$wait_for_js(
    'document.querySelector("#ir_help_example_btn") !== null',
    timeout = 45000
  )
  app$run_js('document.querySelector("#ir_help_example_btn").click();')
  app$wait_for_js(
    'document.querySelector(".modal-body #ir_demo_plot img") !== null',
    timeout = 45000
  )
  expect_false(is.null(app$get_value(output = "ir_demo_plot")))
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)
  Sys.sleep(1.5)
  app$wait_for_idle(timeout = 10000)
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)
  app$run_js('document.querySelector(".modal-footer button").click();')
  app$wait_for_js(
    'document.querySelector(".modal.in, .modal.show") === null',
    timeout = 10000
  )

  ## Abundance IS scRepertoire-backed — it loads the namespace at that boundary
  ## and renders a plot.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 60000)
  expect_identical(app$get_value(export = "scRepertoire_loaded"), TRUE)
  expect_false(is.null(app$get_value(output = "ir_plot_clonalAbundance")))

  ## A second scRepertoire-backed plot also renders (namespace already warm).
  app$set_inputs(ir_tabs = "Homeostasis", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  expect_identical(app$get_value(export = "scRepertoire_loaded"), TRUE)
  expect_false(is.null(app$get_value(output = "ir_plot_clonalHomeostasis")))
})
