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

wait_for_ir_sidebar <- function(app, timeout = 60000) {
  app$wait_for_js(
    sprintf("document.querySelector('%s') !== null", ir_sidebar_selector),
    timeout = timeout
  )
}

activate_ir_tab <- function(app, timeout = 60000) {
  wait_for_ir_sidebar(app, timeout = timeout)
  app$run_js(
    sprintf("document.querySelector('%s').click();", ir_sidebar_selector)
  )
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

test_that("immune_repertoire tab is present with example data (has TCR)", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_present",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  wait_for_ir_sidebar(app)

  # example.crb carries real TCR data — the conditional tab should appear
  tab_present <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\') !== null;'
  )
  expect_true(tab_present)
})

test_that("first IR plot tab is Clonal UMAP", {
  # The default/landing tab should be the Clonal UMAP overview, so the first
  # thing shown is where expanded clones sit on the cell projection.
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_first_tab",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  first_tab <- app$get_js(
    "document.querySelector('#ir_tabs > li > a').textContent.trim();"
  )
  expect_identical(first_tab, "Clonal UMAP")
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
      timeout = 15000
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
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "Isotype", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "Paired Scatter", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(groupby_visible()))
  expect_equal(
    app$get_js(
      "(function(){var e=document.querySelector('#ir_groupBy');return e?e.value:null;})();"
    ),
    ""
  )
  app$wait_for_js(
    "(function(){var x=document.querySelector('#ir_pair_x_group'),y=document.querySelector('#ir_pair_y_group');return !!x && !!y && x.querySelectorAll('option').length>=2 && y.querySelectorAll('option').length>=2;})()",
    timeout = 15000
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
  app$wait_for_idle(timeout = 15000)
  expect_equal(
    app$get_js(
      "(function(){var e=document.querySelector('#ir_groupBy');return e?e.value:null;})();"
    ),
    "cell_type"
  )
  app$wait_for_js(
    "(function(){var x=document.querySelector('#ir_pair_x_group'),y=document.querySelector('#ir_pair_y_group');return !!x && !!y && x.querySelectorAll('option').length>=2 && y.querySelectorAll('option').length>=2;})()",
    timeout = 15000
  )
  expect_gte(as.numeric(n_options("ir_pair_x_group")), 2)
  expect_gte(as.numeric(n_options("ir_pair_y_group")), 2)
})

test_that("Chain is visible on plots whose scRepertoire API accepts it", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_chain_scope",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  chain_visible <- function() {
    app$wait_for_js(
      "(function(){var e=document.querySelector('#ir_chain');return !!e && e.offsetParent!==null;})()",
      timeout = 15000
    )
    TRUE
  }

  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(chain_visible()))

  app$set_inputs(ir_tabs = "SizeDist", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(chain_visible()))
})

test_that("changing 'Group by' keeps the current plot tab", {
  # Changing the grouping must not reset the visualization tabset back to the
  # first tab (Abundance).
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_group_keep_tab",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  active_tab <- function() {
    app$get_js(
      "(function(){var a=document.querySelector('#ir_tabs li.active a');return a?a.textContent.trim():'';})();"
    )
  }

  # move off the default (Abundance) tab
  app$set_inputs(ir_tabs = "Diversity", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_identical(active_tab(), "Diversity")

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
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_dropdown_options",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  n_options <- function(id) {
    app$wait_for_js(
      sprintf(
        "(function(){var e=document.querySelector('#%s');return !!e && e.querySelectorAll('option').length>0;})()",
        id
      ),
      timeout = 15000
    )
    app$get_js(sprintf(
      "(function(){var e=document.querySelector('#%s');return e?e.querySelectorAll('option').length:0;})();",
      id
    ))
  }

  # The global controls (chain / group-by) are hidden on the default Clonal UMAP
  # tab (which uses its own Receptor selector), so move to a tab that shows them.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)

  # Group by: None + grouping variables (sample, seurat_clusters, cell_type)
  expect_gte(as.numeric(n_options("ir_groupBy")), 2)
  # Chain: both + detected chains (TRA/TRB/IGH/IGK/IGL) > 1
  expect_gte(as.numeric(n_options("ir_chain")), 2)
  # Clone call: gene/nt/aa/strict
  expect_gte(as.numeric(n_options("ir_cloneCall")), 2)
})

test_that("immune_repertoire tab can be opened and renders settings", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_open",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  # Chain is hidden on the default Clonal UMAP tab; move to one that shows it.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  app$wait_for_js(
    'document.querySelector("#ir_chain") !== null',
    timeout = 15000
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
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_clonal_umap",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
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
      timeout = 15000
    )
    app$get_js(sprintf(
      "(function(){var e=document.querySelector('#%s');return e?e.querySelectorAll('option').length:0;})();",
      id
    ))
  }
  expect_gte(as.numeric(n_options("ir_p_umap_receptor")), 1)
  expect_gte(as.numeric(n_options("ir_p_umap_projection")), 1)

  # The interactive plotly UMAP should render a plotly canvas (not an R error).
  # Non-faceted Clonal UMAP now renders through the shared projection engine, so
  # the plotly host is #ir_clonalUMAP_projection (not the old #ir_plot_clonalUMAP).
  app$wait_for_js(
    "document.querySelector('#ir_clonalUMAP_projection .plotly') !== null",
    timeout = 20000
  )
  has_plotly <- app$get_js(
    "document.querySelector('#ir_clonalUMAP_projection .plotly') !== null;"
  )
  expect_true(isTRUE(has_plotly))
})

test_that("Display options panel exposes scatter params on scatter-type tabs", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_display_opts",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  control_exists <- function(id) {
    app$get_js(sprintf(
      "document.querySelector('#%s') !== null;",
      id
    ))
  }

  # Abundance (non-scatter): base display params present, scatter ones absent.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  app$wait_for_js(
    "document.querySelector('#ir_d_base_size') !== null && document.querySelector('#ir_d_point_size') === null",
    timeout = 15000
  )
  expect_true(isTRUE(control_exists("ir_d_base_size")))
  expect_false(isTRUE(control_exists("ir_d_point_size")))

  # Clonal UMAP (scatter-type): point size + opacity also present.
  app$set_inputs(ir_tabs = "Clonal UMAP", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  app$wait_for_js(
    "document.querySelector('#ir_d_point_size') !== null && document.querySelector('#ir_d_alpha') !== null",
    timeout = 15000
  )
  expect_true(isTRUE(control_exists("ir_d_point_size")))
  expect_true(isTRUE(control_exists("ir_d_alpha")))
})

test_that("IR page uses the Main-tab layout (Main/Additional/Group boxes)", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_layout",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  # The three left-column parameter boxes (by their info buttons) and the
  # right-column visualization tab strip should all be present.
  exists_el <- function(sel) {
    app$get_js(sprintf("document.querySelector('%s') !== null;", sel))
  }
  expect_true(isTRUE(exists_el("#ir_main_parameters_info")))
  expect_true(isTRUE(exists_el("#ir_additional_parameters_info")))
  expect_true(isTRUE(exists_el("#ir_group_filters_info")))
  expect_true(isTRUE(exists_el("#ir_tabs")))
})

test_that("Clonal UMAP has Show-all toggle and group filters", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_umap_filters",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
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

  # The interactive plotly UMAP should render a plotly canvas (not an R error).
  # Non-faceted host is the shared projection engine's #ir_clonalUMAP_projection.
  app$wait_for_js(
    "document.querySelector('#ir_clonalUMAP_projection .plotly') !== null",
    timeout = 20000
  )
  has_plotly <- app$get_js(
    "document.querySelector('#ir_clonalUMAP_projection .plotly') !== null;"
  )
  expect_true(isTRUE(has_plotly))
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

  # Ungrouped: the non-faceted host renders through the shared projection engine
  # (#ir_clonalUMAP_projection); grouping swaps in the static faceted ggplot.
  app$wait_for_js(
    "document.querySelector('#ir_clonalUMAP_projection .plotly') !== null",
    timeout = 20000
  )
  expect_true(isTRUE(exists_el("#ir_p_umap_group_by")))
  expect_true(isTRUE(exists_el("#ir_clonalUMAP_projection .plotly")))
  expect_false(isTRUE(exists_el("#ir_plot_clonalUMAP_static img")))

  app$set_inputs(ir_p_umap_group_by = "sample", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)

  expect_false(isTRUE(exists_el("#ir_clonalUMAP_projection .plotly")))
  expect_true(isTRUE(exists_el("#ir_plot_clonalUMAP_static img")))
  plot_value <- app$get_value(output = "ir_plot_clonalUMAP_static")
  panel_rows <- vapply(
    plot_value$coordmap$panels,
    function(panel) panel$row,
    integer(1)
  )
  expect_identical(unique(panel_rows), 1L)
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
})

test_that("Clone call is hidden on the Clonal UMAP tab", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_umap_no_clonecall",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  exists_el <- function(sel) {
    app$get_js(sprintf("document.querySelector('%s') !== null;", sel))
  }

  # Default tab is Clonal UMAP: the global Clone call should be omitted there.
  expect_false(isTRUE(exists_el("#ir_cloneCall")))

  # On Abundance it should be back.
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  app$wait_for_js(
    "document.querySelector('#ir_cloneCall') !== null",
    timeout = 15000
  )
  expect_true(isTRUE(exists_el("#ir_cloneCall")))
})

test_that("Main parameters info button opens a help dialog", {
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

  # Move to a tab with several controls, then click the Main parameters info.
  app$set_inputs(ir_tabs = "Diversity", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  app$run_js("document.querySelector('#ir_main_parameters_info').click();")
  app$wait_for_idle(timeout = 10000)
  app$wait_for_js(
    "(function(){var m=document.querySelector('.modal-body');return !!m && /ir-help-card/.test(m.innerHTML);})()",
    timeout = 15000
  )

  # A modal with help cards should appear, containing the param help text.
  modal_html <- app$get_js(
    "(function(){var m=document.querySelector('.modal-body');return m?m.innerHTML:'';})();"
  )
  expect_true(grepl("ir-help-card", modal_html))
  expect_true(grepl("Metric|Clone call|Bootstrap", modal_html))
})
