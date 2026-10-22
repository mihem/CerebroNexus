# test-app-immune_repertoire.R — shinytest2 integration tests for immune repertoire module
#
# The example dataset now ships with real TCR data, so the immune repertoire
# tab is present by default and its UI can be exercised directly.

skip_if_not_installed("shinytest2")
library(shinytest2)

inst_dir <- viewer_app_test_path()

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
## runtime. The ones that only navigate the plot tabset and read the DOM share a
## single driver, created on first use and stopped when the file ends.
##
## Sharing is deliberately NOT applied to a test that would be weakened by a
## reused app. These keep their own AppDriver:
##   * the lazy-loading boundary test — it asserts what a PRISTINE app has not
##     loaded, and the shared driver has scRepertoire warm from other tests.
##     The other checks that need an app which has never opened the repertoire
##     page run on its boot too: Data info still renders, and the landing tab
##     is Clonal UMAP;
##   * tests that assert INITIAL state ("Group by" empty, Clonal UMAP
##     ungrouped) — a reused app carries prior selections;
##   * the info-dialog test — it leaves a modal open over the page;
##   * the grouped Share round trip — it relies on an app that has never opened
##     Linked views, and it leaves the projection grouped.
shared_app <- viewer_shared_app("ir_shared", inst_dir)

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
  # Group by is a real scRepertoire parameter, and the custom BCR Isotype
  # renderer also uses it as its grouping column. It should
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

test_that("Clonal UMAP tab renders with receptor + projection selectors", {
  app <- shared_app()
  activate_ir_tab(app)

  # The Clonal UMAP tab should exist among the visualization tabs.
  has_umap_tab <- app$get_js(
    "(function(){
      var as = document.querySelectorAll('#ir_tabs > li > a');
      for (var i=0;i<as.length;i++){
        if (as[i].textContent.trim() === 'Clonal projection') return true;
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

  # Drawer controls are deliberately absent from the cold landing path. The
  # first open requests and mounts them.
  app$click(selector = "#ir_more_button")
  app$wait_for_js(
    "document.querySelector('#ir_d_point_size') !== null",
    timeout = 45000
  )

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
  if (!isTRUE(exists_el("#ir_additional_parameters_info"))) {
    app$click(selector = "#ir_more_button")
  }
  app$wait_for_js(
    paste0(
      "document.querySelector('#ir_additional_parameters_info') !== null && ",
      "document.querySelector('#ir_group_filters_info') !== null"
    ),
    timeout = 45000
  )
  expect_true(isTRUE(exists_el("#ir_additional_parameters_info")))
  expect_true(isTRUE(exists_el("#ir_group_filters_info")))
  expect_true(isTRUE(exists_el("#ir_tabs")))
  toolbar_geometry <- app$get_js(paste0(
    "(() => {",
    "const toolbar=document.querySelector('#shiny-tab-immune_repertoire ",
    ".cerebro-viz-toolbar').getBoundingClientRect();",
    "const control=document.querySelector('#ir_main_params_UI ",
    ".form-group').getBoundingClientRect();",
    "const settings=document.getElementById('ir_more_button').getBoundingClientRect();",
    "const status=document.querySelector('#ir_selection_status_UI ",
    ".cerebro-selection-status-slot').getBoundingClientRect();",
    "return {sameRow:Math.abs(control.bottom-settings.bottom),",
    "rightGap:toolbar.right-settings.right,statusBelow:status.top>=settings.bottom};",
    "})()"
  ))
  expect_lt(toolbar_geometry$sameRow, 2)
  expect_lt(toolbar_geometry$rightGap, 16)
  expect_true(toolbar_geometry$statusBelow)
})

test_that("Clonal UMAP has Show-all toggle and group filters", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_umap_show_all",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  exists_el <- function(sel) {
    app$get_js(sprintf("document.querySelector('%s') !== null;", sel))
  }

  # Show-all and group filters are deferred until the settings drawer opens.
  app$click(selector = "#ir_more_button")
  app$wait_for_js(
    "document.querySelector('#ir_p_umap_show_all') !== null",
    timeout = 60000
  )
  expect_true(isTRUE(exists_el("#ir_p_umap_show_all")))
  # The non-faceted host renders through the shared Canvas engine.
  wait_for_ir_canvas(app)
  has_canvas <- app$get_js(
    sprintf("document.querySelector('%s') !== null;", ir_canvas_selector)
  )
  expect_true(isTRUE(has_canvas))

  app$wait_for_js(
    "document.querySelector('[id^=\"ir_group_filter_\"]') !== null",
    timeout = 45000
  )
  expect_true(isTRUE(app$get_js(
    "document.querySelector('[id^=\"ir_group_filter_\"]') !== null;"
  )))
})

test_that("Clonal UMAP groups into interactive panels that brush their own cells", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_umap_grouped_panels",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  host <- "#ir_clonalUMAP_projection_cell_view_host"
  state <- "window.cerebroSpecialistViews.get('ir_clonalUMAP_projection')"
  visible_boxes <- paste0(
    "Array.from(document.querySelectorAll('",
    host,
    " .cv-tbtn[data-act=\"box\"]')).filter(b=>b.getClientRects().length>0)"
  )

  app$wait_for_js(
    paste0(
      "(function(){var group=document.querySelector('#ir_p_umap_group_by');",
      "return !!group && group.value==='' && ",
      "document.querySelector('",
      ir_canvas_selector,
      "')!==null;})()"
    ),
    timeout = 60000
  )
  expect_equal(app$get_js(paste0(visible_boxes, ".length")), 1L)

  ## Grouping keeps the interactive canvas: one panel per group, no static image.
  app$set_inputs(ir_p_umap_group_by = "sample", wait_ = FALSE)
  app$wait_for_js(paste0(visible_boxes, ".length >= 2"), timeout = 45000)
  expect_false(isTRUE(app$get_js(
    "document.querySelector('#ir_plot_clonalUMAP_static') !== null"
  )))
  panels <- app$get_js(paste0(visible_boxes, ".length"))
  groups <- readCerebro(
    system.file(
      "extdata/examples/demo_full_tcr_bcr.crb",
      package = "CerebroNexus"
    )
  )
  metadata <- groups$getMetaData()
  expect_equal(panels, length(unique(metadata$sample)))

  ## A brush in the second panel selects only cells of the second group, and the
  ## cells below the plot are those.
  app$run_js(paste0(visible_boxes, "[1].click()"))
  canvases <- app$get_js(paste0(
    "Array.from(document.querySelectorAll('",
    host,
    " canvas:not(.cv-mini)')).filter(c=>c.getClientRects().length>0)",
    ".map(c=>{const r=c.getBoundingClientRect();",
    "return {x:r.x,y:r.y,w:r.width,h:r.height};})"
  ))
  ## Each panel owns two stacked canvases; the second panel starts at index 3.
  box <- canvases[[3]]
  viewer_drag_mouse(
    app,
    box$x + box$w * .02,
    box$y + box$h * .02,
    box$x + box$w * .98,
    box$y + box$h * .98
  )
  app$wait_for_js(paste0(state, ".capture().selection.cells.length > 0"))
  selected <- unlist(app$get_js(paste0(state, ".capture().selection.cells")))
  chosen <- unique(metadata$sample[match(selected, metadata$cell_barcode)])
  expect_length(chosen, 1L)
  expect_identical(
    as.character(chosen),
    levels(factor(metadata$sample))[[2L]]
  )
  app$wait_for_js(
    "document.getElementById('ir_clonalUMAP_number_of_selected_cells').textContent.includes('Selected')"
  )

  app$set_inputs(ir_p_umap_group_by = "", wait_ = FALSE)
  app$wait_for_js(paste0(visible_boxes, ".length === 1"), timeout = 45000)
  logs <- app$get_logs()
  expect_false(any(grepl("Error in|Invalid IHDR", logs$message)))
})

test_that("a shared grouped Clonal UMAP reopens grouped with its selection", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_umap_grouped_share",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  activate_ir_tab(app)

  host <- "#ir_clonalUMAP_projection_cell_view_host"
  share <- ".cerebro-config-open[data-view-id=\"ir_clonalUMAP_projection\"]"
  state <- "window.cerebroSpecialistViews.get('ir_clonalUMAP_projection')"
  visible_boxes <- paste0(
    "Array.from(document.querySelectorAll('",
    host,
    " .cv-tbtn[data-act=\"box\"]')).filter(b=>b.getClientRects().length>0)"
  )
  metadata <- readCerebro(
    system.file(
      "extdata/examples/demo_full_tcr_bcr.crb",
      package = "CerebroNexus"
    )
  )$getMetaData()
  second_group <- levels(factor(metadata$sample))[[2L]]

  app$wait_for_js(
    paste0(
      "(function(){var group=document.querySelector('#ir_p_umap_group_by');",
      "return !!group && group.value==='' && ",
      "document.querySelector('",
      ir_canvas_selector,
      "')!==null;})()"
    ),
    timeout = 60000
  )
  app$set_inputs(ir_p_umap_group_by = "sample", wait_ = FALSE)
  app$wait_for_js(paste0(visible_boxes, ".length >= 2"), timeout = 45000)
  panels <- app$get_js(paste0(visible_boxes, ".length"))

  ## Brush every cell of the second panel.
  app$run_js(paste0(visible_boxes, "[1].click()"))
  canvases <- app$get_js(paste0(
    "Array.from(document.querySelectorAll('",
    host,
    " canvas:not(.cv-mini)')).filter(c=>c.getClientRects().length>0)",
    ".map(c=>{const r=c.getBoundingClientRect();",
    "return {x:r.x,y:r.y,w:r.width,h:r.height};})"
  ))
  ## Each panel owns two stacked canvases; the second panel starts at index 3.
  box <- canvases[[3]]
  viewer_drag_mouse(
    app,
    box$x + box$w * .02,
    box$y + box$h * .02,
    box$x + box$w * .98,
    box$y + box$h * .98
  )
  app$wait_for_js(paste0(state, ".capture().selection.cells.length > 0"))
  selected <- sort(unlist(app$get_js(paste0(
    state,
    ".capture().selection.cells"
  ))))
  expect_identical(
    unique(as.character(metadata$sample[match(
      selected,
      metadata$cell_barcode
    )])),
    second_group
  )

  ## Share is offered in grouped mode, and the JSON it downloads carries the
  ## grouping column, the selected cells and the panel they were brushed in.
  ## This app has never opened Linked views, whose server answers Share.
  app$wait_for_js(
    paste0(
      "(()=>{const b=document.querySelector('",
      share,
      "');return !!b && b.getClientRects().length>0 && !b.disabled;})()"
    ),
    timeout = 15000
  )
  app$run_js(paste0(
    "(() => {const original=URL.createObjectURL;window.__testViewJSON=null;",
    "URL.createObjectURL=function(blob){",
    "if(blob.type.startsWith('application/json'))",
    "blob.text().then(text=>{window.__testViewJSON=text;});",
    "return original.call(URL,blob);};})()"
  ))
  app$click(selector = share)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-dialog').open && ",
      "!document.getElementById('cv-config-download').disabled"
    ),
    timeout = 10000
  )
  ## The PNG holds every panel; it must still download from the grouped view.
  app$click(selector = "#cv-config-png")
  expect_identical(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "PNG download started."
  )
  app$click(selector = "#cv-config-download")
  app$wait_for_js("window.__testViewJSON !== null", timeout = 15000)
  json <- app$get_js("window.__testViewJSON")
  config <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  app$click(selector = "#cv-config-close")
  saved_group <- Filter(
    function(control) identical(control$id, "ir_p_umap_group_by"),
    config$controls
  )
  expect_length(saved_group, 1L)
  expect_identical(saved_group[[1L]]$values[[1L]], "sample")
  expect_identical(sort(unlist(config$selection$cells)), selected)
  expect_identical(config$selection$geometry$panel, 1L)

  ## Reset the page: ungroup and clear the selection.
  app$set_inputs(ir_p_umap_group_by = "", wait_ = FALSE)
  app$wait_for_js(paste0(visible_boxes, ".length === 1"), timeout = 45000)
  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll('",
      host,
      " .cv-clear-btn')).some(b=>b.getClientRects().length>0)"
    ),
    timeout = 15000
  )
  app$run_js(paste0(
    "Array.from(document.querySelectorAll('",
    host,
    " .cv-clear-btn')).find(b=>b.getClientRects().length>0).click()"
  ))
  app$wait_for_js(paste0(state, ".capture().selection.cells.length === 0"))

  ## Open the JSON through the dialog's file input, as a user would.
  app$click(selector = share)
  app$wait_for_js(
    "document.getElementById('cv-config-dialog').open",
    timeout = 10000
  )
  app$run_js(sprintf(
    paste0(
      "(() => {const input=document.getElementById('coordviews_config_upload');",
      "const transfer=new DataTransfer();",
      "transfer.items.add(new File([%s],'grouped.json',",
      "{type:'application/json'}));",
      "input.files=transfer.files;",
      "input.dispatchEvent(new Event('change',{bubbles:true}));})()"
    ),
    jsonlite::toJSON(json, auto_unbox = TRUE)
  ))
  app$wait_for_js(
    "document.getElementById('cv-config-status').classList.contains('is-success')",
    timeout = 15000
  )
  app$click(selector = "#cv-config-close")

  ## Grouping comes back first, then the selection lands in its panel.
  app$wait_for_js(
    paste0(
      visible_boxes,
      ".length === ",
      panels,
      " && ",
      state,
      ".capture().selection.cells.length === ",
      length(selected)
    ),
    timeout = 45000
  )
  expect_identical(
    app$get_js("document.getElementById('ir_p_umap_group_by').value"),
    "sample"
  )
  restored <- app$get_js(paste0(state, ".capture()"))
  expect_identical(sort(unlist(restored$selection$cells)), selected)
  expect_identical(restored$selection$geometry$panel, 1L)
  app$wait_for_js(
    "document.getElementById('ir_clonalUMAP_number_of_selected_cells').textContent.includes('Selected')"
  )
  logs <- app$get_logs()
  expect_false(any(grepl("Error in", logs$message)))
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
  ## Strict identity: a missing or NULL export must fail, not pass as FALSE.
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)

  ## Startup probes availability with system.file() only. Wait past the former
  ## one-second background-prewarm timer, so a deferred loader would have fired.
  Sys.sleep(1.5)
  app$wait_for_idle(timeout = 10000)
  expect_identical(app$get_value(export = "scRepertoire_loaded"), FALSE)
  expect_identical(app$get_value(export = "ir_heavy_deps_loaded"), FALSE)

  ## The module does not break the main app: Data info of an app that has never
  ## opened the repertoire page shows the example's 1000 cells.
  cells_box <- app$get_value(output = "load_data_overview")
  expect_true(grepl("1,?000", cells_box$html))

  ## Landing on the IR tab shows the default "Clonal UMAP" — a self-made plot —
  ## which must not drag in scRepertoire.
  open_ir_tab(app)
  expect_identical(
    app$get_js(
      paste0(
        "(function(){",
        "var active=document.querySelector('#ir_tabs li.active a');",
        "return active?active.getAttribute('data-value'):null;",
        "})()"
      )
    ),
    "Clonal UMAP"
  )
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
  app$wait_for_js(
    "document.querySelector('#ir_plot_cloneSharing .plotly') !== null",
    timeout = 45000
  )
  expect_false(is.null(app$get_value(output = "ir_plot_cloneSharing")))
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

  ## SizeDist previously monopolised the Shiny process on large repertoires and
  ## could leave this and every subsequently selected tab blank. The bundled
  ## data uses the upstream path; large-data profile correctness is covered by
  ## the pure-function tests.
  app$set_inputs(ir_tabs = "SizeDist", wait_ = FALSE)
  app$wait_for_idle(timeout = 45000)
  app$wait_for_js(
    "document.querySelector('#ir_plot_clonalSizeDistribution .plotly') !== null",
    timeout = 45000
  )
  expect_false(is.null(app$get_value(
    output = "ir_plot_clonalSizeDistribution"
  )))
})
