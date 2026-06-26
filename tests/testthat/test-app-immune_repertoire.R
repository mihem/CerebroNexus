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
  inst_dir <- system.file(package = "cerebroAppLite")
}
if (!nzchar(inst_dir) || !file.exists(file.path(inst_dir, "app.R"))) {
  inst_dir <- testthat::test_path("../../inst")
}

# scRepertoire (GitHub-only, not in nix) is required for the immune repertoire
# module to render any UI: the module guards every output with
# req(has_scRepertoire()), so without the package the settings/plot controls
# never reach the DOM and every shinytest2 assertion that looks for them fails.
# Skip the whole file when scRepertoire is absent (e.g. the nix CI environment)
# so CI stays green; the tests still run locally where scRepertoire is present.
skip_if_not_installed("scRepertoire")

test_that("immune_repertoire tab is present with example data (has TCR)", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_present", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # example.crb carries real TCR data — the conditional tab should appear
  tab_present <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\') !== null;'
  )
  expect_true(tab_present)

  app$stop()
})

test_that("first IR plot tab is Abundance, not Scatter", {
  # The default/landing tab should be a common overview plot (Abundance), not
  # the sample-comparison Scatter.
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_first_tab", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  first_tab <- app$get_js(
    "document.querySelector('#ir_tabs > li > a').textContent.trim();"
  )
  expect_identical(first_tab, "Abundance")

  app$stop()
})

test_that("Group by is visible on plots whose grouping it drives", {
  # Group by is a real scRepertoire parameter for Scatter, and the custom BCR
  # Isotype/SHM Proxy renderers also use it as their grouping column. It should
  # only be hidden on Paired Scatter, where comparison is controlled by the
  # paired sample metadata selectors.
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_groupby_scope", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  # Global controls are now rendered server-side per tab (no conditionalPanel),
  # so visibility = the control element exists and is laid out.
  groupby_visible <- function() {
    app$get_js(
      "(function(){var e=document.querySelector('#ir_groupBy');return e!==null && e.offsetParent!==null;})();"
    )
  }

  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "Scatter", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "Isotype", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "SHM Proxy", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(groupby_visible()))

  app$set_inputs(ir_tabs = "Paired Scatter", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_false(isTRUE(groupby_visible()))

  app$stop()
})

test_that("Chain is visible on plots whose scRepertoire API accepts it", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_chain_scope", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  chain_visible <- function() {
    app$get_js(
      "(function(){var e=document.querySelector('#ir_chain');return e!==null && e.offsetParent!==null;})();"
    )
  }

  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(chain_visible()))

  app$set_inputs(ir_tabs = "SizeDist", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(chain_visible()))

  app$set_inputs(ir_tabs = "vizGenes", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_false(isTRUE(chain_visible()))

  app$stop()
})

test_that("changing 'Group by' keeps the current plot tab", {
  # Changing the grouping must not reset the visualization tabset back to the
  # first tab (Abundance).
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_group_keep_tab", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

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

  app$stop()
})

test_that("settings dropdowns render all their options (not just selected)", {
  # selectize widgets rendered inside hidden conditionalPanels / dynamic UI drop
  # all but the selected <option>. We use selectize = FALSE so users can
  # actually choose other values. Assert the real <option> counts here — note
  # set_inputs() bypasses the DOM, so it cannot catch this regression.
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_dropdown_options", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  n_options <- function(id) {
    app$get_js(sprintf(
      "(function(){var e=document.querySelector('#%s');return e?e.querySelectorAll('option').length:0;})();",
      id
    ))
  }

  # Group by: None + grouping variables (sample, seurat_clusters, cell_type)
  expect_gte(as.numeric(n_options("ir_groupBy")), 2)
  # Chain: both + detected chains (TRA/TRB/IGH/IGK/IGL) > 1
  expect_gte(as.numeric(n_options("ir_chain")), 2)
  # Clone call: gene/nt/aa/strict
  expect_gte(as.numeric(n_options("ir_cloneCall")), 2)

  # Scatter selectors (Scatter tab) should list all samples, not just selected
  app$set_inputs(ir_tabs = "Scatter", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_gte(as.numeric(n_options("ir_scatter_x")), 2)
  expect_gte(as.numeric(n_options("ir_scatter_y")), 2)

  app$stop()
})

test_that("immune_repertoire tab can be opened and renders settings", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_open", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # select the tab
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  # the chain selector (a core settings control) should be populated
  chain_present <- app$get_js(
    'document.querySelector("#ir_chain") !== null;'
  )
  expect_true(chain_present)

  app$stop()
})

test_that("scatter sample selectors appear only on the Scatter tab", {
  # The Scatter X/Y selectors are scoped to the Scatter tab via conditionalPanel
  # so they don't clutter the settings panel on every other tab.
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_scatter_scope", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  # The Scatter selectors live inside a conditionalPanel keyed on input.ir_tabs.
  # selectInput renders a selectize widget that hides the native <select>, so we
  # check the conditionalPanel wrapper's computed display, not the <select>.
  scatter_panel_visible <- function() {
    # match the panel whose condition is exactly the Scatter selector panel
    # (input.ir_tabs == 'Scatter'), not the Group-by exclusion panel which also
    # mentions 'Scatter'.
    app$get_js(
      "(function(){
        var cps = document.querySelectorAll('[data-display-if]');
        for (var i=0;i<cps.length;i++){
          var c = cps[i].getAttribute('data-display-if');
          if (c.indexOf(\"== 'Scatter'\") !== -1){
            return window.getComputedStyle(cps[i]).display !== 'none';
          }
        }
        return false;
      })();"
    )
  }

  # default tab (Abundance) — scatter panel hidden
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_false(isTRUE(scatter_panel_visible()))

  # Scatter tab — scatter panel visible
  app$set_inputs(ir_tabs = "Scatter", wait_ = FALSE)
  app$wait_for_idle(timeout = 15000)
  expect_true(isTRUE(scatter_panel_visible()))

  app$stop()
})

test_that("clonal scatter renders without error in default and grouped states", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_scatter", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)
  app$run_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\').click();'
  )
  app$wait_for_idle(timeout = 20000)

  err_pat <- "clonalScatter|getlindex|get1index|undefined columns|names.*attribute"

  # default state: example is split into >= 2 samples, scatter should render
  v1 <- app$get_value(output = "ir_plot_clonalScatter")
  expect_false(isTRUE(grepl(err_pat, v1$html, ignore.case = TRUE)))

  # grouped state (the combination that previously errored)
  app$set_inputs(ir_groupBy = "cell_type", wait_ = FALSE)
  app$wait_for_idle(timeout = 20000)
  v2 <- app$get_value(output = "ir_plot_clonalScatter")
  expect_false(isTRUE(grepl(err_pat, v2$html, ignore.case = TRUE)))

  app$stop()
})

test_that("immune_repertoire module loads without breaking main app", {
  local_app_support(inst_dir)
  app <- AppDriver$new(inst_dir, name = "ir_load", height = 950, width = 1619)
  app$wait_for_idle(timeout = 20000)

  # Data info tab should still render normally (1476 cells in the new example)
  cells_box <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("1,?476", cells_box$html))

  app$stop()
})
