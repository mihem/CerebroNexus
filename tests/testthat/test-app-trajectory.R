# test-app-trajectory.R — shinytest2 integration tests for trajectory module
#
# The bundled app's default landing set ("PBMC - Full (T+B)" ->
# demo_full_tcr_bcr.crb) carries the monocle2 B-cell trajectory, so the
# Trajectory tab is present from the start.

skip_if_not_installed("shinytest2")
library(shinytest2)

## All three tests below run against the same default data set and navigate to
## the tab they assert on, so they share one driver.
shared_app <- viewer_shared_app("trajectory_shared")

test_that("Trajectory tab is present on the default (full T+B) data set", {
  app <- shared_app()

  # Default data set now carries trajectory data -> tab present immediately.
  tab_present <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-trajectory"]\') !== null;'
  )
  expect_true(tab_present)
})

test_that("trajectory module loads without breaking the main app", {
  app <- shared_app()

  # Read the Data info tab explicitly rather than assuming the app is still on
  # its landing tab, so this assertion does not depend on test order.
  app$set_inputs(sidebar = "loadData")
  app$wait_for_idle(timeout = 10000)

  # Default data set (1,476 cells) now carries the trajectory; confirm the
  # Data info tab still renders its cell count normally alongside it.
  cells_box <- app$get_value(output = "load_data_overview")
  expect_true(grepl("1,476", cells_box$html))
})

test_that("trajectory projection fits the viewport with selectors in the top bar", {
  app <- shared_app()

  app$click(selector = 'a[href="#shiny-tab-trajectory"]')
  app$wait_for_idle(timeout = 20000)

  # The method/name selectors and Canvas projection render through nested
  # renderUI, which takes several server round-trips; on a slow (CI)
  # machine one wait_for_idle can return before they exist. Wait for the actual
  # nodes so the assertions below never read a half-rendered tab.
  app$wait_for_js(
    paste0(
      "document.getElementById('trajectory_selected_method') != null && ",
      "document.getElementById('trajectory_selected_name') != null && ",
      "document.querySelector('#trajectory_projection_cell_view_host .cv-canvas-wrap > canvas:not(.cv-mini)') != null && ",
      "document.getElementById('trajectory_number_of_selected_cells') != null"
    ),
    timeout = 30000
  )

  expect_false(app$get_js(
    "document.body.innerText.includes('cerebro-projection-plot')"
  ))
  expect_true(app$get_js(
    paste0(
      "document.getElementById('trajectory_selected_method')",
      ".closest('.cerebro-viz-toolbar') !== null"
    )
  ))
  expect_true(app$get_js(
    paste0(
      "document.getElementById('trajectory_selected_name')",
      ".closest('.cerebro-viz-toolbar') === ",
      "document.getElementById('trajectory_selected_method')",
      ".closest('.cerebro-viz-toolbar')"
    )
  ))

  geometry <- app$get_js(paste0(
    "(() => {",
    "const plot = document.querySelector(",
    "'#trajectory_projection_cell_view_host .cv-canvas-wrap > canvas:not(.cv-mini)');",
    "const host = plot.closest('.cerebro-cell-view-host');",
    "const status = document.querySelector(",
    "'#shiny-tab-trajectory .cerebro-selection-status-slot');",
    "return {",
    "viewport: window.innerHeight,",
    "plotHeight: plot.getBoundingClientRect().height,",
    "plotTop: plot.getBoundingClientRect().top,",
    "plotBottom: plot.getBoundingClientRect().bottom,",
    "hostBottom: host.getBoundingClientRect().bottom,",
    "statusBottom: status.getBoundingClientRect().bottom",
    "};",
    "})()"
  ))

  expect_gte(geometry$plotHeight, 240)
  expect_lt(geometry$statusBottom, geometry$plotTop)
  expect_lte(geometry$plotBottom, geometry$hostBottom)
  expect_lte(geometry$hostBottom, geometry$viewport)
  expect_true(app$get_js(paste0(
    "document.querySelector('[data-view-id=\"trajectory_projection\"]",
    ".cerebro-toolbar-share').offsetParent !== null"
  )))

  drag <- app$get_js(paste0(
    "(() => {",
    "const host=document.getElementById('trajectory_projection_cell_view_host');",
    "host.querySelector('.cv-tbtn[data-act=\"box\"]').click();",
    "const r=host.querySelector('.cv-canvas-wrap > canvas:not(.cv-mini)')",
    ".getBoundingClientRect();",
    "return {x1:r.left+r.width*.08,y1:r.top+r.height*.08,",
    "x2:r.right-r.width*.08,y2:r.bottom-r.height*.08};",
    "})()"
  ))
  viewer_drag_mouse(app, drag$x1, drag$y1, drag$x2, drag$y2)
  app$wait_for_js(
    paste0(
      "!document.getElementById('trajectory_projection_selection_active')",
      ".classList.contains('cerebro-selection-status-hidden') && ",
      "document.querySelector('[data-view-id=\"trajectory_projection\"]",
      ".cerebro-toolbar-share').offsetParent !== null"
    ),
    timeout = 10000
  )
  trajectory_actions <- app$get_js(paste0(
    "(() => {",
    "const share=document.querySelector('[data-view-id=\"trajectory_projection\"]",
    ".cerebro-toolbar-share').getBoundingClientRect();",
    "const settings=document.getElementById(",
    "'trajectory_projection_more_button').getBoundingClientRect();",
    "return share.left < settings.left && settings.right > share.right;",
    "})()"
  ))
  expect_true(trajectory_actions)

  app$run_js(
    "document.querySelector('#trajectory_projection_cell_view_host .cv-zsel-btn').click()"
  )
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#trajectory_projection_cell_view_host .cv-mini.is-on') !== null"
    ),
    timeout = 10000
  )
  app$run_js(paste0(
    "document.querySelector(",
    "'#trajectory_projection_cell_view_host .cv-tbtn[data-act=\"pan\"]'",
    ").click()"
  ))
  viewer_drag_mouse(
    app,
    drag$x1,
    drag$y1,
    drag$x1 + 35,
    drag$y1 + 20
  )
  expect_false(app$get_js(paste0(
    "document.getElementById('trajectory_projection_clear_selection')",
    ".offsetParent === null"
  )))

  app$click(selector = "#trajectory_projection_clear_selection")
  app$wait_for_js(
    paste0(
      "document.getElementById('trajectory_projection_selection_active')",
      ".classList.contains('cerebro-selection-status-hidden')"
    ),
    timeout = 10000
  )
  app$run_js(paste0(
    "document.querySelector(",
    "'#trajectory_projection_cell_view_host .cv-tbtn[data-act=\"reset\"]'",
    ").click()"
  ))
})
