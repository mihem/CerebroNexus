# test-app-trajectory.R — shinytest2 integration tests for trajectory module
#
# The bundled app's default landing set ("PBMC - Full (T+B)" ->
# demo_full_tcr_bcr.crb) carries the monocle2 B-cell trajectory, so the
# Trajectory tab is present from the start.

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

## All three tests below run against the same default data set, and booting the
## app costs far more than the assertions do, so they share one driver created on
## first use and stopped when the file finishes.
##
## Each test navigates to the tab it asserts on instead of inheriting whatever
## tab a previous test left active, so the tests stay order-independent. A test
## that needs a pristine app (e.g. asserting something has NOT loaded yet) must
## create its own AppDriver rather than call shared_app().
shared_driver <- NULL
shared_driver_env <- environment()
shared_app <- function() {
  if (is.null(shared_driver)) {
    local_app_support(inst_dir, envir = shared_driver_env)
    shared_driver <<- AppDriver$new(
      inst_dir,
      name = "trajectory_shared",
      height = 950,
      width = 1619,
      load_timeout = 60000
    )
    withr::defer(shared_driver$stop(), envir = shared_driver_env)
    shared_driver$wait_for_idle(timeout = 20000)
  }
  shared_driver
}

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
  cells_box <- app$get_value(output = "load_data_number_of_cells")
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
      ".classList.contains('cerebro-selection-status-hidden')"
    ),
    timeout = 10000
  )

  app$click(selector = "#trajectory_projection_zoom_to_selection")
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
