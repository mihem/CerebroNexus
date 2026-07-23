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

test_that("Trajectory tab is present on the default (full T+B) data set", {
  app <- AppDriver$new(
    inst_dir,
    name = "trajectory_visible",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  # Default data set now carries trajectory data -> tab present immediately.
  tab_present <- app$get_js(
    'document.querySelector(\'a[href="#shiny-tab-trajectory"]\') !== null;'
  )
  expect_true(tab_present)
})

test_that("trajectory module loads without breaking the main app", {
  app <- AppDriver$new(
    inst_dir,
    name = "trajectory_load",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  # Default data set (1,476 cells) now carries the trajectory; confirm the
  # Data info tab still renders its cell count normally alongside it.
  cells_box <- app$get_value(output = "load_data_number_of_cells")
  expect_true(grepl("1,476", cells_box$html))
})

test_that("trajectory projection fits the viewport with selectors in parameters", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "trajectory_viewport",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  app$click(selector = 'a[href="#shiny-tab-trajectory"]')
  app$wait_for_idle(timeout = 20000)

  # The method/name selectors and the projection render through nested
  # renderUI + Plotly, which take several server round-trips; on a slow (CI)
  # machine one wait_for_idle can return before they exist. Wait for the actual
  # nodes so the assertions below never read a half-rendered tab.
  app$wait_for_js(
    paste0(
      "document.getElementById('trajectory_selected_method') != null && ",
      "document.getElementById('trajectory_selected_name') != null && ",
      "document.querySelector('#trajectory_projection .main-svg') != null && ",
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
      ".closest('.box').querySelector('.box-title')",
      ".innerText.includes('Main parameters')"
    )
  ))
  expect_true(app$get_js(
    paste0(
      "document.getElementById('trajectory_selected_name')",
      ".closest('.box').querySelector('.box-title')",
      ".innerText.includes('Main parameters')"
    )
  ))

  geometry <- app$get_js(paste0(
    "(() => {",
    "const plot = document.getElementById('trajectory_projection');",
    "const box = plot.closest('.box');",
    "const footer = document.getElementById(",
    "'trajectory_number_of_selected_cells');",
    "const svg = plot.querySelector('.main-svg');",
    "const ticks = Array.from(plot.querySelectorAll(",
    "'.xaxislayer-above .xtick text, .yaxislayer-above .ytick text'));",
    "const tickBottom = Math.max(...ticks.map(",
    "tick => tick.getBoundingClientRect().bottom));",
    "return {",
    "viewport: window.innerHeight,",
    "plotHeight: plot.getBoundingClientRect().height,",
    "plotBottom: plot.getBoundingClientRect().bottom,",
    "svgBottom: svg.getBoundingClientRect().bottom,",
    "tickBottom: tickBottom,",
    "footerTop: footer.getBoundingClientRect().top,",
    "boxBottom: box.getBoundingClientRect().bottom,",
    "footerBottom: footer.getBoundingClientRect().bottom",
    "};",
    "})()"
  ))

  expect_gte(geometry$plotHeight, 240)
  expect_lte(geometry$svgBottom, geometry$plotBottom + 1)
  expect_lte(geometry$tickBottom, geometry$plotBottom + 1)
  expect_lt(geometry$tickBottom, geometry$footerTop)
  expect_lte(geometry$footerBottom, geometry$viewport)
  expect_lte(geometry$boxBottom, geometry$viewport)
})
