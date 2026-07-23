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

test_that("IR fill layout survives tab activation and responsive resize", {
  local_app_support(inst_dir)
  app <- AppDriver$new(
    inst_dir,
    name = "ir_fill_viewport",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  app$wait_for_js(
    'document.querySelector(\'a[href="#shiny-tab-immune_repertoire"]\') !== null',
    timeout = 30000
  )
  app$click(selector = 'a[href="#shiny-tab-immune_repertoire"]')
  app$wait_for_idle(timeout = 20000)

  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll(",
      "'#shiny-tab-immune_repertoire .nav-tabs a'))",
      ".some(a => a.textContent.trim() === 'Abundance')"
    ),
    timeout = 30000
  )
  app$run_js(paste0(
    "Array.from(document.querySelectorAll(",
    "'#shiny-tab-immune_repertoire .nav-tabs a'))",
    ".find(a => a.textContent.trim() === 'Abundance').click();"
  ))
  app$wait_for_js(
    paste0(
      "(() => {",
      "const active = document.querySelector('#ir_tabs li.active a');",
      "return active && active.textContent.trim() === 'Abundance';",
      "})()"
    ),
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll(",
      "'#shiny-tab-immune_repertoire .cerebro-fill.is-filled'))",
      ".some(el => el.getClientRects().length > 0)"
    ),
    timeout = 30000
  )

  geometry_js <- paste0(
    "(() => {",
    "const tab = document.getElementById('shiny-tab-immune_repertoire');",
    "const fill = Array.from(tab.querySelectorAll('.cerebro-fill'))",
    ".find(el => el.getClientRects().length > 0);",
    "const row = fill.closest('.cerebro-viz-row');",
    "const param = row.querySelector('.cerebro-param-col');",
    "const viz = row.querySelector('.cerebro-viz-col');",
    "const wrapper = fill.closest('.content-wrapper');",
    "const fr = fill.getBoundingClientRect();",
    "const pr = param.getBoundingClientRect();",
    "const vr = viz.getBoundingClientRect();",
    "return {",
    "viewportHeight: window.innerHeight, viewportWidth: window.innerWidth,",
    "fillTop: fr.top, fillBottom: fr.bottom, fillHeight: fr.height,",
    "fillOverflow: getComputedStyle(fill).overflow,",
    "paramLeft: pr.left, paramTop: pr.top, paramBottom: pr.bottom,",
    "vizLeft: vr.left, vizTop: vr.top,",
    "wrapperClientWidth: wrapper.clientWidth,",
    "wrapperScrollWidth: wrapper.scrollWidth",
    "};",
    "})()"
  )

  desktop <- app$get_js(geometry_js)
  expect_gte(desktop$fillHeight, 240)
  expect_lte(desktop$fillBottom, desktop$viewportHeight)
  expect_identical(desktop$fillOverflow, "visible")
  expect_lt(desktop$paramLeft, desktop$vizLeft)
  expect_lt(abs(desktop$paramTop - desktop$vizTop), 1)

  app$get_chromote_session()$set_viewport_size(width = 800, height = 800)
  app$wait_for_js(
    "window.innerWidth === 800 && window.innerHeight === 800",
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "(() => {",
      "const row = document.querySelector(",
      "'#shiny-tab-immune_repertoire .cerebro-viz-row');",
      "const p = row.querySelector('.cerebro-param-col').getBoundingClientRect();",
      "const v = row.querySelector('.cerebro-viz-col').getBoundingClientRect();",
      "return p.bottom <= v.top + 1;",
      "})()"
    ),
    timeout = 10000
  )

  narrow <- app$get_js(geometry_js)
  expect_lte(narrow$paramBottom, narrow$vizTop + 1)
  expect_lte(narrow$wrapperScrollWidth, narrow$wrapperClientWidth)
  expect_gte(narrow$fillHeight, 240)

  ## Phone-width viewport (mihem asked for mobile coverage). shinytest2 cannot
  ## emulate touch, but a real 390-wide viewport catches gross narrow-layout
  ## breakage: the params must stack above the plot, the plot must keep a usable
  ## height, and — the property the user actually sees — the PAGE must not scroll
  ## sideways. Reuses the booted app, so it adds a resize, not another Chrome
  ## process.
  app$get_chromote_session()$set_viewport_size(width = 390, height = 844)
  app$wait_for_js(
    "window.innerWidth === 390 && window.innerHeight === 844",
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "(() => {",
      "const row = document.querySelector(",
      "'#shiny-tab-immune_repertoire .cerebro-viz-row');",
      "const p = row.querySelector('.cerebro-param-col').getBoundingClientRect();",
      "const v = row.querySelector('.cerebro-viz-col').getBoundingClientRect();",
      "return p.bottom <= v.top + 1;",
      "})()"
    ),
    timeout = 10000
  )

  phone <- app$get_js(geometry_js)
  expect_lte(phone$paramBottom, phone$vizTop + 1)
  expect_gte(phone$fillHeight, 240)

  ## The document must not scroll horizontally. `.content-wrapper` clips its own
  ## overflow (overflow-x: hidden), so a too-wide inner widget is contained
  ## rather than pushing the page sideways — assert the document, which is what
  ## the user perceives as "the page scrolls sideways on my phone".
  page_no_hscroll <- app$get_js(
    "document.documentElement.scrollWidth <= window.innerWidth + 1"
  )
  expect_true(isTRUE(page_no_hscroll))
})
