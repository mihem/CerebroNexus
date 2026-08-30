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
    'document.querySelector(\'a[href="#shiny-tab-coordinated_views"]\') !== null',
    timeout = 30000
  )
  app$click(selector = 'a[href="#shiny-tab-coordinated_views"]')
  app$wait_for_js(
    paste0(
      "(() => {",
      "const p = document.querySelector('.cv-pane:not(.cv-hidden)');",
      "const c = p && p.querySelector('canvas:not(.cv-mini)');",
      "return c && c.getBoundingClientRect().width > 100;",
      "})()"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "Array.from(document.querySelectorAll('.cv-ptitle'))",
      ".some(el => el.textContent.includes('B_cell_maturation'))"
    ),
    timeout = 30000
  )

  ## Focus remains a Linked views interaction; only standalone single-panel
  ## hosts suppress it.
  app$run_js(paste0(
    "document.querySelector(",
    "'.cv-pane:not(.cv-hidden) .cv-canvas-wrap > canvas:not(.cv-mini)')",
    ".dispatchEvent(new MouseEvent('dblclick',",
    "{bubbles:true,cancelable:true}));"
  ))
  app$wait_for_js(
    "document.querySelector('.cv-panes.cv-has-focus') !== null",
    timeout = 10000
  )
  app$click(selector = ".cv-focus-btn.is-on")
  app$wait_for_js(
    paste0(
      "document.querySelector('.cv-panes.cv-has-focus') === null && ",
      "document.querySelector('.cv-panes.cv-focus-transitioning') === null"
    ),
    timeout = 10000
  )

  ## A linked selection owns the lens it came from. Zoom must follow that lens,
  ## and a minimap is useful only while a lens is actually zoomed.
  trajectory_box <- app$get_js(paste0(
    "(() => {",
    "const title = Array.from(document.querySelectorAll('.cv-ptitle'))",
    ".find(el => el.textContent.includes('B_cell_maturation'));",
    "const pane = title.closest('.cv-pane');",
    "pane.querySelector('.cv-tbtn[data-act=\"box\"]').click();",
    "const r = pane.querySelector('canvas:not(.cv-mini)').getBoundingClientRect();",
    "return {x1:r.left+r.width*.08,y1:r.top+r.height*.08,",
    "x2:r.right-r.width*.08,y2:r.bottom-r.height*.08};",
    "})()"
  ))
  viewer_drag_mouse(
    app,
    trajectory_box$x1,
    trajectory_box$y1,
    trajectory_box$x2,
    trajectory_box$y2
  )
  app$wait_for_js(
    paste0(
      "document.querySelector('#cv-selbar:not(.cv-collapse)') && ",
      "document.getElementById('cv-zoom').offsetParent !== null"
    ),
    timeout = 10000
  )
  expect_length(
    unlist(app$get_js(
      "Array.from(document.querySelectorAll('.cv-mini.is-on')).map(String)"
    )),
    0
  )

  app$click(selector = "#cv-zoom")
  app$wait_for_js(
    paste0(
      "(() => {",
      "const title = Array.from(document.querySelectorAll('.cv-ptitle'))",
      ".find(el => el.textContent.includes('B_cell_maturation'));",
      "return title.closest('.cv-pane').querySelector('.cv-mini.is-on') !== null;",
      "})()"
    ),
    timeout = 10000
  )
  zoomed_titles <- unlist(app$get_js(paste0(
    "Array.from(document.querySelectorAll('.cv-pane'))",
    ".filter(p => p.querySelector('.cv-mini.is-on'))",
    ".map(p => p.querySelector('.cv-ptitle').textContent.trim())"
  )))
  expect_length(zoomed_titles, 1)
  expect_match(zoomed_titles[[1]], "B_cell_maturation", fixed = TRUE)
  expect_identical(
    app$get_js("document.getElementById('cv-zoom').textContent.trim()"),
    "Zoom back"
  )

  app$click(selector = "#cv-zoom")
  app$wait_for_js(
    "document.querySelectorAll('.cv-mini.is-on').length === 0",
    timeout = 10000
  )
  app$click(selector = "#cv-clear")
  app$wait_for_js(
    "document.getElementById('cv-selbar').classList.contains('cv-collapse')",
    timeout = 10000
  )

  linked_geometry_js <- paste0(
    "(() => {",
    "const canvas = document.querySelector(",
    "'.cv-pane:not(.cv-hidden) canvas:not(.cv-mini)');",
    "const rect = canvas.getBoundingClientRect();",
    "const wrapper = document.querySelector('.content-wrapper');",
    "const toggle = document.querySelector('.sidebar-toggle').getBoundingClientRect();",
    "return {collapsed: document.body.classList.contains('sidebar-collapse'),",
    "canvasWidth: rect.width, canvasHeight: rect.height,",
    "canvasLeft: rect.left, toggleRight: toggle.right, toggleWidth: toggle.width,",
    "contentWidth: wrapper.getBoundingClientRect().width};",
    "})()"
  )
  linked_open <- app$get_js(linked_geometry_js)
  expect_false(linked_open$collapsed)
  expect_gt(abs(linked_open$canvasWidth - linked_open$canvasHeight), 20)
  expect_equal(linked_open$toggleWidth, 20, tolerance = 1)
  expect_lte(linked_open$toggleRight, linked_open$canvasLeft)

  toggle_style_js <- paste0(
    "(() => {",
    "const style = getComputedStyle(document.querySelector('.sidebar-toggle'));",
    "return {background: style.backgroundColor, color: style.color};",
    "})()"
  )
  toggle_default <- app$get_js(toggle_style_js)
  expect_identical(toggle_default$background, "rgb(236, 235, 235)")
  expect_identical(toggle_default$color, "rgb(107, 107, 112)")

  toggle_center <- app$get_js(paste0(
    "(() => {",
    "const r = document.querySelector('.sidebar-toggle').getBoundingClientRect();",
    "return {x: r.left + r.width / 2, y: r.top + r.height / 2};",
    "})()"
  ))
  app$get_chromote_session()$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = toggle_center$x,
    y = toggle_center$y
  )
  app$wait_for_js(
    paste0(
      "(() => {",
      "const style = getComputedStyle(document.querySelector('.sidebar-toggle'));",
      "return style.backgroundColor === 'rgb(255, 228, 209)' && ",
      "style.color === 'rgb(200, 90, 14)';",
      "})()"
    ),
    timeout = 5000
  )
  toggle_hover <- app$get_js(toggle_style_js)
  expect_identical(toggle_hover$color, "rgb(200, 90, 14)")

  app$click(selector = '.sidebar-toggle')
  app$wait_for_js(
    paste0(
      "document.body.classList.contains('sidebar-collapse') && ",
      "document.querySelector('.content-wrapper').getBoundingClientRect().width > ",
      linked_open$contentWidth + 150
    ),
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'.cv-pane:not(.cv-hidden) canvas:not(.cv-mini)')",
      ".getBoundingClientRect().width > ",
      linked_open$canvasWidth + 20
    ),
    timeout = 10000
  )
  linked_collapsed <- app$get_js(linked_geometry_js)
  expect_true(linked_collapsed$collapsed)
  expect_gt(linked_collapsed$canvasWidth, linked_open$canvasWidth + 20)

  app$run_js(paste0(
    "const square = document.getElementById('cv-square-plots');",
    "square.checked = true;",
    "square.dispatchEvent(new Event('input', {bubbles:true}));"
  ))
  app$wait_for_js(
    paste0(
      "(() => {",
      "const r = document.querySelector(",
      "'.cv-pane:not(.cv-hidden) canvas:not(.cv-mini)').getBoundingClientRect();",
      "return Math.abs(r.width - r.height) < 2;",
      "})()"
    ),
    timeout = 10000
  )

  ## Restore the default shell state before checking the existing responsive
  ## visualization pages below.
  app$click(selector = '.sidebar-toggle')
  app$wait_for_js(
    "!document.body.classList.contains('sidebar-collapse')",
    timeout = 10000
  )

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
    "const toolbar = row.querySelector('.cerebro-viz-toolbar');",
    "const viz = row.querySelector('.cerebro-viz-col');",
    "const wrapper = fill.closest('.content-wrapper');",
    "const fr = fill.getBoundingClientRect();",
    "const tr = toolbar.getBoundingClientRect();",
    "const vr = viz.getBoundingClientRect();",
    "return {",
    "viewportHeight: window.innerHeight, viewportWidth: window.innerWidth,",
    "fillTop: fr.top, fillBottom: fr.bottom, fillHeight: fr.height,",
    "fillOverflow: getComputedStyle(fill).overflow,",
    "toolbarLeft: tr.left, toolbarRight: tr.right,",
    "toolbarTop: tr.top, toolbarBottom: tr.bottom,",
    "vizLeft: vr.left, vizRight: vr.right, vizTop: vr.top,",
    "wrapperClientWidth: wrapper.clientWidth,",
    "wrapperScrollWidth: wrapper.scrollWidth",
    "};",
    "})()"
  )

  desktop <- app$get_js(geometry_js)
  expect_gte(desktop$fillHeight, 240)
  expect_lte(desktop$fillBottom, desktop$viewportHeight)
  expect_identical(desktop$fillOverflow, "visible")
  expect_lte(desktop$toolbarBottom, desktop$vizTop + 1)
  expect_lt(abs(desktop$toolbarLeft - desktop$vizLeft), 1)
  expect_lt(abs(desktop$toolbarRight - desktop$vizRight), 1)

  top_layout_js <- function(tab_name) {
    paste0(
      "(() => {",
      "const row = document.querySelector('#shiny-tab-",
      tab_name,
      " .cerebro-viz-top-layout');",
      "const p = row.querySelector('.cerebro-viz-toolbar').getBoundingClientRect();",
      "const v = row.querySelector('.cerebro-viz-col').getBoundingClientRect();",
      "return {paramBottom:p.bottom, vizTop:v.top, ",
      "paramLeft:p.left, paramRight:p.right, vizLeft:v.left, vizRight:v.right};",
      "})()"
    )
  }
  for (tab_name in c("overview", "trajectory")) {
    app$click(selector = paste0('a[href="#shiny-tab-', tab_name, '"]'))
    app$wait_for_js(
      paste0(
        "(() => {",
        "const row = document.querySelector('#shiny-tab-",
        tab_name,
        " .cerebro-viz-top-layout');",
        "return row && row.getClientRects().length > 0;",
        "})()"
      ),
      timeout = 30000
    )
    layout <- app$get_js(top_layout_js(tab_name))
    expect_lte(layout$paramBottom, layout$vizTop + 1)
    expect_lt(abs(layout$paramLeft - layout$vizLeft), 1)
    expect_lt(abs(layout$paramRight - layout$vizRight), 1)
  }

  standalone_dblclick_stays_put <- function(tab_name, host_id) {
    app$click(selector = paste0('a[href="#shiny-tab-', tab_name, '"]'))
    canvas_selector <- paste0(
      "#",
      host_id,
      " .cv-canvas-wrap > canvas:not(.cv-mini)"
    )
    app$wait_for_js(
      paste0(
        "document.querySelector('",
        canvas_selector,
        "')?.getBoundingClientRect().height > 200"
      ),
      timeout = 30000
    )
    before <- app$get_js(paste0(
      "document.querySelector('",
      canvas_selector,
      "').getBoundingClientRect().height"
    ))
    app$run_js(paste0(
      "document.querySelector('",
      canvas_selector,
      "').dispatchEvent(",
      "new MouseEvent('dblclick',{bubbles:true,cancelable:true}));"
    ))
    app$wait_for_idle(timeout = 10000)
    after <- app$get_js(paste0(
      "document.querySelector('",
      canvas_selector,
      "').getBoundingClientRect().height"
    ))
    expect_equal(after, before, tolerance = 2)
    expect_false(isTRUE(app$get_js(paste0(
      "document.getElementById('",
      host_id,
      "').classList.contains('cv-has-focus')"
    ))))
  }

  app$click(selector = 'a[href="#shiny-tab-overview"]')
  app$wait_for_js(
    "document.getElementById('overview_projection_point_color')?.value === 'cell_type'",
    timeout = 30000
  )
  expect_false(isTRUE(app$get_js(
    "document.getElementById('overview_projection_keep_square') !== null"
  )))
  app$click(selector = "#overview_projection_more_button")
  app$wait_for_js(
    "document.getElementById('overview_projection_more').classList.contains('is-open')",
    timeout = 5000
  )
  filters_fit <- app$get_js(paste0(
    "(() => {",
    "const groups=Array.from(document.querySelectorAll(",
    "'#overview_projection_group_filters_UI .form-group'));",
    "return groups.every((group,index) => {",
    "if(index===groups.length-1)return true;",
    "const button=group.querySelector('.dropdown-toggle');",
    "const nextLabel=groups[index+1].querySelector('label');",
    "return button && nextLabel && ",
    "button.getBoundingClientRect().bottom <= ",
    "nextLabel.getBoundingClientRect().top;",
    "});",
    "})()"
  ))
  expect_true(isTRUE(filters_fit))
  app$click(selector = "#overview_projection_more [data-cerebro-drawer-close]")

  standalone_dblclick_stays_put(
    "overview",
    "overview_projection_cell_view_host"
  )
  standalone_dblclick_stays_put(
    "trajectory",
    "trajectory_projection_cell_view_host"
  )

  app$click(selector = 'a[href="#shiny-tab-immune_repertoire"]')
  app$set_inputs(ir_tabs = "Clonal UMAP", wait_ = FALSE)
  standalone_dblclick_stays_put(
    "immune_repertoire",
    "ir_clonalUMAP_projection_cell_view_host"
  )

  app$click(selector = 'a[href="#shiny-tab-immune_repertoire"]')
  app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#shiny-tab-immune_repertoire .cerebro-viz-top-layout')",
      ".getClientRects().length > 0"
    ),
    timeout = 10000
  )

  ## More settings is a viewport drawer, not another layout row. Opening it
  ## must leave the visualization at exactly the same position and size.
  viz_geometry_js <- paste0(
    "(() => {",
    "const v = document.querySelector(",
    "'#shiny-tab-immune_repertoire .cerebro-viz-col').getBoundingClientRect();",
    "return {left:v.left, top:v.top, width:v.width, height:v.height};",
    "})()"
  )
  viz_before_more <- app$get_js(viz_geometry_js)
  app$click(selector = "#ir_more_button")
  app$wait_for_js(
    paste0(
      "(() => {",
      "const d = document.getElementById('ir_more');",
      "return d.classList.contains('is-open') && ",
      "getComputedStyle(d).position === 'fixed';",
      "})()"
    ),
    timeout = 5000
  )
  viz_with_more <- app$get_js(viz_geometry_js)
  expect_equal(viz_with_more, viz_before_more, tolerance = 1)
  app$click(selector = "#ir_more [data-cerebro-drawer-close]")
  app$wait_for_js(
    "!document.getElementById('ir_more').classList.contains('is-open')",
    timeout = 5000
  )

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
      "const p = row.querySelector('.cerebro-viz-toolbar').getBoundingClientRect();",
      "const v = row.querySelector('.cerebro-viz-col').getBoundingClientRect();",
      "return p.bottom <= v.top + 1;",
      "})()"
    ),
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "(() => {",
      "const w = document.querySelector('.content-wrapper');",
      "return w.scrollWidth <= w.clientWidth;",
      "})()"
    ),
    timeout = 10000
  )

  narrow <- app$get_js(geometry_js)
  expect_lte(narrow$toolbarBottom, narrow$vizTop + 1)
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
      "const p = row.querySelector('.cerebro-viz-toolbar').getBoundingClientRect();",
      "const v = row.querySelector('.cerebro-viz-col').getBoundingClientRect();",
      "return p.bottom <= v.top + 1;",
      "})()"
    ),
    timeout = 10000
  )

  phone <- app$get_js(geometry_js)
  expect_lte(phone$toolbarBottom, phone$vizTop + 1)
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
