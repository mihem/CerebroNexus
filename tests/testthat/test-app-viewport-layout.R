test_that("shared selection actions and layouts survive navigation and resize", {
  inst_dir <- viewer_app_test_path()
  local_app_support(inst_dir)
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = "ir_fill_viewport",
    height = 950,
    width = 1619,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20000)

  # Observe the actual JSON blob after the server has validated the export.
  # Keep the real adapter, Shiny request, download handler and click behavior.
  download_view_config <- function(share_selector) {
    app$run_js(paste0(
      "(() => {const original=URL.createObjectURL;window.__testViewJSON=null;",
      "window.__restoreViewDownload=()=>{URL.createObjectURL=original;",
      "delete window.__restoreViewDownload;delete window.__testViewJSON;};",
      "URL.createObjectURL=function(blob){",
      "if(blob.type.startsWith('application/json'))",
      "blob.text().then(text=>{window.__testViewJSON=text;});",
      "return original.call(URL,blob);};})()"
    ))
    on.exit(app$run_js("window.__restoreViewDownload()"), add = TRUE)
    app$click(selector = share_selector)
    app$wait_for_js(
      "document.getElementById('cv-config-dialog').open && !document.getElementById('cv-config-download').disabled",
      timeout = 10000
    )
    app$click(selector = "#cv-config-download")
    app$wait_for_js("window.__testViewJSON !== null", timeout = 15000)
    config <- jsonlite::fromJSON(
      app$get_js("window.__testViewJSON"),
      simplifyVector = FALSE
    )
    app$click(selector = "#cv-config-close")
    config
  }

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
      ".some(el => el.textContent.includes('B cell maturation'))"
    ),
    timeout = 30000
  )
  app$wait_for_js(
    paste0(
      "(() => {const r=document.getElementById('cv-readout');",
      "return r && r.offsetParent === null && !r.textContent.trim();})()"
    ),
    timeout = 5000
  )
  linked_initial <- app$get_js(paste0(
    "(() => {const panes=Array.from(document.querySelectorAll(",
    "'.cv-pane:not(.cv-hidden)'));return {count:panes.length,",
    "rows:new Set(panes.map(p=>Math.round(p.getBoundingClientRect().top))).size};})()"
  ))
  expect_identical(linked_initial$count, 3L)
  expect_identical(linked_initial$rows, 1L)
  expect_true(app$get_js(
    "document.getElementById('cv-config-open').offsetParent !== null"
  ))
  app$wait_for_js("!document.getElementById('cv-config-open').disabled")
  empty_linked <- download_view_config("#cv-config-open")
  expect_identical(empty_linked$schema, "cerebronexus-linked-view")
  expect_length(empty_linked$selection$cells, 0L)
  expect_gt(empty_linked$dataset$cell_count, 0)
  expect_true(nzchar(empty_linked$dataset$cell_fingerprint))
  linked_right_edges <- app$get_js(paste0(
    "(() => {",
    "const top=document.querySelector('#shiny-tab-coordinated_views .cv-topbar')",
    ".getBoundingClientRect();",
    "const panes=Array.from(document.querySelectorAll(",
    "'#shiny-tab-coordinated_views .cv-pane:not(.cv-hidden)'))",
    ".map(p=>p.getBoundingClientRect());",
    "return {top:top.right,pane:Math.max(...panes.map(p=>p.right))};",
    "})()"
  ))
  expect_lt(abs(linked_right_edges$top - linked_right_edges$pane), 1)

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
    ".find(el => el.textContent.includes('B cell maturation'));",
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
      "Array.from(document.querySelectorAll('.cv-ptitle'))",
      ".find(el => el.textContent.includes('B cell maturation'))",
      ".closest('.cv-pane').querySelector('.cv-zsel-btn').offsetParent !== null && ",
      "document.getElementById('cv-config-open').offsetParent !== null && ",
      "document.getElementById('cv-readout').offsetParent !== null"
    ),
    timeout = 10000
  )
  linked_selection_status <- app$get_js(paste0(
    "(() => {",
    "const bar=document.getElementById('cv-selbar');",
    "const count=document.getElementById('cv-seltext');",
    "const origin=document.getElementById('cv-selorigin');",
    "const coverage=document.getElementById('cv-selcoverage');",
    "return {bar:bar.textContent.trim(),count:count.textContent.trim(),",
    "origin:origin.textContent.trim(),coverage:coverage.textContent.trim(),",
    "coverageLabels:Array.from(coverage.querySelectorAll('.cv-coverage-item'))",
    ".map(el=>el.getAttribute('aria-label')||'')};})()"
  ))
  expect_match(
    linked_selection_status$count,
    "^[0-9,]+ / 1,476 selected$"
  )
  expect_match(linked_selection_status$origin, "^From ")
  expect_no_match(
    linked_selection_status$bar,
    "Active cohort|coordinated across|Selected in"
  )
  expect_no_match(linked_selection_status$coverage, "[0-9,]+/[0-9,]+")
  expect_gt(length(linked_selection_status$coverageLabels), 0)
  expect_true(all(nzchar(linked_selection_status$coverageLabels)))
  linked_actions <- app$get_js(paste0(
    "(() => {",
    "const share=document.getElementById('cv-config-open').getBoundingClientRect();",
    "const settings=document.getElementById('cv-more-btn').getBoundingClientRect();",
    "return {shareBefore:share.left < settings.left,",
    "settingsRightmost:settings.right > share.right};",
    "})()"
  ))
  expect_true(linked_actions$shareBefore)
  expect_true(linked_actions$settingsRightmost)
  selected_linked <- download_view_config("#cv-config-open")
  expect_gt(length(selected_linked$selection$cells), 0L)

  expect_length(
    unlist(app$get_js(
      "Array.from(document.querySelectorAll('.cv-mini.is-on')).map(String)"
    )),
    0
  )

  app$run_js(paste0(
    "Array.from(document.querySelectorAll('.cv-ptitle'))",
    ".find(el => el.textContent.includes('B cell maturation'))",
    ".closest('.cv-pane').querySelector('.cv-zsel-btn').click()"
  ))
  app$wait_for_js(
    paste0(
      "(() => {",
      "const title = Array.from(document.querySelectorAll('.cv-ptitle'))",
      ".find(el => el.textContent.includes('B cell maturation'));",
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
  expect_match(zoomed_titles[[1]], "B cell maturation", fixed = TRUE)
  app$run_js(paste0(
    "Array.from(document.querySelectorAll('.cv-ptitle'))",
    ".find(el => el.textContent.includes('B cell maturation'))",
    ".closest('.cv-pane').querySelector('.cv-tbtn[data-act=\"reset\"]').click()"
  ))
  app$wait_for_js(
    "document.querySelectorAll('.cv-mini.is-on').length === 0",
    timeout = 10000
  )
  app$run_js("document.querySelector('#cv-clear span').click()")
  app$wait_for_js(
    "document.getElementById('cv-selbar').classList.contains('cv-collapse')",
    timeout = 10000
  )
  cleared_linked <- download_view_config("#cv-config-open")
  expect_length(cleared_linked$selection$cells, 0L)
  expect_null(cleared_linked$selection$geometry)
  expect_identical(cleared_linked$dataset, selected_linked$dataset)
  ## Touch users cannot reveal a hover-only toolbar. Coarse-pointer media must
  ## keep the controls visible and provide practical tap targets.
  app$get_chromote_session()$Emulation$setTouchEmulationEnabled(
    enabled = TRUE,
    maxTouchPoints = 1
  )
  app$wait_for_js(
    "getComputedStyle(document.querySelector('.cv-panebar')).opacity === '1'",
    timeout = 5000
  )
  touch_toolbar <- app$get_js(paste0(
    "(() => {",
    "const bar=document.querySelector('.cv-pane:not(.cv-hidden) .cv-panebar');",
    "const button=bar.querySelector('.cv-tbtn');",
    "const bs=getComputedStyle(bar), ts=getComputedStyle(button);",
    "return {opacity:bs.opacity,pointerEvents:bs.pointerEvents,",
    "width:parseFloat(ts.width),height:parseFloat(ts.height)};",
    "})()"
  ))
  expect_identical(touch_toolbar$opacity, "1")
  expect_identical(touch_toolbar$pointerEvents, "auto")
  expect_gte(touch_toolbar$width, 36)
  expect_gte(touch_toolbar$height, 36)
  app$get_chromote_session()$Emulation$setTouchEmulationEnabled(enabled = FALSE)

  ## Legend counts are data, not disabled decoration, so they use the readable
  ## secondary text colour instead of the low-contrast tertiary token.
  app$wait_for_js(
    "document.querySelector('.cv-lg-ct') !== null",
    timeout = 10000
  )
  legend_count_colour <- app$get_js(paste0(
    "getComputedStyle(document.querySelector('.cv-lg-ct')).color"
  ))
  expect_identical(legend_count_colour, "rgb(107, 107, 112)")

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

  sidebar_open <- app$get_js(paste0(
    "(() => {",
    "const sidebar=document.querySelector('.main-sidebar').getBoundingClientRect();",
    "const content=document.querySelector('.content-wrapper').getBoundingClientRect();",
    "const link=document.querySelector('.sidebar-menu > li > a').getBoundingClientRect();",
    "return {sidebarLeft:sidebar.left,sidebarRight:sidebar.right,",
    "contentLeft:content.left,linkX:link.left+link.width/2,",
    "linkY:link.top+link.height/2};",
    "})()"
  ))
  app$get_chromote_session()$Input$dispatchMouseEvent(
    type = "mouseMoved",
    x = sidebar_open$linkX,
    y = sidebar_open$linkY
  )
  app$wait_for_js(
    "document.querySelector('.sidebar-menu > li:hover') !== null",
    timeout = 5000
  )
  sidebar_hover <- app$get_js(paste0(
    "(() => {",
    "const sidebar=document.querySelector('.main-sidebar').getBoundingClientRect();",
    "const content=document.querySelector('.content-wrapper').getBoundingClientRect();",
    "const menu=getComputedStyle(document.querySelector('.sidebar-menu'));",
    "return {overflowX:menu.overflowX,overflowY:menu.overflowY,",
    "sidebarLeft:sidebar.left,sidebarRight:sidebar.right,contentLeft:content.left};",
    "})()"
  ))
  ## The menu scrolls vertically inside the flex column, with the preferences
  ## block pinned below it. Hovering must still not introduce a horizontal
  ## scrollbar or shift the sidebar, which the position checks below own.
  expect_identical(sidebar_hover$overflowX, "hidden")
  expect_true(sidebar_hover$overflowY %in% c("auto", "scroll"))
  expect_equal(
    sidebar_hover$sidebarLeft,
    sidebar_open$sidebarLeft,
    tolerance = 1
  )
  expect_equal(
    sidebar_hover$sidebarRight,
    sidebar_open$sidebarRight,
    tolerance = 1
  )
  expect_equal(
    sidebar_hover$contentLeft,
    sidebar_open$contentLeft,
    tolerance = 1
  )

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
      "style.color === 'rgb(167, 67, 8)';",
      "})()"
    ),
    timeout = 5000
  )
  toggle_hover <- app$get_js(toggle_style_js)
  expect_identical(toggle_hover$color, "rgb(167, 67, 8)")

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
      "const viz = row.querySelector('.cerebro-viz-col');",
      "const v = viz.getBoundingClientRect();",
      "const pane = viz.querySelector('.cv-pane:not(.cv-hidden)').getBoundingClientRect();",
      "return {paramBottom:p.bottom, vizTop:v.top, ",
      "paramLeft:p.left, paramRight:p.right, vizLeft:v.left, vizRight:v.right,",
      "paneRight:pane.right};",
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
        "const pane = row && row.querySelector('.cv-pane:not(.cv-hidden)');",
        "return pane && pane.getClientRects().length > 0;",
        "})()"
      ),
      timeout = 30000
    )
    layout <- app$get_js(top_layout_js(tab_name))
    expect_lte(layout$paramBottom, layout$vizTop + 1)
    expect_lt(abs(layout$paramLeft - layout$vizLeft), 1)
    expect_lt(abs(layout$paramRight - layout$vizRight), 1)
    expect_lt(abs(layout$paramRight - layout$paneRight), 1)
  }

  standalone_dblclick_stays_put <- function(tab_name, host_id, expected_title) {
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
    expect_identical(
      app$get_js(paste0(
        "document.querySelector('#",
        host_id,
        " .cv-ptitle').textContent.trim()"
      )),
      expected_title
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
    "document.getElementById('overview_projection_point_color')?.value === 'seurat_clusters'",
    timeout = 30000
  )
  expect_true(isTRUE(app$get_js(
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
    "overview_projection_cell_view_host",
    app$get_js(
      "document.getElementById('overview_projection_to_display').value"
    )
  )
  standalone_dblclick_stays_put(
    "trajectory",
    "trajectory_projection_cell_view_host",
    app$get_js("document.getElementById('trajectory_selected_name').value")
  )

  app$click(selector = 'a[href="#shiny-tab-immune_repertoire"]')
  app$set_inputs(ir_tabs = "Clonal UMAP", wait_ = FALSE)
  standalone_dblclick_stays_put(
    "immune_repertoire",
    "ir_clonalUMAP_projection_cell_view_host",
    "Clonal UMAP"
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

  ## The mobile shell must clear the fixed navigation toggle, and an inactive
  ## cohort card must not reserve height beyond the toolbar's 7px separator.
  app$click(selector = 'a[href="#shiny-tab-overview"]')
  app$wait_for_js(
    paste0(
      "document.querySelector('#shiny-tab-overview ",
      ".cerebro-selection-status-guide')?.getClientRects().length > 0"
    ),
    timeout = 30000
  )
  mobile_padding_top <- app$get_js(
    "parseFloat(getComputedStyle(document.querySelector('.content-wrapper')).paddingTop)"
  )
  mobile_selection_slot <- app$get_js(paste0(
    "(() => {",
    "const slot=document.querySelector('#shiny-tab-overview ",
    ".cerebro-selection-status-slot');",
    "const guide=slot.querySelector('.cerebro-selection-status-guide');",
    "const hidden=slot.querySelector('.cerebro-selection-status-hidden');",
    "hidden.style.minHeight='120px';",
    "const result={slot:slot.getBoundingClientRect().height,",
    "guide:guide.getBoundingClientRect().height};",
    "hidden.style.removeProperty('min-height');",
    "return result;",
    "})()"
  ))
  expect_equal(mobile_padding_top, 56)
  expect_equal(
    mobile_selection_slot$slot,
    mobile_selection_slot$guide + 7,
    tolerance = 1
  )

  ## The standalone composition card is a canvas overlay, so it can be moved
  ## away from data without leaving its visualization surface. Shiny replaces
  ## the card contents but keeps the stable output slot and its position.
  app$get_chromote_session()$set_viewport_size(width = 1200, height = 800)
  app$wait_for_js(
    "window.innerWidth === 1200 && window.innerHeight === 800",
    timeout = 10000
  )
  app$wait_for_js(
    paste0(
      "document.querySelector('#shiny-tab-overview ",
      ".cv-pane:not(.cv-hidden) canvas:not(.cv-mini)')",
      "?.getBoundingClientRect().width > 100"
    ),
    timeout = 30000
  )
  app$click(
    selector = paste0(
      "#shiny-tab-overview .cv-pane:not(.cv-hidden) ",
      ".cv-tbtn[data-act='box']"
    )
  )
  projection_box <- app$get_js(paste0(
    "(() => {const r=document.querySelector('#shiny-tab-overview ",
    ".cv-pane:not(.cv-hidden) canvas:not(.cv-mini)').getBoundingClientRect();",
    "return {x1:r.left+r.width*.08,y1:r.top+r.height*.08,",
    "x2:r.right-r.width*.08,y2:r.bottom-r.height*.08};})()"
  ))
  app$run_js(sprintf(
    paste0(
      "(() => {const pane=document.querySelector('#shiny-tab-overview ",
      ".cv-pane:not(.cv-hidden)');",
      "pane.dispatchEvent(new MouseEvent('mousedown',",
      "{bubbles:true,button:0,clientX:%f,clientY:%f}));",
      "window.dispatchEvent(new MouseEvent('mousemove',",
      "{bubbles:true,buttons:1,clientX:%f,clientY:%f}));",
      "window.dispatchEvent(new MouseEvent('mouseup',",
      "{bubbles:true,button:0,clientX:%f,clientY:%f}));})()"
    ),
    projection_box$x1,
    projection_box$y1,
    projection_box$x2,
    projection_box$y2,
    projection_box$x2,
    projection_box$y2
  ))
  app$wait_for_js(
    paste0(
      "document.querySelector('#shiny-tab-overview ",
      ".cerebro-selection-composition-drag')?.getClientRects().length > 0"
    ),
    timeout = 10000
  )
  app$wait_for_idle(timeout = 10000)
  card_before <- app$get_js(paste0(
    "(() => {const r=document.querySelector('#shiny-tab-overview ",
    ".cerebro-selection-composition-slot').getBoundingClientRect();",
    "const h=document.querySelector('#shiny-tab-overview ",
    ".cerebro-selection-composition-head').getBoundingClientRect();",
    "return {left:r.left,top:r.top,x:h.left+h.width/2,y:h.top+h.height/2};})()"
  ))
  viewer_drag_mouse(
    app,
    card_before$x,
    card_before$y,
    card_before$x + 150,
    card_before$y - 100
  )
  app$wait_for_js(
    paste0(
      "document.querySelector('#shiny-tab-overview ",
      ".cerebro-selection-composition-slot').dataset.dragged === 'true'"
    ),
    timeout = 5000
  )
  card_after <- app$get_js(paste0(
    "(() => {const s=document.querySelector('#shiny-tab-overview ",
    ".cerebro-selection-composition-slot');const f=s.parentElement.getBoundingClientRect();",
    "const r=s.getBoundingClientRect();return {left:r.left,top:r.top,",
    "inside:r.left>=f.left&&r.top>=f.top&&r.right<=f.right&&r.bottom<=f.bottom};})()"
  ))
  expect_gt(card_after$left, card_before$left + 50)
  expect_lt(card_after$top, card_before$top - 50)
  expect_true(card_after$inside)

  # Cancellation must stop movement as well as release the pointer. Checking
  # the card's position catches a stale drag handler after blur/cancel/capture loss.
  for (ending in c("pointercancel", "blur", "lostpointercapture")) {
    start <- app$get_js(paste0(
      "(() => {const h=document.querySelector('#shiny-tab-overview ",
      ".cerebro-selection-composition-head');const r=h.getBoundingClientRect();",
      "h.addEventListener('pointerdown',e=>{window.__dragPointer=e.pointerId;},{once:true});",
      "return {x:r.left+r.width/2,y:r.top+r.height/2};})()"
    ))
    mouse <- app$get_chromote_session()$Input$dispatchMouseEvent
    mouse(
      type = "mousePressed",
      x = start$x,
      y = start$y,
      button = "left",
      buttons = 1,
      clickCount = 1
    )
    app$wait_for_js(
      "document.querySelector('.cerebro-selection-composition-slot.is-dragging') !== null"
    )
    # Deliver a move so the browser activates pending pointer capture before
    # testing its loss (releasing pending capture alone emits no loss event).
    mouse(
      type = "mouseMoved",
      x = start$x + 1,
      y = start$y + 1,
      button = "left",
      buttons = 1
    )
    position <- function() {
      app$get_js(
        "(() => {const r=document.querySelector('#shiny-tab-overview .cerebro-selection-composition-slot').getBoundingClientRect();return {x:r.x,y:r.y};})()"
      )
    }
    before_cancel <- position()
    app$run_js(switch(
      ending,
      pointercancel = "document.dispatchEvent(new PointerEvent('pointercancel',{pointerId:window.__dragPointer,bubbles:true}))",
      blur = "window.dispatchEvent(new Event('blur'))",
      lostpointercapture = "document.querySelector('#shiny-tab-overview .cerebro-selection-composition-head').releasePointerCapture(window.__dragPointer)"
    ))
    mouse(
      type = "mouseMoved",
      x = start$x - 50,
      y = start$y + 30,
      button = "left",
      buttons = 1
    )
    app$wait_for_js(
      "!document.querySelector('.cerebro-selection-composition-slot.is-dragging')"
    )
    expect_equal(position(), before_cancel, tolerance = 1, info = ending)
    expect_false(app$get_js(
      "document.querySelector('#shiny-tab-overview .cerebro-selection-composition-head').hasPointerCapture(window.__dragPointer)"
    ))
    mouse(
      type = "mouseReleased",
      x = start$x - 50,
      y = start$y + 30,
      button = "left",
      buttons = 0,
      clickCount = 1
    )
  }

  app$get_chromote_session()$set_viewport_size(width = 390, height = 844)
  app$wait_for_js(
    "window.innerWidth === 390 && window.innerHeight === 844",
    timeout = 10000
  )
  mobile_card <- app$get_js(paste0(
    "(() => {const s=document.querySelector('#shiny-tab-overview ",
    ".cerebro-selection-composition-slot');const h=s.querySelector(",
    "'.cerebro-selection-composition-drag');return {position:getComputedStyle(s).position,",
    "handle:getComputedStyle(h).display};})()"
  ))
  expect_identical(mobile_card$position, "static")
  expect_identical(mobile_card$handle, "none")

  app$get_chromote_session()$set_viewport_size(width = 1200, height = 800)
  projection_share <- '#shiny-tab-overview .cerebro-toolbar-share'
  selected_projection <- download_view_config(projection_share)
  expect_identical(selected_projection$schema, "cerebronexus-specialist-view")
  expect_identical(selected_projection$page$id, "overview_projection")
  expect_gt(length(selected_projection$selection$cells), 0L)
  app$click(selector = "#overview_projection_clear_selection")
  app$wait_for_js(paste0(
    "document.getElementById('overview_projection_selection_guide')",
    ".getClientRects().length > 0 && ",
    "!document.querySelector('#shiny-tab-overview .cerebro-selection-composition-drag')"
  ))
  cleared_projection <- download_view_config(projection_share)
  expect_length(cleared_projection$selection$cells, 0L)
  expect_null(cleared_projection$selection$geometry)
  expect_identical(cleared_projection$dataset, selected_projection$dataset)
})
