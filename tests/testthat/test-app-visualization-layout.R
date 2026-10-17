test_that("all visualization pages keep usable controls, legends and canvases", {
  app_dir <- viewer_app_test_path()
  local_app_support(app_dir)
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "visualization_layout",
    width = 1440,
    height = 1000,
    load_timeout = 60000
  )
  on.exit(app$stop(), add = TRUE)
  expect_true(app$get_js(
    "document.querySelector('main#main-content') !== null && document.querySelector('a.cerebro-skip-link').getAttribute('href') === '#main-content'"
  ))
  expect_true(app$get_js(
    "document.querySelector('#shiny-tab-load_data h1')?.textContent.includes('Data info') || Array.from(document.querySelectorAll('h1')).some(el=>el.textContent==='Data info')"
  ))
  expect_true(app$get_js(
    "['auto','scroll'].includes(getComputedStyle(document.querySelector('.main-sidebar')).overflowY)"
  ))
  expect_true(app$get_js(
    "document.getElementById('cerebro-page-loader-announcement').getAttribute('aria-live') === 'polite'"
  ))
  # Exercise the browser's actual change listener. Re-dispatching a change for
  # the already selected data set is a no-op: the loader stays hidden instead of
  # flashing for a load that never starts. The overlay itself only accepts
  # progress from the data set being loaded, so its card and progress mapping are
  # covered by test-dataset-loading-progress.R rather than from here.
  app$run_js(
    "document.getElementById('crb_file_selector').dispatchEvent(new Event('change',{bubbles:true}))"
  )
  app$wait_for_js(
    "document.getElementById('cerebro-dataset-loading')?.hidden === true"
  )
  probe <- paste(
    readLines(testthat::test_path("fixtures", "viewer-layout.js")),
    collapse = "\n"
  )
  artifacts <- Sys.getenv("CEREBRO_LAYOUT_ARTIFACTS")
  if (nzchar(artifacts)) {
    dir.create(artifacts, recursive = TRUE, showWarnings = FALSE)
  }
  pages <- c(
    overview = "PBMC - Full (T+B)",
    coordinated_views = "PBMC - Full (T+B)",
    geneExpression = "PBMC - Full (T+B)",
    trajectory = "PBMC - Full (T+B)",
    immune_repertoire = "PBMC - Full (T+B)",
    spatial = "Mouse brain (Visium)",
    trekker = "Mouse brain (Trekker)",
    hla_tcr_motifs = "HLA & TCR"
  )
  previous <- "PBMC - Full (T+B)"
  measurements <- list()
  for (page in names(pages)) {
    if (pages[[page]] != previous) {
      app$set_inputs(crb_file_selector = pages[[page]], wait_ = FALSE)
      previous <- pages[[page]]
      # A page the outgoing data set had may not exist in the new one, and the
      # server then moves the selection to Data info. That fallback can land
      # after a click that races it, so wait until the selected sidebar entry is
      # one the new data set still offers before choosing the page.
      app$wait_for_js(
        paste0(
          "(() => {const li=document.querySelector(",
          "'.sidebar-menu li.active');",
          "return !!li && li.style.display !== 'none' && ",
          "!!li.querySelector('a[data-toggle=\"tab\"]');})()"
        ),
        timeout = 30000
      )
      app$wait_for_idle(timeout = 30000)
    }
    selector <- paste0('a[href="#shiny-tab-', page, '"]')
    app$wait_for_js(
      sprintf(
        "document.querySelector('%s')?.getClientRects().length > 0",
        selector
      ),
      timeout = 30000
    )
    app$click(selector = selector)
    app$wait_for_js(
      sprintf(
        "Array.from(document.querySelectorAll('#shiny-tab-%s .cv-pane:not(.cv-hidden) canvas:not(.cv-mini)')).some(c=>c.getBoundingClientRect().height > 100)",
        page
      ),
      timeout = 60000
    )
    app$wait_for_idle(timeout = 30000)
    if (page == "coordinated_views") {
      expect_true(app$get_js(
        "document.getElementById('cv-pick-proj').multiple"
      ))
      expect_true(app$get_js(
        "!!document.getElementById('cv-pick-color').selectize"
      ))
      app$run_js(
        "window.__pngURL=null;window.__anchorClick=HTMLAnchorElement.prototype.click;HTMLAnchorElement.prototype.click=function(){window.__pngURL=this.href};window.cerebroLinkedViewsState.downloadPNG();"
      )
      app$wait_for_js(
        "window.__pngURL?.startsWith('data:image/png;base64,')",
        timeout = 30000
      )
      app$run_js(
        "window.__pngSize=null;const image=new Image();image.onload=()=>{window.__pngSize={width:image.naturalWidth,height:image.naturalHeight}};image.src=window.__pngURL;HTMLAnchorElement.prototype.click=window.__anchorClick;"
      )
      app$wait_for_js("window.__pngSize !== null")
      dimensions <- app$get_js("window.__pngSize")
      expect_gt(dimensions$width, 100)
      expect_gt(dimensions$height, 100)
    }
    if (page == "trekker") {
      app$set_inputs(trekker_view = "morph", trekker_morph = 0)
      app$wait_for_idle()
      app$run_js(
        "window.__trekkerCanvas=Array.from(document.querySelectorAll('#shiny-tab-trekker canvas:not(.cv-mini)')).find(c=>c.getClientRects().length);window.__trekkerBefore=window.__trekkerCanvas.toDataURL();jQuery('#trekker_morph').val('1').trigger('change');"
      )
      app$wait_for_js(
        "window.__trekkerCanvas.toDataURL()!==window.__trekkerBefore",
        timeout = 15000
      )
    }
    for (width in c(1440, 760)) {
      app$get_chromote_session()$set_viewport_size(width = width, height = 1000)
      # Sidebar transitions outlive a single paint. Sample until geometry has
      # stopped moving rather than capturing an intermediate animation frame.
      app$run_js("window.__layoutPrevious=null;window.__layoutStable=0")
      app$wait_for_js(
        sprintf(
          paste0(
            "(() => {const next=JSON.stringify((%s)(%s));",
            "window.__layoutStable=next===window.__layoutPrevious?window.__layoutStable+1:0;",
            "window.__layoutPrevious=next;return window.__layoutStable>=3;})()"
          ),
          probe,
          jsonlite::toJSON(page, auto_unbox = TRUE)
        ),
        timeout = 10000,
        interval = 100
      )
      layout <- app$get_js(sprintf(
        "(%s)(%s)",
        probe,
        jsonlite::toJSON(page, auto_unbox = TRUE)
      ))
      label <- paste(page, width)
      expect_gt(length(layout$canvases), 0L, label = label)
      expect_false(layout$documentOverflow, info = label)
      expect_false(layout$paneOverlap, info = label)
      expect_true(layout$accessibleCanvases, info = label)
      expect_true(layout$decorativeMinimaps, info = label)
      expect_true(layout$singleGpuLayer, info = label)
      expect_true(layout$heading, info = label)
      if (!is.null(layout$tabs)) {
        expect_equal(layout$tabs$rows, 1L, info = label)
      }
      for (canvas in layout$canvases) {
        expect_gte(canvas$top + 1, layout$toolbar$bottom, label = label)
        expect_gt(canvas$width, 100, label = label)
        expect_gt(canvas$height, 100, label = label)
        for (legend in layout$legends) {
          if (page == "coordinated_views") {
            expect_lte(legend$bottom, canvas$top + 1, label = label)
          }
          expect_true(
            legend$bottom <= canvas$top + 1 || legend$top >= canvas$bottom - 1,
            info = label
          )
          expect_false(legend$overflow, info = label)
        }
      }
      for (button in layout$actions) {
        expect_gte(button$left + 1, layout$toolbar$left, label = label)
        expect_lte(button$right, layout$toolbar$right + 1, label = label)
        expect_gte(button$font, 12, label = label)
      }
      settings <- Filter(function(x) x$label == "Settings", layout$actions)
      expect_length(settings, 1L)
      measurements[[paste(page, width, sep = "-")]] <- layout
      if (nzchar(artifacts)) {
        screenshot <- tempfile(fileext = ".png")
        app$get_screenshot(file = screenshot)
        file.copy(
          screenshot,
          file.path(artifacts, paste0(page, "-", width, ".png")),
          overwrite = TRUE
        )
        unlink(screenshot)
      }
    }
    # Exercise the real drawer target, including Trekker's shared Linked drawer.
    app$get_chromote_session()$Emulation$setEmulatedMedia(
      features = list(list(name = "prefers-reduced-motion", value = "reduce"))
    )
    app$click(selector = paste0("#shiny-tab-", page, " .cerebro-more-btn"))
    app$wait_for_js(
      "Array.from(document.querySelectorAll('.cerebro-settings-drawer')).some(e=>e.getAttribute('aria-hidden')==='false')"
    )
    app$wait_for_idle(timeout = 30000)
    drawer_probe <- paste(
      readLines(testthat::test_path("fixtures", "viewer-drawer.js")),
      collapse = "\n"
    )
    for (width in c(1440, 760)) {
      app$get_chromote_session()$set_viewport_size(width = width, height = 1000)
      # CDP updates innerWidth before Chrome dispatches resize. Let the browser
      # process resize and paint before inspecting the drawer's modal state.
      app$get_js(
        "new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(() => resolve(true))))"
      )
      drawer_layout <- app$get_js(paste0("(", drawer_probe, ")()"))
      expect_true(drawer_layout$fitsViewport, info = paste(page, width))
      expect_true(drawer_layout$fitsContents, info = paste(page, width))
      expect_false(drawer_layout$overlaps, info = paste(page, width))
      expect_true(drawer_layout$reducedMotion, info = paste(page, width))
      expect_true(drawer_layout$modalSemantics, info = paste(page, width))
      expect_true(drawer_layout$backgroundInert, info = paste(page, width))
      expect_true(drawer_layout$wrapsFocus, info = paste(page, width))
      expect_gt(drawer_layout$controls, 0L, label = page)
    }
    if (page == "hla_tcr_motifs") {
      expect_true(app$get_js(
        "document.getElementById('hla_min_nodes').closest('.cerebro-settings-drawer').getAttribute('aria-hidden')==='false'"
      ))
    }
    app$run_js(
      "document.dispatchEvent(new KeyboardEvent('keydown',{key:'Escape',bubbles:true}))"
    )
    app$wait_for_js(
      "!Array.from(document.querySelectorAll('.cerebro-settings-drawer')).some(e=>e.getAttribute('aria-hidden')==='false')"
    )
    app$get_chromote_session()$set_viewport_size(width = 1440, height = 1000)
    app$get_chromote_session()$Emulation$setEmulatedMedia(features = list())
    if (page == "spatial") {
      expect_true(app$get_js(
        "document.getElementById('spatial_projection_background_image').closest('.cerebro-viz-toolbar') !== null"
      ))
    }
    if (page == "immune_repertoire") {
      app$set_inputs(ir_tabs = "Abundance", wait_ = FALSE)
      app$wait_for_js(
        "document.getElementById('ir_selection_status_UI').textContent.includes('Selection is available in Clonal projection')"
      )
      expect_false(app$get_js(
        "document.querySelector('#shiny-tab-immune_repertoire .cerebro-toolbar-share').getClientRects().length > 0"
      ))
    }
    if (page == "hla_tcr_motifs") {
      state <- "window.cerebroSpecialistViews.get('hla_motif_network')"
      app$wait_for_js(paste0(state, ".ready()"), timeout = 30000)
      expect_false(app$get_js(
        "document.querySelector('#shiny-tab-hla_tcr_motifs .cerebro-toolbar-share').disabled"
      ))
      app$run_js(
        "Array.from(document.querySelectorAll('#hla_motif_network_cell_view_host .cv-tbtn[data-act=\"box\"]')).find(b=>b.getClientRects().length>0).click()"
      )
      box <- app$get_js(
        "(() => {const r=document.querySelector('#hla_motif_network_cell_view_host canvas:not(.cv-mini)').getBoundingClientRect();return {x:r.x,y:r.y,w:r.width,h:r.height};})()"
      )
      viewer_drag_mouse(
        app,
        box$x + box$w * .2,
        box$y + box$h * .2,
        box$x + box$w * .8,
        box$y + box$h * .8
      )
      app$wait_for_js(paste0(state, ".capture().selection.cells.length > 0"))
      before <- app$get_js(paste0(state, ".capture().view"))
      app$click(selector = "#hla_motif_network_focus_selection")
      app$wait_for_js(
        "document.getElementById('hla_motif_network_focus_selection').getAttribute('aria-pressed')==='true'"
      )
      expect_false(identical(
        app$get_js(paste0(state, ".capture().view")),
        before
      ))
      app$click(selector = "#hla_motif_network_clear_selection")
      app$wait_for_js(paste0(state, ".capture().selection.cells.length === 0"))
      expect_null(app$get_js(paste0(state, ".capture().selection.geometry")))
      expect_identical(
        app$get_js(
          "document.getElementById('hla_motif_network_focus_selection').getAttribute('aria-pressed')"
        ),
        "false"
      )
      # Change the allele on Associations while its Network picker is absent.
      app$set_inputs(hla_tabs = "HLA Associations", wait_ = FALSE)
      app$wait_for_js(
        "Object.keys(document.getElementById('hla_association_allele')?.selectize?.options || {}).length>1",
        timeout = 30000
      )
      allele <- app$get_js(
        "Object.keys(document.getElementById('hla_association_allele').selectize.options)[1]"
      )
      app$set_inputs(hla_association_allele = allele)
      app$set_inputs(
        hla_tabs = "Motif Network",
        hla_color_by = "hla_carrier",
        wait_ = FALSE
      )
      allele_json <- jsonlite::toJSON(allele, auto_unbox = TRUE)
      app$wait_for_js(
        sprintf(
          "document.getElementById('hla_color_allele')?.selectize?.getValue()===%s",
          allele_json
        ),
        timeout = 30000
      )
      app$wait_for_js(
        sprintf(
          "document.getElementById('hla_legend_ui')?.textContent.includes(%s)",
          allele_json
        ),
        timeout = 30000
      )
      app$set_inputs(hla_tabs = "HLA Associations", wait_ = FALSE)
      app$wait_for_js(
        sprintf(
          "document.getElementById('hla_association_allele')?.selectize?.getValue()===%s",
          allele_json
        ),
        timeout = 30000
      )
    }
  }
  # Global Shiny notifications leave a drawer open. Replacing its containing
  # output must still restore it before Shiny removes that output's DOM.
  ui <- viewer_ui_test_env()
  fixture <- as.character(shiny::div(
    id = "drawer_lifecycle_probe",
    class = "shiny-bound-output",
    ui$cerebroSettingsButton("drawer_probe_open", "drawer_probe"),
    ui$cerebroSettingsDrawer("drawer_probe", shiny::p("Settings"))
  ))
  app$run_js(sprintf(
    "document.body.insertAdjacentHTML('beforeend',%s)",
    jsonlite::toJSON(fixture, auto_unbox = TRUE)
  ))
  app$run_js("document.getElementById('drawer_probe_open').click()")
  app$wait_for_js(
    "document.getElementById('drawer_probe').getAttribute('aria-hidden')==='false'"
  )
  app$run_js("jQuery(document).trigger('shiny:value')")
  expect_identical(
    app$get_js(
      "document.getElementById('drawer_probe').getAttribute('aria-hidden')"
    ),
    "false"
  )
  app$run_js(
    "jQuery(document.getElementById('drawer_lifecycle_probe')).trigger('shiny:outputinvalidated')"
  )
  app$wait_for_js(
    "document.getElementById('drawer_probe').getAttribute('aria-hidden')==='true'"
  )
  expect_true(app$get_js(
    "document.getElementById('drawer_lifecycle_probe').contains(document.getElementById('drawer_probe'))"
  ))
  app$run_js("document.getElementById('drawer_lifecycle_probe').remove()")
  heights <- vapply(
    measurements,
    function(x) {
      Filter(function(b) b$label == "Settings", x$actions)[[1L]]$height
    },
    numeric(1)
  )
  expect_lt(diff(range(heights)), 1)
  expect_identical(
    measurements[["immune_repertoire-1440"]]$tabs,
    measurements[["hla_tcr_motifs-1440"]]$tabs
  )
  if (nzchar(artifacts)) {
    jsonlite::write_json(
      measurements,
      file.path(artifacts, "geometry.json"),
      auto_unbox = TRUE,
      pretty = TRUE
    )
  }
})
