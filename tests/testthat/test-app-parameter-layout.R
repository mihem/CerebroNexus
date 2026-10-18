test_that("parameter bars use available width and wrap without losing controls", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  app_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(app_dir))
  app <- shinytest2::AppDriver$new(
    app_dir,
    name = "parameter_layout",
    width = 1440,
    height = 950,
    load_timeout = 60000
  )
  withr::defer(app$stop())
  probe <- paste(
    readLines(test_path("fixtures", "parameter-layout.js")),
    collapse = "\n"
  )
  artifacts <- Sys.getenv("CEREBRO_LAYOUT_ARTIFACTS")
  if (nzchar(artifacts)) {
    dir.create(artifacts, recursive = TRUE, showWarnings = FALSE)
  }
  pages <- c(
    overview = "PBMC - Full (T+B)",
    groups = "PBMC - Full (T+B)",
    coordinated_views = "PBMC - Full (T+B)",
    geneExpression = "PBMC - Full (T+B)",
    trajectory = "PBMC - Full (T+B)",
    immune_repertoire = "PBMC - Full (T+B)",
    spatial = "Mouse brain (Visium)",
    trekker = "Mouse brain (Trekker)",
    hla_tcr_motifs = "HLA & TCR"
  )
  previous <- pages[[1]]
  measure <- function(page, suffix = page) {
    for (width in c(1440, 760, 390)) {
      app$set_window_size(width = width, height = 950)
      expression <- sprintf(
        "(%s)(%s)",
        probe,
        jsonlite::toJSON(page, auto_unbox = TRUE)
      )
      app$run_js("window.previousParameters=null;window.stableParameters=0")
      app$wait_for_js(
        sprintf(
          paste0(
            "(() => {const next=JSON.stringify(%s);",
            "stableParameters=next===previousParameters?stableParameters+1:0;",
            "previousParameters=next;return stableParameters>=3;})()"
          ),
          expression
        ),
        interval = 100
      )
      layout <- app$get_js(expression)
      label <- paste(suffix, width)
      expect_gt(layout$count, 0, label = label)
      expect_false(layout$overflow, info = label)
      expect_false(layout$overlap, info = label)
      expect_true(layout$inputsFillFields, info = label)
      if (page == "geneExpression") {
        expect_identical(layout$geneVisible, suffix != "rgb", info = label)
        expect_identical(layout$rgbVisible, suffix == "rgb", info = label)
        if (suffix != "rgb" && width == 1440) {
          expect_gt(layout$geneWidth, layout$projectionWidth, label = label)
          expect_equal(layout$fieldRows, 1, info = label)
        }
      }
      if (nzchar(artifacts)) {
        shot <- app$get_chromote_session()$Page$captureScreenshot(
          format = "png"
        )
        writeBin(
          jsonlite::base64_dec(shot$data),
          file.path(artifacts, paste0(suffix, "-", width, ".png"))
        )
      }
    }
    app$set_window_size(width = 1440, height = 950)
  }
  for (page in names(pages)) {
    if (pages[[page]] != previous) {
      app$set_inputs(crb_file_selector = pages[[page]])
      app$wait_for_js(
        "document.getElementById('cerebro-dataset-loading').hidden",
        timeout = 60000
      )
      previous <- pages[[page]]
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
        paste0(
          "Array.from(document.querySelectorAll(",
          "'#shiny-tab-%s .cerebro-viz-primary .form-group, #shiny-tab-%s .cv-filters .cv-ctl, ",
          "#shiny-tab-%s .cerebro-groups-controls .form-group'))",
          ".some(e=>e.getBoundingClientRect().width>0)"
        ),
        page,
        page,
        page
      ),
      timeout = 30000
    )
    app$wait_for_idle(timeout = 30000)
    if (page == "geneExpression") {
      viewer_set_selectize(
        app,
        "expression_genes_input",
        c("MS4A1", "CD3D", "CD79A", "CD3E")
      )
      app$set_inputs(
        expression_projection_genes_in_separate_panels = "separate"
      )
      expect_identical(
        unlist(app$get_js(paste0(
          "Array.from(document.querySelectorAll(",
          "'#expression_genes_input + .selectize-control .item')).map(e=>e.firstChild.textContent)"
        ))),
        c("MS4A1", "CD3D", "CD79A", "CD3E")
      )
    }
    measure(page)
    if (page == "geneExpression") {
      app$set_inputs(expression_projection_genes_in_separate_panels = "rgb")
      measure(page, "rgb")
      app$set_inputs(
        expression_projection_genes_in_separate_panels = "separate"
      )
      expect_identical(
        unlist(app$get_js(
          "document.getElementById('expression_genes_input').selectize.getValue()"
        )),
        c("MS4A1", "CD3D", "CD79A", "CD3E")
      )
    }
    if (page == "groups") {
      next
    }
    app$click(selector = paste0('#shiny-tab-', page, ' .cerebro-more-btn'))
    app$wait_for_js(
      "!!document.querySelector('.cerebro-settings-drawer.is-open')"
    )
    for (width in c(1440, 390)) {
      app$set_window_size(width = width, height = 950)
      app$wait_for_js(paste0(
        "(() => {const drawer=document.querySelector('.cerebro-settings-drawer.is-open');",
        "const fields=Array.from(drawer.querySelectorAll('.form-group')).filter(e=>e.getClientRects().length);",
        "const bounds=drawer.getBoundingClientRect();return fields.length>0 && fields.every(e=>{",
        "const r=e.getBoundingClientRect();return r.left>=bounds.left && r.right<=bounds.right;});})()"
      ))
      expect_true(
        app$get_js(paste0(
          "Array.from(document.querySelectorAll('.cerebro-settings-drawer.is-open .selectize-control'))",
          ".filter(e=>e.getClientRects().length).every(e=>Math.abs(e.clientWidth-e.closest('.form-group').clientWidth)<2)"
        )),
        info = paste(page, "settings", width)
      )
      if (nzchar(artifacts)) {
        shot <- app$get_chromote_session()$Page$captureScreenshot(
          format = "png"
        )
        writeBin(
          jsonlite::base64_dec(shot$data),
          file.path(artifacts, paste0(page, "-settings-", width, ".png"))
        )
      }
    }
    app$click(
      selector = '.cerebro-settings-drawer.is-open .cerebro-settings-close'
    )
    app$set_window_size(width = 1440, height = 950)
  }
})
