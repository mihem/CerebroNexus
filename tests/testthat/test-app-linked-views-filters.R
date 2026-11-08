## Linked views group filters on a group whose values the first bundle defers
## (mihem/CerebroNexus#222). On the Trekker example only the main group,
## `cluster`, arrives with per-cell values; `celltype` is fetched on demand.

test_that("filtering a deferred group in Linked views shows the intersection", {
  skip_if_demo_data_missing()
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = "linked_views_filters",
    height = 900,
    width = 1500,
    load_timeout = 90000
  )
  withr::defer(app$stop())
  app$run_js(paste0(
    "window.__linkedErrors=[];",
    "window.addEventListener('error',e=>__linkedErrors.push(String(e.message)));"
  ))
  ## The dataset selector is rendered by the server; on a slow runner it is not
  ## bound yet when the app has loaded.
  app$wait_for_js(
    "Shiny.shinyapp.$inputValues.crb_file_selector !== undefined",
    timeout = 60000
  )
  app$set_inputs(crb_file_selector = "Mouse brain (Trekker)", wait_ = FALSE)
  app$wait_for_idle(timeout = 60000)
  app$click(selector = 'a[href="#shiny-tab-coordinated_views"]')
  app$wait_for_js(
    paste0(
      "(() => {const c=document.querySelector(",
      "'.cv-pane:not(.cv-hidden) canvas:not(.cv-mini)');",
      "return !!c && c.getBoundingClientRect().width>100 &&",
      " !!document.querySelector('.cv-filt[data-group=\"celltype\"]');})()"
    ),
    timeout = 60000
  )

  ## Coloured pixels of the first pane: the 1,000 example cells are drawn on a
  ## 2D canvas, so fewer shown cells mean fewer coloured pixels.
  ink <- function() {
    app$get_js(paste0(
      "(() => {const c=document.querySelector(",
      "'.cv-pane:not(.cv-hidden) canvas:not(.cv-mini)');",
      "const d=c.getContext('2d').getImageData(0,0,c.width,c.height).data;",
      "let n=0;for(let i=0;i<d.length;i+=4){",
      "if(d[i+3]>0&&(d[i]<235||d[i+1]<235||d[i+2]<235))n++;}return n;})()"
    ))
  }
  set_filter <- function(group, levels) {
    app$run_js(sprintf(
      paste0(
        "(() => {const f=document.querySelector('.cv-filt[data-group=\"%s\"]');",
        "const keep=%s;f.querySelectorAll('input[data-lv]').forEach(cb=>",
        "{cb.checked=keep.includes(Number(cb.dataset.lv));});",
        "f.querySelector('input[data-lv]').dispatchEvent(",
        "new Event('change',{bubbles:true}));})()"
      ),
      group,
      jsonlite::toJSON(levels)
    ))
  }
  wait_ink <- function(condition) {
    app$wait_for_js(
      paste0(
        "(() => {const c=document.querySelector(",
        "'.cv-pane:not(.cv-hidden) canvas:not(.cv-mini)');",
        "const d=c.getContext('2d').getImageData(0,0,c.width,c.height).data;",
        "let n=0;for(let i=0;i<d.length;i+=4){",
        "if(d[i+3]>0&&(d[i]<235||d[i+1]<235||d[i+2]<235))n++;}",
        "return ",
        condition,
        ";})()"
      ),
      timeout = 20000
    )
  }
  colour <- function() {
    app$get_js("document.getElementById('cv-pick-color').selectize.getValue()")
  }

  all_ink <- ink()
  expect_gt(all_ink, 0)

  ## Only Astro (63 of 1,000 cells): the panes shrink to those cells once the
  ## celltype values arrive, and the colouring stays on cluster.
  set_filter("celltype", 0L)
  wait_ink(sprintf("n > 0 && n < %d", round(all_ink * 0.2)))
  astro_ink <- ink()
  expect_identical(colour(), "cluster")

  ## Every Astro cell is in the first five clusters, so adding that cluster
  ## filter keeps the same cells; selecting all clusters keeps them too.
  same_cells <- sprintf(
    "Math.abs(n - %d) <= %d",
    astro_ink,
    ceiling(astro_ink * 0.05)
  )
  set_filter("cluster", 0:4)
  wait_ink(same_cells)
  set_filter("cluster", 0:17)
  wait_ink(same_cells)

  ## All cell types again: back to the full picture.
  set_filter("celltype", 0:7)
  wait_ink(sprintf("n >= %d", round(all_ink * 0.9)))

  expect_length(app$get_js("window.__linkedErrors"), 0L)
})
