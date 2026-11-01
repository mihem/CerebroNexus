## The Share dialog's download and open requests are answered by the Linked
## views server (coordinated_views/server.R). Every check here runs in a session
## that has never opened Linked views, which is where Share used to time out.

share_overview <- "#shiny-tab-overview .cerebro-toolbar-share"

share_single_view_app <- function(name) {
  inst_dir <- viewer_app_test_path()
  suppressWarnings(shinytest2::local_app_support(inst_dir))
  app <- shinytest2::AppDriver$new(
    inst_dir,
    name = name,
    height = 900,
    width = 1440,
    load_timeout = 60000
  )
  app$click(selector = 'a[href="#shiny-tab-overview"]')
  app$wait_for_js(
    paste0(
      "CerebroDatasetContext.phase()==='ready' && ",
      "window.cerebroSpecialistViews.get('overview_projection')?.ready() && ",
      "!document.querySelector('",
      share_overview,
      "').disabled"
    ),
    timeout = 60000
  )
  expect_false(app$get_js(
    "document.getElementById('shiny-tab-coordinated_views').classList.contains('active')"
  ))
  app
}

test_that("Share on a single-plot page downloads and opens before Linked views", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  ## Download the Projection view's JSON.
  app <- share_single_view_app("share_single_view_download")
  json <- viewer_share_json(app, share_overview)
  expect_match(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "^Downloaded the configuration"
  )
  config <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_identical(config$page$id, "overview_projection")
  app$stop()

  ## Open it in another fresh session.
  app <- share_single_view_app("share_single_view_open")
  withr::defer(app$stop())
  viewer_open_view_json(app, share_overview, json)
})

## Counts the JSON blobs the page turns into downloads.
count_json_downloads <- function(app) {
  app$run_js(paste0(
    "(() => {const original=URL.createObjectURL;window.__testJSONCount=0;",
    "URL.createObjectURL=function(blob){",
    "if(blob.type.startsWith('application/json'))window.__testJSONCount++;",
    "return original.call(URL,blob);};})()"
  ))
}

share_download_once <- function(app) {
  app$click(selector = share_overview)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-dialog').open && ",
      "!document.getElementById('cv-config-download').disabled"
    ),
    timeout = 10000
  )
  app$click(selector = "#cv-config-download")
  app$wait_for_js("window.__testJSONCount >= 1", timeout = 15000)
  Sys.sleep(3)
}

test_that("Opening Linked views before Share neither replays nor repeats a request", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- share_single_view_app("share_after_linked_views")
  withr::defer(app$stop())
  count_json_downloads(app)
  app$click(selector = 'a[href="#shiny-tab-coordinated_views"]')
  app$wait_for_js(
    "document.getElementById('shiny-tab-coordinated_views').classList.contains('active')",
    timeout = 20000
  )
  Sys.sleep(5)
  expect_identical(app$get_js("window.__testJSONCount"), 0L)
  app$click(selector = 'a[href="#shiny-tab-overview"]')
  app$wait_for_js(
    "document.getElementById('shiny-tab-overview').classList.contains('active')",
    timeout = 10000
  )
  share_download_once(app)
  expect_identical(app$get_js("window.__testJSONCount"), 1L)
})

test_that("Opening Linked views after Share does not answer the request again", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  app <- share_single_view_app("linked_views_after_share")
  withr::defer(app$stop())
  count_json_downloads(app)
  share_download_once(app)
  expect_identical(app$get_js("window.__testJSONCount"), 1L)
  app$click(selector = 'a[href="#shiny-tab-coordinated_views"]')
  app$wait_for_js(
    "document.getElementById('shiny-tab-coordinated_views').classList.contains('active')",
    timeout = 20000
  )
  Sys.sleep(8)
  expect_identical(app$get_js("window.__testJSONCount"), 1L)
  expect_false(grepl(
    "Loading linked views",
    app$get_js(
      "document.getElementById('shiny-tab-coordinated_views').innerText"
    ),
    fixed = TRUE
  ))
})

test_that("Opening a Trajectory view reports the cells the trajectory kept", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")

  ## The trajectory holds only some of the data set's cells; a saved selection
  ## may name others, which the view drops. The message counts what it kept.
  crb <- readCerebro(file.path(
    viewer_app_test_path(),
    "extdata",
    "examples",
    "demo_full_tcr_bcr.crb"
  ))
  method <- crb$getMethodsForTrajectories()[[1L]]
  name <- crb$getNamesOfTrajectories(method)[[1L]]
  on_trajectory <- rownames(crb$getTrajectory(method, name)$meta)
  off_trajectory <- setdiff(crb$getCellNames(), on_trajectory)
  skip_if(length(on_trajectory) < 3L || length(off_trajectory) < 2L)

  app <- share_single_view_app("share_trajectory_open")
  withr::defer(app$stop())
  trajectory_share <- "#shiny-tab-trajectory .cerebro-toolbar-share"
  app$click(selector = 'a[href="#shiny-tab-trajectory"]')
  app$wait_for_js(
    paste0(
      "window.cerebroSpecialistViews.get('trajectory_projection')?.ready() && ",
      "!document.querySelector('",
      trajectory_share,
      "').disabled"
    ),
    timeout = 60000
  )
  json <- viewer_share_json(app, trajectory_share)
  ## Rewrite the saved selection in the page, so the JSON keeps its shapes.
  named <- app$get_js(sprintf(
    "(() => {const c=JSON.parse(%s);c.selection.cells=%s;return JSON.stringify(c);})()",
    jsonlite::toJSON(json, auto_unbox = TRUE),
    jsonlite::toJSON(c(on_trajectory[1:3], off_trajectory[1:2]))
  ))
  app$run_js("document.getElementById('cv-config-close').click()")
  viewer_open_view_json(app, trajectory_share, named)
  app$wait_for_js(
    paste0(
      "document.getElementById('cv-config-status').textContent.startsWith(",
      "'Restored 3 of 5 selected cells')"
    ),
    timeout = 15000
  )
  expect_match(
    app$get_js("document.getElementById('cv-config-status').textContent"),
    "^Restored 3 of 5 selected cells"
  )
})
